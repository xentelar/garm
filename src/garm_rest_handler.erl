%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2025 Xentelar Advanced Technologies. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @doc 
%% @end
%% -----------------------------------------------------------------------------

-module(garm_rest_handler).

-include_lib("kernel/include/logger.hrl").

-include("http_commons.hrl").

-type req() :: cowboy_req:req().
-type result() :: true | {false, iodata()}.
-type content_types() :: [{binary(), atom()}].
-type processed_response() :: {stop, req(), state()}.
-type state() :: state().
-type result_ok() :: {ok, {cowboy:http_status(), 
                          cowboy:http_headers(), 
                          iodata()}}.
-type result_error() :: {error, any()}.


-record(state, {
  operation_id        :: binary(),
  cfg = #{}           :: map(),
  valid_body          :: atom(),
  auth_control        :: atom(),
  adapter             :: atom(),
  valid_response      :: atom(),
  origin              :: binary(),
  methods             :: [binary()],
  security            :: map(),
  param_values        :: map()
}).

-define(URI_LONG, 1024).
-define(ENTITY_LENGTH, 1024).

%% =============================================================================
%% public functions
%% =============================================================================

% Cowboy REST callbacks
-export([init/2]).
-export([known_methods/2]).
-export([uri_too_long/2]).
-export([allowed_methods/2]).
-export([malformed_request/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([rate_limited/2]).
-export([valid_content_headers/2]).
-export([valid_entity_length/2]).

-export([content_types_provided/2]).

-export([options/2]).
-export([delete_resource/2]).

-export([content_types_accepted/2]).

%% Handler
-export([process_request/2]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec init(Req :: req(), Opts :: garm_cowboy_config:init_opts()) ->
  {cowboy_rest, Req :: req(), State :: state()}.
init(Req, {MethodsCfg, ValidBody, AuthControl, Adapter, ValidResponse}) ->
  Method = cowboy_req:method(Req),
  Method0 = garm_utils:to_lower(Method),
  Origin = cowboy_req:header(<<"origin">>, Req, <<"*">>),

  ?LOG_DEBUG(#{description => "Request to be processed",
            method => Method,
            request => Req,
            state => {MethodsCfg, ValidBody, AuthControl, Adapter, ValidResponse}}),

  MethodCfg = maps:get(Method0, MethodsCfg, #{}),
  OperationID = maps:get(<<"operationId">>, MethodCfg, not_allowed),
  
  Methods = maps:keys(MethodsCfg) ++ [<<"OPTIONS">>],
  Methods0 = lists:map(fun(M) -> garm_utils:to_upper(M) end, Methods),

  State = #state{
    operation_id = OperationID,
    cfg = MethodCfg,
    valid_body = ValidBody, 
    auth_control = AuthControl, 
    adapter = Adapter,
    valid_response = ValidResponse,
    origin = Origin,
    methods = Methods0
  },

  ?LOG_DEBUG(#{description => "Attempt to process operation",
              operation_id => OperationID,
              state => State}),

  {cowboy_rest, Req, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec known_methods(Req :: req(), State :: state()) ->
  {Value :: [binary()], Req :: req(), State :: state()}.
known_methods(Req, #state{origin = Origin, methods = Methods} = State) ->
  Method = cowboy_req:method(Req),
  case lists:member(Method, Methods) of
    false ->
      ?LOG_DEBUG(#{description => "Unknown Method", 
                  method => Method, methods => Methods}),
      {Methods, resp_headers(Req, Origin), State};

    true ->
      ?LOG_DEBUG(#{description => "Method is ok",
                  method => Method, methods => Methods}),
      {Methods, Req, State}
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec uri_too_long(Req :: req(), State :: state()) ->
  {Value :: [binary()], Req :: req(), State :: state()}.
uri_too_long(Req, #state{origin = Origin} = State) ->
  case cowboy_req:method(Req) of
    <<"OPTIONS">> ->
      ?LOG_DEBUG(#{description => "No validate", 
                  method => <<"OPTIONS">>}),
      {false, Req, State};

    Method ->
      Path = cowboy_req:path(Req),
      case bit_size(Path) of
        A when A>?URI_LONG ->
          ?LOG_DEBUG(#{description => "API Path is too long",
                      method => Method}),
          {true, resp_headers(Req, Origin), State};
        _ ->
          ?LOG_DEBUG(#{description => "API Path is ok", 
                      method => Method}),
          {false, Req, State}
      end
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec allowed_methods(Req :: req(), State :: state()) ->
  {Value :: [binary()], Req :: req(), State :: state()}.
allowed_methods(Req, #state{origin = Origin, methods = Methods} = State) ->
  Method = cowboy_req:method(Req),
  case lists:member(Method, Methods) of
    fasle ->
      ?LOG_DEBUG(#{description => "No allowed Method",
                  method => Method, methods => Methods}),
      {Methods, resp_headers(Req, Origin), State};

    true ->
      ?LOG_DEBUG(#{description => "Allowed Method",
                  method => Method, methods => Methods}),
      {Methods, Req, State}
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec malformed_request(Req :: req(), State :: state()) ->
  {Value :: false, Req :: req(), State :: state()}.
malformed_request(Req, #state{origin = Origin, cfg = Cfg} = State) ->
  try 
    ParamValues = garm_http_request:get_params_values(Cfg, Req),
    HExt = cowboy_req:headers(Req),
    ParamValues0 = ParamValues#{<<"headers-ext">> => HExt},
    {false, Req, State#state{param_values = ParamValues0}}
  catch
    _Class:Exception:Stacktrace ->
      ?LOG_DEBUG(#{description => "Parameter value error", 
        exception => Exception, stacktrace => Stacktrace}),
      {true, resp_headers(Req, Origin), State}
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_authorized(Req :: req(), State :: state()) ->
  {Value :: result(), Req :: req(), State :: state()}.
is_authorized(Req, #state{origin = Origin} = State) ->
  case cowboy_req:method(Req) of
    <<"OPTIONS">> ->
      ?LOG_DEBUG(#{description => "No security is needed", 
                  method => <<"OPTIONS">>}),
      {true, Req, State};

    Method ->
      Req0 = resp_headers(Req, Origin),
      case State#state.auth_control of
        auth_control_undefined ->
          ?LOG_DEBUG(#{description => "Auth Control is undefined", 
                      method => Method}),
          %{{false, <<"auth control is undefined">>}, Req0, State};
          {true, Req, State};

        AuthModule ->
          Cfg = State#state.cfg,
          case maps:get(<<"security">>, Cfg, undefined) of
            undefined ->
              ?LOG_DEBUG(#{description => "Security config not found", 
                          method => Method}),
              {true, Req, State};

            _ ->
              case garm_http_request:get_header_value(<<"Authorization">>, Req) of
                undefined ->
                  ?LOG_DEBUG(#{description => "Authorization header is undefined", 
                          method => Method}),
                  {{false, <<"Authorization header is undefined">>}, Req0, State};

                Token ->
                  Scopes = [],
                  case AuthModule:is_authorized(Token, Scopes) of
                    false -> 
                      ?LOG_DEBUG(#{description => "Authorization expired or not allowed", 
                                  method => Method}),
                      {{false, <<"Authorization expired or not allowed">>}, Req0, State};

                    {true, Security} ->
                      ?LOG_DEBUG(#{description => "Authorization header is ok", 
                                  method => Method}),
                      {true, Req, State#state{security = Security}}
                  end
              end
          end
      end
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec forbidden(Req :: req(), State :: state()) ->
  {Value :: false, Req :: req(), State :: state()}.
forbidden(Req, #state{origin = _Origin} = State) ->
  {false, Req, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec rate_limited(Req :: req(), State :: state()) ->
  {Value :: false, Req :: req(), State :: state()}.
rate_limited(Req, #state{origin = _Originfg} = State) ->
  {false, Req, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec valid_content_headers(Req :: req(), State :: state()) ->
  {Value :: boolean(), Req :: req(), State :: state()}.
valid_content_headers(Req, #state{origin = Origin} = State) ->
  case cowboy_req:method(Req) of
    <<"OPTIONS">> ->
      ?LOG_DEBUG(#{description => "No validate", 
                  method => <<"OPTIONS">>}),
      {true, Req, State};

    Method -> 
      Cfg = State#state.cfg,
      ?LOG_DEBUG(#{description => "Headers from request", 
                  method => Method, headers => maps:get(headers, Req)}),
      case maps:get(<<"requestBody">>, Cfg, undefined) of
        undefined ->
          H = maps:get(headers, Req),
          Req0 = Req#{headers => maps:remove(<<"accept">>, H)},
          ?LOG_DEBUG(#{description => "No Request Body, No validate", 
                      method => Method}),
          {true, Req0, State};

        RequestBody ->
          case garm_http_request:get_header_value(<<"content-type">>, Req) of
            undefined ->
              ?LOG_DEBUG(#{description => "Content-Type is required", 
                          method => Method, request_body => RequestBody}),
              {false, resp_headers(Req, Origin), State};

            ContentType -> 
              ?LOG_DEBUG(#{description => "Content-Type is present", 
                      method => Method, content_type => ContentType}),
              {true, Req, State}
          end
      end
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec valid_entity_length(Req :: req(), State :: state()) ->
  {Value :: true, Req :: req(), State :: state()}.
valid_entity_length(Req, #state{origin = Origin} = State) ->
  case cowboy_req:method(Req) of
    <<"OPTIONS">> ->
      ?LOG_DEBUG(#{description => "No Entity", 
                  method => <<"OPTIONS">>}),
      {true, Req, State};

    Method ->
      Cfg = State#state.cfg,
      case maps:get(<<"requestBody">>, Cfg, no_request_body) of
        no_request_body ->
          ?LOG_DEBUG(#{description => "No Entity", 
                      method => Method}),
          {true, Req, State};
        
        _RequestBody ->
          case cowboy_req:body_length(Req) of
            A when A>?ENTITY_LENGTH ->
              ?LOG_DEBUG(#{description => "Entity is too long", 
                          sise => A, method => Method}),
              {false, resp_headers(Req, Origin), State};
            _ ->
              ?LOG_DEBUG(#{description => "Entity is ok", 
                          method => Method}),
              {true, Req, State}
          end
      end
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec content_types_provided(Req :: req(), State :: state()) ->
  {Value :: content_types(), Req :: req(), State :: state()}.
content_types_provided(Req, #state{origin = Origin, cfg = Cfg} = State) ->
  Req0 = resp_headers(Req, Origin),
  {ContentTypes, Req1} =
  case maps:get(<<"requestBody">>, Cfg, undefined) of
    undefined ->
      {[{{<<"*">>, <<"*">>, '*'}, process_request}], Req};

    RequestBody ->
      Contents = maps:get(<<"content">>, RequestBody),
      ContentsKeys = maps:keys(Contents),
      F = fun(ContentType) -> 
            {content_type(ContentType), process_request} 
          end,
      {lists:map(F, ContentsKeys), Req0}
  end,
  ?LOG_DEBUG(#{description => "Content types provided", 
            method => cowboy_req:method(Req1), 
            content_types_provided => ContentTypes}),
  {ContentTypes, Req1, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec content_types_accepted(Req :: req(), State :: state()) ->
  {Value :: content_types(), Req :: req(), State :: state()}.
content_types_accepted(Req, #state{origin = Origin, cfg = Cfg} = State) ->
  Req0 = resp_headers(Req, Origin),
  {ContentTypes, Req1} =
  case maps:get(<<"requestBody">>, Cfg, undefined) of
    undefined ->
      {[{{<<"*">>, <<"*">>, '*'}, process_request}], Req};

    RequestBody ->
      Contents = maps:get(<<"content">>, RequestBody),
      ContentsKeys = maps:keys(Contents),
      F = fun(ContentType) -> 
            {content_type(ContentType), process_request} 
          end,
      {lists:map(F, ContentsKeys), Req0}
  end,
  ?LOG_DEBUG(#{description => "Content types accepted", 
            domain => maps:get(ref, Req), 
            operation_id => State#state.operation_id,
            method => cowboy_req:method(Req1), 
            content_types_accepted => ContentTypes}),
  {ContentTypes, Req1, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec delete_resource(Req :: req(), State :: state()) ->
  processed_response().
delete_resource(Req, State) ->
  process_request(Req, State).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec options(Req :: req(), State :: state()) ->
  {Value :: true, Req :: req(), State :: state()}.
options(Req0, #state{origin = Origin, methods = Methods} = State) ->
  Methods0 =
  case lists:droplast(Methods) of
    [] ->
      lists:last(Methods);
    T ->
      L = lists:last(Methods),
      M = lists:foldl(fun(M, Acc)-> [M]++[<<", ">>]++Acc end, [], T),
      list_to_binary(M++L)
  end,

  Req = cowboy_req:set_resp_headers(?ACCESS_CONTROL_HEADERS(Origin, Methods0), Req0),

  ?LOG_DEBUG(#{description => "Options Method was processed", 
            headers => cowboy_req:resp_headers(Req)}),
  {ok, Req, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec process_request(req(), state()) -> processed_response().
process_request(Req, State = #state{operation_id = OperationID,
                                        cfg = MethodCfg,
                                        %valid_body = ValidBody,
                                        adapter = Adapter,
                                        param_values = ParamValues,
                                        security = Security,
                                        %valid_response = ValidResponse,
                                        origin = Origin}) ->
  try 

    DomainKey = maps:get(ref, Req),
    case garm_http_request:get_body_from_req(MethodCfg, ParamValues, Req) of
      {error, Reason} ->
        Req0 = resp_headers(Req, Origin),
        process_response({error, Reason}, Req0, State);

      ParamValues0 ->
        ParamValues1 = ParamValues0#{<<"security">> => Security},
        ?LOG_DEBUG(#{description => "Process operationId", domain => DomainKey,
                    operation_id => OperationID, param_values => ParamValues1}),
        {Code, Headers, Body} = maybe_process(DomainKey, Adapter, OperationID, ParamValues1),
        ?LOG_DEBUG(#{description => "Response",
                    domain => DomainKey, operation_id => OperationID, 
                    code => Code, headers => Headers, body => Body}),
        PreparedBody = prepare_body(Code, Body),
        Response = {ok, {Code, Headers, PreparedBody}},
        Req0 = resp_headers(Req, Origin),
        process_response(Response, Req0, State)
    end

  catch
    _Class:Exception:Stacktrace ->
      ?LOG_ERROR(#{description => "general error in request process", 
                  op_id => OperationID, msg => Exception, 
                  stacktrace => Stacktrace}),
      Req1 = resp_headers(Req, Origin),
      process_response({error, Exception}, Req1, State)
  end.

%% =============================================================================
%% private functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec maybe_process(binary(), module(), binary(), map()) -> tuple().
maybe_process(DomainKey, Adapter, OperationID, Populated) ->
  case call(Adapter, process, DomainKey, OperationID, Populated) of
    no_call ->
      ?LOG_ERROR(#{description => "callback process not found", 
                  adapter => Adapter, callback => process, 
                  args => [DomainKey, OperationID, Populated]}),
      garm_http_response:ko(?SERVICE_UNAVAILABLE_HTTP_CODE);

    {Code, Headers, Body} when is_number(Code) ->
      {Code, Headers, Body};

    {Class, Reason, Stacktrace} ->
      ?LOG_ERROR(#{description => "callback process error", 
                  adapter => Adapter, callback => process, 
                  class => Class, reason => Reason,
                  args => [DomainKey, OperationID, Populated],
                  stacktrace => Stacktrace}),
      garm_http_response:ko(?INTERNAL_GATEWAY_ERROR_HTTP_CODE, Reason)
  end.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec process_response(result_ok() | result_error(), req(), state()) ->
  processed_response().
process_response(Response, Req0, State = #state{operation_id = OperationID}) ->
  case Response of
    {ok, {Code, Headers, Body}} ->
      Req1 = cowboy_req:reply(Code, Headers, Body, Req0),
      {stop, Req1, State};

    {error, Message} ->
      ?LOG_ERROR(#{description => "unable to process request for [OperationID, Message]", 
                op_id => OperationID, msg => Message}),

      Req2 = cowboy_req:reply(400, Req0),
      {stop, Req2, State}
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec prepare_body(integer(), map() | binary()) -> binary().
prepare_body(204, Body) when map_size(Body) == 0; length(Body) == 0 ->
  <<>>;
prepare_body(304, Body) when map_size(Body) == 0; length(Body) == 0 ->
  <<>>;
prepare_body(_Code, Body) when is_map(Body) ->
  thoas:encode(Body);
prepare_body(_Code, Body) when is_binary(Body) ->
  Body;
prepare_body(_Code, _Body) ->
  error(unexpected_response).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec resp_headers(term(), binary()) -> term().
resp_headers(Req, Origin) ->
  cowboy_req:set_resp_headers(#{
		<<"Access-Control-Allow-Origin">> => Origin
	}, Req).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec call(atom(), atom(), binary(), binary(), map()) -> term() | atom() | tuple().
call(Handler, Callback, DomainKey, OperationID, Populated) ->
  case erlang:function_exported(Handler, Callback, 3) of
    true ->
      try Handler:Callback(DomainKey, OperationID, Populated) of
				no_call -> no_call;
        Result -> Result
      catch Class:Reason:Stacktrace ->
        ?LOG_ERROR(#{description => "Handler is not loaded", 
              handler => Handler, error => {Class, Reason},
              stacktrace => Stacktrace}),
        no_call
      end;

    false ->
      no_call
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec content_type(binary()) -> tuple().
content_type(<<"text/plain">>) ->
  {<<"text">>,<<"plain">>, '*'};
content_type(<<"text/html">>) ->
  {<<"text">>,<<"html">>, '*'};
content_type(<<"application/json">>) ->
  {<<"application">>,<<"json">>, '*'}.