%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2026 Xentelar Advanced Technologies. All Rights Reserved.
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

-module(garm_rest_handler).

-include_lib("kernel/include/logger.hrl").

-include("http_elements.hrl").

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
  content_type        :: binary(),
  cfg = #{}           :: map(),
  valid_body          :: atom(),
  security_schemas    :: [map()],
  adapter             :: atom(),
  valid_response      :: atom(),
  origin              :: binary(),
  methods             :: [binary()],
  security            :: map(),
  params        			:: map()
}).

-define(URI_LONG, 8192).
-define(ENTITY_LENGTH, 1024*10000).

%% -----------------------------------------------------------------------------
%% public functions
%% -----------------------------------------------------------------------------

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
%% -----------------------------------------------------------------------------
-spec init(Req :: req(), Opts :: garm_cowboy_config:init_opts()) ->
  {cowboy_rest, Req :: req(), State :: state()}.
init(Req, {MethodsCfg, ValidBody, Adapter, ValidResponse}) ->
  Method = cowboy_req:method(Req),
  Method0 = garm_utils:to_lower(Method),
  Origin = cowboy_req:header(<<"origin">>, Req, <<"*">>),
  %?LOG_DEBUG(#{description => "Data Gathering to process the request",
  %  method => Method, request => Req, 
  %  state => {MethodsCfg, ValidBody, Adapter, ValidResponse}}),
  MethodCfg = maps:get(Method0, MethodsCfg, #{}),
  OperationID = maps:get(<<"operationId">>, MethodCfg, not_allowed),
  SecuritySchemas = maps:get(<<"security">>, MethodCfg, []),
  Methods = maps:keys(MethodsCfg) ++ [<<"OPTIONS">>],
  Methods0 = lists:map(fun(M) -> garm_utils:to_upper(M) end, Methods),
  State = #state{
    operation_id = OperationID,
    cfg = MethodCfg,
    valid_body = ValidBody, 
    security_schemas = SecuritySchemas, 
    adapter = Adapter,
    valid_response = ValidResponse,
    origin = Origin,
    methods = Methods0
  },
  %?LOG_DEBUG(#{description => "Processing the request",
  %  operation_id => OperationID, state => State}),
  {cowboy_rest, Req, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec known_methods(Req :: req(), State :: state()) ->
  {Value :: [binary()], Req :: req(), State :: state()}.
known_methods(Req, #state{origin = Origin, methods = Methods} = State) ->
  Method = cowboy_req:method(Req),
  case lists:member(Method, Methods) of
    false ->
      %?LOG_DEBUG(#{description => "Unknown Method", 
      %            method => Method, methods => Methods}),
      {Methods, garm_http_response:resp_headers(Req, Origin), State};
    true ->
      %?LOG_DEBUG(#{description => "Method is ok",
      %            method => Method, methods => Methods}),
      {Methods, Req, State}
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec uri_too_long(Req :: req(), State :: state()) ->
  {Value :: [binary()], Req :: req(), State :: state()}.
uri_too_long(Req, #state{origin = Origin} = State) ->
  case cowboy_req:method(Req) of
    <<"OPTIONS">> ->
      %?LOG_DEBUG(#{description => "No validate", 
      %            method => <<"OPTIONS">>}),
      {false, Req, State};

    _Method ->
      Path = cowboy_req:path(Req),
      case bit_size(Path) of
        A when A>?URI_LONG ->
          %?LOG_DEBUG(#{description => "API Path is too long",
          %            method => Method}),
          {true, garm_http_response:resp_headers(Req, Origin), State};
        _ ->
          %?LOG_DEBUG(#{description => "API Path is ok", 
          %            method => Method}),
          {false, Req, State}
      end
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec allowed_methods(Req :: req(), State :: state()) ->
  {Value :: [binary()], Req :: req(), State :: state()}.
allowed_methods(Req, #state{origin = Origin, methods = Methods} = State) ->
  Method = cowboy_req:method(Req),
  case lists:member(Method, Methods) of
    fasle ->
      %?LOG_DEBUG(#{description => "No allowed Method",
      %            method => Method, methods => Methods}),
      {Methods, garm_http_response:resp_headers(Req, Origin), State};
    true ->
      %?LOG_DEBUG(#{description => "Allowed Method",
      %            method => Method, methods => Methods}),
      {Methods, Req, State}
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec malformed_request(Req :: req(), State :: state()) ->
  {Value :: false, Req :: req(), State :: state()}.
malformed_request(Req, #state{origin = Origin, cfg = MethodCfg} = State) ->
  try 
    Params = garm_http_request:get_params_values(MethodCfg, Req),
    HExt = cowboy_req:headers(Req),
    Params0 = Params#{<<"headers-ext">> => HExt},
    {false, Req, State#state{params = Params0}}
  catch
    _Class:Exception:Stacktrace ->
      ?LOG_DEBUG(#{description => "Parameter value error", 
        exception => Exception, stacktrace => Stacktrace}),
      {true, garm_http_response:resp_headers(Req, Origin), State}
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec is_authorized(Req :: req(), State :: state()) ->
  {Value :: result(), Req :: req(), State :: state()}.
is_authorized(Req, #state{origin = Origin} = State) ->
  case cowboy_req:method(Req) of
    <<"OPTIONS">> ->
      %?LOG_DEBUG(#{description => "No security is needed", 
      %            method => <<"OPTIONS">>}),
      {true, Req, State};
    _Method ->
      Req0 = garm_http_response:resp_headers(Req, Origin),
      case State#state.security_schemas of
        [] ->
          %?LOG_DEBUG(#{description => "Auth Control is undefined", 
          %            method => Method}),
          {true, Req, State};
        SecuritySchemas ->
          case garm_auth:is_authorized(Req, SecuritySchemas) of
            false -> 
              %?LOG_DEBUG(#{description => "Authorization expired or not allowed", 
              %            method => Method}),
              {{false, <<"Authorization expired or not allowed">>}, Req0, State};
            {true, Security} ->
              %?LOG_DEBUG(#{description => "Authorization header is ok", 
              %            method => Method}),
              {true, Req, State#state{security = Security}}
          end
      end
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec forbidden(Req :: req(), State :: state()) ->
  {Value :: false, Req :: req(), State :: state()}.
forbidden(Req, #state{origin = _Origin} = State) ->
  {false, Req, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec rate_limited(Req :: req(), State :: state()) ->
  {Value :: false, Req :: req(), State :: state()}.
rate_limited(Req, #state{origin = _Originfg} = State) ->
  {false, Req, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec valid_content_headers(Req :: req(), State :: state()) ->
  {Value :: boolean(), Req :: req(), State :: state()}.
valid_content_headers(Req, #state{origin = Origin} = State) ->
  case cowboy_req:method(Req) of
    <<"OPTIONS">> ->
      %?LOG_DEBUG(#{description => "No validate", 
      %            method => <<"OPTIONS">>}),
      {true, Req, State};
    _Method -> 
      Cfg = State#state.cfg,
      %?LOG_DEBUG(#{description => "Headers from request", 
      %            method => Method, headers => maps:get(headers, Req)}),
      case maps:get(<<"requestBody">>, Cfg, undefined) of
        undefined ->
          H = maps:get(headers, Req),
          Req0 = Req#{headers => maps:remove(<<"accept">>, H)},
          %?LOG_DEBUG(#{description => "No Request Body, No validate", 
          %            method => Method}),
          {true, Req0, State#state{content_type = undefined}};
        _RequestBody ->
          case garm_http_request:get_header_value(<<"content-type">>, Req) of
            undefined ->
              %?LOG_DEBUG(#{description => "Content-Type is required", 
              %            method => Method, request_body => RequestBody}),
              {false, garm_http_response:resp_headers(Req, Origin), State};

            ContentType -> 
              %?LOG_DEBUG(#{description => "Content-Type is present", 
              %        method => Method, content_type => ContentType}),
              {true, Req, State#state{content_type = ContentType}}
          end
      end
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec valid_entity_length(Req :: req(), State :: state()) ->
  {Value :: true, Req :: req(), State :: state()}.
valid_entity_length(Req, #state{origin = Origin} = State) ->
  case cowboy_req:method(Req) of
    <<"OPTIONS">> ->
      %?LOG_DEBUG(#{description => "No Entity", 
      %            method => <<"OPTIONS">>}),
      {true, Req, State};
    _Method ->
      Cfg = State#state.cfg,
      case maps:get(<<"requestBody">>, Cfg, no_request_body) of
        no_request_body ->
          %?LOG_DEBUG(#{description => "No Entity", 
          %            method => Method}),
          {true, Req, State};
        _RequestBody ->
          case cowboy_req:body_length(Req) of
            A when A>?ENTITY_LENGTH ->
              %?LOG_DEBUG(#{description => "Entity is too long", 
              %            sise => A, method => Method}),
              {false, garm_http_response:resp_headers(Req, Origin), State};
            _ ->
              %?LOG_DEBUG(#{description => "Entity is ok", 
              %            method => Method}),
              {true, Req, State}
          end
      end
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec content_types_provided(Req :: req(), State :: state()) ->
  {Value :: content_types(), Req :: req(), State :: state()}.
content_types_provided(Req, State) ->
  {ContentTypes, Req1} = content_types(Req, State, content_types_provided),
  %?LOG_DEBUG(#{description => "Content types provided", 
  %  domain => maps:get(ref, Req), operation_id => State#state.operation_id,
  %  method => cowboy_req:method(Req1), content_types_provided => ContentTypes}),
  {ContentTypes, Req1, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec content_types_accepted(Req :: req(), State :: state()) ->
  {Value :: content_types(), Req :: req(), State :: state()}.
content_types_accepted(Req, State) ->
  {ContentTypes, Req1} = content_types(Req, State, content_types_accepted),
  %?LOG_DEBUG(#{description => "Content types accepted", 
  %  domain => maps:get(ref, Req), operation_id => State#state.operation_id,
  %  method => cowboy_req:method(Req1), content_types_accepted => ContentTypes}),
  {ContentTypes, Req1, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec delete_resource(Req :: req(), State :: state()) ->
  processed_response().
delete_resource(Req, State) ->
  process_request(Req, State).

%% -----------------------------------------------------------------------------
%% @doc
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

  %?LOG_DEBUG(#{description => "Options Method was processed", 
  %          headers => cowboy_req:resp_headers(Req)}),
  {ok, Req, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec process_request(req(), state()) -> processed_response().
process_request(Req, State = #state{operation_id = OperationID,
                                        cfg = MethodCfg,
                                        valid_body = ValidBody,
                                        content_type = ContentType,
                                        adapter = Adapter,
                                        params = Params,
                                        security = Security,
                                        valid_response = ValidResponse,
                                        origin = Origin}) ->
  Response = process(Req, MethodCfg, Params, Adapter, ValidBody, ValidResponse, ContentType, Security, OperationID),
  Req0 = garm_http_response:resp_headers(Req, Origin),
  reply_response(Response, Req0, State).

%% -----------------------------------------------------------------------------
%% private functions
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec process(req(), map(), map(), atom(), map(), map(), binary(), map(), binary) -> tuple().
process(Req, MethodCfg, Params, Adapter, ValidBody, ValidResponse, ContentType, Security, OperationID) ->
  try
    DomainKey = maps:get(ref, Req),
    case garm_http_request:get_body_from_req(MethodCfg, Params, Req, ValidBody, ContentType) of
      {ok, Params0} ->
        Params1 = Params0#{<<"security">> => Security},
        %?LOG_DEBUG(#{description => "Process operationId", domain => DomainKey,
        %  operation_id => OperationID, params => Params1}),
        case exec_adapter(DomainKey, Adapter, OperationID, Params1) of
          {HttpCode, Headers, BodyRps} ->
            %?LOG_DEBUG(#{description => "Response from adapter",
            %  domain => DomainKey, operation_id => OperationID, rsp_code => Code, 
            %  rsp_headers => Headers, body_response => BodyRps, valid_response => ValidResponse}),
            garm_http_response:prepare_response({HttpCode, Headers, BodyRps}, MethodCfg, ValidResponse);
          {HttpCode, Headers} ->
            %?LOG_DEBUG(#{description => "Response from adapter",
            %  domain => DomainKey, operation_id => OperationID, 
            %  rsp_code => Code, rsp_headers => Headers, body_response => no_response}),
            garm_http_response:prepare_response({HttpCode, Headers, no_response}, MethodCfg, ValidResponse)
      	end;
      {error, Reason} ->
        {error, Reason};
      {error, HttpCode, Reason} ->
        {error, HttpCode, Reason}
    end
  catch
    _Class:Exception:Stacktrace ->
      ?LOG_ERROR(#{description => "HTTP request process general error", 
                  operation_id => OperationID, msg => Exception, 
                  stacktrace => Stacktrace}),
      {error, Exception}
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec exec_adapter(binary(), module(), binary(), map()) -> tuple().
exec_adapter(DomainKey, Adapter, OperationID, Params) ->
  case garm_adapter:process(Adapter, DomainKey, OperationID, Params) of
    {error, Reason} ->
      ?LOG_DEBUG(#{description => "Adapter errors", 
        reason => Reason, adapter => Adapter, domain_key => DomainKey, 
				operation_id => OperationID}),
      garm_http_response:build(?INTERNAL_SERVER_ERROR_HTTP_CODE, #{});
    {HttpCode, Headers} ->
      {HttpCode, Headers};
    {HttpCode, Headers, Body} ->
      {HttpCode, Headers, Body}
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec reply_response(result_ok() | result_error(), req(), state()) ->
  processed_response().
reply_response(Response, Req0, State = #state{operation_id = OperationID}) ->
  case Response of
    {ok, {Code, Headers}} ->
      Req = cowboy_req:reply(Code, Headers, Req0),
      {stop, Req, State};
    {ok, {Code, Headers, Body}} ->
      Req = cowboy_req:reply(Code, Headers, Body, Req0),
      {stop, Req, State};
		{error, content_type_not_found} ->
      ?LOG_DEBUG(#{description => "Unknown content type response", 
        operation_id => OperationID, reason => content_type_not_found}),
      Req = cowboy_req:reply(?BAD_REQUEST_HTTP_CODE, #{}, Req0),
      {stop, Req, State};
		{error, validator_not_found} ->
      ?LOG_DEBUG(#{description => "Response validator not found", 
        operation_id => OperationID, reason => validator_not_found}),
      Req = cowboy_req:reply(?UNSUPPORTED_MEDIA_TYPE_HTTP_CODE, #{}, Req0),
      {stop, Req, State};
    {error, Reason} ->
      ?LOG_DEBUG(#{description => "Unknown error", 
        operation_id => OperationID, reason => Reason}),
      Req = cowboy_req:reply(?INTERNAL_SERVER_ERROR_HTTP_CODE, #{}, Req0),
      {stop, Req, State};
    {error, HttpCode, Reason} ->
      ?LOG_DEBUG(#{description => "Error from adapter", 
        operation_id => OperationID, reason => Reason}),
      Req = cowboy_req:reply(HttpCode, #{}, Req0),
      {stop, Req, State}
  end.

-spec content_types(Req :: req(), State :: state(), Step :: content_types_provided | content_types_accepted) ->
  {Value :: content_types(), Req :: req(), State :: state()}.
content_types(Req, #state{origin = Origin, cfg = Cfg}, Step) ->
  Req0 = garm_http_response:resp_headers(Req, Origin),
  case maps:get(<<"requestBody">>, Cfg, undefined) of
    undefined ->
			case Step of
				content_types_provided -> {[{{<<"*">>, <<"*">>, '*'}, process_request}], Req};
				content_types_accepted -> {[{'*', process_request}], Req}
			end;
    RequestBody ->
      Contents = maps:get(<<"content">>, RequestBody),
      ContentsKeys = maps:keys(Contents),
      F = fun(ContentType) -> 
            {content_type(ContentType), process_request} 
      end,
      {lists:map(F, ContentsKeys), Req0}
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec content_type(binary()) -> tuple().
content_type(ContentType) ->
  case binary:split(ContentType, [<<"/">>], [global]) of
    [CT] -> {CT, '*'};
    [CTl, CTh] -> {CTl, CTh, '*'}
  end.
