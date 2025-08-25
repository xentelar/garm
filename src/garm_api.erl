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

-module(garm_api).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT, #{<<"headers">> => #{}, 
                    <<"bindings">> => #{}, 
                    <<"query-values">> => #{}}).

%% =============================================================================
%% public functions
%% =============================================================================

-export([populate_from_req/3]).
-export([get_header_value/2]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec populate_from_req(map(), cowboy_req:req(), atom()) -> map().
populate_from_req(MethodCfg, Req, ValidBody) ->
  Params = get_params_from_req(MethodCfg, Req),

  case get_body_from_req(MethodCfg, Req) of
    {error, Reason} ->
      {error, Reason};

    Body0 ->
      {ok, maps:merge(Params, Body0)}
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_header_value(any(), cowboy_req:req()) ->
  {any(), cowboy_req:req()} | {error, any(), cowboy_req:req()}.
get_header_value(Name, Req) ->
  Headers = cowboy_req:headers(Req),
  Name0 = garm_utils:to_header(Name),
  maps:get(Name0, Headers, undefined).

%% =============================================================================
%% private functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_params_from_req(map(), cowboy_req:req()) -> map().
get_params_from_req(MethodCfg, Req) ->
  %Headers = cowboy_req:headers(Req),
  case maps:get(<<"parameters">>, MethodCfg, no_params) of
    no_params ->
      ?DEFAULT;

    Params0 -> 
      F = fun(Param, Acc) ->
            ParamName = maps:get(<<"name">>, Param),
            case maps:get(<<"in">>, Param) of
              <<"header">> ->
                get_heder_param(ParamName, Acc, Req);

              <<"path">> ->
                get_binding_param(ParamName, Acc, Req);

              <<"query">> ->
                get_query_param(ParamName, Acc, Req)
            end
      end,
      lists:foldl(F, ?DEFAULT, Params0)
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_body_from_req(map(), cowboy_req:req()) -> map().
get_body_from_req(MethodCfg, Req) ->
  case maps:get(<<"requestBody">>, MethodCfg, no_request_body) of
    no_request_body ->
      #{<<"body">> => #{}};
    
    _RequestBody ->
      get_body_content(Req)
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_heder_param(binary(), map(), term()) -> map().
get_heder_param(ParamName, Params, Req) ->
  Hs = maps:get(<<"headers">>, Params),
  Headers = Hs#{ParamName => get_header_value(ParamName, Req)},
  Params#{<<"headers">> => Headers}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_binding_param(binary(), map(), term()) -> map().
get_binding_param(ParamName, Params, Req) ->
  Bs = maps:get(<<"bindings">>, Params),
  Bindings = Bs#{ParamName => get_binding_value(ParamName, Req)},
  Params#{<<"bindings">> => Bindings}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_query_param(binary(), map(), term()) -> map().
get_query_param(QueryName, Params, Req) ->
  QVs = maps:get(<<"query-values">>, Params),
  QueryValues = QVs#{QueryName => get_query_value(QueryName, Req)},
  Params#{<<"query-values">> => QueryValues}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_body_content(term()) -> map().
get_body_content(Req) ->
  case get_body(Req) of
    {error, Reason} ->
      {error, Reason};

    Body0 ->
      #{<<"body">> => Body0}
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_body(cowboy_req:req()) ->
  {any(), cowboy_req:req()} | {error, any(), cowboy_req:req()}.
get_body(Req0) ->
  {ok, Body, _Req} = cowboy_req:read_body(Req0),
  case prepare_body(Body) of
    {error, Reason} ->
      ?LOG_ERROR(#{description => "The body could not be processed",
                  reason => Reason}),
      {error, Reason};

    Value ->
      ?LOG_DEBUG(#{description => "Json result",
                  json => Value}),
      Value
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_query_value(any(), cowboy_req:req()) ->
  {any(), cowboy_req:req()} | {error, any(), cowboy_req:req()}.
get_query_value(Name, Req) ->
  QS = cowboy_req:parse_qs(Req),
  Value = garm_utils:get_opt(garm_utils:to_qs(Name), QS),
  {Value, Req}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_binding_value(any(), cowboy_req:req()) ->
  {any(), cowboy_req:req()} | {error, any(), cowboy_req:req()}.
get_binding_value(Name, Req) ->
  Name0 = garm_utils:to_binding(Name),
  cowboy_req:binding(Name0, Req, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
prepare_body(Body) ->
  ?LOG_DEBUG(#{description => "Body to process",
              body => Body}),
  case Body of
    <<"">> -> #{};
    _ ->
      try
        {ok, JsonMap} = thoas:decode(Body),
        JsonMap
      catch
        error:_ ->
        {error, {invalid_body, not_json, Body}}
      end
  end.

