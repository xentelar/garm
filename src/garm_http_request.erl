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

-module(garm_http_request).

-moduledoc """
""".

-include_lib("kernel/include/logger.hrl").

-include("http_commons.hrl").

-define(DEFAULT_VALUES, #{~"headers" => #{},
                        ~"headers-ext" => #{}, 
                        ~"bindings" => #{}, 
                        ~"query-values" => #{}}).

%% =============================================================================
%% public functions
%% =============================================================================

-export([get_body_from_req/5]).
-export([get_header_value/2]).
-export([get_params_values/2]).

-doc """
""".
-spec get_header_value(any(), cowboy_req:req()) ->
  {any(), cowboy_req:req()} | {error, any(), cowboy_req:req()}.
get_header_value(Name, Req) ->
  Headers = cowboy_req:headers(Req),
  Name0 = to_header(Name),
  maps:get(Name0, Headers, undefined).

-doc """
""".
-spec get_body_from_req(map(), map(), cowboy_req:req(), map(), binary()) -> map().
get_body_from_req(MethodCfg, ParamValues, Req, ValidBody, ContentType) ->
  % ?LOG_DEBUG(#{description => "Process operationId", method_cfg => MethodCfg, 
  %   req_params => ParamValues, reques => Req, body_val => ValidBody, 
  %   content_type => ContentType}),
  case maps:get(~"requestBody", MethodCfg, undefined) of
    undefined ->
      {ok, ParamValues#{~"body" => #{}}};
    
    _RequestBody ->
      case apply_content_type(MethodCfg, Req, ContentType, ValidBody) of
        {ok, Body} -> {ok, ParamValues#{~"body" => Body}};
        Error -> Error
      end
  end.

-doc """
""".
-spec get_params_values(map(), cowboy_req:req()) -> map().
get_params_values(MethodCfg, Req) ->
  case maps:get(~"parameters", MethodCfg, no_params) of
    no_params ->
      ?DEFAULT_VALUES;

    ParamsCfg -> 
      F = fun(ParamCfg, Values) ->
            ParamName = maps:get(~"name", ParamCfg),
            case maps:get(~"in", ParamCfg) of
              ~"header" ->
                get_header_param(ParamName, Values, ParamCfg, Req);

              ~"path" ->
                get_binding_param(ParamName, Values, ParamCfg, Req);

              ~"query" ->
                get_query_param(ParamName, Values, ParamCfg, Req)
            end
      end,
      lists:foldl(F, ?DEFAULT_VALUES, ParamsCfg)
  end.

%% =============================================================================
%% private functions
%% =============================================================================

-doc """
""".
-spec get_header_param(binary(), map(), map(), term()) -> map().
get_header_param(ParamName, Values, ParamCfg, Req) ->
  Hs = maps:get(~"headers", Values),
  Value = get_header_value(ParamName, Req),
  Headers = Hs#{ParamName => get_value(ParamCfg, Value)},
  Values#{~"headers" => Headers}.

-doc """
""".
-spec get_binding_param(binary(), map(), map(), term()) -> map().
get_binding_param(ParamName, Values, ParamCfg, Req) ->
  Bs = maps:get(~"bindings", Values),
  Value = get_binding_value(ParamName, Req),
  Bindings = Bs#{ParamName => get_value(ParamCfg, Value)},
  Values#{~"bindings" => Bindings}.

-doc """
""".
-spec get_query_param(binary(), map(), map(), term()) -> map().
get_query_param(QueryName, Values, ParamCfg, Req) ->
  QVs = maps:get(~"query-values", Values),
  Value = get_query_value(QueryName, Req),
  QueryValues = QVs#{QueryName => get_value(ParamCfg, Value)},
  Values#{~"query-values" => QueryValues}.

-doc """
""".
-spec get_body(cowboy_req:req()) ->
  {any(), cowboy_req:req()} | {error, any(), cowboy_req:req()}.
get_body(Req0) ->
  case cowboy_req:read_body(Req0) of
    {ok, Body, _Req} ->
      Body;
    {more, Body, _Req} ->
      Body
  end.

-doc """
""".
-spec get_query_value(any(), cowboy_req:req()) ->
  {any(), cowboy_req:req()} | {error, any(), cowboy_req:req()}.
get_query_value(Name, Req) ->
  QS = cowboy_req:parse_qs(Req),
  Value = garm_utils:get_opt(to_qs(Name), QS),
  {Value, Req}.

-doc """
""".
-spec get_binding_value(any(), cowboy_req:req()) ->
  {any(), cowboy_req:req()} | {error, any(), cowboy_req:req()}.
get_binding_value(Name, Req) ->
  Name0 = to_binding(Name),
  cowboy_req:binding(Name0, Req, undefined).

-doc """
""".
-spec get_value(map(), binary()) -> number() | binary() | atom().
get_value(ParamCfg, Value) ->
  #{
    ~"schema" := #{
      ~"type" := Type
    }
  } = ParamCfg,

  case Type of
    ~"number" -> binary_to_integer(Value);
    ~"integer" -> binary_to_integer(Value);
    ~"string" -> Value;
    ~"boolean" -> garm_utils:to_boolean(Value);
    ~"array" -> Value
  end.

-spec apply_content_type(map(), cowboy_req:req(), binary(), map()) -> 
    binary() | map() | {error, pos_integer(), term()}.
apply_content_type(MethodCfg, Req, ContentType, ValidBody) ->
  Body = get_body(Req),
  case maps:get(ContentType, ValidBody, undefined) of
    undefined -> 
      {error, ~"validator module not found"};
    Validator -> 
      case maps:get(~"requestBody", MethodCfg, undefined) of
        undefined ->
          {error, no_request_body};
        RequestBody ->
          case maps:get(~"content", RequestBody, undefined) of
            undefined ->
              {error, no_request_body_content};
            ContentTypes ->
              case maps:get(ContentType, ContentTypes, undefined) of
                undefined ->
                  {error, request_body_config};
                Schema ->
                  Required = maps:get(~"required", RequestBody, false),
                  case garm_validator:validate(Validator, Body, Schema, Required) of
                    {ok, BodyJson} ->
                      {ok, BodyJson};
                    {error, Reason} ->
                      {error, ?BAD_REQUEST_HTTP_CODE, Reason}
                  end
              end
          end
      end
  end.

-doc """
""".
-spec to_header(iodata() | atom() | number()) -> binary().
to_header(Name) ->
  Prepared = garm_utils:to_binary(Name),
  garm_utils:to_lower(Prepared).

-doc """
""".
-spec to_qs(iodata() | atom() | number()) -> binary().
to_qs(Name) ->
  garm_utils:to_binary(Name).

-doc """
""".
-spec to_binding(iodata() | atom() | number()) -> atom().
to_binding(Name) ->
  Prepared = garm_utils:to_binary(Name),
  binary_to_atom(Prepared, utf8).
