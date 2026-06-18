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

-module(garm_cowboy_config).

-moduledoc """
""".

-include_lib("kernel/include/logger.hrl").

-type operations() :: #{binary() => map()}.
-type init_opts()  :: {operations(), atom(), module()}.

-type protocol() :: [{'_',[{
                            ApiPath :: string(),
                            Handler :: atom(),
                            InitOpts :: init_opts()
                          }]}].

-export_type([init_opts/0]).

%% =============================================================================
%% public functions
%% =============================================================================

-export([get_api_paths/2]).

-doc """
""".
-spec get_api_paths({binary(), binary()}, map()) ->  protocol().
get_api_paths({Path, DomainKey}, DomainCfg) ->
  
  Handler = maps:get(<<"handler">>, DomainCfg, handler_undefined),
  ValidBody = maps:get(<<"validBody">>, DomainCfg, valid_body_undefined),
  ValidResponse = maps:get(<<"validResponse">>, DomainCfg, valid_rps_undefined),
  AuthControl = maps:get(<<"authControl">>, DomainCfg, auth_ctrl_undefined),
  Adapter = maps:get(<<"adapter">>, DomainCfg, adapter_undefined),
  
  Paths = build_paths({Path, DomainKey}),
  [
    {'_',
      [build_router_conf(ApiPath, Handler, MethodsCfg, ValidBody, AuthControl, Adapter, ValidResponse) 
        || {ApiPath, MethodsCfg} <- Paths]
    }
  ].

-doc """
""".
-spec build_router_conf(binary(), atom(), map(), atom(), atom(), atom(), atom()) -> tuple().
build_router_conf(ApiPath, Handler, MethodsCfg, ValidBody, AuthControl, Adapter, ValidResponse) ->
  ApiPath0 = binary:replace(ApiPath, <<"}">>, <<"">>, [global]),
  ApiPath1 = binary:replace(ApiPath0, <<"{">>, <<":">>, [global]),
  ?LOG_DEBUG(#{description => "Routing Config", 
              path => ApiPath1, 
              handler => Handler,
              state => {MethodsCfg, ValidBody, AuthControl, Adapter, ValidResponse}}),
  {ApiPath1, Handler, {MethodsCfg, ValidBody, AuthControl, Adapter, ValidResponse}}.

-doc """
""".
-spec build_paths(binary()) -> list().
build_paths({Path, DomainKey}) ->
  {ApiPathsCfg, ComponentsCgf} = garm_config:domain_api(Path, DomainKey),

  F = fun(ApiPath, MethodsCfg, Acc) ->
        MethodsCfg0 = build_methods_cfg(MethodsCfg, ComponentsCgf),
        ?LOG_DEBUG(#{description => "API Path Config", 
                    api_path => ApiPath, cfg => MethodsCfg0}),
        [{ApiPath, MethodsCfg0} | Acc]
      end,

  maps:fold(F, [], ApiPathsCfg).

-doc """
""".
-spec build_methods_cfg(map(), map()) -> map().
build_methods_cfg(MethodsCfg, ComponentsCgf) ->
  F = fun(Method, Cfg, Acc) ->
        case maps:get(~"requestBody", Cfg, undefined) of
          undefined ->
            Acc#{Method => Cfg};

          RequestBody ->
            RequestBody0 = find_content_types(RequestBody, ComponentsCgf),
            ContentTypes = maps:get(~"content", RequestBody0),
            
            RequestBody1 = 
            case maps:get(~"application/json", ContentTypes, undefined) of
              undefined ->
                RequestBody0;
              Schema ->
                C = #{~"components" => ComponentsCgf},
                JesseState = jesse_state:new(C, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]),
                CTs = ContentTypes#{~"application/json" => {Schema, JesseState}},
                RequestBody0#{~"content" => CTs}
            end,
            Cfg0 = Cfg#{~"requestBody" => RequestBody1},
            ?LOG_DEBUG(#{description => "Method Config", 
                  method => Method,
                  config => Cfg0}),
            Acc#{Method => Cfg0}
        end
    end,

  maps:fold(F, #{}, MethodsCfg).

-doc """
""".
-spec find_content_types(map(), map()) -> map().
find_content_types(RequestBody, ComponentsCgf) ->
  case maps:get(~"$ref", RequestBody, undefined) of
    undefined ->
      case maps:get(~"content", RequestBody, undefined) of
        undefined ->
          error(no_content_or_ref);
        _ContentTypes ->
          RequestBody
      end;
    Ref ->
      [<<"#">>, 
      <<"components">>, 
      <<"requestBodies">>, 
      RequestBodySchema] = binary:split(Ref, <<"/">>, [global]),
      RequestBodySchemas = maps:get(<<"requestBodies">>, ComponentsCgf, #{}),
      SchemaDef = maps:get(RequestBodySchema, RequestBodySchemas, #{}),
      case maps:get(~"content", SchemaDef, undefined) of
        undefined ->
          error(no_content_or_ref);
        _ContentTypes ->
          SchemaDef
      end
  end.