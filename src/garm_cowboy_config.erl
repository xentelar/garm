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

-module(garm_cowboy_config).

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

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_api_paths({binary(), binary()}, map()) ->  protocol().
get_api_paths({Path, DomainKey}, DomainCfg) ->
  
  Handler = maps:get(<<"handler">>, DomainCfg, handler_undefined),
  ValidBody = maps:get(<<"validBody">>, DomainCfg, valid_body_undefined),
  ValidResponse = maps:get(<<"validResponse">>, DomainCfg, valid_response_undefined),
  AuthControl = maps:get(<<"authControl">>, DomainCfg, auth_control_undefined),
  Adapter = maps:get(<<"adapter">>, DomainCfg, adapter_undefined),
  
  Paths = build_paths({Path, DomainKey}),
  [
    {'_',
      [build_router_conf(ApiPath, Handler, MethodsCfg, ValidBody, AuthControl, Adapter, ValidResponse) 
        || {ApiPath, MethodsCfg} <- Paths]
    }
  ].

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec build_router_conf(binary(), atom(), map(), atom(), atom(), atom(), atom()) -> tuple().
build_router_conf(ApiPath, Handler, MethodsCfg, ValidBody, AuthControl, Adapter, ValidResponse) ->
  ApiPath0 = binary:replace(ApiPath, <<"}">>, <<"">>, [global]),
  ApiPath1 = binary:replace(ApiPath0, <<"{">>, <<":">>, [global]),
  ?LOG_DEBUG(#{description => "Routing Config", 
              path => ApiPath1, 
              handler => Handler,
              state => {MethodsCfg, ValidBody, AuthControl, Adapter, ValidResponse}}),
  {ApiPath1, Handler, {MethodsCfg, ValidBody, AuthControl, Adapter, ValidResponse}}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
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

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec build_methods_cfg(map(), map()) -> map().
build_methods_cfg(MethodsCfg, ComponentsCgf) ->
  F = fun(Method, Cfg, Acc) ->
        case maps:find(<<"requestBody">>, Cfg) of
          {ok, RequestBodyData} ->
            case maps:find(<<"$ref">>, RequestBodyData) of
              {ok, Ref} ->
                [<<"#">>, 
                  <<"components">>, 
                  <<"requestBodies">>, 
                  RequestBodie] = binary:split(Ref, <<"/">>, [global]),
                
                RequestBodies = maps:get(<<"requestBodies">>, ComponentsCgf, #{}),
                ObjectDef = maps:get(RequestBodie, RequestBodies, #{}),

                Contents = maps:get(<<"content">>, ObjectDef, #{}),

                ?LOG_DEBUG(#{description => "Method Config", 
                      method => Method,
                      object_def => ObjectDef,
                      contents => Contents}),

                Contents0 = find_contents(Contents, ComponentsCgf),
                ?LOG_DEBUG(#{description => "Method Config", 
                      method => Method,
                      cfg => Cfg#{<<"requestBody">> => 
                                    ObjectDef#{<<"content">> => Contents0}
                              }}),
                
                Acc#{Method => Cfg#{<<"requestBody">> => 
                                    ObjectDef#{<<"content">> => Contents0}
                              }};
              error ->
                Acc#{Method => Cfg}
            end;

          error ->
            Acc#{Method => Cfg}
        end
    end,

  maps:fold(F, #{}, MethodsCfg).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec find_contents(map(), map()) -> map().
find_contents(Contents, ComponentsCgf) ->
  F = fun(ContentType, Cfg, Acc) ->
        case ContentType of
          <<"application/json">> ->
            application_json(Acc, Cfg, ComponentsCgf, ContentType);

          <<"text/plain">> ->
            text_plain(Acc, ContentType)
        end
    end,

  maps:fold(F, #{}, Contents).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec application_json(map(), map(), map(), binary()) -> map().
application_json(Acc, Cfg, ComponentsCgf, ContentType) ->
  #{<<"schema">> := 
      #{<<"$ref">> := Ref}
    } = Cfg,

  [<<"#">>, 
    <<"components">>, 
    <<"schemas">>, 
    Object] = binary:split(Ref, <<"/">>, [global]),
      
  Schemas = maps:get(<<"schemas">>, ComponentsCgf, #{}),
  ObjectDef = maps:get(Object, Schemas, #{}),
  Acc#{ContentType => ObjectDef}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec text_plain(map(), binary()) -> map().
text_plain(Acc, ContentType) ->
  ObjectDef = #{<<"type">> => <<"string">>},
  Acc#{ContentType => ObjectDef}.