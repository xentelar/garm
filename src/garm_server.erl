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

-module(garm_server).

-include_lib("kernel/include/logger.hrl").

%% =============================================================================
%% public functions
%% =============================================================================

-export([start/0]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec start() -> {ok, pid()} | {error, any()}.
start() ->
  start_certs(),
  start_domains().

%% =============================================================================
%% private functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec start_domains() -> {ok, pid()} | {error, any()}.
start_domains() ->
  NetOpts = [],
  IP = {0,0,0,0},

  {Path, Domains} = garm_config:domains(),

  F = fun(DomainKey, DomainCfg) ->
        Port = maps:get(<<"apiPort">>, DomainCfg),
        load_domain_cfg(DomainKey, DomainCfg, Path),
        {Transport, TransportOpts} = socket_transport(IP, Port, NetOpts),
        %ExtraOpts = maps:get(cowboy_extra_opts, Params, []),
        CowboyOpts = api_config({Path, DomainKey}, DomainCfg),
        case Transport of
          ssl ->
            cowboy:start_tls(DomainKey, TransportOpts, CowboyOpts),
            ?LOG_DEBUG(#{description => "Domain was started", 
                      domain => DomainKey, transport => ssl});
          tcp ->
            cowboy:start_clear(DomainKey, TransportOpts, CowboyOpts),
            ?LOG_DEBUG(#{description => "Domain was started", 
                      domain => DomainKey, transport => tcp})
        end
    end,

  maps:foreach(F, Domains).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec start_certs() -> term().
start_certs() ->

  Path = garm_config:config_path(),

  Files = garm_config:list_files(Path, <<".pem">>),

  ?LOG_NOTICE(#{description => "load certs", crets => Files}),

  F = fun(CertPath) ->
    %PivatePath = code:priv_dir(garm),
    %CertPath = list_to_binary([PivatePath, <<"/">>, <<"propelauth-key.pem">>]),
    JWK = jose_jwk:from_pem_file(binary_to_list(CertPath)),
    ?LOG_NOTICE(#{description => "load JWK", jwk => JWK}),
    ets:new(certificates, [public, named_table, {read_concurrency, true}]),
    ets:insert(certificates, {jwk, JWK})
  end,

  lists:foreach(F, Files).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec load_domain_cfg(binary(), map(), binary()) -> term().
load_domain_cfg(DomainKey, DomainCfg, Path) ->
  case maps:get(<<"cfg">>, DomainCfg, cfg_undefined) of
    cfg_undefined ->
      ?LOG_DEBUG(#{description => "Domain cfg not found", 
                domain => DomainKey});

    CfgFileName ->
      garm_config:load_domain_cfg(DomainKey, Path, CfgFileName),
      ?LOG_DEBUG(#{description => "Domain cfg was loaded", 
                domain => DomainKey})
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec api_config({binary(), binary()}, map()) -> map().
api_config({Path, DomainKey}, DomainCfg) ->
  #{env => default_dispatch({Path, DomainKey}, DomainCfg)}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec default_dispatch({binary(), binary()}, map()) -> map().
default_dispatch({Path, DomainKey}, DomainCfg) ->
  ApiPaths = garm_router:get_paths({Path, DomainKey}, DomainCfg),
  #{dispatch => cowboy_router:compile(ApiPaths)}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec socket_transport(inet:ip_address(), inet:port_number(), list()) -> tuple().
socket_transport(IP, Port, NetOpts) ->
  Opts = [
    {ip,   IP},
    {port, Port}
  ],
  case garm_utils:get_opt(ssl, NetOpts) of
    SslOpts = [_ | _] ->
      {ssl, Opts ++ SslOpts};

    undefined ->
      {tcp, Opts}
  end.