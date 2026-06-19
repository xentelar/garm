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

-module(garm_server).

-moduledoc """
""".

-include_lib("kernel/include/logger.hrl").

%% =============================================================================
%% public functions
%% =============================================================================

-export([init/0]).

-doc """
""".
-spec init() -> ok.
init() ->
	try

	  init_cfg(),
    init_domains(),
    ?LOG_NOTICE(#{description => "Config was loaded"})

	catch
		error:Exception:Stacktrace ->
			?LOG_ERROR(#{description => "Config have problems",
									exception => Exception,
									stacktrace => Stacktrace})
	end,
  ok.

%% =============================================================================
%% private functions
%% =============================================================================

-doc """
""".
-spec init_domains() -> ok.
init_domains() ->
  NetOpts = [],
  IP = {0,0,0,0},

  {Path, Domains} = garm_config:domains(),

  F = fun(DomainKey, DomainCfg) ->
        Port = maps:get(<<"apiPort">>, DomainCfg),
        OperationsCfg = load_domain_cfg(DomainKey, DomainCfg, Path),
        start_adapter(DomainKey, DomainCfg, OperationsCfg),
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

-doc """
""".
-spec init_cfg() -> term().
init_cfg() ->

  Path = garm_config:config_path(),

  Files = garm_config:list_files(Path, <<".pem">>),

  ?LOG_NOTICE(#{description => "Load certs", crets => Files}),
  
  ets:new(certificates, [public, named_table, {read_concurrency, true}]),

  F = fun(CertPath) ->
    JWK = jose_jwk:from_pem_file(binary_to_list(CertPath)),
    ?LOG_NOTICE(#{description => "Load JWK", jwk => JWK}),
    ets:insert(certificates, {jwk, JWK})
  end,

  lists:foreach(F, Files).

-doc """
""".
-spec load_domain_cfg(binary(), map(), binary()) -> term().
load_domain_cfg(DomainKey, DomainCfg, Path) ->
  case maps:get(<<"cfg">>, DomainCfg, cfg_undefined) of
    cfg_undefined ->
      ?LOG_DEBUG(#{description => "Domain cfg not found", 
                domain => DomainKey}),
      #{};

    CfgFileName ->
      OperationsCfg = garm_config:load_domain_cfg(DomainKey, Path, CfgFileName),
      ?LOG_DEBUG(#{description => "Domain cfg was loaded", 
                domain => DomainKey, operations_cfg => OperationsCfg}),
      OperationsCfg
  end.

-doc """
""".
-spec api_config({binary(), binary()}, map()) -> map().
api_config({Path, DomainKey}, DomainCfg) ->
  #{env => default_dispatch({Path, DomainKey}, DomainCfg)}.

-doc """
""".
-spec default_dispatch({binary(), binary()}, map()) -> map().
default_dispatch({Path, DomainKey}, DomainCfg) ->
  ApiPaths = garm_cowboy_config:get_api_paths({Path, DomainKey}, DomainCfg),
  #{dispatch => cowboy_router:compile(ApiPaths)}.

-doc """
""".
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

-doc """
""".
-spec start_adapter(binary(), map(), map()) -> term().
start_adapter(DomainKey, DomainCfg, OperationsCfg) ->
  case maps:get(<<"adapter">>, DomainCfg, adapter_undefined) of
    adapter_undefined ->
      ?LOG_DEBUG(#{description => "Adapter not found", 
                    domain_key => DomainKey});
    Adapter ->
      try 
        case garm_adapter:init(Adapter, DomainKey, OperationsCfg) of
          {error, Reason} ->
            ?LOG_ERROR(#{description => "Init adapter error",
              reason => Reason, adapter => Adapter, callback => start, 
              args => [DomainKey, OperationsCfg], domain_key => DomainKey});

          _ ->
            ?LOG_DEBUG(#{description => "Init adapter was executed", 
              adapter => Adapter, callback => start, 
              args => [DomainKey, OperationsCfg], domain_key => DomainKey})
        end
      catch
        _Class:Exception:Stacktrace ->
          ?LOG_ERROR(#{description => "General error in init adapter", 
            reason => Exception, adapter => Adapter, callback => start,
            args => [DomainKey, OperationsCfg], domain_key => DomainKey, 
            stacktrace => Stacktrace})
      end
  end.