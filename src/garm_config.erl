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

-module(garm_config).

-include_lib("kernel/include/logger.hrl").

-define(APP, garm).

%% =============================================================================
%% public functions
%% =============================================================================

-export([domains/0]).
-export([domain_api/2]).
-export([load_domain_cfg/3]).
-export([find/1]).
-export([config_path/0]).
-export([list_files/2]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec domains() -> tuple().
domains() ->
	CfgFile = cfg_file(),

	case filelib:is_file(CfgFile) of
		true ->
			Path = filename:dirname(CfgFile),
			[Config] = yamerl_constr:file(CfgFile),

			?LOG_DEBUG(#{description => "domains conf", 
									domains => Config}),

			case lists:keyfind("domains", 1, Config) of
				{_, Domains} ->
					Domains0 = create_map(Domains),

					?LOG_DEBUG(#{description => "Domains config was loaded",
											domains_cfg => Domains0}),

					{Path, Domains0};

				false ->
					error(config_domains_not_found)
			end;

		false ->
			error(config_file_not_found)
	end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec config_path() -> tuple().
config_path() ->
	CfgFile = cfg_file(),

	case filelib:is_file(CfgFile) of
		true ->
			Path = filename:dirname(CfgFile),

			?LOG_NOTICE(#{description => "path config", 
									path => Path}),
			Path;

		false ->
			error(config_file_not_found)
	end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec domain_api(binary(), binary()) -> list().
domain_api(Path, FileName) ->
	FileName0 = list_to_binary([Path, <<"/">>, FileName, <<".yaml">>]),
	case filelib:is_file(FileName0) of
		true ->
			[Config] = yamerl_constr:file(FileName0),

			?LOG_DEBUG(#{description => "full api conf", 
									full_api_def => Config}),
			
			PathsCfg =
			case lists:keyfind("paths", 1, Config) of
				{_, Paths} ->
					Paths0 = create_map(Paths),
					?LOG_DEBUG(#{description => "API paths config was loaded",
											paths_cfg => Paths0}),
					%G = fun(K, V) ->
					%			set(binary_to_atom(K), V)
					%end,
					%maps:foreach(G, Paths0);
					Paths0;

				false ->
					?LOG_ERROR(#{description => "Not found 'paths' in file",
											file => FileName0}),
					error(paths_config_not_found)
			end,

			ComponentsCgf =
			case lists:keyfind("components", 1, Config) of
				{_, Components} ->
					Components0 = create_map(Components),
					?LOG_DEBUG(#{description => "API components config was loaded",
											paths_cfg => Components0}),
					%H = fun(K, V) ->
					%			set(binary_to_atom(K), V)
					%end,
					%maps:foreach(H, Components0);
					Components0;

				false ->
					?LOG_ERROR(#{description => "Not found 'components' in file",
												file => FileName0}),
					error(components_config_not_found)
			end,
			{PathsCfg, ComponentsCgf};

		false ->
			?LOG_ERROR(#{description => "Domain file not found",
									file => FileName0}),
			error(domain_file_not_found)
	end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec load_domain_cfg(binary(), binary(), binary()) -> map().
load_domain_cfg(DomainKey, Path, FileName) ->
	FileName0 = list_to_binary([Path, <<"/">>, FileName]),

	case filelib:is_file(FileName0) of
		true ->
			[Config] = yamerl_constr:file(FileName0),

			?LOG_DEBUG(#{description => "Domain conf file", 
									file => FileName0,
									cfg => Config}),

			case lists:keyfind("operations", 1, Config) of
				{_, OperationsCfg} ->
					OperationsCfg0 = create_map(OperationsCfg),

					?LOG_DEBUG(#{description => "Operations from domain config was loaded",
											file => FileName0,
											operations => OperationsCfg0}),

					set(DomainKey, OperationsCfg0),
					OperationsCfg0;

				false ->
					error(config_domains_not_found)
			end;

		false ->
			error(config_file_not_found)
	end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec find(Key :: atom()) -> undefined | {ok, term()}.
find(Key) ->
  application:get_env(?APP, Key).

%% -----------------------------------------------------------------------------
%% @doc list required files on path
%% @end
%% @private
%% -----------------------------------------------------------------------------
-spec list_files(file:name_all(), string()) -> [binary()].
list_files(Path, ExtentionFile) ->
	case filelib:is_dir(Path) of
		true ->
			{ok, Files} = file:list_dir(Path),
			filter(Path, Files, ExtentionFile);

		false ->
			error(path_is_not_dir)
	end.

%% =============================================================================
%% private functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec set(atom() | tuple(), term()) -> term().
set(KeyCfg, Cfg) ->
	application:set_env(?APP, KeyCfg, Cfg, [{persistent, false}]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec create_map(list()) -> map().
create_map(Config) ->
	?LOG_DEBUG(#{description => "trasnform tuple to map",
							tuple => Config}),
	create_map(Config, #{}).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec create_map(list(), map()) -> map().
create_map([], Acc) ->
	Acc;

create_map([{"parameters", Values} | T], Acc) ->
	Key0 = <<"parameters">>,
	F = fun(Value) ->
				create_map(Value, #{})
		end,
	Acc0 = Acc#{Key0 => lists:map(F, Values)},
	create_map(T, Acc0);

 create_map([{"handler", Value} | T], Acc) ->
 	Key0 = <<"handler">>,
 	Acc0 = Acc#{Key0 => list_to_atom(Value)},
 	create_map(T, Acc0);

create_map([{"validBody", Value} | T], Acc) ->
	Key0 = <<"validBody">>,
	Acc0 = Acc#{Key0 => list_to_atom(Value)},
	create_map(T, Acc0);

create_map([{"authControl", Value} | T], Acc) ->
	Key0 = <<"authControl">>,
	Acc0 = Acc#{Key0 => list_to_atom(Value)},
	create_map(T, Acc0);

create_map([{"adapter", Value} | T], Acc) ->
	Key0 = <<"adapter">>,
	Acc0 = Acc#{Key0 => list_to_atom(Value)},
	create_map(T, Acc0);

create_map([{"validResponse", Value} | T], Acc) ->
	Key0 = <<"validResponse">>,
	Acc0 = Acc#{Key0 => list_to_atom(Value)},
	create_map(T, Acc0);

create_map([[{Key, Val}] | Tail], Acc) ->
	Key0 = list_to_binary(Key),
	?LOG_DEBUG(#{description => "tuple to map",
								key => Key, val => Val}),
  create_map(Tail, Acc#{Key0 => create_map(Val, #{})});

create_map([{Key, Val} | Tail], Acc) ->
	Key0 = list_to_binary(Key),
	?LOG_DEBUG(#{description => "tuple to map",
								key => Key, val => Val}),
  create_map(Tail, Acc#{Key0 => create_map(Val, #{})});
	
create_map(Val, _Acc) when is_atom(Val) ->
	Val;

create_map(Val, _Acc) when is_integer(Val) ->
	Val;

create_map(Val, _Acc) when is_binary(Val) ->
	Val;

create_map(Val, _Acc) ->
	?LOG_DEBUG(#{description => "Val to binary",
							val => Val}),
	case io_lib:printable_list(Val) of
		false ->
			Val;
		true ->
			list_to_binary(Val)
	end.

%% -----------------------------------------------------------------------------
%% @doc filter required files from other files
%% @end
%% @private
%% -----------------------------------------------------------------------------
-spec filter(file:name_all(), [string()], string()) -> [binary()].
filter(Path, Files, ExtentionFile) ->
  F = fun(V, Aux) ->
         case string:find(V, ExtentionFile, leading) of
           nomatch ->
             Aux;

           _ ->
             File = list_to_binary([Path, <<"/">>, V]),
						 ?LOG_INFO(#{description => "file was found", 
						 						file => File}),
             Aux0 = Aux ++ [File],
             Aux0
         end
      end,
  lists:foldl(F, [], Files).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec cfg_file() -> term().
cfg_file() ->
	{ok, Value} = application:get_env(?APP, cfg_file),
	Value.