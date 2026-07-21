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

-module(garm_config).

-moduledoc """
""".

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

-doc """
""".
-spec domains() -> tuple().
domains() ->
	CfgFile = cfg_file(),
	case filelib:is_file(CfgFile) of
		true ->
			Path = filename:dirname(CfgFile),
			[Config] = yamerl_constr:file(CfgFile, [str_node_as_binary]),
			%?LOG_INFO(#{description => "Domains conf", 
			%	domains => Config}),
			case lists:keyfind(~"domains", 1, Config) of
				{_, Domains} ->
					Domains0 = create_map(Domains),
					?LOG_INFO(#{description => "Domains config was loaded", 
						cfg_file => CfgFile, domains_cfg => Domains0}),
					{Path, Domains0};
				
				false ->
					error(config_domains_not_found)
			end;

		false ->
			error(config_file_not_found)
	end.

-doc """
""".
-spec config_path() -> tuple().
config_path() ->
	CfgFile = cfg_file(),
	case filelib:is_file(CfgFile) of
		true ->
			Path = filename:dirname(CfgFile),
			?LOG_NOTICE(#{description => "Path config", 
				path => Path}),
			Path;

		false ->
			error(config_file_not_found)
	end.

-doc """
""".
-spec domain_api(binary(), binary()) -> list().
domain_api(FilPath, FileName) ->
	FileName0 = list_to_binary([FilPath, ~"/", FileName, ~".yaml"]),
	case filelib:is_file(FileName0) of
		true ->
			[ApiDef] = yamerl_constr:file(FileName0, [{map_node_format, map}, str_node_as_binary]),

			?LOG_DEBUG(#{description => "Open API definition", 
				file => FileName0, api_def => ApiDef}),
			
			ApiPaths =
			case maps:get(~"paths", ApiDef, undefined) of
				undefined ->
					?LOG_ERROR(#{description => "API Paths definition not found",
						file => FileName0}),
					error(paths_config_not_found);

				Paths ->
					?LOG_DEBUG(#{description => "API Paths definition was loaded",
						file => FileName0, paths_def => Paths}),
					Paths
			end,

			ComponentsCfg =
			case maps:get(~"components", ApiDef, undefined) of
				undefined ->
					?LOG_WARNING(#{description => "API Components definition not found",
						file => FileName0}),
					#{};

				Components ->
					?LOG_DEBUG(#{description => "API Components definition was loaded",
						file => FileName0, components_cfg => Components}),
					Components
			end,
			{ApiPaths, ComponentsCfg};

		false ->
			?LOG_ERROR(#{description => "Domain file not found",
									file => FileName0}),
			error(domain_file_not_found)
	end.

-doc """
""".
-spec load_domain_cfg(binary(), binary(), binary()) -> map().
load_domain_cfg(DomainKey, FilPath, FileName) ->
	FileName0 = list_to_binary([FilPath, ~"/", FileName]),

	case filelib:is_file(FileName0) of
		true ->
			[Config] = yamerl_constr:file(FileName0, [{map_node_format, map}, str_node_as_binary]),

			?LOG_DEBUG(#{description => "Domain config file", 
				domain_key => DomainKey, file => FileName0, cfg => Config}),

			case maps:get(~"operations", Config, undefined) of
				undefined ->
					?LOG_DEBUG(#{description => "Operations not found", 
						domain_key => DomainKey, file => FileName0}),
					#{};

				OperationsCfg ->
					?LOG_DEBUG(#{description => "Operations config was loaded",
						domain_key => DomainKey, file => FileName0, operations => OperationsCfg}),
					set(DomainKey, OperationsCfg),
					OperationsCfg
			end;

		false ->
			error(config_file_not_found)
	end.

-doc """
""".
-spec find(Key :: atom()) -> undefined | {ok, term()}.
find(Key) ->
  application:get_env(?APP, Key).

-doc """
This function list the required files on path
""".
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

-doc """
""".
-spec set(atom() | tuple(), term()) -> term().
set(KeyCfg, Cfg) ->
	application:set_env(?APP, KeyCfg, Cfg, [{persistent, false}]).

-doc """
""".
-spec create_map(list()) -> map().
create_map(Config) ->
	?LOG_DEBUG(#{description => "Trasnform tuple to map",
							tuple => Config}),
	create_map(Config, #{}).

-doc """
""".
-spec create_map(list(), map()) -> map().
create_map([], Acc) ->
	Acc;

create_map([{Key0 = ~"apiPort", Value} | T], Acc) ->
	Value0 = get_int_from_env(Value),
 	Acc0 = Acc#{Key0 => Value0},
 	create_map(T, Acc0);

create_map([{Key0 = ~"handler", Value} | T], Acc) ->
 	Acc0 = Acc#{Key0 => find_module(Value)},
 	create_map(T, Acc0);

create_map([{Key0 = ~"validBody", Values} | T], Acc) ->
 	F = fun({Key, Val}, Aux) ->
				Validator = find_module(Val),
 				maps:put(Key, Validator, Aux)
 		end,
 	Acc0 = Acc#{Key0 => lists:foldl(F, #{}, Values)},
	create_map(T, Acc0);

create_map([{Key0 = ~"authControl", Value} | T], Acc) ->
	Acc0 = Acc#{Key0 => find_module(Value)},
	create_map(T, Acc0);

create_map([{Key0 = ~"adapter", Value} | T], Acc) ->
	Acc0 = Acc#{Key0 => find_module(Value)},
	create_map(T, Acc0);

create_map([{Key0 = ~"validResponse", Values} | T], Acc) ->
 	F = fun({Key, Val}, Aux) ->
				Validator = find_module(Val),
 				maps:put(Key, Validator, Aux)
 		end,
 	Acc0 = Acc#{Key0 => lists:foldl(F, #{}, Values)},
	create_map(T, Acc0);

create_map([{Key, Val} | Tail], Acc) ->
  create_map(Tail, Acc#{Key => create_map(Val, #{})});

create_map([[{Key, Val}] | Tail], Acc) ->
  create_map(Tail, Acc#{Key => create_map(Val, #{})});

create_map(Val, _Acc) when is_atom(Val) ->
	Val;

create_map(Val, _Acc) when is_integer(Val) ->
	Val;

create_map(Val, _Acc) when is_binary(Val) ->
	Val.

-doc """
""".
-spec filter(file:name_all(), [string()], string()) -> [binary()].
filter(Path, Files, ExtentionFile) ->
	F = fun(V, Aux) -> 
				case string:find(V, ExtentionFile, leading) of
					nomatch -> 
						Aux;

					_ ->
						File = list_to_binary([Path, ~"/", V]),
						?LOG_INFO(#{description => "File was found", 
												file => File}),
						Aux0 = Aux ++ [File],
						Aux0
				end
	end,
	lists:foldl(F, [], Files).

-doc """
""".
-spec cfg_file() -> term().
cfg_file() ->
	{ok, Value} = application:get_env(?APP, cfg_file),
	Value.

-spec find_module(binary()) -> {ok, atom()} | {error, atom()}.
find_module(ModuleName) when is_binary(ModuleName) ->
    case code:where_is_file(binary_to_list(ModuleName) ++ ".beam") of
        non_existing ->
            error(module_not_found);%{error, not_found};
        FullPath ->
            %% Extract the exact module name compiled inside the BEAM binary
            case beam_lib:chunks(FullPath, [attributes]) of
                {ok, {Module, _}} -> Module;
                {error, beam_lib, _} -> error(module_not_found) %{error, invalid_beam}
            end
    end.

-doc """
""".
-spec get_int_from_env(pos_integer() | binary()) -> pos_integer().
get_int_from_env(Value) ->
	case Value of
		N when is_integer(N) -> N;
		S when is_binary(S) -> 
			S0 = binary:replace(S, [~"${", ~"}"], <<>>, [global]),
			Val = unicode:characters_to_list(S0),
			list_to_integer(os:getenv(Val))
	end.
