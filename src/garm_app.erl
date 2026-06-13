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

-module(garm_app).

-moduledoc """
""".

-behaviour(application).

-define(APP, garm).

-include_lib("kernel/include/logger.hrl").

%% =============================================================================
%% public functions
%% =============================================================================

-export([start/2]).
-export([stop/1]).

-export([info/0]).
-export([vsn/0]).
-export([name/0]).

-doc """
""".
-spec start(term(), term()) -> term().
start(_StartType, _StartArgs) ->
	?LOG_NOTICE(#{description => "initializing garm"}),

	case garm_sup:start_link() of
		{ok, _} = OK ->
			start_cache(),
			start_server(),
			OK;
			
		Error ->
			Error
	end.

-doc """
""".
-spec stop(term()) -> term().
stop(_State) ->
    ok.

-doc """
""".
-spec info() -> tuple().
info() ->
	Apps = application:which_applications(),
	{_, Description, Vsn} = lists:keyfind(?APP, 1, Apps),
	{Description, Vsn}.

-doc """
""".
-spec vsn() -> binary().
vsn() ->
	Apps = application:which_applications(),
	{_, _, Vsn} = lists:keyfind(?APP, 1, Apps),
	list_to_binary(Vsn).

-doc """
""".
-spec name() -> binary().
name() ->
	Vsn = vsn(),
	App = atom_to_binary(?APP),
	<<App/binary, "-", Vsn/binary>>.

%% =============================================================================
%% private functions
%% =============================================================================

-doc """
""".
-spec start_server() -> ok.
start_server() ->
	garm_server:init().

-doc """
""".
-spec start_cache() -> ok.
start_cache() ->
	garm_cache_manager:init_cache().
