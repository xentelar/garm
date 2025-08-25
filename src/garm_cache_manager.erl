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

-module(garm_cache_manager).

-include_lib("kernel/include/logger.hrl").

%% =============================================================================
%% public functions
%% =============================================================================

-export([get_user_info/1]).
-export([put_user_info/2]).
-export([jwk/0]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_user_info(binary()) -> map() | atom().
get_user_info(Id) ->
	?LOG_DEBUG(#{description => "search user info on cache", 
							id => Id}),
	cache:get(user_info, Id).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec put_user_info(binary(), map()) -> term().
put_user_info(Id, Value) ->
	?LOG_DEBUG(#{description => "put user info on cache", 
							id => Id, value => Value}),
	cache:put(user_info, Id, Value).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec jwk() -> list().
jwk() ->
	[{_, JWT}] = ets:lookup(certificates, jwk),
	JWT.

%% =============================================================================
%% private functions
%% =============================================================================
