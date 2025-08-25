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

-module(garm_health_handler).

-include_lib("kernel/include/logger.hrl").

-include("http_commons.hrl").

%% =============================================================================
%% public functions
%% =============================================================================

-export([init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([do_response/2]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
allowed_methods(Req, State) ->
	{[?GET], Req, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
resource_exists(Req, State) ->
	do_response(Req, State).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
do_response(Req, State) ->

	live(Req, State),

	{stop, Req, State}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
live(Req, _State) ->
	
	#{<<"healthy">> := HealthyList, 
	<<"unhealthy">> := UnhealthyList} = healthcheck_handler:check_processes(),

	case erlang:length(UnhealthyList) of
		0 -> 
			cowboy_req:reply(200, 
									?DEFAULT_HEADER, 
									thoas:encode(HealthyList), 
									Req
							);
		L when L > 0 ->
			cowboy_req:reply(500, 
									?DEFAULT_HEADER, 
									thoas:encode(UnhealthyList), 
									Req
							)
	end.

%% =============================================================================
%% private functions
%% =============================================================================
