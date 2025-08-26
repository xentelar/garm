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

-module(garm_echo_adpter).

-include_lib("kernel/include/logger.hrl").

%% =============================================================================
%% public functions
%% =============================================================================

-export([start/2]).
-export([process/3]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec start(binary(), map()) -> term().
start(DomainKey, OperationsCfg) ->
	ok.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec process(binary(), binary(), map()) -> tuple().
process(DomainKey, OperationID, Populated) ->

	?LOG_INFO(#{description => "Process operation",
						domain_key => DomainKey,
						op_id => OperationID,
						populated => Populated}),

	garm_http_response:ok(Populated).

%% =============================================================================
%% private functions
%% =============================================================================
