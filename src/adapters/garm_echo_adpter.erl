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

-include("http_commons.hrl").

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
	?LOG_INFO(#{description => "Started Echo adapter",
						domain_key => DomainKey,
						operations_cfg => OperationsCfg}).

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

	Rsp = thoas:encode(Populated),
	Headers = #{<<"content-type">> => <<"application/json">>},
	garm_http_response:ok(?OK_HTTP_CODE, Headers, Rsp).

%% =============================================================================
%% private functions
%% =============================================================================
