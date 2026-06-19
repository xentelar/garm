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

-module(garm_adapter).

-moduledoc """
""".

-type response() :: {Status :: pos_integer(), Headers :: map()} 
  | {Status :: pos_integer(), Headers :: map(), Body :: binary() | map()}
  | {error, term()}.
-export_type([response/0]).

-include_lib("kernel/include/logger.hrl").

%% =============================================================================
%% public definitions
%% =============================================================================

-export([init/3]).
-export([process/4]).

-doc """
Initialize the adapter.

When a `cowboy` process is started this function is called
by the new process to initialize internal parameters.
""".
-callback start(DomainKey :: binary(), OperationsCfg :: map()) -> term() | {error, term()}.

-doc """
Handle the http calls.
""".
-callback process(DomainKey :: binary(), OperationID :: binary(), Populated :: map()) -> response().

-doc """
""".
-spec init(atom(), binary(), map()) -> term() | {error, term()}.
init(Handler, DomainKey, Populated) ->
  try 

    erlang:apply(Handler, start, [DomainKey, Populated])

  catch _Class:Reason:Stacktrace ->
    ?LOG_ERROR(#{description => "Adapter errors", 
        reason => Reason, handler => Handler, callback => start,
        stacktrace => Stacktrace}),
    {error, Reason}
  end.

-doc """
""".
-spec process(atom(), binary(), binary(), map()) -> response().
process(Adapter, DomainKey, OperationID, Populated) ->
  try 

    erlang:apply(Adapter, process, [DomainKey, OperationID, Populated])
    
  catch _Class:Reason:Stacktrace ->
    ?LOG_ERROR(#{description => "Adapter errors", 
          reason => Reason, adapter => Adapter, callback => process,
          stacktrace => Stacktrace}),
    {error, Reason}
  end.

%% =============================================================================
%% private functions
%% =============================================================================
