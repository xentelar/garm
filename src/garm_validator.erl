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

-module(garm_validator).

-moduledoc """
""".

-include_lib("kernel/include/logger.hrl").

%% =============================================================================
%% public definitions
%% =============================================================================

-export([validate/4]).

-doc """
""".
-callback validate(Value :: binary() | map(), ValidatorSchema :: term(), 
                  Required :: true | false) -> {ok, binary() | map()} | {error, term()}.

-spec validate(atom(), binary() | map(), term(), true | false) -> {ok, binary() | map()} | {error, term()}.
validate(Handler, Value, ValidatorSchema, Required) ->
  try 

    erlang:apply(Handler, validate, [Value, ValidatorSchema, Required])

  catch _Class:Reason:_Stacktrace ->
    ?LOG_ERROR(#{description => "Validator not found or not loaded", 
          handler => Handler, callback => validate, reason => Reason}),
    {error, Reason}
  end.

%% =============================================================================
%% private functions
%% =============================================================================
