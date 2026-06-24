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

-module(garm_auth).

-moduledoc """
""".

-include_lib("kernel/include/logger.hrl").

%% =============================================================================
%% public functions
%% =============================================================================

-export([is_authorized/3]).
-export([start/4]).
-export([is_authorized/2]).

-doc """
""".
-callback start(DomainKey :: binary(), SecScheme :: binary(), SecurityDef :: map()) -> {ok, term()} | {error, term()}.


-doc """
""".
-callback is_authorized(Req :: cowboy_req:req(), SecuritySchema :: map()) -> {true, term()} | false.

-doc """
""".
-spec is_authorized(atom(), cowboy_req:req(), map()) -> {true, term()} | false.
is_authorized(AuthControl, Req, SecuritySchema) ->
  try 

    erlang:apply(AuthControl, is_authorized, [Req, SecuritySchema])
    
  catch _Class:Reason:Stacktrace ->
    ?LOG_ERROR(#{description => "Auth Adapter errors", 
          reason => Reason, auth_adapter => AuthControl, callback => is_authorized,
          stacktrace => Stacktrace}),
    false
  end.

-doc """
""".
-spec start(module(), binary(), binary(), map()) -> {ok, term()} | {error, term()}.
start(AuthControl, DomainKey, SecScheme, SecurityDef) ->
  try 

    erlang:apply(AuthControl, start, [DomainKey, SecScheme, SecurityDef])
    
  catch _Class:Reason:Stacktrace ->
    ?LOG_ERROR(#{description => "Auth Adapter errors", 
          reason => Reason, auth_adapter => AuthControl, callback => start,
          stacktrace => Stacktrace}),
    {error, Reason}
  end.

-spec is_authorized(cowboy_req:req(), map()) -> list().
is_authorized(Req, SecuritySchemas) ->
  exec(SecuritySchemas, Req, false).

%% =============================================================================
%% private functions
%% =============================================================================

-spec exec(list(), cowboy_req:req(), {true, term()} | false) -> {true, term()} | false.
exec([], _Req, Result) ->
  Result;

exec([SecSchema | T], Req, Result) ->
  [SecSchemaDef] = maps:values(SecSchema),
  AuthControl = maps:get(~"authControl", SecSchemaDef),
  case is_authorized(AuthControl, Req, SecSchema) of
    false ->
      exec(T, Req, Result);
    {true, Data} ->
      exec([], Req, {true, Data})
  end.
