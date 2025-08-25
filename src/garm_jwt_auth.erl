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

-module(garm_jwt_auth).

-include_lib("kernel/include/logger.hrl").

-define(NOW, erlang:system_time(second)).
-define(CLOCK_SKEW, 2 * 60). % 2 mins
-define(IS_TIME_EXPIRED(Exp), ((Exp + ?CLOCK_SKEW) - ?NOW) =< 0 ).

%% =============================================================================
%% public functions
%% =============================================================================

-export([is_authorized/2]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------  
-spec is_authorized(Token :: binary(), Scopes :: list()) -> {true, #{}}.
is_authorized(Token, Scopes) ->
  
	case validate_bearer(Token) of
		{true, Jwt} ->
			Ctx = Jwt#{<<"scopes">> => Scopes},

			{true, Ctx};
		false ->
			false
	end.

%% =============================================================================
%% private functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec validate_bearer(binary()) -> tuple().
validate_bearer(Bearer) ->
	Token = binary:replace(Bearer, <<"Bearer ">>, <<"">>),

	?LOG_DEBUG(#{description => "token to process", 
							 token => Token}),
	
	JWK = garm_cache_manager:jwk(),
	
	case jose_jwt:verify(JWK, Token) of
		{true, {_, JWT}, JWS} ->
			?LOG_DEBUG(#{description => "verify token", 
							jwt => JWT, jws => JWS}),
			maybe_expired(JWT);

		Error ->
			% invalid token, do something else
			?LOG_ERROR(#{description => "the token is invalid", 
                error => Error}),
			false
	end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec maybe_expired(map()) -> tuple().
maybe_expired(JWT) ->
	case maps:get(<<"exp">>, JWT, undefined) of
		undefined ->

			?LOG_ERROR(#{description => "the key expiration time (exp) is not present"}),
			false;
		
		Exp ->
			case ?IS_TIME_EXPIRED(Exp) of
				true ->
					?LOG_DEBUG(#{description => "token time has expired", 
							jwt => JWT}),
					false;

				false ->
					?LOG_DEBUG(#{description => "token is ok", 
							jwt => JWT}),
					{true, #{<<"jwt">> => JWT}}
			end
	end.