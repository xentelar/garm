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

-module(garm_jwt_auth).

-behaviour(garm_auth).

-moduledoc """
""".

-include_lib("kernel/include/logger.hrl").

-define(NOW, erlang:system_time(second)).
-define(CLOCK_SKEW, 2 * 60). % 2 mins
-define(IS_TIME_EXPIRED(Exp), ((Exp + ?CLOCK_SKEW) - ?NOW) =< 0 ).

%% =============================================================================
%% public functions
%% =============================================================================

-export([is_authorized/2]).
-export([start/3]).

-spec start(binary(), binary(), map()) -> {ok, term()} | {error, term()}.
start(DomainKey, SecScheme , SecurityDef) ->
	case maps:get(~"certificate", SecurityDef, undefined) of
		undefined -> 
			{error, cert_not_found};
		CertFileName ->
			Path = list_to_binary(garm_config:config_path()),
			CertFile = <<Path/binary, "/", CertFileName/binary>>,
			?LOG_INFO(#{description => "Load certificate", 
				domain => DomainKey, security_scheme => SecScheme, 
				cret_file => CertFile}),
			case jose_jwk:from_pem_file(binary_to_list(CertFile)) of
				{error, Reason} ->
					{error, Reason};
				JoseJwk ->
					?LOG_INFO(#{description => "Load JWK from certificate", 
						domain => DomainKey, security_scheme => SecScheme, 
						jwk => JoseJwk, cret_file => CertFile}),
					{ok, JoseJwk}
			end
	end.

-doc """
""".  
-spec is_authorized(cowboy_req:req(), map()) -> {true, term()} | false.
is_authorized(Req, SecuritySchema)  ->
	case garm_http_request:get_header_value(<<"Authorization">>, Req) of
		undefined ->
			?LOG_DEBUG(#{description => "Authorization header is undefined"}),
			false;
		Bearer ->
			JWK = maps:get(~"authData", SecuritySchema),
			Scope = maps:get(~"scope", SecuritySchema),
			case validate_bearer(Bearer, JWK) of
				{true, Jwt} ->
					Ctx = Jwt#{<<"scope">> => Scope},
					{true, Ctx};
				false ->
					false
			end
	end.

%% =============================================================================
%% private functions
%% =============================================================================

-doc """
""".
-spec validate_bearer(binary(), tuple()) -> tuple().
validate_bearer(Bearer, JWK) ->
	Token = binary:replace(Bearer, <<"Bearer ">>, <<"">>),
	?LOG_DEBUG(#{description => "Token to process", 
		token => Token}),
	case jose_jwt:verify(JWK, Token) of
		{true, {_, JWT}, JWS} ->
			?LOG_DEBUG(#{description => "Verify token", 
				jwt => JWT, jws => JWS}),
			maybe_expired(JWT);
		Error ->
			?LOG_ERROR(#{description => "The token is invalid", 
				error => Error}),
			false
	end.

-doc """
""".
-spec maybe_expired(map()) -> tuple().
maybe_expired(JWT) ->
	case maps:get(<<"exp">>, JWT, undefined) of
		undefined ->

			?LOG_ERROR(#{description => "The key expiration time (exp) is not present"}),
			false;
		
		Exp ->
			case ?IS_TIME_EXPIRED(Exp) of
				true ->
					?LOG_DEBUG(#{description => "Token time has expired", 
							jwt => JWT}),
					false;

				false ->
					?LOG_DEBUG(#{description => "Token is ok", 
							jwt => JWT}),
					{true, #{<<"jwt">> => JWT}}
			end
	end.