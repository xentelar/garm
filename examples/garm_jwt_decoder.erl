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

-module(garm_jwt_decoder).

-behaviour(garm_auth).

-include_lib("kernel/include/logger.hrl").

%% -----------------------------------------------------------------------------
%% public functions
%% -----------------------------------------------------------------------------

-export([is_authorized/2]).
-export([start/3]).

-spec start(binary(), binary(), map()) -> {ok, term()} | {error, term()}.
start(DomainKey, SecScheme , SecurityDef) ->
	?LOG_INFO(#{description => "Start JWT decoder", 
		domain => DomainKey, security_scheme => SecScheme, 
		security_def => SecurityDef}),
	{ok, ok}.

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------  
-spec is_authorized(cowboy_req:req(), map()) -> {true, term()} | false.
is_authorized(Req, _SecuritySchema)  ->
	case garm_http_request:get_header_value(<<"Authorization">>, Req) of
		undefined ->
			?LOG_DEBUG(#{description => "Authorization header is undefined"}),
			false;
		Bearer ->
			Token = binary:replace(Bearer, <<"Bearer ">>, <<"">>),
			TK2 = lists:nth(2, binary:split(Token, [<<".">>], [global])),
			Json = base64:decode(TK2, #{padding => false, mode => urlsafe}),
			case thoas:decode(Json) of
				{ok, Jwt} ->
					{true, Jwt};
				{error, Reason} ->
					?LOG_ERROR(#{description => "Bearer error, can't decoded",
						bearer => Bearer, reason => Reason}),
					false
			end
	end.

%% -----------------------------------------------------------------------------
%% private functions
%% -----------------------------------------------------------------------------
