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

-module(garm_http_response).

-moduledoc """
""".

-include_lib("kernel/include/logger.hrl").

%% =============================================================================
%% public functions
%% =============================================================================

-export([build/2]).
-export([build/3]).
-export([resp_headers/2]).
-export([prepare_response/3]).

-doc """
""".
-spec build(pos_integer(), map()) -> tuple().
build(Status, Headers) ->
	msg(Status, Headers).

-doc """
""".
-spec build(pos_integer(), map(), binary()) -> tuple().
build(Status, Headers, Body) ->
	msg(Status, Headers, Body).

-doc """
""".
-spec resp_headers(term(), binary()) -> term().
resp_headers(Req, Origin) ->
  cowboy_req:set_resp_headers(#{
		<<"Access-Control-Allow-Origin">> => Origin
	}, Req).

-doc """
""".
-spec prepare_response(tuple(), map(), atom()) -> binary().
prepare_response({Code, Headers, no_response}, _MethodCfg, ValidResponse) 
															when map_size(ValidResponse)==0 ->
  {ok, {Code, Headers}};

prepare_response({Code, Headers, BodyRps}, _MethodCfg, ValidResponse) 
															when is_binary(BodyRps) andalso map_size(ValidResponse)==0 ->
  {ok, {Code, Headers, BodyRps}};

prepare_response({Code, Headers, no_response}, MethodCfg, ValidResponse) 
															when map_size(ValidResponse)>0 ->
	prepare_response({Code, Headers, <<>>}, MethodCfg, ValidResponse);

prepare_response({Code, Headers, BodyRps}, MethodCfg, ValidResponse) 
															when is_binary(BodyRps) andalso map_size(ValidResponse)>0 ->
	%?LOG_INFO(#{description => "Validation response parameters",
	%		req_body => BodyRps, headers => Headers, http_code => Code,
	%		validator_schema => ValidResponse, method_cfg => MethodCfg}),
	case maps:get(~"responses", MethodCfg, undefined) of
		undefined ->
			{ok, {Code, Headers, BodyRps}};
		RspCfg ->
			HttpCode = integer_to_binary(Code),
			case maps:get(HttpCode, RspCfg, undefined) of
				undefined ->
					{ok, {Code, Headers, BodyRps}};
				HttpCodeCfg ->
					case maps:get(~"content", HttpCodeCfg, undefined) of
						undefined ->
							{ok, {Code, Headers, BodyRps}};
						ContentTypes ->
							case maps:get(~"content-type", Headers, undefined) of
								undefined ->
									{ok, {Code, Headers, BodyRps}};
								ContentType ->
									case maps:get(ContentType, ContentTypes, undefined) of
										undefined ->
											{ok, {Code, Headers, BodyRps}};
										SchemaVal ->
											case maps:get(ContentType, ValidResponse, undefined) of
												undefined ->
													{error, <<"response validator module for [", ContentType/binary, "] not found">>};
												Validator ->
												case garm_validator:validate(Validator, BodyRps, SchemaVal, false) of
													{ok, _BodyRps} ->
														{ok, {Code, Headers, BodyRps}};
													{error, Reason} ->
														{error, Reason}
												end
											end
									end
							end
					end
			end
	end;

prepare_response({_Code, _Headers, _BodyRps}, _MethodCfg, _ValidResponse) ->
  error(unexpected_response).

%% =============================================================================
%% private functions
%% =============================================================================

-doc """
""".
-spec msg(pos_integer(), map(), binary()) -> {pos_integer(), map(), map() | binary()}.
msg(Status, Headers, Body) ->
	{Status, Headers, Body}.

-doc """
""".
-spec msg(pos_integer(), map()) -> {pos_integer(), map()}.
msg(Status, Headers) ->
	{Status, Headers}.
