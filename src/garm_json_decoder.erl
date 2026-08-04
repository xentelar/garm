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
-module(garm_json_decoder).

-behaviour(garm_validator).

-include_lib("kernel/include/logger.hrl").

%% -----------------------------------------------------------------------------
%% public functions
%% -----------------------------------------------------------------------------

-export([init/1]).
-export([validate/4]).

-spec init(ObjectsDef :: map()) -> {ok, term()} | {error, term()}.
init(_ObjectsDef) -> 
	?LOG_INFO(#{description => "Start application/json contect type"}),
	{ok, ok}.

%% -----------------------------------------------------------------------------
%% @doc
%% Only decode a json body
%% -----------------------------------------------------------------------------
-spec validate(binary() | map(), term(), true | false, [binary()]) -> {ok, binary() | map()} | {error, term()}.
validate(ReqBody, _, _Required, _Params) ->
	try

		?LOG_DEBUG(#{description => "Validation request body",
			req_body => ReqBody}),

		case byte_size(ReqBody) of
			0 ->
				?LOG_ERROR(#{description => "Validation error, request body is empty",
					req_body => ReqBody}),
				{ok, #{}};

			Size when Size > 0 ->
				case thoas:decode(ReqBody) of
					{ok, JsonBody} ->
						?LOG_DEBUG(#{description => "Validation request body is ok",
							req_body => ReqBody}),
						{ok, JsonBody};
					{error, Reason} ->
						?LOG_ERROR(#{description => "Validation error, request body is not json",
							req_body => ReqBody, reason => Reason}),
						{ok, #{}}
				end
		end

	catch
		Class:Reason0:_Stacktrace ->
			?LOG_ERROR(#{description => "Validation error",
				error => Class, reason => Reason0}),
			{ok, #{}}
	end.

%% -----------------------------------------------------------------------------
%% private functions
%% -----------------------------------------------------------------------------
