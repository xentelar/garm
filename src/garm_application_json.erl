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

-module(garm_application_json).

-moduledoc """
""".

-behaviour(garm_validator).

-include_lib("kernel/include/logger.hrl").

%% =============================================================================
%% public functions
%% =============================================================================

-export([validate/3]).

-doc """
""".
-spec validate(binary() | map(), term(), true | false) -> {ok, binary() | map()} | {error, term()}.
validate(ReqBody, {Schema, JesseState}, Required) ->
	try

		?LOG_INFO(#{description => "validattion parameters",
			req_body => ReqBody, required => Required,
			validator_schema => Schema}),

		?LOG_DEBUG(#{description => "schema validattion",
			jesse_state => JesseState}),

		case {byte_size(ReqBody), Required} of
			{0, false} ->
				{ok, #{}};

			{0, true} ->
				?LOG_ERROR(#{description => "Validattion error",
					req_body => ReqBody, required => Required}),
				{error, empty_body};

			{Size, _} when Size > 0 ->
				case thoas:decode(ReqBody) of
					{ok, JsonBody} ->
						ObjectDef = maps:get(~"schema", Schema),
						jesse_schema_validator:validate_with_state(ObjectDef, JsonBody, JesseState),
						?LOG_DEBUG(#{description => "Validattion ",
							req_body => ReqBody, required => Required,
							validator_schema => Schema, jesse_state => JesseState}),
						{ok, JsonBody};
					{error, Reason} ->
						?LOG_ERROR(#{description => "Validattion error",
							req_body => ReqBody, required => Required, reason => Reason}),
						{error, Reason}
				end
		end

	catch
		Class:Reason0:_Stacktrace ->
			?LOG_ERROR(#{description => "Validattion error",
				error => Class, reason => Reason0}),
			{error, Reason0}
	end.

%% =============================================================================
%% private functions
%% =============================================================================
