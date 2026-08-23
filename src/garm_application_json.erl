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

-behaviour(garm_validator).

-include_lib("kernel/include/logger.hrl").

%% -----------------------------------------------------------------------------
%% public functions
%% -----------------------------------------------------------------------------

-export([init/1]).
-export([validate/4]).

-spec init(ObjectsDef :: map()) -> {ok, term()} | {error, term()}.
init(ObjectsDef) -> 
	C = #{<<"components">> => ObjectsDef},
  JesseState = jesse_state:new(C, [{default_schema_ver, <<"http://json-schema.org/draft-04/schema#">>}]),
	{ok, JesseState}.

%% -----------------------------------------------------------------------------
%% @doc
%% Decode and validate a json body
%% -----------------------------------------------------------------------------
-spec validate(binary() | map(), term(), true | false, [binary()]) -> {ok, binary() | map()} | {error, term()}.
validate(ReqBody, {Schema, JesseState}, Required, _Params) ->
	try
		%?LOG_DEBUG(#{description => "Validation parameters",
		%	req_body => ReqBody, required => Required,
		%	validator_schema => Schema, jesse_state => JesseState}),
		case {byte_size(ReqBody), Required} of
			{0, false} ->
				{ok, #{}};

			{0, true} ->
				%?LOG_ERROR(#{description => "Validation error",
				%	req_body => ReqBody, required => Required}),
				{error, empty_body};

			{Size, _} when Size > 0 ->
				Utf8Binary = unicode:characters_to_binary(ReqBody),
				case thoas:decode(Utf8Binary) of
					{ok, JsonBody} ->
						ObjectDef = maps:get(<<"schema">>, Schema),
						jesse_schema_validator:validate_with_state(ObjectDef, JsonBody, JesseState),
						%?LOG_DEBUG(#{description => "Validation ",
						%	req_body => ReqBody, required => Required,
						%	validator_schema => Schema, jesse_state => JesseState}),
						{ok, JsonBody};
					{error, Reason} ->
						%?LOG_ERROR(#{description => "Validation error",
						%	req_body => ReqBody, required => Required, reason => Reason}),
						{error, Reason}
				end
		end
	catch
		_Class:Reason0:_Stacktrace ->
			%?LOG_ERROR(#{description => "Validation error",
			%	reason => Reason0, stacktrace => Stacktrace}),
			{error, Reason0}
	end.

%% -----------------------------------------------------------------------------
%% private functions
%% -----------------------------------------------------------------------------
