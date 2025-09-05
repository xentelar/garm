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

-module(garm_http_response).

-include("http_commons.hrl").

%% =============================================================================
%% public functions
%% =============================================================================

-export([ok/2]).
-export([ok/3]).

-export([ko/1]).
-export([ko/2]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec ok(integer(), map()) -> tuple().
ok(Status, Headers) ->
	msg(Status, Headers).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec ok(integer(), map(), binary()) -> tuple().
ok(Status, Headers, Body) ->
	msg(Status, Headers, Body).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec ko(integer()) -> tuple().
ko(Code) when is_integer(Code) ->
	Msg = case lists:keyfind(Code, 1, ?HTTP_DEFAULT_MSGS) of
			{_, M} ->
				M;

			false ->
				<<"Unknown Code">>
		end,
	ko(Code, Msg);

ko(Error) ->
	ko(?BAD_GATEWAY_HTTP_CODE, Error).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec ko(integer(), binary()) -> tuple().
ko(Code, Msg) ->
	Body = #{
						<<"status">> => Code,
						<<"message">> => term_to_msg(Msg)
					},
	msg(Code, #{}, Body).

%% =============================================================================
%% private functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec msg(integer(), map()) -> tuple().
msg(Code, Headers) ->
	{Code, Headers, <<>>}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec msg(integer(), map(), binary()) -> tuple().
msg(Code, Headers, Body) ->
	{Code, Headers, Body}.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec term_to_msg(term()) -> binary().
term_to_msg(Msg0) when not is_binary(Msg0) ->
	R= io_lib:format("~p",[Msg0]),
	M = lists:flatten(R),
	list_to_binary(M);

term_to_msg(Msg) when is_binary(Msg) ->
	Msg.