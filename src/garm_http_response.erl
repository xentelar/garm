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

-include_lib("kernel/include/logger.hrl").

-include("http_commons.hrl").

%% =============================================================================
%% public functions
%% =============================================================================

-export([ok/0]).
-export([ok/1]).
-export([ok/2]).
-export([ko/1]).
-export([ko/2]).
-export([msg/2]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec ok() -> tuple().
ok() ->
	{Code, Desp} = ?HTTP_MSG_OK,
	msg(?OK_HTTP_CODE, #{
		<<"status">> => Code,
		<<"message">> => Desp
	}).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec ok(map()) -> tuple().
ok(Entity) ->
	msg(?OK_HTTP_CODE, Entity).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec ok(integer(), binary()) -> tuple().
ok(Status, Entity) ->
	msg(Status, Entity).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec ko(integer()) -> tuple().

%http code error
ko(Code) when is_integer(Code) ->
	Msg = case lists:keyfind(Code, 1, ?HTTP_DEFAULT_MSGS) of
			{_, M} ->
				M;

			false ->
				<<"Unknown Code">>
		end,
	ko(Code, Msg);

%general error
ko(Error) ->
	ko(?INTERNAL_SERVER_ERROR_HTTP_CODE, Error).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec ko(integer(), binary()) -> tuple().
ko(Code, Msg) ->
	?LOG_ERROR("Error: ~p]", [Msg]),
	msg(Code, #{
						<<"status">> => Code,
						<<"message">> => term_to_msg(Msg)
					}).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec msg(integer(), map()) -> tuple().
msg(Code, Entity) ->
	{Code, #{}, Entity}.

%% =============================================================================
%% private functions
%% =============================================================================

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