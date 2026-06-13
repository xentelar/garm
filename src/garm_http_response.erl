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

-include("http_commons.hrl").

-define(UNKNOWN_STATUS, <<"Unknown Status">>).

%% =============================================================================
%% public functions
%% =============================================================================

-export([build/2]).
-export([build/3]).

-doc """
""".
-spec build(non_neg_integer(), map()) -> tuple().
build(Status, Headers) ->
	Body = status_description(Status),
	msg(Status, Headers, Body).

-doc """
""".
-spec build(non_neg_integer(), map(), binary()) -> tuple().
build(Status, Headers, Body) ->
	Body0 = 
	case lists:keyfind(Status, 1, ?HTTP_DEFAULT_MSGS) of
		{_S, _M} -> Body;
		false -> ?UNKNOWN_STATUS
	end,
	msg(Status, Headers, Body0).

%% =============================================================================
%% private functions
%% =============================================================================

-doc """
""".
-spec msg(non_neg_integer(), map(), binary()) -> tuple().
msg(Status, Headers, Body) ->
	{Status, Headers, Body}.

-doc """
""".
-spec status_description(non_neg_integer()) -> binary().
status_description(Status) ->
	case lists:keyfind(Status, 1, ?HTTP_DEFAULT_MSGS) of
		{_, Description} -> Description;
		false -> ?UNKNOWN_STATUS
	end.
