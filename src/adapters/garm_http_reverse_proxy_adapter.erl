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

-module(garm_http_reverse_proxy_adapter).

-include_lib("kernel/include/logger.hrl").

-include("http_commons.hrl").

%% =============================================================================
%% public functions
%% =============================================================================

-export([process/3]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec process(binary(), binary(), map()) -> tuple().
process(DomainKey, OperationID, Populated) ->

	{ok, Cfg} = garm_config:find(DomainKey),

	?LOG_DEBUG(#{description => "Process operation",
						domain => Cfg,
						op_id => OperationID,
						populated => Populated}),
            
	Headers = maps:get(<<"headers">>, Populated),
	Bindings = maps:get(<<"bindings">>, Populated),
	Body = maps:get(<<"body">>, Populated),

	OpCfg = maps:get(OperationID, Cfg),

	case call(Headers, Bindings, Body, OpCfg) of
		{ok, Status, RespHeaders, ResBody} ->
			garm_http_response:ok(Status, ResBody);

		Error0 ->
			garm_http_response:ko(Error0)
	end.

%% =============================================================================
%% private functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec call(map(), map(), map(), map()) -> term().
call(Headers, Bindings, Body, OpCfg) -> 

	Headers0 = maps:to_list(Headers),
	Payload = thoas:encode(Body),
	{Method, Url} = build_url(OpCfg),

	F = fun(K, V, Url0) ->
				K0 = list_to_binary([<<"{">>, K, <<"}">>]),
				binary:replace(Url0, K0, V)
		end,
	Url1 = maps:fold(F, Url, Bindings),

	Options = [],

	?LOG_DEBUG(#{description => "Call to host",
						method => Method,
						url => Url1,
						headers => Headers0,
						body => Payload}),

	case hackney:request(Method, Url1, Headers0, Payload, Options) of
		{ok, Status, RespHeaders, ClientRef} ->
			{ok, ResBody} = hackney:body(ClientRef),
			?LOG_DEBUG(#{description => "Response from host",
								status => Status,
								headers => RespHeaders,
								body => ResBody}),
			{ok, Status, RespHeaders, ResBody};

		Error ->
			?LOG_ERROR(#{description => "Call error",
								error => Error}),
      {error, Error}

	end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec build_url(map()) -> {binary(), binary()}.
build_url(OpCfg) ->
	#{
		<<"host">> := Host,
		<<"method">> := Method,
		<<"protocol">> := Protocol,
		<<"request">> := Req
	} = OpCfg,
	{Method, list_to_binary([Protocol, <<"://">>, Host, Req])}.