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

-module(garm_storefront_mock_adpter).

-behaviour(garm_adapter).

-include_lib("kernel/include/logger.hrl").

-include("http_elements.hrl").

%% -----------------------------------------------------------------------------
%% public functions
%% -----------------------------------------------------------------------------

-export([start/2]).
-export([process/3]).

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec start(binary(), map()) -> term().
start(DomainKey, OperationsCfg) ->
	?LOG_NOTICE(#{description => "Started Echo adapter",
						domain_key => DomainKey,
						operations_cfg => OperationsCfg}).

%% -----------------------------------------------------------------------------
%% @doc
%% -----------------------------------------------------------------------------
-spec process(binary(), binary(), map()) -> tuple().
process(DomainKey, OperationID, Populated) ->

	?LOG_INFO(#{description => "Process operation",
						domain_key => DomainKey,
						op_id => OperationID}),

	case OperationID of
		<<"createUser">> -> createUser(Populated);
		<<"login">> -> login(Populated);
		<<"findProducts">> -> findProducts(Populated);
		<<"getProduct">> -> getProduct(Populated);
		<<"getCart">> -> getCart(Populated);
		<<"addItem">> -> addItem(Populated);
		<<"deleteItem">> -> deleteItem(Populated);
		<<"checkout">> -> checkout(Populated);
		<<"findOrders">> -> findOrders(Populated);
		<<"getOrder">> -> getOrder(Populated);
		<<"getAddress">> -> getAddress(Populated);
		<<"addAddress">> -> addAddress(Populated);
		<<"updateAddress">> -> updateAddress(Populated)
	end.

%% -----------------------------------------------------------------------------
%% private functions
%% -----------------------------------------------------------------------------

-spec createUser(map()) -> tuple().
createUser(Populated) ->
	?LOG_INFO(#{description => "Create user operation",
						populated => Populated}),
	Headers = #{},
	garm_http_response:build(?CREATED_HTTP_CODE, Headers).

-spec login(map()) -> tuple().
login(Populated) ->
	?LOG_INFO(#{description => "Login operation",
						populated => Populated}),
	Headers = #{},
	garm_http_response:build(?OK_HTTP_CODE, Headers).

-spec findProducts(map()) -> tuple().
findProducts(Populated) ->
	?LOG_INFO(#{description => "Find products operation",
						populated => Populated}),
	R = [
		#{
			<<"id">> => <<"eda5cbc1-a615-4da5-ae73-4a33a9acfb6a">>,
			<<"name">> => <<"Worry Management">>,
			<<"description">> => <<"Mr street sell would civil. People through shake southern force.">>,
			<<"price">> => 91.37,
			<<"category">> => <<"wrong">>,
			<<"image_url">> => <<"https://dummyimage.com/766x809">>,
			<<"stock">> => 94,
			<<"created_at">> => <<"2026-06-13T14:58:52.278Z">>,
			<<"updated_at">> => <<"2026-06-13T14:58:52.278Z">>
		}
	],
	Response = thoas:encode(R),
	Headers = #{<<"content-type">> => <<"application/json">>},
	garm_http_response:build(?OK_HTTP_CODE, Headers, Response).

-spec getProduct(map()) -> tuple().
getProduct(Populated) ->
	?LOG_INFO(#{description => "Get product operation",
						populated => Populated}),
	R = #{
		<<"id">> => <<"eda5cbc1-a615-4da5-ae73-4a33a9acfb6a">>,
		<<"name">> => <<"Worry Management">>,
		<<"description">> => <<"Mr street sell would civil. People through shake southern force.">>,
		<<"price">> => 91.37,
		<<"category">> => <<"wrong">>,
		<<"image_url">> => <<"https://dummyimage.com/766x809">>,
		<<"stock">> => 94,
		<<"created_at">> => <<"2026-06-13T15:01:05.713Z">>,
		<<"updated_at">> => <<"2026-06-13T15:01:05.713Z">>
	},
	Response = thoas:encode(R),
	Headers = #{<<"content-type">> => <<"application/json">>},
	garm_http_response:build(?OK_HTTP_CODE, Headers, Response).

-spec getCart(map()) -> tuple().
getCart(Populated) ->
	?LOG_INFO(#{description => "Get cart operation",
						populated => Populated}),
	R = [
		#{
			<<"product_id">> => <<"3fa85f64-5717-4562-b3fc-2c963f66afa6">>,
			<<"quantity">> => 1
		}
	],
	Response = thoas:encode(R),
	Headers = #{<<"content-type">> => <<"application/json">>},
	garm_http_response:build(?OK_HTTP_CODE, Headers, Response).

-spec addItem(map()) -> tuple().
addItem(Populated) ->
	?LOG_INFO(#{description => "Add item operation",
						populated => Populated}),
	Headers = #{},
	garm_http_response:build(?OK_HTTP_CODE, Headers).

-spec deleteItem(map()) -> tuple().
deleteItem(Populated) ->
	?LOG_INFO(#{description => "Delete item operation",
						populated => Populated}),
	Headers = #{},
	garm_http_response:build(?OK_HTTP_CODE, Headers).

-spec checkout(map()) -> tuple().
checkout(Populated) ->
	?LOG_INFO(#{description => "Checkout operation",
						populated => Populated}),
	R = #{
		<<"id">> => <<"3fa85f64-5717-4562-b3fc-2c963f66afa6">>,
		<<"items">> => [
			#{
				<<"product_id">> => <<"3fa85f64-5717-4562-b3fc-2c963f66afa6">>,
				<<"quantity">> => 1
			}
		],
		<<"total_amount">> => 0,
		<<"status">> => <<"pending">>,
		<<"created_at">> => <<"2026-06-13T15:02:37.031Z">>
	},
	Response = thoas:encode(R),
	Headers = #{<<"content-type">> => <<"application/json">>},
	garm_http_response:build(?OK_HTTP_CODE, Headers, Response).

-spec findOrders(map()) -> tuple().
findOrders(Populated) ->
	?LOG_INFO(#{description => "Find order operation",
						populated => Populated}),
	R = [
		#{
			<<"id">> => <<"3fa85f64-5717-4562-b3fc-2c963f66afa6">>,
			<<"items">> => [
				#{
					<<"product_id">> => <<"3fa85f64-5717-4562-b3fc-2c963f66afa6">>,
					<<"quantity">> => 1
				}
			],
			<<"total_amount">> => 0,
			<<"status">> => <<"pending">>,
			<<"created_at">> => <<"2026-06-13T15:04:16.402Z">>
		}
	],
	Response = thoas:encode(R),
	Headers = #{<<"content-type">> => <<"application/json">>},
	garm_http_response:build(?OK_HTTP_CODE, Headers, Response).

-spec getOrder(map()) -> tuple().
getOrder(Populated) ->
	?LOG_INFO(#{description => "Get order operation",
						populated => Populated}),
	R = #{
		<<"id">> => <<"3fa85f64-5717-4562-b3fc-2c963f66afa6">>,
		<<"items">> => [
			#{
				<<"product_id">> => <<"3fa85f64-5717-4562-b3fc-2c963f66afa6">>,
				<<"quantity">> => 1
			}
		],
		<<"total_amount">> => 0,
		<<"status">> => <<"pending">>,
		<<"created_at">> => <<"2026-06-13T15:05:42.792Z">>
	},
	Response = thoas:encode(R),
	Headers = #{<<"content-type">> => <<"application/json">>},
	garm_http_response:build(?OK_HTTP_CODE, Headers, Response).

-spec getAddress(map()) -> tuple().
getAddress(Populated) ->
	?LOG_INFO(#{description => "Get address operation",
						populated => Populated}),
	R = [
		#{
			<<"line1">> => <<"string">>,
			<<"line2">> => <<"string">>,
			<<"city">> => <<"string">>,
			<<"state">> => <<"string">>,
			<<"postal_code">> => <<"string">>,
			<<"country">> => <<"string">>
		}
	],
	Response = thoas:encode(R),
	Headers = #{<<"content-type">> => <<"application/json">>},
	garm_http_response:build(?OK_HTTP_CODE, Headers, Response).

-spec addAddress(map()) -> tuple().
addAddress(Populated) ->
	?LOG_INFO(#{description => "Add address operation",
						populated => Populated}),
	Headers = #{},
	garm_http_response:build(?OK_HTTP_CODE, Headers).

-spec updateAddress(map()) -> tuple().
updateAddress(Populated) ->
	?LOG_INFO(#{description => "Update address operation",
						populated => Populated}),
	Headers = #{},
	garm_http_response:build(?OK_HTTP_CODE, Headers).
