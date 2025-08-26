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

-module(garm_utils).


%% =============================================================================
%% public functions
%% =============================================================================

-export([to_binary/1]).
-export([to_list/1]).
-export([to_float/1]).
-export([to_int/1]).

-export([to_lower/1]).
-export([to_upper/1]).

-export([to_header/1]).
-export([to_qs/1]).
-export([to_binding/1]).

-export([get_opt/2]).
-export([get_opt/3]).

-export([set_resp_headers/2]).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec to_binary(iodata() | atom() | number()) -> binary().
to_binary(V) when is_binary(V)  -> V;
to_binary(V) when is_list(V)    -> iolist_to_binary(V);
to_binary(V) when is_atom(V)    -> atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_float(V)   -> float_to_binary(V).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec to_list(iodata() | atom() | number()) -> string().
to_list(V) when is_list(V)    -> V;
to_list(V)            -> binary_to_list(to_binary(V)).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec to_float(iodata()) -> number().
to_float(V) ->
  Data = iolist_to_binary([V]),
  case binary:split(Data, <<$.>>) of
    [Data] ->
      binary_to_integer(Data);
    [<<>>, _] ->
      binary_to_float(<<$0, Data/binary>>);
    _ ->
      binary_to_float(Data)
  end.

%%

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec to_int(integer() | binary() | list()) -> integer().
to_int(Data) when is_integer(Data) ->
  Data;
to_int(Data) when is_binary(Data) ->
  binary_to_integer(Data);
to_int(Data) when is_list(Data) ->
  list_to_integer(Data).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec set_resp_headers([{binary(), iodata()}], cowboy_req:req()) -> cowboy_req:req().
set_resp_headers([], Req) ->
  Req;
set_resp_headers([{K, V} | T], Req0) ->
  Req = cowboy_req:set_resp_header(K, V, Req0),
  set_resp_headers(T, Req).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec to_header(iodata() | atom() | number()) -> binary().
to_header(Name) ->
  Prepared = to_binary(Name),
  to_lower(Prepared).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec to_qs(iodata() | atom() | number()) -> binary().
to_qs(Name) ->
  to_binary(Name).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec to_binding(iodata() | atom() | number()) -> atom().
to_binding(Name) ->
  Prepared = to_binary(Name),
  binary_to_atom(Prepared, utf8).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_opt(any(), []) -> any().
get_opt(Key, Opts) ->
  get_opt(Key, Opts, undefined).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec get_opt(any(), [], any()) -> any().
get_opt(Key, Opts, Default) ->
  case lists:keyfind(Key, 1, Opts) of
    {_, Value} -> Value;
    false -> Default
  end.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec to_lower(binary()) -> binary().
to_lower(S) ->
  to_case(lower, S, <<>>).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec to_upper(binary()) -> binary().
to_upper(S) ->
  to_case(upper, S, <<>>).

%% =============================================================================
%% private functions
%% =============================================================================

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
to_case(_Case, <<>>, Acc) ->
  Acc;

to_case(_Case, <<C, _/binary>>, _Acc) when C > 127 ->
  error(badarg);

to_case(Case = lower, <<C, Rest/binary>>, Acc) ->
  to_case(Case, Rest, <<Acc/binary, (to_lower_char(C))>>);

to_case(Case = upper, <<C, Rest/binary>>, Acc) ->
  to_case(Case, Rest, <<Acc/binary, (to_upper_char(C))>>).

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
to_lower_char(C) when is_integer(C), $A =< C, C =< $Z ->
  C + 32;
to_lower_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 ->
  C + 32;
to_lower_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE ->
  C + 32;
to_lower_char(C) ->
  C.

%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
to_upper_char(C) when is_integer(C), $a =< C, C =< $z ->
  C - 32;
to_upper_char(C) when is_integer(C), 16#E0 =< C, C =< 16#F6 ->
  C - 32;
to_upper_char(C) when is_integer(C), 16#F8 =< C, C =< 16#FE ->
  C - 32;
to_upper_char(C) ->
  C.
