-module(cbor).

-export([decode/1]).

decode(List) when is_list(List) ->
    decode(hexstr_to_bin(List));
decode(Data) ->
    build(tokenize(Data, [])).

-define(TK(N, EXPR), tokenize(<<N, T/binary>>,Acc) -> EXPR).
-define(TK_ITEM(N, ITEM), ?TK(N, tokenize(T, [(ITEM)|Acc]))).
-define(TK_SM(N), ?TK_ITEM(N, N)).
-define(TK_NM(N), ?TK_ITEM(N, 16#1f-N)).
-define(TK_STR(N), ?TK(N, tokenize_str((N-16#40), T, Acc))).
-define(TK_UTF8(N), ?TK(N, tokenize_str((N-16#60), T, Acc))).
-define(TK_ARR(N), ?TK_ITEM(N, {list, N-16#80})).
-define(TK_MAP(N), ?TK_ITEM(N, {map, N-16#a0})).
-define(TK_TAG(N), ?TK_ITEM(N, {tag, N - 16#c0})).
-define(TK_SIMPLE(N), ?TK_ITEM(N, {simple, N - 16#e0})).
-define(TK_NI(N), tokenize(<<N, _/binary>>,_) -> throw(not_implemented)).

-define(TK_FIXED(BASE, NAME),
    ?NAME((BASE+0)); ?NAME((BASE+1)); ?NAME((BASE+2)); ?NAME((BASE+3));
    ?NAME((BASE+4)); ?NAME((BASE+5)); ?NAME((BASE+6)); ?NAME((BASE+7));
    ?NAME((BASE+8)); ?NAME((BASE+9)); ?NAME((BASE+10)); ?NAME((BASE+11));
    ?NAME((BASE+12)); ?NAME((BASE+13)); ?NAME((BASE+14)); ?NAME((BASE+15));
    ?NAME((BASE+16)); ?NAME((BASE+17)); ?NAME((BASE+18)); ?NAME((BASE+19));
    ?NAME((BASE+20)); ?NAME((BASE+21)); ?NAME((BASE+22)); ?NAME((BASE+23))
).
-define(TK_LENGTHED(BASE, EXPR),
    tokenize(<<(BASE), Num:8, T/binary>>, Acc) -> EXPR;
    tokenize(<<(BASE+1), Num:16, T/binary>>, Acc) -> EXPR;
    tokenize(<<(BASE+2), Num:32, T/binary>>, Acc) -> EXPR;
    tokenize(<<(BASE+3), Num:64, T/binary>>, Acc) -> EXPR).

-define(TK_LENGTHED_ITEM(BASE, ITEM), ?TK_LENGTHED(BASE, tokenize(T, [(ITEM) | Acc]))).

?TK_FIXED(0, TK_SM); % small integers
?TK_LENGTHED_ITEM(16#18, Num); % 1, 2, 4, 8 byte integers

?TK_FIXED(16#20, TK_NM); % small negative integers
?TK_LENGTHED_ITEM(16#38, -1-Num); % 1, 2, 4, 8 byte negative integers

?TK_FIXED(16#40, TK_STR); % small string
?TK_LENGTHED(16#58, tokenize_str(Num, T, Acc)); % N byte-lengthed string
?TK(16#5f, tokenize(T, [strb | Acc])); % break terminating string

?TK_FIXED(16#60, TK_UTF8); % small UTF-8
?TK_LENGTHED(16#78, tokenize_str(Num, T, Acc)); % N byte-lengthed UTF-8
?TK(16#7f, tokenize(T, [strb | Acc])); % break terminating UTF-8

?TK_FIXED(16#80, TK_ARR); % small array
?TK_LENGTHED_ITEM(16#98, {list, Num}); % N byte-lengthed array
?TK(16#9f, tokenize(T, [listb | Acc])); % break terminating array

?TK_FIXED(16#a0, TK_MAP); % small map
?TK_LENGTHED_ITEM(16#b8, {map, Num}); % N byte-lengthed map
?TK(16#bf, tokenize(T, [mapb | Acc])); % break terminating array

?TK(16#c0, tokenize(T, [timetext | Acc])); % text-based date/time
?TK(16#c1, tokenize(T, [timeepoch | Acc])); % epoch-based date/time
?TK(16#c2, tokenize(T, [posbignum | Acc])); % positive bignum
?TK(16#c3, tokenize(T, [negbignum | Acc])); % negative bignum

?TK_NI(16#c4); % decimal fraction
?TK_NI(16#c5); % bigfloat

% tagged item: 6~20:unassigned, 21:base64, 22:base16, 23:encoded CBOR
?TK_TAG(16#c6); ?TK_TAG(16#c7); ?TK_TAG(16#c8); ?TK_TAG(16#c9);
?TK_TAG(16#ca); ?TK_TAG(16#cb); ?TK_TAG(16#cc); ?TK_TAG(16#cd);
?TK_TAG(16#ce); ?TK_TAG(16#cf); ?TK_TAG(16#d0); ?TK_TAG(16#d1);
?TK_TAG(16#d2); ?TK_TAG(16#d3); ?TK_TAG(16#d4); ?TK_TAG(16#d5);
?TK_TAG(16#d6); ?TK_TAG(16#d7);
% N byte-tagged item, 32:URL, 33:base64url, 34:base64, 35:regex, 36:mime, 55799: selfdesc
?TK_LENGTHED_ITEM(16#d8, {tag, Num});
% simple value
?TK_SIMPLE(16#e0); ?TK_SIMPLE(16#e1); ?TK_SIMPLE(16#e2); ?TK_SIMPLE(16#e3);
?TK_SIMPLE(16#e4); ?TK_SIMPLE(16#e5); ?TK_SIMPLE(16#e6); ?TK_SIMPLE(16#e7);
?TK_SIMPLE(16#e8); ?TK_SIMPLE(16#e9); ?TK_SIMPLE(16#ea); ?TK_SIMPLE(16#eb);
?TK_SIMPLE(16#ec); ?TK_SIMPLE(16#ed); ?TK_SIMPLE(16#ee); ?TK_SIMPLE(16#ef);
?TK_SIMPLE(16#f0); ?TK_SIMPLE(16#f1); ?TK_SIMPLE(16#f2); ?TK_SIMPLE(16#f3);

% atoms
?TK_ITEM(16#f4, false);
?TK_ITEM(16#f5, true);
?TK_ITEM(16#f6, null);
?TK_ITEM(16#f7, undefined);

% simple value, one byte follows
tokenize(<<16#f8, Value, T/binary>>, Acc) -> tokenize(T, [{simple, Value} | Acc]);

% N byte floats
tokenize(<<16#f9, Value:2/binary, T/binary>>, Acc) -> tokenize(T, [decode_hf(Value) | Acc]);
tokenize(<<16#fa, Value:4/binary, T/binary>>, Acc) -> tokenize(T, [decode_sf(Value) | Acc]);
tokenize(<<16#fb, Value:8/binary, T/binary>>, Acc) -> tokenize(T, [decode_df(Value) | Acc]);

?TK_ITEM(16#ff, break);
tokenize(<<>>, Acc) -> Acc;
tokenize(_Data, _Acc) -> throw(invalid).

tokenize_str(Len, Data, Acc) ->
    <<Str:(Len)/binary, T/binary>> = Data,
    tokenize(T, [Str | Acc]).

build(Tokens) -> build(Tokens, []).

% handle indefinite-length items
build([break | Tail], Acc) ->
    {Item, Tail2} = build(Tail, []),
    build(Tail2, [Item | Acc]);

build([listb | Tail], Acc) -> {Acc, Tail};
build([strb | Tail], Acc) -> {iolist_to_binary(Acc), Tail};
build([mapb | Tail], Acc) -> {build_map(Acc), Tail};

% handle fixed-length items
build([{list, N} | Tail], Acc) ->
    {NList, Tail2} = reverse_n(N, Acc, []),
    build(Tail, [lists:reverse(NList) | Tail2]);
build([{map, N} | Tail], Acc) ->
    {NList, Tail2} = reverse_n(N*2, Acc, []),
    build(Tail, [build_map(lists:reverse(NList)) | Tail2]);

% bignums
build([posbignum | Tail], [Bin | AccTail]) ->
    Len = byte_size(Bin) * 8,
    <<Num:(Len)>> = Bin,
    build(Tail, [Num | AccTail]);
build([negbignum | Tail], [Bin | AccTail]) ->
    Len = byte_size(Bin) * 8,
    <<Num:(Len)>> = Bin,
    build(Tail, [-1-Num | AccTail]);

% times
build([timetext | Tail], [Value | AccTail]) -> build(Tail, [{timetext, Value} | AccTail]);
build([timeepoch | Tail], [Value | AccTail]) -> build(Tail, [{timeepoch, Value} | AccTail]);
build([{tag, N} | Tail], [Value | AccTail]) -> build(Tail, [{tag, N, Value} | AccTail]);

build([Token | Tail], Acc) -> build(Tail, [Token | Acc]);

build([], [Item]) -> Item;

build(_, _) -> throw(invalid).

build_map(List) -> build_map(List, []).
build_map([K, V | Tail], Acc) -> build_map(Tail, [{K, V} | Acc]);
build_map([], Acc) -> maps:from_list(Acc).

reverse_n(0, List, Acc) -> {Acc, List};
reverse_n(N, [Hd | Tl], Acc) -> reverse_n(N-1, Tl, [Hd | Acc]).

% Erlang does not support half presition floating point, so emulate it
decode_hf(<<0:1, 0:5, 0:10>>) -> 0.0;
decode_hf(<<1:1, 0:5, 0:10>>) -> -0.0;
decode_hf(<<Sign:1, 0:5, Frac:10>>) ->
    {Frac2, Count} = hf_norm(Frac, 0),
    <<Value:32/float>> = <<Sign:1, (-14-Count+127):8, Frac2:10, 0:13>>, Value;
decode_hf(<<0:1, 31:5, 0:10>>) -> inf;
decode_hf(<<1:1, 31:5, 0:10>>) -> neginf;
decode_hf(<<_:1, 31:5, _:10>>) -> nan;
decode_hf(<<Sign:1, Exp:5, Frac:10>>) ->
    Exp32 = Exp - 15 + 127,
    <<Value:32/float>> = <<Sign:1, Exp32:8, Frac:10, 0:13>>, Value.

% Erlang does not support binary matching for nan/inf, so emulate it
decode_sf(<<Value:32/float>>)-> Value;
decode_sf(<<0:1, 255:8, 0:23>>) -> inf;
decode_sf(<<1:1, 255:8, 0:23>>) -> neginf;
decode_sf(<<_:1, 255:8, _:23>>) -> nan.

decode_df(<<Value:64/float>>)-> Value;
decode_df(<<0:1, 2047:11, 0:52>>) -> inf;
decode_df(<<1:1, 2047:11, 0:52>>) -> neginf;
decode_df(<<_:1, 2047:11, _:52>>) -> nan.

hf_norm(Frac, Count) when Frac < 1024 -> hf_norm(Frac * 2, Count+1);
hf_norm(Frac, Count) -> {Frac, Count}.

%% helper functions
int(C) when $0 =< C, C =< $9 ->
    C - $0;
int(C) when $A =< C, C =< $F ->
    C - $A + 10;
int(C) when $a =< C, C =< $f ->
    C - $a + 10.

hexstr_to_bin(S) ->
    list_to_binary(hexstr_to_list(S)).

hexstr_to_list([X,Y|T]) ->
    [int(X)*16 + int(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
    [].

% unit tests
-include_lib("eunit/include/eunit.hrl").

rfc_value_test() ->
    % testcases from RFC7049, single value
    Testcases = [
        {"00", 0},
        {"01", 1},
        {"0a", 10},
        {"17", 23},
        {"1818", 24},
        {"1819", 25},
        {"1864", 100},
        {"1864", 100},
        {"1903e8", 1000},
        {"1903e8", 1000},
        {"1a000f4240", 1000000},
        {"1b000000e8d4a51000", 1000000000000},
        {"1bffffffffffffffff", 18446744073709551615},
        {"c249010000000000000000", 18446744073709551616},
        {"3bffffffffffffffff", -18446744073709551616},
        {"c349010000000000000000", -18446744073709551617},
        {"20", -1},
        {"29", -10},
        {"3863", -100},
        {"3903e7", -1000},
        {"f90000", 0.0},
        {"f98000", -0.0},
        {"f93c00", 1.0},
        {"fb3ff199999999999a", 1.1},
        {"f93e00", 1.5},
        {"f97bff", 65504.0},
        {"fa47c35000", 100000.0},
        {"fa7f7fffff", 3.4028234663852886e+38},
        {"fb7e37e43c8800759c", 1.0e+300},
        {"f90001", 5.960464477539063e-8},
        {"f90400", 0.00006103515625},
        {"f9c400", -4.0},
        {"fbc010666666666666", -4.1},
        {"f97c00", inf},
        {"f97e00", nan},
        {"f9fc00", neginf},
        {"fa7f800000", inf},
        {"fa7fc00000", nan},
        {"faff800000", neginf},
        {"fb7ff0000000000000", inf},
        {"fb7ff8000000000000", nan},
        {"fbfff0000000000000", neginf},
        {"f4", false},
        {"f5", true},
        {"f6", null},
        {"f7", undefined},
        {"f0", {simple, 16}},
        {"f818", {simple, 24}},
        {"f8ff", {simple, 255}},
        {"c074323031332d30332d32315432303a30343a30305a", {timetext, <<"2013-03-21T20:04:00Z">>}},
        {"c11a514b67b0", {timeepoch, 1363896240}},
        {"c1fb41d452d9ec200000", {timeepoch, 1363896240.5}},
        {"d74401020304", {tag, 23, <<1, 2, 3, 4>>}},
        {"d818456449455446", {tag, 24, <<"dIETF">>}},
        {"d82076687474703a2f2f7777772e6578616d706c652e636f6d", {tag, 32, <<"http://www.example.com">>}},
        {"40", <<"">>},
        {"4401020304", <<1, 2, 3, 4>>},
        {"60", <<"">>},
        {"6161", <<"a">>},
        {"6449455446", <<"IETF">>},
        {"62225c", <<"\"\\">>},
        % All unicodes are decoded to binaries, to check binary values
        {"62c3bc", <<16#c3, 16#bc>>},
        {"63e6b0b4", <<16#e6, 16#b0, 16#b4>>},
        {"64f0908591", <<16#f0, 16#90, 16#85, 16#91>>}
    ],
    lists:foreach(fun({Hex, Result}) ->
        ?assertEqual(Result, decode(hexstr_to_bin(Hex)))
    end, Testcases).

rfc_data_test() ->
    % testcases from RFC7049, datastructures
    Testcases = [
        {"80", [{list, 0}], []},
        {"83010203", [3, 2, 1, {list, 3}], [1, 2, 3]},
        {"8301820203820405", [5, 4, {list, 2}, 3, 2, {list, 2}, 1, {list, 3}], [1, [2, 3], [4, 5]]},
        {"98190102030405060708090a0b0c0d0e0f101112131415161718181819",
            [25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,{list,25}],
            [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]},
        {"a0", [{map, 0}], #{}},
        {"a201020304", [4, 3, 2, 1, {map, 2}], #{1=>2, 3=>4}},
        {"a26161016162820203",
            [3, 2, {list, 2}, <<"b">>, 1, <<"a">>, {map, 2}],
            #{<<"a">> => 1, <<"b">> => [2, 3]}},
        {"826161a161626163",
            [<<"c">>, <<"b">>, {map, 1}, <<"a">>, {list, 2}],
            [<<"a">>, #{<<"b">> => <<"c">>}]},
        {"a56161614161626142616361436164614461656145",
            [<<"E">>, <<"e">>, <<"D">>, <<"d">>, <<"C">>, <<"c">>,
                <<"B">>, <<"b">>, <<"A">>, <<"a">>, {map, 5}],
            #{<<"a">> => <<"A">>, <<"b">> => <<"B">>, <<"c">> => <<"C">>,
                <<"d">> => <<"D">>, <<"e">> => <<"E">>}},
        {"5f42010243030405ff", [break, <<3, 4, 5>>, <<1, 2>>, strb], <<1, 2, 3, 4, 5>>},
        {"7f657374726561646d696e67ff",
            [break, <<"ming">>, <<"strea">>, strb],
            <<"streaming">>},
        {"9fff", [break, listb], []},
        {"9f018202039f0405ffff",
            [break, break, 5, 4, listb, 3, 2, {list, 2}, 1, listb],
            [1, [2, 3], [4, 5]]},
        {"9f01820203820405ff",
            [break, 5, 4, {list, 2}, 3, 2, {list, 2}, 1, listb],
            [1, [2, 3], [4, 5]]},
        {"83018202039f0405ff",
            [break, 5, 4, listb, 3, 2, {list, 2}, 1, {list, 3}],
            [1, [2, 3], [4, 5]]},
        {"83019f0203ff820405",
            [5, 4, {list, 2}, break, 3, 2, listb, 1, {list, 3}],
            [1, [2, 3], [4, 5]]},
        {"9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff",
            [break,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,listb],
            [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]},
        {"bf61610161629f0203ffff",
            [break, break, 3, 2, listb, <<"b">>, 1, <<"a">>, mapb],
            #{<<"a">> => 1, <<"b">> => [2, 3]}},
        {"826161bf61626163ff", [break, <<"c">>, <<"b">>, mapb, <<"a">>, {list, 2}],
            [<<"a">>, #{<<"b">> => <<"c">>}]},
        {"bf6346756ef563416d7421ff", [break, -2, <<"Amt">>, true, <<"Fun">>, mapb],
            #{<<"Fun">> => true, <<"Amt">> => -2}}
    ],
    lists:foreach(fun({Hex, Tokens, Result}) ->
        ?assertEqual(Tokens, tokenize(hexstr_to_bin(Hex), [])),
        ?assertEqual(Result, build(Tokens))
    end, Testcases).

bench_test() ->
    Tc = [
        {"nd list", "9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff"},
        {"fixed list", "98190102030405060708090a0b0c0d0e0f101112131415161718181819"},
        {"fixed map", "a56161614161626142616361436164614461656145"},
        {"nested", "bf61610161629f0203ffff"}
    ],
    N = 1000,
    lists:foreach(fun({Name, Hex}) ->
        Bin = hexstr_to_bin(Hex),
        {Usec, ok} = timer:tc(fun() -> repeat_decode_n(N, Bin) end),
        io:format("~s: ~p #/s ~.4f us/# ~.2f MB/s~n",
            [Name, N * 1000000 div Usec, Usec / N, byte_size(Bin) * N / Usec])
    end, Tc).

repeat_decode_n(0, _) -> ok;
repeat_decode_n(N, Bin) -> decode(Bin), repeat_decode_n(N-1, Bin).
