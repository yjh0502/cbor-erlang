-module(cbor).

-export([decode/1, decode_hf/1]).

decode(List) when is_list(List) ->
    decode(hexstr_to_bin(List));
decode(Data) ->
    build(tokenize(Data, [])).

% small integers
tokenize(<<Byte, T/binary>>, Acc) when Byte =< 16#17 -> tokenize(T, [Byte | Acc]);
% 1, 2, 4, 8 byte integers
tokenize(<<16#18, Num:8, T/binary>>, Acc) -> tokenize(T, [Num | Acc]);
tokenize(<<16#19, Num:16, T/binary>>, Acc) -> tokenize(T, [Num | Acc]);
tokenize(<<16#1a, Num:32, T/binary>>, Acc) -> tokenize(T, [Num | Acc]);
tokenize(<<16#1b, Num:64, T/binary>>, Acc) -> tokenize(T, [Num | Acc]);

% small negative integers
tokenize(<<Byte, T/binary>>, Acc) when Byte =< 16#37 -> tokenize(T, [16#1f - Byte | Acc]);
% 1, 2, 4, 8 byte negative integers
tokenize(<<16#38, Num:8, T/binary>>, Acc) -> tokenize(T, [-1-Num | Acc]);
tokenize(<<16#39, Num:16, T/binary>>, Acc) -> tokenize(T, [-1-Num | Acc]);
tokenize(<<16#3a, Num:32, T/binary>>, Acc) -> tokenize(T, [-1-Num | Acc]);
tokenize(<<16#3b, Num:64, T/binary>>, Acc) -> tokenize(T, [-1-Num | Acc]);

% fixed length strings
tokenize(<<Byte, T/binary>>, Acc) when Byte >= 16#40 andalso Byte =< 16#57 ->
    tokenize_str(Byte - 16#40, T, Acc);
% N byte-lengthed strings
tokenize(<<16#58, Len:8, T/binary>>, Acc) -> tokenize_str(Len, T, Acc);
tokenize(<<16#59, Len:16, T/binary>>, Acc) -> tokenize_str(Len, T, Acc);
tokenize(<<16#5a, Len:32, T/binary>>, Acc) -> tokenize_str(Len, T, Acc);
tokenize(<<16#5b, Len:64, T/binary>>, Acc) -> tokenize_str(Len, T, Acc);
% break terminating string
tokenize(<<16#5f, T/binary>>, Acc) -> tokenize(T, [{str, break} | Acc]);

% fixed length UTF-8
tokenize(<<Byte, T/binary>>, Acc) when Byte >= 16#60 andalso Byte =< 16#77 ->
    tokenize_str(Byte - 16#60, T, Acc);
% N byte-lengthed UTF-8
tokenize(<<16#78, Len:8, T/binary>>, Acc) -> tokenize_str(Len, T, Acc);
tokenize(<<16#79, Len:16, T/binary>>, Acc) -> tokenize_str(Len, T, Acc);
tokenize(<<16#7a, Len:32, T/binary>>, Acc) -> tokenize_str(Len, T, Acc);
tokenize(<<16#7b, Len:64, T/binary>>, Acc) -> tokenize_str(Len, T, Acc);
% break terminating UTF-8
tokenize(<<16#7f, T/binary>>, Acc) -> tokenize(T, [{str, break} | Acc]);

% fixed length array
tokenize(<<Byte, T/binary>>, Acc) when Byte >= 16#80 andalso Byte =< 16#97 ->
    tokenize(T, [{list, Byte - 16#80} | Acc]);
% N byte-lengthed array
tokenize(<<16#98, Len:8, T/binary>>, Acc) -> tokenize(T, [{list, Len} | Acc]);
tokenize(<<16#99, Len:16, T/binary>>, Acc) -> tokenize(T, [{list, Len} | Acc]);
tokenize(<<16#9a, Len:32, T/binary>>, Acc) -> tokenize(T, [{list, Len} | Acc]);
tokenize(<<16#9b, Len:64, T/binary>>, Acc) -> tokenize(T, [{list, Len} | Acc]);
% break terminating array
tokenize(<<16#9f, T/binary>>, Acc) -> tokenize(T, [{list, break} | Acc]);

% fixed length map
tokenize(<<Byte, T/binary>>, Acc) when Byte >= 16#a0 andalso Byte =< 16#b7 ->
    tokenize(T, [{map, Byte - 16#a0} | Acc]);
% N byte-lengthed map
tokenize(<<16#b8, Len:8, T/binary>>, Acc) -> tokenize(T, [{map, Len} | Acc]);
tokenize(<<16#b9, Len:16, T/binary>>, Acc) -> tokenize(T, [{map, Len} | Acc]);
tokenize(<<16#ba, Len:32, T/binary>>, Acc) -> tokenize(T, [{map, Len} | Acc]);
tokenize(<<16#bb, Len:64, T/binary>>, Acc) -> tokenize(T, [{map, Len} | Acc]);
% break terminating map
tokenize(<<16#bf, T/binary>>, Acc) -> tokenize(T, [{map, break} | Acc]);

% text-based date/time
tokenize(<<16#c0, T/binary>>, Acc) -> tokenize(T, [timetext | Acc]);
% epoch-based date/time
tokenize(<<16#c1, T/binary>>, Acc) -> tokenize(T, [timeepoch | Acc]);
% positive bignum
tokenize(<<16#c2, T/binary>>, Acc) -> tokenize(T, [posbignum | Acc]);
% negative bignum
tokenize(<<16#c3, T/binary>>, Acc) -> tokenize(T, [negbignum | Acc]);

% decimal fraction
tokenize(<<16#c4, _T/binary>>, _Acc) -> throw(not_implemented);
% bigfloat
tokenize(<<16#c5, _T/binary>>, _Acc) -> throw(not_implemented);
% tagged item: 6~20, unassigned
tokenize(<<Byte, T/binary>>, Acc) when Byte >= 16#c6 andalso Byte =< 16#d4 ->
    tokenize(T, [{tag, Byte - 16#c0} | Acc]);
% expected conversion, 21:base64, 22:base16, 23:encoded CBOR
tokenize(<<Byte, _T/binary>>, _Acc) when Byte >= 16#d5 andalso Byte =< 16#d7 ->
    throw(not_implemented);
% N byte-tagged item, 32:URL, 33:base64url, 34:base64, 35:regex, 36:mime, 55799: selfdesc
tokenize(<<16#d8, Len:8, T/binary>>, Acc) -> tokenize(T, [{tag, Len} | Acc]);
tokenize(<<16#d9, Len:16, T/binary>>, Acc) -> tokenize(T, [{tag, Len} | Acc]);
tokenize(<<16#da, Len:32, T/binary>>, Acc) -> tokenize(T, [{tag, Len} | Acc]);
tokenize(<<16#db, Len:64, T/binary>>, Acc) -> tokenize(T, [{tag, Len} | Acc]);
% simple value
tokenize(<<Byte, T/binary>>, Acc) when Byte >= 16#e0 andalso Byte =< 16#f3 ->
    tokenize(T, [{simple, Byte - 16#e0} | Acc]);

% atoms
tokenize(<<16#f4, T/binary>>, Acc) -> tokenize(T, [false | Acc]);
tokenize(<<16#f5, T/binary>>, Acc) -> tokenize(T, [true | Acc]);
tokenize(<<16#f6, T/binary>>, Acc) -> tokenize(T, [null| Acc]);
tokenize(<<16#f7, T/binary>>, Acc) -> tokenize(T, [undefined | Acc]);

% simple value, one byte follows
tokenize(<<16#f8, Value, T/binary>>, Acc) -> tokenize(T, [{simple, Value} | Acc]);
% N byte floats
tokenize(<<16#f9, Value:2/binary, T/binary>>, Acc) -> tokenize(T, [decode_hf(Value) | Acc]);
tokenize(<<16#fa, Value:4/binary, T/binary>>, Acc) -> tokenize(T, [decode_sf(Value) | Acc]);
tokenize(<<16#fb, Value:8/binary, T/binary>>, Acc) -> tokenize(T, [decode_df(Value) | Acc]);

tokenize(<<16#ff, T/binary>>, Acc) -> tokenize(T, [break | Acc]);
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

build([{list, break} | Tail], Acc) -> {Acc, Tail};
build([{str, break} | Tail], Acc) -> {iolist_to_binary(Acc), Tail};
build([{map, break} | Tail], Acc) -> {build_map(Acc), Tail};

% handle fixed-length items
build([{list, N} | Tail], Acc) ->
    {NList, Tail2} = reverse_n(N, Acc),
    build(Tail, [lists:reverse(NList) | Tail2]);
build([{map, N} | Tail], Acc) ->
    {NList, Tail2} = reverse_n(N*2, Acc),
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

build(Tokens, Stack) ->
    io:format("invalid state: token:~p, stack:~p~n", [Tokens, Stack]),
    throw(invalid).

build_map(List) -> build_map(List, #{}).
build_map([K, V | Tail], Map) -> build_map(Tail, maps:put(K, V, Map));
build_map([], Acc) -> Acc.

reverse_n(N, List) -> reverse_n(N, List, []).
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
hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a+(N-10).

int(C) when $0 =< C, C =< $9 ->
    C - $0;
int(C) when $A =< C, C =< $F ->
    C - $A + 10;
int(C) when $a =< C, C =< $f ->
    C - $a + 10.
    
to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].
 
list_to_hexstr([]) -> 
    [];
list_to_hexstr([H|T]) ->
    to_hex(H) ++ list_to_hexstr(T).

bin_to_hexstr(Bin) ->
    list_to_hexstr(binary_to_list(Bin)).

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
        % TODO
        %23(h'01020304')              d74401020304
        %24(h'6449455446')            d818456449455446
        %32("http://www.example.com") d82076687474703a2f2f7777772e6578
        %                             616d706c652e636f6d
        %h''                          40
        %h'01020304'                  4401020304
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
        % TODO
        %(_ h'0102', h'030405')       5f42010243030405ff
        {"7f657374726561646d696e67ff",
            [break, <<"ming">>, <<"strea">>, {str, break}],
            <<"streaming">>},
        {"9fff", [break, {list, break}], []},
        {"9f018202039f0405ffff",
            [break, break, 5, 4, {list, break}, 3, 2, {list, 2}, 1, {list, break}],
            [1, [2, 3], [4, 5]]},
        {"9f01820203820405ff",
            [break, 5, 4, {list, 2}, 3, 2, {list, 2}, 1, {list, break}],
            [1, [2, 3], [4, 5]]},
        {"83018202039f0405ff",
            [break, 5, 4, {list, break}, 3, 2, {list, 2}, 1, {list, 3}],
            [1, [2, 3], [4, 5]]},
        {"83019f0203ff820405",
            [5, 4, {list, 2}, break, 3, 2, {list, break}, 1, {list, 3}],
            [1, [2, 3], [4, 5]]},
        {"9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff",
            [break,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,{list,break}],
            [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]},
        {"bf61610161629f0203ffff",
            [break, break, 3, 2, {list, break}, <<"b">>, 1, <<"a">>, {map, break}],
            #{<<"a">> => 1, <<"b">> => [2, 3]}},
        {"826161bf61626163ff", [break, <<"c">>, <<"b">>, {map, break}, <<"a">>, {list, 2}],
            [<<"a">>, #{<<"b">> => <<"c">>}]},
        {"bf6346756ef563416d7421ff", [break, -2, <<"Amt">>, true, <<"Fun">>, {map, break}],
            #{<<"Fun">> => true, <<"Amt">> => -2}}
    ],
    lists:foreach(fun({Hex, Tokens, Result}) ->
        ?assertEqual(Tokens, tokenize(hexstr_to_bin(Hex), [])),
        ?assertEqual(Result, build(Tokens))
    end, Testcases).
