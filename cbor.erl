-module(cbor).

-export([decode/1]).

decode(List) when is_list(List) ->
    decode(hexstr_to_bin(List));
decode(Data) ->
    Tokens = tokenize(Data, []),
    io:format("tokens: ~p~n", [Tokens]),
    build(Tokens).

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
tokenize(<<16#5f, T/binary>>, Acc) -> tokenize(T, [str | Acc]);

% fixed length UTF-8
tokenize(<<Byte, T/binary>>, Acc) when Byte >= 16#60 andalso Byte =< 16#77 ->
    tokenize_str(Byte - 16#60, T, Acc);
% N byte-lengthed UTF-8
tokenize(<<16#78, Len:8, T/binary>>, Acc) -> tokenize_str(Len, T, Acc);
tokenize(<<16#79, Len:16, T/binary>>, Acc) -> tokenize_str(Len, T, Acc);
tokenize(<<16#7a, Len:32, T/binary>>, Acc) -> tokenize_str(Len, T, Acc);
tokenize(<<16#7b, Len:64, T/binary>>, Acc) -> tokenize_str(Len, T, Acc);
% break terminating UTF-8
tokenize(<<16#7f, T/binary>>, Acc) -> tokenize(T, [str | Acc]);

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
tokenize(<<16#c0, _T/binary>>, _Acc) -> throw(not_implemented);
% epoch-based date/time
tokenize(<<16#c1, _T/binary>>, _Acc) -> throw(not_implemented);
% positive bignum
tokenize(<<16#c2, T/binary>>, Acc) -> tokenize(T, [posbignum | Acc]);
% negative bignum
tokenize(<<16#c3, T/binary>>, Acc) -> tokenize(T, [negbignum | Acc]);

% decimal fraction
tokenize(<<16#c4, _T/binary>>, _Acc) -> throw(not_implemented);
% bigfloat
tokenize(<<16#c5, _T/binary>>, _Acc) -> throw(not_implemented);
% tagged item
tokenize(<<Byte, _T/binary>>, _Acc) when Byte >= 16#c6 andalso Byte =< 16#d4 ->
    throw(not_implemented);
% expected conversion
tokenize(<<Byte, _T/binary>>, _Acc) when Byte >= 16#d5 andalso Byte =< 16#d7 ->
    throw(not_implemented);
% N byte-tagged item
tokenize(<<Byte, _T/binary>>, _Acc) when Byte >= 16#d8 andalso Byte =< 16#db ->
    throw(not_implemented);
% simple value
tokenize(<<Byte, _T/binary>>, _Acc) when Byte >= 16#e0 andalso Byte =< 16#f3 ->
    throw(not_implemented);

% atoms
tokenize(<<16#f4, T/binary>>, Acc) -> tokenize(T, [false | Acc]);
tokenize(<<16#f5, T/binary>>, Acc) -> tokenize(T, [true | Acc]);
tokenize(<<16#f6, T/binary>>, Acc) -> tokenize(T, [null| Acc]);
tokenize(<<16#f7, T/binary>>, Acc) -> tokenize(T, [undefined | Acc]);

% simple value, one byte follows
tokenize(<<16#f8, _T/binary>>, _Acc) -> throw(not_implemented);
% N byte floats
tokenize(<<16#f9, Value:2/binary, T/binary>>, Acc) ->
    tokenize(T, [decode_hf(Value) | Acc]);
tokenize(<<16#fa, Value:32/float, T/binary>>, Acc) -> tokenize(T, [Value | Acc]);
tokenize(<<16#fb, Value:64/float, T/binary>>, Acc) -> tokenize(T, [Value | Acc]);

tokenize(<<16#ff, T/binary>>, Acc) -> tokenize(T, [break | Acc]);
tokenize(<<>>, Acc) -> Acc;
tokenize(_Data, _Acc) -> throw(invalid).

tokenize_str(Len, Data, Acc) ->
    <<Str:(Len)/binary, T/binary>> = Data,
    tokenize(T, [Str | Acc]).

build(Tokens) -> build(Tokens, []).

% handle indefinite-length items
build([break | Tail], Acc) -> build(Tail, [{break, []} | Acc]);
build([{list, break} | Tail], [{break, List} | Acc]) -> build(Tail, [List | Acc]);
build([{str, break} | Tail], [{break, List} | Acc]) ->
    build(Tail, [iolist_to_binary(List) | Acc]);
build([{map, break} | Tail], [{break, List} | Acc]) ->
    build(Tail, [build_map(List) | Acc]);
build([Token | Tail], [{break, List} | Acc]) when is_list(List) ->
    build(Tail, [{break, [Token | List]} | Acc]);

% handle fixed-length items
build([{list, N} | Tail], Acc) ->
    {NList, Tail2} = reverse_n(N, Acc),
    build(Tail, [lists:reverse(NList) | Tail2]);
build([{map, N} | Tail], Acc) ->
    {NList, Tail2} = reverse_n(N*2, Acc),
    build(Tail, [build_map(lists:reverse(NList)) | Tail2]);

%bignums
build([posbignum | Tail], [Bin | AccTail]) ->
    Len = byte_size(Bin) * 8,
    <<Num:(Len)>> = Bin,
    build(Tail, [Num | AccTail]);
build([negbignum | Tail], [Bin | AccTail]) ->
    Len = byte_size(Bin) * 8,
    <<Num:(Len)>> = Bin,
    build(Tail, [-1-Num | AccTail]);

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
decode_hf(<<Sign:1, Exp:5, Frac:10>>) ->
    Exp32 = case Exp of
        0 -> 0;
        15 -> 128;
        _ -> Exp - 15 + 127
    end,
    <<Value:32/float>> = <<Sign:1, Exp32:8, Frac:10, 0:13>>, Value.


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
