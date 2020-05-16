-module(amqp10_primitives).

%% API
-export([encode/1,
         decode/1]).

%%%===================================================================
%%% API
%%%===================================================================

encode(null) ->
    <<16#40>>;
encode(false) ->
    <<16#41>>;
encode(true) ->
    <<16#42>>;
encode({boolean, false}) ->
    <<16#56, 16#00>>;
encode({boolean, true}) ->
    <<16#56, 16#01>>;
encode({ubyte, V}) ->
    <<16#50, V:8/unsigned>>;
encode({ushort, V}) ->
    <<16#60, V:16/unsigned>>;
encode({uint, 0}) ->
    <<16#43>>;
encode({uint, V}) when V > 0, V =< 16#FF ->
    <<16#52, V:8/unsigned>>;
encode({uint, V}) ->
    <<16#70, V:32/unsigned>>;
encode({ulong, 0}) ->
    <<16#44>>;
encode({ulong, V}) when V > 0, V =< 16#FF ->
    <<16#53, V:8/unsigned>>;
encode({ulong, V}) ->
    <<16#80, V:64/unsigned>>;
encode({byte, V}) ->
    <<16#51, V:8/signed>>;
encode({short, V}) ->
    <<16#61, V:16/signed>>;
encode({int, V}) when V >= -16#80, V =< 16#7F ->
    <<16#54, V:8/signed>>;
encode({int, V}) ->
    <<16#71, V:32/signed>>;
encode({long, V}) when V >= -16#80, V =< 16#7F ->
    <<16#55, V:8/signed>>;
encode({long, V}) ->
    <<16#81, V:64/signed>>;
encode({float, V}) ->
    <<16#72, V:32/float>>;
encode({double, V}) ->
    <<16#82, V:64/float>>;
encode({char, V}) ->
    <<16#73, V/utf32>>;
encode({timestamp, V}) ->
    <<16#83, V:64/signed>>;
encode({uuid, V}) ->
    <<16#98, V:16/binary>>;
encode({binary, V}) ->
    case byte_size(V) of
        Size when Size =< 16#FF ->
            <<16#A0, Size:8, V/binary>>;
        Size ->
            <<16#B0, Size:32, V/binary>>
    end.

decode(V) ->
    do_decode(V, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_decode(<<>>, Acc) ->
    Acc;
do_decode(<<16#40, Rest/binary>>, Acc) ->
    do_decode(Rest, [null | Acc]);
do_decode(<<16#41, Rest/binary>>, Acc) ->
    do_decode(Rest, [false | Acc]);
do_decode(<<16#42, Rest/binary>>, Acc) ->
    do_decode(Rest, [true | Acc]);
do_decode(<<16#56, 16#00, Rest/binary>>, Acc) ->
    do_decode(Rest, [{boolean, false} | Acc]);
do_decode(<<16#56, 16#01, Rest/binary>>, Acc) ->
    do_decode(Rest, [{boolean, true} | Acc]);
do_decode(<<16#50, V:8/unsigned, Rest/binary>>, Acc) ->
    do_decode(Rest, [{ubyte, V} | Acc]);
do_decode(<<16#60, V:16/unsigned, Rest/binary>>, Acc) ->
    do_decode(Rest, [{ushort, V} | Acc]);
do_decode(<<16#43, Rest/binary>>, Acc) ->
    do_decode(Rest, [{uint, 0} | Acc]);
do_decode(<<16#52, V:8/unsigned, Rest/binary>>, Acc) ->
    do_decode(Rest, [{uint, V} | Acc]);
do_decode(<<16#70, V:32/unsigned, Rest/binary>>, Acc) ->
    do_decode(Rest, [{uint, V} | Acc]);
do_decode(<<16#44, Rest/binary>>, Acc) ->
    do_decode(Rest, [{ulong, 0} | Acc]);
do_decode(<<16#53, V:8/unsigned, Rest/binary>>, Acc) ->
    do_decode(Rest, [{ulong, V} | Acc]);
do_decode(<<16#80, V:64/unsigned, Rest/binary>>, Acc) ->
    do_decode(Rest, [{ulong, V} | Acc]);
do_decode(<<16#51, V:8/signed, Rest/binary>>, Acc) ->
    do_decode(Rest, [{byte, V} | Acc]);
do_decode(<<16#61, V:16/signed, Rest/binary>>, Acc) ->
    do_decode(Rest, [{short, V} | Acc]);
do_decode(<<16#54, V:8/signed, Rest/binary>>, Acc) ->
    do_decode(Rest, [{int, V} | Acc]);
do_decode(<<16#71, V:32/signed, Rest/binary>>, Acc) ->
    do_decode(Rest, [{int, V} | Acc]);
do_decode(<<16#55, V:8/signed, Rest/binary>>, Acc) ->
    do_decode(Rest, [{long, V} | Acc]);
do_decode(<<16#81, V:64/signed, Rest/binary>>, Acc) ->
    do_decode(Rest, [{long, V} | Acc]);
do_decode(<<16#72, V:32/float, Rest/binary>>, Acc) ->
    do_decode(Rest, [{float, V} | Acc]);
do_decode(<<16#82, V:64/float, Rest/binary>>, Acc) ->
    do_decode(Rest, [{double, V} | Acc]).
