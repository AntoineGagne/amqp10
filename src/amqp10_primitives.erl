%% @doc
%% Encode/decode AMQP primitive types.
%%
%% Note that the following types are not supported at the moment:
%%
%% - `Decimal32'
%% - `Decimal64'
%% - `Decimal128'
%% @end
-module(amqp10_primitives).

%% API
-export([encode/1,
         decode_all/1,
         decode/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec encode(term()) -> binary().
encode(V) ->
    do_encode(V).

-spec decode_all(binary()) -> [term()].
decode_all(V) ->
    Decoded = do_decode(V),
    do_decode_all(Decoded, []).

-spec decode(binary()) -> {term(), binary()}.
decode(V) ->
    do_decode(V).

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_encode(null) ->
    <<16#40>>;
do_encode(false) ->
    <<16#41>>;
do_encode(true) ->
    <<16#42>>;
do_encode({boolean, false}) ->
    <<16#56, 16#00>>;
do_encode({boolean, true}) ->
    <<16#56, 16#01>>;
do_encode({ubyte, V}) ->
    <<16#50, V:8/unsigned>>;
do_encode({ushort, V}) ->
    <<16#60, V:16/unsigned>>;
do_encode({uint, 0}) ->
    <<16#43>>;
do_encode({uint, V}) when V > 0, V =< 16#FF ->
    <<16#52, V:8/unsigned>>;
do_encode({uint, V}) ->
    <<16#70, V:32/unsigned>>;
do_encode({ulong, 0}) ->
    <<16#44>>;
do_encode({ulong, V}) when V > 0, V =< 16#FF ->
    <<16#53, V:8/unsigned>>;
do_encode({ulong, V}) ->
    <<16#80, V:64/unsigned>>;
do_encode({byte, V}) ->
    <<16#51, V:8/signed>>;
do_encode({short, V}) ->
    <<16#61, V:16/signed>>;
do_encode({int, V}) when V >= -16#80, V =< 16#7F ->
    <<16#54, V:8/signed>>;
do_encode({int, V}) ->
    <<16#71, V:32/signed>>;
do_encode({long, V}) when V >= -16#80, V =< 16#7F ->
    <<16#55, V:8/signed>>;
do_encode({long, V}) ->
    <<16#81, V:64/signed>>;
do_encode({float, V}) ->
    <<16#72, V:32/float>>;
do_encode({double, V}) ->
    <<16#82, V:64/float>>;
do_encode({char, V}) ->
    <<16#73, V:4/binary>>;
do_encode({timestamp, V}) ->
    <<16#83, V:64/signed>>;
do_encode({uuid, V}) ->
    <<16#98, V:16/binary>>;
do_encode({binary, V}) ->
    case byte_size(V) of
        Size when Size =< 16#FF ->
            <<16#A0, Size:8, V/binary>>;
        Size ->
            <<16#B0, Size:32, V/binary>>
    end;
do_encode({string, V}) ->
    Binary = unicode:characters_to_binary(V),
    case byte_size(Binary) of
        Size when Size =< 16#FF ->
            <<16#A1, Size:8, Binary/binary>>;
        Size ->
            <<16#B1, Size:32, Binary/binary>>
    end;
do_encode({symbol, V}) ->
    case byte_size(V) of
        Size when Size =< 16#FF ->
            <<16#A3, Size:8, V/binary>>;
        Size ->
            <<16#B3, Size:32, V/binary>>
    end;
do_encode({list, []}) ->
    <<16#45>>;
do_encode({list, Elements}) ->
    Count = length(Elements),
    Encoded = << <<(do_encode(Element))/binary>> || Element <- Elements >>,
    case byte_size(Encoded) of
        Size when Count >= 16#FF; Size >= 16#FF ->
            <<16#D0, (Size + 4):32/unsigned, Count:32/unsigned, Encoded/binary>>;
        Size ->
            <<16#C0, (Size + 1):8/unsigned, Count:8/unsigned, Encoded/binary>>
    end.

do_decode_all({Element, <<>>}, Acc) ->
    [Element | Acc];
do_decode_all({Element, Rest}, Acc) ->
    Decoded = do_decode(Rest),
    do_decode_all(Decoded, [Element | Acc]).

do_decode(<<16#40, Rest/binary>>) ->
    {null, Rest};
do_decode(<<16#41, Rest/binary>>) ->
    {false, Rest};
do_decode(<<16#42, Rest/binary>>) ->
    {true, Rest};
do_decode(<<16#56, 16#00, Rest/binary>>) ->
    {{boolean, false}, Rest};
do_decode(<<16#56, 16#01, Rest/binary>>) ->
    {{boolean, true}, Rest};
do_decode(<<16#50, V:8/unsigned, Rest/binary>>) ->
    {{ubyte, V}, Rest};
do_decode(<<16#60, V:16/unsigned, Rest/binary>>) ->
    {{ushort, V}, Rest};
do_decode(<<16#43, Rest/binary>>) ->
    {{uint, 0}, Rest};
do_decode(<<16#52, V:8/unsigned, Rest/binary>>) ->
    {{uint, V}, Rest};
do_decode(<<16#70, V:32/unsigned, Rest/binary>>) ->
    {{uint, V}, Rest};
do_decode(<<16#44, Rest/binary>>) ->
    {{ulong, 0}, Rest};
do_decode(<<16#53, V:8/unsigned, Rest/binary>>) ->
    {{ulong, V}, Rest};
do_decode(<<16#80, V:64/unsigned, Rest/binary>>) ->
    {{ulong, V}, Rest};
do_decode(<<16#51, V:8/signed, Rest/binary>>) ->
    {{byte, V}, Rest};
do_decode(<<16#61, V:16/signed, Rest/binary>>) ->
    {{short, V}, Rest};
do_decode(<<16#54, V:8/signed, Rest/binary>>) ->
    {{int, V}, Rest};
do_decode(<<16#71, V:32/signed, Rest/binary>>) ->
    {{int, V}, Rest};
do_decode(<<16#55, V:8/signed, Rest/binary>>) ->
    {{long, V}, Rest};
do_decode(<<16#81, V:64/signed, Rest/binary>>) ->
    {{long, V}, Rest};
do_decode(<<16#72, V:32/float, Rest/binary>>) ->
    {{float, V}, Rest};
do_decode(<<16#82, V:64/float, Rest/binary>>) ->
    {{double, V}, Rest};
do_decode(<<16#73, V:4/binary, Rest/binary>>) ->
    {{char, V}, Rest};
do_decode(<<16#83, V:64/signed, Rest/binary>>) ->
    {{timestamp, V}, Rest};
do_decode(<<16#98, V:16/binary, Rest/binary>>) ->
    {{uuid, V}, Rest};
do_decode(<<16#A0, Size:8, V:Size/binary, Rest/binary>>) ->
    {{binary, V}, Rest};
do_decode(<<16#B0, Size:32, V:Size/binary, Rest/binary>>) ->
    {{binary, V}, Rest};
do_decode(<<16#A1, Size:8, V:Size/binary, Rest/binary>>) ->
    {{string, unicode:characters_to_list(V)}, Rest};
do_decode(<<16#B1, Size:32, V:Size/binary, Rest/binary>>) ->
    {{string, unicode:characters_to_list(V)}, Rest};
do_decode(<<16#A3, Size:8, V:Size/binary, Rest/binary>>) ->
    {{symbol, V}, Rest};
do_decode(<<16#B3, Size:32, V:Size/binary, Rest/binary>>) ->
    {{symbol, V}, Rest};
do_decode(<<16#45, Rest/binary>>) ->
    {{list, []}, Rest};
do_decode(<<16#C0, Size:8/unsigned, Remainder:Size/binary, Rest/binary>>) ->
    <<Count:8, Binary/binary>> = Remainder,
    {{list, decode_compound(Count, Binary, [])}, Rest};
do_decode(<<16#D0, Size:32/unsigned, Remainder:Size/binary, Rest/binary>>) ->
    <<Count:32, Binary/binary>> = Remainder,
    {{list, decode_compound(Count, Binary, [])}, Rest}.

decode_compound(0, <<>>, Elements) ->
    lists:reverse(Elements);
decode_compound(Count, Binary, Elements) ->
    {Element, Rest} = do_decode(Binary),
    decode_compound(Count - 1, Rest, [Element | Elements]).
