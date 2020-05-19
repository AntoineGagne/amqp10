-module(prop_amqp10_primitives).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

%%%===================================================================
%%% Properties
%%%===================================================================

prop_encode_and_decode_are_inverse() ->
    ?FORALL(Primitive, primitive(),
            begin
                Encoded = amqp10_primitives:encode(Primitive),
                {Decoded, <<>>} = amqp10_primitives:decode(Encoded),
                ?WHENFAIL(io:format("Primitive: ~p~n"
                                    "Encoded: ~p~n"
                                    "Decoded: ~p~n",
                                    [Primitive, [Encoded], Decoded]),
                          equals(Primitive, Decoded))
            end).

%%%===================================================================
%%% Generators
%%%===================================================================

primitive() ->
    oneof([oneof(non_growing_primitives()), oneof(growing_primitives())]).

primitive_null() ->
    exactly(null).

primitive_boolean() ->
    ?LET(V, boolean(),
         ?LET(Boolean, oneof([{boolean, V}, V]), Boolean)).

primitive_ubyte() ->
    ?LETSHRINK(V, range(0, 16#FF), {ubyte, V}).

primitive_ushort() ->
    ?LETSHRINK(V, range(0, 16#FFFF), {ushort, V}).

primitive_uint() ->
    ?LETSHRINK(V, range(0, 16#FFFFFFFF), {uint, V}).

primitive_ulong() ->
    ?LETSHRINK(V, range(0, 16#FFFFFFFFFFFFFFFF), {ulong, V}).

primitive_byte() ->
    ?LETSHRINK(V, range(-16#80, 16#7F), {byte, V}).

primitive_short() ->
    ?LETSHRINK(V, range(-16#8000, 16#7FFF), {short, V}).

primitive_int() ->
    ?LETSHRINK(V, range(-16#80000000, 16#7FFFFFFF), {int, V}).

primitive_long() ->
    ?LETSHRINK(V, range(-16#8000000000000000, 16#7FFFFFFFFFFFFFFF), {long, V}).

primitive_timestamp() ->
    ?LETSHRINK(V, range(-16#8000000000000000, 16#7FFFFFFFFFFFFFFF), {timestamp, V}).

primitive_binary() ->
    ?LET(V, binary(), {binary, V}).

primitive_string() ->
    ?LET(V, string(), {string, V}).

primitive_symbol() ->
    ?LET(Symbol, ascii_binary(), {symbol, Symbol}).

primitive_list() ->
    ?LET(List, list(oneof(non_growing_primitives())), {list, List}).

ascii_binary() ->
    ?LET(Characters, list(byte()), list_to_binary(Characters)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

growing_primitives() ->
    [primitive_list()].

non_growing_primitives() ->
    [primitive_null(),
     primitive_boolean(),
     primitive_ubyte(),
     primitive_ushort(),
     primitive_uint(),
     primitive_ulong(),
     primitive_byte(),
     primitive_short(),
     primitive_int(),
     primitive_long(),
     primitive_timestamp(),
     primitive_binary(),
     primitive_string(),
     primitive_symbol()].
