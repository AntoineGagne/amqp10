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
                [Decoded] = amqp10_primitives:decode(Encoded),
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
    oneof([primitive_null(),
           primitive_boolean(),
           primitive_ubyte(),
           primitive_uint(),
           primitive_ulong(),
           primitive_byte(),
           primitive_short(),
           primitive_int(),
           primitive_long()]).

primitive_null() ->
    exactly(null).

primitive_boolean() ->
    ?LET(V, boolean(),
         ?LET(Boolean, oneof([{boolean, V}, V]), Boolean)).

primitive_ubyte() ->
    ?LET(V, range(0, 16#FF), {ubyte, V}).

primitive_uint() ->
    ?LET(V, range(0, 16#FFFFFFFF), {uint, V}).

primitive_ulong() ->
    ?LET(V, range(0, 16#FFFFFFFFFFFFFFFF), {ulong, V}).

primitive_byte() ->
    ?LET(V, range(-16#80, 16#7F), {byte, V}).

primitive_short() ->
    ?LET(V, range(-16#8000, 16#7FFF), {short, V}).

primitive_int() ->
    ?LET(V, range(-16#80000000, 16#7FFFFFFF), {int, V}).

primitive_long() ->
    ?LET(V, range(-16#8000000000000000, 16#7FFFFFFFFFFFFFFF), {long, V}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
