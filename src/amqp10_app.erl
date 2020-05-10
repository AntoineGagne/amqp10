-module(amqp10_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

-type start_type() :: (
        normal |
        {takeover, Node :: node()} |
        {failover, Node :: node()}
       ).

%%====================================================================
%% API
%%====================================================================

-spec start(StartType :: start_type(), StartArgs :: term()) ->
    {ok, Pid :: pid()} | {ok, Pid :: pid(), State :: term()} | {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    amqp10_sup:start_link().

-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
