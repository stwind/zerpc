-module(server).

-export([ping/0]).
-export([pong/0]).

-export([error_return/1]).

-export([throw/1]).
-export([error/1]).

ping() -> pong.
pong() -> ping.

error_return(Reason) ->
    {error, Reason}.

throw(Error) ->
    erlang:throw(Error).

error(Reason) ->
    erlang:error(Reason).
