-module(server).

-export([ping/0]).
-export([pong/0]).

-export([error/1]).

ping() -> pong.
pong() -> ping.

error(Reason) ->
    {error, Reason}.
