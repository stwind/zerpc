-module(zerpc_ctx).

-export([new/1]).
-export([peer/1]).

-record(ctx, {
        peer :: binary()
    }).

%% ===================================================================
%% Public
%% ===================================================================

new(Peer) ->
    #ctx{peer = Peer}.

peer(#ctx{peer = Peer}) ->
    Peer.
