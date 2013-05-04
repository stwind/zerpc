-module(zerpc_req).

-export([new/2]).
-export([body/1]).
-export([reply/2]).
-export([meta/2]).
-export([meta/3]).
-export([set_meta/3]).

-record(req, {
        peer :: binary(),
        msg :: binary(),
        body :: term(),
        resp :: binary(),
        meta = dict:new() :: dict()
    }).

%% ===================================================================
%% Public
%% ===================================================================

new(Peer, Msg) ->
    Req = #req{peer = Peer, msg = Msg},
    set_meta(start_time, os:timestamp(), Req).

body(#req{body = undefined, msg = Msg} = Req) ->
    Body = parse_msg(Msg),
    {Body, Req#req{body = Body}};
body(Req) ->
    {g(body, Req), Req}.

reply(Reply, #req{peer = Peer} = Req) ->
    Req1 = set_meta(end_time, os:timestamp(), Req),
    zerpc_server:reply(Peer, Reply),
    Req1.

meta(Key, Req) ->
    meta(Key, Req, undefined).

meta(Key, #req{meta = Meta}, Default) ->
    case dict:find(Key, Meta) of
        {ok, Val} -> Val;
        error -> Default
    end.

set_meta(Key, Val, #req{meta = Meta} = Req) ->
    Req#req{meta = dict:store(Key, Val, Meta)}.

%% ===================================================================
%% Public
%% ===================================================================

parse_msg(Msg) ->
    case catch zerpc_proto:parse(Msg) of
        {'EXIT', {badarg, _}} ->
            {error, badarg};
        Body ->
            Body
    end.

g(peer, #req{peer = Peer}) -> Peer;
g(body, #req{body = Body}) -> Body.
