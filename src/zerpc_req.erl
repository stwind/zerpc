-module(zerpc_req).

-export([new/1]).
-export([body/1]).
-export([reply/2]).
-export([meta/2]).
-export([meta/3]).
-export([resp/1]).
-export([set_meta/3]).

-record(req, {
        msg :: binary(),
        body :: term(),
        resp :: binary(),
        meta = dict:new() :: dict()
    }).

%% ===================================================================
%% Public
%% ===================================================================

new(Msg) ->
    set_meta(start_time, os:timestamp(), #req{msg = Msg}).

body(#req{body = undefined, msg = Msg} = Req) ->
    Body = parse_msg(Msg),
    {Body, Req#req{body = Body}};
body(Req) ->
    {g(body, Req), Req}.

reply(Reply, Req) ->
    Req1 = set_meta(end_time, os:timestamp(), Req),
    %zerpc_server:reply(Peer, zerpc_proto:encode(Reply)),
    Req1#req{resp = Reply}.

meta(Key, Req) ->
    meta(Key, Req, undefined).

meta(Key, #req{meta = Meta}, Default) ->
    case dict:find(Key, Meta) of
        {ok, Val} -> Val;
        error -> Default
    end.

resp(Resp) ->
    g(resp, Resp).

set_meta(Key, Val, #req{meta = Meta} = Req) ->
    Req#req{meta = dict:store(Key, Val, Meta)}.

%% ===================================================================
%% Private
%% ===================================================================

parse_msg(Msg) ->
    case catch zerpc_proto:decode(Msg) of
        {'EXIT', {badarg, _}} ->
            {error, badarg};
        Body ->
            Body
    end.

g(body, #req{body = Body}) -> Body;
g(resp, #req{resp = Resp}) -> Resp.
