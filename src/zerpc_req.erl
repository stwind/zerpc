-module(zerpc_req).

-export([new/2]).
-export([body/1]).
-export([reply/2]).
-export([start_time/1]).
-export([end_time/1]).

-record(req, {
        peer :: binary(),
        msg :: binary(),
        body :: term(),
        resp :: binary(),
        start_time = os:timestamp() :: erlang:timestamp(),
        end_time :: erlang:timestamp()
    }).

%% ===================================================================
%% Public
%% ===================================================================

new(Peer, Msg) ->
    #req{peer = Peer, msg = Msg}.

body(#req{body = undefined, msg = Msg} = Req) ->
    Body = parse_msg(Msg),
    {Body, Req#req{body = Body}};
body(Req) ->
    {g(body, Req), Req}.

reply(Reply, #req{peer = Peer} = Req) ->
    Req1 = Req#req{end_time = os:timestamp()},
    zerpc_server:reply(Peer, Reply),
    Req1.

start_time(Req) ->
    {g(start_time, Req), Req}.

end_time(Req) ->
    {g(end_time, Req), Req}.

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
g(body, #req{body = Body}) -> Body;
g(start_time, #req{start_time = Body}) -> Body;
g(end_time, #req{end_time = Body}) -> Body.
