-module(zerpc_log).

-export([execute/1]).

-compile([{parse_transform, lager_transform}]).

%% ===================================================================
%% Public
%% ===================================================================

execute(Req) ->
    {Body, _} = zerpc_req:body(Req),
    {Resp, _} = zerpc_req:body(Req),
    Start = zerpc_req:meta(start_time, Req),
    End = zerpc_req:meta(end_time, Req),
    log(Start, End, Body, Resp),
    {ok, Req}.

%% ===================================================================
%% Private
%% ===================================================================

log(Start, End, Req, Resp) ->
    {Fmt, Args} = msg_time(Start, End),
    {Fmt1, Args1} = msg(Req, Resp),
    lager:error(meta(Req, Resp), Fmt ++ Fmt1, Args ++ Args1).

meta(Req, Resp) ->
    meta_req(Req) ++ meta_resp(Resp).

meta_req({Type, Mod, Fun, Args}) ->
    [
        {type, zerpc}, {cmd_type, Type}, {cmd_mod, Mod}, {cmd_fun, Fun},
        {cmd_args, Args}
    ].

meta_resp({server, Code, Type, Reason, Trace}) ->
    [
        {error_code, Code}, {error_type, Type}, 
        {reason, Reason}, {trace, Trace}
    ].

msg({Type, M, F, A}, {server, _, _, Reason, []}) ->
    {"~pms: ~p ~p:~p/~p -> ~p", [Type, M, F, length(A), Reason]};
msg({Type, M, F, A}, {server, _, _, Reason, Trace}) ->
    {"~p ~p:~p/~p -> ~p~n~p", [Type, M, F, length(A), Reason, Trace]};
msg({Type, M, F, A}, Resp) ->
    {"~p ~p:~p/~p -> ~w", [Type, M, F, length(A), Resp]}.

msg_time(Start, End) ->
    {{Y,M,D},{H,MM,S}} = calendar:now_to_local_time(Start),
    {"~b-~b-~b ~b:~b:~b [~bms] ",[Y,M,D,H,MM,S,diff(Start, End)]}.

diff(Start, End) ->
    timer:now_diff(End, Start) div 1000.
