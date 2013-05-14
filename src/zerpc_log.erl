-module(zerpc_log).

-export([execute/1]).

-compile([{parse_transform, lager_transform}]).

%% ===================================================================
%% Public
%% ===================================================================

execute(Req) ->
    {Body, _} = zerpc_req:body(Req),
    Resp = zerpc_req:resp(Req),
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
    do_log(msg_type(Resp), meta(Req, Resp), Fmt ++ Fmt1, Args ++ Args1).

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
    ];
meta_resp(Resp) ->
    [{resp, Resp}].

msg({Type, M, F, A}, {error, {server, _, _, Reason, _}}) ->
    {"~p ~p:~p/~p -> ~p", [Type, M, F, length(A), Reason]};
msg({Type, M, F, A}, {reply, Resp}) ->
    {"~p ~p:~p/~p -> ~p", [Type, M, F, length(A), Resp]}.

msg_time(Start, End) ->
    {"[~bms] ",[diff(Start, End)]}.

diff(Start, End) ->
    timer:now_diff(End, Start) div 1000.

do_log(error, Meta, Fmt, Args) ->
    lager:error(Meta, Fmt, Args);
do_log(notice, Meta, Fmt, Args) ->
    lager:notice(Meta, Fmt, Args).

msg_type({error, _}) ->
    error;
msg_type(_) ->
    notice.
