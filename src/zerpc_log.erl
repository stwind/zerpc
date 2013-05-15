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
        {cmd_arity, length(Args)}
    ].

meta_resp({error, {server, Code, Type, Reason, Trace}}) ->
    [
        {error_code, Code}, {error_type, Type}, 
        {reason, Reason} | maybe_trace(Trace)
    ];
meta_resp(Resp) ->
    %% XXX: performance issue
    [{resp, oneline(io_lib:format("~p", [Resp]))}].

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
    lager:info(Meta, Fmt, Args).

msg_type({error, _}) ->
    error;
msg_type(_) ->
    notice.

oneline(Str) ->
    replace(Str, "\n\s*", " ").

replace(Str, Old, New) ->
    re:replace(Str, Old, New, [global,{return,list}]).

maybe_trace([]) -> [];
maybe_trace(Trace) -> [{trace, Trace}].
