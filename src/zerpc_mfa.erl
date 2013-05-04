-module(zerpc_mfa).

-export([execute/1]).

%% ===================================================================
%% Public
%% ===================================================================

execute(Req) ->
    {Body, Req1} = zerpc_req:body(Req),
    Result = handle(Body),
    Req2 = zerpc_req:reply(Result, Req1),
    {ok, Req2}.

%% ===================================================================
%% Private
%% ===================================================================

handle({call, Mod, Fun, Args}) ->
    case catch erlang:apply(Mod, Fun, Args) of
        {'EXIT', {undef, _}} ->
            error_reply({fun_undef, Mod, Fun, Args});
        {'EXIT', {Type, BackTrace}} ->
            error_reply(Type, BackTrace);
        {error, Reason} ->
            error_reply({internal_error, Reason});
        Result ->
            zerpc_proto:reply(Result)
    end;
handle({cast, Mod, Fun, Args}) ->
    proc_lib:spawn(Mod, Fun, Args),
    zerpc_proto:noreply().

error_reply(Type) ->
    error_reply(Type, []).

error_reply(Type0, BackTrace) ->
    {Code, Reason} = explain(Type0),
    Type = zerpc_util:to_binary(type(Type0)),
    Error = {server, Code, Type, Reason, fmt_bt(BackTrace)},
    zerpc_proto:error(Error).

type(Type) when is_tuple(Type) ->
    element(1, Type);
type(Type) ->
    Type.

explain(badrpc) ->
    {100, <<"invalid rpc command">>};
explain({fun_undef, Mod0, Fun0, Arity0}) ->
    Mod = zerpc_util:to_binary(Mod0),
    Fun = zerpc_util:to_binary(Fun0),
    Arity = zerpc_util:to_binary(Arity0),
    {101, <<"undefined function ", 
        Mod/binary, ":", Fun/binary, "/", Arity/binary>>};
explain({internal_error, _}) ->
    {900, <<"internal server error">>};
explain(_) ->
    {901, <<"internal server error">>}.

fmt_bt(BT) ->
    [fmt_call(C) || C <- BT].

fmt_call({M, F, A, Meta}) ->
    <<(fmt_mfa(M,F,A))/binary,(fmt_meta(Meta))/binary>>.

fmt_mfa(M0, F0, A0) ->
    M = zerpc_util:to_binary(M0),
    F = zerpc_util:to_binary(F0),
    A = if 
        is_integer(A0) ->
            zerpc_util:to_binary(A0);
        is_list(A0) ->
            zerpc_util:to_binary(length(A0))
    end,
    <<M/binary,":",F/binary,"/",A/binary>>.

fmt_meta([]) ->
    <<>>;
fmt_meta([{file,File0},{line,Line0}]) ->
    File = zerpc_util:to_binary(File0),
    Line = zerpc_util:to_binary(Line0),
    <<" (",File/binary,":",Line/binary,")">>.
