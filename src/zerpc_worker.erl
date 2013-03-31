-module(zerpc_worker).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
        code_change/3]).

-export([start_link/1]).
-export([do/3]).

-record(state, { }).

%% ===================================================================
%% Public
%% ===================================================================

start_link([]) ->
    gen_server:start_link(?MODULE, [], []).

do(Pid, Ctx, Req) ->
    gen_server:cast(Pid, {request, Ctx, Req}).

%% ===================================================================
%% gen_server
%% ===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({process, Ctx, Msg}, State) ->
    handle_request(Ctx, Msg, State),
    {noreply, State};

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% ===================================================================
%% Private
%% ===================================================================

handle_request(Ctx, Msg, _State) ->
    Reply = case catch zerpc_proto:parse(Msg) of
        {'EXIT', {badarg, _}} ->
            error_reply(badrpc);
        Req ->
            call_service(Req)
    end,
    zerpc_server:reply(Ctx, Reply).

call_service({call, Mod, Fun, Args}) ->
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
call_service({cast, Mod, Fun, Args}) ->
    proc_lib:spawn(Mod, Fun, Args),
    zerpc_proto:noreply().

error_reply(Type) ->
    error_reply(Type, []).

error_reply(Type0, BackTrace) ->
    {Code, Reason} = explain(Type0),
    Type = zerpc_util:to_binary(Type0),
    {server, Code, Type, Reason, fmt_bt(BackTrace)}.

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
