-module(zerpc_worker).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
        code_change/3]).

-include("internal.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/1]).
-export([do/1]).

-record(state, { 
        middlewares = [] :: list(module())
    }).

%% ===================================================================
%% Public
%% ===================================================================

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

do(Req) ->
    case poolboy:checkout(?SERVER_POOL, false) of
        full ->
            Reply = zerpc_proto:error({server, 300, system_busy, node(), []}),
            zerpc_req:reply(Reply, Req);
        Worker ->
            gen_server:cast(Worker, {request, Req}),
            poolboy:checkin(?SERVER_POOL, Worker)
    end.

%% ===================================================================
%% gen_server
%% ===================================================================

init(Options) ->
    {ok, #state{middlewares = proplists:get_value(middlewares, Options)}}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({request, Req}, #state{middlewares = Mods} = State) ->
    run_middlewares(Req, Mods),
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

run_middlewares(Req, [Mod | Rest]) ->
    case Mod:execute(Req) of
        {ok, Req1} ->
            run_middlewares(Req1, Rest);
        {error, _Reason} ->
            nop
    end;
run_middlewares(_, []) ->
    ok.
