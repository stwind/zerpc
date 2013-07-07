-module(zerpc_worker).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
        code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-export([start_link/3]).

-record(state, { 
        middlewares = [] :: list(module()),
        context :: term(),
        socket :: term()
    }).

%% ===================================================================
%% Public
%% ===================================================================

start_link(DealerEndpoint, Context, Middlewares) ->
    gen_server:start_link(?MODULE, [DealerEndpoint, Context, Middlewares], []).

%% ===================================================================
%% gen_server
%% ===================================================================

init([DealerEndpoint, Context, Middlewares]) ->
    {ok, Socket} = erlzmq:socket(Context, [rep, {active, true}]),
    ok = erlzmq:connect(Socket, DealerEndpoint),
    {ok, #state{
            socket = Socket,
            middlewares = Middlewares
        }}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({request, Req}, #state{middlewares = Mods} = State) ->
    run_middlewares(Req, Mods),
    {noreply, State};

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({zmq, Socket, Msg, _}, #state{socket = Socket} = State) ->
    {ok, Req} = handle_req(zerpc_req:new(Msg), State),
    reply(Req, State),
    {noreply, State}.

code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% ===================================================================
%% Private
%% ===================================================================

handle_req(Req, #state{middlewares = Mods}) ->
    run_middlewares(Req, Mods).

run_middlewares(Req, [Mod | Rest]) ->
    case Mod:execute(Req) of
        {ok, Req1} ->
            run_middlewares(Req1, Rest);
        {error, _Reason} ->
            nop
    end;
run_middlewares(Req, []) ->
    {ok, Req}.

reply(Req, #state{socket = Socket}) ->
    Reply = zerpc_req:resp(Req),
    ok = erlzmq:send(Socket, zerpc_proto:encode(Reply)).
