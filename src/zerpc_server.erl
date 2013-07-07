-module(zerpc_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
        code_change/3]).

-export([start_link/3]).

-include("internal.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
        context :: term(),
        router :: term(),
        dealer :: term()
    }).

%% ===================================================================
%% Public
%% ===================================================================

start_link(Endpoint,DealerEndpoint, Context) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, 
        [Endpoint,DealerEndpoint, Context], []).

%% ===================================================================
%% gen_server
%% ===================================================================

init([Endpoint, DealerEndpoint, Context]) ->
    {ok, Router} = erlzmq:socket(Context, [router, {active, true}]),
    {ok, Dealer} = erlzmq:socket(Context, [dealer, {active, true}]),
    ok = erlzmq:bind(Router, Endpoint),
    ok = erlzmq:bind(Dealer, DealerEndpoint),
    {ok, #state{router = Router, dealer = Dealer, context = Context}}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({zmq, Router, Msg, Flags}, 
    #state{router = Router, dealer = Dealer} = State) ->
    case proplists:get_bool(rcvmore, Flags) of
        true ->
            erlzmq:send(Dealer, Msg, [sndmore]);
        false ->
            erlzmq:send(Dealer, Msg)
    end,
    {noreply, State};

handle_info({zmq, Dealer, Msg, Flags}, 
    #state{router = Router, dealer = Dealer} = State) ->
    ok = case proplists:get_bool(rcvmore, Flags) of
        true ->
            erlzmq:send(Router, Msg, [sndmore]);
        false ->
            erlzmq:send(Router, Msg)
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

terminate(_Reason, State) ->
    erlzmq:close(State#state.router),
    erlzmq:close(State#state.dealer).

%% ===================================================================
%% Private
%% ===================================================================
