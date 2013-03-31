-module(zerpc_handler).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
        code_change/3]).

-include("internal.hrl").

-export([start_link/0]).
-export([process/2]).

-record(state, {}).

%% ===================================================================
%% Public
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process(Ctx, Msg) ->
    gen_server:cast(?MODULE, {process, Ctx, Msg}).

%% ===================================================================
%% gen_server
%% ===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({process, Ctx, Msg}, State) ->
    handle_msg(Ctx, Msg),
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

handle_msg(Ctx, Msg) ->
    Worker = poolboy:checkout(?POOL),
    ok = zerpc_worker:do(Worker, Ctx, Msg),
    poolboy:checkin(?POOL, Worker).
