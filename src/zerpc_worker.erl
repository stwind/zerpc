-module(zerpc_worker).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
        code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-export([start_link/1]).
-export([do/2]).

-record(state, { 
        hooks = [] :: list(module())
    }).

%% ===================================================================
%% Public
%% ===================================================================

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

do(Pid, Req) ->
    gen_server:cast(Pid, {request, Req}).

%% ===================================================================
%% gen_server
%% ===================================================================

init(Options) ->
    {ok, #state{hooks = proplists:get_value(hooks, Options)}}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({request, Req}, #state{hooks = Hooks} = State) ->
    run_hooks(Req, Hooks),
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

run_hooks(Req, [Hook | Rest]) ->
    case Hook:execute(Req) of
        {ok, Req1} ->
            run_hooks(Req1, Rest);
        {error, _Reason} ->
            nop
    end;
run_hooks(_, []) ->
    ok.
