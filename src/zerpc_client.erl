-module(zerpc_client).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
        code_change/3]).

-include("internal.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/1]).
-export([request/2]).
-export([request/3]).

-define(TIMEOUT, 10000).

-record(state, {
        context :: term(),
        endpoint :: string(),
        socket  :: term(),
        sndtimeo = 5000 :: integer(),
        rcvtimeo = 5000 :: integer()
    }).

%% ===================================================================
%% Public
%% ===================================================================

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

request(Pool, Req) ->
    request(Pool, Req, ?TIMEOUT).

request(Pool, Req, Timeout) ->
    case poolboy:checkout(Pool, false) of
        full ->
            {error, {system_busy, node()}};
        Worker ->
            try gen_server:call(Worker, {request, Req}, Timeout) of
                Result -> Result
                catch
                    exit:{timeout, _} ->
                        {error, {timeout, Pool}};
                    _:Exn ->
                        {error, Exn}
                after
                    poolboy:checkin(Pool, Worker)
            end
    end.

%% ===================================================================
%% gen_server
%% ===================================================================

init(Options) ->
    Context = proplists:get_value(context, Options),
    Endpoint = proplists:get_value(endpoint, Options),
    SendTimeout = proplists:get_value(send_timeout, Options),
    RecvTimeout = proplists:get_value(recv_timeout, Options),
    {ok, open_socket(#state{
                context = Context, endpoint = Endpoint,
                sndtimeo = SendTimeout, rcvtimeo = RecvTimeout
            })}.

handle_call({request, Req}, _From, State) ->
    {Result, State1} = do_request(Req, State),
    {reply, Result, State1};

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

terminate(_Reason, State) ->
    erlzmq:close(State#state.socket).

%% ===================================================================
%% Private
%% ===================================================================

do_request(Req, #state{socket = Socket, endpoint = Endpoint, 
        rcvtimeo = Timeout} = State) ->
    case erlzmq:send(Socket, Req) of
        {error, eagain} ->
            {{error, {send_timeout, Endpoint}}, State};
        ok ->
            receive
                {zmq, Socket, Resp, []} ->
                    {{ok, Resp}, State}
            after Timeout ->
                ok = erlzmq:close(Socket),
                {{error, {recv_timeout, Endpoint}}, open_socket(State)}
            end;
        {error, Reason} ->
            {{error, Reason}, State}
    end.

open_socket(#state{
        context = Context, endpoint = Endpoint, sndtimeo = Sndtimeo
    } = State) ->
    {ok, Socket} = erlzmq:socket(Context, [req,{active,true}]),
    ok = erlzmq:setsockopt(Socket, sndtimeo, Sndtimeo),
    ok = erlzmq:connect(Socket, Endpoint),
    State#state{socket = Socket}.
