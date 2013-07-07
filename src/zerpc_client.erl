-module(zerpc_client).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
        code_change/3]).

-include("internal.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/1]).
-export([request/2]).

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
    Worker = poolboy:checkout(Pool),
    try gen_server:call(Worker, {request, Req}, Timeout) of
        Result -> Result
    catch
        exit:{timeout, _} ->
            {error, {timeout, gen_server}};
        _:Exn ->
            {error, Exn}
    after
        poolboy:checkin(Pool, Worker)
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

do_request(Req, #state{socket = Socket} = State) ->
    case erlzmq:send(Socket, Req) of
        {error, eagain} ->
            {{error, send_timeout}, State};
        ok ->
            case erlzmq:recv(Socket) of
                {error, eagain} ->
                    ok = erlzmq:close(Socket),
                    {{error, recv_timeout}, open_socket(State)};
                {ok, Rep} ->
                    {{ok, Rep}, State}
            end;
        {error, Reason} ->
            {{error, Reason}, State}
    end.

open_socket(#state{
        context = Context, endpoint = Endpoint,
        sndtimeo = Sndtimeo, rcvtimeo = Rcvtimeo
    } = State) ->
    {ok, Socket} = erlzmq:socket(Context, req),
    ok = erlzmq:setsockopt(Socket, sndtimeo, Sndtimeo),
    ok = erlzmq:setsockopt(Socket, rcvtimeo, Rcvtimeo),
    ok = erlzmq:connect(Socket, Endpoint),
    State#state{socket = Socket}.
