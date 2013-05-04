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
        socket  :: term()
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
    Res = (catch gen_server:call(Worker, {request, Req}, Timeout)),
    poolboy:checkin(Pool, Worker),
    Res.

%% ===================================================================
%% gen_server
%% ===================================================================

init(Options) ->
    Context = proplists:get_value(context, Options),
    Endpoint = proplists:get_value(endpoint, Options),
    SendTimeout = proplists:get_value(send_timeout, Options),
    RecvTimeout = proplists:get_value(recv_timeout, Options),
    {ok, Socket}  = erlzmq:socket(Context, req),
    ok = erlzmq:setsockopt(Socket, sndtimeo, SendTimeout),
    ok = erlzmq:setsockopt(Socket, rcvtimeo, RecvTimeout),
    ok = erlzmq:connect(Socket, Endpoint),
    {ok, #state{socket = Socket, context = Context}}.

handle_call({request, Req}, _From, State) ->
    {reply, do_request(Req, State), State};

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

do_request(Req, #state{socket = Socket}) ->
    case erlzmq:send(Socket, Req) of
        {error, eagain} ->
            {error, timeout};
        ok ->
            case erlzmq:recv(Socket) of
                {error, eagain} ->
                    {error, timeout};
                {ok, Rep} ->
                    {ok, Rep}
            end
    end.
