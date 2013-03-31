-module(zerpc_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
        code_change/3]).

-export([start_link/1]).
-export([reply/2]).

-record(state, {
        context :: term(),
        socket  :: term(),
        peer :: undefined | binary()
}).

%% ===================================================================
%% Public
%% ===================================================================

start_link(Endpoint) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Endpoint], []).

reply(Ctx, Message) ->
    gen_server:cast(?MODULE, {reply, Ctx, Message}).

%% ===================================================================
%% gen_server
%% ===================================================================

init([Endpoint]) ->
    {ok, Context} = erlzmq:context(),
    {ok, Socket}  = erlzmq:socket(Context, [router, {active, true}]),
    ok = erlzmq:bind(Socket, Endpoint),
    {ok, #state{socket = Socket, context = Context}}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({reply, Ctx, Resp}, State) ->
    send_reply(Ctx, Resp, State),
    {noreply, State};

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({zmq, Socket, <<>>, [rcvmore]}, #state{
        socket = Socket, peer = Peer
    } = State) when is_binary(Peer) ->
    %% empty message part separates envelope from data
    {noreply, State};

handle_info({zmq, Socket, Message, [rcvmore]}, #state{
        socket = Socket, peer = undefined
    } = State) ->
    %% first message part is peer identity
    {noreply, State#state{peer = Message}};

handle_info({zmq, Socket, Message, []}, #state{
        socket = Socket, peer = Peer
    } = State) ->
    incoming_request(Message, Peer),
    {noreply, State#state{peer = undefined}};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_FromVsn, _ToVsn, State) ->
    {ok, State}.

terminate(_Reason, State) ->
    erlzmq:close(State#state.socket),
    erlzmq:term(State#state.context).

%% ===================================================================
%% Private
%% ===================================================================

incoming_request(Msg, Peer) ->
    Ctx = zerpc_ctx:new(Peer),
    zerpc_router:process(Ctx, Msg).

send_reply(Ctx, Message, #state{socket = Socket}) ->
    Peer = zerpc_ctx:peer(Ctx),
    ok = erlzmq:send(Socket, Peer, [sndmore]),
    ok = erlzmq:send(Socket, <<>>, [sndmore]),
    ok = erlzmq:send(Socket, Message).
