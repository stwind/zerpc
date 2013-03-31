-module(zerpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("internal.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Context) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Context]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Context]) ->
    {ok, { {one_for_one, 5, 10}, children(Context)} }.

%% ===================================================================
%% Private
%% ===================================================================

children(Context) ->
    case zerpc_util:get_env(mode, client) of
        server ->
            [server(Context), router(), worker_pool()];
        client ->
            [client(Context)]
    end.

client(Context) ->
    PoolSize = zerpc_util:get_env(pool_size, 100),
    PoolArgs = [
        {name, {local, ?CLIENT_POOL}},
        {worker_module, zerpc_client},
        {size, PoolSize},
        {max_overflow, zerpc_util:get_env(overflow, PoolSize * 2)}
    ],
    Endpoint = zerpc_util:get_env(endpoint, "tcp://127.0.0.1:5556"),
    WorkerArgs = {Endpoint, Context},
    {zerpc_woker_pool, {poolboy, start_link, [PoolArgs, WorkerArgs]},
        permanent, 5000, worker, [poolboy]}.

server(Context) ->
    Endpoint = zerpc_util:get_env(endpoint, "tcp://*:5556"),
    {zerpc_server, {zerpc_server, start_link, [Endpoint, Context]}, permanent,
        5000, worker, [zerpc_server]}.

router() ->
    {zerpc_router, {zerpc_router, start_link, []}, permanent,
        5000, worker, [zerpc_router]}.

worker_pool() ->
    PoolSize = zerpc_util:get_env(pool_size, 100),
    PoolArgs = [
        {name, {local, ?SERVER_POOL}},
        {worker_module, zerpc_worker},
        {size, PoolSize},
        {max_overflow, zerpc_util:get_env(overflow, PoolSize * 2)}
    ],
    WorkerArgs = [],
    {zerpc_woker_pool, {poolboy, start_link, [PoolArgs, WorkerArgs]},
        permanent, 5000, worker, [poolboy]}.
