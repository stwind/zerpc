-module(zerpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, children()} }.

%% ===================================================================
%% Private
%% ===================================================================

children() ->
    case zerpc_util:get_env(endpoint, client) of
        server ->
            [server(), router(), worker_pool()];
        client ->
            [client()]
    end.

client() ->
    Endpoint = zerpc_util:get_env(endpoint, "tcp://127.0.0.1:5556"),
    {zerpc_client, {zerpc_client, start_link, [Endpoint]}, permanent,
        5000, worker, [zerpc_client]}.

server() ->
    Endpoint = zerpc_util:get_env(endpoint, "tcp://*:5556"),
    {zerpc_server, {zerpc_server, start_link, [Endpoint]}, permanent,
        5000, worker, [zerpc_server]}.

router() ->
    {zerpc_router, {zerpc_router, start_link, []}, permanent,
        5000, worker, [zerpc_router]}.

worker_pool() ->
    PoolSize = zerpc_util:get_env(pool_size, 100),
    PoolArgs = [
        {worker_module, zerpc_worker},
        {size, PoolSize},
        {max_overflow, zerpc_util:get_env(overflow, PoolSize * 2)}
    ],
    WorkerArgs = [],
    {zerpc_woker_pool, {poolboy, start_link, [PoolArgs, WorkerArgs]},
        permanent, 5000, worker, [poolboy]}.
