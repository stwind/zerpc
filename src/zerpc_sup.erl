-module(zerpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("internal.hrl").
-include_lib("eunit/include/eunit.hrl").

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
    Children = children(Context),
    {ok, { {one_for_one, 5, 10}, Children} }.

%% ===================================================================
%% Private
%% ===================================================================

children(Context) ->
    server_pool(Context) ++ client_pool(Context).

server_pool(Context) ->
    case zerpc_util:get_env(server) of
        undefined ->
            [];
        ServerOpts ->
            [server(Context, ServerOpts), router(), worker_pool(ServerOpts)]
    end.

client_pool(Context) ->
    case zerpc_util:get_env(client) of
        undefined ->
            [];
        ClientOpts ->
            lists:flatten([clients(Context, ClientOpts)])
    end.

clients(Context, Opts) ->
    Pools = proplists:get_value(pools, Opts, []),
    [client(Context, P) || P <- Pools].

client(Context, {Name, Opts}) ->
    Size = proplists:get_value(size, Opts),
    PoolArgs = [
        {name, {local, Name}},
        {worker_module, zerpc_client},
        {size, Size},
        {max_overflow, proplists:get_value(overflow, Opts, Size * 3)}
    ],
    WorkerArgs = [
        {context, Context},
        {endpoint, proplists:get_value(endpoint, Opts, "tcp://127.0.0.1:5556")},
        {send_timeout, proplists:get_value(send_timeout, Opts, 5000)},
        {recv_timeout, proplists:get_value(recv_timeout, Opts, 5000)}
    ],
    Id = list_to_atom("zerpc_woker_pool_" ++ atom_to_list(Name)),
    poolboy:child_spec(Id, PoolArgs, WorkerArgs).

server(Context, Opts) ->
    Endpoint = proplists:get_value(endpoint, Opts, "tcp://*:5556"),
    {zerpc_server, {zerpc_server, start_link, [Endpoint, Context]}, 
        permanent, 5000, worker, [zerpc_server]}.

router() ->
    {zerpc_router, {zerpc_router, start_link, []}, permanent,
        5000, worker, [zerpc_router]}.

worker_pool(Opts) ->
    PoolSize = proplists:get_value(size, Opts, 100),
    PoolArgs = [
        {name, {local, ?SERVER_POOL}},
        {worker_module, zerpc_worker},
        {size, PoolSize},
        {max_overflow, zerpc_util:get_env(overflow, PoolSize * 3)}
    ],
    Hooks = proplists:get_value(middlewares, Opts, [zerpc_mfa,zerpc_log]),
    WorkerArgs = [{middlewares, Hooks}],
    poolboy:child_spec(zerpc_woker_pool, PoolArgs, WorkerArgs).
