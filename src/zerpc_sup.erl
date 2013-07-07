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
    case zerpc_util:get_env(mode, client) of
        server ->
            [server(Context), worker_sup(Context)];
        client ->
            lists:flatten([clients(Context)])
    end.

clients(Context) ->
    Pools = zerpc_util:get_env(pools, []),
    [client(Context, P) || P <- Pools].

client(Context, {Name, Options}) ->
    Size = proplists:get_value(size, Options),
    PoolArgs = [
        {name, {local, Name}},
        {worker_module, zerpc_client},
        {size, Size},
        {max_overflow, proplists:get_value(overflow, Options, Size * 2)}
    ],
    WorkerArgs = [
        {context, Context},
        {endpoint, proplists:get_value(endpoint, Options, "tcp://127.0.0.1:5556")},
        {send_timeout, zerpc_util:get_env(send_timeout, 5000)},
        {recv_timeout, zerpc_util:get_env(recv_timeout, 5000)}
    ],
    Id = list_to_atom("zerpc_woker_pool_" ++ atom_to_list(Name)),
    {Id, {poolboy, start_link, [PoolArgs, WorkerArgs]},
        permanent, 5000, worker, [poolboy]}.

server(Context) ->
    Endpoint = zerpc_util:get_env(endpoint, "tcp://*:5556"),
    DealerEndpoint = zerpc_util:get_env(dealer, ?DEALER_ENDPOINT),
    {zerpc_server, {zerpc_server, start_link, [Endpoint,DealerEndpoint,Context]}, 
        permanent, 5000, worker, [zerpc_server]}.

worker_sup(Context) ->
    Size = zerpc_util:get_env(size, 100),
    DealerEndpoint = zerpc_util:get_env(dealer, ?DEALER_ENDPOINT),
    MiddleWares = zerpc_util:get_env(middlewares, [zerpc_mfa,zerpc_log]),
    {zerpc_worker_sup, {zerpc_worker_sup, start_link,
            [Size, DealerEndpoint, MiddleWares, Context]},
        permanent, infinity, supervisor, [zerpc_worker_sup]}.
