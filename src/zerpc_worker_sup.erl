-module(zerpc_worker_sup).

-behaviour(supervisor).

-export([start_link/4, init/1]).

start_link(Size, DealerEndpoint, MiddleWares, Context) ->
    supervisor:start_link(?MODULE, [Size, DealerEndpoint, MiddleWares, Context]).

init([Size, DealerEndpoint, MiddleWares, Context]) ->
    Procs = [
        {{zerpc_worker,N}, {zerpc_worker, start_link, 
                [DealerEndpoint, Context, MiddleWares]}, 
            permanent, brutal_kill, worker, []}
        || N <- lists:seq(1, Size)],
    {ok, {{one_for_one, 10, 10}, Procs}}.
