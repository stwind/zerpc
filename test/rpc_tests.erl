-module(rpc_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(ENDPOINT, "tcp://127.0.0.1:5566").
-define(POOL_SIZE, 5).

-define(WITH_CLIENT(Count, Case), {setup,
        fun() -> start_clients(Count) end,
        fun(Nodes) -> [slave:stop(N) || N <- Nodes] end,
        fun(Nodes) -> ?_test(Case(Nodes)) end
    }).

setup() ->
    {ok, _} = net_kernel:start(['server', shortnames]),
    zerpc_env(node(), server),
    application:start(zerpc).

cleanup(_) ->
    application:stop(zerpc).

start_clients(Count) ->
    Name = fun(N) -> list_to_atom("client" ++ integer_to_list(N)) end,
    [new_client(Name(N)) || N <- lists:seq(1, Count)].

zerpc_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
            {"call", ?WITH_CLIENT(1, pingpong)},
            {"error return", ?WITH_CLIENT(1, error_return)},
            {"throw", ?WITH_CLIENT(1, throw_return)},
            {"erlang error", ?WITH_CLIENT(1, erlang_error)}
        ]}.

%% ===================================================================
%% Test cases
%% ===================================================================

pingpong([Client]) ->
    ?assertMatch(pong, call(Client, zerpc, call, [server, ping, []])),
    ?assertMatch(ping, call(Client, zerpc, call, [server, pong, []])).

error_return([Client]) ->
    {error, Reason} = call(Client, zerpc, call, [server, error_return, [whatever]]),
    ?assertMatch({zerpc_error, {server, 900, _, _, _}}, Reason).

throw_return([Client]) ->
    {error, Reason} = call(Client, zerpc, call, [server, throw, [{error, whatever}]]),
    ?assertMatch({zerpc_error, {server, 900, _, _, _}}, Reason).

erlang_error([Client]) ->
    {error, Reason} = call(Client, zerpc, call, [server, error, [badarg]]),
    {zerpc_error, {server, _, _, _, Trace}} = Reason,
    ?assertMatch([<<"server:error/1", _/binary>> | _], Trace).

%% ===================================================================
%% Helpers
%% ===================================================================

new_client(Name) ->
    Args = "-env ERL_LIBS ../deps -pa ../ebin",
    {ok, Node} = slave:start(localhost, Name, Args),
    zerpc_env(Node, client),
    ok = call(Node, application, start, [zerpc]),
    Node.

zerpc_env(Node, Mode) ->
    call(Node, application, set_env, [zerpc, mode, Mode]),
    call(Node, application, set_env, [zerpc, endpoint, ?ENDPOINT]),
    call(Node, application, set_env, [zerpc, pool_size, 5]).

call(Node, Mod, Fun, Args) ->
    rpc:call(Node, Mod, Fun, Args).
