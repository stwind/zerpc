-module(zerpc).

-export([call/4]).
-export([cast/4]).

%% ===================================================================
%% Public
%% ===================================================================

call(Endpoint, Mod, Fun, Args) ->
    Req = zerpc_proto:call(Mod, Fun, Args),
    Reply = zerpc_client:send(Endpoint, Req),
    handle_result(zerpc_proto:parse(Reply)).

cast(Endpoint, Mod, Fun, Args) ->
    Req = zerpc_proto:cast(Mod, Fun, Args),
    Reply = zerpc_client:send(Endpoint, Req),
    handle_result(zerpc_proto:parse(Reply)).

%% ===================================================================
%% Private
%% ===================================================================

handle_result({error, Error}) ->
    {error, {zerpc_error, Error}};
handle_result(Res) ->
    Res.
