-module(zerpc).

-export([call/4]).
-export([cast/4]).

%% ===================================================================
%% Public
%% ===================================================================

call(Pool, Mod, Fun, Args) ->
    send_req(Pool, zerpc_proto:call(Mod, Fun, Args)).

cast(Pool, Mod, Fun, Args) ->
    send_req(Pool, zerpc_proto:cast(Mod, Fun, Args)).

%% ===================================================================
%% Private
%% ===================================================================

send_req(Pool, Req) ->
    case zerpc_client:request(Pool, Req) of
        {ok, Reply} ->
            zerpc_proto:parse(Reply);
        {error, Reason} ->
            {error, Reason}
    end.
