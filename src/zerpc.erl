-module(zerpc).

-export([call/4]).
-export([cast/4]).

-compile([{parse_transform, lager_transform}]).

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
    case zerpc_client:request(Pool, zerpc_proto:encode(Req)) of
        {ok, Reply} ->
            case zerpc_proto:decode(Reply) of
                {error, {server, _, _, Reason, _}} ->
                    {error, Reason};
                Other ->
                    Other
            end;
        {error, Reason} ->
            {error, Reason}
    end.
