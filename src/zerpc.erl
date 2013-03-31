-module(zerpc).

-export([call/3]).
-export([cast/3]).

%% ===================================================================
%% Public
%% ===================================================================

call(Mod, Fun, Args) ->
    send_req(zerpc_proto:call(Mod, Fun, Args)).

cast(Mod, Fun, Args) ->
    send_req(zerpc_proto:cast(Mod, Fun, Args)).

%% ===================================================================
%% Private
%% ===================================================================

send_req(Req) ->
    case zerpc_client:request(Req) of
        {ok, Reply} ->
            case zerpc_proto:parse(Reply) of
                {error, Error} ->
                    {error, {zerpc_error, Error}};
                Res ->
                    Res
            end;
        {error, Reason} ->
            {error, Reason}
    end.
