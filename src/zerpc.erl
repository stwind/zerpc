-module(zerpc).

-export([call/5]).
-export([cast/5]).

%% ===================================================================
%% Public
%% ===================================================================

call(Pool, Mod, Fun, Args, Timeout) ->
    send_req(Pool, zerpc_proto:call(Mod, Fun, Args), Timeout).

cast(Pool, Mod, Fun, Args, Timeout) ->
    send_req(Pool, zerpc_proto:cast(Mod, Fun, Args), Timeout).

%% ===================================================================
%% Private
%% ===================================================================

send_req(Pool, Req, Timeout) ->
    case zerpc_client:request(Pool, zerpc_proto:encode(Req), Timeout) of
        {ok, Reply} ->
            case zerpc_proto:decode(Reply) of
                {error, Reason} ->
                    {error, reason(Reason)};
                Other ->
                    Other
            end;
        {error, Reason} ->
            {error, Reason}
    end.

reason({server, _, Type, undefined, _}) ->
    Type;
reason({server, _, Type, Value, _}) ->
    {Type, Value}.
