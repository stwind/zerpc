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
    case zerpc_client:request(Pool, Req) of
        {ok, Reply} ->
            case zerpc_proto:parse(Reply) of
                {error, {server, _, _, Reason, _} = Error} ->
                    log_error(zerpc_proto:parse(Req), Error),
                    {error, Reason};
                Other ->
                    Other
            end;
        {error, Reason} ->
            {error, Reason}
    end.

log_error({Cmd, M, F, A}, {server, Code, Type, Reason, Trace}) ->
    Meta = [
        {type, zerpc_error}, {cmd_type, Cmd}, {cmd_mod, M}, {cmd_fun, F},
        {cmd_args, A}, {error_code, Code}, {error_type, Type},
        {reason, Reason}, {trace, Trace}
    ],
    lager:error(Meta, "", []).
