-module(zerpc_mfa).

-export([execute/1]).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Public
%% ===================================================================

execute(Req) ->
    {Body, Req1} = zerpc_req:body(Req),
    Req2 = zerpc_req:reply(handle(Body), Req1),
    {ok, Req2}.

%% ===================================================================
%% Private
%% ===================================================================

handle({call, Mod, Fun, Args}) ->
    case catch erlang:apply(Mod, Fun, Args) of
        {'EXIT', {Type, BackTrace}} ->
            error_reply(Type, BackTrace);
        {error, Reason} ->
            error_reply({catched, Reason});
        Result ->
            zerpc_proto:reply(Result)
    end;
handle({cast, Mod, Fun, Args}) ->
    proc_lib:spawn(Mod, Fun, Args),
    zerpc_proto:noreply().

error_reply(Type) ->
    error_reply(Type, []).

error_reply(Type0, BackTrace) when is_list(BackTrace) ->
    {Code, Reason} = explain(Type0),
    Type = zerpc_util:to_binary(type(Type0)),
    Error = {server, Code, Type, Reason, BackTrace},
    zerpc_proto:error(Error);
error_reply(Type0, Reason) ->
    {Code, Reason1} = explain(Reason),
    Type = zerpc_util:to_binary(type(Type0)),
    Error = {server, Code, Type, Reason1, []},
    zerpc_proto:error(Error).

type(Type) when is_tuple(Type) ->
    element(1, Type);
type(Type) ->
    Type.

explain(badrpc) ->
    {100, badrpc};
explain(undef) ->
    {101, undef};
explain({badmatch, Value}) ->
    {102, Value};

explain({catched, Reason}) ->
    {200, Reason};

explain(Reason) ->
    {900, Reason}.
