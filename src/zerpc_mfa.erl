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
        {'EXIT', {Error, BackTrace}} ->
            error_reply(Error, BackTrace);
        {error, Error} ->
            error_reply({throw, Error}, []);
        Result ->
            zerpc_proto:reply(Result)
    end;
handle({cast, Mod, Fun, Args}) ->
    proc_lib:spawn(Mod, Fun, Args),
    zerpc_proto:noreply().

error_reply(Error, BackTrace) when is_list(BackTrace) ->
    {Code, Type, Reason} = explain(Error),
    zerpc_proto:error({server, Code, Type, Reason, BackTrace});
error_reply(Type, Reason) ->
    {Code, _, Reason1} = explain(Reason),
    zerpc_proto:error({server, Code, type(Type), Reason1, []}).

type(Type) when is_tuple(Type) ->
    element(1, Type);
type(Type) ->
    Type.

explain(undef) ->
    {101, undef, undefined};
explain(function_clause) ->
    {102, function_clause, undefined};
explain(if_clause) ->
    {103, if_clause, undefined};
explain(badarg) ->
    {104, badarg, undefined};
explain(badarith) ->
    {105, badarith, undefined};
explain({badmatch, Value}) ->
    {106, badmatch, Value};
explain({case_clause, Value}) ->
    {107, case_clause, Value};
explain({badarity, Value}) ->
    {108, badarity, Value};

explain({throw, {Type, Reason}}) ->
    {200, Type, Reason};
explain({throw, Type}) ->
    {200, Type, undefined};

explain(Reason) ->
    {900, Reason, Reason}.
