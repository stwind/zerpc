-module(zerpc_proto).

-export([call/3]).
-export([cast/3]).
-export([reply/1]).
-export([noreply/0]).
-export([error/1]).

-export([decode/1]).
-export([encode/1]).

-include_lib("eunit/include/eunit.hrl").


-define(IS_MFA(M,F,A), is_atom(M),is_atom(F),is_list(A)).
-define(IS_ERR(R,C,S), is_atom(R),is_integer(C),is_list(S)).

%% ===================================================================
%% Public
%% ===================================================================

call(Mod, Fun, Args) ->
    {call, Mod, Fun, Args}.

cast(Mod, Fun, Args) ->
    {cast, Mod, Fun, Args}.

reply(Result) ->
    {reply, Result}.

noreply() ->
    {noreply}.

error(Error) ->
    {error, Error}.

encode(Term) ->
    bert:encode(Term).

decode(Binary) ->
    validate(bert:decode(Binary)).

%% ===================================================================
%% Private
%% ===================================================================

validate({call, M, F, A} = Req) when ?IS_MFA(M, F, A) ->
    Req;
validate({cast, M, F, A} = Req) when ?IS_MFA(M, F, A) ->
    Req;
validate({reply, Result}) ->
    Result;
validate({noreply}) ->
    ok;
validate({error, {R,C,_,_,S}} = Error) when ?IS_ERR(R,C,S) ->
    Error;
validate(_) ->
    erlang:error(badarg).

%% ===================================================================
%% Eunit
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

dc(V) ->
    decode(encode(V)).

proto_test_() ->
    [
        {"call", ?_assertMatch({call, m, f, []}, dc(call(m, f, [])))},
        {"cast", ?_assertMatch({cast, m, f, []}, dc(cast(m, f, [])))},
        {"reply", ?_assertMatch(result, dc(reply(result)))},
        {"noreply", ?_assertMatch(ok, dc(noreply()))},
        {"error", ?_assertMatch({error,{server, 100, <<"type">>, <<"class">>, []}}, 
                dc(?MODULE:error({server, 100, <<"type">>, <<"class">>, []})))}
    ].

-endif.
