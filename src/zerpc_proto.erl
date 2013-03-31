-module(zerpc_proto).

-export([call/3]).
-export([cast/3]).
-export([reply/1]).
-export([noreply/0]).
-export([error/1]).

-export([parse/1]).

-define(IS_MFA(M,F,A), is_atom(M),is_atom(F),is_list(A)).
-define(IS_ERR(R,C,T,D,S), is_atom(R),is_integer(C),is_binary(T),is_binary(D),is_list(S)).

%% ===================================================================
%% Public
%% ===================================================================

call(Mod, Fun, Args) ->
    encode({call, Mod, Fun, Args}).

cast(Mod, Fun, Args) ->
    encode({cast, Mod, Fun, Args}).

reply(Result) ->
    encode({reply, Result}).

noreply() ->
    encode({noreply}).

error(Error) ->
    encode({error, Error}).

parse(Binary) ->
    validate(decode(Binary)).

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
    noreply;
validate({error, {R,C,T,D,S}} = Error) when ?IS_ERR(R,C,T,D,S) ->
    Error;
validate(_) ->
    erlang:error(badarg).

encode(Term) ->
    bert:encode(Term).

decode(Binary) ->
    bert:decode(Binary).
