-module(zerpc_util).

-export([get_env/1]).
-export([get_env/2]).

-export([to_binary/1]).

%% ===================================================================
%% Public
%% ===================================================================

get_env(Key) ->
    get_env(Key, undefined).

get_env(Key, Default) ->
    case application:get_env(zerpc, Key) of
        {ok, Val} -> Val;
        undefined -> Default
    end.

to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_atom(Value) ->
    to_binary(atom_to_list(Value));
to_binary(Value) when is_integer(Value) ->
    to_binary(integer_to_list(Value)).
