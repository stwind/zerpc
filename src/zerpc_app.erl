-module(zerpc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Context} = erlzmq:context(1),
    {ok, Sup} = zerpc_sup:start_link(Context),
    {ok, Sup, Context}.

stop(Context) ->
    ok = erlzmq:term(Context),
    ok.
