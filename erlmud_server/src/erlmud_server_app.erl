%%%-------------------------------------------------------------------
%% @doc erlmud_server public API
%% @end
%%%-------------------------------------------------------------------

-module(erlmud_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlmud_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
