-module(aliter).

-behaviour(application).

-include("include/records.hrl").

-export([ start/2
        , shutdown/0
        , stop/1 ]).

start(_Type, StartArgs) ->
  aliter_sup:start_link(StartArgs).

shutdown() ->
  application:stop(aliter).

stop(_State) ->
  ok.
