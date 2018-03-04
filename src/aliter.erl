-module(aliter).

-behaviour(application).

-include("records.hrl").

-export([ start/2
        , shutdown/0
        , stop/1 ]).

start(_Type, StartArgs) ->
    lager:start(),
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ranch),
    aliter_sup:start_link(StartArgs).

shutdown() ->
    application:stop(aliter).

stop(_State) ->
    ok.
