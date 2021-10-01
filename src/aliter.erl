-module(aliter).

-behaviour(application).

-include("records.hrl").

-export([ start/2
        , shutdown/0
        , stop/1
        , get_config/2 ]).

start(_Type, StartArgs) ->
    lager:start(),
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ranch),
    db:init(),
    aliter_sup:start_link(StartArgs).

shutdown() ->
    application:stop(aliter).

stop(_State) ->
    ok.

get_config(Key, Fallback) ->
    case file:consult("aliter.config") of
        {ok, ConfigLs} ->
            case lists:keyfind(Key, 1, ConfigLs) of
                false ->
                    Fallback;
                {Key, Value} ->
                    Value
            end;
        {error, _Reason} ->
            Fallback
    end.
