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

%% It is possible to configure the aliter.config file
%% one wants to use, if a special one is required for
%% testing purposes etc
get_config(Key, Fallback) ->
    case os:get_env("ALITER_CONFIG") of
        ConfigFileName ->
            get_config_filename(ConfigFileName);
        false ->
            get_config_filename("aliter.config")
    end.

get_config_filename(Key, Fallback, ConfigFileName) ->
    case file:consult(ConfigFileName) of
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
