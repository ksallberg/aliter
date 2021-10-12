-module(aliter).

-behaviour(application).

-include("records.hrl").
-include("ro.hrl").

-export([ start/2
        , shutdown/0
        , stop/1
        , get_config/2
        , is_equip/1
        ]).

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
    case os:getenv("ALITER_CONFIG") of
        false ->
            get_config_filename(Key, Fallback, "aliter.config");
        ConfigFileName ->
            get_config_filename(Key, Fallback, ConfigFileName)
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

is_equip(#world_item{type=X}) when X == ?IT_WEAPON orelse
                                   X == ?IT_ARMOR orelse
                                   X == ?IT_PETEGG orelse
                                   X == ?IT_PETARMOR ->
    true;
is_equip(_) ->
    false.
