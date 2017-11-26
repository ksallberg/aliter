-module(aliter).

-behaviour(application).

-include("records.hrl").

-export([ start/2
        , shutdown/0
        , stop/1
        , reload/0 ]).

start(_Type, StartArgs) ->
    lager:start(),
    aliter_sup:start_link(StartArgs).

shutdown() ->
    application:stop(aliter).

stop(_State) ->
    ok.

reload() ->
    {ok, Files} = file:list_dir("_build/default/lib/aliter/ebin"),
    Beams = [string:substr(F, 1, length(F) - 5)
             || F <- Files, string:str(F, ".beam") /= 0],
    LoadFun = fun(File) ->
                      code:purge(list_to_atom(File)),
                      code:load_abs("_build/default/lib/aliter/ebin/"
                                    ++ File)
              end,
    lists:foreach(LoadFun, Beams).
