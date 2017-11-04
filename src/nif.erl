-module(nif).
-export([init/0, pathfind/3]).

init() ->
    ok = erlang:load_nif("priv/nif", 0),
    true.

pathfind(_, _, _) ->
    lager:log(error, self(), "NIF pathfinder not loaded", []).
