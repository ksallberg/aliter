-module(monsters).

-export([strname/1]).

strname(1373) ->
    "Lord of Death";
strname(1001) ->
    "Little scorpion";
strname(_) ->
    "unknown monster".
