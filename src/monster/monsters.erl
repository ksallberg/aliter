-module(monsters).

-export([strname/1]).

strname(1373) ->
    "Lord of Death";
strname(1001) ->
    "Little scorpion";
strname(1511) ->
    "Amon Ra";
strname(1039) ->
    "Baphomet";
strname(1272) ->
    "Dark Lord";
strname(1389) ->
    "Dracula";
strname(1112) ->
    "Drake";
strname(1115) ->
    "Eddga";
strname(1252) ->
    "Garm";
strname(1492) ->
    "Incantation Samurai";
strname(1147) ->
    "Maya";
strname(_) ->
    "unknown monster".
