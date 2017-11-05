-module(hack_it).

-include("records.hrl").

-compile(export_all).

show_char() ->
    {ok, Conn} = redis:connect(),
    CharacterID = 1,
    Char = db:get_char(Conn, CharacterID),
    io:format("Char: ~p\n", [Char]),
    redis:quit(Conn).

mod_char() ->
    {ok, Conn} = redis:connect(),
    CharacterID = 1,
    Char = db:get_char(Conn, CharacterID),
    NewChar = mod_upd(Char),
    db:save_char(Conn, NewChar),
    redis:quit(Conn).

mod_upd(#char{map = _OldMap} = Ch) ->
    %% NewMap = <<"new_1-1">>,
    NewMap = <<"prontera">>,
    Ch#char{map = NewMap,
            save_map=NewMap}.
