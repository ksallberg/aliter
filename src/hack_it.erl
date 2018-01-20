-module(hack_it).

-include("records.hrl").

-export([ show_char/0
        , mod_char/0
        , mod_upd/1 ]).

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
    _NewMap = <<"prontera">>,
    NewMap = <<"prt_fild00">>,
    Ch#char{map = NewMap,
            save_map = NewMap,
            str = 1,
            agi = 1,
            vit = 1,
            int = 2,
            dex = 1,
            luk = 1,
            base_level = 20,
            max_hp = 9999,
            job = 14
           }.
