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
    %% NewMap = <<"prontera">>,
    %% _NewMap = <<"prt_fild00">>,
    NewCh = Ch#char{%map = NewMap,
                    %save_map = NewMap,
                    str = 98,
                    agi = 99,
                    vit = 99,
                    int = 99,
                    dex = 99,
                    luk = 99,
                    base_level = 20,
                    max_hp = 9999,
                    max_sp = 1000,
                    job = 14, %% Crusader
                    x = 53,
                    y = 111,
                    save_x = 53,
                    save_y = 111
                   },
    io:format("NewChar: ~p\n", [NewCh]),
    NewCh.
