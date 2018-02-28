-module(zone_npc).

-include("records.hrl").

-export([ do_all/2
        , say/3
        , close/2
        , menu/3
        , next/2
        , load_all/0 ]).

load_all() ->
    {ok, NPCs} = file:consult("script/prontera.aliter"),
    DoF = fun({Name, Sprite, Map, Positions, _Actions}) ->
                  Pos = fun({X, Y, Dir}) ->
                                Msg = {register_npc, Name, Sprite,
                                       Map, {X, Y}, Dir, self()},
                                gen_server:cast(zone_master, Msg)
                        end,
                  lists:foreach(Pos, Positions)
          end,
    lists:foreach(DoF, NPCs).

do_all(FSM, Packets) ->
    gen_statem:cast(FSM, {send_packets, Packets}).

say(FSM, NPC, Message) ->
    gen_statem:cast(FSM, {send_packet, dialog, {NPC, Message}}).

menu(FSM, NPC, Choices) ->
    gen_statem:cast(FSM, {send_packet, dialog_menu, {NPC, Choices}}).

next(FSM, NPC) ->
    gen_statem:cast(FSM, {send_packet, dialog_next, NPC}).

close(FSM, NPC) ->
    gen_statem:cast(FSM, {send_packet, dialog_close, NPC}).
