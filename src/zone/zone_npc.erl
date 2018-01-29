-module(zone_npc).

-include("records.hrl").

-export([ do_all/2
        , say/3
        , close/2
        , menu/3
        , next/2 ]).

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
