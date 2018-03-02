-module(zone_npc).

-include("records.hrl").

-export([ do_all/2
        , say/3
        , close/2
        , menu/3
        , next/2
        , load_all/0
        , spawn_logic/3 ]).

load_all() ->
    {ok, NPCs} = file:consult("script/npcs.aliter"),
    DoF = fun({Name, Sprite, Map, Positions, Actions}) ->
                  Pos = fun({X, Y, Dir}) ->
                                Msg = {register_npc, Name, Sprite,
                                       Map, {X, Y}, Dir, Actions},
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

spawn_logic(_, _, []) ->
    ok;
spawn_logic(FSM, ActorID, [close | ActionObjectRest]) ->
    zone_npc:close(FSM, ActorID),
    spawn_logic(FSM, ActorID, ActionObjectRest);
spawn_logic(FSM, ActorID, [next | ActionObjectRest]) ->
    zone_npc:next(FSM, ActorID),
    receive
        continue ->
            spawn_logic(FSM, ActorID, ActionObjectRest)
    end;
spawn_logic(FSM, ActorID, [{say, SayMsg} | ActionObjectRest]) ->
    zone_npc:say(FSM, ActorID, SayMsg),
    spawn_logic(FSM, ActorID, ActionObjectRest);
spawn_logic(FSM, ActorID, [{menu, MenuList} | ActionObjectRest]) ->
    MenuAlts = [MenuChoice || {MenuChoice, _MenuAction} <- MenuList],
    zone_npc:menu(FSM, ActorID, MenuAlts),
    receive
        Selected ->
            {_, Action} = lists:nth(Selected, MenuList),
            spawn_logic(FSM, ActorID, [Action |ActionObjectRest])
    end.
