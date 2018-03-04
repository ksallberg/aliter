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
    case file:consult("script/npcs.aliter") of
        {ok, NPCs} ->
            DoF = fun({Name, Sprite, Map, Positions, Actions}) ->
                          Pos = fun({X, Y, Dir}) ->
                                        Msg = {register_npc, Name, Sprite,
                                               Map, {X, Y}, Dir, Actions},
                                        gen_server:cast(zone_master, Msg)
                                end,
                          lists:foreach(Pos, Positions)
                  end,
            lists:foreach(DoF, NPCs);
        {error, Reason} ->
            lager:log(error,
                      self(),
                      "Critical failure: Could not load NPCs: ~p. Exiting",
                      [Reason]),
            timer:sleep(1000), %% give lager time to print before stopping VM
            erlang:halt()
    end.

do_all(Worker, Packets) ->
    gen_server:cast(Worker, {send_packets, Packets}).

say(Worker, NPC, Message) ->
    gen_server:cast(Worker, {send_packet, dialog, {NPC, Message}}).

menu(Worker, NPC, Choices) ->
    gen_server:cast(Worker, {send_packet, dialog_menu, {NPC, Choices}}).

next(Worker, NPC) ->
    gen_server:cast(Worker, {send_packet, dialog_next, NPC}).

close(Worker, NPC) ->
    gen_server:cast(Worker, {send_packet, dialog_close, NPC}).

spawn_logic(_, _, []) ->
    ok;
spawn_logic(Worker, ActorID, [close | ActionObjectRest]) ->
    zone_npc:close(Worker, ActorID),
    spawn_logic(Worker, ActorID, ActionObjectRest);
spawn_logic(Worker, ActorID, [next | ActionObjectRest]) ->
    zone_npc:next(Worker, ActorID),
    receive
        continue ->
            spawn_logic(Worker, ActorID, ActionObjectRest)
    end;
spawn_logic(Worker, ActorID, [{say, SayMsg} | ActionObjectRest]) ->
    zone_npc:say(Worker, ActorID, SayMsg),
    spawn_logic(Worker, ActorID, ActionObjectRest);
spawn_logic(Worker, ActorID, [{menu, MenuList} | ActionObjectRest]) ->
    MenuAlts = [MenuChoice || {MenuChoice, _MenuAction} <- MenuList],
    zone_npc:menu(Worker, ActorID, MenuAlts),
    receive
        Selected ->
            {_, Action} = lists:nth(Selected, MenuList),
            spawn_logic(Worker, ActorID, Action ++ ActionObjectRest)
    end.
