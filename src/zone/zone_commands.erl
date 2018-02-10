-module(zone_commands).

-include("ro.hrl").
-include("records.hrl").

-export([ parse/1
        , execute/4 ]).

parse(String) ->
    string:tokens(String, " ").

execute(_FSM, "caps", Args,
        State = #zone_state{tcp = TCP,
                            map_server = MapServer,
                            account = #account{id = AccountID},
                            char = #char{id = CharacterID,
                                         x = X,
                                         y = Y}}) ->
    Capitalized = string:to_upper(string:join(Args, " ")),
    gen_server:cast(
      MapServer,
      { send_to_other_players_in_sight,
        {X, Y},
        CharacterID,
        actor_message,
        {AccountID, Capitalized}
      }),
    TCP ! {message, Capitalized},
    {ok, State};
execute(FSM, "crash", _Args, _State) ->
    gen_statem:cast(FSM, crash);
execute(FSM, "load", _Args, State = #zone_state{char = C}) ->
    warp_to(FSM, C#char.save_map, C#char.save_x, C#char.save_y, State);
execute(FSM, "warp", [Map | [XStr | [YStr | _]]], State) ->
    case {string:to_integer(XStr), string:to_integer(YStr)} of
        {{X,_}, {Y,_}} when is_integer(X), is_integer(Y) ->
            warp_to(FSM, list_to_binary(Map), X, Y, State);

        _Invalid ->
            zone_fsm:say("Invalid coordinates.", State),
            {ok, State}
    end;
execute(FSM, "jumpto", [PlayerName | _], State) ->
    case gen_server:call(zone_master,
                         {get_player_by,
                          fun(#zone_state{char = C}) ->
                                  C#char.name == PlayerName
                          end
                         }) of
        {ok, #zone_state{char = C}} ->
            warp_to(FSM, C#char.map, C#char.x, C#char.y, State);
        none ->
            zone_fsm:say("Player not found.", State)
    end;
execute(FSM, "zeny", [AddZeny], State) ->
    case string:to_integer(AddZeny) of
        {Zeny, _} when is_integer(Zeny) -> add_zeny(FSM, State, Zeny);
        _Invalid -> zone_fsm:say("Enter a number.", State)
    end;
execute(FSM, "item", [ID], State) ->
    case string:to_integer(ID) of
        {error, _} -> zone_fsm:say("Invalid item ID.", State);
        {ItemID, _} ->
            give_item(FSM, State, ItemID, 1)
    end;
execute(FSM, "hat", [ID], State) ->
    case string:to_integer(ID) of
        {error, _} -> zone_fsm:say("Invalid item ID.", State);
        {SpriteID, _} ->
            change_hat(FSM, State, SpriteID)
    end;
execute(FSM, "monster", [ID, X, Y], State) ->
    case string:to_integer(ID) of
        {error, _} -> zone_fsm:say("Invalid monster ID.", State);
        {MonsterID, _} ->
            {XParse, _} = string:to_integer(X),
            {YParse, _} = string:to_integer(Y),
            spawn_monster(FSM, State, MonsterID, XParse, YParse)
    end;
execute(_FSM, Unknown, _Args, State) ->
    zone_fsm:say("Unknown command `" ++ Unknown ++ "'.", State),
    ok.

warp_to(FSM, Map, X, Y,
        #zone_state{tcp = TCP,
                    map_server = MapServer,
                    account = #account{id = AccountID},
                    char = C} = State) ->
    case gen_server:call(zone_master, {who_serves, Map}) of
        {zone, Port, ZoneServer} ->
            zone_fsm:say(
              ["Warped to ", Map, " (",
               integer_to_list(X), ", ", integer_to_list(Y), ")."],
              State
             ),
            %% TODO: simpler warp if warping on same map
            %% Send warp packets
            gen_server:cast(MapServer,
                            {send_to_other_players_in_sight,
                             {C#char.x, C#char.y},
                             C#char.id,
                             vanish,
                             {AccountID, 3}}),
            %% Move to new Map server
            gen_server:cast(MapServer, {remove_player, AccountID}),
            {ok, NewMap, NewMapServer}
                = gen_server:call(ZoneServer,
                                  {add_player,
                                   Map,
                                   {AccountID, FSM}}),
            NewStateFun = fun(St) ->
                                  St#zone_state{server = ZoneServer,
                                                map = NewMap,
                                                map_server = NewMapServer,
                                                char = C#char{map = Map,
                                                              x = X,
                                                              y = Y}}
                          end,
            zone_fsm:show_actors(NewStateFun(State)),
            gen_statem:cast(FSM, {switch_zones, NewStateFun}),
            TCP ! {warp_zone, {Map, X, Y, ?ZONE_IP, Port}};
        none ->
            zone_fsm:say("Invalid map provided.", State),
            ok
    end.

give_item(FSM, _State, ID, Amount) ->
    gen_statem:cast(FSM, {give_item, ID, Amount}).

change_hat(FSM, _State, ID) ->
    gen_statem:cast(FSM, {sprite, ID}).

spawn_monster(FSM, _State, ID, X, Y) ->
    gen_statem:cast(FSM, {monster, ID, X, Y}).

add_zeny(FSM, State, Zeny) ->
    C = State#zone_state.char,
    OldZeny = C#char.zeny,
    TempNewZeny = OldZeny + Zeny,

    NewZeny = if TempNewZeny > 1000000000 -> 1000000000;
                 TempNewZeny < 0 -> 0;
                 true -> TempNewZeny
              end,
    NewStateFun = fun(St) ->
                          St#zone_state{char = C#char{zeny = NewZeny}}
                  end,
    gen_statem:cast(FSM, {update_state, NewStateFun}),
    State#zone_state.tcp ! {param_change_long, {?SP_ZENY, NewZeny}}.
