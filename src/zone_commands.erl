-module(zone_commands).

-include("ro.hrl").
-include("records.hrl").

-export([ parse/1
        , execute/4 ]).

parse(String) ->
    string:tokens(String, " ").

execute(_Worker, "caps", Args,
        State = #zone_state{map_server = MapServer,
                            account = #account{id = AccountID},
                            char = #char{id = CharacterID,
                                         x = X,
                                         y = Y}}) ->
    Capitalized = string:to_upper(string:join(Args, " ")),
    gen_server:cast(MapServer,
                    {send_to_other_players_in_sight,
                     {X, Y},
                     CharacterID,
                     actor_message,
                     {AccountID, Capitalized}
                    }),
    zone_worker:send(State, {message, Capitalized}),
    {ok, State};
execute(Worker, "crash", _Args, _State) ->
    gen_server:cast(Worker, crash);
execute(Worker, "load", _Args, State = #zone_state{char = C}) ->
    warp_to(Worker, C#char.save_map, C#char.save_x, C#char.save_y, State);
execute(Worker, "warp", [Map | [XStr | [YStr | _]]], State) ->
    case {string:to_integer(XStr), string:to_integer(YStr)} of
        {{X,_}, {Y,_}} when is_integer(X), is_integer(Y) ->
            warp_to(Worker, list_to_binary(Map), X, Y, State);

        _Invalid ->
            zone_worker:say("Invalid coordinates.", State),
            {ok, State}
    end;
execute(Worker, "square", _Args, State) ->
    warp_to(Worker, <<"prontera">>, 147, 203, State);
execute(Worker, "jumpto", [PlayerName | _], State) ->
    case gen_server:call(zone_master,
                         {get_player_by,
                          fun(#zone_state{char = C}) ->
                                  C#char.name == PlayerName
                          end
                         }) of
        {ok, #zone_state{char = C}} ->
            warp_to(Worker, C#char.map, C#char.x, C#char.y, State);
        none ->
            zone_worker:say("Player not found.", State)
    end;
execute(Worker, "zeny", [AddZeny], State) ->
    case string:to_integer(AddZeny) of
        {Zeny, _} when is_integer(Zeny) -> add_zeny(Worker, State, Zeny);
        _Invalid -> zone_worker:say("Enter a number.", State)
    end;
execute(Worker, "item", [ID], State) ->
    case string:to_integer(ID) of
        {error, _} -> zone_worker:say("Invalid item ID.", State);
        {ItemID, _} ->
            give_item(Worker, State, ItemID, 1)
    end;
execute(Worker, "hat", [ID], State) ->
    case string:to_integer(ID) of
        {error, _} -> zone_worker:say("Invalid item ID.", State);
        {SpriteID, _} ->
            change_hat(Worker, State, SpriteID)
    end;
execute(Worker, "monster", [ID, X, Y], State) ->
    case string:to_integer(ID) of
        {error, _} -> zone_worker:say("Invalid monster ID.", State);
        {MonsterID, _} ->
            {XParse, _} = string:to_integer(X),
            {YParse, _} = string:to_integer(Y),
            spawn_monster(Worker, State, MonsterID, XParse, YParse)
    end;
execute(Worker, "npc", [ID, X, Y], State) ->
    case string:to_integer(ID) of
        {error, _} -> zone_worker:say("Invalid NPC ID.", State);
        {NPCID, _} ->
            {XParse, _} = string:to_integer(X),
            {YParse, _} = string:to_integer(Y),
            spawn_npc(Worker, State, NPCID, XParse, YParse)
    end;
execute(Worker, "job", [ID], State) ->
    case string:to_integer(ID) of
        {error, _} -> zone_worker:say("Invalid Job ID.", State);
        {JobID, _} -> change_job(Worker, State, JobID)
    end;
execute(Worker, "heal", _Args, State) ->
    heal(Worker, State);
execute(Worker, "maxstats", _Args, State) ->
    max_stats(Worker, State);
execute(Worker, "guild_add", [Who], State) ->
    guild_add(Worker, State, Who);
execute(_Worker, Unknown, _Args, State) ->
    zone_worker:say("Unknown command `" ++ Unknown ++ "'.", State),
    ok.

warp_to(Worker, Map, X, Y,
        #zone_state{map_server = MapServer,
                    account = #account{id = AccountID},
                    char = C} = State) ->
    case gen_server:call(zone_master, {who_serves, Map}) of
        {zone, Port, ZoneServer} ->
            zone_worker:say(
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
                                   {AccountID, Worker}}),
            NewStateFun = fun(St) ->
                                  St#zone_state{server = ZoneServer,
                                                map = NewMap,
                                                map_server = NewMapServer,
                                                char = C#char{map = Map,
                                                              x = X,
                                                              y = Y}}
                          end,
            zone_worker:show_actors(NewStateFun(State)),
            gen_server:cast(Worker, {switch_zones, NewStateFun}),
            zone_worker:send(State, {warp_zone, {Map, X, Y, ?ZONE_IP, Port}});
        none ->
            zone_worker:say("Invalid map provided.", State),
            ok
    end.

give_item(Worker, _State, ID, Amount) ->
    gen_server:cast(Worker, {give_item, ID, Amount}).

change_hat(Worker, _State, ID) ->
    gen_server:cast(Worker, {hat_sprite, ID}).

change_job(Worker, _State, JobID) ->
    gen_server:cast(Worker, {change_job, JobID}).

heal(Worker, _State) ->
    gen_server:cast(Worker, heal).

max_stats(Worker, _State) ->
    gen_server:cast(Worker, max_stats).

guild_add(Worker, _State, Who) ->
    gen_server:cast(Worker, {guild_add, Who}).

spawn_monster(Worker, _State, ID, X, Y) ->
    gen_server:cast(Worker, {monster, ID, X, Y}).

spawn_npc(Worker, _State, ID, X, Y) ->
    gen_server:cast(Worker, {npc, ID, X, Y}).

add_zeny(Worker, State, Zeny) ->
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
    gen_server:cast(Worker, {update_state, NewStateFun}),
    zone_worker:send(State, {param_change_long, {?SP_ZENY, NewZeny}}).
