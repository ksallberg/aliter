-module(zone_worker).

-behaviour(gen_server).

-include("records.hrl").
-include("ro.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([ start_link/3 ]).

-export([ init/1 ]).

-export([ show_actors/1
        , say/2
        , send/2]).

-export([ code_change/3
        , format_status/2
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2 ]).

-define(WALKSPEED, 150).

send(#zone_state{tcp = TCP, packet_handler = PacketHandler}, Packet) ->
    ragnarok_proto:send_packet(Packet, TCP, PacketHandler).

start_link(TCP, PacketHandler, Server) ->
    gen_server:start_link(?MODULE, [TCP, PacketHandler, Server], []).

init([TCP, PacketHandler, Server]) ->
    process_flag(trap_exit, true),
    {ok, #zone_state{tcp = TCP,
                     packet_handler = PacketHandler, server = Server}}.

handle_cast({connect, AccountID, CharacterID, SessionIDa, _Gender}, State) ->
    Session =
        gen_server:call(char_server,
                        {verify_session, AccountID, CharacterID, SessionIDa}),
    case Session of
        {ok, Worker} ->
            {ok, C = #char_state{char = Char}} =
                gen_server:call(Worker, switch_zone),
            {ok, Map, MapServer} =
                gen_server:call(State#zone_state.server, {add_player,
                                                          Char#char.map,
                                                          {AccountID, self()}}),
            send(State, {account_id, AccountID}),
            send(State, {accept, {zone_master:tick(),
                                  {Char#char.x, Char#char.y, 0}}}),
            Items = case db:get_player_items(Char#char.id) of
                        [] ->
                            [];
                        [#inventory{items=ItemsX}] ->
                            ItemsX
                    end,
            send(State, {inventory, Items}),
            WorldItems = db:get_world_items(Char#char.map),
            case Char#char.guild_id of
                0 ->
                    ok;
                GuildID ->
                    send(State, {guild_status, master}),
                    Guild = db:get_guild(GuildID),
                    send(State, {update_gd_id, Guild})
            end,
            Skills = [ {50,  0, 9, 1, 0, "TF_STEAL", 1}
                     , {28,  1, 9, 1, 0, "TF_HEAL", 1}
                     , {394, 1, 9, 1, 0, "TF_ARROW_VULCAN", 1}
                     , {136, 1, 9, 1, 0, "TF_SONIC_BLOW", 1}],
            send(State, {skill_list, Skills}),
            say("Welcome to Aliter.", State),
            NewState = State#zone_state{map = Map,
                                        map_server = MapServer,
                                        account = C#char_state.account,
                                        char = C#char_state.char,
                                        id_a = C#char_state.id_a,
                                        id_b = C#char_state.id_b,
                                        packet_ver = C#char_state.packet_ver,
                                        char_worker = Worker},
            %% client needs some time before receiving objects on ground
            timer:sleep(1000),
            lists:foreach(
              fun(Item) ->
                      send(State, {item_on_ground, {Item#world_item.obj_id,
                                                    Item#world_item.item,
                                                    1,
                                                    Item#world_item.x,
                                                    Item#world_item.y,
                                                    1,
                                                    2,
                                                    Item#world_item.amount
                                                   }})
              end,
              WorldItems),

            {noreply, NewState};
        invalid ->
            lager:log(warning, "Invalid zone login attempt caught ~p ~p",
                      [{account_id, AccountID},
                       {character_id, CharacterID}]),
            {noreply, State}
    end;
handle_cast({set_server, Server}, State) ->
    {noreply, State#zone_state{server = Server}};
handle_cast({npc_activate, ActorID},
            State = #zone_state{map_server=MapServer}) ->
    case gen_server:call(MapServer, {get_actor, ActorID}) of
        {npc, NPC} ->
            ZoneWorker = self(),
            SpawnFun = fun() ->
                               zone_npc:spawn_logic(ZoneWorker, ActorID,
                                                    NPC#npc.main)
                       end,
            Proc = spawn(SpawnFun),
            {noreply, State#zone_state{npc = {Proc, NPC}}};
        _Invalid ->
            lager:log(error, self(), "NPC not found ~p", [{id, ActorID}]),
            {noreply, State}
    end;
%% TODO: handle selecting 255 (Cancel button)
handle_cast({npc_menu_select, _ActorID, Selection},
            State = #zone_state{npc = {Pid, _NPC}}) ->
    Pid ! Selection,
    {noreply, State};
handle_cast({npc_next, _ActorID}, State = #zone_state{npc = {Pid, _NPC}}) ->
    Pid ! continue,
    {noreply, State};
handle_cast({npc_close, _ActorID}, State = #zone_state{npc = {Pid, _NPC}}) ->
    Pid ! close,
    {noreply, State};
handle_cast(map_loaded, State) ->
    show_actors(State),
    {noreply, State};
handle_cast({create_guild, CharId, GName},
            State = #zone_state{char = Char}) ->
    #char{name=CharName} = Char,
    Guild = #guild{name        = GName,
                   master_id   = CharId,
                   members     = [CharId],
                   master_name = CharName
                  },
    GuildSaved = db:save_guild(Guild),
    GuildID = GuildSaved#guild.id,
    NewChar = Char#char{guild_id=GuildID},
    %% Update char
    db:save_char(NewChar),
    %% Notify client
    send(State, {guild_status, master}),
    send(State, {update_gd_id, GuildSaved}),
    say("Welcome to guild " ++ GName ++ ".", State),
    {noreply, State#zone_state{char=NewChar}};
handle_cast({action_request, _Target, 2},
            State = #zone_state{map_server = MapServer,
                                account = #account{id=AccountID},
                                char = #char{x=X, y=Y}}) ->
    Msg = {AccountID, 0, zone_master:tick(), 0, 0, 0, 0, 2, 0},
    gen_server:cast(MapServer,
                    {send_to_players_in_sight, {X, Y}, actor_effect, Msg}),
    {noreply, State};
handle_cast({action_request, _Target, 3},
            State = #zone_state{map_server = MapServer,
                                account = #account{id = AID},
                                char = #char{x = X, y = Y}}) ->
    gen_server:cast(MapServer,
                    {send_to_players_in_sight, {X, Y}, actor_effect,
                     {AID, 0, zone_master:tick(), 0, 0, 0, 0, 3, 0}}),
    {noreply, State};
%% trying to attack self
handle_cast({action_request, Target, 7},
            State = #zone_state{account=#account{id=ID}}) when Target =:= ID ->
    {noreply, State};
%% re-attack is already ongoing, target is the same
handle_cast({action_request, Target, 7},
            #zone_state{attack_timer=Timer, attack_target=Target}=State)
  when Timer /= undefined ->
    {noreply, State};
%% switching attack target
handle_cast({action_request, _Target, 7} = Msg,
            #zone_state{attack_timer=Timer}=State)
  when Timer /= undefined ->
    erlang:cancel_timer(Timer),
    gen_server:cast(self(), Msg),
    {noreply, State#zone_state{attack_timer=undefined}};
%% initiate attack
handle_cast({action_request, Target, 7},
            State = #zone_state{map_server=MapServer,
                                account=#account{id=AID},
                                char=Char}) ->
    Victim = gen_server:call(MapServer, {get_actor, Target}),
    AttackRes = attack(State, AID, Target, Victim, Char, Target, MapServer),
    case AttackRes of
        {ok, TimerRef} ->
            {noreply, State#zone_state{attack_timer=TimerRef,
                                       attack_target=Target}};
        {error, dead} ->
            {noreply, State#zone_state{attack_timer=undefined}}
    end;
handle_cast({re_attack, Target, 7},
            State = #zone_state{map_server=MapServer,
                                account=#account{id=AID},
                                char=Char}) ->
    Victim = gen_server:call(MapServer, {get_actor, Target}),
    AttackRes = attack(State, AID, Target, Victim, Char, Target, MapServer),
    case AttackRes of
        {ok, TimerRef} ->
            {noreply, State#zone_state{attack_timer=TimerRef,
                                       attack_target=Target}};
        {error, dead} ->
            {noreply, State#zone_state{attack_timer=undefined}}
    end;
handle_cast({use_skill, SkillLvl, 136=SkillID, TargetID},
            State = #zone_state{map_server=MapServer,
                                account=#account{id=AID},
                                char=#char{x=X, y=Y}}) ->
    Dmg = 1000,
    ActorRes = gen_server:call(MapServer, {get_actor, TargetID}),
    {PlayerOrMob, _} = ActorRes,
    Worker = get_actor_worker(ActorRes),
    {ok, NewHp} = gen_server:call(Worker, {dec_hp, Dmg}),
    SrcDelay = 0,
    TargetDelay = 0,
    Div = 10,
    Type = 1,
    Msg = {SkillID, AID, TargetID, zone_master:tick(),
           SrcDelay, TargetDelay, Dmg, SkillLvl, Div, Type},
    Msg2 = {TargetID, ?VANISH_DIED},
    send(State, {notify_skill, Msg}),
    gen_server:cast(MapServer,
                    {send_to_players_in_sight, {X, Y}, notify_skill, Msg}),
    case NewHp of
        0 when PlayerOrMob == player ->
            send(State, {vanish, Msg2}),
            gen_server:cast(MapServer,
                            {send_to_players_in_sight, {X, Y}, vanish, Msg2});
        0 when PlayerOrMob == mob ->
            {_, Mob} = ActorRes,
            gen_server:cast(MapServer, {remove_mob, Mob}),
            gen_server:cast(Worker, stop),
            send(State, {vanish, Msg2}),
            gen_server:cast(MapServer,
                            {send_to_players_in_sight, {X, Y}, vanish, Msg2});
        _ ->
            ok
    end,
    {noreply, State};
%% heal
handle_cast({use_skill, SkillLvl, 28=SkillID, TargetID},
            State = #zone_state{map_server=MapServer,
                                account=#account{id=AID},
                                char=#char{x=X, y=Y}}) ->
    Dmg = 1000,
    ActorRes = gen_server:call(MapServer, {get_actor, TargetID}),
    Worker = get_actor_worker(ActorRes),
    {ok, _NewHp} = gen_server:call(Worker, {inc_hp, Dmg}),
    Msg = {SkillID, SkillLvl, TargetID, AID, 1},
    Msg2 = {46, AID, 1, 1000},
    send(State, {use_skill, Msg}),
    gen_server:cast(MapServer,
                    {send_to_players_in_sight, {X, Y}, use_skill, Msg}),
    send(State, {state_change, Msg2}),
    gen_server:cast(MapServer,
                    {send_to_players_in_sight, {X, Y}, state_change, Msg2}),
    {noreply, State};
handle_cast(cease_attack, #zone_state{attack_timer=undefined} = State) ->
    {noreply, State};
handle_cast(cease_attack, #zone_state{attack_timer=TimerRef} = State) ->
    erlang:cancel_timer(TimerRef),
    {noreply, State#zone_state{attack_timer=undefined}};
handle_cast({wear_equip, Index, Position},
            #zone_state{char=#char{id=CharId}=Char} = State) ->
    [#inventory{items=Items}] = db:get_player_items(CharId),
    #world_item{item=ItemID} = lists:keyfind(Index, #world_item.slot, Items),
    NewEquip = #equip{index = Position,
                      id = ItemID,
                      type = Position, %% Position
                      identified = 1,
                      location = Position,
                      wearstate = Position,
                      is_damaged = 0,
                      refining_level = 0,
                      card1 = 0,
                      card2 = 0,
                      card3 = 0,
                      card4 = 0,
                      hire_expire_date = 0,
                      bind_on_equip_type = 0,
                      sprite_number = 0},
    send(State, {equipment, [NewEquip]}),
    NewChar = db:save_equips_ext(Char, NewEquip),
    {noreply, State#zone_state{char=NewChar}};
%% TODO use GuildID
handle_cast({guild_emblem, _GuildID}, State) ->
    send(State, {guild_relationships, []}),
    {noreply, State};
%% Change ongoing walk
handle_cast({walk, {ToX, ToY, _ToD}},
            State = #zone_state{map = Map,
                                is_walking = true,
                                char = #char{x = X,
                                             y = Y}}) ->
    Path = nif:pathfind(Map#map.id, [X | Y], [ToX | ToY]),
    {noreply, State#zone_state{walk_path = Path,
                               walk_changed = {X, Y}}};
handle_cast({walk, {ToX, ToY, _ToD}},
            State = #zone_state{map = Map,
                                map_server = MapServer,
                                account = #account{id = AccountID},
                                char = C = #char{id = CharacterID,
                                                 x = X,
                                                 y = Y}}) ->
    gen_server:cast(self(), cease_attack),
    PathFound = nif:pathfind(Map#map.id, [X | Y], [ToX | ToY]),
    case PathFound of
        [{SX, SY, SDir} | Path] ->
            {ok, Timer} = walk_interval(SDir),
            {FX, FY, _FDir} = lists:last(PathFound),
            Msg = {send_to_other_players_in_sight,
                   {X, Y},
                   CharacterID,
                   actor_move,
                   {AccountID, {X, Y}, {FX, FY}, zone_master:tick()}},
            gen_server:cast(MapServer, Msg),
            send(State, {move, {{X, Y}, {FX, FY}, zone_master:tick()}}),
            {noreply, State#zone_state{char = C#char{x = SX, y = SY},
                                       is_walking = true,
                                       walk_timer = Timer,
                                       walk_prev = {erlang:timestamp(), SDir},
                                       walk_path = Path}};
        _Error ->
            {noreply, State}
    end;
handle_cast(step, State = #zone_state{char = C,
                                      account = A,
                                      map_server = MapServer,
                                      walk_timer = Timer,
                                      walk_prev = {Time, PDir},
                                      walk_path = Path,
                                      walk_changed = Changed}) ->
    case Path of
        [] ->
            timer:cancel(Timer),
            NewState = State#zone_state{walk_timer = undefined,
                                        walk_path = undefined,
                                        is_walking = false,
                                        walk_changed = false},
            {noreply, NewState};
        [{CX, CY, CDir} | Rest] ->
            if
                CDir == PDir ->
                    NewTimer = Timer;
                true ->
                    timer:cancel(Timer),
                    {ok, NewTimer} = walk_interval(CDir)
            end,
            case Changed of
                {X, Y} ->
                    {FX, FY, _FDir} = lists:last(Path),
                    gen_server:cast(
                      MapServer,
                      { send_to_other_players_in_sight,
                        {X, Y},
                        C#char.id,
                        actor_move,
                        {A#account.id, {X, Y}, {FX, FY}, zone_master:tick()}
                      }
                     ),
                    send(State, {move, {{X, Y}, {FX, FY}, zone_master:tick()}});
                _ -> ok
            end,
            NewState = State#zone_state{char = C#char{x = CX, y = CY},
                                        walk_timer = NewTimer,
                                        walk_prev = {Time, CDir},
                                        walk_path = Rest,
                                        walk_changed = false},
            {noreply, NewState}
    end;
handle_cast({send_packet_if, Pred, Packet, Data}, State) ->
    case Pred(State) of
        true ->
            send(State, {Packet, Data});
        false ->
            ok
    end,
    {noreply, State};
handle_cast(quit, State) ->
    send(State, {quit_response, 0}),
    {noreply, State};
handle_cast({char_select, _Type}, State) ->
    send(State, {confirm_back_to_char, {1}}),
    {noreply, State};
handle_cast({request_name, ActorID},
            State = #zone_state{account = #account{id = AccountID},
                                char = #char{name = CharacterName,
                                             guild_id = GuildID},
                                map_server = MapServer}) ->
    Name =
        if
            ActorID == AccountID ->
                case GuildID of
                    0 ->
                        GuildName = "";
                    _ ->
                        #guild{name=GuildName} = db:get_guild(GuildID)
                end,
                {actor_name_full,
                 {ActorID, CharacterName, <<"">>, GuildName, <<"">>}};
                         %%               party          guild position
            true ->
                case gen_server:call(MapServer, {get_actor, ActorID}) of
                    {player, Worker} ->
                        {ok, #zone_state{char=#char{name=CharName,
                                                    guild_id=OtherGuildID}}}
                            = gen_server:call(Worker, get_state),
                        case OtherGuildID of
                            0 ->
                                OtherGuildName = "";
                            _ ->
                                #guild{name=OtherGuildName}
                                    = db:get_guild(OtherGuildID)
                        end,
                        {actor_name_full, {ActorID,
                                           CharName,
                                           "", %% party
                                           OtherGuildName,
                                           ""}}; %% guild position
                    {mob, Mob} ->
                        {actor_name, {ActorID, Mob#npc.name}};
                    {npc, NPC} ->
                        {actor_name, {ActorID, NPC#npc.name}};
                    none ->
                        "Unknown"
                end
        end,
    send(State, Name),
    {noreply, State};
handle_cast(player_count, State) ->
    Num = gen_server:call(zone_master, player_count),
    send(State, {player_count, Num}),
    {noreply, State};
handle_cast({emotion, Id},
            State = #zone_state{map_server = MapServer,
                                account = #account{id = AccountID},
                                char = #char{id = CharacterID,
                                             x = X,
                                             y = Y}}) ->
    Map = {send_to_other_players_in_sight, {X, Y}, CharacterID,
           emotion, {AccountID, Id}},
    gen_server:cast(MapServer, Map),
    send(State, {emotion, {AccountID, Id}}),
    {noreply, State};
handle_cast({speak, Message},
            State = #zone_state{map_server = MapServer,
                                account = #account{id = AccountID},
                                char = #char{id = CharacterID,
                                             x = X,
                                             y = Y}}) ->
    [_Name | Rest] = re:split(Message, " : ", [{return, list}]),
    Said = lists:concat(Rest),
    if
        (hd(Said) == 92) and (length(Said) > 1) -> % GM command
            [Command | Args] = zone_commands:parse(tl(Said)),
            Worker = self(),
            spawn(
              fun() ->
                      zone_commands:execute(Worker, Command, Args, State)
              end
             );
        true ->
            gen_server:cast(MapServer, {send_to_other_players_in_sight, {X, Y},
                                        CharacterID, actor_message,
                                        {AccountID, Message}}),
            send(State, {message, Message})
    end,
    {noreply, State};
handle_cast({broadcast, Message}, State) ->
    gen_server:cast(zone_master,
                    {send_to_all, {send_to_all,
                                   {send_to_players, broadcast, Message}}}),
    {noreply, State};
handle_cast({switch_zones, Update}, State) ->
    {stop, normal, Update(State)};
handle_cast({hat_sprite, SpriteID},
      #zone_state{char=#char{x=X, y=Y, id=_CharacterID,
                             account_id=AID} = Char,
                  map_server=MapServer} = State) ->
    send(State, {sprite, {AID, 4, SpriteID}}),
    Msg = {send_to_other_players_in_sight, {X, Y},
           AID,
           sprite,
           {AID, 4, SpriteID}},
    gen_server:cast(MapServer, Msg),
    NewChar = Char#char{view_head_top=SpriteID},
    db:save_char(NewChar),
    {noreply, State#zone_state{char=NewChar}};
handle_cast({change_job, JobID},
      #zone_state{char=#char{x=X, y=Y, account_id=AID}=Char,
                  map_server=MapServer}=State) ->
    NewChar = Char#char{job=JobID},
    NewState = State#zone_state{char=NewChar},
    db:save_char(NewChar),
    send(State, {sprite, {AID, 0, JobID}}),
    Msg = {send_to_other_players_in_sight, {X, Y},
           AID,
           sprite,
           {AID, 0, JobID}},
    gen_server:cast(MapServer, Msg),
    {noreply, NewState};
handle_cast(heal,
            #zone_state{char=#char{max_hp=MaxHp, max_sp=MaxSp}=Char} = State) ->
    NewChar = Char#char{hp=MaxHp, sp=MaxSp},
    send(State, {param_change, {?SP_MAX_HP, MaxHp}}),
    send(State, {param_change, {?SP_CUR_HP, MaxHp}}),
    send(State, {param_change, {?SP_MAX_SP, MaxSp}}),
    send(State, {param_change, {?SP_CUR_SP, MaxSp}}),
    {noreply, State#zone_state{char=NewChar}};
handle_cast(max_stats, #zone_state{char=Char} = State) ->
    NewChar = Char#char{str = 98,
                        agi = 99,
                        vit = 99,
                        int = 99,
                        dex = 99,
                        luk = 99,
                        base_level = 20,
                        max_hp = 9999,
                        max_sp = 1000},
    db:save_char(NewChar),
    {noreply, State#zone_state{char=NewChar}};
handle_cast({monster, SpriteID, X, Y},
            #zone_state{map=Map, map_server=MapServer,
                        char=#char{account_id=AID},
                        tcp=TCP, packet_handler=PacketHandler} = State) ->
    MonsterID = gen_server:call(MapServer, next_id),
    {ok, MonsterSrv} = supervisor:start_child(monster_sup,
                                              [10000, TCP,
                                               MonsterID, PacketHandler]),
    NPC = #npc{id=MonsterID,
               name=monsters:strname(SpriteID),
               sprite=SpriteID,
               monster_srv=MonsterSrv,
               map=Map,
               coordinates={X, Y},
               direction=north,
               main=0},
    Srv = zone_map:server_for(Map),
    gen_server:cast(Srv, {register_mob, NPC}),
    send(State, {monster, {SpriteID, X, Y, MonsterID}}),
    Msg = {send_to_other_players_in_sight, {X, Y},
           AID,
           monster,
           {SpriteID, X, Y, MonsterID}},
    gen_server:cast(MapServer, Msg),
    {noreply, State};
handle_cast({npc, SpriteID, X, Y},
            #zone_state{map=Map, map_server=MapServer,
                        char=#char{account_id=AID}} = State) ->
    MonsterID = gen_server:call(MapServer, next_id),
    NPC = #npc{id=MonsterID,
               name="npc",
               sprite=SpriteID,
               map=Map,
               coordinates={X, Y},
               direction=north,
               objecttype=6,
               main=0},
    gen_server:cast(zone_map:server_for(Map), {register_npc, NPC}),
    send(State, {show_npc, NPC}),
    Msg = {send_to_other_players_in_sight, {X, Y},
           AID,
           show_npc,
           NPC},
    gen_server:cast(MapServer, Msg),
    {noreply, State};
handle_cast({give_item, ID, Amount, EquipLocation},
            State = #zone_state{char = #char{id = CharacterID}}) ->
    give_item(State, CharacterID, ID, Amount, EquipLocation),
    {noreply, State};
handle_cast(stop, State = #zone_state{char_worker = Char}) ->
    gen_server:cast(Char, exit),
    {stop, normal, State};
handle_cast({tick, _Tick}, State) ->
    send(State, {tick, zone_master:tick()}),
    {noreply, State};
handle_cast({send_packet, Packet, Data}, State) ->
    send(State, {Packet, Data}),
    {noreply, State};
handle_cast({send_packets, Packets},
            #zone_state{tcp = Socket,
                        packet_handler = PacketHandler} = State) ->
    ragnarok_proto:send_packets(Socket, Packets, PacketHandler),
    {noreply, State};
handle_cast({show_to, Worker},
            State = #zone_state{account = A, char = C }) ->
    gen_server:cast(Worker, {send_packet, change_look, C}),
    gen_server:cast(Worker, {send_packet, actor, {normal, A, C}}),
    {noreply, State};
handle_cast({update_state, Fun}, State) ->
    {noreply, Fun(State)};
handle_cast(crash, _) ->
    exit('crash induced');
handle_cast(request_guild_status,
            State = #zone_state{char = #char{id = CharacterID,
                                             guild_id = GuildID}}) ->
    if
        GuildID /= 0 ->
            GetGuildMaster = db:get_guild_master(GuildID),
            case GetGuildMaster of
                CharacterID ->
                    send(State, {guild_status, master});
                _ ->
                    send(State, {guild_status, member})
            end;
        true ->
            send(State, {guild_status, none})
    end,
    {noreply, State};
handle_cast({request_guild_info, 0},
            State = #zone_state{
                       char = #char{guild_id = GuildID}
                      }) when GuildID /= 0 ->
    GetGuild = db:get_guild(GuildID),
    case GetGuild of
        %% TODO?
        nil -> ok;
        G ->
            send(State, {guild_info, G}),
            send(State, {guild_relationships, G#guild.relationships})
    end,
    {noreply, State};
handle_cast({request_guild_info, 1},
            State = #zone_state{
                       char = #char{guild_id = GuildID}
                      }) when GuildID /= 0 ->
    %% Getmembers will be [] or list with character records
    GetMembers = db:get_guild_members(GuildID),
    send(State, {guild_members, GetMembers}),
    {noreply, State};
handle_cast({request_guild_info, 2}, State) ->
    {noreply, State};
handle_cast({less_effect, _IsLess}, State) ->
    {noreply, State};
handle_cast({drop, Slot, Amount},
            State = #zone_state{
                       map_server = MapServer,
                       char = #char{
                                 id = CharacterID,
                                 map = Map,
                                 x = X,
                                 y = Y}}) ->
    io:format("drop item: ~p\n", [CharacterID]),
    send(State, {drop_item, {Slot, Amount}}),
    case db:get_player_item(CharacterID, Slot) of
        nil ->
            say("Invalid item.", State);
        Item ->
            db:remove_player_item(CharacterID, Slot),
            %% if
            %%     Amount == Item#world_item.amount ->
            %%         db:remove_player_item(CharacterID, Slot);
            %%     true ->
            %%         %% TODO: update amount
            %%         ok
            %% end
            %% TODO
            ObjectID = db:give_world_item(Map, Item#world_item.item,
                                          Amount, X+1, Y+1),
            Msg = {ObjectID, Item#world_item.item, 1, X+1, Y+1, 1, 2, Amount},
            gen_server:cast(MapServer,
                            {send_to_players_in_sight, {X, Y},
                             item_on_ground, Msg})
    end,
    {noreply, State};
handle_cast({pick_up, ObjectID},
            State = #zone_state{map_server = MapServer,
                                account = #account{id = AccountID},
                                char = #char{id = CharacterID,
                                             map = Map,
                                             x = X,
                                             y = Y
                                            }}) ->
    gen_server:cast(MapServer, {send_to_players, item_disappear, ObjectID}),
    Msg = {AccountID, ObjectID, zone_master:tick(), 0, 0, 0, 0, 1, 0},
    gen_server:cast(MapServer,
                    {send_to_players_in_sight, {X, Y}, actor_effect, Msg}),
    case db:get_world_item(Map, ObjectID) of
        nil ->
            say("Item already picked up", State);
        Item ->
            db:remove_world_item(Map, ObjectID),
            give_item(State, CharacterID,
                      Item#world_item.item, Item#world_item.amount,
                      Item#world_item.slot)
    end,
    {noreply, State};
handle_cast({change_direction, Head, Body},
            State = #zone_state{map_server = MapServer,
                                account = #account{id = AccountID},
                                char = #char{id = CharacterID,
                                             x = X,
                                             y = Y}}) ->
    Msg = {send_to_other_players_in_sight, {X, Y},
           CharacterID,
           change_direction,
           {AccountID, Head, Body}},
    gen_server:cast(MapServer, Msg),
    {noreply, State};
handle_cast(exit, State) ->
    lager:log(error, self(), "Zone Worker got EXIT signal", []),
    {stop, normal, State};
handle_cast(Other, State) ->
    lager:log(warning, self(), "Zone Worker got unknown request: ~p", [Other]),
    {noreply, State}.

terminate(_Reason, #zone_state{map_server = MapServer,
                               account = #account{id = AccountID},
                               char = Character}) ->
    Msg = {send_to_other_players, Character#char.id, vanish,
           {AccountID, ?VANISH_LOGGED_OUT}},
    gen_server:cast(MapServer, Msg),
    gen_server:cast(char_server, {save_char, Character}),
    gen_server:cast(MapServer, {remove_player, AccountID});
terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

format_status(_Opt, _) ->
    ok.

handle_call({dec_hp, Dmg}, _From,
            #zone_state{char=#char{hp=Hp}=Char} = State) ->
    NewHp = max(Hp - Dmg, 0),
    NewChar = Char#char{hp=NewHp},
    send(State, {param_change, {?SP_CUR_HP, NewHp}}),
    {reply, {ok, NewHp}, State#zone_state{char=NewChar}};
handle_call({inc_hp, Dmg}, _From,
            #zone_state{char=#char{hp=Hp, max_hp=MaxHp}=Char} = State) ->
    NewHp = min(Hp + Dmg, MaxHp),
    NewChar = Char#char{hp=NewHp},
    send(State, {param_change, {?SP_CUR_HP, NewHp}}),
    {reply, {ok, NewHp}, State#zone_state{char=NewChar}};
handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State}.

handle_info({timeout, _Ref, keep_attacking},
            #zone_state{attack_target=Target} = State) ->
    gen_server:cast(self(), {re_attack, Target, 7}),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

%% Helper walking function
walk_interval(N) ->
    Interval = case N band 1 of
                   1 ->
                       %% Walking diagonally.
                       trunc(?WALKSPEED * 1.4);
                   0 ->
                       %% Walking straight.
                       ?WALKSPEED
               end,
    timer:apply_interval(Interval, gen_server, cast, [self(), step]).

show_actors(#zone_state{map_server = MapServer,
                        char = #char{hp=Hp, max_hp=MaxHp,
                                     sp=Sp, max_sp=MaxSp,
                                     equips=Equips
                                    } = C,
                        account = A
                       } = State) ->
    send(State, {status, C}), %% Send stats to client
    send(State, {status, C}),
    send(State, {param_change, {?SP_MAX_HP, MaxHp}}),
    send(State, {param_change, {?SP_CUR_HP, Hp}}),
    send(State, {param_change, {?SP_MAX_SP, MaxSp}}),
    send(State, {param_change, {?SP_CUR_SP, Sp}}),
    send(State, {equipment, Equips}),
    gen_server:cast(MapServer,
                    {send_to_other_players, C#char.id, change_look, C}),
    gen_server:cast(MapServer,
                    {send_to_other_players, C#char.id, actor, {new, A, C}}),
    gen_server:cast(MapServer,
                    {show_actors, {A#account.id, self()}}).

say(Message, State) ->
    send(State, {message, Message}).

give_item(#zone_state{} = State, CharacterID,
          ID, Amount, EquipLocation) ->
    Slot = db:give_player_item(CharacterID, ID, Amount),
    send(State, {give_item, {Slot,   %% Index
                             Amount, %% Amount
                             ID,     %% ID
                             1,      %% Identified
                             0,      %% Damaged
                             0,      %% Refined
                             0,      %% Card1
                             0,      %%     2
                             0,      %%     3
                             0,      %%     4
                             EquipLocation,
                             4,      %% Type
                             0,      %% Result
                             0,      %% ExpireTime
                             0}}).    %% BindOnEquipType

attack(_State, _AID, _Target, none, _Char, _Target, _MapServer) ->
    {error, dead};
attack(State, AID, Target, {mob, Mob}, #char{x=X, y=Y, str=Str, agi=Agi},
       _Target, MapServer) ->
    SrcSpeed = 100,
    DstSpeed = 100,
    Dmg = 200 + rand:uniform(100) - rand:uniform(100) + Str,
    IsSPDamage = 0,
    Div = 0,
    Dmg2 = 0,
    Msg = {AID, Target, zone_master:tick(), SrcSpeed, DstSpeed,
           Dmg, IsSPDamage, Div, Dmg2, ?BDT_NORMAL},
    Msg2 = {Target, ?VANISH_DIED},
    {ok, NewHp} = gen_server:call(Mob#npc.monster_srv, {dec_hp, Dmg}),
    send(State, {attack, Msg}),
    gen_server:cast(MapServer,
                    {send_to_players_in_sight, {X, Y}, attack, Msg}),
    case NewHp =< 0 of
        false ->
            Delay = 75 + (100 - Agi) * 3,
            TimerRef = erlang:start_timer(Delay, self(), keep_attacking),
            {ok, TimerRef};
        true ->
            gen_server:cast(MapServer, {remove_mob, Mob}),
            gen_server:cast(Mob#npc.monster_srv, stop),
            send(State, {vanish, Msg2}),
            gen_server:cast(MapServer,
                            {send_to_players_in_sight, {X, Y}, vanish, Msg2}),
            {error, dead}
    end;
attack(State, AID, Target, {player, Worker},
       #char{x=X, y=Y, str=_Str, agi=_Agi}, _Target, MapServer) ->
    SrcSpeed = 100,
    DstSpeed = 100,
    Dmg = 1,
    IsSPDamage = 0,
    Div = 0,
    Dmg2 = 0,
    Msg = {AID, Target, zone_master:tick(), SrcSpeed, DstSpeed,
           Dmg, IsSPDamage, Div, Dmg2, ?BDT_NORMAL},
    Msg2 = {Target, ?VANISH_DIED},
    {ok, NewHp} = gen_server:call(Worker, {dec_hp, Dmg}),
    send(State, {attack, Msg}),
    gen_server:cast(MapServer,
                    {send_to_players_in_sight, {X, Y}, attack, Msg}),
    case NewHp =< 0 of
        false ->
            TimerRef = erlang:start_timer(300, self(), keep_attacking),
            {ok, TimerRef};
        true ->
            send(State, {vanish, Msg2}),
            gen_server:cast(MapServer,
                            {send_to_players_in_sight, {X, Y}, vanish, Msg2}),
            {error, dead}
    end.

get_actor_worker({player, Worker}) ->
    Worker;
get_actor_worker({mob, #npc{monster_srv=Worker}}) ->
    Worker.
