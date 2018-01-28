-module(zone_fsm).
-behaviour(gen_fsm).

-include("records.hrl").
-include("ro.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([start_link/1]).

-export([
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([
         init/1,
         locked/2,
         locked/3,
         valid/2,
         valid/3,
         sitting/2,
         sitting/3,
         walking/2,
         walking/3]).

-export([
         show_actors/1,
         say/2]).

-define(WALKSPEED, 150).

send(State, Packet) ->
    State#zone_state.tcp ! Packet.


start_link(TCP) ->
    gen_fsm:start_link(?MODULE, TCP, []).


init({TCP, [DB]}) ->
    process_flag(trap_exit, true),
    {ok, locked, #zone_state{db = DB, tcp = TCP}}.

locked({connect, AccountID, CharacterID, SessionIDa, _Gender},
       State) ->
    Session =
        gen_server:call(char_server,
                        {verify_session, AccountID, CharacterID, SessionIDa}),
    case Session of
        {ok, FSM} ->
            {ok, C = #char_state{char = Char}} =
                gen_statem:call(FSM, switch_zone),
            {ok, Map, MapServer} =
                gen_server:call(State#zone_state.server, {add_player,
                                                          Char#char.map,
                                                          {AccountID, self()}}),
            send(State, {parse, zone_packets:new(C#char_state.packet_ver)}),
            send(State, {account_id, AccountID}),
            send(State, {accept, {zone_master:tick(),
                                  {Char#char.x, Char#char.y, 0}}}),
            Items = db:get_player_items(State#zone_state.db, Char#char.id),
            send(State, {inventory, Items}),
            WorldItems = db:get_world_items(State#zone_state.db, Char#char.map),
            lists:foreach(
              fun(Item) ->
                      send(
                        State,
                        { item_on_ground,
                          { Item#world_item.slot,
                            Item#world_item.item,
                            1, % TODO: identified

                                                % TODO
                            Char#char.x + 1,
                            Char#char.y + 1,
                            1,
                            2,

                            Item#world_item.amount
                          }
                        })
              end,
              WorldItems),

            say("Welcome to Aliter.", State),

            { next_state,
              valid,
              State#zone_state{
                map = Map,
                map_server = MapServer,
                account = C#char_state.account,
                char = C#char_state.char,
                id_a = C#char_state.id_a,
                id_b = C#char_state.id_b,
                packet_ver = C#char_state.packet_ver,
                char_fsm = FSM
               }
            };

        invalid ->
            lager:log(warning, "Invalid zone login attempt caught ~p ~p",
                      [{account_id, AccountID},
                       {character_id, CharacterID}]),
            {next_state, locked, State}
    end;
locked(Event, State) ->
    ?MODULE:handle_event(Event, locked, State).
locked(Event, From, State) ->
    ?MODULE:handle_sync_event(Event, From, locked, State).

valid({npc_activate, ActorID}, State = #zone_state{map_server = MapServer}) ->
    case gen_server:call(MapServer, {get_actor, ActorID}) of
        {npc, NPC} ->
            Env = [{x, NPC#npc.main}, {p, self()}, {i, NPC#npc.id}],
            Pid = spawn(fun() ->
                                elixir:eval("x.new(p, i).main", Env)
                        end),
            {next_state, valid, State#zone_state{npc = {Pid, NPC}}};
        _Invalid ->
            lager:log(error, self(), "NPC not found ~p", [{id, ActorID}]),
            {next_state, valid, State}
    end;

%% TODO: handle selecting 255 (Cancel button)
valid(
  {npc_menu_select, _ActorID, Selection},
  State = #zone_state{npc = {Pid, _NPC}}) ->
    Pid ! Selection,
    {next_state, valid, State};

valid({npc_next, _ActorID}, State = #zone_state{npc = {Pid, _NPC}}) ->
    Pid ! continue,
    {next_state, valid, State};

valid({npc_close, _ActorID}, State = #zone_state{npc = {Pid, _NPC}}) ->
    Pid ! close,
    {next_state, valid, State};

valid(map_loaded, State) -> show_actors(State), {next_state, valid, State};

valid(
  {walk, {ToX, ToY, _ToD}},
  State = #zone_state{
             map = Map,
             map_server = MapServer,
             account = #account{id = AccountID},
             char = C = #char{
                           id = CharacterID,
                           x = X,
                           y = Y
                          }
            }) ->
    PathFound = nif:pathfind(Map#map.id, [X | Y], [ToX | ToY]),
    case PathFound of
        [{SX, SY, SDir} | Path] ->
            {ok, Timer} = walk_interval(SDir),
            {FX, FY, _FDir} = lists:last(PathFound),

            gen_server:cast(
              MapServer,
              { send_to_other_players_in_sight,
                {X, Y},
                CharacterID,
                actor_move,
                {AccountID, {X, Y}, {FX, FY}, zone_master:tick()}
              }
             ),

            send(State, {move, {{X, Y}, {FX, FY}, zone_master:tick()}}),

            { next_state,
              walking,
              State#zone_state{
                char = C#char{x = SX, y = SY},
                walk_timer = Timer,
                walk_prev = {erlang:timestamp(), SDir},
                walk_path = Path
               }
            };

        _Error ->
            {next_state, valid, State}
    end;

valid(
  {action_request, _Target, 2},
  State = #zone_state{
             map_server = MapServer,
             account = #account{id = AccountID},
             char = #char{
                       x = X,
                       y = Y
                      }
            }) ->
    gen_server:cast(
      MapServer,
      { send_to_players_in_sight,
        {X, Y},
        actor_effect,
        {AccountID, 0, zone_master:tick(), 0, 0, 0, 0, 2, 0}
      }
     ),
    {next_state, sitting, State};

valid(Event, State) ->
    ?MODULE:handle_event(Event, valid, State).
valid(Event, From, State) ->
    ?MODULE:handle_sync_event(Event, From, valid, State).

sitting(
  {action_request, _Target, 3},
  State = #zone_state{
             map_server = MapServer,
             account = #account{id = AccountID},
             char = #char{
                       x = X,
                       y = Y
                      }
            }) ->
    gen_server:cast(
      MapServer,
      { send_to_players_in_sight,
        {X, Y},
        actor_effect,
        {AccountID, 0, zone_master:tick(), 0, 0, 0, 0, 3, 0}
      }
     ),
    {next_state, valid, State};

sitting(Event, State) ->
    ?MODULE:handle_event(Event, sitting, State).
sitting(Event, From, State) ->
    ?MODULE:handle_sync_event(Event, From, sitting, State).

walking(
  {walk, {ToX, ToY, _ToD}},
  State = #zone_state{map = Map,
                      char = #char{x = X,
                                   y = Y}}) ->
    Path = nif:pathfind(Map#map.id, [X | Y], [ToX | ToY]),

    {next_state, walking, State#zone_state{walk_path = Path,
                                           walk_changed = {X, Y}}};

walking(
  step,
  State = #zone_state{
             char = C,
             account = A,
             map_server = MapServer,
             walk_timer = Timer,
             walk_prev = {Time, PDir},
             walk_path = Path,
             walk_changed = Changed
            }) ->
    case Path of
        [] ->
            timer:cancel(Timer),
            { next_state,
              valid,
              State#zone_state{
                walk_timer = undefined,
                walk_path = undefined,
                walk_changed = false
               }
            };

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

            { next_state,
              walking,
              State#zone_state{
                char = C#char{x = CX, y = CY},
                walk_timer = NewTimer,
                walk_prev = {Time, CDir},
                walk_path = Rest,
                walk_changed = false
               }
            }
    end;

walking(Event, State) ->
    ?MODULE:handle_event(Event, walking, State).

walking(Event, From, State) ->
    ?MODULE:handle_sync_event(Event, From, walking, State).

handle_event(quit, StateName, State) ->
    send(State, {quit_response, 0}),
    {next_state, StateName, State};

handle_event({char_select, _Type}, StateName, State) ->
    send(State, {confirm_back_to_char, {1}}),
    {next_state, StateName, State};

handle_event(
  {request_name, ActorID},
  StateName,
  State = #zone_state{
             account = #account{id = AccountID},
             char = #char{name = CharacterName},
             map_server = MapServer
            }) ->
    Name =
        if
            ActorID == AccountID ->
                { actor_name_full,
                  {ActorID,
                   CharacterName,
                   "Party Name",
                   "Guild Name",
                   "Tester"}
                };

            true ->
                case gen_server:call(MapServer, {get_actor, ActorID}) of
                    {player, FSM} ->
                        {ok, Z} = gen_fsm:sync_send_all_state_event(FSM,
                                                                    get_state),
                        { actor_name_full,
                          { ActorID,
                            (Z#zone_state.char)#char.name,
                            "Other Party Name",
                            "Other Guild Name",
                            "Other Tester"
                          }
                        };

                    {npc, NPC} ->
                        {actor_name, {ActorID, NPC#npc.name}};

                    none ->
                        "Unknown"
                end
        end,
    send(State, Name),
    {next_state, StateName, State};

handle_event(player_count, StateName, State) ->
    Num = gen_server:call(zone_master, player_count),
    send(State, {player_count, Num}),
    {next_state, StateName, State};

handle_event(
  {emotion, Id},
  StateName,
  State = #zone_state{
             map_server = MapServer,
             account = #account{id = AccountID},
             char = #char{
                       id = CharacterID,
                       x = X,
                       y = Y
                      }
            }) ->
    gen_server:cast(
      MapServer,
      { send_to_other_players_in_sight,
        {X, Y},
        CharacterID,
        emotion,
        {AccountID, Id}
      }
     ),
    send(State, {emotion, {AccountID, Id}}),
    {next_state, StateName, State};

handle_event(
  {speak, Message},
  StateName,
  State = #zone_state{
             map_server = MapServer,
             account = #account{id = AccountID},
             char = #char{
                       id = CharacterID,
                       x = X,
                       y = Y
                      }
            }) ->
    [_Name | Rest] = re:split(Message, " : ", [{return, list}]),
    Said = lists:concat(Rest),
    if
        (hd(Said) == 92) and (length(Said) > 1) -> % GM command
            [Command | Args] = zone_commands:parse(tl(Said)),
            FSM = self(),
            spawn(
              fun() ->
                      zone_commands:execute(FSM, Command, Args, State)
              end
             );
        true ->
            gen_server:cast(
              MapServer,
              { send_to_other_players_in_sight,
                {X, Y},
                CharacterID,
                actor_message,
                {AccountID, Message}
              }
             ),

            send(State, {message, Message})
    end,
    {next_state, StateName, State};

handle_event({broadcast, Message}, StateName, State) ->
    gen_server:cast(
      zone_master,
      %% i love how this looks like noise waves or something
      { send_to_all,
        { send_to_all,
          { send_to_players,
            broadcast,
            Message
          }
        }
      }
     ),
    {next_state, StateName, State};

handle_event({switch_zones, Update}, _StateName, State) ->
    {stop, normal, Update(State)};

handle_event(
  {give_item, ID, Amount},
  StateName,
  State = #zone_state{
             tcp = TCP,
             db = DB,
             char = #char{id = CharacterID}}) ->
    give_item(TCP, DB, CharacterID, ID, Amount),
    {next_state, StateName, State};

handle_event(
  stop,
  _StateName,
  State = #zone_state{char_fsm = Char}) ->
    gen_statem:cast(Char, exit),
    {stop, normal, State};

handle_event({tick, _Tick}, StateName, State) when StateName /= locked ->
    send(State, {tick, zone_master:tick()}),
    {next_state, StateName, State};

handle_event({set_server, Server}, StateName, State) ->
    {next_state, StateName, State#zone_state{server = Server}};

handle_event({send_packet, Packet, Data}, StateName, State) ->
    lager:log(info, self(), "Send packet ~p ~p", [{packet, Packet},
                                                  {data, Data}]),
    send(State, {Packet, Data}),
    {next_state, StateName, State};

handle_event({send_packets, Packets}, StateName, State) ->
    send(State, {send_packets, Packets}),
    {next_state, StateName, State};

handle_event({send_packet_if, Pred, Packet, Data}, StateName, State) ->
    case Pred(State) of
        true ->
            send(State, {Packet, Data});

        false ->
            ok
    end,
    {next_state, StateName, State};

handle_event(
  {show_to, FSM},
  StateName,
  State = #zone_state{
             account = A,
             char = C
            }) ->
    gen_fsm:send_all_state_event(FSM,
                                 { send_packet,
                                   change_look,
                                   C
                                 }
                                ),
    gen_fsm:send_all_state_event(FSM,
                                 { send_packet,
                                   actor,
                                   {normal, A, C}
                                 }
                                ),
    {next_state, StateName, State};

handle_event({get_state, From}, StateName, State) ->
    From ! {ok, State},
    {next_state, StateName, State};

handle_event({update_state, Fun}, StateName, State) ->
    {next_state, StateName, Fun(State)};

handle_event(crash, _, _) ->
    exit('crash induced');

handle_event(
  request_guild_status,
  StateName,
  State = #zone_state{
             db = DB,
             char = #char{
                       id = CharacterID,
                       guild_id = GuildID
                      }
            }) ->
    if
        GuildID /= 0 ->
            GetGuildMaster = db:get_guild_master(DB, GuildID),
            case GetGuildMaster of
                CharacterID ->
                    send(State, {guild_status, master});
                _ ->
                    send(State, {guild_status, member})
            end;
        true ->
            send(State, {guild_status, none})
    end,
    {next_state, StateName, State};

handle_event(
  {request_guild_info, 0},
  StateName,
  State = #zone_state{
             db = DB,
             char = #char{guild_id = GuildID}
            }) when GuildID /= 0 ->
    GetGuild = db:get_guild(DB, GuildID),
    case GetGuild of
        %% TODO?
        nil -> ok;
        G ->
            send(State, {guild_info, G}),
            send(State, {guild_relationships, G#guild.relationships})
    end,
    {next_state, StateName, State};

handle_event(
  {request_guild_info, 1},
  StateName,
  State = #zone_state{
             db = DB,
             char = #char{guild_id = GuildID}
            }) when GuildID /= 0 ->
    GetMembers = gen_server:call(char_server,
                                 {get_chars, db:get_guild_members(DB, GuildID)}),
    case GetMembers of
        {atomic, Members} ->
            send(State, {guild_members, Members});

        _Error ->
            ok
    end,
    {next_state, StateName, State};

handle_event({request_guild_info, 2}, StateName, State) ->
    {next_state, StateName, State};

handle_event({less_effect, _IsLess}, StateName, State) ->
    {next_state, StateName, State};

handle_event({drop, Slot, Amount}, StateName,
             State = #zone_state{
                        db = DB,
                        map_server = MapServer,
                        char = #char{
                                  id = CharacterID,
                                  map = Map,
                                  x = X,
                                  y = Y}}) ->
    send(State, {drop_item, {Slot, Amount}}),
    case db:get_player_item(DB, CharacterID, Slot) of
        nil ->
            say("Invalid item.", State);
        Item ->
            if
                Amount == Item#world_item.amount ->
                    db:remove_player_item(DB, CharacterID, Slot);

                true ->
                    %% TODO: update amount
                    ok
            end,
            %% TODO
            ObjectID = db:give_world_item(DB, Map, Item#world_item.item, Amount),
            gen_server:cast(
              MapServer,
              { send_to_players_in_sight,
                {X, Y},
                item_on_ground,
                %% TODO: actual item ID
                %% TODO: randomize positions
                {ObjectID, Item#world_item.item, 1, X + 1, Y + 1, 1, 2, Amount}
              }
             )
    end,
    {next_state, StateName, State};

handle_event({pick_up, ObjectID}, StateName,
             State = #zone_state{
                        tcp = TCP,
                        db = DB,
                        map_server = MapServer,
                        account = #account{id = AccountID},
                        char = #char{
                                  id = CharacterID,
                                  map = Map,
                                  x = X,
                                  y = Y
                                 }
                       }) ->
    gen_server:cast(
      MapServer,
      {send_to_players, item_disappear, ObjectID}
     ),
    gen_server:cast(
      MapServer,
      { send_to_players_in_sight,
        {X, Y},
        actor_effect,
        { AccountID,
          ObjectID,
          zone_master:tick(),
          0,
          0,
          0,
          0,
          1,
          0
        }
      }
     ),
    case db:get_world_item(DB, ObjectID) of
        nil ->
            say("Item already picked up!", State);

        Item ->
            db:remove_world_item(DB, Map, ObjectID),
            give_item(TCP, DB, CharacterID,
                      Item#world_item.item, Item#world_item.amount)
    end,
    {next_state, StateName, State};

handle_event(
  {change_direction, Head, Body},
  StateName,
  State = #zone_state{
             map_server = MapServer,
             account = #account{id = AccountID},
             char = #char{
                       id = CharacterID,
                       x = X,
                       y = Y
                      }
            }) ->
    gen_server:cast(
      MapServer,
      { send_to_other_players_in_sight,
        {X, Y},
        CharacterID,
        change_direction,
        {AccountID, Head, Body}
      }
     ),
    {next_state, StateName, State};

handle_event(Event, StateName, State) ->
    lager:log(warning, self(), "Zone FSM received unknown event ~p ~p",
              [{event, Event}, {state, StateName}]),
    {next_state, StateName, State}.

handle_sync_event(get_state, _From, StateName, State) ->
    {reply, {ok, State}, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

handle_info({'EXIT', From, Reason}, _StateName, State) ->
    lager:log(error, self(), "Zone FSM got EXIT signal ~p ~p",
              [{from, From}, {reason, Reason}]),
    {stop, normal, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, #zone_state{map_server = MapServer,
                                           account = #account{id = AccountID},
                                           char = Character}) ->
    gen_server:cast(
      MapServer,
      { send_to_other_players,
        Character#char.id,
        vanish,
        {AccountID, 3}
      }
     ),
    gen_server:cast(char_server, {save_char, Character}),
    gen_server:cast(MapServer, {remove_player, AccountID});

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% Helper walking function
walk_interval(N) ->
    Interval =
        case N band 1 of
            1 ->
                %% Walking diagonally.
                trunc(?WALKSPEED * 1.4);
            0 ->
                %% Walking straight.
                ?WALKSPEED
        end,

    timer:apply_interval(
      Interval,
      gen_fsm,
      send_event,
      [self(), step]
     ).

show_actors(
  #zone_state{
     map_server = MapServer,
     char = C,
     account = A
    }) ->
    gen_server:cast(
      MapServer,
      { send_to_other_players,
        C#char.id,
        change_look,
        C
      }
     ),

    gen_server:cast(
      MapServer,
      { send_to_other_players,
        C#char.id,
        actor,
        {new, A, C}
      }
     ),

    gen_server:cast(
      MapServer,
      { show_actors,
        {A#account.id, self()}
      }
     ).

say(Message, State) ->
    send(State, {message, Message}).

give_item(TCP, DB, CharacterID, ID, Amount) ->
    Slot = db:give_player_item(DB, CharacterID, ID, Amount),
    TCP !
        {give_item, {Slot, Amount, ID, 1, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0}}.
