-module(zone_map).
-behaviour(gen_server).

-include("records.hrl").

-export([start_link/1, server_for/1]).

-export([zones/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link(Map) ->
    lager:log(info, self(), "Starting map server ~p ~p ~p",
              [{map, Map#map.name},
               {local, server_for(Map)},
               ?MODULE]),
    gen_server:start_link({local, server_for(Map)},
                          ?MODULE,
                          #map_state{map = Map},
                          []).

init(State) ->
    process_flag(trap_exit, true),
    {ok, State}.

handle_call(
  {get_actor, ActorID},
  _From,
  State = #map_state{
             players = Players,
             npcs = NPCs
            }) ->
    case proplists:lookup(ActorID, Players) of
        {ActorID, FSM} ->
            {reply, {player, FSM}, State};

        none ->
            case lists:keyfind(ActorID, 2, NPCs) of
                none ->
                    {reply, none, State};
                NPC ->
                    {reply, {npc, NPC}, State}
            end
    end;

handle_call(
  {get_player_by, Pred},
  _From,
  State = #map_state{players = Players}) ->
    {reply, get_player_by(Pred, Players), State};

handle_call(
  player_count,
  _From,
  State = #map_state{players = Players}) ->
    {reply, length(Players), State};

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast( {add_player, Player}, State = #map_state{players = Players}) ->
    {noreply, State#map_state{players = [Player | Players]}};

handle_cast({register_npc, NPC}, State = #map_state{npcs = NPCs}) ->
    %% TODO: make it appear on screen for anyone around it
    {noreply, State#map_state{npcs = [NPC | NPCs]}};

handle_cast({remove_player, AccountID},
            State = #map_state{players = Players}) ->
    {noreply,
     State#map_state{players = lists:keydelete(AccountID, 1, Players)}};

handle_cast({send_to_players, Packet, Data}, State) ->
    lists:foreach(
      fun({_ID, FSM}) ->
              gen_fsm:send_all_state_event(
                FSM,
                {send_packet, Packet, Data}
               )
      end,
      State#map_state.players
     ),
    {noreply, State};

handle_cast({send_to_other_players, Self, Packet, Data}, State) ->
    lists:foreach(
      fun({_ID, FSM}) ->
              gen_fsm:send_all_state_event(
                FSM,
                { send_packet_if,
                  fun(#zone_state{char = C}) ->
                          (C#char.id /= Self)
                  end,
                  Packet,
                  Data
                }
               )
      end,
      State#map_state.players
     ),
    {noreply, State};

handle_cast({send_to_players_in_sight, {X, Y}, Packet, Data}, State) ->
    lists:foreach(
      fun({_ID, FSM}) ->
              gen_fsm:send_all_state_event(
                FSM,
                { send_packet_if,
                  fun(#zone_state{char = C}) ->
                          in_range(C, {X, Y})
                  end,
                  Packet,
                  Data
                }
               )
      end,
      State#map_state.players),
    {noreply, State};

handle_cast({send_to_other_players_in_sight, {X, Y}, Self, Packet, Data},
            State) ->
    lists:foreach(
      fun({_ID, FSM}) ->
              gen_fsm:send_all_state_event(
                FSM,
                { send_packet_if,
                  fun(#zone_state{char = C}) ->
                          (C#char.id /= Self) and
                              in_range(C, {X, Y})
                  end,
                  Packet,
                  Data
                }
               )
      end,
      State#map_state.players),
    {noreply, State};

handle_cast({show_actors, {SelfID, SelfFSM}}, State) ->
    lists:foreach(
      fun
          ({ID, _FSM}) when ID == SelfID ->
                         ok;

          ({_ID, FSM}) ->
                         gen_fsm:send_all_state_event(
                           FSM,
                           {show_to, SelfFSM}
                          )
                 end,
      State#map_state.players
     ),

    lists:foreach(
      fun(N) ->
              gen_fsm:send_all_state_event(
                SelfFSM,
                {send_packet, show_npc, N}
               )
      end,
      State#map_state.npcs
     ),

    {noreply, State};

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({'EXIT', From, Reason}, State) ->
    lager:log(error, self(), "Zone map server got EXIT signal ~p ~p",
              [{from, From}, {reason, Reason}]),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

in_range(C, {X, Y}) ->
    (((C#char.x - X) < 17)
     and ((C#char.x - X) > -17)
     and ((C#char.y - Y) < 17)
     and ((C#char.y - Y) > -17)).

get_player_by(_Pred, []) ->
    none;

get_player_by(Pred, [{_ID, FSM} | Players]) ->
    case gen_fsm:sync_send_all_state_event(FSM, get_state) of
        {ok, State} ->
            case Pred(State) of
                false ->
                    get_player_by(Pred, Players);

                true ->
                    {ok, State}
            end;

        _Fail ->
            get_player_by(Pred, Players)
    end.

server_for(Map) ->
    list_to_atom("zone_map_" ++ binary_to_list(Map#map.name)).

zones() ->
    {zones,
     [{6121,
       ["prontera",
        "izlude",
        "prt_monk",
        "prt_fild00",
        "prt_fild01",
        "prt_fild02",
        "prt_fild03",
        "prt_fild04",
        "prt_fild05",
        "prt_fild06",
        "prt_fild07",
        "prt_fild08",
        "prt_fild09",
        "prt_fild10",
        "prt_fild11",
        "new_1-1"]},
      {6122,
       ["geffen",
        "gef_fild00",
        "gef_fild01",
        "gef_fild02",
        "gef_fild03",
        "gef_fild04",
        "gef_fild05",
        "gef_fild06",
        "gef_fild07",
        "gef_fild08",
        "gef_fild09",
        "gef_fild10",
        "gef_fild11",
        "gef_fild12",
        "gef_fild13",
        "gef_fild14"]},
      {6123,
       ["payon",
        "pay_arche",
        "pay_fild01",
        "pay_fild02",
        "pay_fild03",
        "pay_fild04",
        "pay_fild05",
        "pay_fild06",
        "pay_fild07",
        "pay_fild08",
        "pay_fild09",
        "pay_fild10",
        "pay_fild11"]},
      {6124,
       ["morocc",
        "moc_fild01",
        "moc_fild02",
        "moc_fild03",
        "moc_fild04",
        "moc_fild05",
        "moc_fild06",
        "moc_fild07",
        "moc_fild08",
        "moc_fild09",
        "moc_fild10",
        "moc_fild11",
        "moc_fild12",
        "moc_fild13",
        "moc_fild14",
        "moc_fild15",
        "moc_fild16",
        "moc_fild17",
        "moc_fild18",
        "moc_fild19",
        "moc_fild20",
        "moc_fild21",
        "moc_fild22",
        "moc_ruins"]}]}.
