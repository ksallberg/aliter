-module(char_fsm).
-behaviour(gen_fsm).

-include("records.hrl").
-include("ro.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([ start_link/1 ]).

-export([ handle_event/3
        , handle_sync_event/4
        , handle_info/3
        , terminate/3
        , code_change/4 ]).

-export([ init/1
        , locked/2
        , locked/3
        , valid/2
        , valid/3
        , renaming/2
        , renaming/3
        , chosen/2
        , chosen/3 ]).

-define(MAX_SLOTS, 9).
-define(AVAILABLE_SLOTS, 9).
-define(PREMIUM_SLOTS, 9).

start_link(TCP) ->
    gen_fsm:start_link(?MODULE, TCP, []).

init({TCP, [DB]}) ->
    process_flag(trap_exit, true),
    {ok, locked, #char_state{tcp = TCP, db = DB}}.

locked({connect, AccountID, LoginIDa, LoginIDb, _Gender},
       State = #char_state{tcp = TCP, db = DB}) ->
    TCP ! <<AccountID:32/little>>,
    Verify =
        gen_server:call(login_server,
                        {verify_session, AccountID, LoginIDa, LoginIDb}),
    lager:log(info, self(), "Character connect request. ~p ~p ~p",
              [{account,AccountID},
               {ids, {LoginIDa, LoginIDb}},
               {verified, Verify}]),
    case Verify of
        {ok, FSM} ->
            {ok, L} = gen_fsm:sync_send_event(FSM, switch_char),
            gen_server:cast(char_server,
                            {add_session, {AccountID,
                                           self(),
                                           L#login_state.id_a,
                                           L#login_state.id_b
                                          }
                            }
                           ),
            Chars = db:get_account_chars(DB, AccountID),
            TCP ! {parse, char_packets:new(L#login_state.packet_ver)},
            TCP ! {characters, {Chars, ?MAX_SLOTS, ?AVAILABLE_SLOTS,
                                ?PREMIUM_SLOTS}},
            NewState = State#char_state{account = L#login_state.account,
                                        id_a = L#login_state.id_a,
                                        id_b = L#login_state.id_b,
                                        packet_ver = L#login_state.packet_ver,
                                        login_fsm = FSM
                                       },
            {next_state, valid, NewState};
        invalid ->
            TCP ! {refuse, 0},
            {next_state, locked, State}
    end;
locked(Event, State) ->
    ?MODULE:handle_event(Event, locked, State).
locked(Event, From, State) ->
    ?MODULE:handle_sync_event(Event, From, locked, State).

valid({choose, Num},
      #char_state{db = DB, account = #account{id = AccountID}} = State) ->
    GetChar = db:get_account_char(DB, AccountID, Num),
    case GetChar of
        nil ->
            lager:log(warning, self(), "Selected invalid character. ~p",
                      [{account,AccountID}]),
            State#char_state.tcp ! {refuse, 1},
            {next_state, valid, State};
        C ->
            lager:log(info, self(), "Player selected character. ~p ~p",
                      [{account, AccountID}, {character, C#char.id}]),
            {zone, ZonePort, _ZoneServer} =
                gen_server:call(zone_master, {who_serves, C#char.map}),
            State#char_state.tcp ! {zone_connect, {C, ?ZONE_IP, ZonePort}},
            {next_state, chosen, State#char_state{char = C}}
    end;
valid({create, Name, Str, Agi, Vit, Int, Dex, Luk, Num, HairColour, HairStyle},
      State = #char_state{db = DB, account = Account}) ->
    Exists = db:get_char_id(DB, Name),
    case Exists of
        nil ->
            Char = db:save_char(DB,
                                #char{num = Num,
                                      name = Name,
                                      zeny = 500, % TODO: Config flag
                                      str = Str,
                                      agi = Agi,
                                      vit = Vit,
                                      int = Int,
                                      dex = Dex,
                                      luk = Luk,
                                      hair_colour = HairColour,
                                      hair_style = HairStyle,
                                      account_id = Account#account.id
                                     }
                               ),
            lager:log(info, self(), "Created character. ~p~p",
                      [{account, Account}, {char, Char}]),
            State#char_state.tcp ! {character_created, Char};
        _ ->
            State#char_state.tcp ! {creation_failed, 0}
    end,
    {next_state, valid, State};
valid({delete, CharacterID, EMail},
      State = #char_state{
                 db = DB,
                 account = #account{id = AccountID, email = AccountEMail}
                }) ->
    Address =
        case EMail of
            "" -> nil;
            _ -> EMail
        end,
    case Address of
        AccountEMail ->
            case db:get_char(DB, CharacterID) of
                nil ->
                    lager:log(warning,
                              self(),
                              "Character deletion failed. ~p~p~p",
                              [{char_id, CharacterID},
                               {account_id, AccountID},
                               {email, EMail}
                              ]),
                    State#char_state.tcp ! {deletion_failed, 0};
                Char ->
                    db:delete_char(DB, Char),
                    lager:log(info, self(), "Character deleted ~p",
                              [{char, Char}]),
                    State#char_state.tcp ! {character_deleted, ok}
            end;
        _Invalid ->
            State#char_state.tcp ! {deletion_failed, 0}
    end,
    {next_state, valid, State};
valid({check_name, AccountID, CharacterID, NewName},
      State = #char_state{db = DB, account = #account{id = AccountID}}) ->
    Check = db:get_char_id(DB, NewName),
    case Check of
        nil ->
            State#char_state.tcp ! {name_check_result, 1},
            Char = db:get_char(DB, CharacterID),
            NewState = State#char_state{rename = {Char, NewName}},
            {next_state, renaming, NewState};
        _Exists ->
            State#char_state.tcp ! {name_check_result, 0},
            {next_state, valid, State}
    end;
valid(Event, State) ->
    ?MODULE:handle_event(Event, valid, State).
valid(Event, From, State) ->
    ?MODULE:handle_sync_event(Event, From, valid, State).

chosen(stop, State) ->
    NewState = State#char_state{die = gen_fsm:send_event_after(5 * 60 * 1000,
                                                               exit)},
    {next_state, valid, NewState};
chosen(exit, State = #char_state{login_fsm = Login}) ->
    lager:log(info, self(), "Character FSM exiting."),
    gen_fsm:send_event(Login, exit),
    {stop, normal, State};
chosen(Event, State) ->
    ?MODULE:handle_event(Event, chosen, State).
chosen(Event, From, State) ->
    ?MODULE:handle_sync_event(Event, From, chosen, State).

renaming({rename, CharacterID}, #char_state{db = DB,
                                            rename = {#char{name = OldName,
                                                            id = CharacterID,
                                                            renamed = 0
                                                           }, NewName
                                                     }
                                           } = State) ->
    Check = db:get_char_id(DB, NewName),
    case Check of
        nil ->
            db:rename_char(DB, CharacterID, OldName, NewName),
            State#char_state.tcp ! {rename_result, 0};
        _Exists ->
            State#char_state.tcp ! {rename_result, 3}
    end,
    {next_state, valid, State#char_state{rename = undefined}};
renaming({rename, _CharacterID},
         State = #char_state{rename = {#char{renamed = 1}, _NewName}}) ->
    State#char_state.tcp ! {rename_result, 1},
    {next_state, valid, State};
renaming(Event, State) ->
    ?MODULE:handle_event(Event, renaming, State).
renaming(Event, From, State) ->
    ?MODULE:handle_sync_event(Event, From, renaming, State).

handle_event(stop, _StateName, StateData) ->
    lager:log(info, self(), "Character FSM stopping."),
    {stop, normal, StateData};
handle_event({set_server, Server}, StateName, StateData) ->
    {next_state, StateName, StateData#char_state{server = Server}};
handle_event({update_state, Fun}, StateName, StateData) ->
    {next_state, StateName, Fun(StateData)};
handle_event(
  {keepalive, AccountID},
  StateName,
  StateData = #char_state{account = #account{id = AccountID}}) ->
    {next_state, StateName, StateData};
handle_event(Event, StateName, StateData) ->
    lager:log(warning, self(),
              "Character FSM got unhanadled event. ~p ~p ~p",
              [{event, Event}, {state, StateName}, {state_data, StateData}]),
    {next_state, StateName, StateData}.

handle_sync_event(switch_zone, _From, _StateName,
                  StateData = #char_state{die = Die}) ->
    case Die of
        undefined ->
            ok;
        _ ->
            erlang:cancel_timer(Die)
    end,
    {reply, {ok, StateData}, chosen, StateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info({'EXIT', From, Reason}, _StateName, StateData) ->
    lager:log(error, self(), "Character FSM got EXIT ~p~p.",
              [{from, From}, {reason, Reason}]),
    {stop, normal, StateData};
handle_info(Info, StateName, StateData) ->
    lager:log(info, "Character FSM got info: ~p", [{info, Info}]),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, #char_state{account=#account{id=AccountID}}) ->
    gen_server:cast(char_server, {remove_session, AccountID}),
    gen_server:cast(login_server, {remove_session, AccountID});
terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
