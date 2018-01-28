-module(char_fsm).

-behaviour(gen_statem).

-include("records.hrl").
-include("ro.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([ start_link/1
        , init/1
        , locked/3
        , valid/3
        , renaming/3
        , chosen/3
        , callback_mode/0 ]).

-define(MAX_SLOTS, 9).
-define(AVAILABLE_SLOTS, 9).
-define(PREMIUM_SLOTS, 9).

callback_mode() ->
    state_functions.

start_link(TCP) ->
    gen_statem:start_link(?MODULE, TCP, []).

init({TCP, [DB]}) ->
    process_flag(trap_exit, true),
    {ok, locked, #char_state{tcp = TCP, db = DB}}.

locked(_, {_, {set_server, Server}}, State) ->
    {next_state, locked, State#char_state{server = Server}};
locked(_, {_, {connect, AccountID, LoginIDa, LoginIDb, _Gender}},
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
            {ok, L} = gen_statem:call(FSM, switch_char),
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
    end.

valid(_, {_, {choose, Num}},
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
valid(_, {_,
          {create, Name, Str, Agi, Vit, Int, Dex,
           Luk, Num, HairColour, HairStyle}},
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
valid(_, {_, {delete, CharacterID, EMail}},
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
valid(_, {_, {check_name, AccountID, CharacterID, NewName}},
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
valid({call, From}, switch_zone, StateData = #char_state{die = Die}) ->
    case Die of
        undefined ->
            ok;
        _ ->
            erlang:cancel_timer(Die)
    end,
    {next_state, valid, StateData, [{reply, From, {ok, StateData}}]};
valid(_, {_, {keepalive, _AccountID}}, State) ->
    {keep_state, State};
valid(info, {_, stop}, State) ->
    NewState = State#char_state{
                 die = erlang:send_after(5 * 60 * 1000, self(), exit)},
    {next_state, valid, NewState};
valid(cast, exit, State = #char_state{login_fsm = Login,
                                      account=#account{id=AccountID}}) ->
    lager:log(info, self(), "Character FSM exiting."),
    gen_server:cast(char_server, {remove_session, AccountID}),
    gen_server:cast(login_server, {remove_session, AccountID}),
    gen_statem:cast(Login, exit),
    {stop, normal, State};
valid(info, {_, exit}, State = #char_state{login_fsm = Login,
                                           account=#account{id=AccountID}}) ->
    lager:log(info, self(), "Character FSM exiting."),
    gen_server:cast(char_server, {remove_session, AccountID}),
    gen_server:cast(login_server, {remove_session, AccountID}),
    gen_statem:cast(Login, exit),
    {stop, normal, State}.

chosen(info, {_, stop}, State) ->
    NewState = State#char_state{
                 die = erlang:send_after(5 * 60 * 1000, self(), exit)},
    {next_state, valid, NewState};
chosen({call, From}, switch_zone, StateData = #char_state{die = Die}) ->
    case Die of
        undefined ->
            ok;
        _ ->
            erlang:cancel_timer(Die)
    end,
    {next_state, chosen, StateData, [{reply, From, {ok, StateData}}]}.

renaming(_, {_, {rename, CharacterID}},
         #char_state{db = DB,
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
renaming(_, {_, {rename, _CharacterID}},
         State = #char_state{rename = {#char{renamed = 1}, _NewName}}) ->
    State#char_state.tcp ! {rename_result, 1},
    {next_state, valid, State}.
