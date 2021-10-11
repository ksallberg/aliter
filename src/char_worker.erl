-module(char_worker).

-behaviour(gen_server).

-include("records.hrl").
-include("ro.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([ start_link/2 ]).

-export([ init/1
        , code_change/3
        , format_status/2
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2 ]).

start_link(TCP, PacketHandler) ->
    gen_server:start_link(?MODULE, [TCP, PacketHandler], []).

init([TCP, PacketHandler]) ->
    process_flag(trap_exit, true),
    {ok, #char_state{tcp = TCP, packet_handler = PacketHandler}}.

handle_cast({set_server, Server}, State) ->
    {noreply, State#char_state{server = Server}};
handle_cast({connect, AccountID, LoginIDa, LoginIDb, _Gender},
            State = #char_state{tcp = TCP,
                                packet_handler = PacketHandler}) ->
    ragnarok_proto:send_bin(TCP, <<AccountID:32/little>>),
    Verify =
        gen_server:call(login_server,
                        {verify_session, AccountID, LoginIDa, LoginIDb}),
    lager:log(info, self(), "Character connect request. ~p ~p ~p",
              [{account,AccountID},
               {ids, {LoginIDa, LoginIDb}},
               {verified, Verify}]),
    case Verify of
        {ok, LoginState} ->
            gen_server:cast(char_server,
                            {add_session, {AccountID,
                                           self(),
                                           LoginState#login_state.id_a,
                                           LoginState#login_state.id_b
                                          }}),
            Chars = db:get_account_chars(AccountID),
            PacketVer = aliter:get_config(packet_version, ?PACKETVER),
            {MaxSlots, AvailableSlots, PremiumSlots} =
                case PacketVer of
                    20180418 ->
                        {12, 12, 12};
                    _ ->
                        {9, 9, 9}
                end,

            case PacketVer of
                20180418 ->
                    SlotInfoM = {slot_info,
                                 {MaxSlots, AvailableSlots, PremiumSlots}},
                    ragnarok_proto:send_packet(SlotInfoM, TCP, PacketHandler);
                _ ->
                    skip
            end,

            M = {characters,
                 {Chars, MaxSlots, AvailableSlots, PremiumSlots}},

            ragnarok_proto:send_packet(M, TCP, PacketHandler),

            case PacketVer of
                20180418 ->
                    Pin = {pin_code, AccountID},
                    ragnarok_proto:send_packet(Pin, TCP, PacketHandler);
                _ ->
                    skip
            end,

            NewState =
                State#char_state{account = LoginState#login_state.account,
                                 id_a = LoginState#login_state.id_a,
                                 id_b = LoginState#login_state.id_b,
                                 packet_ver = LoginState#login_state.packet_ver
                                },
            {noreply, NewState};
        invalid ->
            ragnarok_proto:send_packet({refuse, 0},
                                       TCP, PacketHandler),
            {noreply, State}
    end;
handle_cast({choose, Num},
            #char_state{account = #account{id = AccountID},
                        packet_handler = PacketHandler,
                        tcp = Socket} = State) ->
    GetChar = db:get_account_char(AccountID, Num),
    case GetChar of
        nil ->
            lager:log(warning, self(), "Selected invalid character. ~p",
                      [{account,AccountID}]),
            ragnarok_proto:send_packet({refuse, 1},
                                       Socket, PacketHandler),
            {noreply, State};
        C ->
            lager:log(info, self(), "Player selected character. ~p ~p",
                      [{account, AccountID}, {character, C#char.id}]),
            {zone, ZonePort, _ZoneServer} =
                gen_server:call(zone_master, {who_serves, C#char.map}),
            ZoneIP = aliter:get_config(zone_ip, ?ZONE_IP),
            M = {zone_connect, {C, ZoneIP, ZonePort}},
            ragnarok_proto:send_packet(M, Socket, PacketHandler),
            {noreply, State#char_state{char = C}}
    end;
%% A bit of copy paste programming: following clause lacks
%% starting class, next clause has it.
handle_cast({create, Name, Str, Agi, Vit, Int, Dex,
             Luk, Num, HairColour, HairStyle}, State) ->
    handle_create({create, Name, Str, Agi, Vit, Int, Dex,
                   Luk, Num, HairColour, HairStyle, undefined}, State),
    {noreply, State};
handle_cast({create, Name, Str, Agi, Vit, Int, Dex,
             Luk, Num, HairColour, HairStyle, StartingJobClass},
           State) ->
    handle_create({create, Name, Str, Agi, Vit, Int, Dex,
                   Luk, Num, HairColour, HairStyle, StartingJobClass}, State),
    {noreply, State};
handle_cast({delete, CharacterID, EMail},
            State = #char_state{
                       tcp = Socket,
                       packet_handler = PacketHandler,
                       account = #account{id = AccountID, email = AccountEMail}
                      }) ->
    Address =
        case EMail of
            "" -> nil;
            _  -> EMail
        end,
    case Address of
        AccountEMail ->
            case db:get_char(CharacterID) of
                nil ->
                    lager:log(warning,
                              self(),
                              "Character deletion failed. ~p~p~p",
                              [{char_id, CharacterID},
                               {account_id, AccountID},
                               {email, EMail}
                              ]),
                    ragnarok_proto:send_packet({deletion_failed, 0}, Socket,
                                               PacketHandler);
                Char ->
                    db:delete_char(Char),
                    lager:log(info, self(), "Character deleted ~p",
                              [{char, Char}]),
                    ragnarok_proto:send_packet({character_deleted, ok}, Socket,
                                               PacketHandler)
            end;
        _Invalid ->
            ragnarok_proto:send_packet({deletion_failed, 0}, Socket,
                                       PacketHandler)
    end,
    {noreply, State};
handle_cast({check_name, AccountID, CharacterID, NewName},
            State = #char_state{account = #account{id = AccountID},
                                tcp = Socket, packet_handler = PacketHandler
                               }) ->
    Check = db:get_char_id(NewName),
    case Check of
        nil ->
            ragnarok_proto:send_packet({name_check_result, 1}, Socket,
                                       PacketHandler),
            Char = db:get_char(CharacterID),
            NewState = State#char_state{rename = {Char, NewName}},
            {noreply, NewState};
        _Exists ->
            ragnarok_proto:send_packet({name_check_result, 0}, Socket,
                                       PacketHandler),
            {noreply, State}
    end;
handle_cast({keepalive, _AccountID}, State) ->
    {noreply, State};

handle_cast(stop, State) ->
    {noreply,
     State#char_state{die = erlang:send_after(5 * 60 * 1000, self(), exit)}};
handle_cast(stop_now, State) ->
    handle_info(exit, State);
handle_cast({update_state, UpdateFun}, State) ->
    NewS = UpdateFun(State),
    {noreply, NewS};
handle_cast({rename, CharacterID},
            #char_state{tcp = Socket,
                        packet_handler = PacketHandler,
                        rename = {#char{id = CharacterID,
                                        renamed = 0
                                       }=Char, NewName}} = State) ->
    Check = db:get_char_id(NewName),
    case Check of
        nil ->
            db:rename_char(Char, NewName),
            ragnarok_proto:send_packet({rename_result, 0}, Socket,
                                       PacketHandler);
        _Exists ->
            ragnarok_proto:send_packet({rename_result, 3}, Socket,
                                       PacketHandler)
    end,
    {noreply, State#char_state{rename = undefined}};
handle_cast({rename, _CharacterID},
            State = #char_state{tcp = Socket,
                                packet_handler = PacketHandler,
                                rename = {#char{renamed = 1}, _NewName}}) ->
    ragnarok_proto:send_packet({rename_result, 1}, Socket, PacketHandler),
    {noreply, State}.

handle_call(switch_zone, _From, StateData = #char_state{die = Die}) ->
    case Die of
        undefined ->
            ok;
        _ ->
            erlang:cancel_timer(Die)
    end,
    {reply, {ok, StateData}, StateData}.

handle_info(exit, #char_state{tcp=Socket,
                              packet_handler=PacketHandler,
                              account = #account{id=AccountID}} = State) ->
    gen_server:cast(char_server, {remove_session, AccountID}),
    ragnarok_proto:close_socket(Socket, PacketHandler),
    {stop, normal, State}.

code_change(_, State, _) ->
    {ok, State}.

format_status(_Opt, _) ->
    ok.

terminate(_Reason, _State) ->
    ok.

handle_create({create, Name, Str, Agi, Vit, Int, Dex,
               Luk, Num, HairColour, HairStyle, StartingJobClass},
              #char_state{account = Account,
                          packet_handler = PacketHandler,
                          tcp = Socket}) ->
    Exists = db:get_char_id(Name),
    case Exists of
        nil ->
            CharToSave0 = #char{num = Num,
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
                                account_id = Account#account.id},
            CharToSave = case StartingJobClass of
                             undefined ->
                                 CharToSave0;
                             _ ->
                                 CharToSave0#char{job=StartingJobClass}
                         end,
            Char = db:save_char(CharToSave),
            lager:log(info, self(), "Created character. ~p~p",
                      [{account, Account}, {char, Char}]),
            ragnarok_proto:send_packet({character_created, Char}, Socket,
                                       PacketHandler);
        _ ->
            ragnarok_proto:send_packet({creation_failed, 0}, Socket,
                                       PacketHandler)
    end.
