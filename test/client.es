#!/usr/bin/env escript
%% -*- erlang -*-
main(_) ->
    {ok, LoginSocket} = gen_tcp:connect("127.0.0.1",
                                        6900,
                                        [binary, {active, false}]),

    %% First, log in with a missing user:
    LoginRequest = [<<16#64:16/little,
                      20111116:32/little>>,
                    list_to_binary(string:left("no_user", 24, 0)),
                    list_to_binary(string:left("test_pw", 24, 0)),
                    <<0:8>>
                   ],
    gen_tcp:send(LoginSocket, iolist_to_binary(LoginRequest)),
    {ok, LoginResponse} = gen_tcp:recv(LoginSocket, 0),
    LoginRefused = match_login_refuse(LoginResponse),
    io:format("Login Refused: ~p \n", [LoginRefused]),

    %% Then register a user
    UserName = binary_to_list(
                 base64:encode(
                   crypto:strong_rand_bytes(10))) ++ "_F",
    LoginRequest2 = [<<16#64:16/little,
                       20111116:32/little>>,
                     list_to_binary(string:left(UserName, 24, 0)),
                     list_to_binary(string:left("password", 24, 0)),
                     <<0:8>>
                    ],

    io:format("Register a new account\n", []),
    gen_tcp:send(LoginSocket, iolist_to_binary(LoginRequest2)),
    {ok, LoginResponse2} = gen_tcp:recv(LoginSocket, 0),
    {CharPort, LoginIDa, AccountID, LoginIDb, Gender} =
        match_login_success(LoginResponse2),
    io:format("CharIP: ~p ~p ~p\n", [CharPort, LoginIDa, LoginIDb]),
    gen_tcp:close(LoginSocket),

    %% Connect to the char server
    {ok, CharSocket} = gen_tcp:connect("127.0.0.1",
                                       CharPort,
                                       [binary, {active, false}]),

    CharConnectRequest = <<16#65:16/little,
                            AccountID:32/little,
                            LoginIDa:32/little,
                            LoginIDb:32/little,
                            0:16/little,
                            Gender:8>>,
    io:format("Attempt connection to char server\n", []),
    gen_tcp:send(CharSocket, CharConnectRequest),
    {ok, CharConnectResponse} = gen_tcp:recv(CharSocket, 0),
    {ok, CharConnectResponse2} = gen_tcp:recv(CharSocket, 0),

    %% Match to see that we get the same AccountID back from char server
    AccountID = match_account_id(CharConnectResponse),
    io:format("Account ID from char server matches!\n", []),
    {MaxSlots, AvailableSlots, PremiumSlots} =
        match_char_response(CharConnectResponse2),
    io:format("CharConnectResponse2: ~p~n",
              [{MaxSlots, AvailableSlots, PremiumSlots}]),
    io:format("MaxSlots: ~p AvailableSlots: ~p PremiumSlots: ~p\n",
             [MaxSlots, AvailableSlots, PremiumSlots]),

    %% Left to test:

    %% Create character

    CharName = binary_to_list(
                 base64:encode(
                   crypto:strong_rand_bytes(7))),

    CreateCharPacket = [<<16#67:16/little>>,
                        list_to_binary(string:left(CharName, 24, 0)),
                        <<1:8, %% str
                          2:8, %% agi
                          3:8, %% vit
                          4:8, %% int
                          5:8, %% dex
                          6:8, %% luk
                          1:8, %% num
                          12:16/little, %% haircolor
                          3:16/little>>], %% hairstyle

    io:format("Creating character!\n", []),
    gen_tcp:send(CharSocket, iolist_to_binary(CreateCharPacket)),
    {ok, CharCreateResponse} = gen_tcp:recv(CharSocket, 0),


    CharNameResponse = match_char_created(CharCreateResponse),
    CharNameResponse2 = string:strip(binary_to_list(CharNameResponse), both, 0),

    io:format("Char names matching: ~p~n",
              [CharName =:= CharNameResponse2]),


    %% Select character
    %% Maybe connect to zone server?

    gen_tcp:close(CharSocket),
    io:format("Client shutting down\n").

match_account_id(<<AccountID:32/little>>) ->
    AccountID;
match_account_id(_) ->
    false.

match_login_refuse(<<16#6a:16/little,
                     _Reason:8,
                     _Some:20/little-binary-unit:8>>) ->
    true;
match_login_refuse(_) ->
    false.

match_login_success(<<16#69:16/little,
                      _ServerLen:16/little,
                      LoginIDa:32/little,
                      AccountID:32/little,
                      LoginIDb:32/little,
                      _Zero:32,
                      _Some:24/little-binary-unit:8,
                      _Some2:16/little,
                      Gender:8,
                      Rest/binary>>) ->
    CharPort = match_login_get_servers(Rest),
    {CharPort, LoginIDa, AccountID, LoginIDb, Gender};
match_login_success(_) ->
    io:format("oops error\n").

match_login_get_servers(<<_CharIP:32/little,
                          CharPort:16/little,
                          _ServerName:20/little-binary-unit:8,
                          _Some:16/little,
                          _Maintenance:16/little,
                          _New:16/little,
                          _Rest/binary>>) ->
    CharPort.

match_char_response(<<16#6b:16/little,
                      _PackLen:16/little,
                      MaxSlots:8/little,
                      AvailableSlots:8/little,
                      PremiumSlots:8/little,
                      0:8, %% beginbilling>>,
                      0:32/little, %% code
                      0:32/little, %% time1
                      0:32/little, %% time2
                      0:8,0:8,0:8,0:8,0:8,0:8,0:8>>) ->
    {MaxSlots, AvailableSlots, PremiumSlots};
match_char_response(_) ->
    io:format("error oh no\n", []).

match_char_created(<<16#006D:16/little,Rest/binary>>) ->
    match_char(Rest).

match_char(<<_Id:32/little,
             _BaseExp:32/little,
             _Zeny:32/little,
             _JobExp:32/little,
             _JobLevel:32/little,
             _BodyState:32/little,
             _Health:32/little,
             _Effects:32/little,
             _Karma:32/little,
             _Manner:32/little,
             _StatusPoints:16/little,
             _Hp:32/little,
             _MaxHp:32/little,
             _Sp:16/little,
             _MaxSp:16/little,
             _WalkSpeed:16/little,
             _Job:16/little,
             _HairStyle:16/little,
             _ViewWeapon:16/little,
             _BaseLevel:16/little,
             _SkillPoints:16/little,
             _ViewHeadBottom:16/little,
             _ViewShield:16/little,
             _ViewHeadTop:16/little,
             _ViewHeadMiddle:16/little,
             _HairColour:16/little,
             _ClothesColour:16/little,
             CharName:24/little-binary-unit:8,
             _Str:8,
             _Agi:8,
             _Vit:8,
             _Int:8,
             _Dex:8,
             _Luk:8,
             _Num:16/little,
             1:16/little,
             _MapName:16/little-binary-unit:8,
             _DeleteDate:32/little,  % delete date
             _Robe:32/little,  % robe
             _SlotChange:32/little, % slot change
             _Unknown:32/little>>) ->
    CharName;
match_char(_) ->
    false.