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
