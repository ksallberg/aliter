#!/usr/bin/env escript
%% -*- erlang -*-
main(_) ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1",
                                   6900,
                                   [binary, {active, false}]),
    LoginRequest = [<<16#64:16/little,
                      20180418:32/little>>,
                    list_to_binary(string:left("test", 24, 0)),
                    list_to_binary(string:left("test", 24, 0)),
                    <<0:8>>
                   ],
    gen_tcp:send(Socket, iolist_to_binary(LoginRequest)),
    {ok, LoginResponse} = gen_tcp:recv(Socket, 0),
    LoginRefused = match_login_refuse(LoginResponse),
    io:format("Login Refused: ~p \n", [LoginRefused]),
    gen_tcp:close(Socket).

match_login_refuse(<<16#6a:16/little,
                     _Reason:8,
                     _Some:20/little-binary-unit:8>>) ->
    true;
match_login_refuse(_) ->
    false.
