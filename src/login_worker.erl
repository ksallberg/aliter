-module(login_worker).

-behaviour(gen_server).

-include("records.hrl").
-include("ro.hrl").

-export([ start_link/2 ]).

-export([ init/1
        , code_change/3
        , format_status/2
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2 ]).

-define(FEMALE, 0).
-define(MALE, 1).

start_link(TCP, PacketHandler) ->
    gen_server:start_link(?MODULE, [TCP, PacketHandler], []).

init([TCP, PacketHandler]) ->
    process_flag(trap_exit, true),
    {ok, #login_state{tcp = TCP, packet_handler = PacketHandler}}.

handle_cast({login, PacketVer, RawLogin, Password, Region},
            State = #login_state{tcp = TCP}) ->
    lager:log(info, self(),
              "Received login request ~p ~p ~p",
              [{packetver, PacketVer},
               {login, RawLogin},
               {region, Region}]),
    %% Create new account if username ends with _M or _F.
    GenderS = string:sub_string(RawLogin, length(RawLogin)-1, length(RawLogin)),
    Login = register_account(RawLogin, Password, GenderS),
    Versioned = State#login_state{packet_ver = PacketVer},
    PacketHandler = State#login_state.packet_handler,
    case Login of
        A = #account{} ->
            successful_login(A, Versioned);
        _ ->
            GetID = db:get_account_id(Login),
            case GetID of
                % Bad login
                nil ->
                    ragnarok_proto:send_packet({refuse, {0, ""}},
                                               TCP, PacketHandler),
                    {noreply, State};
                ID ->
                    Account = db:get_account(ID),
                    Hashed = erlang:md5(Password),
                    if
                        % Bad password
                        Account#account.password /= Hashed ->
                            ragnarok_proto:send_packet({refuse, {1, ""}},
                                                       TCP, PacketHandler),
                            {noreply, State};
                        % Successful auth
                        true ->
                            successful_login(Account, Versioned)
                    end
            end
    end;
handle_cast({set_server, Server}, State) ->
    {noreply, State#login_state{server = Server}};
handle_cast(stop, State) ->
    {noreply,
     State#login_state{die = erlang:send_after(5000, self(), exit)}};
handle_cast(Other, State) ->
    lager:log(warning, self(), "Login worker got unknown request: ~p", [Other]),
    {noreply, State}.


handle_info(exit, #login_state{tcp=Socket,
                               packet_handler=PacketHandler} = State) ->
    ragnarok_proto:close_socket(Socket, PacketHandler),
    {stop, normal, State}.

handle_call(_, _From, State) ->
    {reply, nothing_to_say, State}.

code_change(_, State, _) ->
    {ok, State}.

format_status(_Opt, _) ->
    ok.

terminate(_Reason, _State) ->
    ok.

%% --helpers
successful_login(A, State) ->
    %% Generate random IDs using current time as initial seed
    {A1, A2, A3} = erlang:timestamp(),
    rand:seed(exs1024, {A1, A2, A3}),
    {LoginIDa, LoginIDb} =
        {rand:uniform(16#FFFFFFFF), rand:uniform(16#FFFFFFFF)},
    NewState = State#login_state{account = A,
                                 id_a = LoginIDa,
                                 id_b = LoginIDb},
    gen_server:cast(
      login_server,
      {add_session, {A#account.id, NewState, LoginIDa, LoginIDb}}),
    CharIP = aliter:get_config(char_ip, ?CHAR_IP),
    Servers = [{CharIP, ?CHAR_PORT, ?CHAR_SERVER_NAME,
                0, _Maint = 0, _New = 0}],
    M = {accept, {LoginIDa, LoginIDb, A#account.id, A#account.gender, Servers}},
    Socket = State#login_state.tcp,
    PacketHandler = State#login_state.packet_handler,
    ragnarok_proto:send_packet(M, Socket, PacketHandler),
    {noreply, NewState}.

%% Create account when username ends with _M or _F
register_account(RawLogin, Password, "_M") ->
    create_new_account(RawLogin, Password, ?MALE);
register_account(RawLogin, Password, "_F") ->
    create_new_account(RawLogin, Password, ?FEMALE);
register_account(Login, _Password, _) ->
    Login.

create_new_account(RawLogin, Password, Gender) ->
    Login = string:sub_string(RawLogin, 1, length(RawLogin)-2),
    Check = db:get_account_id(Login),
    case Check of
        nil ->
            lager:log(info, self(), "Created account ~p", [{login, Login}]),
            db:save_account(#account{
                               login = Login,
                               password = erlang:md5(Password),
                               gender = Gender});
        _ ->
            lager:log(warning, self(),
                      "Account already exists; ignore ~p",
                      [{login, Login}]),
            Login
    end.
