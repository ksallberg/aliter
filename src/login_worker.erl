-module(login_worker).

-behaviour(gen_server).

-include("records.hrl").
-include("ro.hrl").

-export([ start_link/3 ]).

-export([ init/1
        , code_change/3
        , format_status/2
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2 ]).

-define(FEMALE, 0).
-define(MALE, 1).

start_link(TCP, DB, PacketHandler) ->
    gen_server:start_link(?MODULE, [TCP, DB, PacketHandler], []).

init([TCP, DB, PacketHandler]) ->
    process_flag(trap_exit, true),
    {ok, #login_state{tcp = TCP, db = DB, packet_handler = PacketHandler}}.

handle_cast({login, PacketVer, RawLogin, Password, Region},
            State = #login_state{tcp = TCP, db = DB}) ->
    lager:log(info, self(),
              "Received login request ~p ~p ~p",
              [{packetver, PacketVer},
               {login, RawLogin},
               {region, Region}]),
    %% Create new account if username ends with _M or _F.
    GenderS = string:sub_string(RawLogin, length(RawLogin)-1, length(RawLogin)),
    Login = register_account(DB, RawLogin, Password, GenderS),
    Versioned = State#login_state{packet_ver = PacketVer},
    PacketHandler = State#login_state.packet_handler,
    case Login of
        A = #account{} ->
            successful_login(A, Versioned);
        _ ->
            GetID = db:get_account_id(DB, Login),
            case GetID of
                % Bad login
                nil ->
                    ragnarok_proto:send_packet({refuse, {0, ""}},
                                               TCP, PacketHandler),
                    {noreply, State};
                ID ->
                    Account = db:get_account(DB, ID),
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
handle_cast(exit, State) ->
    {stop, normal, State}.

handle_call(switch_char, _From, State = #login_state{die = Die}) ->
    case Die of
        undefined ->
            ok;
        _ ->
            erlang:cancel_timer(Die)
    end,
    {reply, {ok, State}, State}.

handle_info(stop, State) ->
    {noreply,
     State#login_state{die = erlang:send_after(5 * 60 * 1000, self(), exit)}};
handle_info(exit, State) ->
    io:format("!!!!!!!!!!!!!!!!!!!!!close login_worker\n", []),
    {stop, normal, State}.

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
    gen_server:cast(
      login_server,
      {add_session, {A#account.id, self(), LoginIDa, LoginIDb}}),
    Servers = [{?CHAR_IP, ?CHAR_PORT, ?CHAR_SERVER_NAME,
                0, _Maint = 0, _New = 0}],
    M = {accept, {LoginIDa, LoginIDb, A#account.id, A#account.gender, Servers}},
    Socket = State#login_state.tcp,
    PacketHandler = State#login_state.packet_handler,
    ragnarok_proto:send_packet(M, Socket, PacketHandler),
    %% ragnarok_proto:close_socket(Socket, PacketHandler),
    handle_info(stop, State#login_state{account = A,
                                        id_a = LoginIDa,
                                        id_b = LoginIDb}).

%% Create account when username ends with _M or _F
register_account(C, RawLogin, Password, "_M") ->
    create_new_account(C, RawLogin, Password, ?MALE);
register_account(C, RawLogin, Password, "_F") ->
    create_new_account(C, RawLogin, Password, ?FEMALE);
register_account(_, Login, _Password, _) ->
    Login.

create_new_account(C, RawLogin, Password, Gender) ->
    Login = string:sub_string(RawLogin, 1, length(RawLogin)-2),
    Check = db:get_account_id(C, Login),
    case Check of
        nil ->
            lager:log(info, self(), "Created account ~p", [{login, Login}]),
            db:save_account(C,
                            #account{
                               login = Login,
                               password = erlang:md5(Password),
                               gender = Gender});
        _ ->
            lager:log(warning, self(),
                      "Account already exists; ignore ~p",
                      [{login, Login}]),
            Login
    end.
