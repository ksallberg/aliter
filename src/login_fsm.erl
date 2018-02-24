-module(login_fsm).

-behaviour(gen_statem).

-include("records.hrl").
-include("ro.hrl").

-export([start_link/1]).

-export([ init/1
        , locked/3
        , valid/3
        , callback_mode/0 ]).

-define(FEMALE, 0).
-define(MALE, 1).

callback_mode() ->
    state_functions.

start_link(TCP) ->
    gen_statem:start_link(?MODULE, TCP, []).

init({TCP, [DB]}) ->
    process_flag(trap_exit, true),
    {ok, locked, #login_state{tcp = TCP, db = DB}}.

locked(_, {set_server, Server}, State) ->
    {next_state, locked, State#login_state{server = Server}};
locked(_Type, {login, PacketVer, RawLogin, Password, Region},
       State = #login_state{tcp = TCP, db = DB}) ->
    lager:log(info, self(),
              "Received login request ~p ~p ~p ~p",
              [ {packetver, PacketVer},
                {login, RawLogin},
                {password, erlang:md5(Password)},
                {region, Region}
              ]),
    %% Create new account if username ends with _M or _F.
    GenderS = string:sub_string(RawLogin, length(RawLogin)-1, length(RawLogin)),
    Login = register_account(DB, RawLogin, Password, GenderS),
    Versioned = State#login_state{packet_ver = PacketVer},
    case Login of
        A = #account{} ->
            successful_login(A, Versioned);

        _ ->
            GetID = db:get_account_id(DB, Login),
            case GetID of
                % Bad login
                nil ->
                    TCP ! {refuse, {0, ""}},
                    {next_state, locked, State};

                ID ->
                    Account = db:get_account(DB, ID),

                    Hashed = erlang:md5(Password),

                    if
                        % Bad password
                        Account#account.password /= Hashed ->
                            TCP ! {refuse, {1, ""}},
                            {next_state, locked, State};
                        % Successful auth
                        true ->
                            successful_login(Account, Versioned)
                    end
            end
    end.

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
    State#login_state.tcp !
        { accept,
          { LoginIDa,
            LoginIDb,
            A#account.id,
            A#account.gender,
            Servers
          }
        },
    State#login_state.tcp ! close,
    valid(
      info,
      stop,
      State#login_state{
        account = A,
        id_a = LoginIDa,
        id_b = LoginIDb
       }
     ).

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

valid({call, From}, switch_char, State = #login_state{die = Die}) ->
    case Die of
        undefined ->
            ok;
        _ ->
            erlang:cancel_timer(Die)
    end,
    {next_state, valid, State, [{reply, From, {ok, State}}]};
valid(cast, exit, State) ->
    {stop, normal, State};

valid(info, stop, State) ->
    {next_state, valid,
     State#login_state{die = erlang:send_after(5 * 60 * 1000, self(), exit)}};
valid(info, exit, State) ->
    {stop, normal, State}.
