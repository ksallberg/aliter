-module(gen_packet_server).

-behaviour(gen_nb_server).

-include("records.hrl").

-export([ start_link/2
        , start_link/1
        , init/1
        , init/2
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , sock_opts/0
        , new_connection/4 ]).

-export([ client_worker/3
        , parse_loop/3
        , new_connection/2 ]).

new_connection(_, _) ->
    throw({error, not_implemented}).
start_link(_) ->
    throw({error, not_implemented}).
start_link(_, _) ->
    throw({error, not_implemented}).
init(_) ->
    throw({error, not_implemented}).

init([St = #nb_state{port = Port}], State) ->
    gen_nb_server:add_listen_socket(
      {"0.0.0.0", Port},
      gen_nb_server:store_cb_state(St, State)
     ).

handle_call(Msg, _From, State) ->
    lager:log(warning, self(), "Got unknown call ~p", [{msg, Msg}]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    lager:log(warning, self(), "Got unknown cast ~p", [{msg, Msg}]),
    {noreply, State}.

handle_info(Msg, State) ->
    lager:log(warning, self(), "Got unknown info ~p", [{msg, Msg}]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

sock_opts() ->
    [binary, {packet, raw}, {active, false}].

new_connection(_ClientIP, _ClientPort, Sock, State) ->
    #nb_state{
       port = Port,
       packet_handler = PacketHandler,
       fsm_args = FArgs,
       server = Server
      } = gen_nb_server:get_cb_state(State),
    lager:log(info, self(), "Client connected ~p",
              [{client, element(2, inet:peername(Sock))}]),
    Pid = spawn(fun() ->
                        Args =
                            case FArgs of
                                [] -> [self()];
                                _ -> [{self(), FArgs}]
                            end,
                        ClientSup = gen_server_tcp:client_sup(Port),
                        {ok, FSM}
                            = supervisor:start_child(ClientSup, Args),
                        gen_fsm:send_all_state_event(FSM, {set_server, Server}),
                        client_worker(Sock, FSM, PacketHandler)
                end),
    gen_tcp:controlling_process(Sock, Pid),
    ok = inet:setopts(Sock, [{active, once}]),
    {ok, State}.

%% @hidden Handle a client's TCP connection.
client_worker(Socket, FSM, PacketHandler) ->
    receive
        {tcp, Socket, Packet} ->
            Event = PacketHandler:unpack(Packet),
            gen_fsm:send_event(FSM, Event),
            ?MODULE:client_worker(Socket, FSM, PacketHandler);

        {tcp_closed, Socket} ->
            gen_fsm:send_event(FSM, stop);

        {parse, NewHandler} ->
            Loop = self(),
            Parser =
                spawn(?MODULE, parse_loop, [Socket, NewHandler, Loop]),
            gen_tcp:controlling_process(Socket, Parser),
            ?MODULE:client_worker(Socket, FSM, NewHandler);

        {send_packets, Packets} ->
            Binaries = lists:map(
                         fun(Packet) ->
                                 case verify(Packet, PacketHandler) of
                                     {ok, Binary} -> Binary
                                 end
                         end, Packets),

            gen_tcp:send(Socket, iolist_to_binary(Binaries)),
            ?MODULE:client_worker(Socket, FSM, PacketHandler);

        {Packet, Data} ->
            Packed = iolist_to_binary(PacketHandler:pack(Packet, Data)),
            case verify({Packet, Data}, PacketHandler) of
                {ok, Binary} ->
                    gen_tcp:send(Socket, Binary);
                {badsize, Wanted} ->
                    lager:log(error, self(),
                              "Ignored attempt to send packet "
                              "of invalid length. ~p ~p ~p ~p ",
                              [{packet, Packet},
                               {data, Data},
                               {wanted, Wanted},
                               {got, byte_size(Packed)}])
            end,
            ?MODULE:client_worker(Socket, FSM, PacketHandler);

        Packet when is_binary(Packet) ->
            gen_tcp:send(Socket, Packet),
            ?MODULE:client_worker(Socket, FSM, PacketHandler);

        close ->
            gen_tcp:close(Socket);

        _Other ->
            ok
    end.

%% @hidden Pack and verify a packet size.
verify({Packet, Data}, PacketHandler) ->
    Packed = iolist_to_binary(PacketHandler:pack(Packet, Data)),
    <<Header:16/little, _/binary>> = Packed,
    Size = PacketHandler:packet_size(Header),
    if
        Size == 0;
        byte_size(Packed) == Size ->
            {ok, Packed};

        true ->
            {badsize, Size}
    end.

%% @hidden Receive a packet, skipping unknown ones.
parse_loop(Socket, PacketHandler, Loop) ->
    case gen_tcp:recv(Socket, 1) of
        {ok, <<H1>>} ->
            case gen_tcp:recv(Socket, 1, 0) of
                {ok, <<H2>>} ->
                    <<Header:16/little>> = <<H1, H2>>,
                    case PacketHandler:packet_size(Header) of
                        undefined ->
                            lager:log(error, self(),
                                      "Received unknown packet ~p",
                                      [{header, Header}]),
                            ?MODULE:parse_loop(Socket, PacketHandler, Loop);
                        %% Variable-length packet
                        0 ->
                            Next = fun() ->
                                           ?MODULE:parse_loop(Socket,
                                                              PacketHandler,
                                                              Loop)
                                   end,
                            case gen_tcp:recv(Socket, 2) of
                                {ok, <<Length:16/little>>} ->
                                    case gen_tcp:recv(Socket, Length - 4) of
                                        {ok, Rest} ->
                                            Loop !
                                                { tcp,
                                                  Socket,
                                                  <<Header:16/little,
                                                    Length:16/little,
                                                    Rest/binary>>
                                                },
                                            ?MODULE:parse_loop(Socket,
                                                               PacketHandler,
                                                               Loop);
                                        {error, Reason} ->
                                            failed_remainder(Header,
                                                             {variable,
                                                              Length - 4},
                                                             Reason),
                                            case Reason of
                                                closed ->
                                                    Loop ! {tcp_closed, Socket};
                                                _ ->
                                                    Next()
                                            end
                                    end;

                                {error, Reason} ->
                                    failed_remainder(Header, {length, 2},
                                                     Reason),
                                    case Reason of
                                        closed ->
                                            Loop ! {tcp_closed, Socket};
                                        _ ->
                                            ?MODULE:parse_loop(Socket,
                                                               PacketHandler,
                                                               Loop)
                                    end
                            end;

                        %% Already read all of it!
                        2 ->
                            Loop ! {tcp, Socket, <<Header:16/little>>},
                            ?MODULE:parse_loop(Socket, PacketHandler, Loop);

                        Size ->
                            case gen_tcp:recv(Socket, Size - 2) of
                                {ok, Rest} ->
                                    Loop ! {tcp, Socket,
                                            <<Header:16/little, Rest/binary>>},
                                    ?MODULE:parse_loop(Socket,
                                                       PacketHandler, Loop);

                                {error, Reason} ->
                                    failed_remainder(Header,
                                                     {packet_size, Size - 2},
                                                     Reason),

                                    case Reason of
                                        closed ->
                                            Loop ! {tcp_closed, Socket};
                                        _ ->
                                            ?MODULE:parse_loop(Socket,
                                                               PacketHandler,
                                                               Loop)
                                    end
                            end
                    end;
                {error, timeout} ->
                    lager:log(error, self(), "Ignore rest ~p",
                              [{got, H1}]),
                    ?MODULE:parse_loop(Socket, PacketHandler, Loop);

                {error, closed} ->
                    Loop ! {tcp_closed, Socket}
            end;

        {error, closed} ->
            Loop ! {tcp_closed, Socket}
    end.

failed_remainder(Header, Size, Reason) ->
    lager:log(warning, self(), "Failed to receive the remainder ~p ~p ~p",
              [ {header, Header},
                {needed, Size},
                {reason, Reason}
              ]).
