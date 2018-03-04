-module(ragnarok_proto).

-behaviour(ranch_protocol).

-export([ start_link/4
        , init/4
        , send_packet/3
        , close_socket/2 ]).

-define(PACKET_HANDLER, login_packets).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, [PacketHandler, DB] = Opts) ->
    io:format("Opts: ~p\n", [Opts]),
    {ok, Worker} = supervisor:start_child(login_worker_sup,
                                          [Socket, DB, PacketHandler]),
    io:format("!!!!!!WORKER: ~p\n", [Worker]),
    ok = ranch:accept_ack(Ref),
    %% ok = Transport:setopts(Socket, [{active, once}]),
    parse_loop(Socket, Transport, PacketHandler, Worker).

verify({Packet, Data}, PacketHandler) ->
    Packed = iolist_to_binary(PacketHandler:pack(Packet, Data)),
    <<Header:16/little, _/binary>> = Packed,
    Size = PacketHandler:packet_size(Header),
    if
        Size == 0 orelse
        byte_size(Packed) == Size ->
            {ok, Packed};
        true ->
            {badsize, Size}
end.

%% Event = PacketHandler:unpack(Packet),
%%             gen_server:cast(Worker, Event),
%% ?MODULE:client_worker(Socket, Worker, PacketHandler);

%% {send_packets, Packets} ->
%%             Binaries = lists:map(
%%                          fun(Packet) ->
%%                                  case verify(Packet, PacketHandler) of
%%                                      {ok, Binary} -> Binary
%%                                  end
%%                          end, Packets),
%%             gen_tcp:send(Socket, iolist_to_binary(Binaries)),
%% ?MODULE:client_worker(Socket, Worker, PacketHandler);


%% Packet when is_binary(Packet) ->
%%             gen_tcp:send(Socket, Packet),
%% ?MODULE:client_worker(Socket, Worker, PacketHandler);

%% @hidden Receive a packet, skipping unknown ones.
parse_loop(Socket, Transport, PacketHandler, Worker) ->
    io:format("apa1 ~p \n", [Transport]),
    case Transport:recv(Socket, 1, 5000) of
        {ok, <<H1>>} ->
            io:format("apa2\n", []),
            case gen_tcp:recv(Socket, 1, 0) of
                {ok, <<H2>>} ->
                    <<Header:16/little>> = <<H1, H2>>,
                    io:format("apa3\n", []),
                    case PacketHandler:packet_size(Header) of
                        undefined ->
                            io:format("NEEEEEJ!\n", []),
                            lager:log(error, self(),
                                      "Received unknown packet ~p",
                                      [{header, Header}]),
                            parse_loop(Socket, Transport, PacketHandler, Worker);
                        %% Variable-length packet
                        0 ->
                            io:format("JAAAAAAAAAA1!\n", []),
                            Next = fun() ->
                                           parse_loop(Socket,
                                                      Transport,
                                                      PacketHandler,
                                                      Worker)
                                   end,
                            case gen_tcp:recv(Socket, 2) of
                                {ok, <<Length:16/little>>} ->
                                    case gen_tcp:recv(Socket, Length - 4) of
                                        {ok, Rest} ->
                                            Worker !
                                                { tcp,
                                                  Socket,
                                                  <<Header:16/little,
                                                    Length:16/little,
                                                    Rest/binary>>
                                                },
                                            parse_loop(Socket,
                                                       Transport,
                                                       PacketHandler,
                                                       Worker);
                                        {error, Reason} ->
                                            failed_remainder(Header,
                                                             {variable,
                                                              Length - 4},
                                                             Reason),
                                            case Reason of
                                                closed ->
                                                    gen_server:cast(Worker,
                                                                    exit);
                                                _ ->
                                                    Next()
                                            end
                                    end;

                                {error, Reason} ->
                                    failed_remainder(Header, {length, 2},
                                                     Reason),
                                    case Reason of
                                        closed ->
                                            Worker ! {tcp_closed, Socket};
                                        _ ->
                                            parse_loop(Socket,
                                                       Transport,
                                                       PacketHandler,
                                                       Worker)
                                    end
                            end;
                        %% Already read all of it!
                        2 ->
                            io:format("JAAAAAAAAAA2!\n", []),

                            Worker ! {tcp, Socket, <<Header:16/little>>},
                            parse_loop(Socket, Transport, PacketHandler, Worker);

                        Size ->
                            io:format("JAAAAAAAAAA3!\n", []),
                            case gen_tcp:recv(Socket, Size - 2) of
                                {ok, Rest} ->
                                    Packet = <<Header:16/little, Rest/binary>>,
                                    handle_unpack(PacketHandler, Packet,
                                                  Worker),
                                    parse_loop(Socket,
                                               Transport,
                                               PacketHandler, Worker);
                                {error, Reason} ->
                                    failed_remainder(Header,
                                                     {packet_size, Size - 2},
                                                     Reason),

                                    case Reason of
                                        closed ->
                                            Worker ! {tcp_closed, Socket};
                                        _ ->
                                            parse_loop(Socket,
                                                       Transport,
                                                       PacketHandler,
                                                       Worker)
                                    end
                            end
                    end;
                {error, timeout} ->
                    lager:log(error, self(), "Ignore rest ~p",
                              [{got, H1}]),
                    parse_loop(Socket, Transport, PacketHandler, Worker);

                {error, closed} ->
                    Worker ! {tcp_closed, Socket}
            end;

        {error, closed} ->
            io:format("buuuuuuuuu\n", []),
            gen_server:cast(Worker, exit)
    end.

handle_unpack(PacketHandler, Packet, Worker) ->
    Event = PacketHandler:unpack(Packet),
    gen_server:cast(Worker, Event).

failed_remainder(Header, Size, Reason) ->
    lager:log(warning, self(), "Failed to receive the remainder ~p ~p ~p",
              [ {header, Header},
                {needed, Size},
                {reason, Reason}
              ]).

send_packet({Packet, Data}, Socket, PacketHandler) ->
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
    end.

close_socket(Socket, _PacketHandler) ->
    ranch_tcp:close(Socket).
