-module(ragnarok_proto).

-behaviour(ranch_protocol).

-export([ start_link/4
        , init/4
        , send_packet/3
        , send_packets/3
        , send_bin/2
        , close_socket/2 ]).

-define(PACKET_HANDLER, login_packets).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, [PacketHandler, DB]) ->
    {ok, Worker} = case PacketHandler of
                       login_packets ->
                           supervisor:start_child(login_worker_sup,
                                                  [Socket, DB, PacketHandler]);
                       char_packets_24 ->
                           supervisor:start_child(char_worker_sup,
                                                  [Socket, DB, PacketHandler])
                   end,
    ok = ranch:accept_ack(Ref),
    parse_loop(Socket, Transport, PacketHandler, Worker);
init(Ref, Socket, Transport, [PacketHandler, DB, Server]) ->
    {ok, Worker} = supervisor:start_child(zone_worker_sup,
                                          [Socket, DB, PacketHandler, Server]),
    ok = ranch:accept_ack(Ref),
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

%% @hidden Receive a packet, skipping unknown ones.
parse_loop(Socket, Transport, PacketHandler, Worker) ->
    case Transport:recv(Socket, 1, infinity) of
        {ok, <<H1>>} ->
            case Transport:recv(Socket, 1, infinity) of
                {ok, <<H2>>} ->
                    <<Header:16/little>> = <<H1, H2>>,
                    case PacketHandler:packet_size(Header) of
                        undefined ->
                            lager:log(error, self(),
                                      "Received unknown packet ~p",
                                      [{header, Header}]),
                            parse_loop(Socket, Transport,
                                       PacketHandler, Worker);
                        %% Variable-length packet
                        0 ->
                            Next = fun() ->
                                           parse_loop(Socket,
                                                      Transport,
                                                      PacketHandler,
                                                      Worker)
                                   end,
                            case Transport:recv(Socket, 2, infinity) of
                                {ok, <<Length:16/little>>} ->
                                    case Transport:recv(Socket, Length - 4,
                                                        infinity) of
                                        {ok, Rest} ->
                                            Packet = <<Header:16/little,
                                                       Length:16/little,
                                                       Rest/binary>>,
                                            handle_unpack(PacketHandler, Packet,
                                                          Worker),
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
                                            gen_server:cast(Worker, stop);
                                        _ ->
                                            parse_loop(Socket,
                                                       Transport,
                                                       PacketHandler,
                                                       Worker)
                                    end
                            end;
                        %% Already read all of it
                        2 ->
                            handle_unpack(PacketHandler, <<Header:16/little>>,
                                          Worker),
                            parse_loop(Socket, Transport,
                                       PacketHandler, Worker);
                        Size ->
                            case Transport:recv(Socket, Size - 2, infinity) of
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
                                            gen_server:cast(Worker, stop);
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
                    gen_server:cast(Worker, stop)
            end;
        {error, closed} ->
            gen_server:cast(Worker, stop)
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
            send_bin(Socket, Binary);
        {badsize, Wanted} ->
            lager:log(error, self(),
                      "Ignored attempt to send packet "
                      "of invalid length. ~p ~p ~p ~p ",
                      [{packet, Packet},
                       {data, Data},
                       {wanted, Wanted},
                       {got, byte_size(Packed)}])
    end.

send_packets(Socket, Packets, PacketHandler) ->
    Binaries = lists:map(
                 fun(Packet) ->
                         case verify(Packet, PacketHandler) of
                             {ok, Binary} -> Binary
                         end
                 end, Packets),
    ranch_tcp:send(Socket, iolist_to_binary(Binaries)).


send_bin(Socket, Packet) ->
    ranch_tcp:send(Socket, Packet).

close_socket(Socket, _PacketHandler) ->
    ranch_tcp:close(Socket).
