-module(monster_worker).

-behaviour(gen_server).

-export([ init/1
        , terminate/2
        , handle_info/2
        , handle_cast/2
        , handle_call/3
        , format_status/2
        , code_change/3
        , start_link/4 ]).

-record(monster_state, {hp=1000,
                        timer=undefined,
                        tcp,
                        id,
                        packet_handler
                       }).

start_link(HP, TCP, ID, PacketHandler) ->
    gen_server:start_link(?MODULE, [HP, TCP, ID, PacketHandler], []).

init([Hp, TCP, ID, PacketHandler]) ->
    TimerRef = erlang:start_timer(5000, self(), emo),
    State = #monster_state{hp=Hp, timer=TimerRef, id=ID,
                           tcp=TCP, packet_handler=PacketHandler},
    {ok, State}.

terminate(_, _State) ->
    ok.

handle_info({timeout, _Ref, emo},
            #monster_state{tcp = TCP,
                           id = ID,
                           packet_handler = PacketHandler} = State) ->
    Emo = rand:uniform(79) + 1,
    Packet = {emotion, {ID, Emo}},
    ragnarok_proto:send_packet(Packet, TCP, PacketHandler),
    TimerRef = erlang:start_timer(5000, self(), emo),
    {noreply, State#monster_state{timer=TimerRef}};
handle_info(_, State) ->
    {noreply, State}.

handle_call({dec_hp, Dmg}, _From, #monster_state{hp = Hp} = State) ->
    NewHp = Hp - Dmg,
    {reply, {ok, NewHp}, State#monster_state{hp = NewHp}}.

handle_cast(stop, #monster_state{timer = TimerRef} = State) ->
    erlang:cancel_timer(TimerRef),
    {stop, normal, State}.

format_status(_Opt, _Whatever) ->
    todo.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
