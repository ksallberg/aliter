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

-export([ ping/3 ]).

-record(monster_state, {hp=1000,
                        timer=undefined,
                        tcp,
                        id
                       }).

start_link(HP, TCP, ID, PacketHandler) ->
    gen_server:start_link(?MODULE, [HP, TCP, ID, PacketHandler], []).

init([Hp, TCP, ID, PacketHandler]) ->
    {ok, TimerRef} = timer:apply_interval(timer:seconds(5),
                                          monster_worker, ping,
                                          [TCP, ID, PacketHandler]),
    State = #monster_state{hp=Hp, timer=TimerRef, tcp=TCP},
    {ok, State}.

terminate(_, _State) ->
    ok.

handle_info(_, State) ->
    {noreply, State}.

handle_call({dec_hp, Dmg}, _From, #monster_state{hp = Hp} = State) ->
    NewHp = Hp - Dmg,
    {reply, {ok, NewHp}, State#monster_state{hp = NewHp}}.

handle_cast(stop, #monster_state{timer = TimerRef} = State) ->
    {ok, cancel} = timer:cancel(TimerRef),
    {stop, normal, State}.

format_status(_Opt, _Whatever) ->
    todo.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

ping(TCP, ID, PacketHandler) ->
    Emo = rand:uniform(79) + 1,
    Packet = {emotion, {ID, Emo}},
    ragnarok_proto:send_packet(Packet, TCP, PacketHandler).
