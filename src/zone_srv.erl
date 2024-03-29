-module(zone_srv).

-behaviour(gen_server).

-include("records.hrl").
-include("ro.hrl").

-export([ start_link/2
        , server_for/1]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).

-record(state, {port, maps, list_name}).

start_link(Port, MapPairs) ->
    lager:log(info, self(), "Starting zone server ~p", [{port, Port}]),
    Name = server_for(Port),
    ListName = listener_for(Port),
    gen_server:start_link({local, Name}, ?MODULE,
                          #state{port = Port,
                                 maps = MapPairs,
                                 list_name = ListName}, []).

init(#state{list_name = ListName, port = Port} = State) ->
    PacketVer = aliter:get_config(packet_version, ?PACKETVER),
    ZonePackets = zone_packets:mod_for("zone_packets",
                                       integer_to_list(PacketVer)),
    {ok, _} = ranch:start_listener(ListName, ranch_tcp,
                                   [{port, Port}], ragnarok_proto,
                                   [ZonePackets, self()]),
    {ok, State}.

handle_call({provides, MapName},
            _From,
            State = #state{port = Port, maps = Maps}) ->
    case proplists:lookup(MapName, Maps) of
        none ->
            {reply, no, State};

        {MapName, _Map} ->
            {reply, {yes, Port}, State}
    end;
handle_call({add_player, MapName, Player}, _From, State) ->
    {MapName, Map} = proplists:lookup(MapName, State#state.maps),
    gen_server:cast(zone_map:server_for(Map), {add_player, Player}),
    {reply, {ok, Map, zone_map:server_for(Map)}, State};
handle_call({get_actor, ActorID}, _From, State = #state{maps = Maps}) ->
    {reply, get_actor(ActorID, Maps), State};
handle_call({get_player_by, Pred}, _From, State = #state{maps = Maps}) ->
    {reply, get_player_by(Pred, Maps), State};
handle_call(player_count, _From, State = #state{maps = Maps}) ->
    {reply, player_count(Maps), State};
handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast({send_to_all, Msg}, State) ->
    lists:foreach(
      fun({_Name, Map}) ->
              gen_server:cast(zone_map:server_for(Map), Msg)
      end,
      State#state.maps
     ),
    {noreply, State};
handle_cast({register_npc, NPC = #npc{map = MapName}},
  State = #state{maps = Maps}) ->
    case proplists:lookup(MapName, Maps) of
        none ->
            {noreply, State};
        {_Name, Map} ->
            gen_server:cast(zone_map:server_for(Map), {register_npc, NPC}),
            {noreply, State}
    end;
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({'EXIT', From, Reason}, State) ->
    lager:log(error, self(), "Zone server got EXIT signal ~p ~p",
              [{from, From}, {reason, Reason}]),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

player_count([]) ->
    0;

player_count([{_Name, Map} | Maps]) ->
    gen_server:call(zone_map:server_for(Map), player_count) +
        player_count(Maps).

get_actor(_ActorID, []) ->
    none;

get_actor(ActorID, [{_Name, Map} | Maps]) ->
    case gen_server:call(zone_map:server_for(Map), {get_actor, ActorID}) of
        none ->
            get_actor(ActorID, Maps);
        Found ->
            {ok, Found}
    end.

get_player_by(_Pred, []) ->
    none;

get_player_by(Pred, [{_Name, Map} | Maps]) ->
    case gen_server:call(zone_map:server_for(Map), {get_player_by, Pred}) of
        {ok, State} ->
            {ok, State};
        none ->
            get_player_by(Pred, Maps)
    end.

server_for(Port) ->
    list_to_atom(lists:concat(["zone_server_", Port])).

listener_for(Port) ->
    list_to_atom(lists:concat(["zone_listener_", Port])).
