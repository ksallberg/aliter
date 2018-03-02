-module(zone_master).

-behaviour(gen_server).

-include("records.hrl").

-export([start_link/1]).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3 ]).
-export([tick/0]).

-record(state, {npc_id, servers}).

start_link(Conf) ->
    lager:log(info, self(), "Starting master zone server.", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Conf, []).

init(_Conf) ->
    process_flag(trap_exit, true),
    application:set_env(zone, started, erlang:timestamp()),
    nif:init(),
    zone_npc:load_all(),
    {zones, Zones} = zone_map:zones(),
    {ok, #state{npc_id = 5000000,
                servers = [zone_srv:server_for(P) || {P, _} <- Zones]
               }}.

handle_call({who_serves, Map}, _From, State) ->
    {reply, who_serves(Map, State#state.servers), State};

handle_call({get_player, ActorID}, _From, State) ->
    Player = get_player(ActorID, State#state.servers),
    {reply, Player, State};

handle_call({get_player_by, Pred}, _From, State) ->
    Player = get_player_by(
               Pred,
               State#state.servers
              ),
    {reply, Player, State};

handle_call(player_count, _from, State) ->
    {reply, player_count(State#state.servers), State};

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast({send_to_all, Msg}, State) ->
    lists:foreach(
      fun(Server) ->
              gen_server:cast(Server, Msg)
      end, State#state.servers),
    {noreply, State};

handle_cast({register_npc, Name, Sprite, Map, {X, Y}, Direction, Object},
            State = #state{npc_id = Id}) ->
    lists:foreach(
      fun(Server) ->
              gen_server:cast(Server, {register_npc,
                                       #npc{id = Id,
                                            name = Name,
                                            sprite = Sprite,
                                            map = Map,
                                            coordinates = {X, Y},
                                            direction = Direction,
                                            objecttype=6,
                                            main = Object}})
      end,
      State#state.servers
     ),
    {noreply, State#state{npc_id = Id + 1}};

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({'EXIT', From, Reason}, State) ->
    lager:log(error, self(), "Zone master got EXIT signal ~p ~p",
              [{from, From}, {reason, Reason}]),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

who_serves(_Map, []) ->
    none;

who_serves(Map, [Server | Servers]) ->
    case gen_server:call(Server, {provides, Map}) of
        {yes, Port} ->
            {zone, Port, Server};

        no ->
            who_serves(Map, Servers)
    end.


player_count([]) ->
    0;

player_count([Server | Servers]) ->
    gen_server:call(Server, player_count) +
        player_count(Servers).


get_player(_ActorID, []) ->
    none;

get_player(ActorID, [Server | Servers]) ->
    case gen_server:call(Server, {get_player, ActorID}) of
        {ok, FSM} ->
            {ok, FSM};

        none ->
            get_player(ActorID, Servers)
    end.

get_player_by(_Pred, []) ->
    none;

get_player_by(Pred, [Server | Servers]) ->
    case gen_server:call(Server, {get_player_by, Pred}) of
        {ok, State} ->
            {ok, State};

        none ->
            get_player_by(Pred, Servers)
    end.

tick() ->
    {ok, Started} = application:get_env(zone, started),
    round(timer:now_diff(erlang:timestamp(), Started) / 1000).
