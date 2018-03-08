-module(zone_srv_sup).

-behaviour(supervisor).

-include("records.hrl").

-export([start_link/2, server_for/1]).
-export([init/1]).

start_link(Port, Maps) ->
    lager:log(info, self(), "Starting zone server supervisor ~p",
              [{port, Port}]),
    supervisor:start_link({local, server_for(Port)}, ?MODULE, {Port, Maps}).

init({Port, Maps}) ->
    MapPairs = [{M#map.name, M} || M <- Maps],
    SupFlags = #{strategy  => one_for_one,
                 intensity => 0,
                 period    => 60},
    ZoneMapsSup = #{id => zone_maps_sup:server_for(Port),
                    start => {zone_maps_sup, start_link, [Port, Maps]},
                    restart => permanent,
                    shutdown => infinity,
                    type => supervisor,
                    modules => [zone_maps_sup]},
    ZoneSrv = #{id => zone_srv:server_for(Port),
                start => {zone_srv, start_link, [Port, MapPairs]},
                restart => permanent,
                shutdown => 1000,
                type => worker,
                modules => [zone_srv]},
    {ok, {SupFlags, [ZoneMapsSup, ZoneSrv]}.

server_for(Port) ->
    list_to_atom(lists:concat(["zone_server_", Port, "_sup"])).
