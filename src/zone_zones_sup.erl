-module(zone_zones_sup).

-behaviour(supervisor).

-include("records.hrl").

-export([start_link/1]).

-export([init/1]).

start_link(Conf) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Conf).

init(_Conf) ->
    {zones, Zones} = zone_map:zones(),
    SupFlags = #{strategy  => one_for_one,
                 intensity => 2,
                 period    => 60},
    AllMaps = aliter_maps:read_cache("priv/maps"),
    MapF = fun({Port, ZoneMaps}) ->
                   Names = [list_to_binary(X) || X <- ZoneMaps],
                   lager:log(info, self(), "Starting slave ~p ~p",
                             [{port, Port}, {maps, Names}]),
                   FiltFun = fun(M) ->
                                     lists:member(M#map.name, Names)
                             end,
                   Maps = lists:filter(FiltFun, AllMaps),
                   #{id => zone_srv_sup:server_for(Port),
                     start => {zone_srv_sup, start_link, [Port, Maps]},
                     restart => permanent,
                     shutdown => infinity,
                     type => supervisor,
                     modules =>[zone_srv_sup]}
           end,
    Specs = lists:map(MapF, Zones),
    ZoneWorkerSup = #{id => zone_worker_sup,
                      start => {zone_worker_sup, start_link, []},
                      restart => permanent,
                      shutdown => 1000,
                      type => supervisor,
                      modules => [zone_worker_sup]},
    {ok, {SupFlags, [ZoneWorkerSup | Specs]}}.
