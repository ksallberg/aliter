-module(zone_master_sup).

-behaviour(supervisor).

-include("records.hrl").

-export([start_link/1]).
-export([init/1]).

start_link(Conf) ->
    lager:log(info, self(), "Starting master supervisor", []),
    supervisor:start_link({local, ?MODULE}, ?MODULE, Conf).

init(Conf) ->
    SupFlags = #{strategy  => one_for_all,
                 intensity => 0,
                 period    => 60},
    ZoneZonesSup = #{id => zone_zones_sup,
                     start => {zone_zones_sup, start_link, [Conf]},
                     restart => permanent,
                     shutdown => infinity,
                     type => supervisor,
                     modules => [zone_zones_sup]},
    ZoneMaster = #{id => zone_master,
                   start => {zone_master, start_link, [Conf]},
                   restart => permanent,
                   shutdown => 5000,
                   type => worker,
                   modules => [zone_master]},
    {ok, {SupFlags, [ZoneZonesSup, ZoneMaster]}}.
