-module(zone_sup).

-behaviour(supervisor).

-include("records.hrl").

-export([start_link/1, init/1, install/0, uninstall/0, stop/0]).

start_link(Config) ->
    Supervisor = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    supervisor:start_child(?MODULE,
                           [node(), zone_master_sup, start_link, [Config]]),
    Supervisor.

init(_Ok) ->
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity => 2,
                 period    => 60},
    ZoneMasterSup = #{id => undefined,
                      start => {rpc, block_call, []},
                      restart => permanent,
                      shutdown => infinity,
                      type => supervisor,
                      modules => [zone_master_sup]
                    },
    {ok, {SupFlags, [ZoneMasterSup]}}.

install() -> ok.
uninstall() -> ok.
stop() ->ok.
