-module(zone_sup).

-behaviour(supervisor).

-include("include/records.hrl").

-export([start_link/1, init/1, install/0, uninstall/0, stop/0]).

start_link(Config) ->
  Supervisor = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  supervisor:start_child(?MODULE, [node(), zone_master_sup, start_link, [Config]]),
  Supervisor.

init([]) ->
    SupFlags = {simple_one_for_one, 2, 60},
    ZoneMasterSup = { undefined,
                      {rpc, block_call, []},
                      permanent,
                      infinity,
                      supervisor,
                      [zone_master_sup]
                    },
  {ok, {SupFlags, [ZoneMasterSup]}}.

install() -> ok.
uninstall() -> ok.

stop() ->
  log:info("Stopping Zone server."),
  ok.
