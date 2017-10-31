-module(char_sup).

-behaviour(supervisor).

-include("records.hrl").

-export([
    start_link/1,
    init/1,
    install/0,
    uninstall/0,
    stop/0]).

start_link(Config) ->
  Supervisor = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  supervisor:start_child(?MODULE, [node(), char_srv, start_link, [Config]]),
  Supervisor.

init([]) ->
    SupFlags = {simple_one_for_one, 2, 60},
    CharSrv = { undefined,
                {rpc, block_call, []},
                permanent,
                1000,
                worker,
                [char_srv]
              },
    {ok, {SupFlags, [CharSrv]}}.

install() -> ok.
uninstall() -> ok.

stop() ->
  log:info("Stopping Char server."),
  ok.
