-module(login_sup).

-behaviour(supervisor).

-include("include/records.hrl").

-export([ start_link/0
        , install/0
        , uninstall/0
        , stop/0 ]).

-export([ init/1 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity => 10,
                 preiod    => 60},
    LoginServ = #{id => login_srv,
                  start => {login_srv, start_link, []},
                  restart => permanent,
                  shutdown => 1000,
                  type => worker,
                  modules => [login_srv]},
    {ok, {SupFlags, [LoginServ]}}.

install() -> ok.

uninstall() -> ok.

stop() ->
    log:info("Stopping Login server."),
    ok.
