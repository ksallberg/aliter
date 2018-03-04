-module(login_sup).

-behaviour(supervisor).

-include("records.hrl").

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
    LoginWorkerSup = #{id => login_worker_sup,
                       start => {login_worker_sup, start_link, []},
                       restart => permanent,
                       shutdown => 1000,
                       type => supervisor,
                       modules => [login_worker_sup]},
    {ok, {SupFlags, [LoginServ, LoginWorkerSup]}}.

install() -> ok.
uninstall() -> ok.
stop() -> ok.
