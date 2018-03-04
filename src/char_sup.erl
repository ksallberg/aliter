-module(char_sup).

-behaviour(supervisor).

-include("records.hrl").

-export([ start_link/0
        , init/1 ]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 2,
                 period => 60},
    CharSrv = #{id => char_srv,
                start => {char_srv, start_link, []},
                restart => permanent,
                shutdown => 1000,
                type => worker,
                modules => [char_srv]},
    CharWorkerSup = #{id => char_worker_sup,
                      start => {char_worker_sup, start_link, []},
                      restart => permanent,
                      shutdown => 1000,
                      type => supervisor,
                      modules => [char_worker_sup]},
    {ok, {SupFlags, [CharSrv, CharWorkerSup]}}.
