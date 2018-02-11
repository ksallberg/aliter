-module(monster_sup).

-behaviour(supervisor).

-include("records.hrl").

-export([ start_link/0 ]).
-export([ init/1 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity => 10,
                 preiod    => 60},
    MonsterServ = #{id => monster_srv,
                    start => {monster_srv, start_link, []},
                    restart => permanent,
                    shutdown => 1000,
                    type => worker,
                    modules => [monster_srv]},
    {ok, {SupFlags, [MonsterServ]}}.
