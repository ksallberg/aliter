-module(monster_sup).

-behaviour(supervisor).

-include("records.hrl").

-export([ start_link/0 ]).
-export([ init/1 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_WhatEver) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 60},
    MonsterServ = #{id => monster_worker,
                    start => {monster_worker, start_link, []},
                    restart => transient,
                    shutdown => 1000,
                    type => worker,
                    modules => [monster_worker]},
    {ok, {SupFlags, [MonsterServ]}}.
