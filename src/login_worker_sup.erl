-module(login_worker_sup).

-behaviour(supervisor).

-export([ start_link/0
        , init/1 ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_WhatEver) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 60},
    ChildSpec = [#{id      => login_worker_sup,
                   start   => {login_worker, start_link, []},
                   restart => transient,
                   shutdown => 1000,
                   type => worker,
                   modules => [login_worker]}],
    {ok, {SupFlags, ChildSpec}}.
