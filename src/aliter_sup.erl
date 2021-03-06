-module(aliter_sup).

-behaviour(supervisor).

-export([ start_link/1
        , init/1 ]).

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

init(_) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity => 10,
                 period    => 60},
    LoginSup = #{id => login_sup,
                 start => {login_sup, start_link, []},
                 restart => permanent,
                 shutdown => 1000,
                 type => supervisor,
                 modules => [login]},
    CharSup = #{id => char_sup,
                start => {char_sup, start_link, []},
                restart => permanent,
                shutdown => 1000,
                type => supervisor,
                modules => [char_srv, char_worker_sup]},
    ZoneSup = #{id => zone_sup,
                start => {zone_sup, start_link, [conf]},
                restart => permanent,
                shutdown => 1000,
                type => supervisor,
                modules => [zone]},
    MonsterSup = #{id => monster_sup,
                   start => {monster_sup, start_link, []},
                   restart => permanent,
                   shutdown => 1000,
                   type => supervisor,
                   modules => [monster]},
    {ok, {SupFlags, [LoginSup, CharSup, ZoneSup, MonsterSup]}}.
