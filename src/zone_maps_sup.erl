-module(zone_maps_sup).

-behaviour(supervisor).

-include("records.hrl").

-export([start_link/2, server_for/1]).

-export([init/1]).

start_link(Port, Maps) ->
    lager:log(info, self(), "Starting maps supervisor ~p", [{port, Port}]),
    supervisor:start_link({local, server_for(Port)}, ?MODULE, Maps).

init(Maps) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity => 0,
                 period    => 60},
    Specs = lists:map(
              fun(M) ->
                      #{id => zone_map:server_for(M),
                        start => {zone_map, start_link, [M]},
                        restart => permanent,
                        shutdown => 5000,
                        type => worker,
                        modules => [zone_map]}
              end, Maps),
    {ok, {SupFlags, Specs}}.

server_for(Port) ->
    list_to_atom(lists:concat(["zone_maps_", Port, "_sup"])).
