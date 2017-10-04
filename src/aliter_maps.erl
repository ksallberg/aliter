-module(aliter_maps).

-include("include/records.hrl").

-export([read_cache/1]).

read_maps(Cache) ->
  read_maps(Cache, 0).

read_maps(<<>>, _N) ->
  [];

read_maps(
  <<Name:12/binary-unit:8,
    Width:16/little,
    Height:16/little,
    Length:32/little,
    _:Length/binary,
    Rest/binary>>,
    N) ->
  MapName =
    case binary:split(Name, <<0>>) of
      [X, _] -> X;
      [X] -> X
    end,

  [ #map{
      id = N,
      name = MapName,
      width = Width,
      height = Height
    } | read_maps(Rest, N + 1)
  ].

read_cache(Filename) ->
  {ok, Cache} = file:read_file(Filename),
  <<_Size:32/little, _NumMaps:16/little, _:16, Maps/binary>> = Cache,
  read_maps(Maps).
