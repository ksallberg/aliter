-module(zone_packets).

-include("ro.hrl").

-export([ unpack/1
        , pack/2
        , packet_size/1
        , new/1 ]).

new(_Whatever) ->
    zone_packets_24.

mod_for(Module, Version) ->
  list_to_atom(lists:concat([Module, "_", Version])).

unpack(Packet) ->
  call(unpack, ?PACKETVER, [Packet]).

pack(Header, Packet) ->
  call(pack, ?PACKETVER, [Header, Packet]).

packet_size(Header) ->
  call("packets", packet_size, ?PACKETVER, [Header]).

call(Fun, Version, Args) ->
  call("zone_packets", Fun, Version, Args).

call(_Module, _Fun, 0, _Args) ->
  undefined;

call(Module, Fun, Version, Args) ->
  case apply(mod_for(Module, Version), Fun, Args) of
    undefined ->
      if
        Version == 24 ->
          undefined;

        true ->
          call(Module, Fun, Version - 1, Args)
      end;

    Event ->
      Event
  end.
