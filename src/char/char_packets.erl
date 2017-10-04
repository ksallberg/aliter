-module(char_packets).

-export([
    unpack/1,
    pack/2,
    packet_size/1]).

-include("include/ro.hrl").

mod_for(Module, Version) ->
  list_to_atom(lists:concat([Module, "_", Version])).


unpack(Packet) ->
  call(unpack, ?PACKETVER, [Packet]).


pack(Header, Packet) ->
  call(pack, ?PACKETVER, [Header, Packet]).


packet_size(Header) ->
  call("packets", packet_size, ?PACKETVER, [Header]).


call(Fun, Version, Args) ->
  call("char_packets", Fun, Version, Args).


call(_Module, _Fun, 0, _Args) ->
  undefined;

call(Module, Fun, Version, Args) ->
  case apply(mod_for(Module, Version), Fun, Args) of
    undefined ->
      call(Module, Fun, Version - 1, Args);
    Event ->
      Event
  end.
