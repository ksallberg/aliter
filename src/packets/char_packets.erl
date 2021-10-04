-module(char_packets).

-export([ unpack/1
        , pack/2
        , packet_size/1
        , new/1
        , mod_for/2]).

-include("ro.hrl").

%% FIXME: WHat should new do?
new(_Whatever) ->
    PacketVer = aliter:get_config(packet_version, ?PACKETVER),
    mod_for("char_packets", integer_to_list(PacketVer)).

mod_for(Module, Version) ->
    list_to_atom(lists:concat([Module, "_", Version])).

unpack(Packet) ->
    PacketVer = aliter:get_config(packet_version, ?PACKETVER),
    call(unpack, PacketVer, [Packet]).

pack(Header, Packet) ->
    PacketVer = aliter:get_config(packet_version, ?PACKETVER),
    call(pack, PacketVer, [Header, Packet]).

packet_size(Header) ->
    PacketVer = aliter:get_config(packet_version, ?PACKETVER),
    call("packets", packet_size, PacketVer, [Header]).

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
