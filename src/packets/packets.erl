-module(packets).

-export([packet_size/1]).

-include("ro.hrl").

mod_for(Version) ->
    list_to_atom(lists:concat(["packets_", Version])).

packet_size(Header) ->
    PacketVer = aliter:get_config(packet_version, ?PACKETVER),
    (mod_for(integer_to_list(PacketVer))):packet_size(Header).
