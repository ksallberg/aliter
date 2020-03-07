-module(packets).

-export([packet_size/1]).

-include("ro.hrl").

mod_for(Version) ->
  list_to_atom(lists:concat(["packets_", Version])).

packet_size(Header) ->
  (mod_for(integer_to_list(?PACKETVER))):packet_size(Header).
