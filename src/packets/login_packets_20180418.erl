-module(login_packets_20180418).

-export([unpack/1, pack/2, packet_size/1]).

% Login request
unpack(<<16#64:16/little,
        PacketVer:32/little,
        Login:24/little-binary-unit:8,
        Password:24/little-binary-unit:8,
        Region:8>>) ->
  Version = PacketVer,
  {login, Version, binary_to_string(Login), binary_to_string(Password), Region};

unpack(Unknown) ->
  lager:log(warning, self(), "Got unknown data ~p", [{data, Unknown}]),
  unknown.

pack(accept, {LoginIDa, LoginIDb, AccountID, Gender, Servers}) ->
  TwitterFlag = 0,
  [ <<16#0ac4:16/little,
      (length(Servers) * (32+128) + (47+16+1)):16/little,
      LoginIDa:32/little,
      AccountID:32/little,
      LoginIDb:32/little,
      0:32>>,
    string_to_binary("", 26),
    <<Gender:8>>,
    string_to_binary("", 16),
    <<TwitterFlag:8>>
  ] ++
    [ [ <<IA, IB, IC, ID, Port:16/little>>,
        string_to_binary(Name, 20),
        <<0:16, Maintenance:16, New:16>>,
        string_to_binary("", 128)
      ] ||
        { {IA, IB, IC, ID},
          Port,
          Name,
          0,
          Maintenance,
          New
        } <- Servers
    ];

pack(refuse, {Reason, S}) ->
  [ <<16#6a:16/little,
      Reason:8>>,
    string_to_binary(S, 20)
  ];

pack(Header, Data) ->
  lager:log(error, self(),
            "Cannot pack unknown data.",
            [{header, Header}, {data, Data}]),
  <<>>.

packet_size(Header) ->
  packets:packet_size(Header).

% Util functions
binary_to_string(Binary) ->
  lists:takewhile(
    fun(C) ->
      C /= 0
    end,
    binary_to_list(Binary)
  ).

string_to_binary(String, Num) ->
  list_to_binary(string:left(String, Num, 0)).
