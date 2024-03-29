# Project Aliter

(NOTE: This is a hard fork of
[https://github.com/aliter/aliter/](https://github.com/aliter/aliter))

A lot has been changed in this version, compared to the repo it forks.
The server compiles with recent Erlang/OTP versions,
and has support for some operations in more recent ragnarok packet versions.

This is an Erlang/OTP based ragnarok online toy server, not completely
implementing the protocol.

For a real working version that compiles on Linux and macOS see
[Hercules](https://github.com/HerculesWS/Hercules).

## Packetver

* 20111116 partially supported. Specifically it works
together with [roBrowser](https://github.com/vthibault/roBrowser).

* 20180418 partially supported. Similar functionality
as 20111116. Only tested with windows based renewal client.

Packetver can be configured in a `aliter config file`.
Otherwise it will fallback to the value defined in `ro.hrl`. (See below)

## aliter config file

A format where you can specify some basic config without needing to recompile
the project.

aliter will look for the `ALITER_CONFIG` environment variable to see if one
is specified, otherwise it will fall back to values defined in `ro.hrl`.

example:
`ALITER_CONFIG=aliter_robrowser.config make start`

## Dependencies

### Erlang/OTP

Erlang/OTP 20+, rebar3.
Also help the Makefile by exporting ERL_TOP to you erlang installation.
(if not already done) (test echo $ERL_TOP)

## How to use

`make start`. If you want to use a custom `aliter config file`,
see `aliter config file` section above on how it is done.

## roBrowser Client
The following [roBrowser](https://www.robrowser.com) config can be used
to connect to aliter, if aliter is configured to use packet version 20111116.

(Needs an older version of the ragnarok client files)

`ALITER_CONFIG=aliter_robrowser.config make start`

Put this in the `applications` dir of roBrowser:

```javascript
var ROConfig = {
    development: true,
    width:       1000,
    height:      800,
    packetver:   20111116,
    servers: [{
        disableKorean: true,
        display:       'aliter',
        desc:          'aliter',
        address:       '127.0.0.1',
        port:          6900,
        version:       24,
        langtype:      20,
        packetver:     20111116
    }]
};
```

## Using / In-game

### Aliter Commands

```
\caps text           - chat "TEXT"
\crash               - Yeeah...
\warp prontera 50 50 - Travel somewhere
\load                - Travel to last saved spot
\item 1228           - You got Combat Knife (1).
\item 2268 1         - Pipe on lower head
\item 1162 2         - broad sword[2] on R-hand
\item 2503 4         - muffler on robe
\item xxxx 8         - left accessory
\item 2314 16        - chain mail on body
\item 2107 32        - shield on L-hand
\item 2405 64        - boots
\item xxxx 128       - right accessory
\item 5170 256       - feather beret to mount on head
\zeny 100            - You get 100 zeny
\hat 110             - Give spriteid to display headgear on top (angel helm)
\hat 224             - (feather beret)
\monster 1373 50 50  - Spawn LOD to tile X:50, Y:50
\npc 20 50 50        - Spawn NPC to tile X:50, Y:50
\job 10              - Become blacksmith
\maxstats            - raise all stats and save to db
\guild_add charname  - add character to the guild I am in
```

### Supporter RO Commands

```
/stand
/sit
/guild Name
```

### Maps

For currently enabled maps, see zone_map:zones/0.

### NPCs

Limited scripting of NPCs is supported, see "script/npcs.aliter",

Example of 'if-statement':

```
{"Young Man", 59, <<"payon">>,
  [{134, 211, south}],
  [{say, "[Young Man]"},
   {say, "Time to chose"},
   {menu, [{"First", [{say, "You chose first.."}]},
           {"Second", [{say, "You chose second!"},
                       {say, "Then I'll keep talking"}]
           }]},
   {say, "OK I'll say this too."},
   close]}.
```

Example of nested 'if-statement':

```
{"Complete", 1373, <<"aldebaran">>,
  [{50, 50, south}],
  [{say, "[Complete dialogue]"},
   {say, "Press next to continue"},
   next,
   {say, "[Complete dialogue]"},
   {say, "Use the menu"},
   {menu, [{"First", [{say, "You chose first.."},
                      {say, "thats ok"}]},
           {"Second", [{say, "You chose second!"},
                       {say, "also ok"}]},
           {"3rd", [{say, "This is an inner menu!"},
                    {menu, [{"a", [{say, "You chose a.."},
                                   {say, "a you said..."}]},
                            {"b", [{say, "You chose b!"},
                                   {say, "b has other logic"},
                                   {say, "some more even..."}]}
                           ]}]}
          ]},
   {say, "Finally, after the menu, I will say this "
         "regardless of what you chose."},
   close]}.
```

# Testing

A basic [lux](https://github.com/hawk/lux) based test exists,
use through `make test`. It will spin up a server, and a fake
client, and make sure parts of the initial communication happens correctly.

# System design

[Ranch](https://github.com/ninenines/ranch) is used as a tcp pool to create a pair of {worker, ragnarok_proto}
per connection *and* server. So {login_worker, ragnarok_proto},
{char_worker, ragnarok_proto} and {zone_worker, ragnarok_proto}.

The workers are responsible for actual business logic and are all gen_servers.
The ragnarok_proto only reads and writes TCP. 'src/packets' contains the code
for parsing and serializing to binary.

login_srv, char_srv and all zone_srv's are mostly used for book keeping.

## Supervisor tree:
![alt tag](https://i.imgur.com/gVsfYY8.png)

# Remember for myself:
`export ERL_TOP=./home/krisallb/Documents/otp_src_23.2`

## Wireshark filter for windows machine from mac

`src host 10.0.1.108 || dst host 10.0.1.108`

## Example on how to encode packet and save to file:

```
CC = [<<16#64:16/little,
 20180418:32/little>>,

list_to_binary(string:left("test", 24, 0)),
list_to_binary(string:left("test", 24, 0)),

<<0:8>>
].

file:write_file("file.bin", iolist_to_binary(CC)).
```
