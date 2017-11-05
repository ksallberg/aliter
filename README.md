# Project Aliter

This is an Erlang/OTP based ragnarok online server.

## Dependencies

TL;DR: Erlang/OTP, and the redis kv-store.

### Erlang/OTP
This branch of Aliter has been tested only with Erlang/OTP 18.3.4.
The main motivation of branching off from the main Aliter branch
was that it did not build using modern Erlang/OTP versions.

Requirements: Erlang/OTP 18.3.4+, rebar3.
Also help the Makefile by exporting ERL_TOP to you erlang installation.
(if not already done) (test echo $ERL_TOP)

#### Lager config
The main branch of Aliter uses a custom loggin module. I replaced this
with lager, the config of lager is placed in priv/app.config, and loaded
to erl command line in the Makefile.

### Database
I used a redis 4.0.2 (REmote DIctionary Server.) as a docker container,
exposing the port towards my server like this:

`docker run -d -p 6379:6379 --name ragna-redis redis redis-server --appendonly yes`

## How to use
`make build; make start`

## PACKETVER
The packetver is configured in include/ro.hrl, currently I use 20111116.
The server was previously using 20090901 and it seems that many package
are still parsed that way and thus the server(s) cannot many requests.

## Client
I have only tested this with the [roBrowser](https://www.robrowser.com)
chrome app. The config I use (by putting it in the applications dir) is:

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
        port:          6941,
        version:       24,
        langtype:      20,
        packetver:     20111116
    }]
};
```
