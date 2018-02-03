# Project Aliter

This is an Erlang/OTP based ragnarok online server.

## Dependencies

TL;DR: Erlang/OTP, and the redis kv-store.

### Erlang/OTP
This branch of Aliter has been tested only with Erlang/OTP 20.2.2.
The main motivation of branching off from the main Aliter branch
was that it did not build using modern Erlang/OTP versions.

Requirements: Erlang/OTP 19+, rebar3.
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
        port:          6900,
        version:       24,
        langtype:      20,
        packetver:     20111116
    }]
};
```

## Using / In-game

### Commands

```
\caps text           - chat "TEXT"
\crash               - Yeeah...
\warp prontera 50 20 - Travel
\item 1228           - You got Combat Knife (1).
\zeny 100             - You get 100 zeny
```

### Maps

Currently supported maps are:

```erlang
 zones() ->
    {zones,
     [{6121,
       ["prontera",
        "izlude",
        "prt_monk",
        "prt_fild00",
        "prt_fild01",
        "prt_fild02",
        "prt_fild03",
        "prt_fild04",
        "prt_fild05",
        "prt_fild06",
        "prt_fild07",
        "prt_fild08",
        "prt_fild09",
        "prt_fild10",
        "prt_fild11",
        "new_1-1"]},
      {6122,
       ["geffen",
        "gef_fild00",
        "gef_fild01",
        "gef_fild02",
        "gef_fild03",
        "gef_fild04",
        "gef_fild05",
        "gef_fild06",
        "gef_fild07",
        "gef_fild08",
        "gef_fild09",
        "gef_fild10",
        "gef_fild11",
        "gef_fild12",
        "gef_fild13",
        "gef_fild14"]},
      {6123,
       ["payon",
        "pay_arche",
        "pay_fild01",
        "pay_fild02",
        "pay_fild03",
        "pay_fild04",
        "pay_fild05",
        "pay_fild06",
        "pay_fild07",
        "pay_fild08",
        "pay_fild09",
        "pay_fild10",
        "pay_fild11"]},
      {6124,
       ["morocc",
        "moc_fild01",
        "moc_fild02",
        "moc_fild03",
        "moc_fild04",
        "moc_fild05",
        "moc_fild06",
        "moc_fild07",
        "moc_fild08",
        "moc_fild09",
        "moc_fild10",
        "moc_fild11",
        "moc_fild12",
        "moc_fild13",
        "moc_fild14",
        "moc_fild15",
        "moc_fild16",
        "moc_fild17",
        "moc_fild18",
        "moc_fild19",
        "moc_fild20",
        "moc_fild21",
        "moc_fild22",
        "moc_ruins"]}]}.
```
