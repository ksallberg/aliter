CC = gcc
CFLAGS = -fPIC -O2 -Wall -shared -o priv/nif.so src/nif.c -lz
OS = ${shell uname -s}
SHA = ${shell git log -1 --pretty=format:%h}

include arch/${OS}/Makefile

all: build

build:
	${CC} ${CFLAGS} ${ARCHFLAGS} \
	    -I${ERL_TOP}/erts/emulator/beam/ \
	    -I${ERL_TOP}/lib/erlang/usr/include \
	    -I/usr/lib/erlang/usr/include
	rebar3 compile

start: build
	erl start_sasl -pa _build/default/lib/*/ebin \
            -sname aliternode \
            -eval "application:start(aliter)." \
            -config priv/app.config

test_start: build
	export ALITER_CONFIG=aliter.config && \
            erl start_sasl -pa _build/default/lib/*/ebin \
                -sname aliternode \
                -eval "application:start(aliter)." \
                -config priv/app.config

clean:
	@@echo "Removing compiled modules..."
	rm -rf _build
	rm -f priv/nif.so
