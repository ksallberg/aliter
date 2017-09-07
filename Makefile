CC = gcc
CFLAGS = -fPIC -O2 -Wall -shared -o priv/nif.so src/nif.c -lz
CONFIG_DIR = ~/.aliter
ERL = erl -pa ebin -pa scbin -pa lib/erlang-redis/ebin -pa
OS = ${shell uname -s}
SHA = ${shell git log -1 --pretty=format:%h}

include arch/${OS}/Makefile

all: build

build:
	${CC} ${CFLAGS} ${ARCHFLAGS} -I${ERL_TOP}/usr/include/
	rebar3 compile

start: build
	${ERL} -noshell -sname aliter -eval "application:start(sasl), \
                                             application:start(aliter)."
shell: build
	${ERL} -sname aliter

clean:
	@@echo "Removing compiled modules..."
	rm -f _build
