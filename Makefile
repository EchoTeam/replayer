
# Default erlang node name is to be replayer@`hostname -s`
sname ?= replayer

# Which file to play with `make play`
file ?= /tmp/merged.log

.PHONY: deps

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

run: all
	erl -pa ./ebin/ -pa ./deps/*/ebin -run event_replayer_app start -sname $(sname)

play: all
	erl -pa ./ebin -pa ./deps/*/ebin -run event_replayer_app start -sname $(sname) -run replayer_controller simple_play "$(file)" -s erlang halt
