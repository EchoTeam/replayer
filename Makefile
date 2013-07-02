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
