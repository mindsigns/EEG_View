ERL ?= erl
APP := eeg_view

.PHONY: deps

all: deps
	@./rebar compile skip_deps=true

compile:
	@./rebar skip_deps=true compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
