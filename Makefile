.PHONY: test

REBAR=./rebar3

compile:
	${REBAR} compile

test:
	${REBAR} ct
	${REBAR} cover

clean:
	${REBAR} clean
