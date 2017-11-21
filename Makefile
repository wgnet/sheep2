compile:
	rebar3 compile

ct:
	rebar3 ct

tests: ct

console:
	rebar3 shell

d:
	rebar3 dialyzer

clean:
	rebar3 clean

clean-all:
	rm -rf _build
	rm rebar.lock
