compile:
	rebar3 compile

ct:
	rebar3 ct

tests: ct

console:
	erl -pa _build/default/lib/*/ebin

d:
	rebar3 dialyzer

clean:
	rebar3 clean

clean-all:
	rm -rf _build
