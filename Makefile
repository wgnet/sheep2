
get-deps:
	rebar get-deps

compile: get-deps
	rebar compile

compile-ct: get-deps
	rebar compile -C rebar_ct.config

test: compile-ct
	rebar skip_deps=true ct -C rebar_ct.config

clean:
	rebar -r clean
	rm -rf logs
	find . -name "*.beam" -delete