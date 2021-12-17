REBARVER = 3.15.2
ifeq ($(OTPVER),24.0)
	REBARVER = 3.17.0
endif

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

rebar3:
	wget https://github.com/erlang/rebar3/releases/download/${REBARVER}/rebar3 &&\
	chmod u+x rebar3
