###############################################################################

-include .env
export

.PHONY: compile

clear:
	rebar3 clean

compile:
	rebar3 clean
	rebar3 compile

run-apigdev:
	rebar3 clean
	rebar3 compile
	rebar3 as apigdev shell

run-mocksdev:
	rebar3 clean
	rebar3 compile
	rebar3 as mocksdev shell

ct:
	rebar3 ct as test

run-prod:
	rebar3 clean
	rebar3 compile
#	rm -fr ./_build/prod
	rebar3 as prod release
	./_build/prod/rel/garm/bin/garm console

release:
	rebar3 as prod release
