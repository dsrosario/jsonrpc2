SHELL := bash
.ONESHELL:
.SHELLFLAGS := -euc
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

clean:
	@rebar3 clean
.PHONY: clean

compile:
	@rebar3 compile
.PHONY: compile

check: xref dialyzer
.PHONY: check

xref:
	@rebar3 xref
.PHONY: xref

dialyzer:
	@rebar3 dialyzer
.PHONY: dialyzer

test: ct proper
.NOTPARALLEL: test
.PHONY: test

ct:
	@rebar3 ct
.PHONY: ct

proper:
	@rebar3 proper
.PHONY: proper

