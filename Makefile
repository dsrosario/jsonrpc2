REBAR3 = rebar3

.PHONY: compile test

compile:
	@$(REBAR3) compile

test:
	@$(REBAR3) ct
	@$(REBAR3) proper
