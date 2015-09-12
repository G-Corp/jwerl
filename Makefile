REBAR = ./rebar3
RM = rm
RM_F = rm -f
RM_RF = rm -rf

.PHONY: compile get-deps test doc perfs

all: escript

compile: get-deps
	@$(REBAR) as prod compile

get-deps:
	@$(REBAR) as prod upgrade

compile-doc: get-deps-doc
	@$(REBAR) as doc compile

get-deps-doc:
	@${REBAR} as doc compile

clean:
	@$(REBAR) as prod,doc clean
	$(RM_F) erl_crash.dump

realclean: 
	@$(REBAR) as prod,doc clean -a
	@$(RM_RF) _build
	@$(RM_RF) log

test: compile
	$(REBAR) as prod eunit

doc: compile compile-doc
	@$(RM_F) README.md
	@$(RM_RF) doc
	@$(REBAR) as prod,doc edoc

dev: compile
	@erl -pa _build/*/lib/*/ebin apps/*/ebin _build/*/lib/*/include 

escript: compile
	@$(REBAR) as prod escriptize

