#
 EGEN_HOME = /Volumes/Plextor/egen/1.12.0.1
 EGEN_BASE = $(shell pwd)

 GCC_HOME = /opt/gnu/gcc/4.6.3
#GCC_HOME = /usr

#
 ENV  = DYLD_LIBRARY_PATH=$(GCC_HOME)/lib
 ENV += ERL_LIBS="$(EGEN_BASE):$(EGEN_BASE)/deps"

 REBAR_BIN  = $(HOME)/Desktop/work/tmp/rebar/rebar

 REBAR_ENV  = $(ENV)
 REBAR_ENV += CC="$(GCC_HOME)/bin/g++"
 REBAR_ENV += CXX="$(GCC_HOME)/bin/g++"
 REBAR_ENV += EGEN_HOME="$(EGEN_HOME)"

 REBAR_OPT  =
#REBAR_OPT += --verbose 3

 ERL_OPT  =
 ERL_OPT += -env EGEN_HOME $(EGEN_HOME) 
 ERL_OPT += +A30
 ERL_OPT += +K true
 ERL_OPT += +P 1048576

 ESCRIPT_OPT  =

 EUNIT_OPT    =

 PLT = .dialyzer_plt.local

 DIALYZER_OPT  =
 DIALYZER_OPT += --no_native 
 DIALYZER_OPT += --plts ~/.dialyzer_plt $(PLT)
 DIALYZER_OPT += --src src
#DIALYZER_OPT += -Wno_return
#DIALYZER_OPT += -Wno_unused
#DIALYZER_OPT += -Wno_improper_lists
#DIALYZER_OPT += -Wno_tuple_as_fun ?
#DIALYZER_OPT += -Wno_fun_app
#DIALYZER_OPT += -Wno_match
#DIALYZER_OPT += -Wno_opaque
#DIALYZER_OPT += -Wunmatched_returns
#DIALYZER_OPT += -Werror_handling
#DIALYZER_OPT += -Wrace_conditions
#DIALYZER_OPT += -Wbehaviours ?
#DIALYZER_OPT += -Wunderspecs
##DIALYZER_OPT += -Woverspecs
##DIALYZER_OPT += -Wspecdiffs

 TYPER_OPT  =
 TYPER_OPT += src/*.erl

#
all: compile

compile:
#	rm -f c_src/*.o
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@
	mv apps/*/ebin/*.app ebin

check-deps list-deps get-deps update-deps delete-deps:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@

doc:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@ skip_deps=true

test:
	ERL_FLAGS="$(EUNIT_OPT)" $(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) skip_deps=true eunit

clean:
	@rm -f erl_crash.dump
	@$(REVAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@

#
dialyzer: compile
#	$(ENV) $(ERLANG_HOME)/bin/$@ $(DIALYZER_OPT) | fgrep -v -f .dialyzer.ignore-warnings
	$(ENV) $(ERLANG_HOME)/bin/$@ $(DIALYZER_OPT)
typer: compile
	$(ENV) $(ERLANG_HOME)/bin/$@ $(TYPER_OPT)

#
env: init
	@$(ENV) $(ERLANG_HOME)/bin/erl $(ERL_OPT) -s folsom
bh ce dm mee: init
	@$(ENV) $(ERLANG_HOME)/bin/erl $(ERL_OPT) -s ergen_$@
init run:
	@ERL_FLAGS="$(ESCRIPT_OPT)" $(ENV) ./$@.escript

#
build_plt: compile
	dialyzer --build_plt --output_plt $(PLT) --apps deps/*/ebin
check_plt:
	dialyzer --check_plt --plt $(PLT) -c .
