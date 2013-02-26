#
 ERGEN_HOME = $(shell pwd)

 EGEN_HOME = $(ERGEN_HOME)/files/egen
 EGEN_VER = 1.12.0

 GCC_HOME = /opt/gnu/gcc/4.6.3

#
 CXX = $(GCC_HOME)/bin/g++

 CXXFLAGS  =
 CXXFLAGS += -std=c++98 # c++03
 CXXFLAGS += -D__unix
 CXXFLAGS += -D__STDC_FORMAT_MACROS -D__STDC_CONSTANT_MACROS
 CXXFLAGS += -g
 CXXFLAGS += -Wall
 CXXFLAGS += -fPIC
 CXXFLAGS += -fno-common
 CXXFLAGS += -I$(EGEN_HOME)/inc

 LDFLAGS  =
 LDFLAGS += -L$(EGEN_HOME)/lib -legen
 LDFLAGS += -lstdc++

#
 ENV  = DYLD_LIBRARY_PATH=$(GCC_HOME)/lib
 ENV += ERL_LIBS="$(ERGEN_HOME):$(ERGEN_HOME)/deps"

 REBAR_BIN  = $(HOME)/Desktop/work/tmp/rebar/rebar

 REBAR_ENV  = $(ENV)
 REBAR_ENV += CXX="$(CXX)"
 REBAR_ENV += CXXFLAGS="$(CXXFLAGS)"
 REBAR_ENV += LDFLAGS="$(LDFLAGS)"
 REBAR_ENV += MAKE=$(MAKE)

 REBAR_OPT  =
#REBAR_OPT += --verbose 3

 ERL_OPT  =
 ERL_OPT += -env EGEN_HOME $(EGEN_HOME)
 ERL_OPT += +A30
 ERL_OPT += +K true
 ERL_OPT += +P 1048576

 PLT = .dialyzer_plt.local

 DIALYZER_OPT  =
 DIALYZER_OPT += --no_native
 DIALYZER_OPT += --plts ~/.dialyzer_plt $(PLT)
 DIALYZER_OPT += --src src

#
all: compile

get-deps delete-deps:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@

post_get-deps: post_delete-deps
	unzip -d $(EGEN_HOME) $(ERGEN_HOME)/files/EGen_v$(EGEN_VER).zip
	patch -p1 -d $(EGEN_HOME) < $(ERGEN_HOME)/files/EGen_v$(EGEN_VER).patch
	cd $(EGEN_HOME)/prj; CCFLAGS="$(CXXFLAGS)" $(MAKE)

post_delete-deps:
	@-rm -rf $(EGEN_HOME)

compile-deps:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) compile

compile clean:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@ skip_deps=true

post_compile:
	@cp apps/*/ebin/*.app ebin

dialyzer:
	@$(ENV) $(ERLANG_HOME)/bin/$@ $(DIALYZER_OPT)

build_plt:
	@$(ERLANG_HOME)/bin/dialyzer --$@ --output_plt $(PLT) --apps deps/*/ebin

build: get-deps compile-deps build_plt

cleanall: clean delete-deps
	@-rm -f $(PLT)

#
run: init
	@$(ENV) $(ERLANG_HOME)/bin/erl $(ERL_OPT) -s folsom -s ergen
bh ce dm mee: init
	@$(ENV) $(ERLANG_HOME)/bin/erl $(ERL_OPT) -s ergen_$@
init:
	@$(ENV) ./$@.escript
