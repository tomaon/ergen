#
 ERGEN_HOME = $(shell pwd)

 EGEN_HOME = $(ERGEN_HOME)/files/egen
 EGEN_VER = 1.12.0

 ERLANG_HOME ?= /opt/erlang/release/latest

 GCC_HOME = /opt/gnu/gcc/4.7.3

#
 CC = $(GCC_HOME)/bin/gcc

 CXX = $(GCC_HOME)/bin/g++

 CXXFLAGS  =
 CXXFLAGS += -std=c++98 # c++03
 CXXFLAGS += -D__unix
 CXXFLAGS += -D__STDC_FORMAT_MACROS -D__STDC_CONSTANT_MACROS
 CXXFLAGS += -g
 CXXFLAGS += -Wall
 CXXFLAGS += -fPIC
 CXXFLAGS += -fno-common

 LDFLAGS  =

#
 ENV  = DYLD_LIBRARY_PATH=$(GCC_HOME)/lib
 ENV += ERL_LIBS="$(ERGEN_HOME):$(ERGEN_HOME)/deps"

 REBAR_BIN  = ./rebar

 REBAR_ENV  =
 REBAR_ENV += PATH=$(ERLANG_HOME)/bin:$(PATH)
 REBAR_ENV += EGEN_HOME="$(EGEN_HOME)"
 REBAR_ENV += CC="$(CC)"
 REBAR_ENV += CXX="$(CXX)"
 REBAR_ENV += CXXFLAGS="$(CXXFLAGS)"
 REBAR_ENV += LDFLAGS="$(LDFLAGS)"
 REBAR_ENV += MAKE=$(MAKE)

 REBAR_OPT  =
#REBAR_OPT += --verbose 3

 ERL_OPT  =
 ERL_OPT += -env EGEN_HOME $(EGEN_HOME)
 ERL_OPT += -pa ebin deps/*/ebin
 ERL_OPT += +A30
 ERL_OPT += +K true
 ERL_OPT += +P 1048576

 PLT = .dialyzer_plt.local

 DIALYZER_OPT  =
 DIALYZER_OPT += --no_native
 DIALYZER_OPT += --plts $(ERLANG_HOME)/.dialyzer_plt $(PLT)
 DIALYZER_OPT += --src src
 DIALYZER_OPT += -I deps
 DIALYZER_OPT += -I ..

#
all: compile

delete-deps get-deps:
	@$(ENV) $(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@

post_delete-deps:
	@-rm -rf $(EGEN_HOME)

post_get-deps: post_delete-deps
	unzip -d $(EGEN_HOME) $(ERGEN_HOME)/files/EGen_v$(EGEN_VER).zip
	patch -p1 -d $(EGEN_HOME) < $(ERGEN_HOME)/files/EGen_v$(EGEN_VER).patch
	cd $(EGEN_HOME)/prj; CCFLAGS="$(CXXFLAGS)" $(MAKE)

clean compile:
	@$(ENV) $(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@ skip_deps=true

post_compile:
	@cp apps/*/ebin/*.app ebin

cleanall:
	@$(ENV) $(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) clean

build_plt:
	@$(ERLANG_HOME)/bin/dialyzer --$@ --output_plt $(PLT) --apps deps/*/ebin

dialyzer:
	@$(ERLANG_HOME)/bin/dialyzer $(DIALYZER_OPT)

build: get-deps
	@$(ENV) $(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) compile

distclean: clean delete-deps
	@-rm -f $(PLT)

#
run: init
	@$(ENV) $(ERLANG_HOME)/bin/erl $(ERL_OPT) -s folsom -s ergen -config files/$@

init:
	@$(ENV) files/$@.escript
