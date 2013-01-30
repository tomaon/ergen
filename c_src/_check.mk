#
 EGEN_HOME=/Volumes/Plextor/egen

#
 CXX=/opt/gnu/gcc/4.6.3/bin/g++
#CXX=/usr/bin/g++

 CXXFLAGS  = -D_TRACE
 CXXFLAGS += -DMSSQL # DB_INDICATOR=long
#CXXFLAGS += -std=c++98 # c++03
 CXXFLAGS += -D__unix
 CXXFLAGS += -D__STDC_FORMAT_MACROS -D__STDC_CONSTANT_MACROS
 CXXFLAGS += -g
#CXXFLAGS += -O2
#CXXFLAGS += -Wall
#CXXFLAGS += -Wextra
#CXXFLAGS += -Werror
 CXXFLAGS += -Wno-deprecated-declarations
 CXXFLAGS += -fPIC
 CXXFLAGS += -fno-common
 CXXFLAGS += -I$(EGEN_HOME)/inc

 CXXFLAGS += -pedantic
 CXXFLAGS += -Wno-long-long
 CXXFLAGS += -Wno-vla

 LDFLAGS  =
 LDFLAGS += -bundle
 LDFLAGS += -flat_namespace
 LDFLAGS += -undefined suppress
 LDFLAGS += -L$(EGEN_HOME)/lib

# for erlang_driver
 ERLANG_HOME = /opt/erlang/release/R15B02
 CXXFLAGS += -I$(ERLANG_HOME)/lib/erlang/usr/include
 CXXFLAGS += -I$(ERLANG_HOME)/lib/erlang/lib/erl_interface-3.7.8/include
 LDFLAGS += -L$(ERLANG_HOME)/lib/erlang/lib/erl_interface-3.7.8/lib
 LDFLAGS += -lerl_interface
 LDFLAGS += -lei

TARGET = libergen_bh_drv.so libergen_ce_drv.so libergen_dm_drv.so libergen_mee_drv.so

B_SRC = \
	ERGenBH_drv.C ERGen.C ERGenDrv.C ERGenDB.C \
	db/dummy/BrokerVolumeDB.C db/dummy/CustomerPositionDB.C \
	db/dummy/DataMaintenanceDB.C db/dummy/MarketFeedDB.C \
	db/dummy/MarketWatchDB.C db/dummy/SecurityDetailDB.C \
	db/dummy/TradeCleanupDB.C db/dummy/TradeLookupDB.C \
	db/dummy/TradeOrderDB.C db/dummy/TradeResultDB.C \
	db/dummy/TradeStatusDB.C db/dummy/TradeUpdateDB.C

C_SRC = ERGenCE_drv.C ERGen.C ERGenDrv.C
D_SRC = ERGenDM_drv.C ERGen.C ERGenDrv.C
M_SRC = ERGenMEE_drv.C ERGen.C ERGenDrv.C

#
.SUFFIXES: .o .C
.C.o:
	$(CXX) -c $< -o $@ $(CXXFLAGS)

#
all: $(TARGET)

libergen_bh_drv.so : $(B_SRC:.C=.o)
	$(CXX) $^ -o $@ $(LDFLAGS)

libergen_ce_drv.so : $(C_SRC:.C=.o)
	$(CXX) $^ -o $@ $(LDFLAGS)

libergen_dm_drv.so : $(D_SRC:.C=.o)
	$(CXX) $^ -o $@ $(LDFLAGS)

libergen_mee_drv.so : $(M_SRC:.C=.o)
	$(CXX) $^ -o $@ $(LDFLAGS)
#
clean:
	@-rm -f $(TARGET) *.*~ *.o
