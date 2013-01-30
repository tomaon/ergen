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
 LDFLAGS += -L$(EGEN_HOME)/lib -legen

# for iodbc
 ODBC_HOME=/usr
#CXXFLAGS += -DUNICODE
 CXXFLAGS += -Wno-deprecated-declarations
 CXXFLAGS += $(shell $(ODBC_HOME)/bin/iodbc-config --cflags)
 LDFLAGS += $(shell $(ODBC_HOME)/bin/iodbc-config --libs)

TARGET = libergen_db_dummy.so

DB_SRC = \
	odbc.C DBDriver.C HandleEnv.C HandleDbc.C HandleStmt.C Sequence.C \
	CashTransactionStmt.C SettlementStmt.C TradeHistoryStmt.C \
	BrokerVolumeDB.C CustomerPositionDB.C DataMaintenanceDB.C \
	MarketFeedDB.C MarketWatchDB.C SecurityDetailDB.C \
	TradeCleanupDB.C TradeLookupDB.C TradeOrderDB.C \
	TradeResultDB.C TradeStatusDB.C TradeUpdateDB.C

#
.SUFFIXES: .o .C
.C.o:
	$(CXX) -c $< -o $@ $(CXXFLAGS)


#
all: $(TARGET)

libergen_db_dummy.so : $(DB_SRC:.C=.o)
	$(CXX) $< -o $@ $(LDFLAGS)

#
clean:
	@-rm -f $(TARGET) *.*~ *.o
