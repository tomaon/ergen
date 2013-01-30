#ifndef DUMMY_DB_DRIVER_H
#define DUMMY_DB_DRIVER_H

#include "../../ERGenDB.H"

#include "BrokerVolumeDB.H"
#include "CustomerPositionDB.H"
#include "DataMaintenanceDB.H"
#include "MarketFeedDB.H"
#include "MarketWatchDB.H"
#include "SecurityDetailDB.H"
#include "TradeCleanupDB.H"
#include "TradeLookupDB.H"
#include "TradeOrderDB.H"
#include "TradeResultDB.H"
#include "TradeStatusDB.H"
#include "TradeUpdateDB.H"

namespace TPCE {

  class CDBConnectionDummy : public CDBConnectionInterface {
  private:
    CBrokerVolumeDB m_BrokerVolumeDB;
    CCustomerPositionDB m_CustomerPositionDB;
    CDataMaintenanceDB m_DataMaintenanceDB;
    CMarketFeedDB m_MarketFeedDB;
    CMarketWatchDB m_MarketWatchDB;
    CSecurityDetailDB m_SecurityDetailDB;
    CTradeCleanupDB m_TradeCleanupDB;
    CTradeLookupDB m_TradeLookupDB;
    CTradeOrderDB m_TradeOrderDB;
    CTradeResultDB m_TradeResultDB;
    CTradeStatusDB m_TradeStatusDB;
    CTradeUpdateDB m_TradeUpdateDB;

  public:
    CDBConnectionDummy() {}

    virtual ~CDBConnectionDummy() {}

    virtual CBrokerVolumeDBInterface *BrokerVolumeDB() {
      return &m_BrokerVolumeDB;
    }

    virtual CCustomerPositionDBInterface *CustomerPositionDB() {
      return &m_CustomerPositionDB;
    }

    virtual CMarketWatchDBInterface *MarketWatchDB() {
      return &m_MarketWatchDB;
    }

    virtual CSecurityDetailDBInterface *SecurityDetailDB() {
      return &m_SecurityDetailDB;
    }

    virtual CTradeLookupDBInterface *TradeLookupDB() {
      return &m_TradeLookupDB;
    }

    virtual CTradeOrderDBInterface *TradeOrderDB() {
      return &m_TradeOrderDB;
    }

    virtual CTradeStatusDBInterface *TradeStatusDB() {
      return &m_TradeStatusDB;
    }

    virtual CTradeUpdateDBInterface *TradeUpdateDB() {
      return &m_TradeUpdateDB;
    }

    virtual CDataMaintenanceDBInterface *DataMaintenanceDB() {
      return &m_DataMaintenanceDB;
    }

    virtual CTradeCleanupDBInterface *TradeCleanupDB() {
      return &m_TradeCleanupDB;
    }

    virtual CMarketFeedDBInterface *MarketFeedDB() {
      return &m_MarketFeedDB;
    }

    virtual CTradeResultDBInterface *TradeResultDB() {
      return &m_TradeResultDB;
    }

  };

  class CDBDriverDummy : public CDBDriverInterface {
  public:
    CDBDriverDummy() {}
    virtual ~CDBDriverDummy() {}

    virtual void SetConfig(const char *szName, int iValue) {
      cout << "CDBDriverDummy::SetConfig/2, " << szName << "=" << iValue << '\r' << endl;
    }


    virtual void SetConfig(const char *szName, long iValue) {
      cout << "CDBDriverDummy::SetConfig/2, " << szName << "=" << iValue << '\r' << endl;
    }

    virtual void SetConfig(const char *szName, const char *szValue, size_t) {
      cout << "CDBDriverDummy::SetConfig/3, " << szName << "=" << szValue << '\r' << endl;
    }

    virtual CDBConnectionInterface *GetDBConnection() {
      return new CDBConnectionDummy();
    }

  };

}

#endif // DUMMY_DB_DRIVER_H
