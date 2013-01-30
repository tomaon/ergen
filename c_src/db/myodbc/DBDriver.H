#ifndef MYODBC_DB_DRIVER_H
#define MYODBC_DB_DRIVER_H

#include "../../ERGenDB.H"

#include "HandleEnv.H"
#include "HandleDbc.H"

namespace TPCE {

  class CBrokerVolumeDB;
  class CCustomerPositionDB;
  class CDataMaintenanceDB;
  class CMarketFeedDB;
  class CMarketWatchDB;
  class CSecurityDetailDB;
  class CTradeCleanupDB;
  class CTradeLookupDB;
  class CTradeOrderDB;
  class CTradeResultDB;
  class CTradeStatusDB;
  class CTradeUpdateDB;

  /*
   */
  class CDBConnectionMyODBC : public CDBConnectionInterface, public CHandleDbc {
  private:
    CBrokerVolumeDB *m_pBrokerVolumeDB;
    CCustomerPositionDB *m_pCustomerPositionDB;
    CDataMaintenanceDB *m_pDataMaintenanceDB;
    CMarketFeedDB *m_pMarketFeedDB;
    CMarketWatchDB *m_pMarketWatchDB;
    CSecurityDetailDB *m_pSecurityDetailDB;
    CTradeCleanupDB *m_pTradeCleanupDB;
    CTradeLookupDB *m_pTradeLookupDB;
    CTradeOrderDB *m_pTradeOrderDB;
    CTradeResultDB *m_pTradeResultDB;
    CTradeStatusDB *m_pTradeStatusDB;
    CTradeUpdateDB *m_pTradeUpdateDB;

  public:
    CDBConnectionMyODBC(CHandleEnv *pEnv);
    virtual ~CDBConnectionMyODBC();

    virtual CBrokerVolumeDBInterface *BrokerVolumeDB();
    virtual CCustomerPositionDBInterface *CustomerPositionDB();
    virtual CMarketWatchDBInterface *MarketWatchDB();
    virtual CSecurityDetailDBInterface *SecurityDetailDB();
    virtual CTradeLookupDBInterface *TradeLookupDB();
    virtual CTradeOrderDBInterface *TradeOrderDB();
    virtual CTradeStatusDBInterface *TradeStatusDB();
    virtual CTradeUpdateDBInterface *TradeUpdateDB();
    virtual CDataMaintenanceDBInterface *DataMaintenanceDB();
    virtual CTradeCleanupDBInterface *TradeCleanupDB();
    virtual CMarketFeedDBInterface *MarketFeedDB();
    virtual CTradeResultDBInterface *TradeResultDB();

  };

  /*
   */
  class CDBDriverMyODBC : public CDBDriverInterface, public CHandleEnv {
  private:
    char m_szConnectString[1024];
    CDBConnectionInterface *m_pDBConnection;

  public:
    CDBDriverMyODBC();
    virtual ~CDBDriverMyODBC();

    virtual void SetConfig(const char *szName, int iValue);
    virtual void SetConfig(const char *szName, long iValue);
    virtual void SetConfig(const char *szName, const char *szValue, size_t uValueLen);

    virtual CDBConnectionInterface *GetDBConnection();

  };

}

#endif // MYODBC_DB_DRIVER_H
