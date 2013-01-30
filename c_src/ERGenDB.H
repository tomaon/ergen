/*
 * ERGenDB.H
 */

#ifndef ERGEN_DB_H
#define ERGEN_DB_H

#include "TxnHarnessDBInterface.h"

namespace TPCE {

  /*
   */
  class CDBConnectionInterface {
  public:
    virtual ~CDBConnectionInterface() {};

    // for CCESUTInterface
    virtual CBrokerVolumeDBInterface *BrokerVolumeDB() = 0;
    virtual CCustomerPositionDBInterface *CustomerPositionDB() = 0;
    virtual CMarketWatchDBInterface *MarketWatchDB() = 0;
    virtual CSecurityDetailDBInterface *SecurityDetailDB() = 0;
    virtual CTradeLookupDBInterface *TradeLookupDB() = 0;
    virtual CTradeOrderDBInterface *TradeOrderDB() = 0;
    virtual CTradeStatusDBInterface *TradeStatusDB() = 0;
    virtual CTradeUpdateDBInterface *TradeUpdateDB() = 0;

    // for CDMSUTInterface
    virtual CDataMaintenanceDBInterface *DataMaintenanceDB() = 0;
    virtual CTradeCleanupDBInterface *TradeCleanupDB() = 0;

    // for MEESUTInterface
    virtual CMarketFeedDBInterface *MarketFeedDB() = 0;
    virtual CTradeResultDBInterface *TradeResultDB() = 0;

  };

  /*
   */
  class CDBDriverInterface {
  private:
    CDBConnectionInterface *m_pDBConnection;

  public:
    virtual ~CDBDriverInterface() {};

    virtual void SetConfig(const char *szName, int iValue) = 0;
    virtual void SetConfig(const char *szName, long iValue) = 0;
    virtual void SetConfig(const char *szName, const char *szValue, size_t uValueLen) = 0;

    virtual CDBConnectionInterface *GetDBConnection() = 0;
  };

  /*
   */
  class CDBDriverManager {
  private:
    CDBDriverInterface *m_pDBDriver;

  public:
    CDBDriverManager(const char *szPath, const char *szSymbol);
    virtual ~CDBDriverManager();

    virtual void SetConfig(const char *szName, int iValue);
    virtual void SetConfig(const char *szName, long iValue);
    virtual void SetConfig(const char *szName, const char *szValue, size_t uValueLen);

    virtual CDBConnectionInterface *GetDBConnection();
  };

} // namespace TPCE

#endif // ERGEN_DB_H
