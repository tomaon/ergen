/*
 */

#include <algorithm> // find,transform

#include "DBDriver.H"

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

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  CDBConnectionMyODBC::CDBConnectionMyODBC(CHandleEnv *pEnv)
    : CDBConnectionInterface(), CHandleDbc(pEnv),
      m_pBrokerVolumeDB(NULL), m_pCustomerPositionDB(NULL),
      m_pDataMaintenanceDB(NULL), m_pMarketFeedDB(NULL),
      m_pMarketWatchDB(NULL), m_pSecurityDetailDB(NULL),
      m_pTradeCleanupDB(NULL), m_pTradeLookupDB(NULL),
      m_pTradeOrderDB(NULL), m_pTradeResultDB(NULL),
      m_pTradeStatusDB(NULL), m_pTradeUpdateDB(NULL) {
  }

  /*
   */
  CDBConnectionMyODBC::~CDBConnectionMyODBC() {

    if (NULL != m_pBrokerVolumeDB) delete m_pBrokerVolumeDB;
    if (NULL != m_pCustomerPositionDB) delete m_pCustomerPositionDB;
    if (NULL != m_pDataMaintenanceDB) delete m_pDataMaintenanceDB;
    if (NULL != m_pMarketFeedDB) delete m_pMarketFeedDB;
    if (NULL != m_pMarketWatchDB) delete m_pMarketWatchDB;
    if (NULL != m_pSecurityDetailDB) delete m_pSecurityDetailDB;
    if (NULL != m_pTradeCleanupDB) delete  m_pTradeCleanupDB;
    if (NULL != m_pTradeLookupDB) delete m_pTradeLookupDB;
    if (NULL != m_pTradeOrderDB) delete m_pTradeOrderDB;
    if (NULL != m_pTradeResultDB) delete m_pTradeResultDB;
    if (NULL != m_pTradeStatusDB) delete m_pTradeStatusDB;
    if (NULL != m_pTradeUpdateDB) delete m_pTradeUpdateDB;

    Disconnect();
  }

  /*
   */
  CBrokerVolumeDBInterface *CDBConnectionMyODBC::BrokerVolumeDB() {
    if (NULL == m_pBrokerVolumeDB) {
      m_pBrokerVolumeDB = new CBrokerVolumeDB(this);
    }
    return m_pBrokerVolumeDB;
  }

  /*
   */
  CCustomerPositionDBInterface *CDBConnectionMyODBC::CustomerPositionDB() {
    if (NULL == m_pCustomerPositionDB) {
      m_pCustomerPositionDB = new CCustomerPositionDB(this);
    }
    return m_pCustomerPositionDB;
  }

  /*
   */
  CMarketWatchDBInterface *CDBConnectionMyODBC::MarketWatchDB() {
    if (NULL == m_pMarketWatchDB) {
      m_pMarketWatchDB = new CMarketWatchDB(this);
    }
    return m_pMarketWatchDB;
  }

  /*
   */
  CSecurityDetailDBInterface *CDBConnectionMyODBC::SecurityDetailDB() {
    if (NULL == m_pSecurityDetailDB) {
      m_pSecurityDetailDB = new CSecurityDetailDB(this);
    }
    return m_pSecurityDetailDB;
  }

  /*
   */
  CTradeLookupDBInterface *CDBConnectionMyODBC::TradeLookupDB() {
    if (NULL == m_pTradeLookupDB) {
      m_pTradeLookupDB = new CTradeLookupDB(this);
    }
    return m_pTradeLookupDB;
  }

  /*
   */
  CTradeOrderDBInterface *CDBConnectionMyODBC::TradeOrderDB() {
    if (NULL == m_pTradeOrderDB) {
      m_pTradeOrderDB = new CTradeOrderDB(this);
    }
    return m_pTradeOrderDB;
  }

  /*
   */
  CTradeStatusDBInterface *CDBConnectionMyODBC::TradeStatusDB() {
    if (NULL == m_pTradeStatusDB) {
      m_pTradeStatusDB = new CTradeStatusDB(this);
    }
    return m_pTradeStatusDB;
  }

  /*
   */
  CTradeUpdateDBInterface *CDBConnectionMyODBC::TradeUpdateDB() {
    if (NULL == m_pTradeUpdateDB) {
      m_pTradeUpdateDB = new CTradeUpdateDB(this);
    }
    return m_pTradeUpdateDB;
  }

  /*
   */
  CDataMaintenanceDBInterface *CDBConnectionMyODBC::DataMaintenanceDB() {
    if (NULL == m_pDataMaintenanceDB) {
      m_pDataMaintenanceDB = new CDataMaintenanceDB(this);
    }
    return m_pDataMaintenanceDB;
  }

  /*
   */
  CTradeCleanupDBInterface *CDBConnectionMyODBC::TradeCleanupDB() {
    if (NULL == m_pTradeCleanupDB) {
      m_pTradeCleanupDB = new CTradeCleanupDB(this);
    }
    return m_pTradeCleanupDB;
  }

  /*
   */
  CMarketFeedDBInterface *CDBConnectionMyODBC::MarketFeedDB() {
    if (NULL == m_pMarketFeedDB) {
      m_pMarketFeedDB = new CMarketFeedDB(this);
    }
    return m_pMarketFeedDB;
  }

  /*
   */
  CTradeResultDBInterface *CDBConnectionMyODBC::TradeResultDB() {
    if (NULL == m_pTradeResultDB) {
      m_pTradeResultDB = new CTradeResultDB(this);
    }
    return m_pTradeResultDB;
  }

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  CDBDriverMyODBC::CDBDriverMyODBC()
    : CDBDriverInterface(), CHandleEnv(), m_pDBConnection(NULL) {

    const char *szConnectString =
      "dsn=myodbc"
      ";server=localhost"
      ";socket=/tmp/mysql.sock"
      ";user=tpce"
      ";database=tpce";

    strncpy(m_szConnectString, szConnectString, sizeof(m_szConnectString));
  }

  /*
   */
  CDBDriverMyODBC:: ~CDBDriverMyODBC() {
    if (NULL != m_pDBConnection) delete m_pDBConnection;
  }

  /*
   */
  void CDBDriverMyODBC::SetConfig(const char *, int) {
  }

  /*
   */
  void CDBDriverMyODBC::SetConfig(const char *, long) {
  }

  /*
   */
  void CDBDriverMyODBC::SetConfig(const char *szName, const char *szValue, size_t) {

    const string szNames[] = {
      "connectstring",
    };
    const size_t uiNames = sizeof(szNames) / sizeof(szNames[0]);

    string s(szName);
    transform(s.begin(), s.end(), s.begin(), ::tolower);

    switch (find(szNames, szNames + uiNames, s) - szNames) {
    case 0:
      strncpy(m_szConnectString, szValue, sizeof(m_szConnectString));
      break;
    default:
      break;
    }
  }

  /*
   */
  CDBConnectionInterface *CDBDriverMyODBC::GetDBConnection() {

    if (NULL == m_pDBConnection) {

      CDBConnectionMyODBC *pDBConnection = new CDBConnectionMyODBC(this);

      if (NULL != pDBConnection) {

        if (pDBConnection->Connect(m_szConnectString,
                                   (SQLPOINTER)SQL_MODE_READ_WRITE,
                                   (SQLPOINTER)SQL_TXN_REPEATABLE_READ)) {

          m_pDBConnection = pDBConnection;
        }
      }
    }

    return m_pDBConnection;
  }

}
