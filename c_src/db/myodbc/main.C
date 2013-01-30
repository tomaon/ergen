/*
 */
#include "TxnHarnessBrokerVolume.h"
#include "TxnHarnessCustomerPosition.h"
#include "TxnHarnessDataMaintenance.h"
#include "TxnHarnessMarketFeed.h"
#include "TxnHarnessMarketWatch.h"
#include "TxnHarnessSecurityDetail.h"
#include "TxnHarnessSendToMarketInterface.h"
#include "TxnHarnessTradeCleanup.h"
#include "TxnHarnessTradeLookup.h"
#include "TxnHarnessTradeOrder.h"
#include "TxnHarnessTradeResult.h"
#include "TxnHarnessTradeStatus.h"
#include "TxnHarnessTradeUpdate.h"

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

using namespace std;
using namespace TPCE;

namespace TPCE {
  class CSendToMarket : public CSendToMarketInterface {
  protected:
    bool SendToMarket(TTradeRequest &trade_mes) {
      cout << "request=" << trade_mes << endl;
      return true;
    }
  };
}


void doBrokerVolume(CDBConnectionInterface *pDbc) {

  TBrokerVolumeTxnInput In;
  memset(&In, 0x00, sizeof(In));
  TBrokerVolumeTxnOutput Out;
  memset(&Out, 0x00, sizeof(Out));

  strcpy(In.broker_list[0], "Donald A. Gunsolus");
  strcpy(In.broker_list[1], "Mabel G. Clawson");
  strcpy(In.broker_list[2], "Ronald Y. Thatch");
  In.broker_list[3][0] = '\0';

  strcpy(In.sector_name, "Technology");

  CBrokerVolumeDBInterface *pDb = pDbc->BrokerVolumeDB();

  CBrokerVolume hDrv(pDb);
  hDrv.DoTxn(&In, &Out);

#ifdef _TRACE
  cout << "result=" << Out << endl;
#endif
}

void doCustomerPosition(CDBConnectionInterface *pDbc) {

  TCustomerPositionTxnInput In;
  memset(&In, 0x00, sizeof(In));
  TCustomerPositionTxnOutput Out;
  memset(&Out, 0x00, sizeof(Out));

  In.cust_id = 4300000002;
  In.get_history = 0;
//strcpy(In.tax_id, "803MO6290MK444");

//In.acct_id_idx = -1;
//In.cust_id = 0;
//In.get_history = 0;
//strcpy(In.tax_id, "948TH7004EM262");

  CCustomerPositionDBInterface *pDb = pDbc->CustomerPositionDB();

  CCustomerPosition hDrv(pDb);
  hDrv.DoTxn(&In, &Out);

#ifdef _TRACE
  cout << "result=" << Out << endl;
#endif
}

void doDataMaintenance(CDBConnectionInterface *pDbc) {

  TDataMaintenanceTxnInput In;
  memset(&In, 0x00, sizeof(In));
  TDataMaintenanceTxnOutput Out;
  memset(&Out, 0x00, sizeof(Out));

  int n = 12;

  switch (n) {
  case 1: {
    strcpy(In.table_name, "ACCOUNT_PERMISSION");
    In.acct_id = 43000000012;
  } break;
  case 2: {
    strcpy(In.table_name, "ADDRESS");
    In.c_id = 4300000002;
    In.co_id = 4300000050;
  } break;
  case 3: {
    strcpy(In.table_name, "COMPANY");
    In.co_id = 4300000050;
  } break;
  case 4: {
    strcpy(In.table_name, "CUSTOMER");
    In.c_id = 4300000002;
  } break;
  case 5: {
    strcpy(In.table_name, "CUSTOMER_TAXRATE");
    In.c_id = 4300000002;
  } break;
  case 6: {
    strcpy(In.table_name, "DAILY_MARKET");
    strcpy(In.symbol, "A");
    In.day_of_month = 20;
    In.vol_incr = 1000;
  } break;
  case 7: {
    strcpy(In.table_name, "EXCHANGE");
  } break;
  case 8: {
    strcpy(In.table_name, "FINANCIAL");
    In.co_id = 4300000050;
  } break;
  case 9: {
    strcpy(In.table_name, "NEWS_ITEM");
    In.co_id = 4300000050;
  } break;
  case 10: {
    strcpy(In.table_name, "SECURITY");
    strcpy(In.symbol, "A");
  } break;
  case 11: {
    strcpy(In.table_name, "TAXRATE");
    strcpy(In.tx_id, "YT7");
  } break;
  case 12: {
    strcpy(In.table_name, "WATCH_ITEM");
    In.c_id = 4300000002;
  } break;
  }

  CDataMaintenanceDBInterface *pDb = pDbc->DataMaintenanceDB();

  CDataMaintenance hDrv(pDb);
  hDrv.DoTxn(&In, &Out);

#ifdef _TRACE
  cout << "result=" << Out << endl;
#endif
}

void doMarketFeed(CDBConnectionInterface *pDbc) {

  TMarketFeedTxnInput In;
  memset(&In, 0x00, sizeof(In));
  TMarketFeedTxnOutput Out;
  memset(&Out, 0x00, sizeof(Out));

//In.unique_symbols = 0;
  strcpy(In.zz_padding1, "");
  strcpy(In.StatusAndTradeType.status_submitted, "SBMT");
  strcpy(In.StatusAndTradeType.type_limit_buy, "TLB");
  strcpy(In.StatusAndTradeType.type_limit_sell, "TLS");
  strcpy(In.StatusAndTradeType.type_stop_loss, "TSL");
  strcpy(In.zz_padding2, "");
  In.Entries[0].price_quote = 32.0; //99.9;
  In.Entries[0].trade_qty = 123;
  strcpy(In.Entries[0].symbol, "A");

  CMarketFeedDBInterface *pDb = pDbc->MarketFeedDB();
  CSendToMarket hS2M;

  CMarketFeed hDrv(pDb, &hS2M);
  hDrv.DoTxn(&In, &Out);

#ifdef _TRACE
  cout << "result=" << Out << endl;
#endif
}

void doMarketWatch(CDBConnectionInterface *pDbc) {

  TMarketWatchTxnInput In;
  memset(&In, 0x00, sizeof(In));
  TMarketWatchTxnOutput Out;
  memset(&Out, 0x00, sizeof(Out));

// 1
//In.c_id = 4300000002;
// 2
//strcpy(In.industry_name, "Electronic Instruments & Controls");
//In.starting_co_id = 4300000001;
//In.ending_co_id   = 4300000050;
// 3
  In.acct_id = 43000000004;

  In.start_day.year = 2004;
  In.start_day.month = 12;
  In.start_day.day = 31;

  CMarketWatchDBInterface *pDb = pDbc->MarketWatchDB();

  CMarketWatch hDrv(pDb);
  hDrv.DoTxn(&In, &Out);

#ifdef _TRACE
  cout << "result=" << Out << endl;
#endif
}

void doSecurityDetail(CDBConnectionInterface *pDbc) {

  TSecurityDetailTxnInput In;
  memset(&In, 0x00, sizeof(In));
  TSecurityDetailTxnOutput Out;
  memset(&Out, 0x00, sizeof(Out));

  strcpy(In.symbol, "A");
  In.start_day.year = 2004;
  In.start_day.month = 10;
  In.start_day.day = 1;
  In.max_rows_to_return = 5;
//In.access_lob_flag = 1;

  CSecurityDetailDBInterface *pDb = pDbc->SecurityDetailDB();

  CSecurityDetail hDrv(pDb);
  hDrv.DoTxn(&In, &Out);

#ifdef _TRACE
  cout << "result=" << Out << endl;
#endif
}

void doTradeCleanup(CDBConnectionInterface *pDbc) {

  TTradeCleanupTxnInput In;
  memset(&In, 0x00, sizeof(In));
  TTradeCleanupTxnOutput Out;
  memset(&Out, 0x00, sizeof(Out));

  In.start_trade_id = 100000000000000;
  strcpy(In.st_canceled_id, "CNCL");
  strcpy(In.st_pending_id, "PNDG");
  strcpy(In.st_submitted_id, "SBMT");

  CTradeCleanupDBInterface *pDb = pDbc->TradeCleanupDB();

  CTradeCleanup hDrv(pDb);
  hDrv.DoTxn(&In, &Out);

#ifdef _TRACE
  cout << "result=" << Out << endl;
#endif
}

void doTradeLookup(CDBConnectionInterface *pDbc) {

  TTradeLookupTxnInput In;
  memset(&In, 0x00, sizeof(In));
  TTradeLookupTxnOutput Out;
  memset(&Out, 0x00, sizeof(Out));

  In.frame_to_execute = 1; // 1,2,3,4
  In.trade_id[0] = 200000000000010; // 1
  In.trade_id[1] = 2;
  In.trade_id[2] = 200000000000003;
  In.max_trades = 3;
  In.acct_id = 43000000011;
  In.max_acct_id = 43000000100;
  In.start_trade_dts.year = 2005;
  In.start_trade_dts.month = 1;
  In.start_trade_dts.day = 3;
  In.end_trade_dts.year = 2010;
  In.end_trade_dts.month = 10;
  In.end_trade_dts.day = 31;
  strncpy(In.symbol, "A", sizeof(In.symbol)-1);

  CTradeLookupDBInterface *pDb = pDbc->TradeLookupDB();

  CTradeLookup hDrv(pDb);
  hDrv.DoTxn(&In, &Out);

#ifdef _TRACE
  cout << "result=" << Out << endl;
#endif
}

void doTradeOrder(CDBConnectionInterface *pDbc) {

  TTradeOrderTxnInput In;
  memset(&In, 0x00, sizeof(In));
  TTradeOrderTxnOutput Out;
  memset(&Out, 0x00, sizeof(Out));

  In.requested_price = 32.1;
  In.acct_id = 43000000015;
  In.is_lifo = 0;
  In.roll_it_back = 0;
  In.trade_qty = 16;
  In.type_is_margin = 0;
  strncpy(In.co_name, "Agilent Technologies, Inc.", sizeof(In.co_name)-1);
  strncpy(In.exec_f_name, "Willie ", sizeof(In.exec_f_name)-1); // SPACE!
  strncpy(In.exec_l_name, "Swigert", sizeof(In.exec_l_name)-1);
  strncpy(In.exec_tax_id, "803MO6290MK444", sizeof(In.exec_tax_id)-1);
  strncpy(In.issue, "COMMON", sizeof(In.issue)-1);
  strncpy(In.st_pending_id, "PNDG", sizeof(In.st_pending_id)-1);
  strncpy(In.st_submitted_id, "SBMT", sizeof(In.st_submitted_id)-1);
//strncpy(In.symbol, "A", sizeof(In.symbol)-1);
  strncpy(In.trade_type_id, "TLB", sizeof(In.trade_type_id)-1); // TLB,TLS,TMB,TMS,TSL

  CTradeOrderDBInterface *pDb = pDbc->TradeOrderDB();
  CSendToMarket hS2M;

  CTradeOrder hDrv(pDb, &hS2M);
  hDrv.DoTxn(&In, &Out);

#ifdef _TRACE
  cout << "result=" << Out << endl;
#endif
}

void doTradeResult(CDBConnectionInterface *pDbc) {

  TTradeResultTxnInput In;
  memset(&In, 0x00, sizeof(In));
  TTradeResultTxnOutput Out;
  memset(&Out, 0x00, sizeof(Out));

  In.trade_id = 2; 
  In.trade_price = 99;

  CTradeResultDBInterface *pDb = pDbc->TradeResultDB();

  CTradeResult hDrv(pDb);
  hDrv.DoTxn(&In, &Out);

  #ifdef _TRACE
    cout << "result=" << Out << endl;
  #endif
}

void doTradeStatus(CDBConnectionInterface *pDbc) {

  TTradeStatusTxnInput In;
  memset(&In, 0x00, sizeof(In));
  TTradeStatusTxnOutput Out;
  memset(&Out, 0x00, sizeof(Out));

  In.acct_id = 43000000011;

  CTradeStatusDBInterface *pDb = pDbc->TradeStatusDB();

  CTradeStatus hDrv(pDb);
  hDrv.DoTxn(&In, &Out);

#ifdef _TRACE
  cout << "result=" << Out << endl;
#endif
}

void doTradeUpdate(CDBConnectionInterface *pDbc) {

  TTradeUpdateTxnInput In;
  memset(&In, 0x00, sizeof(In));
  TTradeUpdateTxnOutput Out;
  memset(&Out, 0x00, sizeof(Out));

  In.frame_to_execute = 3; // 1,2,3
  In.trade_id[0] = 200000000000010;
  In.trade_id[1] = 2;
  In.trade_id[2] = 200000000000003;
  In.max_trades = 3;
  In.acct_id = 43000000011;
  In.max_acct_id = 43000000100;
  In.start_trade_dts.year = 2004;
  In.start_trade_dts.month = 10;
  In.start_trade_dts.day = 1;
  In.end_trade_dts.year = 2010;
  In.end_trade_dts.month = 10;
  In.end_trade_dts.day = 31;
  strncpy(In.symbol, "A", sizeof(In.symbol)-1);
  In.max_updates = 3; // == max_trades

  CTradeUpdateDBInterface *pDb = pDbc->TradeUpdateDB();

  CTradeUpdate hDrv(pDb);
  hDrv.DoTxn(&In, &Out);

#ifdef _TRACE
  cout << "result=" << Out << endl;
#endif
}

int main(int, const char *[]) {

  const char s[] =
    "dsn=myodbc;server=localhost;socket=/tmp/mysql.sock;user=tpce;pwd=tpce;database=tpce";

  CDBDriverMyODBC driver;

  driver.SetConfig("connectString", s, sizeof(s));

  CDBConnectionInterface *pDbc = driver.GetDBConnection();

  if (NULL != pDbc) {
    doBrokerVolume(pDbc);
//  doCustomerPosition(pDbc);
//  doDataMaintenance(pDbc);
//  doMarketFeed(pDbc);
//  doMarketWatch(pDbc);
//  doSecurityDetail(pDbc);
//  doTradeCleanup(pDbc);
//  doTradeLookup(pDbc);
//  doTradeOrder(pDbc);
//  doTradeResult(pDbc);
//  doTradeStatus(pDbc);
//  doTradeUpdate(pDbc);
  }

  return 0;
}
