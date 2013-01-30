/*
 */

#ifndef TRADE_HISTORY_STMT_H
#define TRADE_HISTORY_STMT_H

#include "HandleStmt.H"

#include "TxnHarnessStructs.h" // cTH_ST_ID_len

namespace TPCE {

  class TTradeLookupFrame1TradeInfo;
  class TTradeLookupFrame2TradeInfo;
  class TTradeLookupFrame3TradeInfo;

  class TTradeUpdateFrame1TradeInfo;
  class TTradeUpdateFrame2TradeInfo;
  class TTradeUpdateFrame3TradeInfo;

  /*
   */
  class CTradeHistoryStmt : public CHandleStmt {

  private:
    TIMESTAMP_STRUCT trade_history_dts;
    char trade_history_status_id[cTH_ST_ID_len+1];
    DB_INDICATOR trade_history_dts_ind;
    DB_INDICATOR trade_history_status_id_ind;

    int index;
    TIMESTAMP_STRUCT *trade_history_dts_returned[TradeLookupMaxTradeHistoryRowsReturned];
    char *trade_history_status_id_returned[TradeLookupMaxTradeHistoryRowsReturned];
    DB_INDICATOR *trade_history_dts_ind_returned[TradeLookupMaxTradeHistoryRowsReturned];
    DB_INDICATOR *trade_history_st_id_returned[TradeLookupMaxTradeHistoryRowsReturned];

  protected:
    CTradeHistoryStmt *BindCol();

  public:
    CTradeHistoryStmt(CHandleDbc *pDbc);
    virtual ~CTradeHistoryStmt();

    CTradeHistoryStmt *BindCol(TTradeLookupFrame1TradeInfo *trade_info);
    CTradeHistoryStmt *BindCol(TTradeLookupFrame2TradeInfo *trade_info);
    CTradeHistoryStmt *BindCol(TTradeLookupFrame3TradeInfo *trade_info);

    CTradeHistoryStmt *BindCol(TTradeUpdateFrame1TradeInfo *trade_info);
    CTradeHistoryStmt *BindCol(TTradeUpdateFrame2TradeInfo *trade_info);
    CTradeHistoryStmt *BindCol(TTradeUpdateFrame3TradeInfo *trade_info);

    CTradeHistoryStmt *BindParameter(const TTrade *trade_id);

    bool Execute();
    bool Fetch();

  };

}

#endif // TRADE_HISTORY_STMT_H
