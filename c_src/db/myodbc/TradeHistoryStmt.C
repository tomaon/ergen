/*
 */

#include "TradeHistoryStmt.H"

#include <sstream>

namespace TPCE {

  /*
   */
  CTradeHistoryStmt::CTradeHistoryStmt(CHandleDbc *pDbc) : CHandleStmt(pDbc), index(0) {

    std::ostringstream oss;
    oss << "SELECT";
    oss << "  th_dts AS 'trade_history_dts'";
    oss << ", th_st_id AS 'trade_history_status_id'";
    oss << "  FROM trade_history";
    oss << " WHERE th_t_id = ?";
    oss << " ORDER BY";
    oss << "  th_dts";
    oss << " LIMIT " << TradeLookupMaxTradeHistoryRowsReturned;

    CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
  }

  /*
   */
  CTradeHistoryStmt::~CTradeHistoryStmt() {
  }

  /*
   */
  CTradeHistoryStmt *CTradeHistoryStmt::BindCol() {

    (*((CHandleStmt *)this))
      .BindCol(1,
               &trade_history_dts,
               sizeof(TIMESTAMP_STRUCT),
               &trade_history_dts_ind)
      .BindCol(2,
               trade_history_status_id,
               sizeof(trade_history_status_id),
               &trade_history_status_id_ind);

    return this;
  }

  /*
   */
  CTradeHistoryStmt *CTradeHistoryStmt::BindCol(TTradeLookupFrame1TradeInfo *trade_info) {

    for (int i = 0; i < TradeLookupMaxTradeHistoryRowsReturned; i++) {
      trade_history_dts_returned[i] = &trade_info->trade_history_dts[i];
      trade_history_status_id_returned[i] = trade_info->trade_history_status_id[i];
      trade_history_dts_ind_returned[i] = &trade_info->trade_history_dts_ind[i];
      trade_history_st_id_returned[i] = &trade_info->trade_history_status_id_ind[i];
    }

    return BindCol();
  }
  CTradeHistoryStmt *CTradeHistoryStmt::BindCol(TTradeLookupFrame2TradeInfo *trade_info) {

    for (int i = 0; i < TradeLookupMaxTradeHistoryRowsReturned; i++) {
      trade_history_dts_returned[i] = &trade_info->trade_history_dts[i];
      trade_history_status_id_returned[i] = trade_info->trade_history_status_id[i];
      trade_history_dts_ind_returned[i] = &trade_info->trade_history_dts_ind[i];
      trade_history_st_id_returned[i] = &trade_info->trade_history_status_id_ind[i];
    }

    return BindCol();
  }
  CTradeHistoryStmt *CTradeHistoryStmt::BindCol(TTradeLookupFrame3TradeInfo *trade_info) {

    for (int i = 0; i < TradeLookupMaxTradeHistoryRowsReturned; i++) {
      trade_history_dts_returned[i] = &trade_info->trade_history_dts[i];
      trade_history_status_id_returned[i] = trade_info->trade_history_status_id[i];
      trade_history_dts_ind_returned[i] = &trade_info->trade_history_dts_ind[i];
      trade_history_st_id_returned[i] = &trade_info->trade_history_status_id_ind[i];
    }

    return BindCol();
  }
  CTradeHistoryStmt *CTradeHistoryStmt::BindCol(TTradeUpdateFrame1TradeInfo *trade_info) {

    for (int i = 0; i < TradeLookupMaxTradeHistoryRowsReturned; i++) {
      trade_history_dts_returned[i] = &trade_info->trade_history_dts[i];
      trade_history_status_id_returned[i] = trade_info->trade_history_status_id[i];
      trade_history_dts_ind_returned[i] = &trade_info->trade_history_dts_ind[i];
      trade_history_st_id_returned[i] = &trade_info->trade_history_status_id_ind[i];
    }

    return BindCol();
  }
  CTradeHistoryStmt *CTradeHistoryStmt::BindCol(TTradeUpdateFrame2TradeInfo *trade_info) {

    for (int i = 0; i < TradeLookupMaxTradeHistoryRowsReturned; i++) {
      trade_history_dts_returned[i] = &trade_info->trade_history_dts[i];
      trade_history_status_id_returned[i] = trade_info->trade_history_status_id[i];
      trade_history_dts_ind_returned[i] = &trade_info->trade_history_dts_ind[i];
      trade_history_st_id_returned[i] = &trade_info->trade_history_status_id_ind[i];
    }

    return BindCol();
  }
  CTradeHistoryStmt *CTradeHistoryStmt::BindCol(TTradeUpdateFrame3TradeInfo *trade_info) {

    for (int i = 0; i < TradeLookupMaxTradeHistoryRowsReturned; i++) {
      trade_history_dts_returned[i] = &trade_info->trade_history_dts[i];
      trade_history_status_id_returned[i] = trade_info->trade_history_status_id[i];
      trade_history_dts_ind_returned[i] = &trade_info->trade_history_dts_ind[i];
      trade_history_st_id_returned[i] = &trade_info->trade_history_status_id_ind[i];
    }

    return BindCol();
  }

  /*
   */
  CTradeHistoryStmt *CTradeHistoryStmt::BindParameter(const TTrade *trade_id) {

    (*((CHandleStmt *)this))
      .BindParameter(1, trade_id, sizeof(TTrade));

    return this;
  }

  /*
   */
  bool CTradeHistoryStmt::Execute() {

    index = 0;

    return CHandleStmt::Execute();
  }

  /*
   */
  bool CTradeHistoryStmt::Fetch() {

    memset(&trade_history_dts, 0x00, sizeof(trade_history_dts));
    memset(&trade_history_status_id, 0x00, sizeof(trade_history_status_id));
    trade_history_dts_ind = 0;
    trade_history_status_id_ind = 0;

    bool result = CHandleStmt::Fetch();

    if (result) {

      memcpy(trade_history_dts_returned[index],
             &trade_history_dts, sizeof(trade_history_dts));
      memcpy(trade_history_status_id_returned[index],
             trade_history_status_id, sizeof(trade_history_status_id));
      *trade_history_dts_ind_returned[index] = trade_history_dts_ind;
      *trade_history_st_id_returned[index] = trade_history_status_id_ind;

      index++;
    }

    return result;
  }

}
