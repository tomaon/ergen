/*
 */

#include "SettlementStmt.H"

#include "TxnHarnessStructs.h"

namespace TPCE {

  /*
   */
  CSettlementStmt::CSettlementStmt(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

    const char *statement =
      "SELECT"
      "  se_amt AS 'settlement_amount'"
      ", CAST(se_cash_due_date AS DATETIME) AS 'settlement_cash_due_date'"
      ", se_cash_type AS 'settlement_cash_type'"
      "  FROM settlement"
      " WHERE se_t_id = ?";

    CHandleStmt::Prepare((SQLCHAR *)statement);
  }

  /*
   */
  CSettlementStmt::~CSettlementStmt() {
  }

  /*
   */
  CSettlementStmt *CSettlementStmt::BindCol(TTradeLookupFrame1TradeInfo *trade_info) {

    (*((CHandleStmt *)this))
      .BindCol(1,
               &trade_info->settlement_amount,
               sizeof(trade_info->settlement_amount),
               &trade_info->settlement_amount_ind)
      .BindCol(2,
               &trade_info->settlement_cash_due_date,
               sizeof(trade_info->settlement_cash_due_date),
               &trade_info->settlement_cash_due_date_ind)
      .BindCol(3,
               trade_info->settlement_cash_type,
               sizeof(trade_info->settlement_cash_type),
               &trade_info->settlement_cash_type_ind);

    return this;
  }
  CSettlementStmt *CSettlementStmt::BindCol(TTradeLookupFrame2TradeInfo *trade_info) {

    (*((CHandleStmt *)this))
      .BindCol(1,
               &trade_info->settlement_amount,
               sizeof(trade_info->settlement_amount),
               &trade_info->settlement_amount_ind)
      .BindCol(2,
               &trade_info->settlement_cash_due_date,
               sizeof(trade_info->settlement_cash_due_date),
               &trade_info->settlement_cash_due_date_ind)
      .BindCol(3,
               trade_info->settlement_cash_type,
               sizeof(trade_info->settlement_cash_type),
               &trade_info->settlement_cash_type_ind);

    return this;
  }
  CSettlementStmt *CSettlementStmt::BindCol(TTradeLookupFrame3TradeInfo *trade_info) {

    (*((CHandleStmt *)this))
      .BindCol(1,
               &trade_info->settlement_amount,
               sizeof(trade_info->settlement_amount),
               &trade_info->settlement_amount_ind)
      .BindCol(2,
               &trade_info->settlement_cash_due_date,
               sizeof(trade_info->settlement_cash_due_date),
               &trade_info->settlement_cash_due_date_ind)
      .BindCol(3,
               trade_info->settlement_cash_type,
               sizeof(trade_info->settlement_cash_type),
               &trade_info->settlement_cash_type_ind);

    return this;
  }
  CSettlementStmt *CSettlementStmt::BindCol(TTradeUpdateFrame1TradeInfo *trade_info) {

    (*((CHandleStmt *)this))
      .BindCol(1,
               &trade_info->settlement_amount,
               sizeof(trade_info->settlement_amount),
               &trade_info->settlement_amount_ind)
      .BindCol(2,
               &trade_info->settlement_cash_due_date,
               sizeof(trade_info->settlement_cash_due_date),
               &trade_info->settlement_cash_due_date_ind)
      .BindCol(3,
               trade_info->settlement_cash_type,
               sizeof(trade_info->settlement_cash_type),
               &trade_info->settlement_cash_type_ind);

    return this;
  }
  CSettlementStmt *CSettlementStmt::BindCol(TTradeUpdateFrame2TradeInfo *trade_info) {

    (*((CHandleStmt *)this))
      .BindCol(1,
               &trade_info->settlement_amount,
               sizeof(trade_info->settlement_amount),
               &trade_info->settlement_amount_ind)
      .BindCol(2,
               &trade_info->settlement_cash_due_date,
               sizeof(trade_info->settlement_cash_due_date),
               &trade_info->settlement_cash_due_date_ind)
      .BindCol(3,
               trade_info->settlement_cash_type,
               sizeof(trade_info->settlement_cash_type),
               &trade_info->settlement_cash_type_ind);

    return this;
  }
  CSettlementStmt *CSettlementStmt::BindCol(TTradeUpdateFrame3TradeInfo *trade_info) {

    (*((CHandleStmt *)this))
      .BindCol(1,
               &trade_info->settlement_amount,
               sizeof(trade_info->settlement_amount),
               &trade_info->settlement_amount_ind)
      .BindCol(2,
               &trade_info->settlement_cash_due_date,
               sizeof(trade_info->settlement_cash_due_date),
               &trade_info->settlement_cash_due_date_ind)
      .BindCol(3,
               trade_info->settlement_cash_type,
               sizeof(trade_info->settlement_cash_type),
               &trade_info->settlement_cash_type_ind);

    return this;
  }

  /*
   */
  CSettlementStmt *CSettlementStmt::BindParameter(const TTrade *trade_id) {

    (*((CHandleStmt *)this))
      .BindParameter(1, trade_id, sizeof(TTrade));

    return this;
  }

}
