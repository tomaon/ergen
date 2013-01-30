/*
 */

#include "CashTransactionStmt.H"

#include "TxnHarnessStructs.h"

namespace TPCE {

  /*
   */
  CCashTransactionStmt::CCashTransactionStmt(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

    const char *statement =
      "SELECT"
      "  ct_amt AS 'cash_transaction_amount'"
      ", ct_dts AS 'cash_transaction_dts'"
      ", ct_name AS 'cash_transaction_name'" // nullable
      "  FROM cash_transaction"
      " WHERE ct_t_id = ?";

    CHandleStmt::Prepare((SQLCHAR *)statement);
  }

  /*
   */
  CCashTransactionStmt::~CCashTransactionStmt() {
  }

  /*
   */
  CCashTransactionStmt *CCashTransactionStmt::BindCol(TTradeLookupFrame1TradeInfo *trade_info) {

    (*((CHandleStmt *)this))
      .BindCol(1,
               &trade_info->cash_transaction_amount,
               sizeof(trade_info->cash_transaction_amount),
               &trade_info->cash_transaction_amount_ind)
      .BindCol(2,
               &trade_info->cash_transaction_dts,
               sizeof(trade_info->cash_transaction_dts),
               &trade_info->cash_transaction_dts_ind)
      .BindCol(3,
               trade_info->cash_transaction_name,
               sizeof(trade_info->cash_transaction_name),
               &trade_info->cash_transaction_name_ind);

    return this;
  }
  CCashTransactionStmt *CCashTransactionStmt::BindCol(TTradeLookupFrame2TradeInfo *trade_info) {

    (*((CHandleStmt *)this))
      .BindCol(1,
               &trade_info->cash_transaction_amount,
               sizeof(trade_info->cash_transaction_amount),
               &trade_info->cash_transaction_amount_ind)
      .BindCol(2,
               &trade_info->cash_transaction_dts,
               sizeof(trade_info->cash_transaction_dts),
               &trade_info->cash_transaction_dts_ind)
      .BindCol(3,
               trade_info->cash_transaction_name,
               sizeof(trade_info->cash_transaction_name),
               &trade_info->cash_transaction_name_ind);

    return this;
  }
  CCashTransactionStmt *CCashTransactionStmt::BindCol(TTradeLookupFrame3TradeInfo *trade_info) {

    (*((CHandleStmt *)this))
      .BindCol(1,
               &trade_info->cash_transaction_amount,
               sizeof(trade_info->cash_transaction_amount),
               &trade_info->cash_transaction_amount_ind)
      .BindCol(2,
               &trade_info->cash_transaction_dts,
               sizeof(trade_info->cash_transaction_dts),
               &trade_info->cash_transaction_dts_ind)
      .BindCol(3,
               trade_info->cash_transaction_name,
               sizeof(trade_info->cash_transaction_name),
               &trade_info->cash_transaction_name_ind);

    return this;
  }

  CCashTransactionStmt *CCashTransactionStmt::BindCol(TTradeUpdateFrame1TradeInfo *trade_info) {

    (*((CHandleStmt *)this))
      .BindCol(1,
               &trade_info->cash_transaction_amount,
               sizeof(trade_info->cash_transaction_amount),
               &trade_info->cash_transaction_amount_ind)
      .BindCol(2,
               &trade_info->cash_transaction_dts,
               sizeof(trade_info->cash_transaction_dts),
               &trade_info->cash_transaction_dts_ind)
      .BindCol(3,
               trade_info->cash_transaction_name,
               sizeof(trade_info->cash_transaction_name),
               &trade_info->cash_transaction_name_ind);

    return this;
  }
  CCashTransactionStmt *CCashTransactionStmt::BindCol(TTradeUpdateFrame2TradeInfo *trade_info) {

    (*((CHandleStmt *)this))
      .BindCol(1,
               &trade_info->cash_transaction_amount,
               sizeof(trade_info->cash_transaction_amount),
               &trade_info->cash_transaction_amount_ind)
      .BindCol(2,
               &trade_info->cash_transaction_dts,
               sizeof(trade_info->cash_transaction_dts),
               &trade_info->cash_transaction_dts_ind)
      .BindCol(3,
               trade_info->cash_transaction_name,
               sizeof(trade_info->cash_transaction_name),
               &trade_info->cash_transaction_name_ind);

    return this;
  }
  CCashTransactionStmt *CCashTransactionStmt::BindCol(TTradeUpdateFrame3TradeInfo *trade_info) {

    (*((CHandleStmt *)this))
      .BindCol(1,
               &trade_info->cash_transaction_amount,
               sizeof(trade_info->cash_transaction_amount),
               &trade_info->cash_transaction_amount_ind)
      .BindCol(2,
               &trade_info->cash_transaction_dts,
               sizeof(trade_info->cash_transaction_dts),
               &trade_info->cash_transaction_dts_ind)
      .BindCol(3,
               trade_info->cash_transaction_name,
               sizeof(trade_info->cash_transaction_name),
               &trade_info->cash_transaction_name_ind);

    return this;
  }

  /*
   */
  CCashTransactionStmt *CCashTransactionStmt::BindParameter(const TTrade *trade_id) {

    (*((CHandleStmt *)this))
      .BindParameter(1, trade_id, sizeof(TTrade));

    return this;
  }

}
