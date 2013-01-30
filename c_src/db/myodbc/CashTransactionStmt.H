/*
 */

#ifndef CASE_TRANSACTION_STMT_H
#define CASE_TRANSACTION_STMT_H

#include "HandleStmt.H"

namespace TPCE {

  class TTradeLookupFrame1TradeInfo;
  class TTradeLookupFrame2TradeInfo;
  class TTradeLookupFrame3TradeInfo;

  class TTradeUpdateFrame1TradeInfo;
  class TTradeUpdateFrame2TradeInfo;
  class TTradeUpdateFrame3TradeInfo;

  /*
   */
  class CCashTransactionStmt : public CHandleStmt {

  public:
    CCashTransactionStmt(CHandleDbc *pDbc);
    virtual ~CCashTransactionStmt();

    virtual CCashTransactionStmt *BindCol(TTradeLookupFrame1TradeInfo *trade_info);
    virtual CCashTransactionStmt *BindCol(TTradeLookupFrame2TradeInfo *trade_info);
    virtual CCashTransactionStmt *BindCol(TTradeLookupFrame3TradeInfo *trade_info);

    virtual CCashTransactionStmt *BindCol(TTradeUpdateFrame1TradeInfo *trade_info);
    virtual CCashTransactionStmt *BindCol(TTradeUpdateFrame2TradeInfo *trade_info);
    virtual CCashTransactionStmt *BindCol(TTradeUpdateFrame3TradeInfo *trade_info);

    virtual CCashTransactionStmt *BindParameter(const TTrade *trade_id);
  };

}

#endif // CASE_TRANSACTION_STMT_H
