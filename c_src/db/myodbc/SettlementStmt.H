/*
 */

#ifndef SETTLEMENT_STMT_H
#define SETTLEMENT_STMT_H

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
  class CSettlementStmt : public CHandleStmt {

  public:
    CSettlementStmt(CHandleDbc *pDbc);
    virtual ~CSettlementStmt();

    virtual CSettlementStmt *BindCol(TTradeLookupFrame1TradeInfo *trade_info);
    virtual CSettlementStmt *BindCol(TTradeLookupFrame2TradeInfo *trade_info);
    virtual CSettlementStmt *BindCol(TTradeLookupFrame3TradeInfo *trade_info);

    virtual CSettlementStmt *BindCol(TTradeUpdateFrame1TradeInfo *trade_info);
    virtual CSettlementStmt *BindCol(TTradeUpdateFrame2TradeInfo *trade_info);
    virtual CSettlementStmt *BindCol(TTradeUpdateFrame3TradeInfo *trade_info);

    virtual CSettlementStmt *BindParameter(const TTrade *trade_id);
  };

}

#endif // SETTLEMENT_STMT_H
