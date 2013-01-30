/*
 */

#ifndef TRADE_RESULT_DB_H
#define	TRADE_RESULT_DB_H

#include "ERGenMYODBC.H"

namespace TPCE {

  class CHandleDbc;
  class CHandleStmt;

  /*
   */
  class CTradeResultDB : public CTradeResultDBInterface {

  private:
    CHandleDbc *m_pDbc;
    CHandleStmt *m_pStmt[24];

  public:
    CTradeResultDB(CHandleDbc *pDBc);
    virtual ~CTradeResultDB();

    virtual void DoTradeResultFrame1(const TTradeResultFrame1Input *pIn, TTradeResultFrame1Output *pOut);
    virtual void DoTradeResultFrame2(const TTradeResultFrame2Input *pIn, TTradeResultFrame2Output *pOut);
    virtual void DoTradeResultFrame3(const TTradeResultFrame3Input *pIn, TTradeResultFrame3Output *pOut);
    virtual void DoTradeResultFrame4(const TTradeResultFrame4Input *pIn, TTradeResultFrame4Output *pOut);
    virtual void DoTradeResultFrame5(const TTradeResultFrame5Input *pIn);
    virtual void DoTradeResultFrame6(const TTradeResultFrame6Input *pIn, TTradeResultFrame6Output *pOut);
  };
}

#endif // TRADE_RESULT_DB_H
