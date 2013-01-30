/*
 */

#ifndef TRADE_ORDER_DB_H
#define	TRADE_ORDER_DB_H

#include "ERGenMYODBC.H"
#include "Sequence.H"

namespace TPCE {

  class CHandleDbc;
  class CHandleStmt;

  /*
   */
  class CTradeOrderDB : public CTradeOrderDBInterface {

  private:
    CHandleDbc *m_pDbc;
    CHandleStmt *m_pStmt[18];
    CSequence m_sequence;

  public:
    CTradeOrderDB(CHandleDbc *pDbc);
    virtual ~CTradeOrderDB();

    virtual void DoTradeOrderFrame1(const TTradeOrderFrame1Input *pIn, TTradeOrderFrame1Output *pOut);
    virtual void DoTradeOrderFrame2(const TTradeOrderFrame2Input *pIn, TTradeOrderFrame2Output *pOut);
    virtual void DoTradeOrderFrame3(const TTradeOrderFrame3Input *pIn, TTradeOrderFrame3Output *pOut);
    virtual void DoTradeOrderFrame4(const TTradeOrderFrame4Input *pIn, TTradeOrderFrame4Output *pOut);
    virtual void DoTradeOrderFrame5(void);
    virtual void DoTradeOrderFrame6(void);
  };
}

#endif // TRADE_ORDER_DB_H
