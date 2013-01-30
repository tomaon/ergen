/*
 */

#ifndef TRADE_UPDATE_DB_H
#define	TRADE_UPDATE_DB_H

#include "ERGenMYODBC.H"

namespace TPCE {

  class CHandleDbc;
  class CHandleStmt;

  /*
   */
  class CTradeUpdateDB : public CTradeUpdateDBInterface {

  private:
    CHandleDbc *m_pDbc;
    CHandleStmt *m_pStmt[20];

  public:
    CTradeUpdateDB(CHandleDbc *pDbc);
    virtual ~CTradeUpdateDB();

    virtual void DoTradeUpdateFrame1(const TTradeUpdateFrame1Input *pIn, TTradeUpdateFrame1Output *pOut);
    virtual void DoTradeUpdateFrame2(const TTradeUpdateFrame2Input *pIn, TTradeUpdateFrame2Output *pOut);
    virtual void DoTradeUpdateFrame3(const TTradeUpdateFrame3Input *pIn, TTradeUpdateFrame3Output *pOut);
  };
}

#endif // TRADE_UPDATE_DB_H
