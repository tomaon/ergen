/*
 */

#ifndef TRADE_STATUS_DB_H
#define	TRADE_STATUS_DB_H

#include "ERGenMYODBC.H"

namespace TPCE {

  class CHandleDbc;
  class CHandleStmt;

  /*
   */
  class CTradeStatusDB : public CTradeStatusDBInterface {

  private:
    CHandleDbc *m_pDbc;
    CHandleStmt *m_pStmt[2];

  public:
    CTradeStatusDB(CHandleDbc *pDbc);
    virtual ~CTradeStatusDB();

    virtual void DoTradeStatusFrame1(const TTradeStatusFrame1Input *pIn, TTradeStatusFrame1Output *pOut);
  };

}

#endif // TRADE_STATUS_DB_H
