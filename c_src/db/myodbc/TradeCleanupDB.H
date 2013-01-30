/*
 */

#ifndef TRADE_CLEANUP_DB_H
#define	TRADE_CLEANUP_DB_H

#include "ERGenMYODBC.H"

namespace TPCE {

  class CHandleDbc;
  class CHandleStmt;

  /*
   */
  class CTradeCleanupDB : public CTradeCleanupDBInterface {

  private:
    CHandleDbc *m_pDbc;
    CHandleStmt *m_pStmt[6];

  public:
    CTradeCleanupDB(CHandleDbc *pDbc);
    virtual ~CTradeCleanupDB();

    virtual void DoTradeCleanupFrame1(const TTradeCleanupFrame1Input *pIn);
  };
}

#endif // TRADE_CLEANUP_DB_H
