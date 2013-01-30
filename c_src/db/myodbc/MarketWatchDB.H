/*
 */

#ifndef MARKET_WATCH_DB_H
#define	MARKET_WATCH_DB_H

#include "ERGenMYODBC.H"

namespace TPCE {

  class CHandleDbc;
  class CHandleStmt;

  /*
   */
  class CMarketWatchDB : public CMarketWatchDBInterface {

  private:
    CHandleDbc *m_pDbc;
    CHandleStmt *m_pStmt[3];

  public:
    CMarketWatchDB(CHandleDbc *pDbc);
    virtual ~CMarketWatchDB();

    virtual void DoMarketWatchFrame1(const TMarketWatchFrame1Input *pIn, TMarketWatchFrame1Output *pOut);
  };
}

#endif // MARKET_WATCH_DB_H
