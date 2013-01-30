/*
 */

#ifndef MARKET_FEED_DB_H
#define	MARKET_FEED_DB_H

#include "ERGenMYODBC.H"

namespace TPCE {

  class CHandleDbc;
  class CHandleStmt;

  /*
   */
  class CMarketFeedDB : public CMarketFeedDBInterface {

  private:
    CHandleDbc *m_pDbc;
    CHandleStmt *m_pStmt[5];

  public:
    CMarketFeedDB(CHandleDbc *pDbc);
    virtual ~CMarketFeedDB();

    virtual void DoMarketFeedFrame1(const TMarketFeedFrame1Input *pIn, TMarketFeedFrame1Output *pOut, CSendToMarketInterface *pSendToMarket);
  };
}

#endif // MARKET_FEED_DB_H
