#ifndef MARKET_FEED_DB_H
#define	MARKET_FEED_DB_H

#include "TxnHarnessDBInterface.h"

namespace TPCE {

  class CMarketFeedDB : public CMarketFeedDBInterface {
  public:
    CMarketFeedDB();
    virtual ~CMarketFeedDB();

    virtual void DoMarketFeedFrame1(const TMarketFeedFrame1Input *pIn, TMarketFeedFrame1Output *pOut, CSendToMarketInterface *pSendToMarket);
  };
}

#endif
