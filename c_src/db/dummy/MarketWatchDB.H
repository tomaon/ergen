#ifndef MARKET_WATCH_DB_H
#define	MARKET_WATCH_DB_H

#include "TxnHarnessDBInterface.h"

namespace TPCE {

  class CMarketWatchDB : public CMarketWatchDBInterface {
  public:
    CMarketWatchDB();
    virtual ~CMarketWatchDB();

    virtual void DoMarketWatchFrame1(const TMarketWatchFrame1Input *pIn, TMarketWatchFrame1Output *pOut);
  };
}

#endif
