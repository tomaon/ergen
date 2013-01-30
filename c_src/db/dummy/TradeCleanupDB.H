#ifndef TRADE_CLEANUP_DB_H
#define	TRADE_CLEANUP_DB_H

#include "TxnHarnessDBInterface.h"

namespace TPCE {

  class CTradeCleanupDB : public CTradeCleanupDBInterface {
  public:
    CTradeCleanupDB();
    virtual ~CTradeCleanupDB();

    virtual void DoTradeCleanupFrame1(const TTradeCleanupFrame1Input *pIn);
  };
}

#endif
