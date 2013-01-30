#ifndef TRADE_STATUS_DB_H
#define	TRADE_STATUS_DB_H

#include "TxnHarnessDBInterface.h"

namespace TPCE {

  class CTradeStatusDB : public CTradeStatusDBInterface {
  public:
    CTradeStatusDB();
    virtual ~CTradeStatusDB();

    virtual void DoTradeStatusFrame1(const TTradeStatusFrame1Input *pIn, TTradeStatusFrame1Output *pOut);
  };

}

#endif
