#ifndef TRADE_LOOKUP_DB_H
#define	TRADE_LOOKUP_DB_H

#include "TxnHarnessDBInterface.h"

namespace TPCE {

  class CTradeLookupDB : public CTradeLookupDBInterface {
  public:
    CTradeLookupDB();
    virtual ~CTradeLookupDB();

    virtual void DoTradeLookupFrame1(const TTradeLookupFrame1Input *pIn, TTradeLookupFrame1Output *pOut);
    virtual void DoTradeLookupFrame2(const TTradeLookupFrame2Input *pIn, TTradeLookupFrame2Output *pOut);
    virtual void DoTradeLookupFrame3(const TTradeLookupFrame3Input *pIn, TTradeLookupFrame3Output *pOut);
    virtual void DoTradeLookupFrame4(const TTradeLookupFrame4Input *pIn, TTradeLookupFrame4Output *pOut);
  };
}

#endif
