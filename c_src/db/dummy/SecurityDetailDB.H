#ifndef SECURITY_DETAIL_DB_H
#define	SECURITY_DETAIL_DB_H

#include "TxnHarnessDBInterface.h"

namespace TPCE {

  class CSecurityDetailDB : public CSecurityDetailDBInterface {
  public:
    CSecurityDetailDB();
    virtual ~CSecurityDetailDB();

    virtual void DoSecurityDetailFrame1(const TSecurityDetailFrame1Input *pIn, TSecurityDetailFrame1Output *pOut);
  };
}

#endif
