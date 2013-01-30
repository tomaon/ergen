#ifndef DATA_MAINTENANCE_DB_H
#define	DATA_MAINTENANCE_DB_H

#include "TxnHarnessDBInterface.h"

namespace TPCE {

  class CDataMaintenanceDB : public CDataMaintenanceDBInterface {
  public:
    CDataMaintenanceDB();
    virtual ~CDataMaintenanceDB();

    virtual void DoDataMaintenanceFrame1(const TDataMaintenanceFrame1Input *pIn);
  };
}

#endif
