/*
 */

#ifndef DATA_MAINTENANCE_DB_H
#define	DATA_MAINTENANCE_DB_H

#include "ERGenMYODBC.H"

namespace TPCE {

  class CHandleDbc;

  /*
   */
  class CDataMaintenanceDB : public CDataMaintenanceDBInterface {

  private:
    CHandleDbc *m_pDbc;

  public:
    CDataMaintenanceDB(CHandleDbc *pDbc);
    virtual ~CDataMaintenanceDB();

    virtual void DoDataMaintenanceFrame1(const TDataMaintenanceFrame1Input *pIn);
  };
}

#endif // DATA_MAINTENANCE_DB_H
