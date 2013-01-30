/*
 */

#ifndef SECURITY_DETAIL_DB_H
#define	SECURITY_DETAIL_DB_H

#include "ERGenMYODBC.H"

namespace TPCE {

  class CHandleDbc;
  class CHandleStmt;

  /*
   */
  class CSecurityDetailDB : public CSecurityDetailDBInterface {

  private:
    CHandleDbc *m_pDbc;
    CHandleStmt *m_pStmt[7];

  public:
    CSecurityDetailDB(CHandleDbc *pDbc);
    virtual ~CSecurityDetailDB();

    virtual void DoSecurityDetailFrame1(const TSecurityDetailFrame1Input *pIn, TSecurityDetailFrame1Output *pOut);
  };
}

#endif // SECURITY_DETAIL_DB_H
