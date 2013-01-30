/*
 */

#ifndef CUSTOMER_POSITION_DB_H
#define	CUSTOMER_POSITION_DB_H

#include "ERGenMYODBC.H"

namespace TPCE {

  class CHandleDbc;
  class CHandleStmt;

  /*
   */
  class CCustomerPositionDB : public CCustomerPositionDBInterface {

  private:
    CHandleDbc *m_pDbc;
    CHandleStmt *m_pStmt[4];

  public:
    CCustomerPositionDB(CHandleDbc *pDbc);
    virtual ~CCustomerPositionDB();

    virtual void DoCustomerPositionFrame1(const TCustomerPositionFrame1Input *pIn, TCustomerPositionFrame1Output *pOut);
    virtual void DoCustomerPositionFrame2(const TCustomerPositionFrame2Input *pIn, TCustomerPositionFrame2Output *pOut);
    virtual void DoCustomerPositionFrame3(void);
  };
}

#endif // CUSTOMER_POSITION_DB_H
