/*
 */

#ifndef TRADE_LOOKUP_DB_H
#define	TRADE_LOOKUP_DB_H

#include "ERGenMYODBC.H"

namespace TPCE {

  class CHandleDbc;
  class CHandleStmt;

  /*
   */
  class CTradeLookupDB : public CTradeLookupDBInterface {

  private:
    CHandleDbc *m_pDbc;
    CHandleStmt *m_pStmt[9];

  public:
    CTradeLookupDB(CHandleDbc *pDbc);
    virtual ~CTradeLookupDB();

    virtual void DoTradeLookupFrame1(const TTradeLookupFrame1Input *pIn, TTradeLookupFrame1Output *pOut);
    virtual void DoTradeLookupFrame2(const TTradeLookupFrame2Input *pIn, TTradeLookupFrame2Output *pOut);
    virtual void DoTradeLookupFrame3(const TTradeLookupFrame3Input *pIn, TTradeLookupFrame3Output *pOut);
    virtual void DoTradeLookupFrame4(const TTradeLookupFrame4Input *pIn, TTradeLookupFrame4Output *pOut);
  };
}

#endif // TRADE_LOOKUP_DB_H
