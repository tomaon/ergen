/*
 */

#ifndef HANDLE_DBC_H
#define HANDLE_DBC_H

#include "odbc.H"

namespace TPCE {

  class CHandleEnv;

  /*
   */
  class CHandleDbc {

  private:
    SQLHDBC hDbc;

  public:
    CHandleDbc(CHandleEnv *pEnv);
    virtual ~CHandleDbc();

    virtual SQLHANDLE handle();

    virtual bool Connect(const char *connectString, SQLPOINTER accessMode, SQLPOINTER isolationMode);
    virtual void Disconnect();

    virtual SQLRETURN StartTransaction();
    virtual SQLRETURN CommitTransaction();
    virtual SQLRETURN RollbackTransaction();
  };

}

// SQL_ATTR_ACCESS_MODE
//   SQL_MODE_READ_ONLY
//  *SQL_MODE_READ_WRITE
//
// SQL_ATTR_TXN_ISOLATION
//   SQL_TXN_READ_UNCOMMITTED
//   SQL_TXN_READ_COMMITTED
//   SQL_TXN_REPEATABLE_READ
//   SQL_TXN_SERIALIZABLE

#endif // HANDLE_DBC_H
