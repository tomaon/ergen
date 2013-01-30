/*
 */

#include "HandleDbc.H"

#include "HandleEnv.H"

namespace TPCE {

  /*
   */
  CHandleDbc::CHandleDbc(CHandleEnv *pEnv) : hDbc(SQL_NULL_HANDLE) {

    SQLRETURN rc = SQL_SUCCESS;

    if (SQL_SUCCEEDED(rc)) {

      SQLHANDLE h = SQL_NULL_HANDLE;

      rc =  ODBC::AllocHandle(SQL_HANDLE_DBC, pEnv->handle(), &h);

      if (SQL_SUCCEEDED(rc)) {
        hDbc = h;
      }
    }
  }

  /*
   */
  CHandleDbc::~CHandleDbc() {

    SQLRETURN rc = SQL_NULL_HANDLE != hDbc ? SQL_SUCCESS : SQL_INVALID_HANDLE;

    if (SQL_SUCCEEDED(rc)) {

      rc = ODBC::FreeHandle(SQL_HANDLE_DBC, hDbc);

      if (SQL_SUCCEEDED(rc)) {
        hDbc = SQL_NULL_HANDLE;
      }
    }
  }

  /*
   */
  SQLHANDLE CHandleDbc::handle() {
    return hDbc;
  }

  /*
   */
  bool CHandleDbc::Connect(const char *connectString,
                           SQLPOINTER accessMode, SQLPOINTER isolationMode) {

    SQLRETURN rc = SQL_SUCCESS;

    if (SQL_SUCCEEDED(rc)) {

      rc = ODBC::DriverConnect(hDbc, NULL, (SQLCHAR *)connectString, SQL_NTS,
                               NULL, 0, NULL, SQL_DRIVER_COMPLETE);
    }

    if (SQL_SUCCEEDED(rc)) {
      ODBC::SetConnectAttr(hDbc, SQL_ATTR_ACCESS_MODE, accessMode, SQL_NTS);
      ODBC::SetConnectAttr(hDbc, SQL_ATTR_AUTOCOMMIT, (SQLPOINTER) SQL_AUTOCOMMIT_OFF, SQL_NTS);
      ODBC::SetConnectAttr(hDbc, SQL_ATTR_TXN_ISOLATION, isolationMode, SQL_NTS);
    }

    return SQL_SUCCEEDED(rc);
  }

  /*
   */
  void CHandleDbc::Disconnect() {
    ODBC::Disconnect(hDbc);
  }

  /*
   */
  SQLRETURN CHandleDbc::StartTransaction() {
    return RollbackTransaction();
  }

  /*
   */
  SQLRETURN CHandleDbc::CommitTransaction() {
    return ODBC::EndTran(SQL_HANDLE_DBC, hDbc, SQL_COMMIT);
  }

  /*
   */
  SQLRETURN CHandleDbc::RollbackTransaction() {
    return ODBC::EndTran(SQL_HANDLE_DBC, hDbc, SQL_ROLLBACK );
  }

}
