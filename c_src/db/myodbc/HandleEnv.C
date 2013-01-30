/*
 */

#include "HandleEnv.H"

namespace TPCE {

  /*
   */
  CHandleEnv::CHandleEnv() : hEnv(SQL_NULL_HANDLE) {

    SQLRETURN rc = SQL_SUCCESS;

    if (SQL_SUCCEEDED(rc)) {

      SQLHANDLE h = SQL_NULL_HANDLE;

      rc = ODBC::AllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &h);

      if (SQL_SUCCEEDED(rc)) {
        hEnv = h;
      }
    }

    if (SQL_SUCCEEDED(rc)) {
      ODBC::SetEnvAttr(hEnv, SQL_ATTR_ODBC_VERSION, (SQLPOINTER) SQL_OV_ODBC3, SQL_NTS);
      ODBC::SetEnvAttr(hEnv, SQL_ATTR_CONNECTION_POOLING, (SQLPOINTER) SQL_CP_OFF, SQL_NTS);
      ODBC::SetEnvAttr(hEnv, SQL_ATTR_CP_MATCH, (SQLPOINTER) SQL_CP_STRICT_MATCH, SQL_NTS);
    }
  }

  /*
   */
  CHandleEnv::~CHandleEnv() {

    SQLRETURN rc = SQL_NULL_HANDLE != hEnv ? SQL_SUCCESS : SQL_INVALID_HANDLE;

    if (SQL_SUCCEEDED(rc)) {

      rc = ODBC::FreeHandle(SQL_HANDLE_ENV, hEnv);

      if (SQL_SUCCEEDED(rc)) {
        hEnv = SQL_NULL_HANDLE;
      }
    }
  }

  /*
   */
  SQLHANDLE CHandleEnv::handle() {
    return hEnv;
  }

}
