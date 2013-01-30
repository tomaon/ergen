/*
 */

#include "HandleStmt.H"

#include "HandleDbc.H"

namespace TPCE {

  /*
   */
  CHandleStmt::CHandleStmt(CHandleDbc *pDbc) : hStmt(SQL_NULL_HANDLE) {

    SQLRETURN rc = SQL_SUCCESS;

    if (SQL_SUCCEEDED(rc)) {

      SQLHANDLE h = SQL_NULL_HANDLE;

      rc = ODBC::AllocHandle(SQL_HANDLE_STMT, pDbc->handle(), &h);

      if (SQL_SUCCEEDED(rc)) {
        hStmt = h;
      }
    }
  }

  /*
   */
  CHandleStmt::~CHandleStmt() {

    SQLRETURN rc = SQL_NULL_HANDLE != hStmt ? SQL_SUCCESS : SQL_INVALID_HANDLE;

    if (SQL_SUCCEEDED(rc)) {

      rc = ODBC::FreeHandle(SQL_HANDLE_STMT, hStmt);

      if (SQL_SUCCEEDED(rc)) {
        hStmt = SQL_NULL_HANDLE;
      }
    }
  }

  /*
   */
  SQLHANDLE CHandleStmt::handle() {
    return hStmt;
  }

  /*
   */
  bool CHandleStmt::Prepare(const SQLCHAR *StatementText, SQLINTEGER TextLength) {
    SQLRETURN rc = ODBC::Prepare(hStmt, (SQLCHAR *)StatementText, TextLength);
    return SQL_SUCCEEDED(rc);
  }

  /*
   */
  CHandleStmt& CHandleStmt::BindCol(SQLUSMALLINT ColumnNumber,
                                    SQLSMALLINT TargetType,
                                    SQLPOINTER TargetValuePtr,
                                    SQLLEN BufferLength,
                                    SQLLEN *StrLen_or_Ind) {
    ODBC::BindCol(hStmt,
                  ColumnNumber,
                  TargetType,
                  TargetValuePtr,
                  BufferLength,
                  StrLen_or_Ind);

    return *this;
  }

  /*
   */
  CHandleStmt& CHandleStmt::BindParameter(SQLUSMALLINT ParameterNumber,
                                          SQLSMALLINT InputOutputType,
                                          SQLSMALLINT ValueType,
                                          SQLSMALLINT ParameterType,
                                          SQLULEN ColumnSize,
                                          SQLSMALLINT DecimalDigits,
                                          SQLPOINTER ParameterValuePtr,
                                          SQLLEN BufferLength,
                                          SQLLEN *StrLen_or_IndPtr) {
    ODBC::BindParameter(hStmt,
                        ParameterNumber,
                        InputOutputType,
                        ValueType,
                        ParameterType,
                        ColumnSize,
                        DecimalDigits,
                        ParameterValuePtr,
                        BufferLength,
                        StrLen_or_IndPtr);
    return *this;
  }

  /*
   */
  bool CHandleStmt::Cancel() {
    SQLRETURN rc = ODBC::Cancel(hStmt);
    return SQL_SUCCEEDED(rc);
  }

  /*
   */
  bool CHandleStmt::ExecDirect(const SQLCHAR *StatementText, SQLINTEGER TextLength) {
    SQLRETURN rc = ODBC::ExecDirect(hStmt, (SQLCHAR *)StatementText, TextLength);
    return SQL_SUCCEEDED(rc);
  }

  /*
   */
  bool CHandleStmt::Execute() {
    SQLRETURN rc = ODBC::Execute(hStmt);
    return SQL_SUCCEEDED(rc);
  }

  /*
   */
  bool CHandleStmt::Fetch() {
    SQLRETURN rc = ODBC::Fetch(hStmt);
    return SQL_SUCCEEDED(rc);
  }

  /*
   */
  SQLLEN CHandleStmt::RowCount() {
    SQLLEN RowCount = 0;
    SQLRETURN rc = ODBC::RowCount(hStmt, &RowCount);
    return SQL_SUCCEEDED(rc) ? RowCount : 0; // -1?
  }

}
