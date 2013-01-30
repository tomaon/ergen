/*
 */

#include "odbc.H"

#include <cstdlib>    // exit
#include <cstring>    // strlen
#include <iostream>   // cout,cerr
#include <sstream>    // ostringstream

namespace ODBC {

  using namespace std;

  string Reason(SQLSMALLINT handleType, SQLHANDLE handle);

  // -- --

  /*
   */
  SQLRETURN AllocHandle(SQLSMALLINT HandleType,
                        SQLHANDLE InputHandle,
                        SQLHANDLE *OutputHandlePtr) {

    SQLRETURN rc = SQLAllocHandle(HandleType,
                                  InputHandle,
                                  OutputHandlePtr);
    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SQLAllocHandle, type=" << HandleType << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SQLAllocHandle, " << Reason(HandleType, *OutputHandlePtr) << '\r' << endl;
      break;
    case SQL_INVALID_HANDLE:
    case SQL_ERROR:
      cerr << "ERROR: SQLAllocHandle, " << Reason(HandleType, InputHandle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SQLAllocHandle, " << Reason(HandleType, InputHandle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  /*
   */
  SQLRETURN BindCol(SQLHSTMT StatementHandle,
                    SQLUSMALLINT ColumnNumber,
                    SQLSMALLINT TargetType,
                    SQLPOINTER TargetValuePtr,
                    SQLLEN BufferLength,
                    SQLLEN *StrLen_or_Ind) {

    SQLRETURN rc = SQLBindCol(StatementHandle,
                              ColumnNumber,
                              TargetType,
                              TargetValuePtr,
                              BufferLength,
                              StrLen_or_Ind);
    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SQLBindCol, number=" << ColumnNumber << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SQLBindCol, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    case SQL_ERROR:
    case SQL_INVALID_HANDLE:
      cerr << "ERROR: SQLBindCol, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SQLBindCol, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  /*
   */
  SQLRETURN BindParameter(SQLHSTMT StatementHandle,
                          SQLUSMALLINT ParameterNumber,
                          SQLSMALLINT InputOutputType,
                          SQLSMALLINT ValueType,
                          SQLSMALLINT ParameterType,
                          SQLULEN ColumnSize,
                          SQLSMALLINT DecimalDigits,
                          SQLPOINTER ParameterValuePtr,
                          SQLLEN BufferLength,
                          SQLLEN *StrLen_or_IndPtr) {

    SQLRETURN rc = SQLBindParameter(StatementHandle,
                                    ParameterNumber,
                                    InputOutputType,
                                    ValueType,
                                    ParameterType,
                                    ColumnSize,
                                    DecimalDigits,
                                    ParameterValuePtr,
                                    BufferLength,
                                    StrLen_or_IndPtr);
    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SQLBindParameter, number=" << ParameterNumber << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SQLBindParameter, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    case SQL_ERROR:
    case SQL_INVALID_HANDLE:
      cerr << "ERROR: SQLBindParameter, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SQLBindParameter, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  /*
   */
  SQLRETURN Cancel(SQLHSTMT StatementHandle) {

    SQLRETURN rc = SQLCancel(StatementHandle);

    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SQLCancel, handle=" << StatementHandle << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SQLCancel, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    case SQL_ERROR:
    case SQL_INVALID_HANDLE:
    case SQL_STILL_EXECUTING:
      cerr << "ERROR: SQLCancel, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SQLCancel, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  /*
   */
  SQLRETURN Disconnect(SQLHDBC ConnectionHandle) {

    SQLRETURN rc = SQLDisconnect(ConnectionHandle);

    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SQLDisconnect" << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SQLDisconnect, " << Reason(SQL_HANDLE_DBC, ConnectionHandle) << '\r' << endl;
      break;
    case SQL_ERROR:
    case SQL_INVALID_HANDLE:
    case SQL_STILL_EXECUTING:
      cerr << "ERROR: SQLDisconnect, " << Reason(SQL_HANDLE_DBC, ConnectionHandle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SQLDisconnect, " << Reason(SQL_HANDLE_DBC, ConnectionHandle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  /*
   */
  SQLRETURN DriverConnect(SQLHDBC ConnectionHandle,
                          SQLHWND WindowHandle,
                          SQLCHAR *InConnectionString,
                          SQLSMALLINT StringLength1,
                          SQLCHAR *OutConnectionString,
                          SQLSMALLINT BufferLength,
                          SQLSMALLINT *StringLength2Ptr,
                          SQLUSMALLINT DriverCompletion) {

    //cout << "DEBUG: SQLDriverConnect, " << InConnectionString << '\r' << endl;

    SQLRETURN rc = SQLDriverConnect(ConnectionHandle,
                                    WindowHandle,
                                    InConnectionString,
                                    StringLength1,
                                    OutConnectionString,
                                    BufferLength,
                                    StringLength2Ptr,
                                    DriverCompletion);
    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SQLDriverConnect, " << InConnectionString << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SQLDriverConnect, " << Reason(SQL_HANDLE_DBC, ConnectionHandle) << '\r' << endl;
      break;
    case SQL_NO_DATA:
    case SQL_ERROR:
    case SQL_INVALID_HANDLE:
    case SQL_STILL_EXECUTING:
      cout << "ERROR: SQLDriverConnect, " << InConnectionString << '\r' << endl;
      cerr << "ERROR: SQLDriverConnect, " << Reason(SQL_HANDLE_DBC, ConnectionHandle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SQLDriverConnect, " << Reason(SQL_HANDLE_DBC, ConnectionHandle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  /*
   */
  SQLRETURN EndTran(SQLSMALLINT HandleType,
                    SQLHANDLE Handle,
                    SQLSMALLINT CompletionType) {

    SQLRETURN rc = SQLEndTran(HandleType,
                              Handle,
                              CompletionType);
    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SQLEndTran, type=" << HandleType << "," << CompletionType << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SQLEndTran, " << Reason(HandleType, Handle) << '\r' << endl;
      break;
    case SQL_ERROR:
    case SQL_INVALID_HANDLE:
    case SQL_STILL_EXECUTING:
      cerr << "ERROR: SQLEndTran, " << Reason(HandleType, Handle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SQLEndTran, " << Reason(HandleType, Handle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  /*
   */
  SQLRETURN ExecDirect(SQLHSTMT StatementHandle,
                       SQLCHAR *StatementText,
                       SQLINTEGER TextLength) {

    SQLRETURN rc = SQLExecDirect(StatementHandle,
                                 StatementText,
                                 TextLength);
    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SQLExecDirect, statememt=" << StatementText << '\r' << endl;
      //cout << "DEBUG: SQLExecDirect, handle=" << StatementHandle << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SQLExecDirect, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    case SQL_NEED_DATA:
    case SQL_STILL_EXECUTING:
    case SQL_ERROR:
    case SQL_NO_DATA:
    case SQL_INVALID_HANDLE: // case SQL_PARAM_DATA_AVAILABLE:
      cerr << "ERROR: SQLExecDirect, statement=" << StatementText << '\r' << endl;
      cerr << "ERROR: SQLExecDirect, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SQLExecDirect, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  /*
   */
  SQLRETURN Execute(SQLHSTMT StatementHandle) {

    SQLRETURN rc = SQLExecute(StatementHandle);

    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SQLExecute, handle=" << StatementHandle << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SQLExecute, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    case SQL_NEED_DATA:
    case SQL_STILL_EXECUTING:
    case SQL_ERROR:
    case SQL_NO_DATA:
    case SQL_INVALID_HANDLE: // case SQL_PARAM_DATA_AVAILABLE:
      cerr << "ERROR: SQLExecute, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SQLExecute, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }


  /*
   */
  SQLRETURN Fetch(SQLHSTMT StatementHandle) {

    SQLRETURN rc = SQLFetch(StatementHandle);

    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SQLFetch, handle=" << StatementHandle << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SQLFetch, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    case SQL_NO_DATA:
      cout << "DEBUG: SQLFetch, NODATA" << '\r' << endl;
      break;
    case SQL_STILL_EXECUTING:
    case SQL_ERROR:
    case SQL_INVALID_HANDLE:
      cerr << "ERROR: SQLFetch, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SQLFetch, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  /*
   */
  SQLRETURN FreeHandle(SQLSMALLINT HandleType,
                       SQLHANDLE Handle) {

    SQLRETURN rc = SQLFreeHandle(HandleType,
                                 Handle);
    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SQLFreeHandle, type=" << HandleType << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SQLFreeHandle, " << Reason(HandleType, Handle) << '\r' << endl;
      break;
    case SQL_INVALID_HANDLE:
    case SQL_ERROR:
      cerr << "ERROR: SQLFreeHandle, " << Reason(HandleType, Handle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SQLFreeHandle, " << Reason(HandleType, Handle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  /*
   */
  SQLRETURN Prepare(SQLHSTMT StatementHandle,
                    SQLCHAR *StatementText,
                    SQLINTEGER TextLength) {

    SQLRETURN rc = SQLPrepare(StatementHandle,
                              StatementText,
                              TextLength);
    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SQLPrepare, statement=" << StatementText << '\r' << endl;
      //cout << "DEBUG: SQLPrepare, handle=" << StatementHandle << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SQLPrepare, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    case SQL_STILL_EXECUTING:
    case SQL_ERROR:
    case SQL_INVALID_HANDLE:
      cerr << "ERROR: SQLPrepare, statement=" << StatementText << '\r' << endl;
      cerr << "ERROR: SQLPrepare, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SQLPrepare, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  /*
   */
  SQLRETURN RowCount(SQLHSTMT StatementHandle,
                     SQLLEN *RowCount) {

    SQLRETURN rc = SQLRowCount(StatementHandle,
                               RowCount);
    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SQLRowCount, count=" << *RowCount << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SQLRowCount, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    case SQL_ERROR:
    case SQL_INVALID_HANDLE:
      cerr << "ERROR: SQLRowCount, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SQLRowCount, " << Reason(SQL_HANDLE_STMT, StatementHandle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  /*
   */
  SQLRETURN SetConnectAttr(SQLHDBC ConnectionHandle,
                           SQLINTEGER Attribute,
                           SQLPOINTER ValuePtr,
                           SQLINTEGER StringLength) {

    SQLRETURN rc = SQLSetConnectAttr(ConnectionHandle,
                                     Attribute,
                                     ValuePtr,
                                     StringLength);
    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SetConnectAttr, attr=" << Attribute << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SetConnectAttr, " << Reason(SQL_HANDLE_DBC, ConnectionHandle) << '\r' << endl;
      break;
    case SQL_ERROR:
    case SQL_INVALID_HANDLE:
      cerr << "ERROR: SetConnectAttr, " << Reason(SQL_HANDLE_DBC, ConnectionHandle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SetConnectAttr, " << Reason(SQL_HANDLE_DBC, ConnectionHandle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  /*
   */
  SQLRETURN SetEnvAttr(SQLHENV EnvironmentHandle,
                       SQLINTEGER Attribute,
                       SQLPOINTER ValuePtr,
                       SQLINTEGER StringLength) {

    SQLRETURN rc =  SQLSetEnvAttr(EnvironmentHandle,
                                  Attribute,
                                  ValuePtr,
                                  StringLength);
    switch (rc) {
    case SQL_SUCCESS:
      //cout << "DEBUG: SetEnvAttr, attr=" << Attribute << '\r' << endl;
      break;
    case SQL_SUCCESS_WITH_INFO:
      cout << "INFO: SetEnvAttr, " << Reason(SQL_HANDLE_ENV, EnvironmentHandle) << '\r' << endl;
      break;
    case SQL_ERROR:
    case SQL_INVALID_HANDLE:
    case  SQL_STILL_EXECUTING:
      cerr << "ERROR: SetEnvAttr, " << Reason(SQL_HANDLE_ENV, EnvironmentHandle) << '\r' << endl;
      break;
    default:
      cerr << "FATAL: SetEnvAttr, " << Reason(SQL_HANDLE_ENV, EnvironmentHandle) << '\r' << endl;
      exit(1);
    }

    return rc;
  }

  // -- etc --

  /*
   */
  string Implode(const char *glue, const char *pieces, size_t size, const char *s) {

    ostringstream oss;

    for (const char *p = pieces; '\0' != *p; p += size) {
      oss << s << p << s << glue;
    }

    long pos = ((long) oss.tellp()) - strlen(glue);
    oss.seekp(pos) << '\0';

    return oss.str();
  }

  /*
   */
  string Reason(SQLSMALLINT handleType, SQLHANDLE handle) {

    ostringstream oss;

    SQLRETURN rc = SQL_SUCCESS;
    SQLSMALLINT recNumber;
    SQLCHAR sqlState[SQL_SQLSTATE_SIZE+1];
    SQLINTEGER nativeError;
    SQLCHAR messageText[1024];

    for (recNumber = 1; SQL_SUCCEEDED(rc); recNumber++) {

      rc = SQLGetDiagRec(handleType, handle, recNumber, sqlState, &nativeError,
                         messageText, sizeof(messageText) / sizeof(messageText[0]), NULL);
      switch (rc) {
      case SQL_SUCCESS:
        oss << "[" << sqlState << ":" << nativeError << "]" << messageText << '\r' << endl;
        break;
      case SQL_SUCCESS_WITH_INFO:
        cout << "INFO: SQLGetDiagRec" << '\r' << endl;
        break;
      case SQL_ERROR:
      case SQL_NO_DATA:
      case SQL_INVALID_HANDLE:
        cerr << "ERROR: SQLGetDiagRec"<< '\r' << endl;
        break;
      }
    }

    return oss.str();
  }

}
