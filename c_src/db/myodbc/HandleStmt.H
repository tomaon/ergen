/*
 */

#ifndef HANDLE_STMT_H
#define HANDLE_STMT_H

#include <string.h> // strnlen

#include "DateTime.h" // EGenStandardTypes.h

// DB_INDICATOR (EGenStandardTypes.h), mysql-x64? MUST -DMSSQL
//  DB2     : UINT16
//  MSSQL   : long
//  ORACLE  : sb2
//  other   : INT32

#include "odbc.H"

namespace TPCE {

  class CHandleDbc;

  /*
   */
  class CHandleStmt {

  private:
    SQLHSTMT hStmt;

  public:
    CHandleStmt(CHandleDbc *pDbc);
    virtual ~CHandleStmt();

    virtual SQLHANDLE handle();

    virtual CHandleStmt& BindCol(SQLUSMALLINT ColumnNumber,
                                 INT64 *TargetValuePtr,
                                 SQLLEN, // BufferLength,
                                 DB_INDICATOR *StrLen_or_Ind = NULL) {
      return BindCol(ColumnNumber,
                     SQL_C_SBIGINT,
                     (SQLPOINTER) TargetValuePtr,
                     0,
                     StrLen_or_Ind);
    }
    virtual CHandleStmt& BindCol(SQLUSMALLINT ColumnNumber,
                                 INT32 *TargetValuePtr,
                                 SQLLEN, // BufferLength,
                                 DB_INDICATOR *StrLen_or_Ind = NULL) {
      return BindCol(ColumnNumber,
                     SQL_C_SLONG,
                     (SQLPOINTER) TargetValuePtr,
                     0,
                     StrLen_or_Ind);
    }
    virtual CHandleStmt& BindCol(SQLUSMALLINT ColumnNumber,
                                 double *TargetValuePtr,
                                 SQLLEN, // BufferLength,
                                 DB_INDICATOR *StrLen_or_Ind = NULL) {
      return BindCol(ColumnNumber,
                     SQL_C_DOUBLE,
                     (SQLPOINTER) TargetValuePtr,
                     0,
                     StrLen_or_Ind);
    }
    virtual CHandleStmt& BindCol(SQLUSMALLINT ColumnNumber,
                                 TIMESTAMP_STRUCT *TargetValuePtr,
                                 SQLLEN, // BufferLength,
                                 DB_INDICATOR *StrLen_or_Ind = NULL) {
      return BindCol(ColumnNumber,
                     SQL_C_TIMESTAMP,
                     (SQLPOINTER) TargetValuePtr,
                     0,
                     StrLen_or_Ind);
    }
    virtual CHandleStmt& BindCol(SQLUSMALLINT ColumnNumber,
                                 char *TargetValuePtr,
                                 SQLLEN BufferLength,
                                 DB_INDICATOR *StrLen_or_Ind = NULL) {
      if (1 < BufferLength) {
        return BindCol(ColumnNumber,
                       SQL_C_CHAR,
                       (SQLPOINTER) TargetValuePtr,
                       BufferLength,
                       StrLen_or_Ind);
      } else {
        return BindCol(ColumnNumber,
                       SQL_C_BIT,
                       (SQLPOINTER) TargetValuePtr,
                       0,
                       StrLen_or_Ind);
      }
    }

    virtual CHandleStmt& BindCol(SQLUSMALLINT ColumnNumber,
                                 SQLSMALLINT TargetType,
                                 SQLPOINTER TargetValuePtr,
                                 SQLLEN BufferLength,
                                 DB_INDICATOR *StrLen_or_Ind);

    virtual CHandleStmt& BindParameter(SQLUSMALLINT ParameterNumber,
                                       const INT64 *ParameterValuePtr,
                                       SQLLEN, //BufferLength,
                                       DB_INDICATOR *StrLen_or_IndPtr = NULL) {
      return BindParameter(ParameterNumber,
                           SQL_PARAM_INPUT,
                           SQL_C_SBIGINT,
                           SQL_BIGINT,
                           0,
                           0,
                           (SQLPOINTER) ParameterValuePtr,
                           0,
                           StrLen_or_IndPtr);
    }
    virtual CHandleStmt& BindParameter(SQLUSMALLINT ParameterNumber,
                                       const INT32 *ParameterValuePtr,
                                       SQLLEN, // BufferLength,
                                       DB_INDICATOR *StrLen_or_IndPtr = NULL) {
      return BindParameter(ParameterNumber,
                           SQL_PARAM_INPUT,
                           SQL_C_SLONG,
                           SQL_INTEGER,
                           0,
                           0,
                           (SQLPOINTER) ParameterValuePtr,
                           0,
                           StrLen_or_IndPtr);
    }
    virtual CHandleStmt& BindParameter(SQLUSMALLINT ParameterNumber,
                                       const double *ParameterValuePtr,
                                       SQLLEN, //BufferLength,
                                       DB_INDICATOR *StrLen_or_IndPtr = NULL) {
      return BindParameter(ParameterNumber,
                           SQL_PARAM_INPUT,
                           SQL_C_DOUBLE,
                           SQL_DOUBLE,
                           0,
                           0,
                           (SQLPOINTER) ParameterValuePtr,
                           0,
                           StrLen_or_IndPtr);
    }
    virtual CHandleStmt& BindParameter(SQLUSMALLINT ParameterNumber,
                                       const TIMESTAMP_STRUCT *ParameterValuePtr,
                                       SQLLEN, // BufferLength,
                                       DB_INDICATOR *StrLen_or_IndPtr = NULL) {
      return BindParameter(ParameterNumber,
                           SQL_PARAM_INPUT,
                           SQL_C_TIMESTAMP,
                           SQL_TYPE_TIMESTAMP,
                           0,
                           0,
                           (SQLPOINTER) ParameterValuePtr,
                           0,
                           StrLen_or_IndPtr);
    }
    virtual CHandleStmt& BindParameter(SQLUSMALLINT ParameterNumber,
                                       const char *ParameterValuePtr,
                                       SQLLEN BufferLength,
                                       DB_INDICATOR *StrLen_or_IndPtr = NULL) {
      if (1 < BufferLength) {
        return BindParameter(ParameterNumber,
                             SQL_PARAM_INPUT,
                             SQL_C_CHAR,
                             SQL_CHAR,
                             strnlen(ParameterValuePtr, BufferLength),
                             0,
                             (SQLPOINTER) ParameterValuePtr,
                             BufferLength,
                             StrLen_or_IndPtr);
      } else {
        return BindParameter(ParameterNumber,
                             SQL_PARAM_INPUT,
                             SQL_C_BIT,
                             SQL_CHAR,
                             0,
                             0,
                             (SQLPOINTER) ParameterValuePtr,
                             0,
                             StrLen_or_IndPtr);
      }
    }

    virtual CHandleStmt& BindParameter(SQLUSMALLINT ParameterNumber,
                                       SQLSMALLINT InputOutputType,
                                       SQLSMALLINT ValueType,
                                       SQLSMALLINT ParameterType,
                                       SQLULEN ColumnSize,
                                       SQLSMALLINT DecimalDigits,
                                       SQLPOINTER ParameterValuePtr,
                                       SQLLEN BufferLength,
                                       SQLLEN *StrLen_or_IndPtr);

    virtual bool Cancel();

    virtual bool ExecDirect(const SQLCHAR *StatementText, SQLINTEGER TextLength = SQL_NTS);

    virtual bool Execute();

    virtual bool Fetch();

    virtual bool Prepare(const SQLCHAR *StatemenText, SQLINTEGER TextLength = SQL_NTS);

    virtual SQLLEN RowCount();
  };

}

#endif // HANDLE_STMT_H
