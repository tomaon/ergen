/*
 */

#ifndef ODBC_H
#define ODBC_H

#include <string> // std::string

#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>

# ifndef  SQL_SUCCEEDED
#  define SQL_SUCCEEDED(rc) (((rc) & (~1)) == 0)
# endif

namespace ODBC {

  // @see http://msdn.microsoft.com/ja-jp/library/ms714562%28v=vs.85%29.aspx

  SQLRETURN AllocHandle(SQLSMALLINT HandleType,
                        SQLHANDLE InputHandle,
                        SQLHANDLE *OutputHandlePtr);

  SQLRETURN BindCol(SQLHSTMT StatementHandle,
                    SQLUSMALLINT ColumnNumber,
                    SQLSMALLINT TargetType,
                    SQLPOINTER TargetValuePtr,
                    SQLLEN BufferLength,
                    SQLLEN *StrLen_or_Ind);

  SQLRETURN BindParameter(SQLHSTMT StatementHandle,
                          SQLUSMALLINT ParameterNumber,
                          SQLSMALLINT InputOutputType,
                          SQLSMALLINT ValueType,
                          SQLSMALLINT ParameterType,
                          SQLULEN ColumnSize,
                          SQLSMALLINT DecimalDigits,
                          SQLPOINTER ParameterValuePtr,
                          SQLLEN BufferLength,
                          SQLLEN *StrLen_or_IndPtr);

  // SQLRETURN BrowseConnect(SQLHDBC ConnectionHandle,
  //                         SQLCHAR *InConnectionString,
  //                         SQLSMALLINT StringLength1,
  //                         SQLCHAR *OutConnectionString,
  //                         SQLSMALLINT BufferLength,
  //                         SQLSMALLINT *StringLength2Ptr);

  // SQLRETURN BulkOperations(SQLHSTMT StatementHandle,
  //                          SQLSMALLINT Operation);

  SQLRETURN Cancel(SQLHSTMT StatementHandle);

  // SQLRETURN SQL_API SQLCloseCursor ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle);

  // /*
  //  *  Using SQLLEN * instead of SQLPOINTER for NumericAttribute,
  //  *  makes the prototype the same as SQLColAttributes (deprecated)
  //  *  and clearer for 64bit ports
  //  */
  // SQLRETURN SQL_API SQLColAttribute ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLUSMALLINT	  ColumnNumber,
  //     SQLUSMALLINT	  FieldIdentifier,
  //     SQLPOINTER		  CharacterAttribute,
  //     SQLSMALLINT		  BufferLength,
  //     SQLSMALLINT		* StringLength,
  //     SQLLEN		* NumericAttribute);

  // SQLRETURN SQL_API SQLColumnPrivileges (// ODBC 1.0 Standards Compliance: ODBC
  //     SQLHSTMT              hstmt,
  //     SQLCHAR             * szCatalogName,
  //     SQLSMALLINT           cbCatalogName,
  //     SQLCHAR             * szSchemaName,
  //     SQLSMALLINT           cbSchemaName,
  //     SQLCHAR             * szTableName,
  //     SQLSMALLINT           cbTableName,
  //     SQLCHAR             * szColumnName,
  //     SQLSMALLINT           cbColumnName)

  // SQLRETURN SQL_API SQLColumns ( // ODBC 1.0 Standards Compliance: Open Group
  //     SQLHSTMT		  StatementHandle,
  //     SQLCHAR		* CatalogName,
  //     SQLSMALLINT		  NameLength1,
  //     SQLCHAR		* SchemaName,
  //     SQLSMALLINT		  NameLength2,
  //     SQLCHAR		* TableName,
  //     SQLSMALLINT		  NameLength3,
  //     SQLCHAR		* ColumnName,
  //     SQLSMALLINT		  NameLength4);

  // SQLRETURN SQL_API SQLConnect ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHDBC		  ConnectionHandle,
  //     SQLCHAR		* ServerName,
  //     SQLSMALLINT		  NameLength1,
  //     SQLCHAR		* UserName,
  //     SQLSMALLINT		  NameLength2,
  //     SQLCHAR		* Authentication,
  //     SQLSMALLINT		  NameLength3);

  // SQLRETURN SQL_API SQLCopyDesc ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLHDESC		  SourceDescHandle,
  //     SQLHDESC		  TargetDescHandle);

  // SQLRETURN SQL_API SQLDataSources ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHENV		  EnvironmentHandle,
  //     SQLUSMALLINT	  Direction,
  //     SQLCHAR		* ServerName,
  //     SQLSMALLINT		  BufferLength1,
  //     SQLSMALLINT		* NameLength1,
  //     SQLCHAR		* Description,
  //     SQLSMALLINT		  BufferLength2,
  //     SQLSMALLINT		* NameLength2);

  // SQLRETURN SQL_API SQLDescribeCol ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLUSMALLINT	  ColumnNumber,
  //     SQLCHAR		* ColumnName,
  //     SQLSMALLINT		  BufferLength,
  //     SQLSMALLINT		* NameLength,
  //     SQLSMALLINT		* DataType,
  //     SQLULEN		* ColumnSize,
  //     SQLSMALLINT		* DecimalDigits,
  //     SQLSMALLINT		* Nullable);

  // SQLRETURN SQL_API SQLDescribeParam ( //  ODBC 1.0 Standards Compliance: ODBC
  //     SQLHSTMT              hstmt,
  //     SQLUSMALLINT          ipar,
  //     SQLSMALLINT         * pfSqlType,
  //     SQLULEN             * pcbParamDef,
  //     SQLSMALLINT         * pibScale,
  //     SQLSMALLINT         * pfNullable)

  SQLRETURN Disconnect(SQLHDBC ConnectionHandle);

  SQLRETURN DriverConnect(SQLHDBC ConnectionHandle,
                          SQLHWND WindowHandle,
                          SQLCHAR *InConnectionString,
                          SQLSMALLINT StringLength1,
                          SQLCHAR *OutConnectionString,
                          SQLSMALLINT BufferLength,
                          SQLSMALLINT *StringLength2Ptr,
                          SQLUSMALLINT DriverCompletion);

  // SQLRETURN SQL_API SQLDrivers (// ODBC 2.0 Standards Compliance: ODBC
  //     SQLHENV               henv,
  //     SQLUSMALLINT          fDirection,
  //     SQLCHAR             * szDriverDesc,
  //     SQLSMALLINT           cbDriverDescMax,
  //     SQLSMALLINT         * pcbDriverDesc,
  //     SQLCHAR             * szDriverAttributes,
  //     SQLSMALLINT           cbDrvrAttrMax,
  //     SQLSMALLINT         * pcbDrvrAttr)

  SQLRETURN EndTran(SQLSMALLINT HandleType,
                    SQLHANDLE Handle,
                    SQLSMALLINT CompletionType);

  SQLRETURN ExecDirect(SQLHSTMT StatementHandle,
                       SQLCHAR *StatementText,
                       SQLINTEGER TextLength);

  SQLRETURN Execute(SQLHSTMT StatementHandle);

  SQLRETURN Fetch(SQLHSTMT StatementHandle);

  // SQLRETURN SQL_API SQLFetchScroll ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLSMALLINT		  FetchOrientation,
  //     SQLLEN		  FetchOffset);

  // SQLRETURN SQL_API SQLForeignKeys ( // ODBC 1.0 Standards Compliance: ODBC
  //     SQLHSTMT              hstmt,
  //     SQLCHAR             * szPkCatalogName,
  //     SQLSMALLINT           cbPkCatalogName,
  //     SQLCHAR             * szPkSchemaName,
  //     SQLSMALLINT           cbPkSchemaName,
  //     SQLCHAR             * szPkTableName,
  //     SQLSMALLINT           cbPkTableName,
  //     SQLCHAR             * szFkCatalogName,
  //     SQLSMALLINT           cbFkCatalogName,
  //     SQLCHAR             * szFkSchemaName,
  //     SQLSMALLINT           cbFkSchemaName,
  //     SQLCHAR             * szFkTableName,
  //     SQLSMALLINT           cbFkTableName)

  SQLRETURN FreeHandle(SQLSMALLINT HandleType,
                       SQLHANDLE Handle);

  // SQLRETURN SQL_API SQLFreeStmt ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLUSMALLINT	  Option);

  // SQLRETURN SQL_API SQLGetConnectAttr ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLHDBC		  ConnectionHandle,
  //     SQLINTEGER		  Attribute,
  //     SQLPOINTER		  Value,
  //     SQLINTEGER		  BufferLength,
  //     SQLINTEGER		* StringLength);

  // SQLRETURN SQL_API SQLGetCursorName ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLCHAR		* CursorName,
  //     SQLSMALLINT		  BufferLength,
  //     SQLSMALLINT		* NameLength);

  // SQLRETURN SQL_API SQLGetData ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLUSMALLINT	  ColumnNumber,
  //     SQLSMALLINT		  TargetType,
  //     SQLPOINTER		  TargetValue,
  //     SQLLEN		  BufferLength,
  //     SQLLEN		* StrLen_or_Ind);

  // SQLRETURN SQL_API SQLGetDescField ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLHDESC		  DescriptorHandle,
  //     SQLSMALLINT		  RecNumber,
  //     SQLSMALLINT		  FieldIdentifier,
  //     SQLPOINTER		  Value,
  //     SQLINTEGER		  BufferLength,
  //     SQLINTEGER		* StringLength);

  // SQLRETURN SQL_API SQLGetDescRec ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLHDESC		  DescriptorHandle,
  //     SQLSMALLINT		  RecNumber,
  //     SQLCHAR		* Name,
  //     SQLSMALLINT		  BufferLength,
  //     SQLSMALLINT		* StringLength,
  //     SQLSMALLINT		* Type,
  //     SQLSMALLINT		* SubType,
  //     SQLLEN		* Length,
  //     SQLSMALLINT		* Precision,
  //     SQLSMALLINT		* Scale,
  //     SQLSMALLINT		* Nullable);

  // SQLRETURN SQL_API SQLGetDiagField ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLSMALLINT		  HandleType,
  //     SQLHANDLE		  Handle,
  //     SQLSMALLINT		  RecNumber,
  //     SQLSMALLINT		  DiagIdentifier,
  //     SQLPOINTER		  DiagInfo,
  //     SQLSMALLINT		  BufferLength,
  //     SQLSMALLINT		* StringLength);

  // SQLRETURN SQL_API SQLGetDiagRec ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLSMALLINT		  HandleType,
  //     SQLHANDLE		  Handle,
  //     SQLSMALLINT		  RecNumber,
  //     SQLCHAR		* Sqlstate,
  //     SQLINTEGER		* NativeError,
  //     SQLCHAR		* MessageText,
  //     SQLSMALLINT		  BufferLength,
  //     SQLSMALLINT		* TextLength);

  // SQLRETURN SQL_API SQLGetEnvAttr ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLHENV		  EnvironmentHandle,
  //     SQLINTEGER		  Attribute,
  //     SQLPOINTER		  Value,
  //     SQLINTEGER		  BufferLength,
  //     SQLINTEGER		* StringLength);

  // SQLRETURN SQL_API SQLGetFunctions ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHDBC		ConnectionHandle,
  //     SQLUSMALLINT	FunctionId,
  //     SQLUSMALLINT *	Supported);

  // SQLRETURN SQL_API SQLGetInfo ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHDBC		  ConnectionHandle,
  //     SQLUSMALLINT	  InfoType,
  //     SQLPOINTER		  InfoValue,
  //     SQLSMALLINT		  BufferLength,
  //     SQLSMALLINT		* StringLength);

  // SQLRETURN SQL_API SQLGetStmtAttr ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLINTEGER		  Attribute,
  //     SQLPOINTER		  Value,
  //     SQLINTEGER		  BufferLength,
  //     SQLINTEGER		* StringLength);

  // SQLRETURN SQL_API SQLGetTypeInfo ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLSMALLINT		  DataType);

  // SQLRETURN SQL_API SQLMoreResults ( // ODBC 1.0 Standards Compliance: ODBC
  //     SQLHSTMT              hstmt)

  // SQLRETURN SQL_API SQLNativeSql ( // ODBC 1.0 Standards Compliance: ODBC
  //     SQLHDBC               hdbc,
  //     SQLCHAR             * szSqlStrIn,
  //     SQLINTEGER            cbSqlStrIn,
  //     SQLCHAR             * szSqlStr,
  //     SQLINTEGER            cbSqlStrMax,
  //     SQLINTEGER          * pcbSqlStr)

  // SQLRETURN SQL_API SQLNumParams ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHSTMT            hstmt,
  //     SQLSMALLINT         * pcpar)

  // SQLRETURN SQL_API SQLNumResultCols ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLSMALLINT		* ColumnCount);

  // SQLRETURN SQL_API SQLParamData ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLPOINTER		* Value);

  SQLRETURN Prepare(SQLHSTMT StatementHandle, SQLCHAR *StatementText, SQLINTEGER TextLength);

  // SQLRETURN SQL_API SQLPrimaryKeys ( // ODBC 1.0 Standards Compliance: ODBC
  //     SQLHSTMT              hstmt,
  //     SQLCHAR             * szCatalogName,
  //     SQLSMALLINT           cbCatalogName,
  //     SQLCHAR             * szSchemaName,
  //     SQLSMALLINT           cbSchemaName,
  //     SQLCHAR             * szTableName,
  //     SQLSMALLINT           cbTableName)

  // SQLRETURN SQL_API SQLProcedureColumns ( // ODBC 1.0 Standards Compliance: ODBC
  //     SQLHSTMT              hstmt,
  //     SQLCHAR             * szCatalogName,
  //     SQLSMALLINT           cbCatalogName,
  //     SQLCHAR             * szSchemaName,
  //     SQLSMALLINT           cbSchemaName,
  //     SQLCHAR             * szProcName,
  //     SQLSMALLINT           cbProcName,
  //     SQLCHAR             * szColumnName,
  //     SQLSMALLINT           cbColumnName)

  // SQLRETURN SQL_API SQLProcedures (  // ODBC 1.0 Standards Compliance: ODBC
  //     SQLHSTMT              hstmt,
  //     SQLCHAR             * szCatalogName,
  //     SQLSMALLINT           cbCatalogName,
  //     SQLCHAR             * szSchemaName,
  //     SQLSMALLINT           cbSchemaName,
  //     SQLCHAR             * szProcName,
  //     SQLSMALLINT           cbProcName)

  // SQLRETURN SQL_API SQLPutData ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLPOINTER		  Data,
  //     SQLLEN		  StrLen_or_Ind);

  SQLRETURN RowCount(SQLHSTMT StatementHandle,
                     SQLLEN *RowCount);

  SQLRETURN SetConnectAttr(SQLHDBC ConnectionHandle,
                           SQLINTEGER Attribute,
                           SQLPOINTER ValuePtr,
                           SQLINTEGER StringLength);

  // SQLRETURN SQL_API SQLSetCursorName ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLCHAR		* CursorName,
  //     SQLSMALLINT		  NameLength);

  // SQLRETURN SQL_API SQLSetDescField ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLHDESC		  DescriptorHandle,
  //     SQLSMALLINT		  RecNumber,
  //     SQLSMALLINT		  FieldIdentifier,
  //     SQLPOINTER		  Value,
  //     SQLINTEGER		  BufferLength);

  // SQLRETURN SQL_API SQLSetDescRec ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLHDESC		  DescriptorHandle,
  //     SQLSMALLINT		  RecNumber,
  //     SQLSMALLINT		  Type,
  //     SQLSMALLINT		  SubType,
  //     SQLLEN		  Length,
  //     SQLSMALLINT		  Precision,
  //     SQLSMALLINT		  Scale,
  //     SQLPOINTER		  Data,
  //     SQLLEN		* StringLength,
  //     SQLLEN		* Indicator);

  SQLRETURN SetEnvAttr(SQLHENV EnvironmentHandle,
                       SQLINTEGER Attribute,
                       SQLPOINTER ValuePtr,
                       SQLINTEGER StringLength);

  // SQLRETURN SQL_API SQLSetPos (// ODBC 1.0 Standards Compliance: ODBC
  //     SQLHSTMT              hstmt,
  //     SQLSETPOSIROW         irow,
  //     SQLUSMALLINT          fOption,
  //     SQLUSMALLINT          fLock)

  // SQLRETURN SQL_API SQLSetStmtAttr ( // ODBC 3.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLINTEGER		  Attribute,
  //     SQLPOINTER		  Value,
  //     SQLINTEGER		  StringLength);

  // SQLRETURN SQL_API SQLSpecialColumns ( // ODBC 1.0 Standards Compliance: Open Group
  //     SQLHSTMT		  StatementHandle,
  //     SQLUSMALLINT	  IdentifierType,
  //     SQLCHAR		* CatalogName,
  //     SQLSMALLINT		  NameLength1,
  //     SQLCHAR		* SchemaName,
  //     SQLSMALLINT		  NameLength2,
  //     SQLCHAR		* TableName,
  //     SQLSMALLINT		  NameLength3,
  //     SQLUSMALLINT	  Scope,
  //     SQLUSMALLINT	  Nullable);

  // SQLRETURN SQL_API SQLStatistics ( // ODBC 1.0 Standards Compliance: ISO 92
  //     SQLHSTMT		  StatementHandle,
  //     SQLCHAR		* CatalogName,
  //     SQLSMALLINT		  NameLength1,
  //     SQLCHAR		* SchemaName,
  //     SQLSMALLINT		  NameLength2,
  //     SQLCHAR		* TableName,
  //     SQLSMALLINT		  NameLength3,
  //     SQLUSMALLINT	  Unique,
  //     SQLUSMALLINT	  Reserved);

  // SQLRETURN SQL_API SQLTablePrivileges ( // ODBC 1.0 Standards Compliance: ODBC
  //     SQLHSTMT              hstmt,
  //     SQLCHAR             * szCatalogName,
  //     SQLSMALLINT           cbCatalogName,
  //     SQLCHAR             * szSchemaName,
  //     SQLSMALLINT           cbSchemaName,
  //     SQLCHAR             * szTableName,
  //     SQLSMALLINT           cbTableName)

  // SQLRETURN SQL_API SQLTables ( // ODBC 1.0 Standards Compliance: Open Group
  //     SQLHSTMT		  StatementHandle,
  //     SQLCHAR		* CatalogName,
  //     SQLSMALLINT		  NameLength1,
  //     SQLCHAR		* SchemaName,
  //     SQLSMALLINT		  NameLength2,
  //     SQLCHAR		* TableName,
  //     SQLSMALLINT		  NameLength3,
  //     SQLCHAR		* TableType,
  //     SQLSMALLINT		  NameLength4);

  std::string Implode(const char *glue, const char *pieces, size_t size, const char *s);
}

#endif // ODBC_H
