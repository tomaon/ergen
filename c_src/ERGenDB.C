/*
 * ERGenDB.C
 */

#include "ERGenDB.H"

//#include <dlfcn.h> : TODO

//#include "db/dummy/DBDriver.H"
#include "db/myodbc/DBDriver.H"

namespace TPCE {

  /*
   */
  CDBDriverManager::CDBDriverManager(const char *, const char *) {
    //m_pDBDriver = new CDBDriverDummy();
    m_pDBDriver = new CDBDriverMyODBC();
  }

  /*
   */
  CDBDriverManager::~CDBDriverManager() {
    delete m_pDBDriver;
  }

  /*
   */
  void CDBDriverManager::SetConfig(const char *szName, int iValue) {
    m_pDBDriver->SetConfig(szName, iValue);
  }

  /*
   */
  void CDBDriverManager::SetConfig(const char *szName, long iValue) {
    m_pDBDriver->SetConfig(szName, iValue);
  }

  /*
   */
  void CDBDriverManager::SetConfig(const char *szName, const char *szValue, size_t uValueLen) {
    m_pDBDriver->SetConfig(szName, szValue, uValueLen);
  }

  /*
   */
  CDBConnectionInterface *CDBDriverManager::GetDBConnection() {
    return m_pDBDriver->GetDBConnection();
  }

} // namespace TPCE
