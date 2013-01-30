#include "SecurityDetailDB.H"

namespace TPCE {

  CSecurityDetailDB::CSecurityDetailDB() {
  }

  CSecurityDetailDB::~CSecurityDetailDB() {
  }

  void CSecurityDetailDB::DoSecurityDetailFrame1(const TSecurityDetailFrame1Input *,
                                                 TSecurityDetailFrame1Output *) {
    cout << "CSecurityDetailDB::DoSecurityDetailFrame1/2" << '\r' << endl;
  }

}
