#include "CustomerPositionDB.H"

namespace TPCE {

  CCustomerPositionDB::CCustomerPositionDB() {
  }

  CCustomerPositionDB::~CCustomerPositionDB() {
  }

  void CCustomerPositionDB::DoCustomerPositionFrame1(const TCustomerPositionFrame1Input *,
                                                     TCustomerPositionFrame1Output *) {
    cout << "CCustomerPositionDB::DoCustomerPositionFrame1/2" << '\r' << endl;
  }

  void CCustomerPositionDB::DoCustomerPositionFrame2(const TCustomerPositionFrame2Input *,
                                                     TCustomerPositionFrame2Output *) {
    cout << "CCustomerPositionDB::DoCustomerPositionFrame2/2" << '\r' << endl;
  }

  void CCustomerPositionDB::DoCustomerPositionFrame3(void) {
    cout << "CCustomerPositionDB::DoCustomerPositionFrame3/0" << '\r' << endl;
  }

}
