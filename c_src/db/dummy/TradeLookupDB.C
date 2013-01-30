#include "TradeLookupDB.H"

namespace TPCE {

  CTradeLookupDB::CTradeLookupDB() {
  }

  CTradeLookupDB::~CTradeLookupDB() {
  }

  void CTradeLookupDB::DoTradeLookupFrame1(const TTradeLookupFrame1Input *,
                                           TTradeLookupFrame1Output *) {
    cout << "CTradeLookupDB::DoTradeLookupFrame1/2" << '\r' << endl;
  }

  void CTradeLookupDB::DoTradeLookupFrame2(const TTradeLookupFrame2Input *,
                                           TTradeLookupFrame2Output *) {
    cout << "CTradeLookupDB::DoTradeLookupFrame2/2" << '\r' << endl;
  }

  void CTradeLookupDB::DoTradeLookupFrame3(const TTradeLookupFrame3Input *,
                                           TTradeLookupFrame3Output *) {
    cout << "CTradeLookupDB::DoTradeLookupFrame3/2" << '\r' << endl;
  }

  void CTradeLookupDB::DoTradeLookupFrame4(const TTradeLookupFrame4Input *,
                                           TTradeLookupFrame4Output *) {
    cout << "CTradeLookupDB::DoTradeLookupFrame4/2" << '\r' << endl;
  }

}
