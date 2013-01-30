#include "TradeResultDB.H"

namespace TPCE {

  CTradeResultDB::CTradeResultDB() {
  }

  CTradeResultDB::~CTradeResultDB() {
  }

  void CTradeResultDB::DoTradeResultFrame1(const TTradeResultFrame1Input *,
                                           TTradeResultFrame1Output *) {
    cout << "CTradeResultDB::DoTradeResultFrame1/2" << '\r' << endl;
  }

  void CTradeResultDB::DoTradeResultFrame2(const TTradeResultFrame2Input *,
                                           TTradeResultFrame2Output *) {
    cout << "CTradeResultDB::DoTradeResultFrame2/2" << '\r' << endl;
  }

  void CTradeResultDB::DoTradeResultFrame3(const TTradeResultFrame3Input *,
                                           TTradeResultFrame3Output *) {
    cout << "CTradeResultDB::DoTradeResultFrame3/2" << '\r' << endl;
  }

  void CTradeResultDB::DoTradeResultFrame4(const TTradeResultFrame4Input *,
                                           TTradeResultFrame4Output *) {
    cout << "CTradeResultDB::DoTradeResultFrame4/2" << '\r' << endl;
  }

  void CTradeResultDB::DoTradeResultFrame5(const TTradeResultFrame5Input *) {
    cout << "CTradeResultDB::DoTradeResultFrame5/1" << '\r' << endl;
  }

  void CTradeResultDB::DoTradeResultFrame6(const TTradeResultFrame6Input *,
                                           TTradeResultFrame6Output *) {
    cout << "CTradeResultDB::DoTradeResultFrame6/2" << '\r' << endl;
  }

}
