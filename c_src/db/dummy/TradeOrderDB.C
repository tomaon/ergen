#include "TradeOrderDB.H"

namespace TPCE {

  CTradeOrderDB::CTradeOrderDB() {
  }

  CTradeOrderDB::~CTradeOrderDB() {
  }

  void CTradeOrderDB::DoTradeOrderFrame1(const TTradeOrderFrame1Input *,
                                         TTradeOrderFrame1Output *) {
    cout << "CTradeOrderDB::DoTradeOrderFrame1/2" << '\r' << endl;
  }

  void CTradeOrderDB::DoTradeOrderFrame2(const TTradeOrderFrame2Input *,
                                         TTradeOrderFrame2Output *) {
    cout << "CTradeOrderDB::DoTradeOrderFrame2/2" << '\r' << endl;
  }

  void CTradeOrderDB::DoTradeOrderFrame3(const TTradeOrderFrame3Input *,
                                         TTradeOrderFrame3Output *) {
    cout << "CTradeOrderDB::DoTradeOrderFrame3/2" << '\r' << endl;
  }

  void CTradeOrderDB::DoTradeOrderFrame4(const TTradeOrderFrame4Input *,
                                         TTradeOrderFrame4Output *) {
    cout << "CTradeOrderDB::DoTradeOrderFrame4/2" << '\r' << endl;
  }

  void CTradeOrderDB::DoTradeOrderFrame5(void) {
    cout << "CTradeOrderDB::DoTradeOrderFrame5/0" << '\r' << endl;
  }

  void CTradeOrderDB::DoTradeOrderFrame6(void) {
    cout << "CTradeOrderDB::DoTradeOrderFrame6/0" << '\r' << endl;
  }

}
