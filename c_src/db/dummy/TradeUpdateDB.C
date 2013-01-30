#include "TradeUpdateDB.H"

namespace TPCE {

  CTradeUpdateDB::CTradeUpdateDB() {
  }

  CTradeUpdateDB::~CTradeUpdateDB() {
  }

  void CTradeUpdateDB::DoTradeUpdateFrame1(const TTradeUpdateFrame1Input *,
                                           TTradeUpdateFrame1Output *) {
    cout << "CTradeUpdateDB::DoTradeUpdateFrame1/2" << '\r' << endl;
  }

  void CTradeUpdateDB::DoTradeUpdateFrame2(const TTradeUpdateFrame2Input *,
                                           TTradeUpdateFrame2Output *) {
    cout << "CTradeUpdateDB::DoTradeUpdateFrame2/2" << '\r' << endl;
  }

  void CTradeUpdateDB::DoTradeUpdateFrame3(const TTradeUpdateFrame3Input *,
                                           TTradeUpdateFrame3Output *) {
    cout << "CTradeUpdateDB::DoTradeUpdateFrame3/2" << '\r' << endl;
  }

}
