#include "TradeStatusDB.H"

namespace TPCE {

  CTradeStatusDB::CTradeStatusDB() {
  }

  CTradeStatusDB::~CTradeStatusDB() {
  }

  void CTradeStatusDB::DoTradeStatusFrame1(const TTradeStatusFrame1Input *,
                                           TTradeStatusFrame1Output *) {
    cout << "CTradeStatusDB::DoTradeStatusFrame1/2" << '\r' << endl;
  }

}
