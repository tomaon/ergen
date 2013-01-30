#include "MarketWatchDB.H"

namespace TPCE {

  CMarketWatchDB::CMarketWatchDB() {
  }

  CMarketWatchDB::~CMarketWatchDB() {
  }

  void CMarketWatchDB::DoMarketWatchFrame1(const TMarketWatchFrame1Input *,
                                           TMarketWatchFrame1Output *) {
    cout << "CMarketWatchDB::DoMarketWatchFrame1/2" << '\r' << endl;
  }

}
