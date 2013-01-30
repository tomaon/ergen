#include "MarketFeedDB.H"

namespace TPCE {

  CMarketFeedDB::CMarketFeedDB() {
  }

  CMarketFeedDB::~CMarketFeedDB() {
  }

  void CMarketFeedDB::DoMarketFeedFrame1(const TMarketFeedFrame1Input *,
                                         TMarketFeedFrame1Output *,
                                         CSendToMarketInterface *) {
    cout << "CMarketFeedDB::DoMarketFeedFrame1/3" << '\r' << endl;
  }

}
