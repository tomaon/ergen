#include "BrokerVolumeDB.H"

namespace TPCE {

  CBrokerVolumeDB::CBrokerVolumeDB() {
  }

  CBrokerVolumeDB::~CBrokerVolumeDB() {
  }

  void CBrokerVolumeDB::DoBrokerVolumeFrame1(const TBrokerVolumeFrame1Input *,
                                             TBrokerVolumeFrame1Output *) {
    cout << "CBrokerVolumeDB::DoBrokerVolumeFrame1/2" << '\r' << endl;
  }

}
