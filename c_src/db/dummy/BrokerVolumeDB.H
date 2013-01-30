#ifndef BROKER_VOLUME_DB_H
#define	BROKER_VOLUME_DB_H

#include "TxnHarnessDBInterface.h"

namespace TPCE {

  class CBrokerVolumeDB : public CBrokerVolumeDBInterface {
  public:
    CBrokerVolumeDB();
    virtual ~CBrokerVolumeDB();

    virtual void DoBrokerVolumeFrame1(const TBrokerVolumeFrame1Input *pIn, TBrokerVolumeFrame1Output *pOut);
  };
}

#endif
