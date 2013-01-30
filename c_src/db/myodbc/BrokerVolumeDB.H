/*
 */

#ifndef BROKER_VOLUME_DB_H
#define	BROKER_VOLUME_DB_H

#include "ERGenMYODBC.H"

namespace TPCE {

  class CHandleDbc;

  /*
   */
  class CBrokerVolumeDB : public CBrokerVolumeDBInterface {

  private:
    CHandleDbc *m_pDbc;

  public:
    CBrokerVolumeDB(CHandleDbc *hDbc);
    virtual ~CBrokerVolumeDB();

    virtual void DoBrokerVolumeFrame1(const TBrokerVolumeFrame1Input *pIn, TBrokerVolumeFrame1Output *pOut);
  };
}

#endif // BROKER_VOLUME_DB_H
