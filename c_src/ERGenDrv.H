/*
 * ERGenDrv.H
 */

#ifndef ERGEN_DRV_H
#define ERGEN_DRV_H

#include "erl_driver.h"
#include "erl_interface.h"
#include "ei.h"

#include "BaseLogger.h"
#include "TxnHarnessDBInterface.h"

#include "ERGen.H"

namespace TPCE {

  /*
   */
  class CEncoder {
  public:
    CEncoder() {}

    virtual ~CEncoder() {}

    virtual void encode(ei_x_buff *x, const char *name, const bool& value);
    virtual void encode(ei_x_buff *x, const char *name, const char& value);
    virtual void encode(ei_x_buff *x, const char *name, const double& value);
    virtual void encode(ei_x_buff *x, const char *name, const eMEETradeRequestAction& value);
    virtual void encode(ei_x_buff *x, const char *name, const INT32& value);
    virtual void encode(ei_x_buff *x, const char *name, const INT64& value);
    virtual void encode(ei_x_buff *x, const char *name, const TIMESTAMP_STRUCT& value);
    virtual void encode(ei_x_buff *x, const char *name, const TTickerEntry& value);
    virtual void encode(ei_x_buff *x, const char *name, const TStatusAndTradeType& value);

    virtual void encode(ei_x_buff *x, const char *name, const char value[], long len);

    virtual void encode(ei_x_buff *x, const TTradeRequest& value);
    virtual void encode(ei_x_buff *x, const TBrokerVolumeTxnInput& value);
    virtual void encode(ei_x_buff *x, const TCustomerPositionTxnInput& value);
    virtual void encode(ei_x_buff *x, const TDataMaintenanceTxnInput& value);
    virtual void encode(ei_x_buff *x, const TMarketFeedTxnInput& value);
    virtual void encode(ei_x_buff *x, const TMarketWatchTxnInput& value);
    virtual void encode(ei_x_buff *x, const TSecurityDetailTxnInput& value);
    virtual void encode(ei_x_buff *x, const TTradeCleanupTxnInput& value);
    virtual void encode(ei_x_buff *x, const TTradeLookupTxnInput& value);
    virtual void encode(ei_x_buff *x, const TTradeOrderTxnInput& value);
    virtual void encode(ei_x_buff *x, const TTradeResultTxnInput& value);
    virtual void encode(ei_x_buff *x, const TTradeStatusTxnInput& value);
    virtual void encode(ei_x_buff *x, const TTradeUpdateTxnInput& value);
  };

  /*
   */
  class CDecoder {
  public:
    CDecoder() {}

    virtual ~CDecoder() {}

    virtual bool decode(char *buf, int *index, const char *name, bool& value);
    virtual bool decode(char *buf, int *index, const char *name, char& value);
    virtual bool decode(char *buf, int *index, const char *name, double& value);
    virtual bool decode(char *buf, int *index, const char *name, eMEETradeRequestAction& value);
    virtual bool decode(char *buf, int *index, const char *name, INT32& value);
    virtual bool decode(char *buf, int *index, const char *name, INT64& value);
    virtual bool decode(char *buf, int *index, const char *name, TIMESTAMP_STRUCT& value);
    virtual bool decode(char *buf, int *index, const char *name, TTickerEntry& value);
    virtual bool decode(char *buf, int *index, const char *name, TStatusAndTradeType& value);

    virtual bool decode(char *buf, int *index, const char *name, char value[], long size);

    virtual bool decode(char *buf, int *index, char value[], long size);
    virtual bool decode(char *buf, int *index, TTradeRequest& value);
    virtual bool decode(char *buf, int *index, TBrokerVolumeTxnInput& value);
    virtual bool decode(char *buf, int *index, TCustomerPositionTxnInput& value);
    virtual bool decode(char *buf, int *index, TDataMaintenanceTxnInput& value);
    virtual bool decode(char *buf, int *index, TMarketFeedTxnInput& value);
    virtual bool decode(char *buf, int *index, TMarketWatchTxnInput& value);
    virtual bool decode(char *buf, int *index, TSecurityDetailTxnInput& value);
    virtual bool decode(char *buf, int *index, TTradeCleanupTxnInput& value);
    virtual bool decode(char *buf, int *index, TTradeLookupTxnInput& value);
    virtual bool decode(char *buf, int *index, TTradeOrderTxnInput& value);
    virtual bool decode(char *buf, int *index, TTradeResultTxnInput& value);
    virtual bool decode(char *buf, int *index, TTradeStatusTxnInput& value);
    virtual bool decode(char *buf, int *index, TTradeUpdateTxnInput& value);
  };

  /*
   */
  class CLogger : public CBaseLogger {
  private:
    ErlDrvPort m_ErlDrvPort;

  protected:
    virtual bool SendToLoggerImpl(const char *szPrefix, const char *szTimestamp, const char *szMsg);

  public:
    CLogger(ErlDrvPort ErlDrvPort, INT32 UniqueId);

    virtual ~CLogger();
  };

} // namespace TPCE

void ergen_drv_encode_error0(ei_x_buff *x, const char *error);
void ergen_drv_encode_error1(ei_x_buff *x, const char *error, const char *arg1);

ErlDrvBinary *ergen_drv_bindup(void *p, ErlDrvSizeT size);

void ergen_drv_output_ok(ErlDrvPort port);
void ergen_drv_output_bool(ErlDrvPort port, bool value);
void ergen_drv_output_int32(ErlDrvPort port, INT32 value);
void ergen_drv_output_error0(ErlDrvPort port, const char *error);
void ergen_drv_output_error1(ErlDrvPort port, const char *error, const char *arg1);

#endif // ERGEN_DRV_H
