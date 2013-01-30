/*
 * ERGenCE_drv.C : Customer Emulator
 */

#include "CE.h"
#include "CESUTInterface.h"

#include "ERGenDrv.H"

#define MODULE APP "_ce_drv"

namespace TPCE {

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  class CCESUT : public CCESUTInterface {
  private:
    ErlDrvPort m_ErlDrvPort;
    CEncoder m_Encoder;

  public:
    CCESUT(ErlDrvPort ErlDrvPort)
      : CCESUTInterface(), m_ErlDrvPort(ErlDrvPort), m_Encoder() {}

    virtual ~CCESUT() {}

    virtual bool BrokerVolume(PBrokerVolumeTxnInput pTxnInput);
    virtual bool CustomerPosition(PCustomerPositionTxnInput pTxnInput);
    virtual bool MarketWatch(PMarketWatchTxnInput pTxnInput);
    virtual bool SecurityDetail(PSecurityDetailTxnInput pTxnInput);
    virtual bool TradeLookup(PTradeLookupTxnInput pTxnInput);
    virtual bool TradeOrder(PTradeOrderTxnInput pTxnInput,
                            INT32 iTradeType, bool bExecutorIsAccountOwner);
    virtual bool TradeStatus(PTradeStatusTxnInput pTxnInput);
    virtual bool TradeUpdate(PTradeUpdateTxnInput pTxnInput);
  };

  /*
   */
  bool CCESUT::BrokerVolume(PBrokerVolumeTxnInput pTxnInput) {

    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".BH.BV");
        m_Encoder.encode(&x, *pTxnInput);
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
    cout << "[BV] ! " << *pTxnInput << ", result=" << result << '\r' << endl;
#endif

    ei_x_free(&x);

    return result;
  }

  /*
   */
  bool CCESUT::CustomerPosition(PCustomerPositionTxnInput pTxnInput) {

    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".BH.CP");
        m_Encoder.encode(&x, *pTxnInput);
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
    cout << "[CP] ! " << *pTxnInput << ", result=" << result << '\r' << endl;
#endif

    ei_x_free(&x);

    return result;
  }

  /*
   */
  bool CCESUT::MarketWatch(PMarketWatchTxnInput pTxnInput) {

    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".BH.MW");
        m_Encoder.encode(&x, *pTxnInput);
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
    cout << "[MW] ! " << *pTxnInput << ", result=" << result << '\r' << endl;
#endif

    ei_x_free(&x);

    return result;
  }

  /*
   */
  bool CCESUT::SecurityDetail(PSecurityDetailTxnInput pTxnInput) {

    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".BH.SD");
        m_Encoder.encode(&x, *pTxnInput);
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
    cout << "[SD] ! " << *pTxnInput << ", result=" << result << '\r' << endl;
#endif

    ei_x_free(&x);

    return result;
  }

  /*
   */
  bool CCESUT::TradeLookup(PTradeLookupTxnInput pTxnInput) {

    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".BH.TL");
        m_Encoder.encode(&x, *pTxnInput);
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
    cout << "[TL] ! " << *pTxnInput << ", result=" << result << '\r' << endl;
#endif

    ei_x_free(&x);

    return result;
  }

  /*
   */
  bool CCESUT::TradeOrder(PTradeOrderTxnInput pTxnInput,
                          INT32 iTradeType, bool bExecutorIsAccountOwner) {
    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".BH.TO");
        ei_x_encode_tuple_header(&x, 3);
        {
          m_Encoder.encode(&x, "trade_type", iTradeType);
          m_Encoder.encode(&x, "executor_is_account_owner", bExecutorIsAccountOwner);
          m_Encoder.encode(&x, *pTxnInput);
        }
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
    cout << "[TO] ! " << *pTxnInput << ", result=" << result << '\r' << endl;
#endif

    ei_x_free(&x);

    return result;
  }

  /*
   */
  bool CCESUT::TradeStatus(PTradeStatusTxnInput pTxnInput) {

    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".BH.TS");
        m_Encoder.encode(&x, *pTxnInput);
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
    cout << "[TS] ! " << *pTxnInput << ", result=" << result << '\r' << endl;
#endif

    ei_x_free(&x);

    return result;
  }

  /*
   */
  bool CCESUT::TradeUpdate(PTradeUpdateTxnInput pTxnInput) {

    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".BH.TU");
        m_Encoder.encode(&x, *pTxnInput);
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
    cout << "[TU] ! " << *pTxnInput << ", result=" << result << '\r' << endl;
#endif
    ei_x_free(&x);

    return result;
  }

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  struct TCE {

    CCE *m_pCE;
    CCESUT m_CESUT;
    CLogger m_Logger;
    CInputFiles m_InputFiles;
    TDriverCETxnSettings m_CETxnSettings;

    TCE(ErlDrvPort ErlDrvPort,
        UINT32 UniqueId,
        const char *szPathName,
        TIdent iConfiguredCustomerCount,
        TIdent iActiveCustomerCount,
        INT32 iScaleFactor,
        INT32 iDaysOfInitialTrades,
        INT32 iBrokerVolumeMixLevel,
        INT32 iCustomerPositionMixLevel,
        INT32 iMarketWatchMixLevel,
        INT32 iSecurityDetailMixLevel,
        INT32 iTradeLookupMixLevel,
        INT32 iTradeOrderMixLevel,
        INT32 iTradeStatusMixLevel,
        INT32 iTradeUpdateMixLevel);
    ~TCE() {}

    void DoTxn() { m_pCE->DoTxn(); }
  };

  /*
   */
  TCE::TCE(ErlDrvPort ErlDrvPort,
           UINT32 UniqueId,
           const char *szPathName,
           TIdent iConfiguredCustomerCount,
           TIdent iActiveCustomerCount,
           INT32 iScaleFactor,
           INT32 iDaysOfInitialTrades,
           INT32 iBrokerVolumeMixLevel,
           INT32 iCustomerPositionMixLevel,
           INT32 iMarketWatchMixLevel,
           INT32 iSecurityDetailMixLevel,
           INT32 iTradeLookupMixLevel,
           INT32 iTradeOrderMixLevel,
           INT32 iTradeStatusMixLevel,
           INT32 iTradeUpdateMixLevel)
  : m_pCE(NULL), m_CESUT(ErlDrvPort),
    m_Logger(ErlDrvPort, UniqueId), m_InputFiles(), m_CETxnSettings() {

    m_InputFiles.Initialize(eDriverCE,
                            iConfiguredCustomerCount,
                            iActiveCustomerCount,
                            szPathName);

    TTxnMixGeneratorSettings &rCur= m_CETxnSettings.TxnMixGenerator_settings.cur;
    rCur.BrokerVolumeMixLevel = iBrokerVolumeMixLevel;
    rCur.CustomerPositionMixLevel = iCustomerPositionMixLevel;
    rCur.MarketWatchMixLevel = iMarketWatchMixLevel;
    rCur.SecurityDetailMixLevel = iSecurityDetailMixLevel;
    rCur.TradeLookupMixLevel = iTradeLookupMixLevel;
    rCur.TradeOrderMixLevel = iTradeOrderMixLevel;
    rCur.TradeStatusMixLevel = iTradeStatusMixLevel;
    rCur.TradeUpdateMixLevel = iTradeUpdateMixLevel;

    m_pCE = new CCE(&m_CESUT,
                    &m_Logger,
                    m_InputFiles,
                    iConfiguredCustomerCount,
                    iActiveCustomerCount,
                    iScaleFactor,
                    iDaysOfInitialTrades,
                    UniqueId,
                    &m_CETxnSettings);
  }

} // namespace TPCE

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

typedef struct {
  //
  ErlDrvPort port;
  //
  void *driver;
  //
  UINT32 id;
  char path[PATH_MAX+1];
  TIdent configured_customer_count;
  TIdent active_customer_count;
  INT32 scale_factor;
  INT32 days_of_initial_trades;
  INT32 broker_volume_mix_level;
  INT32 customer_position_mix_level;
  INT32 market_watch_mix_level;
  INT32 security_detail_mix_level;
  INT32 trade_lookup_mix_level;
  INT32 trade_order_mix_level;
  INT32 trade_status_mix_level;
  INT32 trade_update_mix_level;
} driver_data_t;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

/*
 */
static void *ergen_driver_new(driver_data_t *data) {
  return new TPCE::TCE(data->port,
                       data->id,
                       data->path,
                       data->configured_customer_count,
                       data->active_customer_count,
                       data->scale_factor,
                       data->days_of_initial_trades,
                       data->broker_volume_mix_level,
                       data->customer_position_mix_level,
                       data->market_watch_mix_level,
                       data->security_detail_mix_level,
                       data->trade_lookup_mix_level,
                       data->trade_order_mix_level,
                       data->trade_status_mix_level,
                       data->trade_update_mix_level);
}

/*
 */
static void ergen_driver_delete(void *ptr) {
  if (NULL != ptr) {
    TPCE::TCE *driver = (TPCE::TCE *)ptr;
    delete driver;
  }
}

/*
 */
static int ergen_driver_do_txn(void *ptr, char *, int *) {
  TPCE::TCE *driver = (TPCE::TCE *)ptr;
  driver->DoTxn();
  return 1;
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

/*
 */
static void output(ErlDrvData drv_data, char *buf, ErlDrvSizeT) {

  driver_data_t *data = (driver_data_t *)drv_data;

  int result = 0;
  int index = 0, version, arity;

  ei_decode_version(buf, &index, &version);
  ei_decode_tuple_header(buf, &index, &arity);

  if (2 == arity) {

    result = ergen_driver_do_txn(data->driver, buf, &index);

    if(result) {
      ergen_drv_output_bool(data->port, result);
    } else {
      ergen_drv_output_error1(data->port, "badmatch", "cmd");
    }

    return;
  }

  ergen_drv_output_error0(data->port, "badarg");
}

/*
 */
static ErlDrvSSizeT control(ErlDrvData drv_data, unsigned int command,
                            char *buf, ErlDrvSizeT, char **rbuf, ErlDrvSizeT) {

  driver_data_t *data = (driver_data_t *)drv_data;

  ei_x_buff x;

  ei_x_new_with_version(&x);

  switch ((TPCE::eCommand)command) {
  case TPCE::CMD_ALL_INIT:
    {
      int index = 0, version, arity;
      char atom[MAXATOMLEN+1];
      bool found = true;

      ei_decode_version(buf, &index, &version);
      ei_decode_list_header(buf, &index, &arity);

      for (int size = arity, i = 0; found && i < size; i++) {

        ei_decode_tuple_header(buf, &index, &arity);

        if (2 == arity) {

          ei_term term;

          ei_decode_atom(buf, &index, atom);
          ei_decode_ei_term(buf, &index, &term);

          if (0 == strcmp(atom, "id")) {
            data->id = term.value.i_val;
          } else if (0 == strcmp(atom, "path") && ERL_STRING_EXT == term.ei_type) {
            char v[term.size+1];
            ei_decode_string(buf, &index, v);
            strncpy(data->path, v, sizeof(data->path));
          } else if (0 == strcmp(atom, "path") && ERL_BINARY_EXT == term.ei_type) {
            char v[term.size+1];
            long l = term.size;
            ei_decode_binary(buf, &index, v, &l); v[l] = '\0';
            strncpy(data->path, v, sizeof(data->path));
          } else if (0 == strcmp(atom, "configured_customer_count")) {
            data->configured_customer_count = term.value.i_val;
          } else if (0 == strcmp(atom, "active_customer_count")) {
            data->active_customer_count = term.value.i_val;
          } else if (0 == strcmp(atom, "scale_factor")) {
            data->scale_factor = term.value.i_val;
          } else if (0 == strcmp(atom, "days_of_initial_trades")) {
            data->days_of_initial_trades = term.value.i_val;
          } else if (0 == strcmp(atom, "broker_volume_mix_level")) {
            data->broker_volume_mix_level = term.value.i_val;
          } else if (0 == strcmp(atom, "customer_position_mix_level")) {
            data->customer_position_mix_level = term.value.i_val;
          } else if (0 == strcmp(atom, "market_watch_mix_level")) {
            data->market_watch_mix_level = term.value.i_val;
          } else if (0 == strcmp(atom, "security_detail_mix_level")) {
            data->security_detail_mix_level = term.value.i_val;
          } else if (0 == strcmp(atom, "trade_lookup_mix_level")) {
            data->trade_lookup_mix_level = term.value.i_val;
          } else if (0 == strcmp(atom, "trade_order_mix_level")) {
            data->trade_order_mix_level = term.value.i_val;
          } else if (0 == strcmp(atom, "trade_status_mix_level")) {
            data->trade_status_mix_level = term.value.i_val;
          } else if (0 == strcmp(atom, "trade_update_mix_level")) {
            data->trade_update_mix_level = term.value.i_val;
          } else {
            found = false;
          }
        }
      }

      if (found) {
        ei_x_encode_atom(&x, "ok");
      } else {
        ergen_drv_encode_error1(&x, "badmatch", atom);
      }
    }
    break;
  case TPCE::CMD_ALL_CONFIG:
    {
      ei_x_encode_atom(&x, "ok");
    }
    break;
  case TPCE::CMD_ALL_ALLOC:
    {
      void *ptr = ergen_driver_new(data);

      if (NULL != ptr) {

        data->driver = ptr;

        ei_x_encode_atom(&x, "ok");

      } else {
        ergen_drv_encode_error0(&x, "nomem");
      }
    }
    break;
  case TPCE::CMD_ALL_FREE:
    {
      void *ptr = data->driver;

      if (NULL != ptr) {
        ergen_driver_delete(ptr);
        data->driver = NULL;
      }

      ei_x_encode_atom(&x, "ok");
    }
    break;
  default:
    {
      ergen_drv_encode_error0(&x, "badarg");
    }
    break;
  }

  ErlDrvSizeT result = x.index;
  *rbuf = (char *)ergen_drv_bindup(x.buff, result);

  ei_x_free(&x);

  return result;
}

/*
 */
static ErlDrvData start(ErlDrvPort port, char *) {

  if (NULL != port) {

    void *ptr = driver_alloc(sizeof(driver_data_t));

    if (NULL != ptr) {

      set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

      driver_data_t *data = (driver_data_t *)ptr;

      data->port = port;

      strcpy(data->path, "flat_in/"); // << EGenLoader.cpp: FLAT_IN_PATH

      TPCE::CDriverGlobalSettings dgs;
      TPCE::TDriverGlobalSettings &dgs_d = dgs.dft;
      data->configured_customer_count = dgs_d.iConfiguredCustomerCount;
      data->active_customer_count = dgs_d.iActiveCustomerCount;
      data->scale_factor = dgs_d.iScaleFactor;
      data->days_of_initial_trades = dgs_d.iDaysOfInitialTrades;

      TPCE::CTxnMixGeneratorSettings tmgs;
      TPCE::TTxnMixGeneratorSettings &tmgs_d = tmgs.dft;
      data->broker_volume_mix_level = tmgs_d.BrokerVolumeMixLevel;
      data->customer_position_mix_level = tmgs_d.CustomerPositionMixLevel;
      data->market_watch_mix_level = tmgs_d.MarketWatchMixLevel;
      data->security_detail_mix_level = tmgs_d.SecurityDetailMixLevel;
      data->trade_lookup_mix_level = tmgs_d.TradeLookupMixLevel;
      data->trade_order_mix_level = tmgs_d.TradeOrderMixLevel;
      data->trade_status_mix_level = tmgs_d.TradeStatusMixLevel;
      data->trade_update_mix_level = tmgs_d.TradeUpdateMixLevel;

      return (ErlDrvData)data;
    }
  }

  return ERL_DRV_ERROR_GENERAL;
}

/*
 */
static void stop(ErlDrvData drv_data) {

  void *ptr = drv_data;

  if (NULL != ptr) {
    driver_free(ptr);
  }
}

/*
 */
static ErlDrvEntry driver_entry = {
  NULL,                           // init
  start,                          // start
  stop,                           // stop
  output,                         // output
  NULL,                           // ready_input
  NULL,                           // ready_output
  (char *)("lib" MODULE),         // driver_name
  NULL,                           // finish
  NULL,                           // handle (reserved)
  control,                        // control
  NULL,                           // timeout
  NULL,                           // outputv
  NULL,                           // ready_async
  NULL,                           // flush
  NULL,                           // call
  NULL,                           // event
  ERL_DRV_EXTENDED_MARKER,        // extended_marker
  ERL_DRV_EXTENDED_MAJOR_VERSION, // major_version
  ERL_DRV_EXTENDED_MINOR_VERSION, // minor_version
  ERL_DRV_FLAG_USE_PORT_LOCKING,  // driver_flags
  NULL,                           // handle2 (reserved)
  NULL,                           // process_exit
  NULL,                           // stop_select
};

/*
 */
extern "C" DRIVER_INIT(driver) {
  return &driver_entry;
}
