/*
 * ERGenMEE_drv.C : Market Exchange Emulator
 */

#include "MEE.h"
#include "MEESUTInterface.h"

#include "ERGenDrv.H"

#define MODULE APP "_mee_drv"

namespace TPCE {

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  class CMEESUT : public CMEESUTInterface {
  private:
    ErlDrvPort m_ErlDrvPort;
    CEncoder m_Encoder;

  public:
    CMEESUT(ErlDrvPort ErlDrvPort)
      : CMEESUTInterface(), m_ErlDrvPort(ErlDrvPort), m_Encoder() {}

    virtual ~CMEESUT() {}

    virtual bool TradeResult(PTradeResultTxnInput pTxnInput);
    virtual bool MarketFeed(PMarketFeedTxnInput pTxnInput);
  };

  /*
   */
  bool CMEESUT::TradeResult(PTradeResultTxnInput pTxnInput) {

    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".BH.TR");
        m_Encoder.encode(&x, *pTxnInput);
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
    cout << "[TR] ! " << *pTxnInput << ", result=" << result << '\r' << endl;
#endif

    ei_x_free(&x);

    return result;
  }

  /*
   */
  bool CMEESUT::MarketFeed(PMarketFeedTxnInput pTxnInput) {

    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".BH.MF");
        m_Encoder.encode(&x, *pTxnInput);
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
    cout << "[MF] ! " << *pTxnInput << ", result=" << result << '\r' << endl;
#endif

    ei_x_free(&x);

    return result;
  }

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  struct TMEE {

    CMEE *m_pMEE;
    CMEESUT m_MEESUT;
    CDecoder m_Decoder;
    CLogger m_Logger;
    CInputFiles m_InputFiles;

    TMEE(ErlDrvPort ErlDrvPort,
         UINT32 UniqueId,
         const char *szPathName,
         INT32 iTradingTimeSoFar,
         TIdent iConfiguredCustomerCount,
         TIdent iActiveCustomerCount);
    ~TMEE() {}

    INT32 DoTxn(char *buf, int *index);

    void SetBaseTime() { m_pMEE->SetBaseTime(); }
    INT32 SubmitTradeRequest(char *buf, int *index);
    INT32 GenerateTradeResult() { return m_pMEE->GenerateTradeResult(); }
    bool EnableTickerTape() { return m_pMEE->EnableTickerTape(); }
    bool DisableTickerTape() { return m_pMEE->DisableTickerTape(); }
  };

  /*
   */
  TMEE::TMEE(ErlDrvPort ErlDrvPort,
             UINT32 UniqueId,
             const char *szPathName,
             INT32 iTradingTimeSoFar,
             TIdent iConfiguredCustomerCount,
             TIdent iActiveCustomerCount)
    : m_pMEE(NULL), m_MEESUT(ErlDrvPort), m_Decoder(),
      m_Logger(ErlDrvPort, UniqueId), m_InputFiles() {

    m_InputFiles.Initialize(eDriverMEE,
                            iConfiguredCustomerCount,
                            iActiveCustomerCount,
                            szPathName);

    m_pMEE = new CMEE(iTradingTimeSoFar,
                      &m_MEESUT,
                      &m_Logger,
                      m_InputFiles,
                      UniqueId);
  }

  /*
   */
  INT32 TMEE::DoTxn(char *buf, int *index) {

    INT32 result = 0;
    long command;

    ei_decode_long(buf, index, &command);

    switch ((eCommand)command) {
    case CMD_MEE_SUBMIT_TRADE_RESULT:
      result = SubmitTradeRequest(buf, index);
      break;
    case CMD_MEE_GENERATE_TRADE_RESULT:
      result = GenerateTradeResult();
      break;
    default:
      break;
    }

    return result;
  }

  /*
   */
  INT32 TMEE::SubmitTradeRequest(char *buf, int *index) {

    INT32 result = -1;

    TTradeRequest TradeRequest;
    memset(&TradeRequest, 0x00, sizeof(TradeRequest));

    if (m_Decoder.decode(buf, index, TradeRequest)) {

#ifdef _TRACE
      cout << "[TM] << " << TradeRequest << '\r' << endl;
#endif
      result = m_pMEE->SubmitTradeRequest(&TradeRequest);

#ifdef _TRACE
      cout << "[TM] >> " << result << '\r' << endl;
#endif
    }

    return result;
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
  INT32 trading_time_so_far;
  TIdent configured_customer_count;
  TIdent active_customer_count;
} driver_data_t;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

/*
 */
static void *ergen_driver_new(driver_data_t *data) {
  return new TPCE::TMEE(data->port,
                        data->id,
                        data->path,
                        data->trading_time_so_far,
                        data->configured_customer_count,
                        data->active_customer_count);
}

/*
 */
static void ergen_driver_delete(void *ptr) {
  if (NULL != ptr) {
    TPCE::TMEE *driver = (TPCE::TMEE *)ptr;
    delete driver;
  }
}

/*
 */
static INT32 ergen_driver_do_txn(void *ptr, char *buf, int *index) {
  TPCE::TMEE *driver = (TPCE::TMEE *)ptr;
  return driver->DoTxn(buf, index);
}

/*
 */
static void ergen_driver_set_base_time(void *ptr) {
  TPCE::TMEE *driver = (TPCE::TMEE *)ptr;
  driver->SetBaseTime();
}

/*
 */
static bool ergen_driver_enable_ticker_tape(void *ptr) {
  TPCE::TMEE *driver = (TPCE::TMEE *)ptr;
  return driver->EnableTickerTape();
}

/*
 */
static bool ergen_driver_disable_ticker_tape(void *ptr) {
  TPCE::TMEE *driver = (TPCE::TMEE *)ptr;
  return driver->DisableTickerTape();
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

/*
 */
static void output(ErlDrvData drv_data, char *buf, ErlDrvSizeT) {

  driver_data_t *data = (driver_data_t *)drv_data;

  INT32 result = 0;
  int index = 0, version, arity;

  ei_decode_version(buf, &index, &version);
  ei_decode_tuple_header(buf, &index, &arity);

  if (2 == arity) {

    result = ergen_driver_do_txn(data->driver, buf, &index);

    ergen_drv_output_int32(data->port, result);

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
  case TPCE::CMD_MEE_SET_BASE_TIME:
    {
      ergen_driver_set_base_time(data->driver);

      ei_x_encode_atom(&x, "ok");
    }
    break;
  case TPCE::CMD_MEE_ENABLE_TICKER_TAPE:
    {
      bool result = ergen_driver_enable_ticker_tape(data->driver);

      ei_x_encode_boolean(&x, result);
    }
    break;
  case TPCE::CMD_MEE_DISABLE_TICKER_TAPE:
    {
      bool result = ergen_driver_disable_ticker_tape(data->driver);

      ei_x_encode_boolean(&x, result);
    }
    break;
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
          } else if (0 == strcmp(atom, "trading_time_so_far")) {
            data->trading_time_so_far = term.value.i_val;
          } else if (0 == strcmp(atom, "configured_customer_count")) {
            data->configured_customer_count = term.value.i_val;
          } else if (0 == strcmp(atom, "active_customer_count")) {
            data->active_customer_count = term.value.i_val;
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
      data->trading_time_so_far = 0;
      data->configured_customer_count = dgs_d.iConfiguredCustomerCount;
      data->active_customer_count = dgs_d.iActiveCustomerCount;

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
