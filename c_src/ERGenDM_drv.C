/*
 * ERGenDM_drv.C : Data Maintenance
 */

#include "DM.h"
#include "DMSUTInterface.h"

#include "ERGenDrv.H"

#define MODULE APP "_dm_drv"

namespace TPCE {

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  class CDMSUT : public CDMSUTInterface {
  private:
    ErlDrvPort m_ErlDrvPort;
    CEncoder m_Encoder;

  public:
    CDMSUT(ErlDrvPort ErlDrvPort)
      : CDMSUTInterface(), m_ErlDrvPort(ErlDrvPort), m_Encoder() {}

    virtual ~CDMSUT() {}

    virtual bool DataMaintenance(PDataMaintenanceTxnInput pTxnInput);
    virtual bool TradeCleanup(PTradeCleanupTxnInput pTxnInput);
  };

  /*
   */
  bool CDMSUT::DataMaintenance(PDataMaintenanceTxnInput pTxnInput) {

    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".BH.DM");
        m_Encoder.encode(&x, *pTxnInput);
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
    cout << "[DM] ! " << *pTxnInput << ", result=" << result << '\r' << endl;
#endif

    ei_x_free(&x);

    return result;
  }

  /*
   */
  bool CDMSUT::TradeCleanup(PTradeCleanupTxnInput pTxnInput) {

    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".BH.TC");
        m_Encoder.encode(&x, *pTxnInput);
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
    cout << "[TC] ! " << *pTxnInput << ", result=" << result << '\r' << endl;
#endif

    ei_x_free(&x);

    return result;
  }

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  struct TDM {

    CDM *m_pDM;
    CDMSUT m_DMSUT;
    CLogger m_Logger;
    CInputFiles m_InputFiles;

    TDM(ErlDrvPort ErlDrvPort,
        UINT32 UniqueId,
        const char *szPathName,
        TIdent iConfiguredCustomerCount,
        TIdent iActiveCustomerCount,
        INT32 iScaleFactor,
        INT32 iDaysOfInitialTrades);
    ~TDM() {}

    bool DoTxn(char *buf, int *index);
  };

  /*
   */
  TDM::TDM(ErlDrvPort ErlDrvPort,
           UINT32 UniqueId,
           const char *szPathName,
           TIdent iConfiguredCustomerCount,
           TIdent iActiveCustomerCount,
           INT32 iScaleFactor,
           INT32 iDaysOfInitialTrades)
    : m_pDM(NULL), m_DMSUT(ErlDrvPort),
      m_Logger(ErlDrvPort, UniqueId), m_InputFiles() {

    m_InputFiles.Initialize(eDriverDM,
                            iConfiguredCustomerCount,
                            iActiveCustomerCount,
                            szPathName);

    m_pDM = new CDM(&m_DMSUT,
                    &m_Logger,
                    m_InputFiles,
                    iConfiguredCustomerCount,
                    iActiveCustomerCount,
                    iScaleFactor,
                    iDaysOfInitialTrades,
                    UniqueId);
  }

  /*
   */
  bool TDM::DoTxn(char *buf, int *index) {

    bool result = false;
    long command;

    ei_decode_long(buf, index, &command);

    switch ((eCommand)command) {
    case CMD_DM_DO_TXN:
      m_pDM->DoTxn();
      result = true;
      break;
    case CMD_DM_DO_CLEANUP_TXN:
      m_pDM->DoCleanupTxn();
      result = true;
      break;
    default:
      break;
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
  TIdent configured_customer_count;
  TIdent active_customer_count;
  INT32 scale_factor;
  INT32 days_of_initial_trades;
} driver_data_t;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

/*
 */
static void *ergen_driver_new(driver_data_t *data) {
  return new TPCE::TDM(data->port,
                       data->id,
                       data->path,
                       data->configured_customer_count,
                       data->active_customer_count,
                       data->scale_factor,
                       data->days_of_initial_trades);
}

/*
 */
static void ergen_driver_delete(void *ptr) {
  if (NULL != ptr) {
    TPCE::TDM *driver = (TPCE::TDM *)ptr;
    delete driver;
  }
}

/*
 */
static int ergen_driver_do_txn(void *ptr, char *buf, int *index) {
  TPCE::TDM *driver = (TPCE::TDM *)ptr;
  return driver->DoTxn(buf, index);
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
