/*
 * ERGenBH_drv.C : Brokerage House
 */

#include <algorithm> // find

#include "TxnHarnessBrokerVolume.h"
#include "TxnHarnessCustomerPosition.h"
#include "TxnHarnessDataMaintenance.h"
#include "TxnHarnessMarketFeed.h"
#include "TxnHarnessMarketWatch.h"
#include "TxnHarnessSecurityDetail.h"
#include "TxnHarnessSendToMarketInterface.h"
#include "TxnHarnessTradeCleanup.h"
#include "TxnHarnessTradeLookup.h"
#include "TxnHarnessTradeOrder.h"
#include "TxnHarnessTradeResult.h"
#include "TxnHarnessTradeStatus.h"
#include "TxnHarnessTradeUpdate.h"

#include "ERGenDB.H"
#include "ERGenDrv.H"

#define MODULE APP "_bh_drv"

namespace TPCE {

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  class CSendToMarket : public CSendToMarketInterface {
  private:
    ErlDrvPort m_ErlDrvPort;
    CEncoder m_Encoder;

  protected:
    virtual bool SendToMarket(TTradeRequest &trade_mes);

  public:
    CSendToMarket(ErlDrvPort ErlDrvPort)
      : CSendToMarketInterface(), m_ErlDrvPort(ErlDrvPort), m_Encoder() {}

    virtual ~CSendToMarket() {}
  };

  /*
   */
  bool CSendToMarket::SendToMarket(TTradeRequest& trade_mes) {

    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".MEE.TM");
        m_Encoder.encode(&x, trade_mes);
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
    cout << "[TM] ! " << trade_mes << ", result=" << result << '\r' << endl;
#endif

    ei_x_free(&x);

    return result;
  }

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  class CBH {
  private:
    CSendToMarket m_SendToMarket;
    CDecoder m_Decoder;

  protected:
    bool DoTxn(char *buf, int *index, CBrokerVolumeDBInterface *pDB);
    bool DoTxn(char *buf, int *index, CCustomerPositionDBInterface *pDB);
    bool DoTxn(char *buf, int *index, CDataMaintenanceDBInterface *pDB);
    bool DoTxn(char *buf, int *index, CMarketFeedDBInterface *pDB);
    bool DoTxn(char *buf, int *index, CMarketWatchDBInterface *pDB);
    bool DoTxn(char *buf, int *index, CSecurityDetailDBInterface *pDB);
    bool DoTxn(char *buf, int *index, CTradeCleanupDBInterface *pDB);
    bool DoTxn(char *buf, int *index, CTradeLookupDBInterface *pDB);
    bool DoTxn(char *buf, int *index, CTradeOrderDBInterface *pDB);
    bool DoTxn(char *buf, int *index, CTradeResultDBInterface *pDB);
    bool DoTxn(char *buf, int *index, CTradeStatusDBInterface *pDB);
    bool DoTxn(char *buf, int *index, CTradeUpdateDBInterface *pDB);

  public:
    CBH(ErlDrvPort ErlDrvPort) : m_SendToMarket(ErlDrvPort), m_Decoder() {}
    ~CBH() {}

    bool DoTxn(char *buf, int *index, CDBConnectionInterface *pDBConnection) {

      long command;

      ei_decode_long(buf, index, &command);

      switch ((eCommand)command) {
      case CMD_BH_DO_TXN_TS: // MixLevel=190
        return DoTxn(buf, index, pDBConnection->TradeStatusDB());
      case CMD_BH_DO_TXN_MW: //         =180
        return DoTxn(buf, index, pDBConnection->MarketWatchDB());
      case CMD_BH_DO_TXN_SD: //         =140
        return DoTxn(buf, index, pDBConnection->SecurityDetailDB());
      case CMD_BH_DO_TXN_CP: //         =130
        return DoTxn(buf, index, pDBConnection->CustomerPositionDB());
      case CMD_BH_DO_TXN_TO: //         =101
        return DoTxn(buf, index, pDBConnection->TradeOrderDB());
      case CMD_BH_DO_TXN_TL: //         = 80
        return DoTxn(buf, index, pDBConnection->TradeLookupDB());
      case CMD_BH_DO_TXN_BV: //         = 49
        return DoTxn(buf, index, pDBConnection->BrokerVolumeDB());
      case CMD_BH_DO_TXN_TU: //         = 20
        return DoTxn(buf, index, pDBConnection->TradeUpdateDB());
      case CMD_BH_DO_TXN_TR: //         = ?
        return DoTxn(buf, index, pDBConnection->TradeResultDB());
      case CMD_BH_DO_TXN_MF: //         = ?
        return DoTxn(buf, index, pDBConnection->MarketFeedDB());
      case CMD_BH_DO_TXN_DM: //         = ?
        return DoTxn(buf, index, pDBConnection->DataMaintenanceDB());
      case CMD_BH_DO_TXN_TC: //         = 1
        return DoTxn(buf, index, pDBConnection->TradeCleanupDB());
      default:
        return false;
      }
    }

  };

  /*
   */
  bool CBH::DoTxn(char *buf, int *index, CBrokerVolumeDBInterface *pDB) {

    bool result = false;

    if (NULL != pDB) {

      CBrokerVolume Txn(pDB);

      TBrokerVolumeTxnInput TxnInput;
      memset(&TxnInput, 0x00, sizeof(TxnInput));

      TBrokerVolumeTxnOutput TxnOutput;
      memset(&TxnOutput, 0x00, sizeof(TxnOutput));

      if (m_Decoder.decode(buf, index, TxnInput)) {

#ifdef _TRACE
        cout << "[BV] << " << TxnInput << '\r' << endl;
#endif
        Txn.DoTxn(&TxnInput, &TxnOutput);

#ifdef _TRACE
        cout << "[BV] >> " << TxnOutput << '\r' << endl;
#endif
        result = true;
      }
    }

    return result;
  }

  /*
   */
  bool CBH::DoTxn(char *buf, int *index, CCustomerPositionDBInterface *pDB) {

    bool result = false;

    if (NULL != pDB) {

      CCustomerPosition Txn(pDB);

      TCustomerPositionTxnInput TxnInput;
      memset(&TxnInput, 0x00, sizeof(TxnInput));

      TCustomerPositionTxnOutput TxnOutput;
      memset(&TxnOutput, 0x00, sizeof(TxnOutput));

      if (m_Decoder.decode(buf, index, TxnInput)) {

#ifdef _TRACE
        cout << "[CP] << " << TxnInput << '\r' << endl;
#endif

        Txn.DoTxn(&TxnInput, &TxnOutput);

#ifdef _TRACE
        cout << "[CP] >> " << TxnOutput << '\r' << endl;
#endif
        result = true;
      }
    }

    return result;
  }

  /*
   */
  bool CBH::DoTxn(char *buf, int *index, CDataMaintenanceDBInterface *pDB) {

    bool result = false;

    if (NULL != pDB) {

      CDataMaintenance Txn(pDB);

      TDataMaintenanceTxnInput TxnInput;
      memset(&TxnInput, 0x00, sizeof(TxnInput));

      TDataMaintenanceTxnOutput TxnOutput;
      memset(&TxnOutput, 0x00, sizeof(TxnOutput));

      if (m_Decoder.decode(buf, index, TxnInput)) {

#ifdef _TRACE
        cout << "[DM] << " << TxnInput << '\r' << endl;
#endif
        Txn.DoTxn(&TxnInput, &TxnOutput);

#ifdef _TRACE
        cout << "[DM] >> " << TxnOutput << '\r' << endl;
#endif
        result = true;
      }
    }

    return result;
  }

  /*
   */
  bool CBH::DoTxn(char *buf, int *index, CMarketFeedDBInterface *pDB) {

    bool result = false;

    if (NULL != pDB) {

      CMarketFeed Txn(pDB, &m_SendToMarket);

      TMarketFeedTxnInput TxnInput;
      memset(&TxnInput, 0x00, sizeof(TxnInput));

      TMarketFeedTxnOutput TxnOutput;
      memset(&TxnOutput, 0x00, sizeof(TxnOutput));

      if (m_Decoder.decode(buf, index, TxnInput)) {

#ifdef _TRACE
        cout << "[MF] << " << TxnInput << '\r' << endl;
#endif
        Txn.DoTxn(&TxnInput, &TxnOutput);

#ifdef _TRACE
        cout << "[MF] >> " << TxnOutput << '\r' << endl;
#endif
        result = true;
      }
    }

    return result;
  }

  /*
   */
  bool CBH::DoTxn(char *buf, int *index, CMarketWatchDBInterface *pDB) {

    bool result = false;

    if (NULL != pDB) {

      CMarketWatch Txn(pDB);

      TMarketWatchTxnInput TxnInput;
      memset(&TxnInput, 0x00, sizeof(TxnInput));

      TMarketWatchTxnOutput TxnOutput;
      memset(&TxnOutput, 0x00, sizeof(TxnOutput));

      if (m_Decoder.decode(buf, index, TxnInput)) {

#ifdef _TRACE
        cout << "[MW] << " << TxnInput << '\r' << endl;
#endif
        Txn.DoTxn(&TxnInput, &TxnOutput);

#ifdef _TRACE
        cout << "[MW] >> " << TxnOutput << '\r' << endl;
#endif
        result = true;
      }
    }

    return result;
  }

  /*
   */
  bool CBH::DoTxn(char *buf, int *index, CSecurityDetailDBInterface *pDB) {

    bool result = false;

    if (NULL != pDB) {

      CSecurityDetail Txn(pDB);

      TSecurityDetailTxnInput TxnInput;
      memset(&TxnInput, 0x00, sizeof(TxnInput));

      TSecurityDetailTxnOutput TxnOutput;
      memset(&TxnOutput, 0x00, sizeof(TxnOutput));

      if (m_Decoder.decode(buf, index, TxnInput)) {

#ifdef _TRACE
        cout << "[SD] << " << TxnInput << '\r' << endl;
#endif
        Txn.DoTxn(&TxnInput, &TxnOutput);

#ifdef _TRACE
        cout << "[SD] >> " << TxnOutput << '\r' << endl;
#endif
        result = true;
      }
    }

    return result;
  }

  /*
   */
  bool CBH::DoTxn(char *buf, int *index, CTradeCleanupDBInterface *pDB) {

    bool result = false;

    if (NULL != pDB) {

      CTradeCleanup Txn(pDB);

      TTradeCleanupTxnInput TxnInput;
      memset(&TxnInput, 0x00, sizeof(TxnInput));

      TTradeCleanupTxnOutput TxnOutput;
      memset(&TxnOutput, 0x00, sizeof(TxnOutput));

      if (m_Decoder.decode(buf, index, TxnInput)) {

#ifdef _TRACE
        cout << "[TC] << " << TxnInput << '\r' << endl;
#endif
        Txn.DoTxn(&TxnInput, &TxnOutput);

#ifdef _TRACE
        cout << "[TC] >> " << TxnOutput << '\r' << endl;
#endif
        result = true;
      }
    }

    return result;
  }

  /*
   */
  bool CBH::DoTxn(char *buf, int *index, CTradeLookupDBInterface *pDB) {

    bool result = false;

    if (NULL != pDB) {

      CTradeLookup Txn(pDB);

      TTradeLookupTxnInput TxnInput;
      memset(&TxnInput, 0x00, sizeof(TxnInput));

      TTradeLookupTxnOutput TxnOutput;
      memset(&TxnOutput, 0x00, sizeof(TxnOutput));

      if (m_Decoder.decode(buf, index, TxnInput)) {

#ifdef _TRACE
        cout << "[TL] << " << TxnInput << '\r' << endl;
#endif
        Txn.DoTxn(&TxnInput, &TxnOutput);

#ifdef _TRACE
        cout << "[TL] >> " << TxnOutput << '\r' << endl;
#endif
        result = true;
      }
    }

    return result;
  }

  /*
   */
  bool CBH::DoTxn(char *buf, int *index, CTradeOrderDBInterface *pDB) {

    bool result = false;

    if (NULL != pDB) {

      int arity;

      ei_decode_tuple_header(buf, index, &arity);

      if (3 == arity) {

        INT32 iTradeType = 0;             // TODO
        bool bExecutorIsAccountOwner = 0; // TODO

        m_Decoder.decode(buf, index, "trade_type", iTradeType);
        m_Decoder.decode(buf, index, "executor_is_account_owner", bExecutorIsAccountOwner);

        CTradeOrder Txn(pDB, &m_SendToMarket);

        TTradeOrderTxnInput TxnInput;
        memset(&TxnInput, 0x00, sizeof(TxnInput));

        TTradeOrderTxnOutput TxnOutput;
        memset(&TxnOutput, 0x00, sizeof(TxnOutput));

        if (m_Decoder.decode(buf, index, TxnInput)) {

#ifdef _TRACE
          cout << "[TO] << " << TxnInput << '\r' << endl;
#endif
          Txn.DoTxn(&TxnInput, &TxnOutput);

#ifdef _TRACE
          cout << "[TO] >> " << TxnOutput << '\r' << endl;
#endif
          result = true;
        }
      }
    }

    return result;
  }

  /*
   */
  bool CBH::DoTxn(char *buf, int *index, CTradeResultDBInterface *pDB) {

    bool result = false;

    if (NULL != pDB) {

      CTradeResult Txn(pDB);

      TTradeResultTxnInput TxnInput;
      memset(&TxnInput, 0x00, sizeof(TxnInput));

      TTradeResultTxnOutput TxnOutput;
      memset(&TxnOutput, 0x00, sizeof(TxnOutput));

      if (m_Decoder.decode(buf, index, TxnInput)) {

#ifdef _TRACE
        cout << "[TR] << " << TxnInput << '\r' << endl;
#endif
        Txn.DoTxn(&TxnInput, &TxnOutput);

#ifdef _TRACE
        cout << "[TR] >> " << TxnOutput << '\r' << endl;
#endif
        result = true;
      }
    }

    return result;
  }

  /*
   */
  bool CBH::DoTxn(char *buf, int *index, CTradeStatusDBInterface *pDB) {

    bool result = false;

    if (NULL != pDB) {

      CTradeStatus Txn(pDB);

      TTradeStatusTxnInput TxnInput;
      memset(&TxnInput, 0x00, sizeof(TxnInput));

      TTradeStatusTxnOutput TxnOutput;
      memset(&TxnOutput, 0x00, sizeof(TxnOutput));

      if (m_Decoder.decode(buf, index, TxnInput)) {

#ifdef _TRACE
        cout << "[TS] << " << TxnInput << '\r' << endl;
#endif
        Txn.DoTxn(&TxnInput, &TxnOutput);

#ifdef _TRACE
        cout << "[TS] >> " << TxnOutput << '\r' << endl;
#endif
        result = true;
      }
    }

    return result;
  }

  /*
   */
  bool CBH::DoTxn(char *buf, int *index, CTradeUpdateDBInterface *pDB) {

    bool result = false;

    if (NULL != pDB) {

      CTradeUpdate Txn(pDB);

      TTradeUpdateTxnInput TxnInput;
      memset(&TxnInput, 0x00, sizeof(TxnInput));

      TTradeUpdateTxnOutput TxnOutput;
      memset(&TxnOutput, 0x00, sizeof(TxnOutput));

      if (m_Decoder.decode(buf, index, TxnInput)) {

#ifdef _TRACE
        cout << "[TU] << " << TxnInput << '\r' << endl;
#endif
        Txn.DoTxn(&TxnInput, &TxnOutput);

#ifdef _TRACE
        cout << "[TU] >> " << TxnOutput << '\r' << endl;
#endif
        result = true;
      }
    }

    return result;
  }

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  struct TBH {

    CBH m_BH;

    const char *m_szPath;
    const char *m_szSymbol;
    CDBDriverManager *m_pDBDriverManager;

    TBH(ErlDrvPort ErlDrvPort, const char *szPath, const char *szSymbol)
      : m_BH(ErlDrvPort),
        m_szPath(szPath), m_szSymbol(szSymbol),
        m_pDBDriverManager(NULL) {}

    ~TBH() {
      if (NULL != m_pDBDriverManager) {
        delete m_pDBDriverManager;
      }
    }

    CDBDriverManager *GetDBDriverManager() {
      if (NULL == m_pDBDriverManager) {
        m_pDBDriverManager = new CDBDriverManager(m_szPath, m_szSymbol);
      }
      return m_pDBDriverManager;
    }

    CDBConnectionInterface *GetDBConnection() {
      return GetDBDriverManager()->GetDBConnection();
    }

    void SetConfig(const char *szName, int iValue) {
      GetDBDriverManager()->SetConfig(szName, iValue);
    }

    void SetConfig(const char *szName, long iValue) {
      GetDBDriverManager()->SetConfig(szName, iValue);
    }

    void SetConfig(const char *szName, const char *szValue, size_t uValueLen) {
      GetDBDriverManager()->SetConfig(szName, szValue, uValueLen);
    }

    bool DoTxn(char *buf, int *index) {
      return m_BH.DoTxn(buf, index, GetDBConnection());
    }

  };

} // namespace TPCE

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

typedef struct {
  //
  ErlDrvPort port;
  //
  void *driver;
  //
  char path[PATH_MAX+1];
  char symbol[PATH_MAX+1];
} driver_data_t;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

/*
 */
static void *ergen_driver_new(driver_data_t *data) {
  return new TPCE::TBH(data->port, data->path, data->symbol);
}

/*
 */
static void ergen_driver_delete(void *ptr) {
  if (NULL != ptr) {
    TPCE::TBH *driver = (TPCE::TBH *)ptr;
    delete driver;
  }
}

/*
 */
static int ergen_driver_do_txn(void *ptr, char *buf, int *index) {
  TPCE::TBH *driver = (TPCE::TBH *)ptr;
  return driver->DoTxn(buf, index);
}

static void ergen_driver_set_int(void *ptr, const char *name, int value) {
  TPCE::TBH *driver = (TPCE::TBH *)ptr;
  driver->SetConfig(name, value);
}

static void ergen_driver_set_long(void *ptr, const char *name, long value) {
  TPCE::TBH *driver = (TPCE::TBH *)ptr;
  driver->SetConfig(name, value);
}

static void ergen_driver_set_string(void *ptr, const char *name, const char *value, size_t value_len) {
  TPCE::TBH *driver = (TPCE::TBH *)ptr;
  driver->SetConfig(name, value, value_len);
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

      ei_decode_version(buf, &index, &version);
      ei_decode_list_header(buf, &index, &arity);

      for (int size = arity, i = 0; i < size; i++) {

        ei_decode_tuple_header(buf, &index, &arity);

        if (2 == arity) {

          ei_term term;

          ei_decode_atom(buf, &index, atom);
          ei_decode_ei_term(buf, &index, &term);

          if (0 == strcmp(atom, "path") && term.size < ((int)sizeof(data->path))) {
            ei_decode_string(buf, &index, data->path);
          } else if (0 == strcmp(atom, "symbol") && term.size < ((int)sizeof(data->symbol))) {
            ei_decode_string(buf, &index, data->symbol);
          }
        }
      }

      ei_x_encode_atom(&x, "ok");

    } break;
  case TPCE::CMD_ALL_CONFIG:
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

          switch (term.ei_type) {
          case ERL_SMALL_INTEGER_EXT:
            ergen_driver_set_int(data->driver, atom, term.value.i_val);
            break;
          case ERL_INTEGER_EXT:
            ergen_driver_set_long(data->driver, atom, term.value.i_val);
            break;
          case ERL_STRING_EXT: {
            char v[term.size+1];
            ei_decode_string(buf, &index, v);
            ergen_driver_set_string(data->driver, atom, v, sizeof(v));
          } break;
          case ERL_BINARY_EXT: {
            char v[term.size+1];
            long l = term.size;
            ei_decode_binary(buf, &index, v, &l); v[l] = '\0';
            ergen_driver_set_string(data->driver, atom, v, sizeof(v));
          } break;
          default:
            found = false;
            break;
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
