/*
 */

#include "TradeStatusDB.H"

#include <sstream>

#include "HandleDbc.H"
#include "HandleStmt.H"

namespace TPCE {

  /*
   */
  class CTradeStatusDBStmt0 : public CHandleStmt {
  private:
    TTrade trade_id;
    TIMESTAMP_STRUCT trade_dts;
    char status_name[cST_NAME_len+1];
    char type_name[cTT_NAME_len+1];
    char symbol[cSYMBOL_len+1];
    INT32 trade_qty;
    char exec_name[cEXEC_NAME_len+1];
    double charge;
    char s_name[cS_NAME_len+1];
    char ex_name[cEX_NAME_len+1];

    int index;
    TTrade *trade_id_returned[max_trade_status_len];
    TIMESTAMP_STRUCT *trade_dts_returned[max_trade_status_len];
    char *status_name_returned[max_trade_status_len];
    char *type_name_returned[max_trade_status_len];
    char *symbol_returned[max_trade_status_len];
    INT32 *trade_qty_returned[max_trade_status_len];
    char *exec_name_returned[max_trade_status_len];
    double *charge_returned[max_trade_status_len];
    char *s_name_returned[max_trade_status_len];
    char *ex_name_returned[max_trade_status_len];

  public:
    CTradeStatusDBStmt0(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      std::stringstream oss;
      oss << "SELECT";
      oss << "  t_id AS 'trade_id'";
      oss << ", t_dts AS 'trade_dts'";
      oss << ", st_name AS 'status_name'";
      oss << ", tt_name AS 'type_name'";
      oss << ", t_s_symb AS 'symbol'";
      oss << ", t_qty AS 'trade_qty'";
      oss << ", t_exec_name AS 'exec_name'";
      oss << ", t_chrg AS 'charge'";
      oss << ", s_name AS 's_name'";
      oss << ", ex_name AS 'ex_name'";
      oss << "  FROM trade";
      oss << "  JOIN status_type ON st_id = t_st_id";
      oss << "  JOIN trade_type ON tt_id = t_tt_id";
      oss << "  JOIN security ON s_symb = t_s_symb";
      oss << "  JOIN exchange ON ex_id = s_ex_id";
      oss << " WHERE t_ca_id = ?";
      oss << " ORDER BY";
      oss << "  t_dts DESC";
      oss << " LIMIT " << max_trade_status_len;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CTradeStatusDBStmt0() {}

    virtual CTradeStatusDBStmt0 *BindCol() {

      (*((CHandleStmt *)this))
        .BindCol( 1, &trade_id, sizeof(trade_id))
        .BindCol( 2, &trade_dts, sizeof(trade_dts))
        .BindCol( 3, status_name, sizeof(status_name))
        .BindCol( 4, type_name, sizeof(type_name))
        .BindCol( 5, symbol, sizeof(symbol))
        .BindCol( 6, &trade_qty, sizeof(trade_qty))
        .BindCol( 7, exec_name, sizeof(exec_name))
        .BindCol( 8, &charge, sizeof(charge))
        .BindCol( 9, s_name, sizeof(s_name))
        .BindCol(10, ex_name, sizeof(ex_name));

      return this;
    }

    virtual CTradeStatusDBStmt0 *BindCol(TTradeStatusFrame1Output *pOut) {

      for (int i = 0; i < max_trade_status_len; i++) {
        trade_id_returned[i] = &pOut->trade_id[i];
        trade_dts_returned[i] = &pOut->trade_dts[i];
        status_name_returned[i] = pOut->status_name[i];
        type_name_returned[i] = pOut->type_name[i];
        symbol_returned[i] = pOut->symbol[i];
        trade_qty_returned[i] = &pOut->trade_qty[i];
        exec_name_returned[i] = pOut->exec_name[i];
        charge_returned[i] = &pOut->charge[i];
        s_name_returned[i] = pOut->s_name[i];
        ex_name_returned[i] = pOut->ex_name[i];
      }

      return BindCol();
    }

    virtual CTradeStatusDBStmt0 *BindParameter(const TTradeStatusFrame1Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id));

      return this;
    }

    virtual bool Execute() {

      index = 0;

      return CHandleStmt::Execute();
    }

    virtual bool Fetch() {

      trade_id = 0;
      memset(&trade_dts, 0x00, sizeof(trade_dts));
      memset(status_name, 0x00, sizeof(status_name));
      memset(type_name, 0x00, sizeof(type_name));
      memset(symbol, 0x00, sizeof(symbol));
      trade_qty = 0;
      memset(exec_name, 0x00, sizeof(exec_name));
      charge = 0.0;
      memset(s_name, 0x00, sizeof(s_name));
      memset(ex_name, 0x00, sizeof(ex_name));

      bool result = CHandleStmt::Fetch();

      if (result) {

        *trade_id_returned[index] = trade_id;
        memcpy(trade_dts_returned[index], &trade_dts, sizeof(trade_dts));
        memcpy(status_name_returned[index], status_name, sizeof(status_name));
        memcpy(type_name_returned[index], type_name, sizeof(type_name));
        memcpy(symbol_returned[index], symbol, sizeof(symbol));
        *trade_qty_returned[index] = trade_qty;
        memcpy(exec_name_returned[index], exec_name, sizeof(exec_name));
        *charge_returned[index] = charge;
        memcpy(s_name_returned[index], s_name, sizeof(s_name));
        memcpy(ex_name_returned[index], ex_name, sizeof(ex_name));

        index++;
      }

      return result;
    }
  };

  /*
   */
  class CTradeStatusDBStmt1 : public CHandleStmt {
  public:
    CTradeStatusDBStmt1(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  c_l_name AS 'cust_l_name'"
        ", c_f_name AS 'cust_f_name'"
        ", b_name AS 'broker_name'"
        "  FROM customer_account"
        "  JOIN customer ON c_id = ca_c_id"
        "  JOIN broker ON b_id = ca_b_id"
        " WHERE ca_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeStatusDBStmt1() {}

    virtual CTradeStatusDBStmt1 *BindCol(TTradeStatusFrame1Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol(1, pOut->cust_l_name, sizeof(pOut->cust_l_name))
        .BindCol(2, pOut->cust_f_name, sizeof(pOut->cust_f_name))
        .BindCol(3, pOut->broker_name, sizeof(pOut->broker_name));

      return this;
    }

    virtual CTradeStatusDBStmt1 *BindParameter(const TTradeStatusFrame1Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id));

      return this;
    }
  };

  /*
   */
  CTradeStatusDB::CTradeStatusDB(CHandleDbc *pDbc)
    : CTradeStatusDBInterface(), m_pDbc(pDbc) {

    m_pStmt[0] = new CTradeStatusDBStmt0(pDbc);
    m_pStmt[1] = new CTradeStatusDBStmt1(pDbc);
  }

  /*
   */
  CTradeStatusDB::~CTradeStatusDB() {

    delete m_pStmt[0];
    delete m_pStmt[1];
  }

  /*
   */
  void CTradeStatusDB::DoTradeStatusFrame1(const TTradeStatusFrame1Input *pIn,
                                           TTradeStatusFrame1Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeStatusDB::DoTradeStatusFrame1/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    {
      CHandleStmt *pStmt;
      int p;

      { // -- 0 --
        ((CTradeStatusDBStmt0 *)(pStmt = m_pStmt[(p = 0)]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

        pOut->num_found = 0;

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] acct_id=" << pIn->acct_id << '\r' << endl;
#endif
        if (pStmt->Execute()) {

          pOut->num_found = pStmt->RowCount();

#ifdef _TRACE
          cout << "TRACE: [1," << p << "] num_found=" << pOut->num_found << '\r' << endl;
#endif
          for (int size = pOut->num_found, i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [1," << p << ":" << i << "] trade_id=" << pOut->trade_id[i] << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] trade_dts=" << pOut->trade_dts[i] << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] status_name=" << pOut->status_name[i] << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] type_name=" << pOut->type_name[i] << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] symbol=" << pOut->symbol[i] << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] trade_qty=" << pOut->trade_qty[i] << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] exec_name=" << pOut->exec_name[i] << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] charge=" << pOut->charge[i] << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] s_name=" << pOut->s_name[i] << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ex_name=" << pOut->ex_name[i] << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 0 --

      { // -- 1 --
        ((CTradeStatusDBStmt1 *)(pStmt = m_pStmt[(p = 1)]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] acct_id=" << pIn->acct_id << '\r' << endl;
#endif
        if (pStmt->Execute()) {

          for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [1," << p << ":" << i << "] cust_l_name=" << pOut->cust_l_name << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] cust_f_name=" << pOut->cust_f_name << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] broker_name=" << pOut->broker_name << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 1 --
    }

    m_pDbc->CommitTransaction();
  }

}
