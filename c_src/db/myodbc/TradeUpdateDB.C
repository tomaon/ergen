/*
 */

#include "TradeUpdateDB.H"

#include <sstream>

#include <sys/param.h> // MIN

#include "HandleDbc.H"
#include "HandleStmt.H"

#include "CashTransactionStmt.H"
#include "SettlementStmt.H"
#include "TradeHistoryStmt.H"

namespace TPCE {

  /*
   */
  class CTradeUpdateDBStmt01 : public CHandleStmt {
  private:
    char is_cash;
    char is_market;

    bool *is_cash_returned;
    bool *is_market_returned;

  public:
    CTradeUpdateDBStmt01(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      std::ostringstream oss;
      oss << "SELECT";
      oss << "  t_bid_price AS 'bid_price'";
      oss << ", t_exec_name AS 'exec_name'";
      oss << ", t_is_cash AS 'is_cash'";
      oss << ", tt_is_mrkt AS 'is_market'";
      oss << ", t_trade_price AS 'trade_price'"; // nullable
      oss << "  FROM trade";
      oss << "  JOIN trade_type ON t_tt_id = tt_id";
      oss << " WHERE t_id = ?";
      oss << " LIMIT " << TradeUpdateFrame1MaxRows;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CTradeUpdateDBStmt01() {}

    virtual CTradeUpdateDBStmt01 *BindCol(TTradeUpdateFrame1TradeInfo *trade_info) {

      is_cash_returned = &trade_info->is_cash;
      is_market_returned = &trade_info->is_market;

      (*((CHandleStmt *)this))
        .BindCol(1, &trade_info->bid_price,
                 sizeof(trade_info->bid_price), &trade_info->bid_price_ind)
        .BindCol(2, trade_info->exec_name,
                 sizeof(trade_info->exec_name), &trade_info->exec_name_ind)
        .BindCol(3, &is_cash,
                 sizeof(is_cash), &trade_info->is_cash_ind)
        .BindCol(4, &is_market,
                 sizeof(is_market), &trade_info->is_market_ind)
        .BindCol(5, &trade_info->trade_price,
                 sizeof(trade_info->trade_price), &trade_info->trade_price_ind);

      return this;
    }

    virtual CTradeUpdateDBStmt01 *BindParameter(const TTrade *trade_id) {

      (*((CHandleStmt *)this))
        .BindParameter(1, trade_id, sizeof(TTrade));

      return this;
    }

    virtual bool Fetch() {

      is_cash = is_market = 0;

      bool result = CHandleStmt::Fetch();

      if (result) {
        *is_cash_returned = (bool) is_cash;
        *is_market_returned = (bool) is_market;
      }

      return result;
    }

  };

  /*
   */
  class CTradeUpdateDBStmt02 : public CHandleStmt {
  private:
    char is_cash;

    bool *is_cash_returned;

  public:
    CTradeUpdateDBStmt02(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      std::ostringstream oss;
      oss << "SELECT";
      oss << "  t_bid_price AS 'bid_price'";
      oss << ", t_exec_name AS 'exec_name'";
      oss << ", t_is_cash AS 'is_cash'";
      oss << ", t_id AS 'trade_id'";
      oss << ", t_trade_price AS 'trade_price'"; // nullable
      oss << "  FROM trade";
      oss << " WHERE t_ca_id = ?";
      oss << "   AND t_dts >= ?";
      oss << "   AND t_dts <= ?";
      oss << " ORDER BY";
      oss << "  t_dts";
      oss << " LIMIT " << TradeUpdateFrame2MaxRows;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CTradeUpdateDBStmt02() {}

    virtual CTradeUpdateDBStmt02 *BindCol(TTradeUpdateFrame2TradeInfo *trade_info) {

      is_cash_returned = &trade_info->is_cash;

      (*((CHandleStmt *)this))
        .BindCol(1, &trade_info->bid_price,
                 sizeof(trade_info->bid_price), &trade_info->bid_price_ind)
        .BindCol(2, trade_info->exec_name,
                 sizeof(trade_info->exec_name), &trade_info->exec_name_ind)
        .BindCol(3, &is_cash,
                 sizeof(is_cash), &trade_info->is_cash_ind)
        .BindCol(4, &trade_info->trade_id,
                 sizeof(trade_info->trade_id), &trade_info->trade_id_ind)
        .BindCol(5, &trade_info->trade_price,
                 sizeof(trade_info->trade_price), &trade_info->trade_price_ind);

      return this;
    }

    virtual CTradeUpdateDBStmt02 *BindParameter(const TTradeUpdateFrame2Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(2, &pIn->start_trade_dts, sizeof(pIn->start_trade_dts))
        .BindParameter(3, &pIn->end_trade_dts, sizeof(pIn->end_trade_dts));

      return this;
    }

    virtual bool Fetch() {

      is_cash = 0;

      bool result = CHandleStmt::Fetch();

      if (result) {
        *is_cash_returned = is_cash;
      }

      return result;
    }

  };

  /*
   */
  class CTradeUpdateDBStmt03 : public CHandleStmt {
  private:
    char is_cash;

    bool *is_cash_returned;

  public:
    CTradeUpdateDBStmt03(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      std::ostringstream oss;
      oss << "SELECT";
      oss << "  t_ca_id AS 'acct_id'";
      oss << ", t_exec_name AS 'exec_name'";
      oss << ", t_is_cash AS 'is_cash'";
      oss << ", t_trade_price AS 'price'";   // nullable
      oss << ", t_qty AS 'quantity'";
      oss << ", s_name AS 's_name'";
      oss << ", t_dts AS 'trade_dts'";
      oss << ", t_id AS 'trade_id'";
      oss << ", t_tt_id AS 'trade_type'";
      oss << ", tt_name AS 'type_name'";
      oss << "  FROM trade";
      oss << "  JOIN trade_type ON tt_id = t_tt_id";
      oss << "  JOIN security ON s_symb = t_s_symb";
      oss << " WHERE t_s_symb = ?";
      oss << "   AND t_dts >= ?";
      oss << "   AND t_dts <= ?";
      oss << " ORDER BY";
      oss << "  t_dts ASC";
      oss << " LIMIT " << TradeUpdateFrame3MaxRows;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CTradeUpdateDBStmt03() {}

    virtual CTradeUpdateDBStmt03 *BindCol(TTradeUpdateFrame3TradeInfo *trade_info) {

      is_cash_returned = &trade_info->is_cash;

      (*((CHandleStmt *)this))
        .BindCol( 1, &trade_info->acct_id,
                  sizeof(trade_info->acct_id), &trade_info->acct_id_ind)
        .BindCol( 2, trade_info->exec_name,
                  sizeof(trade_info->exec_name), &trade_info->exec_name_ind)
        .BindCol( 3, &is_cash,
                  sizeof(is_cash), &trade_info->is_cash_ind)
        .BindCol( 4, &trade_info->price,
                  sizeof(trade_info->price), &trade_info->price_ind)
        .BindCol( 5, &trade_info->quantity,
                  sizeof(trade_info->quantity), &trade_info->quantity_ind)
        .BindCol( 6, trade_info->s_name,
                  sizeof(trade_info->s_name), &trade_info->s_name_ind)
        .BindCol( 7, &trade_info->trade_dts,
                  sizeof(trade_info->trade_dts), &trade_info->trade_dts_ind)
        .BindCol( 8, &trade_info->trade_id,
                  sizeof(trade_info->trade_id), &trade_info->trade_id_ind)
        .BindCol( 9, trade_info->trade_type,
                  sizeof(trade_info->trade_type), &trade_info->trade_type_ind)
        .BindCol(10, trade_info->type_name,
                 sizeof(trade_info->type_name), &trade_info->type_name_ind);

      return this;
    }

    virtual CTradeUpdateDBStmt03 *BindParameter(const TTradeUpdateFrame3Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, pIn->symbol, sizeof(pIn->symbol))
        .BindParameter(2, &pIn->start_trade_dts, sizeof(pIn->start_trade_dts))
        .BindParameter(3, &pIn->end_trade_dts, sizeof(pIn->end_trade_dts));

      return this;
    }

    virtual bool Fetch() {

      is_cash = 0;

      bool result = CHandleStmt::Fetch();

      if (result) {
        *is_cash_returned = is_cash;
      }

      return result;
    }

  };

  /*
   */
  class CTradeUpdateDBStmt07 : public CHandleStmt {
  public:
    CTradeUpdateDBStmt07(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT t_exec_name AS 'ex_name'"
        "  FROM trade"
        " WHERE t_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeUpdateDBStmt07() {}

    virtual CTradeUpdateDBStmt07 *BindCol(char *ex_name, size_t ex_name_len) {

      (*((CHandleStmt *)this))
        .BindCol(1, ex_name, ex_name_len);

      return this;
    }

    virtual CTradeUpdateDBStmt07 *BindParameter(const TTrade *trade_id) {

      (*((CHandleStmt *)this))
        .BindParameter(1, trade_id, sizeof(TTrade));

      return this;
    }
  };

  /*
   */
  class CTradeUpdateDBStmt08 : public CHandleStmt {
  public:
    CTradeUpdateDBStmt08(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "UPDATE trade"
        "   SET t_exec_name = ?"
        " WHERE t_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeUpdateDBStmt08() {}

    virtual CTradeUpdateDBStmt08 *BindParameter(const TTrade *trade_id,
                                                const char *ex_name, size_t ex_name_len) {
      (*((CHandleStmt *)this))
        .BindParameter(1, ex_name, ex_name_len)
        .BindParameter(2, trade_id, sizeof(TTrade));

      return this;
    }
  };

  /*
   */
  class CTradeUpdateDBStmt09 : public CHandleStmt {
  public:
    CTradeUpdateDBStmt09(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT se_cash_type AS 'cash_type'"
        "  FROM settlement"
        " WHERE se_t_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeUpdateDBStmt09() {}

    virtual CTradeUpdateDBStmt09 *BindCol(char *cash_type, size_t cash_type_len) {

      (*((CHandleStmt *)this))
        .BindCol(1, cash_type, cash_type_len);

      return this;
    }

    virtual CTradeUpdateDBStmt09 *BindParameter(const TTradeUpdateFrame2TradeInfo *trade_info) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &trade_info->trade_id, sizeof(trade_info->trade_id));

      return this;
    }
  };

  /*
   */
  class CTradeUpdateDBStmt10 : public CHandleStmt {
  public:
    CTradeUpdateDBStmt10(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "UPDATE settlement"
        "   SET se_cash_type = ?"
        " WHERE se_t_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeUpdateDBStmt10() {}

    virtual CTradeUpdateDBStmt10 *BindParameter(const TTradeUpdateFrame2TradeInfo *trade_info,
                                                const char *cash_type, size_t cash_type_len) {
      (*((CHandleStmt *)this))
        .BindParameter(1, cash_type, cash_type_len)
        .BindParameter(2, &trade_info->trade_id, sizeof(trade_info->trade_id));

      return this;
    }
  };

  /*
   */
  class CTradeUpdateDBStmt11 : public CHandleStmt {
  public:
    CTradeUpdateDBStmt11(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT ct_name"
        "  FROM cash_transaction"
        " WHERE ct_t_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeUpdateDBStmt11() {}

    virtual CTradeUpdateDBStmt11 *BindCol(char *ct_name, size_t ct_name_len) {

      (*((CHandleStmt *)this))
        .BindCol(1, ct_name, ct_name_len);

      return this;
    }

    virtual CTradeUpdateDBStmt11 *BindParameter(const TTradeUpdateFrame3TradeInfo *trade_info) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &trade_info->trade_id, sizeof(trade_info->trade_id));

      return this;
    }
  };

  /*
   */
  class CTradeUpdateDBStmt12 : public CHandleStmt {
  public:
    CTradeUpdateDBStmt12(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "UPDATE cash_transaction"
        "   SET ct_name = ?"
        " WHERE ct_t_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }
    virtual ~CTradeUpdateDBStmt12() {}

    virtual CTradeUpdateDBStmt12 *BindParameter(const TTradeUpdateFrame3TradeInfo *trade_info,
                                                const char *ct_name, size_t ct_name_len) {
      (*((CHandleStmt *)this))
        .BindParameter(1, ct_name, ct_name_len)
        .BindParameter(2, &trade_info->trade_id, sizeof(trade_info->trade_id));

      return this;
    }

  };

  /*
   */
  CTradeUpdateDB::CTradeUpdateDB(CHandleDbc *pDbc)
    : CTradeUpdateDBInterface(), m_pDbc(pDbc) {

    m_pStmt[ 1] = new CTradeUpdateDBStmt01(pDbc);
    m_pStmt[ 2] = new CTradeUpdateDBStmt02(pDbc);
    m_pStmt[ 3] = new CTradeUpdateDBStmt03(pDbc);
    m_pStmt[ 4] = new CSettlementStmt(pDbc);
    m_pStmt[ 5] = new CCashTransactionStmt(pDbc);
    m_pStmt[ 6] = new CTradeHistoryStmt(pDbc);
    m_pStmt[ 7] = new CTradeUpdateDBStmt07(pDbc);
    m_pStmt[ 8] = new CTradeUpdateDBStmt08(pDbc);
    m_pStmt[ 9] = new CTradeUpdateDBStmt09(pDbc);
    m_pStmt[10] = new CTradeUpdateDBStmt10(pDbc);
    m_pStmt[11] = new CTradeUpdateDBStmt11(pDbc);
    m_pStmt[12] = new CTradeUpdateDBStmt12(pDbc);
  }

  /*
   */
  CTradeUpdateDB::~CTradeUpdateDB() {

    delete m_pStmt[ 1];
    delete m_pStmt[ 2];
    delete m_pStmt[ 3];
    delete m_pStmt[ 4];
    delete m_pStmt[ 5];
    delete m_pStmt[ 6];
    delete m_pStmt[ 7];
    delete m_pStmt[ 8];
    delete m_pStmt[ 9];
    delete m_pStmt[10];
    delete m_pStmt[11];
    delete m_pStmt[12];
  }

  /*
   */
  void CTradeUpdateDB::DoTradeUpdateFrame1(const TTradeUpdateFrame1Input *pIn,
                                           TTradeUpdateFrame1Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeUpdateDB::DoTradeUpdateFrame1/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    { // -- 1 --
      TTrade trade_id;
      TTradeUpdateFrame1TradeInfo trade_info;
      char ex_name[cEX_NAME_len+1];
      SQLLEN row_count;

      ((CTradeUpdateDBStmt01 *)m_pStmt[1])
        ->BindParameter(&trade_id)
        ->BindCol(&trade_info);

      ((CTradeUpdateDBStmt07 *)m_pStmt[7])
        ->BindParameter(&trade_id)
        ->BindCol(ex_name, sizeof(ex_name));

      ((CTradeUpdateDBStmt08 *)m_pStmt[8])
        ->BindParameter(&trade_id, ex_name, sizeof(ex_name));

      pOut->num_found = 0;
      pOut->num_updated = 0;

      for (INT32 max_trades = pIn->max_trades, n = 0; n < max_trades; n++) {

        trade_id = pIn->trade_id[n];

        if (pOut->num_updated < pIn->max_updates) { // -- 7 --

          if (m_pStmt[7]->Execute()) {

            row_count = m_pStmt[7]->RowCount();

#ifdef _TRACE
            cout << "TRACE: [1:" << n << ",7] trade_id=" << trade_id << '\r' << endl;
            cout << "TRACE: [1:" << n << ",7] max_updates=" << pIn->max_updates << '\r' << endl;
            cout << "TRACE: [1:" << n << ",7] num_updated=" << pOut->num_updated << '\r' << endl;
            cout << "TRACE: [1:" << n << ",7] row_count=" << row_count << '\r' << endl;
#endif
            for (SQLLEN size = row_count, i = 0; i < size; i++) {

              memset(ex_name, 0x00, sizeof(ex_name));

              m_pStmt[7]->Fetch();

#ifdef _TRACE
              cout << "TRACE: [1:" << n << ",7:" << i << "] ex_name="
                   << ex_name << '\r' << endl;
#endif
              { // -- 8 --
                string s((const char *)ex_name);
                string::size_type p = s.find(" X ");
                if (string::npos != p) {
                  s = s.replace(p, 3, " ");
                } else {
                  s = s.replace(s.find(" "), 1, " X ");
                }
                strncpy(ex_name, s.c_str(), sizeof(ex_name));

                if (m_pStmt[8]->Execute()) {

                  row_count = m_pStmt[8]->RowCount();
#ifdef _TRACE
                  cout << "TRACE: [1:" << n << ",8] trade_id=" << trade_id << '\r' << endl;
                  cout << "TRACE: [1:" << n << ",8] ex_name=" << ex_name << '\r' << endl;
                  cout << "TRACE: [1:" << n << ",8] row_count=" << row_count << '\r' << endl;
#endif
                  pOut->num_updated += row_count;

                  m_pStmt[8]->Cancel();
                }
              } // -- 8 --
            }

            m_pStmt[7]->Cancel();
          }
        } // -- 7 --

        if (m_pStmt[1]->Execute()) {

          row_count = m_pStmt[1]->RowCount();

#ifdef _TRACE
          cout << "TRACE: [1:" << n << "] trade_id=" << trade_id << '\r' << endl;
          cout << "TRACE: [1:" << n << "] max_trades=" << pIn->max_trades << '\r' << endl;
          cout << "TRACE: [1:" << n << "] row_count=" << row_count << '\r' << endl;
#endif
          pOut->num_found += row_count;

          for (SQLLEN size = row_count, i = 0; i < size; i++) {

            memset(&trade_info, 0x00, sizeof(trade_info));

            m_pStmt[1]->Fetch();

#ifdef _TRACE
            cout << "TRACE: [1:" << n << ":" << i << "] bid_price="
                 << trade_info.bid_price << '\r' << endl;
            cout << "TRACE: [1:" << n << ":" << i << "] exec_name="
                 << trade_info.exec_name << '\r' << endl;
            cout << "TRACE: [1:" << n << ":" << i << "] is_cash="
                 << trade_info.is_cash << '\r' << endl;
            cout << "TRACE: [1:" << n << ":" << i << "] is_market="
                 << trade_info.is_market << '\r' << endl;
            cout << "TRACE: [1:" << n << ":" << i << "] trade_price=";
            if(0 <= trade_info.trade_price_ind)
              cout << trade_info.trade_price << '\r' << endl;
            else
              cout << "NULL" << '\r' << endl;
#endif
            { // -- 4 --
              ((CSettlementStmt *)m_pStmt[4])
                ->BindParameter(&trade_id)
                ->BindCol(&trade_info);

              if (m_pStmt[4]->Execute()) {

                row_count = m_pStmt[4]->RowCount();
#ifdef _TRACE
                cout << "TRACE: [1:" << n << ",4] trade_id=" << trade_id << '\r' << endl;
                cout << "TRACE: [1:" << n << ",4] row_count=" << row_count << '\r' << endl;
#endif
                for (int size = row_count, i = 0; i < size; i++) {

                  m_pStmt[4]->Fetch();
#ifdef _TRACE
                  cout << "TRACE: [1:" << n << ",4:" << i << "] settlement_amount="
                       << trade_info.settlement_amount << '\r' << endl;
                  cout << "TRACE: [1:" << n << ",4:" << i << "] settlement_cash_due_date="
                       << trade_info.settlement_cash_due_date << '\r' << endl;
                  cout << "TRACE: [1:" << n << ",4:" << i << "] settlement_cash_type="
                       << trade_info.settlement_cash_type << '\r' << endl;
#endif
                }

                m_pStmt[4]->Cancel();
              }
            } // -- 4 --

            if (1 == trade_info.is_cash) { // -- 5 --

              ((CCashTransactionStmt *)m_pStmt[5])
                ->BindParameter(&trade_id)
                ->BindCol(&trade_info);

              if (m_pStmt[5]->Execute()) {

                row_count = m_pStmt[5]->RowCount();
#ifdef _TRACE
                cout << "TRACE: [1:" << n << ",5] trade_id=" << trade_id << '\r' << endl;
                cout << "TRACE: [1:" << n << ",5] row_count=" << row_count << '\r' << endl;
#endif
                for (int size = row_count, i = 0; i < size; i++) {

                  m_pStmt[5]->Fetch();
#ifdef _TRACE
                  cout << "TRACE: [1:" << n << ",5:" << i << "] cash_transaction_amount="
                       << trade_info.cash_transaction_amount << '\r' << endl;
                  cout << "TRACE: [1:" << n << ",5:" << i << "] cash_transaction_dts="
                       << trade_info.cash_transaction_dts << '\r' << endl;
                  cout << "TRACE: [1:" << n << ",5:" << i << "] cash_transaction_name="
                       << trade_info.cash_transaction_name << '\r' << endl;
#endif
                }

                m_pStmt[5]->Cancel();
              }
            } // -- 5 --

            { // -- 6 --
              ((CTradeHistoryStmt *)m_pStmt[6])
                ->BindParameter(&trade_id)
                ->BindCol(&trade_info);

              if (m_pStmt[6]->Execute()) {

                row_count = m_pStmt[6]->RowCount();
#ifdef _TRACE
                cout << "TRACE: [1:" << n << ",6] trade_id=" << trade_id << '\r' << endl;
                cout << "TRACE: [1:" << n << ",6] row_count=" << row_count << '\r' << endl;
#endif
                for (int size = row_count, i = 0; i < size; i++) {

                  m_pStmt[6]->Fetch();
#ifdef _TRACE
                  cout << "TRACE: [1:" << n << ",6:" << i << "] trade_history_dts="
                       << trade_info.trade_history_dts[i] << '\r' << endl;
                  cout << "TRACE: [1:" << n << ",6:" << i << "] trade_history_status_id="
                       << trade_info.trade_history_status_id[i] << '\r' << endl;
#endif
                }
                trade_info.trade_history_status_id[row_count][0] = '\0';

                m_pStmt[6]->Cancel();
              }
            } // -- 6 --

            memcpy(&pOut->trade_info[n], &trade_info, sizeof(trade_info));
          }

          m_pStmt[1]->Cancel();
        }
      } // for (max_trades)
    } // -- 1 --

    m_pDbc->CommitTransaction();
  }

  /*
   */
  void CTradeUpdateDB::DoTradeUpdateFrame2(const TTradeUpdateFrame2Input *pIn,
                                           TTradeUpdateFrame2Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeUpdateDB::DoTradeUpdateFrame2/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    { // -- 2 --
      TTradeUpdateFrame2TradeInfo trade_info;
      char cash_type[cSE_CASH_TYPE_len+1];
      SQLLEN row_count;

      ((CTradeUpdateDBStmt02 *)m_pStmt[2])
        ->BindParameter(pIn)
        ->BindCol(&trade_info);

      ((CTradeUpdateDBStmt09 *)m_pStmt[9])
        ->BindParameter(&trade_info)
        ->BindCol(cash_type, sizeof(cash_type));

      ((CTradeUpdateDBStmt10 *)m_pStmt[10])
        ->BindParameter(&trade_info, cash_type, sizeof(cash_type));

      pOut->num_found = 0;
      pOut->num_updated = 0;

      if (m_pStmt[2]->Execute()) {

        row_count = m_pStmt[2]->RowCount();

#ifdef _TRACE
        cout << "TRACE: [2] acct_id=" << pIn->acct_id << '\r' << endl;
        cout << "TRACE: [2] start_trade_dts=" << pIn->start_trade_dts << '\r' << endl;
        cout << "TRACE: [2] trade_history_dts=" << pIn->end_trade_dts << '\r' << endl;
        cout << "TRACE: [2] max_trades=" << pIn->max_trades << '\r' << endl;
        cout << "TRACE: [2] row_count=" << row_count << '\r' << endl;
#endif
        pOut->num_found = MIN(row_count, pIn->max_trades);

        for (INT32 max_trades = pOut->num_found, n = 0; n < max_trades; n++) {

          memset(&trade_info, 0x00, sizeof(trade_info));

          m_pStmt[2]->Fetch();

#ifdef _TRACE
          cout << "TRACE: [2:" << n << "] bid_price=" << trade_info.bid_price << '\r' << endl;
          cout << "TRACE: [2:" << n << "] exec_name=" << trade_info.exec_name << '\r' << endl;
          cout << "TRACE: [2:" << n << "] is_cash=" << trade_info.is_cash << '\r' << endl;
          cout << "TRACE: [2:" << n << "] trade_id=" << trade_info.trade_id << '\r' << endl;
          cout << "TRACE: [2:" << n << "] trade_price=";
          if(0 <= trade_info.trade_price_ind)
            cout << trade_info.trade_price << '\r' << endl;
          else
            cout << "NULL" << '\r' << endl;
          cout << "TRACE: [2:" << n << "] max_updates=" << pIn->max_updates << '\r' << endl;
          cout << "TRACE: [2:" << n << "] num_updated=" << pOut->num_updated << '\r' << endl;
#endif
          if (pOut->num_updated < pIn->max_updates) { // -- 9 --

            if (m_pStmt[9]->Execute()) {

              row_count = m_pStmt[9]->RowCount();
#ifdef _TRACE
              cout << "TRACE: [2:" << n << ",9] trade_id="
                   << trade_info.trade_id << '\r' << endl;
              cout << "TRACE: [2:" << n << ",9] row_count="
                   << row_count << '\r' << endl;
#endif
              for (SQLLEN size = row_count, i = 0; i < size; i++) {

                m_pStmt[9]->Fetch();
#ifdef _TRACE
                cout << "TRACE: [2:" << n << ",9:" << i << "] cash_type="
                     << cash_type << '\r' << endl;
#endif
                { // -- 10 --
                  if (1 == trade_info.is_cash) {
                    if (0 == strcmp(cash_type, "Cash Account")) {
                      strncpy(cash_type, "Cash", sizeof(cash_type));
                    } else {
                      strncpy(cash_type, "Cash Account", sizeof(cash_type));
                    }
                  } else {
                    if (0 == strcmp(cash_type, "Margin Account")) {
                      strncpy(cash_type, "Margin", sizeof(cash_type));
                    } else {
                      strncpy(cash_type, "Margin Account", sizeof(cash_type));
                    }
                  }

                  if (m_pStmt[10]->Execute()) {

                    row_count = m_pStmt[10]->RowCount();
#ifdef _TRACE
                    cout << "TRACE: [2:" << n << ",10] trade_id="
                         <<trade_info. trade_id << '\r' << endl;
                    cout << "TRACE: [2:" << n << ",10] cash_type="
                         << cash_type << '\r' << endl;
                    cout << "TRACE: [2:" << n << ",10] row_count="
                         << row_count << '\r' << endl;
#endif
                    pOut->num_updated += row_count;

                    m_pStmt[10]->Cancel();
                  }
                } // -- 10 --
              }

              m_pStmt[9]->Cancel();
            }
          } // -- 9 --

          { // -- 4 --
            ((CSettlementStmt *)m_pStmt[4])
              ->BindParameter(&trade_info.trade_id)
              ->BindCol(&trade_info);

            if (m_pStmt[4]->Execute()) {

              row_count = m_pStmt[4]->RowCount();
#ifdef _TRACE
              cout << "TRACE: [2:" << n << ",4] trade_id=" << trade_info.trade_id << '\r' << endl;
              cout << "TRACE: [2:" << n << ",4] row_count=" << row_count << '\r' << endl;
#endif
              for (int size = row_count, i = 0; i < size; i++) {

                m_pStmt[4]->Fetch();
#ifdef _TRACE
                cout << "TRACE: [2:" << n << ",4:" << i << "] settlement_amount="
                     << trade_info.settlement_amount << '\r' << endl;
                cout << "TRACE: [2:" << n << ",4:" << i << "] settlement_cash_due_date="
                     << trade_info.settlement_cash_due_date << '\r' << endl;
                cout << "TRACE: [2:" << n << ",4:" << i << "] settlement_cash_type="
                     << trade_info.settlement_cash_type << '\r' << endl;
#endif
              }

              m_pStmt[4]->Cancel();
            }
          } // -- 4 --

          if (1 == trade_info.is_cash) { // -- 5 --

            ((CCashTransactionStmt *)m_pStmt[5])
              ->BindParameter(&trade_info.trade_id)
              ->BindCol(&trade_info);

            if (m_pStmt[5]->Execute()) {

              row_count = m_pStmt[5]->RowCount();
#ifdef _TRACE
              cout << "TRACE: [2:" << n << ",5] trade_id=" << trade_info.trade_id << '\r' << endl;
              cout << "TRACE: [2:" << n << ",5] row_count=" << row_count << '\r' << endl;
#endif
              for (int size = row_count, i = 0; i < size; i++) {

                m_pStmt[5]->Fetch();
#ifdef _TRACE
                cout << "TRACE: [2:" << n << ",5:" << i << "] cash_transaction_amount="
                     << trade_info.cash_transaction_amount << '\r' << endl;
                cout << "TRACE: [2:" << n << ",5:" << i << "] cash_transaction_dts="
                     << trade_info.cash_transaction_dts << '\r' << endl;
                cout << "TRACE: [2:" << n << ",5:" << i << "] cash_transaction_name="
                     << trade_info.cash_transaction_name << '\r' << endl;
#endif
              }

              m_pStmt[5]->Cancel();
            }
          } // -- 5 --

          { // -- 6 --
            ((CTradeHistoryStmt *)m_pStmt[6])
              ->BindParameter(&trade_info.trade_id)
              ->BindCol(&trade_info);

            if (m_pStmt[6]->Execute()) {

              row_count = m_pStmt[6]->RowCount();
#ifdef _TRACE
              cout << "TRACE: [2:" << n << ",6] trade_id=" << trade_info.trade_id << '\r' << endl;
              cout << "TRACE: [2:" << n << ",6] row_count=" << row_count << '\r' << endl;
#endif
              for (int size = row_count, i = 0; i < size; i++) {

                m_pStmt[6]->Fetch();
#ifdef _TRACE
                cout << "TRACE: [2:" << n << ",6:" << i << "] trade_history_dts="
                     << trade_info.trade_history_dts[i] << '\r' << endl;
                cout << "TRACE: [2:" << n << ",6:" << i << "] trade_history_status_id="
                     << trade_info.trade_history_status_id[i] << '\r' << endl;
#endif
              }
              trade_info.trade_history_status_id[row_count][0] = '\0';

              m_pStmt[6]->Cancel();
            }
          } // -- 6 --

          memcpy(&pOut->trade_info[n], &trade_info, sizeof(pOut->trade_info[0]));
        } // for (max_trades)

        m_pStmt[2]->Cancel();
      }
    } // -- 2 --

    m_pDbc->CommitTransaction();
  }

  /*
   */
  void CTradeUpdateDB::DoTradeUpdateFrame3(const TTradeUpdateFrame3Input *pIn,
                                           TTradeUpdateFrame3Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeUpdateDB::DoTradeUpdateFrame3/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    { // -- 3 --
      TTradeUpdateFrame3TradeInfo trade_info;
      char ct_name[cCT_NAME_len+1];
      int row_count;

      ((CTradeUpdateDBStmt03 *)m_pStmt[3])
        ->BindParameter(pIn)
        ->BindCol(&trade_info);

      ((CTradeUpdateDBStmt11 *)m_pStmt[11])
        ->BindParameter(&trade_info)
        ->BindCol(ct_name, sizeof(ct_name));

      ((CTradeUpdateDBStmt12 *)m_pStmt[12])
        ->BindParameter(&trade_info, ct_name, sizeof(ct_name));

      pOut->num_found = 0;
      pOut->num_updated = 0;

      if (m_pStmt[3]->Execute()) {

        row_count = m_pStmt[3]->RowCount();

#ifdef _TRACE
        cout << "TRACE: [3] symbol=" << pIn->symbol << '\r' << endl;
        cout << "TRACE: [3] start_trade_dts=" << pIn->start_trade_dts << '\r' << endl;
        cout << "TRACE: [3] end_trade_dts=" << pIn->end_trade_dts << '\r' << endl;
        cout << "TRACE: [3] max_trades=" << pIn->max_trades << '\r' << endl;
        cout << "TRACE: [3] row_count=" << row_count << '\r' << endl;
#endif
        pOut->num_found = MIN(row_count, pIn->max_trades);

        for (int max_trades = pOut->num_found, n = 0; n < max_trades; n++) {

          memset(&trade_info, 0x00, sizeof(trade_info));

          m_pStmt[3]->Fetch();

#ifdef _TRACE
          cout << "TRACE: [3:" << n << "] acct_id=" << trade_info.acct_id << '\r' << endl;
          cout << "TRACE: [3:" << n << "] exec_name=" << trade_info.exec_name << '\r' << endl;
          cout << "TRACE: [3:" << n << "] is_cash=" << trade_info.is_cash << '\r' << endl;
          cout << "TRACE: [3:" << n << "] price=";
          if(0 <= trade_info.price_ind)
            cout << trade_info.price << '\r' << endl;
          else
            cout << "NULL" << '\r' << endl;
          cout << "TRACE: [3:" << n << "] quantity=" << trade_info.quantity << '\r' << endl;
          cout << "TRACE: [3:" << n << "] s_name=" << trade_info.s_name << '\r' << endl;
          cout << "TRACE: [3:" << n << "] trade_dts=" << trade_info.trade_dts << '\r' << endl;
          cout << "TRACE: [3:" << n << "] trade_id=" << trade_info.trade_id << '\r' << endl;
          cout << "TRACE: [3:" << n << "] trade_type=" << trade_info.trade_type << '\r' << endl;
          cout << "TRACE: [3:" << n << "] type_name=" << trade_info.type_name << '\r' << endl;
#endif
          { // -- 4 --
            ((CSettlementStmt *)m_pStmt[4])
              ->BindParameter(&trade_info.trade_id)
              ->BindCol(&trade_info);

            if (m_pStmt[4]->Execute()) {

              row_count = m_pStmt[4]->RowCount();
#ifdef _TRACE
              cout << "TRACE: [3:" << n << ",4] trade_id=" << trade_info.trade_id << '\r' << endl;
              cout << "TRACE: [3:" << n << ",4] row_count=" << row_count << '\r' << endl;
#endif
              for (int size = row_count, i = 0; i < size; i++) {

                m_pStmt[4]->Fetch();
#ifdef _TRACE
                cout << "TRACE: [3:" << n << ",4:" << i << "] settlement_amount="
                     << trade_info.settlement_amount << '\r' << endl;
                cout << "TRACE: [3:" << n << ",4:" << i << "] settlement_cash_due_date="
                     << trade_info.settlement_cash_due_date << '\r' << endl;
                cout << "TRACE: [3:" << n << ",4:" << i << "] settlement_cash_type="
                     << trade_info.settlement_cash_type << '\r' << endl;
#endif
              }

              m_pStmt[4]->Cancel();
            }
          } // -- 4 --

          if (trade_info.is_cash) {

            if (pOut->num_updated < pIn->max_updates) { // -- 11 --

              if (m_pStmt[11]->Execute()) {

                row_count = m_pStmt[11]->RowCount();
#ifdef _TRACE
                cout << "TRACE: [3:" << n << ",11] trade_id="
                     << trade_info.trade_id << '\r' << endl;
                cout << "TRACE: [3:" << n << ",11] row_count="
                     << row_count << '\r' << endl;
#endif
                for (SQLLEN size = row_count, i = 0; i < size; i++) {

                  m_pStmt[11]->Fetch();
#ifdef _TRACE
                  cout << "TRACE: [3:" << n << ",11:" << i << "] ct_name="
                       << ct_name << '\r' << endl;
#endif
                  { // -- 12 --
                    if (NULL != strstr(ct_name, " shares of ")) {
                      snprintf(ct_name, sizeof(ct_name), "%s %d Shares of %s",
                               trade_info.type_name,
                               trade_info.quantity,
                               trade_info.s_name);
                    } else {
                      snprintf(ct_name, sizeof(ct_name), "%s %d shares of %s",
                               trade_info.type_name,
                               trade_info.quantity,
                               trade_info.s_name);
                    }

                    if (m_pStmt[12]->Execute()) {

                      row_count = m_pStmt[12]->RowCount();
#ifdef _TRACE
                      cout << "TRACE: [3:" << n << ",12] trade_id="
                           <<trade_info. trade_id << '\r' << endl;
                      cout << "TRACE: [3:" << n << ",12] ct_name="
                           << ct_name << '\r' << endl;
                      cout << "TRACE: [3:" << n << ",12] row_count="
                           << row_count << '\r' << endl;
#endif
                      pOut->num_updated += row_count;

                      m_pStmt[12]->Cancel();
                    }
                  } // -- 12 --
                }

                m_pStmt[11]->Cancel();
              }
            } // -- 11 --

            { // -- 5 --
              ((CCashTransactionStmt *)m_pStmt[5])
                ->BindParameter(&trade_info.trade_id)
                ->BindCol(&trade_info);

              if (m_pStmt[5]->Execute()) {

                row_count = m_pStmt[5]->RowCount();
#ifdef _TRACE
                cout << "TRACE: [3:" << n << ",5] trade_id=" << trade_info.trade_id << '\r' << endl;
                cout << "TRACE: [3:" << n << ",5] row_count=" << row_count << '\r' << endl;
#endif
                for (int size = row_count, i = 0; i < size; i++) {

                  m_pStmt[5]->Fetch();
#ifdef _TRACE
                  cout << "TRACE: [3:" << n << ",5:" << i << "] cash_transaction_amount="
                       << trade_info.cash_transaction_amount << '\r' << endl;
                  cout << "TRACE: [3:" << n << ",5:" << i << "] cash_transaction_dts="
                       << trade_info.cash_transaction_dts << '\r' << endl;
                  cout << "TRACE: [3:" << n << ",5:" << i << "] cash_transaction_name="
                       << trade_info.cash_transaction_name << '\r' << endl;
#endif
                }

                m_pStmt[5]->Cancel();
              }
            } // -- 5 --

          } // is_cash

          { // -- 6 --
            ((CTradeHistoryStmt *)m_pStmt[6])
              ->BindParameter(&trade_info.trade_id)
              ->BindCol(&trade_info);

            if (m_pStmt[6]->Execute()) {

              row_count = m_pStmt[6]->RowCount();
#ifdef _TRACE
              cout << "TRACE: [3:" << n << ",6] trade_id=" << trade_info.trade_id << '\r' << endl;
              cout << "TRACE: [3:" << n << ",6] row_count=" << row_count << '\r' << endl;
#endif
              for (int size = row_count, i = 0; i < size; i++) {

                m_pStmt[6]->Fetch();
#ifdef _TRACE
                cout << "TRACE: [3:" << n << ",6:" << i << "] trade_history_dts="
                     << trade_info.trade_history_dts[i] << '\r' << endl;
                cout << "TRACE: [3:" << n << ",6:" << i << "] trade_history_status_id="
                     << trade_info.trade_history_status_id[i] << '\r' << endl;
#endif
              }
              trade_info.trade_history_status_id[row_count][0] = '\0';

              m_pStmt[6]->Cancel();
            }
          } // -- 6 --

          memcpy(&pOut->trade_info[n], &trade_info, sizeof(pOut->trade_info[0]));

        } // for (max_trades)

        m_pStmt[3]->Cancel();
      }
    } // -- 3 --

    m_pDbc->CommitTransaction();
  }

}
