/*
 */

#include "TradeLookupDB.H"

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
  class CTradeLookupDBStmt1 : public CHandleStmt {
  private:
    char is_cash;
    char is_market;

    bool *is_cash_returned;
    bool *is_market_returned;

  public:
    CTradeLookupDBStmt1(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

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
      oss << " LIMIT " << TradeLookupFrame1MaxRows;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CTradeLookupDBStmt1() {}

    virtual CTradeLookupDBStmt1 *BindCol(TTradeLookupFrame1TradeInfo *trade_info) {

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

    virtual CTradeLookupDBStmt1 *BindParameter(const TTrade *trade_id) {

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
  class CTradeLookupDBStmt2 : public CHandleStmt {
  private:
    char is_cash;

    bool *is_cash_returned;

  public:
    CTradeLookupDBStmt2(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

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
      oss << " LIMIT " << TradeLookupFrame2MaxRows;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CTradeLookupDBStmt2() {}

    virtual CTradeLookupDBStmt2 *BindCol(TTradeLookupFrame2TradeInfo *trade_info) {

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

    virtual CTradeLookupDBStmt2 *BindParameter(const TTradeLookupFrame2Input *pIn) {

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
  class CTradeLookupDBStmt3 : public CHandleStmt {
  private:
    char is_cash;

    bool *is_cash_returned;

  public:
    CTradeLookupDBStmt3(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      std::ostringstream oss;
      oss << "SELECT";
      oss << "  t_ca_id AS 'acct_id'";
      oss << ", t_exec_name AS 'exec_name'";
      oss << ", t_is_cash AS 'is_cash'";
      oss << ", t_trade_price AS 'price'"; // nullable
      oss << ", t_qty AS 'quantity'";
      oss << ", t_dts AS 'trade_dts'";
      oss << ", t_id AS 'trade_id'";
      oss << ", t_tt_id AS 'trade_type'";
      oss << "  FROM trade";
      oss << " WHERE t_s_symb = ?";
      oss << "   AND t_dts >= ?";
      oss << "   AND t_dts <= ?";
      oss << "   AND t_ca_id <= ?";
      oss << " ORDER BY";
      oss << "  t_dts ASC";
      oss << " LIMIT " << TradeLookupFrame3MaxRows;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CTradeLookupDBStmt3() {}

    virtual CTradeLookupDBStmt3 *BindCol(TTradeLookupFrame3TradeInfo *trade_info) {

      is_cash_returned = &trade_info->is_cash;

      (*((CHandleStmt *)this))
        .BindCol(1, &trade_info->acct_id,
                 sizeof(trade_info->acct_id), &trade_info->acct_id_ind)
        .BindCol(2, trade_info->exec_name,
                 sizeof(trade_info->exec_name), &trade_info->exec_name_ind)
        .BindCol(3, &is_cash,
                 sizeof(is_cash), &trade_info->is_cash_ind)
        .BindCol(4, &trade_info->price,
                 sizeof(trade_info->price), &trade_info->price_ind)
        .BindCol(5, &trade_info->quantity,
                 sizeof(trade_info->quantity), &trade_info->quantity_ind)
        .BindCol(6, &trade_info->trade_dts,
                 sizeof(trade_info->trade_dts), &trade_info->trade_dts_ind)
        .BindCol(7, &trade_info->trade_id,
                 sizeof(trade_info->trade_id), &trade_info->trade_id_ind)
        .BindCol(8, trade_info->trade_type,
                 sizeof(trade_info->trade_type), &trade_info->trade_type_ind);

      return this;
    }

    virtual CTradeLookupDBStmt3 *BindParameter(const TTradeLookupFrame3Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, pIn->symbol, sizeof(pIn->symbol))
        .BindParameter(2, &pIn->start_trade_dts, sizeof(pIn->start_trade_dts))
        .BindParameter(3, &pIn->end_trade_dts, sizeof(pIn->end_trade_dts))
        .BindParameter(4, &pIn->max_acct_id, sizeof(pIn->max_acct_id));

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
  class CTradeLookupDBStmt4 : public CHandleStmt {
  public:
    CTradeLookupDBStmt4(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT t_id AS 'trade_id'"
        "  FROM trade"
        " WHERE t_ca_id = ?"
        "   AND t_dts >= ?"  // trade_dts (!= start_trade_dts)
        " ORDER BY"
        "  t_dts ASC"
        " LIMIT 1";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeLookupDBStmt4() {}

    virtual CTradeLookupDBStmt4 *BindCol(TTrade *trade_id) {

      (*((CHandleStmt *)this))
        .BindCol(1, trade_id, sizeof(TTrade));

      return this;
    }

    virtual CTradeLookupDBStmt4 *BindParameter(const TTradeLookupFrame4Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(2, &pIn->trade_dts, sizeof(pIn->trade_dts));

      return this;
    }

  };

  /*
   */
  class CHoldingHistoryStmt : public CHandleStmt {
  public:
    CHoldingHistoryStmt(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      std::ostringstream oss;
      oss << "SELECT";
      oss << "  c.hh_h_t_id AS 'holding_history_id'";
      oss << ", c.hh_t_id AS 'holding_history_trade_id'";
      oss << ", c.hh_before_qty AS 'quantity_before'";
      oss << ", c.hh_after_qty AS 'quantity_after'";
      oss << "  FROM holding_history p";
      oss << "  JOIN holding_history c ON c.hh_h_t_id = p.hh_h_t_id";
      oss << " WHERE p.hh_t_id = ?";
      oss << " LIMIT " << TradeLookupFrame4MaxRows;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CHoldingHistoryStmt() {}

    virtual CHoldingHistoryStmt *BindCol(TTradeLookupFrame4TradeInfo *trade_info) {

      (*((CHandleStmt *)this))
        .BindCol(1, &trade_info->holding_history_id,
                 sizeof(trade_info->holding_history_id),
                 &trade_info->holding_history_id_ind)
        .BindCol(2,
                 &trade_info->holding_history_trade_id,
                 sizeof(trade_info->holding_history_trade_id),
                 &trade_info->holding_history_trade_id_ind)
        .BindCol(3,
                 &trade_info->quantity_before,
                 sizeof(trade_info->quantity_before),
                 &trade_info->quantity_after_ind)
        .BindCol(4,
                 &trade_info->quantity_after,
                 sizeof(trade_info->quantity_after),
                 &trade_info->quantity_before_ind);

      return this;
    }

    virtual CHoldingHistoryStmt *BindParameter(const TTrade *trade_id) {

      (*((CHandleStmt *)this))
        .BindParameter(1, trade_id, sizeof(TTrade));

      return this;
    }

  };

  /*
   */
  CTradeLookupDB::CTradeLookupDB(CHandleDbc *pDbc)
    : CTradeLookupDBInterface(), m_pDbc(pDbc) {

    m_pStmt[1] = new CTradeLookupDBStmt1(pDbc);
    m_pStmt[2] = new CTradeLookupDBStmt2(pDbc);
    m_pStmt[3] = new CTradeLookupDBStmt3(pDbc);
    m_pStmt[4] = new CTradeLookupDBStmt4(pDbc);
    m_pStmt[5] = new CSettlementStmt(pDbc);
    m_pStmt[6] = new CCashTransactionStmt(pDbc);
    m_pStmt[7] = new CTradeHistoryStmt(pDbc);
    m_pStmt[8] = new CHoldingHistoryStmt(pDbc);
  }

  /*
   */
  CTradeLookupDB::~CTradeLookupDB() {

    delete m_pStmt[1];
    delete m_pStmt[2];
    delete m_pStmt[3];
    delete m_pStmt[4];
    delete m_pStmt[5];
    delete m_pStmt[6];
    delete m_pStmt[7];
    delete m_pStmt[8];
  }

  /*
   */
  void CTradeLookupDB::DoTradeLookupFrame1(const TTradeLookupFrame1Input *pIn,
                                           TTradeLookupFrame1Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeLookupDB::DoTradeLookupFrame1/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    { // -- 1 --
      TTrade trade_id;
      TTradeLookupFrame1TradeInfo trade_info;
      SQLLEN row_count;

      CHandleStmt *pStmt;
      int p;

      ((CTradeLookupDBStmt1 *)m_pStmt[1])
        ->BindParameter(&trade_id)
        ->BindCol(&trade_info);

      pOut->num_found = 0;

      for (INT32 max_trades = pIn->max_trades, n = 0; n < max_trades; n++) {

        trade_id = pIn->trade_id[n];

#ifdef _TRACE
        cout << "TRACE: [1,1:" << n << "] trade_id=" << trade_id << '\r' << endl;
        cout << "TRACE: [1,1:" << n << "] max_trades=" << pIn->max_trades << '\r' << endl;
#endif
        if (m_pStmt[1]->Execute()) { // SELECT : trade,trade_type

          row_count = m_pStmt[1]->RowCount();

#ifdef _TRACE
          cout << "TRACE: [1:" << n << "] row_count=" << row_count << '\r' << endl;
#endif
          pOut->num_found += row_count;

          for (SQLLEN size = row_count, i = 0; i < size; i++) {

            memset(&trade_info, 0x00, sizeof(trade_info));

            m_pStmt[1]->Fetch();

#ifdef _TRACE
            cout << "TRACE: [1,1:" << n << ":" << i << "] bid_price="
                 << trade_info.bid_price << '\r' << endl;
            cout << "TRACE: [1,1:" << n << ":" << i << "] exec_name="
                 << trade_info.exec_name << '\r' << endl;
            cout << "TRACE: [1,1:" << n << ":" << i << "] is_cash="
                 << trade_info.is_cash << '\r' << endl;
            cout << "TRACE: [1,1:" << n << ":" << i << "] is_market="
                 << trade_info.is_market << '\r' << endl;
            cout << "TRACE: [1,1:" << n << ":" << i << "] trade_price=";
            if(0 <= trade_info.trade_price_ind)
              cout << trade_info.trade_price << '\r' << endl;
            else
              cout << "NULL" << '\r' << endl;
#endif
          }

          if (0 < row_count) {

            { // -- 5 --
              ((CSettlementStmt *)(pStmt = m_pStmt[(p = 5)]))
                ->BindParameter(&trade_id)
                ->BindCol(&trade_info);
#ifdef _TRACE
              cout << "TRACE: [1,1:" << n << "," << p << "] trade_id=" << trade_id << '\r' << endl;
#endif
              if (pStmt->Execute()) { // SELECT : settlement

                for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

                  pStmt->Fetch();
#ifdef _TRACE
                  cout << "TRACE: [1,1:" << n << "," << p << ":" << i << "] settlement_amount="
                       << trade_info.settlement_amount << '\r' << endl;
                  cout << "TRACE: [1,1:" << n << "," << p << ":" << i << "] settlement_cash_due_date="
                       << trade_info.settlement_cash_due_date << '\r' << endl;
                  cout << "TRACE: [1,1:" << n << "," << p << ":" << i << "] settlement_cash_type="
                       << trade_info.settlement_cash_type << '\r' << endl;
#endif
                }

                pStmt->Cancel();
              }
            } // -- 5 --

            if (1 == trade_info.is_cash) { // -- 6 --

              ((CCashTransactionStmt *)(pStmt = m_pStmt[(p = 6)]))
                ->BindParameter(&trade_id)
                ->BindCol(&trade_info);
#ifdef _TRACE
              cout << "TRACE: [1,1:" << n << "," << p << "] trade_id=" << trade_id << '\r' << endl;
#endif
              if (pStmt->Execute()) { // SELECT : cash_transaction

                for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

                  pStmt->Fetch();
#ifdef _TRACE
                  cout << "TRACE: [1,1:" << n << "," << p << ":" << i << "] cash_transaction_amount="
                       << trade_info.cash_transaction_amount << '\r' << endl;
                  cout << "TRACE: [1,1:" << n << "," << p << ":" << i << "] cash_transaction_dts="
                       << trade_info.cash_transaction_dts << '\r' << endl;
                  cout << "TRACE: [1,1:" << n << "," << p << ":" << i << "] cash_transaction_name="
                       << trade_info.cash_transaction_name << '\r' << endl;
#endif
                }

                pStmt->Cancel();
              }
            } // -- 6 --

            { // -- 7 --
              ((CTradeHistoryStmt *)(pStmt = m_pStmt[(p = 7)]))
                ->BindParameter(&trade_id)
                ->BindCol(&trade_info);
#ifdef _TRACE
              cout << "TRACE: [1,1:" << n << "," << p << "] trade_id=" << trade_id << '\r' << endl;
#endif
              if (pStmt->Execute()) { // SELECT : trade_history

                row_count = pStmt->RowCount();

                for (int size = row_count, i = 0; i < size; i++) {

                  pStmt->Fetch();
#ifdef _TRACE
                  cout << "TRACE: [1,1:" << n << "," << p << ":" << i << "] trade_history_dts="
                       << trade_info.trade_history_dts[i] << '\r' << endl;
                  cout << "TRACE: [1,1:" << n << "," << p << ":" << i << "] trade_history_status_id="
                       << trade_info.trade_history_status_id[i] << '\r' << endl;
#endif
                }
                trade_info.trade_history_status_id[row_count][0] = '\0'; // SHOULD?

                pStmt->Cancel();
              }
            } // -- 7 --

            memcpy(&pOut->trade_info[n], &trade_info, sizeof(trade_info));
          }

          m_pStmt[1]->Cancel();
        }
      } // for (max_trades)

#ifdef _TRACE
      cout << "TRACE: [1] num_found=" << pOut->num_found << '\r' << endl;
#endif
    } // -- 1 --

    m_pDbc->CommitTransaction();
  }

  /*
   */
  void CTradeLookupDB::DoTradeLookupFrame2(const TTradeLookupFrame2Input *pIn,
                                           TTradeLookupFrame2Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeLookupDB::DoTradeLookupFrame2/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    { // -- 2 --
      TTradeLookupFrame2TradeInfo trade_info;
      SQLLEN row_count;

      CHandleStmt *pStmt;
      int p;

      ((CTradeLookupDBStmt2 *)m_pStmt[2])
        ->BindParameter(pIn)
        ->BindCol(&trade_info);

      pOut->num_found = 0;

#ifdef _TRACE
      cout << "TRACE: [2,2] acct_id=" << pIn->acct_id << '\r' << endl;
      cout << "TRACE: [2,2] start_trade_dts=" << pIn->start_trade_dts << '\r' << endl;
      cout << "TRACE: [2,2] trade_history_dts=" << pIn->end_trade_dts << '\r' << endl;
#endif

      if (m_pStmt[2]->Execute()) {

        row_count = m_pStmt[2]->RowCount();
        pOut->num_found = MIN(row_count, pIn->max_trades);

#ifdef _TRACE
        cout << "TRACE: [2,2] row_count=" << row_count << '\r' << endl;
        cout << "TRACE: [2,2] max_trades=" << pIn->max_trades << '\r' << endl;
        cout << "TRACE: [2,2] num_found=" << pOut->num_found << '\r' << endl;
#endif
        for (INT32 max_trades = pOut->num_found, n = 0; n < max_trades; n++) {

          memset(&trade_info, 0x00, sizeof(trade_info));

          m_pStmt[2]->Fetch();

#ifdef _TRACE
          cout << "TRACE: [2,2:" << n << "] bid_price=" << trade_info.bid_price << '\r' << endl;
          cout << "TRACE: [2,2:" << n << "] exec_name=" << trade_info.exec_name << '\r' << endl;
          cout << "TRACE: [2,2:" << n << "] is_cash=" << trade_info.is_cash << '\r' << endl;
          cout << "TRACE: [2,2:" << n << "] trade_id=" << trade_info.trade_id << '\r' << endl;
          cout << "TRACE: [2,2:" << n << "] trade_price=";
          if(0 <= trade_info.trade_price_ind)
            cout << trade_info.trade_price << '\r' << endl;
          else
            cout << "NULL" << '\r' << endl;
#endif
          { // -- 5 --
            ((CSettlementStmt *)(pStmt = m_pStmt[(p = 5)]))
              ->BindParameter(&trade_info.trade_id)
              ->BindCol(&trade_info);
#ifdef _TRACE
            cout << "TRACE: [2,2:" << n << "," << p << "] trade_id="
                 << trade_info.trade_id << '\r' << endl;
#endif
            if (pStmt->Execute()) { // SELECT : settlement

              for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

                pStmt->Fetch();
#ifdef _TRACE
                cout << "TRACE: [2,2:" << n << "," << p << "] settlement_amount="
                     << trade_info.settlement_amount << '\r' << endl;
                cout << "TRACE: [2,2:" << n << "," << p << "] settlement_cash_due_date="
                     << trade_info.settlement_cash_due_date << '\r' << endl;
                cout << "TRACE: [2,2:" << n << "," << p << "] settlement_cash_type="
                     << trade_info.settlement_cash_type << '\r' << endl;
#endif
              }

              pStmt->Cancel();
            }
          } // -- 5 --

          if (1 == trade_info.is_cash) { // -- 6 --

            ((CCashTransactionStmt *)(pStmt = m_pStmt[(p = 6)]))
              ->BindParameter(&trade_info.trade_id)
              ->BindCol(&trade_info);
#ifdef _TRACE
            cout << "TRACE: [2,2:" << n << "," << p << "] trade_id="
                 << trade_info.trade_id << '\r' << endl;
#endif
            if (pStmt->Execute()) { // SELECT : cash_transaction

              for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

                pStmt->Fetch();
#ifdef _TRACE
                cout << "TRACE: [2,2:" << n << "," << p << ":" << i << "] cash_transaction_amount="
                     << trade_info.cash_transaction_amount << '\r' << endl;
                cout << "TRACE: [2,2:" << n << "," << p << ":" << i << "] cash_transaction_dts="
                     << trade_info.cash_transaction_dts << '\r' << endl;
                cout << "TRACE: [2,2:" << n << "," << p << ":" << i << "] cash_transaction_name="
                     << trade_info.cash_transaction_name << '\r' << endl;
#endif
              }

              pStmt->Cancel();
            }
          } // -- 6 --

          { // -- 7 --
            ((CTradeHistoryStmt *)(pStmt = m_pStmt[(p = 7)]))
              ->BindParameter(&trade_info.trade_id)
              ->BindCol(&trade_info);
#ifdef _TRACE
            cout << "TRACE: [2,2:" << n << "," << p << "] trade_id="
                 << trade_info.trade_id << '\r' << endl;
#endif
            if (pStmt->Execute()) { // SELECT : trade_history

              row_count = pStmt->RowCount();

              for (int size = row_count, i = 0; i < size; i++) {

                pStmt->Fetch();
#ifdef _TRACE
                cout << "TRACE: [2,2:" << n << "," << p << ":" << i << "] trade_history_dts="
                     << trade_info.trade_history_dts[i] << '\r' << endl;
                cout << "TRACE: [2,2:" << n << "," << p << ":" << i << "] trade_history_status_id="
                     << trade_info.trade_history_status_id[i] << '\r' << endl;
#endif
              }
              trade_info.trade_history_status_id[row_count][0] = '\0'; // SHOULD?

              pStmt->Cancel();
            }
          } // -- 7 --

          memcpy(&pOut->trade_info[n], &trade_info, sizeof(trade_info));

        } // for (max_trades)

        m_pStmt[2]->Cancel();
      }
    } // -- 2 --

    m_pDbc->CommitTransaction();
  }

  /*
   */
  void CTradeLookupDB::DoTradeLookupFrame3(const TTradeLookupFrame3Input *pIn,
                                           TTradeLookupFrame3Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeLookupDB::DoTradeLookupFrame3/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    { // -- 3 --
      TTradeLookupFrame3TradeInfo trade_info;
      SQLLEN row_count;

      CHandleStmt *pStmt;
      int p;

      ((CTradeLookupDBStmt3 *)m_pStmt[3])
        ->BindParameter(pIn)
        ->BindCol(&trade_info);

      pOut->num_found = 0;

#ifdef _TRACE
      cout << "TRACE: [3,3] symbol=" << pIn->symbol << '\r' << endl;
      cout << "TRACE: [3,3] start_trade_dts=" << pIn->start_trade_dts << '\r' << endl;
      cout << "TRACE: [3,3] end_trade_dts=" << pIn->end_trade_dts << '\r' << endl;
      cout << "TRACE: [3,3] max_acct_id=" << pIn->max_acct_id << '\r' << endl;
      cout << "TRACE: [3,3] max_trades=" << pIn->max_trades << '\r' << endl;
#endif

      if (m_pStmt[3]->Execute()) {

        row_count = m_pStmt[3]->RowCount();
        pOut->num_found = MIN(row_count, pIn->max_trades);

#ifdef _TRACE
        cout << "TRACE: [3,3] row_count=" << row_count << '\r' << endl;
        cout << "TRACE: [3,3] max_trades=" << pIn->max_trades << '\r' << endl;
        cout << "TRACE: [3,3] num_found=" << pOut->num_found << '\r' << endl;
#endif
        for (INT32 max_trades = pOut->num_found, n = 0; n < max_trades; n++) {

          memset(&trade_info, 0x00, sizeof(trade_info));

          m_pStmt[3]->Fetch();

#ifdef _TRACE
          cout << "TRACE: [3,3:" << n << "] acct_id=" << trade_info.acct_id << '\r' << endl;
          cout << "TRACE: [3,3:" << n << "] exec_name=" << trade_info.exec_name << '\r' << endl;
          cout << "TRACE: [3,3:" << n << "] is_cash=" << trade_info.is_cash << '\r' << endl;
          cout << "TRACE: [3,3:" << n << "] price=" << trade_info.price << '\r' << endl;
          cout << "TRACE: [3,3:" << n << "] quantity=" << trade_info.quantity << '\r' << endl;
          cout << "TRACE: [3,3:" << n << "] trade_dts=" << trade_info.trade_dts << '\r' << endl;
          cout << "TRACE: [3,3:" << n << "] trade_id=" << trade_info.trade_id << '\r' << endl;
          cout << "TRACE: [3,3:" << n << "] trade_type=" << trade_info.trade_type << '\r' << endl;
#endif
          { // -- 5 --
            ((CSettlementStmt *)(pStmt = m_pStmt[(p = 5)]))
              ->BindParameter(&trade_info.trade_id)
              ->BindCol(&trade_info);
#ifdef _TRACE
            cout << "TRACE: [3,3:" << n << "," << p << "] trade_id="
                 << trade_info.trade_id << '\r' << endl;
#endif
            if (pStmt->Execute()) { // SELECT : settlement

              for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

                pStmt->Fetch();
#ifdef _TRACE
                cout << "TRACE: [3,3:" << n << "," << p << "] settlement_amount="
                     << trade_info.settlement_amount << '\r' << endl;
                cout << "TRACE: [3,3:" << n << "," << p << "] settlement_cash_due_date="
                     << trade_info.settlement_cash_due_date << '\r' << endl;
                cout << "TRACE: [3,3:" << n << "," << p << "] settlement_cash_type="
                     << trade_info.settlement_cash_type << '\r' << endl;
#endif
              }

              pStmt->Cancel();
            }
          } // -- 5 --

          if (1 == trade_info.is_cash) { // -- 6 --

            ((CCashTransactionStmt *)(pStmt = m_pStmt[(p = 6)]))
              ->BindParameter(&trade_info.trade_id)
              ->BindCol(&trade_info);
#ifdef _TRACE
            cout << "TRACE: [3,3:" << n << "," << p << "] trade_id="
                 << trade_info.trade_id << '\r' << endl;
#endif
            if (pStmt->Execute()) { // SELECT : cash_transaction

              for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

                pStmt->Fetch();
#ifdef _TRACE
                cout << "TRACE: [3,3:" << n << "," << p << ":" << i << "] cash_transaction_amount="
                     << trade_info.cash_transaction_amount << '\r' << endl;
                cout << "TRACE: [3,3:" << n << "," << p << ":" << i << "] cash_transaction_dts="
                     << trade_info.cash_transaction_dts << '\r' << endl;
                cout << "TRACE: [3,3:" << n << "," << p << ":" << i << "] cash_transaction_name="
                     << trade_info.cash_transaction_name << '\r' << endl;
#endif
              }

              pStmt->Cancel();
            }
          } // -- 6 --

          { // -- 7 --
            ((CTradeHistoryStmt *)(pStmt = m_pStmt[(p = 7)]))
              ->BindParameter(&trade_info.trade_id)
              ->BindCol(&trade_info);
#ifdef _TRACE
            cout << "TRACE: [3,3:" << n << "," << p << "] trade_id="
                 << trade_info.trade_id << '\r' << endl;
#endif
            if (pStmt->Execute()) { // SELECT : trade_history

              row_count = pStmt->RowCount();

              for (int size = row_count, i = 0; i < size; i++) {

                pStmt->Fetch();
#ifdef _TRACE
                cout << "TRACE: [3,3:" << n << "," << p << ":" << i << "] trade_history_dts="
                     << trade_info.trade_history_dts[i] << '\r' << endl;
                cout << "TRACE: [3,3:" << n << "," << p << ":" << i << "] trade_history_status_id="
                     << trade_info.trade_history_status_id[i] << '\r' << endl;
#endif
              }
              trade_info.trade_history_status_id[row_count][0] = '\0'; // SHOULD?

              pStmt->Cancel();
            }
          } // -- 7 --

          memcpy(&pOut->trade_info[n], &trade_info, sizeof(trade_info));

        } // for (max_trades)

        m_pStmt[3]->Cancel();
      }
    } // -- 3 --

    m_pDbc->CommitTransaction();
  }

  /*
   */
  void CTradeLookupDB::DoTradeLookupFrame4(const TTradeLookupFrame4Input *pIn,
                                           TTradeLookupFrame4Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeLookupDB::DoTradeLookupFrame4/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    { // -- 4 --
      TTradeLookupFrame4TradeInfo trade_info;

      CHandleStmt *pStmt;
      int p;

      ((CTradeLookupDBStmt4 *)m_pStmt[4])
        ->BindParameter(pIn)
        ->BindCol(&pOut->trade_id);

      pOut->num_trades_found = 0;
      pOut->num_found = 0;

#ifdef _TRACE
      cout << "TRACE: [4,4] acct_id=" << pIn->acct_id << '\r' << endl;
      cout << "TRACE: [4,4] trade_dts=" << pIn->trade_dts << '\r' << endl;
#endif

      if (m_pStmt[4]->Execute()) {

        pOut->num_trades_found = m_pStmt[4]->RowCount();

#ifdef _TRACE
        cout << "TRACE: [4,4] num_trades_found=" << pOut->num_trades_found << '\r' << endl;
#endif
        for (INT32 max_trades = pOut->num_trades_found, n = 0; n < max_trades; n++) {

          m_pStmt[4]->Fetch();

#ifdef _TRACE
          cout << "TRACE: [4,4:" << n << "] trade_id=" << pOut->trade_id << '\r' << endl;
#endif
          { // -- 8 --
            ((CHoldingHistoryStmt *)(pStmt = m_pStmt[(p = 8)]))
              ->BindParameter(&pOut->trade_id)
              ->BindCol(&trade_info);

#ifdef _TRACE
            cout << "TRACE: [4,4:" << n << "," << p << "] trade_id="
                 << pOut->trade_id << '\r' << endl;
#endif
            if (pStmt->Execute()) {

              pOut->num_found = m_pStmt[4]->RowCount();

#ifdef _TRACE
              cout << "TRACE: [4,4:" << n << "," << p << "] num_found=" << pOut->num_found << '\r' << endl;
#endif
              for (int size = pOut->num_found, i = 0; i < size; i++) {

                memset(&trade_info, 0x00, sizeof(trade_info));

                pStmt->Fetch();
#ifdef _TRACE
                cout << "TRACE: [4,4:" << n << "," << p << ":" << i << "] holding_history_id="
                     << trade_info.holding_history_id << '\r' << endl;
                cout << "TRACE: [4,4:" << n << "," << p << ":" << i << "] holding_history_trade_id="
                     << trade_info.holding_history_trade_id << '\r' << endl;
                cout << "TRACE: [4,4:" << n << "," << p << ":" << i << "] quantity_before="
                     << trade_info.quantity_before << '\r' << endl;
                cout << "TRACE: [4,4:" << n << "," << p << ":" << i << "] quantity_after="
                     << trade_info.quantity_after << '\r' << endl;
#endif
                memcpy(&pOut->trade_info[i], &trade_info, sizeof(trade_info));
              }

              pStmt->Cancel();
            }
          } // -- 8 --

        } // for (max_trades)

        m_pStmt[4]->Cancel();
      }
    } // -- 4 --

    m_pDbc->CommitTransaction();
  }

}
