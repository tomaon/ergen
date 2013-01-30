/*
 */

#include "MarketFeedDB.H"

#include "HandleDbc.H"
#include "HandleStmt.H"

namespace TPCE {

  /*
   */
  class CMarketFeedDBStmt0 : public CHandleStmt {
  public:
    CMarketFeedDBStmt0(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "UPDATE last_trade"
        "   SET lt_price = ?"
        "     , lt_vol = lt_vol + ?"
        "     , lt_dts = ?"
        " WHERE lt_s_symb = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CMarketFeedDBStmt0() {}

    virtual CMarketFeedDBStmt0 *BindParameter(const TTickerEntry *entry,
                                              const TIMESTAMP_STRUCT *now_dts) {
      (*((CHandleStmt *)this))
        .BindParameter(1, &entry->price_quote, sizeof(entry->price_quote))
        .BindParameter(2, &entry->trade_qty, sizeof(entry->trade_qty))
        .BindParameter(3, now_dts, sizeof(TIMESTAMP_STRUCT))
        .BindParameter(4, entry->symbol, sizeof(entry->symbol));

      return this;
    }
  };

  /*
   */
  class CMarketFeedDBStmt1 : public CHandleStmt {
  public:
    CMarketFeedDBStmt1(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  tr_t_id AS 'req_trade_id'"
        ", tr_bid_price AS 'req_price_quote'"
        ", tr_tt_id AS 'req_trade_type'"
        ", tr_qty AS 'req_trade_qty'"
        "  FROM trade_request"
        " WHERE tr_s_symb = ?"
        "   AND ("
        "    (tr_tt_id = ? AND tr_bid_price >= ?)"
        "     OR"
        "    (tr_tt_id = ? AND tr_bid_price <= ?)"
        "     OR"
        "    (tr_tt_id = ? AND tr_bid_price >= ?)"
        "  )";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CMarketFeedDBStmt1() {}

    virtual CMarketFeedDBStmt1 *BindCol(TTradeRequest *trade_request) {

      (*((CHandleStmt *)this))
        .BindCol(1, &trade_request->trade_id, sizeof(trade_request->trade_id))
        .BindCol(2, &trade_request->price_quote, sizeof(trade_request->price_quote))
        .BindCol(3, trade_request->trade_type_id, sizeof(trade_request->trade_type_id))
        .BindCol(4, &trade_request->trade_qty, sizeof(trade_request->trade_qty));

      return this;
    }

    virtual CMarketFeedDBStmt1 *BindParameter(const TTickerEntry *entry,
                                              const TStatusAndTradeType *type) {
      (*((CHandleStmt *)this))
        .BindParameter(1, entry->symbol, sizeof(entry->symbol))
        .BindParameter(2, type->type_stop_loss, sizeof(type->type_stop_loss))
        .BindParameter(3, &entry->price_quote, sizeof(entry->price_quote))
        .BindParameter(4, type->type_limit_sell, sizeof(type->type_limit_sell))
        .BindParameter(5, &entry->price_quote, sizeof(entry->price_quote))
        .BindParameter(6, type->type_limit_buy, sizeof(type->type_limit_buy))
        .BindParameter(7, &entry->price_quote, sizeof(entry->price_quote));

      return this;
    }
  };

  /*
   */
  class CMarketFeedDBStmt2 : public CHandleStmt {
  public:
    CMarketFeedDBStmt2(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "UPDATE trade"
        "   SET t_dts = ?"
        "     , t_st_id = ?"
        " WHERE t_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CMarketFeedDBStmt2() {}

    virtual CMarketFeedDBStmt2 *BindParameter(const TTradeRequest *trade_request,
                                              const TStatusAndTradeType *type,
                                              const TIMESTAMP_STRUCT *now_dts) {
      (*((CHandleStmt *)this))
        .BindParameter(1, now_dts, sizeof(TIMESTAMP_STRUCT))
        .BindParameter(2, type->status_submitted, sizeof(type->status_submitted))
        .BindParameter(3, &trade_request->trade_id, sizeof(trade_request->trade_id));

      return this;
    }
  };

  /*
   */
  class CMarketFeedDBStmt3 : public CHandleStmt {
  public:
    CMarketFeedDBStmt3(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "DELETE"
        "  FROM trade_request"
        " WHERE tr_t_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CMarketFeedDBStmt3() {}

    virtual CMarketFeedDBStmt3 *BindParameter(const TTradeRequest *trade_request) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &trade_request->trade_id, sizeof(trade_request->trade_id));

      return this;
    }
  };

  /*
   */
  class CMarketFeedDBStmt4 : public CHandleStmt {
  public:
    CMarketFeedDBStmt4(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "INSERT INTO trade_history ("
        "  th_t_id"
        ", th_dts"
        ", th_st_id"
        ") VALUES ("
        "  ?"
        ", ?"
        ", ?"
        ")";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CMarketFeedDBStmt4() {}

    virtual CMarketFeedDBStmt4 *BindParameter(const TTradeRequest *trade_request,
                                              const TStatusAndTradeType *type,
                                              const TIMESTAMP_STRUCT *now_dts) {
      (*((CHandleStmt *)this))
        .BindParameter(1, &trade_request->trade_id, sizeof(trade_request->trade_id))
        .BindParameter(2, now_dts, sizeof(TIMESTAMP_STRUCT))
        .BindParameter(3, type->status_submitted, sizeof(type->status_submitted));

      return this;
    }
  };

  /*
   */
  CMarketFeedDB::CMarketFeedDB(CHandleDbc *pDbc)
    : CMarketFeedDBInterface(), m_pDbc(pDbc) {

    m_pStmt[0] = new CMarketFeedDBStmt0(pDbc);
    m_pStmt[1] = new CMarketFeedDBStmt1(pDbc);
    m_pStmt[2] = new CMarketFeedDBStmt2(pDbc);
    m_pStmt[3] = new CMarketFeedDBStmt3(pDbc);
    m_pStmt[4] = new CMarketFeedDBStmt4(pDbc);
  }

  /*
   */
  CMarketFeedDB::~CMarketFeedDB() {

    delete m_pStmt[0];
    delete m_pStmt[1];
    delete m_pStmt[2];
    delete m_pStmt[3];
    delete m_pStmt[4];
  }

  /*
   */
  void CMarketFeedDB::DoMarketFeedFrame1(const TMarketFeedFrame1Input *pIn,
                                         TMarketFeedFrame1Output *pOut,
                                         CSendToMarketInterface *pSendToMarket) {
#ifdef _TRACE
    cout << "TRACE: CMarketFeedDB::DoMarketFeedFrame1/3" << '\r' << endl;
#endif

    TTradeRequest trade_request, trade_request_buffer[max_feed_len];
    TIMESTAMP_STRUCT now_dts;
    SQLLEN row_count;
    INT32 rows_sent;

    current_timestamp(&now_dts);

    pOut->num_updated = 0;
    pOut->send_len = 0;

    for (int n = 0; n < max_feed_len; n++) {

      const TStatusAndTradeType *type = &pIn->StatusAndTradeType;
      const TTickerEntry *entry = &pIn->Entries[n];

#ifdef _TRACE
      if ('\0' == entry->symbol[0]) continue; // for debug
#endif

      memset(trade_request_buffer, 0x00, sizeof(trade_request_buffer));
      rows_sent = 0;

      m_pDbc->StartTransaction();

      {
        row_count = 0;

        { // -- 0 --
          ((CMarketFeedDBStmt0 *)m_pStmt[0])
            ->BindParameter(entry, &now_dts);

#ifdef _TRACE
          cout << "TRACE: [1:" << n << ",0] price_quote=" << entry->price_quote << '\r' << endl;
          cout << "TRACE: [1:" << n << ",0] trade_qty=" << entry->trade_qty << '\r' << endl;
          cout << "TRACE: [1:" << n << ",0] now_dts=" << now_dts << '\r' << endl;
          cout << "TRACE: [1:" << n << ",0] symbol=" << entry->symbol << '\r' << endl;
#endif
          if (m_pStmt[0]->Execute()) { // UPDATE : last_trade

            row_count = m_pStmt[0]->RowCount();

#ifdef _TRACE
            cout << "TRACE: [1:" << n << ",0] row_count=" << row_count << '\r' << endl;
#endif
            m_pStmt[0]->Cancel();
          }
        } // -- 0 --

        if (0 < row_count) { // -- 1 --

          pOut->num_updated += row_count;

          ((CMarketFeedDBStmt1 *)m_pStmt[1])
            ->BindParameter(entry, type)
            ->BindCol(&trade_request);

#ifdef _TRACE
          cout << "TRACE: [1:" << n << ",1] symbol=" << entry->symbol << '\r' << endl;
          cout << "TRACE: [1:" << n << ",1] price_quote=" << entry->price_quote << '\r' << endl;
          cout << "TRACE: [1:" << n << ",1] type_stop_loss="  << type->type_stop_loss << '\r' << endl;
          cout << "TRACE: [1:" << n << ",1] type_limit_sell=" << type->type_limit_sell << '\r' << endl;
          cout << "TRACE: [1:" << n << ",1] type_limit_buy=" << type->type_limit_buy << '\r' << endl;
#endif
          if (m_pStmt[1]->Execute()) { // SELECT : trade_request

            for (int size = m_pStmt[1]->RowCount(), i = 0; i < size; i++) {

              memset(&trade_request, 0x00, sizeof(trade_request));

              m_pStmt[1]->Fetch();
#ifdef _TRACE
              cout << "TRACE: [1:" << n << ",1:" << i << "] req_trade_id="
                   << trade_request.trade_id << '\r' << endl;
              cout << "TRACE: [1:" << n << ",1:" << i << "] req_price_quote="
                   << trade_request.price_quote << '\r' << endl;
              cout << "TRACE: [1:" << n << ",1:" << i << "] req_trade_type="
                   << trade_request.trade_type_id << '\r' << endl;
              cout << "TRACE: [1:" << n << ",1:" << i << "] req_trade_qty="
                   << trade_request.trade_qty << '\r' << endl;
#endif
              { // -- 2 --
                ((CMarketFeedDBStmt2 *)m_pStmt[2])
                  ->BindParameter(&trade_request, type, &now_dts);
#ifdef _TRACE
                cout << "TRACE: [1:" << n << ",1:" << i << ",2] now_dts="
                     << now_dts << '\r' << endl;
                cout << "TRACE: [1:" << n << ",1:" << i << ",2] status_submitted="
                     << type->status_submitted << '\r' << endl;
                cout << "TRACE: [1:" << n << ",1:" << i << ",2] req_trade_id="
                     << trade_request.trade_id << '\r' << endl;
#endif
                if (m_pStmt[2]->Execute()) { // UPDATE : trade
#ifdef _TRACE
                  cout << "TRACE: [1:" << n << ",1:" << i << ",2] row_count="
                       << m_pStmt[2]->RowCount() << '\r' << endl;
#endif
                  m_pStmt[2]->Cancel();
                }
              } // -- 2 --

              { // -- 3 --
                ((CMarketFeedDBStmt3 *)m_pStmt[3])
                  ->BindParameter(&trade_request);
#ifdef _TRACE
                cout << "TRACE: [1:" << n << ",1:" << i << ",3] req_trade_id="
                     << trade_request.trade_id << '\r' << endl;
#endif
                if (m_pStmt[3]->Execute()) { // DELETE : trade_request
#ifdef _TRACE
                  cout << "TRACE: [1:" << n << ",1:" << i << ",3] row_count="
                       << m_pStmt[3]->RowCount() << '\r' << endl;
#endif
                  m_pStmt[3]->Cancel();
                }
              } // -- 3 --

              { // -- 4 --
                ((CMarketFeedDBStmt4 *)m_pStmt[4])
                  ->BindParameter(&trade_request, type, &now_dts);
#ifdef _TRACE
                cout << "TRACE: [1:" << n << ",1:" << i << ",4] req_trade_id="
                     << trade_request.trade_id << '\r' << endl;
                cout << "TRACE: [1:" << n << ",1:" << i << ",4] now_dts="
                     << now_dts << '\r' << endl;
                cout << "TRACE: [1:" << n << ",1:" << i << ",4] status_submitted="
                     << type->status_submitted << '\r' << endl;
#endif
                if (m_pStmt[4]->Execute()) { // INSERT : trade_history
#ifdef _TRACE
                  cout << "TRACE: [1:" << n << ",1:" << i << ",4] row_count="
                       << m_pStmt[4]->RowCount() << '\r' << endl;
#endif
                  m_pStmt[4]->Cancel();
                }
              } // -- 4 --

              memcpy(&trade_request_buffer[rows_sent++],
                     &trade_request, sizeof(trade_request_buffer[0]));
            }

            m_pStmt[1]->Cancel();
          }
        } // -- 1 --

      }

      m_pDbc->CommitTransaction();

#ifdef _TRACE
      cout << "TRACE: [1:" << n << "] rows_sent=" << rows_sent << '\r' << endl;
#endif

      {
        for (int i = 0; i < rows_sent; i++) {
          pSendToMarket->SendToMarketFromFrame(trade_request_buffer[i]);
        }
        pOut->send_len += rows_sent;
      }

    } // for (max_feed_len)

#ifdef _TRACE
    cout << "TRACE: [1] num_updated=" << pOut->num_updated << '\r' << endl;
    cout << "TRACE: [1] send_len=" << pOut->send_len << '\r' << endl;
#endif
  }

}
