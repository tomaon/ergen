/*
 */

#include "TradeCleanupDB.H"

#include "HandleDbc.H"
#include "HandleStmt.H"

namespace TPCE {

  /*
   */
  class CTradeCleanupDBStmt0 : public CHandleStmt {
  public:
    CTradeCleanupDBStmt0(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT tr_t_id"
        "  FROM trade_request"
        " ORDER BY"
        "  tr_t_id";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeCleanupDBStmt0() {}

    virtual CTradeCleanupDBStmt0 *BindCol(TTrade *tr_t_id) {

      (*((CHandleStmt *)this))
        .BindCol(1, tr_t_id, sizeof(TTrade));

      return this;
    }
  };

  /*
   */
  class CTradeCleanupDBStmt1 : public CHandleStmt {
  public:
    CTradeCleanupDBStmt1(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

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

    virtual ~CTradeCleanupDBStmt1() {}

    virtual CTradeCleanupDBStmt1 *BindParameter(const TTrade *th_t_id,
                                                const TIMESTAMP_STRUCT *th_dts,
                                                const char *th_st_id, size_t th_st_id_len) {
      (*((CHandleStmt *)this))
        .BindParameter(1, th_t_id, sizeof(TTrade))
        .BindParameter(2, th_dts, sizeof(TIMESTAMP_STRUCT))
        .BindParameter(3, th_st_id, th_st_id_len);

      return this;
    }
  };

  /*
   */
  class CTradeCleanupDBStmt2 : public CHandleStmt {
  public:
    CTradeCleanupDBStmt2(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "UPDATE trade"
        "   SET t_st_id = ?"
        "     , t_dts = ?"
        " WHERE t_id = ? ";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeCleanupDBStmt2() {}

    virtual CTradeCleanupDBStmt2 *BindParameter(const TTrade *t_t_id,
                                                const TIMESTAMP_STRUCT *t_dts,
                                                const char *t_st_id, size_t t_st_id_len) {
      (*((CHandleStmt *)this))
        .BindParameter(1, t_st_id, t_st_id_len)
        .BindParameter(2, t_dts, sizeof(TIMESTAMP_STRUCT))
        .BindParameter(3, t_t_id, sizeof(TTrade));

      return this;
    }
  };

  /*
   */
  class CTradeCleanupDBStmt4 : public CHandleStmt {
  public:
    CTradeCleanupDBStmt4(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "DELETE"
        "  FROM trade_request";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeCleanupDBStmt4() {}
  };

  /*
   */
  class CTradeCleanupDBStmt5 : public CHandleStmt {
  public:
    CTradeCleanupDBStmt5(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT t_id"
        "  FROM trade"
        " WHERE t_id >= ?"
        "   AND t_st_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeCleanupDBStmt5() {}

    virtual CTradeCleanupDBStmt5 *BindParameter(const TTradeCleanupFrame1Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->start_trade_id, sizeof(pIn->start_trade_id))
        .BindParameter(2, pIn->st_submitted_id, sizeof(pIn->st_submitted_id));

      return this;
    }

    virtual CTradeCleanupDBStmt5 *BindCol(TTrade *t_id) {

      (*((CHandleStmt *)this))
        .BindCol(1, t_id, sizeof(TTrade));

      return this;
    }
  };

  /*
   */
  CTradeCleanupDB::CTradeCleanupDB(CHandleDbc *pDbc)
    : CTradeCleanupDBInterface(), m_pDbc(pDbc) {

    m_pStmt[0] = new CTradeCleanupDBStmt0(pDbc);
    m_pStmt[1] = new CTradeCleanupDBStmt1(pDbc);
    m_pStmt[2] = new CTradeCleanupDBStmt2(pDbc);
    m_pStmt[4] = new CTradeCleanupDBStmt4(pDbc);
    m_pStmt[5] = new CTradeCleanupDBStmt5(pDbc);
  }

  /*
   */
  CTradeCleanupDB::~CTradeCleanupDB() {

    delete m_pStmt[0];
    delete m_pStmt[1];
    delete m_pStmt[2];
    delete m_pStmt[4];
    delete m_pStmt[5];
  }

  /*
   */
  void CTradeCleanupDB::DoTradeCleanupFrame1(const TTradeCleanupFrame1Input *pIn) {

#ifdef _TRACE
    cout << "TRACE: CTradeCleanupDB::DoTradeCleanupFrame1/1" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    {
      TTrade trade_id;
      TIMESTAMP_STRUCT now_dts;

      CHandleStmt *pStmt;
      int p;

      current_timestamp(&now_dts);

      { // -- 0 --
        ((CTradeCleanupDBStmt0 *)m_pStmt[0])
          ->BindCol(&trade_id);

        if (m_pStmt[0]->Execute()) { // SELECT : trade_request

          for (INT32 max_trades = m_pStmt[0]->RowCount(), n = 0; n < max_trades; n++) {

            trade_id = 0;

            m_pStmt[0]->Fetch();

#ifdef _TRACE
            cout << "TRACE: [1,0:" << n << "] trade_id=" << trade_id << '\r' << endl;
#endif
            { // -- 1 --
              ((CTradeCleanupDBStmt1 *)(pStmt = m_pStmt[(p = 1)]))
                ->BindParameter(&trade_id, &now_dts,
                                pIn->st_submitted_id, sizeof(pIn->st_submitted_id));
#ifdef _TRACE
              cout << "TRACE: [1,0:" << n << "," << p << "] trade_id="
                   << trade_id << '\r' << endl;
              cout << "TRACE: [1,0:" << n << "," << p << "] now_dts="
                   << now_dts << '\r' << endl;
              cout << "TRACE: [1,0:" << n << "," << p << "] st_submitted_id="
                   << pIn->st_submitted_id << '\r' << endl;
#endif
              if (pStmt->Execute()) { // INSERT : trade_history
#ifdef _TRACE
                cout << "TRACE: [1,0:" << n << "," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
                pStmt->Cancel();
              }
            } // -- 1 --

            { // -- 2 --
              ((CTradeCleanupDBStmt2 *)(pStmt = m_pStmt[(p = 2)]))
                ->BindParameter(&trade_id, &now_dts,
                                pIn->st_canceled_id, sizeof(pIn->st_canceled_id));
#ifdef _TRACE
              cout << "TRACE: [1,0:" << n << "," << p << "] trade_id="
                   << trade_id << '\r' << endl;
              cout << "TRACE: [1,0:" << n << "," << p << "] now_dts="
                   << now_dts << '\r' << endl;
              cout << "TRACE: [1,0:" << n << "," << p << "] st_canceled_id="
                   << pIn->st_canceled_id << '\r' << endl;
#endif
              if (pStmt->Execute()) { // UPDATE : trade
#ifdef _TRACE
                cout << "TRACE: [1,0:" << n << "," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
                pStmt->Cancel();
              }
            } // -- 2 --

            { // -- 3 (=1) --
              ((CTradeCleanupDBStmt1 *)(pStmt = m_pStmt[1]))
                ->BindParameter(&trade_id, &now_dts,
                                pIn->st_canceled_id, sizeof(pIn->st_canceled_id));
#ifdef _TRACE
              cout << "TRACE: [1,0:" << n << ",3] trade_id="
                   << trade_id << '\r' << endl;
              cout << "TRACE: [1,0:" << n << ",3] now_dts="
                   << now_dts << '\r' << endl;
              cout << "TRACE: [1,0:" << n << ",3] st_canceled_id="
                   << pIn->st_canceled_id << '\r' << endl;
#endif
              if (pStmt->Execute()) { // INSERT : trade_history
#ifdef _TRACE
                cout << "TRACE: [1,0:" << n << ",3] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
                pStmt->Cancel();
              }
            } // -- 3 --

          } // for (max_trades)

          m_pStmt[0]->Cancel();
        }
      } // -- 0 --

      { // -- 4 --
        pStmt = m_pStmt[(p = 4)];
        if (pStmt->Execute()) { // DELETE : trade_request

#ifdef _TRACE
          cout << "TRACE: [1," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
          pStmt->Cancel();
        }
      } // -- 4 --

      { // -- 5 --
        ((CTradeCleanupDBStmt5 *)m_pStmt[5])
          ->BindParameter(pIn)
          ->BindCol(&trade_id);

#ifdef _TRACE
        cout << "TRACE: [1,5] start_trade_id=" << pIn->start_trade_id << '\r' << endl;
        cout << "TRACE: [1,5] st_submitted_id=" << pIn->st_submitted_id << '\r' << endl;
#endif
        if (m_pStmt[5]->Execute()) { // SELECT : trade

          for (int max_trades = m_pStmt[5]->RowCount(), n = 0; n < max_trades; n++) {

            trade_id = 0;

            m_pStmt[5]->Fetch();

#ifdef _TRACE
            cout << "TRACE: [1,5:" << n << "] trade_id=" << trade_id << '\r' << endl;
#endif
            { // -- 6 (=2) --
              ((CTradeCleanupDBStmt2 *)(pStmt = m_pStmt[2]))
                ->BindParameter(&trade_id, &now_dts,
                                pIn->st_canceled_id, sizeof(pIn->st_canceled_id));
#ifdef _TRACE
              cout << "TRACE: [1,5" << n << ",6] trade_id=" << trade_id << '\r' << endl;
              cout << "TRACE: [1,5" << n << ",6] now_dts=" << now_dts << '\r' << endl;
              cout << "TRACE: [1,5" << n << ",6] st_canceled_id=" << pIn->st_canceled_id << '\r' << endl;
#endif
              if (pStmt->Execute()) { // UPDATE : trade
#ifdef _TRACE
                cout << "TRACE: [1,5" << n << ",6] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
                pStmt->Cancel();
              }
            } // -- 6 --

            { // -- 7 (=1) --
              ((CTradeCleanupDBStmt1 *)(pStmt = m_pStmt[1]))
                ->BindParameter(&trade_id, &now_dts,
                                pIn->st_canceled_id, sizeof(pIn->st_canceled_id));
#ifdef _TRACE
              cout << "TRACE: [1,5" << n << ",7] trade_id=" << trade_id << '\r' << endl;
              cout << "TRACE: [1,5" << n << ",7] now_dts=" << now_dts << '\r' << endl;
              cout << "TRACE: [1,5" << n << ",7] st_canceled_id=" << pIn->st_canceled_id << '\r' << endl;
#endif
              if (pStmt->Execute()) { // INSERT : trade_history
#ifdef _TRACE
                cout << "TRACE: [1,5" << n << ",7] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
                pStmt->Cancel();
              }
            } // -- 7 --

          } // for (max_trades)
        }
      } // -- 5 --
    }

    m_pDbc->CommitTransaction();
  }

}
