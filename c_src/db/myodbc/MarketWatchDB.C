/*
 */

#include "MarketWatchDB.H"

#include "HandleDbc.H"
#include "HandleStmt.H"

namespace TPCE {

  /*
   */
  class CMarketWatchDBStmt0 : public CHandleStmt {
  public:
    CMarketWatchDBStmt0(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  COALESCE(SUM(lt_price * s_num_out), 0.0) AS 'new_price'"
        ", COALESCE(SUM(dm_close * s_num_out), 0.0) AS 'old_price'"
        "  FROM watch_list"
        "  JOIN watch_item ON wi_wl_id = wl_id"
        "  JOIN security ON s_symb = wi_s_symb"
        "  JOIN last_trade ON lt_s_symb = s_symb"
        "  JOIN daily_market ON dm_s_symb = s_symb"
        " WHERE wl_c_id = ?"
        "   AND dm_date = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CMarketWatchDBStmt0() {}

    virtual CMarketWatchDBStmt0 *BindCol(double *new_price, double *old_price) {

      (*((CHandleStmt *)this))
        .BindCol(1, new_price, sizeof(double))
        .BindCol(2, old_price, sizeof(double));

      return this;
    }

    virtual CMarketWatchDBStmt0 *BindParameter(const TMarketWatchFrame1Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->c_id, sizeof(pIn->c_id))
        .BindParameter(2, &pIn->start_day, sizeof(pIn->start_day));

      return this;
    }

  };

  /*
   */
  class CMarketWatchDBStmt1 : public CHandleStmt {
  public:
    CMarketWatchDBStmt1(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  COALESCE(SUM(lt_price * s_num_out), 0) AS 'new_price'"
        ", COALESCE(SUM(dm_close * s_num_out), 0) AS 'old_price'"
        "  FROM industry"
        "  JOIN company ON co_in_id = in_id"
        "  JOIN security ON s_co_id = co_id"
        "  JOIN last_trade ON lt_s_symb = s_symb"
        "  JOIN daily_market ON dm_s_symb = s_symb"
        " WHERE in_name = ?"
        "   AND co_id between ? AND ?"
        "   AND dm_date = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CMarketWatchDBStmt1() {}

    virtual CMarketWatchDBStmt1 *BindCol(double *new_price, double *old_price) {

      (*((CHandleStmt *)this))
        .BindCol(1, new_price, sizeof(double))
        .BindCol(2, old_price, sizeof(double));

      return this;
    }

    virtual CMarketWatchDBStmt1 *BindParameter(const TMarketWatchFrame1Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, pIn->industry_name, sizeof(pIn->industry_name))
        .BindParameter(2, &pIn->starting_co_id, sizeof(pIn->starting_co_id))
        .BindParameter(3, &pIn->ending_co_id, sizeof(pIn->ending_co_id))
        .BindParameter(4, &pIn->start_day, sizeof(pIn->start_day));

      return this;
    }

  };

  /*
   */
  class CMarketWatchDBStmt2 : public CHandleStmt {
  public:
    CMarketWatchDBStmt2(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  COALESCE(SUM(lt_price * s_num_out), 0) AS 'new_price'"
        ", COALESCE(SUM(dm_close * s_num_out), 0) AS 'old_price'"
        "  FROM holding_summary"
        "  JOIN security ON s_symb = hs_s_symb"
        "  JOIN last_trade ON lt_s_symb = s_symb"
        "  JOIN daily_market ON dm_s_symb = s_symb"
        " WHERE hs_ca_id = ?" // acct_id
        "   AND dm_date = ?"; // start_day

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CMarketWatchDBStmt2() {}

    virtual CMarketWatchDBStmt2 *BindCol(double *new_price, double *old_price) {

      (*((CHandleStmt *)this))
        .BindCol(1, new_price, sizeof(double))
        .BindCol(2, old_price, sizeof(double));

      return this;
    }

    virtual CMarketWatchDBStmt2 *BindParameter(const TMarketWatchFrame1Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(2, &pIn->start_day, sizeof(pIn->start_day));

      return this;
    }

  };

  /*
   */
  CMarketWatchDB::CMarketWatchDB(CHandleDbc *pDbc)
    : CMarketWatchDBInterface(), m_pDbc(pDbc) {

    m_pStmt[0] = new CMarketWatchDBStmt0(pDbc);
    m_pStmt[1] = new CMarketWatchDBStmt1(pDbc);
    m_pStmt[2] = new CMarketWatchDBStmt2(pDbc);
  }

  /*
   */
  CMarketWatchDB::~CMarketWatchDB() {

    delete m_pStmt[0];
    delete m_pStmt[1];
    delete m_pStmt[2];
  }

  /*
   */
  void CMarketWatchDB::DoMarketWatchFrame1(const TMarketWatchFrame1Input *pIn,
                                           TMarketWatchFrame1Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CMarketWatchDB::DoMarketWatchFrame1/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    {
      double new_price;
      double old_price;

      CHandleStmt *pStmt = NULL;
      int p = 0;

      if (0 != pIn->c_id) { // Prospective-Watch

        ((CMarketWatchDBStmt0 *)(pStmt = m_pStmt[(p = 0)]))
          ->BindParameter(pIn)
          ->BindCol(&new_price, &old_price);

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] c_id=" << pIn->c_id << '\r' << endl;
        cout << "TRACE: [1," << p << "] start_day=" << pIn->start_day << '\r' << endl;
#endif
        if (pStmt->Execute()) {

#ifdef _TRACE
          cout << "TRACE: [1," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
        }

      } else if ('\0' != pIn->industry_name[0]) { // Industry-Watch

        ((CMarketWatchDBStmt1 *)(pStmt = m_pStmt[(p = 1)]))
          ->BindParameter(pIn)
          ->BindCol(&new_price, &old_price);

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] industry_name=" << pIn->industry_name << '\r' << endl;
        cout << "TRACE: [1," << p << "] starting_co_id=" << pIn->starting_co_id << '\r' << endl;
        cout << "TRACE: [1," << p << "] ending_co_id=" << pIn->ending_co_id << '\r' << endl;
        cout << "TRACE: [1," << p << "] start_day=" << pIn->start_day << '\r' << endl;
#endif
        if (pStmt->Execute()) {

#ifdef _TRACE
          cout << "TRACE: [1," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
        }

      } else if (0 != pIn->acct_id) { // Portfolio-Watch

        ((CMarketWatchDBStmt2 *)(pStmt = m_pStmt[(p = 2)]))
          ->BindParameter(pIn)
          ->BindCol(&new_price, &old_price);

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] acct_id=" << pIn->acct_id << '\r' << endl;
        cout << "TRACE: [1," << p << "] start_day=" << pIn->start_day << '\r' << endl;
#endif
        if (pStmt->Execute()) {

#ifdef _TRACE
          cout << "TRACE: [1," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
        }
      }

      if (NULL != pStmt) {

        for (int size = pStmt->RowCount() , i = 0; i < size; i++) {

          new_price = 0.0;
          old_price = 0.0;

          pStmt->Fetch();

#ifdef _TRACE
          cout << "TRACE: [1," << p << ":" << i << "] new_price=" << new_price << '\r' << endl;
          cout << "TRACE: [1," << p << ":" << i << "] old_price=" << old_price << '\r' << endl;
#endif
          if (0 != old_price) {
            pOut->pct_change = 100 * (new_price / old_price - 1);
          } else {
            pOut->pct_change = 0.0;
          }
        }

        pStmt->Cancel();
      }

#ifdef _TRACE
      cout << "TRACE: [1] pct_change=" << pOut->pct_change << '\r' << endl;
#endif
    }

    m_pDbc->CommitTransaction();
  }
}
