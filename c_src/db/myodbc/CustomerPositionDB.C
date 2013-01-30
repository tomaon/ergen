/*
 */

#include "CustomerPositionDB.H"

#include <sstream>

#include "HandleDbc.H"
#include "HandleStmt.H"

namespace TPCE {

  /*
   */
  class CCustomerPositionDBStmt0 : public CHandleStmt {
  public:
    CCustomerPositionDBStmt0(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT c_id AS 'cust_id'"
        "  FROM customer"
        " WHERE c_tax_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CCustomerPositionDBStmt0() {}

    virtual CCustomerPositionDBStmt0 *BindCol(TCustomerPositionFrame1Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol(1, &pOut->cust_id, sizeof(pOut->cust_id));

      return this;
    }

    virtual CCustomerPositionDBStmt0 *BindParameter(const TCustomerPositionFrame1Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, pIn->tax_id, sizeof(pIn->tax_id));

      return this;
    }

  };

  /*
   */
  class CCustomerPositionDBStmt1 : public CHandleStmt {
  public:
    CCustomerPositionDBStmt1(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  c_st_id"
        ", c_l_name"
        ", c_f_name"
        ", c_m_name"  // nullable
        ", c_gndr"    // nullable
        ", c_tier"
        ", c_dob"
        ", c_ad_id"
        ", c_ctry_1"  // nullable
        ", c_area_1"  // nullable
        ", c_local_1" // nullable
        ", c_ext_1"   // nullable
        ", c_ctry_2"  // nullable
        ", c_area_2"  // nullable
        ", c_local_2" // nullable
        ", c_ext_2"   // nullable
        ", c_ctry_3"  // nullable
        ", c_area_3"  // nullable
        ", c_local_3" // nullable
        ", c_ext_3"   // nullable
        ", c_email_1" // nullable
        ", c_email_2" // nullable
        "  FROM customer"
        " WHERE c_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CCustomerPositionDBStmt1() {}

    virtual CCustomerPositionDBStmt1 *BindCol(TCustomerPositionFrame1Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol( 1, pOut->c_st_id, sizeof(pOut->c_st_id))
        .BindCol( 2, pOut->c_l_name, sizeof(pOut->c_l_name))
        .BindCol( 3, pOut->c_f_name, sizeof(pOut->c_f_name))
        .BindCol( 4, pOut->c_m_name, sizeof(pOut->c_m_name))
        .BindCol( 5, pOut->c_gndr, sizeof(pOut->c_gndr))
        .BindCol( 6, &pOut->c_tier, sizeof(pOut->c_tier))
        .BindCol( 7, &pOut->c_dob, sizeof(pOut->c_dob))
        .BindCol( 8, &pOut->c_ad_id, sizeof(pOut->c_ad_id))
        .BindCol( 9, pOut->c_ctry_1, sizeof(pOut->c_ctry_1))
        .BindCol(10, pOut->c_area_1, sizeof(pOut->c_area_1))
        .BindCol(11, pOut->c_local_1, sizeof(pOut->c_local_1))
        .BindCol(12, pOut->c_ext_1, sizeof(pOut->c_ext_1))
        .BindCol(13, pOut->c_ctry_2, sizeof(pOut->c_ctry_2))
        .BindCol(14, pOut->c_area_2, sizeof(pOut->c_area_2))
        .BindCol(15, pOut->c_local_2, sizeof(pOut->c_local_2))
        .BindCol(16, pOut->c_ext_2, sizeof(pOut->c_ext_2))
        .BindCol(17, pOut->c_ctry_3, sizeof(pOut->c_ctry_3))
        .BindCol(18, pOut->c_area_3, sizeof(pOut->c_area_3))
        .BindCol(19, pOut->c_local_3, sizeof(pOut->c_local_3))
        .BindCol(20, pOut->c_ext_3, sizeof(pOut->c_ext_3))
        .BindCol(21, pOut->c_email_1, sizeof(pOut->c_email_1))
        .BindCol(22, pOut->c_email_2, sizeof(pOut->c_email_2));

      return this;
    }

    virtual CCustomerPositionDBStmt1 *BindParameter(const TIdent *cust_id) {

      (*((CHandleStmt *)this))
        .BindParameter(1, cust_id, sizeof(TIdent));

      return this;
    }

  };

  /*
   */
  class CCustomerPositionDBStmt2 : public CHandleStmt {
  private:
    TIdent acct_id;
    double cash_bal;
    double asset_total;

    int index;
    TIdent *acct_id_returned[max_acct_len];
    double *cash_bal_returned[max_acct_len];
    double *asset_total_returned[max_acct_len];

  public:
    CCustomerPositionDBStmt2(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      std::ostringstream oss;
      // oss << "SELECT";
      // oss << "  ca_id AS 'acct_id'";
      // oss << ", ca_bal AS 'cash_bal'";
      // oss << ", SUM(hs_qty * lt_price) AS 'asset_total'";
      // oss << "  FROM customer_account";
      // oss << "  LEFT OUTER JOIN holding_summary ON hs_ca_id = ca_id";
      // oss << "  JOIN last_trade ON lt_s_symb = hs_s_symb"; // ?LEFT OUTER JOIN?
      // oss << " WHERE ca_c_id = ?";
      // oss << " GROUP BY";
      // oss << "  ca_id";
      // oss << ", ca_bal";
      // oss << " ORDER BY";
      // oss << "  asset_total ASC";
      // oss << " LIMIT " << max_acct_len;

      oss << "SELECT";
      oss << "  ca_id AS 'acct_id'";
      oss << ", ca_bal AS 'cash_bal'";
      oss << ", COALESCE(";
      oss << "    (SELECT SUM(hs_qty * lt_price)";
      oss << "       FROM holding_summary";
      oss << "       JOIN last_trade ON lt_s_symb = hs_s_symb";
      oss << "      WHERE hs_ca_id = ca_id)";
      oss << "    ,0) AS 'asset_total'";
      oss << "  FROM customer_account";
      oss << " WHERE ca_c_id = ?";
      oss << " GROUP BY";
      oss << "  ca_id";
      oss << ", ca_bal";
      oss << " ORDER BY";
      oss << "  asset_total ASC";
      oss << " LIMIT " << max_acct_len;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CCustomerPositionDBStmt2() {}

    virtual CCustomerPositionDBStmt2 *BindCol() {

      (*((CHandleStmt *)this))
        .BindCol(1, &acct_id, sizeof(acct_id))
        .BindCol(2, &cash_bal, sizeof(cash_bal))
        .BindCol(3, &asset_total, sizeof(asset_total));

      return this;
    }

    virtual CCustomerPositionDBStmt2 *BindCol(TCustomerPositionFrame1Output *pOut) {

      for (int i = 0; i < max_acct_len; i++) {
        acct_id_returned[i] = &pOut->acct_id[i];
        cash_bal_returned[i] = &pOut->cash_bal[i];
        asset_total_returned[i] = &pOut->asset_total[i];
      }

      return BindCol();
    }

    virtual CCustomerPositionDBStmt2 *BindParameter(const TIdent *cust_id) {

      (*((CHandleStmt *)this))
        .BindParameter(1, cust_id, sizeof(TIdent));

      return this;
    }

    virtual bool Execute() {

      index = 0;

      return CHandleStmt::Execute();
    }

    virtual bool Fetch() {

      acct_id = 0;
      cash_bal = 0.0;
      asset_total = 0.0;

      bool result = CHandleStmt::Fetch();

      if (result) {

        *acct_id_returned[index] = acct_id;
        *cash_bal_returned[index] = cash_bal;
        *asset_total_returned[index] = asset_total;

        index++;
      }

      return result;
    }

  };

  /*
   */
  class CCustomerPositionDBStmt3 : public CHandleStmt {
  private:
    TTrade trade_id;
    char symbol[cSYMBOL_len+1];
    INT32 qty;
    char trade_status[cST_NAME_len+1];
    TIMESTAMP_STRUCT hist_dts;

    int index;
    TTrade *trade_id_returned[max_hist_len];
    char *symbol_returned[max_hist_len];
    INT32 *qty_returned[max_hist_len];
    char *trade_status_returned[max_hist_len];
    TIMESTAMP_STRUCT *hist_dts_returned[max_hist_len];

  public:
    CCustomerPositionDBStmt3(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      std::ostringstream oss;
      oss << "SELECT";
      oss << "  t_id AS 'trade_id'";
      oss << ", t_s_symb AS 'symbol'";
      oss << ", t_qty AS 'qty'";
      oss << ", st_name AS 'trade_status'";
      oss << ", th_dts AS 'hist_dts'";
      oss << "  FROM (";
      oss << "    SELECT t_id AS 'id'";
      oss << "      FROM trade";
      oss << "     WHERE t_ca_id = ?";
      oss << "     ORDER BY";
      oss << "       t_dts DESC";
      oss << "     LIMIT 10";
      oss << "  ) AS t";
      oss << "  JOIN trade ON t_id = id";
      oss << "  JOIN trade_history ON th_t_id = t_id";
      oss << "  JOIN status_type ON st_id = th_st_id";
      oss << " ORDER BY";
      oss << "  th_dts DESC";
      oss << " LIMIT " << max_hist_len;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CCustomerPositionDBStmt3() {}

    virtual CCustomerPositionDBStmt3 *BindCol() {

      (*((CHandleStmt *)this))
        .BindCol(1, &trade_id, sizeof(trade_id))
        .BindCol(2, symbol, sizeof(symbol))
        .BindCol(3, &qty, sizeof(qty))
        .BindCol(4, trade_status, sizeof(trade_status))
        .BindCol(5, &hist_dts, sizeof(hist_dts));

      return this;
    }

    virtual CCustomerPositionDBStmt3 *BindCol(TCustomerPositionFrame2Output *pOut) {

      for (int i = 0; i < max_hist_len; i++) {
        trade_id_returned[i] = &pOut->trade_id[i];
        symbol_returned[i] = pOut->symbol[i];
        qty_returned[i] = &pOut->qty[i];
        trade_status_returned[i] = pOut->trade_status[i];
        hist_dts_returned[i] = &pOut->hist_dts[i];
      }

      return BindCol();
    }

    virtual CCustomerPositionDBStmt3 *BindParameter(const TCustomerPositionFrame2Input *pIn) {

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
      memset(symbol, 0x00, sizeof(symbol));
      qty = 0;
      memset(trade_status, 0x00, sizeof(trade_status));
      memset(&hist_dts, 0x00, sizeof(hist_dts));

      bool result = CHandleStmt::Fetch();

      if (result) {

        *trade_id_returned[index] = trade_id;
        memcpy(symbol_returned[index], symbol, sizeof(symbol));
        *qty_returned[index] = qty;
        memcpy(trade_status_returned[index], trade_status, sizeof(trade_status));
        memcpy(hist_dts_returned[index], &hist_dts, sizeof(hist_dts));

        index++;
      }

      return result;
    }

  };

  /*
   */
  CCustomerPositionDB::CCustomerPositionDB(CHandleDbc *pDbc)
    : CCustomerPositionDBInterface(), m_pDbc(pDbc) {

    m_pStmt[0] = new CCustomerPositionDBStmt0(pDbc);
    m_pStmt[1] = new CCustomerPositionDBStmt1(pDbc);
    m_pStmt[2] = new CCustomerPositionDBStmt2(pDbc);
    m_pStmt[3] = new CCustomerPositionDBStmt3(pDbc);
  }

  /*
   */
  CCustomerPositionDB::~CCustomerPositionDB() {

    delete m_pStmt[0];
    delete m_pStmt[1];
    delete m_pStmt[2];
    delete m_pStmt[3];
  }

  /*
   */
  void CCustomerPositionDB::DoCustomerPositionFrame1(const TCustomerPositionFrame1Input *pIn,
                                                     TCustomerPositionFrame1Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CCustomerPositionDB::DoCustomerPositionFrame1/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction(); // Commit -> 4, Rollback -> _

    {
      CHandleStmt *pStmt;
      int p;

      if (0 != pIn->cust_id) {

        pOut->cust_id = pIn->cust_id;

      } else { // -- 0 --

        ((CCustomerPositionDBStmt0 *)(pStmt = m_pStmt[(p = 0)]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] tax_id=" << pIn->tax_id << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : customer

          for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [1," << p << ":" << i << "] cust_id=" << pOut->cust_id << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 0 --

      if (0 != pOut->cust_id) {

        { // -- 1 --
          ((CCustomerPositionDBStmt1 *)(pStmt = m_pStmt[(p = 1)]))
            ->BindParameter(&pOut->cust_id)
            ->BindCol(pOut);

#ifdef _TRACE
          cout << "TRACE: [1," << p << "] cust_id=" << pOut->cust_id << '\r' << endl;
#endif
          if (pStmt->Execute()) { // SELECT : customer

            for (INT32 size =  pStmt->RowCount(), i = 0; i < size; i++) {

              pStmt->Fetch();

#ifdef _TRACE
              cout << "TRACE: [1," << p << ":" << i << "] c_st_id=" << pOut->c_st_id << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_l_name=" << pOut->c_l_name << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_f_name=" << pOut->c_f_name << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_m_name=" << pOut->c_m_name << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_gndr=" << pOut->c_gndr << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_tier=" << (int)pOut->c_tier << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_dob=" << pOut->c_dob << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_ad_id=" << pOut->c_ad_id << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_ctry_1=" << pOut->c_ctry_1 << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_area_1=" << pOut->c_area_1 << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_local_1=" << pOut->c_local_1 << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_ext_1=" << pOut->c_ext_1 << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_ctry_2=" << pOut->c_ctry_2 << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_area_2=" << pOut->c_area_2 << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_local_2=" << pOut->c_local_2 << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_ext_2=" << pOut->c_ext_2 << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_ctry_3=" << pOut->c_ctry_3 << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_area_3=" << pOut->c_area_3 << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_local_3=" << pOut->c_local_3 << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_ext_3=" << pOut->c_ext_3 << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_email_1=" << pOut->c_email_1 << '\r' << endl;
              cout << "TRACE: [1," << p << ":" << i << "] c_email_2=" << pOut->c_email_2 << '\r' << endl;
#endif
            }

            pStmt->Cancel();
          }
        } // -- 1 --

        { // -- 2 --
          ((CCustomerPositionDBStmt2 *)(pStmt = m_pStmt[(p = 2)]))
            ->BindParameter(&pOut->cust_id)
            ->BindCol(pOut);

#ifdef _TRACE
          cout << "TRACE: [1:" << p << "] cust_id=" << pOut->cust_id << '\r' << endl;
#endif
          if (pStmt->Execute()) { // SELECT : customer,holding_summary,last_trade

            pOut->acct_len = pStmt->RowCount();

#ifdef _TRACE
            cout << "TRACE: [1:" << p << "] acct_len=" << pOut->acct_len << '\r' << endl;
#endif
            for (int size = pOut->acct_len, i = 0; i < size; i++) {

              pStmt->Fetch();

#ifdef _TRACE
              cout << "TRACE: [1:" << p << ":" << i << "] acct_id=" << pOut->acct_id[i] << '\r' << endl;
              cout << "TRACE: [1:" << p << ":" << i << "] cash_bal=" << pOut->cash_bal[i] << '\r' << endl;
              cout << "TRACE: [1:" << p << ":" << i << "] asset_total=" << pOut->asset_total[i] << '\r' << endl;
#endif
            }

            pStmt->Cancel();
          }
        } // -- 2 --

      } // 0 != pOut->cust_id

    }
  }

  /*
   */
  void CCustomerPositionDB::DoCustomerPositionFrame2(const TCustomerPositionFrame2Input *pIn,
                                                     TCustomerPositionFrame2Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CCustomerPositionDB::DoCustomerPositionFrame2/2" << '\r' << endl;
#endif

    {
      CHandleStmt *pStmt;
      int p;

      { // -- 3 --
        ((CCustomerPositionDBStmt3 *)(pStmt = m_pStmt[(p = 3)]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [2:" << p << "] acct_id=" << pIn->acct_id << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : trade,trade_history,status_type

          pOut->hist_len = pStmt->RowCount();

#ifdef _TRACE
          cout << "TRACE: [2:" << p << "] hist_len=" << pOut->hist_len << '\r' << endl;
#endif
          for (int size = pOut->hist_len, i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [2:" << p << ":" << i << "] trade_id=" << pOut->trade_id[i] << '\r' << endl;
            cout << "TRACE: [2:" << p << ":" << i << "] symbol=" << pOut->symbol[i] << '\r' << endl;
            cout << "TRACE: [2:" << p << ":" << i << "] qty=" << pOut->qty[i] << '\r' << endl;
            cout << "TRACE: [2:" << p << ":" << i << "] trade_status=" << pOut->trade_status[i] << '\r' << endl;
            cout << "TRACE: [2:" << p << ":" << i << "] hist_dts=" << pOut->hist_dts[i] << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 3 --

    }
  }

  /*
   */
  void CCustomerPositionDB::DoCustomerPositionFrame3(void) {

#ifdef _TRACE
    cout << "TRACE: CCustomerPositionDB::DoCustomerPositionFrame3/0" << '\r' << endl;
#endif

    m_pDbc->CommitTransaction();
  }

}
