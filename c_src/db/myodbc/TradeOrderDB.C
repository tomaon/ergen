/*
 */

#include "TradeOrderDB.H"

#include <sstream>

#include "HandleDbc.H"
#include "HandleStmt.H"
#include "Sequence.H"

namespace TPCE {

  using namespace std;

  /*
   */
  class CTradeOrderDBStmt01 : public CHandleStmt {
  public:
    CTradeOrderDBStmt01(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  ca_name AS 'acct_name'"
        ", ca_b_id AS 'broker_id'"
        ", b_name AS 'broker_name'"
        ", c_f_name AS 'cust_f_name'"
        ", ca_c_id AS 'cust_id'"
        ", c_l_name AS 'cust_l_name'"
        ", c_tier AS 'cust_tier'"
        ", c_tax_id AS 'tax_id'"
        ", ca_tax_st AS 'tax_status'"
        "  FROM customer_account"
        "  JOIN customer ON c_id = ca_c_id"
        "  JOIN broker ON b_id = ca_b_id"
        " WHERE ca_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt01() {}

    virtual CTradeOrderDBStmt01 *BindCol(TTradeOrderFrame1Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol(1, pOut->acct_name, sizeof(pOut->acct_name))
        .BindCol(2, &pOut->broker_id, sizeof(pOut->broker_id))
        .BindCol(3, pOut->broker_name, sizeof(pOut->broker_name))
        .BindCol(4, pOut->cust_f_name, sizeof(pOut->cust_f_name))
        .BindCol(5, &pOut->cust_id, sizeof(pOut->cust_id))
        .BindCol(6, pOut->cust_l_name, sizeof(pOut->cust_l_name))
        .BindCol(7, &pOut->cust_tier, sizeof(pOut->cust_tier))
        .BindCol(8, pOut->tax_id, sizeof(pOut->tax_id))
        .BindCol(9, &pOut->tax_status, sizeof(pOut->tax_status));

      return this;
    }

    virtual CTradeOrderDBStmt01 *BindParameter(const TTradeOrderFrame1Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt02 : public CHandleStmt {
  public:
    CTradeOrderDBStmt02(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT ap_acl"
        "  FROM account_permission"
        " WHERE ap_ca_id = ?"
        "   AND ap_f_name = ?"
        "   AND ap_l_name = ?"
        "   AND ap_tax_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt02() {}

    virtual CTradeOrderDBStmt02 *BindCol(TTradeOrderFrame2Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol(1, pOut->ap_acl, sizeof(pOut->ap_acl));

      return this;
    }

    virtual CTradeOrderDBStmt02 *BindParameter(const TTradeOrderFrame2Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(2, pIn->exec_f_name, sizeof(pIn->exec_f_name))
        .BindParameter(3, pIn->exec_l_name, sizeof(pIn->exec_l_name))
        .BindParameter(4, pIn->exec_tax_id, sizeof(pIn->exec_tax_id));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt03 : public CHandleStmt {
  public:
    CTradeOrderDBStmt03(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  s_ex_id AS 'exch_id'"
        ", s_name"
        ", s_symb AS 'symbol'"
        "  FROM company"
        "  JOIN security ON s_co_id = co_id"
        "  JOIN last_trade ON lt_s_symb = s_symb"
        " WHERE co_name = ?"
        "   AND s_issue = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt03() {}

    virtual CTradeOrderDBStmt03 *BindCol(TTradeOrderFrame3Output *pOut,
                                         char *exch_id, size_t exch_id_len) {
      (*((CHandleStmt *)this))
        .BindCol(1, exch_id, exch_id_len)
        .BindCol(2, pOut->s_name, sizeof(pOut->s_name))
        .BindCol(3, pOut->symbol, sizeof(pOut->symbol));

      return this;
    }

    virtual CTradeOrderDBStmt03 *BindParameter(const TTradeOrderFrame3Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, pIn->co_name, sizeof(pIn->co_name))
        .BindParameter(2, pIn->issue, sizeof(pIn->issue));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt04 : public CHandleStmt {
  public:
    CTradeOrderDBStmt04(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  s_ex_id AS 'exch_id'"
        ", co_name"
        ", s_name"
        "  FROM security"
        "  JOIN company ON co_id = s_co_id"
        " WHERE s_symb = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt04() {}

    virtual CTradeOrderDBStmt04 *BindCol(TTradeOrderFrame3Output *pOut,
                                         char *exch_id, size_t exch_id_len) {
      (*((CHandleStmt *)this))
        .BindCol(1, exch_id, exch_id_len)
        .BindCol(2, pOut->co_name, sizeof(pOut->co_name))
        .BindCol(3, pOut->s_name, sizeof(pOut->s_name));

      return this;
    }

    virtual CTradeOrderDBStmt04 *BindParameter(const TTradeOrderFrame3Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, pIn->symbol, sizeof(pIn->symbol));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt05 : public CHandleStmt {
  public:
    CTradeOrderDBStmt05(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT lt_price AS 'market_price'"
        "  FROM last_trade"
        " WHERE lt_s_symb = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt05() {}

    virtual CTradeOrderDBStmt05 *BindCol(TTradeOrderFrame3Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol(1, &pOut->market_price, sizeof(pOut->market_price));

      return this;
    }

    virtual CTradeOrderDBStmt05 *BindParameter(const TTradeOrderFrame3Output *pOut) {

      (*((CHandleStmt *)this))
        .BindParameter(1, pOut->symbol, sizeof(pOut->symbol));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt06 : public CHandleStmt {
  public:
    CTradeOrderDBStmt06(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "   tt_is_mrkt AS 'type_is_market'"
        " , tt_is_sell AS 'type_is_sell'"
        "  FROM trade_type"
        " WHERE tt_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt06() {}

    virtual CTradeOrderDBStmt06 *BindCol(TTradeOrderFrame3Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol(1, &pOut->type_is_market, sizeof(pOut->type_is_market))
        .BindCol(2, &pOut->type_is_sell, sizeof(pOut->type_is_sell));

      return this;
    }

    virtual CTradeOrderDBStmt06 *BindParameter(const TTradeOrderFrame3Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, pIn->trade_type_id, sizeof(pIn->trade_type_id));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt07 : public CHandleStmt {
  public:
    CTradeOrderDBStmt07(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT hs_qty"
        "  FROM holding_summary"
        " WHERE hs_ca_id = ?"
        "   AND hs_s_symb = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt07() {}

    virtual CTradeOrderDBStmt07 *BindCol(INT32 *hs_qty) {

      (*((CHandleStmt *)this))
        .BindCol(1, hs_qty, sizeof(INT32));

      return this;
    }

    virtual CTradeOrderDBStmt07 *BindParameter(const TTradeOrderFrame3Input *pIn,
                                               const TTradeOrderFrame3Output *pOut) {
      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(2, pOut->symbol, sizeof(pOut->symbol));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt08 : public CHandleStmt {
  public:
    CTradeOrderDBStmt08(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  h_qty AS 'hold_qty'"
        ", h_price AS 'hold_price'"
        "  FROM holding"
        " WHERE h_ca_id = ?"
        "   AND h_s_symb = ?"
        " ORDER BY"
        "  h_dts DESC";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt08() {}

    virtual CTradeOrderDBStmt08 *BindCol(INT32 *hold_qty, double *hold_price) {

      (*((CHandleStmt *)this))
        .BindCol(1, hold_qty, sizeof(INT32))
        .BindCol(2, hold_price, sizeof(double));

      return this;
    }

    virtual CTradeOrderDBStmt08 *BindParameter(const TTradeOrderFrame3Input *pIn,
                                               const TTradeOrderFrame3Output *pOut) {
      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(2, pOut->symbol, sizeof(pOut->symbol));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt09 : public CHandleStmt {
  public:
    CTradeOrderDBStmt09(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  h_qty AS 'hold_qty'"
        ", h_price AS 'hold_price'"
        "  FROM holding"
        " WHERE h_ca_id = ?"
        "   AND h_s_symb = ?"
        " ORDER BY"
        "  h_dts ASC";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt09() {}

    virtual CTradeOrderDBStmt09 *BindCol(INT32 *hold_qty, double *hold_price) {

      (*((CHandleStmt *)this))
        .BindCol(1, hold_qty, sizeof(INT32))
        .BindCol(2, hold_price, sizeof(double));

      return this;
    }

    virtual CTradeOrderDBStmt09 *BindParameter(const TTradeOrderFrame3Input *pIn,
                                               const TTradeOrderFrame3Output *pOut) {
      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(2, pOut->symbol, sizeof(pOut->symbol));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt10 : public CHandleStmt {
  public:
    CTradeOrderDBStmt10(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT SUM(tx_rate) AS 'tax_rates'"
        "  FROM customer_taxrate"
        "  JOIN taxrate ON tx_id = cx_tx_id"
        " WHERE cx_c_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt10() {}

    virtual CTradeOrderDBStmt10 *BindCol(double *tax_rates) {

      (*((CHandleStmt *)this))
        .BindCol(1, tax_rates, sizeof(double));

      return this;
    }

    virtual CTradeOrderDBStmt10 *BindParameter(const TTradeOrderFrame3Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->cust_id, sizeof(pIn->cust_id));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt11 : public CHandleStmt {
  public:
    CTradeOrderDBStmt11(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT cr_rate AS 'comm_rate'"
        "  FROM commission_rate"
        " WHERE cr_c_tier = ?"
        "   AND cr_tt_id = ?"
        "   AND cr_ex_id = ?"
        "   AND cr_from_qty <= ?"
        "   AND cr_to_qty >= ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt11() {}

    virtual CTradeOrderDBStmt11 *BindCol(TTradeOrderFrame3Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol(1, &pOut->comm_rate, sizeof(pOut->comm_rate));

      return this;
    }

    virtual CTradeOrderDBStmt11 *BindParameter(const TTradeOrderFrame3Input *pIn,
                                               const char *exch_id, size_t exch_id_len) {
      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->cust_tier, sizeof(pIn->cust_tier))
        .BindParameter(2, pIn->trade_type_id, sizeof(pIn->trade_type_id))
        .BindParameter(3, exch_id, exch_id_len)
        .BindParameter(4, &pIn->trade_qty, sizeof(pIn->trade_qty))
        .BindParameter(5, &pIn->trade_qty, sizeof(pIn->trade_qty));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt12 : public CHandleStmt {
  public:
    CTradeOrderDBStmt12(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT ch_chrg AS 'charge_amount'"
        "  FROM charge"
        " WHERE ch_c_tier = ?"
        "   AND ch_tt_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt12() {}

    virtual CTradeOrderDBStmt12 *BindCol(TTradeOrderFrame3Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol(1, &pOut->charge_amount, sizeof(pOut->charge_amount));

      return this;
    }

    virtual CTradeOrderDBStmt12 *BindParameter(const TTradeOrderFrame3Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->cust_tier, sizeof(pIn->cust_tier))
        .BindParameter(2, pIn->trade_type_id, sizeof(pIn->trade_type_id));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt13 : public CHandleStmt {
  public:
    CTradeOrderDBStmt13(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT ca_bal AS 'acct_bal'"
        "  FROM customer_account"
        " WHERE ca_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt13() {}

    virtual CTradeOrderDBStmt13 *BindCol(double *acct_bal) {

      (*((CHandleStmt *)this))
        .BindCol(1, acct_bal, sizeof(double));

      return this;
    }

    virtual CTradeOrderDBStmt13 *BindParameter(const TTradeOrderFrame3Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt14 : public CHandleStmt {
  public:
    CTradeOrderDBStmt14(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT sum(hs_qty * lt_price) AS 'hold_assets'"
        "  FROM holding_summary"
        "  JOIN last_trade ON lt_s_symb = hs_s_symb"
        " WHERE hs_ca_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt14() {}

    virtual CTradeOrderDBStmt14 *BindCol(double *hold_assets) {

      (*((CHandleStmt *)this))
        .BindCol(1, hold_assets, sizeof(double));

      return this;
    }

    virtual CTradeOrderDBStmt14 *BindParameter(const TTradeOrderFrame3Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt15 : public CHandleStmt {
  public:
    CTradeOrderDBStmt15(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "INSERT INTO trade ("
        "  t_id"
        ", t_dts"
        ", t_st_id"
        ", t_tt_id"
        ", t_is_cash"
        ", t_s_symb"
        ", t_qty"
        ", t_bid_price"
        ", t_ca_id"
        ", t_exec_name"
        ", t_trade_price"
        ", t_chrg"
        ", t_comm"
        ", t_tax"
        ", t_lifo"
        ") VALUES ("
        "  ?"
        ", ?"
        ", ?"
        ", ?"
        ", ?"
        ", ?"
        ", ?"
        ", ?"
        ", ?"
        ", ?"
        ", NULL" // t_trade_price
        ", ?"
        ", ?"
        ", 0"    // t_tax
        ", ?"
        ")";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt15() {}

    virtual CTradeOrderDBStmt15 *BindParameter(const TTradeOrderFrame4Input *pIn,
                                               const TTradeOrderFrame4Output *pOut,
                                               const TIMESTAMP_STRUCT *now_dts) {
      (*((CHandleStmt *)this))
        .BindParameter( 1, &pOut->trade_id, sizeof(pOut->trade_id))
        .BindParameter( 2, now_dts, sizeof(TIMESTAMP_STRUCT))
        .BindParameter( 3, pIn->status_id, sizeof(pIn->status_id))
        .BindParameter( 4, pIn->trade_type_id, sizeof(pIn->trade_type_id))
        .BindParameter( 5, &pIn->is_cash, sizeof(pIn->is_cash))
        .BindParameter( 6, pIn->symbol, sizeof(pIn->symbol))
        .BindParameter( 7, &pIn->trade_qty, sizeof(pIn->trade_qty))
        .BindParameter( 8, &pIn->requested_price, sizeof(pIn->requested_price))
        .BindParameter( 9, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(10, pIn->exec_name, sizeof(pIn->exec_name))
        .BindParameter(11, &pIn->charge_amount, sizeof(pIn->charge_amount))
        .BindParameter(12, &pIn->comm_amount, sizeof(pIn->comm_amount))
        .BindParameter(13, &pIn->is_lifo, sizeof(pIn->is_lifo));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt16 : public CHandleStmt {
  public:
    CTradeOrderDBStmt16(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "INSERT INTO trade_request ("
        "  tr_t_id"
        ", tr_tt_id"
        ", tr_s_symb"
        ", tr_qty"
        ", tr_bid_price"
        ", tr_b_id"
        ") VALUES ("
        "  ?"
        ", ?"
        ", ?"
        ", ?"
        ", ?"
        ", ?"
        ")";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeOrderDBStmt16() {}

    virtual CTradeOrderDBStmt16 *BindParameter(const TTradeOrderFrame4Input *pIn,
                                               const TTradeOrderFrame4Output *pOut) {
      (*((CHandleStmt *)this))
        .BindParameter(1, &pOut->trade_id, sizeof(pOut->trade_id))
        .BindParameter(2, pIn->trade_type_id, sizeof(pIn->trade_type_id))
        .BindParameter(3, pIn->symbol, sizeof(pIn->symbol))
        .BindParameter(4, &pIn->trade_qty, sizeof(pIn->trade_qty))
        .BindParameter(5, &pIn->requested_price, sizeof(pIn->requested_price))
        .BindParameter(6, &pIn->broker_id, sizeof(pIn->broker_id));

      return this;
    }

  };

  /*
   */
  class CTradeOrderDBStmt17 : public CHandleStmt {
  public:
    CTradeOrderDBStmt17(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

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

    virtual ~CTradeOrderDBStmt17() {}

    virtual CTradeOrderDBStmt17 *BindParameter(const TTradeOrderFrame4Input *pIn,
                                               const TTradeOrderFrame4Output *pOut,
                                               const TIMESTAMP_STRUCT *now_dts) {
      (*((CHandleStmt *)this))
        .BindParameter(1, &pOut->trade_id, sizeof(pOut->trade_id))
        .BindParameter(2, now_dts, sizeof(TIMESTAMP_STRUCT))
        .BindParameter(3, pIn->status_id, sizeof(pIn->status_id));

      return this;
    }

  };

  /*
   */
  CTradeOrderDB::CTradeOrderDB(CHandleDbc *pDbc)
    : CTradeOrderDBInterface(), m_pDbc(pDbc), m_sequence(pDbc) {

    m_pStmt[ 1] = new CTradeOrderDBStmt01(pDbc);
    m_pStmt[ 2] = new CTradeOrderDBStmt02(pDbc);
    m_pStmt[ 3] = new CTradeOrderDBStmt03(pDbc);
    m_pStmt[ 4] = new CTradeOrderDBStmt04(pDbc);
    m_pStmt[ 5] = new CTradeOrderDBStmt05(pDbc);
    m_pStmt[ 6] = new CTradeOrderDBStmt06(pDbc);
    m_pStmt[ 7] = new CTradeOrderDBStmt07(pDbc);
    m_pStmt[ 8] = new CTradeOrderDBStmt08(pDbc);
    m_pStmt[ 9] = new CTradeOrderDBStmt09(pDbc);
    m_pStmt[10] = new CTradeOrderDBStmt10(pDbc);
    m_pStmt[11] = new CTradeOrderDBStmt11(pDbc);
    m_pStmt[12] = new CTradeOrderDBStmt12(pDbc);
    m_pStmt[13] = new CTradeOrderDBStmt13(pDbc);
    m_pStmt[14] = new CTradeOrderDBStmt14(pDbc);
    m_pStmt[15] = new CTradeOrderDBStmt15(pDbc);
    m_pStmt[16] = new CTradeOrderDBStmt16(pDbc);
    m_pStmt[17] = new CTradeOrderDBStmt17(pDbc);
  }

  /*
   */
  CTradeOrderDB::~CTradeOrderDB() {

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
    delete m_pStmt[13];
    delete m_pStmt[14];
    delete m_pStmt[15];
    delete m_pStmt[16];
    delete m_pStmt[17];
  }

  /*
   */
  void CTradeOrderDB::DoTradeOrderFrame1(const TTradeOrderFrame1Input *pIn,
                                         TTradeOrderFrame1Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeOrderDB::DoTradeOrderFrame1/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    {
      CHandleStmt *pStmt;
      int p;

      { // -- 1 --
        ((CTradeOrderDBStmt01 *)(pStmt = m_pStmt[(p = 1)]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] acct_id=" << pIn->acct_id << '\r' << endl;
#endif
        if (pStmt->Execute()) {

          pOut->num_found = pStmt->RowCount();

#ifdef _TRACE
          cout << "TRACE: [1," << p << "] num_found=" << pOut->num_found << '\r' << endl;
#endif
          for (int size = pOut->num_found, i = 0; i < size; i++) {

            m_pStmt[1]->Fetch();

#ifdef _TRACE
            cout << "TRACE: [1," << p << ":" << i << "] acct_name=" << pOut->acct_name << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] broker_id=" << pOut->broker_id << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] broker_name=" << pOut->broker_name << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] cust_f_name=" << pOut->cust_f_name << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] cust_id=" << pOut->cust_id << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] cust_l_name=" << pOut->cust_l_name << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] cust_tier=" << pOut->cust_tier << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] num_found=" << pOut->num_found << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] tax_id=" << pOut->tax_id << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] tax_status=" << pOut->tax_status << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 1 --
    }
  }

  /*
   */
  void CTradeOrderDB::DoTradeOrderFrame2(const TTradeOrderFrame2Input *pIn,
                                         TTradeOrderFrame2Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeOrderDB::DoTradeOrderFrame2/2" << '\r' << endl;
#endif

    {
      CHandleStmt *pStmt;
      int p;

      { // -- 2 --
        ((CTradeOrderDBStmt02 *)(pStmt = m_pStmt[(p = 2)]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [2," << p << "] acct_id=" << pIn->acct_id << '\r' << endl;
        cout << "TRACE: [2," << p << "] exec_f_name=" << pIn->exec_f_name << '\r' << endl;
        cout << "TRACE: [2," << p << "] exec_l_name=" << pIn->exec_l_name << '\r' << endl;
        cout << "TRACE: [2," << p << "] exec_tax_id=" << pIn->exec_tax_id << '\r' << endl;
#endif
        if (pStmt->Execute()) {

          for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [2," << p << ":" << i << "] ap_acl=" << pOut->ap_acl << '\r' << endl;
#endif
          }

          m_pStmt[2]->Cancel();
        }
      } // -- 2 --
    }
  }

  /*
   */
  void CTradeOrderDB::DoTradeOrderFrame3(const TTradeOrderFrame3Input *pIn,
                                         TTradeOrderFrame3Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeOrderDB::DoTradeOrderFrame3/4" << '\r' << endl;
#endif

    {
      char exch_id[cEX_ID_len+1];
      INT32 hs_qty;

      CHandleStmt *pStmt;
      int p;

      { // -- 3,4 --
        if ('\0' == pIn->symbol[0]) {

          ((CTradeOrderDBStmt03 *)(pStmt = m_pStmt[(p = 3)]))
            ->BindParameter(pIn)
            ->BindCol(pOut, exch_id, sizeof(exch_id));

#ifdef _TRACE
          cout << "TRACE: [3," << p << "] co_name=" << pIn->co_name << '\r' << endl;
          cout << "TRACE: [3," << p << "] issue=" << pIn->issue << '\r' << endl;
#endif
          strncpy(pOut->co_name, pIn->co_name, sizeof(pOut->co_name));

        } else {

          ((CTradeOrderDBStmt04 *)(pStmt = m_pStmt[(p = 4)]))
            ->BindParameter(pIn)
            ->BindCol(pOut, exch_id, sizeof(exch_id));

#ifdef _TRACE
          cout << "TRACE: [3," << p << "] symbol=" << pIn->symbol << '\r' << endl;
#endif
          strncpy(pOut->symbol, pIn->symbol, sizeof(pOut->symbol));
        }

        pOut->requested_price = pIn->requested_price;

        if (pStmt->Execute()) {

          for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [3," << p << ":" << i << "] exch_id="
                 << exch_id << '\r' << endl;
            cout << "TRACE: [3," << p << ":" << i << "] co_name="
                 << pOut->co_name << '\r' << endl;
            cout << "TRACE: [3," << p << ":" << i << "] s_name="
                 << pOut->s_name << '\r' << endl;
            cout << "TRACE: [3," << p << ":" << i << "] symbol="
                 << pOut->symbol << '\r' << endl;
            cout << "TRACE: [3," << p << ":" << i << "] requested_price="
                 << pOut->requested_price << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 3,4 --

      { // -- 5 --
        ((CTradeOrderDBStmt05 *)(pStmt = m_pStmt[(p = 5)]))
          ->BindParameter(pOut)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [3," << p << "] symbol=" << pIn->symbol << '\r' << endl;
#endif
        if (pStmt->Execute()) {

          for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [3," << p << ":" << i << "] market_price="
                 << pOut->market_price << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 5 --

      { // -- 6 --
        ((CTradeOrderDBStmt06 *)(pStmt = m_pStmt[(p = 6)]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [3," << p << "] trade_type_id=" << pIn->trade_type_id << '\r' << endl;
#endif
        if (pStmt->Execute()) {

          for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

            if (1 == pOut->type_is_market) {
              pOut->requested_price = pOut->market_price;
            }

#ifdef _TRACE
            cout << "TRACE: [3," << p << ":" << i << "] type_is_market="
                 << pOut->type_is_market << '\r' << endl;
            cout << "TRACE: [3," << p << ":" << i << "] type_is_sell="
                 << pOut->type_is_sell << '\r' << endl;
            cout << "TRACE: [3," << p << ":" << i << "] market_price="
                 << pOut->market_price << '\r' << endl;
            cout << "TRACE: [3," << p << ":" << i << "] requested_price="
                 << pIn->requested_price << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 6 --

      { // -- 7 --
        ((CTradeOrderDBStmt07 *)(pStmt = m_pStmt[(p = 7)]))
          ->BindParameter(pIn, pOut)
          ->BindCol(&hs_qty);

#ifdef _TRACE
        cout << "TRACE: [3," << p << "] acct_id=" << pIn->acct_id << '\r' << endl;
        cout << "TRACE: [3," << p << "] symbol=" << pOut->symbol << '\r' << endl;
#endif
        if (pStmt->Execute()) {

          for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [3," << p << ":" << i << "] hs_qty=" << hs_qty << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 7 --

      { // -- 8,9 --
        double buy_value = 0.0;
        double sell_value = 0.0;
        double hold_price = 0.0;
        INT32 hold_qty = 0;

        if (pIn->is_lifo) {
          ((CTradeOrderDBStmt08 *)(pStmt = m_pStmt[(p = 8)]))
            ->BindParameter(pIn, pOut)
            ->BindCol(&hold_qty, &hold_price);
        } else {
          ((CTradeOrderDBStmt09 *)(pStmt = m_pStmt[(p = 9)]))
            ->BindParameter(pIn, pOut)
            ->BindCol(&hold_qty, &hold_price);
        }

#ifdef _TRACE
        cout << "TRACE: [3," << p << "] acct_id=" << pIn->acct_id << '\r' << endl;
        cout << "TRACE: [3," << p << "] symbol=" << pOut->symbol << '\r' << endl;
        cout << "TRACE: [3," << p << "] is_lifo=" << pIn->is_lifo << '\r' << endl;
#endif
        if (pStmt->Execute()) {

          double requested_price = pOut->requested_price;
          INT32 needed_qty = pIn->trade_qty;

          if (1 == pOut->type_is_sell) { // sell

            if (hs_qty > 0) {

              for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

                pStmt->Fetch();
#ifdef _TRACE
                cout << "TRACE: [3," << p << ":" << i << "][S] buy_value="
                     << pOut->buy_value << '\r' << endl;
                cout << "TRACE: [3," << p << ":" << i << "][S] sell_value="
                     << pOut->sell_value << '\r' << endl;
#endif
                if (hold_qty > needed_qty) {
                  buy_value  += needed_qty * hold_price;
                  sell_value += needed_qty * requested_price;
                  needed_qty  = 0;
                } else {
                  buy_value  += hold_qty * hold_price;
                  sell_value += hold_qty * requested_price;
                  needed_qty  = needed_qty - hold_qty;
                }
              }
            }

          } else { // buy

            if (hs_qty < 0) {

              for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

                pStmt->Fetch();
#ifdef _TRACE
                cout << "TRACE: [3," << p << ":" << i << "][B] buy_value="
                     << pOut->buy_value << '\r' << endl;
                cout << "TRACE: [3," << p << ":" << i << "][B] sell_value="
                     << pOut->sell_value << '\r' << endl;
#endif
                if (hold_qty + needed_qty < 0) {
                  buy_value  += needed_qty * requested_price;
                  sell_value += needed_qty * hold_price;
                  needed_qty  = 0;
                } else {
                  hold_qty = - hold_qty;
                  buy_value  += hold_qty * requested_price;
                  sell_value += hold_qty * hold_price;
                  needed_qty  = needed_qty - hold_qty;
                }
              }
            }

          } // buy

          pStmt->Cancel();
        }

        pOut->buy_value = buy_value;
        pOut->sell_value = sell_value;

#ifdef _TRACE
        cout << "TRACE: [3," << p << "] buy_value=" << pOut->buy_value << '\r' << endl;
        cout << "TRACE: [3," << p <<  "] sell_value=" << pOut->sell_value << '\r' << endl;
#endif
      } // -- 8,9 --

      { // -- 10 --
        pOut->tax_amount = 0.0;

        if ((pOut->sell_value > pOut->buy_value) and
            (pIn->tax_status == 1 or pIn->tax_status == 2)) {

          double tax_rates = 0.0;

          ((CTradeOrderDBStmt10 *)(pStmt = m_pStmt[(p = 10)]))
            ->BindParameter(pIn)
            ->BindCol(&tax_rates);

#ifdef _TRACE
          cout << "TRACE: [3," << p << "] cust_id=" << pIn->cust_id << '\r' << endl;
#endif
          if (pStmt->Execute()) {

            for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

              pStmt->Fetch();

#ifdef _TRACE
              cout << "TRACE: [3," << p << ":" << i << "][B] tax_rates=" << tax_rates << '\r' << endl;
#endif
            }

            pStmt->Cancel();
          }

          pOut->tax_amount = (pOut->sell_value - pOut->buy_value) * tax_rates;
        }

#ifdef _TRACE
        cout << "TRACE: [3] tax_amount=" << pOut->tax_amount << '\r' << endl;
#endif
      } // -- 10 --

      { // -- 11 --
        ((CTradeOrderDBStmt11 *)(pStmt = m_pStmt[(p = 11)]))
          ->BindParameter(pIn, exch_id, sizeof(exch_id))
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [3," << p << "] cust_tier=" << pIn->cust_tier << '\r' << endl;
        cout << "TRACE: [3," << p << "] trade_type_id=" << pIn->trade_type_id << '\r' << endl;
        cout << "TRACE: [3," << p << "] exch_id=" << exch_id << '\r' << endl;
        cout << "TRACE: [3," << p << "] trade_qty=" << pIn->trade_qty << '\r' << endl;
#endif
        if (pStmt->Execute()) {

          for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [3," << p << ":" << i << "] comm_rate=" << pOut->comm_rate << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 11 --

      { // -- 12 --
        ((CTradeOrderDBStmt12 *)(pStmt = m_pStmt[(p = 12)]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [3," << p << "] cust_tier=" << pIn->cust_tier << '\r' << endl;
        cout << "TRACE: [3," << p << "] trade_type_id=" << pIn->trade_type_id << '\r' << endl;
#endif
        if (pStmt->Execute()) {

          for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [3," << p << ":" << i << "] charge_amount="
                 << pOut->charge_amount << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 12 --

      { // -- 13,14 => 15 --
        pOut->acct_assets = 0.0;

        if (1 == pIn->type_is_margin) {

          double acct_bal = 0.0;
          double hold_assets = 0.0;

          { // -- 13 --
            ((CTradeOrderDBStmt13 *)(pStmt = m_pStmt[(p = 13)]))
              ->BindParameter(pIn)
              ->BindCol(&acct_bal);

#ifdef _TRACE
            cout << "TRACE: [3," << p << "] acct_id=" << pIn->acct_id << '\r' << endl;
#endif
            if (pStmt->Execute()) {

              for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

                pStmt->Fetch();
#ifdef _TRACE
                cout << "TRACE: [3," << p << ":" << i << "] acct_bal=" << acct_bal << '\r' << endl;
#endif
              }

              pStmt->Cancel();
            }
          } // -- 13 --

          { // -- 14 --
            ((CTradeOrderDBStmt14 *)(pStmt = m_pStmt[(p = 14)]))
              ->BindParameter(pIn)
              ->BindCol(&hold_assets);

#ifdef _TRACE
            cout << "TRACE: [3," << p << "] acct_id=" << pIn->acct_id << '\r' << endl;
#endif
            if (pStmt->Execute()) {

              for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

                pStmt->Fetch();
#ifdef _TRACE
                cout << "TRACE: [3," << p << ":" << i << "] hold_assets=" << hold_assets << '\r' << endl;
#endif
              }

              pStmt->Cancel();
            }
          } // -- 14 --

          pOut->acct_assets = hold_assets + acct_bal;
        }

#ifdef _TRACE
        cout << "TRACE: [3] acct_assets=" << pOut->acct_assets << '\r' << endl;
#endif
      } // -- 13,14 => 15 --

      {
        if (1 == pOut->type_is_market) {
          strncpy(pOut->status_id, pIn->st_submitted_id, sizeof(pOut->status_id));
        } else {
          strncpy(pOut->status_id, pIn->st_pending_id, sizeof(pOut->status_id));
        }

#ifdef _TRACE
        cout << "TRACE: [3] type_is_market=" << pOut->type_is_market << '\r' << endl;
        cout << "TRACE: [3] st_submitted_id=" << pIn->st_submitted_id << '\r' << endl;
        cout << "TRACE: [3] st_pending_id=" << pIn->st_pending_id << '\r' << endl;
        cout << "TRACE: [3] status_id=" << pOut->status_id << '\r' << endl;
#endif
      }

    }
  }

  /*
   */
  void CTradeOrderDB::DoTradeOrderFrame4(const TTradeOrderFrame4Input *pIn,
                                         TTradeOrderFrame4Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeOrderDB::DoTradeOrderFrame4/2" << '\r' << endl;
#endif

    {
      TIMESTAMP_STRUCT now_dts;

      CHandleStmt *pStmt;
      int p;

      current_timestamp(&now_dts);
      pOut->trade_id = 0;

      {
        const char s[] = "trade";
        m_sequence.Generate(s, sizeof(s), &pOut->trade_id);
      }

      if (0 != pOut->trade_id) {

        { // -- 15 --
          ((CTradeOrderDBStmt15 *)(pStmt = m_pStmt[(p = 15)]))
            ->BindParameter(pIn, pOut, &now_dts);

#ifdef _TRACE
          cout << "TRACE: [4," << p << "] trade_id=" << pOut->trade_id << '\r' << endl;
          cout << "TRACE: [4," << p << "] now_dts=" << now_dts << '\r' << endl;
          cout << "TRACE: [4," << p << "] status_id=" << pIn->status_id << '\r' << endl;
          cout << "TRACE: [4," << p << "] trade_type_id=" << pIn->trade_type_id << '\r' << endl;
          cout << "TRACE: [4," << p << "] is_cash=" << pIn->is_cash << '\r' << endl;
          cout << "TRACE: [4," << p << "] symbol=" << pIn->symbol << '\r' << endl;
          cout << "TRACE: [4," << p << "] trade_qty=" << pIn->trade_qty << '\r' << endl;
          cout << "TRACE: [4," << p << "] requested_price=" << pIn->requested_price << '\r' << endl;
          cout << "TRACE: [4," << p << "] acct_id=" << pIn->acct_id << '\r' << endl;
          cout << "TRACE: [4," << p << "] exec_name=" << pIn->exec_name << '\r' << endl;
          cout << "TRACE: [4," << p << "] charge_amount=" << pIn->charge_amount << '\r' << endl;
          cout << "TRACE: [4," << p << "] comm_amount=" << pIn->comm_amount << '\r' << endl;
          cout << "TRACE: [4," << p << "] is_lifo=" << pIn->is_lifo << '\r' << endl;
#endif
          if (pStmt->Execute()) {

#ifdef _TRACE
            cout << "TRACE: [4," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
            pStmt->Cancel();
          }
        } // -- 15 --

        if (1 != pIn->type_is_market) { // -- 16 --

          ((CTradeOrderDBStmt16 *)(pStmt = m_pStmt[(p = 16)]))
            ->BindParameter(pIn, pOut);

#ifdef _TRACE
          cout << "TRACE: [4," << p << "] trade_id=" << pOut->trade_id << '\r' << endl;
          cout << "TRACE: [4," << p << "] trade_type_id=" << pIn->trade_type_id << '\r' << endl;
          cout << "TRACE: [4," << p << "] symbol=" << pIn->symbol << '\r' << endl;
          cout << "TRACE: [4," << p << "] trade_qty=" << pIn->trade_qty << '\r' << endl;
          cout << "TRACE: [4," << p << "] requested_price=" << pIn->requested_price << '\r' << endl;
          cout << "TRACE: [4," << p << "] broker_id=" << pIn->broker_id << '\r' << endl;
#endif
          if (pStmt->Execute()) {

#ifdef _TRACE
            cout << "TRACE: [4," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
            pStmt->Cancel();
          }
        } // -- 16 --

        { // -- 17 --
          ((CTradeOrderDBStmt17 *)(pStmt = m_pStmt[(p = 17)]))
            ->BindParameter(pIn, pOut, &now_dts);

#ifdef _TRACE
          cout << "TRACE: [4," << p << "] trade_id=" << pOut->trade_id << '\r' << endl;
          cout << "TRACE: [4," << p << "] now_dts=" << now_dts << '\r' << endl;
          cout << "TRACE: [4," << p << "] status_id=" << pIn->status_id << '\r' << endl;
#endif
          if (pStmt->Execute()) {

#ifdef _TRACE
            cout << "TRACE: [4," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
            pStmt->Cancel();
          }
        } // -- 17 --

      } // 0 != trade_id
    }
  }

  /*
   */
  void CTradeOrderDB::DoTradeOrderFrame5(void) {

#ifdef _TRACE
    cout << "TRACE: CTradeOrderDB::DoTradeOrderFrame5/0" << '\r' << endl;
#endif

    m_pDbc->RollbackTransaction();
  }

  /*
   */
  void CTradeOrderDB::DoTradeOrderFrame6(void) {

#ifdef _TRACE
    cout << "TRACE: CTradeOrderDB::DoTradeOrderFrame6/0" << '\r' << endl;
#endif

    m_pDbc->CommitTransaction();
  }

}
