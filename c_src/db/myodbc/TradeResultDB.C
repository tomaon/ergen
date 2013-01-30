/*
 */

#include "TradeResultDB.H"

#include <sstream>

#include "HandleDbc.H"
#include "HandleStmt.H"

namespace TPCE {

  /*
   */
  class CTradeResultDBStmt00 : public CHandleStmt {
  public:
    CTradeResultDBStmt00(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  t_ca_id AS 'acct_id'"
        ", t_tt_id AS 'type_id'"
        ", t_s_symb AS 'symbol'"
        ", t_qty AS 'trade_qty'"
        ", t_chrg AS 'charge'"
        ", t_lifo AS 'is_lifo'"
        ", t_is_cash AS 'trade_is_cash'"
        ", tt_name AS 'type_name'"
        ", tt_is_sell AS 'type_is_sell'"
        ", tt_is_mrkt AS 'type_is_market'"
        "  FROM trade"
        "  JOIN trade_type ON tt_id = t_tt_id"
        " WHERE t_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt00() {}

    virtual CTradeResultDBStmt00 *BindCol(TTradeResultFrame1Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol( 1, &pOut->acct_id, sizeof(pOut->acct_id))
        .BindCol( 2, pOut->type_id, sizeof(pOut->type_id))
        .BindCol( 3, pOut->symbol, sizeof(pOut->symbol))
        .BindCol( 4, &pOut->trade_qty, sizeof(pOut->trade_qty))
        .BindCol( 5, &pOut->charge, sizeof(pOut->charge))
        .BindCol( 6, &pOut->is_lifo, sizeof(pOut->is_lifo))
        .BindCol( 7, &pOut->trade_is_cash, sizeof(pOut->trade_is_cash))
        .BindCol( 8, pOut->type_name, sizeof(pOut->type_name))
        .BindCol( 9, &pOut->type_is_sell, sizeof(pOut->type_is_sell))
        .BindCol(10, &pOut->type_is_market, sizeof(pOut->type_is_market));

      return this;
    }

    virtual CTradeResultDBStmt00 *BindParameter(const TTradeResultFrame1Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->trade_id, sizeof(pIn->trade_id));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt01 : public CHandleStmt {
  public:
    CTradeResultDBStmt01(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT hs_qty"
        "  FROM holding_summary"
        " WHERE hs_ca_id = ?"
        "   AND hs_s_symb = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt01() {}

    virtual CTradeResultDBStmt01 *BindCol(TTradeResultFrame1Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol(1, &pOut->hs_qty, sizeof(pOut->hs_qty));

      return this;
    }

    virtual CTradeResultDBStmt01 *BindParameter(const TTradeResultFrame1Output *pOut) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pOut->acct_id, sizeof(pOut->acct_id))
        .BindParameter(2, pOut->symbol, sizeof(pOut->symbol));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt02 : public CHandleStmt {
  public:
    CTradeResultDBStmt02(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  ca_b_id AS 'broker_id'"
        ", ca_c_id AS 'cust_id'"
        ", ca_tax_st AS 'tax_status'"
        "  FROM customer_account"
        " WHERE ca_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt02() {}

    virtual CTradeResultDBStmt02 *BindCol(TTradeResultFrame2Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol(1, &pOut->broker_id, sizeof(pOut->broker_id))
        .BindCol(2, &pOut->cust_id, sizeof(pOut->cust_id))
        .BindCol(3, &pOut->tax_status, sizeof(pOut->tax_status));

      return this;
    }

    virtual CTradeResultDBStmt02 *BindParameter(const TTradeResultFrame2Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt03 : public CHandleStmt {
  private:
    INT32 m_qty;

  public:
    CTradeResultDBStmt03(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "INSERT INTO holding_summary ("
        "  hs_ca_id"
        ", hs_s_symb"
        ", hs_qty"
        ") VALUES ("
        "  ?"
        ", ?"
        ", ?"
        ")";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt03() {}

    virtual CTradeResultDBStmt03 *BindParameter(const TTradeResultFrame2Input *pIn) {

      m_qty = 1 == pIn->type_is_sell ? pIn->trade_qty * -1 : pIn->trade_qty;

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(2, pIn->symbol, sizeof(pIn->symbol))
        .BindParameter(3, &m_qty, sizeof(m_qty));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt04 : public CHandleStmt {
  private:
    INT32 m_qty;

  public:
    CTradeResultDBStmt04(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "UPDATE holding_summary"
        "   SET hs_qty = hs_qty + ?"
        " WHERE hs_ca_id = ?"
        "   AND hs_s_symb = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt04() {}

    virtual CTradeResultDBStmt04 *BindParameter(const TTradeResultFrame2Input *pIn) {

      m_qty = 1 == pIn->type_is_sell ? pIn->trade_qty * -1 : pIn->trade_qty;

      (*((CHandleStmt *)this))
        .BindParameter(1, &m_qty, sizeof(m_qty))
        .BindParameter(2, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(3, pIn->symbol, sizeof(pIn->symbol));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt05 : public CHandleStmt {
  public:
    CTradeResultDBStmt05(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "DELETE"
        "  FROM holding_summary"
        " WHERE hs_ca_id = ?"
        "   AND hs_s_symb = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt05() {}

    virtual CTradeResultDBStmt05 *BindParameter(const TTradeResultFrame2Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(2, pIn->symbol, sizeof(pIn->symbol));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt06 : public CHandleStmt {
  public:
    CTradeResultDBStmt06(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  h_t_id AS 'hold_id'"
        ", h_qty AS 'hold_qty'"
        ", h_price AS 'hold_price'"
        "  FROM holding"
        " WHERE h_ca_id = ?"
        "   AND h_s_symb = ?"
        " ORDER BY"
        "  h_dts DESC";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt06() {}

    virtual CTradeResultDBStmt06 *BindCol(TTrade *hold_id,
                                          INT32 *hold_qty,
                                          double *hold_price) {
      (*((CHandleStmt *)this))
        .BindCol(1, hold_id, sizeof(TTrade))
        .BindCol(2, hold_qty, sizeof(INT32))
        .BindCol(3, hold_price, sizeof(double));

      return this;
    }

    virtual CTradeResultDBStmt06 *BindParameter(const TTradeResultFrame2Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(2, pIn->symbol, sizeof(pIn->symbol));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt07 : public CHandleStmt {
  public:
    CTradeResultDBStmt07(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  h_t_id AS 'hold_id'"
        ", h_qty AS 'hold_qty'"
        ", h_price AS 'hold_price'"
        "  FROM holding"
        " WHERE h_ca_id = ?"
        "   AND h_s_symb = ?"
        " ORDER BY"
        "  h_dts ASC";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt07() {}

    virtual CTradeResultDBStmt07 *BindCol(TTrade *hold_id,
                                          INT32 *hold_qty,
                                          double *hold_price) {
      (*((CHandleStmt *)this))
        .BindCol(1, hold_id, sizeof(TTrade))
        .BindCol(2, hold_qty, sizeof(INT32))
        .BindCol(3, hold_price, sizeof(double));

      return this;
    }

    virtual CTradeResultDBStmt07 *BindParameter(const TTradeResultFrame2Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(2, pIn->symbol, sizeof(pIn->symbol));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt08 : public CHandleStmt {
  private:
    INT32 m_qty;

  public:
    CTradeResultDBStmt08(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "INSERT INTO holding ("
        "  h_t_id"
        ", h_ca_id"
        ", h_s_symb"
        ", h_dts"
        ", h_price"
        ", h_qty"
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

    virtual ~CTradeResultDBStmt08() {}

    virtual CTradeResultDBStmt08 *BindParameter(const TTradeResultFrame2Input *pIn,
                                                const TTradeResultFrame2Output *pOut,
                                                const INT32 *h_qty) {

      m_qty = 1 == pIn->type_is_sell ? (*h_qty) * -1 : (*h_qty);

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->trade_id, sizeof(pIn->trade_id))
        .BindParameter(2, &pIn->acct_id, sizeof(pIn->acct_id))
        .BindParameter(3, pIn->symbol, sizeof(pIn->symbol))
        .BindParameter(4, &pOut->trade_dts, sizeof(pOut->trade_dts))
        .BindParameter(5, &pIn->trade_price, sizeof(pIn->trade_price))
        .BindParameter(6, &m_qty, sizeof(m_qty));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt09 : public CHandleStmt {
  public:
    CTradeResultDBStmt09(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "UPDATE holding"
        "   SET h_qty = ?"
        " WHERE h_t_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt09() {}

    virtual CTradeResultDBStmt09 *BindParameter(const TTrade *h_t_id,
                                                const INT32 *h_qty) {
      (*((CHandleStmt *)this))
        .BindParameter(1, h_qty, sizeof(INT32))
        .BindParameter(2, h_t_id, sizeof(TTrade));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt10 : public CHandleStmt {
  public:
    CTradeResultDBStmt10(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "DELETE"
        "  FROM holding"
        " WHERE h_t_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt10() {}

    virtual CTradeResultDBStmt10 *BindParameter(const TTrade *h_t_id) {

      (*((CHandleStmt *)this))
        .BindParameter(1, h_t_id, sizeof(TTrade));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt11 : public CHandleStmt {
  private:
    INT32 m_qty;

  public:
    CTradeResultDBStmt11(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "INSERT INTO holding_history ("
        "  hh_h_t_id"
        ", hh_t_id"
        ", hh_before_qty"
        ", hh_after_qty"
        ") VALUES ("
        "  ?"
        ", ?"
        ", ?"
        ", ?"
        ")";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt11() {}

    virtual CTradeResultDBStmt11 *BindParameter(const TTradeResultFrame2Input *pIn,
                                                const TTrade *hh_h_t_id,
                                                const TTrade *hh_t_id,
                                                const INT32 *hh_before_qty,
                                                const INT32 *hh_after_qty) {

      m_qty = 1 == pIn->type_is_sell ? (*hh_after_qty) * -1 : (*hh_after_qty);

      (*((CHandleStmt *)this))
        .BindParameter(1, hh_h_t_id, sizeof(TTrade))
        .BindParameter(2, hh_t_id, sizeof(TTrade))
        .BindParameter(3, hh_before_qty, sizeof(INT32))
        .BindParameter(4, &m_qty, sizeof(m_qty));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt12 : public CHandleStmt {
  public:
    CTradeResultDBStmt12(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT SUM(tx_rate) AS 'tax_rates'"
        "  FROM customer_taxrate"
        "  JOIN taxrate ON tx_id = cx_tx_id"
        " WHERE cx_c_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt12() {}

    virtual CTradeResultDBStmt12 *BindCol(double *tax_rates) {

      (*((CHandleStmt *)this))
        .BindCol(1, tax_rates, sizeof(double));

      return this;
    }

    virtual CTradeResultDBStmt12 *BindParameter(const TTradeResultFrame3Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->cust_id, sizeof(pIn->cust_id));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt13 : public CHandleStmt {
  public:
    CTradeResultDBStmt13(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "UPDATE trade"
        "   SET t_tax = ?"
        " WHERE t_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt13() {}

    virtual CTradeResultDBStmt13 *BindParameter(const TTradeResultFrame3Input *pIn,
                                                const TTradeResultFrame3Output *pOut) {
      (*((CHandleStmt *)this))
        .BindParameter(1, &pOut->tax_amount, sizeof(pOut->tax_amount))
        .BindParameter(2, &pIn->trade_id, sizeof(pIn->trade_id));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt14 : public CHandleStmt {
  public:
    CTradeResultDBStmt14(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  cr_rate AS 'comm_rate'"
        ", s_name"
        "  FROM customer"
        "  JOIN commission_rate x ON cr_c_tier = c_tier"
        "  JOIN security ON s_ex_id = cr_ex_id"
        " WHERE c_id = ?"
        "   AND s_symb = ?"
        "   AND cr_tt_id = ?"
        "   AND cr_from_qty <= ?"
        "   AND cr_to_qty >= ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt14() {}

    virtual CTradeResultDBStmt14 *BindCol(TTradeResultFrame4Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol(1, &pOut->comm_rate, sizeof(pOut->comm_rate))
        .BindCol(2, pOut->s_name, sizeof(pOut->s_name));

      return this;
    }

    virtual CTradeResultDBStmt14 *BindParameter(const TTradeResultFrame4Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->cust_id, sizeof(pIn->cust_id))
        .BindParameter(2, pIn->symbol, sizeof(pIn->symbol))
        .BindParameter(3, pIn->type_id, sizeof(pIn->type_id))
        .BindParameter(4, &pIn->trade_qty, sizeof(pIn->trade_qty))
        .BindParameter(5, &pIn->trade_qty, sizeof(pIn->trade_qty));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt16 : public CHandleStmt {
  public:
    CTradeResultDBStmt16(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "UPDATE trade"
        "   SET t_comm = ?"
        "     , t_dts = ?"
        "     , t_st_id = ?"
        "     , t_trade_price = ?"
        " WHERE t_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt16() {}

    virtual CTradeResultDBStmt16 *BindParameter(const TTradeResultFrame5Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->comm_amount, sizeof(pIn->comm_amount))
        .BindParameter(2, &pIn->trade_dts, sizeof(pIn->trade_dts))
        .BindParameter(3, pIn->st_completed_id, sizeof(pIn->st_completed_id))
        .BindParameter(4, &pIn->trade_price, sizeof(pIn->trade_price))
        .BindParameter(5, &pIn->trade_id, sizeof(pIn->trade_id));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt17 : public CHandleStmt {
  public:
    CTradeResultDBStmt17(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

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

    virtual ~CTradeResultDBStmt17() {}

    virtual CTradeResultDBStmt17 *BindParameter(const TTradeResultFrame5Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->trade_id, sizeof(pIn->trade_id))
        .BindParameter(2, &pIn->trade_dts, sizeof(pIn->trade_dts))
        .BindParameter(3, pIn->st_completed_id, sizeof(pIn->st_completed_id));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt18 : public CHandleStmt {
  public:
    CTradeResultDBStmt18(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "UPDATE broker"
        "   SET b_comm_total = b_comm_total + ?"
        "     , b_num_trades = b_num_trades + 1"
        " WHERE b_id = ?";


      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt18() {}

    virtual CTradeResultDBStmt18 *BindParameter(const TTradeResultFrame5Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->comm_amount, sizeof(pIn->comm_amount))
        .BindParameter(2, &pIn->broker_id, sizeof(pIn->broker_id));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt19 : public CHandleStmt {
  public:
    CTradeResultDBStmt19(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "INSERT INTO settlement ("
        "  se_t_id"
        ", se_cash_type"
        ", se_cash_due_date"
        ", se_amt"
        ") VALUES ("
        "  ?"
        ", ?"
        ", ?"
        ", ?"
        ")";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt19() {}

    virtual CTradeResultDBStmt19 *BindParameter(const TTradeResultFrame6Input *pIn,
                                                const char *cash_type, size_t cash_type_len) {
      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->trade_id, sizeof(pIn->trade_id))
        .BindParameter(2, cash_type, cash_type_len)
        .BindParameter(3, &pIn->due_date, sizeof(pIn->due_date))
        .BindParameter(4, &pIn->se_amount, sizeof(pIn->se_amount));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt20 : public CHandleStmt {
  public:
    CTradeResultDBStmt20(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "UPDATE customer_account"
        "   SET ca_bal = ca_bal + ?"
        " WHERE ca_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt20() {}

    virtual CTradeResultDBStmt20 *BindParameter(const TTradeResultFrame6Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->se_amount, sizeof(pIn->se_amount))
        .BindParameter(2, &pIn->acct_id, sizeof(pIn->acct_id));

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt21 : public CHandleStmt {
  public:
    CTradeResultDBStmt21(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "INSERT INTO cash_transaction ("
        "  ct_dts"
        ", ct_t_id"
        ", ct_amt"
        ", ct_name"
        ") VALUES ("
        "  ?"
        ", ?"
        ", ?"
        ", ?"
        ")";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt21() {}

    virtual CTradeResultDBStmt21 *BindParameter(const TTradeResultFrame6Input *pIn,
                                                const char *ct_name, size_t ct_name_len) {
      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->trade_dts, sizeof(pIn->trade_dts))
        .BindParameter(2, &pIn->trade_id, sizeof(pIn->trade_id))
        .BindParameter(3, &pIn->se_amount, sizeof(pIn->se_amount))
        .BindParameter(4, ct_name, ct_name_len);

      return this;
    }
  };

  /*
   */
  class CTradeResultDBStmt22 : public CHandleStmt {
  public:
    CTradeResultDBStmt22(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT ca_bal AS 'acct_bal'"
        "  FROM customer_account"
        " WHERE ca_id = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CTradeResultDBStmt22() {}

    virtual CTradeResultDBStmt22 *BindCol(TTradeResultFrame6Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol(1, &pOut->acct_bal, sizeof(pOut->acct_bal));

      return this;
    }

    virtual CTradeResultDBStmt22 *BindParameter(const TTradeResultFrame6Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, &pIn->acct_id, sizeof(pIn->acct_id));

      return this;
    }
  };

  /*
   */
  CTradeResultDB::CTradeResultDB(CHandleDbc *pDbc)
    : CTradeResultDBInterface(), m_pDbc(pDbc) {

    m_pStmt[ 0] = new CTradeResultDBStmt00(pDbc);
    m_pStmt[ 1] = new CTradeResultDBStmt01(pDbc);
    m_pStmt[ 2] = new CTradeResultDBStmt02(pDbc);
    m_pStmt[ 3] = new CTradeResultDBStmt03(pDbc);
    m_pStmt[ 4] = new CTradeResultDBStmt04(pDbc);
    m_pStmt[ 5] = new CTradeResultDBStmt05(pDbc);
    m_pStmt[ 6] = new CTradeResultDBStmt06(pDbc);
    m_pStmt[ 7] = new CTradeResultDBStmt07(pDbc);
    m_pStmt[ 8] = new CTradeResultDBStmt08(pDbc);
    m_pStmt[ 9] = new CTradeResultDBStmt09(pDbc);
    m_pStmt[10] = new CTradeResultDBStmt10(pDbc);
    m_pStmt[11] = new CTradeResultDBStmt11(pDbc);
    m_pStmt[12] = new CTradeResultDBStmt12(pDbc);
    m_pStmt[13] = new CTradeResultDBStmt13(pDbc);
    m_pStmt[14] = new CTradeResultDBStmt14(pDbc);
    m_pStmt[16] = new CTradeResultDBStmt16(pDbc);
    m_pStmt[17] = new CTradeResultDBStmt17(pDbc);
    m_pStmt[18] = new CTradeResultDBStmt18(pDbc);
    m_pStmt[19] = new CTradeResultDBStmt19(pDbc);
    m_pStmt[20] = new CTradeResultDBStmt20(pDbc);
    m_pStmt[21] = new CTradeResultDBStmt21(pDbc);
    m_pStmt[22] = new CTradeResultDBStmt22(pDbc);
  }

  /*
   */
  CTradeResultDB::~CTradeResultDB() {

    delete m_pStmt[ 0];
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
    delete m_pStmt[16];
    delete m_pStmt[17];
    delete m_pStmt[18];
    delete m_pStmt[19];
    delete m_pStmt[20];
    delete m_pStmt[21];
    delete m_pStmt[22];
  }

  /*
   */
  void CTradeResultDB::DoTradeResultFrame1(const TTradeResultFrame1Input *pIn,
                                           TTradeResultFrame1Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeResultDB::DoTradeResultFrame1/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction(); // Commit -> 6, Rollback -> X

    {
      CHandleStmt *pStmt;
      int p;

      pOut->num_found = 0;

      { // -- 0 --
        ((CTradeResultDBStmt00 *)(pStmt = m_pStmt[(p = 0)]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] t_id=" << pIn->trade_id << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : trade,trade_type

          pOut->num_found = pStmt->RowCount(); // = 1

#ifdef _TRACE
          cout << "TRACE: [1," << p << "] num_found=" << pOut->num_found << '\r' << endl;
#endif
          for (SQLLEN size = pOut->num_found, i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [1," << p << ":" << i << "] acct_id=" << pOut->acct_id << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] type_id=" << pOut->type_id << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] symbol=" << pOut->symbol << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] trade_qty=" << pOut->trade_qty << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] charge=" << pOut->charge << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] is_lifo=" << pOut->is_lifo << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] trade_is_cash=" << pOut->trade_is_cash << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] type_name=" << pOut->type_name << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] type_is_sell=" << pOut->type_is_sell << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] type_is_market=" << pOut->type_is_market << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 0 --

      pOut->hs_qty = 0;

      { // -- 1 --
        ((CTradeResultDBStmt01 *)(pStmt = m_pStmt[(p = 1)]))
          ->BindParameter(pOut)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] hs_ca_id=" << pOut->acct_id << '\r' << endl;
        cout << "TRACE: [1," << p << "] hs_s_symb=" << pOut->symbol << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : holding_summary

          for (SQLLEN size =  pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [1," << p << ":" << i << "] hs_qty=" << pOut->hs_qty << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 1 --

    }
  }

  /*
   */
  void CTradeResultDB::DoTradeResultFrame2(const TTradeResultFrame2Input *pIn,
                                           TTradeResultFrame2Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeResultDB::DoTradeResultFrame2/2" << '\r' << endl;
#endif

    {
      TTrade hold_id;
      double hold_price;
      INT32 hold_qty;
      INT32 needed_qty;

      CHandleStmt *pStmt;
      int p;

      current_timestamp(&pOut->trade_dts);

      { // -- 2 --
        ((CTradeResultDBStmt02 *)(pStmt = m_pStmt[(p = 2)]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [2," << p << "] ca_id=" << pIn->acct_id << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : customer_account

          for (SQLLEN size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [2," << p << ":" << i << "] broker_id=" << pOut->broker_id << '\r' << endl;
            cout << "TRACE: [2," << p << ":" << i << "] cust_id=" << pOut->cust_id << '\r' << endl;
            cout << "TRACE: [2," << p << ":" << i << "] tax_status=" << pOut->tax_status << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 2 --

      if (0 == pIn->hs_qty) {

        { // -- 3 --
          ((CTradeResultDBStmt03 *)(pStmt = m_pStmt[(p = 3)]))
            ->BindParameter(pIn);

#ifdef _TRACE
          cout << "TRACE: [2," << p << "] hs_ca_id=" << pIn->acct_id << '\r' << endl;
          cout << "TRACE: [2," << p << "] hs_s_symb=" << pIn->symbol << '\r' << endl;
          cout << "TRACE: [2," << p << "] hs_qty=" << pIn->trade_qty << '\r' << endl;
          cout << "TRACE: [2," << p << "] type_is_sell=" << pIn->type_is_sell << '\r' << endl;
#endif
          if (pStmt->Execute()) { // INSERT : holding_summary

#ifdef _TRACE
            cout << "TRACE: [2," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
            pStmt->Cancel();
          }
        } // -- 3 --

      } else if (abs(pIn->hs_qty) != pIn->trade_qty) {

        { // --  4 --
          ((CTradeResultDBStmt04 *)(pStmt = m_pStmt[(p = 4)]))
            ->BindParameter(pIn);

#ifdef _TRACE
          cout << "TRACE: [2," << p << "] hs_qty+=" << pIn->trade_qty << '\r' << endl;
          cout << "TRACE: [2," << p << "] hs_ca_id=" << pIn->acct_id << '\r' << endl;
          cout << "TRACE: [2," << p << "] hs_s_symb=" << pIn->symbol << '\r' << endl;
          cout << "TRACE: [2," << p << "] type_is_sell=" << pIn->type_is_sell << '\r' << endl;
#endif
          if (pStmt->Execute()) { // UPDATE : holding_summary

#ifdef _TRACE
            cout << "TRACE: [2," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
            pStmt->Cancel();
          }
        } // -- 4 --

      }

      needed_qty = pIn->trade_qty;

      if (0 < abs(pIn->hs_qty)) {

        if (1 == pIn->is_lifo) {
          ((CTradeResultDBStmt06 *)(pStmt = m_pStmt[(p = 6)]))
            ->BindParameter(pIn)
            ->BindCol(&hold_id, &hold_qty, &hold_price);
        } else {
          ((CTradeResultDBStmt07 *)(pStmt = m_pStmt[(p = 7)]))
            ->BindParameter(pIn)
            ->BindCol(&hold_id, &hold_qty, &hold_price);
        }

#ifdef _TRACE
        cout << "TRACE: [2," << p << "] h_ca_id=" << pIn->acct_id << '\r' << endl;
        cout << "TRACE: [2," << p << "] h_s_symb=" << pIn->symbol << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : holding [DESC|ASC]

          double buy_value = 0;
          double sell_value = 0;

          for (SQLLEN size = pStmt->RowCount(), i = 0; 0 != needed_qty && i < size; i++) {

            hold_id = 0;
            hold_qty = 0;
            hold_price = 0.0;

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [2," << p << ":" << i << "] hold_id=" << hold_id << '\r' << endl;
            cout << "TRACE: [2," << p << ":" << i << "] hold_qty=" << hold_qty << '\r' << endl;
            cout << "TRACE: [2," << p << ":" << i << "] hold_price=" << hold_price << '\r' << endl;
#endif
            CHandleStmt *pStmt2;
            int p2;

            if (hold_qty > needed_qty) {

              INT32 qty =
                hold_qty + (1 == pIn->type_is_sell ? needed_qty * -1 : needed_qty);

              { // -- 11 --
                ((CTradeResultDBStmt11 *)(pStmt2 = m_pStmt[(p2 = 11)]))
                  ->BindParameter(pIn, &hold_id, &pIn->trade_id, &hold_qty, &qty);
#ifdef _TRACE
                cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] hh_h_t_id="
                     << hold_id << '\r' << endl;
                cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] hh_t_id="
                     << pIn->trade_id << '\r' << endl;
                cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] hh_before_qty="
                     << hold_qty << '\r' << endl;
                cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] hh_after_qty="
                     << qty << '\r' << endl;
                cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] type_is_sell="
                     << pIn->type_is_sell << '\r' << endl;
#endif
                if (pStmt2->Execute()) { // INSERT : holding_history
#ifdef _TRACE
                  cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] row_count="
                       << pStmt2->RowCount() << '\r' << endl;
#endif
                  pStmt2->Cancel();
                }
              } // -- 11 --

              { // -- 9 -- : TODO, CURRENT_ROW
                ((CTradeResultDBStmt09 *)(pStmt2 = m_pStmt[(p2 = 9)]))
                  ->BindParameter(&hold_id, &qty);
#ifdef _TRACE
                cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] h_qty="
                     << qty << '\r' << endl;
                cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] h_t_id="
                     << hold_id << '\r' << endl;
#endif
                if (pStmt2->Execute()) { // UPDATE : holding
#ifdef _TRACE
                  cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] row_count="
                       << pStmt2->RowCount() << '\r' << endl;
#endif
                  pStmt2->Cancel();
                }
              } // -- 9 --

              if (1 == pIn->type_is_sell) {
                sell_value += needed_qty * pIn->trade_price;
                buy_value  += needed_qty * hold_price;
                needed_qty  = 0;
              } else {
                sell_value += needed_qty * hold_price;
                buy_value  += needed_qty * pIn->trade_price;
                needed_qty  = 0;
              }

            } else { // hold_qty <= needed_qty


              INT32 qty = 0;

              { // -- 11 --
                ((CTradeResultDBStmt11 *)(pStmt2 = m_pStmt[(p2 = 11)]))
                  ->BindParameter(pIn, &hold_id, &pIn->trade_id, &hold_qty, &qty);
#ifdef _TRACE
                cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] hh_h_t_id="
                     << hold_id << '\r' << endl;
                cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] hh_t_id="
                     << pIn->trade_id << '\r' << endl;
                cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] hh_before_qty="
                     << hold_qty << '\r' << endl;
                cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] hh_after_qty="
                     << qty << '\r' << endl;
                cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] type_is_sell="
                     << pIn->type_is_sell << '\r' << endl;
#endif
                if (pStmt2->Execute()) { // INSERT : holding_history
#ifdef _TRACE
                  cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] row_count="
                       << pStmt2->RowCount() << '\r' << endl;
#endif
                  pStmt2->Cancel();
                }
              } // -- 11 --

              { // -- 10 -- : TODO, CURRENT_ROW
                ((CTradeResultDBStmt10 *)(pStmt2 = m_pStmt[(p2 = 10)]))
                  ->BindParameter(&hold_id);
#ifdef _TRACE
                cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] h_t_id="
                     << hold_id << '\r' << endl;
#endif
                if (pStmt2->Execute()) { // DELETE : holding
#ifdef _TRACE
                  cout << "TRACE: [2," << p << ":" << i << "," << p2 << "] row_count="
                       << pStmt2->RowCount() << '\r' << endl;
#endif
                  pStmt2->Cancel();
                }
              } // -- 10 --

              if (1 == pIn->type_is_sell) {
                sell_value += hold_qty * pIn->trade_price;
                buy_value  += hold_qty * hold_price;
                needed_qty -= hold_qty;
              } else {
                sell_value -= hold_qty * hold_price;
                buy_value  -= hold_qty * pIn->trade_price;
                needed_qty += hold_qty;
              }

            } // hold_qty <= needed_qty

#ifdef _TRACE
            cout << "TRACE: [2," << p << ":" << i << "] sell_value=" << sell_value << '\r' << endl;
            cout << "TRACE: [2," << p << ":" << i << "] buy_value=" << buy_value << '\r' << endl;
            cout << "TRACE: [2," << p << ":" << i << "] needed_qty=" << needed_qty << '\r' << endl;
#endif
          } // for (size)

          pStmt->Cancel();
        }
      } // 0 < hs_qty

#ifdef _TRACE
      cout << "TRACE: [2] needed_qty=" << needed_qty << '\r' << endl;
#endif

      if (0 < needed_qty) {

        { // -- 11 --
          hold_qty = 0;

          ((CTradeResultDBStmt11 *)(pStmt = m_pStmt[(p = 11)]))
            ->BindParameter(pIn, &pIn->trade_id, &pIn->trade_id, &hold_qty, &needed_qty);

#ifdef _TRACE
          cout << "TRACE: [2," << p << "] hh_h_t_id=" << pIn->trade_id << '\r' << endl;
          cout << "TRACE: [2," << p << "] hh_t_id=" << pIn->trade_id << '\r' << endl;
          cout << "TRACE: [2," << p << "] hh_before_qty=" << hold_qty << '\r' << endl;
          cout << "TRACE: [2," << p << "] hh_after_qty=" << needed_qty << '\r' << endl;
          cout << "TRACE: [2," << p << "] type_is_sell=" << pIn->type_is_sell << '\r' << endl;
#endif
          if (pStmt->Execute()) { // INSERT : holding_history

#ifdef _TRACE
            cout << "TRACE: [2," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
            pStmt->Cancel();
          }
        } // -- 11 --

        { // -- 8 --
          ((CTradeResultDBStmt08 *)(pStmt = m_pStmt[(p = 8)]))
            ->BindParameter(pIn, pOut, &needed_qty);

#ifdef _TRACE
          cout << "TRACE: [2," << p << "] h_t_id=" << pIn->trade_id << '\r' << endl;
          cout << "TRACE: [2," << p << "] h_ca_id=" << pIn->acct_id << '\r' << endl;
          cout << "TRACE: [2," << p << "] h_s_symb=" << pIn->symbol << '\r' << endl;
          cout << "TRACE: [2," << p << "] h_dts=" << pOut->trade_dts << '\r' << endl;
          cout << "TRACE: [2," << p << "] h_price=" << pIn->trade_price << '\r' << endl;
          cout << "TRACE: [2," << p << "] h_qty=" << needed_qty << '\r' << endl;
          cout << "TRACE: [2," << p << "] type_is_sell=" << pIn->type_is_sell << '\r' << endl;
#endif
          if (pStmt->Execute()) { // INSERT : holding

#ifdef _TRACE
            cout << "TRACE: [2," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
            pStmt->Cancel();
          }
        } // -- 8 --

      } else if (abs(pIn->hs_qty) == pIn->trade_qty) {

        { // -- 5 --
          ((CTradeResultDBStmt05 *)(pStmt = m_pStmt[(p = 5)]))
            ->BindParameter(pIn);

#ifdef _TRACE
          cout << "TRACE: [2," << p << "] hs_ca_id=" << pIn->acct_id << '\r' << endl;
          cout << "TRACE: [2," << p << "] hs_s_symb=" << pIn->symbol << '\r' << endl;
#endif
          if (pStmt->Execute()) { // DELETE : holding_summary

#ifdef _TRACE
            cout << "TRACE: [2," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
            pStmt->Cancel();
          }
        } // -- 5 --

      } // hs_qtr == trade_qty
    }
  }

  /*
   */
  void CTradeResultDB::DoTradeResultFrame3(const TTradeResultFrame3Input *pIn,
                                           TTradeResultFrame3Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeResultDB::DoTradeResultFrame3/2" << '\r' << endl;
#endif

    {
      double tax_rates;

      CHandleStmt *pStmt;
      int p;

      pOut->tax_amount = 0.0;

      { // -- 12 --
        ((CTradeResultDBStmt12 *)(pStmt = m_pStmt[(p = 12)]))
          ->BindParameter(pIn)
          ->BindCol(&tax_rates);

#ifdef _TRACE
        cout << "TRACE: [3," << p << "] cust_id=" << pIn->cust_id << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : customer_taxrate,taxrate

          for (SQLLEN size = pStmt->RowCount(), i = 0; i < size; i++) {

            tax_rates = 0.0;

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [3," << p << ":" << i << "] tax_rates=" << tax_rates << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 12 --

      {
        pOut->tax_amount = (pIn->sell_value - pIn->buy_value) * tax_rates;

#ifdef _TRACE
        cout << "TRACE: [3] sell_value=" << pIn->sell_value << '\r' << endl;
        cout << "TRACE: [3] buy_value=" << pIn->buy_value << '\r' << endl;
        cout << "TRACE: [3] tax_rates=" << tax_rates << '\r' << endl;
        cout << "TRACE: [3] tax_amount=" << pOut->tax_amount << '\r' << endl;
#endif
      }

      { // -- 13 --
        ((CTradeResultDBStmt13 *)(pStmt = m_pStmt[(p = 13)]))
          ->BindParameter(pIn, pOut);

#ifdef _TRACE
        cout << "TRACE: [3," << p << "] t_tax=" << pOut->tax_amount << '\r' << endl;
        cout << "TRACE: [3," << p << "] t_id=" << pIn->trade_id << '\r' << endl;
#endif
        if (pStmt->Execute()) { // UPDATE : trade

#ifdef _TRACE
          cout << "TRACE: [3," << p << "] row_count=" <<  pStmt->RowCount() << '\r' << endl;
#endif
          pStmt->Cancel();
        }
      } // -- 13 --

    }
  }

  /*
   */
  void CTradeResultDB::DoTradeResultFrame4(const TTradeResultFrame4Input *pIn,
                                           TTradeResultFrame4Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeResultDB::DoTradeResultFrame4/2" << '\r' << endl;
#endif

    {
      CHandleStmt *pStmt;
      int p;

      { // -- 14 --
        ((CTradeResultDBStmt14 *)(pStmt =  m_pStmt[(p = 14)]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [4," << p << "] c_id=" << pIn->cust_id << '\r' << endl;
        cout << "TRACE: [4," << p << "] s_symb=" << pIn->symbol << '\r' << endl;
        cout << "TRACE: [4," << p << "] cr_tt_id=" << pIn->type_id << '\r' << endl;
        cout << "TRACE: [4," << p << "] cr_from_qty=" << pIn->trade_qty << '\r' << endl;
        cout << "TRACE: [4," << p << "] cr_to_qty=" << pIn->trade_qty << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : commission_rate,customer,security

          for (SQLLEN size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [4," << p << ":" << i << "] s_name=" << pOut->s_name << '\r' << endl;
            cout << "TRACE: [4," << p << ":" << i << "] comm_rate=" << pOut->comm_rate << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 14 --

    }
  }

  /*
   */
  void CTradeResultDB::DoTradeResultFrame5(const TTradeResultFrame5Input *pIn) {

#ifdef _TRACE
    cout << "TRACE: CTradeResultDB::DoTradeResultFrame5/1" << '\r' << endl;
#endif

    {
      CHandleStmt *pStmt;
      int p;

      { // -- 16 --
        ((CTradeResultDBStmt16 *)(pStmt = m_pStmt[(p = 16)]))
          ->BindParameter(pIn);

#ifdef _TRACE
        cout << "TRACE: [5," << p << "] t_comm=" << pIn->comm_amount << '\r' << endl;
        cout << "TRACE: [5," << p << "] t_dts=" << pIn->trade_dts << '\r' << endl;
        cout << "TRACE: [5," << p << "] t_st_id=" << pIn->st_completed_id << '\r' << endl;
        cout << "TRACE: [5," << p << "] t_trade_price=" << pIn->trade_price << '\r' << endl;
        cout << "TRACE: [5," << p << "] t_id=" << pIn->trade_id << '\r' << endl;
#endif
        if (pStmt->Execute()) { // UPDATE : trade

#ifdef _TRACE
          cout << "TRACE: [5," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
          pStmt->Cancel();
        }
      } // -- 16 --

      { // -- 17 --
        ((CTradeResultDBStmt17 *)(pStmt = m_pStmt[(p = 17)]))
          ->BindParameter(pIn);

        if (pStmt->Execute()) { // INSERT : trade_history

#ifdef _TRACE
          cout << "TRACE: [5," << p << "] row_count=" << pStmt->RowCount()  << '\r' << endl;
#endif
          pStmt->Cancel();
        }
      } // -- 17 --

      { // -- 18 --
        ((CTradeResultDBStmt18 *)(pStmt = m_pStmt[(p = 18)]))
          ->BindParameter(pIn);

#ifdef _TRACE
        cout << "TRACE: [5," << p << "] b_comm_total+=" << pIn->comm_amount << '\r' << endl;
        cout << "TRACE: [5," << p << "] b_id=" << pIn->broker_id << '\r' << endl;
#endif
        if (pStmt->Execute()) { // UPDATE : broker

#ifdef _TRACE
          cout << "TRACE: [5," << p << "] row_count=" << pStmt->RowCount()  << '\r' << endl;
#endif
          pStmt->Cancel();
        }
      } // -- 18 --

    }
  }

  /*
   */
  void CTradeResultDB::DoTradeResultFrame6(const TTradeResultFrame6Input *pIn,
                                           TTradeResultFrame6Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CTradeResultDB::DoTradeResultFrame6/2" << '\r' << endl;
#endif

    {
      CHandleStmt *pStmt;
      int p;

      { // -- 19 --
        char cash_type[cSE_CASH_TYPE_len+1];

        ((CTradeResultDBStmt19 *)(pStmt = m_pStmt[(p = 19)]))
          ->BindParameter(pIn, cash_type, sizeof(cash_type));

        snprintf(cash_type, sizeof(cash_type), "%s",
                 1 == pIn->trade_is_cash ? "Cash Account" : "Margin");
#ifdef _TRACE
        cout << "TRACE: [6," << p << "] se_t_id=" << pIn->trade_id << '\r' << endl;
        cout << "TRACE: [6," << p << "] se_cash_type=" << cash_type << '\r' << endl;
        cout << "TRACE: [6," << p << "] se_cash_due_date=" << pIn->due_date << '\r' << endl;
        cout << "TRACE: [6," << p << "] se_amt=" << pIn->se_amount << '\r' << endl;
#endif
        if (pStmt->Execute()) { // INSERT : settlement

#ifdef _TRACE
          cout << "TRACE: [6," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
          pStmt->Cancel();
        }
      } // -- 19 --

#ifdef _TRACE
      cout << "TRACE: [6] trade_is_cash=" << pIn->trade_is_cash << '\r' << endl;
#endif

      if (1 == pIn->trade_is_cash) {

        { // -- 20 --
          ((CTradeResultDBStmt20 *)(pStmt = m_pStmt[(p = 20)]))
            ->BindParameter(pIn);

#ifdef _TRACE
          cout << "TRACE: [6," << p << "] ca_bal+=" << pIn->se_amount << '\r' << endl;
          cout << "TRACE: [6," << p << "] ca_id=" << pIn->acct_id << '\r' << endl;
#endif
          if (pStmt->Execute()) { // UPDATE : customer_account

#ifdef _TRACE
            cout << "TRACE: [6," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
            pStmt->Cancel();
          }
        } // -- 20 --

        { // -- 21 --
          char ct_name[cCT_NAME_len+1];

          ((CTradeResultDBStmt21 *)(pStmt = m_pStmt[(p = 21)]))
            ->BindParameter(pIn, ct_name, sizeof(ct_name));

          snprintf(ct_name, sizeof(ct_name), "%s %d shares of %s",
                   pIn->type_name, pIn->trade_qty, pIn->s_name);
#ifdef _TRACE
          cout << "TRACE: [6," << p << "] ct_dts=" << pIn->trade_dts << '\r' << endl;
          cout << "TRACE: [6," << p << "] ct_t_id=" << pIn->trade_id << '\r' << endl;
          cout << "TRACE: [6," << p << "] ct_amt=" << pIn->se_amount << '\r' << endl;
          cout << "TRACE: [6," << p << "] ct_name=" << ct_name << '\r' << endl;
#endif
          if (pStmt->Execute()) { // INSERT : cash_transaction

#ifdef _TRACE
            cout << "TRACE: [6," << p << "] row_count=" << pStmt->RowCount() << '\r' << endl;
#endif
            pStmt->Cancel();
          }
        } // -- 21 --

      } // is_cash

      { // -- 22 --
        ((CTradeResultDBStmt22 *)(pStmt = m_pStmt[p = 22]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [6," << p << "] ca_id=" << pIn->acct_id << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : customer_account

          for (SQLLEN size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [6," << p << ":" << i << "] acct_bal=" << pOut->acct_bal << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 22 --

    }

    m_pDbc->CommitTransaction();
  }

}
