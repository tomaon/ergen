/*
 */

#include "SecurityDetailDB.H"

#include <sstream>

#include <sys/param.h> // MIN

#include "HandleDbc.H"
#include "HandleStmt.H"

namespace TPCE {

  /*
   */
  class CSecurityDetailDBStmt0 : public CHandleStmt {
  public:
    CSecurityDetailDBStmt0(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  s_name"
        ", co_id"
        ", co_name"
        ", co_sp_rate AS 'sp_rate'"
        ", co_ceo AS 'ceo_name'"
        ", co_desc"
        ", co_open_date AS 'open_date'"
        ", co_st_id"
        ", ca.ad_line1 AS 'co_ad_line1'"           // nullable
        ", ca.ad_line2 AS 'co_ad_line2'"           // nullable
        ", zca.zc_town AS 'co_ad_town'"
        ", zca.zc_div AS 'co_ad_div'"
        ", ca.ad_zc_code AS 'co_ad_zip'"
        ", ca.ad_ctry AS 'co_ad_cty'"              // nullable
        ", s_num_out AS 'num_out'"
        ", s_start_date AS 'start_date'"
        ", s_exch_date AS 'ex_date'"
        ", s_pe AS 'pe_ratio'"
        ", s_52wk_high AS 's52_wk_high'"
        ", s_52wk_high_date AS 's52_wk_high_date'"
        ", s_52wk_low AS 's52wk_low'"
        ", s_52wk_low_date AS 's52_wk_low_date'"
        ", s_dividend AS 'divid'"
        ", s_yield AS 'yield'"
        ", zea.zc_div AS 'ex_ad_div'"
        ", ea.ad_ctry AS 'ex_ad_cty'"              // nullable
        ", ea.ad_line1 AS 'ex_ad_line1'"           // nullable
        ", ea.ad_line2 AS 'ex_ad_line2'"           // nullable
        ", zea.zc_town AS 'ex_ad_town'"
        ", ea.ad_zc_code AS 'ex_ad_zip'"
        ", ex_close"
        ", ex_desc"                                // nullable
        ", ex_name"
        ", ex_num_symb"
        ", ex_open"
        "  FROM security"
        "  JOIN company ON co_id = s_co_id"
        "  JOIN address ca ON ca.ad_id = co_ad_id"
        "  JOIN zip_code zca ON zca.zc_code = ca.ad_zc_code"
        "  JOIN exchange ON ex_id = s_ex_id"
        "  JOIN address ea ON ea.ad_id = ex_ad_id"
        "  JOIN zip_code zea ON zea.zc_code = ea.ad_zc_code"
        " WHERE s_symb = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CSecurityDetailDBStmt0() {}

    virtual CSecurityDetailDBStmt0 *BindCol(TIdent *co_id,
                                            TSecurityDetailFrame1Output *pOut) {
      (*((CHandleStmt *)this))
        .BindCol( 1, pOut->s_name, sizeof(pOut->s_name))
        .BindCol( 2, co_id, sizeof(TIdent))
        .BindCol( 3, pOut->co_name, sizeof(pOut->co_name))
        .BindCol( 4, pOut->sp_rate, sizeof(pOut->sp_rate))
        .BindCol( 5, pOut->ceo_name, sizeof(pOut->ceo_name))
        .BindCol( 6, pOut->co_desc, sizeof(pOut->co_desc))
        .BindCol( 7, &pOut->open_date, sizeof(pOut->open_date))
        .BindCol( 8, pOut->co_st_id, sizeof(pOut->co_st_id))
        .BindCol( 9, pOut->co_ad_line1, sizeof(pOut->co_ad_line1))
        .BindCol(10, pOut->co_ad_line2, sizeof(pOut->co_ad_line2))
        .BindCol(11, pOut->co_ad_town, sizeof(pOut->co_ad_town))
        .BindCol(12, pOut->co_ad_div, sizeof(pOut->co_ad_div))
        .BindCol(13, pOut->co_ad_zip, sizeof(pOut->co_ad_zip))
        .BindCol(14, pOut->co_ad_cty, sizeof(pOut->co_ad_cty))
        .BindCol(15, &pOut->num_out, sizeof(pOut->num_out))
        .BindCol(16, &pOut->start_date, sizeof(pOut->start_date))
        .BindCol(17, &pOut->ex_date, sizeof(pOut->ex_date))
        .BindCol(18, &pOut->pe_ratio, sizeof(pOut->pe_ratio))
        .BindCol(19, &pOut->s52_wk_high, sizeof(pOut->s52_wk_high))
        .BindCol(20, &pOut->s52_wk_high_date, sizeof(pOut->s52_wk_high_date))
        .BindCol(21, &pOut->s52_wk_low, sizeof(pOut->s52_wk_low))
        .BindCol(22, &pOut->s52_wk_low_date, sizeof(pOut->s52_wk_low_date))
        .BindCol(23, &pOut->divid, sizeof(pOut->divid))
        .BindCol(24, &pOut->yield, sizeof(pOut->yield))
        .BindCol(25, pOut->ex_ad_div, sizeof(pOut->ex_ad_div))
        .BindCol(26, pOut->ex_ad_cty, sizeof(pOut->ex_ad_cty))
        .BindCol(27, pOut->ex_ad_line1, sizeof(pOut->ex_ad_line1))
        .BindCol(28, pOut->ex_ad_line2, sizeof(pOut->ex_ad_line2))
        .BindCol(29, pOut->ex_ad_town, sizeof(pOut->ex_ad_town))
        .BindCol(30, pOut->ex_ad_zip, sizeof(pOut->ex_ad_zip))
        .BindCol(31, &pOut->ex_close, sizeof(pOut->ex_close))
        .BindCol(32, pOut->ex_desc, sizeof(pOut->ex_desc))
        .BindCol(33, pOut->ex_name, sizeof(pOut->ex_name))
        .BindCol(34, &pOut->ex_num_symb, sizeof(pOut->ex_num_symb))
        .BindCol(35, &pOut->ex_open, sizeof(pOut->ex_open));

      return this;
    }

    virtual CSecurityDetailDBStmt0 *BindParameter(const TSecurityDetailFrame1Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, pIn->symbol, sizeof(pIn->symbol));

      return this;
    }
  };

  /*
   */
  class CSecurityDetailDBStmt1 : public CHandleStmt {
  private:
    char cp_co_name[cCO_NAME_len+1];
    char cp_in_name[cIN_NAME_len+1];

    int index;
    char *cp_co_name_returned[max_comp_len];
    char *cp_in_name_returned[max_comp_len];

  public:
    CSecurityDetailDBStmt1(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      std::ostringstream oss;
      oss << "SELECT";
      oss << "  co_name AS 'cp_co_name'";
      oss << ", in_name AS 'cp_in_name'";
      oss << "  FROM company_competitor";
      oss << "  JOIN company ON co_id = cp_comp_co_id";
      oss << "  JOIN industry ON in_id = cp_in_id";
      oss << " WHERE cp_co_id = ?";
      oss << " LIMIT " << max_comp_len;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CSecurityDetailDBStmt1() {}

    virtual CSecurityDetailDBStmt1 *BindCol() {

      (*((CHandleStmt *)this))
        .BindCol(1, cp_co_name, sizeof(cp_co_name))
        .BindCol(2, cp_in_name, sizeof(cp_in_name));

      return this;
    }

    virtual CSecurityDetailDBStmt1 *BindCol(TSecurityDetailFrame1Output *pOut) {

      for (int i = 0; i < max_comp_len; i++) {
        cp_co_name_returned[i] = pOut->cp_co_name[i];
        cp_in_name_returned[i] = pOut->cp_in_name[i];
      }

      return BindCol();
    }

    virtual CSecurityDetailDBStmt1 *BindParameter(const TIdent *co_id) {

      (*((CHandleStmt *)this))
        .BindParameter(1, co_id, sizeof(TIdent));

      return this;
    }

    virtual bool Execute() {

      index = 0;

      return CHandleStmt::Execute();
    }

    virtual bool Fetch() {

      memset(cp_co_name, 0x00, sizeof(cp_co_name));
      memset(cp_in_name, 0x00, sizeof(cp_in_name));

      bool result = CHandleStmt::Fetch();

      if (result) {

        memcpy(cp_co_name_returned[index], cp_co_name, sizeof(cp_co_name));
        memcpy(cp_in_name_returned[index], cp_in_name, sizeof(cp_in_name));

        index++;
      }

      return result;
    }
  };

  /*
   */
  class CSecurityDetailDBStmt2 : public CHandleStmt {
  private:
    TFinInfo fin;

    int index;
    TFinInfo *fin_returned[max_fin_len];

  public:
    CSecurityDetailDBStmt2(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      std::ostringstream oss;
      oss << "SELECT";
      oss << "  fi_year AS 'year'";
      oss << ", fi_qtr AS 'qtr'";
      oss << ", fi_qtr_start_date AS 'start_date'";
      oss << ", fi_revenue AS 'rev'";
      oss << ", fi_net_earn AS 'net_earn'";
      oss << ", fi_basic_eps AS 'basic_eps'";
      oss << ", fi_dilut_eps AS 'dilut_eps'";
      oss << ", fi_margin AS 'margin'";
      oss << ", fi_inventory AS 'invent'";
      oss << ", fi_assets AS 'assets'";
      oss << ", fi_liability AS 'liab'";
      oss << ", fi_out_basic AS 'out_basic'";
      oss << ", fi_out_dilut AS 'out_dilut'";
      oss << "  FROM financial";
      oss << " WHERE fi_co_id = ?";
      oss << " ORDER BY";
      oss << "  fi_year ASC";
      oss << ", fi_qtr";
      oss << " LIMIT " << max_fin_len;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CSecurityDetailDBStmt2() {}

    virtual CSecurityDetailDBStmt2 *BindCol() {

      (*((CHandleStmt *)this))
        .BindCol( 1, &fin.year, sizeof(fin.year), &fin.year_ind)
        .BindCol( 2, &fin.qtr, sizeof(fin.qtr), &fin.qtr_ind)
        .BindCol( 3, &fin.start_date, sizeof(fin.start_date), &fin.start_date_ind)
        .BindCol( 4, &fin.rev, sizeof(fin.rev), &fin.rev_ind)
        .BindCol( 5, &fin.net_earn, sizeof(fin.net_earn), &fin.net_earn_ind)
        .BindCol( 6, &fin.basic_eps, sizeof(fin.basic_eps), &fin.basic_eps_ind)
        .BindCol( 7, &fin.dilut_eps, sizeof(fin.dilut_eps), &fin.dilut_eps_ind)
        .BindCol( 8, &fin.margin, sizeof(fin.margin), &fin.margin_ind)
        .BindCol( 9, &fin.invent, sizeof(fin.invent), &fin.invent_ind)
        .BindCol(10, &fin.assets, sizeof(fin.assets), &fin.assets_ind)
        .BindCol(11, &fin.liab, sizeof(fin.liab), &fin.liab_ind)
        .BindCol(12, &fin.out_basic, sizeof(fin.out_basic), &fin.out_basic_ind)
        .BindCol(13, &fin.out_dilut, sizeof(fin.out_dilut), &fin.out_dilut_ind);

      return this;
    }

    virtual CSecurityDetailDBStmt2 *BindCol(TSecurityDetailFrame1Output *pOut) {

      for (int i = 0; i < max_fin_len; i++) {
        fin_returned[i] = &pOut->fin[i];
      }

      return BindCol();
    }

    virtual CSecurityDetailDBStmt2 *BindParameter(const TIdent *co_id) {

      (*((CHandleStmt *)this))
        .BindParameter(1, co_id, sizeof(TIdent));

      return this;
    }

    virtual bool Execute() {

      index = 0;

      return CHandleStmt::Execute();
    }

    virtual bool Fetch() {

      memset(&fin, 0x00, sizeof(fin));

      bool result = CHandleStmt::Fetch();

      if (result) {

        memcpy(fin_returned[index], &fin, sizeof(fin));

        index++;
      }

      return result;
    }
  };

  /*
   */
  class CSecurityDetailDBStmt3 : public CHandleStmt {
  private:
    TDailyHistory day;

    int index;
    TDailyHistory *day_returned[max_day_len];

  public:
    CSecurityDetailDBStmt3(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      std::ostringstream oss;
      oss << "SELECT";
      oss << "  dm_date AS 'date'";
      oss << ", dm_close AS 'close'";
      oss << ", dm_high AS 'high'";
      oss << ", dm_low AS 'low'";
      oss << ", dm_vol AS 'vol'";
      oss << "  FROM daily_market";
      oss << " WHERE dm_s_symb = ?";
      oss << "   AND dm_date >= ?";
      oss << " ORDER BY";
      oss << "  dm_date ASC";
      oss << " LIMIT " << max_day_len;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CSecurityDetailDBStmt3() {}

    virtual CSecurityDetailDBStmt3 *BindCol() {

      (*((CHandleStmt *)this))
        .BindCol(1, &day.date, sizeof(day.date), &day.date_ind)
        .BindCol(2, &day.close, sizeof(day.close), &day.close_ind)
        .BindCol(3, &day.high, sizeof(day.high), &day.high_ind)
        .BindCol(4, &day.low, sizeof(day.low), &day.low_ind)
        .BindCol(5, &day.vol, sizeof(day.vol), &day.vol_ind);

      return this;
    }

    virtual CSecurityDetailDBStmt3 *BindCol(TSecurityDetailFrame1Output *pOut) {

      for (int i = 0; i < max_day_len; i++) {
        day_returned[i] = &pOut->day[i];
      }

      return BindCol();
    }

    virtual CSecurityDetailDBStmt3 *BindParameter(const TSecurityDetailFrame1Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, pIn->symbol, sizeof(pIn->symbol))
        .BindParameter(2, &pIn->start_day, sizeof(pIn->start_day));

      return this;
    }

    virtual bool Execute() {

      index = 0;

      return CHandleStmt::Execute();
    }

    virtual bool Fetch() {

      memset(&day, 0x00, sizeof(day));

      bool result = CHandleStmt::Fetch();

      if (result) {

        memcpy(day_returned[index], &day, sizeof(day));

        index++;
      }

      return result;
    }
  };

  /*
   */
  class CSecurityDetailDBStmt4 : public CHandleStmt {
  public:
    CSecurityDetailDBStmt4(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "SELECT"
        "  lt_price AS 'last_price'"
        ", lt_open_price AS 'last_open'"
        ", lt_vol AS 'last_vol'"
        "  FROM last_trade"
        " WHERE lt_s_symb = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    virtual ~CSecurityDetailDBStmt4() {}

    virtual CSecurityDetailDBStmt4 *BindCol(TSecurityDetailFrame1Output *pOut) {

      (*((CHandleStmt *)this))
        .BindCol(1, &pOut->last_price, sizeof(pOut->last_price))
        .BindCol(2, &pOut->last_open, sizeof(pOut->last_open))
        .BindCol(3, &pOut->last_vol, sizeof(pOut->last_vol));

      return this;
    }

    virtual CSecurityDetailDBStmt4 *BindParameter(const TSecurityDetailFrame1Input *pIn) {

      (*((CHandleStmt *)this))
        .BindParameter(1, pIn->symbol, sizeof(pIn->symbol));

      return this;
    }
  };

  /*
   */
  class CSecurityDetailDBStmt5 : public CHandleStmt {
  private:
    TNews news;

    int index;
    TNews *news_returned[max_news_len];

  public:
    CSecurityDetailDBStmt5(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      std::ostringstream oss;
      oss << "SELECT";
      oss << "  ni_item AS 'item'";
      oss << ", ni_dts AS 'dts'";
      oss << ", ni_source AS 'src'";
      oss << ", ni_author AS 'auth'"; // nullable
      oss << ", '' AS 'headline'";
      oss << ", '' AS 'summary'";
      oss << "  FROM news_xref";
      oss << "  JOIN news_item ON ni_id = nx_ni_id";
      oss << " WHERE nx_co_id = ?";
      oss << " LIMIT " << max_news_len;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CSecurityDetailDBStmt5() {}

    virtual CSecurityDetailDBStmt5 *BindCol() {

      (*((CHandleStmt *)this))
        .BindCol(1, news.item, sizeof(news.item))
        .BindCol(2, &news.dts, sizeof(news.dts))
        .BindCol(3, news.src, sizeof(news.src))
        .BindCol(4, news.auth, sizeof(news.auth), &news.auth_ind)
        .BindCol(5, news.headline, sizeof(news.headline))
        .BindCol(6, news.summary, sizeof(news.summary));

      return this;
    }

    virtual CSecurityDetailDBStmt5 *BindCol(TSecurityDetailFrame1Output *pOut) {

      for (int i = 0; i < max_news_len; i++) {
        news_returned[i] = &pOut->news[i];
      }

      return BindCol();
    }

    virtual CSecurityDetailDBStmt5 *BindParameter(const TIdent *co_id) {

      (*((CHandleStmt *)this))
        .BindParameter(1, co_id, sizeof(TIdent));

      return this;
    }

    virtual bool Execute() {

      index = 0;

      return CHandleStmt::Execute();
    }

    virtual bool Fetch() {

      memset(&news, 0x00, sizeof(news));

      bool result = CHandleStmt::Fetch();

      if (result) {

        memcpy(news_returned[index], &news, sizeof(news));

        index++;
      }

      return result;
    }
  };

  /*
   */
  class CSecurityDetailDBStmt6 : public CHandleStmt {
  private:
    TNews news;

    int index;
    TNews *news_returned[max_news_len];

  public:
    CSecurityDetailDBStmt6(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      std::ostringstream oss;
      oss << "SELECT";
      oss << "  '' AS 'item'";
      oss << ", ni_dts AS 'dts'";
      oss << ", ni_source AS 'src'";
      oss << ", ni_author AS 'auth'";       // nullable
      oss << ", ni_headline AS 'headline'";
      oss << ", ni_summary AS 'summary'";
      oss << "  FROM news_xref";
      oss << "  JOIN news_item ON ni_id = nx_ni_id";
      oss << " WHERE nx_co_id = ?";
      oss << " LIMIT " << max_news_len;

      CHandleStmt::Prepare((SQLCHAR *)oss.str().c_str());
    }

    virtual ~CSecurityDetailDBStmt6() {}

    virtual CSecurityDetailDBStmt6 *BindCol() {

      (*((CHandleStmt *)this))
        .BindCol(1, news.item, sizeof(news.item))
        .BindCol(2, &news.dts, sizeof(news.dts))
        .BindCol(3, news.src, sizeof(news.src))
        .BindCol(4, news.auth, sizeof(news.auth), &news.auth_ind)
        .BindCol(5, news.headline, sizeof(news.headline))
        .BindCol(6, news.summary, sizeof(news.summary));

      return this;
    }

    virtual CSecurityDetailDBStmt6 *BindCol(TSecurityDetailFrame1Output *pOut) {

      for (int i = 0; i < max_news_len; i++) {
        news_returned[i] = &pOut->news[i];
      }

      return BindCol();
    }

    virtual CSecurityDetailDBStmt6 *BindParameter(const TIdent *co_id) {

      (*((CHandleStmt *)this))
        .BindParameter(1, co_id, sizeof(TIdent));

      return this;
    }

    virtual bool Execute() {

      index = 0;

      return CHandleStmt::Execute();
    }

    virtual bool Fetch() {

      memset(&news, 0x00, sizeof(news));

      bool result = CHandleStmt::Fetch();

      if (result) {

        memcpy(news_returned[index], &news, sizeof(news));

        index++;
      }

      return result;
    }
  };

  /*
   */
  CSecurityDetailDB::CSecurityDetailDB(CHandleDbc *pDbc)
    : CSecurityDetailDBInterface(), m_pDbc(pDbc) {

    m_pStmt[0] = new CSecurityDetailDBStmt0(pDbc);
    m_pStmt[1] = new CSecurityDetailDBStmt1(pDbc);
    m_pStmt[2] = new CSecurityDetailDBStmt2(pDbc);
    m_pStmt[3] = new CSecurityDetailDBStmt3(pDbc);
    m_pStmt[4] = new CSecurityDetailDBStmt4(pDbc);
    m_pStmt[5] = new CSecurityDetailDBStmt5(pDbc);
    m_pStmt[6] = new CSecurityDetailDBStmt6(pDbc);
  }

  /*
   */
  CSecurityDetailDB::~CSecurityDetailDB() {

    delete m_pStmt[0];
    delete m_pStmt[1];
    delete m_pStmt[2];
    delete m_pStmt[3];
    delete m_pStmt[4];
    delete m_pStmt[5];
    delete m_pStmt[6];
  }

  /*
   */
  void CSecurityDetailDB::DoSecurityDetailFrame1(const TSecurityDetailFrame1Input *pIn,
                                                 TSecurityDetailFrame1Output *pOut) {
#ifdef _TRACE
    cout << "TRACE: CSecurityDetailDB::DoSecurityDetailFrame1/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    {
      TIdent co_id;
      SQLLEN row_count;

      CHandleStmt *pStmt;
      int p;

      { // -- 0 --
        ((CSecurityDetailDBStmt0 *)(pStmt = m_pStmt[(p = 0)]))
          ->BindParameter(pIn)
          ->BindCol(&co_id, pOut);

#ifdef _TRACE
        cout << "TRACE: [1" << p << "] symbol=" << pIn->symbol << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : security,company, ...

          for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

            co_id = 0;

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [1," << p << ":" << i << "] s_name=" << pOut->s_name << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] co_id=" << co_id << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] co_name=" << pOut->co_name << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] sp_rate=" << pOut->sp_rate << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ceo_name=" << pOut->ceo_name << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] co_desc=" << pOut->co_desc << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] open_date=" << pOut->open_date << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] co_st_id=" << pOut->co_st_id << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] co_ad_line1=" << pOut->co_ad_line1 << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] co_ad_line2=" << pOut->co_ad_line2 << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] co_ad_town=" << pOut->co_ad_town << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] co_ad_div=" << pOut->co_ad_div << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] co_ad_zip=" << pOut->co_ad_zip << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] co_ad_cty=" << pOut->co_ad_cty << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] num_out=" << pOut->num_out << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] start_date=" << pOut->start_date << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ex_date=" << pOut->ex_date << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] pe_ratio=" << pOut->pe_ratio << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] s52_wk_high=" << pOut->s52_wk_high << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] s52_wk_high_date=" << pOut->s52_wk_high_date << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] s52_wk_low=" << pOut->s52_wk_low << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] s52_wk_low_date=" << pOut->s52_wk_low_date << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] divid=" << pOut->divid << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] yield=" << pOut->yield << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ex_ad_div=" << pOut->ex_ad_div << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ex_ad_cty=" << pOut->ex_ad_cty << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ex_ad_line1=" << pOut->ex_ad_line1 << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ex_ad_line2=" << pOut->ex_ad_line2 << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ex_ad_town=" << pOut->ex_ad_town << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ex_ad_zip=" << pOut->ex_ad_zip << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ex_close=" << pOut->ex_close << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ex_desc=" << pOut->ex_desc << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ex_name=" << pOut->ex_name << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ex_num_symb=" << pOut->ex_num_symb << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] ex_open=" << pOut->ex_open << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 0 --

      { // -- 1 --
        ((CSecurityDetailDBStmt1 *)(pStmt = m_pStmt[(p = 1)]))
          ->BindParameter(&co_id)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] co_id=" << co_id << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : company_competitor, ...

          for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [1," << p << ":" << i << "] cp_co_name=" << pOut->cp_co_name[i] << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] cp_in_name=" << pOut->cp_in_name[i] << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 1 --

      { // -- 2 --
        ((CSecurityDetailDBStmt2 *)(pStmt = m_pStmt[(p = 2)]))
          ->BindParameter(&co_id)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] co_id=" << co_id << '\r' << endl;
#endif
        if (pStmt->Execute()) {

          pOut->fin_len = pStmt->RowCount();

#ifdef _TRACE
          cout << "TRACE: [1," << p << "] fin_len=" << pOut->fin_len << '\r' << endl;
#endif
          for (int size = pOut->fin_len, i = 0; i < size; i++) {

            pStmt->Fetch(); // SELECT : financial

#ifdef _TRACE
            const TFinInfo& fin = pOut->fin[i];
            cout << "TRACE: [1," << p << ":" << i << "] year=" << fin.year << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] qtr=" << fin.qtr << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] start_date=" << fin.start_date << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] rev=" << fin.rev << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] net_earn=" << fin.net_earn << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] basic_eps=" << fin.basic_eps << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] dilut_eps=" << fin.dilut_eps << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] margin=" << fin.margin << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] invent=" << fin.invent << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] assets=" << fin.assets << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] liab=" << fin.liab << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] out_basic=" << fin.out_basic << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] out_dilut=" << fin.out_dilut << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 2 --

      { // -- 3 --
        ((CSecurityDetailDBStmt3 *)(pStmt = m_pStmt[(p = 3)]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] symbol=" << pIn->symbol << '\r' << endl;
        cout << "TRACE: [1," << p << "] start_day=" << pIn->start_day << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : daily_market

          row_count = pStmt->RowCount();
          pOut->day_len = MIN(row_count, pIn->max_rows_to_return);

#ifdef _TRACE
          cout << "TRACE: [1," << p << "] row_count=" << row_count << '\r' << endl;
          cout << "TRACE: [1," << p << "] max_rows_to_return=" << pIn->max_rows_to_return << '\r' << endl;
          cout << "TRACE: [1," << p << "] day_len=" << pOut->day_len << '\r' << endl;
#endif
          for (int size = pOut->day_len, i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            const TDailyHistory& day = pOut->day[i];
            cout << "TRACE: [1," << p << ":" << i << "] date=" <<  day.date << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] close=" << day.close << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] high=" << day.high << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] low=" << day.low << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] vol=" << day.vol << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 3 --

      { // -- 4 --
        ((CSecurityDetailDBStmt4 *)(pStmt = m_pStmt[(p = 4)]))
          ->BindParameter(pIn)
          ->BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] symbol=" << pIn->symbol << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : last_trade

          for (int size = pStmt->RowCount(), i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            cout << "TRACE: [1," << p << ":" << i << "] last_price=" << pOut->last_price << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] last_open=" << pOut->last_open << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] last_vol=" << pOut->last_vol << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 4 --

      { // -- 5,6 --
        if (1 == pIn->access_lob_flag) {
          ((CSecurityDetailDBStmt5 *)(pStmt = m_pStmt[(p = 5)]))
            ->BindParameter(&co_id)
            ->BindCol(pOut);
        } else {
          ((CSecurityDetailDBStmt6 *)(pStmt = m_pStmt[(p = 6)]))
            ->BindParameter(&co_id)
            ->BindCol(pOut);
        }

#ifdef _TRACE
        cout << "TRACE: [1," << p << "] co_id=" << co_id << '\r' << endl;
#endif
        if (pStmt->Execute()) { // SELECT : news_item,news_xref

          pOut->news_len = pStmt->RowCount();

#ifdef _TRACE
          cout << "TRACE: [1," << p << "] news_len=" << pOut->news_len << '\r' << endl;
#endif
          for (int size = pOut->news_len, i = 0; i < size; i++) {

            pStmt->Fetch();

#ifdef _TRACE
            const TNews& news = pOut->news[i];
            cout << "TRACE: [1," << p << ":" << i << "] item=";
            if (p == 5) {
              cout << "(len=" << strlen(news.item) << ")" << '\r' << endl;
            } else {
              cout << news.item << '\r' << endl;
            }
            cout << "TRACE: [1," << p << ":" << i << "] dts=" <<  news.dts <<'\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] src=" << news.src << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] auth=" << news.auth << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] headline=" << news.headline << '\r' << endl;
            cout << "TRACE: [1," << p << ":" << i << "] summary=" << news.summary << '\r' << endl;
#endif
          }

          pStmt->Cancel();
        }
      } // -- 5,6 --

    }

    m_pDbc->CommitTransaction();
  }
}
