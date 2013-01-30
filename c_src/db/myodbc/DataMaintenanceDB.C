/*
 */

#include "DataMaintenanceDB.H"

#include <sstream>

#include "HandleDbc.H"
#include "HandleStmt.H"

namespace TPCE {

  /*
   */
  CDataMaintenanceDB::CDataMaintenanceDB(CHandleDbc *pDbc)
    : CDataMaintenanceDBInterface(), m_pDbc(pDbc) {
  }

  /*
   */
  CDataMaintenanceDB::~CDataMaintenanceDB() {
  }

  /*
   */
  void CDataMaintenanceDB::DoDataMaintenanceFrame1(const TDataMaintenanceFrame1Input *pIn) {

#ifdef _TRACE
    cout << "TRACE: CDataMaintenanceDB::DoDataMaintenanceFrame1/1" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    {
      CHandleStmt hStmt(m_pDbc);

      if (0 == strcmp(pIn->table_name, "ACCOUNT_PERMISSION")) {

        std::ostringstream oss;
        oss << "UPDATE account_permission";
        oss << "   SET ap_acl = CASE ap_acl WHEN '1111' THEN '0011' ELSE '1111' END";
        oss << " WHERE ap_ca_id = " << pIn->acct_id;

#ifdef _TRACE
        cout << "TRACE: [1,AP] acct_id=" << pIn->acct_id << '\r' << endl;
#endif
        if (hStmt.ExecDirect((SQLCHAR *)oss.str().c_str())) {

#ifdef _TRACE
          cout << "TRACE: [1,AP] row_count=" << hStmt.RowCount() << '\r' << endl;
#endif
          hStmt.Cancel();
        }

      } else if (0 == strcmp(pIn->table_name, "ADDRESS")) {

        std::ostringstream oss;
        oss << "UPDATE address";
        oss << "   SET ad_line2 = "
            << "CASE ad_line2 WHEN 'Apt. 10C' THEN 'Apt. 22' ELSE 'Apt. 10C' END";
        oss << " WHERE ad_id = (";

        if (0 != pIn->c_id) {
          oss << "SELECT c_ad_id";
          oss << "  FROM customer";
          oss << " WHERE c_id = " << pIn->c_id;
        } else {
          oss << "SELECT co_ad_id";
          oss << "  FROM company";
          oss << " WHERE co_id = " << pIn->co_id;
        }

        oss << ")";

#ifdef _TRACE
        cout << "TRACE: [1,AD] c_id=" << pIn->c_id << '\r' << endl;
        cout << "TRACE: [1,AD] co_id=" << pIn->co_id << '\r' << endl;
#endif
        if (hStmt.ExecDirect((SQLCHAR *)oss.str().c_str())) {

#ifdef _TRACE
          cout << "TRACE: [1,AD] row_count=" << hStmt.RowCount() << '\r' << endl;
#endif
          hStmt.Cancel();
        }

      } else if (0 == strcmp(pIn->table_name, "COMPANY")) {

        std::ostringstream oss;
        oss << "UPDATE company";
        oss << "   SET co_sp_rate = CASE co_sp_rate WHEN 'ABA' THEN 'AAA' ELSE 'ABA' END";
        oss << " WHERE co_id = " << pIn->co_id;

#ifdef _TRACE
        cout << "TRACE: [1,CO] co_id=" << pIn->co_id << '\r' << endl;
#endif
        if (hStmt.ExecDirect((SQLCHAR *)oss.str().c_str())) {

#ifdef _TRACE
          cout << "TRACE: [1,CO] row_count=" << hStmt.RowCount() << '\r' << endl;
#endif
          hStmt.Cancel();
        }

      } else if (0 == strcmp(pIn->table_name, "CUSTOMER")) {

        std::ostringstream oss;
        oss << "UPDATE customer";
        oss << "   SET c_email_2 = CONCAT(";
        oss << "         SUBSTRING(c_email_2, 1, INSTR(c_email_2, '@'))";
        oss << "       , CASE c_email_2 LIKE BINARY '%@mindspring.com'";
        oss << "           WHEN 1 THEN 'earthlink.com'";
        oss << "                  ELSE 'mindspring.com' END";
        oss << "       )";
        oss << " WHERE c_id = " << pIn->c_id;

#ifdef _TRACE
        cout << "TRACE: [1,C] c_id=" << pIn->c_id << '\r' << endl;
#endif
        if (hStmt.ExecDirect((SQLCHAR *)oss.str().c_str())) {

#ifdef _TRACE
          cout << "TRACE: [1,C] row_count=" << hStmt.RowCount() << '\r' << endl;
#endif
          hStmt.Cancel();
        }

      } else if (0 == strcmp(pIn->table_name, "CUSTOMER_TAXRATE")) {

        std::ostringstream oss;
        oss << "UPDATE customer_taxrate";
        oss << "   SET cx_tx_id = CASE cx_tx_id";
        oss << "         WHEN 'US1' THEN 'US2'";
        oss << "         WHEN 'US2' THEN 'US3'";
        oss << "         WHEN 'US3' THEN 'US4'";
        oss << "         WHEN 'US4' THEN 'US5'";
        oss << "         WHEN 'US5' THEN 'US1'";
        oss << "         WHEN 'CN1' THEN 'CN2'";
        oss << "         WHEN 'CN2' THEN 'CN3'";
        oss << "         WHEN 'CN3' THEN 'CN4'";
        oss << "         WHEN 'CN4' THEN 'CN1'";
        oss << "                    ELSE cx_tx_id END";
        oss << " WHERE cx_c_id = " << pIn->c_id;
        oss << "   AND (cx_tx_id LIKE 'US%' OR cx_tx_id LIKE 'CN%')";

#ifdef _TRACE
        cout << "TRACE: [1,CX] c_id=" << pIn->c_id << '\r' << endl;
#endif
        if (hStmt.ExecDirect((SQLCHAR *)oss.str().c_str())) {

#ifdef _TRACE
          cout << "TRACE: [1,CX] row_count=" << hStmt.RowCount() << '\r' << endl;
#endif
          hStmt.Cancel();
        }

      } else if (0 == strcmp(pIn->table_name, "DAILY_MARKET")) {

        std::ostringstream oss;
        oss << "UPDATE daily_market";
        oss << "   SET dm_vol = dm_vol + " << pIn->vol_incr;
        oss << " WHERE dm_s_symb = '" << pIn->symbol << "'";
        oss << "   AND EXTRACT(DAY FROM dm_date) = " << pIn->day_of_month;

#ifdef _TRACE
        cout << "TRACE: [1,DM] symbol=" << pIn->symbol << '\r' << endl;
        cout << "TRACE: [1,DM] day_of_month=" << pIn->day_of_month << '\r' << endl;
        cout << "TRACE: [1,DM] vol_incr=" << pIn->vol_incr << '\r' << endl;
#endif
        if (hStmt.ExecDirect((SQLCHAR *)oss.str().c_str())) {

#ifdef _TRACE
          cout << "TRACE: [1,DM] row_count=" << hStmt.RowCount() << '\r' << endl;
#endif
          hStmt.Cancel();
        }

      } else if (0 == strcmp(pIn->table_name, "EXCHANGE")) {

        const char *statement =
          "UPDATE exchange"
          "   SET ex_desc = CASE ex_desc LIKE BINARY '%LAST UPDATED%'"
          "                   WHEN 1 THEN CONCAT("
          "                     SUBSTRING(ex_desc, 1, LENGTH(ex_desc) - 17)"
          "                   , DATE_FORMAT(NOW(),GET_FORMAT(DATETIME,'ISO'))"
          "                   )"
          "                   ELSE CONCAT("
          "                     ex_desc"
          "                   , ' LAST UPDATED '"
          "                   , DATE_FORMAT(NOW(),GET_FORMAT(DATETIME,'ISO'))"
          "                   )"
          "                 END";

        if (hStmt.ExecDirect((SQLCHAR *)statement)) {

#ifdef _TRACE
          cout << "TRACE: [1,EX] row_count=" << hStmt.RowCount() << '\r' << endl;
#endif
          hStmt.Cancel();
        }

      } else if (0 == strcmp(pIn->table_name, "FINANCIAL")) {

        std::ostringstream oss;
        oss << "UPDATE financial";
        oss << "   SET fi_qtr_start_date = CASE EXTRACT(DAY FROM fi_qtr_start_date)";
        oss << "                             WHEN 1 THEN fi_qtr_start_date + INTERVAL 1 DAY";
        oss << "                                    ELSE fi_qtr_start_date - INTERVAL 1 DAY";
        oss << "                           END";
        oss << " WHERE fi_co_id = " << pIn->co_id;

#ifdef _TRACE
        cout << "TRACE: [1,FI] co_id=" << pIn->co_id << '\r' << endl;
#endif
        if (hStmt.ExecDirect((SQLCHAR *)oss.str().c_str())) {

#ifdef _TRACE
          cout << "TRACE: [1,FI] row_count=" << hStmt.RowCount() << '\r' << endl;
#endif
          hStmt.Cancel();
        }

      } else if (0 == strcmp(pIn->table_name, "NEWS_ITEM")) {

        std::ostringstream oss;
        oss << "UPDATE news_item";
        oss << "  JOIN news_xref ON ni_id = nx_ni_id";
        oss << "   SET ni_dts = ni_dts + INTERVAL 1 DAY";
        oss << " WHERE nx_co_id = " << pIn->co_id;

#ifdef _TRACE
        cout << "TRACE: [1,NI] co_id=" << pIn->co_id << '\r' << endl;
#endif
        if (hStmt.ExecDirect((SQLCHAR *)oss.str().c_str())) {

#ifdef _TRACE
          cout << "TRACE: [1,NI] row_count=" << hStmt.RowCount() << '\r' << endl;
#endif
          hStmt.Cancel();
        }

      } else if (0 == strcmp(pIn->table_name, "SECURITY")) {

        std::ostringstream oss;
        oss << "UPDATE security";
        oss << "   SET s_exch_date = s_exch_date + INTERVAL 1 DAY";
        oss << " WHERE s_symb = '" << pIn->symbol << "'";

#ifdef _TRACE
        cout << "TRACE: [1,S] symbol=" << pIn->symbol << '\r' << endl;
#endif
        if (hStmt.ExecDirect((SQLCHAR *)oss.str().c_str())) {

#ifdef _TRACE
          cout << "TRACE: [1,S] row_count=" << hStmt.RowCount() << '\r' << endl;
#endif
          hStmt.Cancel();
        }

      } else if (0 == strcmp(pIn->table_name, "TAXRATE")) {

        std::ostringstream oss;
        oss << "UPDATE taxrate";
        oss << "   SET tx_name = CASE tx_name LIKE BINARY '% Tax %'";
        oss << "                   WHEN 1 THEN REPLACE(tx_name,' Tax ',' tax ')";
        oss << "                          ELSE REPLACE(tx_name,' tax ',' Tax ')";
        oss << "                 END";
        oss << " WHERE tx_id = '" << pIn->tx_id << "'";

#ifdef _TRACE
        cout << "TRACE: [1,TX] tx_id=" << pIn->tx_id << '\r' << endl;
#endif
        if (hStmt.ExecDirect((SQLCHAR *)oss.str().c_str())) {

#ifdef _TRACE
          cout << "TRACE: [1,TX] row_count=" << hStmt.RowCount() << '\r' << endl;
#endif
          hStmt.Cancel();
        }

      } else if (0 == strcmp(pIn->table_name, "WATCH_ITEM")) {

        char old_symbol[cSYMBOL_len+1];
        char new_symbol[cSYMBOL_len+1];

        { // -- 1 --
          std::ostringstream oss;
          oss << "SELECT o.wi_s_symb AS 'old_symbol'";
          oss << "  FROM (";
          oss << "    SELECT";
          oss << "      @rownum := @rownum + 1 AS 'rownum'";
          oss << "    , wi_s_symb";
          oss << "      FROM (SELECT @rownum := 0) s, (";
          oss << "        SELECT wi_s_symb";
          oss << "          FROM watch_list";
          oss << "          JOIN watch_item ON wi_wl_id = wl_id";
          oss << "         WHERE wl_c_id = " << pIn->c_id;
          oss << "         ORDER BY wi_s_symb ASC";
          oss << "      ) i";
          oss << "  ) o";
          oss << " WHERE rownum = (";
          oss << "   SELECT COUNT(*)";
          oss << "     FROM watch_list";
          oss << "     JOIN watch_item ON wi_wl_id = wl_id";
          oss << "    WHERE wl_c_id = " << pIn->c_id;
          oss << " ) / 2";

          hStmt.BindCol(1, old_symbol, sizeof(old_symbol));

#ifdef _TRACE
          cout << "TRACE: [1,WI,1] c_id=" << pIn->c_id << '\r' << endl;
#endif
          if (hStmt.ExecDirect((SQLCHAR *)oss.str().c_str())) {

            for (size_t size = hStmt.RowCount(), i = 0; i < size; i++) {

              memset(old_symbol, 0x00, sizeof(old_symbol));

              hStmt.Fetch();
#ifdef _TRACE
              cout << "TRACE: [1,WI,1:" << i << "] old_symbol=" << old_symbol << '\r' << endl;
#endif
            }

            hStmt.Cancel();
          }
        } // -- 1 --

        { // -- 2 --
          std::ostringstream oss;
          oss << "SELECT s_symb";
          oss << "  FROM security";
          oss << " WHERE s_symb > '" << old_symbol << "'";
          oss << "   AND NOT EXISTS(";
          oss << "         SELECT 1";
          oss << "           FROM watch_list";
          oss << "           JOIN watch_item ON wi_wl_id = wl_id";
          oss << "          WHERE wl_c_id = " << pIn->c_id;
          oss << "            AND wi_s_symb = s_symb";
          oss << "       )";
          oss << " ORDER BY";
          oss << "  s_symb ASC";
          oss << " LIMIT 1";

          hStmt.BindCol(1, new_symbol, sizeof(new_symbol));

#ifdef _TRACE
          cout << "TRACE: [1,WI,2] c_id=" << pIn->c_id << '\r' << endl;
          cout << "TRACE: [1,WI,2] old_symbol=" << old_symbol << '\r' << endl;
#endif
          if (hStmt.ExecDirect((SQLCHAR *)oss.str().c_str())) {

#ifdef _TRACE
            cout << "TRACE: [1,WI,2] row_count=" << hStmt.RowCount() << '\r' << endl;
#endif
            for (size_t size = hStmt.RowCount(), i = 0; i < size; i++) {

              memset(new_symbol, 0x00, sizeof(new_symbol));

              hStmt.Fetch();

#ifdef _TRACE
              cout << "TRACE: [1,WI,2:" << i << "] new_symbol=" << new_symbol << '\r' << endl;
#endif
            }

            hStmt.Cancel();
          }
        } // -- 2 --

        { // -- 3 --
          std::ostringstream oss;
          oss << "UPDATE watch_item";
          oss << "  JOIN watch_list ON wl_id = wi_wl_id";
          oss << "   SET wi_s_symb = '" << new_symbol << "'";
          oss << " WHERE wl_c_id = " << pIn->c_id;
          oss << "   AND wi_s_symb = '" << old_symbol << "'";

#ifdef _TRACE
          cout << "TRACE: [1,WI,3] c_id=" << pIn->c_id << '\r' << endl;
          cout << "TRACE: [1,WI,3] old_symbol=" << old_symbol << '\r' << endl;
          cout << "TRACE: [1,WI,3] new_symbol=" << new_symbol << '\r' << endl;
#endif
          if (hStmt.ExecDirect((SQLCHAR *)oss.str().c_str())) {

#ifdef _TRACE
            cout << "TRACE: [1,WI,3] row_count=" << hStmt.RowCount() << '\r' << endl;
#endif
            hStmt.Cancel();
          }
        } // -- 3 --
      }
    }

    m_pDbc->CommitTransaction();
  }

}
