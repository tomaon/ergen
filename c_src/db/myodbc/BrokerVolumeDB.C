/*
 */

#include "BrokerVolumeDB.H"

#include <sstream>

#include "HandleDbc.H"
#include "HandleStmt.H"

namespace TPCE {

  /*
   */
  class CBrokerVolumeDBStmt : public CHandleStmt {
  private:
    char broker_name[cB_NAME_len+1];
    double volume;

    int index;
    char *broker_name_returned[max_broker_list_len];
    double *volume_returned[max_broker_list_len];

  public:
    CBrokerVolumeDBStmt(CHandleDbc *pDbc) : CHandleStmt(pDbc) {}
    virtual ~CBrokerVolumeDBStmt() {}

    virtual CBrokerVolumeDBStmt& BindCol() {

      (*((CHandleStmt *)this))
        .BindCol(1, broker_name, sizeof(broker_name))
        .BindCol(2, &volume, sizeof(volume));

      return *this;
    }

    virtual CBrokerVolumeDBStmt& BindCol(TBrokerVolumeFrame1Output *pOut) {

      for (int i = 0; i < max_broker_list_len; i++) {
        broker_name_returned[i] = pOut->broker_name[i];
        volume_returned[i] = &pOut->volume[i];
      }

      return BindCol();
    }

    virtual bool ExecDirect(const TBrokerVolumeFrame1Input *pIn) {

      std::ostringstream oss;
      oss << "SELECT";
      oss << "  b_name AS 'broker_name'";
      oss << ", SUM(tr_qty * tr_bid_price) AS volume";
      oss << "  FROM sector";
      oss << "  JOIN industry ON in_sc_id = sc_id";
      oss << "  JOIN company ON co_in_id = in_id";
      oss << "  JOIN security ON s_co_id = co_id";
      oss << "  JOIN trade_request ON tr_s_symb = s_symb";
      oss << "  JOIN broker ON b_id = tr_b_id AND b_name IN (" << GetBrokerList(pIn).c_str() << ")";
      oss << " WHERE sc_name = '" << pIn->sector_name << "'";
      oss << " GROUP BY";
      oss << "  b_name";
      oss << " ORDER BY";
      oss << "  volume DESC";
      oss << " LIMIT " << max_broker_list_len;

      index = 0;

      return CHandleStmt::ExecDirect((const SQLCHAR *)oss.str().c_str());
    }

    virtual bool Fetch() {

      memset(broker_name, 0x00, sizeof(broker_name));
      volume = 0.0;

      bool result = CHandleStmt::Fetch();

      if (result) {

        memcpy(broker_name_returned[index], broker_name, sizeof(broker_name));
        *volume_returned[index] = volume;

        index++;
      }

      return result;
    }

    virtual std::string GetBrokerList(const TBrokerVolumeFrame1Input *pIn) {
      return ODBC::Implode(",",
                           (const char *)pIn->broker_list,
                           sizeof(pIn->broker_list[0]),
                           "'");
    }
  };

  /*
   */
  CBrokerVolumeDB::CBrokerVolumeDB(CHandleDbc *pDbc)
    : CBrokerVolumeDBInterface(), m_pDbc(pDbc) {
  }

  /*
   */
  CBrokerVolumeDB::~CBrokerVolumeDB() {
  }

  /*
   */
  void CBrokerVolumeDB::DoBrokerVolumeFrame1(const TBrokerVolumeFrame1Input *pIn,
                                             TBrokerVolumeFrame1Output *pOut) {

#ifdef _TRACE
    cout << "TRACE: CBrokerVolumeDB::DoBrokerVolumeFrame1/2" << '\r' << endl;
#endif

    m_pDbc->StartTransaction();

    {
      pOut->list_len = 0;

      {
        CBrokerVolumeDBStmt hStmt(m_pDbc);

        hStmt.BindCol(pOut);

#ifdef _TRACE
        cout << "TRACE: [1] broker_list=" << hStmt.GetBrokerList(pIn) << '\r' << endl;
        cout << "TRACE: [1] sector_name=" << pIn->sector_name << '\r' << endl;
#endif
        if (hStmt.ExecDirect(pIn)) {

          pOut->list_len = hStmt.RowCount();

#ifdef _TRACE
          cout << "TRACE: [1] list_len=" << pOut->list_len << '\r' << endl;
#endif
          for (SQLLEN size = pOut->list_len, i = 0; i < size; i++) {

            hStmt.Fetch();

#ifdef _TRACE
            cout << "TRACE: [1:" << i << "] broker_name=" << pOut->broker_name[i] << '\r' << endl;
            cout << "TRACE: [1:" << i << "] volume=" << pOut->volume[i] << '\r' << endl;
#endif
          }

          hStmt.Cancel();
        }
      }

    }

    m_pDbc->CommitTransaction();
  }

}
