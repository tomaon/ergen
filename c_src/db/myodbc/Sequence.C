/*
 */

#include "Sequence.H"

#include "HandleStmt.H"

namespace TPCE {

  /*
   */
  class CSequenceStmt0 : public CHandleStmt {
  public:
    CSequenceStmt0(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "UPDATE sequence"
        "   SET id = LAST_INSERT_ID(id+1)"
        "  WHERE name = ?";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    ~CSequenceStmt0() {}
  };

  /*
   */
  class CSequenceStmt1 : public CHandleStmt {
  public:
    CSequenceStmt1(CHandleDbc *pDbc) : CHandleStmt(pDbc) {

      const char *statement =
        "  SELECT LAST_INSERT_ID()";

      CHandleStmt::Prepare((SQLCHAR *)statement);
    }

    ~CSequenceStmt1() {}
  };

  /*
   */
  CSequence::CSequence(CHandleDbc *pDbc) {

    m_pStmt[0] = new CSequenceStmt0(pDbc);
    m_pStmt[1] = new CSequenceStmt1(pDbc);
  }

  /*
   */
  CSequence::~CSequence() {

    delete m_pStmt[0];
    delete m_pStmt[1];
  }

  /*
   */
  INT64 CSequence::Generate(const char *name, size_t name_len, INT64 *value) {

    bool result = false;

    m_pStmt[0]->BindParameter(1, name, name_len);

    result = m_pStmt[0]->Execute();

    if (result) {

      m_pStmt[1]->BindCol(1, value, sizeof(INT64));

      result = m_pStmt[1]->Execute();

      if (result) {

        for (int size = m_pStmt[1]->RowCount(), i = 0; i < size; i++) {
          m_pStmt[1]->Fetch();
        }

        m_pStmt[1]->Cancel();
      }

      m_pStmt[0]->Cancel();
    }

    return result ? *value : -1;
  }

}
