/*
 */

#ifndef SEQUENCE_STMT_H
#define SEQUENCE_STMT_H

#include "EGenStandardTypes.h"

namespace TPCE {

  class CHandleDbc;
  class CHandleStmt;

  /*
   */
  class CSequence {
  private:
    CHandleStmt *m_pStmt[2];

  public:
    CSequence(CHandleDbc *pDbc);
    virtual ~CSequence();

    virtual INT64 Generate(const char *name, size_t name_len, INT64 *value);
  };

}

#endif // SEQUENCE_STMT_H
