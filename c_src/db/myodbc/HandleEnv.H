/*
 */

#ifndef HANDLE_ENV_H
#define HANDLE_ENV_H

#include "odbc.H"

namespace TPCE {

  /*
   */
  class CHandleEnv {

  private:
    SQLHENV hEnv;

  public:
    CHandleEnv();
    virtual ~CHandleEnv();

    virtual SQLHANDLE handle();
  };

}

#endif // HANDLE_ENV_H
