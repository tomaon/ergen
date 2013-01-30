/*
 * ERGen.H
 */

#ifndef ERGEN_H
#define ERGEN_H

#define APP "ergen"

#include <climits>
#include <cstring>

#include "DateTime.h"

#ifdef _TRACE

# include <ostream>

# include "TxnHarnessDBInterface.h"

#endif

namespace TPCE {

  enum eCommand {

    CMD_ALL_INIT                  =   1,
    CMD_ALL_ALLOC                 =   2,
    CMD_ALL_CONFIG                =   3,
    CMD_ALL_FREE                  =   4,
    CMD_CE_DO_TXN                 =  11,
    CMD_DM_DO_TXN                 =  21,
    CMD_DM_DO_CLEANUP_TXN         =  22,
    CMD_MEE_SET_BASE_TIME         =  31,
    CMD_MEE_SUBMIT_TRADE_RESULT   =  32,
    CMD_MEE_GENERATE_TRADE_RESULT =  33,
    CMD_MEE_ENABLE_TICKER_TAPE    =  34,
    CMD_MEE_DISABLE_TICKER_TAPE   =  35,

    CMD_BH_DO_TXN_BV              = 101,
    CMD_BH_DO_TXN_CP              = 102,
    CMD_BH_DO_TXN_DM              = 103,
    CMD_BH_DO_TXN_MF              = 104,
    CMD_BH_DO_TXN_MW              = 105,
    CMD_BH_DO_TXN_SD              = 106,
    CMD_BH_DO_TXN_TC              = 107,
    CMD_BH_DO_TXN_TL              = 108,
    CMD_BH_DO_TXN_TO              = 109,
    CMD_BH_DO_TXN_TR              = 110,
    CMD_BH_DO_TXN_TS              = 111,
    CMD_BH_DO_TXN_TU              = 112,

    CMD_BH_DO_TXN_TM              = 201,
  };

  TIMESTAMP_STRUCT *current_timestamp(TIMESTAMP_STRUCT *ts);

#ifdef _TRACE

  // -- DateTime.h --
  std::ostream& operator<<(std::ostream& out, const TIMESTAMP_STRUCT& val);

  // -- TxnHarnessDBInterface.h --
  std::ostream& operator<<(std::ostream& out, const TStatusAndTradeType& val);
  std::ostream& operator<<(std::ostream& out, const TTickerEntry& val);
  std::ostream& operator<<(std::ostream& out, const TTradeRequest& val);

  std::ostream& operator<<(std::ostream& out, const TBrokerVolumeTxnInput& val);
  std::ostream& operator<<(std::ostream& out, const TBrokerVolumeTxnOutput& val);
  std::ostream& operator<<(std::ostream& out, const TCustomerPositionTxnInput& val);
  std::ostream& operator<<(std::ostream& out, const TCustomerPositionTxnOutput& val);
  std::ostream& operator<<(std::ostream& out, const TDataMaintenanceTxnInput& val);
  std::ostream& operator<<(std::ostream& out, const TDataMaintenanceTxnOutput& val);
  std::ostream& operator<<(std::ostream& out, const TMarketFeedTxnInput& val);
  std::ostream& operator<<(std::ostream& out, const TMarketFeedTxnOutput& val);
  std::ostream& operator<<(std::ostream& out, const TMarketWatchTxnInput& val);
  std::ostream& operator<<(std::ostream& out, const TMarketWatchTxnOutput& val);
  std::ostream& operator<<(std::ostream& out, const TSecurityDetailTxnInput& val);
  std::ostream& operator<<(std::ostream& out, const TSecurityDetailTxnOutput& val);
  std::ostream& operator<<(std::ostream& out, const TTradeCleanupTxnInput& val);
  std::ostream& operator<<(std::ostream& out, const TTradeCleanupTxnOutput& val);
  std::ostream& operator<<(std::ostream& out, const TTradeLookupTxnInput& val);
  std::ostream& operator<<(std::ostream& out, const TTradeLookupTxnOutput& val);
  std::ostream& operator<<(std::ostream& out, const TTradeOrderTxnInput& val);
  std::ostream& operator<<(std::ostream& out, const TTradeOrderTxnOutput& val);
  std::ostream& operator<<(std::ostream& out, const TTradeResultTxnInput& val);
  std::ostream& operator<<(std::ostream& out, const TTradeResultTxnOutput& val);
  std::ostream& operator<<(std::ostream& out, const TTradeStatusTxnInput& val);
  std::ostream& operator<<(std::ostream& out, const TTradeStatusTxnOutput& val);
  std::ostream& operator<<(std::ostream& out, const TTradeUpdateTxnInput& val);
  std::ostream& operator<<(std::ostream& out, const TTradeUpdateTxnOutput& val);

#endif // _TRACE

} // namespace TPCE

#endif // ERGEN_H
