/*
 * ERGen.C
 */

#include <ctime>

#ifdef _TRACE
# include <sstream>
#endif

#include "ERGen.H"

namespace TPCE {

  /*
   */
  TIMESTAMP_STRUCT *current_timestamp(TIMESTAMP_STRUCT *ts) {

    time_t t;
    struct tm tm;

    t = time(NULL);
    localtime_r(&t, &tm); // gettimeofday? clock_gettime?

    ts->year = tm.tm_year + 1900;
    ts->month = tm.tm_mon + 1;
    ts->day = tm.tm_mday;
    ts->hour = tm.tm_hour;
    ts->minute = tm.tm_min;
    ts->second = tm.tm_sec;
    ts->fraction = 0;

    return ts;
  }

#ifdef _TRACE

  static inline std::string S(const char *val) {
    std::ostringstream oss;
    oss << '\'' << val << "':=" << strlen(val);
    return oss.str();
  }

  // -- DateTime.h --

  std::ostream& operator<<(std::ostream& out, const TIMESTAMP_STRUCT& val) {
    return out << '{'
               << val.year << ','
               << val.month << ','
               << val.day << "},{"
               << val.hour << ','
               << val.minute << ','
               << val.second << '.'
               << val.fraction
               << '}';
  }

  // -- TxnHarnessDBInterface.h --

  std::ostream& operator<<(std::ostream& out, const TStatusAndTradeType& val) {
    return out << "{'TStatusAndTradeType'"
               << ",status_submitted=" << S(val.status_submitted)
               << ",type_limit_buy=" << S(val.type_limit_buy)
               << ",type_limit_sell=" << S(val.type_limit_sell)
               << ",type_stop_loss=" << S(val.type_stop_loss)
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTickerEntry& val) {
    return out << "{'TTickerEntry'"
               << ",price_quote=" << val.price_quote
               << ",trade_qty=" << val.trade_qty
               << ",symbol=" << S(val.symbol)
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTradeRequest& val) {
    return out << "{'TTradeRequest'"
               << ",price_quote=" << val.price_quote
               << ",trade_id=" << val.trade_id
               << ",trade_qty=" << val.trade_qty
               << ",eAction=" << (int)val.eAction
               << ",symbol=" << S(val.symbol)
               << ",trade_type_id=" << S(val.trade_type_id)
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TBrokerVolumeTxnInput& val) {

    std::ostringstream oss;
    long pos;

    for (int i = 0; '\0' != val.broker_list[i][0]; i++) oss << S(val.broker_list[i]) << ',';
    if (0 < (pos = oss.tellp())) oss.seekp(pos - 1) << ends;

    return out << "{'TBrokerVolumeTxnInput'"
               << ",broker_list=[" << oss.str() << ']'
               << ",sector_name=" << S(val.sector_name)
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TBrokerVolumeTxnOutput& val) {

    std::ostringstream oss;
    long pos;

    for (int i = 0; i < val.list_len; i++) oss << val.volume[i] << ',';
    if (0 < (pos = oss.tellp())) oss.seekp(pos - 1) << ends;

    return out << "{'TBrokerVolumeTxnOutput'"
               << ",volume=[" << oss.str() << ']'
               << ",list_len=" << val.list_len
               << ",status=" << val.status
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TCustomerPositionTxnInput& val) {
    return out << "{'TCustomerPositionTxnInput'"
               << ",acct_id_idx=" << val.acct_id_idx
               << ",cust_id=" << val.cust_id
               << ",get_history=" << val.get_history
               << ",tax_id=" << S(val.tax_id)
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TCustomerPositionTxnOutput& val) {

    std::ostringstream oss[8];
    long pos;

    for (int i = 0; i < val.acct_len; i++) {
      oss[0] << val.asset_total[i] << ',';
      oss[1] << val.cash_bal[i] << ',';
      oss[2] << val.acct_id[i] << ',';
    }
    if (0 < (pos = oss[0].tellp())) oss[0].seekp(pos - 1) << ends;
    if (0 < (pos = oss[1].tellp())) oss[1].seekp(pos - 1) << ends;
    if (0 < (pos = oss[2].tellp())) oss[2].seekp(pos - 1) << ends;

    for (int i = 0; i < val.hist_len; i++) {
      oss[3] << val.trade_id[i] << ',';
      oss[4] << val.qty[i] << ',';
      oss[5] << val.hist_dts[i] << ',';
      oss[6] << S(val.symbol[i]) << ',';
      oss[7] << S(val.trade_status[i]) << ',';
    }
    if (0 < (pos = oss[3].tellp())) oss[3].seekp(pos - 1) << ends;
    if (0 < (pos = oss[4].tellp())) oss[4].seekp(pos - 1) << ends;
    if (0 < (pos = oss[5].tellp())) oss[5].seekp(pos - 1) << ends;
    if (0 < (pos = oss[6].tellp())) oss[6].seekp(pos - 1) << ends;
    if (0 < (pos = oss[7].tellp())) oss[7].seekp(pos - 1) << ends;

    return out << "{'TCustomerPositionTxnOutput'"
               << ",asset_total=[" << oss[0].str() << ']'
               << ",cash_bal=[" << oss[1].str() << ']'
               << ",acct_id=[" << oss[2].str() << ']'
               << ",trade_id=[" << oss[3].str() << ']'
               << ",c_ad_id=" << val.c_ad_id
               << ",qty=[" << oss[4].str() << ']'
               << ",acct_len=" << val.acct_len
               << ",hist_len=" << val.hist_len
               << ",status=" << val.status
               << ",hist_dts=[" << oss[5].str() << ']'
               << ",c_dob=" << val.c_dob
               << ",symbol=[" << oss[6].str() << ']'
               << ",trade_status=[" << oss[7].str() << ']'
               << ",c_area_1=" << S(val.c_area_1)
               << ",c_area_2=" << S(val.c_area_2)
               << ",c_area_3=" << S(val.c_area_3)
               << ",c_ctry_1=" << S(val.c_ctry_1)
               << ",c_ctry_2=" << S(val.c_ctry_2)
               << ",c_ctry_3=" << S(val.c_ctry_3)
               << ",c_email_1=" << S(val.c_email_1)
               << ",c_email_2=" << S(val.c_email_2)
               << ",c_ext_1=" << S(val.c_ext_1)
               << ",c_ext_2=" << S(val.c_ext_2)
               << ",c_ext_3=" << S(val.c_ext_3)
               << ",c_f_name=" << S(val.c_f_name)
               << ",c_gndr=" << S(val.c_gndr)
               << ",c_l_name=" << S(val.c_l_name)
               << ",c_local_1=" << S(val.c_local_1)
               << ",c_local_2=" << S(val.c_local_2)
               << ",c_local_3=" << S(val.c_local_3)
               << ",c_m_name=" << S(val.c_m_name)
               << ",c_st_id=" << S(val.c_st_id)
               << ",c_tier=" << (int)val.c_tier
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TDataMaintenanceTxnInput& val) {
    return out << "{'TDataMaintenanceTxnInput'"
               << ",acct_id=" << val.acct_id
               << ",c_id=" << val.c_id
               << ",co_id=" << val.co_id
               << ",day_of_month=" << val.day_of_month
               << ",vol_incr=" << val.vol_incr
               << ",symbol=" << S(val.symbol)
               << ",table_name=" << S(val.table_name)
               << ",tx_id=" << S(val.tx_id)
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TDataMaintenanceTxnOutput& val) {
    return out << "{'TDataMaintenanceTxnOutput'"
               << ",status=" << val.status
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TMarketFeedTxnInput& val) {

    std::ostringstream oss;
    long pos;

    for (int i = 0; i < max_feed_len; i++) oss << val.Entries[i] << ',';
    if (0 < (pos = oss.tellp())) oss.seekp(pos - 1) << ends;

    return out << "{'TMarketFeedTxnInput'"
               << ",unique_symbols=" << val.unique_symbols
               << ",zz_padding1=" << S(val.zz_padding1)
               << ",StatusAndTradeType=" << val.StatusAndTradeType
               << ",zz_padding2=" << S(val.zz_padding2)
               << " Entries=[" << oss.str() << ']'
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TMarketFeedTxnOutput& val) {
    return out << "{'TMarketFeedTxnOutput'"
               << ",send_len=" << val.send_len
               << ",status=" << val.status
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TMarketWatchTxnInput& val) {
    return out << "{'TMarketWatchTxnInput'"
               << ",acct_id=" << val.acct_id
               << ",c_id=" << val.c_id
               << ",ending_co_id=" << val.ending_co_id
               << ",starting_co_id=" << val.starting_co_id
               << ",start_day=" << val.start_day
               << ",industry_name=" << S(val.industry_name)
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TMarketWatchTxnOutput& val) {
    return out << "{'TMarketWatchTxnOutput'"
               << ",pct_change=" << val.pct_change
               << ",status=" << val.status
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TSecurityDetailTxnInput& val) {
    return out << "{'TSecurityDetailTxnInput'"
               << ",max_rows_to_return=" << val.max_rows_to_return
               << ",access_lob_flag=" << val.access_lob_flag
               << ",start_day=" << val.start_day
               << ",symbol=" << S(val.symbol)
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TSecurityDetailTxnOutput& val) {
    return out << "{'TSecurityDetailTxnOutput'"
               << ",last_vol=" << val.last_vol
               << ",news_len=" << val.news_len
               << ",status=" << val.status
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTradeCleanupTxnInput& val) {
    return out << "{'TTradeCleanupTxnInput'"
               << ",start_trade_id=" << val.start_trade_id
               << ",st_canceled_id=" << S(val.st_canceled_id)
               << ",st_pending_id=" << S(val.st_pending_id)
               << ",st_submitted_id=" << S(val.st_submitted_id)
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTradeCleanupTxnOutput& val) {
    return out << "{'TTradeCleanupTxnOutput'"
               << ",status=" << val.status
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTradeLookupTxnInput& val) {

    std::ostringstream oss;
    long pos;

    for (int i = 0; i < val.max_trades; i++) oss << val.trade_id[i] << ',';
    if (0 < (pos = oss.tellp())) oss.seekp(pos - 1) << ends;

    return out << "{'TTradeLookupTxnInput'"
               << ",trade_id=[" << oss.str() << ']'
               << ",acct_id=" << val.acct_id
               << ",max_acct_id=" << val.max_acct_id
               << ",frame_to_execute=" << val.frame_to_execute
               << ",max_trades=" << val.max_trades
               << ",end_trade_dts=" << val.end_trade_dts
               << ",start_trade_dts=" << val.start_trade_dts
               << ",symbol=" << S(val.symbol)
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTradeLookupTxnOutput& val) {

    std::ostringstream oss[3];
    long pos;

    for (int i = 0; i < val.num_found; i++) {
      oss[0] << val.trade_list[i] << ',';
      oss[1] << val.is_cash[i] << ',';
      oss[2] << val.is_market[i] << ',';
    }
    if (0 < (pos = oss[0].tellp())) oss[0].seekp(pos - 1) << ends;
    if (0 < (pos = oss[1].tellp())) oss[1].seekp(pos - 1) << ends;
    if (0 < (pos = oss[2].tellp())) oss[2].seekp(pos - 1) << ends;

    return out << "{'TTradeLookupTxnOutput'"
               << ",frame_executed=" << val.frame_executed
               << ",num_found=" << val.num_found
               << ",status=" << val.status
               << ",trade_list=[" << oss[0].str() << ']'
               << ",is_cash=[" << oss[1].str() << ']'
               << ",is_market=[" << oss[2].str() << ']'
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTradeOrderTxnInput& val) {
    return out << "{'TTradeOrderTxnInput'"
               << ",requested_price=" << val.requested_price
               << ",acct_id=" << val.acct_id
               << ",is_lifo=" << val.is_lifo
               << ",roll_it_back=" << val.roll_it_back
               << ",trade_qty=" << val.trade_qty
               << ",type_is_margin=" << val.type_is_margin
               << ",co_name=" << S(val.co_name)
               << ",exec_f_name=" << S(val.exec_f_name)
               << ",exec_l_name=" << S(val.exec_l_name)
               << ",exec_tax_id=" << S(val.exec_tax_id)
               << ",issue=" << S(val.issue)
               << ",st_pending_id=" << S(val.st_pending_id)
               << ",st_submitted_id=" << S(val.st_submitted_id)
               << ",symbol=" << S(val.symbol)
               << ",trade_type_id=" << S(val.trade_type_id)
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTradeOrderTxnOutput& val) {
    return out << "{'TTradeOrderTxnOutput'"
               << ",buy_value=" << val.buy_value
               << ",sell_value=" << val.sell_value
               << ",tax_amount=" << val.tax_amount
               << ",trade_id=" << val.trade_id
               << ",status=" << val.status
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTradeResultTxnInput& val) {
    return out << "{'TTradeResultTxnInput'"
               << ",trade_price=" << val.trade_price
               << ",trade_id=" << val.trade_id
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTradeResultTxnOutput& val) {
    return out << "{'TTradeResultTxnOutput'"
               << " acct_bal=" << val.acct_bal
               << ",acct_id=" << val.acct_id
               << ",load_unit=" << val.load_unit
               << ",status=" << val.status
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTradeStatusTxnInput& val) {
    return out << "{'TTradeStatusTxnInput'"
               << ",acct_id=" << val.acct_id
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTradeStatusTxnOutput& val) {

    std::ostringstream oss[2];
    long pos;

    for (int i = 0; i < max_trade_status_len; i++) {
      oss[0] << val.trade_id[i] << ',';
      oss[1] << S(val.status_name[i]) << ',';
    }
    if (0 < (pos = oss[0].tellp())) oss[0].seekp(pos - 1) << ends;
    if (0 < (pos = oss[1].tellp())) oss[1].seekp(pos - 1) << ends;

    return out << "{'TTradeStatusTxnOutput'"
               << ",trade_id=[" << oss[0].str() << ']'
               << ",status=" << val.status
               << ",status_name=[" << oss[1].str() << ']'
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTradeUpdateTxnInput& val) {

    std::ostringstream oss;
    long pos;

    for (int i = 0; i < val. max_trades; i++) {
      oss << val.trade_id[i] << ',';
    }
    if (0 < (pos = oss.tellp())) oss.seekp(pos - 1) << ends;

    return out << "{'TTradeUpdateTxnInput'"
               << ",trade_id=[" << oss.str() << ']'
               << ",acct_id=" << val.acct_id
               << ",max_acct_id=" << val.max_acct_id
               << ",frame_to_execute=" << val.frame_to_execute
               << ",max_trades=" << val.max_trades
               << ",max_updates=" << val.max_updates
               << ",end_trade_dts=" << val.end_trade_dts
               << ",start_trade_dts=" << val.start_trade_dts
               << ",symbol=" << S(val.symbol)
               << '}';
  }

  std::ostream& operator<<(std::ostream& out, const TTradeUpdateTxnOutput& val) {

    std::ostringstream oss[3];
    long pos;

    for (int i = 0; i < val.num_found; i++) {
      oss[0] << val.trade_list[i] << ',';
      oss[1] << val.is_cash[i] << ',';
      oss[2] << val.is_market[i] << ',';
    }
    if (0 < (pos = oss[0].tellp())) oss[0].seekp(pos - 1) << ends;
    if (0 < (pos = oss[1].tellp())) oss[1].seekp(pos - 1) << ends;
    if (0 < (pos = oss[2].tellp())) oss[2].seekp(pos - 1) << ends;

    return out << "{'TTradeUpdateTxnOutput'"
               << ",trade_list=[" << oss[0].str() << ']'
               << ",frame_executed=" << val.frame_executed
               << ",num_found=" << val.num_found
               << ",num_updated=" << val.num_updated
               << ",status=" << val.status
               << ",is_cash=[" << oss[1].str() << ']'
               << ",is_market=[" << oss[2].str() << ']'
               << '}';
  }

#endif // _TRACE

} // namespace TPCE
