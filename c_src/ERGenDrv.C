/*
 * ERGenDrv.C
 */

#include "EGenLogFormatterTab.h"

#include "ERGenDrv.H"

namespace TPCE {

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const char *name, const bool& value) {
    ei_x_encode_tuple_header(x, 2);
    {
      ei_x_encode_atom(x, name);
      ei_x_encode_boolean(x, value);
    }
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const char *name, const char& value) {
    ei_x_encode_tuple_header(x, 2);
    {
      ei_x_encode_atom(x, name);
      ei_x_encode_char(x, value);
    }
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const char *name, const double& value) {
    ei_x_encode_tuple_header(x, 2);
    {
      ei_x_encode_atom(x, name);
      ei_x_encode_double(x, value);
    }
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const char *name, const eMEETradeRequestAction& value) {
    ei_x_encode_tuple_header(x, 2);
    {
      char c = value;
      ei_x_encode_atom(x, name);
      ei_x_encode_char(x, c);
    }
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const char *name, const INT32& value) {
    ei_x_encode_tuple_header(x, 2);
    {
      ei_x_encode_atom(x, name);
      ei_x_encode_long(x, value);
    }
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const char *name, const INT64& value) {
    ei_x_encode_tuple_header(x, 2);
    {
      long long v = value; // INT64=[long long|long]
      ei_x_encode_atom(x, name);
      ei_x_encode_longlong(x, v);
    }
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const char *name, const TIMESTAMP_STRUCT& value) {
    ei_x_encode_tuple_header(x, 2);
    {
      ei_x_encode_atom(x, name);
      ei_x_encode_tuple_header(x, 2);
      {
        ei_x_encode_tuple_header(x, 3);
        {
          ei_x_encode_long(x, value.year);
          ei_x_encode_ulong(x, value.month);
          ei_x_encode_ulong(x, value.day);
        }
        ei_x_encode_tuple_header(x, 3);
        {
          ei_x_encode_ulong(x, value.hour);
          ei_x_encode_ulong(x, value.minute);
          ei_x_encode_ulong(x, value.second);
        }
      }
    }
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const char *name, const TTickerEntry& value) {
    ei_x_encode_tuple_header(x, 2);
    {
      ei_x_encode_atom(x, name);
      ei_x_encode_tuple_header(x, 3);
      {
        encode(x, "price_quote", value.price_quote);
        encode(x, "price_quote", value.trade_qty);
        encode(x, "symbol", value.symbol, strlen(value.symbol));
      }
    }
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const char *name, const TStatusAndTradeType& value) {
    ei_x_encode_tuple_header(x, 2);
    {
      ei_x_encode_atom(x, name);
      ei_x_encode_tuple_header(x, 4);
      {
        encode(x, "status_submitted", value.status_submitted, strlen(value.status_submitted));
        encode(x, "type_limit_buy", value.type_limit_buy, strlen(value.type_limit_buy));
        encode(x, "type_limit_sell", value.type_limit_sell, strlen(value.type_limit_sell));
        encode(x, "type_stop_loss", value.type_stop_loss, strlen(value.type_stop_loss));
      }
    }
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const char *name, const char value[], long len) {
    ei_x_encode_tuple_header(x, 2);
    {
      ei_x_encode_atom(x, name);
      ei_x_encode_binary(x, value, len);
    }
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const TTradeRequest& value) {
    ei_x_encode_list_header(x, 6);
    {
      encode(x, "price_quote", value.price_quote);
      encode(x, "trade_id", value.trade_id);
      encode(x, "trade_qty", value.trade_qty);
      encode(x, "e_action", value.eAction);
      encode(x, "symbol", value.symbol, strlen(value.symbol));
      encode(x, "trade_type_id", value.trade_type_id, strlen(value.trade_type_id));
    }
    ei_x_encode_empty_list(x);
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const TBrokerVolumeTxnInput& value) {
    ei_x_encode_list_header(x, 2);
    {
      ei_x_encode_tuple_header(x, 2);
      {
        ei_x_encode_atom(x, "broker_list");
        for (int i = 0; i < max_broker_list_len; i++) {
          if ('\0' == value.broker_list[i][0]) break;
          ei_x_encode_list_header(x, 1);
          {
            const char *p = value.broker_list[i];
            ei_x_encode_binary(x, p, strlen(p));
          }
        }
        ei_x_encode_empty_list(x);
      }
      encode(x, "sector_name", value.sector_name, strlen(value.sector_name));
    }
    ei_x_encode_empty_list(x);
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const TCustomerPositionTxnInput& value) {
    ei_x_encode_list_header(x, 4);
    {
      encode(x, "acct_id_idx", value.acct_id_idx);
      encode(x, "cust_id", value.cust_id);
      encode(x, "get_history", value.get_history);
      encode(x, "tax_id", value.tax_id, strlen(value.tax_id));
    }
    ei_x_encode_empty_list(x);
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const TDataMaintenanceTxnInput& value) {
    ei_x_encode_list_header(x, 8);
    {
      encode(x, "acct_id", value.acct_id);
      encode(x, "c_id", value.c_id);
      encode(x, "co_id", value.co_id);
      encode(x, "day_of_month", value.day_of_month);
      encode(x, "vol_incr", value.vol_incr);
      encode(x, "symbol", value.symbol, strlen(value.symbol));
      encode(x, "table_name", value.table_name, strlen(value.table_name));
      encode(x, "tx_id", value.tx_id, strlen(value.tx_id));
    }
    ei_x_encode_empty_list(x);
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const TMarketFeedTxnInput& value) {
    ei_x_encode_list_header(x, 5);
    {
      encode(x, "unique_symbols", value.unique_symbols);
      encode(x, "zz_padding1", value.zz_padding1, strlen(value.zz_padding1));
      encode(x, "status_and_trade_type", value.StatusAndTradeType);
      encode(x, "zz_padding2", value.zz_padding2, strlen(value.zz_padding2));
      ei_x_encode_tuple_header(x, 2);
      {
        ei_x_encode_atom(x, "entries");
        for (int i = 0; i <  max_feed_len; i++) {
          ei_x_encode_list_header(x, 1);
          {
            encode(x, "ticker_entry", value.Entries[i]);
          }
        }
        ei_x_encode_empty_list(x);
      }
    }
    ei_x_encode_empty_list(x);
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const TMarketWatchTxnInput& value) {
    ei_x_encode_list_header(x, 6);
    {
      encode(x, "acct_id", value.acct_id);
      encode(x, "c_id", value.c_id);
      encode(x, "ending_co_id", value.ending_co_id);
      encode(x, "starting_co_id", value.starting_co_id);
      encode(x, "start_day", value.start_day);
      encode(x, "industry_name", value.industry_name, strlen(value.industry_name));
    }
    ei_x_encode_empty_list(x);
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const TSecurityDetailTxnInput& value) {
    ei_x_encode_list_header(x, 4);
    {
      encode(x, "max_rows_to_return", value.max_rows_to_return);
      encode(x, "access_lob_flag", value.access_lob_flag);
      encode(x, "start_day", value.start_day);
      encode(x, "symbol", value.symbol, strlen(value.symbol));
    }
    ei_x_encode_empty_list(x);
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const TTradeCleanupTxnInput& value) {
    ei_x_encode_list_header(x, 4);
    {
      encode(x, "start_trade_id", value.start_trade_id);
      encode(x, "st_canceled_id", value.st_canceled_id, strlen(value.st_canceled_id));
      encode(x, "st_pending_id", value.st_pending_id, strlen(value.st_pending_id));
      encode(x, "st_submitted_id", value.st_submitted_id, strlen(value.st_submitted_id));
    }
    ei_x_encode_empty_list(x);
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const TTradeLookupTxnInput& value) {
    ei_x_encode_list_header(x, 8);
    {
      ei_x_encode_tuple_header(x, 2);
      {
        ei_x_encode_atom(x, "trade_id");
        for (int i = 0; i < TradeLookupFrame1MaxRows; i++) {
          if (i >= value.max_trades) break;
          ei_x_encode_list_header(x, 1);
          {
            long long v = value.trade_id[i];
            ei_x_encode_longlong(x, v);
          }
        }
        ei_x_encode_empty_list(x);
      }
      encode(x, "acct_id", value.acct_id);
      encode(x, "max_acct_id", value.max_acct_id);
      encode(x, "frame_to_execute", value.frame_to_execute);
      encode(x, "max_trades", value.max_trades);
      encode(x, "end_trade_dts", value.end_trade_dts);
      encode(x, "start_trade_dts", value.start_trade_dts);
      encode(x, "symbol", value.symbol, strlen(value.symbol));
    }
    ei_x_encode_empty_list(x);
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const TTradeOrderTxnInput& value) {
    ei_x_encode_list_header(x, 15);
    {
      encode(x, "requested_price", value.requested_price);
      encode(x, "acct_id", value.acct_id);
      encode(x, "is_lifo", value.is_lifo);
      encode(x, "roll_it_back", value.roll_it_back);
      encode(x, "trade_qty", value.trade_qty);
      encode(x, "type_is_margin", value.type_is_margin);
      encode(x, "co_name", value.co_name, strlen(value.co_name));
      encode(x, "exec_f_name", value.exec_f_name, strlen(value.exec_f_name));
      encode(x, "exec_l_name", value.exec_l_name, strlen(value.exec_l_name));
      encode(x, "exec_tax_id", value.exec_tax_id, strlen(value.exec_tax_id));
      encode(x, "issue", value.issue, strlen(value.issue));
      encode(x, "st_pending_id", value.st_pending_id, strlen(value.st_pending_id));
      encode(x, "st_submitted_id", value.st_submitted_id, strlen(value.st_submitted_id));
      encode(x, "symbol", value.symbol, strlen(value.symbol));
      encode(x, "trade_type_id", value.trade_type_id, strlen(value.trade_type_id));
    }
    ei_x_encode_empty_list(x);
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const TTradeResultTxnInput& value) {
    ei_x_encode_list_header(x, 2);
    {
      encode(x, "trade_price", value.trade_price);
      encode(x, "trade_id", value.trade_id);
    }
    ei_x_encode_empty_list(x);
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const TTradeStatusTxnInput& value) {
    ei_x_encode_list_header(x, 1);
    {
      encode(x, "acct_id", value.acct_id);
    }
    ei_x_encode_empty_list(x);
  }

  /*
   */
  void CEncoder::encode(ei_x_buff *x, const TTradeUpdateTxnInput& value) {
    ei_x_encode_list_header(x, 9);
    {
      ei_x_encode_tuple_header(x, 2);
      {
        ei_x_encode_atom(x, "trade_id");
        for (int i = 0; i < TradeUpdateFrame1MaxRows; i++) {
          if (i >= value.max_trades) break;
          ei_x_encode_list_header(x, 1);
          {
            long long v = value.trade_id[i];
            ei_x_encode_longlong(x, v);
          }
        }
        ei_x_encode_empty_list(x);
      }
      encode(x, "acct_id", value.acct_id);
      encode(x, "max_acct_id", value.max_acct_id);
      encode(x, "frame_to_execute", value.frame_to_execute);
      encode(x, "max_trades", value.max_trades);
      encode(x, "max_updates", value.max_updates);
      encode(x, "end_trade_dts", value.end_trade_dts);
      encode(x, "start_trade_dts", value.start_trade_dts);
      encode(x, "symbol", value.symbol, strlen(value.symbol));
    }
    ei_x_encode_empty_list(x);
  }

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  bool CDecoder::decode(char *buf, int *index, const char *, bool& value) {

    int arity;
    int v = 0;
    int result = -1;

    ei_decode_tuple_header(buf, index, &arity);

    if (2 == arity) {

      ei_skip_term(buf, index);
      result = ei_decode_boolean(buf, index, &v);

      if (0 == result) {
        value = v;
      }
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, const char *, char& value) {

    int arity;
    char v = 0;
    int result = -1;

    ei_decode_tuple_header(buf, index, &arity);

    if (2 == arity) {

      ei_skip_term(buf, index);
      result = ei_decode_char(buf, index, &v);

      if (0 == result) {
        value = v;
      }
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, const char *, double& value) {

    int arity;
    double v = 0;
    int result = -1;

    ei_decode_tuple_header(buf, index, &arity);

    if (2 == arity) {

      ei_skip_term(buf, index);
      result = ei_decode_double(buf, index, &v);

      if (0 == result) {
        value = v;
      }
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, const char *, eMEETradeRequestAction& value) {

    int arity;
    char v = 0;
    int result = -1;

    ei_decode_tuple_header(buf, index, &arity);

    if (2 == arity) {

      ei_skip_term(buf, index);
      result = ei_decode_char(buf, index, &v);

      if (0 == result) {
        switch (v) {
        case 0:
          value = eMEEProcessOrder;
          break;
        case 1:
          value = eMEESetLimitOrderTrigger;
          break;
        default:
          result = -1;
        }
      }
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, const char *, INT32& value) {

    int arity;
    long v = 0;
    int result = -1;

    ei_decode_tuple_header(buf, index, &arity);

    if (2 == arity) {

      ei_skip_term(buf, index);
      result = ei_decode_long(buf, index, &v);

      if (0 == result) {
        value = v;
      }
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, const char *, INT64& value) {

    int arity;
    long long v = 0;
    int result = -1;

    ei_decode_tuple_header(buf, index, &arity);

    if (2 == arity) {

      ei_skip_term(buf, index);
      result = ei_decode_longlong(buf, index, &v);

      if (0 == result) {
        value = v;
      }
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, const char *, TIMESTAMP_STRUCT& value) {

    int arity;
    long v[1] = {0};
    unsigned long uv[5] = {0,0,0,0,0};
    int result = -1;

    ei_decode_tuple_header(buf, index, &arity);

    if (2 == arity) {

      ei_skip_term(buf, index);
      ei_decode_tuple_header(buf, index, &arity);
      {
        ei_decode_tuple_header(buf, index, &arity);
        {
          ei_decode_long(buf, index, &v[0]);
          ei_decode_ulong(buf, index, &uv[0]);
          ei_decode_ulong(buf, index, &uv[1]);
        }
        ei_decode_tuple_header(buf, index, &arity);
        {
          ei_decode_ulong(buf, index, &uv[2]);
          ei_decode_ulong(buf, index, &uv[3]);
          result = ei_decode_ulong(buf, index, &uv[4]);
        }
      }

      if (0 == result) {
        value.year = v[0];
        value.month = uv[0];
        value.day =uv[1];
        value.hour = uv[2];
        value.minute = uv[3];
        value.second = uv[4];
      }
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, const char *, TTickerEntry& value) {

    int arity;
    int result = -1;

    ei_decode_tuple_header(buf, index, &arity);

    if (3 == arity) {

      decode(buf, index, "price_quote", value.price_quote);
      decode(buf, index, "trade_qty", value.trade_qty);
      decode(buf, index, "symbol", value.symbol, sizeof(value.symbol));

      result = 0;
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, const char *, TStatusAndTradeType& value) {

    int arity;
    int result = -1;

    ei_decode_tuple_header(buf, index, &arity);

    if (4 == arity) {

      decode(buf, index, "status_submitted", value.status_submitted, sizeof(value.status_submitted));
      decode(buf, index, "type_limit_buy", value.type_limit_buy, sizeof(value.type_limit_buy));
      decode(buf, index, "type_limit_sell", value.type_limit_sell, sizeof(value.type_limit_sell));
      decode(buf, index, "type_stop_loss", value.type_stop_loss, sizeof(value.type_stop_loss));

      result = 0;
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, const char *, char value[], long size) {

    int arity;
    char v[size];
    bool result = false;

    ei_decode_tuple_header(buf, index, &arity);

    if (2 == arity) {

      ei_skip_term(buf, index);
      result = decode(buf, index, v, size);

      if (result) {
        strncpy(value, v, size);
      }
    }

    return result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, char value[], long size) {

    int ei_type, ei_size;
    int result = -1;

    ei_get_type(buf, index, &ei_type, &ei_size);

    if (ERL_BINARY_EXT == ei_type && size >= ei_size) {

      char v[ei_size+1];
      long len = 0;

      result = ei_decode_binary(buf, index, v, &len), v[len] = '\0';

      if (0 == result) {
        strncpy(value, v, size);
      }
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, TTradeRequest& value) {

    int arity;
    int result = -1;

    ei_decode_list_header(buf, index, &arity);

    if (6 == arity) {

      decode(buf, index, "price_quote", value.price_quote);
      decode(buf, index, "trade_id", value.trade_id);
      decode(buf, index, "trade_qty", value.trade_qty);
      decode(buf, index, "e_action", value.eAction);
      decode(buf, index, "symbol", value.symbol, sizeof(value.symbol));
      decode(buf, index, "trade_type_id", value.trade_type_id, sizeof(value.trade_type_id));

      result = ei_skip_term(buf, index);
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, TBrokerVolumeTxnInput& value) {

    int arity;
    int result = -1;

    ei_decode_list_header(buf, index, &arity);

    if (2 == arity) {

      ei_decode_tuple_header(buf, index, &arity);
      {
        if (2 == arity) {
          ei_skip_term(buf, index); // broker_list
          ei_decode_list_header(buf, index, &arity);
          if (0 < arity) {
            for (int i = 0; i < arity; i++) {
              decode(buf, index, value.broker_list[i], sizeof(value.broker_list[0]));
            }
            value.broker_list[arity][0] = '\0';
            ei_skip_term(buf, index);
          }
        }
      }
      decode(buf, index, "sector_name", value.sector_name, sizeof(value.sector_name));
      result = ei_skip_term(buf, index);
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, TCustomerPositionTxnInput& value) {

    int arity;
    int result = -1;

    ei_decode_list_header(buf, index, &arity);

    if (4 == arity) {

      decode(buf, index, "acct_id_idx", value.acct_id_idx);
      decode(buf, index, "cust_id", value.cust_id);
      decode(buf, index, "get_history", value.get_history);
      decode(buf, index, "tax_id", value.tax_id, sizeof(value.tax_id));

      result = ei_skip_term(buf, index);
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, TDataMaintenanceTxnInput& value) {

    int arity;
    int result = -1;

    ei_decode_list_header(buf, index, &arity);

    if (8 == arity) {

      decode(buf, index, "acct_id", value.acct_id);
      decode(buf, index, "c_id", value.c_id);
      decode(buf, index, "co_id", value.co_id);
      decode(buf, index, "day_of_month", value.day_of_month);
      decode(buf, index, "vol_incr", value.vol_incr);
      decode(buf, index, "symbol", value.symbol, sizeof(value.symbol));
      decode(buf, index, "table_name", value.table_name, sizeof(value.table_name));
      decode(buf, index, "tx_id", value.tx_id, sizeof(value.tx_id));

      result = ei_skip_term(buf, index);
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, TMarketFeedTxnInput& value) {

    int arity;
    int result = -1;

    ei_decode_list_header(buf, index, &arity);

    if (5 == arity) {

      decode(buf, index, "unique_symbols", value.unique_symbols);
      decode(buf, index, "zz_padding1", value.zz_padding1, sizeof(value.zz_padding1));
      decode(buf, index, "status_and_trade_type", value.StatusAndTradeType);
      decode(buf, index, "zz_padding2", value.zz_padding2, sizeof(value.zz_padding2));
      ei_decode_tuple_header(buf, index, &arity);
      if (2 == arity) {
        ei_skip_term(buf, index); // entries
        {
          ei_decode_list_header(buf, index, &arity);
          if (0 < arity) {
            for (int i = 0; i < arity; i++) {
              decode(buf, index, "ticker_entry", value.Entries[i]);
            }
            ei_skip_term(buf, index);
          }
        }
      }

      result = ei_skip_term(buf, index);
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, TMarketWatchTxnInput& value) {

    int arity;
    int result = -1;

    ei_decode_list_header(buf, index, &arity);

    if (6 == arity) {

      decode(buf, index, "acct_id", value.acct_id);
      decode(buf, index, "c_id", value.c_id);
      decode(buf, index, "ending_co_id", value.ending_co_id);
      decode(buf, index, "starting_co_id", value.starting_co_id);
      decode(buf, index, "start_day", value.start_day);
      decode(buf, index, "industry_name", value.industry_name, sizeof(value.industry_name));

      result = ei_skip_term(buf, index);
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, TSecurityDetailTxnInput& value) {

    int arity;
    int result = -1;

    ei_decode_list_header(buf, index, &arity);

    if (4 == arity) {

      decode(buf, index, "max_rows_to_return", value.max_rows_to_return);
      decode(buf, index, "access_lob_flag", value.access_lob_flag);
      decode(buf, index, "start_day", value.start_day);
      decode(buf, index, "symbol", value.symbol, sizeof(value.symbol));

      result = ei_skip_term(buf, index);
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, TTradeCleanupTxnInput& value) {

    int arity;
    int result = -1;

    ei_decode_list_header(buf, index, &arity);

    if (4 == arity) {

      decode(buf, index, "start_trade_id", value.start_trade_id);
      decode(buf, index, "st_canceled_id", value.st_canceled_id, sizeof(value.st_canceled_id));
      decode(buf, index, "st_pending_id", value.st_pending_id, sizeof(value.st_pending_id));
      decode(buf, index, "st_submitted_id", value.st_submitted_id, sizeof(value.st_submitted_id));

      result = ei_skip_term(buf, index);
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, TTradeLookupTxnInput& value) {

    int arity;
    int result = -1;

    ei_decode_list_header(buf, index, &arity);

    if (8 == arity) {

      ei_decode_tuple_header(buf, index, &arity);
      {
        if (2 == arity) {
          ei_skip_term(buf, index); // trade_id
          ei_decode_list_header(buf, index, &arity);
          if (0 < arity) {
            for (int i = 0; i < arity; i++) {
              long long v = 0;
              ei_decode_longlong(buf, index, &v);
              value.trade_id[i] = v;
            }
            ei_skip_term(buf, index);
          }
        }
      }
      decode(buf, index, "acct_id", value.acct_id);
      decode(buf, index, "max_acct_id", value.max_acct_id);
      decode(buf, index, "frame_to_execute", value.frame_to_execute);
      decode(buf, index, "max_trades", value.max_trades);
      decode(buf, index, "end_trade_dts", value.end_trade_dts);
      decode(buf, index, "start_trade_dts", value.start_trade_dts);
      decode(buf, index, "symbol", value.symbol, sizeof(value.symbol));

      result = ei_skip_term(buf, index);
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, TTradeOrderTxnInput& value) {

    int arity;
    int result = -1;

    ei_decode_list_header(buf, index, &arity);

    if (15 == arity) {

      decode(buf, index, "requested_price", value.requested_price);
      decode(buf, index, "acct_id", value.acct_id);
      decode(buf, index, "is_lifo", value.is_lifo);
      decode(buf, index, "roll_it_back", value.roll_it_back);
      decode(buf, index, "trade_qty", value.trade_qty);
      decode(buf, index, "type_is_margin", value.type_is_margin);
      decode(buf, index, "co_name", value.co_name, sizeof(value.co_name));
      decode(buf, index, "exec_f_name", value.exec_f_name, sizeof(value.exec_f_name));
      decode(buf, index, "exec_l_name", value.exec_l_name, sizeof(value.exec_l_name));
      decode(buf, index, "exec_tax_id", value.exec_tax_id, sizeof(value.exec_tax_id));
      decode(buf, index, "issue", value.issue, sizeof(value.issue));
      decode(buf, index, "st_pending_id", value.st_pending_id, sizeof(value.st_pending_id));
      decode(buf, index, "st_submitted_id", value.st_submitted_id, sizeof(value.st_submitted_id));
      decode(buf, index, "symbol", value.symbol, sizeof(value.symbol));
      decode(buf, index, "trade_type_id", value.trade_type_id, sizeof(value.trade_type_id));

      result = ei_skip_term(buf, index);
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, TTradeResultTxnInput& value) {

    int arity;
    int result = -1;

    ei_decode_list_header(buf, index, &arity);

    if (2 == arity) {

      decode(buf, index, "trade_price", value.trade_price);
      decode(buf, index, "trade_id", value.trade_id);

      result = ei_skip_term(buf, index);
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, TTradeStatusTxnInput& value) {

    int arity;
    int result = -1;

    ei_decode_list_header(buf, index, &arity);

    if (1 == arity) {

      decode(buf, index, "acct_id", value.acct_id);

      result = ei_skip_term(buf, index);
    }

    return 0 == result;
  }

  /*
   */
  bool CDecoder::decode(char *buf, int *index, TTradeUpdateTxnInput& value) {

    int arity;
    int result = -1;

    ei_decode_list_header(buf, index, &arity);

    if (9 == arity) {

      ei_decode_tuple_header(buf, index, &arity);
      {
        if (2 == arity) {
          ei_skip_term(buf, index); // trade_id
          ei_decode_list_header(buf, index, &arity);
          if (0 < arity) {
            for (int i = 0; i < arity; i++) {
              long long v = 0;
              ei_decode_longlong(buf, index, &v);
              value.trade_id[i] = v;
            }
            ei_skip_term(buf, index);
          }
        }
      }
      decode(buf, index, "acct_id", value.acct_id);
      decode(buf, index, "max_acct_id", value.max_acct_id);
      decode(buf, index, "frame_to_execute", value.frame_to_execute);
      decode(buf, index, "max_trades", value.max_trades);
      decode(buf, index, "max_updates", value.max_updates);
      decode(buf, index, "end_trade_dts", value.end_trade_dts);
      decode(buf, index, "start_trade_dts", value.start_trade_dts);
      decode(buf, index, "symbol", value.symbol, sizeof(value.symbol));

      result = ei_skip_term(buf, index);
    }

    return 0 == result;
  }

  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------

  /*
   */
  CLogger::CLogger(ErlDrvPort ErlDrvPort, INT32 UniqueId)
    : CBaseLogger(eDriverAll, UniqueId, new CLogFormatTab), m_ErlDrvPort(ErlDrvPort) {
  }

  /*
   */
  CLogger::~CLogger() {
  }

  /*
   */
  bool CLogger::SendToLoggerImpl(const char *szPrefix, const char *, const char *szMsg) {

    ei_x_buff x;

    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    {
      ei_x_encode_atom(&x, "ok");
      ei_x_encode_tuple_header(&x, 2);
      {
        ei_x_encode_atom(&x, APP ".LG");
        ei_x_encode_list_header(&x, 2);
        {
          ei_x_encode_binary(&x, szPrefix, strlen(szPrefix));
          ei_x_encode_binary(&x, szMsg, strlen(szMsg));
        }
        ei_x_encode_empty_list(&x);
      }
    }

    int result = driver_output(m_ErlDrvPort, x.buff, x.index);

#ifdef _TRACE
#endif

    ei_x_free(&x);

    return result;
  }

} // namespace TPCE

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

/*
 */
void ergen_drv_decode_bool(char *buf, int *index, bool *p) {
  TPCE::CDecoder Decoder;
  Decoder.decode(buf, index, "", *p);
}

/*
 */
void ergen_drv_decode_char(char *buf, int *index, char *p) {
  TPCE::CDecoder Decoder;
  Decoder.decode(buf, index, "", *p);
}

/*
 */
void ergen_drv_decode_double(char *buf, int *index, double *p) {
  TPCE::CDecoder Decoder;
  Decoder.decode(buf, index, "", *p);
}

/*
 */
void ergen_drv_decode_int32(char *buf, int *index, INT32 *p) {
  TPCE::CDecoder Decoder;
  Decoder.decode(buf, index, "", *p);
}

/*
 */
void ergen_drv_decode_int64(char *buf, int *index, INT64 *p) {
  TPCE::CDecoder Decoder;
  Decoder.decode(buf, index, "", *p);
}

/*
 */
void ergen_drv_decode_int64_array(char *buf, int *index, INT64 *p) {

  int arity;
  char atom[MAXATOMLEN+1];
  long long value;

  ei_decode_tuple_header(buf, index, &arity);

  if (2 == arity) {
    ei_decode_atom(buf, index, atom);
    ei_decode_list_header(buf, index, &arity);
    if (0 < arity) {
      for (int i = 0; i < arity; i++) {
        ei_decode_longlong(buf, index, &value);
        p[i] = value;
        //fprintf(stderr, "[int64] %s[%d]=%lld\r\n", atom, i, value);
      }
      ei_skip_term(buf, index);
    } else {
      //fprintf(stderr, "[int64] %s, size=%d\r\n", atom, arity);
    }
  }
}

/*
 */
void ergen_drv_decode_string(char *buf, int *index, char *p) {

  int arity;
  long len;
  char atom[MAXATOMLEN+1];

  ei_decode_tuple_header(buf, index, &arity);

  if (2 == arity) {
    ei_decode_atom(buf, index, atom);
    ei_decode_binary(buf, index, p, &len), p[len] = '\0';
    //fprintf(stderr, "[string] %s=%s, len=%ld\r\n", atom, p, len);
  }
}

/*
 */
void ergen_drv_decode_string_array(char *buf, int *index, char *p, size_t size) {

  int arity;
  long len;
  char atom[MAXATOMLEN+1];

  ei_decode_tuple_header(buf, index, &arity);

  if (2 == arity) {
    ei_decode_atom(buf, index, atom);
    ei_decode_list_header(buf, index, &arity);
    if (0 < arity) {
      for (int i = 0; i < arity; i++, p += size) {
        ei_decode_binary(buf, index, p, &len), p[len] = '\0';
        //fprintf(stderr, "[string] %s=%s, len=%ld\r\n", atom, p, len);
      }
      ei_skip_term(buf, index);
    } else {
      //fprintf(stderr, "[string] %s, size=%d\r\n", atom, arity);
    }
  }
}

/*
 */
void ergen_drv_decode_timestamp(char *buf, int *index, void *p) {

  TPCE::TIMESTAMP_STRUCT *ts = (TPCE::TIMESTAMP_STRUCT *)p;

  TPCE::CDecoder Decoder;
  Decoder.decode(buf, index, "", *ts);
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

/*
 */
void ergen_drv_encode_bool(ei_x_buff *x, const char *name, bool value) {
  TPCE::CEncoder Encoder;
  Encoder.encode(x, name, value);
}

/*
 */
void ergen_drv_encode_char(ei_x_buff *x, const char *name, char value) {
  TPCE::CEncoder Encoder;
  Encoder.encode(x, name, value);
}

/*
 */
void ergen_drv_encode_double(ei_x_buff *x, const char *name, double value) {
  TPCE::CEncoder Encoder;
  Encoder.encode(x, name, value);
}

/*
 */
void ergen_drv_encode_int32(ei_x_buff *x, const char *name, INT32 value) {
  TPCE::CEncoder Encoder;
  Encoder.encode(x, name, value);
}

/*
 */
void ergen_drv_encode_int64(ei_x_buff *x, const char *name, INT64 value) {
  TPCE::CEncoder Encoder;
  Encoder.encode(x, name, value);
}

/*
 */
void ergen_drv_encode_string(ei_x_buff *x, const char *name, const char *value) {
  TPCE::CEncoder Encoder;
  Encoder.encode(x, name, value);
}

/*
 */
void ergen_drv_encode_timestamp(ei_x_buff *x, const char *name, const void *value) {

  TPCE::TIMESTAMP_STRUCT *ts = (TPCE::TIMESTAMP_STRUCT *)value;

  TPCE::CEncoder Encoder;
  Encoder.encode(x, name, ts);
}

/*
 */
void ergen_drv_encode_error0(ei_x_buff *x, const char *error) {
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "error");
  ei_x_encode_atom(x, error);
}

/*
 */
void ergen_drv_encode_error1(ei_x_buff *x, const char *error, const char *arg1) {
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, "error");
  ei_x_encode_tuple_header(x, 2);
  ei_x_encode_atom(x, error);
  ei_x_encode_string(x, arg1);
}

/*
 */
ErlDrvBinary *ergen_drv_bindup(void *p, ErlDrvSizeT size) {

  ErlDrvBinary *result = (ErlDrvBinary *)driver_alloc_binary(size);

  memcpy(&result->orig_bytes[0], p, size);

  return result;
}

void ergen_drv_output_ok(ErlDrvPort port) {

  ei_x_buff x;

  ei_x_new_with_version(&x);
  ei_x_encode_atom(&x, "ok");

  driver_output(port, x.buff, x.index);

  ei_x_free(&x);
}

void ergen_drv_output_bool(ErlDrvPort port, bool value) {

  ei_x_buff x;

  ei_x_new_with_version(&x);
  ei_x_encode_tuple_header(&x, 2);
  ei_x_encode_atom(&x, "ok");
  ei_x_encode_boolean(&x, value);

  driver_output(port, x.buff, x.index);

  ei_x_free(&x);
}

void ergen_drv_output_int32(ErlDrvPort port, INT32 value) {

  ei_x_buff x;

  ei_x_new_with_version(&x);
  ei_x_encode_tuple_header(&x, 2);
  ei_x_encode_atom(&x, "ok");
  ei_x_encode_long(&x, value);

  driver_output(port, x.buff, x.index);

  ei_x_free(&x);
}

void ergen_drv_output_error0(ErlDrvPort port, const char *error) {

  ei_x_buff x;

  ei_x_new_with_version(&x);
  ergen_drv_encode_error0(&x, error);

  driver_output(port, x.buff, x.index);

  ei_x_free(&x);
}

void ergen_drv_output_error1(ErlDrvPort port, const char *error, const char *arg1) {

  ei_x_buff x;

  ei_x_new_with_version(&x);
  ergen_drv_encode_error1(&x, error, arg1);

  driver_output(port, x.buff, x.index);

  ei_x_free(&x);
}
