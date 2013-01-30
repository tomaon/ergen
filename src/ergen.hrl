%% ergen.hrl

-include_lib("amqp_client/include/amqp_client.hrl").

-type(sup_ref() :: atom()|pid()). % supervisor:sup_ref()
-type(tab() :: atom()|integer()). % ets:tab()

%% << c_src/ERGen.H

-define(APP, "ergen").

%% << c_src/ERGen.H :: eCommand

-define(CMD_ALL_INIT,                   1).
-define(CMD_ALL_ALLOC,                  2).
-define(CMD_ALL_CONFIG,                 3).
-define(CMD_ALL_FREE,                   4).
-define(CMD_CE_DO_TXN,                 11).
-define(CMD_DM_DO_TXN,                 21).
-define(CMD_DM_DO_CLEANUP_TXN,         22).
-define(CMD_MEE_SET_BASE_TIME,         31).
-define(CMD_MEE_SUBMIT_TRADE_RESULT,   32).
-define(CMD_MEE_GENERATE_TRADE_RESULT, 33).
-define(CMD_MEE_ENABLE_TICKER_TAPE,    34).
-define(CMD_MEE_DISABLE_TICKER_TAPE,   35).

-define(CMD_BH_DO_TXN_BV,             101).
-define(CMD_BH_DO_TXN_CP,             102).
-define(CMD_BH_DO_TXN_DM,             103).
-define(CMD_BH_DO_TXN_MF,             104).
-define(CMD_BH_DO_TXN_MW,             105).
-define(CMD_BH_DO_TXN_SD,             106).
-define(CMD_BH_DO_TXN_TC,             107).
-define(CMD_BH_DO_TXN_TL,             108).
-define(CMD_BH_DO_TXN_TO,             109).
-define(CMD_BH_DO_TXN_TR,             110).
-define(CMD_BH_DO_TXN_TS,             111).
-define(CMD_BH_DO_TXN_TU,             112).

-define(CMD_BH_DO_TXN_TM,             201).
