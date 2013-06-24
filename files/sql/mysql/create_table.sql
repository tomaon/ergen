SELECT @@default_storage_engine INTO @x_default_storage_engine;
-- SET @@default_storage_engine = ndbcluster;

-- == L1 ==

-- ---------------------------------------------------------
--  2.2.6.8 NEWS_ITEM (NI_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS news_item (
  ni_id       BIGINT(12)   NOT NULL -- IDENT_T
, ni_headline VARCHAR(80)  NOT NULL -- CHAR(80)
, ni_summary  VARCHAR(255) NOT NULL -- CHAR(255)
, ni_item     MEDIUMBLOB   NOT NULL -- BLOB(100000) or BLOB_REF
, ni_dts      DATETIME     NOT NULL -- DATETIME
, ni_source   VARCHAR(30)  NOT NULL -- CHAR(30)
, ni_author   VARCHAR(30)           -- CHAR(30)
, CONSTRAINT pk_news_item
    PRIMARY KEY (ni_id)
);

-- ---------------------------------------------------------
--  2.2.6.10 SECTOR (SC_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS sector (
  sc_id   CHAR(2)     NOT NULL -- CHAR(2)
, sc_name VARCHAR(30) NOT NULL -- CHAR(30)
, CONSTRAINT pk_sector
    PRIMARY KEY (sc_id)
);

-- ---------------------------------------------------------
--  2.2.7.2 STATUS_TYPE (ST_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS status_type (
  st_id   CHAR(4)  NOT NULL -- CHAR(4)
, st_name CHAR(10) NOT NULL -- CHAR(10)
, CONSTRAINT pk_status_type
    PRIMARY KEY (st_id)
);

-- ---------------------------------------------------------
--  2.2.5.9 TRADE_TYPE (TT_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS trade_type (
  tt_id      CHAR(3)     NOT NULL -- CHAR(3) / 'TMB','TMS','TSL','TLS','TLB'
, tt_name    VARCHAR(12) NOT NULL -- CHAR(12)
, tt_is_sell TINYINT(1)  NOT NULL -- BOOLEAN
, tt_is_mrkt TINYINT(1)  NOT NULL -- BOOLEAN
, CONSTRAINT pk_trade_type
    PRIMARY KEY (tt_id)
);

-- ---------------------------------------------------------
--  2.2.7.3 TAXRATE (TX_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS taxrate (
  tx_id   CHAR(4)      NOT NULL                      -- CHAR(4)
, tx_name VARCHAR(50)  NOT NULL                      -- CHAR(50)
, tx_rate DECIMAL(6,5) NOT NULL CHECK (tx_rate >= 0) -- NUM(6,5)
, CONSTRAINT pk_taxrate
    PRIMARY KEY (tx_id)
);


-- ---------------------------------------------------------
--  2.2.7.4 ZIP_CODE (ZC_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS zip_code (
  zc_code CHAR(12)    NOT NULL -- CHAR(12)
, zc_town VARCHAR(80) NOT NULL -- CHAR(80)
, zc_div  VARCHAR(80) NOT NULL -- CHAR(80)
, CONSTRAINT pk_zip_code
    PRIMARY KEY (zc_code)
);

-- == L2 ==

-- ---------------------------------------------------------
--  2.2.7.1 ADDRESS (AD_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS address (
  ad_id      BIGINT(12)  NOT NULL -- IDENT_T
, ad_line1   VARCHAR(80)          -- CHAR(80)
, ad_line2   VARCHAR(80)          -- CHAR(80)
, ad_zc_code CHAR(12)    NOT NULL -- CHAR(12)
, ad_ctry    VARCHAR(80)          -- CHAR(80)
, CONSTRAINT pk_address
    PRIMARY KEY (ad_id)
, CONSTRAINT fk_address_zc
    FOREIGN KEY (ad_zc_code) REFERENCES zip_code (zc_code)
);

-- ---------------------------------------------------------
--  2.2.5.3 CHARGE (CH_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS charge (
  ch_tt_id  CHAR(3)       NOT NULL                     -- CHAR(3)
, ch_c_tier TINYINT(1)    NOT NULL                     -- NUM(1) / 1,2,3
, ch_chrg   DECIMAL(10,2) NOT NULL CHECK (ch_chrg > 0) -- VALUE_T
, CONSTRAINT pk_charge
    PRIMARY KEY (ch_tt_id, ch_c_tier)
, CONSTRAINT fk_charge_tt
    FOREIGN KEY (ch_tt_id) REFERENCES trade_type (tt_id)
);

-- ---------------------------------------------------------
--  2.2.6.6 INDUSTRY (IN_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS industry (
  in_id    CHAR(2)     NOT NULL -- CHAR(2)
, in_name  VARCHAR(50) NOT NULL -- CHAR(50)
, in_sc_id CHAR(2)     NOT NULL -- CHAR(2)
, CONSTRAINT pk_industry
    PRIMARY KEY (in_id)
, CONSTRAINT fk_industry_sc
    FOREIGN KEY (in_sc_id) REFERENCES sector (sc_id)
);

-- == L3 ==

-- ---------------------------------------------------------
--  2.2.5.1 BROKER (B_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS broker (
  b_id         BIGINT(12)    NOT NULL -- IDENT_T
, b_st_id      CHAR(4)       NOT NULL -- CHAR(4)
, b_name       VARCHAR(49)   NOT NULL -- CHAR(49)
, b_num_trades INT(10)       NOT NULL -- NUM(9)
, b_comm_total DECIMAL(12,2) NOT NULL -- BALANCE_T
, CONSTRAINT pk_broker
    PRIMARY KEY (b_id)
, CONSTRAINT fk_broker_st
    FOREIGN KEY (b_st_id) REFERENCES status_type (st_id)
);

-- ---------------------------------------------------------
--  2.2.4.2 CUSTOMER (C_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS customer (
  c_id      BIGINT(12)  NOT NULL -- IDENT_T
, c_tax_id  VARCHAR(20) NOT NULL -- CHAR(20) / VARCHAR?
, c_st_id   CHAR(4)     NOT NULL -- CHAR(4)
, c_l_name  VARCHAR(25) NOT NULL -- CHAR(25)
, c_f_name  VARCHAR(20) NOT NULL -- CHAR(20)
, c_m_name  CHAR(1)              -- CHAR(1)
, c_gndr    CHAR(1)              -- CHAR(1) / 'M','F'
, c_tier    TINYINT(1)  NOT NULL -- NUM(1) / 1,2,3
, c_dob     DATE        NOT NULL -- DATE
, c_ad_id   BIGINT(12)  NOT NULL -- IDENT_T
, c_ctry_1  VARCHAR(3)           -- CHAR(3)
, c_area_1  VARCHAR(3)           -- CHAR(3)
, c_local_1 VARCHAR(10)          -- CHAR(10)
, c_ext_1   VARCHAR(5)           -- CHAR(5)
, c_ctry_2  VARCHAR(3)           -- CHAR(3)
, c_area_2  VARCHAR(3)           -- CHAR(3)
, c_local_2 VARCHAR(10)          -- CHAR(10)
, c_ext_2   VARCHAR(5)           -- CHAR(5)
, c_ctry_3  VARCHAR(3)           -- CHAR(3)
, c_area_3  VARCHAR(3)           -- CHAR(3)
, c_local_3 VARCHAR(10)          -- CHAR(10)
, c_ext_3   VARCHAR(5)           -- CHAR(5)
, c_email_1 VARCHAR(50)          -- CHAR(50)
, c_email_2 VARCHAR(50)          -- CHAR(50)
, CONSTRAINT pk_customer
    PRIMARY KEY (c_id)
, CONSTRAINT fk_customer_st
    FOREIGN KEY (c_st_id) REFERENCES status_type (st_id)
, CONSTRAINT fk_customer_ad
    FOREIGN KEY (c_ad_id) REFERENCES address (ad_id)
);

-- ---------------------------------------------------------
--  2.2.6.1 COMPANY (CO_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS company (
  co_id        BIGINT(12)   NOT NULL -- IDENT_T
, co_st_id     CHAR(4)      NOT NULL -- CHAR(4)
, co_name      VARCHAR(60)  NOT NULL -- CHAR(60)
, co_in_id     CHAR(2)      NOT NULL -- CHAR(2)
, co_sp_rate   CHAR(4)      NOT NULL -- CHAR(4)
, co_ceo       VARCHAR(46)  NOT NULL -- CHAR(46)
, co_ad_id     BIGINT(12)   NOT NULL -- IDENT_T
, co_desc      VARCHAR(150) NOT NULL -- CHAR(150)
, co_open_date DATE         NOT NULL -- DATE
, CONSTRAINT pk_company
    PRIMARY KEY (co_id)
, CONSTRAINT fk_company_st
    FOREIGN KEY (co_st_id) REFERENCES status_type (st_id)
, CONSTRAINT fk_company_in
    FOREIGN KEY (co_in_id) REFERENCES industry (in_id)
, CONSTRAINT fk_company_ad
    FOREIGN KEY (co_ad_id) REFERENCES address (ad_id)
);

-- ---------------------------------------------------------
--  2.2.6.4 EXCHANGE (EX_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS exchange (
  ex_id       CHAR(6)      NOT NULL -- CHAR(6) / 'NYSE','NASDAQ','AMEX','PCX'
, ex_name     VARCHAR(100) NOT NULL -- CHAR(100)
, ex_num_symb MEDIUMINT(7) NOT NULL -- NUM(6)
, ex_open     SMALLINT(5)  NOT NULL -- NUM(4)
, ex_close    SMALLINT(5)  NOT NULL -- NUM(4)
, ex_desc     VARCHAR(150)          -- CHAR(150)
, ex_ad_id    BIGINT(12)   NOT NULL -- IDENT_T
, CONSTRAINT pk_exchange
    PRIMARY KEY (ex_id)
, CONSTRAINT fk_exchange_ad
    FOREIGN KEY (ex_ad_id) REFERENCES address (ad_id)
);

-- ---------------------------------------------------------
--  2.2.6.9 NEWS_XREF (NX_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS news_xref (
  nx_ni_id BIGINT(12) NOT NULL -- IDENT_T
, nx_co_id BIGINT(12) NOT NULL -- IDENT_T
, CONSTRAINT pk_news_xref
    PRIMARY KEY (nx_co_id, nx_ni_id)
, CONSTRAINT fk_news_xref_ni
    FOREIGN KEY (nx_ni_id) REFERENCES news_item (ni_id)
, CONSTRAINT fk_news_xref_co
    FOREIGN KEY (nx_co_id) REFERENCES company (co_id)
);

-- == L4 ==

-- ---------------------------------------------------------
--  2.2.4.3 CUSTOMER_ACCOUNT (CA_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS customer_account (
  ca_id     BIGINT(12)    NOT NULL -- IDENT_T
, ca_b_id   BIGINT(12)    NOT NULL -- IDENT_T
, ca_c_id   BIGINT(12)    NOT NULL -- IDENT_T
, ca_name   VARCHAR(50)            -- CHAR(50)
, ca_tax_st TINYINT(1)    NOT NULL -- NUM(1) / 0,1,2
, ca_bal    DECIMAL(12,2) NOT NULL -- BALANCE_T
, CONSTRAINT pk_customer_account
    PRIMARY KEY (ca_id)
, CONSTRAINT fk_customer_account_b
    FOREIGN KEY (ca_b_id) REFERENCES broker (b_id)
, CONSTRAINT fk_customer_account_c
    FOREIGN KEY (ca_c_id) REFERENCES customer (c_id)
);

-- ---------------------------------------------------------
--  2.2.6.2 COMPANY_COMPETITOR (CP_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS company_competitor (
  cp_co_id      BIGINT(12) NOT NULL -- IDENT_T
, cp_comp_co_id BIGINT(12) NOT NULL -- IDENT_T
, cp_in_id      CHAR(2)    NOT NULL -- CHAR(2)
, CONSTRAINT pk_company_competitor
    PRIMARY KEY (cp_co_id, cp_comp_co_id, cp_in_id)
, CONSTRAINT fk_company_competitor_co1
    FOREIGN KEY (cp_co_id) REFERENCES company (co_id)
, CONSTRAINT fk_company_competitor_co2
    FOREIGN KEY (cp_comp_co_id) REFERENCES company (co_id)
, CONSTRAINT fk_company_competitor_in
    FOREIGN KEY (cp_in_id) REFERENCES industry (in_id)
);

-- ---------------------------------------------------------
--  2.2.5.4 COMMISSION_RATE (CR_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS commission_rate (
  cr_c_tier   TINYINT(1)   NOT NULL                                 -- NUM(1)/ 1,2,3
, cr_tt_id    CHAR(3)      NOT NULL                                 -- CHAR(3)
, cr_ex_id    CHAR(6)      NOT NULL                                 -- CHAR(6)
, cr_from_qty MEDIUMINT(7) NOT NULL CHECK (cr_from_qty >= 0)        -- S_QTY_T
, cr_to_qty   MEDIUMINT(7) NOT NULL CHECK (cr_to_qty > cr_from_qty) -- S_QTY_T
, cr_rate     DECIMAL(5,2) NOT NULL CHECK (cr_rate >= 0)            -- NUM(5,2)
, CONSTRAINT pk_commission_rate
    PRIMARY KEY (cr_c_tier, cr_tt_id, cr_ex_id, cr_from_qty)
, CONSTRAINT fk_commission_rate_tt
    FOREIGN KEY (cr_tt_id) REFERENCES trade_type (tt_id)
, CONSTRAINT fk_commission_rate_ex
    FOREIGN KEY (cr_ex_id) REFERENCES exchange (ex_id)
);

-- ---------------------------------------------------------
--  2.2.4.4 CUSTOMER_TAXRATE (CX_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS customer_taxrate (
  cx_tx_id CHAR(4)   NOT NULL -- CHAR(4)
, cx_c_id BIGINT(12) NOT NULL -- IDENT_T
, CONSTRAINT pk_customer_taxrate
    PRIMARY KEY (cx_c_id, cx_tx_id)
, CONSTRAINT fk_customer_taxrate_tx
    FOREIGN KEY (cx_tx_id) REFERENCES taxrate (tx_id)
, CONSTRAINT fk_customer_taxrate_c
    FOREIGN KEY (cx_c_id) REFERENCES customer (c_id)
);

-- ---------------------------------------------------------
--  2.2.6.5 FINANCIAL (FI_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS financial (
  fi_co_id          BIGINT(12)    NOT NULL -- IDENT_T
, fi_year           SMALLINT(5)   NOT NULL -- NUM(4)
, fi_qtr            TINYINT(2)    NOT NULL -- NUM(1) / 1,2,3,4
, fi_qtr_start_date DATE          NOT NULL -- DATE
, fi_revenue        DECIMAL(15,2) NOT NULL -- FIN_AGG_T
, fi_net_earn       DECIMAL(15,2) NOT NULL -- FIN_AGG_T
, fi_basic_eps      DECIMAL(10,2) NOT NULL -- VALUE_T
, fi_dilut_eps      DECIMAL(10,2) NOT NULL -- VALUE_T
, fi_margin         DECIMAL(10,2) NOT NULL -- VALUE_T
, fi_inventory      DECIMAL(15,2) NOT NULL -- FIN_AGG_T
, fi_assets         DECIMAL(15,2) NOT NULL -- FIN_AGG_T
, fi_liability      DECIMAL(15,2) NOT NULL -- FIN_AGG_T
, fi_out_basic      BIGINT(13)    NOT NULL -- S_COUNT_T
, fi_out_dilut      BIGINT(13)    NOT NULL -- S_COUNT_T
, CONSTRAINT pk_financial
    PRIMARY KEY (fi_co_id, fi_year, fi_qtr)
, CONSTRAINT fk_financial_co
    FOREIGN KEY (fi_co_id) REFERENCES company (co_id)
);

-- ---------------------------------------------------------
--  2.2.6.11 SECURITY (S_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS security (
  s_symb           CHAR(15)      NOT NULL -- CHAR(15)
, s_issue          CHAR(6)       NOT NULL -- CHAR(6)
, s_st_id          CHAR(4)       NOT NULL -- CHAR(4)
, s_name           VARCHAR(70)   NOT NULL -- CHAR(70)
, s_ex_id          CHAR(6)       NOT NULL -- CHAR(6)
, s_co_id          BIGINT(12)    NOT NULL -- IDENT_T
, s_num_out        BIGINT(13)    NOT NULL -- S_COUNT_T
, s_start_date     DATE          NOT NULL -- DATE
, s_exch_date      DATE          NOT NULL -- DATE
, s_pe             DECIMAL(10,2) NOT NULL -- VALUE_T
, s_52wk_high      DECIMAL(8,2)  NOT NULL -- S_PRICE_T
, s_52wk_high_date DATE          NOT NULL -- DATE
, s_52wk_low       DECIMAL(8,2)  NOT NULL -- S_PRICE_T
, s_52wk_low_date  DATE          NOT NULL -- DATE
, s_dividend       DECIMAL(10,2) NOT NULL -- VALUE_T
, s_yield          DECIMAL(5,2)  NOT NULL -- NUM(5,2) percent
, CONSTRAINT pk_security
    PRIMARY KEY (s_symb)
, CONSTRAINT fk_security_st
    FOREIGN KEY (s_st_id) REFERENCES status_type (st_id)
, CONSTRAINT fk_security_ex
    FOREIGN KEY (s_ex_id) REFERENCES exchange (ex_id)
, CONSTRAINT fk_security_co
    FOREIGN KEY (s_co_id) REFERENCES company (co_id)
);

-- ---------------------------------------------------------
--  2.2.4.9 WATCH_LIST (WL_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS watch_list (
  wl_id   BIGINT(12) NOT NULL -- IDENT_T
, wl_c_id BIGINT(12) NOT NULL -- IDENT_T
, CONSTRAINT pk_watch_list
    PRIMARY KEY (wl_id)
, CONSTRAINT fk_watch_list_wl
    FOREIGN KEY (wl_c_id) REFERENCES customer (c_id)
);

-- == L5 ==

-- ---------------------------------------------------------
--  2.2.4.1 ACCOUNT_PERMISSION (AP_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS account_permission (
  ap_ca_id  BIGINT(12)  NOT NULL -- IDENT_T
, ap_acl    CHAR(4)     NOT NULL -- CHAR(4)
, ap_tax_id VARCHAR(20) NOT NULL -- CHAR(20) / VARCHAR? PK?
, ap_l_name VARCHAR(25) NOT NULL -- CHAR(25)
, ap_f_name VARCHAR(20) NOT NULL -- CHAR(20)
, CONSTRAINT pk_account_permission
    PRIMARY KEY (ap_ca_id, ap_tax_id)
, CONSTRAINT fk_account_permission_ca
    FOREIGN KEY (ap_ca_id) REFERENCES customer_account (ca_id)
);

-- ---------------------------------------------------------
--  2.2.6.3 DAILY_MARKET (DM_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS daily_market (
  dm_date   DATE         NOT NULL -- DATE
, dm_s_symb CHAR(15)     NOT NULL -- CHAR(15)
, dm_close  DECIMAL(8,2) NOT NULL -- S_PRICE_T
, dm_high   DECIMAL(8,2) NOT NULL -- S_PRICE_T
, dm_low    DECIMAL(8,2) NOT NULL -- S_PRICE_T
, dm_vol    BIGINT(13)   NOT NULL -- S_COUNT_T
, CONSTRAINT pk_daily_market
    PRIMARY KEY (dm_s_symb, dm_date)
, CONSTRAINT fk_daily_market_s
  FOREIGN KEY (dm_s_symb) REFERENCES security (s_symb)
);

-- ---------------------------------------------------------
--  2.2.4.7 HOLDING_SUMMARY (HS_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS holding_summary (
  hs_ca_id  BIGINT(12)   NOT NULL -- IDENT_T
, hs_s_symb CHAR(15)     NOT NULL -- CHAR(15)
, hs_qty    MEDIUMINT(7) NOT NULL -- S_QTY_T
, CONSTRAINT pk_holding_summary
    PRIMARY KEY (hs_ca_id, hs_s_symb)
, CONSTRAINT fk_holding_summary_ca
    FOREIGN KEY (hs_ca_id) REFERENCES customer_account (ca_id)
, CONSTRAINT fk_holding_summary_s
    FOREIGN KEY (hs_s_symb) REFERENCES security (s_symb)
);

-- ---------------------------------------------------------
--  2.2.6.7 LAST_TRADE (LT_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS last_trade (
  lt_s_symb     CHAR(15)     NOT NULL -- CHAR(15)
, lt_dts        DATETIME     NOT NULL -- DATETIME
, lt_price      DECIMAL(8,2) NOT NULL -- S_PRICE_T
, lt_open_price DECIMAL(8,2) NOT NULL -- S_PRICE_T
, lt_vol        BIGINT(13)   NOT NULL -- S_COUNT_T
, CONSTRAINT pk_last_trade
    PRIMARY KEY (lt_s_symb)
, CONSTRAINT fk_last_trade_s
    FOREIGN KEY (lt_s_symb) REFERENCES security (s_symb)
);

-- ---------------------------------------------------------
--  2.2.5.6 TRADE (T_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS trade (
  t_id          BIGINT(16)    NOT NULL                         -- TRADE_T
, t_dts         DATETIME      NOT NULL                         -- DATETIME
, t_st_id       CHAR(4)       NOT NULL                         -- CHAR(4)
, t_tt_id       CHAR(3)       NOT NULL                         -- CHAR(3)
, t_is_cash     TINYINT(1)    NOT NULL                         -- BOOLEAN
, t_s_symb      CHAR(15)      NOT NULL                         -- CHAR(15)
, t_qty         MEDIUMINT(7)  NOT NULL CHECK (t_qty > 0)       -- S_QTY_T
, t_bid_price   DECIMAL(8,2)  NOT NULL CHECK (t_bid_price > 0) -- S_PRICE_T
, t_ca_id       BIGINT(12)    NOT NULL                         -- IDENT_T
, t_exec_name   VARCHAR(49)   NOT NULL                         -- CHAR(49)
, t_trade_price DECIMAL(8,2)                                   -- S_PRICE_T
, t_chrg        DECIMAL(10,2) NOT NULL CHECK (t_chrg >= 0)     -- VALUE_T
, t_comm        DECIMAL(10,2) NOT NULL CHECK (t_comm >= 0)     -- VALUE_T
, t_tax         DECIMAL(10,2) NOT NULL CHECK (t_tax >= 0)      -- VALUE_T
, t_lifo        TINYINT(1)    NOT NULL                         -- BOOLEAN
, CONSTRAINT pk_trade
    PRIMARY KEY (t_id)
, CONSTRAINT fk_trade_st
    FOREIGN KEY (t_st_id) REFERENCES status_type (st_id)
, CONSTRAINT fk_trade_tt
    FOREIGN KEY (t_tt_id) REFERENCES trade_type (tt_id)
, CONSTRAINT fk_trade_s
    FOREIGN KEY (t_s_symb) REFERENCES security (s_symb)
, CONSTRAINT fk_trade_ca
    FOREIGN KEY (t_ca_id) REFERENCES customer_account (ca_id)
);

-- ---------------------------------------------------------
--  2.2.4.8 WATCH_ITEM (WI_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS watch_item (
  wi_wl_id  BIGINT(12) NOT NULL -- IDENT_T
, wi_s_symb CHAR(15)   NOT NULL -- CHAR(15)
, CONSTRAINT pk_watch_item
    PRIMARY KEY (wi_wl_id, wi_s_symb)
, CONSTRAINT fk_watch_item_wl
    FOREIGN KEY (wi_wl_id) REFERENCES watch_list (wl_id)
, CONSTRAINT fk_watch_item_s
    FOREIGN KEY (wi_s_symb) REFERENCES security (s_symb)
);

-- == L6 ==

-- ---------------------------------------------------------
--  2.2.5.2 CASH_TRANSACTION (CT_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS cash_transaction (
  ct_t_id BIGINT(16)   NOT NULL -- TRADE_T
, ct_dts DATETIME      NOT NULL -- DATETIME
, ct_amt DECIMAL(10,2) NOT NULL -- VALUE_T
, ct_name VARCHAR(100)          -- CHAR(100)
, CONSTRAINT pk_cash_transaction
    PRIMARY KEY (ct_t_id)
, CONSTRAINT fk_cash_transaction_t
    FOREIGN KEY (ct_t_id) REFERENCES trade (t_id)
);

-- ---------------------------------------------------------
--  2.2.4.5 HOLDING (H_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS holding (
  h_t_id   BIGINT(16)   NOT NULL                     -- TRADE_T
, h_ca_id  BIGINT(12)   NOT NULL                     -- IDENT_T
, h_s_symb CHAR(15)     NOT NULL                     -- CHAR(15)
, h_dts    DATETIME     NOT NULL                     -- DATETIME
, h_price  DECIMAL(8,2) NOT NULL CHECK (h_price > 0) -- S_PRICE_T
, h_qty    MEDIUMINT(7) NOT NULL                     -- S_QTY_T
, CONSTRAINT pk_holding
    PRIMARY KEY (h_t_id)
, CONSTRAINT fk_holding_t
    FOREIGN KEY (h_t_id) REFERENCES trade (t_id)
, CONSTRAINT fk_holding_hs
    FOREIGN KEY (h_ca_id, h_s_symb) REFERENCES holding_summary (hs_ca_id, hs_s_symb)
);

-- ---------------------------------------------------------
--  2.2.4.6 HOLDING_HISTORY (HH_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS holding_history (
  hh_h_t_id     BIGINT(16)   NOT NULL -- TRADE_T
, hh_t_id       BIGINT(16)   NOT NULL -- TRADE_T
, hh_before_qty MEDIUMINT(7) NOT NULL -- S_QTY_T
, hh_after_qty  MEDIUMINT(7) NOT NULL -- S_QTY_T
, CONSTRAINT pk_holding_history
    PRIMARY KEY (hh_t_id, hh_h_t_id)
, CONSTRAINT fk_holding_history_t1
    FOREIGN KEY (hh_h_t_id) REFERENCES trade (t_id)
, CONSTRAINT fk_holding_history_t2
    FOREIGN KEY (hh_t_id) REFERENCES trade (t_id)
);

-- ---------------------------------------------------------
--  2.2.5.5 SETTLEMENT (SE_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS settlement (
  se_t_id          BIGINT(16)    NOT NULL -- TRADE_T
, se_cash_type     VARCHAR(40)   NOT NULL -- CHAR(40)
, se_cash_due_date DATE          NOT NULL -- DATE
, se_amt           DECIMAL(10,2) NOT NULL -- VALUE_T
, CONSTRAINT pk_settlement
    PRIMARY KEY (se_t_id)
, CONSTRAINT fk_settlement_t
    FOREIGN KEY (se_t_id) REFERENCES trade (t_id)
);

-- ---------------------------------------------------------
--  2.2.5.7 TRADE_HISTORY (TH_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS trade_history (
  th_t_id  BIGINT(16) NOT NULL -- TRADE_T
, th_dts   DATETIME   NOT NULL -- DATETIME
, th_st_id CHAR(4)    NOT NULL -- CHAR(4)
, CONSTRAINT pk_trade_history
    PRIMARY KEY (th_t_id, th_st_id)
, CONSTRAINT fk_trade_history_t
    FOREIGN KEY (th_t_id) REFERENCES trade (t_id)
, CONSTRAINT fk_trade_history_st
    FOREIGN KEY (th_st_id) REFERENCES status_type (st_id)
);

-- ---------------------------------------------------------
--  2.2.5.8 TRADE_REQUEST (TR_)
-- ---------------------------------------------------------
CREATE TABLE IF NOT EXISTS trade_request (
  tr_t_id      BIGINT(16)   NOT NULL                          -- TRADE_T
, tr_tt_id     CHAR(3)      NOT NULL                          -- CHAR(3)
, tr_s_symb    CHAR(15)     NOT NULL                          -- CHAR(15)
, tr_qty       MEDIUMINT(7) NOT NULL CHECK (tr_qty > 0)       -- S_QTY_T
, tr_bid_price DECIMAL(8,2) NOT NULL CHECK (tr_bid_price > 0) -- S_PRICE_T
, tr_b_id      BIGINT(12)   NOT NULL                          -- IDENT_T
, CONSTRAINT pk_trade_request
    PRIMARY KEY (tr_t_id)
, CONSTRAINT fk_trade_request_t
    FOREIGN KEY (tr_t_id) REFERENCES trade (t_id)
, CONSTRAINT fk_trade_request_tt
    FOREIGN KEY (tr_tt_id) REFERENCES trade_type (tt_id)
, CONSTRAINT fk_trade_request_s
    FOREIGN KEY (tr_s_symb) REFERENCES security (s_symb)
, CONSTRAINT fk_trade_request_b
    FOREIGN KEY (tr_b_id) REFERENCES broker (b_id)
);

-- = other =

CREATE TABLE IF NOT EXISTS sequence (
  name ENUM('trade')
, id   BIGINT NOT NULL
);

INSERT INTO sequence VALUE ('trade', 0)
;

COMMIT
;

-- UPDATE sequence SET id = LAST_INSERT_ID(id+1) WHERE name = 'trade';
-- SELECT LAST_INSERT_ID();

SET @@default_storage_engine = @x_default_storage_engine;
