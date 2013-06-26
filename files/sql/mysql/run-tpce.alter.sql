SET @tmp_ndb_table_no_logging = @@ndb_table_no_logging;
SET @@ndb_table_no_logging = 1;
SELECT @@ndb_table_no_logging;

SET @tmp_ndb_use_copying_alter_table = @@ndb_use_copying_alter_table;
SET @@ndb_use_copying_alter_table = 1;
SELECT @@ndb_use_copying_alter_table;

-- PARTITIONS=3, NODEGROUP=[0]

-- == L6 ==

-- ---------------------------------------------------------
--  2.2.5.8 TRADE_REQUEST (TR_)
-- ---------------------------------------------------------
ALTER TABLE trade_request
  DROP FOREIGN KEY fk_trade_request_t
-- ROP INDEX fk_trade_request_t
, DROP FOREIGN KEY fk_trade_request_tt
, DROP INDEX fk_trade_request_tt
, DROP FOREIGN KEY fk_trade_request_s
, DROP INDEX fk_trade_request_s
, DROP FOREIGN KEY fk_trade_request_b
, DROP INDEX fk_trade_request_b
;

ALTER TABLE trade_request
  ENGINE ndbcluster
  PARTITION BY KEY (tr_t_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

CREATE INDEX ix_tr_s_symb ON trade_request (tr_s_symb);

-- ---------------------------------------------------------
--  2.2.5.7 TRADE_HISTORY (TH_)
-- ---------------------------------------------------------
ALTER TABLE trade_history
  DROP FOREIGN KEY fk_trade_history_t
-- ROP INDEX       fk_trade_history_t
, DROP FOREIGN KEY fk_trade_history_st
, DROP INDEX       fk_trade_history_st
;

ALTER TABLE trade_history
  ENGINE ndbcluster
  PARTITION BY KEY (th_t_id) -- != PK
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

CREATE INDEX ix_th_t_id_dts ON trade_history (th_t_id, th_dts);

-- ---------------------------------------------------------
--  2.2.5.5 SETTLEMENT (SE_)
-- ---------------------------------------------------------
ALTER TABLE settlement
  DROP FOREIGN KEY fk_settlement_t
-- ROP INDEX       fk_settlement_t
;

ALTER TABLE settlement
  ENGINE ndbcluster
  PARTITION BY KEY (se_t_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.4.6 HOLDING_HISTORY (HH_)
-- ---------------------------------------------------------
ALTER TABLE holding_history
  DROP FOREIGN KEY fk_holding_history_t1
, DROP INDEX       fk_holding_history_t1
, DROP FOREIGN KEY fk_holding_history_t2
-- ROP INDEX       fk_holding_history_t2
;

ALTER TABLE holding_history
  ENGINE ndbcluster
  PARTITION BY KEY (hh_t_id) -- != PK
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- CREATE UNIQUE INDEX ui_holding_history
--   USING HASH
--   ON holding_history (hh_h_t_id, hh_t_id)
-- ;

-- ---------------------------------------------------------
--  2.2.4.5 HOLDING (H_)
-- ---------------------------------------------------------
ALTER TABLE holding
  DROP FOREIGN KEY fk_holding_t
-- ROP INDEX       fk_holding_t
, DROP FOREIGN KEY fk_holding_hs
, DROP INDEX       fk_holding_hs
;

ALTER TABLE holding
  ENGINE ndbcluster
  PARTITION BY KEY (h_t_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

CREATE INDEX ix_h_ca_id_s_symb_dts ON holding (h_ca_id, h_s_symb, h_dts);

-- ---------------------------------------------------------
--  2.2.5.2 CASH_TRANSACTION (CT_)
-- ---------------------------------------------------------
ALTER TABLE cash_transaction
  DROP FOREIGN KEY fk_cash_transaction_t
-- ROP INDEX       fk_cash_transaction_t
;

ALTER TABLE cash_transaction
  ENGINE ndbcluster
  PARTITION BY KEY (ct_t_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
 ;

-- == L5 ==

-- ---------------------------------------------------------
--  2.2.4.8 WATCH_ITEM (WI_)
-- ---------------------------------------------------------
ALTER TABLE watch_item
  DROP FOREIGN KEY fk_watch_item_wl
-- ROP INDEX       fk_watch_item_wl
, DROP FOREIGN KEY fk_watch_item_s
, DROP INDEX       fk_watch_item_s
;

ALTER TABLE watch_item
  ENGINE ndbcluster
  PARTITION BY KEY (wi_wl_id) -- != PK
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.5.6 TRADE (T_)
-- ---------------------------------------------------------
ALTER TABLE trade
  DROP FOREIGN KEY fk_trade_st
, DROP INDEX       fk_trade_st
, DROP FOREIGN KEY fk_trade_tt
, DROP INDEX       fk_trade_tt
, DROP FOREIGN KEY fk_trade_s
, DROP INDEX       fk_trade_s
, DROP FOREIGN KEY fk_trade_ca
, DROP INDEX       fk_trade_ca
;

ALTER TABLE trade
  ENGINE ndbcluster
  PARTITION BY KEY (t_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

CREATE INDEX ix_t_ca_id_dts ON trade (t_ca_id, t_dts);
CREATE INDEX ix_t_s_symb_dts ON trade (t_s_symb, t_dts);

-- ---------------------------------------------------------
--  2.2.6.7 LAST_TRADE (LT_)
-- ---------------------------------------------------------
ALTER TABLE last_trade
  DROP FOREIGN KEY fk_last_trade_s
-- ROP INDEX       fk_last_trade_s
;

ALTER TABLE last_trade
  ENGINE ndbcluster
  PARTITION BY KEY (lt_s_symb)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.4.7 HOLDING_SUMMARY (HS_)
-- ---------------------------------------------------------
ALTER TABLE holding_summary
  DROP FOREIGN KEY fk_holding_summary_ca
-- ROP INDEX       fk_holding_summary_ca
, DROP FOREIGN KEY fk_holding_summary_s
, DROP INDEX       fk_holding_summary_s
;

ALTER TABLE holding_summary
  ENGINE ndbcluster
  PARTITION BY KEY (hs_ca_id) -- != PK
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.6.3 DAILY_MARKET (DM_)
-- ---------------------------------------------------------
ALTER TABLE daily_market
  DROP FOREIGN KEY fk_daily_market_s
-- ROP INDEX       fk_daily_market_s
;

ALTER TABLE daily_market
  ENGINE ndbcluster
  PARTITION BY KEY (dm_s_symb, dm_date)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.4.1 ACCOUNT_PERMISSION (AP_)
-- ---------------------------------------------------------
ALTER TABLE account_permission
  DROP FOREIGN KEY fk_account_permission_ca
-- ROP INDEX       fk_account_permission_ca
;

ALTER TABLE account_permission
  ENGINE ndbcluster
  PARTITION BY KEY (ap_ca_id) -- != PK
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- == L4 ==

-- ---------------------------------------------------------
--  2.2.4.9 WATCH_LIST (WL_)
-- ---------------------------------------------------------
ALTER TABLE watch_list
  DROP FOREIGN KEY fk_watch_list_wl
, DROP INDEX       fk_watch_list_wl
;

ALTER TABLE watch_list
  ENGINE ndbcluster
  PARTITION BY KEY (wl_id) -- wl_c_id?, TODO
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- CREATE UNIQUE INDEX ui_watch_list
--   USING HASH
--   ON watch_list (wl_id)
-- ;

-- ---------------------------------------------------------
--  2.2.6.11 SECURITY (S_)
-- ---------------------------------------------------------
ALTER TABLE security
  DROP FOREIGN KEY fk_security_st
, DROP INDEX       fk_security_st
, DROP FOREIGN KEY fk_security_ex
, DROP INDEX       fk_security_ex
, DROP FOREIGN KEY fk_security_co
, DROP INDEX       fk_security_co
;

ALTER TABLE security
  ENGINE ndbcluster
  PARTITION BY KEY (s_symb)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

CREATE INDEX ix_s_co_id ON security (s_co_id);

-- ---------------------------------------------------------
--  2.2.6.5 FINANCIAL (FI_)
-- ---------------------------------------------------------
ALTER TABLE financial
  DROP FOREIGN KEY fk_financial_co
-- ROP INDEX       fk_financial_co
;

ALTER TABLE financial
  ENGINE ndbcluster
  PARTITION BY KEY (fi_co_id) -- != PK
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.4.4 CUSTOMER_TAXRATE (CX_)
-- ---------------------------------------------------------
ALTER TABLE customer_taxrate
  DROP FOREIGN KEY fk_customer_taxrate_tx
, DROP INDEX       fk_customer_taxrate_tx
, DROP FOREIGN KEY fk_customer_taxrate_c
-- ROP INDEX       fk_customer_taxrate_c
;

ALTER TABLE customer_taxrate
  ENGINE ndbcluster
  PARTITION BY KEY (cx_c_id) -- != PK
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.5.4 COMMISSION_RATE (CR_)
-- ---------------------------------------------------------
ALTER TABLE commission_rate
  DROP FOREIGN KEY fk_commission_rate_tt
, DROP INDEX       fk_commission_rate_tt
, DROP FOREIGN KEY fk_commission_rate_ex
, DROP INDEX       fk_commission_rate_ex
;

ALTER TABLE commission_rate
  ENGINE ndbcluster
  PARTITION BY KEY (cr_c_tier, cr_tt_id, cr_ex_id) -- != PK
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.6.2 COMPANY_COMPETITOR (CP_)
-- ---------------------------------------------------------
ALTER TABLE company_competitor
  DROP FOREIGN KEY fk_company_competitor_co1
-- ROP INDEX       fk_company_competitor_co1
, DROP FOREIGN KEY fk_company_competitor_co2
, DROP INDEX       fk_company_competitor_co2
, DROP FOREIGN KEY fk_company_competitor_in
, DROP INDEX       fk_company_competitor_in
;

ALTER TABLE company_competitor
  ENGINE ndbcluster
  PARTITION BY KEY (cp_co_id) -- != PK
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.4.3 CUSTOMER_ACCOUNT (CA_)
-- ---------------------------------------------------------
ALTER TABLE customer_account
  DROP FOREIGN KEY fk_customer_account_b
, DROP INDEX       fk_customer_account_b
, DROP FOREIGN KEY fk_customer_account_c
, DROP INDEX       fk_customer_account_c
;

ALTER TABLE customer_account
  ENGINE ndbcluster
  PARTITION BY KEY (ca_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

CREATE INDEX ix_ca_c_id ON customer_account (ca_c_id);

-- == L3 ==

-- ---------------------------------------------------------
--  2.2.6.9 NEWS_XREF (NX_)
-- ---------------------------------------------------------
ALTER TABLE news_xref
  DROP FOREIGN KEY fk_news_xref_ni
, DROP INDEX       fk_news_xref_ni
, DROP FOREIGN KEY fk_news_xref_co
-- ROP INDEX       fk_news_xref_co
;

ALTER TABLE news_xref
  ENGINE ndbcluster
  PARTITION BY KEY (nx_co_id) -- != PK
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.6.4 EXCHANGE (EX_)
-- ---------------------------------------------------------
ALTER TABLE exchange
  DROP FOREIGN KEY fk_exchange_ad
, DROP INDEX       fk_exchange_ad
;

ALTER TABLE exchange
  ENGINE ndbcluster -- Memory?
  PARTITION BY KEY (ex_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.6.1 COMPANY (CO_)
-- ---------------------------------------------------------
ALTER TABLE company
  DROP FOREIGN KEY fk_company_st
, DROP INDEX       fk_company_st
, DROP FOREIGN KEY fk_company_in
, DROP INDEX       fk_company_in
, DROP FOREIGN KEY fk_company_ad
, DROP INDEX       fk_company_ad
;

ALTER TABLE company
  ENGINE ndbcluster
  PARTITION BY KEY (co_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

CREATE INDEX ix_co_name ON company (co_name);
CREATE INDEX ix_co_in_id ON company (co_in_id);

-- ---------------------------------------------------------
--  2.2.4.2 CUSTOMER (C_)
-- ---------------------------------------------------------
ALTER TABLE customer
  DROP FOREIGN KEY fk_customer_st
, DROP INDEX       fk_customer_st
, DROP FOREIGN KEY fk_customer_ad
, DROP INDEX       fk_customer_ad
;

ALTER TABLE customer
  ENGINE ndbcluster
  PARTITION BY KEY (c_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

CREATE INDEX ix_c_tax_id ON customer (c_tax_id);

-- ---------------------------------------------------------
--  2.2.5.1 BROKER (B_)
-- ---------------------------------------------------------
ALTER TABLE broker
  DROP FOREIGN KEY fk_broker_st
, DROP INDEX       fk_broker_st
;

ALTER TABLE broker
  ENGINE ndbcluster
  PARTITION BY KEY (b_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

CREATE INDEX ix_b_name ON broker (b_name);

-- == L2 ==

-- ---------------------------------------------------------
--  2.2.6.6 INDUSTRY (IN_)
-- ---------------------------------------------------------
ALTER TABLE industry
  DROP FOREIGN KEY fk_industry_sc
, DROP INDEX       fk_industry_sc
;

ALTER TABLE industry
  ENGINE ndbcluster
  PARTITION BY KEY (in_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

CREATE INDEX ix_in_name ON industry (in_name);
CREATE INDEX ix_in_sc_id ON industry (in_sc_id);

-- ---------------------------------------------------------
--  2.2.5.3 CHARGE (CH_)
-- ---------------------------------------------------------
ALTER TABLE charge
  DROP FOREIGN KEY fk_charge_tt
-- ROP INDEX       fk_charge_tt
;

ALTER TABLE charge
  ENGINE ndbcluster
  PARTITION BY KEY (ch_tt_id, ch_c_tier)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.7.1 ADDRESS (AD_)
-- ---------------------------------------------------------
ALTER TABLE address
  DROP FOREIGN KEY fk_address_zc
, DROP INDEX       fk_address_zc
;

ALTER TABLE address
  ENGINE ndbcluster
  PARTITION BY KEY (ad_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- == L1 ==

-- ---------------------------------------------------------
--  2.2.7.4 ZIP_CODE (ZC_)
-- ---------------------------------------------------------
ALTER TABLE zip_code
  ENGINE ndbcluster
  PARTITION BY KEY (zc_code)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.7.3 TAXRATE (TX_)
-- ---------------------------------------------------------
ALTER TABLE taxrate
  ENGINE ndbcluster
  PARTITION BY KEY (tx_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.5.9 TRADE_TYPE (TT_)
-- ---------------------------------------------------------
ALTER TABLE trade_type
  ENGINE ndbcluster -- Memory?
  PARTITION BY KEY (tt_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.7.2 STATUS_TYPE (ST_)
-- ---------------------------------------------------------
ALTER TABLE status_type
  ENGINE ndbcluster
  PARTITION BY KEY (st_id) -- Memory?
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- ---------------------------------------------------------
--  2.2.6.10 SECTOR (SC_)
-- ---------------------------------------------------------
ALTER TABLE sector
  ENGINE ndbcluster
  PARTITION BY KEY (sc_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

CREATE INDEX ix_sc_name ON sector (sc_name);

-- ---------------------------------------------------------
--  2.2.6.8 NEWS_ITEM (NI_)
-- ---------------------------------------------------------
ALTER TABLE news_item
  ENGINE ndbcluster
  PARTITION BY KEY (ni_id)
  PARTITIONS 3
  (
    PARTITION p0 NODEGROUP 0
  , PARTITION p1 NODEGROUP 0
  , PARTITION p2 NODEGROUP 0
  )
;

-- == ==

SET @@ndb_use_copying_alter_table = @tmp_ndb_use_copying_alter_table;
SELECT @@ndb_use_copying_alter_table;

SET @@ndb_table_no_logging = @tmp_ndb_table_no_logging;
SELECT @@ndb_table_no_logging;
