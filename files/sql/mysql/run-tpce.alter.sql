SELECT @@ndb_table_no_logging INTO @x_ndb_table_no_logging;
SET @@ndb_table_no_logging = 1;
SELECT @@ndb_table_no_logging;

SELECT @@ndb_use_copying_alter_table INTO @x_ndb_use_copying_alter_table;
SET @@ndb_use_copying_alter_table = 1;
SELECT @@ndb_use_copying_alter_table;

-- == L6 ==

-- ---------------------------------------------------------
--  2.2.5.8 TRADE_REQUEST (TR_)
-- ---------------------------------------------------------
ALTER TABLE trade_request
  DROP FOREIGN KEY fk_trade_request_t
;
ALTER TABLE trade_request
  DROP FOREIGN KEY fk_trade_request_tt
;
ALTER TABLE trade_request
  DROP FOREIGN KEY fk_trade_request_s
;
ALTER TABLE trade_request
  DROP FOREIGN KEY fk_trade_request_b
;

ALTER TABLE trade_request ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.5.7 TRADE_HISTORY (TH_)
-- ---------------------------------------------------------
ALTER TABLE trade_history
  DROP FOREIGN KEY fk_trade_history_t
;
ALTER TABLE trade_history
  DROP FOREIGN KEY fk_trade_history_st
;

ALTER TABLE trade_history ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.5.5 SETTLEMENT (SE_)
-- ---------------------------------------------------------
ALTER TABLE settlement
  DROP FOREIGN KEY fk_settlement_t
;

ALTER TABLE settlement ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.4.6 HOLDING_HISTORY (HH_)
-- ---------------------------------------------------------
ALTER TABLE holding_history
  DROP FOREIGN KEY fk_holding_history_t1
;
ALTER TABLE holding_history
  DROP FOREIGN KEY fk_holding_history_t2
;

ALTER TABLE holding_history ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.4.5 HOLDING (H_)
-- ---------------------------------------------------------
ALTER TABLE holding
  DROP FOREIGN KEY fk_holding_t
;
ALTER TABLE holding
  DROP FOREIGN KEY fk_holding_hs
;

ALTER TABLE holding ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.5.2 CASH_TRANSACTION (CT_)
-- ---------------------------------------------------------
ALTER TABLE cash_transaction
  DROP FOREIGN KEY fk_cash_transaction_t
;

ALTER TABLE cash_transaction ENGINE = ndbcluster
;

-- == L5 ==

-- ---------------------------------------------------------
--  2.2.4.8 WATCH_ITEM (WI_)
-- ---------------------------------------------------------
ALTER TABLE watch_item
  DROP FOREIGN KEY fk_watch_item_wl
;
ALTER TABLE watch_item
  DROP FOREIGN KEY fk_watch_item_s
;

ALTER TABLE watch_item ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.5.6 TRADE (T_)
-- ---------------------------------------------------------
ALTER TABLE trade
  DROP FOREIGN KEY fk_trade_st
;
ALTER TABLE trade
  DROP FOREIGN KEY fk_trade_tt
;
ALTER TABLE trade
  DROP FOREIGN KEY fk_trade_s
;
ALTER TABLE trade
  DROP FOREIGN KEY fk_trade_ca
;

ALTER TABLE trade ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.6.7 LAST_TRADE (LT_)
-- ---------------------------------------------------------
ALTER TABLE last_trade
  DROP FOREIGN KEY fk_last_trade_s
;

ALTER TABLE last_trade ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.4.7 HOLDING_SUMMARY (HS_)
-- ---------------------------------------------------------
ALTER TABLE holding_summary
  DROP FOREIGN KEY fk_holding_summary_ca
;
ALTER TABLE holding_summary
  DROP FOREIGN KEY fk_holding_summary_s
;

ALTER TABLE holding_summary ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.6.3 DAILY_MARKET (DM_)
-- ---------------------------------------------------------
ALTER TABLE daily_market
  DROP FOREIGN KEY fk_daily_market_s
;

ALTER TABLE daily_market ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.4.1 ACCOUNT_PERMISSION (AP_)
-- ---------------------------------------------------------
ALTER TABLE account_permission
  DROP FOREIGN KEY fk_account_permission_ca
;

ALTER TABLE account_permission ENGINE = ndbcluster
;

-- == L4 ==

-- ---------------------------------------------------------
--  2.2.4.9 WATCH_LIST (WL_)
-- ---------------------------------------------------------
ALTER TABLE watch_list
  DROP FOREIGN KEY fk_watch_list_wl
;

ALTER TABLE watch_list ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.6.11 SECURITY (S_)
-- ---------------------------------------------------------
ALTER TABLE security
  DROP FOREIGN KEY fk_security_st
;
ALTER TABLE security
  DROP FOREIGN KEY fk_security_ex
;
ALTER TABLE security
  DROP FOREIGN KEY fk_security_co
;

ALTER TABLE security ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.6.5 FINANCIAL (FI_)
-- ---------------------------------------------------------
ALTER TABLE financial
  DROP FOREIGN KEY fk_financial_co
;

ALTER TABLE financial ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.4.4 CUSTOMER_TAXRATE (CX_)
-- ---------------------------------------------------------
ALTER TABLE customer_taxrate
  DROP FOREIGN KEY fk_customer_taxrate_tx
;
ALTER TABLE customer_taxrate
  DROP FOREIGN KEY fk_customer_taxrate_c
;

ALTER TABLE customer_taxrate ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.5.4 COMMISSION_RATE (CR_)
-- ---------------------------------------------------------
ALTER TABLE commission_rate
  DROP FOREIGN KEY fk_commission_rate_tt
;
ALTER TABLE commission_rate
  DROP FOREIGN KEY fk_commission_rate_ex
;

ALTER TABLE commission_rate ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.6.2 COMPANY_COMPETITOR (CP_)
-- ---------------------------------------------------------
ALTER TABLE company_competitor
  DROP FOREIGN KEY fk_company_competitor_co1
;
ALTER TABLE company_competitor
  DROP FOREIGN KEY fk_company_competitor_co2
;
ALTER TABLE company_competitor
  DROP FOREIGN KEY fk_company_competitor_in
;

ALTER TABLE company_competitor ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.4.3 CUSTOMER_ACCOUNT (CA_)
-- ---------------------------------------------------------
ALTER TABLE customer_account
  DROP FOREIGN KEY fk_customer_account_b
;
ALTER TABLE customer_account
  DROP FOREIGN KEY fk_customer_account_c
;

ALTER TABLE customer_account ENGINE = ndbcluster
;

-- == L3 ==

-- ---------------------------------------------------------
--  2.2.6.9 NEWS_XREF (NX_)
-- ---------------------------------------------------------
ALTER TABLE news_xref
  DROP FOREIGN KEY fk_news_xref_ni
;
ALTER TABLE news_xref
  DROP FOREIGN KEY fk_news_xref_co
;

ALTER TABLE news_xref ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.6.4 EXCHANGE (EX_)
-- ---------------------------------------------------------
ALTER TABLE exchange
  DROP FOREIGN KEY fk_exchange_ad
;

ALTER TABLE exchange ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.6.1 COMPANY (CO_)
-- ---------------------------------------------------------
ALTER TABLE company
  DROP FOREIGN KEY fk_company_st
;
ALTER TABLE company
  DROP FOREIGN KEY fk_company_in
;
ALTER TABLE company
  DROP FOREIGN KEY fk_company_ad
;

ALTER TABLE company ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.4.2 CUSTOMER (C_)
-- ---------------------------------------------------------
ALTER TABLE customer
  DROP FOREIGN KEY fk_customer_st
;
ALTER TABLE customer
  DROP FOREIGN KEY fk_customer_ad
;

ALTER TABLE customer ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.5.1 BROKER (B_)
-- ---------------------------------------------------------
ALTER TABLE broker
  DROP FOREIGN KEY fk_broker_st
;

ALTER TABLE broker ENGINE = ndbcluster
;

-- == L2 ==

-- ---------------------------------------------------------
--  2.2.6.6 INDUSTRY (IN_)
-- ---------------------------------------------------------
ALTER TABLE industry
  DROP FOREIGN KEY fk_industry_sc
;

ALTER TABLE industry ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.5.3 CHARGE (CH_)
-- ---------------------------------------------------------
ALTER TABLE charge
  DROP FOREIGN KEY fk_charge_tt
;

ALTER TABLE charge ENGINE = ndbcluster ;

-- ---------------------------------------------------------
--  2.2.7.1 ADDRESS (AD_)
-- ---------------------------------------------------------
ALTER TABLE address
  DROP FOREIGN KEY fk_address_zc
;

ALTER TABLE address ENGINE = ndbcluster
;

-- == L1 ==

-- ---------------------------------------------------------
--  2.2.7.4 ZIP_CODE (ZC_)
-- ---------------------------------------------------------
ALTER TABLE zip_code ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.7.3 TAXRATE (TX_)
-- ---------------------------------------------------------
ALTER TABLE taxrate ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.5.9 TRADE_TYPE (TT_)
-- ---------------------------------------------------------
ALTER TABLE trade_type ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.7.2 STATUS_TYPE (ST_)
-- ---------------------------------------------------------
ALTER TABLE status_type ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.6.10 SECTOR (SC_)
-- ---------------------------------------------------------
ALTER TABLE sector ENGINE = ndbcluster
;

-- ---------------------------------------------------------
--  2.2.6.8 NEWS_ITEM (NI_)
-- ---------------------------------------------------------
ALTER TABLE news_item ENGINE = ndbcluster
;

SET @@ndb_use_copying_alter_table = @x_ndb_use_copying_alter_table;
SELECT @@ndb_use_copying_alter_table;

SET @@ndb_table_no_logging = @x_ndb_table_no_logging;
SELECT @@ndb_table_no_logging;
