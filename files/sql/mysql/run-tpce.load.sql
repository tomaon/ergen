--
-- TODO : ln -s $EGEN_HOME/flat_out
--

SELECT @@sql_log_bin INTO @x_sql_log_bin;
SET sql_log_bin = 0;

-- == L1 ==

--  2.2.6.8 NEWS_ITEM (NI_)
LOAD DATA LOCAL INFILE "./flat_out/NewsItem.txt"
     INTO TABLE news_item FIELDS TERMINATED BY "|";
--  2.2.6.10 SECTOR (SC_)
LOAD DATA LOCAL INFILE "./flat_out/Sector.txt"
     INTO TABLE sector FIELDS TERMINATED BY "|";
--  2.2.7.2 STATUS_TYPE (ST_)
LOAD DATA LOCAL INFILE "./flat_out/StatusType.txt"
     INTO TABLE status_type FIELDS TERMINATED BY "|";
--  2.2.5.9 TRADE_TYPE (TT_)
LOAD DATA LOCAL INFILE "./flat_out/TradeType.txt"
     INTO TABLE trade_type FIELDS TERMINATED BY "|";
--  2.2.7.3 TAXRATE (TX_)
LOAD DATA LOCAL INFILE "./flat_out/Taxrate.txt"
     INTO TABLE taxrate FIELDS TERMINATED BY "|";
--  2.2.7.4 ZIP_CODE (ZC_)
LOAD DATA LOCAL INFILE "./flat_out/ZipCode.txt"
     INTO TABLE zip_code FIELDS TERMINATED BY "|";

-- == L2 ==

--  2.2.7.1 ADDRESS (AD_)
LOAD DATA LOCAL INFILE "./flat_out/Address.txt"
     INTO TABLE address FIELDS TERMINATED BY "|";
--  2.2.5.3 CHARGE (CH_)
LOAD DATA LOCAL INFILE "./flat_out/Charge.txt"
     INTO TABLE charge FIELDS TERMINATED BY "|";
--  2.2.6.6 INDUSTRY (IN_)
LOAD DATA LOCAL INFILE "./flat_out/Industry.txt"
     INTO TABLE industry FIELDS TERMINATED BY "|";

-- == L3 ==

--  2.2.5.1 BROKER (B_)
LOAD DATA LOCAL INFILE "./flat_out/Broker.txt"
     INTO TABLE broker FIELDS TERMINATED BY "|";
--  2.2.4.2 CUSTOMER (C_)
LOAD DATA LOCAL INFILE "./flat_out/Customer.txt"
     INTO TABLE customer FIELDS TERMINATED BY "|";
--  2.2.6.1 COMPANY (CO_)
LOAD DATA LOCAL INFILE "./flat_out/Company.txt"
     INTO TABLE company FIELDS TERMINATED BY "|";
--  2.2.6.4 EXCHANGE (EX_)
LOAD DATA LOCAL INFILE "./flat_out/Exchange.txt"
     INTO TABLE exchange FIELDS TERMINATED BY "|";
--  2.2.6.9 NEWS_XREF (NX_)
LOAD DATA LOCAL INFILE "./flat_out/NewsXRef.txt"
     INTO TABLE news_xref FIELDS TERMINATED BY "|";

-- == L4 ==

--  2.2.4.3 CUSTOMER_ACCOUNT (CA_)
LOAD DATA LOCAL INFILE "./flat_out/CustomerAccount.txt"
     INTO TABLE customer_account FIELDS TERMINATED BY "|";
--  2.2.6.2 COMPANY_COMPETITOR (CP_)
LOAD DATA LOCAL INFILE "./flat_out/CompanyCompetitor.txt"
     INTO TABLE company_competitor FIELDS TERMINATED BY "|";
--  2.2.5.4 COMMISSION_RATE (CR_)
LOAD DATA LOCAL INFILE "./flat_out/CommissionRate.txt"
     INTO TABLE commission_rate FIELDS TERMINATED BY "|";
--  2.2.4.4 CUSTOMER_TAXRATE (CX_)
LOAD DATA LOCAL INFILE "./flat_out/CustomerTaxrate.txt"
     INTO TABLE customer_taxrate FIELDS TERMINATED BY "|";
--  2.2.6.5 FINANCIAL (FI_)
LOAD DATA LOCAL INFILE "./flat_out/Financial.txt"
     INTO TABLE financial FIELDS TERMINATED BY "|";
--  2.2.6.11 SECURITY (S_)
LOAD DATA LOCAL INFILE "./flat_out/Security.txt"
     INTO TABLE security FIELDS TERMINATED BY "|";
--  2.2.4.9 WATCH_LIST (WL_)
LOAD DATA LOCAL INFILE "./flat_out/WatchList.txt"
     INTO TABLE watch_list FIELDS TERMINATED BY "|";

-- == L5 ==

--  2.2.4.1 ACCOUNT_PERMISSION (AP_)
LOAD DATA LOCAL INFILE "./flat_out/AccountPermission.txt"
     INTO TABLE account_permission FIELDS TERMINATED BY "|";
--  2.2.6.3 DAILY_MARKET (DM_)
LOAD DATA LOCAL INFILE "./flat_out/DailyMarket.txt"
     INTO TABLE daily_market FIELDS TERMINATED BY "|";
--  2.2.4.7 HOLDING_SUMMARY (HS_)
LOAD DATA LOCAL INFILE "./flat_out/HoldingSummary.txt"
     INTO TABLE holding_summary FIELDS TERMINATED BY "|";
--  2.2.6.7 LAST_TRADE (LT_)
LOAD DATA LOCAL INFILE "./flat_out/LastTrade.txt"
     INTO TABLE last_trade FIELDS TERMINATED BY "|";
--  2.2.5.6 TRADE (T_)
LOAD DATA LOCAL INFILE "./flat_out/Trade.txt"
     INTO TABLE trade FIELDS TERMINATED BY "|";
--  2.2.4.8 WATCH_ITEM (WI_)
LOAD DATA LOCAL INFILE "./flat_out/WatchItem.txt"
     INTO TABLE watch_item FIELDS TERMINATED BY "|";

-- == L6 ==

--  2.2.5.2 CASH_TRANSACTION (CT_)
LOAD DATA LOCAL INFILE "./flat_out/CashTransaction.txt"
     INTO TABLE cash_transaction FIELDS TERMINATED BY "|";
--  2.2.4.5 HOLDING (H_)
LOAD DATA LOCAL INFILE "./flat_out/Holding.txt"
     INTO TABLE holding FIELDS TERMINATED BY "|";
--  2.2.4.6 HOLDING_HISTORY (HH_)
LOAD DATA LOCAL INFILE "./flat_out/HoldingHistory.txt"
     INTO TABLE holding_history FIELDS TERMINATED BY "|";
--  2.2.5.5 SETTLEMENT (SE_)
LOAD DATA LOCAL INFILE "./flat_out/Settlement.txt"
     INTO TABLE settlement FIELDS TERMINATED BY "|";
--  2.2.5.7 TRADE_HISTORY (TH_)
LOAD DATA LOCAL INFILE "./flat_out/TradeHistory.txt"
     INTO TABLE trade_history FIELDS TERMINATED BY "|";
--  2.2.5.8 TRADE_REQUEST (TR_)
-- LOAD DATA LOCAL INFILE "./flat_out/TradeRequest.txt"
--      INTO TABLE trade_request FIELDS TERMINATED BY "|";

COMMIT;

SET sql_log_bin = @x_sql_log_bin;
