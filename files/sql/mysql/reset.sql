
DELETE FROM cash_transaction WHERE ct_t_id < 100000
;
DELETE FROM holding_history WHERE hh_t_id < 100000
;
DELETE FROM holding WHERE h_t_id < 100000
;
DELETE FROM settlement WHERE se_t_id < 100000
;
DELETE FROM trade_request WHERE tr_t_id < 100000
;
DELETE FROM trade_history WHERE th_t_id < 100000
;
DELETE FROM trade WHERE t_id < 100000
;

UPDATE sequence SET id = 0 WHERE name = 'trade'
;

COMMIT
;

SET TRANSACTION ISOLATION LEVEL READ COMMITTED
;

SELECT * FROM sequence
;
