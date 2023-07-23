ALTER TABLE IF EXISTS refundcheck.purchase
    ADD COLUMN amount numeric;

ALTER TABLE IF EXISTS refundcheck.refund
    ADD COLUMN customer_id integer;

