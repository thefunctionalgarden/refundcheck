ALTER TABLE IF EXISTS refundcheck.payment
    ADD COLUMN "data" character varying(10000);
    
ALTER TABLE IF EXISTS refundcheck.payment
    ADD COLUMN trx_id character varying(200);

ALTER TABLE IF EXISTS refundcheck.payment
    ADD COLUMN status character varying(100);

ALTER TABLE IF EXISTS refundcheck.payment
    ALTER COLUMN date SET DEFAULT CURRENT_DATE;

    