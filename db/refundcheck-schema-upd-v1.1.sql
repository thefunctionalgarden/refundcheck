


ALTER TABLE IF EXISTS refundcheck.seller
    RENAME pass TO key;

ALTER TABLE refundcheck.seller
    ALTER COLUMN key TYPE character varying(500) COLLATE pg_catalog."default";

ALTER TABLE IF EXISTS refundcheck.seller
    ALTER COLUMN date_created DROP NOT NULL;

ALTER TABLE IF EXISTS refundcheck.seller
    ADD COLUMN key_expires_in integer;

ALTER TABLE refundcheck.seller
    ALTER COLUMN name TYPE character varying(500) COLLATE pg_catalog."default";

ALTER TABLE IF EXISTS refundcheck.seller
    ADD COLUMN available_calls integer DEFAULT 20;

ALTER TABLE IF EXISTS refundcheck.seller
    ADD COLUMN url character varying(500);


CREATE TABLE refundcheck.payment
(
    date timestamp with time zone,
    seller_id integer,
    amount integer,
    concept character varying(100),
    items integer,
    best_before timestamp with time zone
);

ALTER TABLE IF EXISTS refundcheck.payment
    OWNER to refundcheck_u;


CREATE TABLE refundcheck.usage
(
    ts timestamp with time zone,
    seller_id integer,
    interface character varying(1),
    op character varying(20),
    path character varying(100),
    params character varying(1000)
);

ALTER TABLE IF EXISTS refundcheck.usage
    OWNER to refundcheck_u;

