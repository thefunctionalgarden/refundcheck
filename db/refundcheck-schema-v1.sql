--
-- PostgreSQL database dump
--

-- Dumped from database version 14.7 (Ubuntu 14.7-0ubuntu0.22.04.1)
-- Dumped by pg_dump version 14.7 (Ubuntu 14.7-0ubuntu0.22.04.1)

-- Started on 2023-05-18 18:09:39 -03

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 6 (class 2615 OID 16404)
-- Name: refundcheck; Type: SCHEMA; Schema: -; Owner: cam
--

CREATE SCHEMA refundcheck;


ALTER SCHEMA refundcheck OWNER TO cam;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- TOC entry 213 (class 1259 OID 16433)
-- Name: customer; Type: TABLE; Schema: refundcheck; Owner: cam
--

CREATE TABLE refundcheck.customer (
    id integer NOT NULL,
    mail character varying(320) NOT NULL,
    date_created date DEFAULT CURRENT_DATE NOT NULL
);


ALTER TABLE refundcheck.customer OWNER TO cam;

--
-- TOC entry 212 (class 1259 OID 16432)
-- Name: Customer_id_seq; Type: SEQUENCE; Schema: refundcheck; Owner: cam
--

ALTER TABLE refundcheck.customer ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME refundcheck."Customer_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 1000000000
    CACHE 1
);


--
-- TOC entry 218 (class 1259 OID 16474)
-- Name: product_type; Type: TABLE; Schema: refundcheck; Owner: cam
--

CREATE TABLE refundcheck.product_type (
    id smallint NOT NULL,
    description character varying(100)
);


ALTER TABLE refundcheck.product_type OWNER TO cam;

--
-- TOC entry 217 (class 1259 OID 16473)
-- Name: Product_Type_id_seq; Type: SEQUENCE; Schema: refundcheck; Owner: cam
--

ALTER TABLE refundcheck.product_type ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME refundcheck."Product_Type_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- TOC entry 220 (class 1259 OID 16480)
-- Name: refund_type; Type: TABLE; Schema: refundcheck; Owner: cam
--

CREATE TABLE refundcheck.refund_type (
    id smallint NOT NULL,
    description character varying(100)
);


ALTER TABLE refundcheck.refund_type OWNER TO cam;

--
-- TOC entry 219 (class 1259 OID 16479)
-- Name: Refund_Type_id_seq; Type: SEQUENCE; Schema: refundcheck; Owner: cam
--

ALTER TABLE refundcheck.refund_type ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME refundcheck."Refund_Type_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- TOC entry 211 (class 1259 OID 16425)
-- Name: seller; Type: TABLE; Schema: refundcheck; Owner: cam
--

CREATE TABLE refundcheck.seller (
    id integer NOT NULL,
    name character varying(100) NOT NULL,
    mail character varying(320) NOT NULL,
    pass character varying(100) NOT NULL,
    date_created date DEFAULT CURRENT_DATE NOT NULL
);


ALTER TABLE refundcheck.seller OWNER TO cam;

--
-- TOC entry 210 (class 1259 OID 16424)
-- Name: Seller_id_seq; Type: SEQUENCE; Schema: refundcheck; Owner: cam
--

ALTER TABLE refundcheck.seller ALTER COLUMN id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME refundcheck."Seller_id_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 1000000
    CACHE 1
);


--
-- TOC entry 216 (class 1259 OID 16468)
-- Name: amount_range; Type: TABLE; Schema: refundcheck; Owner: cam
--

CREATE TABLE refundcheck.amount_range (
    description character varying(100),
    min_val integer,
    max_val integer,
    id character(1) NOT NULL
);


ALTER TABLE refundcheck.amount_range OWNER TO cam;

--
-- TOC entry 214 (class 1259 OID 16439)
-- Name: purchase; Type: TABLE; Schema: refundcheck; Owner: cam
--

CREATE TABLE refundcheck.purchase (
    seller_id integer NOT NULL,
    customer_id integer NOT NULL,
    product_type_id smallint NOT NULL,
    date date DEFAULT CURRENT_DATE NOT NULL,
    amount_range_id character(1),
    amount_currency character(3),
    transaction_id character varying NOT NULL
);


ALTER TABLE refundcheck.purchase OWNER TO cam;

--
-- TOC entry 215 (class 1259 OID 16455)
-- Name: refund; Type: TABLE; Schema: refundcheck; Owner: cam
--

CREATE TABLE refundcheck.refund (
    date date DEFAULT CURRENT_DATE NOT NULL,
    description character varying(2000),
    type_id smallint NOT NULL,
    seller_id integer NOT NULL,
    purchase_transaction_id character varying NOT NULL
);


ALTER TABLE refundcheck.refund OWNER TO cam;

--
-- TOC entry 3403 (class 0 OID 16468)
-- Dependencies: 216
-- Data for Name: amount_range; Type: TABLE DATA; Schema: refundcheck; Owner: cam
--

COPY refundcheck.amount_range (description, min_val, max_val, id) FROM stdin;
0-5	0	5	A
5-10	5	10	B
10-25	10	25	C
25-100	25	100	D
100+	100	1000000	E
\.


--
-- TOC entry 3400 (class 0 OID 16433)
-- Dependencies: 213
-- Data for Name: customer; Type: TABLE DATA; Schema: refundcheck; Owner: cam
--

COPY refundcheck.customer (id, mail, date_created) FROM stdin;
1	customer.01@somemail.com	2023-05-15
2	customer.02@somemail.com	2023-05-15
\.


--
-- TOC entry 3405 (class 0 OID 16474)
-- Dependencies: 218
-- Data for Name: product_type; Type: TABLE DATA; Schema: refundcheck; Owner: cam
--

COPY refundcheck.product_type (id, description) FROM stdin;
1	Service
2	Info Product
3	Physical Product
\.


--
-- TOC entry 3401 (class 0 OID 16439)
-- Dependencies: 214
-- Data for Name: purchase; Type: TABLE DATA; Schema: refundcheck; Owner: cam
--

COPY refundcheck.purchase (seller_id, customer_id, product_type_id, date, amount_range_id, amount_currency, transaction_id) FROM stdin;
1	1	1	2023-05-15	C	USD	t-1
\.


--
-- TOC entry 3402 (class 0 OID 16455)
-- Dependencies: 215
-- Data for Name: refund; Type: TABLE DATA; Schema: refundcheck; Owner: cam
--

COPY refundcheck.refund (date, description, type_id, seller_id, purchase_transaction_id) FROM stdin;
2023-05-17	a descr of the refund	1	1	t-1
2023-05-17	a descr of the refund	1	1	t-1
2023-05-17	a descr of the refund	1	1	t-1
\.


--
-- TOC entry 3407 (class 0 OID 16480)
-- Dependencies: 220
-- Data for Name: refund_type; Type: TABLE DATA; Schema: refundcheck; Owner: cam
--

COPY refundcheck.refund_type (id, description) FROM stdin;
1	Total refund
2	Partial refund
\.


--
-- TOC entry 3398 (class 0 OID 16425)
-- Dependencies: 211
-- Data for Name: seller; Type: TABLE DATA; Schema: refundcheck; Owner: cam
--

COPY refundcheck.seller (id, name, mail, pass, date_created) FROM stdin;
1	John Doe	john.doe@johndeer.com	pass1234	2023-05-15
2	John Wan	seller.01@somemail.com	pass1234	2023-05-15
3	John Doe	seller.02@somemail.com	pass1234	2023-05-15
\.


--
-- TOC entry 3413 (class 0 OID 0)
-- Dependencies: 212
-- Name: Customer_id_seq; Type: SEQUENCE SET; Schema: refundcheck; Owner: cam
--

SELECT pg_catalog.setval('refundcheck."Customer_id_seq"', 11, true);


--
-- TOC entry 3414 (class 0 OID 0)
-- Dependencies: 217
-- Name: Product_Type_id_seq; Type: SEQUENCE SET; Schema: refundcheck; Owner: cam
--

SELECT pg_catalog.setval('refundcheck."Product_Type_id_seq"', 3, true);


--
-- TOC entry 3415 (class 0 OID 0)
-- Dependencies: 219
-- Name: Refund_Type_id_seq; Type: SEQUENCE SET; Schema: refundcheck; Owner: cam
--

SELECT pg_catalog.setval('refundcheck."Refund_Type_id_seq"', 2, true);


--
-- TOC entry 3416 (class 0 OID 0)
-- Dependencies: 210
-- Name: Seller_id_seq; Type: SEQUENCE SET; Schema: refundcheck; Owner: cam
--

SELECT pg_catalog.setval('refundcheck."Seller_id_seq"', 3, true);


--
-- TOC entry 3248 (class 2606 OID 16506)
-- Name: amount_range Amount_Range_pkey; Type: CONSTRAINT; Schema: refundcheck; Owner: cam
--

ALTER TABLE ONLY refundcheck.amount_range
    ADD CONSTRAINT "Amount_Range_pkey" PRIMARY KEY (id);


--
-- TOC entry 3242 (class 2606 OID 16501)
-- Name: customer Customer_UQ_mail; Type: CONSTRAINT; Schema: refundcheck; Owner: cam
--

ALTER TABLE ONLY refundcheck.customer
    ADD CONSTRAINT "Customer_UQ_mail" UNIQUE (mail);


--
-- TOC entry 3244 (class 2606 OID 16437)
-- Name: customer Customer_pkey; Type: CONSTRAINT; Schema: refundcheck; Owner: cam
--

ALTER TABLE ONLY refundcheck.customer
    ADD CONSTRAINT "Customer_pkey" PRIMARY KEY (id);


--
-- TOC entry 3250 (class 2606 OID 16478)
-- Name: product_type Product_Type_pkey; Type: CONSTRAINT; Schema: refundcheck; Owner: cam
--

ALTER TABLE ONLY refundcheck.product_type
    ADD CONSTRAINT "Product_Type_pkey" PRIMARY KEY (id);


--
-- TOC entry 3246 (class 2606 OID 16516)
-- Name: purchase Purchase_pkey; Type: CONSTRAINT; Schema: refundcheck; Owner: cam
--

ALTER TABLE ONLY refundcheck.purchase
    ADD CONSTRAINT "Purchase_pkey" PRIMARY KEY (seller_id, transaction_id);


--
-- TOC entry 3252 (class 2606 OID 16494)
-- Name: refund_type Refund_Type_pkey; Type: CONSTRAINT; Schema: refundcheck; Owner: cam
--

ALTER TABLE ONLY refundcheck.refund_type
    ADD CONSTRAINT "Refund_Type_pkey" PRIMARY KEY (id);


--
-- TOC entry 3240 (class 2606 OID 16431)
-- Name: seller Seller_pkey; Type: CONSTRAINT; Schema: refundcheck; Owner: cam
--

ALTER TABLE ONLY refundcheck.seller
    ADD CONSTRAINT "Seller_pkey" PRIMARY KEY (id);


--
-- TOC entry 3256 (class 2606 OID 16507)
-- Name: purchase Purchase_FK_Amount_Range; Type: FK CONSTRAINT; Schema: refundcheck; Owner: cam
--

ALTER TABLE ONLY refundcheck.purchase
    ADD CONSTRAINT "Purchase_FK_Amount_Range" FOREIGN KEY (amount_range_id) REFERENCES refundcheck.amount_range(id) NOT VALID;


--
-- TOC entry 3254 (class 2606 OID 16449)
-- Name: purchase Purchase_FK_Customer; Type: FK CONSTRAINT; Schema: refundcheck; Owner: cam
--

ALTER TABLE ONLY refundcheck.purchase
    ADD CONSTRAINT "Purchase_FK_Customer" FOREIGN KEY (customer_id) REFERENCES refundcheck.customer(id) NOT VALID;


--
-- TOC entry 3255 (class 2606 OID 16488)
-- Name: purchase Purchase_FK_Product_Type; Type: FK CONSTRAINT; Schema: refundcheck; Owner: cam
--

ALTER TABLE ONLY refundcheck.purchase
    ADD CONSTRAINT "Purchase_FK_Product_Type" FOREIGN KEY (product_type_id) REFERENCES refundcheck.product_type(id) NOT VALID;


--
-- TOC entry 3253 (class 2606 OID 16444)
-- Name: purchase Purchase_FK_Seller; Type: FK CONSTRAINT; Schema: refundcheck; Owner: cam
--

ALTER TABLE ONLY refundcheck.purchase
    ADD CONSTRAINT "Purchase_FK_Seller" FOREIGN KEY (seller_id) REFERENCES refundcheck.seller(id) NOT VALID;


--
-- TOC entry 3257 (class 2606 OID 16495)
-- Name: refund Refund_FK_Refund_Type; Type: FK CONSTRAINT; Schema: refundcheck; Owner: cam
--

ALTER TABLE ONLY refundcheck.refund
    ADD CONSTRAINT "Refund_FK_Refund_Type" FOREIGN KEY (type_id) REFERENCES refundcheck.refund_type(id) NOT VALID;


-- Completed on 2023-05-18 18:09:39 -03

--
-- PostgreSQL database dump complete
--

