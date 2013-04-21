--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

SET search_path = kb, pg_catalog;

--
-- Data for Name: person; Type: TABLE DATA; Schema: kb; Owner: karl
--

COPY person (id, name, password, email) FROM stdin;
1	karl	$2a$06$Mob7eMJzEYZBtHpByIgi3e.Mialg13e2.lygmwfkuOFditUdqYsXq	ktvoelker@gmail.com
\.


--
-- Name: person_id_seq; Type: SEQUENCE SET; Schema: kb; Owner: karl
--

SELECT pg_catalog.setval('person_id_seq', 1, true);


--
-- PostgreSQL database dump complete
--

