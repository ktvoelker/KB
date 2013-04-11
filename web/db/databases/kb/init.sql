
\connect postgres

DROP DATABASE IF EXISTS kb;
CREATE DATABASE kb;

\connect kb

DROP ROLE IF EXISTS web;
CREATE ROLE web;

