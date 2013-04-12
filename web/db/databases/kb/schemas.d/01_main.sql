
DROP SCHEMA IF EXISTS kb;
CREATE SCHEMA kb;

SET search_path = 'kb';

CREATE TABLE person (
  id       BIGSERIAL PRIMARY KEY,
  name     TEXT      NOT NULL,
  password TEXT      NOT NULL,
  email    TEXT
);

CREATE TABLE tag (
  id      BIGSERIAL PRIMARY KEY,
  tag     TEXT      NOT NULL,
  created TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE TABLE note (
  id      BIGSERIAL PRIMARY KEY,
  owner   BIGINT    NOT NULL REFERENCES person(id),
  title   TEXT      NOT NULL,
  body    TEXT      NOT NULL,
  created TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE INDEX note_owner_index ON note (owner);

CREATE TABLE note_tag (
  tag_id  BIGSERIAL NOT NULL REFERENCES tag(id),
  note_id BIGSERIAL NOT NULL REFERENCES note(id),
  PRIMARY KEY (tag_id, note_id)
);

CREATE INDEX note_tag_tag_id_index ON note_tag (tag_id);

CREATE INDEX note_tag_note_id_index ON note_tag (note_id);

GRANT USAGE ON SCHEMA kb TO web;
GRANT TRUNCATE ON ALL TABLES IN SCHEMA kb TO web;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA kb TO web;
GRANT SELECT, USAGE, UPDATE ON ALL SEQUENCES IN SCHEMA kb TO web;

