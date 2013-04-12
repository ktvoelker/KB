#!/usr/bin/env python3

from bottle import request, response, route, static_file, run

import configparser
import json
import os
import psycopg2

CONFIG = configparser.ConfigParser()
CONFIG.read("kb.ini")

@route("/")
def home():
  return static("index.html")

@route("/static/<filename:path>")
def static(filename):
  return static_file(filename, root="static")

def connect():
  sockets = os.getcwd() + "/db/sockets"
  return psycopg2.connect(database="kb", user="web", host=sockets)

@route("/authenticate", method="POST")
def authenticate():
  name = request.forms.name
  password = request.forms.password
  conn = connect()
  c = conn.cursor()
  c.execute(
      "SELECT id FROM kb.person WHERE name = %s AND password = crypt(%s, password)",
      (name, password))
  row = c.fetchone()
  c.close()
  conn.close()
  if row:
    response.set_cookie("person_id", row[0], secret=CONFIG["global"]["secret"])
    return None
  else:
    abort(403)

def person_id(mandatory=True):
  ret = request.get_cookie("person_id", secret=CONFIG["global"]["secret"])
  if ret is None:
    abort(401)
  return ret

@route("/note/:id")
def note(id):
  pid = person_id()
  conn = connect()
  c = conn.cursor()
  c.execute("""
    SELECT title, body, created
    FROM kb.note
    WHERE owner = %s
    AND id = %s
    """, (pid, id))
  row = c.fetchone()
  if row:
    ret = {"id": id, "title": row[0], "body": row[1], "created": row[2].isoformat()}
    c.close()
    conn.close()
    return ret
  else:
    abort(404)

@route("/note/list")
def note_list():
  query = request.query.query
  if query:
    abort(400)
  else:
    pid = person_id()
    conn = connect()
    c = conn.cursor()
    c.execute("""
      SELECT id, title, created
      FROM kb.note
      WHERE owner = %s
      ORDER BY created DESC
      LIMIT %s
      """, (pid, CONFIG["global"]["query-limit"]))
    ret = [{"id": row[0], "title": row[1], "created": row[2].isoformat()} for row in c]
    c.close()
    conn.close()
    return {"notes": ret}

if __name__ == "__main__":
  run(host="localhost", port=8123)

