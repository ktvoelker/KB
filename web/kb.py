#!/usr/bin/env python3

from bottle import request, response, route, static_file, run, abort

import configparser
import json
import os
import psycopg2

CONFIG = configparser.ConfigParser()
CONFIG.read("kb.ini")

SOCKETS = os.getcwd() + "/db/pg"

NOTE_FIELDS = ["title", "body"]

@route("/")
def home():
  return static("index.html")

@route("/static/<filename:path>")
def static(filename):
  return static_file(filename, root="static")

def transact(fn):
  def result(*args, **kwargs):
    with psycopg2.connect(database="kb", user="web", host=SOCKETS) as conn:
      with conn.cursor() as cur:
        return fn(cur, *args, **kwargs)
  return result

@route("/authenticate", method="POST")
@transact
def authenticate(c):
  name = request.forms.name
  password = request.forms.password
  c.execute(
      "SELECT id FROM kb.person WHERE name = %s AND password = crypt(%s, password)",
      (name, password))
  if c.rowcount == 1:
    row = c.fetchone()
    response.set_cookie("person_id", row[0], secret=CONFIG["global"]["secret"])
    return None
  else:
    abort(403)

def person_id(mandatory=True):
  ret = request.get_cookie("person_id", secret=CONFIG["global"]["secret"])
  if mandatory and ret is None:
    abort(401)
  return ret

@route("/note/:id")
@transact
def note_get(c, id):
  pid = person_id()
  c.execute("""
    SELECT title, body, created
    FROM kb.note
    WHERE owner = %s
    AND id = %s
    """, (pid, id))
  if c.rowcount == 1:
    row = c.fetchone()
    ret = {"id": id, "title": row[0], "body": row[1], "created": row[2].isoformat()}
    return ret
  else:
    abort(404)

@route("/note/new", method="POST")
@transact
def note_new(c):
  pid = person_id()
  fields = ["owner"]
  slots = ["%%s"]
  params = [pid]
  for field in NOTE_FIELDS:
    if not request.forms[field]:
      abort(400)
    fields.append(field)
    slots.append("%%s")
    params.append(request.forms.getunicode(field))
  c.execute(
      "INSERT INTO kb.note (" + ", ".join(fields) + ") VALUES (" +
      ", ".join(slots) + ") RETURNING id", params)
  if c.rowcount == 1:
    (id,) = c.fetchone()
    return {"id": id}
  else:
    abort(500)

@route("/note/:id", method="POST")
@transact
def note_edit(c, id):
  pid = person_id()
  changes = []
  params = []
  for field in NOTE_FIELDS:
    val = request.forms.getunicode(field)
    if val:
      changes.append("%s = %%s" % (field,))
      params.append(val)
  if len(changes) > 0:
    params.append(pid)
    params.append(id)
    c.execute(
        "UPDATE kb.note SET " + ", ".join(changes) +
        "WHERE owner = %s AND id = %s", params)
  return None

@route("/note/list")
@transact
def note_list(c):
  query = request.query.query
  if query:
    abort(400)
  else:
    pid = person_id()
    c.execute("""
      SELECT id, title, created
      FROM kb.note
      WHERE owner = %s
      ORDER BY created DESC
      LIMIT %s
      """, (pid, CONFIG["global"]["query-limit"]))
    ret = [{"id": row[0], "title": row[1], "created": row[2].isoformat()} for row in c]
    return {"notes": ret}

if __name__ == "__main__":
  run(host="localhost", port=8123)

