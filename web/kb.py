#!/usr/bin/env python3

from bottle import route, static_file, run

@route("/")
def home():
  return static("index.html")

@route("/static/<filename:path>")
def static(filename):
  return static_file(filename, root="static")

if __name__ == "__main__":
  run(host="localhost", port=8123)

