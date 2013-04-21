#!/bin/sh
set -e
VERSION=3.3
[ -d ENV ] || virtualenv-$VERSION ENV
source ENV/bin/activate
pip-$VERSION install -r requirements.txt
touch .ready
