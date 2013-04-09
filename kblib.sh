#!/usr/bin/env bash
set -e

MAGIC="24A6942F-61EC-4E37-8D2F-5746B1D11B53"

if [ -z "$EDITOR" ]; then
  EDITOR="vim"
fi

REPO="$HOME/.kb"
MAGICFILE="$REPO/.magic"

function find_gnu_sed {
  # TODO something more intelligent
  which gsed || which sed
}

SED="$(find_gnu_sed)"

function init {
  if [ -d "$REPO" -a -f "$MAGICFILE" -a "$MAGIC" = "$(cat "$MAGICFILE")" ]; then
    cd "$REPO"
  elif [ -e "$REPO" ]; then
    echo "Repo directory $REPO already exists."
    exit 1
  else
    mkdir -p "$REPO"
    echo -n "$MAGIC" > "$MAGICFILE"
    cd "$REPO"
  fi

  if [ ! -d "$REPO/.git" ]; then
    git init
    git add .magic
    git commit -m "Magic number"
  fi
}

function edit_ok {
  FILE="$1"
  BACKUP="$(mktemp -t kb-bak)"
  cp "$FILE" "$BACKUP"
  if "$EDITOR" "$FILE"; then
    if diff -q "$BACKUP" "$FILE"; then
      rm "$BACKUP"
    else
      rm "$BACKUP"
      return 0
    fi
  else
    rm "$BACKUP"
  fi
  return 1
}

function extract_filename {
  SRC="$1"
  TITLE="$(grep -e '^\s*title\s*:\s*' "$SRC" | head -n 1)"
  if [ -n "$TITLE" ]; then
    echo -n "$TITLE" | "$SED" -e 's/^\s*title\s*:\s*//'
  fi
}

function run {
  COMMAND="$1"
  shift 1

  if [ -z "$COMMAND" ]; then
    COMMAND="help"
  fi

  case "$COMMAND" in

    a | ad | add)
      TMPDATA="$(mktemp -t kb-add)"
      done=""
      while [ -z "$done" ]; do
        if edit_ok "$TMPDATA"; then
          FILENAME="$(extract_filename "$TMPDATA")"
          if [ -n "$FILENAME" -a ! -e "$FILENAME" ]; then
            done="done"
          fi
        fi
      done
      mv "$TMPDATA" "$FILENAME"
      ;;

    e | ed | edi | edit)
      # TODO
      true
      ;;

    d | de | del | dele | delet | delete)
      # TODO
      true
      ;;

    l | li | lis | list | ls)
      # TODO
      true
      ;;

    h | he | hel | help)
      # TODO
      echo "HELP!"
      ;;

    *)
      echo "Unknown command: $COMMAND."
      exit 1
      ;;

  esac
}

