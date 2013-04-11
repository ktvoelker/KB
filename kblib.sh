#!/usr/bin/env bash
set -e

MAGIC="24A6942F-61EC-4E37-8D2F-5746B1D11B53"

if [ -z "$EDITOR" ]; then
  EDITOR="vim"
fi

REPO="$HOME/.kb"
MAGICFILE="$REPO/.magic"

if [ "$(uname -s)" = "Darwin" ]; then
  function sed {
    gsed "$@"
  }
  function xargs {
    gxargs "$@"
  }
  function find {
    gfind "$@"
  }
  function echo {
    gecho "$@"
  }
fi

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

function extract_meta {
  FIELD="$1"
  SRC="$2"
  VAL="$(grep -e '^\s*'"$FIELD"'\s*:\s*' "$SRC" | head -n 1)"
  if [ -n "$VAL" ]; then
    echo -n "$VAL" | sed -e 's/^\s*'"$FIELD"'\s*:\s*//'
  fi
}

function extract_title {
  SRC="$1"
  extract_meta title "$SRC"
}

function extract_filename {
  SRC="$1"
  extract_title "$SRC" | sed -e 's/\s/_/g' | (cat -; echo -n ".txt")
}

function extract_tags {
  SRC="$1"
  extract_meta tags "$SRC"
}

function match {
  PRED="$1"
  FILE="$2"

  case "$PRED" in

    tag:*)
      TAG="$(tail -c +5 <<<"$PRED")"
      if extract_tags "$FILE" | grep -Eiq '\b'"$TAG"'\b'; then
        return 0
      else
        return 1
      fi
      ;;

    *)
      if grep -Eiq "$PRED" "$FILE"; then
        return 0
      else
        return 1
      fi

  esac
}

function filter_by {
  if [ $# -eq 0 ]; then
    cat -
  else
    for file in $(cat -); do
      for pred in "$@"; do
        if ! match "$pred" "$file"; then
          continue 2
        fi
      done
      echo "$file"
    done
  fi
}

function list_all {
  find . -mindepth 1 -maxdepth 1 -name '*.txt'
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
      "$EDITOR" "$TMPDATA"
      if [ -z "$(cat "$TMPDATA")" ]; then
        echo "Aborted."
        exit 1
      else
        FILENAME="$(extract_filename "$TMPDATA")"
        if [ -n "$FILENAME" -a ! -e "$FILENAME" ]; then
          mv "$TMPDATA" "$FILENAME"
          TITLE="$(extract_title "$FILENAME")"
          git add "$FILENAME"
          git commit -m "Added '$TITLE'"
        else
          echo "Error: could not extract filename."
          echo "The new entry has been left at $TMPDATA."
          exit 1
        fi
      fi
      ;;

    e | ed | edi | edit)
      MATCHES="$(list_all | filter_by "$@")"
      COUNT="$(wc -l <<<"$MATCHES")"
      if [ "$COUNT" -eq 0 ]; then
        echo "No matches."
      elif [ "$COUNT" -eq 1 ]; then
        FILENAME="$(basename "$(tr -d "\n" <<<"$MATCHES")")"
        OLDTITLE="$(extract_title "$FILENAME")"
        "$EDITOR" "$FILENAME"
        if git status --porcelain | grep "^ M $FILENAME"; then
          NEWFILENAME="$(extract_filename "$FILENAME")"
          TITLE="$(extract_title "$FILENAME")"
          git add "$FILENAME"
          if [ "$NEWFILENAME" != "$FILENAME" ]; then
            git mv "$FILENAME" "$NEWFILENAME"
            git commit -m "Renamed '$OLDTITLE' to '$TITLE'"
          else
            git commit -m "Edited '$TITLE'"
          fi
        fi
      else
        echo "$COUNT matches:"
        for i in "$MATCHES"; do
          echo "  $i"
        done
      fi
      ;;

    d | de | del | dele | delet | delete)
      # TODO
      echo "Not implemented."
      ;;

    l | li | lis | list | ls)
      {
        for i in $(list_all | filter_by "$@"); do
          extract_title "$i"
        done
      } | sort
      ;;

    h | he | hel | help)
      echo "Usage:"
      echo "  kb (a[dd] | (e[dit] | d[elete] | l[ist]) QUERY)"
      ;;

    *)
      echo "Unknown command: $COMMAND."
      exit 1
      ;;

  esac
}

