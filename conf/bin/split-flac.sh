#!/usr/bin/env bash

DIR=$1

if [ -z "$DIR" ]; then
    echo "Usage: $0 <directory>"
    exit 1
fi

if [ ! -d "$DIR" ]; then
    echo "Directory $DIR does not exist"
    exit 1
fi

fd . -e cue "$DIR" --print0 | while IFS= read -r -d '' FILE; do
  echo "Splitting $FILE"
  CUE_DIR=$(dirname "$FILE")

  mkdir -p "$CUE_DIR/.split"

  enca -L ru -x UTF-8 "$FILE"
  shnsplit -d "$CUE_DIR/.split" -f "$FILE" -t "%n. %t" -o "flac flac -V --best -o %f -" "$CUE_DIR"/*.flac

  rm -f "$CUE_DIR/.split"/00*pregap*
  cuetag.sh "$FILE" "$CUE_DIR/.split"/*.flac

  for f in "$CUE_DIR/.split"/*.flac; do
    FILENAME=$(basename "$f")
    DIRNAME=$(dirname "$f")
    NEW_FILENAME=$(echo "$FILENAME" | tr -d '?' | tr -d ':')

    if [ "$FILENAME" != "$NEW_FILENAME" ]; then
      mv "$f" "$DIRNAME/$NEW_FILENAME"
    fi
  done

  # Clean
  rm "$FILE"
  rm "$CUE_DIR"/*.flac
  mv "$CUE_DIR/.split"/* "$CUE_DIR"
  rm -r "$CUE_DIR/.split"
  printf "\n"
done
