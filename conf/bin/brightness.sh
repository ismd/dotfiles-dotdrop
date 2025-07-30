#!/usr/bin/env bash

set -e

if ! command -v brightnessctl &> /dev/null; then
  echo "Error: brightnessctl not found"
  exit 1
fi

# Switch case
case $1 in
  "up")
    brightnessctl s +5% > /dev/null
    ;;
  "down")
    brightnessctl s 5%- > /dev/null
    ;;
  *)
    echo "Usage: $0 {up|down}"
    exit 1
    ;;
esac

brightness=$(brightnessctl get)
max_brightness=$(brightnessctl max)
percentage=$((brightness * 100 / max_brightness))

notify-send -t 1000 -h string:x-canonical-private-synchronous:brightness \
  -i display-brightness-symbolic \
  "Brightness" "${percentage}%"
