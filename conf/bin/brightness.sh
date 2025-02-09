#!/usr/bin/env bash

# Switch case
case $1 in
  "up")
    brightnessctl s +5%
    ;;
  "down")
    brightnessctl s 5%-
    ;;
  *)
    echo "Invalid argument"
    ;;
esac

notify-send -t 1000 "Brightness" "$(brightnessctl g | awk '{print int($1 / $(brightnessctl m) * 100)}')"
