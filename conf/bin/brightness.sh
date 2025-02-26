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

# notify-send -t 1000 "Brightness" "$(brightnessctl | grep 'Current brightness' | awk -F'[()%]' '{print $2}')%" -h string:x-canonical-private-synchronous:brightness -i display-brightness-symbolic
