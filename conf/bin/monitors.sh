#!/usr/bin/env bash

INTERNAL_MONITOR="eDP-1"

check_external_monitors() {
  local MONITOR_LIST=$(hyprctl monitors -j)
  local EXTERNAL_CONNECTED=$(echo "$MONITOR_LIST" | jq -r '.[] | .name' | grep -v "$INTERNAL_MONITOR" | wc -l)

  if [ "$EXTERNAL_CONNECTED" -gt 0 ]; then
    hyprctl keyword monitor $INTERNAL_MONITOR, disable
  else
    hyprctl keyword monitor $INTERNAL_MONITOR, preferred, auto, 1
  fi
}

check_external_monitors

handle() {
  case $1 in
    monitoradded*) check_external_monitors ;;
    monitorremoved*) check_external_monitors ;;
  esac
}

socat -U - UNIX-CONNECT:$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock | while read -r line; do handle "$line"; done
