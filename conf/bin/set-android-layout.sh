#!/usr/bin/env bash

hyprctl dispatch "setfloating class:code"
hyprctl dispatch "resizewindowpixel exact 80% 100%,class:code"
hyprctl dispatch "resizewindowpixel 0 -70,class:code"
hyprctl dispatch "movewindowpixel exact 12 12,class:code"

hyprctl dispatch "setfloating title:Android Emulator - Pixel_9_Pro_XL:5554"
hyprctl dispatch "resizewindowpixel exact 20% 100%,title:Android Emulator - Pixel_9_Pro_XL:5554"
hyprctl dispatch "resizewindowpixel -36 0,title:Android Emulator - Pixel_9_Pro_XL:5554"
hyprctl dispatch "movewindowpixel exact 80% 12,title:Android Emulator - Pixel_9_Pro_XL:5554"
hyprctl dispatch "movewindowpixel 24 0,title:Android Emulator - Pixel_9_Pro_XL:5554"
