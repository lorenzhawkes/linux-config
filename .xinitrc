#!/bin/bash

xsetroot -cursor_name left_ptr

# dbus-launch --exit-with-session /home/lorenz/.local/bin/startxmonad
exec /home/lorenz/.local/bin/startxmonad

# exec i3
