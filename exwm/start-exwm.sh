#!/usr/bin/sh

# Source .profile for common environment vars
. ~/.profile

# Enable screen locking on suspend
xss-lock --slock &

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init --use-exwm
