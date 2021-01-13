#!/usr/bin/sh

# Source .profile for common environment vars
. ~/.profile

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init --use-exwm
