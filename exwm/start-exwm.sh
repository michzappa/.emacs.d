#!/usr/bin/sh

# Source .profile for common environment vars
. ~/.profile

# Enable screen locking on suspend
xss-lock --slock &

# Compositor
compton &

# Weird thing about java apps
export _JAVA_AWT_WM_NONREPARENTING=1

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init --use-exwm
