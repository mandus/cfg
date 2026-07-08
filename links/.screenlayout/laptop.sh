#!/bin/bash

# Turn off all but the internal display

for disp in $(xrandr -q|rg -o -P -r '$1' '^(DP-[0-9]|HDMI-[0-9])') ; do
	xrandr --output $disp --off
done

# make internal display primary
xrandr --output eDP-1 --auto --primary

# just in case; refresh xmodmap
xmodmap ~/.xmodmap
