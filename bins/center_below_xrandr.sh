#!/bin/bash

# script can be improved by using 'xrandr -q | grep '\<connected\>'' to find
# information about laptop-screen (primary) and other screen

# This only works with two connected
if [[ $(xrandr -q | grep '\<connected\>' | wc -l) -gt 1 ]] ; then

    mainscreen=$(xrandr -q | grep '\<connected\>' | grep 'primary')
    main_id=$(echo $mainscreen | awk '{print $1}')
    main_x=$(echo $mainscreen | awk '{print $4}' | sed 's/\(.*\)x.*/\1/')
    main_y=$(echo $mainscreen | awk '{print $4}' | sed 's/.*x\([^+]*\).*/\1/')

    otherscreen=$(xrandr -q | grep '\<connected\>' | grep -v 'primary')
    other_id=$(echo $otherscreen | awk '{print $1}')
    other_x=$(echo $otherscreen | awk '{print $3}' | sed 's/\(.*\)x.*/\1/')
    other_y=$(echo $otherscreen | awk '{print $3}' | sed 's/.*x\([^+]*\).*/\1/')

	# depends on which is bigger... Now we assume "main" is the bigger one...
    center_left=$(($main_x/2-$other_x/2))

    # use xrandr to center primary screen above 
    xrandr --output "$main_id" --pos 0x0 --output "$other_id" --pos ${center_left}x${main_y} 
fi
