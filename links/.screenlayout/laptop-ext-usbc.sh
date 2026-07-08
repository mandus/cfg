#!/bin/bash

# first make sure the first connected displayport (usb-c) display is activated and primary
dpdisp=$(xrandr -q|rg '\<connected'|rg -o -P -r '$1' '^(DP-[0-9])')
if [ -n $dpdisp ] ; then 
    xrandr --output $dpdisp --auto --primary
fi

if ! xrandr -q|rg -q '\<connected.*\<primary' ; then
	echo "No primary connected display detected"
	exit 1
fi

# This only works with two connected
if [[ $(xrandr -q | rg '\<connected\>' | wc -l) -eq 2 ]] ; then

    mainscreen=$(xrandr -q | grep '\<connected\>.*primary')
    main_id=$(echo $mainscreen | awk '{print $1}')
    main_x=$(echo $mainscreen | awk '{print $4}' | sed 's/\(.*\)x.*/\1/')
    main_y=$(echo $mainscreen | awk '{print $4}' | sed 's/.*x\([^+]*\).*/\1/')

    otherscreen=$(xrandr -q | grep '\<connected\>' | grep -v 'primary')
    other_id=$(echo $otherscreen | awk '{print $1}')
    other_x=$(echo $otherscreen | awk '{print $3}' | sed 's/\(.*\)x.*/\1/')
    other_y=$(echo $otherscreen | awk '{print $3}' | sed 's/.*x\([^+]*\).*/\1/')

    center_vert=$(($main_y/2-$other_y/2))
	echo "main $main_id --pos 0x0"
	echo "other $other_id --pos ${main_x}x${center_vert}"


	# use xrandr to place laptop (the "other") right of and vertically centered 
    xrandr --output "$main_id" --pos 0x0 --output "$other_id" --pos ${main_x}x${center_vert}
fi

# just in case; refresh xmodmap
xmodmap ~/.xmodmap
