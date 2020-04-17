#!/bin/bash

# change volume of main sink according to pactl RUNNING state

run_sink=$(pactl list short sinks | grep RUNNING | awk '{print $1}' | head -1)

arg=$1
vol='+5%'
if [ ${arg} == "mute" ] ; then 
    pactl set-sink-mute ${run_sink} toggle
    exit 0
fi

if [ ${arg} == "m" ] ; then
    vol='-5%'
fi
pactl set-sink-volume ${run_sink} ${vol}
