#!/usr/bin/env bash

log=$HOME/tmp/kbd_change.log
ts=$(date +%Y-%m-%d_%H:%M:%S)

echo "${ts} Change in keyboard detected: $*" >> ${log}
# env >> ${log}

ev=$1
id=$2

if [[ ${ev} == "XIDeviceEnabled" && ${id} == "24" ]] ; then
    (xmodmap /home/asmund/.xmodmap 2>&1) >> ${log}
	echo "${ts} Keyboard map updated" >> ${log}
fi
