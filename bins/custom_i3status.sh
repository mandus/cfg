#!/bin/bash
# prepend some custom stuff to i3status
source $HOME/.bash_alias
#make sure my home/bin is in path
export PATH=$PATH:$HOME/bin

on_wayland=$(env | grep -i wayland)
ip_file=$HOME/.realip

if [[ -n $on_wayland ]] ; then
    trap "killProc i3blocks" EXIT
    i3blocks
else
    trap "killProc i3status" EXIT
    i3status | (read line && echo $line && read line && echo $line && while : 
    do
        read line
        custom="["
        #if [[ -f $HOME/bin/show_xps_touchsense.sh ]] ; then
        #    touchsense=$(show_xps_touchsense.sh)
        #    custom="${custom}{ \"full_text\": \"☟ ${touchsense}\" },"
        #fi
        if [[ -z $on_wayland && -x /usr/bin/xbacklight ]] ; then
            brightness=$(xbacklight | sed 's/\..*//')
            custom="${custom}{ \"full_text\": \"☀ ${brightness}\" },"
        fi
        if [[ ! -f ${ip_file} || $((`date +%s` - `stat -L --format %Y ${ip_file}`)) -gt 7200 ]] ; then 
            curl -s ipinfo.io > ${ip_file}
        fi
        realip=$(cat ${ip_file} | jq -jr .ip)
        ipthrough=$(cat ${ip_file} | jq -jr .org)
        track=$(ncspot_showtrack.py)
        custom="${custom}{ \"full_text\": \"${track}\"}, "
        custom="${custom}{ \"full_text\": \"⇹ ${realip} by ${ipthrough}\" },"
        echo "${line/[/$custom}" || exit 1
    done)
fi
