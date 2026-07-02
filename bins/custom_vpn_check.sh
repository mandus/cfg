#!/bin/sh

if ! ip link show|rg -q vpn0 ; then
	# interface is not present yet; the case if we haven't used VPN in the session yet
	echo "VPN: off"
	exit 0
fi
state=$(ip link show|rg -o -P -r '$1' 'vpn0.*state ([A-Z]+)')
printf "VPN: ${state}"
