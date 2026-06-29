#!/bin/bash

state=$(ip link show|rg -o -P -r '$1' 'vpn0.*state ([A-Z]+)')
printf "VPN: ${state}"
