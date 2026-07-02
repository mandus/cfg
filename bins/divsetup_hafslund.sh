#!/bin/bash

# add ssh passphrase
ssh-add
# pass will not prompt for passphrase, so get agent going with a working passphrase
gpg -dq ~/.password-store/dev/mistral.ai/for-pi-api-key.gpg >/dev/null

# various mouses - should probably check if they are available first...
# the internal one
xinput set-prop 11 "libinput Natural Scrolling Enabled" 1
# Ergo MX trackball
(xinput list|rg -q Ergo) && xinput set-prop $(xinput list|grep "Ergo.*pointer"|rg -P -o '(?<=id=)[0-9]+') "libinput Natural Scrolling Enabled" 1

# external hdmi connected directly - make it primary (skip disconnected)
(xrandr -q |rg -q -P '^HDMI-[0-9]+.(?!dis)connected') && xrandr --output $(xrandr -q |rg -o -P -r '$1' '^(HDMI-[0-9]+).(?!dis)connected') --primary && echo "set HDMI as primary"
# external DP (probably over usb-c) connected - make it primary (skip disconnected)
(xrandr -q |rg -q -P '^DP-[0-9]+.(?!dis)connected') && xrandr --output $(xrandr -q |rg -o -P -r '$1' '^(DP-[0-9]+).(?!dis)connected') --primary && echo "set DP as primary"

echo "Connected displays:"
xrandr -q|rg '\<connected'
