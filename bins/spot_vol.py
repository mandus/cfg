#!/usr/bin/env python3

import dbus
import sys

assert len(sys.argv) == 2
inp = sys.argv[1]
assert inp in ['+', '-'] or 0.0 <= float(inp) <= 1.0

ncs = dbus.SessionBus().get_object('org.mpris.MediaPlayer2.ncspot', '/org/mpris/MediaPlayer2')
ncs_prop = dbus.Interface(ncs, 'org.freedesktop.DBus.Properties')

if inp in ['+', '-']:
    adj = 0.05
    # make sure that we get float from dbus - just in case; unless tampered with it defaults to a float here
    vol = float(ncs_prop.Get('org.mpris.MediaPlayer2.Player', 'Volume'))
    ncs_prop.Set('org.mpris.MediaPlayer2.Player', 'Volume', eval(f'{vol}{inp}{adj}'))
else:
    ncs_prop.Set('org.mpris.MediaPlayer2.Player', 'Volume', float(sys.argv[1]))
