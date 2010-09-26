#!/usr/bin/env python
# Copyright (C) 2010 by Michael Markert
# Licensed under MIT/X-License
# http://www.opensource.org/licenses/mit-license.php

from __future__ import print_function
import alsaaudio
from contextlib import closing

MUTE_STR = "Muted"

def get_volume(control):
    """Return decorated output of given control.
    
    control -- String denoting alsa control.
    """
    with closing(alsaaudio.Mixer(control)) as mixer:
        muted = mixer.getmute()[0]
        volumes = set(mixer.getvolume())
        if muted:
            return MUTE_STR
        else:
            return "/".join(map(str, volumes)) + "%"


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Print decorated state of alsa control.')
    parser.add_argument('control', nargs='?', default='Master')
    args = parser.parse_args()
    print(get_volume(args.control))
