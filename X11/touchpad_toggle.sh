#!/bin/bash

# toggle state of synaptics touchpad

tpid=`xinput list | grep SynPS | sed 's/.*id\=\([0-9]\+\).*/\1/g'`

declare -i status
status=`xinput list-props ${tpid} | grep Device\ Enabled | sed -e 's/.*\:[ \t]\+//g'`

if [ 0 -eq ${status} ] ; then
    xinput enable ${tpid}
else
    xinput disable ${tpid}
fi
