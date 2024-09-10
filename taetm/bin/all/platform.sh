#!/bin/sh
NOOP=noop
#
# extract this system's platform specification by determining
# MachineDep's value and outputting it.
#
# If your cpp doesn't supply a machine definition, supply one yourself
# in: $TAE/config/TAEplat.c
#
# Modification history:
#
# created : 29-Aug-1990 baw (slight modification to old tools/cpu script).
# 05-oct-92	IBM RS/6000 port...rt


plat=$TAE/config/TAEplat.c

if [ ! -f $plat ]
then
	echo "UNKNOWN"
else
  cc -E $plat | sed \
  -e '/^ $/d' \
  -e '/^$/d' \
  -e '/^#  *[0-9]/d' \
  -e '/^# configfile:/d' \
  -e 's/^# architecture:  //' 
fi
