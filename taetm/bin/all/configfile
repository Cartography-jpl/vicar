#!/bin/sh
NOOP=noop
#
# extract this system's platform config file by determining
# MacroFile's value and outputting it.
#
# If your cpp doesn't supply a machine definition, supply one yourself
# in: $TAE/config/TAEplat.c
#
# Modification history:
#
# 27-sep-91	Initial version (borrowed heavily from platform.sh)...ljn


plat=$TAE/config/TAEplat.c

if [ -f $plat ]
then
  cc -E $plat | sed \
  -e '/^$/d' \
  -e '/^#  *[0-9]/d' \
  -e '/^# architecture:/d' \
  -e 's/^# configfile:  //' 
fi
