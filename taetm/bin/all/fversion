#!/bin/sh
NOOP=noop

# Modification History:
#
# modified:  6-Sep-1990 baw (new version file name)
# created : 31-Aug-1990 baw (from bin/all/cversion and tools/fversion*)
# 18-jul-94	Include directory was incorrectly defined as $TAE/inc,
#		it should have been $TAEINC...krw
VERSION_FILE=./.version.c


# first get the version number
#
if (test "$1")
then
    VERSION=$1
else
    echo -n 'enter version number: '	#prompt user
    read VERSION         				# get version number
    test $VERSION || VERSION=`date`		# give default if nullstring
fi
echo \"${VERSION}\"

cat << END_OF_FILE > ${VERSION_FILE}
/*
 *	This module provides the version number for fortran programs.
 */

#include "forstr.inp"

	struct  {
		int	verlen;
		char	versn[32];
		} FOR_NAME(vers) = {sizeof(VERSION)-1, VERSION};

END_OF_FILE
set -x	
cc -c -DVERSION="\"${VERSION}\"" -D_NO_PROTO -I$TAEINC $VERSION_FILE

