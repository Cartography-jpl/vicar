#!/bin/sh
NOOP=noop	#force Bourne Shell
#
#  Script to compile and link single-file TAE Plus application.
#
# CHANGE LOG
# 20-feb-91	added Ultrix specifics (PR 800)...kbs
# 06-mar-90     Produce more generic message...ljn
# 26-mar-90     added platform specifics (PR 800)...kbs/krw/cew
# 27-mar-90	generate prog_name.mak (09-apr-91 revamped) ...ljn,cew
# 18-nov-92	PR 1718: On some platforms (i.e. Ultrix and SGI), the suffix
#		supplied for basename is a regular expression.  Thus
#		"basename minwptc .c" will result in "minwp".  Need to
#		backslash the ".".  Also need to put the suffix in quotes
#		for the SGI...cew
# 25-may-93     $TAEBIN/{all,csh} changed to $TAE/bin/{all,csh}...kbs
# 09-jul-93     Add option -a (or -A) for ANSI C compilation...kbs,rt
# 03-nov-93	PR2456 - HP getopts is diff, so process args ourselves...kbs

# assume not ANSI
DEFINE_ANSI=
OPT_USAGE="$0: Only valid option is -a (or -A) to build an ANSI programs."

while [ $# -gt 0 ]
do
    case $1 in

    -[aA])
	DEFINE_ANSI="-DANSIC"
	shift 1		# NOTE
	;;

	# don't accept any other option
    -?*)
     	echo "$OPT_USAGE"
	exit 1
	;;

	# Filename processed later
     *) 
	break ;;	# exit loop
     esac
done

if [ $#  != 1 ] ; then
    echo "$0: Single argument expected (name of .c file)."
    exit 1
else
    if [ ! -s $1 -a ! -s $1.c ] ; then
    	echo "C program $1 does not exist or is empty."
	exit 1
    fi
fi

if [ -z "$TAEPLAT" ] ; then
    echo "\$TAEPLAT (TAE platform) is not set in your environment."
    echo "Type:  source $TAE/bin/csh/taesetup"
    echo "Then run $0 again."
    exit 1
fi

if test "$TAEPLAT" = mipsel || test "$TAEPLAT" = sgi
then
    prog=`basename $1 '\.c'`		#in case .c extension specified
else
    prog=`basename $1 .c`
fi

IMAKEFILE=$TAE/bin/all/taeccone.imak
MAKEFILE=$prog.mak

echo "     Beginning single source file build."

#If $prog.mak already exists, back it up and produce a warning.
if [ -f ${prog}.mak ] ; then
    echo "     Backing up existing file ${prog}.mak to ${prog}.mak.bak."
    test -f ${prog}.mak.bak && rm -f ${prog}.mak.bak
    mv ${prog}.mak ${prog}.mak.bak
fi

#Generate prog_name.mak file to be used to build program.
echo "     Generating file ${prog}.mak."
imake -TTAEmake.tmpl -I$TAE/config $DEFINE_ANSI -DPROG_NAME=$prog -f $IMAKEFILE -s $MAKEFILE

if test $TAEPLAT && test "$TAEPLAT" = apollo
then
echo "     make -P -f ${MAKEFILE}"
make -P -f $MAKEFILE 
else
echo "     make -f ${MAKEFILE}"
make -f $MAKEFILE 
fi
exit 0
