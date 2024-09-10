#!/usr/local/bin/perl

# Convert a .csh script to .sh.
# Much is specific for taesetup* files, but the rest is general.
#
# Usage:  (intended to be called from installtae.ah)
#
# taesetupGen.pl $TAE/bin/csh/taesetupmin.sh $TAE/bin/sh/taesetupmin.sh
# taesetupGen.pl $TAE/bin/csh/taesetup.sh $TAE/bin/sh/taesetup.sh
# taesetupGen.pl $TAE/bin/csh/taesetupsrc.sh $TAE/bin/sh/taesetupsrc.sh
# taesetupGen.pl $TAE/bin/csh/taesetupclassic.sh $TAE/bin/sh/taesetupclassic.sh

# Change Log:
# 25-may-93 Initial...palm, kbs
# 01-jul-93 Mark sh version as generated so no one accidentally edits...kbs
# 29-jun-94 PR2720 (Marty's last day) Handle calls to "source" differently 
#           from comments which contain "source"...kbs

# read from CSH_VERSION, write to SH_VERSION
open (CSH_VERSION, "$ARGV[0]") || die "Error: cannot read CSH_VERSION '$ARGV[0]'";
open (SH_VERSION, ">$ARGV[1]") || die "Error: cannot write SH_VERSION '$ARGV[1]'";

# Output top of sh version including warning not to edit this.
print SH_VERSION "NOOP=noop       # MAC doesn't understand #!/bin/sh\n";
print SH_VERSION "#\n";
print SH_VERSION "#\tCAUTION: This is a GENERATED FILE. DO NOT EDIT!\n";
print SH_VERSION "#\n";
print SH_VERSION "# Changes should be made to the versions in \$TAE/bin/csh directory.\n";
print SH_VERSION "#-------------------------------------------------------------------\n";
print SH_VERSION "#\n";

while (<CSH_VERSION>)
    {
    #
    #	Note: the right side of the setenv assignment might
    #	be a quoted string with all kinds of strange characters.
    #	So we take everything to the right of the "=" and
    #   use it as the value.
    #
    #	The pattern is: white setenv white word white allElse 
    #	where "allElse" becomes $value.
    #
    if (/\s*setenv\s*(\w*)\s*(.*)/)		# setenv statement?
	{
	$TAEvariable = $1;			# first pattern match
	$value = $2;				# second pattern match
	print SH_VERSION "$TAEvariable=$value\n";	# produce sh statement
	push (@exportList, $TAEvariable);	# and remember export name
	}
    else	# don't filter, except for special cases (unique to taesetup*)
	{
	# handle $path reference in comments
        s/path\s*=\s*\(\$path /PATH="\$PATH:/;
        s/BIN\)/BIN"/;

        # handle if/endif (note spaces around brackets)
        s/^if\s*\(/if [ /;
        s/==\s*rs6000\)/= rs6000 ] ; /;
        s/^endif/fi/;

	# change actual CSH source calls to SH dot-space calls
	# NOTE: Assumption is that such lines begin in column one.
	s/^source/\./;
	s/\/csh\//\/sh\//;
	### s/\s*source \$TAE/ \. \$TAE/;

	# fix comments about how to execute this
	s/\s*source/ execute/;
	s/\s*\.cshrc/ .login/;

	print SH_VERSION;
	}
    }
# Length of export statement may someday be a problem, 
# but this is fine for now.
print SH_VERSION "\n" . "export  @exportList\n";
close (CSH_VERSION);
close (SH_VERSION);
