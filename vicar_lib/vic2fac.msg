!
!	Message file for facility VIC2
!
!  This file contains the message id's and detailed texts for
!  the Vicar2 internal errors.
!
!
!	NOTE :  This file is built by ERRORBLD.COM.  It should never
!		be modified directly.  To make changes to the source,
!		change ERRORBLD.COM.
!
!
.KEY UNDEFOPT	! UNDEFINED_OPTIONAL
Undefined optional.

Explanation:

An optional argument given to a VICAR run-time library routine
is not recognized as a valid optional argument.

User action:

This error is usually a program error and the cognizant
programmer should be notified.

Programmer action:

Check the Run-Time Library reference manual to determine what
optional parameters are allowed for the subroutine call in question,
or notify the system programmer.

.KEY NOFREUN	! NO_FREE_UNITS
No free units.

Explanation:

The maximum number of units has been reached.  This error generally
means that too many files are opened at once.

User action:

Consult the cognizant programmer for the program in which the
error occurred and determine whether or not it is a program error.
If not, consult the system programmer.

Programmer action:

Reduce the number of files open simultaneously.  If you are opening
and closing many files but not many are open at once, use the
``CLOS_ACT'', ``FREE'' optional on XVCLOSE to free the unit
number for re-use when you close the file.

.KEY INSUFMEM	! INSUFFICIENT_MEMORY
Explanation:

Insufficient memory for operation.  VICAR was not able to allocate
sufficient memory for an internal function.

User action:

This error probably indicates a memory quota was exceeded.
See the system manager or system programmer to determine the
exact cause and perhaps increase the amount of memory available.

.KEY STOROPT	! UNABLE_TO_STORE_OPTIONAL
Explanation:

An internal error occurred, preventing the storage
of an optional argument to the routine indicated.

User action:

This error probably indicates an internal error, usually due
to insufficient memory.  Please consult the system programmer.

.KEY NOSCHUN	! NO_SUCH_UNIT
Explanation:

An action was requested on a nonexistent unit.

User action:

This is normally a program error, so consult the cognizant programmer.

Programmer action:

Verify that the unit number being used is valid.  This error
generally indicates that the program failed to call XVUNIT before
using a unit number, or that the variable which contains the unit
number has been inadvertantly written over.

.KEY MODOPNUN	! CANNOT_MOD_OPEN_UNIT
Explanation:

An attempt to call the routine XVADD was made after a unit was
already open.  In order to prevent the internal table from being
corrupted, this operation is defined as illegal.

User action:

Consult the cognizant programmer.

Programmer action:

Prevent the calling of XVADD after a unit has been opened.
NL may be updated by using only XLADD, and other fields may
not be modified after a unit is opened.

.KEY NONASC	! STRING_HAS_NON_ASCII_CHARS
Explanation:

A string was passed to the indicated routine which contained
characters which are not valid ASCII characters.  This generally
indicates a program error.

User action:

The contents of the string should be checked.  Please consult
the cognizant programmer.

Programmer action:

Use the debugger or a print statement to determine what the
indicated string contains.

.KEY BADSIZ	! IMPROPER_IMAGE_SIZE_PARAM
Explanation:

A size argument (U_N1, U_N2, U_N3, or U_N4) was provided to the
indicated routine that was not in an allowable range
(usually less than zero).

User action:

Check all the parameters which could have led to the bad size.
If they are good, it is probably a program error and the cognizant
programmer should be consulted.

.KEY BADMETH	! IMPROPER_METHOD_STRING
Explanation:

The METHOD optional argument contained an invalid value for the
routine indicated.  This is a program error.  The valid values for
METHOD are ``RANDOM'' and ``SEQ''.

User action:

Please consult the cognizant programmer so that the value of
METHOD can be checked.

.KEY BADOPSTR	! IMPROPER_OP_STRING
Explanation:

The OP optional parameter had an invalid value associated with
it in the indicated routine.  This is a program error.

User action:

Please consult the cognizant programmer so that the value of
OP can be checked.

Programmer action:

The only valid values for OP are ``READ'', ``WRITE'', and
``UPDATE''.  Check that the call to XVOPEN or XVADD uses one
of these values.

.KEY BADFOR	! IMPROPER_FORMAT_STRING
Explanation:

The pixel format being passed to the indicated routine is not
one of the valid values.  This is a program error.

User action:

Please consult the cognizant programmer to verify that a
valid format string is being passed.

Programmer action:

Make sure that the FORMAT argument to the indicated routine
is one of following values:  BYTE, HALF, FULL, REAL, DOUB, or COMP.

.KEY ODDOPT	! ODD_NUMBER_OF_OPTIONALS
Explanation:

The number of optional arguments to the indicated routine is odd.
Since the optional arguments are given in pairs as KEYWORD, VALUE,
an odd number is not allowed.

User action:

Please consult the cognizant programmer to check that the
calling sequence is valid.

Programmer action:

Check the indicated routine call and verify that the calling
sequence is legitimate.

.KEY ALROPN	! FILE_IS_ALREADY_OPEN
Explanation:

XVOPEN has been called on a file which is already open.
This is a program error.

User action:

Please notify the cognizant programmer.

.KEY BADNAM	! BAD_FILE_PARAM_NAME
Explanation:

An error was encountered when XVUNIT tried to look up a file
name in the command line.  This error probably indicates a
mismatch between the program and the PDF file associated with it,
and usually has to do with the ``INP'' or ``OUT'' parameters.

User action:

Please consult the cognizant programmer so that any
disparity between the program and the PDF can be corrected.

.KEY NOTERM	! NO_IO_TO_TERMINAL
Explanation:

The terminal is not allowed for I/O operations.  The file name
given to a program points to the user's terminal (for example
the logical name TT).  Data can not be written to the terminal.
This error occurs under VMS only.

User action:

Use another file name.

.KEY SIZREQ	! IMAGE_SIZE_REQUIRED
Explanation:

XVOPEN was asked to process a file with incomplete information,
such as creating an output file where no primary input file is
present and no size information was supplied by the user or program.

User action:

Use the size field (if available) to work around the problem, and
notify the cognizant programmer of the failure of the program to
either calculate the size or require its input.

.KEY BADDIM	! IMPROPER_DIMENSION
Explanation:

The value for the image dimension supplied to the VICAR
I/O routines was not a valid value.

User action:

If the image dimension was specified on the command line,
verify that it is in the proper range (greater than zero and
less than the maximum allowable dimension, which is currently 4).
If it was not given on the command line, consult the cognizant programmer.

.KEY NOARRAY	! ARRAY_IO_NOT_ALLOWED
Explanation:

An attempt was made to open a non-disk file for array I/O.
The program being used was designed only to work on disk files.

User action:

Use a disk file for the file on which the program failed.

.KEY SECDEL	! CANNOT_DELETE_SECTION
Explanation:

The area of memory allocated for an array file, called
a mapped section, cannot be deleted.

User action:

This is a system error.  Consult the cognizant programmer
for the VICAR executive.

.KEY WAITFL	! IO_WAIT_FAIL
Explanation:

I/O wait fail.  A wait for asynchronous I/O returned a bad status.

User action:

This is an executive error and the system programmer
should be notified.

.KEY BUG	! INTERNAL_ERROR
Explanation:

An internal bug check in the VICAR run-time library failed,
indicating a bug in the run-time library.

User Action:

This error should never occur, hence please notify the
VICAR system programmer immediately.

.KEY NOEFLG	! UNABLE_TO_ACQUIRE_AN_EVENT_FLAG
Explanation:

Unable to acquire an event flag.  The VICAR Run-Time Library
I/O routines need event flags to control the open files.
This error generally means that the system provided event
flags are all being used.

User action:

This is probably a program error, caused by having too many
files open at once or by using event flags for some other
purpose.  Consult the cognizant programmer.

.KEY BADCVT	! IMPROPER_CONVERT_SETTING
Explanation:

The CONVERT or BIN_CVT optional parameter had an invalid value
associated with it in the indicated routine.  This is a program error.

User action:

Please consult the cognizant programmer so that the value of
CONVERT and BIN_CVT can be checked.

Programmer action:

The only valid values for CONVERT and BIN_CVT are ``ON'' and ``OFF''.
Check that the call to XVOPEN or XVADD uses one of these values.

.KEY NOTOPN	! FILE_NOT_OPEN
Explanation:

The indicated routine tried to operate on an unopened file.

User action:

This indicates a program error.  See the cognizant programmer.

Programmer action:

Make sure that XVOPEN is called prior to the indicated operation.
If so, make sure that the program checks the status of the open.
The status checking can be achieved automatically with the
OPEN_ACT option in XVOPEN or via the routine XVEACTION.

.KEY BADPARINST	! BAD_PARAM_INSTANCE
Explanation:

The instance given to the routine XVPONE (or XVIPONE) was not found
in the parameters.

User action:

Verify that all parameters have been given enough values.  If the
problem is not obvious, consult the cognizant programmer.

Programmer action:

Check the counts in the PDF to ensure that the user inputs all
required values.  For optional values, use XVPCNT to get the
number of values given before calling XVPONE.

.KEY NOTAVAIL	! NOT_IMPLEMENTED
Explanation:

The indicated routine was called with an optional argument which,
although listed in the programmer's reference manual, has not
yet been implemented.

User Action:

This indicates a program error, so consult the cognizant programmer.

Programmer Action:

Find another way to accomplish the same function if possible.
If the function is not indicated as being unimplemented in the
VICAR RTL programmer reference manual, inform MIPL of the problem.

.KEY BADORG	! BAD_ORG
Explanation:

The file organization keyword ORG passed to the indicated
routine is not one of the valid values.  The valid file
organizations are ``BSQ'' (Band SeQuential), ``BIL'' (Band
Interleaved by Line), and ``BIP'' (Band Interleaved by Pixel).

User Action:

This is a program error, so notify the cognizant programmer.

.KEY BADLBL	! BAD_INPUT_LABEL
Explanation:

An error occurred while parsing the input label.  Either the
file has no valid VICAR label, or the label is corrupted.

User action:

If the file has no label, LABEL-CREATE may be used to create one.
If the file has a label but this error still occurs, then the
label has probably been corrupted, which could be due to problems
transferring the file if it came from a foreign system, or it could
indicate an executive error.  In the latter case, consult the VICAR
system programmer.

.KEY EOF	! END_OF_FILE

Explanation:

The end of file was reached.  For input disk files, this error
probably indicates an attempt to read beyond the number of lines
in the image.  Otherwise, this  error indicates that the physical
end of file was reached on an I/O operation.

User action:

This could be either a user or a programmer error.  Verify that
all the parameters to the program in question are good values,
and that the input file is correct, i.e. not truncated or
mangled.  If the file is okay, consult the cognizant programmer.

.KEY OPNINP	! UNABLE_TO_OPEN_PRIMARY_INPUT
Explanation:

In the course of opening a file other that the primary input,
an attempt was made to open the primary input to get
label/control information, which failed.

User action:

Check the file specification of the primary input.  Also, if it is
on tape, make sure that it is not on the same tape as the output
file which is being processed.

.KEY BADINTFMT	! IMPROPER_INTFMT
Explanation:

An invalid value was given for INTFMT or BINTFMT, either as an optional
argument to XVOPEN, XVADD, or XVGET, as an argument to one of the XVTRANS
routines, or in the input label.

User action:

Verify that the input file has a valid value for INTFMT and BINTFMT.
If the problem is not obvious, consult the cognizant programmer.

Programmer action:

Check that all INTFMT and BINTFMT values are valid.  Currently, the valid
values are ``LOW'' and ``HIGH''.  If you are working on a machine with a
different integer representation, please consult the VICAR executive
programmer about adding it to the system.

.KEY BADOPR	! IMPROPER_OPERATION
Explanation:

Improper operation.  The program tried to perform an operation
on the indicated file which is forbidden in the mode in which
the file has been opened.  For instance, trying to write to a
file which has been opened for READ would cause this error.

User action:

This is generally a program error.  Consult the cognizant
programmer for the program in question.

.KEY HSTNTASC	! HIST_NAME_HAS_NON_ASCII_CHAR
Explanation:

The value for the label processing optional HIST has a non-ASCII
character.  The HIST item (task name) must be a valid ASCII string,
8 characters or less in length.

User action:

This is most likely a program error.  Please consult the
cognizant programmer.  It could, however, be due to a corrupted
history label in one of the input files.

.KEY BADHOST	! INVALID_HOST
Explanation:

An invalid value was given for the HOST argument for XVHOST.

User action:

If the HOST name was a program parameter, verify that the name of the
host is correct and is a supported host.  The currently supported hosts
are:

ALLIANT          Alliant FX series computer.
CRAY             Cray (port is incomplete).
DECSTATN         DECstation (any DEC RISC machine) running Ultrix.
HP-700           HP 9000/700 series workstation.
MAC-AUX          Macintosh running A/UX.
MAC-MPW          Macintosh running native mode with Mac Programmers Workbench.
SGI              Silicon Graphics workstation.
SUN-3            Sun 3, any model.
SUN-4            Sun 4 or SPARCstation, or clone such as Solbourne.
TEK              Tektronix workstation.
VAX-VMS          VAX running VMS.

NATIVE or LOCAL  The currently running machine

Programmer action:

Check that the value being passed in to XVHOST is valid and in the
above list.  If the desired machine is not supported, contact the
VICAR executive programmer about doing a port.

.KEY BADREALFMT	! IMPROPER_REALFMT
Explanation:

An invalid value was given for REALFMT or BREALFMT, either as an optional
argument to XVOPEN, XVADD, or XVGET, as an argument to one of the XVTRANS
routines, or in the input label.

User action:

Verify that the input file has a valid value for REALFMT and BREALFMT.
If the problem is not obvious, consult the cognizant programmer.

Programmer action:

Check that all REALFMT and BREALFMT values are valid.  Currently, the
valid values are ``VAX'', ``IEEE'', and ``RIEEE''.  If you are working
on a machine with a different floating-point representation, please
consult the VICAR executive programmer about adding it to the system.

.KEY NOTASK	! NO_TASKS_IN_LABEL
Explanation:

The history label of the indicated file does not contain any
history subsets, denoted by the TASK keyword.  This situation
is usually caused by the user deleting the history label.

User action:

Use the program LABEL or the DCL DUMP facility to examine the
label of the image file in question.  If the label contains
any non-empty TASK keywords, there is probably an executive
bug, so consult the system programmer.  If not, and a history
label is desired, use LABEL-REMOVE to remove the system
label, and LABEL-CREATE to create a new one.

.KEY FNDKEY	! CANNOT_FIND_KEY
Explanation:

The indicated routine is unable to find the specified key
in the label.

User action:

If the key is user specified, or if you know what keys the
program is looking for, check the label to verify that the
label item exists.  If, after checking, the problem is not
obvious, consult the cognizant programmer.

.KEY BADLBLTP	! BAD_LABEL_TYPE
Explanation:

A label type other than ``HISTORY'', ``SYSTEM'', or ``PROPERTY'' has
been input to the indicated routine, or neither has been specified.

User action:

The validity of the label type must be checked.  Please consult
the cognizant programmer.

Programmer action:

Verify that the call to the indicated routine used the values
``HISTORY'', ``SYSTEM'', or ``PROPERTY'' for the label type.

.KEY ORGMSMTCH	! ORG_MISMATCH
Explanation:

The program being used requires a specific file organization
(``BSQ'', ``BIL'', or ``BIP''), and the image file in question
is not in that organization.

User Action:

Check the documentation of the program in question to find out
what organization the file should be in, and convert the file
to the proper organization.

Programmer Action:

Make the program flexible enough to handle any organization.

.KEY LNGMESS	! ERR_MESS_TOO_LONG
Explanation:

The error message given on a *_MESS optional has exceeded the
maximum string size allowed (132 characters).

User action:

Consult the cognizant programmer.

.KEY NOMEM	! NO_MEMORY_FOR_LABEL_PROCESS
Explanation:

Memory required for label processing is dynamically allocated.
There is insufficient memory for operation.

User action:

This is a system error which probably indicates a memory quota
was exceeded.  See the system manager or system programmer to
determine the exact cause and perhaps increase the amount of
memory available.

.KEY STRTREC	! START_REC_ERROR
Explanation:

The starting record for a read or write operation, given by the LINE,
BAND, or SAMP optional arguments, is only partially specified.  This
condition arises when an image file has a third dimension which is
greater than one unit in size (for example, a band-sequential image
with more than one band), and the starting value in either the second
or third dimension was defaulted (for example, you have a band-sequential
image and you specify BAND but not LINE).  The error condition was
raised because the record being requested is ambiguous.

User Action:

This is a program error.  Notify the cognizant programmer.

Programmer Action:

Either completely specify the record in the call to XVREAD or XVWRIT
(for band-sequential images, give both LINE and BAND), or completely
default it (give neither).

.KEY NOSCHTSK	! NO_SUCH_TASK
Explanation:

The requested task was not in the history label of the image file.
This could be either a program or user error.

User action:

Use ``LABEL-LIST 'TASK'' on the image file to determine the tasks
contained in the label.  If a task name is being given on the
command line, check the detailed help for the program being used to
verify that the correct syntax is being used.  If the problem is
still not evident, consult the cognizant programmer.

.KEY NOKEY	! NO_SUCH_KEY
Explanation:

The indicated routine is unable to find the specified key in the
label.  This can be either a user or programmer error.

User action:

If the key is user specified, check the label to verify that the
label item exists.  If, after checking, the problem is not obvious,
consult the cognizant programmer.

.KEY NOLBL	! NO_SYSTEM_LABEL
Explanation:

The system label could not be found.  Either the file is
unlabeled, or the label is corrupted.

User action:

Use the program LABEL to create a label for the file.

.KEY BADINST	! ILLEGAL_INSTANCE
Explanation:

In a call to XVUNIT, the value given for the INSTANCE argument
is greater than the actual number of files given in the corresponding
parameter.

User action:

Verify that all the proper parameters have been input.  If they have,
then this message indicates that a program error has occurred and
the cognizant programmer should be consulted.

Programmer action:

Modify the PDF to require that the user input the appropriate
number of files.

.KEY ILLFOREQ	! ILLEGAL_FORMAT_REQUEST
Explanation:

The type of a label item is not ``STRING'', ``INT'', ``REAL'',
or ``DOUB''.

User action:

Consult the cognizant programmer so that the validity of
the type specified can be checked.

.KEY CONVERR	! CONVERSION_ERROR
Explanation:

The data item passed to the indicated routine is incompatible
with the FORMAT parameter.  For example, an integer is expected
and a real is passed.

User action:

If the parameter is user specified check to see that the data
item being passed to the routine is in the expected format.
If the problem is still not obvious, consult the cognizant programmer.

Programmer action:

Check the value of the item passed to the indicated routine.

.KEY HOSTLONG	! HOST_TOO_LONG
Explanation:

The host name given on the HOST or BHOST optional has exceeded the
maximum string size allowed (11 characters).

User action:

Consult the cognizant programmer.

.KEY BADLINST	! IMPROPER_LABEL_INSTANCE
Explanation:

An INSTANCE number was provided to a label routine that was not
in the allowable range (usually less than zero).

User action:

Check all the parameters which could have led to the bad instance.
If they are good, it is probably a program error and the cognizant
programmer should be consulted.

.KEY MODINPLBL	! CAN_NOT_MODIFY_AN_INPUT_LABEL
Explanation:

An attempt was made to modify the label of a read-only file.

User action:

Consult the cognizant programmer.

.KEY BADLEN	! IMPROPER_LENGTH
Explanation:

ULEN was either required for the indicated routine and not
given, or it was given an invalid value.

User action:

The size and existence of the value given to ULEN must be
checked. Please consult the cognizant programmer.

Programmer action:

The value of ULEN must be positive and less than the maximum
string size.  It is required to be given for multi-valued string
items passed from C, and all string items (multi-valued or single
value) passed from FORTRAN declared with a BYTE or LOGICAL*1.
Strings passed from FORTRAN declared as CHARACTER do not require
the ULEN optional.

.KEY BADSAMP	! IMPROPER_SAMP_SIZE_PARAM
Explanation:

A sample size argument (SAMP, NSAMPS, U_NS) was provided to the
indicated routine and was not in an allowable range (usually less
than zero).

User action:

Check all the parameters which could have led to the bad sample size.
If they are good, it is probably a program error and the cognizant
programmer should be consulted.

.KEY BADLINE	! IMPROPER_LINE_SIZE_PARAM
Explanation:

A line size argument (LINE, NLINES, U_NL) was provided to the
indicated routine and was not in an allowable range (usually less
than zero).

User action:

Check all the parameters which could have led to the bad sample size.
If they are good, it is probably a program error and the cognizant
programmer should be consulted.

.KEY NULLREQ	! NULL_REQUEST
Explanation:

The NHIST argument passed to the routine XLHINFO or XLPINFO is either
zero or negative.  No information is being requested.

User action:

The value of the argument passed to the indicated routine must be
checked. Please consult the cognizant programmer.

Programmer action:

Check the argument NHIST being passed to the routine XLHINFO or XLPINFO.

.KEY EOLAB	! END_OF_LABEL
Explanation:

You have reached the end of the label on an XLNINFO.  This is an
informational message which is normally used as a flag within a routine.

User action:

If this message is displayed consult the cognizant programmer.

.KEY TAPOPR	! ILLEGAL_TAPE_OPERATION
Explanation:

An attempt has been made to open a tape file for update.

User action:

If the program requires a file, use a disk or memory file.
If the problem is not obvious consult the cognizant programmer.

.KEY TAPMETH	! ILLEGAL_TAPE_METHOD
Explanation:

An attempt has been made to open a tape file for random access.

User action:

If the program requires a file, use a disk or memory file.
If the problem is not obvious consult the cognizant programmer.

.KEY BADBAND	! IMPROPER_BAND_SIZE_PARAM
Explanation:

A band size argument (BAND, NBANDS, U_NB) was provided to the
indicated routine and was not in an allowable range (usually less
than zero).

User action:

Check all the parameters which could have led to the bad sample size.
If they are good, it is probably a program error and the cognizant
programmer should be consulted.

.KEY TAPPOS	! TAPE_POSITIONING_ERROR
Explanation:

Bad device status. Failed to position tape.

User action:

Check to see that the tape drive is functioning properly.

.KEY PARMVERS	! PARM_FILE_VERSION
Explanation:

An unrecognized version number was found in the PARMS file,
usually indicating a new format not supported in the version
of VICAR being used.

User action:

Use a newer version of VICAR that supports the format of the
PARMS file, or regenerate the PARMS file using the same version
of VICAR being used to read the file.  If the same version is
being used for creating and reading the PARMS file, and the error
still occurs, then consult the VICAR executive programmer.

.KEY TOOLATE	! TOO_LATE
Explanation:

Occurs when label processing to a tape file is attempted after
the first I/O to that file.  Due to the sequential nature of the
tape device, once a record is read or written to a tape file, the
labels may not be read or modified.

User action:

Program may require file to be on disk.  Write desired file to disk.
If the problem is still not obvious consult the cognizant programmer.

.KEY NOLAB	! FILE_HAS_NO_LABEL
Explanation:

Input file does not contain a valid VICAR label.

User action:

Check file to verify that a label does exist.  If not, LABEL-CREATE
may be used to create a label.

.KEY DUPKEY	! DUPLICATE_KEY
Explanation:

An attempt has been made to add a label item with the label routine
XLADD, mode ``ADD'', for a key that already exists.  A duplicate
item cannot be added under the same HISTORY subset, the same PROPERTY
subset, or the SYSTEM label.  To modify an existing item, use XLADD
with mode ``REPLACE''.

User action:

If the item is user specified a new subset must be created to
accept the item, or the label item must be replaced instead of
added.  If the item is not user specified and this message is
displayed, consult the cognizant programmer.

.KEY BADBINSIZ	! IMPROPER_BINARY_SIZE_PARAM
Explanation:

A binary size argument (U_NBB or U_NLB) was provided to the
indicated routine that was not in an allowable range (usually
less than zero).

User action:

Check all the parameters which could have led to the bad binary
size.  If they are good, it is probably a program error and the
cognizant programmer should be consulted.

.KEY LNGCOND	! COND_STRING_TOO_LONG
Explanation:

The value for a COND optional argument has exceeded the maximum
number of allowable characters.

User action:

Please consult the cognizant programmer.

Programmer action:

Check the COND optional argument in the indicated routine
and verify that it is within the allowable string length (132).

.KEY LNGACT	! ACT_STRING_TOO_LONG
Explanation:

The value for the optional argument CLOS_ACT is longer than
the maximum number of allowed characters (132).

User action:

Please consult the cognizant programmer.

Programmer action:

Verify that the string passed with CLOS_ACT is the proper length.

.KEY ERRACT	! BAD_ERR_ACT_VALUE
Explanation:

The optional arguments OPEN_ACT, IO_ACT, LBL_ACT, and ERR_ACT,
as well as the error action argument to XVEACTION, must contain
the characters ``U'', ``A'', or ``S'', or any combination thereof.

User action:

Please consult the cognizant programmer.

Programmer action:

Verify that the error action argument contains only the characters
``U'', ``S'', and ``A'', alone or in any combination.

.KEY BADELEM	! IMPROPER_ELEMENT_NUMBER
Explanation:

A value for ELEMENT or NELEMENTS was provided to a label routine
that was not in the allowable range (usually less than zero).

User action:

Check all the parameters which could have led to the bad element
number.  If they are good, it is probably a program error and the
cognizant programmer should be consulted.

.KEY FORREQ	! FORMAT_OPTIONAL_REQUIRED
Explanation:

The FORMAT optional to XLADD cannot be defaulted.

User action:

Consult the cognizant programmer.

Programmer action:

Enter the desired FORMAT on the XLADD call.

.KEY VARREC	! VARREC_ERROR
Explanation:

If the ``VARREC'' option is given to COND, then ``NOLABELS'',
``NOBLOCK'', and a tape file must be specified as well.

User action:

Make sure the input file is on tape.  If it is, then this is
a program error.  Please consult the cognizant programmer.

Programmer action:

If ``VARREC'' is specified, verify that ``NOLABELS'',
``NOBLOCK'', and a tape file are also specified.

.KEY BADFILE	! IMPROPER_FILE_NUMBER
Explanation:

The U_FILE optional is less than zero.

User action:

This is a program error.  Please consult the cognizant programmer.

Programmer action:

Check the value of U_FILE.

.KEY NOTMULT	! BUF_NOT_MULT_OF_REC
Explanation:

For a tape which is being blocked, the block size for the tape (usually
given by BLOCKING in the mount command) is not an integral number of records.
This case is not allowed because it makes reading the tape difficult and puts
data in a non-standard format.  A second possibility is that the record size
is greater than 65,534 bytes.  If this is the case, you must default the
block size on the mount, and you must not use the NOBLOCK option in the
XVOPEN call, or you will get this error.

User action:

Verify that block size you gave on the MOUNT command is an integral number
of records.  The record size for a band-sequential (BSQ) or band-interleaved
by line (BIL) image is NS * (bytes per pixel), and for a band interleaved by
pixel (BIP) image is NB * (bytes per pixel).  If your record size is greater
than 65,534 bytes, then do not specify the block size on the MOUNT command,
and do not use the NOBLOCK option from XVOPEN.

.KEY ENDOFVOL	! END_OF_VOLUME
Explanation:

The end of volume mark (double tape mark or double end of
file) was hit when trying to open a file on an input tape.

User action:

Scan the tape to determine the actual number of files on it,
and make sure that the program does not try to access a file
beyond that number.

.KEY NOTAPE	! NO_TAPE_SUPPORT
Explanation:

The VICAR Run-Time Library may be built without tape support
if desired.  This error indicates that a tape function was
attempted when the support for tapes is not included in the
build of the RTL currently being used.

User action:

Use a different version of VICAR that is built with tape support,
if available.  If not, consult your system manager about rebuilding
the RTL with tape support, if tape drives are available on the
system being used.  Note that use of tapes requires the use of TAE.

.KEY NOTPARMFILE	! NOT_PARAMETER_FILE
Explanation:

The file given with PARMS must be a parameter file created by
another program with XVPOPEN, XVPOUT, etc., and not an image file.

User action:

Check that the filename you gave is correct.  If it is, then
consult the cognizant programmer for the program that created the file.

Programmer action:

Parameter files must be created using XVPOPEN, XVPOUT,
and XVPCLOSE, and must have a file type of ``PARAM'', ``PARM'',
or ``PARMS''.  Normal image files cannot be used as parameter files.

.KEY MULTPARMFILE	! MULTIPLE_PARAMETER_FILES
Explanation:

An attempt was made to open a parameter file before the previous
one was closed.  Only one parameter file can be open at a time.

User action:

This is a program error.  Consult the cognizant programmer.

Programmer action:

Finish processing one parameter file before using another.
Make sure that all parameter files are closed using XVPCLOSE.
XVCLOSE will not properly close a parameter file.

.KEY FILETYPE	! BAD_FILE_TYPE
Explanation:

The file type (the TYPE optional to the indicated routine) is not
a supported type.  Currently the types supported are ``IMAGE'',
``PARMS'', ``PARM'', and ``PARAM'', and the IBIS types ``GRAPH1'',
``GRAPH2'', ``GRAPH3'', and ``TABULAR''.

User action:

If the file type was specified on the command line (e.g., you tried
to use LABEL-REPLACE to change it), then use one of the valid types.
Otherwise, it may be a program error.

Programmer action:

Use only the valid types listed above.

.KEY BADTRANS	! INVALID_FORMAT_TRANSLATION
Explanation:

The data translation specified, either data type or host
format (or both), is not legal.  Currently there are some
restrictions on host translations allowed with the DOUB data format.

User action:

Make sure you specified the correct file.  If it is in a foreign
host format, try converting it to the machine's native format
and run the program again.

Programmer action:

Notify the VICAR executive programmer if you need the particular
translation to be implemented.

.KEY NOTAE	! NO_TAE_SUPPORT
Explanation:

The VICAR Run-Time Library may be built without support for
TAE (Transportable Applications Executive) if desired.  This
error indicates that a function requiring TAE was attempted
when the support for TAE is not included in the build of the
RTL currently being used.

User action:

Use a different version of VICAR that is built with TAE support,
if available.  If not, consult your system manager about installing
TAE and rebuilding the RTL to use TAE.

.KEY NONSEQWRIT	! NON_SEQUENTIAL_WRITE
Explanation:

Some devices, such as tapes, only allow sequential access on
writes.  The program attempted to either back up and re-write
a record or write only part of a record to a file on a
sequential device.

User action:

If the program requires a file, use a disk or memory file.
If the problem is not obvious consult the cognizant programmer.

.KEY NOTMOUNTED	! DEVICE_NOT_MOUNTED
Explanation:

Magnetic tape devices must be mounted prior to use with the
MOUNT command.

User action:

Mount the tape with the MOUNT command, and try opening
the file again.

.KEY XVCMDERR	! XVCOMMAND_ERROR
Explanation:

An internal error occurred in XVCOMMAND having to do with
message-passing to the TAE host.  Either the PARBLK could
not be built, or it could not be sent or received.

User action:

Notify the VICAR system programmer.

.KEY XVCMDFAIL	! XVCOMMAND_FAIL
Explanation:

The command submitted with a call to XVCOMMAND returned
an error status.

User action:

This is most likely a program error.  Make sure your inputs
to the program were correct, then notify the cognizant
programmer.

Programmer action:

Check the command submitted with XVCOMMAND.  Only intrinsic
commands and procedures using intrinsic commands may be
used with XVCOMMAND, i.e. no processes or DCL commands.
The failure may be a syntax error, an error in the
parameters, or an execution error.  The command should
have printed an error message; use this to find the
problem.

.KEY PARNOTFND	! PARAM_NOT_FOUND
Explanation:

A call to XVPARM or a related routine requested a parameter
that is not in the PDF.

User action:

This is a program error.  Consult the cognizant programmer.

Programmer action:

A parameter or a parameter qualifier in an XVPARM call (or
another parameter routine such as XVP) was not found in the
parblock passed to the program from TAE.  Check both the XVPARM
call and the PDF to make sure the parameter exists in both
places and that the spellings match.

.KEY BLTYPELONG	! BLTYPE_TOO_LONG
Explanation:

The binary label type name given on the BLTYPE optional has exceeded the
maximum string size allowed (11 characters).

User action:

Consult the cognizant programmer.

.KEY PARBLKERR	! PARBLK_ERROR
Explanation:

An invalid type was found in a variable in the parblock
passed from TAE.

User action:

Notify the VICAR system programmer.

Programmer action:

The VARIABLE structure for the parameter requested in a call
to GET_PARM had an invalid type.  It was not V_INTEGER, V_REAL,
or V_STRING.  Check the parblock passed from TAE for vailidity.

.KEY BADMODESTR	! IMPROPER_MODE_STRING
Explanation:

The MODE optional parameter had an invalid value associated
with it in the indicated routine.  This is a program error.

User action:

Please consult the cognizant programmer so that the value of
MODE can be checked.

Programmer action:

The only valid values for MODE are ``ADD'', ``INSERT'',
and ``REPLACE''.  Check that the call to XVOPEN or XVADD uses
one of these values.

.KEY NOSCHPROP	! NO_SUCH_PROPERTY
Explanation:

The requested property set was not in the property label of the image file.
This could be either a program or user error.

User action:

Use ``LABEL-LIST 'PROPERTY'' on the image file to determine the properties
contained in the label.  If a property name is being given on the
command line, check the detailed help for the program being used to
verify that the correct syntax is being used.  If the problem is
still not evident, consult the cognizant programmer.

.KEY PROPREQ	! PROPERTY_OPTIONAL_REQUIRED
Explanation:

The PROPERTY optional to XLADD, XLDEL, XLGET, and XLINFO cannot be
defaulted if the routine is processing PROPERTY labels.

User action:

Consult the cognizant programmer.

Programmer action:

Enter the desired PROPERTY on the label routine call.

.KEY UNDEFENV	! UNDEF_ENV_VAR
Explanation:

Filenames in Unix may include references to environment variables by
preceding the environment variable with a dollar sign ($).  The variable
name may optionally be in {braces}.  This error indicates that the
referenced environment variable is not defined.  This error may also
occur if the user's home directory was specified via a tilde (~) with
no username, and the $HOME environment variable is not defined.

User action:

Check the given filename to make sure the environment variable was
spelled correctly, and do a "printenv" from the shell to make sure
the variable is defined.

.KEY UNDEFUSR	! UNDEF_USER_NAME
Explanation:

Filenames in Unix may include references to the home directories
of other users by starting the filename with a tilde (~) followed
by the user name.  This error indicates that the referenced user name
is not present on the system.

User action:

Check the given filename to make sure the user name was spelled
correctly, and that the user does actually exist.

.KEY UNCLSBRC	! UNCLOSED_BRACES
Explanation:

Filenames in Unix may include references to environment variables by
preceding the environment variable with a dollar sign ($).  The variable
name may optionally be in {braces}.  This error indicates that an
opening brace was found for an environment variable but no closing
brace was present.

User action:

Check the given filename to make sure that a closing brace exists
if you used an opening brace.

.KEY TRUNCNAME	! NAME_TRUNCATED
Explanation:

The name returned by x/zvfilename was too big for the caller-supplied
buffer.  The name has been truncated to fit.

User action:

Try to use a shorter filename (including directory path), and contact
the cognizant programmer.

Programmer action:

Provide a larger buffer to the x/zvfilename call.

.KEY BADUPDHIST	! IMPROPER_UPD_HIST_SETTING
Explanation:

The UPD_HIST optional parameter had an invalid value
associated with it in the indicated routine.  This is a program error.

User action:

Please consult the cognizant programmer so that the value of
UPD_HIST can be checked.

Programmer action:

The only valid values for UPD_HIST are ``ON'' and ``OFF''.
Check that the call to XVOPEN uses one of these values.

.KEY TOOBIG	! FILE_TOO_BIG
Explanation:

The file size is too big for this version of VICAR.  Some operating
systems limit files to 2GB; others have no such limit.  One of three
cases might have occurred:

* The file is > 2 GB and such files are not supported on this OS/VICAR version
* The record size (NL*NB+NBB for BSQ files) is > 2 GB (on any platform)
* The file is > 2 GB and array I/O was attempted (on any platform)

User action:

For the first case, reduce the file size or move to an OS that supports
large files.  For the second, reduce the record size (records > 2 GB are
not supported on any platform).  For the third, avoid using array I/O
(array I/O can never exceed 2 GB on any platform).

.KEY GENERR	! general error
Explanation:

This message is informational, indicating that some error has
occurred.  It should always be accompanied by a detailed error message.

User action:

Look at the accompanying message to find out what the error is
and act accordingly.

.KEY HOSTMSG	! A host dependent error occurred, message to follow
Explanation:

This is an informational message only.  A host dependent error
occurred, and the message is given.  The host message (and
probably a stack trace if under VMS) should appear immediately
following this message.

User action:

Interpret the host error message and act accordingly.

.KEY BADSTAT	! Unrecognized error status
Explanation:

VICAR's internal error handler has been asked to issue a message
for a status it does not recognize.  This is probably a failure
on the part of the system programmer to keep the executive error
modules up to date.

User action:

Record the status value given and notify the system programmer
immediately.

