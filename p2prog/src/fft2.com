$!****************************************************************************
$!
$! Build proc for MIPL module fft2
$! VPACK Version 1.9, Friday, February 22, 2002, 16:16:37
$!
$! Execute by entering:		$ @fft2
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module fft2 ***"
$!
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Repack .or. Create_PDF .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to fft2.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("fft2.imake") .nes. ""
$   then
$      vimake fft2
$      purge fft2.bld
$   else
$      if F$SEARCH("fft2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fft2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fft2.bld "STD"
$   else
$      @fft2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fft2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fft2.com -mixed -
	-i fft2.imake -
	-p fft2.pdf -
	-t tstfft2.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fft2.imake
#define PROCEDURE fft2
#define R2LIB 
$ Return
$!#############################################################################
$PDF_File:
$ create fft2.pdf
procedure help=*
PARM 	INP	TYPE=STRING
PARM 	OUT	TYPE=STRING     COUNT=1
PARM	SIZE	TYPE=INTEGER	COUNT=(0,4)	DEFAULT=--
PARM	SL	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM	SS	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM	NL	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM	NS	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM	MODE	KEYWORD VALID=(FORWARD,INVERSE) COUNT=0:1	DEFAULT=--
PARM	FORMAT	KEYWORD	VALID=(BYTE,HALF,FULL,REAL,COMP)	DEFAULT=BYTE
PARM	TRANSPOS KEYWORD VALID=TRANSPOS COUNT=0:1 DEFAULT=--
PARM	SCRATCH	STRING			DEFAULT=""
PARM	BUFPOW	INTEGER			DEFAULT=18

BODY

  LOCAL IDS (STRING,80)
  LOCAL IN1 (STRING,80)
  LOCAL OUT1 (STRING,80)

  LET _ONFAIL = "RETURN"
  LET IDS="&SCRATCH"//"IDS"
  LET IN1 = "&INP"
  LET OUT1 = "&OUT"

  IF ($COUNT(TRANSPOS)=0) GOTO FFT

    IF (MODE="INVERSE") GOTO INV_MODE
      LET IN1 = "&INP"
      LET OUT1 = "&SCRATCH"//"FLOT"
      GOTO FFT

INV_MODE>
    LET IN1 = "&SCRATCH"//"FLOT"
    LET OUT1 = "&OUT"
    IF ($COUNT(SIZE)=0) GOTO NOSIZE
      FLOT INP=@INP OUT=@IN1 SIZE=@SIZE MODE=TRANS
      GOTO FFT 

NOSIZE>
      FLOT INP=@INP OUT=@IN1 SL=@SL SS=@SS NL=@NL NS=@NS MODE=TRANS
      
FFT>
  FFT22 INP=@IN1 OUT=@OUT1 SIZE=@SIZE SL=@SL SS=@SS NL=@NL NS=@NS +
        MODE=@MODE FORMAT=@FORMAT SCRATCH=@IDS BUFPOW=@BUFPOW

  IF ($COUNT(TRANSPOS)=0) RETURN

  IF ($COUNT(SIZE)=0) GOTO NOSIZE2
    FLOT INP=@OUT1 OUT=@OUT SIZE=@SIZE MODE=TRANS
NOSIZE2>
    FLOT INP=@OUT1 OUT=@OUT SL=@SL SS=@SS NL=@NL NS=@NS MODE=TRANS

END-PROC
.TITLE
VICAR PROCEDURE FFT2
.HELP
PURPOSE

 FFT2 is a VICAR procedure that computes the forward and inverse complex 
 two-dimensional Fourier Transform.  It invokes program FFT22.

EXECUTION:

The following is the execution statement format for FFT2:

	FFT2 IN OUT SIZE PARAMS

where INP, OUT, SIZE, PARAMS are parameters discussed in their respective
parameter sections.
.page
OPERATION:

The maximum image dimension (NL or NS) may not equal or exceed 4096; in
practice, this implies that the maximum dimension allowed for FFT22 is
4075.

See FFT22 for details of operation.  Details on the algorithm for the 
Fourier transform are given in HELP FFT22.

The output of the forward transform (and the input of the inverse)
is a two-dimensional Fourier-transform image in complex format.
Most Vicar programs do not support complex format, so if the user
wishes to do any processing on such an image with Vicar programs, 
one must generally first separate it into its REAL components using 
program CCOMP (q.v.),

.PAGE
WRITTEN BY:   L.W.Kamp  24 August 1985

COGNIZANT PROGRAMMER: L.W.Kamp

REV:          A. Scop   8 May 1995     (CRI) MSTP S/W CONVERSION
                                       Made portable for UNIX

.LEVEL1
.VARI INP
The input VICAR image file.
.VARI OUT
The output VICAR image file.
.VARI SIZE
The VICAR size field.
.VARI SL
Size field starting line.
.VARI SS
Starting sample
.VARI NL
Number of lines
.VARI NS
Number of samples
.VARI MODE
Forward or inverse mode.
.VARI FORMAT
Image data format.
.VARI SCRATCH
Scratch directory (if needed).
.VARI BUFPOW
Buffer size for FFT22.
.VARI TRANSPOS
Transpose the FFT?
.LEVEL2
.VARI INP
A VICAR labelled image file
.VARI OUT
OUT specifies the output transform dataset name for a forward transform,
or the output image dataset name for an inverse transform.

The output FFT of an image with NL lines and NS samples will be of
COMPLEX format, with NS lines and NL samples, i.e., it is transposed
with respect to the input.  This is done to speed up processing.
(The output of the inverse tranform is transposed a second time, hence
has the same orientation as the original image.)

In order to process an FFT with Vicar programs, one generally must
first separate it into its REAL components using program CCOMP (q.v.),
as most Vicar programs do not handle complex format.

.VARI SIZE
The standard size field defining the area of the input picture that is to
be transformed.

SIZE = (SL, SS, NL, NS).
.VARI SL
Starting line of the area to be transformed.
.VARI SS
Starting sample of the area to be transformed
.VARI NL
Number of lines in the area to be transformed.
.VARI NS
Number of samples in the area to be transformed.
.VARI MODE
FORWARD specifies that the fourier transform is to be from image space
to the conjugate dimension (frequency or wavelength).  This is the default
if the input data format is not complex.

INVERSE indicates that the fourier transformation is to be from frequency
(or wavelength) space, back to image space.  This is the default if the
input data format is complex.

Note that it is permissible to specify FORWARD for complex input data, but
not INVERSE with non-complex data:  the FFT must always be in complex
format.
.vari FORMAT
This keyword specifies the data format of the image file, which is the
input file in the FORWARD mode and the output file in the INVERSE mode.
(The FFT file always has COMPLEX format.)

Currently, this keyword is ignored in FORWARD mode, since Vicar2 convention
is not to override input file formats.

Valid values are: BYTE, HALF, FULL, REAL, COMP.

Default is BYTE.
.VARI SCRATCH
This specifies a scratch directory that is used by the program to store
one or two intermediate files which may be required.  The two possible
scratch files are:

 1) If TRANSPOS was specified, a file containing the FT after transposition
  (INVERSE mode) or before transposition (if REVERSE mode).

 2) A scratch file required by the FFT program itself during intermediate
   steps of the processing.  It is only required when both of the following
   conditions are met:
 
   (a) the image buffer required by the program is larger than
      the buffer size specified by the BUFPOW parameter. The image
      buffer size required is N*N complex numbers, where N is the
      smallest power of 2 that equals or exceeds both NL and NS,
      i.e., 8*N*N bytes; and,

   (b) the output file is not large enough to be used for scratch
      storage.  This will be true if its format is not complex, and
      also if the number of lines of the output is not equal to the
      number of samples.

   The default BUFPOW (18) is sufficient to contain a 128*128 image.
   However, note that a 64*256 image, say, requires a scratch file,
   since it is the larger of NL and NS that determines the buffer size.

The second scratch file will be deleted at the end of the execution of 
FFT2 by the program, but the first will remain.

The default for this parameter is the local directory.  
The user may specify a different location for any reason (e.g., if that 
disk is full, or not available).  If this parameter is specified for a 
case in which it is not required, it is ignored.

.vari BUFPOW
This determines the size of the buffer that the program uses to 
transpose the FFT.  Since this buffer size (in bytes) is always a power
of 2, this parameter specifies the power of 2 which equals the buffer
size:  BUF_SIZE = 2 ** BUFPOW.

Since the FFT will be transposed, this buffer must be able to hold
at least two lines of N complex numbers, where N is the larger of NS
and NL, hence:
                BUF_SIZE >= 2 * 8 * N,
 or:
                               2
                BUFPOW >= 4 + log(N).

The above conditions must be met for the program to run at all.
Furthermore, the entire transposition can be done in memory if
BUF_SIZE >= 8*N*N, or BUFPOW >= 3+logN.  This will speed processing,
assuming that the memory is available and that paging is not a problem.
.page
The user will not in general need to specify this parameter, but it
is provided for the following contingencies:

 (a) to allow improved performance, since increasing the buffer size
    will cut down on the amount of I/O done in the program, but can
    increase paging if a large working set is not available;
 
 (b) to obviate the need for a scratch file (see parameter SCRATCH);

.VARI TRANSPOS
This keyword gives the user the option to transpose the FT before
processing it (if MODE=INVERSE was specified) or after generating it 
(if MODE=FORWARD was specified).  Default is not to transpose the FT.

The reason for this option is that the standard FT format used by this
procedure is the transpose of the normal standard (which follows from
direct application of the FT formula).  The reason for this is to
save compute time;  further ramifications of this fact are discussed 
in the HELP text for program FFT22, q.v..  If the user wishes to have
the FT in "normal" format (at the expense of some extra CPU time),
then keyword TRANSPOS should be specified.

Bear in mind that if a forward transform is taken using TRANSPOS, then
a subsequent inverse transform should also use this keyword!

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfft2.pdf
procedure
refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
gen a 256 256
!
! forward:
fft2 a b
! inverse:
fft2 b c 'inverse
! check result (should be ramp image):
list c (1,1,5,5)
!
! test parm SIZE:
fft2 a b (20,20,128,128) 
fft2 b c 'inverse 
list c (1,1,5,5)
!
! test BUFPOW and SCRATCH:
fft2 a b
fft2 b c scratch=scr bufpow=14 'inverse 
!
! check result of last 2 steps:
list c (1,1,5,5)
!
! test TRANSPOS
gen a 256 256 sinc=0
fft2 a b 'transpos
fft2 b c 'inverse
!  these two areas should be each other's transpose:
list a (1,1,5,5)
list c (1,1,5,5)

end-proc
$ Return
$!#############################################################################
