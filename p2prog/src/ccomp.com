$!****************************************************************************
$!
$! Build proc for MIPL module ccomp
$! VPACK Version 1.7, Thursday, April 21, 1994, 15:29:17
$!
$! Execute by entering:		$ @ccomp
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
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
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module ccomp ***"
$!
$ Create_Source = ""
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
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to ccomp.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("ccomp.imake") .nes. ""
$   then
$      vimake ccomp
$      purge ccomp.bld
$   else
$      if F$SEARCH("ccomp.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ccomp
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ccomp.bld "STD"
$   else
$      @ccomp.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ccomp.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ccomp.com -
	-s ccomp.f -
	-i ccomp.imake -
	-p ccomp.pdf -
	-t tstccomp.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ccomp.f
$ DECK/DOLLARS="$ VOKAGLEVE"
       PROGRAM  ccomp
C#######################################################################
C  NAME OF ROUTINE
C      ccomp (Convert Complex)
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      ccmp is a VICAR applications program which performs conversions
C      between comples pixel format and two real format images.  Two
C      types of transformations are possible (amplitude and phase) or
C      (real and imaginary).  The transformations may be done in either
C      direction:  a complex image to two real images, or two real
C      images to a complex image.
C
C  PR[CEPARED FOR USE ON MIPL SYSTEM BY
C      FRANK EVANS
C  ORIGINAL CCOMP PROGRAM BY
C      FRANK EVANS
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     4-94  CRI  MSTP (S/W CONVERSION) VICAR PORTING
C
C    CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C
C      ccomp INP=a OUT=(b,c) optional parameters
C        or
C      ccomp INP=(b,c) OUT=a optional parameters
C
C      ccomp a (b,c) optional parameters
C        or
C      ccomp (b,c) a optional parameters
C
C       Here 'a' represents the input or output complex image file name,
C       'b' represents the input or output image real or amplitude file name.
C       'c' represents the input or output image imaginary or phase file name.
C         When (b,c) are inputs or outputs they are paired as:
C              (real,imaginary) or (amplitude,phase) 
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file name(s).
C      OUT    - Output file name(s).
C      POLAR  - for amplitude and phase (default).
C      RECTANG- for real and imaginary.
C      FORWARD- for complex input (default).
C      INVERSE- for complex output.
C  OUTPUT PARAMETERS
C      The output image produced is written to the output file(s).
C  PROGRAM LIMITATIONS
C      1. The input image must be COMP data for keyword FORWARD.
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      INCLUDE 'VICMAIN_FOR'
C
      SUBROUTINE MAIN44 
C
C#######################################################################
C  NAME OF ROUTINE
C     MAIN44 (name for top level subroutine by VICAR convention)
C
C  PURPOSE
C      MAIN44 processes parameters entered by user to perform translation.
C#######################################################################

	IMPLICIT  NONE
	INTEGER*4 IN1UNIT, IN2UNIT, OUT1UNIT, OUT2UNIT, STATUS
	INTEGER*4 SL, SS, NL, NS, NLI, NSI, LINE, SAMP
	COMPLEX*8 COMPBUF(4096)
	REAL*4	  REAL1BUF(4096), REAL2BUF(4096)
	REAL*4	  AMP
	CHARACTER*8  INFORMAT
	LOGICAL*1 PHASEAMP, XVPTST

        CALL IFMESSAGE ('CCOMP version 02-MAY-94') 

        CALL XVEACTION ('SA',' ')

	PHASEAMP = XVPTST('POLAR')


	IF (XVPTST('INVERSE')) GOTO 2000



	CALL XVUNIT (IN1UNIT,'INP',1,STATUS,' ')
	CALL XVOPEN (IN1UNIT,STATUS,
     +		'OP','READ', 'U_FORMAT', 'COMPLEX',' ')
	CALL XVSIZE (SL, SS, NL, NS, NLI, NSI)
	CALL XVGET (IN1UNIT, STATUS, 'FORMAT', INFORMAT, ' ')
	IF (INFORMAT(1:4) .NE. 'COMP') THEN
	    CALL MABEND (' Input must be complex ')
	ENDIF


	CALL XVUNIT (OUT1UNIT,'OUT',1,STATUS,' ')
	CALL XVOPEN (OUT1UNIT,STATUS,
     +		'OP','WRITE',  'U_FORMAT','REAL', 'O_FORMAT','REAL',
     +		'U_NL',NL, 'U_NS',NS, ' ')
	CALL XVUNIT (OUT2UNIT,'OUT',2,STATUS,' ')
	CALL XVOPEN (OUT2UNIT,STATUS,
     +		'OP','WRITE',  'U_FORMAT','REAL', 'O_FORMAT','REAL',
     +		'U_NL',NL, 'U_NS',NS, ' ')


	DO LINE = SL, NL+SL-1
	    CALL XVREAD (IN1UNIT, COMPBUF, STATUS, 'LINE',LINE,
     +				'SAMP',SS, 'NSAMPS',NS, ' ')
	    IF (PHASEAMP) THEN
		DO SAMP = 1, NS
		    AMP = CABS(COMPBUF(SAMP))
		    REAL1BUF(SAMP) = AMP
		    IF (AMP .EQ. 0) THEN
			REAL2BUF(SAMP) = 0.0
		    ELSE
			REAL2BUF(SAMP) = ATAN2( AIMAG(COMPBUF(SAMP)), 
     +					   REAL(COMPBUF(SAMP))    )
		    ENDIF
		ENDDO
	    ELSE
		DO SAMP = 1, NS
		    REAL1BUF(SAMP) = REAL(COMPBUF(SAMP))
		    REAL2BUF(SAMP) = AIMAG(COMPBUF(SAMP))
		ENDDO
	    ENDIF
	    CALL XVWRIT (OUT1UNIT, REAL1BUF, STATUS, ' ')
	    CALL XVWRIT (OUT2UNIT, REAL2BUF, STATUS, ' ')
	ENDDO

	CALL XVCLOSE (IN1UNIT,STATUS,' ')
	CALL XVCLOSE (OUT1UNIT,STATUS,' ')
	CALL XVCLOSE (OUT2UNIT,STATUS,' ')

	RETURN



2000	CONTINUE


	CALL XVUNIT(IN1UNIT,'INP',1,STATUS,' ')
	CALL XVOPEN(IN1UNIT,STATUS,
     +		'OP','READ', 'U_FORMAT', 'REAL', ' ')
	CALL XVSIZE (SL, SS, NL, NS, NLI, NSI)

	CALL XVUNIT(IN2UNIT,'INP',2,STATUS,' ')
	CALL XVOPEN(IN2UNIT,STATUS,
     +		'OP','READ', 'U_FORMAT', 'REAL', ' ')


	CALL XVUNIT (OUT1UNIT,'OUT',1,STATUS,' ')
	CALL XVOPEN (OUT1UNIT,STATUS,
     +		'OP','WRITE',  'U_FORMAT','COMPLEX', 'O_FORMAT','COMPLEX',
     +		'U_NL',NL, 'U_NS',NS, ' ')


	DO LINE = SL, NL+SL-1
	    CALL XVREAD (IN1UNIT, REAL1BUF, STATUS, 'LINE',LINE,
     +				'SAMP',SS, 'NSAMPS',NS, ' ')
	    CALL XVREAD (IN2UNIT, REAL2BUF, STATUS, 'LINE',LINE,
     +				'SAMP',SS, 'NSAMPS',NS, ' ')
	    IF (PHASEAMP) THEN
		DO SAMP = 1, NS
		    COMPBUF(SAMP) = REAL1BUF(SAMP)*EXP((0,1)*REAL2BUF(SAMP))
		ENDDO
	    ELSE
		DO SAMP = 1, NS
                    COMPBUF(SAMP) = CMPLX(REAL1BUF(SAMP),REAL2BUF(SAMP)) 
		ENDDO
	    ENDIF
	    CALL XVWRIT (OUT1UNIT, COMPBUF, STATUS, ' ')
	ENDDO

	CALL XVCLOSE (IN1UNIT,STATUS,' ')
	CALL XVCLOSE (IN2UNIT,STATUS,' ')
	CALL XVCLOSE (OUT1UNIT,STATUS,' ')


	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ccomp.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ccomp

   To Create the build file give the command:

		$ vimake ccomp			(VMS)
   or
		% vimake ccomp			(Unix)


************************************************************************/


#define PROGRAM	ccomp
#define R2LIB

#define MODULE_LIST ccomp.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create ccomp.pdf
process help=*
PARM INP TYPE=(STRING) COUNT=1:2
PARM OUT TYPE=(STRING) COUNT=1:2
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM TRANS  TYPE=KEYWORD VALID=(POLAR,RECTANG) DEFAULT=POLAR
PARM DIRECT TYPE=KEYWORD VALID=(FORWARD,INVERSE) DEFAULT=FORWARD

!# annot function="Vicar Data Conversion"
!# annot keywords=(complex,transform,image,amplitude,phase,real,imaginary)

END-PROC
.TITLE
Converts images from complex to real data formats or vice-versa
.HELP
PURPOSE

    CCOMP converts images between complex pixel format and two real
format images.  Two types of transformation are possible (amplitude and phase)
or (real and imaginary).  The transformation may be done in either direction:
a complex image to two real images, or two real images to a complex image.


EXECUTION

    ccomp  IN.CMP  (OUT.AMP,OUT.PH)
    ccomp  IN.CMP  (OUT.RE, OUT.IM)  'RECT

    ccomp  (IN.AMP,IN.PH)  OUT.CMP    'POLAR 'INVERSE
    ccomp  (IN.RE, IN.IM)  OUT.CMP    'RECT  'INVERSE

'POLAR is the default transformation and 'FORWARD is the default direction.




Original Programmer:   Frank Evans         November 1986

Cognizant Programmer:  Frank Evans

Made portable for UNIX  RNR(CRI)           02-MAY-94

.LEVEL1
.VARIABLE INP
For FORWARD mode:
  complex image
For INVERSE mode:
  (real and imaginary) or
  (amplitude and phase) images
.VARIABLE OUT
For FORWARD mode:
  (real and imaginary) or
  (amplitude and phase) images
For INVERSE mode:
  complex image
.VARIABLE SIZE
VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE TRANS
Keyword for the transformation:
'POLAR for amplitude and phase.
'RECTANG for real and imaginary.
.VARIABLE DIRECT
Keyword for the direction:
FORWARD for complex input.
INVERSE for complex output.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstccomp.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
!
!  TEST WITH REAL AND IMAGINARY IMAGES
!
gen CCIMG1 5 5 LINC=10 SINC=4 IVAL=0 'COMP
label-list CCIMG1
list CCIMG1
!
!   COMPLEX TO REAL AND IMAGINARY 
!
ccomp CCIMG1 (CCIRE,CCIIM) 'RECT 'FORWARD
label-list CCIRE
list CCIRE
label-list CCIIM
list CCIIM
!
!  NOW REVERSE TO SEE IF INVERSED
!
ccomp (CCIRE,CCIIM) CCIMG2 'RECT 'INVERSE
label-list CCIMG2
list CCIMG2
!
!   check for differences
!
difpic (CCIMG1,CCIMG2) DIFF
list DIFF
!
!
!   COMPLEX TO AMPLITUDE AND PHASE 
!
ccomp CCIMG1 (CCIAMP,CCIPH) 'POLAR
label-list CCIAMP
list CCIAMP
label-list CCIPH
list CCIPH
!
!   REVERSE AND COMPARE TO ORIGINAL
!
ccomp (CCIAMP,CCIPH) CCIMG3 'INVERSE
label-list CCIMG3
list CCIMG3
!
!   check for differences
!
difpic (CCIMG1,CCIMG3) DIFF1
list DIFF1
end-proc
$ Return
$!#############################################################################
