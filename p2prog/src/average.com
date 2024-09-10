$!****************************************************************************
$!
$! Build proc for MIPL module average
$! VPACK Version 1.7, Monday, May 02, 1994, 10:26:56
$!
$! Execute by entering:		$ @average
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
$ write sys$output "*** module average ***"
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
$ write sys$output "Invalid argument given to average.com file -- ", primary
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
$   if F$SEARCH("average.imake") .nes. ""
$   then
$      vimake average
$      purge average.bld
$   else
$      if F$SEARCH("average.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake average
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @average.bld "STD"
$   else
$      @average.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create average.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack average.com -
	-s average.f -
	-i average.imake -
	-p average.pdf -
	-t tstaverag.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create average.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C      PROGRAM  AVERAGE
C#######################################################################
C  NAME OF ROUTINE
C      "AVERAGE"  ( AVERAGE images )
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JULY 1983
C  ENVIRONMENT
C      UNIX or VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     3-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C  CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C      average INP=(a...) OUT=b SIZE=(sl,ss,nl,ns) optional parameters
C      average INP=(a...) OUT=b SL=sl SS=ss NL=nl NS=ns optional parameters
C      average (a...) b (sl,ss,nl,ns) optional parameters
C      average (a...) b optional parameters
C
C       Here (a...) represents a list of 2 to 48 file names.
C       b represents an output image file name.
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file names.
C      OUT    - Output file names.
C      SIZE   - Standard Vicar size field:  (SL,SS,NL,NS)
C               SL = Starting line number.
C               SS = Starting sample number.
C               NL = Number of lines.
C               NS = Number of samples.
C               The same SIZE parameters apply to each of the input
C               image files.
C  SUBROUTINES CALLED
C      MAIN44 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C#######################################################################
C  NAME OF ROUTINE
C      MAIN44   (name of top level subroutine by VICAR convention)
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JULY 1983
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT AND OUTPUT PARAMETERS     
C      SEE UNDER PROGRAM AVERAGE.
C      
C  CALLED BY
C      "average"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'fortport'
      INTEGER TEMP
      BYTE IBUF(120000),OBUF(20000)
      INTEGER IDSN(48), IS(48)
C
C
C
C=================START OF EXECUTABLE CODE===============================     
C
      CALL IFMESSAGE ('AVERAGE version 02-MAY-94')
      CALL XVEACTION ('SA', ' ')
      CALL XVPCNT( 'INP', NI )

         CALL XVUNIT( IDSN(1), 'INP', 1, IND, ' ' )
         CALL XVOPEN( IDSN(1), IND, 'OP', 'READ', ' ' )
         CALL XVSIZE( SL, SS, NL, NS, NLI, NSI )   ! GET SIZE PARAMETER.
C
C  OPEN DATA SETS
      DO I=2,NI
         CALL XVUNIT( IDSN(I), 'INP', I, IND, ' ' )
         CALL XVOPEN( IDSN(I), IND, 'OP', 'READ', ' ' )
      END DO

         CALL XVUNIT( OUTFILE, 'OUT', 1, IND, ' ' )
         CALL XVOPEN( OUTFILE, IND, 'OP', 'WRITE', 
     .         'U_NL', NL, 'U_NS', NS, ' ' )
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCC  MAIN LOOP

      EL = SL+NL-1

      DO 491 II=SL,EL           ! LOOP OVER LINES.
              IF ( II .EQ. SL )   THEN
                 ISP = 1
                 DO I = 1, NI

                  CALL XVREAD( IDSN(I), IBUF(ISP), IND, 'LINE', 
     .                  SL, ' ' )
                  ISP = ISP + NSI
                 END DO
              ELSE
                 ISP = 1
                 DO I = 1, NI

                  CALL XVREAD( IDSN(I), IBUF(ISP), IND, ' ' )
                  ISP = ISP + NSI
                 END DO
              END IF
                 IS(1) = SS
                 DO I = 2,NI
                    IS(I) = IS(I-1) + NSI
                 END DO

                 DO  J = 1, NS              ! THE SAMPLE LOOP.
                     TEMP = 0
                     DO I = 1, NI
                        TEMP = TEMP + BYTE2INT( IBUF(  IS(I) ) )
                        IS(I) = IS(I) + 1
                     END DO
                     TEMP = TEMP / NI              ! AVERAGE
                     OBUF(J) = INT2BYTE(TEMP)      ! LOW ORDER BYTE.   
                 END DO

                 CALL XVWRIT( OUTFILE, OBUF, IND, ' ' )

491   CONTINUE

      RETURN          ! NORMAL END.
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create average.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM average

   To Create the build file give the command:

		$ vimake average			(VMS)
   or
		% vimake average			(Unix)


************************************************************************/


#define PROGRAM	average
#define R2LIB

#define MODULE_LIST average.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create average.pdf
process help=*
PARM INP TYPE=STRING COUNT=2:48
PARM OUT TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
!# parm inp(3-48) hints=default
END-PROC
.TITLE
AVERAGE
.HELP
PURPOSE:
AVERAGE takes up to 48 input images and averages them together to make an
output image

EXECUTION:

Example

AVERAGE INP=(A,B,C) OUT=D  will average images A, B, and C to form D.

If the size parameter is used (SIZE=(SL,SS,NL,NS)), only the defined area in
each input image will be used to create the new file.  

PROGRAM LIMITATIONS

     1. The input and output images must be byte data.
     2. Maximum number of samples is 20000 per line.  The sum of the number
        of samples per line for the input images must not exceed 120000.


WRITTEN BY:                  Steve Pohorsky                5 Oct 1984
COGNIZANT PROGRAMMER:        Steve Pohorsky
REVISION:  1                                                5 Oct 1984

.LEVEL1
.VARIABLE INP
STRING - Input image files
.VARIABLE OUT
STRING - Output image file
.VARIABLE SIZE
INTEGER - Region of input files
to be concatenated
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE NS
INTEGER - Number of lines
.VARIABLE NL
INTEGER - Number of samples
.LEVEL2
.VARIABLE INP
INP specifies the input data sets.  Up to 48 are allowed.
.VARIABLE SIZE
The SIZE parameter may be used when only a sub-region of each image is to
be concatenated; it has the format SIZE=(SL,SS,NL,NS), where the parameters
are starting line, starting sample, number of lines, and number of samples,
respectively.  SIZE=(1,1,10,10), for example, will cause AVERAGE to only look
at the first ten samples of each of the first ten lines in each image, when
performing the concatenation. 
.VARIABLE SL
INTEGER - Starting line (see SIZE)
.VARIABLE SS
INTEGER - Starting sample (see SIZE)
.VARIABLE NS
INTEGER - Number of lines (see SIZE)
.VARIABLE NL
INTEGER - Number of samples (see SIZE)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstaverag.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!  THIS IS A TEST OF PROGRAM AVERAGE
!
!     BUILD 4 INPUT IMAGES 
!
gen LOOK1  NL=3 NS=10  IVAL=0
gen LOOK2  NL=3 NS=10  IVAL=5
gen LOOK3  NL=3 NS=10  IVAL=10
!
list LOOK1
list LOOK2
list LOOK3
!
average INP=(LOOK1, LOOK2, LOOK3) OUT=(LOOK11)
list LOOK11
average INP=(LOOK1, LOOK2, LOOK3) OUT=(LOOK11)  SIZE=(2,2,2,5)
list LOOK11
!   CLEAN UP
! 
! DCL DELETE LOOK*.Z*;*
!
end-proc
$ Return
$!#############################################################################
