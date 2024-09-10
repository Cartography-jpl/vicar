$!****************************************************************************
$!
$! Build proc for MIPL module rotate2
$! VPACK Version 1.5, Wednesday, June 09, 1993, 11:49:36
$!
$! Execute by entering:		$ @rotate2
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
$ write sys$output "*** module rotate2 ***"
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
$   if F$SEARCH("rotate2.imake") .nes. ""
$   then
$      vimake rotate2
$      purge rotate2.bld
$   else
$      if F$SEARCH("rotate2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rotate2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rotate2.bld "STD"
$   else
$      @rotate2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rotate2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rotate2.com -
	-s rotate2.f -
	-i rotate2.imake -
	-p rotate2.pdf -
	-t tstrotate2.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rotate2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     PROGRAM ROTATE2
C      9 JUN 93   ...SP....   MADE PORTABLE FOR UNIX.  CHANGED TO USE XVEACTION
C                             INSTEAD OF XVCHECK.
C      4 NOV 88   ...SP....   ADDED CODE TO HANDLE CASE OF NLO=1 OR NSO=1.
C      1 MAY 84   ...CCA...   CONVERT TO VICAR2
C      1 JAN 84   ...CCA...   CONVERT TO VAX, NO VXCTL
C     25 JUL 72   ...ARG...   INITIAL VERSIONM
C      1 MAR 73   ...FGS...   CORRECT CENTER OPTION IN OUTPUT PICTURE
C     22 MAR 73   ...FGS...   INCLUDE FAKIBCOM
C     15 MAY 73   ...FGS...   DELETE PARAMETER DATA SET
C     27 JUN 75   ...DAH...   CHANGES FOR CONVERSION TO 360/OS
C     29 MAR 79   ...JAM...   INCORPORATE HALF AND SPLINE OPTIONS
C      1 APR 79   ...JAM...   SET DEFAULT ANGLE TO ZERO
C      1 APR 79   ...JAM...   REMOVE EQUIVALENCE BETWEEN KWD AND PAR
C
C ** PURPOSE... TO GENERATE GEOM PARAMETER SETS DESCRIBING PICTURE
C               ROTATIONS AND TO FETCH LGEOM. 
C
C ** TO  'GEOM' USE...
C     ROTATE2 INP SIZE=() LINE=X SAMP=X ANGL=X CENT=(L,S)
C
C         WHERE
C   LINE, SAMP, ARE THE CENTER OF ROTATION FOR THE INPUT PICTURE
C   THESE MAY BE DEFAULTED TO THE CENTER OF THE INPUT PICTURE
C
C   ANGL IS THE ROTATION IN DEGREES CLOCKWISE FROM 'UP'.
C
C   CENT IS THE LINE AND SAMPLE LOCATION OF THE CENTER OF ROTATION
C   IN THE OUTPUT PICTURE. IT CAN BE DEFAULTED TO THE CENTER OF THE
C   OUTPUT PICTURE.
C
C   THE KEYWORD 'NOIN' MAY BE USED FOR TRANSMISSION TO LGEOM
C
      REAL RPARM(16),LL,LS
      INTEGER SL,SS,STAT, NLO2, NSO2
      CHARACTER*256 PDS
      CHARACTER*8 FORMAT
      LOGICAL XVPTST
      CHARACTER*132 OUT1
      CHARACTER*132 OUT2
      DATA OUT1/' REGION (    ,     ,     ,     ) OF THE INPUT PICTURE I
     +S ROTATED         DEGREES ABOUT       ,        '/

C==================================================================

      CALL XVEACTION('SA',' ') ! SET XV ERROR ACTION

      ICENT = 0
      ILINE = 0
      ISAMP = 0
      ANG=0.
      CALL ZIA(RPARM,16)

C        OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,' ')

C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      IF(FORMAT.NE.'BYTE'.AND.FORMAT.NE.'HALF') THEN
         CALL QPRINT(' ROTATE2 ACCEPTS BYTE AND HALFWORD DATA ONLY')
         CALL ABEND
      END IF

C        GET SIZE INFORMATION 
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)

C        CLOSE INPUT DATA SET
      CALL XVCLOSE(IUNIT,STAT,' ')

C        GET NAME OF PARAMETER DATA SET
      CALL XVP('PDS',PDS,NPDS)

C           PROCESS PARAMETERS
C        'LINE'
      CALL XVPARM('LINE',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT .NE. 0) THEN
         CLI = RPARM(1)
	 CLO = CLI - SL + 1
	 ILINE = 1
      END IF

C        'SAMPLE'
      CALL XVPARM('SAMPLE',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT .NE. 0) THEN
	 CSI = RPARM(1)
	 CSO = CSI - SS + 1
	 ISAMP = 1
      END IF

C        'CENTER'
      CALL XVPARM('CENTER',RPARM,ICOUNT,IDEF,2)
      IF(ICOUNT .NE. 0) THEN
	 CLO = RPARM(1)
	 CSO = RPARM(2)
	 ICENT = 1
      END IF

C        'ANGLE'
      CALL XVPARM('ANGLE',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT .NE. 0) ANG = RPARM(1)


      NLI = MIN0(NLI+1-SL,NLO)
      NSI = MIN0(NSI+1-SS,NSO)
      WRITE (OUT1(10:13),'(I4)') SL
      WRITE (OUT1(16:19),'(I4)') SS
      WRITE (OUT1(22:25),'(I4)') NLI
      WRITE (OUT1(28:31),'(I4)') NSI
      IF(ILINE .EQ. 0) CLI=.5*(SL+NLI)
      IF(ISAMP .EQ. 0) CSI=.5*(SS+NSI)
      IF(ICENT .EQ. 0) CLO=.5*(1+NLO)
      IF(ICENT .EQ. 0) CSO=.5*(1+NSO)
      WRITE (OUT1(66:72),'(F7.1)') ANG
      WRITE (OUT1(87:92),'(F6.1)') CLI
      WRITE (OUT1(95:100),'(F6.1)') CSI
      CALL XVMESSAGE(OUT1(2:102),' ')
      WRITE (OUT2,9900) CLO,CSO
9900  FORMAT (
     +' THE CENTER OF ROTATION IN THE OUTPUT PICTURE IS LOCATED AT PIXEL
     +     ',F6.1,',  ',F6.1)
      CALL XVMESSAGE(OUT2(2:85),' ')

      ANG=-ANG*3.14159/180.
      C=COS(ANG)
      S=SIN(ANG)


      NLO2 = MAX( NLO, 2 )   ! IF NLO OR NSO IS 1, THEN USE 2 AS THE
      NSO2 = MAX( NSO, 2 )   ! ENDING TIEPOINT LOCATION SO RECTANGLE
                             ! WILL NOT BE DEGENERATE.

      DS=CSI-CSO
      DL=CLI-CLO
      FL=1.-CLO
      FS=1.-CSO
      LL=NLO2-CLO
      LS=NSO2-CSO

C        Open the parameter data set
      I = 3
      IF (XVPTST('NOINTERP')) I = I + 1
      IF (FORMAT .EQ. 'HALF') I = I + 1
      CALL XVPOPEN(STAT,I,64,PDS,'SA',IDUMMY)

C        Write out the parameters
      CALL XVPOUT(STAT,'NAH',1,'INT',1)
      CALL XVPOUT(STAT,'NAV',1,'INT',1)

      RPARM(1) = 1
      RPARM(2) = 1
      RPARM(3) = S*FS+C*FL-FL+DL + 1
      RPARM(4) = C*FS-S*FL-FS+DS + 1
      RPARM(5) = 1
      RPARM(6) = NSO2
      RPARM(7) = S*LS+C*FL-FL+DL + 1
      RPARM(8) = C*LS-S*FL-LS+DS + NSO2
      RPARM(9) = NLO2
      RPARM(10) = 1
      RPARM(11) = S*FS+C*LL-LL+DL + NLO2
      RPARM(12) = C*FS-S*LL-FS+DS + 1
      RPARM(13) = NLO2
      RPARM(14) = NSO2
      RPARM(15) = S*LS+C*LL-LL+DL + NLO2
      RPARM(16) = C*LS-S*LL-LS+DS + NSO2

      CALL XVPOUT(STAT,'TIEPOINT',RPARM,'REAL',16)

      IF(XVPTST('NOINTERP')) THEN
         CALL XVPOUT(STAT,'INTERP','NOIN','STRING',1)
      END IF

      IF(FORMAT .EQ. 'HALF') THEN
         CALL XVPOUT(STAT,'FORMAT','HALF','STRING',1)
      END IF

C        Close the parameter data set
      CALL XVPCLOSE(STAT)

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rotate2.imake
#define  PROGRAM   rotate2

#define MODULE_LIST rotate2.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create rotate2.pdf
PROCESS HELP=*
PARM 	INP	TYPE=STRING	COUNT=1
PARM	PDS	TYPE=STRING	COUNT=1         DEFAULT=ZZPAR
PARM 	SIZE	TYPE=INTEGER	COUNT=(0:4) 	DEFAULT=--
PARM 	SL	TYPE=INTEGER	COUNT=(0:1)	DEFAULT=--
PARM 	SS	TYPE=INTEGER	COUNT=(0:1)	DEFAULT=--
PARM	NL	TYPE=INTEGER	COUNT=(0:1)	DEFAULT=--
PARM	NS	TYPE=INTEGER	COUNT=(0:1)	DEFAULT=--
PARM	ANGLE	TYPE=REAL	COUNT=(0:1)	DEFAULT=--
PARM	LINE	TYPE=REAL	COUNT=(0:1)	DEFAULT=--
PARM	SAMPLE	TYPE=REAL	COUNT=(0:1)	DEFAULT=--
PARM	CENTER	TYPE=REAL	COUNT=(0:2)	DEFAULT=--
PARM	NOINTERP   TYPE=KEYWORD	COUNT=(0:1) VALID="NOINTERP" DEFAULT=--
END-PROC
.title
vicar Program ROTATE2
.help
PURPOSE:

ROTATE2 will compute the geometric transformation parameters for rotating a 
picture by any amount about a specified point.
ROTATE2 is typically not called directly by the user but rather from
procedure ROTATE, which will rotate a picture by any amount about a 
specified point.

EXECUTION:
   For the typical usage, see the HELP for procedure ROTATE.

   The following is the execution statement format for ROTATE2:
		rotate2 inp  params
   where INP,  and PARAMS	 are parameters discussed in their 
   respective sections.

OPERATION:

ROTATE2 generates parameters for LGEOM or MGEOM to rotate a picture.  These 
parameters are passed via ROTATE2's generated parameter data set.

The rotation is about an axis normal to the picture and intersecting it at
the specified pixel center of rotation.

The size field should take into account any increase in the number 
of lines and samples due to the rotation.
examples:

1) rotate2 IN par size=(1,1,100,160) line=15. samp=35. angl=24.2
----This example will set up to rotate the 100x160 sample file by 24.2 degrees
    about the pixel at line 15 and sample 35.

2) rotate2 IN par size=(1,1,100,160) angl=24.2
----This example does the same but about the center of the picture.

3) rotate2 IN par angl=-1. center=(50.,30.)
----This example will set up to  rotate IN by -1. degrees about its center and
    translate the rotated picture so that the center of rotation in the 
    output occupies line 50, sample 30.
.page
 LIMITATIONS
  1. The input file must be either BYTE or HALFWORD.
.page

 ORIGINAL PROGRAMMER:    A. R. Gillespie, 25 Jul 1972
 COGNIZANT PROGRAMMER:   Steve Pohorsky              
 PORTED TO UNIX: Steve Pohorsky

 REVISION HISTORY
  93-6-8    SP   Made portable for UNIX.
.level1
.vari inp
The data file to be rotated.
.vari pds
The output parameter data set.
.vari size
The area to be rotated.
.vari sl
The starting line of the size 
field.
.vari ss
The starting  sample of the 
size field.
.vari nl
The number of lines in the 
size field.
.vari ns 
The number of samples in the 
size field.
.vari angle
Amount of rotation in degrees.
.vari line
The line number of the center 
of rotation.
.vari sample
The sample of the center of 
rotation.
.vari center
The location of the 
output center of rotation.
.vari nointerp
Indicates no interpolation.
.level2
.vari inp
A VICAR labelled image to be rotated.
.vari pds
An output file of GEOM parameters in "parms" format.  See LGEOM or MGEOM 
documentation.  (The default for this parameter data set is a temporary file 
with the name ZZPAR)
.vari size
The size field indicates which area of the input image is to be 
rotated.  The NL and NS parameters specify the size of the output image.
.vari sl
The starting line of the size field.
.vari ss
The starting sample of the size field.
.vari nl
The number of lines in the size field.  Also, the number of lines in the
output image.
.vari ns
The number of bytes in the size field.  Also, the number of bytes in the 
output image.
.vari angle
This specifies the number of degrees clockwise from up to rotate the image. 
May be positive or negative. 
Default = 0.
.vari line
This is the line number of the center of rotation in the input image.
Default = .5 * (sl + nl) ...the center line of the picture.
.vari sample
This is the sample number of the center of rotation in the input image.
Default = .5 * (ss + ns) ...the center sample of the picture.  Values are
in samples, not bytes.
.vari center
This specifies the center of rotation in the output image.  Default is the 
same as that specified for or defaulted for the input image.  (Note that the
sample values are expressed in units of pixels, not bytes.)
.vari nointerp
This specifies that no interpolation is to be performed during the GEOM.
In this case, the DN value of the point closest to the fractional line and
sample is used.  This method ("nearest neighbor") is somewhat faster, but is
not as accurate as the four point interpolation.  Default = NOINTERP.
$ Return
$!#############################################################################
$Test_File:
$ create tstrotate2.pdf
PROCEDURE
refgbl $echo
body
let _onfail="continue"
WRITE "THIS IS A TEST OF MODULE ROTATE"
WRITE "WE WILL ROTATE A GEN'D IMAGE BY -45 DEG SUCH THAT"
WRITE "SHADING SHOULD APPEAR IN THE SAMPLE DIRECTION ONLY"
let $echo="yes"
gen A NL=15 NS=15 IVAL=90
list A
rotate A B ANGLE=-45. IDSNAM=IDS.DAT IDSNS=1000
list B
WRITE "SHIFT THE OUTPUT CENTER OF ROTATION AND USE NOINTERP"
rotate A B ANGLE=-45. 'NOIN CENTER=(8,4)
list B
WRITE "NOW LETS ROTATE ABOUT (10,6) [104 DN]"
WRITE " AND MAKE IT END UP AT (3,3)....AND IN HALFWORD"
gen C NL=15 NS=16 IVAL=90 'HALF
list C
rotate C D ANGLE=-45. LINE=10 SAMP=6 CENTER=(3,3)
list D
end-proc
$ Return
$!#############################################################################
