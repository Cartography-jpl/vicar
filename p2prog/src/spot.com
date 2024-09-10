$!****************************************************************************
$!
$! Build proc for MIPL module spot
$! VPACK Version 1.9, Friday, July 10, 1998, 15:30:21
$!
$! Execute by entering:		$ @spot
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
$ write sys$output "*** module spot ***"
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
$ write sys$output "Invalid argument given to spot.com file -- ", primary
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
$   if F$SEARCH("spot.imake") .nes. ""
$   then
$      vimake spot
$      purge spot.bld
$   else
$      if F$SEARCH("spot.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake spot
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @spot.bld "STD"
$   else
$      @spot.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create spot.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack spot.com -
	-s spot.f -
	-i spot.imake -
	-p spot.pdf -
	-t tstspot.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create spot.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C**** VERSION 3 VICAR-2 PARAMS 20 AUGUST 1984 MEM
C     VERSION 2 VICAR-2 IO     13 AUGUST 1984 MEM
C     VERSION 1 VAX CONVERSION 7 AUGUST 1984 MEM
      INCLUDE 'fortport'
      COMMON/C1/DN
      CHARACTER*8 STRING
      INTEGER OUTUNIT,IND,CNT
      INTEGER*4 DNMAX,SIGMAX,SIGMAY,X0,Y0
      BYTE DN(2000)
C
C**** PARAMETER INITIALIZATION
      NL=1024
      NS=1024
      SL=1
      SS=1
      DNMAX=255
      SIGMAX=125
      SIGMAY=125
      X0=512
      Y0=512
C**** VICAR*2 OPENS
      CALL IFMESSAGE('SPOT version 02-MAY-94')
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      CALL XVUNIT(OUTUNIT,'OUT',1,IND,' ')
      CALL XVOPEN(OUTUNIT,IND,'U_NL',NL,'U_NS',NS,'OP','WRITE',' ')
C**** NOW PROCESS THE TAE/PDF
C     TO GET THE PARAMETER VALUES
C     USE DEFAULTS IF "CNT"=0
      CALL XVP('DNMAX',DNMAX,CNT)
      CALL XVP('SIGMAX',SIGMAX,CNT)
      IF(CNT.EQ.0)SIGMAX=NS/8
      CALL XVP('SIGMAY',SIGMAY,CNT)
      IF(CNT.EQ.0)SIGMAY=NL/8
      CALL XVP('X0',X0,CNT)
      IF(CNT.EQ.0)X0=NS/2
      CALL XVP('Y0',Y0,CNT)
      IF(CNT.EQ.0)Y0=NL/2

c
c     bam 7/98 AR-9267
c
c     make sure we don't have a divide by 0
c
      if (dnmax .ne. 0 ) then
          DXMX=255/DNMAX
      else
          dxmx = 0 
      end if

      EPS=1.E-5
      CALL XVP('SHAPE',STRING,CNT)
      IF(CNT.EQ.0)ISHAPE=1
      IF(STRING.EQ.'GAUSSIAN')ISHAPE=1
      IF(STRING.EQ.'CONICAL')ISHAPE=2
      IF(STRING.EQ.'RECPROCL')ISHAPE=3
      IF(STRING.EQ.'RECPSQRE')ISHAPE=4
      IF(STRING.EQ.'EXPONENT')ISHAPE=5
      IF(STRING.EQ.'DOME')ISHAPE=6


C**** COMPUTE RADIAL DISTANCE

      DO 100 IY=1,NL
c
c     bam 7/98 AR-9267
c
c     make sure we don't have a divide by 0
c     for both sigmay and sigmax
c
      if (sigmay .ne. 0 ) then
          YARG=(IY-Y0)*1./(SIGMAY*1.)
      else
          yarg = 0 
      end if

      YARGS=YARG**2

      DO 99 IX=1,NS
          if (sigmax .ne. 0 ) then
              XARG=(IX-X0)*1./(SIGMAX*1.)
          else
              xarg = 0 
          end if

      R2=XARG**2+YARGS
      
      R=SQRT(R2)
C**** BRANCH TO SHAPE
               GO TO (51,52,53,54,55,56),ISHAPE
C     GAUSSIAN SPOT
   51 IF (R2.GE.100.) R2=100.
      FACT=EXP(-0.5*R2)
      GO TO 60
C     CONICAL SPOT
   52 FACT=1.-R
      IF (FACT.LT.0.) FACT=0.
      GO TO 60
C     RECIPROCAL SPOT
   53 IF (R.GT.EPS) TMPF=1./R
      IF ((R.EQ.0.).OR.(TMPF.GE.DXMX)) TMPF=DXMX
      FACT=TMPF
      GO TO 60
C     RECIPROCAL SQUARED SPOT
   54 IF (R2.GT.EPS) TMPF=1./R2
      IF ((R2.EQ.0.).OR.(TMPF.GT.DXMX)) TMPF=DXMX
      FACT=TMPF
      GO TO 60
C     EXPONENTIAL SPOT
   55 IF (R.GE.100.) R=100.
      FACT=EXP(-1.*R)
      GO TO 60
C     DOME SPOT
   56 AF=1.-R2
      IF (AF.LE.0.) AF=0.
      FACT=SQRT(AF)
C**** CALCULATE OUTPUT VALUE AT SAMPLE LOCATION OF EACH PIXEL
   60 IVAL = DNMAX*FACT
      DN(IX)=INT2BYTE(IVAL)
   99 CONTINUE
      CALL XVWRIT(OUTUNIT,DN,IND,' ')
  100 CONTINUE
      IF(ISHAPE.EQ.1)CALL XVMESSAGE('****GAUSSIAN PATTERN GENERATED '
     *,' ')
      IF(ISHAPE.EQ.2)CALL XVMESSAGE('****CONICAL PATTERN GENERATED '
     *,' ')
      IF(ISHAPE.EQ.3)CALL XVMESSAGE('****RECIPROCAL PATTERN GENERATED '
     *,' ')
      IF(ISHAPE.EQ.4)CALL XVMESSAGE('****RECP SQUARED PATTERN GENERATED'
     *,' ')
      IF(ISHAPE.EQ.5)CALL XVMESSAGE('****EXPONENTIAL PATTERN GENERATED'
     *,' ')
      IF(ISHAPE.EQ.6)CALL XVMESSAGE('****DOME PATTERN GENERATED ',' ')
      CALL XVCLOSE(OUTUNIT,IND,' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create spot.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM spot

   To Create the build file give the command:

		$ vimake spot			(VMS)
   or
		% vimake spot			(Unix)


************************************************************************/


#define PROGRAM	spot
#define R2LIB

#define MODULE_LIST spot.f

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
$ create spot.pdf
process help=*
PARM OUT      TYPE=STRING  
PARM SIZE     TYPE=INTEGER COUNT=4 VALID=(0:1024) DEFAULT=(1,1,1024,1024)
PARM SL       TYPE=INTEGER DEFAULT=1
PARM SS       TYPE=INTEGER DEFAULT=1
PARM NL       TYPE=INTEGER VALID=(0:1024) DEFAULT=1024
PARM NS       TYPE=INTEGER VALID=(0:1024) DEFAULT=1024
PARM SHAPE    TYPE=KEYWORD VALID=(GAUSSIAN,CONICAL,RECPROCL,+
                                  RECPSQRE,EXPONENT,DOME)+
                           DEFAULT=GAUSSIAN+
                           COUNT=(0:1)
PARM DNMAX    TYPE=INTEGER VALID=(0:255) DEFAULT=255
PARM SIGMAX   TYPE=INTEGER DEFAULT=2
PARM SIGMAY   TYPE=INTEGER DEFAULT=2
PARM X0       TYPE=INTEGER DEFAULT=6
PARM Y0       TYPE=INTEGER DEFAULT=6

!# annot function="Generating Synthetic Images"
!# annot keywords=(profiles,size,BYTE,shape,OUT,DN)
END-PROC
.TITLE
Synthesizes images of spots of various sizes and profiles
.HELP
PURPOSE

   spot is a VICAR*2 applications program to generate spots of various
   profiles and sizes. spot currently creates only byte images.

 EXECUTION:
  
  spot OUT=sp.dat SIZE=(1,1,24,24) SHAPE=GAUSSIAN X0=10 Y0=10+
  SIGMAX=1 SIGMAY=1

   This example will create a 24 x 24 data set 'SP.DAT' with
   a gaussian profile centered at line=10, sample=10.

  NOTE that non-circular symetrical spots can be created by making
  SIGMAX not equal to SIGMAY.

   The SHAPE keyword has six values;
   'GAUSSIAN' FOR GAUSSIAN spot
   'CONICAL'  FOR CONICAL spot
   'RECPROCL' FOR RECIPROCAL spot
   'RECPSQRE' FOR RECIPROCAL SQUARED spot
   'EXPONENT' FOR EXPONENTIAL spot
   'DOME'     FOR DOME spot
   Other keywords are defined in the TUTOR mode.
.page
 RESTRICTIONS:
  1. BYTE DATA ONLY.
  2. MAX SIZE IS 1024 LINE BY 1024 SAMPLES.
  3. SL=1 SS=1 ARE FIXED; THEY ARE IGNORED IN PROGRAM.

 PROGRAM HISTORY:

  28 MAR  1994  CRI  MSTP S/W CONVERSION (VICAR PORTING) 
  15 AUG  1984...M.E.MORRILL...CONVERTED TO VAX-VICAR*2
  27 JUNE 1975...D.A.HASS...CHANGES FOR CONVERSION TO 360/OS
  12 APRIL 1973...F.G.STAUDHAMMER...DOCUMENTED
  15 MARCH 1973...Y.K.CHEN...ORIGINAL RELEASE

  CURRENT COGNIZANT PROGRAMMER:  L. W. Kamp
.LEVEL1
.VARIABLE OUT
 An output data set
.VARIABLE SIZE
 The standard Vicar size field
.VARIABLE SL
 Starting line:
INTERNALLY SET=1
.VARIABLE SS
 Starting sample:
INTERNALLY SET=1
.VARIABLE NL
 Number of lines
 Valid: 1-1024
.VARIABLE NS
 Number of samples
 Valid: 1-1024
.VARIABLE SHAPE
 KEYWORD-OPTIONAL
 Valid: GAUSSIAN,CONICAL,
RECPROCAL,RECPSQRE,EXPONENT,DOME
.VARIABLE DNMAX
 INTEGER-OPTIONAL
 - Maximum DN (255)
.VARIABLE SIGMAX
 KEYWORD-OPTIONAL
 - Variance in Sample direction
 - Default NS/8
.VARIABLE SIGMAY
 KEYWORD-OPTIONAL
 - Variance in Line direction
 - Default NL/8
.VARIABLE X0
 KEYWORD-OPTIONAL
 - Sample coordinate of spot
 center relative to 1,1
 - Default NS/2
.VARIABLE Y0
 KEYWORD-OPTIONAL
 - Line coordinate of spot
 center relative to 1,1
 -Default NL/2
.LEVEL2
.VARIABLE OUT
 An output data set
.VARIABLE SIZE
 The standard Vicar size field
.VARIABLE SL
 Starting line-INTERNALLY SET=1
.VARIABLE SS
 Starting sample-INTERNALLY SET=1
.VARIABLE NL
 Number of lines
 Valid: 1-1024
.VARIABLE NS
 Number of samples
 Valid: 1-1024
.VARIABLE SHAPE
There are six options avalable. In the following 'X' is the sample
coordinate and 'Y' is the line coordinate.
  A. GAUSSIAN (DEFAULT): This specifies the spot shape to be Gaussian.
     The DN(X,Y) of the output dtat set will be given by;
     
     DN(X,Y)= DNMAX*EXP(-(X-X0)**2/(2.0*SIGMAX**2))
              *EXP(-(Y-Y0)**2/(2.0*SIGMAY**2))
  
  B. CONICAL: This specifies the spot shape to be conical;
    
     DN(X,Y)=DNMAX*(1.0-R)   Where;
    
     R=SQRT(((X-X0)/SIGMAX)**2+((Y-Y0)/SIGMAY)**2)

  C. RECPROCL: This specifies the spot shape to be reciprocal;
 
     DN(X,Y)=DNMAX*(1.0/R)   Where R is defined above.

  D. RECPSQURE: This specifies the spot shape to be the reciprocal
     squared;
 
     DN(X,Y)=DNMAX*(1.0/R**2)  Where R is defined above.

  E. EXPONENT: This specifies the spot shape to be exponential;
    
     DN(X,Y)=DNMAX*EXP(-R)  Where R is defined above.

  F. DOME: This specifies the spot shape to be dome shaped;
    
     DN(X,Y)=DNMAX*SQRT(1.0-R**2)  Where R is defined above.

NOTE THAT FOR NEGATIVE COMPUTED DN'S THE OUTPUT IS TRUNCATED TO ZERO.
.VARIABLE SIGMAX
  This specifies the variance of the DN(X,Y) distribution in the sample
  direction. For the GAUSSIAN shape, SIGMAX is the Sigma (half-width)
  of the spot. For all other spot shapes 2*SIGMAX is the entire extent
  of the spot shape in the sample direction. SIGMAX must be greater
  than 0. The default value is NS/8.
.VARIABLE SIGMAY
  This is the equivalent of SIGMAX in the line direction.
.VARIABLE X0
  This specifies the center of the spot relative to SS=1: Center=X0.
  The default is X0=NS/2.
.VARIABLE Y0
  This specifies the center of the spot relative to SL=1: Center=Y0.
  The default is Y0=NL/2.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstspot.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
write "THIS ONLY TESTS THE MODE NEEDED BY GALILEO"
write "TO CHECK OUT STARCAT"
!
spot OUT=sp.dat SIZE=(1,1,24,24)
list sp.dat 'ZEROES
!
end-proc
$ Return
$!#############################################################################
