$!****************************************************************************
$!
$! Build proc for MIPL module gridgen
$! VPACK Version 1.7, Friday, May 06, 1994, 15:42:51
$!
$! Execute by entering:		$ @gridgen
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
$ write sys$output "*** module gridgen ***"
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
$ write sys$output "Invalid argument given to gridgen.com file -- ", primary
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
$   if F$SEARCH("gridgen.imake") .nes. ""
$   then
$      vimake gridgen
$      purge gridgen.bld
$   else
$      if F$SEARCH("gridgen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gridgen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gridgen.bld "STD"
$   else
$      @gridgen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gridgen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gridgen.com -
	-s gridgen.f -
	-i gridgen.imake -
	-p gridgen.pdf -
	-t tstgridgen.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gridgen.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'

C VICAR program GRIDGEN
C Generates a perfect grid: either a MARK format grid location file, or an
C image.
C
      SUBROUTINE MAIN44

!     Program GRIDGEN local variables 
      IMPLICIT INTEGER(A-Z)
      COMMON/C1/OBUF(1200)

      INTEGER*4 GSL,GSS,DNG,DNB
      LOGICAL XVPTST,IMAGE

      INTEGER INC, CNT, NCOL, NROW, DUM, OUNIT, STAT

!     Initialize GRIDGEN local variables 
      DATA INC /0/, CNT /0/, GSL /0/, GSS /0/, NROW /0/, DNG /0/
     &     NCOL /0/, DNB /0/, DUM /0/, OUNIT /0/, STAT /0/
      DATA OBUF /1200*0/


!     Begin program GRIDGEN

      CALL IFMESSAGE ('GRIDGEN version 1 July 1994', ' ')

!     Set default error handling action
      CALL XVEACTION ('SA', ' ')
 
!     Determine if the keyword 'IMAGE' was set on input parameters
      IMAGE = XVPTST('IMAGE')

!     Obtain information about values for the indicated parameters
      CALL XVP('INC',INC,CNT)
      CALL XVP('GSL',GSL,CNT)
      CALL XVP('GSS',GSS,CNT)
      CALL XVP('NROW',NROW,CNT)
      CALL XVP('NCOL',NCOL,CNT)
      CALL XVP('DNGRID',DNG,CNT)
      CALL XVP('DNBACK',DNB,CNT)

!     If the keyword 'IMAGE' was set in the input parameters
      IF (IMAGE) GOTO 100
      NSO = NROW*NCOL*2
      IF (NSO.GT.1200) THEN
           CALL XVMESSAGE ('***Too many grid intersections', ' ')
           CALL XVMESSAGE ('***Reduce NROW or NCOL', ' ')
           GOTO 999
      ENDIF

      CALL GENGRID (OBUF,NROW,NCOL,GSL,GSS,INC)
      CALL PGRID   (OBUF,NROW,NCOL,DUM,0)
      CALL XVUNIT  (OUNIT,'OUT',1,STAT, ' ')
      CALL XVOPEN(OUNIT,STAT,'U_NL',1,'U_NS',NSO,
     *     'O_FORMAT','REAL','OP','WRITE', ' ')
      CALL XVWRIT(OUNIT,OBUF,STAT, ' ')
      GOTO 200

  100 NLO = GSL + NROW*INC - INC/2
      NSO = GSS + NCOL*INC - INC/2
      CALL XVUNIT (OUNIT,'OUT',1,STAT, ' ')
      CALL XVOPEN (OUNIT,STAT,'U_NL',NLO,'U_NS',NSO,
     *            'O_FORMAT','BYTE','OP','WRITE', ' ')

      CALL GENIMAGE (OUNIT,DNB,DNG,NLO,NSO,NROW,NCOL,GSL,GSS,INC)

  200 CALL XVMESSAGE ('GRIDGEN task completed', ' ')
      RETURN
  999 CALL XVMESSAGE ('***GRIDGEN task canceled', ' ')
      CALL ABEND
      END
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                 Subroutine to generate grid locations...
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
C
      SUBROUTINE GENGRID (OBUF,NROW,NCOL,GSL,GSS,INC)

      IMPLICIT INTEGER(A-Z)

!     Subroutine GENGRID passed parameteres
      REAL*4 OBUF(1200)
      INTEGER*4 GSL,  GSS
      INTEGER   NROW, NCOL, INC

!     Subroutine GENGRID local parameteres
      INTEGER*4 GSS0
      INTEGER   I, J, K

!     Subroutine GENGRID local parameter initialization
      DATA      I /0/, J /0/, K /0/
      DATA      GSS0 /0/

!     Begin Subroutine GENGRID
      GSS0 = GSS
      K = 1

      DO J=1,NROW	!ROWS
          DO I=1,NCOL   !COLUMNS
              OBUF(K) = GSL
              OBUF(K+1) = GSS
              K = K + 2
              GSS = GSS + INC
          ENDDO
          GSS = GSS0
          GSL = GSL + INC
      ENDDO
      RETURN
      END
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                 Subroutine GENIMAGE
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
C
      SUBROUTINE GENIMAGE(OUNIT,DNB,DNG,NLO,NSO,NROW,NCOL,GSL,GSS,INC)

      IMPLICIT INTEGER(A-Z)

!     Subroutine GENIMAGE passed parameteres
      INTEGER*4 GSL,GSS,DNG,DNB
      INTEGER   OUNIT, NLO, NSO, NROW, NCOL, INC

!     Subroutine GENIMAGE local parameteres
      COMMON/C2/PIC(1200),PIC2(1200)
      LOGICAL*1 PIC,PIC2
      INTEGER   L, STAT

!     Initialize Subroutine GENIMAGE local parameters
      DATA      STAT /0/, L /0/
      DATA      PIC /1200*0/, PIC2 /1200*0/


!     Begin Subroutine GENIMAGE
      CALL ITLA(DNB,PIC,NSO)			! Start with background
      CALL MVE(-5,NCOL,DNG,PIC(GSS),0,INC)	! Add vertical grid rulings
      CALL ITLA(DNG,PIC2,NSO)			! Generate horizontal grid

      DO L=1,NLO
          IF (L.EQ.GSL) THEN
              CALL XVWRIT(OUNIT,PIC2,STAT, ' ')
              GSL = GSL + INC
          ELSE
              CALL XVWRIT(OUNIT,PIC,STAT, ' ')
          ENDIF
      ENDDO

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gridgen.imake
#define  PROGRAM   gridgen

#define MODULE_LIST gridgen.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create gridgen.pdf
process help = *
PARM OUT    TYPE=STRING  COUNT=1
PARM IMAGE  TYPE=KEYWORD COUNT=(0:1) VALID=IMAGE DEFAULT=--
PARM INC    TYPE=INTEGER COUNT=(0:1) VALID=(1:10000) DEFAULT=40
PARM GSL    TYPE=INTEGER COUNT=(0:1) VALID=(1:10000) DEFAULT=20
PARM GSS    TYPE=INTEGER COUNT=(0:1) VALID=(1:10000) DEFAULT=20
PARM NROW   TYPE=INTEGER COUNT=(0:1) VALID=(1:800) DEFAULT=20
PARM NCOL   TYPE=INTEGER COUNT=(0:1) VALID=(1:800) DEFAULT=20
PARM DNGRID TYPE=INTEGER COUNT=(0:1) VALID=(0:255) DEFAULT=5
PARM DNBACK TYPE=INTEGER COUNT=(0:1) VALID=(0:255) DEFAULT=240
END-PROC
.TITLE
	GRIDGEN
.HELP

PURPOSE:

   GRIDGEN is VICAR applications program which generates a synthetic
   grid target consisting of uniformly spaced horizontal and vertical
   grid rulings.  The program is used for geometric calibration of
   vidicon/CCD camera systems.

EXECUTION:

        GRIDGEN OUT=GRID PARAMS

Parameters are defined in the TUTOR mode. SIZE is ingored.
.PAGE
OPERATION:

   GRIDGEN has two operating modes: (1) in the default mode, the output
   consists of a grid location file in MARK format; (2) if the keyword
   IMAGE is specified, the output consists of a gridded image.

   The grid is specified by giving the line-sample coordinates of
   the upper-left-most grid intersection (see parameters GSL and GSS), the
   grid spacing (see parameter INC), and the number of horizontal and
   vertical grid rulings (see parameters NROW and NCOL).

   The grid location file contains the line-sample coordinates of each
   grid intersection is stored in a single record as ordered pairs of floating
   point numbers: L1,S1,L2,S2,L3,S3,...,Ln,Sn where n=NROW*NCOL.
   The rows are ordered from top-to-bottom, and the columns from left-to-
   right.  The intersections of the top-most row are stored first, followed
   by the second row, etc.

   If IMAGE is specified, the user may specify the DN value of the grid
   rulings and the background (see parameters DNGRID and DNBACK).

EXAMPLES:
   The following statement will generate a mark format file containing the
   intersections of a 20x20 grid of rulings spaced 40 pixels apart (all
   values represent the defaults):

         GRIDGEN OUT INC=40 GSL=20 GSS=20 NROW=20 NCOL=20

   The following statement will generate a grid image with grid rulings of
   5 DN overlayed on a background of 240 DN:

         GRIDGEN OUT INC=40 GSL=20 GSS=20 NROW=20 NCOL=20 DNGRID=5
        +       DNBACK=240 'IMAGE

RESTRICTIONS:

   NROW*NCOL must be less than 1200.

PROGRAM HISTORY:

   01 July 1994...CRI ...........Made portable for UNIX
   21 Sep  1987...G.M.Yagi.......Extensive changes
   27 MAR  1985...M.E.MORRILL....ADD OUTPUT OF GRID IMAGE
   30 OCT  1984...M.E.MORRILL....CONVERSION TO VAX-VICAR*2
   06 MAY  1983...M.E.MORRILL....INITIAL RELEASE IBM 

   Written by: Mike Morrill
   Current cognizant programmer:  Gary Yagi

.LEVEL1
.VARIABLE OUT
 STRING-REQUIRED
 Either a grid
 location file
 (default) or a
 grid image.
.VARIABLE IMAGE
 KEYWORD-OPTIONAL
 Specifies IMAGE output.
.VARIABLE INC
 INTEGER-OPTIONAL
 Spacing between 
 intersections.
.VARIABLE GSL
 INTEGER-OPTIONAL
 Grid starting line.
.VARIABLE GSS
 INTEGER-OPTIONAL
 Grid starting sample.
.VARIABLE NROW
 INTEGER-OPTIONAL
 Number of horizontal
 grid rulings.
.VARIABLE NCOL
 INTEGER-OPTIONAL
 Number of vertical
 grid rulings.
.VARIABLE DNGRID
 INTEGER-OPTIONAL
 DN value of the grid
 rulings for IMAGE mode.
.VARIABLE DNBACK
 INTEGER-OPTIONAL
 DN value of the
 background for
 IMAGE mode.
.LEVEL2
.VARIABLE OUT
 STRING-REQUIRED
 Either a grid location file in MARK format (default) or a gridded image
 (see IMAGE keyword).
.VARIABLE IMAGE
 KEYWORD-OPTIONAL
 Specifies gridded image output.
.VARIABLE INC
 INTEGER-OPTIONAL
 Pixel spacing between intersections.
.VARIABLE GSL
 INTEGER-OPTIONAL
 GSL and GSS specify the line-sample coordinates of the upper-left-most
 grid intersection.
.VARIABLE GSS
 INTEGER-OPTIONAL
 Grid starting sample.
.VARIABLE NROW
 INTEGER-OPTIONAL
 Number of horizontal grid rulings.
.VARIABLE NCOL
 INTEGER-OPTIONAL
 Number of vertical grid rulings.
.VARIABLE DNGRID
 INTEGER-OPTIONAL
 Specifies the DN value of the grid rulings for IMAGE mode.
.VARIABLE DNBACK
 INTEGER-OPTIONAL
 Specifies the DN value of the background for IMAGE mode.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgridgen.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
gridgen OUT
gridgen OUT 'IMAGE
list OUT (15,15,10,10)
list OUT (775,775,10,10)
end-proc
$ Return
$!#############################################################################
