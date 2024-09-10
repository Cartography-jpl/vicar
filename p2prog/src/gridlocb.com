$!****************************************************************************
$!
$! Build proc for MIPL module gridlocb
$! VPACK Version 1.9, Monday, April 13, 1998, 10:43:15
$!
$! Execute by entering:		$ @gridlocb
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
$ write sys$output "*** module gridlocb ***"
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
$ write sys$output "Invalid argument given to gridlocb.com file -- ", primary
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
$   if F$SEARCH("gridlocb.imake") .nes. ""
$   then
$      vimake gridlocb
$      purge gridlocb.bld
$   else
$      if F$SEARCH("gridlocb.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gridlocb
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gridlocb.bld "STD"
$   else
$      @gridlocb.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gridlocb.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gridlocb.com -
	-s gridlocb.f -
	-i gridlocb.imake -
	-p gridlocb.pdf -
	-t tstgridlocb.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gridlocb.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  REVISION HISTORY
C     5-95  VRU  ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)
C     7-97  RRD  ADDED NEW LABEL ITEMS FOR GRID SIZE 
      INCLUDE 'VICMAIN_FOR'
C Geometric Calibration Program "gridlocb"
C Locates grid intersections to sub-pixel accuracy given initial positions...
C           gridlocb (f.dat,tf.dat,iloc.dat) oloc NHOR=20 NVER=20
C
C Comment:  GOTO statements are neat!
C
      SUBROUTINE MAIN44
      PARAMETER (NPOINT = 7)
      COMMON/C1/LABEL,PAIRS(2,2500)

      REAL*4 A(2,2),WORK(2,NPOINT)
      INTEGER IUNIT(2),NL(2),NS(2),OUNIT,SAMP
      INTEGER*2 ARAY1(1024,1024), ARAY2(1024,1024),IBUFFER1(1024)
      CHARACTER*4320 LABEL
      CHARACTER*86 MSG1
      CHARACTER*72 MSG2

      CALL IFMESSAGE('GRIDLOCB version 14-JULY-97')
      MSG1 = 'NO ZERO CROSSING FOUND SEARCHING RECORD ****** OF FILTRD D
     &ATA SET * NEAR PIXEL ******'
      MSG2 = ' INTERSECTION AT LINE ********** SAMPLE ********** *******
     &*** **********'

C          Open the (filtered) input grid target images...
C            IUNIT(1) = filtered data set
C            IUNIT(2) = transposed/filtered data set
      DO I=1,2
          CALL XVUNIT(IUNIT(I),'INP',I,ISTAT,' ')
          CALL XVOPEN(IUNIT(I),ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
          CALL XVGET(IUNIT(I),ISTAT,'NL',NL(I),'NS',NS(I),' ')
          CALL LABPROC(IUNIT(I),LABEL,NLAB)
      ENDDO
      DO 480 L = 1,NL(1)
        CALL XVREAD(IUNIT(1),IBUFFER1,ISTAT,' ')
        DO 470 LL = 1,NS(1)
          ARAY1(LL,L) = IBUFFER1(LL)
470     CONTINUE
480   CONTINUE
      DO 481 L = 1,NL(2)
        CALL XVREAD(IUNIT(2),IBUFFER1,ISTAT,' ')
        DO 471 LL = 1,NS(2)
          ARAY2(LL,L) = IBUFFER1(LL)
471     CONTINUE
481   CONTINUE
C
      I = 3
C            Read input (approximate) grid locations (mark format)
      CALL XVUNIT(INP,'INP',I,ISTAT,' ')
      CALL XVOPEN(INP,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL LABPROC(INP,LABEL,NLAB)
      CALL XVREAD(INP,PAIRS,ISTAT,' ')

	NHOR = 0
	NVER = 0
C---------GET GRID SIZE FROM INTERLOC MARK FILE LABEL
	  CALL XLGET(INP,'HISTORY','GRID_NROW',NHOR,IST,'HIST',
     1                'INTERLOC','INSTANCE',1,' ')
	  IF (IST .NE. 1) CALL XVMESSAGE('GRID_NROW NOT FOUND IN LABEL',
     1                                   ' ')

	  CALL XLGET(INP,'HISTORY','GRID_NCOL',NVER,IST,'HIST',
     1                'INTERLOC','INSTANCE',1,' ')
	  IF (IST .NE. 1) CALL XVMESSAGE('GRID_NCOL NOT FOUND IN LABEL',
     1                                   ' ')
C
C-------OVERRIDE WITH GRID SIZE FROM PARAMETERS
	CALL XVPARM('NHOR',NH,ICNTH,IDEF,0)
	IF (ICNTH .EQ. 1) NHOR=NH
	CALL XVPARM('NVER',NV,ICNTV,IDEF,0)
	IF (ICNTV .EQ. 1) NVER=NV
C
      NUMBER = NHOR*NVER
      IF (NUMBER .EQ. 0) GO TO 991
C
C Do for each intersection coordinate pair in the mark file
C
      DO 100 I=1,NUMBER
      LINE = PAIRS(1,I)
      SAMP = PAIRS(2,I)
      IF (PAIRS(1,I).EQ.-99.) GOTO 94

      DO 90 J=1,10

      DO 80 K=1,2      ! K=1 regular image, K=2 transposed image

      IF (K.EQ.1) THEN
           ITEM = SAMP
           IREC = LINE
      ELSE
           ITEM = LINE
           IREC = SAMP
      ENDIF
C           Find zero crossing for each point...
      IF (K .EQ. 1) THEN
      CALL FINDZERO(ARAY1,NL(K),NS(K),
     &         IREC,ITEM,NPOINT,WORK,*92)
      ENDIF
      IF (K .EQ. 2) THEN
      CALL FINDZERO(ARAY2,NL(K),NS(K),
     &         IREC,ITEM,NPOINT,WORK,*92)
      ENDIF
C           Fit points to straight line and return slope and offset...
      CALL FITLINE(WORK,NPOINT,A(1,K),A(2,K))
   80 CONTINUE
C           Solve the two equations simultaneously to find intersection...
      XA = (A(1,1)*A(2,2) + A(2,1)) /(1. - A(1,1)*A(1,2))
      YA = (A(1,2)*A(2,1) + A(2,2)) /(1. - A(1,1)*A(1,2))
      LX = NINT(XA)
      LY = NINT(YA)
C
      IF (LX.EQ.SAMP.AND.LY.EQ.LINE) GOTO 95
      LINE = LY
      SAMP = LX
      IF (J.EQ.10) CALL XVMESSAGE('*** Over 10 iterations',' ')
   90 CONTINUE

      GOTO 95
C
C            Here if no zero crossing found...
   92 WRITE(MSG1(40:46),'(I6)') LINE
      WRITE(MSG1(67:67),'(I1)') K
      WRITE(MSG1(80:85),'(I6)') SAMP
      CALL XVMESSAGE(MSG1(1:86),' ')
C            Flag intersections as missing...
   94 YA = -99.
      XA = -99.

C            Store refined intersection...
   95 PAIRS(1,I) = YA
      PAIRS(2,I) = XA
      WRITE(MSG2(23:32),'(F10.3)') YA
      WRITE(MSG2(41:50),'(F10.3)') XA
      WRITE(MSG2(52:61),'(I10)') LINE
      WRITE(MSG2(63:72),'(I10)') SAMP
      CALL XVMESSAGE(MSG2,' ')

  100 CONTINUE
C
C          Write refined intersections to output file...
      CALL XVUNIT(OUNIT,'OUT',1,ISTAT,' ')
      CALL XVOPEN(OUNIT,ISTAT,'OP','WRITE','U_FORMAT','REAL',
     &          'OPEN_ACT','SA','IO_ACT','SA',
     &          'O_FORMAT','REAL','U_NL',1,'U_NS',2*NUMBER,' ')

C-----ADD THE GRID SIZE TO THE VICAR LABEL
      CALL XLADD(OUNIT,'HISTORY','GRID_NROW',NHOR,ISTAT,'FORMAT','INT',
     &           ' ')
      CALL XLADD(OUNIT,'HISTORY','GRID_NCOL',NVER,ISTAT,'FORMAT','INT',
     &           ' ')

C-----Write refined intersections to output file...
      CALL XVWRIT(OUNIT,PAIRS,ISTAT,' ')
      CALL XVCLOSE(OUNIT,ISTAT,' ')
      CALL XVMESSAGE('GRIDLOCB task completed',' ')
      RETURN
C
991	CALL XVMESSAGE('UNKNOWN GRID SIZE',' ')
	CALL ABEND
      END
C Routine to find the zero crossing for each point along the line
C Output: WORK will contain (line,samp) coordinates for NPOINTs.
C
      SUBROUTINE FINDZERO(PIC,NL,NS,IREC,ITEM,NPOINT,WORK,*)
      INTEGER*2 PIC(1024,1024)
      REAL*4 WORK(2,NPOINT)
      INTEGER SAMP,SAMP0

      LINE = IREC
      IF (IREC.LT.1.OR.IREC.GT.NS) RETURN1
      LMAX = MIN0(IREC+NPOINT/2,NL)
      LINE = MAX0(LMAX-NPOINT+1,1)
      SAMP0 = ITEM
C
      DO 70 L=1,NPOINT
      SAMP = SAMP0
C
   60 D1 = PIC(SAMP,LINE)
      D2 = PIC(SAMP+1,LINE)
C
      IF (D1.EQ.0) THEN
           SAMP0 = SAMP			!Exactly on zero crossing
           WORK(1,L) = LINE
           WORK(2,L) = SAMP0
           GOTO 70
      ENDIF

      IF (D1.GT.0.AND.D2.LT.0) THEN
           R = SAMP - D1/(D2-D1)	!Interpolate to find zero crossing...
           WORK(1,L) = LINE
           WORK(2,L) = R
           SAMP0 = R
           GOTO 70
      ENDIF

      IF (D1.GT.0) SAMP=SAMP+1
      IF (D1.LT.0) SAMP=SAMP-1
      IF(IABS(SAMP-SAMP0).LT.6
     &      .AND.SAMP.GT.0.AND.SAMP.LE.NS) GOTO 60
      RETURN1

   70 LINE = LINE + 1

      RETURN
      END
C Routine to fit points to a straight line and return the slope and offset
C 
      SUBROUTINE FITLINE(WORK,NPOINT,SLOPE,OFFSET)
      REAL*4 WORK(2,NPOINT)
      REAL*8 XMEAN,YMEAN,XY,SQ
C         Fit points to a straight line: Y = mX + b...
      XMEAN = 0.
      YMEAN = 0.
      XY = 0.
      SQ = 0.

      DO L=1,NPOINT
          XMEAN = XMEAN + WORK(1,L)
          YMEAN = YMEAN + WORK(2,L)
          XY = XY + WORK(1,L)*WORK(2,L)
          SQ = SQ + WORK(1,L)**2
      ENDDO

      D = NPOINT*SQ - XMEAN**2
      SLOPE = (NPOINT*XY-XMEAN*YMEAN)/D
      OFFSET = (SQ*YMEAN-XMEAN*XY)/D
      RETURN
      END
      SUBROUTINE LABPROC(IUNI,LABEL,NLAB)
      IMPLICIT INTEGER(A-Z)
      INTEGER INSTANCES(20)
      CHARACTER*32 TASKS(20)
      CHARACTER*4320 LABEL
      CHARACTER*132 MSG
      CHARACTER*12 UNAME
      CHARACTER*28 TIME
      CHARACTER*65 HBUF

      HBUF = '----TASK:------------USER:--------------------------------
     &------'
      MSG = ' '
      LABEL = ' '
      CALL VIC1LAB(IUNI,STAT,NLAB,LABEL,0)
      CNT=20                             !EXTRACTS VIC*2 LAB
      CALL XLHINFO(IUNI,TASKS,INSTANCES,CNT,STAT,' ')
      DO 801 J=1,CNT
      UNAME = ' '
      TIME = ' '
      CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',TASKS(J),
     *'INSTANCE',INSTANCES(J),'FORMAT','STRING',' ')
      IF (STAT .NE. 1) CALL MABEND('ERROR:  BAD STAT')
      CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',TASKS(J),
     *'INSTANCE',INSTANCES(J),'FORMAT','STRING',' ')
      IF (STAT .NE. 1) CALL MABEND('ERROR:  BAD STAT')
      HBUF(10:17) = TASKS(J)
      HBUF(27:38) = UNAME
      HBUF(39:64) = TIME
801   LABEL(1+(NLAB+J-1)*72:1+(NLAB+J-1)*72+64) = HBUF
      NLAB=NLAB+CNT
      DO 800 I=1,NLAB
      MSG = LABEL(1+(I-1)*72:1+(I-1)*72+71)
      CALL XVMESSAGE(MSG,' ')
800   MSG = ' '   
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gridlocb.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM gridlocb

   To Create the build file give the command:

		$ vimake gridlocb			(VMS)
   or
		% vimake gridlocb			(Unix)


************************************************************************/


#define PROGRAM	gridlocb
#define R2LIB

#define MODULE_LIST gridlocb.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create gridlocb.pdf
PROCESS 	HELP=*
  PARM INP	TYPE=STRING	COUNT=3
  PARM OUT	TYPE=STRING	COUNT=1
  PARM NHOR	TYPE=INTEGER		COUNT=1
  PARM NVER	TYPE=INTEGER		COUNT=1
END-PROC
.TITLE
VICAR2 PROGRAM "gridlocb"
.HELP
PURPOSE

	"gridlocb" locates the intersections of grid rulings in an image to
	sub-pixel accuracy.  The image is nominally a rectilinear grid
	network of horizontal and vertical rulings.  The primary use of
	the program is in the geometric calibration of vidicon (and possibly
	CCD) camera systems.

.PAGE
VICAR2 COMMAND LINE FORMAT

	gridlocb INP=(f.dat,tf.dat,marka.dat) OUT=markb NHOR=n NVER=m

  where...

      f.dat	is a version of the grid image, filtered to enhance the
		vertical grid rulings.
      tf.dat	is a transposed version of the grid image, filtered to
		enhance the horizontal grid rulings.
      marka.dat	contains nominal positions of each grid intersection,
                accurate to within 2 pixels.	    
      markb	will contain the final location of each intersection,
		accurate to within 0.1 pixel (more or less).
      n 	is the number of horizontal grid rulings in the image.
      m 	is the number of vertical grid rulings in the image.

  Both f.dat and tf.dat are input in 16-bit integer format (HALF).  marka.dat
  and markb contain (line,sample) pairs in REAL*4 format suitable for input
  to the program "mark".

.PAGE
OPERATION

	The imaged grid pattern is assumed to consist of dark (low dn)
	grid rulings on a light (high dn) background.  The grid pattern
	should be reasonably oriented in a vertical-horizontal direction,
	although small rotations may be tolerated.

	"gridlocb" requires, as input, two filtered versions of the imaged
	grid, and nominal locations of each intersection (accurate to 
	within 2 pixels).

	The first filtered version (f.dat) may be generated via "filter"
        using a	49x7 filter with identical weights 3,2,1,0,-1,-2,-3 for each
	line.  This filter will enhance the vertical grid rulings, such
	that pixels immediately to the left of each grid ruling are
	positive and pixels immediately to the right are negative.
	"gridlocb" will search for a zero DN value or a positive to negative
	transition along each line segment.  If a positive to negative
	transition is located, then the zero DN point is interpolated.
	Note that the filtered output has to be in 16-bit (HALF) format
	to preserve negative DN values.

	The second filtered version (tf.dat) is generated by first transposing
	the grid image (using "flot" with keyword TRANS) and applying the
	filter above to enhance the horizontal grid rulings.

	The nominal grid locations (marka.dat) may be generated via "gridloca",
	or "interloc", or "starcat".  "fixloc" may be used to correct or flag
	bad locations.

	"gridlocb" will locate the vertical and horizontal grid rulings by
	locating their zero-crossings at each point within a 7-pixel
	diameter of each nominal intersection (7 points are acquired in
	each direction).  A least squares fit is applied over these points
	to determine (local) equations for the vertical and horizontal
	lines.  The intersection is then solved for simultaneously.

	"gridlocb" will reject an intersection if its initial or final position
	is outside the image or if either vertical or horizontal grid
	rulings cannot be located.  Rejected intersections are flagged
	as (-99.0,-99.0).

.PAGE
EXAMPLE
	Let 'raw' be the raw version of the imaged grid target.  Filtered
	version f.dat may be generated as follows:
	
	filter raw f.dat 'HALF DNMIN=-32768 DNMAX=32767 +
	NLW=49 NSW=7 'NONSYM WEIGHTS=( +
		3,2,1,0,-1,-2,-3,+
		3,2,1,0,-1,-2,-3,+
		  .   .   .  
		  .   .   .
		3,2,1,0,-1,-2,-3 )

	Filtered version tf.dat may be generated by running "flot",
		flot raw TRAN 'TRANS
	followed by the filter above.

	The generation of the input locations (marka.dat) is messy.  See
	"gridloca", "interloc", or "starcat".

	After running "gridlocb",
		gridlocb (f.dat,tf.dat,marka.dat) markb NHOR=20 NVER=21
	the resulting locations may be listed,
		fixloc markb NC=21
	or marked for display:
		mark (raw,markb) out
.PAGE
WRITTEN BY: Arnie Schwartz circa 1972

COGNIZANT PROGRAMMER:  Gary Yagi

REVISION HISTORY:
  13 Apr 1998   R. Patel     Modified tst pdf to include correct path
                             for test data.
  14 Jul 1997   R. Dudley       Added new label items for grid size.
  8  May 1995   V. Unruh ... (CRI) Made portable for UNIX
  28 Aug 1987	G.M. Yagi	Use array I/O.  Replace FORTRAN77 with GOTO.
  circa  1985   D. Meyers	VAX conversion.  Rewritten in FORTRAN77.

.LEVEL1
.VARIABLE INP
INP=(f.dat,tf.dat,marka.dat) where f.dat and tf.dat are the filtered and
transposed+filtered grid targets and marka.dat are the nominal locations of
each grid intersection.
.VARIABLE OUT
Output will contain the located grid intersections as (line,sample) pairs.
.VARIABLE NHOR
Number of horizontal grid rulings.
.VARIABLE NVER
Number of vertical grid rulings.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgridlocb.pdf
procedure
! To run on UNIX,   Type tstgridlocb "/project/it/testdata/gll/"
! To run on AXP     Move the test files f.dat, tf.dat, m2.dat, raw.dat, 
! or the VAX,       and marka.dat to your directory, and then type - 
!                   tstgridlocb ""
refgbl $echo
refgbl $autousage
refgbl $syschar
PARM  DIR   TYPE=STRING INIT="/project/test_work/testdata/cassini/iss/"
LOCAL fdat  TYPE=STRING
LOCAL tfdat TYPE=STRING
LOCAL m2dat TYPE=STRING
LOCAL marka TYPE=STRING
LOCAL raw   TYPE=STRING
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
let fdat="&DIR"//"f.dat"
let tfdat="&DIR"//"tf.dat"
let m2dat="&DIR"//"m2.dat"
let marka="&DIR"//"marka.dat"
let raw="&DIR"//"raw.dat"
if($syschar(1)="VAX_VMS")
  let DIR="wms_test_work:[testdata.cassini.iss]"
endif

gridlocb (&fdat,&tfdat,&marka) markb NHOR=21 NVER=21
fixloc markb NC=21
!    Regression test: compare w/previous result 
fixloc &m2dat NC=21
!    To display the results, look at 'out' on IDX
! mark (&raw,markb) out
end-proc
$ Return
$!#############################################################################
