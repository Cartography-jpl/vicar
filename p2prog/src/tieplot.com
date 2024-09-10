$!****************************************************************************
$!
$! Build proc for MIPL module tieplot
$! VPACK Version 1.8, Tuesday, June 13, 1995, 15:43:13
$!
$! Execute by entering:		$ @tieplot
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
$ write sys$output "*** module tieplot ***"
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
$ write sys$output "Invalid argument given to tieplot.com file -- ", primary
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
$   if F$SEARCH("tieplot.imake") .nes. ""
$   then
$      vimake tieplot
$      purge tieplot.bld
$   else
$      if F$SEARCH("tieplot.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tieplot
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tieplot.bld "STD"
$   else
$      @tieplot.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tieplot.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tieplot.com -
	-s tieplot.f -
	-i tieplot.imake -
	-p tieplot.pdf -
	-t tsttieplot.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tieplot.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'            
      SUBROUTINE MAIN44
C  IBIS MOSAIC TIEPOINT PLOTTING ROUTINE   A. ZOBRIST
C		REVISED		K.F. EVANS  MARCH 1986
C   7-10-95  JCT   (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C

      IMPLICIT  INTEGER(A-Z)
      INTEGER   DUMMY, NL, NS
      INTEGER   IBIS_RD, UNIT, STATUS
      REAL      FSCALE, X,Y,DX,DY, PT
      REAL*4    ROWBUF(7)                
      INTEGER*4 FRAME(20)
      DIMENSION DCOL(7)
      CHARACTER*24 MSG
      CHARACTER*24 POSTSCRIPTFILENAME
      CHARACTER*10 YAXISTITLE
      CHARACTER*10 XAXISTITLE
      CHARACTER*24 HEADERTITLE(2)
C
      CALL IFMESSAGE('TIEPLOT version 10 July 95')
      CALL XVEACTION('SA',' ')

      YAXISTITLE = 'LINE'
      XAXISTITLE = 'SAMPLE'
      HEADERTITLE(1) = 'TIEPOINT PLOT' 
      HEADERTITLE(2) = 'FRAME  X  SCALE  XXXX'
      postscriptfilename = 'tieplot.psf'
C     Open IBIS file for 'read'
      CALL XVUNIT(UNIT, 'INP', 1, STATUS, ' ')
      CALL IBIS_FILE_OPEN (UNIT,IBIS_RD,'READ',0,0,' ',' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)
      CALL IBIS_FILE_GET(IBIS_RD,'NC',NCOL,1,1)
      CALL IBIS_FILE_GET(IBIS_RD,'NR',CLEN,1,1)

      CALL XVP ('NL', NL, DUMMY)
      CALL XVP ('NS', NS, DUMMY)
      CALL XVPARM ('KEYCOL', DCOL(1), DUMMY, KEYCDF,7)
      CALL XVP ('OLDCOLS', DCOL(4), DUMMY)
      CALL XVP ('NEWCOLS', DCOL(6), DUMMY)
      CALL XVPARM ('NUMCOL', DCOL(2), DUMMY, NUMBDF,7)
      CALL XVPARM ('CHARCOL', DCOL(3), DUMMY, CHARDF,7)
      CALL XVPARM ('ICHAR', SYMB, DUMMY, ICHADF,1)
      CALL XVP ('SCALE', FSCALE, DUMMY)
      CALL XVP ('KEY',FRAME,FCOUNT)
      IF (KEYCDF.EQ.1)  FCOUNT = 1
C

      call plotfn (postscriptfilename)
      call xrtbegin (status) 
      if (status .ne. 1) goto 9999
      CALL HEADER (HEADERTITLE, 2, 1)
      CALL AXESTITLES (XAXISTITLE, YAXISTITLE, 270, ' ', 0)
      CALL AXESREVERSE (0, 1)  ! Set X normal(0), set Y Reverse(1)

C     Open IBIS record for read
      CALL IBIS_RECORD_OPEN (IBIS_RD,IBIS_RECORD,' ',
     &                       dcol,7,'REAL',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)

      DO 100 IBIG = 1, FCOUNT
	     WRITE (msg, '(A,I4,A,F6.1)') 'FRAME',FRAME(IBIG), 
     &                    '  SCALE ',FSCALE
         HEADERTITLE(2) = MSG
         CALL HEADER (HEADERTITLE, 2, 1)
         call setactiveset (1)
         DO 20 IW = 1, CLEN
            CALL IBIS_RECORD_READ (IBIS_RECORD, ROWBUF, IW, STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(IBIS_RD,STATUS,1)
            FR = NINT(ROWBUF(1))
            IF (KEYCDF.EQ.0 .AND. FR.NE.FRAME(IBIG)) GO TO 20
            Y = (ROWBUF(4))
            X = ROWBUF(5)
            DY = -(ROWBUF(6)-ROWBUF(4)) *FSCALE
            DX = (ROWBUF(7)-ROWBUF(5)) *FSCALE
            PT = ROWBUF(2)
            IF (NUMBDF.EQ.0)  CALL NUMBER (X,Y, .10, PT, 0., -1)
            IF (CHARDF.EQ.0)  SYMB = NINT(ROWBUF(3))
            IF (CHARDF.EQ.0 .OR. ICHADF.EQ.0)
     * 			      CALL SYMBOL (X,Y, .10, ' ',SYMB, 0., -1)
            CALL PLOT (X,Y, 3)
            CALL PLOT (X+DX,Y+DY, 2)
 20	 CONTINUE
C        If loop count is complete terminate Xrt/graph else page 
         IF (IBIG .EQ. FCOUNT) THEN
            CALL PLOT (0., 0., 999)
         ELSE
            call xrtpage (status)
            if (status .ne. 1) goto 9999
         ENDIF
 100  CONTINUE
C
9999  continue
      CALL IBIS_FILE_CLOSE (IBIS_RD,'UDELETE',STATUS)
      RETURN
      END
C
C******************************************************
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tieplot.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM tieplot

   To Create the build file give the command:

		$ vimake tieplot			(VMS)
   or
		% vimake tieplot			(Unix)


************************************************************************/


#define PROGRAM	tieplot
#define R2LIB

#define MODULE_LIST tieplot.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport
/* #define LIB_CPLT*/
#define LIB_MOTIF
#define LIB_XRT_GRAPH
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create tieplot.pdf
PROCESS      HELP=*
! TIEPLOT PDF - VICAR/IBIS MOSAIC SOFTWARE
! VICAR2/MIPL VERSION
PARM INP     STATUS=(STRING,72)
PARM NL      STATUS=INTEGER
PARM NS      STATUS=INTEGER
PARM NEWCOLS STATUS=INTEGER COUNT=2 DEFAULT=(1,2)
PARM OLDCOLS STATUS=INTEGER COUNT=2 DEFAULT=(3,4)
PARM NUMCOL  STATUS=INTEGER DEFAULT=1
PARM CHARCOL STATUS=INTEGER DEFAULT=1
PARM ICHAR   STATUS=INTEGER DEFAULT=0
PARM SCALE   STATUS=REAL   DEFAULT=8
PARM KEY     STATUS=INTEGER  COUNT=(1:20) DEFAULT=0
PARM KEYCOL  STATUS=INTEGER DEFAULT=1
PARM NODISP  STATUS=KEYWORD COUNT=(0,1) VALID=NODISP DEFAULT=--

END-PROC
.TITLE
VICAR/IBIS Program TIEPLOT
.HELP
PURPOSE

     TIEPLOT plots tiepoints in an IBIS interface file by
     drawing vectors to indicate the direction and amount of
     shift between the old (line,sample)  new (line,sample)
     pairs.  The image area is outlined and labeled.  A number
     or symbol from the interface file may be plotted at each 
     tiepoint position.

     
TAE COMMAND LINE FORMAT

     TIEPLOT INP=A PARAMS
     A                   is an IBIS interface file.     
     PARAMS              is a standard VICAR parameter field.

OPERATION

     For each tiepoint that has the correct entry in the
     KEYCOL, a vector is drawn with a length proportional
     to the distance between the old (line,sample) coordinates
     and the new (line,sample) coordinates.
     
.PAGE
EXAMPLES

TIEPLOT TIEPOINTS.INT NL=1056 NS=1204  SCALE=10

In this example, just one plot will be produced since the KEYCOL parameter
has not been specified.  The default columns for the tiepoints (columns 
1, 2, 3, and 4,  same as output by PICMATCH) will be assumed.  The length
of the offset vectors will be magnified by a factor of 10, and no
symbols or numbers will label the vectors. The plot output will be 
displayed.



IGENER OUT=I.DAT NCOL=7 LCOL=30
MF INP=I.DAT FUNC=("C1=AINT(INDEX/16)+1","C2=100*INDEX", "C3=100*INDEX", +
		"C4=C2+SIN(INDEX/5.)","C5=C3-2*COS(INDEX/5.)","C6=INDEX")

TIEPLOT INP=I.DAT NL=3000 NS=3000 KEYCOL=1 KEY=(1,2)+
  SCALE=20 NEWCOLS=(4,5) OLDCOLS=(2,3) NUMCOL=6 +
  'NODISP

In this example an IBIS file of 7 columns and 30 rows is created,
it is divided into 2 subsets of 15 rows each (control column C1
has 1 in the first 15 rows and 2 in the next 15 rows). Old coordinates
are in columns 2 and 3, new coordinates are in columns 4 and 5.
Number of each point is contained in column 6. Two plots, each con-
taining vectors for 15 points will be generated. Keyword NODISP indicates
that the plot output is not to be displayed, but sent directly to
the output PostScript file.


Original Programmer:  A. L. Zobrist       10 October 1980

Cognizant Programmer:  K. F. Evans

Revision:  2			March 1986

         Made portable for UNIX and XRT/graph  J. Turner (CRI) 8 May 95

.LEVEL1
.VARIABLE INP
Input IBIS interface file
.VARIABLE KEYCOL
Control column
.VARIABLE KEY
Keys in the control column
.VARIABLE SCALE
Magnification factor for shifts
.VARIABLE NL
Size of an area in lines
.VARIABLE NS
Size of an area in samples
.VARIABLE NEWCOLS
Columns of new (line,sample)
.VARIABLE OLDCOLS
Columns of old (line,sample)
.VARIABLE NUMCOL
Columns of identifying numbers
.VARIABLE CHARCOL
Column containing Calcomp
special symbol numbers
.VARIABLE ICHAR
Calcomp special symbol number
.LEVEL2
.VARIABLE INP
     INP=A               Input IBIS interface file
.VARIABLE KEYCOL
     KEYCOL=N            The  integer N specifies a  control 
                         column  for selecting a  subset  of 
                         the data for plotting.
.VARIABLE KEY
     KEY=(K1,...,KM)     The  integers  K1,...,KM  specifies 
                         which  keys  in the control  column 
                         are subsetted for plotting.
.VARIABLE SCALE
     SCALE=R             The floating decimal R specifies  a 
                         magnification   factor  to   be 
                         applied to the shifts when plotted.
.VARIABLE NL
     NL=P                The  integer P specify the size  of 
                         the area being plotted in lines. 
.VARIABLE NS
     NL=Q                The  integer Q specify the size  of 
                         the area being plotted in samples.
.VARIABLE NEWCOLS
     NEWCOLS=(A,B)       The  integers A and B  specify  the 
                         columns  of  new coordinates.
.VARIABLE OLDCOLS
     OLDCOLS=(C,D)       The  integers C and D  specify  the 
                         columns   of  old coordinates.
.VARIABLE NUMCOL
     NUMCOL=E            The  integer E specifies  a  column 
                         that  contains identifying  numbers 
                         to  be plotted beside each  vector.  
                         If this keyword is omitted, then no 
                         numbers are plotted.
.VARIABLE CHARCOL
     CHARCOL=F           The  integer  F specifies a  column 
                         that contains the number of the 
			 identifying symbol to be plotted as 
			 the base of each vector.  The numbers
			 should be in the range of 0 to 15.
                         If this keyword is omitted, then no 
                         symbols are plotted from a column.
.VARIABLE ICHAR
     ICHAR=N             The number N specifies the identifying
			 symbol to be plotted as the base of 
			 each vector.  The number should be in 
			 the range of 0 to 15.  This can be
                         used as an alternative  to  the 
                         CHAR keyword.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttieplot.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
ibis-gen out=i.dat nc=7 nr=30
mf inp=i.dat func=("c1=aint(index/16)+1","c2=100*INDEX",+
	"C3=100*INDEX","C4=C2+0.1","C5=C3-0.2","C6=INDEX")
ibis-list inp=i.dat
tieplot inp=i.dat NL=3000 NS=3000 KEYCOL=1 KEY=(1,2) +
	SCALE=1000 OLDCOLS=(2,3) NEWCOLS=(4,5) NUMCOL=6  
end-proc
$ Return
$!#############################################################################
