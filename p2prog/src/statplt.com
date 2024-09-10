$!****************************************************************************
$!
$! Build proc for MIPL module statplt
$! VPACK Version 1.9, Wednesday, February 02, 2005, 10:38:42
$!
$! Execute by entering:		$ @statplt
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
$ write sys$output "*** module statplt ***"
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
$ write sys$output "Invalid argument given to statplt.com file -- ", primary
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
$   if F$SEARCH("statplt.imake") .nes. ""
$   then
$      vimake statplt
$      purge statplt.bld
$   else
$      if F$SEARCH("statplt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake statplt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @statplt.bld "STD"
$   else
$      @statplt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create statplt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack statplt.com -mixed -
	-s statplt.f -
	-i statplt.imake -
	-p statplt.pdf -
	-t tststatplt.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create statplt.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      implicit none
C     July 10, 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)

      REAL      MEANS(600),COVARIANCE(600),EX(363),EY(363)
      INTEGER   IPARM(40),STATUS
      integer   bands, unit, classes, ibis
      integer   npix,nb,row

      integer   idel, icount, idef, locxs, locys, locxy
      integer   ichan, jchan, I, II, III, L

      real      xscale, xinc, xloc, xi, sigma, xlo, xhi, ylo, yhi
      real      x, xx, yscale, yinc, yloc, y, yy, q
      real      a2, b2, c2, theta, sinsq, cossq, sincos
      real      r, dx, dy
     
      character*80 classname

      CHARACTER*63 plotname
      CHARACTER*12 xtitle, ytitle

      CALL IFMESSAGE('STATPLT version 10 July 95')

C     SET DEFAULT VALUES
      xscale    = 0.0
      xinc      = 0.0
      xloc      = 0.0
      xi        = 0.0
      sigma     = 0.0
      x         = 0.0
      xloc      = 0.0
      xx        = 0.0
      yscale    = 0.0
      yinc      = 0.0
      yloc      = 0.0
      y         = 0.0
      yy        = 0.0
      q         = 0.0
      a2        = 0.0
      b2        = 0.0
      c2        = 0.0
      theta     = 0.0
      sinsq     = 0.0
      cossq     = 0.0
      sincos    = 0.0
      r         = 0.0
      dx        = 0.0
      dy        = 0.0
      bands     = 0
      unit      = 0
      classes   = 0
      ibis      = 0
      npix      = 0
      nb        = 0
      row       = 0
      idel      = 0
      icount    = 0
      idef      = 0
      locxs     = 0
      locys     = 0
      locxy     = 0
      I         = 0
      II        = 0
      III       = 0
      L         = 0
      XSCALE    = 1.0
      YSCALE    = 1.0
      ICHAN     = 1
      JCHAN     = 2
      SIGMA     = 1.0
      XLO       = 0.0
      XHI       = 255.0
      YLO       = 0.0
      YHI       = 255.0

C                PROCESS PARAMETERS
C        'BANDS'
      CALL XVPARM('BANDS',IPARM,ICOUNT,IDEF,2)
      ICHAN=IPARM(1)
      JCHAN=IPARM(2)

C        'SIGMA'
      CALL XVPARM('SIGMA',SIGMA,ICOUNT,IDEF,1)

C        'PLOTNAME'
      CALL XVPARM('PLOTNAME',plotname,ICOUNT,IDEF,1)

C        'XSCALE'
      CALL XVPARM('XSCALE',IPARM,ICOUNT,IDEF,2)
      XLO=IPARM(1)
      XHI=IPARM(2)

C        'YSCALE'
      CALL XVPARM('YSCALE',IPARM,ICOUNT,IDEF,2)
      YLO=IPARM(1)
      YHI=IPARM(2)

C          OPEN INPUT DATA SET
      CALL XVUNIT(UNIT,'INP',1,STATUS,' ')

      if (status .lt. 0) call xvsignal(unit, status, 1) 

C     SET  POINTERS FOR THE LOCATIONS OF MEANS AND SIGMAS

      LOCXS = (ICHAN*(ICHAN+1))/2
      LOCYS = (JCHAN*(JCHAN+1))/2
      LOCXY = MAX0(LOCXS,LOCYS)-IABS(JCHAN-ICHAN)

      call istat_file_open(unit,'read',0,0,0,status) 

      if (status .lt. 0) call istat_signal(unit, status, 1) 
	
      ! Get file information 
      call istat_file_Info(unit, classes, bands, ibis) 

C     INITIALIZE PLOT
!     Specify output PostScript file */
      call plotfn (plotname)

      call xrtbegin (status)
      if (status .ne. 1) then
         call xvmessage ('Error on XRT/graph initialization',' ')
         goto 9999
      endif

      call setwidgetaspect (800,800)
      call setgraphaspect (800, 800)

      WRITE (XTITLE,'(A4,I2)') 'BAND', ICHAN
      WRITE (YTITLE,'(A4,I2)') 'BAND', JCHAN
      CALL axestitles (xtitle,ytitle,90,' ',0)

      EX(362) = 0.0
      EY(362) = 0.0
      EX(363) = 1.0
      EY(363) = 1.0

      XX = SIGMA/XSCALE
      YY = SIGMA/YSCALE

C     READ A RECORD, PLOT THE MEAN AND THE SIGMA ELLIPSE
      DO III = 1, classes 
         row = III

         call istat_record_read (unit, row, classname, npix,
     +	                         nb,means,covariance,status) 
         if (status .lt. 0) call istat_signal(unit,status,1) 

!        Get elipse center point
         X = means(ichan)
         Y = means(jchan)

         IF(X .GE. XLO .AND. X .LE. XHI .and.
     +      Y .GE. YLO .AND. Y .LE. YHI) THEN
            Q = III
            CALL NUMBER(X,Y+0.1,0.12,Q,0.0,-1)

C           CALCULATE THE SIGMA ELLIPSE POINTS and PUT IN EX & EY ARRAYS

            A2 = covariance(LOCXS)+1E-9
            B2 = covariance(LOCYS)+1E-9
            C2 = covariance(LOCXY)+1E-9
            Q = A2*B2-C2*C2

            DO L=1,180
               THETA = real (L)
               !! Provide a crutch for CODA1.  A High performance arithmetic
               !! trap will occur when result from sin or cos approaches zero
               !! and the result is squared 
               if (L .eq. 180) then
                  sinsq = 0.0
               else
                  SINSQ = SIN(THETA)**2
               endif
               if (L .eq. 90) then
                  cossq = 0.0
               else
                  COSSQ = COS(THETA)**2
               endif
               if (L .eq. 90 .or. L .eq. 180) then
                  sincos = 0.0
               else
                  SINCOS = SIN(THETA)*COS(THETA)
               endif
               R = B2*COSSQ+A2*SINSQ-2.0*C2*SINCOS+1E-15
               if (cossq .eq. 0.0) then
                  dx = 0.0
               else
                  DX = SQRT(COSSQ*Q/R)
               endif
               IF(L.GT.90) DX=-DX
               if (sinsq .eq. 0.0) then
                  dy = 0.0
               else
                  DY = SQRT(SINSQ*Q/R)
               endif
               EX(L) = X+DX*SIGMA
               EY(L) = Y+DY*SIGMA
               EX(L+180) = X-DX*SIGMA
               EY(L+180) = Y-DY*SIGMA
               IF(EX(L).LT. XLO) EX(L)= XLO
               IF(EX(L).GT.XHI) EX(L)=XHI
               IF(EY(L).GT.YHI) EY(L)=YHI
               IF(EX(L+180).GT. XHI) EX(L+180)=XHI
               IF(EX(L+180).LT. XLO) EX(L+180)=XLO
               IF(EY(L+180).LT. YLO) EY(L+180)=YLO
            END DO
            EX(361) = EX(1)
            EY(361) = EY(1)
            CALL LINE(EX,EY,361,1,0,0)
         END IF
      END DO 

C     CLOSE PLOT FILE
      CALL PLOT(0.0,0.0,999)

C     CLOSE INPUT DATA SET
      CALL XVCLOSE(UNIT,STATUS,' ')

9999  continue
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create statplt.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM statplt

   To Create the build file give the command:

		$ vimake statplt			(VMS)
   or
		% vimake statplt			(Unix)
************************************************************************/
#define PROGRAM	statplt
#define R2LIB
#define MODULE_LIST statplt.f
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport
#define FTN_STRING
#define LIB_MOTIF
#define LIB_XRT_GRAPH

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create statplt.pdf
PROCESS help=*
PARM INP    STATUS=STRING
PARM PLOTNAME  STATUS=STRING COUNT=(0,1) DEFAULT='statplt.psf'
PARM SIZE   STATUS=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL     STATUS=INTEGER DEFAULT=1
PARM NL     STATUS=INTEGER DEFAULT=0
PARM BANDS  STATUS=INTEGER COUNT=2 VALID=(1:4) DEFAULT=(1,2)
PARM SIGMA  STATUS=REAL    DEFAULT=1.0
PARM XSCALE STATUS=INTEGER COUNT=2 DEFAULT=(0,255)
PARM YSCALE STATUS=INTEGER COUNT=2 DEFAULT=(0,255)
PARM NODISP STATUS=KEYWORD COUNT=(0,1) VALID=NODISP DEFAULT=--
END-PROC
.TITLE
STATPLT
.HELP
PURPOSE: 

STATPLT generates a plot of the contents of a classification statistics
dataset. The plot consists of the centroids and Bayesian confidence regions
for each class, for any two input bands. If the display of the plot is not 
suppressed The plot is automatically displayed, and if requested, is saved
in an output PostScript file.

EXECUTION:
STATPLT SDS BANDS=(1,5) SIGMA=2.5   
                                    The resultant plot will have DN of
                                    Band 1 as the x-axis and the DN of
                                    Band 5 as the y-axis. Ellipses
                                    representing the 2.5 standard
                                    deviation confidence boundaries will
                                    be drawn around each centroid. The plot
                                    will be automatically displayed and
                                    optionally saved in the output PostScript
                                    file, 'statplt.psf'.

STATPLT SDS XSCALE=(0,100) PLOTNAME=(statplt1.psf)

                                    This plot will have the x-axis 
                                    rescaled such that the range 0 to 100 DN
                                    spans the entire length of the x-axis.
                                    Any class whose Band 1 mean (the band
                                    being plotted on the x-axis) is outside
                                    the 0 to 100 DN range, will not be
                                    plotted.  The plot will be automatically
                                    displayed and optionally saved in the 
                                    output PostScript file, 'statplt.psf'.

STATPLT SDS XSCALE=(0,100) YSCALE=(0,100) 'NODISP PLOTNAME=(statplt2.psf)

                                    In addition to scaling both axes for
                                    the range 0 to 100, the plot is not
                                    displayed, but is automatically saved in
                                    the output PostScript file.

OPERATION:
     For each class in a classification statistics dataset (or for those
specified by the size field) STATPLT:

           1.  Marks the position of the centroid of the class.
           2.  Labels the centroid with its corresponding class number.
           3.  Draws the ellipse that bounds the range of the class.
               The number of standard deviations within this boundary
               is specified by the SIGMA parameter.

If the centroid of a class lies outside the range of either the x or y axis,
that class will be ignored.

If the plot display is not suppressed by the use of the keyword 'NODISP, the 
plot is displayed at the user terminal via calls to XRTPS. XRTPS also
displays three menu selections and then waits for the user to select one of
three options before continuing. The three options are; 1) exit, 2)
page, and 3) save. Selecting 'exit' results in immediate termination of
STATPLT.  Selecting 'page' results in immediate termination of STATPLT, as
STATPLT creates only one plot each time that it is run. Selecting 'save' 
results in the displayed image being saved in the output PostScript file, 
followed by the termination of STATPLT.

WRITTEN BY:  Ron Alley, 24 October 1978
COGNIZANT PROGRAMMER:  S. Pohorsky
REVISION: 5 April 1984 (Conversion to VAX)
          Made portable for UNIX and for XRT/graph. J. Turner (CRI) May 1995
.LEVEL1
.VARIABLE INP
STRING - Input image file; must
be a classification statsitics 
dataset.
.VARIABLE ONAME
STRING - Output PostScript file
name. STATPLT names output file 
as 'postscript.psf' if not specified
on VICAR command line.
.VARIABLE SIZE
INTEGER - Standard VICAR size 
field
.VARIABLE SL
INTEGER - Starting line (class)
.VARIABLE NL
INTEGER - Number of lines 
(classes)
.VARIABLE BANDS
INTEGER - The bands used for the
x and y axes, respectively.
.VARIABLE SIGMA
REAL - The number of standard
deviations about each class.
.VARIABLE XSCALE
INTEGER - The limits in DN 
values for the x-axis.
.VARIABLE YSCALE
INTEGER - The limits in DN 
values for the y-axis.
.VARIABLE 'NODISP
KEYWORD - Specify 'NODISP to
suppress the display of the 
plot on the display. Plot will
be saved in PostScript file.
.LEVEL2
.VARIABLE INP
The input dataset must be in the classification statistics dataset format.
This is the format output by STATS and USTATS. Alternatively, stats may be
gathered in an IBIS interface file and converted to a statistics dataset via
the program SDSIBIS.
.VARIABLE BANDS
The plot produced by STATPLT is two dimensional. Therefore, only two bands
are plotted. The first value of BANDS is used for the x-axis, and the second
value is used for the y-axis. The default is BANDS=(1,2).
.VARIABLE SIGMA
Assuming Normal distributions (STATS, USTATS, and FASTCLAS all make this
assumption), confidence boundaries in two dimensions take the shape of
ellipses. The SIGMA variable allows the user to choose a level of
confidence, in terms of standard deviations from the mean. The default is
SIGMA=1, a one standard deviation region about the mean.
.VARIABLE XSCALE
The values given by XSCALE define the end-points of the axis. 
The default is XSCALE=(0,255).
.VARIABLE YSCALE
The values given by YSCALE define the end-points of the axis. 
The default is YSCALE=(0,255).
.VARIABLE NODISP
The keyword NODISP is used to suppress the automatic display of the output
plot image. When NODISP is specified, the automatic display of the plot
image is suppressed, but is automatically saved in the output PostScript
file. The output PostScript file may be printed using the command 'qpr'.
.VARIABLE PLOTNAME
STATPLT provides the user with the capability of specifying the name of the
output PostScript file.  If a name is not specified, STATPLT identifies the
output file as 'statplt.psf'. 
.END
$ Return
$!#############################################################################
$Test_File:
$ create tststatplt.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
! setup datasets
!
GEN STATPLTA NL=50 NS=50 LINC=0 SINC=5
GEN STATPLTB NL=50 NS=50 LINC=5 SINC=1
GEN STATPLTC NL=50 NS=50 LINC=3 SINC=2
GEN STATPLTD NL=50 NS=50 LINC=-2 SINC=3 IVAL=130
STATS (STATPLTA,STATPLTB,STATPLTC,STATPLTD) statplte +
       CLASS1=(1,1,10,25) CLASS2=(11,1,20,15)   CLASS3=(31,1,5,25) +
       CLASS4=(41,1,8,25) CLASS5=(1,20,25,8)    CLASS6=(25,20,25,25) +
       CLASS7=(35,20,10,30) CLASS8=(1,35,20,15) CLASS9=(25,35,25,15) +
       CLASS10=(1,35,30,12)
!
statplt statplte plotname=statplt1_SGI.psf
!       change scales
statplt statplte xscale=(0,160) yscale=(50,250) +
        plotname=statplt2_SGI.psf
!       ellipses should halve in size;
statplt statplte sigma=0.5 plotname=statplt3_SGI.psf
!       same plot, switching axes
statplt statplte bands=(2,1) plotname=statplt4_SGI.psf
!       try bands 3 and 4
statplt statplte bands=(3,4) plotname=statplt5_SGI.psf
!
end-proc
$ Return
$!#############################################################################
