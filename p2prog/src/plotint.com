$!****************************************************************************
$!
$! Build proc for MIPL module plotint
$! VPACK Version 1.9, Thursday, November 30, 2006, 14:56:34
$!
$! Execute by entering:		$ @plotint
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
$ write sys$output "*** module plotint ***"
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
$ write sys$output "Invalid argument given to plotint.com file -- ", primary
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
$   if F$SEARCH("plotint.imake") .nes. ""
$   then
$      vimake plotint
$      purge plotint.bld
$   else
$      if F$SEARCH("plotint.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake plotint
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @plotint.bld "STD"
$   else
$      @plotint.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create plotint.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack plotint.com -mixed -
	-s plotint.f -
	-p plotint.pdf -
	-i plotint.imake -
	-t tstplotint.pdf tstplotint.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create plotint.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C---- VICAR PROGRAM "PLOTINT".
C     PURPOSE: TO PLOT  DATA CONTAINED IN  COLUMNS OF AN
C               IBIS INTERFACE FILE.
C
C     PROGRAMMER: BORIS GOKHMAN, JULY 1981.
C
C	MODIFIED:   FRANK EVANS 	NOVEMBER 1985  
C			FOR F77 CALCOMP, GENERAL MODERNIZATION
C
C       MODIFIED:   EJB    SEPT 87 FOR MULTIPLE PLOTS AS PER
C                          CONTROL COLUMN ON SPECIFIED OUTPUT DEVICES
C
C       MODIFIED:   BAM   MSTP PORTING  12/95
C
C	MODIFIED:   PXA   JAN 97 CONVERTED CALCOMP CALLS TO XRTPS
C			  CALLS AND REWROTE PLOT PROCEDURES

c       aug2006  -lwk-  fixed bug (Linux only) when YCOLSTR defaulted;
c			added NODISP and PLOTOUT parameters to support
c			output to file instead of display (no code needed
c			for NODISP as it is parsed by xrtps routines)


      IMPLICIT INTEGER (A-Z)
      INTEGER	NPOINT(400), NSTART(400), NCOLY(20),nystr
      INTEGER	INTEQ(20),LINTYP(20),SYMTYP(20)
      INTEGER	COUNT, FREQ,DEF,RUNIT,ibis_in,
     *	 	XRANDF,YRANDF, SLENGTH
      REAL	COLX(2000),COLY(2000,20),CONTRL(100000)
      REAL	COLMIN, COLMAX
      REAL	XRAN(2),YRAN(2), XRANG1,XRANG2, YRANG1,YRANG2
      REAL	FIRSTX,DELTAX, FIRSTY,DELTAY
      REAL	HEIGHT, Y, XLEN,YLEN,YMIN,YMAX,XMIN,XMAX
      REAL	ARRMIN,ARRMAX
      INTEGER	YSTRPTR(20), YSTRLEN(20)
      INTEGER   status
      BYTE	YSTRTMP(1200)
      CHARACTER*60  TITLE, XLABEL, YLABEL,label(1),plotout
      CHARACTER*20 YCOLSTR(20)
      REAL	SCALER(4),SCAX(4)
      EQUIVALENCE(SCALER(1),YMIN),(SCALER(2),YMAX),
     *           (SCALER(3),FIRSTY),(SCALER(4),DELTAY)
      EQUIVALENCE(SCAX(1),XMIN),(SCAX(2),XMAX),
     *           (SCAX(3),FIRSTX),(SCAX(4),DELTAX)
C& MOIFIED EJB SEPT 87 ************************
      CHARACTER*7 DEVICE

C&
C& GET THE VALUE OF DEVICE OR SET DEFAULT
C&   
        CALL XVP('DEVICE',DEVICE,COUNT)
C& ********************************************
C
      call xvp('PLOTOUT',plotout,count)
      call plotfn(plotout)

C---- READ PARAMETERS 
C
C---- GET THE NUMBERS OF THE X-COL AND THE Y-COL AND CONTROL COL
C
      CALL XVP('YCOL',NCOLY,MYCOL)
      CALL XVP('XCOL',NCOLX,COUNT)
C
      CALL XVP('CONTROL',NCONTR,COUNT)
C--  GET THE SIZE OF THE PLOT.
      CALL XVP('XLEN',XLEN,COUNT)
      CALL XVP('YLEN',YLEN,COUNT)
      IF(XLEN .EQ. 0) THEN
	CALL MABEND('X-AXIS LENGTH MUST BE GREATER THAN ZERO...')
      ENDIF
      IF(YLEN .EQ. 0) THEN
	CALL MABEND('Y-AXIS LENGTH MUST BE GREATER THAN ZERO...')
      ENDIF
      IF(XLEN .GT. 6.0) THEN
	CALL MABEND('X-AXIS LENGTH CANNOT EXCEED 5 INCHES (DEFAULT)...')
      ENDIF
      IF(YLEN .GT. 6.0) THEN
	CALL MABEND('Y-AXIS LENGTH CANNOT EXCEED 5 INCHES ...')
      ENDIF		
C
C--  GET THE TEXT/TITLE INFO
      CALL XVP('TITLE',TITLE,COUNT)
      CALL XVP('XLABEL',XLABEL,COUNT)
      CALL XVP('YLABEL',YLABEL,COUNT)
      CALL XVP('HEIGHT',HEIGHT,COUNT)
      CALL XVP('YCOLSTR',YSTRTMP,NYSTR)
      if (nystr.gt.0) then
        CALL XVSPTR(YSTRTMP,NYSTR,YSTRPTR,YSTRLEN)
        DO L = 1 , NYSTR
	    DO K = 1, YSTRLEN(L)
	        YCOLSTR(L)(K:K) = CHAR(YSTRTMP(YSTRPTR(L)+K-1))
	    ENDDO
        ENDDO
      endif
C
C---- READ THE AXIS LABELS AND SYMBOL STUFF
      CALL XVP('FREQ',FREQ,COUNT)
      CALL XVPARM ('SYMTYPE',SYMTYP,COUNT, DEF,20)
      IF (DEF .EQ. 1) THEN
          DO I = 1,MYCOL
	    SYMTYP(I) = 0
	  ENDDO
      ENDIF
      DO I = 1,MYCOL
          LINTYP(I) = FREQ*(SYMTYP(I)-1)
      ENDDO
      DO IYCOL = 1,MYCOL
         INTEQ(IYCOL) = MOD(IYCOL-1,15)
      ENDDO
C--  READ THE X AND Y RANGE IF PROVIDED
      CALL XVPARM('XRANGE',XRAN,COUNT,XRANDF,2)
      CALL XVPARM('YRANGE',YRAN,COUNT,YRANDF,2)
      print *, 'xrandf = ',XRANDF
      print *, 'yrandf = ',YRANDF
      XRANG1 = XRAN(1)
      XRANG2 = XRAN(2)
      YRANG1 = YRAN(1)
      YRANG2 = YRAN(2)
      IF (XRANG1 .EQ. XRANG2) THEN
	CALL MABEND('XRANGE MUST VARY...')
      ENDIF
      IF (YRANG1 .EQ. YRANG2) THEN
	CALL MABEND('YRANGE MUST VARY...')
      ENDIF
C
C--- OPEN THE INTERFACE FILE
	CALL XVUNIT(RUNIT,'INP',1,STATUS,' ')
	IF (STATUS .NE. 1) THEN
		CALL XVMESSAGE('ERROR ON IBIS UNIT CALL',' ')
		CALL XVMESSAGE('PROGRAM TERMINATED',' ')
		RETURN
	END IF
       call ibis_file_open(RUNIT,ibis_in,'read',0,0,' ',' ',status)
       if (status .ne. 1) call ibis_signal_u(RUNIT,status,1)
       call ibis_file_get(ibis_in,'nc',ncol,1,1)
       call ibis_file_get(ibis_in,'nr',CLEN,1,1)
       call ibis_column_read(ibis_in,CONTRL,ncol,1,clen,status)
       if (status .ne. 1) call ibis_signal_u(RUNIT,status,1)	
      CALL FINDCO(ibis_in,CONTRL,NCONTR,CLEN,NSTART,NPOINT,NPLOTS)
C
C--   START THE PLOTTER
      LDEV = 9
C
c      CALL PLOTS(0,0,LDEV)
      call xrtbegin(status)
      print *,'xrt status (non-zero, success) = ',status
C
C	Turn the autoaxes off. Otherwise an extra set of axes appear.
      call displayaxes(0,0,0)
      call displayaxes(1,1,0)
C
C
C---- BEGIN PLOTS. THIS LOOP MAKES COMPLETE PLOTS(TOTAL # =NPLOTS)
C     ACCORDING TO THE CONTROL COLUMN.
C
      print *,'nplots = ',nplots
      DO 100 IPLOT = 1,NPLOTS
C---- READ "X" AND "Y" COLUMNS.
C  Rewrite GETSET calls with ibis column read calls
      print *,'CLEN = ',clen
      call ibis_column_read(ibis_in,colx,ncolx,1,clen,status)
      if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)
      do l=1,mycol
	call ibis_column_read(ibis_in,coly(1,l),ncoly(l),1,clen,status)
	if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)
      end do
      LASTPT = NPOINT(IPLOT)
      print *,'LASTPT = ',lastpt
C
C---- SCALE AND DRAW X-AXIS.
C
c   code to check if y case applies to x case
	 IF (XRANDF .NE. 0) THEN
         COLMIN = ARRMIN(COLX,LASTPT)
         COLMAX = ARRMAX(COLX,LASTPT)
         IF (XMIN.GT.COLMIN) XMIN=COLMIN
         IF (XMAX.LT.COLMAX) XMAX=COLMAX
	 ELSE
         COLMIN = ARRMIN(COLX,LASTPT)
         COLMAX = ARRMAX(COLX,LASTPT)
         IF (XMIN.GT.COLMIN) XMIN=COLMIN
         IF (XMAX.LT.COLMAX) XMAX=COLMAX
	 ENDIF
c   cccccccccccccccccccccccccccccccccccccccccccc
      print *,'xrang1 = ',xrang1
      print *,'xrang2 = ',xrang2
      print *,'xmin = ',xmin
      print *,'xmax = ',xmax
      IF(XRANDF .NE. 0) THEN
        CALL SCALE(COLX,XLEN,LASTPT,1)
        FIRSTX = COLX(LASTPT+1)
        DELTAX = COLX(LASTPT+2)
      ELSE
c        CALL SCALE(COLX,XLEN,LASTPT,1)
        FIRSTX = XRANG1
        DELTAX = (XRANG2-XRANG1)/XLEN
        COLX(LASTPT+1) = FIRSTX
        COLX(LASTPT+2) = DELTAX
      ENDIF
      LXPNT = INT((XRANG2-XRANG1)/DELTAX)
      print *,'firstx = ',firstx
      print *,'deltax = ',deltax
      print *,'lxpnt = ',lxpnt
      print *,'FIRSTX AND DELTAX BEFORE AXIS...',firstx,deltax
      NCHAR = MAX( SLENGTH(XLABEL),1)
      call axestitles(xlabel,ylabel,270,' ',0)
C
C---- SCALE AND DRAW Y-AXIS.
C
      YMIN = 1.E+19
      YMAX = -1.E+19

      DO L = 1, MYCOL
	 IF (YRANDF .NE. 0) THEN
         COLMIN = ARRMIN(COLY(1,L),LASTPT)
         COLMAX = ARRMAX(COLY(1,L),LASTPT)
         IF (YMIN.GT.COLMIN) YMIN=COLMIN
         IF (YMAX.LT.COLMAX) YMAX=COLMAX
	 ELSE
         COLMIN = ARRMIN(COLY(1,L),LASTPT)
         COLMAX = ARRMAX(COLY(1,L),LASTPT)
         IF (YMIN.GT.COLMIN) YMIN=COLMIN
         IF (YMAX.LT.COLMAX) YMAX=COLMAX
	 ENDIF
      ENDDO
      print *,'ymin = ',ymin
      print *,'ymax = ',ymax
      IF (YRANDF .NE. 0) THEN
        CALL SCALE(SCALER,YLEN,2,1)
      ELSE
        FIRSTY = YRANG1
        DELTAY = (YRANG2-YRANG1)/YLEN
      ENDIF
      LYPNT = INT((YRANG2-YRANG1)/DELTAY)
      print *,'lypnt = ',lypnt
      print *,'yrang1 = ',yrang1
      print *,'yrang2 = ',yrang2
      print *,'FIRSTY AND DELTAY BEFORE AXIS...',firsty,deltay
C
      NCHAR = MAX( SLENGTH(TITLE), 1)
      label(1) = TITLE
      print *,'label(1) = ',label(1)
      call header(label(1),1,center)
C
C---- SET THE SCALING PARAMETERS.
C
      PRINT *,'LASTPT = ',LASTPT
      DO L = 1,MYCOL
         COLY(LASTPT+1,L) = FIRSTY
         COLY(LASTPT+2,L) = DELTAY
      ENDDO
C
C---- MAKE THE PLOT AND PLOT SYMBOLS 
C
      COLX(LASTPT+1) = FIRSTX
      COLX(LASTPT+2) = DELTAX
      Y = 0.0
      DO L = 1,MYCOL
	  call setactiveset(l)
          Y = Y + 2.0*0.15
          IF (YSTRLEN(L) .GT. 0)  THEN
	      IF (SYMTYP(L) .NE. 1) THEN 
     	        CALL SYMBOL(XLEN+0.5,Y,0.15,YCOLSTR(L),0,0.0,YSTRLEN(L))
	      ENDIF
	  ENDIF
	  IF (YRANDF .NE. 0) THEN
c       call setaxesminimums(xrang1,yrang1,yrang1)
       call setaxesmaximums(xrang2,yrang2,yrang2)
          CALL LINE(COLX,COLY(1,L), LASTPT,1, LINTYP(L), INTEQ(L))
	  ELSE
       call setaxesminimums(xrang1,yrang1,0)
       call setaxesmaximums(xrang2,yrang2,0)
c       CALL LINE(COLX,COLY(1,L),LASTPT,1,LINTYP(L),INTEQ(L))
        call line(colx,coly(1,l),lastpt,1,lintyp(l),inteq(l))
	  ENDIF 
      ENDDO
C
C& MODIFIED BY EJB SEPT 87 *********************
C&  BRANCH
       IF(DEVICE .EQ. 'CALCOMP') THEN
C& RESET (MOVE) ORIGIN AS FOR CALCOMP PLOTTER 
C& BUT CONTINUE THE SAME 'PLOT FILE'
        IF (IPLOT .LT. NPLOTS) CALL PLOT(0.,0.,  -3)
       ELSEIF(DEVICE .EQ. 'IMAGE') THEN
        IF (IPLOT .LT. NPLOTS) CALL NEWPLT
C&
       ELSE
C& ASSUME PRINTRONIX/REGIS/TEK OUTPUT DEVICE;
C& CLOSE THIS 'PLOT FILE' AND OPEN A NEW ONE FOR THE NEXT PLOT
       IF(IPLOT.LT.NPLOTS) CALL PLOT(0.,0.,999)
       IF(IPLOT.LT.NPLOTS) CALL PLOTS(0,0,LDEV)
       ENDIF
C& *********************************************
C
  100 CONTINUE
C
      CALL PLOT(0.,0.,999)
	call ibis_file_close(ibis_in,' ',status)
	if (status .ne. 1) call ibis_signal_u(RUNIT,status,1)

      RETURN
      END
C
C**************************************
	INTEGER FUNCTION SLENGTH(STRING)
	INTEGER	I
	CHARACTER*(*) STRING

	I = LEN(STRING)
	DO WHILE (ICHAR(STRING(I:I)) .EQ. 32 .AND. I .GT. 1)
	    I = I - 1
	ENDDO
	SLENGTH = I
	RETURN
	END
C
C*************************************
      REAL FUNCTION ARRMIN(ARRAY,NUMBER)
      REAL  ARRAY(1), DUMMY
      DUMMY = ARRAY(1)
      DO I = 2,NUMBER
        DUMMY = AMIN1(DUMMY,ARRAY(I))
      ENDDO
      ARRMIN = DUMMY
      RETURN
      END
C*************************************
      REAL FUNCTION ARRMAX(ARRAY,NUMBER)
      REAL  ARRAY(1), DUMMY
      DUMMY = ARRAY(1)
      DO I = 2,NUMBER
        DUMMY = AMAX1(DUMMY,ARRAY(I))
      ENDDO
      ARRMAX = DUMMY
      RETURN
      END


	SUBROUTINE FINDCO (ibis_in,CONTRL,NCONTR,LENGTH,NSTART,
     *		     NPOINT,NSETS)
	INTEGER*4 NPOINT(1), NSTART(1)
	INTEGER ibis_in,status 
C	REAL CONTRL(100000)
	DIMENSION CONTRL(1)
c	DIMENSION COLUMN(1)
C	NROWS = (LENGTH-1)/128+1
C	NRECTR = NROWS*(NCONTR-1)+2
C
C---- READ CONTROL COLUMN AND FIND START POINTS AND LENGTHS
C     FOR THE SETS.
C
C	CALL GETCOL (UNIT,NCONTR,LENGTH,CONTRL)
C	call ibis_column_read(unit,ncontr,length,contrl,status)
	call ibis_column_read(ibis_in,contrl,ncontr,1,length,status)
	if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)
	NSTART(1) = 1
	NPT = 0
	NSETS = 0
	LENGTH=100
	L = LENGTH-1
	DO NELEM=1,L
	  NPT = NPT+1
	  IF (CONTRL(NELEM).NE.CONTRL(NELEM+1)) THEN
	    NSETS = NSETS+1
	    NSTART(NSETS+1) = NELEM+1
	    NPOINT(NSETS) = NPT
	    NPT = 0
	  ENDIF
	ENDDO
	NSETS = NSETS+1
	NPOINT(NSETS) = NPT+1
	RETURN
	END
C     ******************************

$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create plotint.pdf
PROCESS      HELP=*
PARM INP TYPE=(STRING,72)
PARM XCOL TYPE=INTEGER
PARM YCOL TYPE=INTEGER COUNT=(1:20)
PARM CONTROL TYPE=INTEGER DEFAULT=0
PARM XLEN TYPE=REAL DEFAULT=5.0
PARM YLEN TYPE=REAL DEFAULT=5.0
PARM YCOLSTR TYPE=(STRING,20) COUNT=(0:20) DEFAULT=--
PARM XLABEL TYPE=(STRING,60) DEFAULT=""
PARM YLABEL TYPE=(STRING,60) DEFAULT=""
PARM TITLE TYPE=(STRING,60)  DEFAULT=""
PARM HEIGHT TYPE=REAL DEFAULT=.15
PARM FREQ TYPE=INTEGER DEFAULT=1
PARM SYMTYPE TYPE=INTEGER COUNT=(0:20) DEFAULT=1  VALID=1:2
PARM XRANGE TYPE=REAL COUNT=(0:2) DEFAULT=(0.0,5.0)
PARM YRANGE TYPE=REAL COUNT=(0:2) DEFAULT=(0.0,5.0)
PARM DEVICE TYPE=KEYWORD VALID=(CALCOMP,IMAGE,PRINT,TEK,REGIS) +
            DEFAULT=PRINT
PARM NODISP KEYWORD COUNT=0:1 VALID=NODISP DEFAULT=--
PARM PLOTOUT TYPE=(STRING,60) DEFAULT="PLOTINT.PSF"
END-PROC
.TITLE
VICAR/IBIS Program PLOTINT
.HELP
PURPOSE

     PLOTINT plots data contained in columns of an IBIS interface file
	using the device independent Calcomp plotting system.
	Either lines or symbols may be plotted, and the graph can 
	be fully annotated with titles, etc.  

.PAGE
Vicar Plotting

The Vicar Plotting system can produce plots on a display device if required.
The plotting system always generates a postscript file suitable for plotting on 
either a hard copy device or a display device. The postscript file can be 
defaulted to a system derived name or can be explicitly named by the user.  

The user can run plotting programs either interactively or in batch.  Whether 
or not plots are required, a plotting device (node) must be specified. This may
or may not be the plotting device the user requires.  To override the system 
defaulted device, the user must select a display device.  This is implemented 
from an Alpha using the following following:
           set display/create/trans=tcpip/node=poconos.  
This allocates a UNIX workstation named "poconos". If the user is on an Alpha
and wishes to plot to a UNIX workstation, the user first type: 
           xhost coda2 
where coda2 is the name of the Alpha from which plots will be sent.  
.PAGE
To plot on a UNIX workstation, the user needs to type on the UNIX workstation
           setenv DISPLAY poconos:0.0
which allocates the UNIX workstation named "poconos".      
Note: poconos and coda2 are examples of nodes.  The user may send and direct 
plots from and to any node he chooses.  

Interactively, either on the command line or in a pdf, the user may decide 
decide whether or not to plot the graphics.   If the user requires only a hard
copy, he must add 'nodisp to the program command line.   This will inhibit the 
plotting of the graphics on a display device.  

In batch, the user may not request plots on a display device.  Only postscript
files will be produced.  In order to generate postscript files in batch mode , 
the user MUST allocate a display device from WITHIN the batch script.  If the 
user fails to do this, no postscript files will be produced.    


.PAGE
EXECUTION


     PLOTINT INP=TRANS.INT XCOL=1 YCOL=(3,4) CONTROL=6  +
	YCOLSTR = ("AVERAGE POPULATION X1000","AIRLINE FLIGHTS") +
	XLABEL = "DISTANCE BETWEEN CITIES"  YLABEL = "THOUSANDS"  +
	TITLE = "GRAVITY TRANSPORTATION MODELS"   +
        XLEN=9.0 YLEN=7.5  SYMTYP=(1,1) 'CALCOMP

     PLOTINT INP=FUNCTION.INT XCOL=2 YCOL=3 CONTROL=1 +
	XLABEL = "X-AXIS"  YLABEL = "Y-AXIS"  TITLE = "HYPERBOLA" +
	XLEN=6.0 YLEN=3.0 FREQ=4 SYMTYPE=2  +
	XRANGE=(-7.5,7.5) YRANGE=(0,100)  'PRINT

    INP specifies the input interface file that contains the data to be 
plotted.  The column number of independent data is XCOL and the column(s)
of dependent data are in YCOL.  The control column (CONTROL) must have the
same entry for all of the rows that are to be on one plot.  The axis lengths
XLEN and YLEN are in inches.  The SYMTYPE parameter specifies what type of
plotting is done for each dependent variable:  0 for symbols only, 1 for
lines only, and 2 for both symbols and lines.  Only every FREQ'th data point
is plotted; default is to plot each one.  If the ranges are defaulted then
a Calcomp automatic scaling is done. The DEVICE parameter indicates the 
output device (as per PLOTTING).  The default is for PRINTRONIX/ReGIS/IMAGE/TEK.
When multiple plots are desired on the CALCOMP, this switch must be set.  All
of the plots are then plotted on one sheet of paper approx. 4.5" apart.


.PAGE




RESTRICTIONS

     The  control column must not specify more than 400 sets 
     (each set plotted on a different page).  Maximum number 
     of columns is 20.  All plotted texts must be shorter
     than 60 characters.


WRITTEN BY:                     B. Gokhman    25AUG1981
COGNIZANT PROGRAMMER:           Frank Evans
REVISION:                       B	KFE	November 1985
.LEVEL1
.VARIABLE INP
Input interface file
.VARIABLE XCOL
Column of independent variable
.VARIABLE YCOL
Columns of dependent variables
.VARIABLE CONTROL
Control column
.VARIABLE XLEN
Length of X-axis in inches
.VARIABLE YLEN
Length of Y-axis in inches
.VARIABLE YCOLSTR
Names of dependent variables
.VARIABLE XLABEL
String for X-axis
.VARIABLE YLABEL
String for Y-axis
.VARIABLE TITLE
String of text of the title
.VARIABLE HEIGHT
Height of the letters
in the title. 
.VARIABLE FREQ
Frequency of plotted symbol
.VARIABLE SYMTYPE
Type of data line:
  (lines,symbols and lines)
.VARIABLE XRANGE
Range for X-variable
.VARIABLE YRANGE
Range for Y-variable
.VARIABLE DEVICE
Output device
.vari nodisp
Suppresses plot display,
write to file instead.
.vari plotout
Name of output file if
NODISP specified.
.LEVEL2
.VARIABLE INP
     INP=int             Input    IBIS    interface    file, 
                         containing data . Each 
                         variable contained in a column.  If 
                         multiple  sets  are to  be  plotted 
                         from  the  same file they  must  be 
                         designated  by the identical  entry 
                         in the control column.
.VARIABLE XCOL
     XCOL=I              Integer  I  specifies  the   column 
                         containing independent variable.
.VARIABLE YCOL
     YCOL=(J1,...,JN)    Integer  J1,...,JN specify  columns 
                         containing   dependent   variables.  
                         The   maximum  number  of   columns 
                         allowed is N=20.
.VARIABLE CONTROL
     CONTROL=K           Integer  K  specifies  the  control 
                         column.   All data to be plotted on 
                         a  page  must have the  same  entry 
                         (number  or alpha) in  the  control 
                         column.   A  change of entry in the 
                         control  column will  indicate  the 
                         beginning  of  a  new  dataset  and 
                         cause initiation of a new plot.

.VARIABLE XLEN
     XLEN=X              X  specifies  the length of the 
			 X-axis in inches.   
.VARIABLE YLEN
     YLEN=Y              Y  specifies  the length of the  
			 Y-axis in inches.   
.VARIABLE YCOLSTR
     YCOLSTR=("STRING1","STRING2",...)
                         Strings "STRING1",...,"STRINGN" are 
                         optional.   They will be written on 
                         the   plot  next  to   the   symbol 
                         designating    the    corresponding 
                         variable.
.VARIABLE XLABEL
     XLABEL="STRING"      "STRING"  will be written along the 
                         X-axis (up to 60 characters).
.VARIABLE YLABEL
     YLABEL="STRING"      "STRING" will be written along  the 
                         Y-axis (up to 60 characters).
.VARIABLE TITLE
     TITLE="STRING"       String of charaters "STRING" (up to 
                         60  characters)  will  be   written 
                         under the plot.
.VARIABLE HEIGHT
     HEIGHT=H            Real H  specifies the size  of the
                         title lettering in inches. 
			 Default: Letters are 0.15 inch tall.
.VARIABLE FREQ
     FREQ=M              Integer  M specifies  frequency  of 
                         plotted  symbol (datapoint/symbol).  
                         M=1 - symbol for every data  point, 
                         M=2  symbol  for every second  data 
                         point, etc. Default:  M=1.
.VARIABLE SYMTYPE
     SYMTYPE=(M1,...,MN) Allows  to select the type of  data 
                         line for each dependent variable.

                         M=1   - only   lines  plotted   for 
                         corresponding variable.

                         M=2   - both  lines   and   symbols 
                         plotted for corresponding variable.
			 Default is M=1
.VARIABLE XRANGE
     XRANGE=(X1,X2)      The  lower and upper limits of  the 
                         X-axis      (inches).      Default:  
                         automatic scaling of X-axis.

.VARIABLE YRANGE
     YRANGE=(Y1,Y2)      The  lower and upper limits of  the 
                         Y-axis (inches). 
                         Default:   automatic scaling of the 
                         Y-axis.
.VARIABLE DEVICE
     DEVICE="KEYWORD"    KEYWORD specifies output device:
     or 'KEYWORD
                         CALCOMP --Calcomp plotter
                         REGIS   --REGIS plotting package
                         IMAGE   --IMAGE plotting device
                         TEK     --Tektronix plotting
                         PRINT   --Printronix plotting

                         For CALCOMP/IMAGE this variable must be set.  It
                         will produce one plot file with all of the plots 
                         contained therein.  Output is then on one piece 
                         of paper for the CALCOMP.  Multiple plots do work
                         on the IMAGE device, but there is no screen pause
                         between  plots.  For the other devices
                         default is acceptable.  One plot file is produced
                         for each plot.  
                         
.vari plotout
The name of the output postscript file if NODISP specified.

Note that this file will always be created, but it will be empty unless
NODISP is specified.
$ Return
$!#############################################################################
$Imake_File:
$ create plotint.imake
#define PROGRAM	plotint
#define R2LIB

#define MODULE_LIST plotint.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_MOTIF
#define LIB_XRT_GRAPH

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/*#define DEBUG	/* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tstplotint.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"

  ibis-gen out=TEST.INT NC=4 NR=100  
  mf TEST.INT FUNCTION=("c1=index","c4=1") 
  mf TEST.INT FUNCTION=("c2=(c1*c1)*sin(c1)","c3=c2")
  ibis-list TEST.INT
  plotint inp=TEST.INT  xcol=1 ycol=(2,3) cont=4 symt=(2,2) freq=1 + 
    xlabel="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMONPQRSTUVWXYZ01234567" +
    ylabel="abcdefghijklmonpqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567" +
    title = "Interface File "  +
    xrange = (0.0,15.) yrange=(0.0,100.) 

   ibis-gen out=TEST2.INT NC=5 NR=100 
   mf TEST2.INT FUNCTION=("c1=index","c5=1")
   mf TEST2.INT FUNCTION=("c2=sin(c1)*cos(c1)","c3=.5*c1/5", +
	"c4=tan(c1)*c2")
   ibis-list TEST2.INT
   plotint inp=TEST2.INT xcol=1 ycol=(2,3,4) cont=5 +
	symt=(2,2,2) freq=10 +
    xlabel="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567" + 
	ylabel="ordinates of 3 funcs" title="interface file of 3 funcs"

# repeat last case with output to file instead of display
   plotint inp=TEST2.INT xcol=1 ycol=(2,3,4) cont=5 +
	symt=(2,2,2) freq=10 +
    xlabel="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567" + 
	ylabel="ordinates of 3 funcs" title="interface file of 3 funcs" +
        'nodisp plotout="testplot.psf"

# repeat last case omitting filename to show that default output name is used:
# (in previous version, the output filename was the default in xrtps:
# postscript.psf, instead of the default in the pdf: PLOTINT.PSF)
   plotint inp=TEST2.INT xcol=1 ycol=(2,3,4) cont=5 +
	symt=(2,2,2) freq=10 +
    xlabel="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567" + 
	ylabel="ordinates of 3 funcs" title="interface file of 3 funcs" +
        'nodisp

end-proc
$!-----------------------------------------------------------------------------
$ create tstplotint.log
tstplotint
  ibis-gen out=TEST.INT NC=4 NR=100
Beginning VICAR task ibis
  mf TEST.INT FUNCTION=("c1=index","c4=1")
Beginning VICAR task mf
  mf TEST.INT FUNCTION=("c2=(c1*c1)*sin(c1)","c3=c2")
Beginning VICAR task mf
  ibis-list TEST.INT
Beginning VICAR task ibis
 
Number of Rows:100  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
        1.00        0.84        0.84        1.00
        2.00        3.64        3.64        1.00
        3.00        1.27        1.27        1.00
        4.00      -12.11      -12.11        1.00
        5.00      -23.97      -23.97        1.00
        6.00      -10.06      -10.06        1.00
        7.00       32.19       32.19        1.00
        8.00       63.32       63.32        1.00
        9.00       33.38       33.38        1.00
       10.00      -54.40      -54.40        1.00
       11.00     -121.00     -121.00        1.00
       12.00      -77.27      -77.27        1.00
       13.00       71.01       71.01        1.00
       14.00      194.16      194.16        1.00
       15.00      146.31      146.31        1.00
       16.00      -73.70      -73.70        1.00
       17.00     -277.84     -277.84        1.00
       18.00     -243.32     -243.32        1.00
       19.00       54.11       54.11        1.00
       20.00      365.18      365.18        1.00
       21.00      368.97      368.97        1.00
       22.00       -4.28       -4.28        1.00
       23.00     -447.65     -447.65        1.00
       24.00     -521.61     -521.61        1.00
       25.00      -82.72      -82.72        1.00
       26.00      515.49      515.49        1.00
       27.00      697.20      697.20        1.00
       28.00      212.39      212.39        1.00
       29.00     -558.12     -558.12        1.00
       30.00     -889.23     -889.23        1.00
 
Rows: 31:60
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
       31.00     -388.28     -388.28        1.00
       32.00      564.66      564.66        1.00
       33.00     1088.90     1088.90        1.00
       34.00      611.62      611.62        1.00
       35.00     -524.52     -524.52        1.00
       36.00    -1285.35    -1285.35        1.00
       37.00     -881.00     -881.00        1.00
       38.00      427.96      427.96        1.00
       39.00     1465.93     1465.93        1.00
       40.00     1192.18     1192.18        1.00
       41.00     -266.64     -266.64        1.00
       42.00    -1616.74    -1616.74        1.00
       43.00    -1537.95    -1537.95        1.00
       44.00       34.27       34.27        1.00
       45.00     1723.08     1723.08        1.00
       46.00     1908.18     1908.18        1.00
       47.00      272.97      272.97        1.00
       48.00    -1770.06    -1770.06        1.00
       49.00    -2289.96    -2289.96        1.00
       50.00     -655.94     -655.94        1.00
       51.00     1743.27     1743.27        1.00
       52.00     2667.84     2667.84        1.00
       53.00     1112.15     1112.15        1.00
       54.00    -1629.43    -1629.43        1.00
       55.00    -3024.26    -3024.26        1.00
       56.00    -1635.58    -1635.58        1.00
       57.00     1417.10     1417.10        1.00
       58.00     3340.02     3340.02        1.00
       59.00     2216.49     2216.49        1.00
       60.00    -1097.32    -1097.32        1.00
 
Rows: 61:90
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
       61.00    -3594.92    -3594.92        1.00
       62.00    -2841.41    -2841.41        1.00
       63.00      664.23      664.23        1.00
       64.00     3768.43     3768.43        1.00
       65.00     3493.35     3493.35        1.00
       66.00     -115.66     -115.66        1.00
       67.00    -3840.43    -3840.43        1.00
       68.00    -4152.02    -4152.02        1.00
       69.00     -546.49     -546.49        1.00
       70.00     3792.06     3792.06        1.00
       71.00     4794.27     4794.27        1.00
       72.00     1315.82     1315.82        1.00
       73.00    -3606.52    -3606.52        1.00
       74.00    -5394.66    -5394.66        1.00
       75.00    -2181.27    -2181.27        1.00
       76.00     3269.84     3269.84        1.00
       77.00     5926.16     5926.16        1.00
       78.00     3127.05     3127.05        1.00
       79.00    -2771.71    -2771.71        1.00
       80.00    -6360.89    -6360.89        1.00
       81.00    -4132.70    -4132.70        1.00
       82.00     2106.15     2106.15        1.00
       83.00     6671.06     6671.06        1.00
       84.00     5173.39     5173.39        1.00
       85.00    -1272.15    -1272.15        1.00
       86.00    -6829.90    -6829.90        1.00
       87.00    -6220.34    -6220.34        1.00
       88.00      274.12      274.12        1.00
       89.00     6812.61     6812.61        1.00
       90.00     7241.37     7241.37        1.00
 
Rows: 91:100
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
+-----------+-----------+-----------+-----------
       91.00      877.68      877.68        1.00
       92.00    -6597.40    -6597.40        1.00
       93.00    -8201.69    -8201.69        1.00
       94.00    -2167.05    -2167.05        1.00
       95.00     6166.44     6166.44        1.00
       96.00     9064.74     9064.74        1.00
       97.00     3571.73     3571.73        1.00
       98.00    -5506.76    -5506.76        1.00
       99.00    -9793.23    -9793.23        1.00
      100.00    -5063.66    -5063.66        1.00
  plotint inp=TEST.INT  xcol=1 ycol=(2,3) cont=4 symt=(2,2) freq=1  +
    xlabel="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMONPQRSTUVWXYZ01234567"  +
    ylabel="abcdefghijklmonpqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567"  +
    title = "Interface File "   +
    xrange = (0.0,15.) yrange=(0.0,100.)
Beginning VICAR task plotint
   ibis-gen out=TEST2.INT NC=5 NR=100
Beginning VICAR task ibis
   mf TEST2.INT FUNCTION=("c1=index","c5=1")
Beginning VICAR task mf
   mf TEST2.INT FUNCTION=("c2=sin(c1)*cos(c1)","c3=.5*c1/5",  +
	"c4=tan(c1)*c2")
Beginning VICAR task mf
   ibis-list TEST2.INT
Beginning VICAR task ibis
 
Number of Rows:100  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
        1.00        0.45        0.10        0.71        1.00
        2.00       -0.38        0.20        0.83        1.00
        3.00       -0.14        0.30        0.02        1.00
        4.00        0.49        0.40        0.57        1.00
        5.00       -0.27        0.50        0.92        1.00
        6.00       -0.27        0.60        0.08        1.00
        7.00        0.50        0.70        0.43        1.00
        8.00       -0.14        0.80        0.98        1.00
        9.00       -0.38        0.90        0.17        1.00
       10.00        0.46        1.00        0.30        1.00
       11.00       -0.00        1.10        1.00        1.00
       12.00       -0.45        1.20        0.29        1.00
       13.00        0.38        1.30        0.18        1.00
       14.00        0.14        1.40        0.98        1.00
       15.00       -0.49        1.50        0.42        1.00
       16.00        0.28        1.60        0.08        1.00
       17.00        0.26        1.70        0.92        1.00
       18.00       -0.50        1.80        0.56        1.00
       19.00        0.15        1.90        0.02        1.00
       20.00        0.37        2.00        0.83        1.00
       21.00       -0.46        2.10        0.70        1.00
       22.00        0.01        2.20        0.00        1.00
       23.00        0.45        2.30        0.72        1.00
       24.00       -0.38        2.40        0.82        1.00
       25.00       -0.13        2.50        0.02        1.00
       26.00        0.49        2.60        0.58        1.00
       27.00       -0.28        2.70        0.91        1.00
       28.00       -0.26        2.80        0.07        1.00
       29.00        0.50        2.90        0.44        1.00
       30.00       -0.15        3.00        0.98        1.00
 
Rows: 31:60
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
       31.00       -0.37        3.10        0.16        1.00
       32.00        0.46        3.20        0.30        1.00
       33.00       -0.01        3.30        1.00        1.00
       34.00       -0.45        3.40        0.28        1.00
       35.00        0.39        3.50        0.18        1.00
       36.00        0.13        3.60        0.98        1.00
       37.00       -0.49        3.70        0.41        1.00
       38.00        0.28        3.80        0.09        1.00
       39.00        0.26        3.90        0.93        1.00
       40.00       -0.50        4.00        0.56        1.00
       41.00        0.16        4.10        0.03        1.00
       42.00        0.37        4.20        0.84        1.00
       43.00       -0.46        4.30        0.69        1.00
       44.00        0.02        4.40        0.00        1.00
       45.00        0.45        4.50        0.72        1.00
       46.00       -0.39        4.60        0.81        1.00
       47.00       -0.12        4.70        0.02        1.00
       48.00        0.49        4.80        0.59        1.00
       49.00       -0.29        4.90        0.91        1.00
       50.00       -0.25        5.00        0.07        1.00
       51.00        0.50        5.10        0.45        1.00
       52.00       -0.16        5.20        0.97        1.00
       53.00       -0.36        5.30        0.16        1.00
       54.00        0.46        5.40        0.31        1.00
       55.00       -0.02        5.50        1.00        1.00
       56.00       -0.44        5.60        0.27        1.00
       57.00        0.39        5.70        0.19        1.00
       58.00        0.12        5.80        0.99        1.00
       59.00       -0.49        5.90        0.41        1.00
       60.00        0.29        6.00        0.09        1.00
 
Rows: 61:90
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
       61.00        0.25        6.10        0.93        1.00
       62.00       -0.50        6.20        0.55        1.00
       63.00        0.16        6.30        0.03        1.00
       64.00        0.36        6.40        0.85        1.00
       65.00       -0.47        6.50        0.68        1.00
       66.00        0.03        6.60        0.00        1.00
       67.00        0.44        6.70        0.73        1.00
       68.00       -0.40        6.80        0.81        1.00
       69.00       -0.11        6.90        0.01        1.00
       70.00        0.49        7.00        0.60        1.00
       71.00       -0.29        7.10        0.90        1.00
       72.00       -0.25        7.20        0.06        1.00
       73.00        0.50        7.30        0.46        1.00
       74.00       -0.17        7.40        0.97        1.00
       75.00       -0.36        7.50        0.15        1.00
       76.00        0.47        7.60        0.32        1.00
       77.00       -0.03        7.70        1.00        1.00
       78.00       -0.44        7.80        0.26        1.00
       79.00        0.40        7.90        0.20        1.00
       80.00        0.11        8.00        0.99        1.00
       81.00       -0.49        8.10        0.40        1.00
       82.00        0.30        8.20        0.10        1.00
       83.00        0.24        8.30        0.94        1.00
       84.00       -0.50        8.40        0.54        1.00
       85.00        0.17        8.50        0.03        1.00
       86.00        0.35        8.60        0.85        1.00
       87.00       -0.47        8.70        0.68        1.00
       88.00        0.04        8.80        0.00        1.00
       89.00        0.44        8.90        0.74        1.00
       90.00       -0.40        9.00        0.80        1.00
 
Rows: 91:100
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
       91.00       -0.11        9.10        0.01        1.00
       92.00        0.49        9.20        0.61        1.00
       93.00       -0.30        9.30        0.90        1.00
       94.00       -0.24        9.40        0.06        1.00
       95.00        0.50        9.50        0.47        1.00
       96.00       -0.18        9.60        0.97        1.00
       97.00       -0.35        9.70        0.14        1.00
       98.00        0.47        9.80        0.33        1.00
       99.00       -0.04        9.90        1.00        1.00
      100.00       -0.44       10.00        0.26        1.00
   plotint inp=TEST2.INT xcol=1 ycol=(2,3,4) cont=5  +
	symt=(2,2,2) freq=10  +
    xlabel="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567"  +
	ylabel="ordinates of 3 funcs" title="interface file of 3 funcs"
Beginning VICAR task plotint
   plotint inp=TEST2.INT xcol=1 ycol=(2,3,4) cont=5  +
	symt=(2,2,2) freq=10  +
    xlabel="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567"  +
	ylabel="ordinates of 3 funcs" title="interface file of 3 funcs"  +
        'nodisp plotout="testplot.psf"
Beginning VICAR task plotint
   plotint inp=TEST2.INT xcol=1 ycol=(2,3,4) cont=5  +
	symt=(2,2,2) freq=10  +
    xlabel="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567"  +
	ylabel="ordinates of 3 funcs" title="interface file of 3 funcs"  +
        'nodisp
Beginning VICAR task plotint
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
