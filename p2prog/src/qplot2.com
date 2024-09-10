$!****************************************************************************
$!
$! Build proc for MIPL module qplot2
$! VPACK Version 1.9, Friday, September 15, 2006, 14:26:04
$!
$! Execute by entering:		$ @qplot2
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
$ write sys$output "*** module qplot2 ***"
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
$ write sys$output "Invalid argument given to qplot2.com file -- ", primary
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
$   if F$SEARCH("qplot2.imake") .nes. ""
$   then
$      vimake qplot2
$      purge qplot2.bld
$   else
$      if F$SEARCH("qplot2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake qplot2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @qplot2.bld "STD"
$   else
$      @qplot2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create qplot2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack qplot2.com -mixed -
	-s qplot2.f -
	-i qplot2.imake -
	-p qplot2.pdf -
	-t tstqplot2.pdf tstqplot2.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create qplot2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     PROGRAM QPLOT2
C     10 JUL 95   ...CRS (CRI) MST S/W CONVERSION (VICAR PORTING)
C     22 AUG 85   ...JHR...    CONVERTED TO VICAR2, RENAMED QPLOT2
C     22 APR 82   ...JHR...    INITIAL RELEASE
C      E,QPLOT2,IN,*,,PARAMS
C     THIS PROGRAM PLOTS LINES OF DN VS RELATIVE SAMPLE NUMBER.
C     A MAXIMUM OF 10 LINES MAY BE PLOTTED ON THE GRAPH
C     A MAXIMUM OF 10 DATA SETS MAY BE USED
C     ANY LINE DIRECTION MAY BE SPECIFIED
C     IF THE LINE DIRECTION IS NOT HORIZONTAL OR VERTICAL
C     THE OUTPUT SAMPLE POINTS ARE SPACED THE SAME AS THE X AND Y
C     AXES, I.E. IF THE LINE DIRECTION IS 45 DEGREES THE NUMBER OF
C     OUTPUT SAMPLES WILL BE THE SQUARE ROOT OF 2 TIMES THE NUMBER
C     OF INPUT SAMPLES
C
C      * PROCESS IN,SL,SS,EL,ES     SPECIFIES THE INPUT NUMBER,
C          STARTING LINE, STARTING SAMPLE, ENDING LINE, AND
C          ENDING SAMPLE.
C
C
      EXTERNAL EQUIV
      COMMON/C1/ SIZE,DSPLAC,RDS,XMIN,XMAX,YMIN,YMAX
     &          ,XSCLMN,XSCLMX,YSCLMN,YSCLMX,XSCLDT
     &          ,YSCLDT,XLNGTH,YLNGTH,HALF,NORM,NCHAN
      COMMON/C2/ SL,SS,EL,ES,IN,UNIT,ILINE,NLINES
     &          ,NLI,NSI,NSCHAN,GTYPE,XPAGE,LB,LABTOP
      common/commonheader/headermsg,iiline,i2line
      character*56 headermsg(220) !! Labels * (lines per label+2)
      integer      iiline,i2line  !! index into header strings

      REAL*4 RPARM(256),XAXIS(4),YAXIS(4)
      REAL*4 XMAX(10),XMIN(10),YMAX(10),YMIN(10)
     &      ,XSCLMN,XSCLMX,YSCLMN,YSCLMX,XLNGTH,YLNGTH
      INTEGER*4 IN(10),SL(10),SS(10),EL(10),ES(10),HALF(10),UNIT(10)
     &         ,GTYPE,TTLTOP,NLI(10),NSI(10),STAT,IPARM(256),TICS
     &         ,STATUS
      CHARACTER*52 XTTL,YTTL,TTL,CBUF,XTITLE,YTITLE,TITLE,PLOTOUT
      CHARACTER*4 FORMAT(10)
      character*10 labels (11)      

      character*52 msg       
      LOGICAL   XVPTST
      LOGICAL   NORM
      character*1 LPARM(1024)
C
      CALL IFMESSAGE('QPLOT2 version 10-July-95')
C
C   SET DEFAULTS AND INITIALIZE
      YTITLE = 'DN VALUE'
      XTITLE = 'RELATIVE SAMPLE NUMBER'
      TITLE  = 'IPL LINE PLOT'
      PLOTOUT= 'qplot.psf'
      GTYPE=0
      NCHAN=1
      SIZE=.12
      DSPLAC=0.
      RDS=0.
      NTITX=22
      NTITY=8
      NTITLE=13
      NORM=.FALSE.
      TICS=0
      LABTOP=1
      TTLTOP=1
      XLNGTH=9.0
      YLNGTH=7.0
      XSCLMN=0.
      XSCLMX=0.
      YSCLMN=0.
      YSCLMX=0.
      TTL='IPL LINE PLOT'
      XTTL='RELATIVE SAMPLE NUMBER'
      YTTL='DN VALUE'
      DO 5 J=1,10
        XMIN(J)=0.
        XMAX(J)=0.
        YMIN(J)=0.
        YMAX(J)=255.
        HALF(J)=0
    5 CONTINUE
      XPAGE=0.5
      iiline = 1
      i2line = 0
C
C        OPEN INPUT DATA SETS
C
      CALL XVP('INP',LPARM,NI)
      DO 10 I=1,NI
      CALL XVUNIT(UNIT(I),'INP',I,STAT,' ')
      CALL XVOPEN(UNIT(I),STAT,'U_FORMAT','REAL',' ')
      CALL XVGET(UNIT(I),STAT,'NL',NLI(I),'NS',NSI(I),
     &           'FORMAT',FORMAT(I),' ')
      IF (FORMAT(I).EQ.'HALF') HALF(I)=1
   10 CONTINUE
C
C        *** PROCESS PARAMETERS ***
C
C  'NCHAN'
      CALL XVPARM('NCHAN',NCHAN,ICOUNT,IDEF,1)
      NSCHAN=NSI(1)/NCHAN
C  'PROCESS'
      CALL XVPARM('PROCESS',IPARM,ICOUNT,IDEF,50)
      IF(ICOUNT.NE.0) THEN
         GTYPE=1
         NLINES=ICOUNT/5
         IF(5*NLINES.NE.ICOUNT) THEN
            CALL XVMESSAGE('INVALID COUNT FOR PARAMETER "PROCESS"',' ')
            CALL ABEND
         END IF
         DO I=1,NLINES
            IN(I)=IPARM(5*(I-1)+1)
            SL(I)=IPARM(5*(I-1)+2)
            SS(I)=IPARM(5*(I-1)+3)
            EL(I)=IPARM(5*(I-1)+4)
            ES(I)=IPARM(5*(I-1)+5)
            IF(IN(I).LT.1.OR.IN(I).GT.NI) THEN
               CALL mabend ('INVALID INPUT NUMBER SPECIFIED',' ')
            ENDIF
            IF(SL(I).LT.1) CALL MABEND('INVALID STARTING LINE')
            IF(SS(I).LT.1) CALL MABEND('INVALID STARTING SAMPLE')
            IF(EL(I).GT.NLI(IN(I))) CALL MABEND('INVALID ENDING LINE')
            IF(ES(I).GT.NSI(IN(I)))CALL MABEND('INVALID ENDING SAMPLE')
	    IF (SL(I).EQ.EL(I) .AND. SS(I).EQ.ES(I)) then
       	       CALL MABEND('NULL LINE SEGMENT SPECIFIED')
            endif
            IF (FORMAT(IN(I)).EQ.'HALF') YMAX(I)=32767
         END DO
      END IF
C  'SPROCESS'
      CALL XVPARM('SPROCESS',IPARM,ICOUNT,IDEF,20)
      IF(ICOUNT.NE.0) THEN
         IF(GTYPE.NE.0) THEN
            CALL XVMESSAGE
     &        ('CANNOT SPECIFY BOTH PROCESS AND SPROCESS',' ')
            CALL ABEND
         END IF
         IF(NI.NE.1) THEN
            CALL XVMESSAGE
     &         ('SPECTRAL PLOTS REQUIRE 1 INPUT IN MSS FORMAT',' ')
            CALL ABEND
         END IF
         IF(NCHAN.EQ.1) THEN
            CALL XVMESSAGE('MUST SPECIFY NCHAN FOR SPECTRAL PLOTS',' ')
            CALL ABEND
         END IF
         GTYPE=2
         NLINES=ICOUNT/2
         IF(2*NLINES.NE.ICOUNT) THEN
            CALL XVMESSAGE('INVALID COUNT FOR PARAMETER "SPROCESS"',' ')
            CALL ABEND
         END IF
         DO I=1,NLINES
            IN(I)=1
            SL(I)=IPARM(2*(I-1)+1)
            SS(I)=IPARM(2*(I-1)+2)
         END DO
	TITLE = 'IPL SPECTRAL PLOT' 
        NTITLE=17
	XTITLE = 'CHANNEL NUMBER'
         NTITX=14
         IF(FORMAT(1).EQ.'HALF') YMAX(1)=32767
      END IF
C  'LABELSIZ'
      CALL XVPARM('LABELSIZ',SIZE,ICOUNT,IDEF,1)
C  'LOLABEL'
      IF(XVPTST('LOLABEL')) LABTOP=0
C  'TICS'
      IF(XVPTST('TICS')) TICS=1
C  'DISPLACEMENT'
      CALL XVPARM('DISPLACE',DSPLAC,ICOUNT,IDEF,1)
C  'XLENGTH'
      CALL XVPARM('XLENGTH',XLNGTH,ICOUNT,IDEF,1)
C  'YLENGTH'
      CALL XVPARM('YLENGTH',YLNGTH,ICOUNT,IDEF,1)
C  'XSCALE'
      CALL XVPARM('XSCALE',RPARM,ICOUNT,IDEF,2)
      IF(ICOUNT.EQ.2) THEN
         XSCLMN=RPARM(1)
         XSCLMX=RPARM(2)
      ENDIF
C  'YSCALE'
      CALL XVPARM('YSCALE',RPARM,ICOUNT,IDEF,2)
      IF(ICOUNT.EQ.2) THEN
         YSCLMN=RPARM(1)
         YSCLMX=RPARM(2)
      ENDIF
C  'XVALUES'
      CALL XVPARM('XVALUES',RPARM,ICOUNT,IDEF,20)
      IF(ICOUNT.GE.2) THEN
         N=ICOUNT/2
         IF(2*N.NE.ICOUNT) THEN
            CALL XVMESSAGE('INVALID COUNT FOR PARAMETER "XVALUES"',' ')
            CALL ABEND
         END IF
         DO I=1,N
            XMIN(I)=RPARM(2*(I-1)+1)
            XMAX(I)=RPARM(2*(I-1)+2)
         END DO
      ENDIF
C  'YVALUES'
      CALL XVPARM('YVALUES',RPARM,ICOUNT,IDEF,20)
      IF(ICOUNT.GE.2) THEN
         N=ICOUNT/2
         IF(2*N.NE.ICOUNT) THEN
            CALL XVMESSAGE('INVALID COUNT FOR PARAMETER "YVALUES"',' ')
            CALL ABEND
         END IF
         DO I=1,N
            YMIN(I)=RPARM(2*(I-1)+1)
            YMAX(I)=RPARM(2*(I-1)+2)
         END DO
      ENDIF
C  'LOTITLE'
      IF(XVPTST('LOTITLE')) TTLTOP=0
C  'NORM'
      NORM = XVPTST('NORM')
      IF(NORM) YLNGTH=5.
      IF(NORM) YSCLMX=1.
C  'RDS'
      CALL XVPARM('RDS',RDS,ICOUNT,IDEF,1)
C  'XTITLE'
      CALL XVPARM('XTITLE',CBUF,ICOUNT,IDEF,1)
      IF(CBUF.NE.XTTL) THEN
	 XTITLE = ' '
	 WRITE(XTITLE(1:),'(A)') CBUF
         NTITX=INDEX(CBUF,'   ')
         IF (NTITX .LE. 0) NTITX=52
      END IF
C  'YTITLE'
      CALL XVPARM('YTITLE',CBUF,ICOUNT,IDEF,1)
      IF(CBUF.NE.YTTL) THEN
	 YTITLE = ' '
	 WRITE(YTITLE(1:),'(A)') CBUF
         NTITY=INDEX(CBUF,'   ')
         IF (NTITY .LE. 0) NTITY=52
      END IF
C  'TITLE'
      CALL XVPARM('TITLE',CBUF,ICOUNT,IDEF,1)
      IF(CBUF.NE.TTL) THEN
	 TITLE = ' '
	 WRITE(TITLE(1:),'(A)') CBUF
         NTITLE=INDEX(CBUF,'   ')
         IF (NTITLE .LE. 0) NTITLE=52
      END IF
C
C  'PLOTOUT'
C     Resolve output PostScript filename
      CALL XVPARM('PLOTOUT',CBUF,ICOUNT,IDEF,1)
      IF(IDEF .EQ. 0) THEN
          PLOTOUT = CBUF
      END IF
C
      !! Speify output PostScript filename
      call plotfn (plotout)

C  INITIALIZE PLOTTER AND SET ORIGIN AT (.5,.5)
      !! Initialize XRT/graph
      CALL XRTBEGIN(STAT)
      IF (STAT.NE.1) CALL MABEND('Unable to initialize XRT/graph')
C
C  FIND LENGTH OF LONGEST LINE
      IF(GTYPE.EQ.1) THEN
         NP=0
         DO J=1,NLINES
         NX=IABS(SL(J)-EL(J))
         NY=IABS(SS(J)-ES(J))
         NTEST=SQRT(FLOAT(NX*NX+NY*NY))+1
         IF (NTEST.GT.NP) NP=NTEST
         END DO
      END IF
      IF(GTYPE.EQ.2) NP=NCHAN
C
C   LX IS NUMBER OF BYTES NEEDED FOR X ARRAY.
C    (ONE FULLWORD FOR EACH PT. PLUS TWO MORE FOR XSCLMN AND XSCLDT)
      LX=4*(NP+2)
      LY=LX
      LCHECK=LX
C
C  DRAW X AXIS
      XSCLDT=(XSCLMX-XSCLMN)/XLNGTH
      IF (XSCLDT.NE.0.) GO TO 230
      XAXIS(1)=XMIN(1)
      XAXIS(2)=XMAX(1)
      DO J=1,NLINES
         XAXIS(1)=AMIN1(XAXIS(1),XMIN(J))
         XAXIS(2)=AMAX1(XAXIS(2),XMAX(J))
      END DO
      IF (XAXIS(1).GE.XAXIS(2)) XAXIS(2)=NP
      CALL SCALE(XAXIS,XLNGTH,2,1)
      XSCLDT=XAXIS(4)
      XSCLMN=XAXIS(3)

230   continue
      IF(TICS.EQ.1) THEN
C  SMALL
         NTICS=10*XLNGTH
         NTICS=2*XLNGTH
      END IF
C
C  DRAW Y AXIS
      YSCLDT=(YSCLMX-YSCLMN)/YLNGTH
      IF(YSCLDT.NE.0) GO TO 330
      YAXIS(1)=YMIN(1)
      YAXIS(2)=YMAX(1)
      DO J=1,NLINES
         YAXIS(1)=AMIN1(YAXIS(1),YMIN(J))
         YAXIS(2)=AMAX1(YAXIS(2),YMAX(J))
      END DO
      CALL SCALE(YAXIS,YLNGTH,2,1)
      YSCLMN=YAXIS(3)
      YSCLDT=YAXIS(4)
      YSCLMX=YSCLMN+YLNGTH*YSCLDT
330   Continue
      IF(TICS.EQ.1) THEN
C           SMALL
         NTICS=10*YLNGTH
         NTICS=2*YLNGTH
      END IF
C
C  DRAW TITLE  (DEFAULT = 'IPL LINE PLOT')
      headermsg(iiline) = title
      iiline = iiline + 3

      call header (title, 1, 1) !! Title string, 1 line, adjust center
      call axestitles (xtitle, ytitle,90,' ',0)

      labels (1) = ' '
      do II = 1, 11
         write (msg (1:),'(a)') 'Line   ' 
         write (msg (6:),'(i2)') II 
         labels (II+1) = msg 
      end do

400   CONTINUE
C
      DO 850 ILINE=1,NLINES
C  SET LB=1 IF DATA SET IS SAME AS PREVIOUS ONE
      LB=0
      IF(ILINE.GT.1) THEN
         IF(IN(ILINE).EQ.IN(ILINE-1)) LB=1
      END IF
      if (iline .eq. 6) then
         i2line = iiline
         headermsg(iiline) = title
         iiline = iiline + 3
      endif
C
C  ENSURE X ARRAY IS LARGE ENOUGH TO USE AS INPUT BUFFER ALSO
      IF(LX.LT.4*NSI(IN(ILINE))) LX=4*NSI(IN(ILINE))
C
C   Set plot data-set equal to the line number being processed
      call setactiveset (iline)

C CALL SUBROUTINE GRAPH VIA STACKA AND EQUIV
      CALL STACKA(6,EQUIV,2,LX,LY,LCHECK,IND)
      IF(IND.EQ.1) GO TO 995

  850 CONTINUE

      !! Display labels
      call setlabel (labels,NLINES+1,1,5)!! Align VERTICAL, NORTHEAST corner

      !! Display graph
      call xrtpage (status)
      if (status .ne. 1) goto 9999

      !! Display labels on the 2nd and possibly the 3rd page 
      if (i2line .eq. 0) then
         !! If i2line == 0, then 5 or less samples
         call header (headermsg, iiline, 0) !! Title string, lines, adjust left
      else
         !! Display first set of labels and header
         call setlabel (labels,NLINES+1,1,4)!! Align VERTICAL, NORTHEAST corner
         call header (headermsg, i2line-1,0)!! Title string, lines, adjust left
         call xrtpage (status)
         if (status .ne. 1) goto 9999
         !! Display second set of labels and header
         call setlabel (labels,NLINES+1,1,4)!! Align VERTICAL, NORTHEAST corner
         call header (headermsg(i2line), iiline-i2line+1, 0) 
      endif
C
C  CLOSE XRT/graph
      CALL PLOT(0.,0.,999)
C
C  CLOSE INPUT DATA SETS
9999  continue
      DO 960 I=1,NI
      CALL XVCLOSE(UNIT(I),STAT,' ')
  960 CONTINUE
C
      RETURN
C
995   CALL XVMESSAGE('INSUFFICIENT CORE',' ')
      CALL PLOT(0.,0.,999)
      CALL ABEND
      END
C
C  **********************************************************
C
      SUBROUTINE EQUIV(X,LX,Y,LY,LCHECK,IND)
C
C
      IND=0
      IF(LY.LT.LCHECK) GO TO 899
      CALL GRAPH(X,X,Y)
      RETURN
C
C   INSUFFICIENT CORE RETURN
899   IND=1
      RETURN
      END
C
C  **********************************************************
C
      SUBROUTINE GRAPH(X,RBUF,Y)
C
      COMMON/C1/ SIZE,DSPLAC,RDS,XMIN,XMAX,YMIN,YMAX
     &          ,XSCLMN,XSCLMX,YSCLMN,YSCLMX,XSCLDT
     &          ,YSCLDT,XLNGTH,YLNGTH,HALF,NORM,NCHAN
      COMMON/C2/ SLX,SSX,ELX,ESX,INX,UNIT,ILINE,NLINES
     &          ,NLI,NSI,NSCHAN,GTYPE,XPAGE,LB,LABTOP
      common/commonheader/headermsg,iiline,i2line
      character*56 headermsg(220) !! Labels * (lines per label+2)
      integer      iiline,i2line  !! index into header strings

C
      character*56 xheadermsg
      REAL*8 MEAN,SIGMA,DBLV
      REAL*4 XMAX(10),XMIN(10),YMAX(10),YMIN(10)
     &      ,XSCLMN,XSCLMX,YSCLMN,YSCLMX
     &      ,XLNGTH,YLNGTH
      REAL*4 X(1),RBUF(1),Y(1),YT(4)
      INTEGER*4 INX(10),SLX(10),SSX(10),ELX(10),ESX(10),NLI(10),NSI(10)
      INTEGER*4 HALF(10),UNIT(10),SN,SL,SS,EL,ES,STAT,GTYPE
      LOGICAL   NORM
      CHARACTER*24 STLAB1
      CHARACTER*12 STLAB2
      CHARACTER*56  LABEL(20)
C
      STLAB1 = 'AVE GRAY LEVEL = '
      STLAB2 = 'STD DEV = '
      MEAN=0.0
      SIGMA=0.0
      INTEQ=ILINE-1
      IN=1
      LN=SLX(ILINE)
      SN=SSX(ILINE)
C
      
      IF(GTYPE.EQ.1) THEN
C
         IN=INX(ILINE)
         SL=SLX(ILINE)
         SS=SSX(ILINE)
         EL=ELX(ILINE)
         ES=ESX(ILINE)
         NSAMP=MAX0(SS,ES)
         LINC=0
         IF(EL.GT.SL) LINC=+1
         IF(EL.LT.SL) LINC=-1
         SINC=0
         IF(ES.GT.SS) SINC=+1
         IF(ES.LT.SS) SINC=-1
      END IF
C
      IF(GTYPE.EQ.2) GO TO 400
      IF(EL.EQ.SL) GO TO 100
      IF(ES.EQ.SS) GO TO 200
      GO TO 300
C
C  HORIZONTAL LINE
100   continue
      CALL XVREAD(UNIT(IN),RBUF,STAT,'LINE',LN,'NSAMPS',NSAMP,' ')
C      CALL XVCHECK('XVREAD  ',1,'INP',IN,STAT)
      NPTS=IABS(ES-SS)+1
      DO 150 IPT=1,NPTS
      Y(IPT)=RBUF(SN)
      DBLV=Y(IPT)
      MEAN=MEAN+DBLV
      SIGMA=SIGMA+DBLV*DBLV
      SN=SN+SINC
  150 CONTINUE
      GO TO 500
C
C  VERTICAL LINE
200   continue
      NPTS=IABS(EL-SL)+1
      DO 250 IPT=1,NPTS
      CALL XVREAD(UNIT(IN),RBUF,STAT,'LINE',LN,'NSAMPS',NSAMP,' ')
C      CALL XVCHECK('XVREAD  ',2,'INP',IN,STAT)
      Y(IPT)=RBUF(SN)
      DBLV=Y(IPT)
      MEAN=MEAN+DBLV
      SIGMA=SIGMA+DBLV*DBLV
      LN=LN+LINC
  250 CONTINUE
      GO TO 500
C
C  SLANT LINE
300   continue
      NX=IABS(SS-ES)
      NY=IABS(SL-EL)
      NPTS=IFIX(SQRT(FLOAT(NY*NY+NX*NX)))+1
      DZ=ATAN2(FLOAT(NY),FLOAT(NX))
      ADX=COS(DZ)
      ADY=SIN(DZ)
      DX=0.0
      DY=0.0
C
      DO 350 IPT=1,NPTS
      CALL XVREAD(UNIT(IN),RBUF,STAT,'LINE',LN,'NSAMPS',NSAMP,' ')
C      CALL XVCHECK('XVREAD  ',3,'INP',IN,STAT)
      YT(1)=RBUF(SN)
      YT(2)=RBUF(SN+SINC)
C        READ NEXT LINE OF DATA (EXCEPT FOR FIRST OR LAST POINT -
C        IN THAT CASE READ SAME LINE)
      LN2=LN+LINC
      IF (IPT.EQ.1.OR.IPT.EQ.NPTS) LN2=LN
      CALL XVREAD(UNIT(IN),RBUF,STAT,'LINE',LN2,'NSAMPS',NSAMP,' ')
C      CALL XVCHECK('XVREAD  ',4,'INP',IN,STAT)
      YT(3)=RBUF(SN)
      YT(4)=RBUF(SN+SINC)
C
      Y(IPT)=YT(1)+DX*(YT(2)-YT(1))+DY*(YT(3)+DX*(YT(4)-YT(3))-YT(1)
     &             -DX*(YT(2)-YT(1)))
      DBLV=Y(IPT)
      MEAN=MEAN+DBLV
      SIGMA=SIGMA+DBLV*DBLV
C
C        CHECK FOR LINE/SAMPLE INCREMENTING
      DX=DX+ADX
      DY=DY+ADY
      IF(DX.LT.1.0) GO TO 330
C        INCREMENT SAMPLE NUMBER
      SN=SN+SINC
      DX=DX-1.0
      IF(DY.LT.1.0) GO TO 350
C        INCREMENT LINE NUMBER
330   LN=LN+LINC
      DY=DY-1.0
  350 CONTINUE
      GO TO 500
C
C        SPECTRAL PLOT
400   continue
      CALL XVREAD(UNIT(IN),RBUF,STAT,'LINE',LN,' ')
C      CALL XVCHECK('XVREAD  ',5,'INP',IN,STAT)
      NPTS=NCHAN
      DO 450 IPT=1,NPTS
      Y(IPT)=RBUF((IPT-1)*NSCHAN+SN)
      DBLV=Y(IPT)
      MEAN=MEAN+DBLV
      SIGMA=SIGMA+DBLV*DBLV
  450 CONTINUE
C
C
C        SCALE DATA ACCORDING TO YVALUES PARAMETERS
500   continue
      DNMAX=255.0
      IF(HALF(IN).EQ.1) DNMAX=32767.0
      YINC=(YMAX(ILINE)-YMIN(ILINE))/DNMAX
      yinc = 1.0
      IF((YINC.EQ.1.).AND.(YMIN(ILINE).EQ.0.)) GO TO 620
      DO 610 ID=1,NPTS
      Y(ID)=Y(ID)*YINC+YMIN(ILINE)
  610 CONTINUE
C
C        SCALE DATA ACCORDING TO RDS PARAMETER
620   continue
      IF(RDS.EQ.0) GO TO 630
      DO 625 ID=1,NPTS
      Y(ID)=SQRT(AMAX1(Y(ID)**2-RDS**2,0.))
  625 CONTINUE
C
C        NORMALIZE DATA
630   continue
      IF (.NOT.NORM) GO TO 640
      YPEAK=Y(1)
      DO 635 ID=2,NPTS
      IF (YPEAK.LT.Y(ID)) YPEAK=Y(ID)
  635 CONTINUE
      DO 638 ID=1,NPTS
      Y(ID)=Y(ID)/YPEAK
  638 CONTINUE
C
C        ADD DISPLACEMENT
640   continue
      IF (DSPLAC.EQ.0.) GO TO 650
      DO 645 ID=1,NPTS
      Y(ID)=Y(ID)+INTEQ*DSPLAC
  645 CONTINUE
C
C        COMPUTE MEAN AND STANDARD DEVIATION
650   MEAN=MEAN/NPTS
      SIGMA=DSQRT(DABS(SIGMA/NPTS-MEAN*MEAN))
C
C        LOAD X ARRAY
      X(1)=XMIN(ILINE)
      XINC=(XMAX(ILINE)-XMIN(ILINE))/(NPTS-1)
      IF (XINC.NE.0.) GO TO 660
      X(1)=1.
      XINC=1.
  660 DO 665 IQ=2,NPTS
      X(IQ)=X(IQ-1)+XINC
  665 CONTINUE
      X(NPTS+1)=XSCLMN
      X(NPTS+2)=XSCLDT
C
      DO 670 ID=1,NPTS
      IF(Y(ID).GT.YSCLMX) Y(ID)=YSCLMX
      IF(Y(ID).LT.YSCLMN) Y(ID)=YSCLMN
  670 CONTINUE
      Y(NPTS+1)=YSCLMN
      Y(NPTS+2)=YSCLDT
      IDENSE=NPTS/XLNGTH
      IF(NLINES.EQ.1) IDENSE=0
C
      !! Set SCALE factor to 1.0, as XRT/graph will automatically scale
      !! the X & Y values before displaying the values.
      x(npts+2) = 1.0
      y(npts+2) = 1.0
      CALL LINE (X,Y,NPTS,1,IDENSE,INTEQ)
      !! Move to (0,0) and set new origin
      call setactiveset (0)
      call plot (0.0, 0.0, 3)
C
C
C
C  **********************************************************
C
C       * LABEL PROCESSING *
C
C
C

      inline = 1
      YPAGE=AMAX1(7.,YLNGTH)
      IF(LABTOP.EQ.1) YPAGE=11.5
      XL2=0.
      XL1=0
      IF(SIZE.EQ.0.) GO TO 800

C        CHECK IF SAME DATA SET
      IF(LB.EQ.0) GO TO 710

      headermsg (iiline) = 'SAME LABELS'
      inline =inline + 1
      YPAGE = YPAGE-2.0*SIZE
      GO TO 730
C
C        GET LABELS
710   continue

      CALL LABGET(UNIT(IN),NLAB,LABEL)
C        PRINT LABELS
      NCH=56

      xheadermsg = ' '
      write (xheadermsg (1:),'(a)') 'Line   ' 
      write (xheadermsg (6:),'(i2)') ILINE
      headermsg (iiline) = xheadermsg 
      iiline = iiline + 1 

      DO 720 ILAB=1,NLAB
C      CALL SYMBOL(XPAGE,YPAGE,SIZE,%DESCR(LABEL(1,ILAB)),0,0.,NCH)
      
         headermsg (iiline) = label(ilab) 
         iiline = iiline + 1 

  720 CONTINUE
C        PRINT MEAN AND STANDARD DEVIATION
730   continue

      write (xheadermsg (1:),'(a)') stlab1       !! 'AVE GRAY SCALE = '
      write (xheadermsg (18:),'(f6.2)') mean 
      headermsg (iiline) = xheadermsg
      iiline = iiline + 1

      write (xheadermsg (1:),'(a)') stlab2       !! 'STD DEV = '
      write (xheadermsg (11:),'(f6.2)') sigma 
      headermsg (iiline) = xheadermsg
      iiline = iiline + 1

C        PRINT SL, SS, EL, ES
      IF(GTYPE.EQ.1) THEN
         write (xheadermsg (1:),'(a)') 'SL='
         write (xheadermsg (4:),'(i3)') SL 
         write (xheadermsg (11:),'(a)') 'SS='
         write (xheadermsg (14:),'(i3)') SS 
         headermsg (iiline) = xheadermsg
         iiline = iiline + 1

         write (xheadermsg (1:),'(a)') 'EL='
         write (xheadermsg (4:),'(i3)') el 
         write (xheadermsg (11:),'(a)') 'EL='
         write (xheadermsg (14:),'(i3)') es 
         headermsg (iiline) = xheadermsg
         iiline = iiline + 1

      ELSE
         write (xheadermsg (1:),'(a)') 'LINE='
         write (xheadermsg (4:),'(f6.2)') float(ln) 
         write (xheadermsg (11:),'(a)') 'SAMPLE='
         write (xheadermsg (44:),'(f6.2)') float(sn) 
         headermsg (iiline) = xheadermsg
         iiline = iiline + 1

      END IF
C
800   XL=AMAX1(XL1-XPAGE,XL2-XPAGE)
      XPAGE=XPAGE+XL+0.5
      iiline = iiline + 2       !! Bump index for header strings
C
      RETURN
      END

C
C
C
C  **********************************************************
C
C
C
      SUBROUTINE LABGET(UNIT,NLAB,LABEL)

      INTEGER*4 INSTAN(200),STAT,UNIT,COUNT
      CHARACTER*500 VALUE
      CHARACTER*32 FORMAT
      CHARACTER*28 TIME,LTIME
      CHARACTER*8 TASKS(200),UNAME,LUNAME
      CHARACTER*32 KEY,LKEY
      CHARACTER*1600 LTASKS
C     LOGICAL*1 LTASKS(1600),LUNAME(8),LKEY(32)
      CHARACTER*56   LABEL(20)
C      LOGICAL*1 LABEL(56,20),LTIME(28),LVALUE(500)
      EQUIVALENCE (TASKS,LTASKS),(UNAME,LUNAME),(TIME,LTIME)
      EQUIVALENCE (KEY,LKEY),(VALUE,LVALUE)

C        BLANK OUT LABEL BUFFER AND INITIALIZE LABEL POINTER
      DO I=1,20
         LABEL(I) = ' '
      ENDDO
C      CALL MVE(1,20*56,' ',LABEL,0,1)
      ILAB=1
      NTASKS=200
C
C        GET NAMES OF ALL HISTORY TASKS
      CALL XLHINFO(UNIT,TASKS,INSTAN,NTASKS,STAT,' ')
C      CALL XVCHECK('XLHINFO ',1,'INP',UNIT,STAT)
C
      DO 200 I=1,NTASKS
C        GET USER AND TIME
      CALL XLGET(UNIT,'HISTORY','USER',UNAME,STAT,'HIST',TASKS(I),
     &           'INSTANCE',INSTAN(I),'FORMAT','STRING',' ')
C      CALL XVCHECK('XLGET   ',1,'INP',UNIT,STAT)
      CALL XLGET(UNIT,'HISTORY','DAT_TIM',TIME,STAT,'HIST',
     &           TASKS(I),'INSTANCE',INSTAN(I),'FORMAT','STRING',' ')
c      CALL XVCHECK('XLGET   ',2,'INP',UNIT,STAT)
C        CONVERT DAT_TIM TO UPPERCASE
      CALL CCASE(TIME,1,28)
C        FILL IN TASK, USER, TIME LINE
C                              1         2         3         4        4 
C                     1234567890123456789012345678901234567890123456789
       LABEL(ILAB) = 'TASK:            USER: '
       WRITE(LABEL(ILAB)(7:14), '(A8)' ) LTASKS(8*I-7:8*I)
       WRITE(LABEL(ILAB)(23:30), '(A8)' ) LUNAME
       WRITE(LABEL(ILAB)(33:56), '(A24)' ) LTIME 
c      CALL MVL('TASK:',LABEL(1,ILAB),5)
c      CALL MVL(LTASKS(8*I-7),LABEL(7,ILAB),8)
c      CALL MVL('USER:',LABEL(17,ILAB),5)
c      CALL MVL(LUNAME,LABEL(23,ILAB),8)
c      CALL MVL(LTIME,LABEL(33,ILAB),24)
      ILAB=ILAB+1
      IF(ILAB.GT.20) GO TO 500
C
C        SET TO CURRENT TASK
      CALL XLINFO(UNIT,'HISTORY','TASK',FORMAT,LENGTH,COUNT,
     &            STAT,'HIST',TASKS(I),'INSTANCE',INSTAN(I),' ')
C      CALL XVCHECK('XLINFO  ',1,'INP',UNIT,STAT)
      ICHAR=1
C
      DO 100 J=1,999
C        GET NEXT KEYWORD
      CALL XLNINFO(UNIT,KEY,FORMAT,LENGTH,COUNT,STAT,' ')
      IF(STAT.NE.1.OR.KEY.EQ.'TASK') GO TO 150
      IF(KEY.EQ.'DAT_TIM'.OR.KEY.EQ.'USER') GO TO 100
C        GET VALUE
      CALL XLGET(UNIT,'HISTORY',KEY,VALUE,STAT,'HIST',TASKS(I),
     &           'INSTANCE',INSTAN(I),'FORMAT','STRING',
     &           'LENGTH',LENGTH,' ')
c      CALL XVCHECK('XLGET   ',3,'INP',UNIT,STAT)
C        TRUNCATE VALUE IF KEYWORD AND VALUE WILL NOT FIT ON ONE LINE
      IF(LENGTH.GT.47) LENGTH=47
C        SEE IF KEYWORD AND VALUE WILL FIT ON PRESENT LINE
      IF(ICHAR+LENGTH+9.LT.56) GO TO 50
      ICHAR=1
      ILAB=ILAB+1
      IF(ILAB.GT.20) GO TO 500
C        FILL IN KEYWORD AND VALUE INTO LABEL BUFFER
50    WRITE(LABEL(ILAB)(ICHAR:(ICHAR+7)), '(A8)') LKEY
      WRITE(LABEL(ILAB)(ICHAR+8:ICHAR+8), '(A1)' ) '='
      WRITE(LABEL(ILAB)(ICHAR+9:), '(A)') LVALUE
C       CALL MVL(LKEY,LABEL(ICHAR,ILAB),8)
C      CALL MVL('=',LABEL(ICHAR+8,ILAB),1)
C      CALL MVL(LVALUE,LABEL(ICHAR+9,ILAB),LENGTH)
      ICHAR=ICHAR+LENGTH+11
C
  100 CONTINUE

150   ILAB=ILAB+1
      IF(ILAB.GT.20) GO TO 500

  200 CONTINUE
500   NLAB = ILAB-1

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create qplot2.imake
/***********************************************************************
                     IMAKE FILE FOR PROGRAM qplot2 

   To Create the build file give the command:

		$ vimake qplot2 			(VMS)
   or
		% vimake qplot2 			(Unix)
************************************************************************/
#define PROGRAM	qplot2 
#define MODULE_LIST qplot2.f
#define MAIN_LANG_FORTRAN
#define R2LIB
#define USES_FORTRAN 
/* #define FTN_STRING */
/* #define FTNINC_LIST fortport */
/* #define LIB_MATH77 */
#define LIB_MOTIF 
#define LIB_XRT_GRAPH 
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create qplot2.pdf
process help=*
 PARM INP        TYPE=STRING   COUNT=(1:10)
 PARM NCHAN      TYPE=INTEGER  COUNT=1         DEFAULT=1
 PARM PROCESS    TYPE=INTEGER  COUNT=(0,5:50)  DEFAULT=--
 PARM SPROCESS   TYPE=INTEGER  COUNT=(0,2:20)  DEFAULT=--
 PARM TITLE      TYPE=STRING                   DEFAULT="IPL LINE PLOT"
 PARM LOTITLE    TYPE=KEYWORD  VALID=(LOTITLE,HITITLE)  DEFAULT=HITITLE
 PARM XTITLE     TYPE=STRING   DEFAULT="RELATIVE SAMPLE NUMBER"
 PARM YTITLE     TYPE=STRING                   DEFAULT="DN VALUE"
 PARM PLOTOUT    TYPE=STRING                   DEFAULT="QPLOT.PSF"
 PARM LABELSIZ   TYPE=REAL                     DEFAULT=0.12
 PARM LOLABEL    TYPE=KEYWORD  VALID=(LOLABEL,HILABEL)  DEFAULT=HILABEL
 PARM TICS       TYPE=KEYWORD  VALID=(TICS,NOTICS)      DEFAULT=NOTICS
 PARM NORM       TYPE=KEYWORD  VALID=(NORM,NONORM)      DEFAULT=NONORM
 PARM RDS        TYPE=REAL                     DEFAULT=0.0
 PARM DISPLACE   TYPE=REAL                     DEFAULT=0.0
 PARM XLENGTH    TYPE=REAL     COUNT=1         DEFAULT=9.0
 PARM YLENGTH    TYPE=REAL     COUNT=1         DEFAULT=7.0
 PARM XSCALE     TYPE=REAL     COUNT=(0,2)     DEFAULT=--
 PARM YSCALE     TYPE=REAL     COUNT=(0,2)     DEFAULT=--
 PARM XVALUES    TYPE=REAL     COUNT=(0,2:20)  DEFAULT=--
 PARM YVALUES    TYPE=REAL     COUNT=(0,2:20)  DEFAULT=--
 parm nodisp keyword count=0:1 valid=nodisp default=--
 
!# annot function="VICAR Pixel Listings and Plots"
!# annot keywords=(plot,"DN values",spectral,"multi channel data",Regis,
!#   Tektronix,VRDI,Printronix,PostScript)

END-PROC
.TITLE
Line or spectral plots to VRDI, Tektronix, Regis, Printronix
.HELP
Purpose:  QPLOT2 plots the DN values along a specified straight line through
  an image.  It also does spectral plots for multi-channel data. The common
  plotting routines are used to send the plots either to a file for later
  hardcopy, or to a plotting capable quicklook device such as a REGIS or
  TEKTRONIX terminal or a VRDI image display device.  The PLOTTING command
  is used to select the output device type. Possible devices are:

        Quicklook:
          IMAGE:      A VRDI image display device
          TEKTRONIX:  A Tektronix compatible terminal.
          REGIS:      A REGIS compatible terminal (VT240, VT241, VT125)
        
        Hardcopy: (Note: Files are not automatically printed)
          PRINT:      A PRINTRONIX P-series printer
                       (Output file PRINTRONX.PLT)
          LASER:      A Talaris QUIC-based laser printer
                      *** This is not currently supported ***
                       (Output file LSRPLT01.Q)
          CALCOMP:    *** See the cognizant engineer for the CALCOMP 
                      *** for instructions.

Parameters:  There are two types of parameters:

      (1) Parameters with a single specification:
		INP		NCHAN
		TITLE		LOTITLE		XTITLE
		YTITLE		LABELSIZ	LOLABEL
		NORM		RDS		DISPLACE
		XLENGTH		YLENGTH
		XSCALE		YSCALE
      (2) Parameters which have a specification for each line plotted:
		PROCESS		SPROCESS
                XVALUES         YVALUES

Restrictions:
  (1) Maximum number of lines plotted is 10.
  (2) Maximum x axis length is 6 feet. (On PRINTRONIX)
  (3) Maximum y axis length is 12 inches. (On PRINTRONIX)
  (4) Spectral plots require a single input in mss format.

Note:  This program makes use of multiple intermediate data sets which are
  deleted when the program executes normally.  If this program is run under
  a default directory which does not allow the creation of these files
  (either because of disk quotas or protection) the program will die a
  horrible death and the plot file will not be created.  Also if for some
  reason other than this, the program dies a horrible death, there could
  be a couple of miscellaneous files laying around.

History:

  Original Programmer:  John H. Reimer,  22 Aug. 1982
  Converted to Vicar2 by:  John H. Reimer,  22 April 1985
  Cognizant Programmer:  John H. Reimer
  Ported to Unix (MSTP S/W Conversion) C. Randy Schenk (CRI) 10 JUly 1995
.level1
.variable inp
STRING - Input data set
.variable process
INTEGER - DSN1,SL1,SS1,EL1,ES1, DSN2,SL2,SS2,EL2,ES2, ...
.variable nchan
INTEGER - Number of channels in mss formatted input data set
.variable sprocess
INTEGER - LINE1,SAMPLE1, LINE2,SAMPLE2, ...
.variable title
STRING - Title of plots.
.variable lotitle
KEYWORD - Lower position for title.
.variable xtitle
STRING - X axis title
.variable ytitle
STRING - Y axis title
.variable labelsiz
REAL - Height of label (inches).
.variable lolabel
KEYWORD - Lower position for label.
.variable tics
KEYWORD - Put 10 tic marks per inch.
.variable norm
KEYWORD - Normalizes data to 1.
.variable rds
REAL - DN scaling factor.
.variable displace
REAL - Displacement for subsequent lines.
.variable xlength
REAL - Length of X axis (inches).
.variable ylength
REAL - Length of Y axis (inches).
.variable xscale
REAL - Min & Max of X axis.
.variable yscale
REAL - MIN & Max of Y axis.
.variable xvalues
REAL - Rescaling factors.
.variable yvalues
REAL - Rescaling factors.
.vari nodisp
Option to suppress display and create
postscript file instead.
.vari plotout
Name of the output postscript file.
.level2
.variable inp
Input data set; maximum number of 10.
.variable process
Specifies one or more lines to be plotted. Following the keyword,
each plot is specified by a set of five numbers. The first value in
each set specifies the Input Data Set Number and the remaining four
values specify the Starting Line, Starting Sample, Ending Line, and Ending
Sample. (No Default).  NOTE:  The line plotted will cover
SQRT((EL-SL)**2+(ES-SS)**2) "relative samples" along the horizontal axis
and will start at "relative sample" 1.
.variable nchan
Specifies the number of spectral channels in an input data set in mss
format. This keyword is used in conjunction with the SPROCESS keyword.
(Default is 1)
.variable sprocess
Specifies one or more spectral plots. This keyword requires that the input
be in mss format and that NCHAN is specified. Following the keyword,
each spectral plot is specified by a set of two numbers. The first value
in each set specifies the Line and the second value the Sample of the
point within the first channel through which the spectral plot is to be
done. (No Default)
.variable title
Used to specify the title of plots (Max length of 52 characters).
(Default is 'IPL LINE PLOT', or for spectral plots, 'IPL SPECTRAL PLOT')
.variable lotitle
Specifies that the title will be written within the 8.5 x 11 area
(if the y axis length is less than or equal to 7 inches). (Default
is to place the title at the top of the page)
.variable xtitle
Specifies the title for the X axis (Max length of 52 characters). (Default is
'Relative Sample Number')
.variable ytitle
Specifies the title for the Y axis (Max length of 52 characters). (Default is
'DN Value')
.variable labelsiz
Specifies the height in inches of the label characters. For a value
of 0.0, no labels are printed. (Default is 0.12)
.variable lolabel
Specifies that labels will be written within the 8.5 x 11 space.
(Default is to place the labels at the top of the page.)
.variable norm
Causes DN values to be scaled linearly so that the largest value
becomes 1.  The length of the y axis is set to 5 inches.
(Default is that this is not done.)
.variable rds
Causes DN values to be scaled by the following equation:
OUT=SQRT(IN**2-RDS**2). (Default is that this is not done)
.variable displace
Specifies that subsequent lines on the same plot will be displaced
by the given amount.  This is specified in terms of the final plotted
vertical values, rather than input DN, in the cases where the input
values are scaled. (Default is 0.0)
.variable xlength
Specifies the length in inches of the X axis. (Default is 9.0; Max is 72.0)
.variable ylength
Specifies the length in inches of the Y axis. (Default is 7.0; max is 12.0)
.variable xscale
Specifies the scale used along the X axis.  The X axis will be drawn going
from a minimum of the first value to a maximum of the second.  The defaults
for these values are obtained by determining the minimum and maximum X
values to be plotted on the axis and then passing these values to the
subroutine SCALE.
SCALE determines the scaling so that 1 inch along the axis will always be
an interval of 1,2,3,4,5,6 or 8*(10**n) units.  This usually results in
having the plotted lines occupy only a portion of the axis.  By using the
XSCALE keyword the user can force plots to occupy a greater portion of the
X axis.  Axis values are printed every inch, however, and if it is desired
that these values be nice round numbers the quantity:
                  (XSCALEMAX-XSCALEMIN)/XLENGTH
should be a nice round number.
.variable yscale
Specifies the scale used along the Y axis.  The Y axis will be drawn going
from a minimum of the first value to a maximum of the second.  The defaults
for these values are obtained by determining the minimum and maximum Y
values to be plotted on the axis and then passing these values to the
subroutine SCALE.
SCALE determines the scaling so that 1 inch along the axis will always be
an interval of 1,2,3,4,5,6 or 8*(10**n) units.  This usually results in
having the plotted lines occupy only a portion of the axis.  By using the
YSCALE keyword the user can force plots to occupy a greater portion of the
Y axis.  Axis values are printed every inch, however, and if it is desired
that these values be nice round numbers the quantity:
                  (YSCALEMAX-YSCALEMIN)/YLENGTH
should be a nice round number.
.variable xvalues
Allows the user to rescale the actual x (sample) data values of the
lines to be plotted. XVALUES is followed by two real values specifying the
minimum and maximum x values. The first value (the minimum) is the point
along the X axis where the first data point will be plotted.  The second
value (the maximum) is the point along the X axis where the last data
point will be plotted.
(DEFAULTS: XMIN=1.0, XMAX=SQRT[(EL-SL)**2+(ES-SS)**2]+1
.variable yvalues
Allows the user to rescale the actual y (DN) data values of the lines
to be plotted. YVALUES is followed by groups of two real values, one
group for each line to be plotted. The two values in each group specify the
minimum and maximum y values. The first value (the minimum) is the point
along the Y axis where a DN of zero will be plotted.  The second value
(the maximum) is the point along the Y axis where a DN of 255 (32767
for halfword data) will be plotted.
(DEFAULT: YMIN=0.0, YMAX=255 (byte) YMAX=32767 (halfword)   )

.vari plotout
The name of the output postscript file if NODISP specified.

Note that this file will always be created, but it will be empty unless
NODISP is specified.

.end
$ Return
$!#############################################################################
$Test_File:
$ create tstqplot2.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
gen inp1 nl=20 ns=20
gen inp2 nl=20 ns=20 linc=10 sinc=10
gen inp3 nl=128 ns=128 linc=5 sinc=5
!
! Just defaults
!
qplot2 (inp1,inp2,inp3) proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70) +
        plotout="qplot1.psf"
!
! create postscript output:
qplot2 (inp1,inp2,inp3) proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70) +
        plotout="qplot1.psf" 'nodisp
!
! Change some labels
!
qplot2 (inp1,inp2,inp3) title="MIPL test run for qplot2" +
        xtitle="This is the X axis" ytitle="This is the Y axis" +
        proc=(3,1,1,64,64) +
        plotout="qplot2.psf"
!
end-proc
$!-----------------------------------------------------------------------------
$ create tstqplot2.log
tstqplot2
gen inp1 nl=20 ns=20
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen inp2 nl=20 ns=20 linc=10 sinc=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen inp3 nl=128 ns=128 linc=5 sinc=5
Beginning VICAR task gen
GEN Version 6
GEN task completed
qplot2 (inp1,inp2,inp3) proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70)  +
        plotout="qplot1.psf"
Beginning VICAR task qplot2
QPLOT2 version 10-July-95
qplot2 (inp1,inp2,inp3) proc=(1,1,1,20,20, 2,1,1,20,20, 3,50,50,70,70)  +
        plotout="qplot1.psf" 'nodisp
Beginning VICAR task qplot2
QPLOT2 version 10-July-95
qplot2 (inp1,inp2,inp3) title="MIPL test run for qplot2"  +
        xtitle="This is the X axis" ytitle="This is the Y axis"  +
        proc=(3,1,1,64,64)  +
        plotout="qplot2.psf"
Beginning VICAR task qplot2
QPLOT2 version 10-July-95
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
