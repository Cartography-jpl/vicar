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
