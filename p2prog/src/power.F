      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C     POWER SPECTRUM PROGRAM
C
C     04/16/97  ... SP  ... Made portable for UNIX & VMS.  Replaced handling
C                           of SIZE parameter with call to XVSIZE.  Revised
C                           plotting code using program OTF1 as a guide.
C                           Revised label processing code to do string searches
C                           in an uppercase copy of the label data; this turned 
C                           out to be unnecessary: the problem was that the 
C                           FORMAT optional is needed in the XLGET.
C     11/16/95  ... CCA ... MADE SCAL R*4
C     07/18/94  ... CCA ... ADD OPTION OF NO TABLE COLUMN HEADERS
c     06/14/94  ... CCA ... FIXED FMAX, DELETED HISTOGRAM PLOT
c     04/11/94  ... CCA ... ADDED DNSCALE
c     02/22/94  ... CCA ... MAJOR CODE CLEANUP, REWORK OF LABEL HANDLING,
c			    TEST FILE AND PLOTTING OF LABELS
C     11/22/93  ... CCA ... ADD ASCII TABLE OUTPUT, CHECKS FOR OPEN
C                           ERRORS, FIX TEST, MOD SELECTION OF PLOT
C                           OR PRINT
C     09/01/84  ... FFM ... CONVERT FROM IBM TO VAX
C               T. RINDFLEISCH ... ORIGINAL RELEASE
C
      IMPLICIT COMPLEX(C),CHARACTER*1(H),REAL*8(D)
      DIMENSION C(1024),POW(513),PLAT(1025)

      COMMON /C1/ NLI,NSI

      REAL SCAL,KPP,BSCALE
      CHARACTER*60 FILEMSG
      CHARACTER*52 HTITX,HTITLE
      CHARACTER*255 FILENAME,PLOTFILE,TBL
      CHARACTER*80  HEADR(70)    !strings for the plot header ala OTF1.
      CHARACTER*60 MSG2
      CHARACTER*24 MSG3
      CHARACTER*24 MSG5
      CHARACTER*24 MSG6
      CHARACTER*1 TAB

      CHARACTER*24 MESS
      CHARACTER*24 TITY
      INTEGER STATUS,COUNT,DEF,SL,SS
      INTEGER PLOTT,PTR,EL
      INTEGER*2 BUF(1024)
      LOGICAL XVPTST

      EQUIVALENCE (C(181),PLAT(1))

      DATA MESS/' MIPL RMS POWER SPECTRUM'/
      DATA TITY/' AMPLITUDE   (DN P-P)   '/
      DATA MSG2/'    TRANSFORM  SL =        SS =        NL =        NS =
     +     '/
      DATA MSG3/'         POINT TRANSFORM'/
      DATA MSG5/'    MEAN (DN) =         '/
      DATA MSG6/'    SIGMA(DN) =         '/


c	PRINT = 0
	PLOTT = 0
      HTITX(1:52)=' '
      HTITLE(1:52)=' '

C       OPEN INPUT DATA 
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'U_FORMAT','HALF','OPEN_ACT', 'SA',
     .             'IO_ACT', 'SA' ,' ')

C       GET INPUT SIZE
      CALL XVSIZE( SL, SS, NL, NS, NLI, NSI)
C
C       PARAM 'EXPONENT'
      CALL XVPARM('EXPONENT',NP,COUNT,DEF,1)
      IF (COUNT .EQ. 0) THEN
         IF (NS .GE. 1024) THEN		!EXPONENT NOT GIVEN, MAKE ONE
		NP=10
	 ELSE IF (NS .GE. 512) THEN
		NP=9
	 ELSE IF (NS .GE. 256) THEN
		NP=8
	 ELSE IF (NS .GE. 128) THEN
		NP=7
	 ELSE IF (NS .GE. 64) THEN
		NP=6
	 ELSE IF (NS .GE. 32) THEN
		NP=5
	 ELSE IF (NS .GE. 16) THEN
		NP=4
	 ELSE 
         	NP=3
	 END IF
      END IF
C
C       PARAM 'SCALE'              !DN PER INCH OF PLOT
      CALL XVPARM('SCALE',SCAL,COUNT,DEF,1)
C
C       PARAM 'DNSCALE'              !SCALING OF DNS BEFORE POWER
      CALL XVPARM('DNSCALE',BSCALE,COUNT,DEF,1)
C
C       PARAM 'FMAX'
      CALL XVPARM('FMAX',FMAX,COUNT,DEF,1)
C
C       PARAM 'TITLE'
      CALL XVPARM('TITLE',HTITLE,COUNT,DEF,1)
      IF (COUNT .GT. 0) THEN
         CALL XVSPTR(HTITLE,COUNT,PTR,NTITLE)  !GET LENGTH AS ENTERED.
      ELSE
         NTITLE=0
      END IF
C
C       PARAM 'TITLEX'
	CALL XVPARM('TITLEX',HTITX,COUNT,DEF,1)
	NTITX=LEN(HTITX)
C
C       PARAM 'YLEN'
      CALL XVPARM('YLEN',YLEN,COUNT,DEF,1)
      IF((YLEN.LT.0.0).OR.(YLEN.GT. 30.0)) THEN
          CALL XVMESSAGE('ILLEGAL VALUE FOR YLEN',' ')
          CALL ABEND
      END IF
C
C	IF (XVPTST('PRINT')) PRINT=1
C
	IF (XVPTST('PLOT')) PLOTT=1

	CALL XVPARM('FILE',PLOTFILE,COUNT,DEF,1)
	IF (COUNT .EQ. 1) THEN
		PLOTT=1
	ELSE
		PLOTFILE='POWER.PLT'
	END IF
C
C----------------------------------------------------------
C       NTOT = OUTPUT SIZE IN SAMPLES
C       NS=NUMBER OF SAMPLES 
      NTOT=2**NP
      NMID=NTOT/2+1
      IF(NS.GT.NTOT) NS=NTOT
C
C       PRINT OUT THE HEADER
      CALL XVMESSAGE(MESS(2:),' ')

C----------------------------------------------------------
C------PROCESSING SECTION
C       ADJUST NL AND NS SO THEY MAKE SENSE
      IF (SL+NL-1 .GT. NLI) THEN
	CALL XVMESSAGE('NUMBER OF LINES TRUNCATED',' ')
	NL=NLI-SL+1
      END IF
      IF (SS+NS-1 .GT. NSI) THEN
	CALL XVMESSAGE('NUMBER OF SAMPLES TRUNCATED',' ')
	NS=NSI-SS+1
      END IF
      IF(NL+NS.LE.0) GO TO 997

C       NLEFT=ADDITIONAL SAMPLES POWER WILL HAVE TO GENERATE

        NLEFT=NTOT-NS
        DO J=1,NMID
         POW(J)=0.0
        END DO
C
C       DAV=SUM OF "a" IN DOUBLE PRECISION
C       DVR=SUM OF "a**2" IN DOUBLE PRECISION
      DAV=0.0D+00
      DVR=0.0D+00
      DNOR=1.0D+00/DFLOAT(NS)
C
C       READ INPUT DATA INTO HALFWORD BUFFER
      EL=SL+NL-1
      DO LINE=SL,EL
         CALL XVREAD(INUNIT,BUF,STATUS,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         RN=0.0
         RNN=0.0
C          PUT THE DN IN THE REAL PART OF COMPLEX NUMBER AND ZERO OUT THE
C          IMAGINARY PART
         DO KP=1,NS
            KPP=FLOAT(BUF(KP))/BSCALE  !NOTE THAT KPP IS DECLARED REAL.
            RN=RN+KPP
            RNN=RNN+KPP*KPP
            C(KP)=CMPLX(KPP,0.0)
         END DO
C
C          AVERAGING THE AVAILABLE INPUT DATA
         VQAV= RN/FLOAT(NS)
         IF(NLEFT.LT.1) GO TO 400
C
C          USE THE AVERAGE VALUE TO FILL IN THE ADDITIONAL SAMPLES
         DO KP=1,NLEFT
            C(NS+KP)=CMPLX(VQAV,0.0)
         END DO
  400    DAV=DAV+DBLE(RN)
         DVR=DVR+DBLE(RNN)
C    
C          1-DIMENSIONAL DIRECT FOURIER TRANSFORM
C        CALL PRNT(7,64,C,'1C*8 BUFFER BEFORE FFTT=.')
C
         CALL FFTT(NP,-1,C)
C
C        CALL PRNT(7,34,C,'1C*8 BUFFER AFTER FFTT=.') 
C
C          ADD THE RESULTS OF ONE LINE'S TRANSFORM TO A CUMULATIVE POWER
C          SPECTRUM

c--------Accumulate the amplitude of the complex data into POW
         CALL CMAG(NMID,C,POW)

C        CALL PRNT(7,NMID,POW,'1CUMULATIVE POWER SPECTRUM=.')
      END DO

	RNOR = 1.0/(NL*NS)

c-------Take summed amplitudes in POW and get mean Peak-to-peak amplitude
c-------at each frequency.
      DO K=1,NMID
         POW(K)= 2*POW(K)*RNOR
c        POW(K)=SQRT(POW(K)/NL)/NS*2      !wrong (see also in CMAG)
      END DO

c-----To make element one the mean DN of image, divide by 4
      POW(1)=POW(1)/4

C COMPUTE MEAN AND SIGMA FOR PRINTOUT
C      DNOR=1.0D+00/(DFLOAT(NS)*DFLOAT(NL))
	DNOR=DBLE(RNOR)
      DAV=DNOR*DAV
      VVR=DSQRT(DABS(DNOR*DVR-DAV*DAV))
      VAV=DAV
      WRITE (MSG2(20:24),'(I5)') SL
      WRITE (MSG2(32:36),'(I5)') SS
      WRITE (MSG2(44:48),'(I5)') NL
      WRITE (MSG2(56:60),'(I5)') NS
      WRITE (MSG3(4:8),'(I5)') NTOT
      WRITE (MSG5(16:24),'(F9.4)') VAV
      WRITE (MSG6(16:24),'(F9.4)') VVR
      CALL XVMESSAGE(MSG2(2:60),' ')
      CALL XVMESSAGE(MSG3(2:24),' ')
      CALL XVMESSAGE(MSG5(2:24),' ')
      CALL XVMESSAGE(MSG6(2:24),' ')

      VF=2.D0*FMAX/NTOT       !FREQ. FMAX IS AT ELEMENT NTOT/2

C------------------------------------------------------
C-------OPEN OUTPUT TEXT FILE TO PRINT ALL POINTS TO NMID
	CALL XVPARM('TABLE',TBL,COUNT,DEF,1)
	IF(COUNT .GT. 0) THEN
 	  TAB=CHAR(9)
	  OPEN(99,FILE=TBL,STATUS='UNKNOWN',IOSTAT=J,ERR=998)

	  IF (XVPTST('COLHDR')) 
     1            WRITE(99,121) 'FREQUENCY',TAB,'RMS_POWER'

	  DO 119 K=1,NMID
	    FQ = (K-1)*0.5/NMID         !FREQ. 0.5 IS AT ELEMENT NMID
119	  WRITE(99,FMT=120,IOSTAT=J,ERR=998) FQ,TAB,POW(K)

	  CLOSE(99)
	END IF

121	FORMAT(1X,A9,A1,A9)
120	FORMAT(1X,F7.4,A1,F10.3)

C------------------------------------------------------

      IF (PLOTT .EQ. 1) THEN
C-----INITIALIZE THE PLOTTER
        CALL PLOTFN(PLOTFILE)
        CALL XRTBEGIN(STATUS)
        IF (STATUS.NE.1) CALL MABEND('Unable to start up plotting')
!!        CALL SETLANDSCAPE(0)      ! SET TO 'PORTRAIT' !does not help!
        CALL DISPLAYAXES(1,1,0)   ! x,y1 and y2 axes displayed 1=yes 0=no
        CALL SETACTIVESET(0)      ! 0= no triangles (default), 1= triangles
        YLEN = YLEN*SCAL          !convert from inches to application units.
C       PLOT THE HEADER, ETC; First load all the strings for heading in HEADR.

      NHDR = 1
      HEADR(NHDR) = MESS(2:)
      NHDR = NHDR + 1

      CALL XVP('INP',FILENAME,ICNT)	!INPUT FILENAME
      FILEMSG='INPUT = '//FILENAME
      HEADR(NHDR) = FILEMSG
      NHDR = NHDR + 1

      FILEMSG='PLOT  = '//PLOTFILE	!OUTPUT PLOT FILENAME
      HEADR(NHDR) = FILEMSG
      NHDR = NHDR + 1

C-----PLOT THE LABELS
      CALL OTFLABS(INUNIT,HEADR, NHDR, *996) 
C
C PLOT AREA PROCESSED, TRANSFORM SIZE, MEAN AND SIGMA

      HEADR(NHDR) = MSG2(2:)
      NHDR = NHDR + 1
      HEADR(NHDR) = MSG3(2:)
      NHDR = NHDR + 1
      HEADR(NHDR) = MSG5(2:)
      NHDR = NHDR + 1
      HEADR(NHDR) = MSG6(2:)
      NHDR = NHDR + 1

C PLOT PARM 'TITLE'
      IF (NTITLE.NE.0) THEN
        HEADR(NHDR) = HTITLE
        NHDR = NHDR + 1
      END IF

      NHDR = NHDR-1
      CALL HEADER(HEADR,NHDR,0) !WRITE HEADR ARRAY TO PLOT FILE.

C       PLOT THE X & Y AXIS LABELS
      CALL AXESTITLES( HTITX, TITY(2:),90,  ' ', 0)

C-----DOUBLE THE NUMBER OF POINTS IN THE PLOT BUFFER AND INTERPOLATE
      DO J=1,NMID
         PLAT(2*J-1)=POW(J)
      END DO

      PLAT(2)=0.375*POW(1)+0.75*POW(2)-0.125*POW(3)
      PLAT(4)=0.5*(POW(2)+POW(3))
      PLAT(NTOT)=-0.125*POW(NMID-2)+0.75*POW(NMID-1)+0.375*POW(NMID)
      K=NMID-2

      DO J=3,K
         PLAT(2*J)=-0.0625*(POW(J-1)+POW(J+2))+0.5625*(POW(J)+POW(J+1))
      END DO

C-----THERE ARE NOW NTOT POINTS IN THE BUFFER
      VSCL=1.0
C      K=(2*NMID-1)*fmax/0.5        !change number of pts to do if fmax ne .5
      K=NTOT*fmax/0.5               !change number of pts to do if fmax ne .5
      X=0.0
      Y=YLEN
c
C-----MOVE TO (X,Y)
      CALL PLOT(X,Y,3)

      DO J=1,K
	 X = (J-1)*fmax/K          ! use application units.
         Y=VSCL*PLAT(J)
         IF(Y.GT.YLEN) Y=YLEN
         IF(Y.LT.0.0) Y=0.0
         CALL PLOT(X,Y,2)          ! DRAW TO (X,Y)
      END DO

	CALL PLOT(0.,0.,999)       ! TERMINATES ALL PLOTS

	END IF
C------------------------------------------------------

	CALL XVCLOSE(INUNIT,STATUS,' ')
	RETURN

997	CALL XVMESSAGE('REQUESTED AREA OUTSIDE INPUT PICTURE',' ')
	CALL ABEND
998	CALL XVMESSAGE('ERROR OPENING/WRITING TABLE FILE',' ')
	CALL PRNT(4,1,J,'IOSTAT=.')
	CALL ABEND
996	CALL ABEND
	END

c *********************************************************************
      SUBROUTINE CMAG(NMID,COMP,POW)
C       has to use CCOMP instead of COMP, otherwise there will be an error 
C       of multiple declaration of names
C       add the results of one line's transform to a cumulative power spectrum
C       NMID  = number of frequencies to be updated 
C       COMP  = input array of data (COMPLEX * 8)
C       POW   = power spectrum array
C       POW(I) = POW(I) + ABS(COMP(I))**2
      REAL * 4 POW(1)
      COMPLEX * 8 COMP(1)
c-----sum the amplitudes of the complex data
      DO I=1,NMID
       POW(I)=POW(I)+CABS(COMP(I))
c       POW(I)=POW(I)+CABS(COMP(I))**2            wrong
      END DO
      RETURN
      END

c *************************************************************
	SUBROUTINE OTFLABS(INU,HEADR,ICNT,*)
	IMPLICIT INTEGER*4 (A-Z)
	INTEGER      LV2(60),LPL(60),ICNT
	CHARACTER*72 V1(60),V2(60),PL(60),HT(60)
        CHARACTER*80 HEADR(70)

	CALL LABPROC(INU,V1,NV1,V2,LV2,NV2,PL,LPL,NPP,HT,NH)

	IF (NV1 .GT. 0) THEN
	 DO I=1,NV1
            HEADR(ICNT)=V1(I)
            ICNT=ICNT+1
	 END DO

	ELSE IF (NPP .GT. 0) THEN
	 DO I=1,NPP
            HEADR(ICNT)=PL(I)
            ICNT=ICNT+1
	 END DO

	ELSE IF (NV2 .GT. 0) THEN
	 DO I=1,NV2
            HEADR(ICNT)=V2(I)
            ICNT=ICNT+1
	 END DO

	ELSE
	   CALL XVMESSAGE('LABEL ERROR',' ')
	   RETURN 1
	END IF

	IF (NH .GT. 0) THEN
	 DO I=1,NH
            HEADR(ICNT)=HT(I)
            ICNT=ICNT+1
	 END DO
	END IF

	RETURN
	END
C------------------------------------------------------------------
	SUBROUTINE KVPAIR(IU,PAIR,LNG,NUM)
C-------EXTRACTS KEYWORD=VALUE STRINGS FROM FIRST PROPERTY LABEL
	IMPLICIT INTEGER*4 (A-Z)
	INTEGER*4 LNG(60)
	character*72 pair(60)
	character*2048 buf
	character*2048 UPBUF   !UPPER case version of data in buf.  Label
                               !info is mixed case under UNIX.  This way
                               !we don't have to test for PROPERTY or Property.

	max=2048
        buf = ' '              ! This blanks all of buf.
	call xlgetlabel(iu,buf,max,ist)
        UPBUF = buf            !copy data
        call UPRCASE(UPBUF)  !convert to upper case for case insensitive tests
	i=0
	b = index(UPBUF(1:),'PROPERTY')
	IF (B .EQ. 0) GO TO 100

50	L=INDEX(UPBUF(B+1:),' ')
	L=B+L
	Q=INDEX(UPBUF(B+1:),'''')
	Q=B+Q
	IF(Q .LT. L) THEN		!ARE THERE QUOTES IN THE VALUE?
		Q2=INDEX(UPBUF(Q+1:),'''')
		Q2=Q+Q2
		L=INDEX(UPBUF(Q2+1:),' ')
		L=Q2+L
	END IF
	E=L-1
C-------STOP WHEN REACH A 'TASK' OR NEXT PROPERTY LABEL
	IF(UPBUF(B:B+3).EQ. 'TASK') GO TO 100
	IF(I .GT. 1 .AND. UPBUF(B:B+3).EQ. 'PROP') GO TO 100
	I=I+1
	KK=E-B+1
	LNG(I) = KK			!STORE LENGTH
	PAIR(I) = BUF(B:E)        !STORE STRING (mixed case).
	IF(E .EQ. MAX) GO TO 100        !END OF LABEL?
	DO J=E+1,MAX			!LOOK FOR NEXT STRING
	B=J
	IF(BUF(J:J) .NE. ' ') GO TO 50
	END DO
100	NUM=I
	RETURN
	END
c ************************************************************
	subroutine V2PAIR(IU,PAIR,LNG,NUM)
C-------RETURN THE KEYWORD=VALUE PAIRS FROM THE FIRST V2 TASK
	IMPLICIT INTEGER*4 (A-Z)
	INTEGER*4 LNG(60)
	character*72 pair(60)
	character*2048 buf
	character*2048 UPBUF   !UPPER case version of data in buf.  Label
                               !info is mixed case under UNIX.  This way
                               !we don't have to test for PROPERTY or Property.

	max=2048
        buf = ' '              ! This blanks all of buf.
	call xlgetlabel(iu,buf,max,ist)
        UPBUF = buf            !copy data
        call UPRCASE(UPBUF)  !convert to upper case for case insensitive tests

	i=0
	IF (index(UPBUF(1:),'PROPERTY') .GT. 0) GO TO 100
	B = index(UPBUF(1:),'TASK')
	IF (B .EQ. 0) go to 100

50	L=INDEX(UPBUF(B+1:),' ')
	L=B+L
	Q=INDEX(UPBUF(B+1:),'''')
	Q=B+Q
	IF(Q .LT. L) THEN
		Q2=INDEX(UPBUF(Q+1:),'''')
		Q2=Q+Q2
		L=INDEX(UPBUF(Q2+1:),' ')
		L=Q2+L
	END IF
	E=L-1
C-------STOP WHEN ENCOUNTER THE SECOND TASK
	IF(I .GT. 1 .AND. UPBUF(B:B+3).EQ. 'TASK') GO TO 100
	I=I+1
	KK=E-B+1
	LNG(I) = KK			!STORE LENGTH
	PAIR(I) = BUF(B:E)	!STORE STRING  (mixed case)
	IF(E .EQ. MAX) GO TO 100	!END OF LABEL?
	DO J=E+1,MAX			!LOOK FOR NEXT STRING
	B=J
	IF(BUF(J:J) .NE. ' ') GO TO 50
	END DO
100	NUM=I
	RETURN
	END

      SUBROUTINE LABPROC(IUNI,LABV1,NV1,VPAIR,LV2,NV2,PPAIR,LPL,
     1                   NPP,HSTRY,NH)
      IMPLICIT INTEGER(A-Z)
      INTEGER INSTANCES(20),LV2(60),LPL(60),STAT
      CHARACTER*8 TASKS(20)
      CHARACTER*12 UNAME
      CHARACTER*28 TIME
      CHARACTER*72 HBUF,BL
      CHARACTER*72 LABV1(60),HSTRY(60),VPAIR(60),PPAIR(60)
      CHARACTER*4320 LABS

c--------------------------------------------------------------
c-------get all vicar1 labels
	NV1=60
        CALL VIC1LAB(IUNI,STAT,NV1,LABS,60)
	DO J=1,NV1
           LABV1(J) = LABS(72*(J-1)+1:72*J)  !BREAK INTO PIECES 72 CHARS LONG.
	END DO

c----------------------------------------------------------------
c-------get keyword=value pairs from vicar property labels
	NPP=0
	IF (NV1 .EQ. 0) call kvpair(IUNI,ppair,LPL,npp)

c----------------------------------------------------------------
c-------get keyword=value pairs from first vicar2 task
	NV2=0
	IF (NV1 .EQ. 0 .AND. NPP .EQ. 0) 
     1            call v2pair(IUNI,vpair,LV2,nv2)

c----------------------------------------------------------------

c-------get user and date for each vicar2 history task
        BL(1:36) = '------------------------------------'
        BL(37:72) = '------------------------------------'
	BL(1:1) = ' '
	BL(6:10) = 'TASK:'
	BL(23:27) = 'USER:'

	nh=20                             !EXTRACT VIC*2 LAB
	CALL XLHINFO(IUNI,TASKS,INSTANCES,NH,STAT,' ')

	DO 801 J=1,NH
         LD = 0
         LU = 0
	CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',TASKS(J),
     *            'INSTANCE',INSTANCES(J),'LENGTH',LU,
     *            'FORMAT','STRING','ULEN',12,' ')
         IF (STAT .NE. 1) 
     *     CALL XVMESSAGE('Warning: USER label item not found',' ')
        CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',TASKS(J),
     *            'INSTANCE',INSTANCES(J),'LENGTH',LD,
     *            'FORMAT','STRING','ULEN',28,' ')
         IF (STAT .NE. 1) 
     *     CALL XVMESSAGE('Warning: DAT_TIM label item not found',' ')

	HBUF(1:72) = BL(1:72)
	HBUF(11:18) = TASKS(J)
	if (LU .GT. 0) HBUF(28:28+LU-1) = UNAME(1:LU)
	IF (LD .GT. 0) HBUF(40:40+LD-1) = TIME(1:LD)
801	HSTRY(J) = HBUF

      RETURN
      END
