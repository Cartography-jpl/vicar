C-------PROGRAM OTF1
C 05 AUG 1997 ...TXH... Ported from VAX to MIPS Env.  Merged changes
C                       made by CCA and the previous MIPS version.
C 10 JUL 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
         COMMON/C1/R,A,REALV,AIMAGV,WTS,AMPL9,WSFT9,F29,PPLOT,
     &             CPLOT,AMPL10,PARMS,OBUF

         COMPLEX*8 A(2050)

         REAL*4 R(1026),F(1026),PARMS(512),WTS(512),AMPL9(131)
         REAL*4 CPLOT(131),PPLOT(131),AMPL10(131),WSFT9(129)
         REAL*4 F29(129),REALV(129),AIMAGV(129),C1,C2,X,Y,RINL
         REAL*4 AMPLSUM,NORMSUM,DENM,CENTR,CEN,SUM1,SUM,PHASE
         REAL*4 VAL,PI,FTNORM,OBUF(400)

         INTEGER*2 IBUF(4096)
         INTEGER CNT,IUNI,STAT,OUNI,OSTAT,ZOOMFLAG
         INTEGER NUMBER_OF_LINES_WITHOUT_EDGE,NOPRT,STATUS

         CHARACTER*256 PLOT_DS
         CHARACTER*80  HEADR(70),IMESS
         CHARACTER*40  INFILE,OUTFILE,TBL
         CHARACTER*121 LO
         CHARACTER*140 ZPBUF
         CHARACTER*9   BL
         CHARACTER*101 LOG
         CHARACTER*1   LOGR,LOGI,LOGA,LOGW,BLNK,ONE,TAB
         CHARACTER*31  MPHASE
         CHARACTER*37  MCYCLE

         LOGICAL XVPTST

         DATA PI/3.1415926536E0/

         DO I = 1,101
            LOG(I:I) = '1'
         END DO

         DO I= 1,121
            LO(I:I) = '1'
         END DO
         LO(1:20) = '                    ' 

         BL=' '
         LOGR='R'
         LOGI='I'
         LOGA='A'
         LOGW='W'
         BLNK=' '
         ONE='1'
         IMESS='INTEGRATED MTF AMPLITUDE FROM .25 TO .48 = '
         MPHASE='PHASE (RAD) multiplied *'
         MCYCLE='CYCLES PER SAMPLE multiplied *' 
         CALL IFMESSAGE('OTF1 version 05-AUG-1997')

         CALL XVTRANS_SET(TRANBUF,'HALF','REAL',STAT)
         IF (STAT .NE. 1) CALL MABEND('BUFFER SETUP UNSUCCESSFUL')

C**************************
C     OPEN INPUT IF THERE, AND GET PROCESSING AREA BRANCH TO 200 IF INPUT 
C     IS LSF OR DATA OPEN OUTPUT IF NEEDED
C**************************
         IPIC=0
         ISL=0
         ISS=0
         INL=0
         INS=0
         INLINES=0
         INSAMPS=0
         LINLAS=0
         LINO=0
         CALL XVUNIT (IUNI,'INP',1,INSTAT,' ')
         IF (INSTAT .NE. 1) GOTO 200
         CALL XVP('INP',INFILE,ICNT)
         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &            'U_FORMAT','HALF',' ')
         IPIC=1
         CALL XVSIZE(ISL,ISS,INL,INS,INLINES,INSAMPS)
         IF (INL .GT. INLINES)
     &      CALL XVMESSAGE ('INL TOO LARGE!!!!',' ')
         IF (INL .GT. INLINES) INL=INLINES
         IF(INS .GT. INSAMPS)
     &      CALL XVMESSAGE ('INS TOO LARGE!!!!',' ')
         IF(INS .GT. INSAMPS) INS=INSAMPS
         INSM1=INS-1      !THIS IS NEEDED FOR LSF COUNTS
         LINO=ISL
         LINLAS=ISL+INL-1
         IF (LINLAS .GT. INLINES)
     &      CALL MABEND ('TOO MANY LINES!!!!')
         IF (INS-ISS .GT. INSAMPS)
     &      CALL MABEND ('TOO MANY SAMPS!!!!')
         ISSO=ISS
9        CONTINUE
200      CONTINUE               !BRANCH TO HERE FOR NON IMAGE INPUT

C***************************
C     SET DEFAULTS AND INIT BUFFERS
C***************************
         DO 1 I=1,129
            REALV(I)=0.0E0
1           AIMAGV(I)=0.0E0      
         DO 2 I=1,4096
2           IBUF(I)=0
         DO 3 I=1,4488
3           R(I)=0.0E0      
         AMPLSUM=0.0E0
         NORMSUM=0.0E0
         COUNT=0.0
         NPTSIN=0
         KEY=0         !KEY=1=REFLECT DATA/KEY=2=MEAN FILLIN
         KEY4=1        !KEY4=1=NORMALIZE /KEY4=0= DON'T
         ILSF=0
         IDATA=0
         IPRINT=0
         IPLOT=0
         ITST=0
         NUMBER_OF_LINES_WITHOUT_EDGE=0
         CALL XVP('INTERVAL',TIMINT,CNT)
         CALL XVP('NOISE',NOISE,CNT)

C*************************************
C     VICAR PARAM PROCESSOR
C*************************************
         IF (XVPTST('DIAG')) IPRINT=1
         CALL XVP ('PLOT',PLOT_DS,CNT)
         CALL PLOTFN (PLOT_DS)
         IF (CNT .NE. 0) IPLOT=1
         CALL XVPARM ('PZOOM',PZOOM,CNT,IDEF,1)
         IF (IDEF .EQ. 0) THEN
             ZOOMFLAG=1
         ELSE
             ZOOMFLAG=0
         ENDIF
         CALL XVP('LSF',PARMS,CNT)     !INPUTS LSF REAL*4 DATA
         IF (CNT .EQ. 0) GOTO 7
         NPTSIN=CNT
         DO 4 I=3,12
            I1=I
            IF (NPTSIN .LE. 2**I) GOTO 5
4        CONTINUE
5        NTOT=2**I1
         DO 6 I=1,NPTSIN
6           R(I)=PARMS(I)
         IPOWER=I1
         ILSF=1
         LINO=1
7        CALL XVP ('DATA',PARMS,CNT)    !INPUTS RAW REAL*4 DATA
         IF (CNT .EQ. 0) GOTO 107
         NPTSIN=CNT
         DO 104 I=3,12
            I1=I
            IF (NPTSIN .LE. 2**I) GOTO 105
104      CONTINUE
105      NTOT=2**I1
         INS=NPTSIN
         INSM1=INS-1
         IPOWER=I1
         IDATA=1
         LINO=1
         DO 106 I=1,NPTSIN
106         R(I)=PARMS(I)
         LEFTL=0
         IF (R(1) .GT. R(INS)) LEFTL=1  !FIND DIRECTION OF GRADIENT
         IF (LEFTL .EQ. 1) GOTO 500
C****************************
C     DIFFERENTIATE TO GET LSF
C****************************
         IF (IPRINT .EQ. 1)
     &      CALL PRNT(4,1,INSM1,'NUMBER OF ELEMENTS IN LSF= ') 
         DO 501 I=1,INSM1
501         R(I) = R(I+1)-R(I)
         GOTO 503
500      DO 502 I = 1,INSM1
502         R(I) = R(I)-R(I+1)
503      CONTINUE
107      IF (XVPTST('REFLECT')) KEY=1
         IF (XVPTST('MEAN')) KEY=2
         IF (XVPTST('NONORMAL')) KEY4=0
         IF (XVPTST('SINCTST')) ITST=1
         IF (ITST .EQ. 0) GOTO 504        !TEST WITH SINC
         LINO=1
         NPTSIN=60
         INS=NPTSIN
         INSM1=INS-1
         ILSF=1
         NTOT=64
         IPOWER=6
         DO 505 I=1,60
            DENM=(2.0E0*PI*(FLOAT(I)-30.5E0)/5.0E0)
            R(I)=(SIN(DENM)/DENM)**2   !TAB LSF
505      CONTINUE 
504      CONTINUE
C******************************
C     BEGIN DATA ACQUISITION
C******************************
         IF (IPIC .EQ. 1) GOTO 10        !IMAGE INTEGER INPUT
         IF (IDATA .EQ. 1) GOTO 24       !PAR STRING REAL INPUT
         IF (ILSF .EQ. 1) GOTO 24        !PAR STRING LSF INPUT
10       CONTINUE
11       CONTINUE
         CALL XVREAD(IUNI,IBUF,STAT,'LINE',LINO,'SAMP',ISSO,
     &               'NSAMPS',INS,' ')
         IF (IPRINT .EQ. 0) GOTO 99
         CALL PRNT(2,INS,IBUF,'IBUF=. ')
         DO 100 I=1,INS                                
         CALL PRNT(4,1,I,'I=. ')
100      CALL PRNT(2,1,IBUF(I),'IBUF(I)=. ')
99       CONTINUE
C***************************
C     FIND LINE SPREAD FUNCTION
C***************************
         LEFTL=0
         IF (IBUF(1) .GT. IBUF(INS)) LEFTL=1  !FIND DIRECTION OF GRADIENT
         IF (LEFTL .EQ. 1) GOTO 13
C****************************
C     DIFFERENTIATE TO GET LSF
C****************************
         IF (IPRINT .EQ. 1) 
     &      CALL PRNT(4,1,INSM1,'NUMBER OF ELEMENTS IN LSF= ') 
         DO 12 I = 1,INSM1
12       IBUF(I)=IBUF(I+1)-IBUF(I)
         GOTO 15
13       DO 14 I=1,INSM1
14          IBUF(I)=IBUF(I)-IBUF(I+1)
15       CONTINUE
         IF (IPRINT .EQ. 0) GOTO 98
         DO 101 I=1,INSM1                               
            CALL PRNT(4,1,I,'I=. ')                       
101         CALL PRNT(2,1,IBUF(I),'DIFF IBUF(I)=. ')      
98       CONTINUE
         MAX=IBUF(1)
C*************************
C     LOCATE MAX IN LSF
C*************************
         DO 16 I=1,INSM1
            IF (IBUF(I) .LT. MAX) GOTO 16
            MAX=IBUF(I)
            I1=I
16       CONTINUE
         INSM2=INSM1-1
         IF (IPRINT .EQ. 0) GOTO 97
         CALL PRNT (4,1,I1,'INDEX FOR MAX I1=. ')      
         CALL PRNT (4,1,MAX,'MAX =. ')            
         CALL PRNT (4,1,INSM2,'INSM2 IS NUM IN LSF-1=. ')  
97       CONTINUE
         IBAD=0
C************************
C     FIND EDGES OF LSF
C************************
         DO 17 I=I1,INSM2
            IRT=I
            IF (IBUF(I) .LT. 1) IBUF(I)=0   !TRUNCATES NEGATIVES
            IF (IBUF(I) .LT. 1) GOTO 18
            IF ((IBUF(I) .LT. IBUF(I+1)) .AND. 
     &          (IBUF(I) .LT. NOISE)) GOTO 18
            IF (IBUF(I) .LT. IBUF(I+1)) IBAD=IBAD+1   !COUNTS BAD PIXELS
17       CONTINUE
         IF (IBAD .NE. 0)
     &      CALL PRNT(4,1,IBAD,'NUMBER OF BAD PIXELS ON RIGHT= ')
         IF (IBAD .NE. 0)
     &      CALL PRNT(4,1,LINO,'LINE NUMBER=. ')
18       CONTINUE
         IF (IPRINT .EQ. 0) GOTO 96
         CALL XVMESSAGE ('FIND EDGE OF LSF',' ')           
         DO 102 I=1,INSM1                              
            CALL PRNT (4,1,I,'I=. ')                      
102         CALL PRNT (2,1,IBUF(I),'IBUF(I)=. ')          
96       CONTINUE
         DO 19 I=2,I1
            I2=I1-I+2
            ILEFT=I2
            IF (IPRINT .EQ. 1)
     &         CALL PRNT(4,1,I2,'INDEX FOR LEFT EDGE LSF I2=. ')        
            IF (IBUF(I2) .LT. 1) IBUF(I2)=0
            IF (IBUF(I2) .LT. 1) GOTO 20
            IF ((IBUF(I2) .LT. IBUF(I2-1)) .AND.
     &          (IBUF(I2) .LT. NOISE)) GOTO 20
            IF (IBUF(I2) .LT. IBUF(I2-1)) IBAD=IBAD+1   !COUNTS BAD PIXELS
19       CONTINUE
         IF (IBAD .NE. 0)
     &      CALL PRNT(4,1,IBAD,'NUMBER OF BAD PIXELS ON LEFT=. ')
         IF (IBAD .NE. 0)
     &      CALL PRNT(4,1,LINO,'LINE NUMBER=. ')
20       CONTINUE
         IF(IPRINT .EQ. 0) GOTO 95
         DO 103 I=1,INSM1                                
            CALL PRNT(4,1,I,'INDEX FOR LSF EDGE I=. ')
103         CALL PRNT(2,1,IBUF(I),'IBUF(I)=. ')
95       CONTINUE
         IF((I1-ILEFT) .GT. 30) ILEFT=I1-30   !SETS MAXIMUM WIDTH OF LSF
         IF((IRT-I1) .GT. 30) IRT=I1+30       !TO MAX +- 30 ELEMENTS
         I1=0
         IF (IPRINT .EQ. 1)
     &      CALL PRNT(4,1,ILEFT,'ILEFT=. ')          
         IF (IPRINT .EQ. 1)
     &      CALL PRNT(4,1,IRT,'IRT=. ')
         DO 21 I=ILEFT,IRT
            I1=I1+1
            CALL XVTRANS(TRANBUF,IBUF(I),R(I1),1)
            IF (IPRINT .EQ. 1)
     &         CALL PRNT(7,1,R(I1),'LOAD R(I1)=. ')     
21       CONTINUE
         NPTSIN=I1
         IF(IPRINT .EQ. 1)
     &      CALL PRNT(4,1,NPTSIN,'NPTSIN=. ')        
         DO 22 I=3,12
            I1=I
            IF (NPTSIN .LE. 2**I) GOTO 23
22       CONTINUE
23       NTOT=2**I1
         IPOWER=I1
C****************************
C     PROCESS LSF NOW
C****************************
24       CONTINUE
         NTOT=2**IPOWER
         NTOTOL=NTOT
         NZERO=NPTSIN+1
         N1=NTOT/2
         N2=N1+1
         N3=N1+2
         N4=NTOT+2
         IF (TIMINT .LT. 1.0E-20) TIMINT=1.0
         TIME1=TIMINT*NTOT
         IF(NPTSIN .EQ. NTOT) GOTO 31    !NO FILL INS NEEDED
         IF(KEY .EQ. 1) GOTO 26          !FILL IN BY REFLECTING LSF
         IF(KEY .EQ. 2) GOTO 28          !FILL IN BY REPLECATING MEAN LSF
         DO 25 I=NZERO,NTOT             !SUPPLY BLANKS IF NO FILL INS
25          R(I)=0.0E0
         GOTO 31
26       J=0                            !REFLECTION OF LSF
         DO 27 I=NZERO,NTOT
            J=J+2
27          R(I)=R(I-J)
         GOTO 31
28       SUM=0.0E0                      !MEAN LSF
         DO 29 I=1,NPTSIN
29          SUM=SUM+R(I)
         SUM=SUM/NPTSIN
         DO 30 I=NZERO,NTOT
30          R(I)=SUM
31       CONTINUE
         IF (KEY4 .EQ. 0) GOTO 34         !NO NORMALIZE MTF
         SUM=0.0E0
         DO 32 I=1,NTOT
32          SUM=SUM+R(I)
         IF (SUM .EQ. 0) THEN
            CALL XVMESSAGE ('No Edge Found',' ')
            CALL PRNT(4,1,LINO,'LINE  NUMBER=. ')
            NUMBER_OF_LINES_WITHOUT_EDGE =
     &                            NUMBER_OF_LINES_WITHOUT_EDGE+1
            GOTO 481
         ENDIF
         IF (IPRINT .EQ. 1) THEN
            CALL XVMESSAGE ('NORMALIZING LSF',' ')
            CALL PRNT(7,1,SUM,'SUM OF LSF=. ')
         ENDIF
         DO 33 I=1,NTOT                 !DIVIDE BY SUM OF LSF
33          R(I)=R(I)/SUM                  !TO NORMALIZE MTF
         SUM=1.0E0
34       CONTINUE
C****************************
C     COMPUTE FIRST MOMENT FOR WEIGHT CENTER OF LSF BY EQU (2)
C****************************
         SUM=0.0E0
         SUM1=0.0E0 
         DO 35 I=1,NTOT         
            SUM=SUM+R(I)
            CEN=FLOAT(I-1)*R(I)
35          SUM1=SUM1+CEN                  
         IF (SUM .EQ. 0) THEN
            CALL XVMESSAGE ('No Edge Found',' ')
            CALL PRNT (4,1,LINO,'LINE  NUMBER=. ')
            GOTO 481
         ENDIF
         CENTR=SUM1/SUM 
         IF (IPRINT .EQ. 1) THEN
            CALL PRNT(7,1,SUM1,'SUM1=. ')
            CALL PRNT(7,1,SUM,'SUM=. ')
            CALL PRNT(7,1,CENTR,'CENTR=. ')      
         ENDIF
C****************************
C     SAMPLING THEOREM TO PRODUCE 256 WIDE LSF FOR FFTT
C     LSF AFTER RESAMPLING IS SPLIT WITH MAXIMUM AT R(1),
C     MINIMUM=0 AT R(128) AND MAXIMUM AT R(256).
C****************************
         IF (IPRINT .EQ. 1)
     &      CALL PRNT(7,NTOT,R,'AT SAMPLING THEORM R(I)=. ')
         NT2=NTOT/2
         I=CENTR+1.0E-5                 !WANT FRACTIONAL PART OF CENTR
         CEN=128.0E0+NT2+CENTR-I        !WANT FRACTIONAL PART OF CENTR
         IF (IPRINT .EQ. 1)
     &      CALL PRNT(7,1,CEN,'FRACTIONAL OFFSET CEN=. ') 
         SUM1=0.0E0
         DO 39 I=1,256+NTOT              !REDEFINE PER NATHAN
            VAL=(CEN-I)*PI                  !"EQU 3" 
            IF (ABS(CEN-I) .LT. 1.0E-10) GOTO 38   
            WTS(I)=SIN(VAL)/VAL             !"EQUATION 3"
            GOTO 39
38          WTS(I)=1.0E0
39          SUM1=SUM1+WTS(I)                !SUM OF WEIGHTS
         IF (IPRINT .EQ. 1) THEN
            CALL PRNT(7,1,SUM1,'SUM1 OF WTS=. ')   
            CALL PRNT(7,256+NTOT,WTS,'WTS(I)=. ')  
         ENDIF
         J=256+1                         !REDEFINE PER NATHAN
         DO 42 I=1,J                     !DOES THE RESAMPLING
            SUM1=0.0E0
            DO 41 K=1,NTOT
41             SUM1=SUM1+WTS(I+K-1)*R(K)       !"INTX" EQU 3.
            F(I)=SUM1
42       CONTINUE
         IF (IPRINT .EQ. 1) CALL PRNT(7,256,F,'INT F(I)=. ')  
         I=1+(CENTR+1.0E-5)              !FRACTIONAL PART OF CENTR
         MIDPT=128+NT2-I+1     
         I=0
         IF (IPRINT .EQ. 1) CALL PRNT(4,1,MIDPT,'MIDPT=. ')   
         DO 43 K=MIDPT,256               !CENTERS LSF, SPLITS
            I=I+1                           !AND FOLDS WITH MAX AT END
43          R(I)=F(K)                       !POINTS
         IF (IPRINT .EQ. 1) CALL PRNT(7,256,R,'R(I)=. ') 
         I=257
         LLT=MIDPT-1
         IF (IPRINT .EQ. 1) CALL PRNT(4,1,LLT,'LLT=. ')    
         DO 44 K=1,LLT                   !LEFT JUSTIFIES LSF
            J=LLT-K+1
            I=I-1
44          R(I)=F(J)
         NTOT=256
         IPOWER=8
         IF (IPRINT .EQ. 1) THEN
            CALL XVMESSAGE ('RESAMPLED DATA FOR TRANSFORM',' ')
            CALL PRNT(7,256,R,'R=. ')
         ENDIF
         NTOTOL=NTOT
         N1=NTOT/2
         N2=N1+1
         N3=N1+2
         N4=NTOT+2
         TIME1=TIMINT*NTOT    !INITIAL PHASE SHIFT 

C********************
C     DIRECT FORWARD FFT 
C********************
         DO 46 I=1,NTOT
46          A(I)=CMPLX(R(I),0.0E0)
         CALL FFTT(IPOWER,-1,A)       !REALLY ONLY THREE ARGUMENTS!!!!

C*********************
C     PROCESS FFTT OUTPUT FOR EACH LINE NORMALIZE ONLY ONCE AFTER F.T. 
C     TO AVOID ROUND OFF AND TRUNC. CONSULTATION WITH R. NATHAN
C     REMOVE FFTT FACTOR OF "2" AND NORMALIZE FT COMPONENTS BY "/REAL(A(1))"
C     ASSUMES AIMAG(A(1))=0.0 ALWAYS!!
C*********************
         FTNORM=REAL(A(1))
         DO 48 I=1,N2                          !SUM UP FT ELEMENTS
            REALV(I)=REALV(I)+(REAL(A(I))/FTNORM) !FOR EACH LINE
            AIMAGV(I)=AIMAGV(I)+(AIMAG(A(I))/FTNORM)
48       CONTINUE
481      LINO=LINO+1
         IF ((ILSF .EQ. 1) .OR. (IDATA .EQ. 1)) GOTO 49
         IF (LINO .LE. LINLAS) GOTO 11     !GO READ ANOTHER LINE
49       CONTINUE
         IF (ISL .NE. 1) THEN
            LINES_PRO = LINLAS-ISL+1-NUMBER_OF_LINES_WITHOUT_EDGE
            RINL=FLOAT(LINES_PRO)      !NUMBER OF LINES PROCESSED
            IF (ITST .EQ. 0)
     &         CALL PRNT
     &              (4,1,LINES_PRO,'NUMBER OF LINES PROCESSED=. ')
            ELSE
               LINES_PRO = LINO-1-NUMBER_OF_LINES_WITHOUT_EDGE 
               RINL=FLOAT(LINES_PRO)    !NUMBER OF LINES PROCESSED
               CALL PRNT 
     &              (4,1,LINES_PRO,'NUMBER OF LINES PROCESSED=. ')
            ENDIF

C     PRINT OUT TRANSFORM VALUES
C***************************
C-----IF OUTPUT ASCII FILE REQUIRED?
	    CALL XVPARM('TABLE',TBL,ICNT,IDEF,1)
	    IF(IDEF .EQ. 0) THEN
	       ITBL = 1
 	       TAB=CHAR(9)
	       OPEN(11,FILE=TBL,STATUS='UNKNOWN',IOSTAT=JST,ERR=999)
               IF (XVPTST('COLHDR')) THEN
                  IF (XVPTST('PHASE')) 
     &               WRITE(11,904)
     &                  ' FREQUENCY',TAB,'AMPLITUDE',TAB,'PHASE'
                  IF (XVPTST('NOPHASE')) 
     &               WRITE(11,905) ' FREQUENCY',TAB,'AMPLITUDE'
               ENDIF
	    ENDIF

904	    FORMAT(1X,A10,A1,A9,A1,A5)
905	    FORMAT(1X,A10,A1,A9)

C-------IS FULL PRINTOUT REQUIRED?
	    NOPRT=0
	    IF (XVPTST('NOPRINT')) NOPRT=1
	    IF (NOPRT .EQ. 0) THEN
            ZPBUF=' '
            CALL XVMESSAGE(' ',' ')
            ZPBUF(1:51)=
     &         '   CYCLES/     FREQUENCY    REAL(FT)     IMAG.(FT)'
            ZPBUF(55:102)=
     &         'INTENSITY    AMPLITUDE     PHASE     WAVE LENGTH'
            CALL XVMESSAGE(ZPBUF,' ')
            ZPBUF=' '
            ZPBUF(4:9)='SAMPLE'
            ZPBUF(81:87)='RADIANS'
            ZPBUF(95:99)='SHIFT'
            CALL XVMESSAGE(ZPBUF,' ')
            ZPBUF=' '
	 ENDIF

         K=1

         DO 54 I=1,N2                   !LOOP TO PRINTOUT OTF VS FREQUENCY
            C1=REALV(I)/RINL               !AVERAGE THE SUMED F.T. COMPONENTS
            C2=AIMAGV(I)/RINL
            IF (ABS(C1) .LT. 1.0E-8) C1=0.0E0 !SUPRESS ERROR VALUES
            IF (ABS(C2) .LT. 1.0E-8) C2=0.0E0
            F2=(I-1)/TIME1
            AMPL2=C1*C1+C2*C2
            AMPL=SQRT(AMPL2)

            AMPL9(I)=AMPL             !LOAD FOR PLOTTING
            AMPL10(I)=AMPL

C            AMPL9(I)=-AMPL             !LOAD FOR PLOTTING
C            AMPL10(I)=-AMPL
            IF (AMPL .LT. 0.0005E0) GOTO 60 !SUPRESS PHASE ERRORS

C<<<<<< **** NOTE TO DEVELOPER ****
C       The following change was made as a work-around for the subroutine
C       FFTT.  The problem with FFTT is its inconsistency between platforms on 
C       calculation of small floating values for both real and imaginary parts 
C       of the complex number.  The problem may be caused by hardware round-off.
C       In the case of this program, it affected the imaginary part of the 
C       complex number, which has direct impact on the calculation of PHASE. 
C       REF:  FR90520
C       -- Thomas Huang (txh)
C>>>>>> ***************************

            IF (ABS(C2) .LT. (1E-3 * ABS(C1))) THEN
               IF (C1 .GT. 0.0) THEN
                  PHASE = 0.0
               ELSE
                  PHASE = PI
               ENDIF
            ELSE
               PHASE=ATAN2(C2,C1)
            ENDIF

C<<<<<<< **** ORIGINAL CODE ****
C           PHASE=ATAN2(C2,C1)
C>>>>>>> ***********************

            GOTO 61
60          PHASE=0.0E0
61          CONTINUE      
            F3=(I-1)/FLOAT(NTOTOL)         !DEFINES FREQUENCY

C            CPLOT(I)=F3                   !LOAD FOR PLOTTING

C<<<<<< Changed to use the INTERVAL input value.  TXH

            CPLOT(I)=F3*TIMINT         !LOAD FOR PLOTTING

            IF (ZOOMFLAG .EQ. 1) THEN

               PPLOT(I)=PHASE*PZOOM     !LOAD FOR PLOTTING
C               PPLOT(I)=-PHASE*PZOOM     !LOAD FOR PLOTTING

            ELSE

               PPLOT(I)=PHASE
C               PPLOT(I)=-PHASE

            ENDIF
            IF((F3 .LT. 0.2500) .OR.
     &         (F3 .GT. 0.484)) GOTO 51
            AMPLSUM=AMPLSUM+AMPL           !INTEGERATES AMPL
51          CONTINUE
            IF(ABS(F2) .LT. 1.0E-20) GOTO 52
            WSFT=PHASE/(2.0E0*PI*F2)
            GOTO 53
52          WSFT=0.0E0
53          CONTINUE
            F29(I)=F2
            WSFT9(I)=WSFT
	    IF (NOPRT .EQ. 0) THEN
               WRITE (ZPBUF(1:10),'(F10.5)') F3
               WRITE (ZPBUF(14:23),'(F10.5)') F2
               WRITE (ZPBUF(27:36),'(F10.5)') C1
               WRITE (ZPBUF(40:49),'(F10.5)') C2
               WRITE (ZPBUF(53:62),'(F10.5)') AMPL2
               WRITE (ZPBUF(66:75),'(F10.5)') AMPL
               WRITE (ZPBUF(79:88),'(F10.5)') PHASE
               WRITE (ZPBUF(92:101),'(F10.5)') WSFT
               CALL XVMESSAGE(ZPBUF,' ')
	    ENDIF

	    IF ((ITBL .EQ. 1) .AND. XVPTST('PHASE')) 
     &         WRITE(11,903) F3,TAB,AMPL,TAB,PHASE
	    IF (ITBL .EQ. 1 .AND. XVPTST('NOPHASE')) 
     &         WRITE(11,902) F3,TAB,AMPL

            OBUF(K)=F3
            OBUF(K+1)=C1
            OBUF(K+2)=C2
            K=K+3
54       CONTINUE

903	 FORMAT(1x,F10.5,A1,F10.5,A1,F10.5)
902	 FORMAT(1x,F10.5,A1,F10.5)
	 IF (ITBL .EQ. 1) CLOSE(11)

         CALL PRNT(4,1,K,'NUMBER OUTPUT = . ')

	 CALL XVP('OUT',OUTFILE,OSTAT)
         IF (OSTAT .EQ. 1) THEN
            CALL XVUNIT(OUNI,'OUT',1,OSTAT,' ')
            CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT',
     &                  'SA','U_FORMAT','REAL','O_FORMAT',
     &                  'REAL','U_NL',1,'U_NS',372,'OP',
     &                  'WRITE',' ')
            CALL XVWRIT(OUNI,OBUF,STAT,'NSAMPS',K,' ')
            CALL XVCLOSE(OUNI,STAT,' ')
         ENDIF

         WRITE (IMESS(48:56),'(F9.5)') AMPLSUM
         CALL XVMESSAGE(IMESS,' ')

         IF(IPLOT .EQ. 1) GOTO 57
C************************
C     LINE PRINTER PLOTS
C************************
         ZPBUF=' '
         ZPBUF(2:10)='FREQUENCY'
         ZPBUF(20:21)='-1'
         ZPBUF(70:70)='0'
         ZPBUF(119:120)='+1'
         CALL XVMESSAGE(ZPBUF,' ')
         CALL XVMESSAGE(LO,' ')
         DO 56 I=1,N2                     
            DO 55 J=1,101
55             LOG(J:J)=BLNK
            LOG(51:51)=ONE
            INLO=(REALV(I)+1.)*51.
            IF (INLO .LT. 0) INLO=0
            IF (INLO .GT. 101) INLO=101
            LOG(INLO:INLO)=LOGR
            INLO=(AIMAGV(I)+1.)*51.
            IF (INLO .LT. 0) INLO=0
            IF (INLO .GT. 101) INLO=101
            LOG(INLO:INLO)=LOGI
            INLO=(-AMPL10(I)+1.)*51.
            IF(INLO .LT. 0) INLO=0
            IF(INLO .GT. 101) INLO=101
            LOG(INLO:INLO)=LOGA
            INLO=(WSFT9(I)+1.)*51.
            IF (INLO .LT. 0) INLO=0
            IF (INLO .GT. 101) INLO=101
            LOG (INLO:INLO)=LOGW
            WRITE (ZPBUF(1:10),'(F10.5)') F29(I)
            ZPBUF(20:120)=LOG(1:101)
            ZPBUF(10:18)=BL
56          CALL XVMESSAGE(ZPBUF,' ')
57       CONTINUE

         IF (IPLOT .EQ. 0) GOTO 995

C*******************
C     PRINTRONIX PLOTS 
C*******************
         CALL XRTBEGIN(STATUS)
         IF (STATUS .NE. 1) CALL MABEND('Unable to OPEN plotter')
         CALL DISPLAYAXES(1,1,1)   ! x,y1 and y2 axes displayed 1=yes 0=no
         CALL SETACTIVESET(0)      ! 0= no triangles (default), 1= triangles
         X=0.22                         !PRINT HEADER ON PLOT
         Y=3.5

C-------PRINT HEADER ON PLOT
C-------PRINT INPUT FILENAME AND PLOTNAME ON PLOT
C-------GET AND PLOT ALL LABELS
         HEADR(1)='OTF1 MODULATION TRANSFER FUNCTION'
         IF (INSTAT .EQ. 1) THEN
            HEADR(2)='INPUT = '//INFILE
            I=3
         ELSE
            I=2
         END IF
         HEADR(I)='PLOT  = '//PLOT_DS
         I=I+1

C-------PRINT INPUT FILENAME ON PLOT
         IF (INSTAT .EQ. 1) THEN
            CALL OTFLABS(IUNI,HEADR,I,*994)
            I=I+1
         ENDIF
         HEADR(I)(1:80)=IMESS
         CALL HEADER (HEADR,I,0) ! 0=left justify, 1=center justify, 2=right

         WRITE (MCYCLE(31:37), '(F7.2)') TIMINT 
         IF (ZOOMFLAG .EQ. 0) THEN
            CALL AXESTITLES(MCYCLE,'AMPLITUDE',90,
     &                   'PHASE (PI=1)',90)
         ELSE
            WRITE (MPHASE(25:31),'(F7.2)') PZOOM
            CALL AXESTITLES
     &           (MCYCLE,'AMPLITUDE',90,MPHASE,90)
         ENDIF
 
         AMPL9(130)=0.00000
         AMPL9(131)=1.00000
         CPLOT(130)=0.00000
         CPLOT(131)=1.00000
         PPLOT(130)=0.00000
         PPLOT(131)=1.00000
         CALL LINE(CPLOT,AMPL9,N2,1,0,0)

C     plot the "PHASE" curve
         do ii=1,n2  ! scale phase to lie between -1 and 1 .
            pplot(ii)=pplot(ii)/3.14159
         enddo
         CALL LINE(CPLOT,PPLOT,N2,1,0,0)
         CALL PLOT(0,0,999)

995      IF (INSTAT .EQ. 1) CALL XVCLOSE(IUNI,STAT,' ')
         RETURN
999      CALL XVMESSAGE ('ERROR OPENNING TABLE FILE',' ')
         CALL PRNT(4,1,J,'IOSTAT=.')
994      CALL ABEND
         RETURN
      END


      SUBROUTINE OTFLABS(INU,HEADR,ICNT,*)
         IMPLICIT INTEGER*4 (A-Z)
         INTEGER*4 LV2(60),LPL(60),ICNT
         CHARACTER*72 V1(60),V2(60),PL(60),HT(60)
         CHARACTER*80 HEADR(70)
 
         CALL LABPROC(INU,V1,NV1,V2,LV2,NV2,PL,LPL,NPP,HT,NH)
 
         IF (NV1 .GT. 0) THEN
            DO I=1,NV1
               HEADR(ICNT)=V1(I)
               ICNT=ICNT+1
            ENDDO
         ELSE IF (NPP .GT. 0) THEN
            DO I=1,NPP
               HEADR(ICNT)=PL(I)
               ICNT=ICNT+1
            ENDDO
         ELSE IF (NV2 .GT. 0) THEN
            DO I=1,NV2
               HEADR(ICNT)=V2(I)
               ICNT=ICNT+1
            ENDDO
         ELSE
            CALL XVMESSAGE('LABEL ERROR',' ')
            RETURN 1
         ENDIF

         IF (NH .GT. 0) THEN
            DO I=1,NH
               HEADR(ICNT)=HT(I)
               ICNT=ICNT+1
            ENDDO
         ENDIF
 
         RETURN
      END


      SUBROUTINE KVPAIR(IU,PAIR,LNG,NUM)
C-------EXTRACTS KEYWORD=VALUE STRINGS FROM FIRST PROPERTY LABEL
         IMPLICIT INTEGER*4 (A-Z)
         INTEGER*4 LNG(60)
         character*72 pair(60)
         character*2048 buf

         max=2048
         call xlgetlabel(iu,buf,max,ist)
         i=0
         b = index(buf(1:),'PROPERTY')
         IF (B .EQ. 0) GOTO 100

50       L=INDEX(BUF(B+1:),' ')
         L=B+L
         Q=INDEX(BUF(B+1:),'''')
         Q=B+Q
         IF (Q .LT. L) THEN		!ARE THERE QUOTES IN THE VALUE?
            Q2=INDEX(BUF(Q+1:),'''')
            Q2=Q+Q2
            L=INDEX(BUF(Q2+1:),' ')
            L=Q2+L
         ENDIF
         E=L-1
C-------STOP WHEN REACH A 'TASK' OR NEXT PROPERTY LABEL
         IF (BUF(B:B+3) .EQ. 'TASK') GOTO 100
         IF ((I .GT. 1) .AND. 
     &       (BUF(B:B+3) .EQ. 'PROP')) GOTO 100
         I=I+1
         KK=E-B+1
         LNG(I) = KK			!STORE LENGTH
         PAIR(I) = BUF(B:E)        !STORE STRING
         IF (E .EQ. MAX) GOTO 100        !END OF LABEL?
         DO J=E+1,MAX			!LOOK FOR NEXT STRING
            B=J
            IF(BUF(J:J) .NE. ' ') GOTO 50
         END DO
100      NUM=I
         RETURN
      END
c ************************************************************


      subroutine V2PAIR(IU,PAIR,LNG,NUM)
C-------RETURN THE KEYWORD=VALUE PAIRS FROM THE FIRST V2 TASK
         IMPLICIT INTEGER*4 (A-Z)
         INTEGER*4 LNG(60)
         character*72 pair(60)
         character*2048 buf

         max=2048
         call xlgetlabel(iu,buf,max,ist)
         i=0
         IF (index(buf(1:),'PROPERTY') .GT. 0) GOTO 100
         B = index(buf(1:),'TASK')
         IF (B .EQ. 0) goto 100

50       L=INDEX(BUF(B+1:),' ')
         L=B+L
         Q=INDEX(BUF(B+1:),'''')
         Q=B+Q
         IF (Q .LT. L) THEN
            Q2=INDEX(BUF(Q+1:),'''')
            Q2=Q+Q2
            L=INDEX(BUF(Q2+1:),' ')
            L=Q2+L
         ENDIF
         E=L-1
C-------STOP WHEN ENCOUNTER THE SECOND TASK
         IF ((I .GT. 1) .AND. 
     &       (BUF(B:B+3).EQ. 'TASK')) GOTO 100
         I=I+1
         KK=E-B+1
         LNG(I) = KK			!STORE LENGTH
         PAIR(I) = BUF(B:E)	!STORE STRING
         IF (E .EQ. MAX) GOTO 100	!END OF LABEL?
         DO J=E+1,MAX			!LOOK FOR NEXT STRING
            B=J
            IF(BUF(J:J) .NE. ' ') GOTO 50
         END DO
100      NUM=I
         RETURN
      END


      SUBROUTINE LABPROC(IUNI,LABV1,NV1,VPAIR,LV2,NV2,PPAIR,LPL,
     &                   NPP,HSTRY,NH)
         IMPLICIT INTEGER(A-Z)
         INTEGER INSTANCES(20),LV2(60),LPL(60)
         CHARACTER*8 TASKS(20)
         CHARACTER*12 UNAME
         CHARACTER*28 TIME
         CHARACTER*72 HBUF,BL
         CHARACTER*72 LABV1(60),HSTRY(60),VPAIR(60),PPAIR(60)
         CHARACTER*4320 LABS

c-------get all vicar1 labels
         NV1=40
         CALL VIC1LAB(IUNI,STAT,NV1,LABS,0)
         DO J=1,NV1
            LABV1(J)=LABS(72*(J-1)+1:72*(J-1)+72)
         END DO

c-------get keyword=value pairs from vicar property labels
         NPP=0
         IF (NV1 .EQ. 0) call kvpair(IUNI,ppair,LPL,npp)

c-------get keyword=value pairs from first vicar2 task
         NV2=0
         IF ((NV1 .EQ. 0) .AND. (NPP .EQ. 0)) 
     &      call v2pair(IUNI,vpair,LV2,nv2)

c-------get user and date for each vicar2 history task
         BL = '----TASK:-------------USER:------------------------'

         nh=20                             !EXTRACT VIC*2 LAB
         CALL XLHINFO(IUNI,TASKS,INSTANCES,NH,STAT,' ')

         DO 801 J=1,NH
            CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')
            CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')

            HBUF(1:72) = BL(1:72)
            HBUF(10:22) = TASKS(J)
            HBUF(28:39) = UNAME
            HBUF(40:68) = TIME
801         HSTRY(J) = HBUF

         RETURN
      END

