      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     PROGRAM ustats
C     19 OCT 79   ...REA...    INITIAL RELEASE
C     29 AUG 85   ...JHR...    CONVERT TO VICAR2
C      5 SEP 94   ...CRS (CRI) REVISE FOR PORTING
C     10 JUL 95   ...VRU (CRI) CHANGED FIRST OUTPUT FILE FORMAT TO ISTATFILE
C     15 APR 98   ...RRP (AR-9900) UPDATED USTATS.PDF TO RESTRICT CERTAIN
C                        PARAMETERS TO BE LESS THEN OR EQUAL TO ZERO.
      REAL        RPARM(200)
      INTEGER     IPARM(200),SL,SS,IEXCL(11),STAT,IUNIT(24),OUNIT
      LOGICAL*1   QPRT,QN,QALL,QCHK,QMSS,QBAND(24)
      LOGICAL*1   XVPTST
      CHARACTER*8 FORMAT
      CHARACTER*80 CHARINP(10)

      COMMON /C1/ INCL,INCS,NCLASS,BOUND,PERC,NCLUS,NCHAN,NEXCL,IEXCL,
     &            IUNIT,SL,SS,NLO,NSO,NSI,NSCHAN,MSSCHN,
     &            QPRT,QN,QALL,QCHK,QMSS,QBAND
      EXTERNAL    CLUSTR

      DATA QBAND/24*.TRUE./

C        SET DEFAULTS AND INITIALIZE
      OUNIT=0
      IEXCL(1) = 9999
      NEXCL = 1
      INCL = 20
      INCS = 20
      QPRT = .TRUE.
      NCLASS = 10
      BOUND = 5.0
      PERC = 0.0
      QMSS = .FALSE.
      NCLUS = 500
      QN = .FALSE.
      QALL = .FALSE.
      QCHK = .FALSE.
C
      CALL IFMESSAGE('ustats version 15-APR-98')
C        DETERMINE NUMBER OF INPUTS
      CALL XVP('INP',CHARINP,NI)
      NCHAN = NI
      IF(NI.EQ.1) QMSS = .TRUE.

C          OPEN INPUT DATA SETS
      DO I=1,NI
         CALL XVUNIT(IUNIT(I),'INP',I,STAT,' ')
         CALL XVOPEN(IUNIT(I),STAT,'U_FORMAT','HALF',' ')
      END DO

C        GET DATA FORMAT AND CHECK
      DO I=1,NI
         CALL XVGET(IUNIT(I),STAT,'FORMAT',FORMAT,' ')
         IF(FORMAT.NE.'BYTE') THEN
            CALL XVMESSAGE('ustats ACCEPTS BYTE DATA ONLY',' ')
            CALL ABEND
         END IF
      END DO

C        GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
      IF(SL+NLO-1 .GT. NLI) THEN
        CALL XVMESSAGE(
     &             'NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE',' ')
        CALL ABEND
      END IF
      IF(SS+NSO-1 .GT. NSI) THEN
         CALL XVMESSAGE(
     &       'NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE',' ')
         CALL ABEND
      END IF

C           *** PROCESS PARAMETERS ***

C        'INC'
      CALL XVPARM('INC',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.NE.0) THEN
         INCL=IPARM(1)
         INCS=IPARM(1)
      END IF
C        'LINC'
      CALL XVPARM('LINC',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.NE.0) INCL=IPARM(1)
C        'SINC'
      CALL XVPARM('SINC',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.NE.0) INCS=IPARM(1)
C        'NOPRINT'
      IF ( XVPTST('NOPRINT') )  QPRT=.FALSE.
C        'CLAS'
      CALL XVPARM('CLASSES',IPARM,ICCLAS,IDEF,1)
      IF(ICCLAS.NE.0) NCLASS=IPARM(1)
C        'INITIAL'
      CALL XVPARM('INITIAL',RPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.NE.0) BOUND = RPARM(1)
C        'PERCENT'
      CALL XVPARM('PERCENT',RPARM,ICPERC,IDEF,1)
      IF(ICPERC.NE.0) PERC=RPARM(1)
C        'MSS'
      CALL XVPARM('MSS',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.NE.0) NCHAN=IPARM(1)
      MSSCHN = NCHAN
C        'USE'
      CALL XVPARM('USE',IPARM,ICOUNT,IDEF,24)
      IF(ICOUNT.NE.0) THEN
         NCHAN = ICOUNT
         DO I=1,NCHAN
            QBAND(IPARM(I)) = .FALSE.
         END DO
         DO I=1,MSSCHN
            QBAND(I) = .NOT.QBAND(I)
         END DO
      END IF
C        'EXCLUDE'
      CALL XVPARM('EXCLUDE',IPARM,ICOUNT,IDEF,24)
      IF(ICOUNT.NE.0) THEN
         NEXCL = ICOUNT
         DO I=1,NEXCL
            IEXCL(I+1) = IPARM(I)
         END DO
         NEXCL = NEXCL+1
      END IF
C        'CLUSTERS'
      CALL XVPARM('CLUSTERS',IPARM,ICOUNT,IDEF,1)
      IF(ICOUNT.NE.0) NCLUS=IPARM(1)
C        'NONN'
      IF(XVPTST('NONN')) QN=.TRUE.
C        'ALL'
      IF(XVPTST('ALL')) QALL=.TRUE.
C        'NOTIFY'
      IF(XVPTST('NOTIFY')) QCHK=.TRUE.

      IF(QMSS.AND.NSO.EQ.NSI) NSO=NSO/MSSCHN
      NSCHAN = NSI/MSSCHN
      IF(ICPERC.NE.0.AND.ICCLAS.EQ.0) NCLASS=NCLUS
      IF(QALL) NCLASS=NCLUS

C        DETERMINE BUFFER SIZES
      I = 2*NSO*MSSCHN+4
      IF(QMSS) I=2*NSCHAN*MSSCHN
      J = 4*NCLUS*NCHAN+4
      K = 2*J+4
      L = 4*NCLUS+4
C      M = 12+4*NCHAN+2*NCHAN*(NCHAN+1)
      IF(M.LT.132) M=132

C        MAIN PROCESSING
      CALL STACKA(8,CLUSTR,4,I,J,K,L,*900)

C        CLOSE INPUT DATA SETS
      DO I=1,NI
         CALL XVCLOSE(IUNIT(I),STAT,' ')
      END DO

      RETURN

900   CALL XVMESSAGE(
     &           'INSUFFICIENT CORE FOR STACKA OPERATION',' ')
      CALL ABEND
      END

      SUBROUTINE CLUSTR(IN,IX,AVG,JX,VAR,KX,PTS,LX)


      REAL*4        xx(1000), zz(1000)
      REAL*8        VAR(NCLUS,NCHAN)
      REAL*4        AVG(NCLUS,NCHAN),PTS(NCLUS)
      INTEGER*4     IEXCL(11),SL,SS,STAT,IUNIT(24),OUNIT
      INTEGER*2     IN(NSO,NCHAN)
      LOGICAL*1     QPRT,QN,QALL,QCHK,QMSS,QBAND(24)
      CHARACTER*10  IFMT,IFMT1,FXFMT,FXFMTS,FXFMTM
      CHARACTER*1400 MOUT
      CHARACTER*8   XOUT

      COMMON /C1/ INCL,INCS,NCLASS,BOUND,PERC,NCLUS,NCHAN,NEXCL,IEXCL,
     &            IUNIT,SL,SS,NLO,NSO,NSI,NSCHAN,MSSCHN,
     &            QPRT,QN,QALL,QCHK,QMSS,QBAND

      OUNIT=0
      IFMT='         '
      FXFMT='         '
      I = 4*NCLUS
      IF(I.GT.LX) THEN
         CALL XVMESSAGE('INSUFFICIENT CORE FOR STACKA BUFFERS',' ')
         CALL ABEND
      END IF
      I = NCLUS*NCHAN
      CALL ZIA(AVG,I)
      CALL ZIA(VAR,2*I)
      CALL ZIA(PTS,NCLUS)
      NUM = 0
      MCLUS = 1
      BOUND = BOUND*BOUND
      ICHK = INCL+NLO/10
      MOUT(1:32) = ' ustats IS     PERCENT COMPLETE'
C
C***SAMPLE THE IMAGE TO FORM CLUSTERS
C
      DO 900 I=1,NLO,INCL
C
C        IF NECESSARY,  NOTIFY THE OPERATOR THAT JOB IS N% COMPLETE
      IF(.NOT.QCHK) GO TO 100
      IF(I.LT.ICHK) GO TO 100
      J = (100*(I-INCL))/NLO
      WRITE(MOUT(12:14), '(I3)') J
      CALL XVMESSAGE(MOUT(1:32),' ')
      ICHK = ICHK+NLO/10

100   CALL GETLIN(IUNIT,I,IN,SL,SS,NSCHAN,NCHAN,MSSCHN,NSO,QMSS,QBAND)

      DO 700 J=1,NSO,INCS
      CALL EXCHK(IN,J,IEXCL,NEXCL,NCHAN,NSO,*700)

      IF(NUM.EQ.0) CALL ADD(IN,J,1,NCHAN,NCLUS,NSO,NUM,VAR,AVG,PTS,*300)
C
C     FIND NEAREST CLUSTER
C
      MCLUS = NUM+1
      BDIST = BOUND
      DO 200 K=1,NUM
         X = DIST(IN,J,AVG,K,NCHAN,NCLUS,NSO)
         IF(X.GT.BDIST) GO TO 200
         MCLUS = K
         BDIST = X
  200 CONTINUE
      IF(MCLUS.LE.NCLUS) GO TO 250
      MOUT(1:32) = '    SAMPLING INCOMPLETE AT LINE'
      WRITE(MOUT(32:36),'(I5)') I
      CALL XVMESSAGE(MOUT(2:36),' ')
      GO TO 1000
  250 CONTINUE
      CALL ADD(IN,J,MCLUS,NCHAN,NCLUS,NSO,NUM,VAR,AVG,PTS,*300)
  300 CONTINUE
      IF(QN) GO TO 700
C
C     CHECK PIXEL(S) TO THE LEFT
C
      L = J
  400 CONTINUE
      L = L-1
      IF(L.LE.0) GO TO 500
      CALL EXCHK(IN,L,IEXCL,NEXCL,NCHAN,NSO,*500)
      X = DIST(IN,L,AVG,MCLUS,NCHAN,NCLUS,NSO)
      IF(X.GT.BOUND) GO TO 500
      CALL ADD(IN,L,MCLUS,NCHAN,NCLUS,NSO,NUM,VAR,AVG,PTS,*400)
  500 CONTINUE
C
C     CHECK PIXEL(S) TO THE RIGHT
C
      L = J
  600 CONTINUE
      L = L+1
      IF(L.GT.NSO) GO TO 700
      CALL EXCHK(IN,L,IEXCL,NEXCL,NCHAN,NSO,*700)
      X = DIST(IN,L,AVG,MCLUS,NCHAN,NCLUS,NSO)
      IF(X.GT.BOUND) GO TO 700
      CALL ADD(IN,L,MCLUS,NCHAN,NCLUS,NSO,NUM,VAR,AVG,PTS,*600)
  700 CONTINUE
  900 CONTINUE
 1000 CONTINUE
      CALL XVMESSAGE(' ',' ')
      MOUT(1:22) = '       CLUSTERS FORMED'
      WRITE(MOUT(1:6),'(I6)') NUM
      CALL XVMESSAGE(MOUT(2:22),' ')
C
C***ELIMINATE ONE-PIXEL CLUSTERS
C
      I=1
      PIX = 0.0
      DO 1200 J=1,NUM
      PIX = PIX+PTS(J)
      IF(PTS(J).EQ.1.0.AND..NOT.QALL) GO TO 1200
      PTS(I) = PTS(J)
      DO K=1,NCHAN
         AVG(I,K) = AVG(J,K)
         X = (VAR(J,K)-PTS(J)*AVG(J,K)*AVG(J,K))/(PTS(J)-0.999999)
         VAR(I,K) = SQRT(X)
      END DO
      I = I+1
 1200 CONTINUE
      IF(QALL) CALL SORT(PTS,AVG,VAR,NUM,NCLUS,NCHAN)
      IF(QALL) GO TO 2500
      IF(I.EQ.1) THEN
         CALL XVMESSAGE(
     &      ' ***NO CLUSTER CONTAINS MORE THAN ONE PIXEL***',' ') 
         CALL XVMESSAGE(' ***PROGRAM TERMINATED***',' ')
         CALL ABEND
      END IF
      NUM = I-1
      MOUT= '       CLUSTERS AFTER REMOVING ONE-PIXEL CLUSTERS' 
      WRITE(MOUT(1:6),'(I6)') NUM
      CALL XVMESSAGE(MOUT(2:50),' ')
      IF(NUM.EQ.1) GO TO 2500
C
C***COMBINE CLUSTERS THAT OVERLAP BY ONE STANDARD DEVIATION
C
      CALL SORT(PTS,AVG,VAR,NUM,NCLUS,NCHAN)
      LAST = NUM-1
      NUM2 = NUM
      DO 1800 I=1,LAST
      IF(PTS(I).EQ.0.0) GO TO 1800
      N = I+1
 1300 CONTINUE
      DO 1600 J=N,NUM
      IF(PTS(J).EQ.0.0) GO TO 1600
      DO K=1,NCHAN
         X = VAR(I,K)+VAR(J,K)-ABS(AVG(I,K)-AVG(J,K))
         IF(X.LE.0.0) GO TO 1600
      END DO
C
C     COMBINE 2 CLUSTERS, PUTTING RESULT IN THE FIRST CLUSTER'S POSITION
C     SET POPULATION OF SECOND CLUSTER TO ZERO
C
      SUM = PTS(I)+PTS(J)
      DO 1500 K=1,NCHAN
      AVG2 = (PTS(I)*AVG(I,K)+PTS(J)*AVG(J,K))/SUM
      X = ((PTS(I)-1.0)*VAR(I,K)*VAR(I,K)+(PTS(J)-1.0)*VAR(J,K)*VAR(J,K)
     +          +PTS(I)*AVG(I,K)*AVG(I,K)+PTS(J)*AVG(J,K)*AVG(J,K)-
     +           SUM*AVG2*AVG2)/(SUM-1.0)

      VAR(I,K) = SQRT(X)
      AVG(I,K) = AVG2
 1500 CONTINUE
      PTS(I) = SUM
      PTS(J) = 0.0
      NUM2 = NUM2-1
      GO TO 1300
 1600 CONTINUE
 1800 CONTINUE
      CALL SORT(PTS,AVG,VAR,NUM,NCLUS,NCHAN)
      NUM = NUM2
      MOUT=' '
      MOUT(8:44)=
     +'CLUSTERS AFTER COMBINING THOSE WHICH '
      MOUT(45:77)= 'OVERLAP BY ONE STANDARD DEVIATION'
      WRITE(MOUT(1:6),'(I6)') NUM
      CALL XVMESSAGE(MOUT(2:77),' ')
 2500 CONTINUE
C
C***PRINT THE POPULATION AND MEANS FOR EACH CLUSTER
C
      IF(.NOT.QPRT) GO TO 3000
      NW = 10
      ND = 2
      IFMT= '(I10)    '
      FXFMT='(F10.2)  '
      IF(NCHAN.GT.12) THEN
         NW=120/NCHAN
         IFMT= '(I9)     '
         FXFMT='(F9.2)   '
      ENDIF
      IF(NCHAN.GT.13)THEN
         IFMT= '(I8)     '
         FXFMT='(F.2)    '
      ENDIF
      IF(NCHAN.GT.15)THEN
         IFMT= '(I7)     '
         FXFMT='(F7.2)   '
      ENDIF
      IF(NCHAN.GT.17)THEN
         ND=1
         IFMT= '(I6)     '
         FXFMT='(F6.1)   '
      ENDIF
      IF(NCHAN.GT.20)THEN
         ND=0
         IFMT= '(I5)     '
         FXFMT='(F5.0)   '
      ENDIF
      MOUT='      PIXELS'
      CALL XVMESSAGE(' ',' ')
      DO I=1,NCHAN
         WRITE(MOUT(12+NW*I-(NW)+1:12+NW*I),IFMT) I
         IF(NW.GT.6) MOUT(7+NW*I:(7+NW*I)+4)='BAND'
      END DO
      CALL XVMESSAGE(MOUT(2:(12+NW*NCHAN)+1),' ')
      DO 2800 I=1,NUM
      MOUT= ' '
      WRITE(MOUT(1:5),'(I5)') I
      WRITE(MOUT(6:12),'(I7)') NINT(PTS(I))
      DO J=1,NCHAN
           WRITE(MOUT(12+NW*J-(NW)+1:12+NW*J),FXFMT) AVG(I,J)
      END DO
      CALL XVMESSAGE(MOUT(2:(12+NW*NCHAN)),' ')
 2800 CONTINUE
 3000 CONTINUE
C
C***PRINT THE POPULATION, MEANS, AND SIGMAS FOR ALL CLASSES TO BE OUTPUT
C     COMPUTE THE WIDTH OF COLUMNS FOR MEANS (NWM) AND SIGMAS (NWS),
C     AND DECIMAL POINTS (ND)
C
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('CLASSES RETAINED FOR OUTPUT',' ')
      NW = 18
      ND = 2
      IFMT1=  '(I18)    '
      FXFMTS= '(F8.2)   '
      FXFMTM= '(F10.2)  '
      IF(NCHAN.GT.7) THEN
         NW=126/NCHAN
         IFMT1= '(I15)    ' 
         FXFMTS='(F7.2)   '
         FXFMTM='(F8.2)   '
      END IF 
      NWS = (NW-1)/2
      NWM = NW-NWS
      IF(NW.LT.14) ND=1
      IF(NW.LT.12) ND=0
      IF(NCHAN.EQ.9) THEN
         IFMT1= '(I14)    '
         FXFMTS='(F6.2)   '
         FXFMTM='(F8.2)   '
      END IF 
      IF(NCHAN.EQ.10) THEN
         IFMT1= '(I12)    '
         FXFMTS='(F5.2)   '
         FXFMTM='(F7.1)   '
      END IF 
      IF(NCHAN.EQ.11) THEN
         IFMT1= '(I11)    '
         FXFMTS='(F5.2)   '
         FXFMTM='(F6.0)   '
      END IF
      IF(NCHAN.EQ.12) THEN
         IFMT1= '(I10)    '
         FXFMTS='(F4.2)   '
         FXFMTM='(F6.0)   '
      END IF
      IF(NCHAN.GT.12) THEN
         IFMT1= '(I9)     '
         FXFMTS='(F4.2)   '
         FXFMTM='(F5.0)   '
      END IF
      IF(NCHAN.GT.14) THEN
         IFMT1= '(I8)     '
         FXFMTS='(F3.2)   '
         FXFMTM='(F5.0)   '
      END IF
      IF(NCHAN.GT.15) THEN
         IFMT1= '(I7)     '
         FXFMTS='(F3.2)   '
         FXFMTM='(F4.0)   '
      END IF
      IF(NCHAN.GT.18) THEN
         IFMT1= '(I6)     '
         FXFMTS='(F2.1)   '
         FXFMTM='(F4.0)   '
      END IF
      IF(NCHAN.GT.22) THEN
         IFMT1= '(I5)     '
         FXFMTS='(F2.0)   '
         FXFMTM='(F3.0)   '
      END IF

C     COMPOSE AND PRINT TWO LINES OF HEADER
C
      MOUT(1:4)='    '
      DO I=1,NCHAN
         WRITE(MOUT(4+NW*I-(NW)+1:4+NW*I),IFMT1) I
         IF(NW.GE.9) MOUT(NW*I-2:NW*I+1)='BAND'
      END DO
      CALL XVMESSAGE(MOUT(2:4+NW*NCHAN),' ')
      MOUT = ' CLASS'
      IF(NW.GE.10) THEN
         DO I=1,NCHAN
            MOUT(3+NW*I-NWS:3+NW*I-NWS+3)='MEAN'
            MOUT(4+NW*I:4+NW*I+2)='SIG'
         END DO
      ELSE
         DO I=1,NCHAN
            MOUT(6+NW*I-NWS:6+NW*I-NWS)='M'
            MOUT(6+NW*I:6+NW*I)='S'
         END DO
      END IF
      CALL XVMESSAGE(MOUT(2:NW*NCHAN+6),' ')
      MOUT(6:6)= ' '
C
C     DETERMINE IF CLUSTER IS TO BE KEPT, AND IF SO, PRINT POPULATION, M
C     AND SIGMAS
C
      CUTOFF = 0.01*PERC*PIX
      DO 3300 I=1,NUM
         IF(PTS(I).LT.CUTOFF.OR.I.GT.NCLASS) GO TO 3400
         WRITE(MOUT(1:5),'(I5)') I         
         DO J=1,NCHAN
           WRITE(MOUT(6+NW*J-(NWM+NWS)+1:6+NW*J),FXFMTM) AVG(I,J)
           WRITE(MOUT(6+NW*J-NWS+1:6+NW*J),FXFMTS) VAR(I,J)
         END DO
         CALL XVMESSAGE(MOUT(2:6+NW*NCHAN),' ')
 3300 CONTINUE
      GO TO 3500
 3400 CONTINUE
      NUM = I-1
 3500 CONTINUE


C        DETERMINE NUMBER OF SAMPLES FOR OUTPUT DATA SET
      NSOUT = 12+4*NCHAN+2*NCHAN*(NCHAN+1)
      
C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL ISTAT_FILE_OPEN(OUNIT,'WRITE',NUM,NCHAN,' ',ISTAT)
      IF (ISTAT.LT.0) CALL ISTAT_SIGNAL(OUNIT,ISTAT,1)

C        WRITE OUTPUT, ONE RECORD FOR EACH CLASS
      XOUT(1:5) ='CLASS'
      DO 3700 I=1,NUM
         WRITE(XOUT(6:8),'(I3)') I
         LOC = 0
         DO J=1,NCHAN
            LOC = LOC + J
            ZZ(J) = AVG(I,J)
            XX(LOC) = SNGL(VAR(I,J)*VAR(I,J))
         END DO
      CALL ISTAT_RECORD_WRITE(OUNIT,I,XOUT,NINT(PTS(I)),NCHAN,
     &                        ZZ,XX,ISTAT)
         IF (ISTAT.LT.0) CALL ISTAT_SIGNAL(OUNIT,ISTAT,1)
 3700 CONTINUE

C        CLOSE OUTPUT DATA
      CALL ISTAT_FILE_CLOSE(OUNIT,ISTAT,1)
      IF (ISTAT.LT.0) CALL ISTAT_SIGNAL(OUNIT,ISTAT,1)
      RETURN
      END

      SUBROUTINE GETLIN(IUNIT,LINE,IN,SL,SS,NSCHAN,NCHAN,MSSCHN,NSO,
     &                  QMSS,QBAND)

C
C     GETLIN GETS A LINE OF INPUT FOR ALL CHANNELS AND FORMATS THE DATA
C     INTO THE ARRAY 'IN'.
C
      INTEGER*4    SL,SS,STAT,IUNIT(24)
      INTEGER*2    IN(NSO,NCHAN)
      LOGICAL*1    QMSS
      LOGICAL*1    QBAND(24)

      N = SL+LINE-1
      IF(QMSS) GO TO 200
C
C        MULTIPLE INPUTS
      DO I=1,NCHAN
      CALL XVREAD(IUNIT(I),IN(1,I),STAT,'LINE',N,
     +                              'SAMP',SS,'NSAMPS',NSO,' ')
      END DO

      RETURN
C
C        MSS INPUT
200   NBYTES = NSCHAN*MSSCHN-SS-1
      CALL XVREAD(IUNIT(1),IN,STAT,'LINE',N,'SAMP',S
     +                                   S,'NSAMPS',NBYTES,' ')

      IF(NSO.EQ.NSCHAN) GO TO 250
C        SIZE FIELD BEING USED
      DO I=2,MSSCHN
         M = (I-1)*NSCHAN
         K = M/NSO+1
         J = M-NSO*(K-1)+1
         CALL MVE(2,NSO,IN(J,K),IN(1,I),1,1)
      END DO
  250 CONTINUE
      IF(MSSCHN.EQ.NCHAN) RETURN
C
C        NOT ALL BANDS TO BE USED; REMOVE THE BANDS TO BE IGNORED
      J = 1
      N = 2*NSO
      DO 300 I=1,MSSCHN
         IF(.NOT.QBAND(I)) GO TO 300
         IF(I.NE.J) CALL MVE(1,N,IN(1,I),IN(1,J),1,1)
         J = J+1
  300 CONTINUE

      RETURN
      END

      SUBROUTINE SORT(PTS,AVG,VAR,NUM,NCLUS,NCHAN)
C
C***SORT CLUSTERS BY POPULATION
C
      REAL*8 VAR(NCLUS,NCHAN)
      REAL PTS(NCLUS),AVG(NCLUS,NCHAN)

      M = NUM/2
 2000 CONTINUE
      K = NUM-M
      J = 1
 2100 CONTINUE
      I = J
 2200 CONTINUE
      L = I+M
      IF(PTS(I).GT.PTS(L)) GO TO 2400
      HOLD = PTS(I)
      PTS(I) = PTS(L)
      PTS(L) = HOLD
      DO 2300 N=1,NCHAN
         HOLD = AVG(I,N)
         AVG(I,N) = AVG(L,N)
         AVG(L,N) = HOLD
         HOLD = VAR(I,N)
         VAR(I,N) = VAR(L,N)
         VAR(L,N) = HOLD
 2300 CONTINUE
      I = I-M
      IF(I.GE.1) GO TO 2200
 2400 CONTINUE
      J = J+1
      IF(J.LE.K) GO TO 2100
      M = M/2
      IF(M.GT.0) GO TO 2000

      RETURN
      END

      FUNCTION DIST(IN,ISAMP,AVG,ICLUS,NCHAN,NCLUS,NSO)
C
C     DIST COMPUTES THE SQUARE OF THE EUCLIDEAN DISTANCE OF PIXEL 'ISAMP
C     FROM THE MEAN OF CLUSTER 'ICLUS'
C
      INTEGER*2 IN(NSO,NCHAN)
      REAL AVG(NCLUS,NCHAN)

      DIST = 0.0
      DO I=1,NCHAN
         X = IN(ISAMP,I)
         X = X-AVG(ICLUS,I)
         DIST = DIST+X*X
      END DO

      RETURN
      END

      SUBROUTINE EXCHK(IN,ISAMP,IEXCL,NEXCL,NCHAN,NSO,*)
C
C     EXCHK CHECKS TO SEE IF PIXEL 'ISAMP' HAS A DN THAT IS TO BE EXCLUD
C     IF SO, THE ALTERNATE RETURN IS USED.
C
      INTEGER IEXCL(11)
      INTEGER*2 IN(NSO,NCHAN)

      DO 200 I=1,NEXCL
         DO J=1,NCHAN
            IF(IN(ISAMP,J).EQ.IEXCL(I)) RETURN 1
         END DO
  200 CONTINUE

      RETURN
      END

      SUBROUTINE ADD(IN,ISAMP,ICLUS,NCHAN,NCLUS,NSO,NUM,VAR,AVG,PTS,*)
C
C     ADD COMBINES PIXEL 'ISAMP' WITH CLUSTER 'ICLUS'
C
      REAL*8 VAR(NCLUS,NCHAN)
      REAL AVG(NCLUS,NCHAN),PTS(NCLUS)
      INTEGER*2 IN(NSO,NCHAN)

      DO N=1,NCHAN
         X = IN(ISAMP,N)
         VAR(ICLUS,N) = VAR(ICLUS,N)+X*X
         AVG(ICLUS,N) = (AVG(ICLUS,N)*PTS(ICLUS)+X)/(PTS(ICLUS)+1.0)
      END DO
      PTS(ICLUS) = PTS(ICLUS)+1.0
      IN(ISAMP,1) = 9999
      IF(ICLUS.GT.NUM) NUM=ICLUS

      RETURN 1
      END
