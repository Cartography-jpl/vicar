      INCLUDE 'VICMAIN_FOR'
C**********************************************************************
      SUBROUTINE MAIN44
C
C        MODIFIED FOR VAX CONVERSION BY ALAN MAZER 28-JUL-83
C        CONVERTED TO VICAR2 BY J. REIMER 14-AUG-85
C
C        9-88  SP   MODIFIED BECAUSE DIV HAS BEEN RENAMED TO DIVV.
C        4-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C
      EXTERNAL MAIN
      COMMON /C1/ IUNIT,OUNIT,SL,SS,NLO,NSO,FORMAT,HIGH,NLW,NSW,
     &            ICYCLE,JCYCLE,IDC 

      INTEGER*4 OUNIT,STAT,SL,SS,HIGH
      CHARACTER*8 FORMAT
      LOGICAL XVPTST

C        SET DEFAULTS AND INITIALIZE
      HIGH=0
      NLW=11
      NSW=11
      ICYCLE=0
      JCYCLE=0
      IDC=128

C
      CALL IFMESSAGE('BOXFLT2 version 02-MAY-94')
C
C          OPEN INPUT DATA SET
      CALL XVEACTION('SA',' ')
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')
C
C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      IF(FORMAT.NE.'BYTE'.AND.FORMAT.NE.'HALF') THEN
         CALL XVMESSAGE('BOXFLT2 ACCEPTS BYTE AND HALFWORD DATA ONLY',
     +                  ' ')
         CALL ABEND
      END IF
C
C        GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
      IF(SL+NLO-1 .GT. NLI) THEN
         CALL XVMESSAGE
     +        ('NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE',' ')
         CALL ABEND
      END IF
      IF(SS+NSO-1 .GT. NSI) THEN
         CALL XVMESSAGE
     +        ('NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE',' ')
         CALL ABEND
      END IF
C
C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_FORMAT','HALF',
     &            'U_NL',NLO,'U_NS',NSO,' ')

C           PROCESS PARAMETERS
C        'HIGHPASS'
      IF(XVPTST('HIGHPASS')) HIGH=1
C        'NLW'
      CALL XVPARM('NLW',NLW,ICOUNT,IDEF,1)
      IF(NLW/2*2.EQ.NLW) CALL XVMESSAGE
     +                          ('WARNING-NLW IS AN EVEN INTEGER',' ')
C        'NSW'
      CALL XVPARM('NSW',NSW,ICOUNT,IDEF,1)
      IF(NSW/2*2.EQ.NSW) CALL XVMESSAGE
     +                          ('WARNING-NSW IS AN EVEN INTEGER',' ')
C        'CYCLE'
      IF(XVPTST('CYCLE')) THEN
         ICYCLE=1
         JCYCLE=1
      END IF
C        'SCYCLE'
      IF(XVPTST('SCYCLE')) ICYCLE=1
C        'LCYCLE'
      IF(XVPTST('LCYCLE')) JCYCLE=1
C        'DCLEVEL'
      CALL XVPARM('DCLEVEL',IDC,ICOUNT,IDEF,1)

      NS=NSO+NSW
      CALL STACKA(7,MAIN,5,4*NS,4*NS,2*NS,2*NS,2*NS)

C        CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE MAIN(ISUM,LX,TBUF,MX,INBUF,IX,OUTBUF,JX,IDBUF,KX)

      COMMON /C1/ IUNIT,OUNIT,SL,SS,NLO,NSO,FORMAT,HIGH,NLW,NSW,
     &            ICYCLE,JCYCLE,IDC 

      INTEGER*4 ISUM(1),TBUF(1),OUNIT,STAT,SS,SL,HIGH
      INTEGER*2 INBUF(1),OUTBUF(1),IDBUF(1)
      CHARACTER*8 FORMAT

      M=NLW/2+1
      N=NSW/2+1
      L=N-1
      CALL ZIA(ISUM,NSO+NSW-1)   

      DO I=1,NLO
          ILINE=1-M+I
          IF(JCYCLE .EQ. 0) THEN
              IF(ILINE .LT. 1) ILINE=2-ILINE
              IF(ILINE .GT. NLO) ILINE=NLO+NLO-ILINE
          ELSE              	      
              IF(ILINE .LT. 1) ILINE=NLO+ILINE
              IF(ILINE .GT. NLO) ILINE=ILINE-NLO
          END IF
          CALL XVREAD(IUNIT,INBUF(N),STAT,'LINE',SL+ILINE-1,
     &                'SAMP',SS,'NSAMPS',NSO,' ')    
          IF (I.LE.NLW) THEN
              IF(N.GT.1) THEN
                  IF(ICYCLE .EQ. 0) THEN
                      CALL RFLCT(N,NSO,INBUF)
                  ELSE 
                      CALL CYCLE(N,NSO,INBUF)
                  END IF
              END IF
              CALL ADDV(6,NSO+NSW-1,INBUF,ISUM,1,1)
          END IF
          CALL XVWRIT(OUNIT,INBUF(N),STAT,' ')
      END DO
      CALL ZAIRE(TBUF,ISUM,NSO,NSW)
      CALL DIVV(4,NSO,NLW*NSW,TBUF,0,1)
      CALL MVE(-6,NSO,TBUF,OUTBUF,1,1)

C        RE-OPEN OUTPUT FOR UPDATE
      CALL XVCLOSE(OUNIT,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','UPDATE','U_FORMAT','HALF',' ')
      ILINE=(NLW+1)/2

      DO I=2,NLO
          CALL XVREAD(OUNIT,IDBUF(N),STAT,'LINE',I-1,' ')
          CALL XVWRIT(OUNIT,OUTBUF,STAT,'LINE',I-1,' ')
          IF(N.GT.1) THEN
              IF(ICYCLE .EQ. 0) THEN 
                  CALL RFLCT(N,NSO,IDBUF)
              ELSE 
                  CALL CYCLE(N,NSO,IDBUF)
              END IF
          END IF
          CALL RSUBV(6,NSO+NSW-1,IDBUF,ISUM,1,1)

          JLINE=ILINE+1
          IF(JCYCLE .EQ. 0.AND.JLINE.GT.NLO) JLINE=NLO+NLO-JLINE
          IF(JCYCLE.NE.0.AND.JLINE .GT. NLO) JLINE=JLINE-NLO
          CALL XVREAD(IUNIT,INBUF(N),STAT,'LINE',SL+JLINE-1,
     &                'SAMP',SS,'NSAMPS',NSO,' ')        
          IF(N.GT.1) THEN
              IF(ICYCLE .EQ. 0) THEN 
                  CALL RFLCT(N,NSO,INBUF)
              ELSE 
                  CALL CYCLE(N,NSO,INBUF)
              END IF
          END IF
          CALL ADDV(6,NSO+NSW-1,INBUF,ISUM,1,1)
          CALL ZAIRE(TBUF,ISUM,NSO,NSW)
          CALL DIVV(4,NSO,NLW*NSW,TBUF,0,1)
          CALL MVE(-6,NSO,TBUF,OUTBUF,1,1)
          ILINE=ILINE+1
      END DO

      CALL XVWRIT(OUNIT,OUTBUF,STAT,'LINE',I-1,' ')

      IF(HIGH .NE. 1) RETURN

C        DO HIGHPASS OPERATION
      DO I=1,NLO
          CALL XVREAD(IUNIT,INBUF,STAT,'LINE',SL+I-1,
     &                'SAMP',SS,'NSAMPS',NSO,' ')
          CALL XVREAD(OUNIT,OUTBUF,STAT,'LINE',I,' ')
          CALL RSUBV(2,NSO,OUTBUF,INBUF,1,1)
          CALL ADDV(-6,NSO,IDC,INBUF,0,1)
          IF(FORMAT.EQ.'BYTE') CALL CUTOFF(INBUF,NSO)
          CALL XVWRIT(OUNIT,INBUF,STAT,'LINE',I,' ')
      END DO

      CALL XVMESSAGE('HIGH PASS FILTER PERFORMED.',' ')

20    RETURN
      END
C**********************************************************************
      SUBROUTINE RFLCT(N,NSO,INBUF)
      INTEGER*2 INBUF(1)

      L=N-1
      CALL MVE(2,L,INBUF(N+1),INBUF(N-1),1,-1)
      CALL MVE(2,L,INBUF(NSO+L-1),INBUF(NSO+N),-1,1)

      RETURN
      END
C**********************************************************************
      SUBROUTINE CYCLE(N,NSO,INBUF)
      INTEGER*2 INBUF(1)

      L=N-1
      CALL MVE(2,L,INBUF(NSO+1),INBUF(1),1,1)
      CALL MVE(2,L,INBUF(N),INBUF(N+NSO),1,1)

      RETURN
      END
C**********************************************************************
      SUBROUTINE CUTOFF(INBUF,NSO)
      INTEGER*2 INBUF(1)

      DO I=1,NSO
          IF (INBUF(I).GT.255) INBUF(I)=255
          IF (INBUF(I).LT.0) INBUF(I)=0
      END DO

      RETURN
      END
