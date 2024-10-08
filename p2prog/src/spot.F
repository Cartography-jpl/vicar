      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C**** VERSION 3 VICAR-2 PARAMS 20 AUGUST 1984 MEM
C     VERSION 2 VICAR-2 IO     13 AUGUST 1984 MEM
C     VERSION 1 VAX CONVERSION 7 AUGUST 1984 MEM
      INCLUDE 'fortport'
      COMMON/C1/DN
      CHARACTER*8 STRING
      INTEGER OUTUNIT,IND,CNT
      INTEGER*4 DNMAX,SIGMAX,SIGMAY,X0,Y0
      BYTE DN(2000)
C
C**** PARAMETER INITIALIZATION
      NL=1024
      NS=1024
      SL=1
      SS=1
      DNMAX=255
      SIGMAX=125
      SIGMAY=125
      X0=512
      Y0=512
C**** VICAR*2 OPENS
      CALL IFMESSAGE('SPOT version 02-MAY-94')
      CALL XVSIZE(SL,SS,NL,NS,NLI,NSI)
      CALL XVUNIT(OUTUNIT,'OUT',1,IND,' ')
      CALL XVOPEN(OUTUNIT,IND,'U_NL',NL,'U_NS',NS,'OP','WRITE',' ')
C**** NOW PROCESS THE TAE/PDF
C     TO GET THE PARAMETER VALUES
C     USE DEFAULTS IF "CNT"=0
      CALL XVP('DNMAX',DNMAX,CNT)
      CALL XVP('SIGMAX',SIGMAX,CNT)
      IF(CNT.EQ.0)SIGMAX=NS/8
      CALL XVP('SIGMAY',SIGMAY,CNT)
      IF(CNT.EQ.0)SIGMAY=NL/8
      CALL XVP('X0',X0,CNT)
      IF(CNT.EQ.0)X0=NS/2
      CALL XVP('Y0',Y0,CNT)
      IF(CNT.EQ.0)Y0=NL/2

c
c     bam 7/98 AR-9267
c
c     make sure we don't have a divide by 0
c
      if (dnmax .ne. 0 ) then
          DXMX=255/DNMAX
      else
          dxmx = 0 
      end if

      EPS=1.E-5
      CALL XVP('SHAPE',STRING,CNT)
      IF(CNT.EQ.0)ISHAPE=1
      IF(STRING.EQ.'GAUSSIAN')ISHAPE=1
      IF(STRING.EQ.'CONICAL')ISHAPE=2
      IF(STRING.EQ.'RECPROCL')ISHAPE=3
      IF(STRING.EQ.'RECPSQRE')ISHAPE=4
      IF(STRING.EQ.'EXPONENT')ISHAPE=5
      IF(STRING.EQ.'DOME')ISHAPE=6


C**** COMPUTE RADIAL DISTANCE

      DO 100 IY=1,NL
c
c     bam 7/98 AR-9267
c
c     make sure we don't have a divide by 0
c     for both sigmay and sigmax
c
      if (sigmay .ne. 0 ) then
          YARG=(IY-Y0)*1./(SIGMAY*1.)
      else
          yarg = 0 
      end if

      YARGS=YARG**2

      DO 99 IX=1,NS
          if (sigmax .ne. 0 ) then
              XARG=(IX-X0)*1./(SIGMAX*1.)
          else
              xarg = 0 
          end if

      R2=XARG**2+YARGS
      
      R=SQRT(R2)
C**** BRANCH TO SHAPE
               GO TO (51,52,53,54,55,56),ISHAPE
C     GAUSSIAN SPOT
   51 IF (R2.GE.100.) R2=100.
      FACT=EXP(-0.5*R2)
      GO TO 60
C     CONICAL SPOT
   52 FACT=1.-R
      IF (FACT.LT.0.) FACT=0.
      GO TO 60
C     RECIPROCAL SPOT
   53 IF (R.GT.EPS) TMPF=1./R
      IF ((R.EQ.0.).OR.(TMPF.GE.DXMX)) TMPF=DXMX
      FACT=TMPF
      GO TO 60
C     RECIPROCAL SQUARED SPOT
   54 IF (R2.GT.EPS) TMPF=1./R2
      IF ((R2.EQ.0.).OR.(TMPF.GT.DXMX)) TMPF=DXMX
      FACT=TMPF
      GO TO 60
C     EXPONENTIAL SPOT
   55 IF (R.GE.100.) R=100.
      FACT=EXP(-1.*R)
      GO TO 60
C     DOME SPOT
   56 AF=1.-R2
      IF (AF.LE.0.) AF=0.
      FACT=SQRT(AF)
C**** CALCULATE OUTPUT VALUE AT SAMPLE LOCATION OF EACH PIXEL
   60 IVAL = DNMAX*FACT
      DN(IX)=INT2BYTE(IVAL)
   99 CONTINUE
      CALL XVWRIT(OUTUNIT,DN,IND,' ')
  100 CONTINUE
      IF(ISHAPE.EQ.1)CALL XVMESSAGE('****GAUSSIAN PATTERN GENERATED '
     *,' ')
      IF(ISHAPE.EQ.2)CALL XVMESSAGE('****CONICAL PATTERN GENERATED '
     *,' ')
      IF(ISHAPE.EQ.3)CALL XVMESSAGE('****RECIPROCAL PATTERN GENERATED '
     *,' ')
      IF(ISHAPE.EQ.4)CALL XVMESSAGE('****RECP SQUARED PATTERN GENERATED'
     *,' ')
      IF(ISHAPE.EQ.5)CALL XVMESSAGE('****EXPONENTIAL PATTERN GENERATED'
     *,' ')
      IF(ISHAPE.EQ.6)CALL XVMESSAGE('****DOME PATTERN GENERATED ',' ')
      CALL XVCLOSE(OUTUNIT,IND,' ')
      RETURN
      END
