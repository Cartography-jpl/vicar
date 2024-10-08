CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compress an image.  The vertical direction is compressed.  The
C horizontal direction may be magnified or compressed.
C 
C
      SUBROUTINE COMPRESS(SCALE,NS,SAMP,WGHT,buf,rbuf,obuf)
      IMPLICIT NONE
      INTEGER*4 NS,SAMP(NS),OBUF(NS)
      REAL*4 SCALE,BUF(1),RBUF(NS,2),WGHT(NS)

      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI
      COMMON/CP/OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      COMMON/CP/ZOOML,ZOOMS,LZOOM,IZOOM,GSCALE,ILO,IHI,LFLAG
      INTEGER*4 IUNIT,ICODE,SLI,SSI,NLI,NSI
      INTEGER*4 OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      INTEGER*4 LZOOM,IZOOM,ILO,IHI,LFLAG
      REAL*4 ZOOML,ZOOMS,GSCALE

      INTEGER*4 I1,I2,J1,J2
      INTEGER*4 J,L,N,ELI,LINE,IND,INC
      REAL*4 R,C,D,Y2

      R = 1.0/ZOOML
      ELI = SLI + NLI - 1
      I1 = 1
      I2 = 2
      IF (LZOOM.LT.0) GOTO 40
C
C     ....Here to compress an image using a real zoom in the vertical
C     ....direction.  The horizontal zoom may be real or integral valued.
      J1 = SLI
      C = ZOOML*SCALE
      D = 0.
      CALL SREAD(IUNIT,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     &		SAMP,WGHT,buf,rbuf(1,i2))
C
      DO 20 L=1,NLO
      CALL SMUL(NSO,RBUF(1,I2),RBUF,1.-D)
      Y2 = L*R + SLI
      J2 = Y2
      J2 = MIN0(J2,ELI)
      D = Y2 - J2
      J1 = MIN0(J1+1,ELI)
      DO WHILE (J1.NE.J2)
         CALL SREAD(IUNIT,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     &			SAMP,WGHT,buf,rbuf(1,i2))
         CALL ADDV(7,NSO,RBUF(1,I2),RBUF,1,1)
         J1 = MIN0(J1+1,ELI)
      ENDDO
      CALL SREAD(IUNIT,J1,SSI,NSI,NSO,IZOOM,ZOOMS,
     &			SAMP,WGHT,buf,rbuf(1,i2))
      CALL INTRPV(OCODE,NSO,ILO,IHI,C,C*D,RBUF,RBUF(1,I2),
     &			obuf(sso),obuf(sso))
   20 CALL XVWRIT(OUNIT,OBUF,ind,' ')
      RETURN

C     ....Here to compress vertical scale by an integral lzoom
   40 INC = -LZOOM
      N = INC - 1
      LINE = SLI
      C = ZOOML*SCALE

      DO 50 L=1,NLO
      CALL SREAD(IUNIT,LINE,SSI,NSI,NSO,IZOOM,ZOOMS,
     &			SAMP,WGHT,buf,rbuf(1,i2))
      DO J=1,N
         CALL SREAD(IUNIT,LINE+J,SSI,NSI,NSO,IZOOM,ZOOMS,
     &			SAMP,WGHT,buf,rbuf)
         CALL ADDV(7,NSO,RBUF,RBUF(1,I2),1,1)
      ENDDO
      LINE = LINE + INC
      CALL OUT_SCALE(OCODE,C,ILO,IHI,NSO,RBUF(1,I2),obuf(sso),obuf(sso))
   50 CALL XVWRIT(OUNIT,OBUF,ind,' ')

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Multiply a line by a constant C
C
      SUBROUTINE SMUL(NSO,RIN,ROUT,C)
      REAL*4 RIN(NSO),ROUT(NSO)
C
      DO I=1,NSO
         ROUT(I) = C*RIN(I)
      ENDDO
      RETURN
      END
