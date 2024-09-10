CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compress an image via integral LZOOM, IZOOM by first converting the
C DN to some unspecified units via a table look-up, computing the average
C value for each area, and then converting back to DN via the inverse table.
C Input and output images may be byte.
C
      SUBROUTINE LOOKUP(INTERP,LZOOM,IZOOM,NSI,NSO,buf,rbuf,*)
      IMPLICIT INTEGER (A-Z)
      BYTE BUF(NSI)
      LOGICAL INTERP
      REAL*4 RBUF(NSO)

      COMMON/CP/ICODE,SLI0,SSI0,SLI,SSI,NLI,NSIX,NLI0,NSI0,OUNIT,OCODE,
     &          NLO,NSOX,ZOOML,ZOOMS,GSCALE,ILO,IHI,LFLAG
      INTEGER SLI0,SSI0,SLI,SSI,OUNIT,OCODE

      COMMON/C3/IUNIT
      REAL*4 ZOOML,ZOOMS,GSCALE

      INTEGER*4 SINC
      REAL*4 C
      INCLUDE 'slookup.inc'
      INCLUDE 'fortport'

      IF (.NOT.INTERP.OR.LZOOM.GE.0.OR.IZOOM.GE.0.OR.NLI.EQ.1
     &		.OR.GSCALE.NE.1.0) GOTO 990
      CALL MGNSIZE(*999)	!Initialize voltage to dn arrays
      LINC = -LZOOM
      SINC = -IZOOM
      M = LINC - 1
      N = SINC - 1
      C = ZOOML*ZOOMS
      IF (SLI.GT.1) CALL XVREAD(IUNIT,BUF,IND,'LINE',SLI-1,' ')

      DO L=1,NLO
         CALL XVREAD(IUNIT,BUF,IND,' ')
         II = SSI
         DO I=1,NSO
            ISUM = INPTABLE(BYTE2INT(BUF(II)))
            II = II +1
            DO J=1,N
               ISUM = ISUM + INPTABLE(BYTE2INT(BUF(II)))
               II = II + 1
            ENDDO
            RBUF(I) = ISUM
         ENDDO

         DO K=1,M
            CALL XVREAD(IUNIT,BUF,IND,' ')
            II = SSI
            DO I=1,NSO
               ISUM = INPTABLE(BYTE2INT(BUF(II)))
               II = II +1
               DO J=1,N
                  ISUM = ISUM + INPTABLE(BYTE2INT(BUF(II)))
                  II = II + 1
               ENDDO
               RBUF(I) = RBUF(I) + ISUM
            ENDDO
         ENDDO

         DO I=1,NSO
            J = RBUF(I)*C + 0.5
            BUF(I) = OUTTABLE(J)
         ENDDO
         CALL XVWRIT(OUNIT,BUF,IND,' ')
      ENDDO
      RETURN

  990 CALL XVMESSAGE(' ***Invalid parameter option for LOOKUP',' ')
      IF (.NOT.INTERP) THEN
         CALL XVMESSAGE(' ***Interpolation cannot be suppressed',' ')
      ELSE IF (LZOOM.EQ.0.OR.IZOOM.EQ.0) THEN
         CALL XVMESSAGE(' ***Integral ZOOM must be specified',' ')
      ELSE IF (LZOOM.GT.0.OR.IZOOM.GT.0) THEN
         CALL XVMESSAGE(' ***Image magnification not permitted',' ')
      ELSE IF (GSCALE.NE.1.0) THEN
         CALL XVMESSAGE(' ***SCALE parameter cannot be specified',' ')
      ELSE
         CALL XVMESSAGE(' ***Input image contains only one line',' ')
      ENDIF
  999 RETURN1
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Get slope and offset and compute DN-to-volts table.
C
      SUBROUTINE DTVTABLE(slope,offset,table)
      REAL*4 TABLE(0:255)

      CALL XVP('SLOPE',slope,icnt)
      CALL XVP('OFFSET',offset,icnt)

      DO I=0,255
         Dn = I
         Vo = 10.0**((Dn-OFFSET)/SLOPE)
         TABLE(I) = Vo
      ENDDO

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compress an image via integral LZOOM, IZOOM for Magellan.
C Input and output images may be byte.
C Note: In this version of SIZE, all Magellan specific code is
C consolidated here and in subroutines SMGN1 and MgnSizeInit.
C The subroutines MgnDNtoVo and MgnVotoDN are never called.
C
      SUBROUTINE SMGN(INTERP,LZOOM,IZOOM,TABLE,SLOPE,OFFSET,NSI,NSO,
     &		buf,rbuf,*)
      BYTE BUF(NSI)
      LOGICAL INTERP
      REAL*4 RBUF(NSO),TABLE(0:255)

      COMMON/CP/ICODE,SLI0,SSI0,SLI,SSI,NLI,NSIX,NLI0,NSI0,OUNIT,OCODE,
     &          NLO,NSOX,ZOOML,ZOOMS,GSCALE,ILO,IHI,LFLAG
      INTEGER SLI0,SSI0,SLI,SSI,OUNIT,OCODE

      COMMON/C3/IUNIT

      REAL*4 LOGTBL(0:2249)

      INTEGER*4 SINC
      REAL*4 C
      INCLUDE 'fortport'

      IF (.NOT.INTERP.OR.LZOOM.GE.0.OR.IZOOM.GE.0.OR.NLI.EQ.1
     &		.OR.GSCALE.NE.1.0) GOTO 990
C     ....Generate log table
      DO I=0,2249
         V = 1.0 + 0.004*I
         LOGTBL(I) = ALOG10(V)
      ENDDO

      LINC = -LZOOM
      SINC = -IZOOM
      M = LINC - 1
      N = SINC - 1
      C = ZOOML*ZOOMS
      OFFSET = OFFSET + 0.5
      IF (SLI.GT.1) CALL XVREAD(IUNIT,BUF,IND,'LINE',SLI-1,' ')

      DO L=1,NLO
         CALL XVREAD(IUNIT,BUF,IND,' ')
         II = SSI
         DO I=1,NSO
            RSUM = TABLE(BYTE2INT(BUF(II)))
            II = II +1
            DO J=1,N
               RSUM = RSUM + TABLE(BYTE2INT(BUF(II)))
               II = II + 1
            ENDDO
            RBUF(I) = RSUM
         ENDDO

         DO K=1,M
            CALL XVREAD(IUNIT,BUF,IND,' ')
            II = SSI
            DO I=1,NSO
               RSUM = TABLE(BYTE2INT(BUF(II)))
               II = II +1
               DO J=1,N
                  RSUM = RSUM + TABLE(BYTE2INT(BUF(II)))
                  II = II + 1
               ENDDO
               RBUF(I) = RBUF(I) + RSUM
            ENDDO
         ENDDO

         DO I=1,NSO
            V = RBUF(I)*C
            IF (V.LT.100.0) THEN
               IF (V.LT.1.0) THEN
                  IF (V.LT.0.1) THEN
                     IF (V.LT.0.01) THEN !Here if V < 0.01
                        IDN = SLOPE*ALOG10(V) + OFFSET  !Convert to DN
                        GOTO 50
                     ELSE		!Here if 0.01 < V < 0.1
                        V = 100.0*V
                        P = -2.
                     ENDIF
                  ELSE			!Here if 0.1 < V < 1
                     V = 10.0*V
                     P = -1.
                  ENDIF
               ELSE
                  IF (V.LT.10.0) THEN	!Here if 1 < V < 10
                      P = 0.
                  ELSE			!Here if 10 < V < 100
                      V = 0.1*V
                      P = 1.
                  ENDIF
               ENDIF
            ELSE			!Here if 100 < V
               IF (V.LT.1000.0) THEN	!Here if 100 < V < 1000
                  V = 0.01*V
                  P = 2.
               ELSE
                  IF (V.LT.10000.) THEN !Here if 1000 < V < 10000
                     V = 0.001*V
                     P = 3.
                  ELSE   
                     IDN = SLOPE*ALOG10(V) + OFFSET  !Convert to DN
                     GOTO 50
                  ENDIF
               ENDIF
            ENDIF     
            IX = 250.*(V-1.) + 0.5
            IX = MIN0(IX,2249)
            IDN = SLOPE*(LOGTBL(IX)+P) + OFFSET  !Convert to DN
   50       BUF(I) = INT2BYTE(IDN)
         ENDDO
         CALL XVWRIT(OUNIT,BUF,IND,' ')
      ENDDO
      RETURN

  990 CALL XVMESSAGE(' ***Invalid parameter option for Magellan',' ')
      IF (.NOT.INTERP) THEN
         CALL XVMESSAGE(' ***Interpolation cannot be suppressed',' ')
      ELSE IF (LZOOM.EQ.0.OR.IZOOM.EQ.0) THEN
         CALL XVMESSAGE(' ***Integral ZOOM must be specified',' ')
      ELSE IF (LZOOM.GT.0.OR.IZOOM.GT.0) THEN
         CALL XVMESSAGE(' ***Image magnification not permitted',' ')
      ELSE IF (GSCALE.NE.1.0) THEN
         CALL XVMESSAGE(' ***SCALE parameter cannot be specified',' ')
      ELSE
         CALL XVMESSAGE(' ***Input image contains only one line',' ')
      ENDIF
      RETURN1
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compress an image for Magellan.
C Special case: ZOOM=-3 (exact solution)
C Input and output images may be byte.
C
      SUBROUTINE SMGN1(INTERP,TABLE,SLOPE,OFFSET,NSI,NSO,buf,rbuf,*)
      BYTE BUF(NSI)
      LOGICAL INTERP
      REAL*4 RBUF(NSO),TABLE(0:255)

      COMMON/CP/ICODE,SLI0,SSI0,SLI,SSI,NLI,NSIX,NLI0,NSI0,OUNIT,OCODE,
     &          NLO,NSOX,ZOOML,ZOOMS,GSCALE,ILO,IHI,LFLAG
      INTEGER SLI0,SSI0,SLI,SSI,OUNIT,OCODE

      COMMON/C3/IUNIT

      INCLUDE 'fortport'

      IF (.NOT.INTERP.OR.NLI.EQ.1.OR.GSCALE.NE.1.0) GOTO 990
      IF (SLI.GT.1) CALL XVREAD(IUNIT,BUF,IND,'LINE',SLI-1,' ')
      OFFSET = OFFSET + 0.5		!For rounding off result

      DO L=1,NLO
         CALL XVREAD(IUNIT,BUF,IND,' ')
         II = SSI
         DO I=1,NSO
            RBUF(I) = TABLE(BYTE2INT(BUF(II)))
     &           + TABLE(BYTE2INT(BUF(II+1)))
     &           + TABLE(BYTE2INT(BUF(II+2)))
            II = II + 3
         ENDDO

         DO K=1,2
            CALL XVREAD(IUNIT,BUF,IND,' ')
            II = SSI
            DO I=1,NSO
               RBUF(I) = RBUF(I) + TABLE(BYTE2INT(BUF(II)))
     &           + TABLE(BYTE2INT(BUF(II+1)))
     &           + TABLE(BYTE2INT(BUF(II+2)))
               II = II + 3
            ENDDO
         ENDDO

         DO I=1,NSO
            Vo = RBUF(I)/9.0
            IDN = SLOPE*ALOG10(Vo) + OFFSET
            BUF(I) = INT2BYTE(IDN)
         ENDDO
         CALL XVWRIT(OUNIT,BUF,IND,' ')
      ENDDO
      RETURN

  990 CALL XVMESSAGE(' ***Invalid parameter option for Magellan',' ')
      IF (.NOT.INTERP) THEN
         CALL XVMESSAGE(' ***Interpolation cannot be suppressed',' ')
      ELSE IF (GSCALE.NE.1.0) THEN
         CALL XVMESSAGE(' ***SCALE parameter cannot be specified',' ')
      ELSE
         CALL XVMESSAGE(' ***Input image contains only one line',' ')
      ENDIF
      RETURN1
      END
