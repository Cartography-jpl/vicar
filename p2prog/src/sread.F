CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Read an image line and magnify or reduce it via interpolation.
C The input line (BUF) may be byte, halfword, fullword, or real*4.
C The output line (RBUF) is REAL*4.
C
      SUBROUTINE SREAD(IUNIT,LINE,SSI,NSI,NSO,IZOOM,ZOOMS,
     &		SAMP,WGHT,buf,rbuf)
      INTEGER*4 IUNIT,LINE,SSI,NSI,NSO,IZOOM,SAMP(NSO)
      REAL*4 ZOOMS,WGHT(NSO),BUF(NSI),RBUF(NSO)

      IF (IZOOM.EQ.1) THEN
         CALL XVREAD(IUNIT,rbuf,ind,'LINE',LINE,'SAMP',SSI,
     &		   'NSAMPS',NSI,' ')
         RETURN
      ENDIF

      CALL XVREAD(IUNIT,buf,ind,'LINE',LINE,'SAMP',SSI,
     &   'NSAMPS',NSI,' ')

      IF (NSI.EQ.1) THEN		
         CALL MVE(7,NSO,BUF,RBUF,0,1)		!Replicate pixel NSO times.
      ELSE IF (ZOOMS.GT.1.) THEN
         CALL MAG(NSO,NSI,BUF,SAMP,WGHT,rbuf)	!Magnify the line
      ELSE IF (IZOOM.LT.0) THEN
         CALL SHRINZ(IZOOM,NSO,NSI,BUF,rbuf)	!Compress line via integral zoom
      ELSE				
         CALL SHRINK(NSO,NSI,BUF,SAMP,WGHT,rbuf) !Compress line via real zoom
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnifies an image line via interpolation.
C
      SUBROUTINE MAG(NSO,NSI,BUF,SAMP,WGHT,rbuf)
      INTEGER*4 NSO,NSI,SAMP(NSO)
      REAL*4 BUF(NSI),RBUF(NSO),WGHT(NSO)

      I0 = 0
      D2 = BUF(1)

      DO I=1,NSO
         I2 = SAMP(I)
         IF (I2.NE.I0) THEN
            I0 = I2
            D1 = D2
            D2 = BUF(I2)
         ENDIF
         A = WGHT(I)
         RBUF(I) = A*D1 + (1.0-A)*D2
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compress a line by a floating pt zoom factor
C
      SUBROUTINE SHRINK(NSO,NSI,BUF,SAMP,WGHT,rbuf)
      INTEGER*4 NSO,NSI,SAMP(NSO)
      REAL*4 BUF(NSI),RBUF(NSO),WGHT(NSO)

      I1 = 1
      D1 = BUF(1)
      D0 = 0.0
 
      DO I=1,NSO
         D = D1 - D0
         I2 = SAMP(I)
         I1 = I1 + 1
         IF (I1.LT.I2) THEN
            RSUM = 0
            DO WHILE (I1.LT.I2)
               RSUM = RSUM + BUF(I1)
               I1 = I1 + 1
            ENDDO
            D = D + RSUM
         ENDIF
         D1 = BUF(I2)
         D0 = WGHT(I)*D1
         RBUF(I) = D + D0
      ENDDO
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compresses a line by an integer zoom factor
C
      SUBROUTINE SHRINZ(IZOOM,NSO,NSI,BUF,RBUF)
      INTEGER*4 IZOOM,NSO,NSI
      REAL*4 BUF(NSI),RBUF(NSO)

      INC = -IZOOM
      II = 1

      DO I=1,NSO
         RBUF(I) = 0
         DO J=1,INC
            RBUF(I) = RBUF(I) + BUF(II)
            II = II + 1
         ENDDO
      ENDDO
      RETURN
      END
