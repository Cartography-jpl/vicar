CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Expand or reduce an image without interpolation.
C Input and output data formats are either both byte or both halfword.
C Mixed data modes (e.g. byte input, halfword output) are handled by
C enabling VICAR I/O data conversion via the XVOPEN calls.
C
      SUBROUTINE SNOIN(buf,obuf,samp)
      IMPLICIT NONE
      INTEGER*2 BUF(1)		!size=NSI
      LOGICAL*1 OBUF(1)		!size=NSO
      INTEGER*4 SAMP(1)		!size=NSO

      COMMON/CP/IUNIT,ICODE,SLI,SSI,NLI,NSI
      COMMON/CP/OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      COMMON/CP/ZOOML,ZOOMS,LZOOM,IZOOM,GSCALE,ILO,IHI,LFLAG
      INTEGER*4 IUNIT,ICODE,SLI,SSI,NLI,NSI
      INTEGER*4 OUNIT,OCODE,SLO,SSO,NLO,NSO,NLOUT,NSOUT
      INTEGER*4 LZOOM,IZOOM,ILO,IHI,LFLAG
      REAL*4 ZOOML,ZOOMS,GSCALE

      COMMON/C1/TBL(0:255),TBLH(-32768:32767)
      LOGICAL*1 TBL
      INTEGER*2 TBLH

      COMMON/C2/RBUF(50000)
      REAL*4 RBUF
      INTEGER*2 HBUF(50000)
      INTEGER*4 FBUF(50000)
      EQUIVALENCE (RBUF,HBUF,FBUF)

      INTEGER*4 I,J,L,ILINE,ILINE0,IND
      REAL*4 OFFSET,RSLI,R

      R = 1./ZOOMS
      OFFSET = 0.5
      IF (ZOOMS.LT.1.0) OFFSET=1.0
C
C     ....Assign input pixel to each pixel on output image line.
      DO I=1,NSO			    !I=output pixel index
         J = R*(I-OFFSET) + 1.00002 !J=corresponding input pixel index
         IF (J.LT.1) J=1
         IF (J.GT.NSI) J=NSI
         SAMP(I) = J		    !Build sample look-up table
      ENDDO
C
C     ....Set up stretch table for byte and halfword input
      IF (LFLAG.EQ.1) THEN
         IF (ICODE.EQ.1) CALL TBLGEN(GSCALE,ILO,IHI,tbl)
         IF (ICODE.EQ.2) CALL TBLGENH(GSCALE,ILO,IHI,tblh)
      ENDIF
C
      R = 1./ZOOML			!Input line increment
      RSLI = SLI + .000002		!Float input starting-line
      OFFSET = 0.5
      IF (ZOOML.LT.1.0) OFFSET=1.
      ILINE0 = 0

      DO 100 L=1,NLO
      ILINE = R*(L-OFFSET) + RSLI	!Compute input line number
      IF (ILINE.NE.ILINE0) CALL XVREAD(IUNIT,buf,IND,'LINE',ILINE,
     &		'SAMP',SSI,'NSAMPS',NSI,' ')
      IF (ICODE.EQ.1) THEN
         CALL EXPAND(NSO,BUF,SAMP,LFLAG,TBL,obuf(sso))
      ELSE IF (ICODE.EQ.2) THEN
         CALL EXPANDH(NSO,BUF,SAMP,LFLAG,TBLH,hbuf(sso))
      ELSE IF (ICODE.EQ.3) THEN
         CALL EXPANDF(NSO,BUF,SAMP,LFLAG,GSCALE,ILO,IHI,fbuf(sso))
      ELSE
         CALL EXPANDR(NSO,BUF,SAMP,GSCALE,rbuf(sso))
      ENDIF
      CALL XVWRIT(OUNIT,OBUF,IND,' ')
  100 ILINE0 = ILINE

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Generate look-up table (TBL) for byte input.  Table causes scaling of input
C by GSCALE and truncation of output at ILO and IHI.
C
      SUBROUTINE TBLGEN(GSCALE,ILO,IHI,tbl)
      BYTE TBL(0:255)
      INCLUDE 'fortport'

      DO I=0,255			!Do DN=0 to 255
         D = GSCALE*I			!Scale DN by GSCALE
         IF (D.LT.0.) D=D-.5		!Round off to nearest integer
         IF (D.GT.0.) D=D+.5
         IDN = D
         IDN = MAX0(IDN,ILO)
         IDN = MIN0(IDN,IHI)
         TBL(I) = INT2BYTE(IDN)
      ENDDO

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Generate look-up table (TBLH) for halfword input.  Table causes scaling of
C input by GSCALE and truncation of output at ILO and IHI.
C
      SUBROUTINE TBLGENH(GSCALE,ILO,IHI,tblh)
      INTEGER*2 TBLH(-32768:32767)

      DO I=-32768,32767
         D = GSCALE*I
         IF (D.LT.0.) D=D-.5		!Round-off to nearest integer
         IF (D.GT.0.) D=D+.5
         IDN = D
         IDN = MAX0(IDN,ILO)
         IDN = MIN0(IDN,IHI)
         TBLH(I) = IDN
      ENDDO

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnify a byte image line without interpolation.
C
      SUBROUTINE EXPAND(NSO,BUF,SAMP,LFLAG,TBL,obuf)
      IMPLICIT NONE
      INTEGER*4  NSO,LFLAG,SAMP(NSO),I
      BYTE BUF(1),OBUF(NSO),TBL(0:255)
      INCLUDE 'fortport'
C
      IF (LFLAG.EQ.0) THEN
         DO I=1,NSO
            OBUF(I) = BUF(SAMP(I))
         ENDDO
      ELSE
         DO I=1,NSO
            OBUF(I) = TBL(BYTE2INT(BUF(SAMP(I))))
         ENDDO
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnify a halfword image line without interpolation.
C
      SUBROUTINE EXPANDH(NSO,BUF,SAMP,LFLAG,TBLH,hbuf)
      IMPLICIT NONE
      INTEGER*4 NSO,LFLAG,SAMP(NSO),I
      INTEGER*2 BUF(1),HBUF(NSO),TBLH(-32768:32767)

      IF (LFLAG.EQ.0) THEN
         DO I=1,NSO
            HBUF(I) = BUF(SAMP(I))
         ENDDO
      ELSE
         DO I=1,NSO
            HBUF(I) = TBLH(BUF(SAMP(I)))
         ENDDO
      ENDIF
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnify a fullword image line without interpolation.
C
      SUBROUTINE EXPANDF(NSO,BUF,SAMP,LFLAG,SCALE,ILO,IHI,fbuf)
      IMPLICIT NONE
      INTEGER*4 NSO,LFLAG,ILO,IHI
      INTEGER*4 BUF(1),FBUF(NSO),SAMP(NSO),I,DN
      REAL*4 SCALE

      IF (LFLAG.EQ.0) THEN
         DO I=1,NSO
            FBUF(I) = BUF(SAMP(I))
         ENDDO
      ELSE
         DO I=1,NSO
            DN = NINT(SCALE*BUF(SAMP(I)))
            IF (DN.GT.IHI) THEN
               DN = IHI
            ELSE IF (DN.LT.ILO) THEN
               DN = ILO
            ENDIF
            FBUF(I) = DN
         ENDDO
      ENDIF   
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Magnify a REAL*4 image line without interpolation.
C
      SUBROUTINE EXPANDR(NSO,BUF,SAMP,SCALE,rbuf)
      IMPLICIT NONE
      INTEGER*4 NSO,SAMP(NSO),I
      REAL*4 SCALE,BUF(1),RBUF(NSO)

      IF (SCALE.NE.1.0) THEN
         DO I=1,NSO
            RBUF(I) = SCALE*BUF(SAMP(I))
        ENDDO
      ELSE
         DO I=1,NSO
           RBUF(I) = BUF(SAMP(I))
         ENDDO
      ENDIF
      RETURN
      END
