      INCLUDE 'VICMAIN_FOR'            
      SUBROUTINE MAIN44
C  IBIS MOSAIC TIEPOINT PLOTTING ROUTINE   A. ZOBRIST
C		REVISED		K.F. EVANS  MARCH 1986
C   7-10-95  JCT   (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C

      IMPLICIT  INTEGER(A-Z)
      INTEGER   DUMMY, NL, NS
      INTEGER   IBIS_RD, UNIT, STATUS
      REAL      FSCALE, X,Y,DX,DY, PT
      REAL*4    ROWBUF(7)                
      INTEGER*4 FRAME(20)
      DIMENSION DCOL(7)
      CHARACTER*24 MSG
      CHARACTER*24 POSTSCRIPTFILENAME
      CHARACTER*10 YAXISTITLE
      CHARACTER*10 XAXISTITLE
      CHARACTER*24 HEADERTITLE(2)
C
      CALL IFMESSAGE('TIEPLOT version 10 July 95')
      CALL XVEACTION('SA',' ')

      YAXISTITLE = 'LINE'
      XAXISTITLE = 'SAMPLE'
      HEADERTITLE(1) = 'TIEPOINT PLOT' 
      HEADERTITLE(2) = 'FRAME  X  SCALE  XXXX'
      postscriptfilename = 'tieplot.psf'
C     Open IBIS file for 'read'
      CALL XVUNIT(UNIT, 'INP', 1, STATUS, ' ')
      CALL IBIS_FILE_OPEN (UNIT,IBIS_RD,'READ',0,0,' ',' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)
      CALL IBIS_FILE_GET(IBIS_RD,'NC',NCOL,1,1)
      CALL IBIS_FILE_GET(IBIS_RD,'NR',CLEN,1,1)

      CALL XVP ('NL', NL, DUMMY)
      CALL XVP ('NS', NS, DUMMY)
      CALL XVPARM ('KEYCOL', DCOL(1), DUMMY, KEYCDF,7)
      CALL XVP ('OLDCOLS', DCOL(4), DUMMY)
      CALL XVP ('NEWCOLS', DCOL(6), DUMMY)
      CALL XVPARM ('NUMCOL', DCOL(2), DUMMY, NUMBDF,7)
      CALL XVPARM ('CHARCOL', DCOL(3), DUMMY, CHARDF,7)
      CALL XVPARM ('ICHAR', SYMB, DUMMY, ICHADF,1)
      CALL XVP ('SCALE', FSCALE, DUMMY)
      CALL XVP ('KEY',FRAME,FCOUNT)
      IF (KEYCDF.EQ.1)  FCOUNT = 1
C

      call plotfn (postscriptfilename)
      call xrtbegin (status) 
      if (status .ne. 1) goto 9999
      CALL HEADER (HEADERTITLE, 2, 1)
      CALL AXESTITLES (XAXISTITLE, YAXISTITLE, 270, ' ', 0)
      CALL AXESREVERSE (0, 1)  ! Set X normal(0), set Y Reverse(1)

C     Open IBIS record for read
      CALL IBIS_RECORD_OPEN (IBIS_RD,IBIS_RECORD,' ',
     &                       dcol,7,'REAL',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)

      DO 100 IBIG = 1, FCOUNT
	     WRITE (msg, '(A,I4,A,F6.1)') 'FRAME',FRAME(IBIG), 
     &                    '  SCALE ',FSCALE
         HEADERTITLE(2) = MSG
         CALL HEADER (HEADERTITLE, 2, 1)
         call setactiveset (1)
         DO 20 IW = 1, CLEN
            CALL IBIS_RECORD_READ (IBIS_RECORD, ROWBUF, IW, STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(IBIS_RD,STATUS,1)
            FR = NINT(ROWBUF(1))
            IF (KEYCDF.EQ.0 .AND. FR.NE.FRAME(IBIG)) GO TO 20
            Y = (ROWBUF(4))
            X = ROWBUF(5)
            DY = -(ROWBUF(6)-ROWBUF(4)) *FSCALE
            DX = (ROWBUF(7)-ROWBUF(5)) *FSCALE
            PT = ROWBUF(2)
            IF (NUMBDF.EQ.0)  CALL NUMBER (X,Y, .10, PT, 0., -1)
            IF (CHARDF.EQ.0)  SYMB = NINT(ROWBUF(3))
            IF (CHARDF.EQ.0 .OR. ICHADF.EQ.0)
     * 			      CALL SYMBOL (X,Y, .10, ' ',SYMB, 0., -1)
            CALL PLOT (X,Y, 3)
            CALL PLOT (X+DX,Y+DY, 2)
 20	 CONTINUE
C        If loop count is complete terminate Xrt/graph else page 
         IF (IBIG .EQ. FCOUNT) THEN
            CALL PLOT (0., 0., 999)
         ELSE
            call xrtpage (status)
            if (status .ne. 1) goto 9999
         ENDIF
 100  CONTINUE
C
9999  continue
      CALL IBIS_FILE_CLOSE (IBIS_RD,'UDELETE',STATUS)
      RETURN
      END
C
C******************************************************
