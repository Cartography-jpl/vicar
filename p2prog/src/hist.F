      INCLUDE 'VICMAIN_FOR'
C Vicar program HIST - Computes and plots or prints histogram
C	HIST  INP=A  user-parameters...
C Note: PPLOT has been disabled (see AVIRIS version).  To enable PPLOT,
C enable all statements commented out (containing ###).
C
      SUBROUTINE MAIN44
      EXTERNAL WORK
      COMMON/C1/IUNIT,IFORM,SL,SS,SB,NL,NS,NB,NLI,NSI,NBI
      INTEGER*4 SL,SS,SB,NL,NS,NB,NLI,NSI,NBI

      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      COMMON/C3/IBINS,BOUNDL,BOUNDU,BINWID,ILIM
      INTEGER SINC,LINC

      COMMON/CPAR/MODE,ISPIKE,NOCUM,QEXCLUDE,QPPLOT
      LOGICAL NOCUM,QEXCLUDE,QPPLOT

      COMMON/PAUSE/QPAUSE
      LOGICAL QPAUSE

      INTEGER BATCH, XVPIXSIZEU
      REAL*4 BOUNDS(2)
      LOGICAL XVPTST
      CHARACTER*8 FMT
      CHARACTER*3 ORGIN 

C==================================================================
      CALL XVMESSAGE( 'HIST version 15-NOV-05',' ')
C     ....Open input image
      CALL XVUNIT(iunit,'INP',1,ISTAT,' ')
      CALL XVOPEN(IUNIT,ISTAT,'OPEN_ACT','SA','IO_ACT','SA',' ')

c     Check organization of image, prohibit BIP
      CALL XVGET(IUNIT,ISTAT,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')

      CALL XVSIZE(sl,ss,nl,ns,nli,nsi)
      call XVBANDS(sb,nb,nbi)

      IF ( sb .GT. nbi ) CALL MABEND(
     +  '*** SB is greater than the total number of bands')
                 
      IF ( sb + nb - 1 .GT. nbi)  THEN
         CALL XVMESSAGE('***Number of bands truncated', ' ')
         nb = nbi + 1 - sb
      ENDIF

C     ....Determine input image data format:  user specification can 
C     ....override label, but check that NS is correct
      CALL XVGET( IUNIT, ISTAT, 'FORMAT', FMT, ' ') 
      ISTAT = XVPIXSIZEU( IBP, FMT, IUNIT)  !PIXEL SIZE
      CALL XVPARM('FORMAT',FMT,ICNT,IDEF,0)
      IF (FMT.EQ.'BYTE') THEN
	IFORM=1
	IBPU = 1
      ELSEIF (FMT.EQ.'HALF' .OR. FMT.EQ.'WORD') THEN
	IFORM=2
	IBPU = 2
      ELSEIF (FMT.EQ.'FULL') THEN
	IFORM=4
	IBPU = 4
      ELSEIF (FMT.EQ.'REAL') THEN
	IFORM=7
	IBPU = 4
      ELSEIF (FMT.EQ.'DOUB') THEN
	IFORM=8
	IBPU = 8
      ELSE
	CALL XVMESSAGE('** FORMAT '//FMT//' NOT SUPPORTED **',' ')
	CALL ABEND
      ENDIF
      IF (NS*IBPU .GT. NSI*IBP) THEN
	CALL XVMESSAGE(
     1  '*** LINE LENGTH EXCEEDED, SAMPLES TRUNCATED ***',' ')
	NS = (NSI*IBP)/IBPU
      ENDIF

      CALL XVPARM('LINC',LINC,ICNT,IDEF,0)
      CALL XVPARM('SINC',SINC,ICNT,IDEF,0)
      CALL XVPARM('INC',INC,ICNT,IDEF,0)
      IF (ICNT .GT. 0) THEN
         LINC = INC
         SINC = INC
      ENDIF
C     ....Determine plot mode
      MODE = 0
      IF (XVPTST('NOHIST')) MODE=-1
      IF (XVPTST('WIDE'))   MODE=1
      IF (XVPTST('SCREEN')) MODE=2
      IF (XVPTST('SPLOT'))  MODE=3
C###      QPPLOT = (XVPTST('PPLOT'))
C###      IF (QPPLOT) THEN	
C###	CALL XVPARM('MODE',FMT,ICNT,IDEF)
C###	IF (IDEF.EQ.1) MODE=4
C###      ENDIF

      IF (MODE.EQ.1) NOCUM=XVPTST('NOCUM')
      IF (MODE.EQ.2 .AND. IFORM.NE.1) CALL MABEND(
     +	'Screen oriented histogram not supported for non-byte data')
      IF (MODE.EQ.3 .AND. BATCH().EQ.1) MODE=-1		!No SPLOT's in batch
C
      CALL XVPARM('SPIKES',ISPIKE,ICNT,IDEF,0)
      CALL XVPARM('NLINES',IBINS,ICNT,IDEF,0)

      CALL XVPARM('LIMITS',BOUNDS,ICNT,IDEF,0)
      IF (ICNT .GT. 0) THEN
         ILIM = 1
         BOUNDL = BOUNDS(1)
         BOUNDU = BOUNDS(2)
         BINWID = (BOUNDU-BOUNDL)/(IBINS-1)
         IF (IFORM.LT.7 .AND. BINWID.LT.1.0) BINWID=1.0
         ITEMP = BOUNDU-BOUNDL+1  ! NUMBER OF VALUES IN THE LIMITS.
         IF (FMT .EQ. 'BYTE' .AND. IBINS .GT. ITEMP) IBINS = ITEMP
      ELSE
         ILIM = 0
         IF (FMT .EQ. 'BYTE') THEN
            BOUNDL = 0.0     ! DEFAULTS FOR BYTE.
            BOUNDU = 255.0
            IBINS = MIN(256, IBINS)
            IF (IBINS .EQ. 256)  THEN
               BINWID = 1.0
            ELSE
               BINWID = (BOUNDU-BOUNDL)/(IBINS-1)
            END IF
         END IF
      END IF
         
      IBINS = IBINS + 2  !HIST(1), HIST(IBINS) ARE FOR OUT OF LIMITS PIXELS.

      QEXCLUDE = XVPTST('EXCLUDE')		!Exclude zeroes?
      QPAUSE = XVPTST('PAUSE').AND.BATCH().NE.1 !Interactive pauses?

      M = 8*MAX(NSI,IBINS)
      N = 8*MAX(65538,IBINS)
      CALL STACKA(4,WORK,2,M,N)   ! ALLOCATE 2 BUFFERS & CALL WORK
      RETURN
      END
C HIST main driver
C
      SUBROUTINE WORK(BUF,BUFSIZ,HIST,HSTSIZ)
      INCLUDE 'pgminc'            ! FOR XQINI...
      INTEGER HSTSIZ,BUFSIZ,HIST(4*HSTSIZ)
      BYTE BUF(BUFSIZ)
      integer IHIST(65536)
      COMMON/C1/IUNIT,IFORM,SL,SS,SB,NL,NS,NB,NLI,NSI,NBI
      INTEGER*4 SL,SS,SB

      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      COMMON/C3/IBINS,BOUNDL,BOUNDU,BINWID,ILIM
      INTEGER SINC

      COMMON/CPAR/MODE,ISPIKE,NOCUM,QEXCLUDE,QPPLOT
      LOGICAL NOCUM,QEXCLUDE,QPPLOT

      INTEGER IAREA(600),MM(2),PARB(xprdim)
      REAL*8 SUM,SUM2,RMEAN,SDEV
      REAL RMM(2)
      EQUIVALENCE (MM,RMM)
C     ....Get area specifications
      CALL XVPARM('AREA',IAREA,ICNT,IDEF,0)
      IF (icnt .eq. 0) THEN
         IAREA(1) = 1
         IAREA(2) = 1
         IAREA(3) = NL
         IAREA(4) = NS
         icnt     = 4
      ENDIF
      if ( mod(icnt,4) .ne. 0)  
     .   call mabend('Invalid count for AREA parameter')

      NAREAS = ICNT/4		!Number of areas to be plotted
      LINES = 0

      ISB = SB  !starting band
      INB = NB  !number of bands
      DO 100 I=1,NAREAS			!Loop through each area
         ISL = IAREA(1+(I-1)*4) + SL - 1 !Starting line
         ISS = IAREA(2+(I-1)*4) + SS - 1 !Starting sample
         INL = IAREA(3+(I-1)*4) !Number of lines
         INS = IAREA(4+(I-1)*4) !Number of samples
         IF (ISL+INL-1.GT.NLI) THEN
            CALL XVMESSAGE(
     * '***Specified area crosses image boundary, lines truncated',' ')
         INL = NLI - ISL + 1
         LINES = LINES + 1
      ENDIF
      IF (ISS+INS-1.GT.NSI) THEN
         CALL XVMESSAGE(
     * '**Specified area crosses image boundary, samples truncated',' ')
         INS = NSI - ISS + 1
         LINES = LINES + 1
      ENDIF
C     ....Compute the compressed histogram (HIST).  If the DN limits are
C     ....not user-specified (ILIM=0), determine BOUNDL,BOUNDU,BINWID.
      IF (IFORM.EQ.1) THEN
         CALL TAB1(IUNIT,buf,ihist,hist,mm,sum,sum2,nzeroes)
      ELSE IF (IFORM.EQ.2) THEN
         CALL TAB2(IUNIT,ILIM,buf,ihist,hist,mm,sum,sum2,nzeroes,
     &		ibins,boundl,boundu,binwid)
      ELSE IF (IFORM.EQ.4) THEN
        CALL TAB4(IUNIT,ILIM,IBINS,buf,hist,mm,sum,sum2,nzeroes,
     &		boundl,boundu,binwid)
      ELSE IF (IFORM.EQ.7) THEN
        CALL TAB7(IUNIT,ILIM,IBINS,buf,hist,rmm,sum,sum2,nzeroes,
     &		boundl,boundu,binwid)
      ELSE
        CALL TAB8(IUNIT,ILIM,IBINS,buf,hist,rmm,sum,sum2,nzeroes,
     &		boundl,boundu,binwid)
      ENDIF
C     ....Compute mean and standard deviation of area
      NPIXELS = (1+(INL-1)/LINC)*(1+(INS-1)/SINC)*(INB)
      IF (QEXCLUDE) NPIXELS=NPIXELS-NZEROES
      IF (NPIXELS.NE.0.) THEN
         RMEAN = SUM/FLOAT(NPIXELS)
         SDEV = FLOAT(NPIXELS)*SUM2-SUM*SUM
         IF (SDEV.GT.0.) SDEV=DSQRT(SDEV)/FLOAT(NPIXELS)
         IF (SDEV.LE.0.) SDEV= 0.0
      ELSE
         RMEAN = 0.0
         SDEV = 0.0
      ENDIF
C     ....Print the histogram
      CALL XVMESSAGE(' ',' ')
      IF (MODE.LE.1) THEN				! normal, wide,
         CALL PHIST(HIST,RMEAN,SDEV,NPIXELS,MM,RMM,LINES) ! or nohist
         CALL XVMESSAGE(' ',' ')
      ELSE IF (MODE.EQ.2) THEN				! screen
         CALL SHIST(IHIST,RMEAN,SDEV,NPIXELS,MM,
     &			BOUNDL,BOUNDU,BINWID,ISPIKE)
      ENDIF
      IF (MODE.EQ.3 .OR. QPPLOT) THEN			! splot and/or
         IF (IFORM.LT.7) THEN				! pplot
            RMM(1) = MM(1)
            RMM(2) = MM(2)
         ENDIF
         CALL PLOTXY(HIST,HIST,BUF,RMEAN,SDEV,NPIXELS,RMM,LINES)
      ENDIF
  100 CONTINUE
C     ....Output MEAN and SIGMA parameters
      CALL XQINI(PARB, xprdim, xabort)
      CALL XQREAL(PARB,'MEAN',1,sngl(RMEAN),xadd,ISTAT)      
      CALL XQREAL(PARB,'SIGMA',1,sngl(SDEV),xadd,ISTAT)      
      CALL XQOUT(PARB,ISTAT)
      RETURN
      END
C Compute histogram of input image (IHIST) and compress it (HIST).
C
      SUBROUTINE TAB1(IUNIT,buf,ihist,hist,mm,sum,sum2,nzeroes)
      BYTE BUF(*)
      INTEGER*4 IHIST(0:255),HIST(258),MM(2)
      REAL*8 SUM,SUM2
      INTEGER*4 BAND
      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      COMMON/C3/IBINS,BOUNDL,BOUNDU,BINWID,dummy
      INTEGER SINC
      REAL*8 RDN
C
      NZEROES = 0
      CALL ZIA(IHIST,256)
      IEL = ISL + INL - 1	!ending line
      
      NX = INS
      IF (SINC.GT.1) NX = 1+(INS-1)/SINC
C     ....Compute 256 grey-level histogram
      DO BAND= ISB, ISB+INB-1
       DO LINE=ISL,IEL,LINC
	   CALL XVREAD(IUNIT,BUF,ISTAT,'LINE',LINE,'BAND',BAND,' ')
	   IF (SINC.GT.1) CALL MVE(1,NX,BUF(ISS),BUF(ISS),SINC,1)
	      CALL HSUB(1,NX,BUF(ISS),ihist,0,255)
        ENDDO
      ENDDO

      MINDN = 255
      MAXDN = 0
      SUM = 0.0
      SUM2 = 0.0
      CALL ZIA(HIST,IBINS)
C     ....Compress the histogram into IBINS grey-levels
      DO IDN=0,255
         RDN = IDN
         IFREQ = IHIST(IDN)
         SUM = SUM + IFREQ*RDN
         SUM2 = SUM2 + IFREQ*RDN**2
         IF (IFREQ.GT.0) THEN
            MAXDN = IDN			!Maximum DN value
            MINDN = MIN0(MINDN,IDN)	!Minimum DN value
         ENDIF
         K = MAX((IDN-BOUNDL)/BINWID+2.5,1.5)
         K = MIN(K,IBINS)
         HIST(K) = HIST(K) + IFREQ
      ENDDO

      MM(1) = MINDN
      MM(2) = MAXDN
      NZEROES = IHIST(0)
      RETURN
      END
C Compute histogram for halfword data.
C
      SUBROUTINE TAB2(IUNIT,ILIM,buf,ihist,hist,mm,sum,sum2,nzeroes,
     &		ibins,boundl,boundu,binwid)
      INTEGER*2 BUF(*)
      INTEGER IHIST(-32768:32767),HIST(*),MM(2)
      REAL*8 SUM,SUM2
      INTEGER*4 BAND
      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      INTEGER SINC
      REAL*8 RDN

      CALL ZIA(IHIST,65536)
      IEL = ISL + INL - 1		!Ending line
      IES = ISS + INS - 1		!Ending sample
C     ....Compute 64K grey-level histogram
      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
	    CALL XVREAD(IUNIT,BUF,ISTAT,'LINE',LINE,'BAND',BAND,' ')
         IF (LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            MINDN= BUF(ISS)     ! INIT MIN AND MAX
            MAXDN= BUF(ISS)
         END IF

         DO I=ISS,IES,SINC
            IDN = BUF(I)      ! FIND MIN AND MAX
            IF (IDN.LT.MINDN) THEN
               MINDN = IDN
            ELSE IF (IDN.GT.MAXDN) THEN
               MAXDN = IDN
            ENDIF
            IHIST(IDN) = IHIST(IDN) + 1
         ENDDO
       ENDDO
      ENDDO
C     ....Determine optimum compression parameters (if not user-specified)
      IF (ILIM.EQ.0) THEN
         CALL PINC(IHIST,IBINS,mindn,maxdn,ibinwid)
         BOUNDL = MINDN
         BOUNDU = MAXDN
         IF (MINDN.EQ.MAXDN) BOUNDU=BOUNDU+1.
         BINWID = IBINWID
         IBINS = (MAXDN-MINDN+IBINWID-1)/IBINWID + 3 
      ENDIF  ! ALLOW SPACE, SINCE HIST(1), HIST(IBINS) ARE FOR OUT OF LIMITS
             ! PIXELS

      CALL ZIA(HIST,IBINS)
      NZEROES = IHIST(0)
      SUM = 0.0
      SUM2 = 0.0
C     ....Compress the histogram
      DO IDN=-32768,32767
         RDN = IDN
         IFREQ = IHIST(IDN)
         SUM = SUM + IFREQ*RDN
         SUM2 = SUM2 + IFREQ*RDN**2
         K = MAX1((IDN-BOUNDL)/BINWID+2.5,1.5)
         K = MIN(K,IBINS)
         HIST(K) = HIST(K) + IFREQ
      ENDDO

      MM(1) = MINDN
      MM(2) = MAXDN
      RETURN
      END
C Compute histogram for FULL data.
C
      SUBROUTINE TAB4(IUNIT,ILIM,IBINS,buf,hist,mm,sum,sum2,
     &		nzeroes,boundl,boundu,binwid)
      INTEGER*4 BUF(*)
      INTEGER   HIST(*),MM(2)
      REAL*8 SUM,SUM2
      INTEGER*4 BAND
      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      INTEGER SINC,LINC,ISB,INB,ISL,ISS,INL,INS
C==================================================================

      IEL = ISL + INL - 1		!Ending line
      IES = ISS + INS - 1		!Ending sample
      IF (ILIM.EQ.1) GOTO 50	!Skip if limits were user-specified
C     ....Determine limits for DN-range
      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
         CALL XVREAD(IUNIT,BUF,ISTAT,'LINE',LINE,'BAND',BAND,' ')
         IF (LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            MINDN= BUF(ISS)     ! INIT MIN AND MAX
            MAXDN= BUF(ISS)
         END IF
         DO I=ISS,IES,SINC
            IDN = BUF(I)      ! FIND MIN AND MAX
            IF (IDN.LT.MINDN) THEN
               MINDN = IDN
            ELSE IF (IDN.GT.MAXDN) THEN
               MAXDN = IDN
            ENDIF
         ENDDO
       ENDDO
      ENDDO

      BOUNDL = MINDN
      BOUNDU = MAXDN
      BINWID = (BOUNDU-BOUNDL)/(IBINS-3)
      IF(MINDN .EQ. MAXDN) BINWID = 1.0 ! AVOID DIVISION BY ZERO.

   50 CALL ZIA(HIST,IBINS)
      SUM = 0.0
      SUM2 = 0.0
      NZEROES = 0

      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
         CALL XVREAD(IUNIT,BUF,ISTAT,'LINE',LINE,'BAND',BAND,' ')
         IF (ILIM.EQ.1 .AND. LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            MINDN= BUF(ISS)     ! INIT MIN AND MAX
            MAXDN= BUF(ISS)
         END IF
         DO I=ISS,IES,SINC
            IDN = BUF(I)
            IF (ILIM.EQ.1) THEN
               IF (IDN.LT.MINDN) THEN
                  MINDN = IDN
               ELSE IF (IDN.GT.MAXDN) THEN
                  MAXDN = IDN
               ENDIF
            ENDIF
            IF (IDN.EQ.0) NZEROES=NZEROES+1
            SUM = SUM + IDN
            SUM2 = SUM2 + FLOAT(IDN)**2
            K = MAX((IDN-BOUNDL)/BINWID+2.5,1.5)
            K = MIN(K,IBINS)
            HIST(K) = HIST(K) + 1
         ENDDO
       ENDDO
      ENDDO
      MM(1) = MINDN
      MM(2) = MAXDN
      RETURN
      END
C Compute histogram for REAL*4 data
C
      SUBROUTINE TAB7(IUNIT,ILIM,IBINS,BUF,hist,rmm,sum,sum2,
     &		nzeroes,boundl,boundu,binwid)
      REAL RMM(2),BUF(*)
      REAL*8 SUM,SUM2
      INTEGER HIST(*)
      INTEGER*4 BAND
      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      INTEGER SINC, LINE, ISB, INB

      IEL = ISL + INL - 1		!Ending line
      IES = ISS + INS - 1		!Ending sample
      IF (ILIM.EQ.1) GOTO 50	!Skip if limits are user-specified
C     ....Determine limits for DN-range
      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
         CALL XVREAD(IUNIT,BUF,ISTAT,'LINE',LINE,'BAND',BAND,' ')
         IF (LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            DNMIN= BUF(ISS)     ! INIT MIN AND MAX
            DNMAX= BUF(ISS)
         END IF
         DO I=ISS,IES,SINC
            DN = BUF(I)
            IF (DN.LT.DNMIN) THEN
               DNMIN = DN
            ELSE IF (DN.GT.DNMAX) THEN
               DNMAX = DN
            ENDIF
         ENDDO
       ENDDO
      ENDDO

      BOUNDL = DNMIN
      BOUNDU = DNMAX
      BINWID = (BOUNDU-BOUNDL)/(IBINS-3)

   50 CALL ZIA(HIST,IBINS)
      SUM = 0.0
      SUM2 = 0.0
      NZEROES = 0

      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
         CALL XVREAD(IUNIT,BUF,ISTAT,'LINE',LINE,'BAND',BAND,' ')
         IF (ILIM.EQ.1 .AND. LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            DNMIN= BUF(ISS)     ! INIT MIN AND MAX
            DNMAX= BUF(ISS)
         END IF
         DO I=ISS,IES,SINC
            DN = BUF(I)
            IF (ILIM.EQ.1) THEN
               IF (DN.LT.DNMIN) THEN
                  DNMIN = DN
               ELSE IF (DN.GT.DNMAX) THEN
                  DNMAX = DN
               ENDIF
            ENDIF
            IF (DN.EQ.0.0) NZEROES=NZEROES+1
            SUM = SUM + DN
            SUM2 = SUM2 + DN*DN    ! IF THIS OVERFLOWS OR UNDERFLOWS
                     ! IT CAN BE REPLACED WITH METHOD IN MATH77 SSTAT1
            K = MAX((DN-BOUNDL)/BINWID+2.5,1.5)
            K = MIN(K,IBINS)
            HIST(K) = HIST(K) + 1
         ENDDO
       ENDDO
      ENDDO
      RMM(1) = DNMIN
      RMM(2) = DNMAX
      RETURN
      END
C Compute histogram for REAL*8 data
C
      SUBROUTINE TAB8(IUNIT,ILIM,IBINS,BUF,hist,rmm,sum,sum2,
     &		nzeroes,boundl,boundu,binwid)
      REAL RMM(2)
      REAL*8 SUM,SUM2, BUF(*)
      INTEGER HIST(*)
      INTEGER*4 BAND
      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      INTEGER SINC

      IEL = ISL + INL - 1		!Ending line
      IES = ISS + INS - 1		!Ending sample
      IF (ILIM.EQ.1) GOTO 50	!Skip if limits are user-specified
C     ....Determine limits for DN-range
      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
         CALL XVREAD(IUNIT,BUF,ISTAT,'LINE',LINE,'BAND',BAND,' ')
         IF (LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            DNMIN= BUF(ISS)     ! INIT MIN AND MAX
            DNMAX= BUF(ISS)
         END IF
         DO I=ISS,IES,SINC
            DN = BUF(I)
            IF (DN.LT.DNMIN) THEN
               DNMIN = DN
            ELSE IF (DN.GT.DNMAX) THEN
               DNMAX = DN
            ENDIF
         ENDDO
       ENDDO
      enddo

      BOUNDL = DNMIN
      BOUNDU = DNMAX
      BINWID = (BOUNDU-BOUNDL)/(IBINS-3)

   50 CALL ZIA(HIST,IBINS)
      SUM = 0.0
      SUM2 = 0.0
      NZEROES = 0

      DO BAND=ISB,ISB+INB-1
       DO LINE=ISL,IEL,LINC
         CALL XVREAD(IUNIT,BUF,ISTAT,'LINE',LINE,'BAND',BAND,' ')
         IF (ILIM.EQ.1 .AND. LINE.EQ.ISL .and. BAND.eq.ISB)  THEN
            DNMIN= BUF(ISS)     ! INIT MIN AND MAX
            DNMAX= BUF(ISS)
         END IF
         DO I=ISS,IES,SINC
            DN = BUF(I)
            IF (ILIM.EQ.1) THEN
               IF (DN.LT.DNMIN) THEN
                  DNMIN = DN
               ELSE IF (DN.GT.DNMAX) THEN
                  DNMAX = DN
               ENDIF
            ENDIF
            IF (DN.EQ.0.0) NZEROES=NZEROES+1
            SUM = SUM + DN
            SUM2 = SUM2 + DN*DN
            K = MAX((DN-BOUNDL)/BINWID+2.5,1.5)
            K = MIN(K,IBINS)
            HIST(K) = HIST(K) + 1
         ENDDO
       ENDDO
      enddo
      RMM(1) = DNMIN
      RMM(2) = DNMAX
      RETURN
      END

C Find a suitable DN increment for printing out a halfword histogram.
C  (FIND MINDN, MAXDN, AND INC)

      SUBROUTINE PINC(HIST,NLIN,mindn,maxdn,inc)
      INTEGER*4 HIST(-32768:32767)
      N1 = 0
      N2 = 0
      N4 = 0
      N8 = 0
      N16 = 0
      N32 = 0
      N64 = 0
      N128 = 0
      N256 = 0
      MINDN = 32767
      MAXDN = -32768
      I = -32769
C
      DO I512=1,65536,256
	M128 = N128
	DO I256=1,2
          M64 = N64
          DO I128=1,2  
            M32 = N32
            DO I64=1,2   
              M16 = N16
              DO I32=1,2  
                M8 = N8
                DO I16=1,2
                  M4 = N4
                  DO I8=1,2 
                    M2 = N2
                    DO I4=1,2  
                      M1 = N1
                      DO I2=1,2 
                        I = I + 1
                        IF (HIST(I).GT.0) THEN
                           MAXDN = I
                           MINDN = MIN0(MINDN,I)
                           N1=N1+1    
                        ENDIF
                      ENDDO                          
                      IF (M1.LT.N1) N2=N2+1
                    ENDDO                            
                    IF (M2.LT.N2) N4=N4+1
                  ENDDO                              
                  IF (M4.LT.N4) N8=N8+1
                ENDDO                                
                IF (M8.LT.N8) N16=N16+1
              ENDDO                                   
              IF (M16.LT.N16) N32=N32+1
            ENDDO     
            IF (M32.LT.N32) N64=N64+1
          ENDDO         
          IF (M64.LT.N64) N128=N128+1
        ENDDO 
        IF (M128.LT.N128) N256=N256+1
      ENDDO

      INC = 1
      IF (N1.LE.NLIN.AND.N1.GT.N2) RETURN
      INC = 2
      IF (N2.LE.NLIN.AND.N2.GT.N4) RETURN
      INC = 4
      IF (N4.LE.NLIN.AND.N4.GT.N8) RETURN
      INC = 8
      IF (N8.LE.NLIN.AND.N8.GT.N16) RETURN
      INC = 16
      IF (N16.LE.NLIN.AND.N16.GT.N32) RETURN
      INC = 32
      IF (N32.LE.NLIN.AND.N32.GT.N64) RETURN
      INC = 64
      IF (N64.LE.NLIN.AND.N64.GT.N128) RETURN
      INC = 128
      IF (N128.LE.NLIN.AND.N128.GT.N256)RETURN
      INC = 256
      IF (N256.GT.NLIN) INC=512
      RETURN
      END
C Output histogram in normal or wide format
C (MODE=0 or 1).
C
      SUBROUTINE PHIST(HIST,RMEAN,SDEV,NPIXELS,MM,RMM,LINES)
      INTEGER HIST(*),MM(2)
      REAL RMM(2)
      REAL*8 RMEAN,SDEV

      COMMON/C1/IUNIT,IFORM,dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9

      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      COMMON/C3/IBINS,BOUNDL,BOUNDU,BINWID,dummy
      INTEGER SINC

      COMMON/CPAR/MODE,ISPIKE,NOCUM,QEXCLUDE,QPPLOT
      LOGICAL NOCUM,QEXCLUDE,QPPLOT

      INTEGER SPIKES(9)
      REAL*8 GRAYLEVEL
      CHARACTER*80  RUNTIME
      CHARACTER*132 PBUF
      DATA PBUF/' '/
      CHARACTER*132 WIDELI
      DATA WIDELI/' '/
      CHARACTER*132 MSG
      CHARACTER*132 MSG2
      DATA MSG2/'MIN. DN=              MAX. DN=             '/
      CHARACTER*6 COL3,PCTILE,CDF
      DATA        PCTILE/'PCTILE'/,CDF/'   CDF'/

      IF (MODE.LT.0) GOTO 1000
C     ....Find spike locations
      CALL ZIA(SPIKES,9)
      DO J=1,ISPIKE
         JSAV = J
         MAX=0
         DO I=1,IBINS
            IF (HIST(I).GT.MAX) IMAX=I
            IF (HIST(I).GT.MAX) MAX=HIST(I)
         ENDDO
         IF (MAX.EQ.0) GOTO 6
         SPIKES(J) = HIST(IMAX)
         HIST(IMAX) = -J		!Flag spikes as negative
      ENDDO
    6 IF (MAX.EQ.0 .AND. JSAV.NE.1) MAX=SPIKES(JSAV-1)
      GRAYLEVEL = BOUNDL - BINWID	!Compute DN of HIST(1)
C     ....Compute number of decimal places needed to represent the gray level
C     ....value (NDEC). If fixed point representation won't work, NDEC is
C     ....negative.
      NLEFT = ALOG10(AMAX1(ABS(BOUNDL),ABS(BOUNDU))) + 1.0
      IF (IFORM .GE. 7)  THEN   ! floating point data could be out of INT range.
	  NDEC = 1.5-ALOG10(BINWID)

      ELSE IF (GRAYLEVEL.EQ.INT(GRAYLEVEL) .AND. 
     .         BINWID.EQ.INT(BINWID)) THEN
	  NDEC = 0
      ELSE
	  NDEC = 1.5-ALOG10(BINWID)
      ENDIF
      IF (NLEFT .GT. 7 .AND. IFORM .GE. 7) THEN  !FOR REALS, USE EXP. NOTATION
          NDEC=-1                              !IF MORE THAN 7 DIGITS.
      ELSE IF (NDEC.LT.0 .AND. NLEFT.LE.11) THEN
	  NDEC=0
      ELSE IF (NDEC.GT. 10 .OR. NLEFT.GT.10) THEN
	  NDEC=-1
      ELSE IF (NDEC+NLEFT.GT.10) THEN
	  NDEC=-1
      ENDIF
C     ....Create/print format headers
      PT = 0.0
      IF (MODE.EQ.1) THEN	!bar-graph line with + marking tens columns
         DO I=30,130,10
            WIDELI(I:I) = '+'
         ENDDO
C			     	!print header lines
         WRITE (MSG,100) ISL,ISS,INL,INS,LINC,SINC
  100    FORMAT('FREQUENCY DISTRIBUTION     SL=',I5,'     SS=',I5,
     *           '     NL=',I5,'     NS=',I5,'     LINC=',I3,
     *		 '    SINC=',I3)
         CALL QPRNT(MSG(1:100),LINES)
         CALL QPRNT(' ',LINES)
C
         IF (NOCUM) THEN
            COL3 = PCTILE
         ELSE
            COL3 = CDF
         ENDIF

         WRITE (MSG,200) COL3,10,20,30,40,50,60,70,80,90,100
  200    FORMAT('       GRAY    FREQ  ',A6,'   ',10I10,' ')
         CALL QPRNT(MSG,LINES)
         CALL QPRNT(WIDELI,LINES)

         DO I=1,IBINS
            IF (HIST(I).EQ.0) THEN   ! ZERO ENTRIES ARE SKIPPED.
               IF (I.NE.1.AND.HIST(I-1).NE.0) CALL QPRNT(' ',LINES)
            ELSE
               PBUF = WIDELI
               IF (I.EQ.1) THEN
                  PBUF(1:11) = '< LOW LIMIT'
               ELSE IF (I.EQ.IBINS) THEN
                  PBUF(1:11) = '>HIGH LIMIT'
               ELSE
                  IF ( NDEC .EQ. 0) THEN
                     WRITE (PBUF(1:11),'(I11)') NINT(GRAYLEVEL)
                  ELSE IF (NDEC .LT. 0) THEN
                     if (graylevel .lt. 0.D0) then
                        WRITE (PBUF(1:11),'(1PE11.4)') GRAYLEVEL
                     else
                        WRITE (PBUF(1:11),'(1PE11.5)') GRAYLEVEL
                     endif
                  ELSE IF (NDEC .GT. 0) THEN
                     NDEC = MIN(10,NDEC)
                     WRITE (RUNTIME,130) NDEC
130                     FORMAT( '(F11.', I2.2, ')' )
                     WRITE (PBUF(1:11), RUNTIME) GRAYLEVEL
                  ENDIF
               ENDIF
C              ....if one of the n=spike largest, label on graph
               IF (HIST(I) .LT. 0) THEN
                  J = -HIST(I)
                  HIST(I) = SPIKES(J)
                  WRITE (PBUF(131:131),'(I1)') J
               ENDIF
C	       ....update number of pixels seen
               WRITE (PBUF(12:19),'(I8)') HIST(I)
               IF (NOCUM) THEN
                  WRITE (PBUF(20:27),
     +             '(F8.3)') 100.0*HIST(I)/FLOAT(NPIXELS)
               ELSE
                  PT = PT+HIST(I)
                  WRITE (PBUF(20:27),'(F8.3)') 100.0*PT/FLOAT(NPIXELS)
               ENDIF
C		       draw bar of chart
               J = (MIN(HIST(I),MAX)*100)/MAX
               IF (J.NE.0) THEN
                  DO K=1,J
                     PBUF(30+K-1:30+K-1) = '*'
                  END DO
               END IF
               CALL QPRNT(PBUF,LINES)
            ENDIF
            GRAYLEVEL = GRAYLEVEL + BINWID
         ENDDO
      ELSE
         DO I=1,IBINS
            IF (HIST(I).NE.0) THEN
               PBUF(1:76) = ' '
               IF (I.EQ.1) THEN
                  PBUF(1:11) = '< LOW LIMIT'
               ELSE IF (I.EQ.IBINS) THEN
                  PBUF(1:11) = '>HIGH LIMIT'
               ELSE
                  IF ( NDEC .EQ. 0) THEN
                     WRITE (PBUF(1:11),'(I11)') NINT(GRAYLEVEL)
                  ELSE IF (NDEC .LT. 0) THEN
                     if (graylevel .lt. 0.D0) then
                        WRITE (PBUF(1:11),'(1PE11.4)') GRAYLEVEL
                     else
                        WRITE (PBUF(1:11),'(1PE11.5)') GRAYLEVEL
                     endif
                  ELSE IF (NDEC .GT. 0) THEN
                     NDEC = MIN(10,NDEC)
                     WRITE (RUNTIME,130) NDEC
                     WRITE (PBUF(1:11), RUNTIME) GRAYLEVEL
                  ENDIF
               ENDIF
C              ....    if one of the n=spike largest, label on graph

               IF (HIST(I) .LT. 0) THEN
                  J = -HIST(I)
                  HIST(I) = SPIKES(J)
                  WRITE (PBUF(76:76),'(I1)') J
               ENDIF
C	       ....update number of pixels seen
               WRITE (PBUF(12:19),'(I8)') HIST(I)
               IF (I.GT.2.AND.HIST(I-1).EQ.0) PBUF(12:12) = '*'
C	       ....draw bar of chart
               J = (MIN(HIST(I),MAX)*50)/MAX
               IF (J.NE.0) THEN
                   DO IUNCON = 24,24+J-1
                       PBUF(IUNCON:IUNCON) = '*'
                   ENDDO
               ENDIF
               CALL QPRNT(PBUF(1:76),LINES)
            ENDIF
            GRAYLEVEL = GRAYLEVEL + BINWID
         ENDDO
      ENDIF
C     ....print statistics for graph data and return
1000  MSG(1:44) = 'AVERAGE GRAY LEVEL=XXXXXXXX       STANDARD D'
      MSG(45:89) = 'EVIATION=XXXXXXXX       NUMBER ELEMENTS=XXXXX'
      MSG(90:93) = 'XXX '
      CALL REALCON( sngl(RMEAN), MSG(20:), 8)
      CALL REALCON( sngl(SDEV), MSG(54:), 8)
      WRITE (MSG(85:92),'(I8)') NPIXELS !Number of pixels used
      CALL QPRNT(' ',LINES)
      IF (QEXCLUDE) CALL XVMESSAGE('EXCLUDING PIXELS OF DN=0',' ')
      IF (MODE.EQ.0) THEN
         CALL QPRNT(MSG(1:34),LINES)
         CALL QPRNT(MSG(35:68),LINES)
         CALL QPRNT(MSG(69:93),LINES)
      ELSE
         CALL QPRNT(MSG(1:93),LINES)
      ENDIF
C     ....Report minimum and maximum DN
      IF (IFORM.LT.7) THEN	
         WRITE (MSG2(9:18),'(I10)') MM(1)
         WRITE (MSG2(31:40),'(I10)') MM(2)
      ELSE
         CALL REALCON( RMM(1), MSG2(9:),8)
         CALL REALCON( RMM(2), MSG2(31:),8)
      ENDIF
      IF (MODE.EQ.1) THEN
         CALL QPRNT(MSG2(1:42),LINES)
      ELSE
         CALL QPRNT(MSG2(1:20),LINES)
         CALL QPRNT(MSG2(23:42),LINES)
      ENDIF
      RETURN
      END
C Plot histogram on either:
C   1) VT240 compatible terminal using REGIS graphics (MODE=3)
C   2) HP plotter (QPPLOT)
C
      SUBROUTINE PLOTXY(HIST,RHIST,BUF,RMEAN,SDEV,NPIXELS,RMM,LINES)
      INTEGER HIST(*)
      REAL RHIST(*),BUF(*),RMM(2)
      REAL*8 RMEAN,SDEV

      COMMON/C1/IUNIT,IFORM,dum1,dum2,dum3,dum4,dum5,dum6,dum7,dum8,dum9

      COMMON/C2/ISB,INB,ISL,ISS,INL,INS,LINC,SINC
      COMMON/C3/IBINS,BOUNDL,BOUNDU,BINWID,dummy
      INTEGER SINC

      COMMON/CPAR/MODE,ISPIKE,NOCUM,QEXCLUDE,QPPLOT
      LOGICAL NOCUM,QEXCLUDE,QPPLOT

      CHARACTER*132 MSG
      INTEGER SPIKES(9)
C      ....Scale x-axis
      X1 = AMAX1(BOUNDL,RMM(1))			! start of hist
      X2 = AMIN1(BOUNDU,RMM(2))			! end of hist
      IF (X1.EQ.X2) GO TO 300
      DIV = 10.0**INT(ALOG10(X2-X1))		! span between tic-marks
      XLO = DIV*INT(X1/DIV)			! lower limit of graph
      NXTIC = 1+(X2-XLO)/DIV			! # of tic-marks
      XHI = XLO+NXTIC*DIV			! upper limit of graph

      IF (NXTIC.LE.3) THEN			! adjust # of tic-marks
         NXTIC = 5*NXTIC
         DIV = DIV/5.0
      ENDIF
      IF (NXTIC.LE.7) THEN
         NXTIC = 2*NXTIC
         DIV = DIV/2.0
      ENDIF
      DO WHILE (X1 .GE. XLO+DIV)
         XLO = XLO+DIV
         NXTIC = NXTIC-1
      ENDDO
      DO WHILE (X2 .LE. XHI-DIV)
         XHI = XHI-DIV
         NXTIC = NXTIC-1
      ENDDO
C     ....Scale y-axis
      NSPIKES = MAX(ISPIKE,2)
      CALL ZIA(SPIKES,NSPIKES)
      DO I=2,IBINS-1
         J = NSPIKES
         DO WHILE (HIST(I).GT.SPIKES(J) .AND. J.GT.0)
            J = J-1
         ENDDO
         IF (J.NE.NSPIKES) THEN
            K = NSPIKES
            DO L=J+2,NSPIKES
               SPIKES(K) = SPIKES(K-1)
               K = K-1
            ENDDO
            SPIKES(J+1) = HIST(I)
         ENDIF
      ENDDO
      IF (SPIKES(2).EQ.0) CALL XVMESSAGE(
     +                 '***Only 1 bin occupied, no plotting done.',' ')
      IF (SPIKES(2).EQ.0) GO TO 300
      TOP = SPIKES(ISPIKE)
      YHI = 10**INT(1.0+ALOG10(TOP))
      IF (YHI/5.0 .GE. TOP) YHI = YHI/5.0
      IF (YHI/2.0 .GE. TOP) YHI = YHI/2.0
      DIV = YHI/10.0
      NYTIC = 10
      DO WHILE (TOP .LE. YHI-DIV)
         YHI = YHI-DIV
         NYTIC = NYTIC-1
      ENDDO
      DO I=2,IBINS-1			! truncate spikes
         IF (HIST(I).GT.INT(YHI)) HIST(I)=YHI
      ENDDO
C     ....Compute x values
      X = BOUNDL
      IBINS=IBINS-2
      DO I=1,IBINS
         RHIST(I) = HIST(I)
         BUF(I) = X
         X = X+BINWID
      ENDDO
      RHIST(IBINS+1) = HIST(IBINS+1)
C     ....Plot the data
      IF (MODE.EQ.3) CALL SPLOT(BUF,RHIST(2),IBINS,XLO,XHI,
     &		0.0,YHI,NXTIC,NYTIC,0,0)
C     ....Print statistics for graph data
  300 WRITE (MSG,500) NPIXELS,RMM(1),RMM(2),RMEAN,SDEV
  500 FORMAT('_',I9,'_PIXELS___RANGE_',F12.1,'_TO_',F12.1,'___MEAN_',
     *		F12.3,'___STD_DEV_',F12.3)
      I = 97
      CALL SQUEEZE(MSG,I)
      IF (MODE.EQ.3 .OR. MODE.EQ.4) CALL XVMESSAGE(MSG(1:I),' ')
C
C###	IF (MODE.EQ.3) THEN
C###	    CALL XVINTRACT('IPARAM','PEN PLOT? ''YES or Cr')
C###	    CALL XVIPARM('HPPLOT',MSG(101),ICNT,IDEF)
C###	    QPPLOT = MSG(101).EQ.'Y' .OR. MSG(101).EQ.'y'
C###	ENDIF
C								submit to the
C								HP plotter
C###	IF (QPPLOT) THEN
C###	    CALL HPPLOT(BUF,RHIST(2),IBINS,XLO,XHI,0.0,YHI,
C###  +			NXTIC,NYTIC,0,0,4,7)			! print the line
C###	    CALL HPLABEL(6700,6500-50*I,MSG)			! of stats
C###	    CALL XVGET(IUNIT,ISTAT,'NAME',MSG,' ')
C###	    N = 1
C###	    DO WHILE (MSG(N).NE.0 .AND. MSG(N).NE.' ')
C###		N = N+1
C###	    ENDDO
C###	    ENCODE (58,700,MSG(N)) ISL,ISS,INL,INS,LINC,SINC
C###  700	    FORMAT('___(', I5, ',', I5, ',', I5, ',', I5,
C###     +		   ')____LINC_=_', I4, '____SINC_=_', I4)
C###	    I = N+57
C###	    CALL SQUEEZE(MSG,I)
C###	    CALL HPLABEL(7000,6500-50*I,MSG)			! print params
C###	    CALL XVPARM('TITLE',MSG,ICNT,IDEF)
C###	    IF (IDEF.EQ.0) THEN					! print title
C###		I = 1
C###		DO WHILE (MSG(I).NE.0)
C###		    I = I+1
C###		ENDDO
C###		CALL HPLABEL(6400,6500-50*I,MSG)
C###	    ENDIF
C###	    CALL HPEND(7000)
C###	ENDIF
      RETURN
      END
C Plot histogram onto standard 23x80 terminal screen with horizontal
C DN-axis.
C
      SUBROUTINE SHIST(IHIST,RMEAN,SDEV,NPIXELS,MM,
     &		BOUNDL,BOUNDU,BINWID,ISPIKE)
      INTEGER*4 IHIST(256),MM(2)
      REAL*8 RMEAN,SDEV

      INTEGER SPIKES(9),HIST(80)
      CHARACTER*132 MSG
      CHARACTER*80 SCREEN(22)  ! THE TOP 22 LINES OF SCREEN
C==================================================================

      IF (BOUNDL.EQ.0.0 .AND. BOUNDU.EQ.255.0) THEN
         LOW = MM(1)
         IHI = MM(2)
      ELSE
         LOW = BOUNDL
         IHI = BOUNDU
      ENDIF
      IBINWIDTH = 1 + (IHI-LOW)/80
      NBINS = 1 + (IHI-LOW)/IBINWIDTH
      CALL ZIA(HIST,80)
      N = LOW
C     ....Compress the histogram to 80 grey-levels
      DO I=1,NBINS
         DO J=1,IBINWIDTH
            N = N+1
            HIST(I) = HIST(I)+IHIST(N)
         ENDDO
      ENDDO
C     ....Find spike locations
      CALL ZIA(SPIKES,9)
      DO J=1,ISPIKE
         JSAV = J
         MAX = 0
         DO I=1,NBINS
            IF (HIST(I) .GT. MAX) THEN
               IMAX = I
               MAX = HIST(I)
            ENDIF
         ENDDO
         IF(MAX.EQ.0) GOTO 6
         SPIKES(J) = HIST(IMAX)
         HIST(IMAX) = -J
        ENDDO
    6 IF (MAX.EQ.0 .AND. JSAV.NE.1) MAX=SPIKES(JSAV-1)
      DIVISOR = MAX/19.0
      NUM = LOW+IBINWIDTH/2
      DO I = 1,22
         SCREEN(I) = ' '  ! BLANK EACH LINE.  FORTRAN PADS WITH BLANKS.
      END DO
C
C     ....Plot the histogram on the screen
      DO I=1,NBINS
         IF (MOD(I,5).EQ.1) THEN
            WRITE (MSG(1:3),'(I3)') NUM
            DO J=1,3      ! LABEL AXIS EVERY FIFTH BIN IN LINES 20-22.
               SCREEN(J+19)(I:I) = MSG(J:J)
            ENDDO
            NUM = NUM+5*IBINWIDTH
         ENDIF
         IF (HIST(I).NE.0) THEN
            IF (HIST(I) .LT. 0) THEN		!Flag spike
               WRITE (SCREEN(1)(I:I), '(I1)') -HIST(I)
               N = 2
            ELSE
               N = 20.5-HIST(I)/DIVISOR
            ENDIF
            DO K=N,19
               SCREEN(K)(I:I) = '*'
            ENDDO
         ENDIF
      ENDDO
C     ....Print the screen
      DO I=1,22
         CALL XVMESSAGE(SCREEN(I),' ')
      ENDDO
C     ....Print statistics for graph data and return mean
      WRITE (MSG,500) NPIXELS,MM(1),MM(2),RMEAN,SDEV
  500 FORMAT(I10,'PIXELS   RANGE',I4,'-',I3,'     MEAN',F8.3,
     *		'     STD DEV',F8.3)
      CALL XVMESSAGE(MSG(1:70),' ')
      RETURN
      END
C Remove all blanks from array BUF, and replace '_' with blank.
C A zero byte is placed at the new end of the string, and the length,
C N, is updated.
C
	SUBROUTINE SQUEEZE(BUF,N)
	CHARACTER*(*) BUF
C
	I = 0
	DO J=1,N
           IF (BUF(J:J).NE.' ') THEN
              I=I+1
              IF (BUF(J:J).NE.'_') THEN
                 BUF(I:I) = BUF(J:J)
              ELSE
                 BUF(I:I) = ' '
              ENDIF
           ENDIF
	ENDDO

	BUF(I+1:I+1) = CHAR(0)
	N = I
	RETURN
	END
C Print message, increase line count, and check for full screen
C
      SUBROUTINE QPRNT(BUF,LINES)
      COMMON/PAUSE/QPAUSE
      LOGICAL QPAUSE
      CHARACTER*(*) BUF
C
      CALL XVMESSAGE(BUF,' ')
      LINES = LINES + 1
      IF (LINES.GE.23 .AND. QPAUSE) THEN
	  CALL XVINTRACT('IPARAM','PRESS RETURN')
	  LINES = 0
      ENDIF
      RETURN
      END
