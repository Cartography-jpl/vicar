      INCLUDE 'VICMAIN_FOR'
C  95-1-2  ...AS....  (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C  85-3-28 ...LWK...  CONVERTED TO VAX VICAR2, ADDED STACKA CALL
C                      & 'SHIFT' KEYWORD
      SUBROUTINE MAIN44
      IMPLICIT INTEGER (A-Z)
      INTEGER*4 AREA(4)
      EXTERNAL WORK

      CALL IFMESSAGE('PSF version 2-JAN-95')
      CALL XVEACTION('SA',' ')

      CALL XVUNIT( IUN, 'INP', 1, ISTAT,' ')
      CALL XVOPEN( IUN, ISTAT, 'U_FORMAT', 'HALF', ' ')
      CALL XVSIZE( SL, SS, NLO, NSO, NL, NS)

      CALL XVPARM( 'AREA', AREA, CNT, DEF, 4)	! (NO DEFAULT ALLOWED)
      IF (AREA(3).EQ.(AREA(3)/2)*2) THEN
	AREA(3)=AREA(3)+1
	CALL XVMESSAGE('NL OF AREA INCREASED BY 1',' ')
      ENDIF
      IF (AREA(4).EQ.(AREA(4)/2)*2) THEN
	AREA(4)=AREA(4)+1
	CALL XVMESSAGE('NS OF AREA INCREASED BY 1',' ')
      ENDIF

      NB1 = 2*MAX0( NS, NSO)		! 'IN' BUFFER SIZE (IN BYTES)
      NB2 = 2*AREA(4)			! PSF BUFFER SIZE
      CALL STACKA( 10,WORK, 2, NB1, NB2, NL, NS, NLO, NSO, AREA, IUN)
      RETURN
      END

C*****************************************************************
      SUBROUTINE WORK( IN, NB1, PSF, NB2, NL, NS, NLO, NSO, AREA, IUN)
      IMPLICIT INTEGER (A-Z)
      INTEGER*2 IN( NB1/2), PSF(NB2/2)
      INTEGER*4 AREA(4)
      CHARACTER*8 FMT
      REAL*4 XDN, CENX, CENY

      CALL XVGET( IUN, STAT, 'FORMAT', FMT,' ')
      IF (FMT.NE.'BYTE' .AND. FMT.NE.'HALF' .AND. FMT.NE.'WORD') THEN
	CALL MABEND('** ONLY BYTE & HALFWORD FORMATS SUPPORTED **')
      ENDIF
      IF (FMT.EQ.'BYTE') THEN
	IH = 2			! ** FOR VICAR2 BUG IN 'NSAMPS'
	LIMIT = 255
      ELSE
	IH = 1
	LIMIT = 32767
      ENDIF

      LT = AREA(1)
      LB = AREA(3)+LT-1
      IL = AREA(2)
      IR = AREA(4)+IL-1
      IF (LB.GT.NL) LB=NL
      IF (IR.GT.NS) IR=NS
      IF (IL.GT.NS .OR. LT.GT.NL) THEN
	CALL MABEND('** AREA IS OUTSIDE INPUT IMAGE **')
      ENDIF
      NLIN=LB-LT+1
      NPIX=IR-IL+1
      NLIN2=NLIN/2
      NLINL=NLIN-NLIN2
      NSA2=NPIX/2
      NSAL=NPIX-NSA2

      CALL XVUNIT( OUN, 'OUT', 1, STATUS,' ')
      CALL XVOPEN( OUN, STATUS, 'OP', 'WRITE', 'U_NL', NLO, 'U_NS',
     .	NSO, 'U_FORMAT', 'HALF', ' ')

C  READ BORDER POINTS TO GET MAX DN
C
      N=0
      MEAN=0
      CALL XVREAD( IUN, IN, STATUS, 'LINE', LT,' ')
      DO L=LT,LB
	IF (L.EQ.LT .OR. L.EQ.LB) THEN
	  DO J=IL,IR
	    N=N+1
	    MEAN=MEAN+IN(J)
	  ENDDO
	ELSE
	  N=N+2
	  MEAN=MEAN+IN(IL)+IN(IR)
	ENDIF
	IF (L.LT.LB) CALL XVREAD( IUN, IN, STATUS,' ')
      ENDDO
      MEAN=NINT(FLOAT(MEAN)/FLOAT(N))
      CALL PRNT(4,1,MEAN,'BACKGROUND DN =')
C
C  DETERMINE CENTROID
C
      XDN = 0.0
      CENX = 0.0
      CENY = 0.0
      CALL XVREAD( IUN, IN, STATUS, 'LINE', LT,' ')
      DO L=LT,LB
	DO J=IL,IR
	  N = IN(J)-MEAN
	  IF (N.LT.0) N=0
	  N = N*N
	  XDN = XDN+N
	  CENX = CENX+FLOAT(J-IL)*N
	  CENY = CENY+FLOAT(L-LT)*N
	ENDDO
	IF (L.LT.LB) CALL XVREAD( IUN, IN, STATUS,' ')
      ENDDO
      ICENX = CENX/XDN+IL+.5
      ICENY = CENY/XDN+LT+.5
      CALL PRNT(4,1,ICENX,'X CENTROID =')
      CALL PRNT(4,1,ICENY,'Y CENTROID =')
C
C  SET UP AREA TO CONTAIN PSF
C IF 'SHIFT' WAS SPECIFIED, RETAIN ORIGINAL SIZE, MOVE CENTER.
C ELSE RETAIN ORIGINAL AREA & QUARTER THE PSF UNEQUALLY.
C **> (MAY BE BETTER TO REDUCE SIZE & QUARTER SYMMETRICALLY!)
      IF (XVPTST( 'SHIFT')) THEN
	LB=ICENY+NLIN2		! BOTTOM LINE OF PSF
	LT=ICENY-NLIN2		! TOP LINE OF PSF
	IR=ICENX+NSA2		! RIGHT EDGE OF PSF
	IL=ICENX-NSA2		! LEFT EDGE OF PSF
	IF (LB.GT.NL) LB=NL
	IF (IR.GT.NS) IR=NS
	IF (LT.LT.1) LT=1
	IF (IL.LT.1) IL=1
	NLIN=LB-LT+1		! NL OF PSF
	NPIX=IR-IL+1		! NS OF PSF
	NSA2=NPIX/2
	NLIN2=NLIN/2
      ELSE
	NLIN2 = ICENY-LT+1
	NSA2 = ICENX-IL+1
      ENDIF
      NLINL=NLIN-NLIN2
      NSAL=NPIX-NSA2
C
C  COPY DATA TO OUTPUT AND SUBTRACT BACKGROUND
C
      N=0
      DO OLIN = 1, NLO			! OUTPUT LINE NUMBER
	CALL MVE(2,NSO,0,IN,0,1)	! ZERO THE OUTPUT BUFFER
	ILIN = 0			! ZERO THE INPUT LINE NUMBER
	IF (OLIN.LE.NLINL)
     .	 ILIN = OLIN+LT-1+NLIN2  	! BOTTOM OF PSF
	IF (OLIN.GE.NLO-NLIN2+1)
     .	 ILIN = OLIN-NLO+NLIN2+LT-1	! TOP OF PSF
	IF (ILIN.NE.0) THEN
	  CALL XVREAD( IUN, PSF, STATUS, 'LINE', ILIN, 'SAMP', IL,
     .	   'NSAMPS', IH*NPIX,' ')		! READ THE PSF AREA
	  CALL SUBV(-6,NPIX,MEAN,PSF,0,1) ! SUBTRACT BACKGROUND
	  CALL TRUNC(PSF,0,LIMIT,NPIX)
	  CALL MVE(2,NSA2,PSF,IN(NSO-NSA2+1),1,1) ! LEFT SIDE OF PSF
	  CALL MVE(2,NSAL,PSF(NSA2+1),IN,1,1)	! RIGHT SIDE OF PSF
	ENDIF

	CALL XVWRIT( OUN, IN, STAT,' ')
      ENDDO

      RETURN
      END


      SUBROUTINE TRUNC( BUF, LO, HI, N)
C  TRUNCATE N BUF VALUES TO RANGE (LO, HI).
      IMPLICIT INTEGER*4 (A-Z)
      INTEGER*2 BUF(1)

      DO I=1,N
	IF (BUF(I).LT.LO) BUF(I) = LO
	IF (BUF(I).GT.HI) BUF(I) = HI
      ENDDO
      RETURN
      END
