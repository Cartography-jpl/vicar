CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Determine size of output picture.
C Inputs:  NLI,NSI
C          LZFLG,IZFLG=1 if zoom factor is specified, =0 otherwise
C          INTERP=.TRUE. if interpolation is specified
C Outputs: NLOUT,NSOUT
C	   ZOOML,ZOOMS
C          LZOOM,IZOOM
C
      SUBROUTINE GETSIZE(IUNIT,interp,sli,ssi,nli,nsi,slo,sso,nlo,nso,
     &		nlout,nsout,zooml,zooms,lzoom,izoom,*)
      IMPLICIT NONE
      INTEGER*4 IUNIT
      LOGICAL INTERP
      INTEGER*4 SLI,SSI,NLI,NSI !Input image size field (see AREA parameter)
      INTEGER*4 SLO,SSO,NLO,NSO !Output area mapped from input area
      INTEGER*4 NLOUT,NSOUT     !Size of ouput image
      REAL*4 ZOOML,ZOOMS	!Floating point zoom factors
      INTEGER*4 LZOOM,IZOOM	!Integer zoom factors

      INTEGER*4 PAR(4),N,IND,IDEF0,IDEF,LZFLG,IZFLG
      INTEGER*4 NLIN,NSIN
      REAL*4 S,EPS/1.E-6/
      LOGICAL XVPTST
      CHARACTER*42 MSG
  160 FORMAT ('      INPUT AREA=(',I5,',',I5,',',I5,',',I5,')')
  170 FORMAT ('     OUTPUT SIZE= ',I5,' X ',I5)

C     ....Get input and output size
      CALL XVPARM('SIZE',PAR,N,IDEF0,4)
      IF (IDEF0.NE.1 .AND. N.EQ.4) THEN
         NLOUT = PAR(3)
         NSOUT = PAR(4)
         CALL XVGET(IUNIT,ind,'NL',nlin,'NS',nsin,' ')
      ELSE
         CALL XVSIZE(sli,ssi,nlout,nsout,nlin,nsin)
      ENDIF

C     ....Get output offset
      CALL XVPARM('IOFFSET',PAR,N,IDEF,2)
      IF (N.EQ.2) THEN
         SLO = PAR(1)
         SSO = PAR(2)
      ELSE
         SLO = 1
         SSO = 1
      ENDIF

C     ....Check for Magellan images
      IF ((SLO.NE.1 .OR. SSO.NE.1) .AND.
     &    (XVPTST('LOOKUP') .OR. XVPTST('VOLTS'))) GOTO 990

C     ....Get input image size field
      CALL XVPARM('AREA',PAR,N,IDEF,4)
      IF (N.EQ.4) THEN 
         SLI = PAR(1)	!(SLI,SSI,NLI,NSI) maps to (SLO,SSO,NLO,NSO)
         SSI = PAR(2)
         NLI = PAR(3)
         NSI = PAR(4)
         IF (SLI.LT.1.OR.SSI.LT.1) GOTO 980
         IF (NLI.LT.1.OR.NSI.LT.1) GOTO 980
         IF (NLI.GT.NLIN-SLI+1) GOTO 980
         IF (NSI.GT.NSIN-SSI+1) GOTO 980
      ELSE
         SLI = 1
         SSI = 1
         NLI = NLIN
         NSI = NSIN
      ENDIF
      WRITE (MSG,160) SLI,SSI,NLI,NSI
      CALL XVMESSAGE(MSG,' ')

      CALL GETZOOM(lzflg,izflg,zooml,zooms,lzoom,izoom,*999)
      INTERP = .NOT.XVPTST('NOIN')
C
C    ....If horizontal zoom is not specified, compute it from size field
      IF (IZFLG.EQ.0) THEN	
         CALL XVPARM('NS',par,n,idef,1)
	 IF (NSOUT.EQ.NSIN.AND.IDEF0.EQ.1.AND.IDEF.EQ.1) GOTO 950
         NSO = NSOUT - SSO + 1
	 ZOOMS = (1.0*NSO)/NSI
         IF (NSO.LT.NSI) THEN
	    N = NSI/NSO
            S = 1./ZOOMS - N
            IF (ABS(S).LT.1.E-6) IZOOM=-N
	 ELSE
            N = NSO/NSI
            S = ZOOMS - N
            IF (ABS(S).LT.1.E-6) IZOOM=N
	 ENDIF
         GOTO 40
      ENDIF

C     ....Here if horizontal zoom is specified.  Compute NSO from it.
      IF (IZOOM.NE.0) THEN
         IF (IZOOM.LT.0) THEN
            IF (INTERP) THEN
	       NSO = -NSI/IZOOM
            ELSE
               NSO = -(NSI-1)/IZOOM + 1
            ENDIF
	 ELSE
   	    NSO = NSI*IZOOM
	 ENDIF
      ELSE			!Floating point zoom is specified.
         IF (.NOT.INTERP.AND.ZOOMS.LT.1.0) THEN
            NSO = (NSI-1)*ZOOMS + EPS + 1
         ELSE
	    NSO = ZOOMS*NSI + EPS
         ENDIF
      ENDIF

      CALL XVPARM('NS',par,n,idef,1)
      IF (IDEF.NE.1) NSOUT=PAR(1)
      IF (IDEF.EQ.1. AND. IDEF0.EQ.1) NSOUT=SSO+NSO-1

C    ....Vertical zoom
C     ....If vertical zoom is not specified, compute it from size field
   40 IF (LZFLG.EQ.0) THEN
         CALL XVPARM('NL',par,n,idef,1)
	 IF (NLOUT.EQ.NLIN.AND.IDEF0.EQ.1.AND.IDEF.EQ.1) GOTO 960
         NLO = NLOUT - SLO + 1
         ZOOML = (1.*NLO)/NLI
         IF (NLO.LT.NLI) THEN
            N = NLI/NLO
            S = 1.0/ZOOML - N
            IF (ABS(S).LT.1.E-6) LZOOM=-N
	 ELSE
            N = NLO/NLI
            S = ZOOML - N
            IF (ABS(S).LT.1.E-6) LZOOM=N
	 ENDIF
         GOTO 80
      ENDIF

C     ....Here if vertical zoom is specified.  Compute NLO from it.
      IF (LZOOM.NE.0) THEN	!Integral zoom is specified,
         IF (LZOOM.LT.0) THEN
            IF (INTERP) THEN
	       NLO = -NLI/LZOOM
            ELSE
               NLO = -(NLI-1)/LZOOM + 1
            ENDIF
	 ELSE
            NLO = NLI*LZOOM
  	 ENDIF
      ELSE			!Floating point zoom is specified.
         IF (.NOT.INTERP.AND.ZOOML.LT.1.0) THEN
            NLO = (NLI-1)*ZOOMS + 1.0 + EPS
         ELSE
	    NLO = ZOOML*NLI + EPS
         ENDIF
      ENDIF

      CALL XVPARM('NL',par,n,idef,1)
      IF (IDEF.NE.1) NLOUT=PAR(1)
      IF (IDEF.EQ.1 .AND. IDEF0.EQ.1) NLOUT=SLO+NLO-1

   80 WRITE(MSG,170) NLOUT,NSOUT
      CALL XVMESSAGE(MSG,' ')
      IF (NSOUT.GT.50000) GOTO 970
      IF (NLO.EQ.1) THEN
         LZOOM = 1
         ZOOML = 1.
      ENDIF
      IF (NSO.EQ.1) THEN
         IZOOM = 1
         ZOOMS = 1.
      ENDIF
      IF (LZOOM.EQ.1.AND.IZOOM.EQ.1) INTERP=.FALSE.
      RETURN

  950 CALL XVMESSAGE('***Must specify either SZOOM or NS',' ')
      RETURN1
  960 CALL XVMESSAGE('***Must specify either LZOOM or NL',' ')
      RETURN1
  970 CALL XVMESSAGE('***Output sample size exceeds 50,000',' ')
      RETURN1
  980 CALL XVMESSAGE('***Invalid input picture area',' ')
      RETURN1
  990 CALL XVMESSAGE('***Magellan images cannot be offset',' ')
  999 RETURN1
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Check for ZOOM, LZOOM, or SZOOM parameters
C Outputs: ZOOML,ZOOMS = floating pt zoom factors
C          LZOOM,IZOOM = integral zoom factors (=0 if not an integer)
C          LZFLG,IZFLG = 1 if zoom factors are specified, =0 otherwise
C
      SUBROUTINE GETZOOM(lzflg,izflg,zooml,zooms,lzoom,izoom,*)

      IZFLG = 0
      LZFLG = 0

      CALL XVPARM('ZOOM',R,INUM,IDEF,1)
      IF (INUM.GT.0) THEN 
          IF (R.EQ.0.0) GOTO 998
          CALL QPAR(n,r)		!Check for integral zoom
          ZOOML = R
          ZOOMS = R
          LZOOM = N
          IZOOM = N
          IF (IZOOM.EQ.-1) IZOOM=1
          IF (LZOOM.EQ.-1) LZOOM=1
          IZFLG = 1
          LZFLG = 1
      ENDIF

      CALL XVPARM('LZOOM',R,INUM,IDEF,1)
      IF (INUM.GT.0) THEN 
          IF (R.EQ.0.0) GOTO 999
          CALL QPAR(n,r)
          ZOOML = R
          LZOOM = N
          IF (LZOOM.EQ.-1) LZOOM=1
          LZFLG = 1
      ENDIF

      CALL XVPARM('SZOOM',R,INUM,IDEF,1)
      IF (INUM.GT.0) THEN 
          IF (R.EQ.0.0) GOTO 999
          CALL QPAR(n,r)
          ZOOMS = R
          IZOOM = N
          IF (IZOOM.EQ.-1) IZOOM=1
          IZFLG = 1
      ENDIF

      RETURN

  998 CALL XVMESSAGE(' ***ZOOM cannot be zero',' ')
      RETURN1
  999 CALL XVMESSAGE(' ***LZOOM and SZOOM cannot be zero',' ')
      RETURN1
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Check for integer or real zoom factor.   An integer value is returned
C if possible.
C
C Input: R=real zoom factor specified via ZOOM=R parameter.
C Outputs:  N=integer zoom factor.  This has same meaning as in VIDS.
C            =0 if zoom factor is not an integer.
C If R is negative, it is converted to a fraction.
C E.g. if R=-2, then R=0.5 on output.
C
      SUBROUTINE QPAR(n,r)
      REAL R, S
      REAL*4 EPS
      DATA EPS/1.E-6/

      IF (R.GE.1.0) GOTO 20
C     ....Here for compression.  Compute shrink factor S.
      IF (R.GT.0.) THEN
         S = 1./R
      ELSE
         S = -R
         R = 1./S		!Convert negative zoom to fraction
      ENDIF

      N = S + EPS		!Check for integer shrink factor
      S = S - N
      IF (ABS(S).LE.EPS) THEN
         N = -N
      ELSE
         N = 0
      ENDIF
      RETURN
C
C     ....Here for magnification (R.GT.1.0)
   20 N = R + EPS		!Check for integer zoom factor
      S = R - N
      IF (ABS(S).LE.EPS) THEN
         R = N
      ELSE
         N = 0
      ENDIF

      RETURN
      END
