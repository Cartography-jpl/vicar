	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C VICAR PROGRAM filter:    ARRAY PROCESSOR EMULATION VERSION OF FILTERAP
C  SPATIAL FILTER PROGRAM
C DERIVED FROM FIL.FTN  AP FILTER (PDP-11 TASK)
C THIS VERSION READS AND WRITES HALFWORD DATA 
C WEIGHTS CAN BE VERTICALLY SYMMETRIC OR ASYMMETRIC

C  03-08-14    ...NTT...  Enabled 3D image capability.  Reads are
C                         no longer sequential.  BIP images prohibited.
C  96-10-11    ...BAM...  Removed all checks for the physical size of the
C                         memory of the array processor which we don't
C                         have any more. I left in the checks for 4096
C                         samples, however, which really could be changed
C                         to a much bigger number at any time when required.
C  95-1-28     ...SP....  Modified for name change: from VADD to VADDEM.
C  95-1-2      ...AS....  (CRI)  MSTP S/W CONVERSION (VICAR PORTING)
C  85-8-18     ...LWK...  BUG FIXES
C  85-4-18     ...LWK...  CONVERTED TO VICAR2
C  84-10-11    ...LWK...  AP EMULATION VERSION
C  01 JAN  84  ...CCA...    MAKE HALFWORD RANGE -32768 TO 32767
C  01 NOV  83  ...CCA...    CONVERT TO VAX
C  13 JULY 81  ...HJF...   FIX BUG - DIVISOR > 32768 NOW WORKS
C  7/81        ...HJF... REMOVE APWD AND APWR (USE NEW DAPEX)
C  30 JUNE 81  ... ? ...    MAKE DIVIDE I*4, DEL SPACE,ADD ASYM
C  6/81        ...HJF... CORRECT DIVIDE ERROR (MAKE IT I*4)
C  27 MAY 81   ...HJF...   INITIAL RELEASE - ARRAY PROCESSOR
	IMPLICIT INTEGER(A-Z)
	COMMON NLO,NSO,NBO,NLW,NSW,I4DIV,ISC(2),IDMIN,IDMAX,ASYM
	COMMON LOC,LOC1,LWTS,LOUT,LTEMP,LSUM,LCW,LCL
	COMMON LSA,LMIN,LMAX,LSM
	COMMON NLWH,NSWH,NSS,NLWNSS
	COMMON /FPC/ SA,DNMIN,DNMAX,SM
	COMMON /RW/ LINE,SKIP,IFMT,OFMT,IUN,OUN,BAND
	REAL SM,SA,DNMIN,DNMAX,FLOAT,RNSS,RSPACE
	INTEGER*4 BAND
	INTEGER*4 BANDOUT,LINEOUT

	INTEGER*2 W(4096),A(4096),ISTAT
        integer*4 w4(4096)
        equivalence (w,w4)
        integer*4 a4(4096)
        equivalence (a,a4)

C	REAL T0,SECNDS,T1
C
        CALL IFMESSAGE('FILTER Version 11-AUG-03')
	CALL APINIT(1,0,ISTAT)
	IF(ISTAT.LT.0) GOTO 910
	CALL GETPAR(W,SL,SS,SB,IFMT,OFMT,IUN,OUN)
	SKIP = SS 
	IF(NSO.GT.4096) GOTO 950
	NLWH=NLW/2
	NLWHP1=NLWH+1
	NSWH=NSW/2
	NSWHP1=NSWH+1
	NLWNSW=NLWHP1*NSW            ! TOP HALF OF SYMMETRIC WEIGHT MATRIX
	IF(ASYM.EQ.1) NLWNSW=NLW*NSW ! CHECK FOR ASYMMETRIC WTS
	NSS=NSO+NSW-1
	NLWNSS=NLW*NSS               ! AP MEMORY REQUIREMENT
	RNSS=NSS                     ! CALC AP SPACE REQUIRED
	RSPACE=RNSS*(NLW+1)+2*NSO+NLWNSW


c	IF(RSPACE.GT.65530) GOTO 940    bam 10/96

	SM=FLOAT(ISC(2))/FLOAT(I4DIV)        ! MULTIPLICATIVE SCALE FACTOR
	SA=ISC(1)+.5                         ! ADDITIVE SCALE FACTOR
	DNMIN=IDMIN
	DNMAX=IDMAX
C
C	SF.VFC USES THE FOLLOWING AP MEMORY LAYOUT
C	DO NOT CHANGE IT, UNLESS SF.VFC IS ALSO CHANGED *****
	LOC1=0
	LOC=0
	LWTS=NLWNSS
	LOUT=LWTS+NLWNSW                    ! LOC OF OUTPUT LINE
	LTEMP=LOUT+NSO                      ! LOC FOR TEMP STORAGE LINE
	LSUM=LTEMP+NSO
	LSA=LSUM+NSS
	LMIN=LSA+1
	LMAX=LMIN+1
	LSM=LMAX+1
C
c	T0=SECNDS(0.)
	CALL APPUT(SA,LSA,4,2)                 ! SEND R*4 CONSTANTS TO AP
	CALL APPUT(W,LWTS,2*NLWNSW,1)          ! TRANSFER THE WEIGHTS
	CALL APWD
	CALL VFLT(LWTS,1,LWTS,1,NLWNSW)        ! FLOAT THEM
	CALL VSMUL(LWTS,1,LSM,LWTS,1,NLWNSW)   ! SCALE THEM
	LCW=LWTS+NLWH*NSW          ! STORE LOC OF CENTER WT FOR SYM FILTER
	IF(ASYM.EQ.1) LCW=0        ! CHECK FOR ASYMMETRIC FILTER
C
C NLW LINES ARE STORED IN A CIRCULAR BUFFER
C LOC1 POINTS TO THE TOP LINE
C LCL POINTS TO THE CENTER LINE
C THE EDGES ARE HANDLED BY UNFOLDING
C FOR EXAMPLE, IF NLW EQ 5, THE INITIAL CIRCULAR BUFFER WILL CONTAIN
C LINE 3 AT LOC 3*NSS
C LINE 2 AT LOC 4*NSS
C LINE 1 AT LOC 0
C LINE 2 AT LOC NSS
C LINE 3 AT LOC 2*NSS
C
	BANDOUT=0
	DO 200 BAND=SB,SB+NBO-1
	BANDOUT = BANDOUT + 1
	LINEOUT = 0
	LCL=0                       ! LOC OF CENTER LINE
	LOC=0
	LINE = SL
	DO 360 L=1,NLWHP1      ! READ IN FIRST NLWHP1 LINES, STARTING AT 0
	CALL GNL(A)	   	! GET NEXT LINE INTO AP AT LOC
	LINE = LINE + 1
c	LINEOUT = LINEOUT + 1 		! NO MORE SEQUENTIAL READS!
360	LOC=LOC+NSS
C
	LOC1=0                      ! ASSUME NLWH EQ 0
	IF(NLWH.EQ.0) GOTO 375      ! IF NO UNFOLDING REQUIRED, GOTO 375
	LOC=0                       ! SET TO UNFOLD TOP OF PICTURE
	LOC1=NLWNSS
	DO 370 I=1,NLWH             ! UNFOLD NLWH LINES
	LOC=LOC+NSS                 ! STEP LOC OF LINE TO BE UNFOLDED
	LOC1=LOC1-NSS               ! STEP LOC OF UNFOLDED LINE
	CALL VMOV(LOC,1,LOC1,1,NSS) ! UNFOLD A LINE
370	CALL APWR
C
C NLW LINES ARE NOW IN AP MEMORY, LOC1 POINTS TO TOP LINE
C
375	OL=0                        ! INITIALIZE OUTPUT LINE COUNTER
400	OL=OL+1                     ! INCREMENT OUTPUT LINE COUNTER
	LINEOUT = LINEOUT + 1 
	CALL APFIL(A,LINEOUT,BANDOUT)               ! FILTER AND OUTPUT LINE OL
	IF(OL.EQ.NLO) GOTO 500      ! IF ALL DONE, GOTO 500
	LOC=LOC1                    ! LOC FOR NEXT INPUT LINE
	LOC1=LOC1+NSS               ! UPDATE LOC OF TOP LINE
	IF(LOC1.EQ.NLWNSS) LOC1=0
	LCL=LCL+NSS                 ! UPDATE LOC OF CENTER LINE
	IF(LCL.EQ.NLWNSS) LCL=0
	IF(OL.GE.NLO-NLWH) GOTO 450 ! IF NO MORE INPUT LINES, GOTO 450
	CALL GNL(A)		    ! GET NEXT LINE
	 LINE = LINE + 1 
	IF(OL.EQ.NLO-NLWHP1) LOCN=LOC       ! SAVE LOC OF LINE NLO
	GOTO 400
C
450	CONTINUE                    ! UNFOLD LINES AT BOTTOM OF PICTURE
	IF(LOCN.EQ.0) LOCN=NLWNSS
	LOCN=LOCN-NSS               ! STEP BACK ONE LINE
	CALL VMOV(LOCN,1,LOC,1,NSS) ! UNFOLD THE LINE
	GOTO 400
C
500	CONTINUE
200	CONTINUE				  !for each band...
c	CALL PRNT(7,1,SECNDS(T0),' SEC.')
	RETURN
C
910	CALL MABEND('APINIT ERR')
950	CALL MABEND('NSO GT 4096')
	END

C***************************************************************
	SUBROUTINE APFIL(A,LINEOUT,BANDOUT)
	IMPLICIT INTEGER(A-Z)
	COMMON NLO,NSO,NBO,NLW,NSW,I4DIV,ISC(2),IDMIN,IDMAX,ASYM
	COMMON LOC,LOC1,LWTS,LOUT,LTEMP,LSUM,LCW,LCL
	COMMON LSA,LMIN,LMAX,LSM
	COMMON NLWH,NSWH,NSS,NLWNSS
	COMMON /RW/ LINE,SKIP,IFMT,OFMT,IUN,OUN,BAND
c	COMMON /OUT/ LINEOUT, BANDOUT
	INTEGER*2 A(4096)
	INTEGER*4 BANDOUT,LINEOUT

C
C	SEE SF.VFC FOR DETAILS
	CALL SF(LOC1,LWTS,LOUT,NSO,NSW,NLW,NSS,NLWNSS,LCW,LCL)
	CALL APWR
	CALL APGET(A,LTEMP,NSO,1)
	CALL APWD
	CALL XVWRIT( OUN, A, I,'LINE',LINEOUT,'NSAMPS', NSO,
     +	        'BAND',BANDOUT,' ')
C
	RETURN
        end
C
C=====================================================================
	 subroutine GNL(A)
	 IMPLICIT INTEGER(A-Z)
	 COMMON NLO,NSO,NBO,NLW,NSW,I4DIV,ISC(2),IDMIN,IDMAX,ASYM
	 COMMON LOC,LOC1,LWTS,LOUT,LTEMP,LSUM,LCW,LCL
	 COMMON LSA,LMIN,LMAX,LSM
	 COMMON NLWH,NSWH,NSS,NLWNSS
	 COMMON /RW/ LINE,SKIP,IFMT,OFMT,IUN,OUN,BAND
	 INTEGER*2 A(4096)
	 INTEGER*4 BAND
C

      CALL XVREAD( IUN, A, I ,'LINE', LINE, 'SAMP', SKIP,'NSAMPS', NSO,
     +	'BAND', BAND, ' ')
	
	 CALL APPUT(A,LTEMP,NSO,1)          ! TRANSFER NEXT LINE TO AP
	 CALL APWD

	 CALL FLN(LTEMP,LOC+NSWH,LOC+NSS-NSWH,NSO,NSWH) ! UNFOLD IT

	 RETURN
	 END

C**********************************************************************
         SUBROUTINE GETPAR(WTAB,SL,SS,SB,IFMT,OFMT,IUN,OUN)
         IMPLICIT INTEGER(A-Z)
         COMMON NLO,NSO,NBO,NLW,NSW,DIVIDE,ISC(2),IDMIN,IDMAX,IASYM
         INTEGER NONS,DIVSW,SCALE(2),WBUF(50000)
         INTEGER ASYM, MINSW, MAXSW, RANGE(2)
         INTEGER*2 WTAB(4096),W(120)
	    CHARACTER*8 FMT
	    CHARACTER*3 ORGIN
         CHARACTER*90 ERROR3
         DATA NONS/0/,DIVSW/0/,SCALE/0,1/,ASYM/0/
         DATA MINSW/0/, MAXSW/0/, RANGE/0,255/
         DATA W/-23,-19,5,31,43,42,37,34,37,42,43,31,5,-19,-23,-16,17,  
     *                       42,36,7,-20,-36,-40,-36,-20,7,36,42,17,-16,
     *            15,44,22,-28,-59,-52,-28,-17,-28,-52,-59,-28,22,44,15,
     *            41,25,-40,-69,-12,78,137,153,137,78,-12,-69,-40,25,41,
     *           41,-21,-76,-8,135,185,103,40,103,185,135,-8,-76,-21,41,
     *     22,-61,-58,102,193,-67,-566,-825,-566,-67,193,102,-58,-61,22,
     *    3,-80,-19,173,94,-607,-1617,-2104,-1617,-607,94,173,-19,-80,3,
     *      -3,-84,0,191,16,-897,-2141,30001,-2141,-897,16,191,0,-84,-3/
C
         ERROR3(1:45)='FILTER-REQUESTED SIZE OF PICTURE EXCEEDS SIZE'
         ERROR3(46:90)=' SPECIFIED IN SYSTEM LABEL.  POSSIBLE ERROR. '
	CALL XVUNIT( IUN, 'INP', 1, I,' ')
	CALL XVOPEN( IUN, I, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     .	 'U_FORMAT', 'HALF',' ')
	CALL XVSIZE( SL, SS, NL, NS, NLI, NSI)
	CALL XVBANDS(SB, NB, NBI)
 
	IF ( SB .GT. NBI ) CALL MABEND(
     +  'SB is greater than the total number of bands')
                 
      IF ( SB + NB - 1 .GT. NBI) THEN 
	 CALL XVMESSAGE('***Number of bands truncated', ' ')
	 NB = NBI + 1 - SB
      ENDIF
	

c     Check organization of image, prohibit BIP
      CALL XVGET(IUN,I,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')	 

	IF (NL.GT.NLI .OR. NS.GT.NSI) THEN
	  CALL MABEND(ERROR3)
	ENDIF
	IF (NS.GT.4096) GOTO 970

	CALL XVGET(  IUN, I, 'FORMAT', FMT,' ')
	IF (FMT.EQ.'BYTE') THEN
	  IFMT = 1
	ELSEIF (FMT.EQ.'HALF' .OR. FMT.EQ.'WORD') THEN
	  IFMT = 2
	ELSE
	  CALL MABEND('** ONLY BYTE/HALF FORMATS SUPPORTED **')
	ENDIF

C  OUTPUT FORMAT:

	OFMT = IFMT			! BY DEFAULT, SAME AS INPUT
	IF (XVPTST('BYTE').EQ.true) THEN
	  OFMT = 1
	  FMT = 'BYTE'
	ELSEIF (XVPTST('HALF').EQ.true) THEN
	  OFMT = 2
	  FMT = 'HALF'
	ENDIF

C  WEIGHTS

	CALL XVPARM( 'NLW', NLW, I, NLDEF,1)
	IF (I.EQ.0) NLW = 15		!FOR CASE WHEN CALLED INSIDE PROC
	CALL XVPARM( 'NSW', NSW, I, NSDEF,1)
	IF (I.EQ.0) NSW = 15
	CALL XVPARM( 'WEIGHTS', WBUF, NW, WDEF,500)
	IF (NW.EQ.0) THEN
	  IF (NLW.EQ.15 .AND. NSW.EQ.15) THEN
	    CALL MVE( 6, 120, W, WBUF,1,1)	!DEFAULT WEIGHTS
	    NONS = 1				!NONSYMMETRIC
	    ASYM = 0
	  ELSE
	    CALL MABEND('DEFAULT WEIGHTS REQUIRE NLW=NSW=15')
	  ENDIF
	ELSE
	  IF (XVPTST('NONSYMME').EQ.true) NONS = 1
	  IF (XVPTST('ASYMMETR').EQ.true) ASYM = 1
	ENDIF

C   IF NONSYMMETRIC OPTION WAS SPECIFIED ,THEN TOP HALF OF WEIGHT
C    TABLE IS INPUT
	ILA=(NLW+1)/2
	IF (NONS.EQ.0 .AND. ASYM.EQ.0) THEN
	  IF(ILA*NSW.GT.4096) GOTO 960
	  ILB=(NSW+1)/2
	  DO IJ=1,ILA
	  DO IK=1,ILB
	    IKM=IK+(IJ-1)*ILB
	    WTAB((IJ-1)*NSW+IK) = WBUF(IKM)
	    WTAB(IJ*NSW+1-IK) = WBUF(IKM)
	  ENDDO
	  ENDDO
	ELSE
	  ILA=ILA*NSW
          IF (ASYM.EQ.1) ILA=NLW*NSW
	  IF (ILA.GT.4096) GOTO 960
	  DO IJ=1,ILA
	    WTAB(IJ) = WBUF(IJ)
	  ENDDO
	ENDIF

	CALL XVPARM( 'RANGE', RANGE, CNT, J,2)
	IF (CNT.GT.0) THEN
	  MINSW = 1
	  MAXSW = 1
	ELSE
	  CALL XVPARM( 'DNMIN', RANGE, CNT, J,1)
	  IF (CNT.GT.0) MINSW = 1
	  CALL XVPARM( 'DNMAX', RANGE(2), CNT, J,1)
	  IF (CNT.GT.0) MAXSW = 1
	ENDIF

	CALL XVPARM( 'SCALE', SCALE, I, J,2)

	CALL XVPARM( 'DIVIDE', DIVIDE, CNT, J,1)
	IF (CNT.GT.0) DIVSW = 1
C
	IJF = NSW*(NLW-1)/2
	IJF1 = IJF+1
	IJF2 = IJF+NSW			! NUMBER OF WEIGHTS TO SEND
	IF(ASYM.EQ.1) IJF2=NLW*NSW
C CHECK THAT DATA WILL FIT IN AP MEMORY
c	IF ((NLW+1)*(NS+NSW-1)+2*NS+IJF2.GT.65530) GO TO 980

	IF (DIVSW.LE.0) THEN
	  DIVIDE=0
	  IF (ASYM.EQ.1) THEN
	    DO I2=1,IJF2
	      DIVIDE = DIVIDE+WTAB(I2)
	    ENDDO
	  ELSE
	    IF (IJF.NE.0) THEN
	      DO I2=1,IJF
	        DIVIDE = DIVIDE+WTAB(I2)*2
	      ENDDO
	    ENDIF
	    DO I2=IJF1,IJF2
	      DIVIDE = DIVIDE+WTAB(I2)
	    ENDDO
	  ENDIF
	ENDIF
	IF (DIVIDE.EQ.0) DIVIDE=1

      NLO = NL
      NSO = NS
      NBO = NB
C
C  OPEN OUTPUT
	CALL XVUNIT( OUN, 'OUT', 1, I,' ')
	CALL XVOPEN( OUN, I, 'OP', 'WRITE', 'U_FORMAT', 'HALF',
     .   'O_FORMAT', FMT, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     .   'U_NL',NLO,'U_NS',NSO,'U_NB',NBO,' ')  ! added 8.22.03

	NEO2 = NSO*2		! # OF BYTES
	ISC(1) = SCALE(1)
	ISC(2) = SCALE(2)
	IDMIN = RANGE(1)
	IDMAX = RANGE(2)
	IF (MINSW.EQ.0) THEN
	  IF (OFMT.EQ.1) IDMAX = 0
	  IF (OFMT.EQ.2) IDMAX = -32768
	ENDIF
	IF (MAXSW.EQ.0) THEN
	  IF (OFMT.EQ.1) IDMAX = 255
	  IF (OFMT.EQ.2) IDMAX = 32767
	ENDIF
	IASYM=ASYM

	RETURN

960      CALL MABEND('NLW*NSW TOO LARGE')
970      CALL MABEND('NS GT 4096')
        END

C*****************************************************************
C THE FOLLOWING ROUTINES REPLACE VFC ROUTINES IN THE AP VERSION:


	SUBROUTINE SF(ILOC1,ILWTS,ILOUT,INSO,INSW,INLW,INSS,INLWNSS,
     .	  ILCW,ILCL)

C THIS REPLACES VFC ROUTINE. NOTE ALL ARGUMENTS ARE COPIED TO LOCALS.

	IMPLICIT INTEGER(A-Z)

	LOC1 = ILOC1
	LWTS = ILWTS
	LOUT = ILOUT
	NSO = INSO
	NSW = INSW
	NLW = INLW
	NSS = INSS
	NLWNSS = INLWNSS
	LCW = ILCW
	LCL = ILCL

	LTEMP = LOUT+NSO
	LSUM = LTEMP+NSO
	LSA = LSUM+NSS
	LMIN = LSA+1
	LMAX = LMIN+1

	IF(LCW.EQ.0) GOTO 50	! CHECK FOR ASYMMETRIC WTS
	CALL CONV(LCL,1,LCW,1,LOUT,1,NSO,NSW) ! SYMMETRIC WTS
	LT=LCL
	LB=LCL
	DO 40 I=1,NLW/2
	IF (LT.EQ.0) LT = NLWNSS
	LT=LT-NSS ! LOC OF TOP LINE
	LB=LB+NSS ! LOC OF BOTTOM LINE
	IF (LB.EQ.NLWNSS) LB = 0
	CALL VADDEM(LT,1,LB,1,LSUM,1,NSS)
	LCW=LCW-NSW
	CALL CONV(LSUM,1,LCW,1,LTEMP,1,NSO,NSW)
	CALL VADDEM(LTEMP,1,LOUT,1,LOUT,1,NSO)
40	CONTINUE
	GOTO 60

50	CALL CONV(LOC1,1,LWTS,1,LOUT,1,NSO,NSW)
	LD = LOC1
	LW = LWTS
	DO 80 L=1,NLW-1
	LD = LD+NSS
	IF (LD.EQ.NLWNSS) LD = 0
	LW = LW+NSW
	CALL CONV(LD,1,LW,1,LTEMP,1,NSO,NSW)
	CALL VADDEM(LTEMP,1,LOUT,1,LOUT,1,NSO)
80	CONTINUE

60	CALL VSADD(LOUT,1,LSA,LOUT,1,NSO)
	CALL VCLIP(LOUT,1,LMIN,LMAX,LOUT,1,NSO)
	CALL VFIX(LOUT,1,LTEMP,1,NSO)               ! FIX

	RETURN
	END
	SUBROUTINE FLN(ILTEMP,ILS,ILE,INSO,INSWH)

C REPLACE FLN.VFC

	IMPLICIT INTEGER (A-Z)

	LTEMP = ILTEMP
	LS = ILS
	LE = ILE
	NSO = INSO
	NSWH = INSWH

	CALL VFLT(LTEMP,1,LS,1,NSO) ! FLOAT
	IF(NSWH.EQ.0) RETURN
	CALL VMOV(LS+1,1,LS-1,-1,NSWH)
	CALL VMOV(LE-2,-1,LE,1,NSWH)

	RETURN
	END
