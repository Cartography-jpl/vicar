$!****************************************************************************
$!
$! Build proc for MIPL module filter
$! VPACK Version 1.9, Friday, October 10, 2003, 12:36:54
$!
$! Execute by entering:		$ @filter
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module filter ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to filter.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("filter.imake") .nes. ""
$   then
$      vimake filter
$      purge filter.bld
$   else
$      if F$SEARCH("filter.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake filter
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @filter.bld "STD"
$   else
$      @filter.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create filter.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack filter.com -mixed -
	-s filter.f -
	-i filter.imake -
	-p filter.pdf -
	-t tstfilter.pdf new_session_3d.log old_session_3d.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create filter.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
	IF (XVPTST('BYTE')) THEN
	  OFMT = 1
	  FMT = 'BYTE'
	ELSEIF (XVPTST('HALF')) THEN
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
	  IF (XVPTST('NONSYMME')) NONS = 1
	  IF (XVPTST('ASYMMETR')) ASYM = 1
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create filter.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM filter

   To Create the build file give the command:

		$ vimake filter			(VMS)
   or
		% vimake filter			(Unix)


************************************************************************/


#define PROGRAM	filter
#define R2LIB

#define MODULE_LIST filter.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define FTNINC_LIST fortport
/************************* End of Imake file ***************************/

$ Return
$!#############################################################################
$PDF_File:
$ create filter.pdf
process help=*
PARM	INP	TYPE=STRING	
PARM	OUT	TYPE=STRING
PARM	SIZE	TYPE=INTEGER	COUNT=4        DEFAULT=(1,1,0,0)
PARM	SL	TYPE=INTEGER			DEFAULT=1
PARM	SS	TYPE=INTEGER			DEFAULT=1
PARM	SB	TYPE=INTEGER			DEFAULT=1
PARM	NL	TYPE=INTEGER			DEFAULT=0
PARM	NS	TYPE=INTEGER			DEFAULT=0
PARM	NB	TYPE=INTEGER			DEFAULT=0
PARM	NLW	TYPE=INTEGER			DEFAULT=15
PARM	NSW 	TYPE=INTEGER			DEFAULT=15
PARM	RANGE	TYPE=INTEGER	COUNT=(0:2)	DEFAULT=--
PARM	SCALE	TYPE=INTEGER	COUNT=2		DEFAULT=(0,1)
PARM	DIVIDE	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM 	WEIGHTS	TYPE=INTEGER	COUNT=0:500	DEFAULT=--
PARM	SYMM KEYWORD VALID=(NONSYMME,ASYMMETR) COUNT=(0:1) DEFAULT=--
PARM	DNMAX	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM	DNMIN	TYPE=INTEGER	COUNT=0:1	DEFAULT=--
PARM    OFORM   KEYWORD  COUNT=0:1   VALID=(BYTE,HALF)  DEFAULT=--
PARM    PARMS   TYPE=(STRING,100) COUNT=0:1   DEFAULT=--
END-PROC
.TITLE
VICAR1 PROGRAM "FILTER"
.HELP
PURPOSE:

"FILTER" is a VICAR applications program which performs two-dimensional
convolution filtering.  For each output sample, the program computes
a weighted average of a rectangular set of input pixels followed by
a linear transformation.  "FILTER" may be used to perform high-pass or 
low-pass filtering.

The following system procedure will invoke "FILTER":

  FILTER2 - first invoked program "FIL2", which generates a proper set
        of weights, and then "FILTER".

It is recommended that the user normally calls the above procedure
rather than "FILTER" directly.

.page
EXECUTION:

   The following is the execution statement format for "FILTER":

             FILTER INP=PIX OUT=OPIX PARAMS

   where INP, OUT, and PARAMS are parameters discussed in their res-
pective parameter sections. 

OPERATION:

Before "FILTER" can be used a proper set of weights must be generated for
the camera system which took the picture.  These weights are usually 
generated by the weight generator program "FIL2" (invoked by procedure
"FILTER2").
.page
"FILTER" accepts as input a rectangular set of weights and calculates each
output point FP(l,s) as follows:
               nlw nsw 
    TP(l,s) = SUM SUM  P[l-(nlw+1)/2 + i , s-(nsw+1)/2 + j] * W(i,j)
               i=1 j=1
and
    FP(l,s) = A + B * TP(l,s) /  D
where
	P  	is the input file
	W	is the weight matrix
	nlw 	is the number of lines in the weight matrix
	nsw  	is the number of samples in the weight matrix
	A,B	are the two scaling inputs
	D	is the parameter for scaling
.PAGE
RESTRICTIONS:

1) Maximum number of output samples per line is 4096.
2) For asymmetric FILTERs, NW = NLW*NSW must not exceed 4096.
3) For symmetric or vertically symmetric FILTERs, NW = NSW * (NLW+1)/2
	must not exceed 4096.
.PAGE
EXAMPLES:

1) FILTER IN OUT
   The file will be filtered by the default set of weights.

2) FILTER IN OUT NLW=3 NSW=5 WEIGHTS=(0,-1,-2,-1,-2,10)
   The file will be filtered with the weight table shown below:
         0   -1   -2   -1   0
	-1   -2   10   -2   -1
	 0   -1   -2   -1   0
   The final linear transformation is
		FP(L,S) = 0 + 1*TP(L,S) / 6
   where 6 is the default for the DIVIDE parameter, (i.e., the sum of the
   entries in the weight table).

3) FILTER IN OUT NLW=3 NSW=5 SCALE=(-30,1) DIVIDE=3 +
	WEIGHTS=(0,-1,-2,-1,-2,10)
   This is the same as the example above except the final transformation is 
   given by:
		FP(L,S) = -30 + 1*TP(L,S) / 3

4) FILTER IN OUT NLW=3 NSW=5 'NONS 'HALF DNMIN=-15 +
	WEIGHTS=(-2,5,4,3,-1,-5,20,3,1,1)
   These parameters generate the horizontally nonsymmetric weights shown
   below:
   	 -2   5   4   3   -1
	 -5  20   3   1    1
	 -2   5   4   3   -1
   and the halfword output will be computed as follows:  If the output is less
   than or equal to -15 dn, it is set to -15 dn.  If the output is greater than
   or equal to 32767 dn, then it is set to 32767 dn.  If the output is between
   -15 and 32767 dn, then no linear transformation is applied.

5) FILTER IN OUT NLW=3 NSW=3 'ASYM WEIGHTS=(-1,0,0,0,0,0,0,0,1)
   These parmeters will produce a diagonal gradient picture using this weight
   matrix:  
  	-1   0   0
	 0   0   0
	 0   0   1
.PAGE
TIMING:
  TBD

AP VERSION WRITTEN BY:  H. J. FREIDEN,   27 MAY 1981

EMULATION VERSION IMPLEMENTED BY:  L.W.KAMP,  11 OCT. 1984

COGNIZANT PROGRAMMER:  L. W. KAMP

REVISIONS:  
            A. Scop (CRI)    2 JAN 1995  Made portable for UNIX
            F. Moss         22 Oct 1995  Rename FILTEREM to FILTER throughout
                                         the entire com file, make programs
                                         FILTERAP, APFLAG, FILTEREM, & proc
                                         FILTER obsolete at a later date.
            B. McGuffie     11 Oct 1996  Removed all restrictions to AP memory
                                         size. 
.LEVEL1
.VARI INP
Input image file
.VARI OUT
filtered image file
.VARI SIZE
Vicar size field
.VARI SL
size field starting line
.VARI SS
Size field starting sample
.VARI SB
Size field starting band
.VARI NL
Size field number of lines
.VARI NS
Size field number of samples
.VARI NB
Size field number of bands
.VARI NLW
Number of lines of weights
.VARI NSW
Number of samples of weights
.VARI SYMM
Horizontally nonsymmetric or 
asymmetric weights?
.VARI WEIGHTS
Defines the weight matrix
.VARI DIVIDE
Scaling paramater upon output
.VARI SCALE
Linear output scaling:
(offset,scale)
.VARI DNMAX
Defines maximum output dn
.VARI DNMIN
Defines minimum output dn
.VARI RANGE
DN interval to which output
will be clipped.
.vari OFORM
Output data format.
(Default: input format)
.VARI PARMS
 Parameter data set name 
.LEVEL2
.VARI INP
A VICAR labeled image file
.VARI OUT
A file to write the filtered product into
.VARI SIZE
The standard size field defining the area of the input picture that is to
be filtered.
.VARI SL
Starting line of the area to be filtered.
.VARI SS
Starting sample of the area to be filtered
.VARI SB
Starting band to be filtered
.VARI NL
Number of lines in the area to be filtered.
.VARI NS
Number of samples in the area to be filtered.
.VARI NB
Number of bands to be filtered.
.VARI NLW
This specifies the number of lines of weights. Must be odd. Default = 15
.VARI NSW
This specifies the number of samples in each line of the weight matrix.
Must be odd. Always refers to samples not bytes. Default = 15
.VARI SYMM
This keyword parameter has 2 valid values:  NONSYMME and ASYMMETR.

If it is null (default), then the weight matrix is assumed to be
  symmetric, i.e., only one quadrant containing (NLW+1)/2*(NSW+1)/2 weights
  is input.

NONSYMME indicates horizontally nonsymmetric weights, i.e., the weights to
  be input will consist of ( NSW * (NLW+1)/2 ) integers representing the
  upper half of the vertically symetric FILTER weight matrix.  

ASYMMETR indicates that NLW*NSW weights for the asymmetric FILTER will be 
  input.
.VARI WEIGHTS
This keyword is followed by ( (NLW+1) * (NSW+1) / 4 ) integers, representing 
the upper left quadrant of the four-way symmteric matrix of weights.  

If NONSYMMETRIC or ASYMMETRIC was specified, then the upper half or all
of the weight matrix must be specified, respectively.

Each value must be less than 32767 in absolute value.  

Default is the SURVEYOR high-pass FILTER (15*15 weights, symmetric).
.VARI DIVIDE
This is used in the final transformation equation.  Each output point O(l,s)
is given by:

        O(l,s) = A + B*T(l,s) / DIVIDE

where A and B are defined by SCALE and T is the output of the convolution.
The default is that the sum of the weights is used.  If this sum is zero, 
then 1 is used.
.VARI SCALE
This keyword specifies the application of a linear transformation to each
output point T(l,s):

         O(l,s) = A + B*T(l,s) / DIVIDE

where SCALE=(A,B).  Default is SCALE=(0,1).
See also DIVIDE.
.VARI DNMAX
All pixels with DN greater than DNMAX upon output are set to DNMAX. 

DNMAX is synonymous with RANGE(1).

The default depends on the output data format: 255 for byte data, 
and 32767 for halfword.
.VARI DNMIN
All pixels with DN less than DNMIN upon output are set to DNMIN. 

DNMIN is synonymous with RANGE(2).

The defaults depends on the output data format: 0 for byte, and 
-32768 for halfword.

DNMIN must be positive for byte data.  
.VARI RANGE
The pair of values (a,b) specifies the range of DN to which the output will
be clipped.

RANGE is synonymous with (DNMIN, DNMAX).  If both RANGE and DNMIN or DNMAX
are specified, then RANGE will take precedence.

The default depends on the output data format: it is (0,255) for Byte data,
and (32768,32767) for Halfword.
.vari OFORM
This specifies the data format of the output.  Valid: BYTE, HALF.

Default is that the output has the same format as the input.
.VARI PARMS
 PARMS can be used to specify the name of an optional parameter data
 set. Any combination of the allowable parameters may be given. If
 any of the parameters are given interactively, the interactive value
 takes precedence.
$ Return
$!#############################################################################
$Test_File:
$ create tstfilter.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
write "THIS IS A TEST OF MODULE filter"
write "WE WILL SPATIALLY filter AN IMAGE AND LIST THE RESULTS"
write "AFTER EACH RUN."
!
write "GET A PICTURE"
gen filter.X NL=800 NS=800
write "LIST INPUT"
list filter.X (41,41,10,10)
!
write "DO DEFAULT FILTER"
filter filter.X filter.Y 
list filter.Y (1,1,10,10)
!
write "DO DEFAULT FILTER USING SIZE FIELD"
filter filter.X filter.Y (41,41,100,100)
list filter.Y (1,1,10,10)
!
write "NOW FILTER WITH HALFWORD INPUT AND OUTPUT"
insert filter.X filter.Y (41,41,100,100)
cform filter.Y filter.Z 
filter filter.Z filter.X 
list filter.X (1,1,10,20) 
!
write "NOW FILTER WITH NONSYMMETRIC WEIGHTS"
filter filter.Y filter.X NLW=5 NSW=5 'NONS +
   WEIGHTS=(-60,10,10,10,-50,   +
	    -10,60,40,50,-20,   +
	     -5,80,100,70,-10)
list filter.X (1,1,10,10)
!
write "NOW DO SAME WITH A SCALE FACTOR TO MULTIPLY VALUES BY 2"
filter filter.Y filter.X NLW=5 NSW=5 'NONS SCALE=(0,2) +
   WEIGHTS=(-60,10,10,10,-50,   +
	    -10,60,40,50,-20,   +
	     -5,80,100,70,-10)
list filter.X (1,1,10,10)
write "NOW FILTER WITH ASYMMETRIC WEIGHTS"
filter filter.Y filter.X NLW=3 NSW=3 'ASYM +
	WEIGHTS=(-20,50,20      +
	           5,100,0      +
	         -10,20,-10)
list filter.X (1,1,10,10)
!
! Test 3D images
!
gen filter.3X NL=100 NS=100 NB=3
list filter.3X (41,41,10,10)
filter filter.3X filter.3Y 
list filter.3Y 
end-proc
$!-----------------------------------------------------------------------------
$ create new_session_3d.log
tstfilter
write "THIS IS A TEST OF MODULE filter"
THIS IS A TEST OF MODULE filter
write "WE WILL SPATIALLY filter AN IMAGE AND LIST THE RESULTS"
WE WILL SPATIALLY filter AN IMAGE AND LIST THE RESULTS
write "AFTER EACH RUN."
AFTER EACH RUN.
write "GET A PICTURE"
GET A PICTURE
gen filter.X NL=800 NS=800
Beginning VICAR task gen
GEN Version 6
GEN task completed
write "LIST INPUT"
LIST INPUT
list filter.X (41,41,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:43 2003
     Samp    41      43      45      47      49
   Line
     41      80  81  82  83  84  85  86  87  88  89
     42      81  82  83  84  85  86  87  88  89  90
     43      82  83  84  85  86  87  88  89  90  91
     44      83  84  85  86  87  88  89  90  91  92
     45      84  85  86  87  88  89  90  91  92  93
     46      85  86  87  88  89  90  91  92  93  94
     47      86  87  88  89  90  91  92  93  94  95
     48      87  88  89  90  91  92  93  94  95  96
     49      88  89  90  91  92  93  94  95  96  97
     50      89  90  91  92  93  94  95  96  97  98
write "DO DEFAULT FILTER"
DO DEFAULT FILTER
filter filter.X filter.Y
Beginning VICAR task filter
FILTER Version 11-AUG-03
list filter.Y (1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:43 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:43 2003
     Samp     1       3       5       7       9
   Line
      1       0   0   1   2   3   4   5   6   7   8
      2       0   2   4   4   5   6   7   8   9  10
      3       1   4   5   5   6   7   8   9  10  11
      4       2   4   5   6   7   8   9  10  11  12
      5       3   5   6   7   8   9  10  11  12  13
      6       4   6   7   8   9  10  11  12  13  14
      7       5   7   8   9  10  11  12  13  14  15
      8       6   8   9  10  11  12  13  14  15  16
      9       7   9  10  11  12  13  14  15  16  17
     10       8  10  11  12  13  14  15  16  17  18
write "DO DEFAULT FILTER USING SIZE FIELD"
DO DEFAULT FILTER USING SIZE FIELD
filter filter.X filter.Y (41,41,100,100)
Beginning VICAR task filter
FILTER Version 11-AUG-03
list filter.Y (1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:43 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:47 2003
     Samp     1       3       5       7       9
   Line
      1      78  80  81  82  83  84  85  86  87  88
      2      80  82  84  84  85  86  87  88  89  90
      3      81  84  85  85  86  87  88  89  90  91
      4      82  84  85  86  87  88  89  90  91  92
      5      83  85  86  87  88  89  90  91  92  93
      6      84  86  87  88  89  90  91  92  93  94
      7      85  87  88  89  90  91  92  93  94  95
      8      86  88  89  90  91  92  93  94  95  96
      9      87  89  90  91  92  93  94  95  96  97
     10      88  90  91  92  93  94  95  96  97  98
write "NOW FILTER WITH HALFWORD INPUT AND OUTPUT"
NOW FILTER WITH HALFWORD INPUT AND OUTPUT
insert filter.X filter.Y (41,41,100,100)
Beginning VICAR task insert
INSERT version 02-MAY-94
cform filter.Y filter.Z
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = BYTE
OUTPUT FORMAT = HALF
CONVERSION COMPLETE
filter filter.Z filter.X
Beginning VICAR task filter
FILTER Version 11-AUG-03
list filter.X (1,1,10,20)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:43 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:47 2003
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1        78    80    81    82    83    84    85    86    87    88    89    90    91    92    93
      2        80    82    84    84    85    86    87    88    89    90    91    92    93    94    95
      3        81    84    85    85    86    87    88    89    90    91    92    93    94    95    96
      4        82    84    85    86    87    88    89    90    91    92    93    94    95    96    97
      5        83    85    86    87    88    89    90    91    92    93    94    95    96    97    98
      6        84    86    87    88    89    90    91    92    93    94    95    96    97    98    99
      7        85    87    88    89    90    91    92    93    94    95    96    97    98    99   100
      8        86    88    89    90    91    92    93    94    95    96    97    98    99   100   101
      9        87    89    90    91    92    93    94    95    96    97    98    99   100   101   102
     10        88    90    91    92    93    94    95    96    97    98    99   100   101   102   103

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:43 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:47 2003
     Samp      16    17    18    19    20
   Line
      1        94    95    96    97    98
      2        96    97    98    99   100
      3        97    98    99   100   101
      4        98    99   100   101   102
      5        99   100   101   102   103
      6       100   101   102   103   104
      7       101   102   103   104   105
      8       102   103   104   105   106
      9       103   104   105   106   107
     10       104   105   106   107   108
write "NOW FILTER WITH NONSYMMETRIC WEIGHTS"
NOW FILTER WITH NONSYMMETRIC WEIGHTS
filter filter.Y filter.X NLW=5 NSW=5 'NONS  +
   WEIGHTS=(-60,10,10,10,-50,    +
	    -10,60,40,50,-20,    +
	     -5,80,100,70,-10)
Beginning VICAR task filter
FILTER Version 11-AUG-03
list filter.X (1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:43 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:48 2003
     Samp     1       3       5       7       9
   Line
      1      79  80  82  83  84  85  86  87  88  89
      2      80  80  82  83  84  85  86  87  88  89
      3      81  82  84  85  86  87  88  89  90  91
      4      82  83  85  86  87  88  89  90  91  92
      5      83  84  86  87  88  89  90  91  92  93
      6      84  85  87  88  89  90  91  92  93  94
      7      85  86  88  89  90  91  92  93  94  95
      8      86  87  89  90  91  92  93  94  95  96
      9      87  88  90  91  92  93  94  95  96  97
     10      88  89  91  92  93  94  95  96  97  98
write "NOW DO SAME WITH A SCALE FACTOR TO MULTIPLY VALUES BY 2"
NOW DO SAME WITH A SCALE FACTOR TO MULTIPLY VALUES BY 2
filter filter.Y filter.X NLW=5 NSW=5 'NONS SCALE=(0,2)  +
   WEIGHTS=(-60,10,10,10,-50,    +
	    -10,60,40,50,-20,    +
	     -5,80,100,70,-10)
Beginning VICAR task filter
FILTER Version 11-AUG-03
list filter.X (1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:43 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:48 2003
     Samp     1       3       5       7       9
   Line
      1     158 159 163 165 167 169 171 173 175 177
      2     160 161 165 167 169 171 173 175 177 179
      3     163 164 168 170 172 174 176 178 180 182
      4     165 166 170 172 174 176 178 180 182 184
      5     167 168 172 174 176 178 180 182 184 186
      6     169 170 174 176 178 180 182 184 186 188
      7     171 172 176 178 180 182 184 186 188 190
      8     173 174 178 180 182 184 186 188 190 192
      9     175 176 180 182 184 186 188 190 192 194
     10     177 178 182 184 186 188 190 192 194 196
write "NOW FILTER WITH ASYMMETRIC WEIGHTS"
NOW FILTER WITH ASYMMETRIC WEIGHTS
filter filter.Y filter.X NLW=3 NSW=3 'ASYM  +
	WEIGHTS=(-20,50,20       +
	           5,100,0       +
	         -10,20,-10)
Beginning VICAR task filter
FILTER Version 11-AUG-03
list filter.X (1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:43 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:48 2003
     Samp     1       3       5       7       9
   Line
      1      80  82  83  84  85  86  87  88  89  90
      2      81  82  83  84  85  86  87  88  89  90
      3      82  83  84  85  86  87  88  89  90  91
      4      83  84  85  86  87  88  89  90  91  92
      5      84  85  86  87  88  89  90  91  92  93
      6      85  86  87  88  89  90  91  92  93  94
      7      86  87  88  89  90  91  92  93  94  95
      8      87  88  89  90  91  92  93  94  95  96
      9      88  89  90  91  92  93  94  95  96  97
     10      89  90  91  92  93  94  95  96  97  98
gen filter.3X NL=100 NS=100 NB=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
list filter.3X (41,41,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     1
 ***********
     Samp    41      43      45      47      49
   Line
     41      80  81  82  83  84  85  86  87  88  89
     42      81  82  83  84  85  86  87  88  89  90
     43      82  83  84  85  86  87  88  89  90  91
     44      83  84  85  86  87  88  89  90  91  92
     45      84  85  86  87  88  89  90  91  92  93
     46      85  86  87  88  89  90  91  92  93  94
     47      86  87  88  89  90  91  92  93  94  95
     48      87  88  89  90  91  92  93  94  95  96
     49      88  89  90  91  92  93  94  95  96  97
     50      89  90  91  92  93  94  95  96  97  98


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     2
 ***********
     Samp    41      43      45      47      49
   Line
     41      81  82  83  84  85  86  87  88  89  90
     42      82  83  84  85  86  87  88  89  90  91
     43      83  84  85  86  87  88  89  90  91  92
     44      84  85  86  87  88  89  90  91  92  93
     45      85  86  87  88  89  90  91  92  93  94
     46      86  87  88  89  90  91  92  93  94  95
     47      87  88  89  90  91  92  93  94  95  96
     48      88  89  90  91  92  93  94  95  96  97
     49      89  90  91  92  93  94  95  96  97  98
     50      90  91  92  93  94  95  96  97  98  99


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     3
 ***********
     Samp    41      43      45      47      49
   Line
     41      82  83  84  85  86  87  88  89  90  91
     42      83  84  85  86  87  88  89  90  91  92
     43      84  85  86  87  88  89  90  91  92  93
     44      85  86  87  88  89  90  91  92  93  94
     45      86  87  88  89  90  91  92  93  94  95
     46      87  88  89  90  91  92  93  94  95  96
     47      88  89  90  91  92  93  94  95  96  97
     48      89  90  91  92  93  94  95  96  97  98
     49      90  91  92  93  94  95  96  97  98  99
     50      91  92  93  94  95  96  97  98  99 100
filter filter.3X filter.3Y
Beginning VICAR task filter
FILTER Version 11-AUG-03
list filter.3Y
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1       0   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28
      2       0   2   4   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30
      3       1   4   5   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
      4       2   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32
      5       3   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33
      6       4   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34
      7       5   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35
      8       6   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
      9       7   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37
     10       8  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38
     11       9  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39
     12      10  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40
     13      11  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41
     14      12  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42
     15      13  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43
     16      14  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44
     17      15  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45
     18      16  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46
     19      17  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
     20      18  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48
     21      19  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49
     22      20  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50
     23      21  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51
     24      22  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52
     25      23  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53
     26      24  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
     27      25  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55
     28      26  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56
     29      27  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57
     30      28  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58
     31      29  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59
     32      30  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60
     33      31  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61
     34      32  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62
     35      33  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
     36      34  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64
     37      35  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65
     38      36  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66
     39      37  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67
     40      38  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68
     41      39  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69
     42      40  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70
     43      41  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71
     44      42  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
     45      43  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
     46      44  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74
     47      45  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75
     48      46  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76
     49      47  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77
     50      48  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78
     51      49  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
     52      50  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80
     53      51  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81
     54      52  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82
     55      53  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83
     56      54  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84
     57      55  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85
     58      56  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86
     59      57  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87
     60      58  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88
     61      59  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89
     62      60  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
     63      61  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91
     64      62  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92
     65      63  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93
     66      64  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94
     67      65  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
     68      66  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96
     69      67  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97
     70      68  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98
     71      69  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99
     72      70  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100
     73      71  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101
     74      72  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102
     75      73  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103
     76      74  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104
     77      75  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105
     78      76  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106
     79      77  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107
     80      78  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
     81      79  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109
     82      80  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110
     83      81  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
     84      82  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112
     85      83  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
     86      84  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114
     87      85  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
     88      86  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
     89      87  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117
     90      88  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118
     91      89  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
     92      90  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
     93      91  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
     94      92  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122
     95      93  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123
     96      94  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124
     97      95  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125
     98      95  98  99 100 100 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
     99      96  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
    100      99 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1       0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29
      2       1   3   5   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
      3       2   5   6   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32
      4       3   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33
      5       4   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34
      6       5   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35
      7       6   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
      8       7   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37
      9       8  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38
     10       9  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39
     11      10  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40
     12      11  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41
     13      12  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42
     14      13  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43
     15      14  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44
     16      15  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45
     17      16  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46
     18      17  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
     19      18  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48
     20      19  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49
     21      20  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50
     22      21  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51
     23      22  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52
     24      23  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53
     25      24  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
     26      25  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55
     27      26  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56
     28      27  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57
     29      28  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58
     30      29  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59
     31      30  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60
     32      31  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61
     33      32  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62
     34      33  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
     35      34  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64
     36      35  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65
     37      36  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66
     38      37  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67
     39      38  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68
     40      39  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69
     41      40  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70
     42      41  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71
     43      42  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
     44      43  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73
     45      44  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
     46      45  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75
     47      46  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76
     48      47  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77
     49      48  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78
     50      49  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
     51      50  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80
     52      51  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81
     53      52  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82
     54      53  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83
     55      54  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84
     56      55  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85
     57      56  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86
     58      57  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87
     59      58  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88
     60      59  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89
     61      60  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
     62      61  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91
     63      62  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92
     64      63  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93
     65      64  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94
     66      65  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
     67      66  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96
     68      67  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97
     69      68  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98
     70      69  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99
     71      70  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100
     72      71  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101
     73      72  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102
     74      73  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103
     75      74  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104
     76      75  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105
     77      76  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106
     78      77  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107
     79      78  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
     80      79  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109
     81      80  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110
     82      81  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
     83      82  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112
     84      83  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
     85      84  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114
     86      85  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
     87      86  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
     88      87  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117
     89      88  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118
     90      89  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
     91      90  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
     92      91  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121
     93      92  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
     94      93  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123
     95      94  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124
     96      95  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125
     97      96  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
     98      96  99 100 101 101 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
     99      97 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128
    100     100 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     3
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1       0   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30
      2       2   4   6   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32
      3       3   6   7   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33
      4       4   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34
      5       5   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35
      6       6   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
      7       7   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37
      8       8  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38
      9       9  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39
     10      10  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40
     11      11  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41
     12      12  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42
     13      13  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43
     14      14  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44
     15      15  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45
     16      16  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46
     17      17  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
     18      18  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48
     19      19  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49
     20      20  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50
     21      21  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51
     22      22  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52
     23      23  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53
     24      24  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
     25      25  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55
     26      26  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56
     27      27  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57
     28      28  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58
     29      29  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59
     30      30  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60
     31      31  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61
     32      32  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62
     33      33  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
     34      34  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64
     35      35  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65
     36      36  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66
     37      37  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67
     38      38  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68
     39      39  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69
     40      40  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70
     41      41  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71
     42      42  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
     43      43  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73
     44      44  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74
     45      45  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
     46      46  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76
     47      47  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77
     48      48  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78
     49      49  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
     50      50  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80
     51      51  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81
     52      52  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82
     53      53  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83
     54      54  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84
     55      55  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85
     56      56  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86
     57      57  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87
     58      58  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88
     59      59  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89
     60      60  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
     61      61  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91
     62      62  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92
     63      63  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93
     64      64  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94
     65      65  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
     66      66  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96
     67      67  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97
     68      68  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98
     69      69  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99
     70      70  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100
     71      71  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101
     72      72  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102
     73      73  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103
     74      74  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104
     75      75  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105
     76      76  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106
     77      77  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107
     78      78  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
     79      79  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109
     80      80  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110
     81      81  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
     82      82  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112
     83      83  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
     84      84  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114
     85      85  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
     86      86  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
     87      87  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117
     88      88  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118
     89      89  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
     90      90  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
     91      91  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121
     92      92  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122
     93      93  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
     94      94  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124
     95      95  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125
     96      96  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
     97      97  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
     98      97 100 101 102 102 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128
     99      98 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129
    100     101 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     1
 ***********
     Samp    31      33      35      37      39      41      43      45      47      49      51      53      55      57      59
   Line
      1      29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58
      2      31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60
      3      32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61
      4      33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62
      5      34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
      6      35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64
      7      36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65
      8      37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66
      9      38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67
     10      39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68
     11      40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69
     12      41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70
     13      42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71
     14      43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
     15      44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73
     16      45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74
     17      46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75
     18      47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76
     19      48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77
     20      49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78
     21      50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
     22      51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80
     23      52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81
     24      53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82
     25      54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83
     26      55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84
     27      56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85
     28      57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86
     29      58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87
     30      59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88
     31      60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89
     32      61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
     33      62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91
     34      63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92
     35      64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93
     36      65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94
     37      66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
     38      67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96
     39      68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97
     40      69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98
     41      70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99
     42      71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100
     43      72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101
     44      73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102
     45      74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    31      33      35      37      39      41      43      45      47      49      51      53      55      57      59
   Line
     46      75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104
     47      76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105
     48      77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106
     49      78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107
     50      79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
     51      80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109
     52      81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110
     53      82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
     54      83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112
     55      84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
     56      85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114
     57      86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
     58      87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
     59      88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117
     60      89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118
     61      90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
     62      91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
     63      92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121
     64      93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122
     65      94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123
     66      95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124
     67      96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125
     68      97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
     69      98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
     70      99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128
     71     100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129
     72     101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130
     73     102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131
     74     103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132
     75     104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133
     76     105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
     77     106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135
     78     107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136
     79     108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137
     80     109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138
     81     110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139
     82     111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
     83     112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141
     84     113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142
     85     114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
     86     115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
     87     116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145
     88     117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146
     89     118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147
     90     119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148
     91     120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149
     92     121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150
     93     122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    31      33      35      37      39      41      43      45      47      49      51      53      55      57      59
   Line
     94     123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
     95     124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153
     96     125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154
     97     126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155
     98     127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156
     99     128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157
    100     130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     2
 ***********
     Samp    31      33      35      37      39      41      43      45      47      49      51      53      55      57      59
   Line
      1      30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59
      2      32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61
      3      33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62
      4      34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
      5      35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64
      6      36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65
      7      37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66
      8      38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67
      9      39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68
     10      40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69
     11      41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70
     12      42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71
     13      43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
     14      44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73
     15      45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74
     16      46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75
     17      47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76
     18      48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77
     19      49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78
     20      50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
     21      51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80
     22      52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81
     23      53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82
     24      54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83
     25      55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84
     26      56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85
     27      57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86
     28      58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87
     29      59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88
     30      60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89
     31      61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
     32      62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91
     33      63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92
     34      64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93
     35      65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94
     36      66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
     37      67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96
     38      68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97
     39      69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98
     40      70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99
     41      71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100
     42      72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101
     43      73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102
     44      74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103
     45      75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    31      33      35      37      39      41      43      45      47      49      51      53      55      57      59
   Line
     46      76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105
     47      77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106
     48      78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107
     49      79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
     50      80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109
     51      81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110
     52      82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
     53      83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112
     54      84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
     55      85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114
     56      86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
     57      87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
     58      88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117
     59      89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118
     60      90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
     61      91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
     62      92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121
     63      93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122
     64      94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123
     65      95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124
     66      96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125
     67      97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
     68      98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
     69      99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128
     70     100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129
     71     101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130
     72     102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131
     73     103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132
     74     104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133
     75     105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
     76     106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135
     77     107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136
     78     108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137
     79     109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138
     80     110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139
     81     111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
     82     112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141
     83     113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142
     84     114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
     85     115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
     86     116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145
     87     117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146
     88     118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147
     89     119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148
     90     120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149
     91     121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150
     92     122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151
     93     123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    31      33      35      37      39      41      43      45      47      49      51      53      55      57      59
   Line
     94     124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153
     95     125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154
     96     126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155
     97     127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156
     98     128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157
     99     129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158
    100     131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     3
 ***********
     Samp    31      33      35      37      39      41      43      45      47      49      51      53      55      57      59
   Line
      1      31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60
      2      33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62
      3      34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
      4      35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64
      5      36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65
      6      37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66
      7      38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67
      8      39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68
      9      40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69
     10      41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70
     11      42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71
     12      43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
     13      44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73
     14      45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74
     15      46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75
     16      47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76
     17      48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77
     18      49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78
     19      50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
     20      51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80
     21      52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81
     22      53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82
     23      54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83
     24      55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84
     25      56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85
     26      57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86
     27      58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87
     28      59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88
     29      60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89
     30      61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
     31      62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91
     32      63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92
     33      64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93
     34      65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94
     35      66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
     36      67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96
     37      68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97
     38      69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98
     39      70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99
     40      71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100
     41      72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101
     42      73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102
     43      74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103
     44      75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104
     45      76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    31      33      35      37      39      41      43      45      47      49      51      53      55      57      59
   Line
     46      77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106
     47      78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107
     48      79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
     49      80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109
     50      81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110
     51      82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
     52      83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112
     53      84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
     54      85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114
     55      86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
     56      87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
     57      88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117
     58      89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118
     59      90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
     60      91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
     61      92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121
     62      93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122
     63      94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123
     64      95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124
     65      96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125
     66      97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
     67      98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
     68      99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128
     69     100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129
     70     101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130
     71     102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131
     72     103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132
     73     104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133
     74     105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
     75     106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135
     76     107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136
     77     108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137
     78     109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138
     79     110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139
     80     111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
     81     112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141
     82     113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142
     83     114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
     84     115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
     85     116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145
     86     117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146
     87     118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147
     88     119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148
     89     120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149
     90     121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150
     91     122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151
     92     123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
     93     124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    31      33      35      37      39      41      43      45      47      49      51      53      55      57      59
   Line
     94     125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154
     95     126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155
     96     127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156
     97     128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157
     98     129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158
     99     130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
    100     132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     1
 ***********
     Samp    61      63      65      67      69      71      73      75      77      79      81      83      85      87      89
   Line
      1      59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88
      2      61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
      3      62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91
      4      63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92
      5      64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93
      6      65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94
      7      66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
      8      67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96
      9      68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97
     10      69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98
     11      70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99
     12      71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100
     13      72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101
     14      73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102
     15      74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103
     16      75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104
     17      76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105
     18      77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106
     19      78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107
     20      79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
     21      80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109
     22      81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110
     23      82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
     24      83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112
     25      84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
     26      85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114
     27      86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
     28      87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
     29      88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117
     30      89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118
     31      90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
     32      91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
     33      92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121
     34      93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122
     35      94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123
     36      95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124
     37      96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125
     38      97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
     39      98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
     40      99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128
     41     100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129
     42     101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130
     43     102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131
     44     103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132
     45     104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    61      63      65      67      69      71      73      75      77      79      81      83      85      87      89
   Line
     46     105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
     47     106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135
     48     107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136
     49     108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137
     50     109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138
     51     110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139
     52     111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
     53     112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141
     54     113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142
     55     114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
     56     115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
     57     116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145
     58     117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146
     59     118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147
     60     119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148
     61     120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149
     62     121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150
     63     122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151
     64     123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
     65     124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153
     66     125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154
     67     126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155
     68     127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156
     69     128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157
     70     129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158
     71     130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
     72     131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160
     73     132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161
     74     133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162
     75     134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163
     76     135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164
     77     136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165
     78     137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166
     79     138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167
     80     139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168
     81     140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169
     82     141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170
     83     142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171
     84     143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172
     85     144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173
     86     145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174
     87     146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
     88     147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176
     89     148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177
     90     149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178
     91     150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179
     92     151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
     93     152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    61      63      65      67      69      71      73      75      77      79      81      83      85      87      89
   Line
     94     153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182
     95     154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183
     96     155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184
     97     156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185
     98     157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186
     99     158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187
    100     160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     2
 ***********
     Samp    61      63      65      67      69      71      73      75      77      79      81      83      85      87      89
   Line
      1      60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89
      2      62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91
      3      63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92
      4      64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93
      5      65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94
      6      66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
      7      67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96
      8      68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97
      9      69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98
     10      70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99
     11      71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100
     12      72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101
     13      73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102
     14      74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103
     15      75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104
     16      76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105
     17      77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106
     18      78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107
     19      79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
     20      80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109
     21      81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110
     22      82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
     23      83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112
     24      84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
     25      85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114
     26      86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
     27      87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
     28      88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117
     29      89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118
     30      90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
     31      91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
     32      92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121
     33      93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122
     34      94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123
     35      95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124
     36      96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125
     37      97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
     38      98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
     39      99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128
     40     100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129
     41     101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130
     42     102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131
     43     103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132
     44     104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133
     45     105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    61      63      65      67      69      71      73      75      77      79      81      83      85      87      89
   Line
     46     106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135
     47     107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136
     48     108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137
     49     109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138
     50     110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139
     51     111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
     52     112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141
     53     113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142
     54     114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
     55     115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
     56     116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145
     57     117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146
     58     118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147
     59     119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148
     60     120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149
     61     121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150
     62     122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151
     63     123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
     64     124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153
     65     125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154
     66     126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155
     67     127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156
     68     128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157
     69     129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158
     70     130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
     71     131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160
     72     132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161
     73     133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162
     74     134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163
     75     135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164
     76     136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165
     77     137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166
     78     138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167
     79     139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168
     80     140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169
     81     141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170
     82     142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171
     83     143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172
     84     144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173
     85     145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174
     86     146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
     87     147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176
     88     148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177
     89     149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178
     90     150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179
     91     151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
     92     152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181
     93     153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    61      63      65      67      69      71      73      75      77      79      81      83      85      87      89
   Line
     94     154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183
     95     155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184
     96     156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185
     97     157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186
     98     158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187
     99     159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188
    100     161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     3
 ***********
     Samp    61      63      65      67      69      71      73      75      77      79      81      83      85      87      89
   Line
      1      61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
      2      63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92
      3      64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93
      4      65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94
      5      66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
      6      67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96
      7      68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97
      8      69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98
      9      70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99
     10      71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100
     11      72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101
     12      73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102
     13      74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103
     14      75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104
     15      76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105
     16      77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106
     17      78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107
     18      79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
     19      80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109
     20      81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110
     21      82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
     22      83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112
     23      84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113
     24      85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114
     25      86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115
     26      87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116
     27      88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117
     28      89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118
     29      90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119
     30      91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
     31      92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121
     32      93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122
     33      94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123
     34      95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124
     35      96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125
     36      97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
     37      98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
     38      99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128
     39     100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129
     40     101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130
     41     102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131
     42     103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132
     43     104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133
     44     105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134
     45     106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    61      63      65      67      69      71      73      75      77      79      81      83      85      87      89
   Line
     46     107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136
     47     108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137
     48     109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138
     49     110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139
     50     111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
     51     112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141
     52     113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142
     53     114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
     54     115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
     55     116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145
     56     117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146
     57     118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147
     58     119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148
     59     120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149
     60     121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150
     61     122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151
     62     123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
     63     124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153
     64     125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154
     65     126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155
     66     127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156
     67     128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157
     68     129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158
     69     130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
     70     131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160
     71     132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161
     72     133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162
     73     134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163
     74     135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164
     75     136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165
     76     137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166
     77     138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167
     78     139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168
     79     140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169
     80     141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170
     81     142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171
     82     143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172
     83     144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173
     84     145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174
     85     146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
     86     147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176
     87     148 149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177
     88     149 150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178
     89     150 151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179
     90     151 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
     91     152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181
     92     153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182
     93     154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    61      63      65      67      69      71      73      75      77      79      81      83      85      87      89
   Line
     94     155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184
     95     156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185
     96     157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186
     97     158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187
     98     159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188
     99     160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189
    100     162 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     1
 ***********
     Samp    91      93      95      97      99
   Line
      1      89  90  91  92  93  94  95  96  97  99
      2      91  92  93  94  95  96  97  98  99 102
      3      92  93  94  95  96  98  98  99 100 103
      4      93  94  95  96  97  98  99 100 101 103
      5      94  95  96  97  98  99 100 101 102 104
      6      95  96  97  98  99 100 101 102 103 105
      7      96  97  98  99 100 101 102 103 104 106
      8      97  98  99 100 101 102 103 104 105 107
      9      98  99 100 101 102 103 104 105 106 108
     10      99 100 101 102 103 104 105 106 107 109
     11     100 101 102 103 104 105 106 107 108 110
     12     101 102 103 104 105 106 107 108 109 111
     13     102 103 104 105 106 107 108 109 110 112
     14     103 104 105 106 107 108 109 110 111 113
     15     104 105 106 107 108 109 110 111 112 114
     16     105 106 107 108 109 110 111 112 113 115
     17     106 107 108 109 110 111 112 113 114 116
     18     107 108 109 110 111 112 113 114 115 117
     19     108 109 110 111 112 113 114 115 116 118
     20     109 110 111 112 113 114 115 116 117 119
     21     110 111 112 113 114 115 116 117 118 120
     22     111 112 113 114 115 116 117 118 119 121
     23     112 113 114 115 116 117 118 119 120 122
     24     113 114 115 116 117 118 119 120 121 123
     25     114 115 116 117 118 119 120 121 122 124
     26     115 116 117 118 119 120 121 122 123 125
     27     116 117 118 119 120 121 122 123 124 126
     28     117 118 119 120 121 122 123 124 125 127
     29     118 119 120 121 122 123 124 125 126 128
     30     119 120 121 122 123 124 125 126 127 129
     31     120 121 122 123 124 125 126 127 128 130
     32     121 122 123 124 125 126 127 128 129 131
     33     122 123 124 125 126 127 128 129 130 132
     34     123 124 125 126 127 128 129 130 131 133
     35     124 125 126 127 128 129 130 131 132 134
     36     125 126 127 128 129 130 131 132 133 135
     37     126 127 128 129 130 131 132 133 134 136
     38     127 128 129 130 131 132 133 134 135 137
     39     128 129 130 131 132 133 134 135 136 138
     40     129 130 131 132 133 134 135 136 137 139
     41     130 131 132 133 134 135 136 137 138 140
     42     131 132 133 134 135 136 137 138 139 141
     43     132 133 134 135 136 137 138 139 140 142
     44     133 134 135 136 137 138 139 140 141 143
     45     134 135 136 137 138 139 140 141 142 144


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    91      93      95      97      99
   Line
     46     135 136 137 138 139 140 141 142 143 145
     47     136 137 138 139 140 141 142 143 144 146
     48     137 138 139 140 141 142 143 144 145 147
     49     138 139 140 141 142 143 144 145 146 148
     50     139 140 141 142 143 144 145 146 147 149
     51     140 141 142 143 144 145 146 147 148 150
     52     141 142 143 144 145 146 147 148 149 151
     53     142 143 144 145 146 147 148 149 150 152
     54     143 144 145 146 147 148 149 150 151 153
     55     144 145 146 147 148 149 150 151 152 154
     56     145 146 147 148 149 150 151 152 153 155
     57     146 147 148 149 150 151 152 153 154 156
     58     147 148 149 150 151 152 153 154 155 157
     59     148 149 150 151 152 153 154 155 156 158
     60     149 150 151 152 153 154 155 156 157 159
     61     150 151 152 153 154 155 156 157 158 160
     62     151 152 153 154 155 156 157 158 159 161
     63     152 153 154 155 156 157 158 159 160 162
     64     153 154 155 156 157 158 159 160 161 163
     65     154 155 156 157 158 159 160 161 162 164
     66     155 156 157 158 159 160 161 162 163 165
     67     156 157 158 159 160 161 162 163 164 166
     68     157 158 159 160 161 162 163 164 165 167
     69     158 159 160 161 162 163 164 165 166 168
     70     159 160 161 162 163 164 165 166 167 169
     71     160 161 162 163 164 165 166 167 168 170
     72     161 162 163 164 165 166 167 168 169 171
     73     162 163 164 165 166 167 168 169 170 172
     74     163 164 165 166 167 168 169 170 171 173
     75     164 165 166 167 168 169 170 171 172 174
     76     165 166 167 168 169 170 171 172 173 175
     77     166 167 168 169 170 171 172 173 174 176
     78     167 168 169 170 171 172 173 174 175 177
     79     168 169 170 171 172 173 174 175 176 178
     80     169 170 171 172 173 174 175 176 177 179
     81     170 171 172 173 174 175 176 177 178 180
     82     171 172 173 174 175 176 177 178 179 181
     83     172 173 174 175 176 177 178 179 180 182
     84     173 174 175 176 177 178 179 180 181 183
     85     174 175 176 177 178 179 180 181 182 184
     86     175 176 177 178 179 180 181 182 183 185
     87     176 177 178 179 180 181 182 183 184 186
     88     177 178 179 180 181 182 183 184 185 187
     89     178 179 180 181 182 183 184 185 186 188
     90     179 180 181 182 183 184 185 186 187 189
     91     180 181 182 183 184 185 186 187 188 190
     92     181 182 183 184 185 186 187 188 189 191
     93     182 183 184 185 186 187 188 189 190 192


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    91      93      95      97      99
   Line
     94     183 184 185 186 187 188 189 190 191 193
     95     184 185 186 187 188 189 190 191 192 194
     96     185 186 187 188 189 190 191 192 193 195
     97     186 187 188 189 190 191 192 193 194 196
     98     187 188 189 190 191 192 193 193 194 197
     99     188 189 190 191 192 193 194 194 196 198
    100     190 191 192 193 194 195 196 197 198 200


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     2
 ***********
     Samp    91      93      95      97      99
   Line
      1      90  91  92  93  94  95  96  97  98 100
      2      92  93  94  95  96  97  98  99 100 103
      3      93  94  95  96  97  99  99 100 101 104
      4      94  95  96  97  98  99 100 101 102 104
      5      95  96  97  98  99 100 101 102 103 105
      6      96  97  98  99 100 101 102 103 104 106
      7      97  98  99 100 101 102 103 104 105 107
      8      98  99 100 101 102 103 104 105 106 108
      9      99 100 101 102 103 104 105 106 107 109
     10     100 101 102 103 104 105 106 107 108 110
     11     101 102 103 104 105 106 107 108 109 111
     12     102 103 104 105 106 107 108 109 110 112
     13     103 104 105 106 107 108 109 110 111 113
     14     104 105 106 107 108 109 110 111 112 114
     15     105 106 107 108 109 110 111 112 113 115
     16     106 107 108 109 110 111 112 113 114 116
     17     107 108 109 110 111 112 113 114 115 117
     18     108 109 110 111 112 113 114 115 116 118
     19     109 110 111 112 113 114 115 116 117 119
     20     110 111 112 113 114 115 116 117 118 120
     21     111 112 113 114 115 116 117 118 119 121
     22     112 113 114 115 116 117 118 119 120 122
     23     113 114 115 116 117 118 119 120 121 123
     24     114 115 116 117 118 119 120 121 122 124
     25     115 116 117 118 119 120 121 122 123 125
     26     116 117 118 119 120 121 122 123 124 126
     27     117 118 119 120 121 122 123 124 125 127
     28     118 119 120 121 122 123 124 125 126 128
     29     119 120 121 122 123 124 125 126 127 129
     30     120 121 122 123 124 125 126 127 128 130
     31     121 122 123 124 125 126 127 128 129 131
     32     122 123 124 125 126 127 128 129 130 132
     33     123 124 125 126 127 128 129 130 131 133
     34     124 125 126 127 128 129 130 131 132 134
     35     125 126 127 128 129 130 131 132 133 135
     36     126 127 128 129 130 131 132 133 134 136
     37     127 128 129 130 131 132 133 134 135 137
     38     128 129 130 131 132 133 134 135 136 138
     39     129 130 131 132 133 134 135 136 137 139
     40     130 131 132 133 134 135 136 137 138 140
     41     131 132 133 134 135 136 137 138 139 141
     42     132 133 134 135 136 137 138 139 140 142
     43     133 134 135 136 137 138 139 140 141 143
     44     134 135 136 137 138 139 140 141 142 144
     45     135 136 137 138 139 140 141 142 143 145


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    91      93      95      97      99
   Line
     46     136 137 138 139 140 141 142 143 144 146
     47     137 138 139 140 141 142 143 144 145 147
     48     138 139 140 141 142 143 144 145 146 148
     49     139 140 141 142 143 144 145 146 147 149
     50     140 141 142 143 144 145 146 147 148 150
     51     141 142 143 144 145 146 147 148 149 151
     52     142 143 144 145 146 147 148 149 150 152
     53     143 144 145 146 147 148 149 150 151 153
     54     144 145 146 147 148 149 150 151 152 154
     55     145 146 147 148 149 150 151 152 153 155
     56     146 147 148 149 150 151 152 153 154 156
     57     147 148 149 150 151 152 153 154 155 157
     58     148 149 150 151 152 153 154 155 156 158
     59     149 150 151 152 153 154 155 156 157 159
     60     150 151 152 153 154 155 156 157 158 160
     61     151 152 153 154 155 156 157 158 159 161
     62     152 153 154 155 156 157 158 159 160 162
     63     153 154 155 156 157 158 159 160 161 163
     64     154 155 156 157 158 159 160 161 162 164
     65     155 156 157 158 159 160 161 162 163 165
     66     156 157 158 159 160 161 162 163 164 166
     67     157 158 159 160 161 162 163 164 165 167
     68     158 159 160 161 162 163 164 165 166 168
     69     159 160 161 162 163 164 165 166 167 169
     70     160 161 162 163 164 165 166 167 168 170
     71     161 162 163 164 165 166 167 168 169 171
     72     162 163 164 165 166 167 168 169 170 172
     73     163 164 165 166 167 168 169 170 171 173
     74     164 165 166 167 168 169 170 171 172 174
     75     165 166 167 168 169 170 171 172 173 175
     76     166 167 168 169 170 171 172 173 174 176
     77     167 168 169 170 171 172 173 174 175 177
     78     168 169 170 171 172 173 174 175 176 178
     79     169 170 171 172 173 174 175 176 177 179
     80     170 171 172 173 174 175 176 177 178 180
     81     171 172 173 174 175 176 177 178 179 181
     82     172 173 174 175 176 177 178 179 180 182
     83     173 174 175 176 177 178 179 180 181 183
     84     174 175 176 177 178 179 180 181 182 184
     85     175 176 177 178 179 180 181 182 183 185
     86     176 177 178 179 180 181 182 183 184 186
     87     177 178 179 180 181 182 183 184 185 187
     88     178 179 180 181 182 183 184 185 186 188
     89     179 180 181 182 183 184 185 186 187 189
     90     180 181 182 183 184 185 186 187 188 190
     91     181 182 183 184 185 186 187 188 189 191
     92     182 183 184 185 186 187 188 189 190 192
     93     183 184 185 186 187 188 189 190 191 193


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    91      93      95      97      99
   Line
     94     184 185 186 187 188 189 190 191 192 194
     95     185 186 187 188 189 190 191 192 193 195
     96     186 187 188 189 190 191 192 193 194 196
     97     187 188 189 190 191 192 193 194 195 197
     98     188 189 190 191 192 193 194 194 195 198
     99     189 190 191 192 193 194 195 195 197 199
    100     191 192 193 194 195 196 197 198 199 201


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 ***********
 Band =     3
 ***********
     Samp    91      93      95      97      99
   Line
      1      91  92  93  94  95  96  97  98  99 101
      2      93  94  95  96  97  98  99 100 101 104
      3      94  95  96  97  98 100 100 101 102 105
      4      95  96  97  98  99 100 101 102 103 105
      5      96  97  98  99 100 101 102 103 104 106
      6      97  98  99 100 101 102 103 104 105 107
      7      98  99 100 101 102 103 104 105 106 108
      8      99 100 101 102 103 104 105 106 107 109
      9     100 101 102 103 104 105 106 107 108 110
     10     101 102 103 104 105 106 107 108 109 111
     11     102 103 104 105 106 107 108 109 110 112
     12     103 104 105 106 107 108 109 110 111 113
     13     104 105 106 107 108 109 110 111 112 114
     14     105 106 107 108 109 110 111 112 113 115
     15     106 107 108 109 110 111 112 113 114 116
     16     107 108 109 110 111 112 113 114 115 117
     17     108 109 110 111 112 113 114 115 116 118
     18     109 110 111 112 113 114 115 116 117 119
     19     110 111 112 113 114 115 116 117 118 120
     20     111 112 113 114 115 116 117 118 119 121
     21     112 113 114 115 116 117 118 119 120 122
     22     113 114 115 116 117 118 119 120 121 123
     23     114 115 116 117 118 119 120 121 122 124
     24     115 116 117 118 119 120 121 122 123 125
     25     116 117 118 119 120 121 122 123 124 126
     26     117 118 119 120 121 122 123 124 125 127
     27     118 119 120 121 122 123 124 125 126 128
     28     119 120 121 122 123 124 125 126 127 129
     29     120 121 122 123 124 125 126 127 128 130
     30     121 122 123 124 125 126 127 128 129 131
     31     122 123 124 125 126 127 128 129 130 132
     32     123 124 125 126 127 128 129 130 131 133
     33     124 125 126 127 128 129 130 131 132 134
     34     125 126 127 128 129 130 131 132 133 135
     35     126 127 128 129 130 131 132 133 134 136
     36     127 128 129 130 131 132 133 134 135 137
     37     128 129 130 131 132 133 134 135 136 138
     38     129 130 131 132 133 134 135 136 137 139
     39     130 131 132 133 134 135 136 137 138 140
     40     131 132 133 134 135 136 137 138 139 141
     41     132 133 134 135 136 137 138 139 140 142
     42     133 134 135 136 137 138 139 140 141 143
     43     134 135 136 137 138 139 140 141 142 144
     44     135 136 137 138 139 140 141 142 143 145
     45     136 137 138 139 140 141 142 143 144 146


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    91      93      95      97      99
   Line
     46     137 138 139 140 141 142 143 144 145 147
     47     138 139 140 141 142 143 144 145 146 148
     48     139 140 141 142 143 144 145 146 147 149
     49     140 141 142 143 144 145 146 147 148 150
     50     141 142 143 144 145 146 147 148 149 151
     51     142 143 144 145 146 147 148 149 150 152
     52     143 144 145 146 147 148 149 150 151 153
     53     144 145 146 147 148 149 150 151 152 154
     54     145 146 147 148 149 150 151 152 153 155
     55     146 147 148 149 150 151 152 153 154 156
     56     147 148 149 150 151 152 153 154 155 157
     57     148 149 150 151 152 153 154 155 156 158
     58     149 150 151 152 153 154 155 156 157 159
     59     150 151 152 153 154 155 156 157 158 160
     60     151 152 153 154 155 156 157 158 159 161
     61     152 153 154 155 156 157 158 159 160 162
     62     153 154 155 156 157 158 159 160 161 163
     63     154 155 156 157 158 159 160 161 162 164
     64     155 156 157 158 159 160 161 162 163 165
     65     156 157 158 159 160 161 162 163 164 166
     66     157 158 159 160 161 162 163 164 165 167
     67     158 159 160 161 162 163 164 165 166 168
     68     159 160 161 162 163 164 165 166 167 169
     69     160 161 162 163 164 165 166 167 168 170
     70     161 162 163 164 165 166 167 168 169 171
     71     162 163 164 165 166 167 168 169 170 172
     72     163 164 165 166 167 168 169 170 171 173
     73     164 165 166 167 168 169 170 171 172 174
     74     165 166 167 168 169 170 171 172 173 175
     75     166 167 168 169 170 171 172 173 174 176
     76     167 168 169 170 171 172 173 174 175 177
     77     168 169 170 171 172 173 174 175 176 178
     78     169 170 171 172 173 174 175 176 177 179
     79     170 171 172 173 174 175 176 177 178 180
     80     171 172 173 174 175 176 177 178 179 181
     81     172 173 174 175 176 177 178 179 180 182
     82     173 174 175 176 177 178 179 180 181 183
     83     174 175 176 177 178 179 180 181 182 184
     84     175 176 177 178 179 180 181 182 183 185
     85     176 177 178 179 180 181 182 183 184 186
     86     177 178 179 180 181 182 183 184 185 187
     87     178 179 180 181 182 183 184 185 186 188
     88     179 180 181 182 183 184 185 186 187 189
     89     180 181 182 183 184 185 186 187 188 190
     90     181 182 183 184 185 186 187 188 189 191
     91     182 183 184 185 186 187 188 189 190 192
     92     183 184 185 186 187 188 189 190 191 193
     93     184 185 186 187 188 189 190 191 192 194


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:49:49 2003
     Samp    91      93      95      97      99
   Line
     94     185 186 187 188 189 190 191 192 193 195
     95     186 187 188 189 190 191 192 193 194 196
     96     187 188 189 190 191 192 193 194 195 197
     97     188 189 190 191 192 193 194 195 196 198
     98     189 190 191 192 193 194 195 195 196 199
     99     190 191 192 193 194 195 196 196 198 200
    100     192 193 194 195 196 197 198 199 200 202
end-proc
disable-log
$!-----------------------------------------------------------------------------
$ create old_session_3d.log
tstfilter
write "THIS IS A TEST OF MODULE filter"
THIS IS A TEST OF MODULE filter
write "WE WILL SPATIALLY filter AN IMAGE AND LIST THE RESULTS"
WE WILL SPATIALLY filter AN IMAGE AND LIST THE RESULTS
write "AFTER EACH RUN."
AFTER EACH RUN.
write "GET A PICTURE"
GET A PICTURE
gen filter.X NL=800 NS=800
Beginning VICAR task gen
GEN Version 6
GEN task completed
write "LIST INPUT"
LIST INPUT
list filter.X (41,41,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:51:05 2003
     Samp    41      43      45      47      49
   Line
     41      80  81  82  83  84  85  86  87  88  89
     42      81  82  83  84  85  86  87  88  89  90
     43      82  83  84  85  86  87  88  89  90  91
     44      83  84  85  86  87  88  89  90  91  92
     45      84  85  86  87  88  89  90  91  92  93
     46      85  86  87  88  89  90  91  92  93  94
     47      86  87  88  89  90  91  92  93  94  95
     48      87  88  89  90  91  92  93  94  95  96
     49      88  89  90  91  92  93  94  95  96  97
     50      89  90  91  92  93  94  95  96  97  98
write "DO DEFAULT FILTER"
DO DEFAULT FILTER
filter filter.X filter.Y
Beginning VICAR task filter
FILTER Version 11-OCT-96
list filter.Y (1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:51:05 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:51:05 2003
     Samp     1       3       5       7       9
   Line
      1       0   0   1   2   3   4   5   6   7   8
      2       0   2   4   4   5   6   7   8   9  10
      3       1   4   5   5   6   7   8   9  10  11
      4       2   4   5   6   7   8   9  10  11  12
      5       3   5   6   7   8   9  10  11  12  13
      6       4   6   7   8   9  10  11  12  13  14
      7       5   7   8   9  10  11  12  13  14  15
      8       6   8   9  10  11  12  13  14  15  16
      9       7   9  10  11  12  13  14  15  16  17
     10       8  10  11  12  13  14  15  16  17  18
write "DO DEFAULT FILTER USING SIZE FIELD"
DO DEFAULT FILTER USING SIZE FIELD
filter filter.X filter.Y (41,41,100,100)
Beginning VICAR task filter
FILTER Version 11-OCT-96
list filter.Y (1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:51:05 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:51:08 2003
     Samp     1       3       5       7       9
   Line
      1      78  80  81  82  83  84  85  86  87  88
      2      80  82  84  84  85  86  87  88  89  90
      3      81  84  85  85  86  87  88  89  90  91
      4      82  84  85  86  87  88  89  90  91  92
      5      83  85  86  87  88  89  90  91  92  93
      6      84  86  87  88  89  90  91  92  93  94
      7      85  87  88  89  90  91  92  93  94  95
      8      86  88  89  90  91  92  93  94  95  96
      9      87  89  90  91  92  93  94  95  96  97
     10      88  90  91  92  93  94  95  96  97  98
write "NOW FILTER WITH HALFWORD INPUT AND OUTPUT"
NOW FILTER WITH HALFWORD INPUT AND OUTPUT
insert filter.X filter.Y (41,41,100,100)
Beginning VICAR task insert
INSERT version 02-MAY-94
cform filter.Y filter.Z
Beginning VICAR task cform
CFORM VERSION 06-JUN-1998
OUT = IN *     1.000+     0.000
INPUT FORMAT = BYTE
OUTPUT FORMAT = HALF
CONVERSION COMPLETE
filter filter.Z filter.X
Beginning VICAR task filter
FILTER Version 11-OCT-96
list filter.X (1,1,10,20)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:51:05 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:51:09 2003
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1        78    80    81    82    83    84    85    86    87    88    89    90    91    92    93
      2        80    82    84    84    85    86    87    88    89    90    91    92    93    94    95
      3        81    84    85    85    86    87    88    89    90    91    92    93    94    95    96
      4        82    84    85    86    87    88    89    90    91    92    93    94    95    96    97
      5        83    85    86    87    88    89    90    91    92    93    94    95    96    97    98
      6        84    86    87    88    89    90    91    92    93    94    95    96    97    98    99
      7        85    87    88    89    90    91    92    93    94    95    96    97    98    99   100
      8        86    88    89    90    91    92    93    94    95    96    97    98    99   100   101
      9        87    89    90    91    92    93    94    95    96    97    98    99   100   101   102
     10        88    90    91    92    93    94    95    96    97    98    99   100   101   102   103

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:51:05 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:51:09 2003
     Samp      16    17    18    19    20
   Line
      1        94    95    96    97    98
      2        96    97    98    99   100
      3        97    98    99   100   101
      4        98    99   100   101   102
      5        99   100   101   102   103
      6       100   101   102   103   104
      7       101   102   103   104   105
      8       102   103   104   105   106
      9       103   104   105   106   107
     10       104   105   106   107   108
write "NOW FILTER WITH NONSYMMETRIC WEIGHTS"
NOW FILTER WITH NONSYMMETRIC WEIGHTS
filter filter.Y filter.X NLW=5 NSW=5 'NONS  +
   WEIGHTS=(-60,10,10,10,-50,    +
	    -10,60,40,50,-20,    +
	     -5,80,100,70,-10)
Beginning VICAR task filter
FILTER Version 11-OCT-96
list filter.X (1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:51:05 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:51:09 2003
     Samp     1       3       5       7       9
   Line
      1      79  80  82  83  84  85  86  87  88  89
      2      80  80  82  83  84  85  86  87  88  89
      3      81  82  84  85  86  87  88  89  90  91
      4      82  83  85  86  87  88  89  90  91  92
      5      83  84  86  87  88  89  90  91  92  93
      6      84  85  87  88  89  90  91  92  93  94
      7      85  86  88  89  90  91  92  93  94  95
      8      86  87  89  90  91  92  93  94  95  96
      9      87  88  90  91  92  93  94  95  96  97
     10      88  89  91  92  93  94  95  96  97  98
write "NOW DO SAME WITH A SCALE FACTOR TO MULTIPLY VALUES BY 2"
NOW DO SAME WITH A SCALE FACTOR TO MULTIPLY VALUES BY 2
filter filter.Y filter.X NLW=5 NSW=5 'NONS SCALE=(0,2)  +
   WEIGHTS=(-60,10,10,10,-50,    +
	    -10,60,40,50,-20,    +
	     -5,80,100,70,-10)
Beginning VICAR task filter
FILTER Version 11-OCT-96
list filter.X (1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:51:05 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:51:10 2003
     Samp     1       3       5       7       9
   Line
      1     158 159 163 165 167 169 171 173 175 177
      2     160 161 165 167 169 171 173 175 177 179
      3     163 164 168 170 172 174 176 178 180 182
      4     165 166 170 172 174 176 178 180 182 184
      5     167 168 172 174 176 178 180 182 184 186
      6     169 170 174 176 178 180 182 184 186 188
      7     171 172 176 178 180 182 184 186 188 190
      8     173 174 178 180 182 184 186 188 190 192
      9     175 176 180 182 184 186 188 190 192 194
     10     177 178 182 184 186 188 190 192 194 196
write "NOW FILTER WITH ASYMMETRIC WEIGHTS"
NOW FILTER WITH ASYMMETRIC WEIGHTS
filter filter.Y filter.X NLW=3 NSW=3 'ASYM  +
	WEIGHTS=(-20,50,20       +
	           5,100,0       +
	         -10,20,-10)
Beginning VICAR task filter
FILTER Version 11-OCT-96
list filter.X (1,1,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:51:05 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:51:10 2003
     Samp     1       3       5       7       9
   Line
      1      80  82  83  84  85  86  87  88  89  90
      2      81  82  83  84  85  86  87  88  89  90
      3      82  83  84  85  86  87  88  89  90  91
      4      83  84  85  86  87  88  89  90  91  92
      5      84  85  86  87  88  89  90  91  92  93
      6      85  86  87  88  89  90  91  92  93  94
      7      86  87  88  89  90  91  92  93  94  95
      8      87  88  89  90  91  92  93  94  95  96
      9      88  89  90  91  92  93  94  95  96  97
     10      89  90  91  92  93  94  95  96  97  98
gen filter.3X NL=100 NS=100 NB=3
Beginning VICAR task gen
GEN Version 6
GEN task completed
list filter.3X (41,41,10,10)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:51:10 2003
 ***********
 Band =     1
 ***********
     Samp    41      43      45      47      49
   Line
     41      80  81  82  83  84  85  86  87  88  89
     42      81  82  83  84  85  86  87  88  89  90
     43      82  83  84  85  86  87  88  89  90  91
     44      83  84  85  86  87  88  89  90  91  92
     45      84  85  86  87  88  89  90  91  92  93
     46      85  86  87  88  89  90  91  92  93  94
     47      86  87  88  89  90  91  92  93  94  95
     48      87  88  89  90  91  92  93  94  95  96
     49      88  89  90  91  92  93  94  95  96  97
     50      89  90  91  92  93  94  95  96  97  98


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:51:10 2003
 ***********
 Band =     2
 ***********
     Samp    41      43      45      47      49
   Line
     41      81  82  83  84  85  86  87  88  89  90
     42      82  83  84  85  86  87  88  89  90  91
     43      83  84  85  86  87  88  89  90  91  92
     44      84  85  86  87  88  89  90  91  92  93
     45      85  86  87  88  89  90  91  92  93  94
     46      86  87  88  89  90  91  92  93  94  95
     47      87  88  89  90  91  92  93  94  95  96
     48      88  89  90  91  92  93  94  95  96  97
     49      89  90  91  92  93  94  95  96  97  98
     50      90  91  92  93  94  95  96  97  98  99


 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:51:10 2003
 ***********
 Band =     3
 ***********
     Samp    41      43      45      47      49
   Line
     41      82  83  84  85  86  87  88  89  90  91
     42      83  84  85  86  87  88  89  90  91  92
     43      84  85  86  87  88  89  90  91  92  93
     44      85  86  87  88  89  90  91  92  93  94
     45      86  87  88  89  90  91  92  93  94  95
     46      87  88  89  90  91  92  93  94  95  96
     47      88  89  90  91  92  93  94  95  96  97
     48      89  90  91  92  93  94  95  96  97  98
     49      90  91  92  93  94  95  96  97  98  99
     50      91  92  93  94  95  96  97  98  99 100
filter filter.3X filter.3Y
Beginning VICAR task filter
FILTER Version 11-OCT-96
[VIC2-GENERR] Exception in XVREAD, processing file: filter.3X
[VIC2-STRTREC] Bad starting record for read or write operation; program error.
 Current line in image = 0
 ** ABEND called **
continue
list filter.3Y
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:51:10 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:51:12 2003
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line

     29       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0  69

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Fri Aug 22 15:51:10 2003
 Task:FILTER    User:ntt       Date_Time:Fri Aug 22 15:51:12 2003
 ***********
 Band =     1
 ***********
     Samp    31      33      35      37      39      41      43      45      47      49      51      53      55      57      59
   Line

     29     160 128   0   0   0   0   0   0   0   3   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
end-proc
disable-log
$ Return
$!#############################################################################
