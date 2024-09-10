$!****************************************************************************
$!
$! Build proc for MIPL module power
$! VPACK Version 1.8, Tuesday, June 03, 1997, 08:08:31
$!
$! Execute by entering:		$ @power
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
$ write sys$output "*** module power ***"
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
$ write sys$output "Invalid argument given to power.com file -- ", primary
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
$   if F$SEARCH("power.imake") .nes. ""
$   then
$      vimake power
$      purge power.bld
$   else
$      if F$SEARCH("power.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake power
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @power.bld "STD"
$   else
$      @power.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create power.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack power.com -
	-s power.f -
	-i power.imake -
	-p power.pdf -
	-t tstpower.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create power.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C     POWER SPECTRUM PROGRAM
C
C     04/16/97  ... SP  ... Made portable for UNIX & VMS.  Replaced handling
C                           of SIZE parameter with call to XVSIZE.  Revised
C                           plotting code using program OTF1 as a guide.
C                           Revised label processing code to do string searches
C                           in an uppercase copy of the label data; this turned 
C                           out to be unnecessary: the problem was that the 
C                           FORMAT optional is needed in the XLGET.
C     11/16/95  ... CCA ... MADE SCAL R*4
C     07/18/94  ... CCA ... ADD OPTION OF NO TABLE COLUMN HEADERS
c     06/14/94  ... CCA ... FIXED FMAX, DELETED HISTOGRAM PLOT
c     04/11/94  ... CCA ... ADDED DNSCALE
c     02/22/94  ... CCA ... MAJOR CODE CLEANUP, REWORK OF LABEL HANDLING,
c			    TEST FILE AND PLOTTING OF LABELS
C     11/22/93  ... CCA ... ADD ASCII TABLE OUTPUT, CHECKS FOR OPEN
C                           ERRORS, FIX TEST, MOD SELECTION OF PLOT
C                           OR PRINT
C     09/01/84  ... FFM ... CONVERT FROM IBM TO VAX
C               T. RINDFLEISCH ... ORIGINAL RELEASE
C
      IMPLICIT COMPLEX(C),CHARACTER*1(H),REAL*8(D)
      DIMENSION C(1024),POW(513),PLAT(1025)

      COMMON /C1/ NLI,NSI

      REAL SCAL,KPP,BSCALE
      CHARACTER*60 FILEMSG
      CHARACTER*52 HTITX,HTITLE
      CHARACTER*255 FILENAME,PLOTFILE,TBL
      CHARACTER*80  HEADR(70)    !strings for the plot header ala OTF1.
      CHARACTER*60 MSG2
      CHARACTER*24 MSG3
      CHARACTER*24 MSG5
      CHARACTER*24 MSG6
      CHARACTER*1 TAB

      CHARACTER*24 MESS
      CHARACTER*24 TITY
      INTEGER STATUS,COUNT,DEF,SL,SS
      INTEGER PLOTT,PTR,EL
      INTEGER*2 BUF(1024)
      LOGICAL XVPTST

      EQUIVALENCE (C(181),PLAT(1))

      DATA MESS/' MIPL RMS POWER SPECTRUM'/
      DATA TITY/' AMPLITUDE   (DN P-P)   '/
      DATA MSG2/'    TRANSFORM  SL =        SS =        NL =        NS =
     +     '/
      DATA MSG3/'         POINT TRANSFORM'/
      DATA MSG5/'    MEAN (DN) =         '/
      DATA MSG6/'    SIGMA(DN) =         '/


c	PRINT = 0
	PLOTT = 0
      HTITX(1:52)=' '
      HTITLE(1:52)=' '

C       OPEN INPUT DATA 
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'U_FORMAT','HALF','OPEN_ACT', 'SA',
     .             'IO_ACT', 'SA' ,' ')

C       GET INPUT SIZE
      CALL XVSIZE( SL, SS, NL, NS, NLI, NSI)
C
C       PARAM 'EXPONENT'
      CALL XVPARM('EXPONENT',NP,COUNT,DEF,1)
      IF (COUNT .EQ. 0) THEN
         IF (NS .GE. 1024) THEN		!EXPONENT NOT GIVEN, MAKE ONE
		NP=10
	 ELSE IF (NS .GE. 512) THEN
		NP=9
	 ELSE IF (NS .GE. 256) THEN
		NP=8
	 ELSE IF (NS .GE. 128) THEN
		NP=7
	 ELSE IF (NS .GE. 64) THEN
		NP=6
	 ELSE IF (NS .GE. 32) THEN
		NP=5
	 ELSE IF (NS .GE. 16) THEN
		NP=4
	 ELSE 
         	NP=3
	 END IF
      END IF
C
C       PARAM 'SCALE'              !DN PER INCH OF PLOT
      CALL XVPARM('SCALE',SCAL,COUNT,DEF,1)
C
C       PARAM 'DNSCALE'              !SCALING OF DNS BEFORE POWER
      CALL XVPARM('DNSCALE',BSCALE,COUNT,DEF,1)
C
C       PARAM 'FMAX'
      CALL XVPARM('FMAX',FMAX,COUNT,DEF,1)
C
C       PARAM 'TITLE'
      CALL XVPARM('TITLE',HTITLE,COUNT,DEF,1)
      IF (COUNT .GT. 0) THEN
         CALL XVSPTR(HTITLE,COUNT,PTR,NTITLE)  !GET LENGTH AS ENTERED.
      ELSE
         NTITLE=0
      END IF
C
C       PARAM 'TITLEX'
	CALL XVPARM('TITLEX',HTITX,COUNT,DEF,1)
	NTITX=LEN(HTITX)
C
C       PARAM 'YLEN'
      CALL XVPARM('YLEN',YLEN,COUNT,DEF,1)
      IF((YLEN.LT.0.0).OR.(YLEN.GT. 30.0)) THEN
          CALL XVMESSAGE('ILLEGAL VALUE FOR YLEN',' ')
          CALL ABEND
      END IF
C
C	IF (XVPTST('PRINT')) PRINT=1
C
	IF (XVPTST('PLOT')) PLOTT=1

	CALL XVPARM('FILE',PLOTFILE,COUNT,DEF,1)
	IF (COUNT .EQ. 1) THEN
		PLOTT=1
	ELSE
		PLOTFILE='POWER.PLT'
	END IF
C
C----------------------------------------------------------
C       NTOT = OUTPUT SIZE IN SAMPLES
C       NS=NUMBER OF SAMPLES 
      NTOT=2**NP
      NMID=NTOT/2+1
      IF(NS.GT.NTOT) NS=NTOT
C
C       PRINT OUT THE HEADER
      CALL XVMESSAGE(MESS(2:),' ')

C----------------------------------------------------------
C------PROCESSING SECTION
C       ADJUST NL AND NS SO THEY MAKE SENSE
      IF (SL+NL-1 .GT. NLI) THEN
	CALL XVMESSAGE('NUMBER OF LINES TRUNCATED',' ')
	NL=NLI-SL+1
      END IF
      IF (SS+NS-1 .GT. NSI) THEN
	CALL XVMESSAGE('NUMBER OF SAMPLES TRUNCATED',' ')
	NS=NSI-SS+1
      END IF
      IF(NL+NS.LE.0) GO TO 997

C       NLEFT=ADDITIONAL SAMPLES POWER WILL HAVE TO GENERATE

        NLEFT=NTOT-NS
        DO J=1,NMID
         POW(J)=0.0
        END DO
C
C       DAV=SUM OF "a" IN DOUBLE PRECISION
C       DVR=SUM OF "a**2" IN DOUBLE PRECISION
      DAV=0.0D+00
      DVR=0.0D+00
      DNOR=1.0D+00/DFLOAT(NS)
C
C       READ INPUT DATA INTO HALFWORD BUFFER
      EL=SL+NL-1
      DO LINE=SL,EL
         CALL XVREAD(INUNIT,BUF,STATUS,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         RN=0.0
         RNN=0.0
C          PUT THE DN IN THE REAL PART OF COMPLEX NUMBER AND ZERO OUT THE
C          IMAGINARY PART
         DO KP=1,NS
            KPP=FLOAT(BUF(KP))/BSCALE  !NOTE THAT KPP IS DECLARED REAL.
            RN=RN+KPP
            RNN=RNN+KPP*KPP
            C(KP)=CMPLX(KPP,0.0)
         END DO
C
C          AVERAGING THE AVAILABLE INPUT DATA
         VQAV= RN/FLOAT(NS)
         IF(NLEFT.LT.1) GO TO 400
C
C          USE THE AVERAGE VALUE TO FILL IN THE ADDITIONAL SAMPLES
         DO KP=1,NLEFT
            C(NS+KP)=CMPLX(VQAV,0.0)
         END DO
  400    DAV=DAV+DBLE(RN)
         DVR=DVR+DBLE(RNN)
C    
C          1-DIMENSIONAL DIRECT FOURIER TRANSFORM
C        CALL PRNT(7,64,C,'1C*8 BUFFER BEFORE FFTT=.')
C
         CALL FFTT(NP,-1,C)
C
C        CALL PRNT(7,34,C,'1C*8 BUFFER AFTER FFTT=.') 
C
C          ADD THE RESULTS OF ONE LINE'S TRANSFORM TO A CUMULATIVE POWER
C          SPECTRUM

c--------Accumulate the amplitude of the complex data into POW
         CALL CMAG(NMID,C,POW)

C        CALL PRNT(7,NMID,POW,'1CUMULATIVE POWER SPECTRUM=.')
      END DO

	RNOR = 1.0/(NL*NS)

c-------Take summed amplitudes in POW and get mean Peak-to-peak amplitude
c-------at each frequency.
      DO K=1,NMID
         POW(K)= 2*POW(K)*RNOR
c        POW(K)=SQRT(POW(K)/NL)/NS*2      !wrong (see also in CMAG)
      END DO

c-----To make element one the mean DN of image, divide by 4
      POW(1)=POW(1)/4

C COMPUTE MEAN AND SIGMA FOR PRINTOUT
C      DNOR=1.0D+00/(DFLOAT(NS)*DFLOAT(NL))
	DNOR=DBLE(RNOR)
      DAV=DNOR*DAV
      VVR=DSQRT(DABS(DNOR*DVR-DAV*DAV))
      VAV=DAV
      WRITE (MSG2(20:24),'(I5)') SL
      WRITE (MSG2(32:36),'(I5)') SS
      WRITE (MSG2(44:48),'(I5)') NL
      WRITE (MSG2(56:60),'(I5)') NS
      WRITE (MSG3(4:8),'(I5)') NTOT
      WRITE (MSG5(16:24),'(F9.4)') VAV
      WRITE (MSG6(16:24),'(F9.4)') VVR
      CALL XVMESSAGE(MSG2(2:60),' ')
      CALL XVMESSAGE(MSG3(2:24),' ')
      CALL XVMESSAGE(MSG5(2:24),' ')
      CALL XVMESSAGE(MSG6(2:24),' ')

      VF=2.D0*FMAX/NTOT       !FREQ. FMAX IS AT ELEMENT NTOT/2

C------------------------------------------------------
C-------OPEN OUTPUT TEXT FILE TO PRINT ALL POINTS TO NMID
	CALL XVPARM('TABLE',TBL,COUNT,DEF,1)
	IF(COUNT .GT. 0) THEN
 	  TAB=CHAR(9)
	  OPEN(99,FILE=TBL,STATUS='UNKNOWN',IOSTAT=J,ERR=998)

	  IF (XVPTST('COLHDR')) 
     1            WRITE(99,121) 'FREQUENCY',TAB,'RMS_POWER'

	  DO 119 K=1,NMID
	    FQ = (K-1)*0.5/NMID         !FREQ. 0.5 IS AT ELEMENT NMID
119	  WRITE(99,FMT=120,IOSTAT=J,ERR=998) FQ,TAB,POW(K)

	  CLOSE(99)
	END IF

121	FORMAT(1X,A9,A1,A9)
120	FORMAT(1X,F7.4,A1,F10.3)

C------------------------------------------------------

      IF (PLOTT .EQ. 1) THEN
C-----INITIALIZE THE PLOTTER
        CALL PLOTFN(PLOTFILE)
        CALL XRTBEGIN(STATUS)
        IF (STATUS.NE.1) CALL MABEND('Unable to start up plotting')
!!        CALL SETLANDSCAPE(0)      ! SET TO 'PORTRAIT' !does not help!
        CALL DISPLAYAXES(1,1,0)   ! x,y1 and y2 axes displayed 1=yes 0=no
        CALL SETACTIVESET(0)      ! 0= no triangles (default), 1= triangles
        YLEN = YLEN*SCAL          !convert from inches to application units.
C       PLOT THE HEADER, ETC; First load all the strings for heading in HEADR.

      NHDR = 1
      HEADR(NHDR) = MESS(2:)
      NHDR = NHDR + 1

      CALL XVP('INP',FILENAME,ICNT)	!INPUT FILENAME
      FILEMSG='INPUT = '//FILENAME
      HEADR(NHDR) = FILEMSG
      NHDR = NHDR + 1

      FILEMSG='PLOT  = '//PLOTFILE	!OUTPUT PLOT FILENAME
      HEADR(NHDR) = FILEMSG
      NHDR = NHDR + 1

C-----PLOT THE LABELS
      CALL OTFLABS(INUNIT,HEADR, NHDR, *996) 
C
C PLOT AREA PROCESSED, TRANSFORM SIZE, MEAN AND SIGMA

      HEADR(NHDR) = MSG2(2:)
      NHDR = NHDR + 1
      HEADR(NHDR) = MSG3(2:)
      NHDR = NHDR + 1
      HEADR(NHDR) = MSG5(2:)
      NHDR = NHDR + 1
      HEADR(NHDR) = MSG6(2:)
      NHDR = NHDR + 1

C PLOT PARM 'TITLE'
      IF (NTITLE.NE.0) THEN
        HEADR(NHDR) = HTITLE
        NHDR = NHDR + 1
      END IF

      NHDR = NHDR-1
      CALL HEADER(HEADR,NHDR,0) !WRITE HEADR ARRAY TO PLOT FILE.

C       PLOT THE X & Y AXIS LABELS
      CALL AXESTITLES( HTITX, TITY(2:),90,  ' ', 0)

C-----DOUBLE THE NUMBER OF POINTS IN THE PLOT BUFFER AND INTERPOLATE
      DO J=1,NMID
         PLAT(2*J-1)=POW(J)
      END DO

      PLAT(2)=0.375*POW(1)+0.75*POW(2)-0.125*POW(3)
      PLAT(4)=0.5*(POW(2)+POW(3))
      PLAT(NTOT)=-0.125*POW(NMID-2)+0.75*POW(NMID-1)+0.375*POW(NMID)
      K=NMID-2

      DO J=3,K
         PLAT(2*J)=-0.0625*(POW(J-1)+POW(J+2))+0.5625*(POW(J)+POW(J+1))
      END DO

C-----THERE ARE NOW NTOT POINTS IN THE BUFFER
      VSCL=1.0
C      K=(2*NMID-1)*fmax/0.5        !change number of pts to do if fmax ne .5
      K=NTOT*fmax/0.5               !change number of pts to do if fmax ne .5
      X=0.0
      Y=YLEN
c
C-----MOVE TO (X,Y)
      CALL PLOT(X,Y,3)

      DO J=1,K
	 X = (J-1)*fmax/K          ! use application units.
         Y=VSCL*PLAT(J)
         IF(Y.GT.YLEN) Y=YLEN
         IF(Y.LT.0.0) Y=0.0
         CALL PLOT(X,Y,2)          ! DRAW TO (X,Y)
      END DO

	CALL PLOT(0.,0.,999)       ! TERMINATES ALL PLOTS

	END IF
C------------------------------------------------------

	CALL XVCLOSE(INUNIT,STATUS,' ')
	RETURN

997	CALL XVMESSAGE('REQUESTED AREA OUTSIDE INPUT PICTURE',' ')
	CALL ABEND
998	CALL XVMESSAGE('ERROR OPENING/WRITING TABLE FILE',' ')
	CALL PRNT(4,1,J,'IOSTAT=.')
	CALL ABEND
996	CALL ABEND
	END

c *********************************************************************
      SUBROUTINE CMAG(NMID,COMP,POW)
C       has to use CCOMP instead of COMP, otherwise there will be an error 
C       of multiple declaration of names
C       add the results of one line's transform to a cumulative power spectrum
C       NMID  = number of frequencies to be updated 
C       COMP  = input array of data (COMPLEX * 8)
C       POW   = power spectrum array
C       POW(I) = POW(I) + ABS(COMP(I))**2
      REAL * 4 POW(1)
      COMPLEX * 8 COMP(1)
c-----sum the amplitudes of the complex data
      DO I=1,NMID
       POW(I)=POW(I)+CABS(COMP(I))
c       POW(I)=POW(I)+CABS(COMP(I))**2            wrong
      END DO
      RETURN
      END

c *************************************************************
	SUBROUTINE OTFLABS(INU,HEADR,ICNT,*)
	IMPLICIT INTEGER*4 (A-Z)
	INTEGER      LV2(60),LPL(60),ICNT
	CHARACTER*72 V1(60),V2(60),PL(60),HT(60)
        CHARACTER*80 HEADR(70)

	CALL LABPROC(INU,V1,NV1,V2,LV2,NV2,PL,LPL,NPP,HT,NH)

	IF (NV1 .GT. 0) THEN
	 DO I=1,NV1
            HEADR(ICNT)=V1(I)
            ICNT=ICNT+1
	 END DO

	ELSE IF (NPP .GT. 0) THEN
	 DO I=1,NPP
            HEADR(ICNT)=PL(I)
            ICNT=ICNT+1
	 END DO

	ELSE IF (NV2 .GT. 0) THEN
	 DO I=1,NV2
            HEADR(ICNT)=V2(I)
            ICNT=ICNT+1
	 END DO

	ELSE
	   CALL XVMESSAGE('LABEL ERROR',' ')
	   RETURN 1
	END IF

	IF (NH .GT. 0) THEN
	 DO I=1,NH
            HEADR(ICNT)=HT(I)
            ICNT=ICNT+1
	 END DO
	END IF

	RETURN
	END
C------------------------------------------------------------------
	SUBROUTINE KVPAIR(IU,PAIR,LNG,NUM)
C-------EXTRACTS KEYWORD=VALUE STRINGS FROM FIRST PROPERTY LABEL
	IMPLICIT INTEGER*4 (A-Z)
	INTEGER*4 LNG(60)
	character*72 pair(60)
	character*2048 buf
	character*2048 UPBUF   !UPPER case version of data in buf.  Label
                               !info is mixed case under UNIX.  This way
                               !we don't have to test for PROPERTY or Property.

	max=2048
        buf = ' '              ! This blanks all of buf.
	call xlgetlabel(iu,buf,max,ist)
        UPBUF = buf            !copy data
        call UPRCASE(UPBUF)  !convert to upper case for case insensitive tests
	i=0
	b = index(UPBUF(1:),'PROPERTY')
	IF (B .EQ. 0) GO TO 100

50	L=INDEX(UPBUF(B+1:),' ')
	L=B+L
	Q=INDEX(UPBUF(B+1:),'''')
	Q=B+Q
	IF(Q .LT. L) THEN		!ARE THERE QUOTES IN THE VALUE?
		Q2=INDEX(UPBUF(Q+1:),'''')
		Q2=Q+Q2
		L=INDEX(UPBUF(Q2+1:),' ')
		L=Q2+L
	END IF
	E=L-1
C-------STOP WHEN REACH A 'TASK' OR NEXT PROPERTY LABEL
	IF(UPBUF(B:B+3).EQ. 'TASK') GO TO 100
	IF(I .GT. 1 .AND. UPBUF(B:B+3).EQ. 'PROP') GO TO 100
	I=I+1
	KK=E-B+1
	LNG(I) = KK			!STORE LENGTH
	PAIR(I) = BUF(B:E)        !STORE STRING (mixed case).
	IF(E .EQ. MAX) GO TO 100        !END OF LABEL?
	DO J=E+1,MAX			!LOOK FOR NEXT STRING
	B=J
	IF(BUF(J:J) .NE. ' ') GO TO 50
	END DO
100	NUM=I
	RETURN
	END
c ************************************************************
	subroutine V2PAIR(IU,PAIR,LNG,NUM)
C-------RETURN THE KEYWORD=VALUE PAIRS FROM THE FIRST V2 TASK
	IMPLICIT INTEGER*4 (A-Z)
	INTEGER*4 LNG(60)
	character*72 pair(60)
	character*2048 buf
	character*2048 UPBUF   !UPPER case version of data in buf.  Label
                               !info is mixed case under UNIX.  This way
                               !we don't have to test for PROPERTY or Property.

	max=2048
        buf = ' '              ! This blanks all of buf.
	call xlgetlabel(iu,buf,max,ist)
        UPBUF = buf            !copy data
        call UPRCASE(UPBUF)  !convert to upper case for case insensitive tests

	i=0
	IF (index(UPBUF(1:),'PROPERTY') .GT. 0) GO TO 100
	B = index(UPBUF(1:),'TASK')
	IF (B .EQ. 0) go to 100

50	L=INDEX(UPBUF(B+1:),' ')
	L=B+L
	Q=INDEX(UPBUF(B+1:),'''')
	Q=B+Q
	IF(Q .LT. L) THEN
		Q2=INDEX(UPBUF(Q+1:),'''')
		Q2=Q+Q2
		L=INDEX(UPBUF(Q2+1:),' ')
		L=Q2+L
	END IF
	E=L-1
C-------STOP WHEN ENCOUNTER THE SECOND TASK
	IF(I .GT. 1 .AND. UPBUF(B:B+3).EQ. 'TASK') GO TO 100
	I=I+1
	KK=E-B+1
	LNG(I) = KK			!STORE LENGTH
	PAIR(I) = BUF(B:E)	!STORE STRING  (mixed case)
	IF(E .EQ. MAX) GO TO 100	!END OF LABEL?
	DO J=E+1,MAX			!LOOK FOR NEXT STRING
	B=J
	IF(BUF(J:J) .NE. ' ') GO TO 50
	END DO
100	NUM=I
	RETURN
	END

      SUBROUTINE LABPROC(IUNI,LABV1,NV1,VPAIR,LV2,NV2,PPAIR,LPL,
     1                   NPP,HSTRY,NH)
      IMPLICIT INTEGER(A-Z)
      INTEGER INSTANCES(20),LV2(60),LPL(60),STAT
      CHARACTER*8 TASKS(20)
      CHARACTER*12 UNAME
      CHARACTER*28 TIME
      CHARACTER*72 HBUF,BL
      CHARACTER*72 LABV1(60),HSTRY(60),VPAIR(60),PPAIR(60)
      CHARACTER*4320 LABS

c--------------------------------------------------------------
c-------get all vicar1 labels
	NV1=60
        CALL VIC1LAB(IUNI,STAT,NV1,LABS,60)
	DO J=1,NV1
           LABV1(J) = LABS(72*(J-1)+1:72*J)  !BREAK INTO PIECES 72 CHARS LONG.
	END DO

c----------------------------------------------------------------
c-------get keyword=value pairs from vicar property labels
	NPP=0
	IF (NV1 .EQ. 0) call kvpair(IUNI,ppair,LPL,npp)

c----------------------------------------------------------------
c-------get keyword=value pairs from first vicar2 task
	NV2=0
	IF (NV1 .EQ. 0 .AND. NPP .EQ. 0) 
     1            call v2pair(IUNI,vpair,LV2,nv2)

c----------------------------------------------------------------

c-------get user and date for each vicar2 history task
        BL(1:36) = '------------------------------------'
        BL(37:72) = '------------------------------------'
	BL(1:1) = ' '
	BL(6:10) = 'TASK:'
	BL(23:27) = 'USER:'

	nh=20                             !EXTRACT VIC*2 LAB
	CALL XLHINFO(IUNI,TASKS,INSTANCES,NH,STAT,' ')

	DO 801 J=1,NH
         LD = 0
         LU = 0
	CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',TASKS(J),
     *            'INSTANCE',INSTANCES(J),'LENGTH',LU,
     *            'FORMAT','STRING','ULEN',12,' ')
         IF (STAT .NE. 1) 
     *     CALL XVMESSAGE('Warning: USER label item not found',' ')
        CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',TASKS(J),
     *            'INSTANCE',INSTANCES(J),'LENGTH',LD,
     *            'FORMAT','STRING','ULEN',28,' ')
         IF (STAT .NE. 1) 
     *     CALL XVMESSAGE('Warning: DAT_TIM label item not found',' ')

	HBUF(1:72) = BL(1:72)
	HBUF(11:18) = TASKS(J)
	if (LU .GT. 0) HBUF(28:28+LU-1) = UNAME(1:LU)
	IF (LD .GT. 0) HBUF(40:40+LD-1) = TIME(1:LD)
801	HSTRY(J) = HBUF

      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create power.imake
#define  PROGRAM   power

#define MODULE_LIST power.f

#define MAIN_LANG_FORTRAN
#define R2LIB 
#define USES_FORTRAN

#define LIB_MOTIF
#define LIB_XRT_GRAPH

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create power.pdf
process help=*
 PARM INP         STATUS=STRING          COUNT=1
 PARM SIZE        STATUS=INTEGER         COUNT=4          DEFAULT=(1,1,0,0)
 PARM SL          STATUS=INTEGER         COUNT=1          DEFAULT=1
 PARM SS          STATUS=INTEGER         COUNT=1          DEFAULT=1
 PARM NL          STATUS=INTEGER         COUNT=1          DEFAULT=0
 PARM NS          STATUS=INTEGER         COUNT=1          DEFAULT=0
 PARM EXPONENT    STATUS=INTEGER  COUNT=(0:1) VALID=(3:10) DEFAULT=--
 PARM SCALE       STATUS=REAL                             DEFAULT=2.
 PARM DNSCALE     STATUS=REAL                             DEFAULT=1.0
 PARM FMAX        STATUS=REAL                             DEFAULT=0.5
 PARM YLEN        STATUS=REAL                             DEFAULT=7.0
 PARM TYPE	  STATUS=KEYWORD  COUNT=(0:1) VALID=(PLOT) DEFAULT=--
 PARM FILE	  STATUS=STRING          COUNT=0:1        DEFAULT=-- 
 PARM TITLE       STATUS=(STRING,52)     COUNT=0:1        DEFAULT=--
 PARM TITLEX      STATUS=(STRING,52)     COUNT=0:1 +
                  DEFAULT="FREQUENCY (CPS)"         
 PARM TABLE       STATUS=STRING		COUNT=0:1	DEFAULT=--
 PARM COLUMNS     STATUS=KEYWORD  COUNT=(0:1) VALID=(COLHDR,NOCOLHDR) +
                  DEFAULT=COLHDR
 PARM NODISP  STATUS=KEYWORD COUNT=(0:1) VALID=NODISP   DEFAULT=--
 END-PROC
.TITLE
 VICAR Application program POWER
.HELP
 "POWER" computes the 1 - dimensional power spectrum of a specified portion of 
  a picture.  

  Although the output is called 'power spectrum', the value computed is
  the square root of the power spectrum (the peak-to-peak amplitude in DN).
  Therefore, strictly speaking the output is an 'amplitude spectrum'.
  
  This output can be:
  1.  Written to a plot file which can be plotted on a POSTSCRIPT printer 
      or other POSTSCRIPT device.
  2.  Plotted directly to an X image display device.
  3.  Written as a tab-delimited ASCII file for use by commmerical packages.

 OPERATION:
  
  POWER uses the fast Fourier Transform to produce the power spectrum of each
  line of a specified portion of an input picture.  POWER produces a single 
  resultant power spectrum by averaging these together as a function of 
  frequency. The desired portion of the picture is specified in the size field.

  If a transform size (EXPONENT) is specified larger than the input sample 
  size, the necessary additional samples are generated by averaging the 
  available input data so to minimize sin(x)/x ringing. If it is defaulted,
  the transform size will be the largest power of 2 less than the input
  sample size.

  POWER will take the image format from the input's system label.

  The parameter TABLE is used to specify the name of the file 
  to contain the tab-delimited ASCII table of Frequency and Power values.

  If TYPE=PLOT is selected, the plot is generate as a POSTSCRIPT file.  (See 
  FILE parameter.)  The plot is also displayed on the X display unless the
  NODISP keyword is specified.  (An X display appears to be necessary even
  if NODISP is specified and no plot is displayed.)

  FILE='filename' is used to name the output plot file.  Specifying FILE 
  implies TYPE=PLOT. The default value of FILE is POWER.PLT.

  Example to plot on a X device:
  
         (..select an X display if not already done..)
         power inp=a.img 'plot
         
  Example to plot on the postscript printer:

         (..select an X display if not already done..)
         power inp=a.img 'plot FILE=P.PLT
         (..send P.PLT to a POSTSCRIPT printer using some OS command..)

  Example to request only tab-delimited ASCII file output:

 	 power inp=a.img table=pow.tbl


  ORIGINAL PROGRAMMER: T. C. Rindfleisch
  CURRENT COGNIZANT PROGRAMMER: F. F. Moss   

HISTORY
     04/29/97  ... SP  ... Made portable for UNIX.  Adapted for XRT pltting
                           package.  Changed COUNT for FILE parameter to 0:1
                           so that POWER can run without setting an X DISPLAY
                           by specifying neither 'PLOT nor FILE.  (Previously
                           the COUNT received by the program was always 1.)
     11/16/95  ... CCA ... MADE SCAL R*4
     07/18/94  ... CCA ... ADD OPTION OF NO TABLE COLUMN HEADERS
     10/20/94  ... CCA ... Corrected accumulation of amplitude
     06/14/94  ... CCA ... FIXED FMAX, DELETED HISTOGRAM PLOT
     04/11/94  ... CCA ... ADDED DNSCALE
     02/22/94  ... CCA ... MAJOR CODE CLEANUP, REWORK OF LABEL HANDLING,
			   TEST FILE AND PLOTTING OF LABELS
     11/22/93  ... CCA ... ADD ASCII TABLE OUTPUT, CHECKS FOR OPEN
                           ERRORS, FIX TEST, MOD SELECTION OF PLOT
                           OR PRINT
     09/01/84  ... FFM ... CONVERT FROM IBM TO VAX
               T. RINDFLEISCH ... ORIGINAL RELEASE

.LEVEL1

.VARIABLE INP
 input data set

.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
You can enter SL,SS,NL,
and NS together as SIZE, OR
enter the SL,SS,NL, and NS
parameters separately.
.VARIABLE SL
Starting line number
.VARIABLE SS
Starting sample number
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples

.VARIABLE EXPONENT
 transform size

.VARIABLE SCALE
 output plot amplitude scale

.VARIABLE DNSCALE
 scaling factor of input image

.VARIABLE FMAX
 highest frequency in the 
 plotted spectrum

.VARIABLE YLEN
 length of Y axis

.VARIABLE TYPE
 specification 
 of plotter output

.VARIABLE FILE
 specification of output 
 plot filename

.VARIABLE TITLE
 user specified label to both
 printer and plotter

.VARIABLE TITLEX
 user specified label of X
 axis to the plotter only

.VARIABLE TABLE
 name of file to contain
 tab-delimited plot data

.VARIABLE COLUMNS
 selects table column
 headers or not

.VARIABLE NODISP
 If present, no plot
 is displayed on screen

.LEVEL2

.VARIABLE INP
 string - input data set (only 1 input allowed)

.vari size
The size parameter determines the boundaries in the input
file from which POWER is to operate.  It is specified
as  (SL,SS,NL,NS), where
	SL is the starting line 
	SS is the starting sample
	NL is the number of lines to be copied
	NS is the number of samples (pixels) in each line

.VARIABLE EXPONENT
 EXPONENT=E where E is an integer between or equal to 3 and 10, specifying
 the exponent of 2 for the desired 1 - dimensional transform size. If half
 word data, E referes to the transform size in samples and not bytes.
 The default is the biggest power of 2 that is less than the number of input
 samples.

.VARIABLE SCALE
 REAL - DEFAULT=2.0
 SCALE = S where S is a real specifying the output plot amplitude scale.

.VARIABLE DNSCALE
 REAL - DEFAULT=1.0
 Specifies that the input DNs have been scaled up from the original values,
 and that the scale is to be removed to yield plots scaled to the original
 DNs.  An example would be scaling done during filtering.

.VARIABLE FMAX
 FMAX = F where F is a floating point number specifying the highest frequency
 in the spectrum (the nyquist or aliasing frequency). It is numerically equal
 to 1/2 of the reciprocal of the pixel-to-pixel spacing measured in whatever
 units are used. The length of the frequency axis on the line printer is
 2**(E-1) lines. It is printed 6 data points per inch, and the annotation 
 on the frequency axis is given by F/2**(E -1) DN every line, where F and E 
 are given by the FMAX and EXPONENT keywords, respectively. Default is F=0.5,
 corresponding to a pixel spacing of unity.  

 FMAX applies only to the plots, the output tables ignore it.

.VARIABLE YLEN
 specifies the length in inches of the Y axis.(default=7.0,max=30.)

.VARIABLE TYPE
 KEYWORD - VALID=(PLOT)
 Specifies POSTSCRIPT plotting.  If 'PLOT or FILE are specified,
 then the program needs permission to write to an X display.
 If neither 'PLOT nor FILE are specified, an X device is not required and
 no POSTSCIPT plotting is generated. (See also under NODISP parameter.)

.VARIABLE FILE
 Specifies the filename of the file to which the POSTSCRIPT plot is written.
 If FILE is specified, PLOT is unnecessary.  Defaults to POWER.PLT.

.VARIABLE TITLE
 string - this parameter is used to add labeling to both the line printer
 graph and plotter graph. The maximum number of characters in the added
 label is 52. The title will be placed at the top of all the graphs gen-
 erated. No title is provided if TITLE is defaulted.

.VARIABLE TITLEX
 string - this parameter is used to change the X - axis label in the plotter
 graph. The maximum number of characters in the string is 52. The default
 is FREQUENCY(CPS) where CPS can be interpreted as cycles per second or 
 cycles per sample.

.VARIABLE TABLE
 string - this parameter specifies that the plot data be output to a file
 in tab-delimited ASCII form.  It also specifies the name of the file to 
 contain the plot data.  The first column of data is the FREQ
 value.  A tab character separates this from the second column of data. 
 The second column contains the square root of the power spectrum.

.VARIABLE COLUMNS
 keyword - COLHDR specifies to put column headers into the ASCII table.
           NOCOLHDR specifies no headers in the table. 
 The headers are "FREQUENCY" and "RMS POWER".  
 Default is to put headers in the table.

.VARIABLE NODISP
 Keyword--Optional
 If present, no display is shown in interactive mode and output plot files
 are automatically saved.  When not present, plot is displayed and files are
 saved.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpower.pdf
procedure
!
refgbl ($echo,$becho)
refgbl $syschar
LOCAL DIR    TYPE=STRING 
LOCAL INPIC   TYPE=STRING
body
write  "BEFORE running this test, make sure your process has an X display."
!  This is mainly an issue for VMS.
!
let _onfail="continue"
let $echo="yes"
let $becho="yes"
if ($syschar(1) = "UNIX")
   LET DIR   ="/project/test_work/testdata/cassini/iss/"
else 
   LET DIR   ="WMS_TEST_WORK:[TESTDATA.CASSINI.ISS]"
end-if

LET INPIC = "&DIR"//"sum2.38"      

! THIS IS A TEST SCRIPT FOR THE PROGRAM - POWER

let $becho="no"
write "TESTS USING AN IMAGE CONTAINING A ONE CYCLE SINE WAVE OVER 256 SAMPLES"
write "THE MEAN IS 100 DN AND THE AMPLITUDE (PEAK-TO-PEAK) IN DN IS 200."
write "So the first element of the table should be the mean:  100 and the" 
write "max value should be about 200 at a frequency of 1/256 = .0039"
let $becho="yes"

f2 out=A size=(1,1,50,256) 'half func="100*(1+sin(3.1415926*samp/128.))"
power A 'plot file=tstpower.sin1 table=pow.sin1 scale=30 fmax=.1
typetext pow.sin1

f2 out=A size=(1,1,50,256) 'byte func="100*(1+sin(3.1415926*samp/128.))"
power A 'plot file=tstpower.sin2 table=pow.sin2 scale=30 fmax=.1
typetext pow.sin2

let $becho="no"
write "TRY A SINE WAVE OVER 256 LINES"
let $becho="yes"

f2 out=A size=(1,1,256,50) 'byte func="100*(1+sin(3.1415926*line/128.))"
power A 'plot file=tstpower.sin3 table=pow.sin3 scale=20 fmax=.1
typetext pow.sin3

let $becho="no"
write "HALFWORD DATA TESTS on actual data"
let $becho="yes"

power &INPIC (11,11,50,140) 'PLOT file=tstpower.case1

power &INPIC (1,1,100,500) 'PLOT FILE=tstpower.case2 EXPONENT=9 TITLEX="FREQ"

power &INPIC (1,1,150,64) 'PLOT EXPONENT=6 FMAX=2.0 TITLE="FLORANCE" 
if ($syschar(1) = "UNIX")
   ush mv POWER.PLT tstpower.case3
else
   dcl ren POWER.PLT tstpower.case3
end-if

f2 &INPIC T FUNC="IN1*20."
power T (1,1,150,64) 'PLOT EXPONENT=6 FMAX=2.0 TITLE="FLORANCE" DNSCALE=20. +
          file=tstpower.case4

power &INPIC (1,1,120,50) EXPONENT=6 'PLOT TITLE="PLOT ONLY" file=tstpower.case6

power &INPIC (1,1,120,50) TABLE=POW.PLT 
typetext POW.PLT

let $becho="no"
write "BYTE DATA TESTS"
let $becho="yes"

LET INPIC = "&DIR"//"grid.byte"      

power &INPIC (300,300,100,100) EXPONENT=6  TITLE="BYTE TEST"

power &INPIC (300,300,100,100) EXPO=6 TABLE=POW.PLT2 
typetext POW.PLT2

power &INPIC (300,300,100,100) EXPO=6 TABLE=POW.PLT3 'nocolhdr 
typetext POW.PLT3

end-proc
$ Return
$!#############################################################################
