$!****************************************************************************
$!
$! Build proc for MIPL module picsum
$! VPACK Version 1.9, Tuesday, May 27, 2003, 15:48:46
$!
$! Execute by entering:		$ @picsum
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
$ write sys$output "*** module picsum ***"
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
$ write sys$output "Invalid argument given to picsum.com file -- ", primary
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
$   if F$SEARCH("picsum.imake") .nes. ""
$   then
$      vimake picsum
$      purge picsum.bld
$   else
$      if F$SEARCH("picsum.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake picsum
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @picsum.bld "STD"
$   else
$      @picsum.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create picsum.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack picsum.com -mixed -
	-s picsum.f -
	-i picsum.imake -
	-p picsum.pdf -
	-t tstpicsum.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create picsum.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C Radiometric Calibration Program PICSUM -- Adds multiple frames together.
C        PICSUM  (I1,I2,I3,...,In)  OUT  user-parameters...
C Inputs may be byte or halfword.
C
C Cassini data is 12 bits and so the range for the despike routine is 0 - 4095.

      SUBROUTINE MAIN44

         INTEGER*2 LTBL(0:4095),HTBL(0:4095), IN(32768)
         INTEGER OUNI,IUNI(30),IUNIT,XVPTST,ASCALE,PICSCALE,TFLAG
         INTEGER BUF(30), EXPOS, EXPOS1, SCLK, 
     &           SCLKS(30), TNL, TNS

         REAL*4 PAR(2),LSCALE

         CHARACTER*100 FN, FNAME
         CHARACTER*8 FMT, TFMT
         CHARACTER*5 PROJECT
         CHARACTER*5 CAMERA, CAMERA1, FILT1, FILT11, FILT2, FILT21
         CHARACTER*5 GAIN, GAIN1
         CHARACTER*4 MODE, MODE1

         CALL IFMESSAGE ('PICSUM version May 27, 2003')

         CALL XVPCNT('INP',NI)	!Number of input datasets

C        Open all inputs and print out picture labels...

         CALL XVP('LIST',FNAME,NJ)		! use a search list or INP

         IF ((NI .NE. 1) .AND. (NJ .NE. 0)) THEN
            CALL XVMESSAGE('Give only one INP if LIST is given',' ')
            CALL ABEND()
         ENDIF

         IF (NI .NE. 1) THEN 
            DO I=1,NI
               CALL XVUNIT(IUNI(I),'INP',I,IND,' ')
               CALL XVOPEN (IUNI(I),IND,'OPEN_ACT','SA','IO_ACT','SA',
     &                      'U_FORMAT','HALF',' ')
               IF (I .EQ. 1) 
     &            CALL GETPROJ(IUNI(I),PROJECT,ICAM,IFRM,ISTAT)
               IF (PROJECT .EQ. 'CASSI') THEN
                  CALL CASSIPROC(IUNI(I),ISTAT,SGC,MINT,CAMERA,GAIN,
     &                       MODE,EXPOS,FILT1,FILT2,SCLK)
                  IF (I .EQ. 1) THEN
                     CAMERA1 = CAMERA
                     GAIN1 = GAIN
                     MODE1 = MODE
                     EXPOS1 = EXPOS
                     FILT11 = FILT1
                     FILT21 = FILT2
                  ELSEIF ((CAMERA1 .NE. CAMERA) .OR.
     &                    (GAIN1 .NE. GAIN) .OR. 
     &                    (MODE1 .NE. MODE) .OR. 
     &                    (EXPOS1 .NE. EXPOS) .OR. 
     &                    (FILT11 .NE. FILT1) .OR. 
     &                    (FILT21 .NE. FILT2)) THEN
                     CALL XVMESSAGE 
     & ('CAMERA, GAIN, MODE, EXPOS, FILT1, FILT2 must be the same',' ')
                     CALL ABEND()
                  ENDIF
                  SCLKS(I) = SCLK
               ELSE
                  CALL LABPROC(IUNI(I),PROJECT,ISTAT,sgc,mint)
               ENDIF
            ENDDO
         ELSEIF (NJ .NE. 0) THEN		!open search list

C Read INP just to get the VICAR label put on the output
            CALL XVUNIT(IUNIT,'INP',1,IND,' ')
            CALL XVOPEN(IUNIT,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &                'U_FORMAT','HALF',' ')
            OPEN(UNIT=99,NAME=FNAME,STATUS='OLD',ERR=950)
            READ(99,FMT=1) FN                     !SKIP FIRST LINE
1	    FORMAT(A)
	
	    DO I=1,31				!open files in list
	       READ(99,FMT=1,END=11) FN
	       IF (I .GE. 31) GO TO 920
	       NI = I
	       CALL XVUNIT(IUNI(I),'NONE',I,IST,'U_NAME',FN,' ')
               CALL XVOPEN(IUNI(I),IST,'OPEN_ACT','SA','IO_ACT','SA',
     &                'U_FORMAT','HALF',' ')
               IF (I .EQ. 1) 
     &            CALL GETPROJ(IUNI(I),PROJECT,ICAM,IFRM,ISTAT)
               IF (PROJECT .EQ. 'CASSI') THEN
                  CALL CASSIPROC(IUNI(I),ISTAT,SGC,MINT,CAMERA,GAIN,
     &                       MODE,EXPOS,FILT1,FILT2,SCLK)
                  IF (I .EQ. 1) THEN
                     CAMERA1 = CAMERA
                     GAIN1 = GAIN
                     MODE1 = MODE
                     EXPOS1 = EXPOS
                     FILT11 = FILT1
                     FILT21 = FILT2
                  ELSEIF (CAMERA1.NE.CAMERA .OR. GAIN1.NE.GAIN .OR.
     &                    MODE1.NE.MODE .OR. EXPOS1.NE.EXPOS .OR.
     &                    FILT11.NE.FILT1 .OR. FILT21.NE.FILT2) THEN
                     CALL XVMESSAGE
     &  ('CAMERA, GAIN, MODE, EXPOS, FILT1, FILT2 must be the same',' ')
                     CALL ABEND()
                  ENDIF
                  SCLKS(I) = SCLK
               ELSE
                  CALL LABPROC(IUNI(I),PROJECT,ISTAT,sgc,mint)
               ENDIF
	    ENDDO
11	    CONTINUE
         ELSE
            CALL XVMESSAGE 
     &         ('Specify at least two INPs or one INP with LIST',' ')
            CALL ABEND
         END IF

C        Get size and format of first input image NLxNS....
         CALL XVGET(IUNI(1),IND,'FORMAT',FMT,'NL',NL,'NS',NS,' ')
         IF ((FMT .NE. 'BYTE') .AND. (FMT .NE. 'HALF')) THEN
            CALL XVMESSAGE('***Illegal input format',' ')
            GOTO 999
         ENDIF
         DO I=1,NI-1
            CALL XVGET(IUNI(I),IND,'FORMAT',TFMT,'NL',TNL,'NS',TNS,' ')
            IF (TFMT.NE.FMT .OR. TNL.NE.NL .OR. TNS.NE.NS) THEN
               CALL XVMESSAGE
     &         ('***All inputs must be of the same size and format',' ')
               GOTO 999
            ENDIF
         ENDDO

         ASCALE = XVPTST('ASCALE')		!Auto picture scale flag
         IF (ASCALE.EQ.1) THEN
            XSCALE = 128./NI
            PICSCALE = 128
         ELSE
            PICSCALE = NI
         ENDIF

         TFLAG = 0				!Threshold flag for despiking
         CALL XVPARM('TSCALE',PAR,ICNT,IDEF,2)
         IF (IDEF.EQ.0) THEN
            TFLAG = 1			!Turn flag on
            LSCALE = PAR(1)/SQRT(SGC)
            HSCALE = PAR(2)/SQRT(SGC)
            CALL XVPARM('MINT',ival,icnt,idef,1)
            IF (IDEF.NE.1) MINT=IVAL
            CALL PRNT(4,1,MINT,'MINT=.')

            DO I=0,4095
               DN = SQRT(FLOAT(I))
               LTBL(I) = LSCALE*DN
               HTBL(I) = HSCALE*DN
               IF (LTBL(I) .LT. MINT) LTBL(I)=MINT
               IF (HTBL(I) .LT. MINT) HTBL(I)=MINT
            ENDDO
         ENDIF

         CALL XVUNIT(OUNI,'OUT',1,IND,' ')
         CALL XVOPEN(OUNI,IND,'OPEN_ACT','SA','IO_ACT','SA',
     &         'OP','WRITE','U_NL',NL,'U_NS',NS,'O_FORMAT','HALF',' ')
         CALL XLADD
     &      (OUNI,'HISTORY','PICSCALE',PICSCALE,IND,'FORMAT','INT',' ')
         CALL XLADD(OUNI,'HISTORY','NFRAMES',NI,IND,'FORMAT','INT',' ')
         IF (PROJECT .EQ. 'CASSI') THEN
            CALL SORTIN(SCLKS,NI)
            CALL XLADD(OUNI,'HISTORY','SCLKS',SCLKS,IND,'FORMAT','INT',
     &              'NELEMENT',NI,' ')
         ENDIF
         CALL SUMIT(IUNI,OUNI,IN,IN,BUF,NL,NS,NI,TFLAG,
     &      ASCALE,XSCALE,LTBL,HTBL)
         CALL XVCLOSE(OUNI,IND,' ')
         CALL XVMESSAGE('PICSUM task completed',' ')
         RETURN
CCCCCCCCC
C
920      CALL XVMESSAGE('more than 30 filenames in LIST',' ')
         GO TO 999
950      CALL XVMESSAGE('could not open input list file',' ')
         CALL XVMESSAGE(FNAME,' ')
999      CALL XVMESSAGE('***PICSUM task cancelled',' ')
         END



C Perform the pixel summation
C
      SUBROUTINE SUMIT(IUNI,OUNI,IN,OBUF,BUF,NL,NS,NI,
     &	TFLAG,ASCALE,XSCALE,LTBL,HTBL)

         INTEGER IUNI(NI),OUNI,TFLAG,ASCALE
         INTEGER*2 IN(NS,NI),OBUF(NS),LTBL(0:4095),HTBL(0:4095)
         INTEGER*4 BUF(NI)

         NBAD = 0

         DO 100 L=1,NL
            DO I=1,NI
               CALL XVREAD(IUNI(I),IN(1,I),IND,' ')
            ENDDO

            IF (TFLAG .EQ. 1) THEN    !If thresholds are specified,
               DO I=1,NS
                  NP = 0
                  DO J=1,NI
                     IDN = IN(I,J)
                     IF (IDN.GT.0) THEN
                        NP = NP + 1
                        BUF(NP) = IDN
                     ENDIF
                  ENDDO
                  CALL DESPIKE(BUF,NI,NP,N,IDN,LTBL,HTBL)  !despike data.
                  NBAD = NBAD + (NI-N)	   !Number of bad samples
                  OBUF(I) = MIN0(IDN,32767)
               ENDDO
            ELSE
               DO I=1,NS	    !Else, just add lines together
                  IDN = IN(I,1)
                  DO J=2,NI
                     IDN = IDN + IN(I,J)
                  ENDDO
                  OBUF(I) = MIN0(IDN,32767)
               ENDDO
            ENDIF

            IF (ASCALE .EQ. 1) THEN
               DO I=1,NS
                  IDN = OBUF(I)*XSCALE
                  OBUF(I) = MIN0(IDN,32767)
               ENDDO
            ENDIF
  100    CALL XVWRIT(OUNI,OBUF,IND,' ')

         IF (TFLAG.EQ.1) 
     &      CALL PRNT(4,1,NBAD,'Number of bad pixels=.')
         RETURN
      END



C Scan for noise spikes
C
      SUBROUTINE DESPIKE(BUF,NI,NP,N,IDN,LTBL,HTBL)

         INTEGER BUF(NI)

         INTEGER*2 LTBL(0:4095),HTBL(0:4095)
         INTEGER HTHRESH

         IF (NP.EQ.0) THEN		!If all samples are zero
            N = 0
            IDN = 0		!return zero
            RETURN
         ELSEIF (NP.EQ.1) THEN	!If only one non-zero sample
            N = 1
            IDN = BUF(1)*NI	!then no screening necessary
            RETURN
         ENDIF

C     ....Here if at least two non-zero samples are in BUF
         CALL SORTIN(BUF,NP)	!Sort the samples
         MIX = (NP+1)/2		!Index of median
         MEDIAN = BUF(MIX)		!Median DN value
         ISUM = MEDIAN		!Initialize sum
         N = 1			!Number of points in sum

C     ....Add all samples less than median
         IEND = MIX - 1
         IF (IEND.EQ.0) GOTO 20	!Skip if BUF(1) is median
         LTHRESH = LTBL(MEDIAN)	!Lower threshold

         DO I=1,IEND
            IDN = BUF(I)
            IF (MEDIAN-IDN.LE.LTHRESH) THEN
               N = N + 1
               ISUM = ISUM + IDN
            ENDIF
         ENDDO

C     ....Add all samples greater than median
   20    IBEG = MIX + 1
         HTHRESH = HTBL(MEDIAN)	!Upper threshold

         DO I=IBEG,NP
            IDN = BUF(I)
            IF (IDN-MEDIAN.LE.HTHRESH) THEN
               N = N + 1
               ISUM = ISUM + IDN
            ENDIF
         ENDDO

         IF (N.LT.NI) THEN
            IDN = (FLOAT(NI)*ISUM)/N + 0.5
         ELSE
            IDN = ISUM
         ENDIF

         RETURN
      END



C Print input label
C
      SUBROUTINE LABPROC(IUNI,PROJECT,ISTATUS,SGC,MINT)

         INTEGER LBUF(80)

         CHARACTER*5 PROJECT
         CHARACTER*132 MSG

C     ...Galileo gain ratios for 400K  100K  40K  10K
C
         REAL*4 GLL_GAIN_RATIO(4)
         DATA GLL_GAIN_RATIO/47.091,9.809,4.799,1.0/

         INTEGER MINTHRESH(4)  !minimum threshold (DN)
         DATA MINTHRESH/1,1,1,2/

         REAL*4 GLL_GAIN_CONSTANT !electrons/DN at 10K
         DATA GLL_GAIN_CONSTANT/42.3/ 
 
         EQUIVALENCE (EXPO,IEXPO)


  101    FORMAT(' CAMERA=',I4,' FRAME=',I9,' FILTER=',I1,' GAIN=',I1,
     &    ' EXP=',F8.1,' RATE=',I3)

         IF (ISTATUS.NE.1) THEN
            CALL GETLABCON(IUNI,PROJECT,lbuf,ind)
            ICAM = LBUF(6)
            IFRAME = LBUF(2)
            IFILT = LBUF(4)
            IGAIN = LBUF(7)
            IEXPO = LBUF(3)
            IRATE = LBUF(5)
            WRITE(MSG,101,ERR=10) ICAM,IFRAME,IFILT,IGAIN,EXPO,IRATE
   10       CALL XVMESSAGE (MSG,' ')
         ENDIF

         IF (PROJECT.EQ.'GLL') THEN
            SGC = GLL_GAIN_CONSTANT*GLL_GAIN_RATIO(IGAIN)
            MINT = MINTHRESH(IGAIN)
            IF (MOD(LBUF(17),10).EQ.1)
     &         CALL XVMESSAGE ('EXTENDED-EXPOSURE',' ')
         ELSE
            SGC = 1.
            MINT = 3
         ENDIF
         RETURN
      END



C
C Check Cassini input label
C
      SUBROUTINE CASSIPROC(IUNI,ISTATUS,SGC,MINT,CAMERA,GAIN,MODE,
     &  EXPOS,FILT1,FILT2,SCLK)

         INCLUDE 'cas_isslab'
c         INCLUDE 'cas_isslab.fin'  ! remove before delivery

         INTEGER IND, EXPOS, SCLK

         CHARACTER*132 MSG
         CHARACTER*5 CAMERA, FILT1, FILT2, GAIN
         CHARACTER*4 MODE

C     ...Cassini gain ratios for 1400K  400K  100K  40K
C
         INTEGER MINTHRESH(4)     !minimum threshold (DN)
         DATA MINTHRESH/1,1,1,2/

c         REAL*4 CASSI_GAIN_RATIO(4)
c         DATA CASSI_GAIN_RATIO/17.1,7.9,2.4,1.0/

c         REAL*4 CASSI_GAIN_CONSTANT     !electrons/DN at 24K
c         DATA CASSI_GAIN_CONSTANT/12.6/

c     ...Use System Gain Constant as calculated by CISSCAL/DECAL
	 REAL*4 CASSI_SGC_NAC1(4), CASSI_SGC_NAC2(4)
	 REAL*4 CASSI_SGC_WAC1(4), CASSI_SGC_WAC2(4)
         DATA CASSI_SGC_NAC1/233.04, 98.88, 30.27, 12.85/
         DATA CASSI_SGC_NAC2/219.80, 99.32, 29.15, 13.93/
         DATA CASSI_SGC_WAC1/210.566, 85.09, 27.68, 11.85/
         DATA CASSI_SGC_WAC2/194.30, 90.13, 27.66, 11.74/

         IF (ISTATUS.EQ.1) THEN
            CALL XVMESSAGE
     &         ('Must have a correct Cassini Vicar label',' ')
            CALL ABEND()
         ELSE
            CALL ABLE97(IND,IUNI)
            SCLK = LAB_SCLK
            CAMERA = LAB_CAMERA
            GAIN = LAB_GAIN
            MODE=LAB_MODE
            EXPOS = LAB_EXPOS
            FILT1 = LAB_FILTER1
            FILT2 = LAB_FILTER2

            WRITE
     &        (MSG,201,ERR=20)SCLK,CAMERA,GAIN,MODE,EXPOS,FILT1,FILT2

  201       FORMAT('SCLK=',I9,' CAM=',A5,' GAIN=',A5,' MODE=',A4,
     &' EXP=',I7,' FILT1=',A5,' FILT2=',A5)

   20       CALL XVMESSAGE (MSG,' ')
            IF (LAB_GAIN .EQ. '1400K'
     &          .OR. LAB_GAIN(1:3) .EQ. '215') THEN
               IGAIN = 1
            ELSEIF (LAB_GAIN .EQ. '400K'
     &              .OR. LAB_GAIN(1:2) .EQ. '95') THEN
               IGAIN = 2
            ELSEIF (LAB_GAIN .EQ. '100K'
     &              .OR. LAB_GAIN(1:2) .EQ. '29') THEN 
               IGAIN = 3
            ELSE
               IGAIN = 4
            ENDIF
            IF (LAB_CAMERA .EQ. 'ISSNA') THEN
	       IF (LAB_OPTTEMP .LE. 15.) THEN
                  SGC = CASSI_SGC_NAC1(IGAIN)
               ELSE
                  SGC = CASSI_SGC_NAC2(IGAIN)
               ENDIF
            ELSE
	       IF (LAB_OPTTEMP .LE. 15.) THEN
                  SGC = CASSI_SGC_WAC1(IGAIN)
               ELSE
                  SGC = CASSI_SGC_WAC2(IGAIN)
               ENDIF
            ENDIF
c            SGC = CASSI_GAIN_CONSTANT*CASSI_GAIN_RATIO(IGAIN)
            MINT = MINTHRESH(IGAIN)
         ENDIF
         RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create picsum.imake
#define PROGRAM picsum

#define MODULE_LIST picsum.f

#define FTNINC_LIST cas_isslab

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN

/* #define LIB_LOCAL /* remove before delivery */
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
$ Return
$!#############################################################################
$PDF_File:
$ create picsum.pdf
process help=*
PARM INP    TYPE=STRING        COUNT=(1:30)
PARM OUT    TYPE=STRING        COUNT=1
PARM ASCALE TYPE=KEYWORD       COUNT=(0:1) VALID=ASCALE	  DEFAULT=--
PARM TSCALE TYPE=REAL          COUNT=(0:2) 		  DEFAULT=--
PARM MINT   TYPE=INTEGER       COUNT=(0:1) VALID=(0:4095) DEFAULT=3
PARM LIST   TYPE=STRING        COUNT=0:1                  DEFAULT=--
!# parm inp(2-30) hints=default
END-PROC
.TITLE
VICAR Program PICSUM
.HELP
PURPOSE:

PICSUM will add up to 30 byte or halfword images together. The input samples
may be optionally screened for high and/or low noise spikes.  PICSUM was
originally written to support Galileo camera calibration.  

Reference: D-4264  MIPL Software Structural Design for the Instrument
 Calibration of GLL SSI Science Processing.

EXECUTION:

    PICSUM  (I1,I2,I3,...,In)  OUT  user-parameters...

The input images may be byte or halfword format.  The output image is in
halfword format.

All input images must be the same size and format.  The product of the number
of samples on each line (NS) and the number of input images (NI) must be
less than 32768.  For example, if 30 images are input, then the number of
samples must be less than 1092.

For Cassini images, camera, gain, mode, expos, filt1, and filt2 must be the
same for all images.

The input images can be given using a SRCH list (LIST).  However, you must
give one (and only one) file with INP if you use LIST.  That one file should
be the first image listed in your SRCH list.  That one file given with INP
is used only for its VICAR label which will be used for the output image (OUT).

.page
DESPIKE ALGORITHM:

The despike algorithm is invoked by specifying the TSCALE parameter.  The
algorithm identifies noise spikes (e.g. transmission or radiation noise).

For a given pixel location, the DN value from each input image is retrieved and
the median DN value determined (Note that when only two images are input, the
median is always the lower DN).  The DN values from each image is then
compared with the median and all pixels differing from the median by more
than a low and high threshold are rejected.  Also, any input sample less than
or equal to 0 DN is rejected as invalid (assumed to be a data drop-out).

For a given pixel, low and high thresholds are computed as follows:
	LTHRESH = lscale*SQRT(median)/SQRT(C)
	HTHRESH = hscale*SQRT(median)/SQRT(C)
where C is the camera system gain constant in electrons/DN and lscale and
hscale are specified via the TSCALE parameter.  For Galileo images, the system
gain constant is 42.3 for 10K gain-state, 203.0 for 40K, 414.9 for 100K, and
1991.9 for 400K.  For Cassini images, the system gain constant is 42.3 for 24K 
gain-state, 203.0 for 100K, 414.9 for 400K, and 1991.9 for 1400K. 

(**NOTE THIS WILL CHANGE ONCE THE ACTUAL VALUES ARE DETERMINED)

For non-Galileo and non_Cassini images, the system gain constant is assumed
to be 1.0.

The computed thresholds (LTHRESH and HTHRESH) are not allowed to be smaller
than a minimum threshold, as specified by the MINT parameter.  If defaulted,
MINT is assigned the following values:  1 DN for 400K gain-state, 1 DN for
100K, 1 DN for 40K, and 2 DN for 10K.  For non-Galileo and non_Cassini images,
the default is MINT=3.

(**NOTE THIS MAY CHANGE FOR CASSINI)

After all bad samples are eliminated, the remaining samples are summed and the
result scaled appropriately to compensate for the discarded samples.  If all
samples are bad (only true if all data is less than 1 DN) then 0 DN is stored.

.page
OUTPUT PICTURE SCALE

If the ASCALE keyword is specified, the output pixels will be scaled by
the factor 128/N, where N is the number of input frames.

If the output DN value at any sample position exceeds 32767, then it is set
equal to 32767.

The output picture scale (either N or 128/N) and number of input frames
are recorded in the output picture label (label items PICSCALE and NFRAMES).

If the input are Cassini images, SCLKS is also put in the output VICAR
label.  SCLKS are sorted and listed in ascending order.

.page
EXAMPLES

1) PICSUM  (IN1,IN2,IN3,IN4,IN5)  OUT  TSCALE=(3.,3.)

   Five input images are added together to form the output image.  If all
   the input images are in byte format, the despike algorithm is invoked.
   The specified TSCALE values will cause any sample differing from the
   median by more than a 3 sigma shot-noise level to be ignored.

   Suppose that for a given pixel location, the input sample values are
   9,10,12,12,13.  The median is 12.  The computed low and high thresholds
   are 2.  The sample value 9 is discarded and the resulting sum is 47.
   This sum is multiplied by 5/4 to account for the discarded sample.

2) PICSUM IN1 OUT LIST=SRCH.LIST

where SRCH.LIST looks like

NEXT FILE = 00001
IN1
IN2
IN3
IN4


.page
PROGRAM HISTORY

ORIGINAL PROGRAMMER: Gary Yagi, circa 1982
CURRENT COGNIZANT PROGRAMMER: Gary Yagi
HISTORY:
  06 June  97 TXH Ported from VAX to Unix and VAX/VMS to support Cassini
  15 May   96 CCA Added first estimate of Cassini gain ratios
  08 Dec   94 JRY Change the despike range from 0:255 to 0:4095 for Cassini.
  12 July  94 JRY For Cassini images, sclks put in VICAR label and LIST added
  28 April 94 JRY Modified to work with Cassini images
  28 April 91 GMY Make despike threshold gain-state dependent
  06 April 91 GMY Add despike algorithm
  04 Nov   87 GMY Added ASCALE parameter
  16 Jan   87 GMY Code and documentation clean-up
  25 SEPT  84 MEM CONVERSION TO VICAR*2
  16 MARCH 84 MEM CONVERSION TO VAX/VICAR-1
           82 GMY INITIAL VERSION
.LEVEL1
.VARIABLE INP
 STRING--REQUIRED
 From 1 to 30 input
 images (byte or half)
.VARIABLE OUT
 STRING--REQUIRED
 Output image file
 name (halfword)
.VARIABLE ASCALE
 KEYWORD--OPTIONAL
 Causes each output
 pixel to be scaled
 by 128/#inputs.
.VARI TSCALE
 REAL--OPTIONAL
 Low and high thresholds
 for identifying spikes
.VARI MINT
 INTEGER--OPTIONAL
 Minimum threshold value
.VARI LIST
STRING--OPTIONAL
SRCH list containing the
input images
.LEVEL2
.VARIABLE INP
 STRING--REQUIRED
 From 1 to 30 input images.  The images may be byte or halfword format.
 However, all inputs must be of the same format.  If only 1 image is given,
 that 1 image is used only for its VICAR label, and the images that are
 PICSUM'd must be given using LIST.
.VARIABLE OUT
 STRING--REQUIRED
 Output image file name (halfword)
.VARIABLE ASCALE
 KEYWORD--OPTIONAL
 Causes each output pixel to be scaled by 128/#inputs.
.VARI TSCALE
 REAL--OPTIONAL
 Low and high thresholds for identifying spikes.  See HELP PICSUM for details.
.VARI MINT
 INTEGER--OPTIONAL
 Minimum threshold value.  See HELP PICSUM for details.
.VARI LIST
 STRING--OPTIONAL
 The input images can be given either with INP or with a SRCH list (LIST).  If
 LIST is used, you must still give one image with INP which is used only for
 its VICAR label which is used for the output image.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpicsum.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"

local dir string

! Test of picsum using gen
gen out=pic1.img nl=10 ns=10 ival=10 linc=4 sinc=6
list pic1.img

gen out=pic2.img nl=10 ns=10 ival=10 linc=6 sinc=4
list pic2.img

picsum inp=(pic1.img,pic2.img) out=sum.img
list sum.img

picsum inp=(pic1.img,pic2.img) out=sum.img tscale=(1.,1.)
list sum.img

picsum inp=(pic1.img,pic2.img) out=sum.img 'ascale
list sum.img


!!Testing with DN values close/exceed 32767
gen out=pic1.img nl=10 ns=10 ival=32764 'half
list pic1.img

gen out=pic2.img nl=10 ns=10 'half
list pic2.img

picsum inp=(pic1.img, pic2.img) out=picx.img
list picx.img


!!Testing with one HALF and one BYTE input
gen out=pic1.img nl=10 ns=10 ival=32764 'half
list pic1.img

gen out=pic2.img nl=10 ns=10
list pic2.img

picsum inp=(pic1.img, pic2.img) out=picx.img
picsum inp=(pic2.img, pic1.img) out=picz.img
difpic (picx.img, picz.img)
list picx.img


!! Test of Cassini ground calibration images
if ($syschar(1)="UNIX")
   let dir = "/project/test_work/testdata/cassini/iss/"
   ush cp /project/test_work/testdata/cassini/iss/sum2.1 .
   ush cp /project/test_work/testdata/cassini/iss/sum2.2 .
   ush cp /project/test_work/testdata/cassini/iss/sum2.3 .
else
   let dir = "wms_test_work:[testdata.cassini.iss]"
   dcl copy wms_test_work:[testdata.cassini.iss]sum2.1 *
   dcl copy wms_test_work:[testdata.cassini.iss]sum2.2 *
   dcl copy wms_test_work:[testdata.cassini.iss]sum2.3 *
end-if

picsum (&"dir"sum2.1 &"dir"sum2.3 &"dir"sum2.2) out.img
hist out.img 'nohist
label-l out.img

!!Testing the LIST parameter.  This should generate the same output as the 
!!above test case.
!Create list of the files
createfile picsum.srchlist
addtofile picsum.srchlist "NEXT FILE=0001"
addtofile picsum.srchlist "sum2.1"
addtofile picsum.srchlist "sum2.3"
addtofile picsum.srchlist "sum2.2"

picsum &"dir"sum2.1 out.img list=picsum.srchlist
hist out.img 'nohist
label-l out.img

if ($syschar(1)="UNIX")
   ush rm sum2.*
else
   dcl del sum2.*;*
end-if


write "The output DN values should be (sum of 3 DNs * 128.0/3.0)."
picsum (&"dir"sum2.1 &"dir"sum2.2 &"dir"sum2.3) out.img 'ascale
list &"dir"sum2.1 sl=10 ss=10 nl=100 ns=100 linc=10 sinc=10
list &"dir"sum2.2 sl=10 ss=10 nl=100 ns=100 linc=10 sinc=10
list &"dir"sum2.3 sl=10 ss=10 nl=100 ns=100 linc=10 sinc=10
list out.img sl=10 ss=10 nl=100 ns=100 linc=10 sinc=10
hist out.img 'nohist

picsum (&"dir"sum2.1 &"dir"sum2.2 &"dir"sum2.3) out.img tscale=(3.,3.)
hist out.img 'nohist

picsum (&"dir"sum2.1 &"dir"sum2.2 &"dir"sum2.3) out.img tscale=(3.,3.) mint=5
hist out.img 'nohist

!! Will not run, different mode and expos
!picsum (&"dir"sum2.1 &"dir"ubw_1.byte) out.img

end-proc
$ Return
$!#############################################################################
