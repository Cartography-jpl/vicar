$!****************************************************************************
$!
$! Build proc for MIPL module hist
$! VPACK Version 1.9, Tuesday, November 15, 2005, 14:46:27
$!
$! Execute by entering:		$ @hist
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
$ write sys$output "*** module hist ***"
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
$ write sys$output "Invalid argument given to hist.com file -- ", primary
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
$   if F$SEARCH("hist.imake") .nes. ""
$   then
$      vimake hist
$      purge hist.bld
$   else
$      if F$SEARCH("hist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake hist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @hist.bld "STD"
$   else
$      @hist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create hist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack hist.com -mixed -
	-s hist.f splot.c -
	-i hist.imake -
	-p hist.pdf -
	-t tsthist.pdf session.log.sun-solr session.log.x86-linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create hist.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create splot.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include "xvmaininc.h"
#include "ftnbridge.h"

#define	 XMAX  780
#define  YMAX  370
#define	 XMIN  100
#define  YMIN   10
#define  TICLEN	5
#define	 BUFSIZE 132

/*  a routine called by splot  */
char *splotlabelr(r) float r;
   {
   static char buf[20];
   sprintf(buf,"%9.g",r);
   if(9!=strlen(buf))
      sprintf(buf,"%9.2g",r);

   return buf;
   }

int FTN_NAME(splot)(x,y,n,xlo,xhi,ylo,yhi,xtic,ytic,mode,grid1)
   float *x,	/* array of x data points */
	 *y,	/* array of y data plints */
	 *xlo,	/* min x value		  */
	 *xhi,	/* max x value		  */
	 *ylo,	/* min y value		  */
	 *yhi;	/* max y value		  */
   int	 *n,	/* number of points	  */
	 *xtic,	/* number of x tickmarks  */
	 *ytic,	/* number of y tickmarks  */
	 *mode, /* mode of plotting 	  */
         *grid1; /* 0=nogrid 1=grid	  */
/*
 *	Plots data on screen of VT240 terminal 
 *
 *		Programmer Stan Schultz
 *		Startdate  ?/?/84
 *		modified   8/20/85	SDS to check if last string = string
 *		modified   1/14/86	SDS to add grid option
 *              modified   1/26/87      REA adapt to be a VICAR subroutine
 *
 *		Mode is an integer that is defined as follows
 *
 *			mode =-1	draw continuious line
 *			mode = 0	draw continuious line
 *			mode = 1	break line when y=ylo
 *			mode = 2	point plot
 */
{
   int	i,j,ix,iy,nx,ny,out,grid,lastx,lasty;
   float rangex,rangey,offx,offy,t;
   char esc=27;
   char s[BUFSIZE],c;

   rangey = (*yhi-*ylo);
   offy   = (*ylo);
   rangex = (*xhi-*xlo);
   offx   = (*xlo);
   if(*grid1!=0)
      grid = 1;
   else
      grid = 0;

   printf("\033[2J\033[0;0H",esc,esc);			/* clear screen */
   printf("%cP1pS(A[0,0][799,479])W(P1)",esc);		/* enter regis */
   if(*xtic>0 && *ytic>0)
      {
      printf("P[%d,%d]V[%d,%d]V[%d,%d]",XMIN,YMIN,XMAX,YMIN,XMAX,YMAX);/* draw */
      printf("V[%d,%d]V[%d,%d]P[%d,%d]",XMIN,YMAX,XMIN,YMIN,XMIN,YMAX);/* box  */
      for(j=grid;j<2;j++)			/*  X tickmarks */
         {
         int iy2;
         iy = j*(YMAX-YMIN-TICLEN*2) + YMIN;
         if(grid)
            {
            iy  = YMAX;
            iy2 = YMIN;
            }
         else
            iy2 = iy +TICLEN*2;
         for(i=0;i<=*xtic;i++)
            {
            ix = XMIN + i*(XMAX-XMIN)/(*xtic);
            printf("\n;P[%d,%d]V[%d,%d]",ix,iy,ix,iy2);
            if(j==1)
               {
	       t = offx + i * rangex/(*xtic);
	       printf("\n;P[%d,%d]T\"%s\"",ix-80,iy+20,splotlabelr(t));
               }
            }
         }

      for(j=grid;j<2;j++)			/* Y tickmarks */
         {
         int ix2;
         ix = j*(XMAX-XMIN-TICLEN) + XMIN;
         if(grid)
            ix2 = XMIN;
         else
            ix2 = ix+TICLEN;
         for(i=0;i<=*ytic;i++)
            {
            iy = YMIN + i*(YMAX-YMIN)/(*ytic);
            printf("\n;P[%d,%d]V[%d,%d]",ix,iy,ix2,iy);
            if(j==1)
               {
               t = offy + (*ytic-i) * rangey/(*ytic);
               printf("\n;P[%d,%d]T\"%s\"",5,iy-7,splotlabelr(t));
               }
            }
         }
      }

   s[0]=0;
   out=0;/* not out of bounds */
   for(i=0;i<*n;i++)				/* plot data at given scale */
      {
      if(c=='Q')
          break;
      iy = YMAX - (y[i]-offy) * (YMAX-YMIN) /rangey;
      ix = XMIN + (x[i]-offx) * (XMAX-XMIN) /rangex;
      if(i!=0 && ix==lastx && iy==lasty)
         continue;
      else
         {
         lastx=ix;
         lasty=iy;
         }
      if(iy<YMIN || iy>YMAX || ix<XMIN || ix>XMAX)/* point is out of bounds */
         out++;
      if(out)
         {
         if(iy < YMIN) iy = YMIN;
         if(iy > YMAX) iy = YMAX;
         if(ix < XMIN) ix = XMIN;
         if(ix > XMAX) ix = XMAX;
         if(out>1 && i<*n-1)   /* if not first out of bounds point */
            {
            ny = YMAX - (y[i+1]-offy) * (YMAX-YMIN) /rangey;
            nx = XMIN + (x[i+1]-offx) * (XMAX-XMIN) /rangex;
            if(nx<YMIN || ny>YMAX || nx<XMIN || nx>XMAX) /* if next point is */
               continue;                                 /* also off screen  */
            out=0;                             /* if next point is on screen */
            }
         }
      if(i==0)
         printf("P[%d,%d]",ix,iy);
      if(out<=1)
         {
         s[0]=0;j=0;
         switch (*mode)
            {
            case 2 : sprintf(s,"P[%d,%d]V[%d,%d]",ix,iy,ix,iy);
	             break;
	    case 1 : if((y[i]==offy))	/* break line */
	                break;
	             if((i!=0) && (y[i-1]==offy))
		        sprintf(s,"P[%d,%d]",ix,iy);
                        j = strlen(s);
                     if(y[i]==offy)
                        break;
            case 0 : 
	    case -1: sprintf(s+j,"V[%d,%d]",ix,iy);
	             break;
            }
         puts(s);	/* send command */
         }
      }

   printf("%c\\%c[22;1H",esc,esc);		/* exit regis */
   return;
}

/*
main()            NOTE THIS IS COMMENTED OUT
   {
   char a[20];
   float x[100],xlo=0,xhi=100;
   int ntic=10,grid=0,mode=1,i,npts=100;

   printf("enter grid :");
   gets(a);
   sscanf(a,"%d",&grid);

   for(i=0;i<100;i++)
      x[i]=i;

   clrscr();
   splot(x,x,&npts,&xlo,&xhi,&xlo,&xhi,&ntic,&ntic,&mode,&grid);
   }
*/
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create hist.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM hist

   To Create the build file give the command:

		$ vimake hist			(VMS)
   or
		% vimake hist			(Unix)


************************************************************************/


#define PROGRAM	hist
#define R2LIB

#define MODULE_LIST hist.f splot.c

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define USES_C
#define FTNINC_LIST pgminc

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create hist.pdf
process help=*
SUBCMD-DEFAULT NORMAL
LOCAL LMEAN   TYPE=REAL INITIAL=0.0
LOCAL LSIGMA  TYPE=REAL INITIAL=0.0
PARM INP      TYPE=STRING
PARM SIZE     TYPE=INTEGER COUNT=4 VALID=(0:999999)	DEFAULT=(1,1,0,0)
PARM SL       TYPE=INTEGER COUNT=1 VALID=(1:999999)	DEFAULT=1
PARM SS       TYPE=INTEGER COUNT=1 VALID=(1:999999)	DEFAULT=1
PARM SB       TYPE=INTEGER COUNT=1 VALID=(1:999999)	DEFAULT=1
PARM NL       TYPE=INTEGER COUNT=1 VALID=(0:999999)	DEFAULT=0
PARM NS       TYPE=INTEGER COUNT=1 VALID=(0:999999)	DEFAULT=0
PARM NB       TYPE=INTEGER COUNT=1 VALID=(0:999999)	DEFAULT=0
PARM INC      TYPE=INTEGER COUNT=0:1 VALID=(1:999999)	DEFAULT=--
PARM LINC     TYPE=INTEGER COUNT=1 VALID=(1:999999)	DEFAULT=1
PARM SINC     TYPE=INTEGER COUNT=1 VALID=(1:999999)	DEFAULT=1
PARM FORMAT   TYPE=KEYWORD COUNT=0:1 VALID=(FULL,BYTE,REAL,HALF,DOUB) DEFAULT=--
PARM SPIKES   TYPE=INTEGER COUNT=1 VALID=(1:9)	DEFAULT=2
PARM NLINES   TYPE=INTEGER COUNT=1 VALID=(8:20000)	DEFAULT=256
PARM LIMITS   TYPE=REAL    COUNT=(0,2)			DEFAULT=--
PARM EXCLUDE  TYPE=KEYWORD COUNT=(0:1) VALID=EXCLUDE	DEFAULT=--
PARM MODE  TYPE=KEYWORD VALID=(WIDE,NORMAL,SCREEN,SPLOT,NOHIST) DEFAULT=NORMAL
!###PARM PPLOT KEYWORD COUNT=0:1 VALID=PPLOT		DEFAULT=--
!###PARM TITLE TYPE=STRING DEFAULT=""
PARM NOCUM    TYPE=KEYWORD COUNT=0:1 VALID=NOCUM 	DEFAULT=--
PARM AREA     TYPE=INTEGER COUNT=0:100 VALID=(1:999999)	DEFAULT=--
PARM PAUSE    TYPE=KEYWORD VALID=(PAUSE,NOPAUSE)	DEFAULT=NOPAUSE
PARM MEAN     TYPE=NAME					DEFAULT=LMEAN
PARM SIGMA    TYPE=NAME					DEFAULT=LSIGMA
END-SUBCMD
SUBCMD IPARAM
PARM HPPLOT   TYPE=KEYWORD COUNT=(0:1) VALID=YES	DEFAULT=--
PARM NEWPAGE  TYPE=STRING				DEFAULT="NEWPAGE"
END-SUBCMD
END-PROC
.TITLE
VICAR Program HIST
.HELP
PURPOSE:
HIST prints DN-frequency histograms of one or more areas of an image.

EXECUTION:
	HIST  INP=PIC  user-parameters...
where
    PIC is a VICAR image of arbitrary size.  PIC may be in byte, halfword
    (16-bit integer), fullword (32-bit integer), floating-point (REAL*4),
    double precision (REAL*8) data format.  See FORMAT keyword.

PIC should normally reside on random access storage (e.g. magnetic disk)
to avoid the possibility of tape rewinding.  The exception is for byte
or halfword data where only one area is requested (see AREA parameter).

.page
OPERATION:

The format of the plotted histogram is specified via the MODE keyword.  Valid
values are NORMAL, WIDE, SCREEN, SPLOT, and NOHIST.  The DN axis may be plotted
vertically using 80 columns (NORMAL) or 132 columns (WIDE); or plotted
horizontally and formatted to fit on a standard 23x80 terminal screen (SCREEN).
SPLOT outputs a plot to a VT240 compatible terminal using the REGIS graphics
package.  The SCREEN mode works only on byte data, and the SPLOT mode will not
run in batch. 

The FORMAT keyword may be used to specify the data format of the input image.
If defaulted, the data format is determined from the input picture label.
Valid values are BYTE, HALF, FULL, REAL, and DOUB.

The DN-range displayed in the histogram may be specified using the LIMITS
parameter.  If defaulted, these limits are set to the minimum and maximum
DN values in the specified image area.

The number of lines (DN-levels) used to plot the output histogram is specified
by the NLINES parameter (default=256).

The 'PAUSE keyword introduces pauses every 23 lines to prevent the histogram
from scrolling off the terminal screen.

The computed mean and standard deviation of the input image may be output as
parameters which may be subsequently passed on to another program:
	LOCAL SD  TYPE=REAL		!Declare SD and AVG
	LOCAL AVG TYPE=REAL		!as local TAE variables
	HIST  PIC  SIGMA=SD  MEAN=AVG	!Compute SD and AVG
	PROG  PIC  STATS=(SD,AVG)	!Pass SD and AVG to program PROG

.page
DESCRIPTION OF THE PRINTED HISTOGRAM

The following is an example of a histogram printed using the 132-column (WIDE)
format:

        GRAY    FREQ     CDF           10        20        30        40 . . .
                              +         +         +         +         + . . .
 < LOW LIMIT     123   0.023  *         +         +         +         + . . .
        -129    1275   0.199  **        +         +         +         + . . .
         128    5395   1.042  **********+         +         +         + . . .
         385    9440   2.517  ******************  +         +         + . . .
         642   13536   4.632  ***************************   +         + . . .
          .       .       .      .      .      .     .    .     .     .
          .       .       .      .      .      .     .    .     .     .
          .       .       .      .      .      .     .    .     .     .
        6296    1238  95.234  **        +         +         +         + . . .
 >HIGH LIMIT     935 100.000  *         +         +         +         + . . .

The histogram consists of a bar graph, where the length of the bar at each DN
level is proportional to the number of pixels at that level.  The graph is
annotated on the left with a table of the frequency and cummulative distri-
bution function (CDF) at each DN-level.  The CDF is the percent count of all
pixels up to and including the given DN-level.  If the keyword 'NOCUM is
specified, this column changes to the percent count at that DN-level.
The CDF is not reported in the NORMAL screen format.

If there are pixels outside the DN-range specified by the LIMITS parameter,
these are indicated by the <LOW LIMIT and >HIGH LIMIT entries.

If the input data format is other than BYTE, the histogram is usually
compressed before display.  This is because the DN-range is normally larger
than can be displayed in the specified number of lines (NLINES parameter).
In these cases, the frequency displayed at a given DN-level represents the
number of pixels between that DN-level and the next higher DN-level in the
display.

For WIDE mode, histograms are preceded by an information line, such as:
FREQUENCY DISTRIBUTION    SL=1  SS=1  NL=15  NS=15   LINC=  1    SINC=  1
If the SIZE field is specified, then the SL and SS values in this message
are computed relative to the subimage defined by the SIZE field.

Any zero entries in a wide or normal mode histogram are skipped in the
printout.  For wide mode, one or more consecutive zero entries is represented
by a single blank line in the histogram.  For normal mode, one or more 
consecutive zero entries is indicated by an * following the DN value
in the next line of the displayed histogram.
.page
Examples

The following command prints out an 80-column-format histogram of input
image A:
	HIST  A 

The following command produces a line plot of the histogram using the
Regis graphics package.  It should only be run on a VT240 compatible terminal.
DN values are plotted along the x-axis, while frequencies are plotted along the
y-axis.
	HIST  A  'SPLOT

The following command will print out a histogram of the 15x15 pixel area in
the upper-left corner of image A.  The histogram will be plotted using a wide
(132 column) format:
	HIST  A  AREA=(1,1,15,15)  'WIDE

The following command will print two graphs in the narrow format using a
maximum of three hundred lines to represent the data:
	HIST  A  AREA=(1,1,15,15,100,100,10,40)  NLINES=300
Two histograms are plotted.  The first for the area (1,1,15,15) and the second
for the area (100,100,10,40).

The following command produces a histogram of image A for which only every
third sample of every second line will be read (for speed).
	HIST  A  SPIKES=3  LINC=2  SINC=3
SPIKES specifies that the 3 largest frequencies will be saturated.

.page
PROGRAM HISTORY

WRITTEN BY:  Alan Mazer, September 1983
COGNIZANT PROGRAMMER:  Gary Yagi
REVISIONS:
     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 23 SEPT 1983
     REVISION 1 BY ASM, FEBRUARY 7 1984 - SPEED ENHANCEMENTS
         1) REPLACED CONVERSION OF ALL INPUT DATA TO REAL-TYPE WITH
	     SEPARATE TABULATION ROUTINES FOR EACH TYPE
         2) ADDED LOOK-UP TABLE FOR BYTE-IMAGE PROCESSING
  84-10-9   LWK  converted to Vicar2, check for rounding error in sdev.
  84-10-11  LWK  for byte data, compute stats from histogram.
  84-12-13  LWK  revised treatment of BINS, LIMITS.
  85-4-17   REA  fixed bug in LINC & AREA parameters
  85-4-17   LWK  revised processing of REAL*4 data
  86-11-11  REA  modify formatting, hist collection routines, add
		       output parameters MEAN, SIGMA
  87-1-12   REA  add EXCLUDE, SCREEN parameters
  87-2-4    REA  add SPLOT, PPLOT and TITLE parameters
  87-3-2    REA  disable PPLOT (AVIRIS version)
  89-10-12  GMY  Massive changes to source code and help file.
	  	 Replaced BINS parameter with NLINES parameter.
		 Changed algorithms for collecting and compressing
		 histograms.
  90-11-7   LWK  Fixed bug in treatment of SINC with byte data
  91-2-18   GMY  Fixed case where halfword image has only one DN.
  92-10-19  SP   Made portable for UNIX.  modified to support DOUB format.
                 Corrected bug where HIST did not find MIN and MAX correctly
                 when LIMITS specified for halfword image.
  98-03-98  RRP  Fixed TAB4 to prevent division by zero when both lower and
                 upper bounds are the same in which case bandwidth equals 0.
  99-01-19  LWK  enabled for 3-D images
  05-11-15  RGD  Fixed bug with overflow of RDN on certain images
.LEVEL1
.VARIABLE INP
Input image file
.VARIABLE SIZE
Standard VICAR size field
.VARIABLE SL
Starting line
.VARIABLE SS
Starting sample
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE FORMAT
KEYWORD - Input data format 
(BYTE, HALF, FULL, REAL, DOUB)
.VARIABLE SPIKES
Bar to be normalized 
.VARIABLE SINC
Sample increment
.VARIABLE LINC
Line increment
.VARIABLE INC
Line/sample increment
.VARIABLE AREA
Area(s) to be graphed
.VARIABLE MODE
KEYWORD - Format options
Valid: NORMAL, WIDE, SCREEN,
       SPLOT, NOHIST
.VARIABLE PPLOT
KEYWORD - HP pen plot option
Valid: PPLOT
.VARIABLE TITLE
STRING - Optional title for
pen plot.
.VARI NOCUM
KEYWORD - Non-cumulative
percentages
Valid: NOCUM
.VARIABLE NLINES
Max number of graph bars
.VARIABLE LIMITS
Range of pixel values
.VARIABLE EXCLUDE
Exclude 0 DN pixels from
mean and std dev calculations
.vari NOHIST
KEYWORD: suppress histogram
Valid: NOHIST
.vari PAUSE
KEYWORD: adds interactive
pausing when screen full.
Valid: NOPAUSE, PAUSE
.vari MEAN
Output parameter
.vari SIGMA
Output parameter
.LEVEL2
.vari INP
	EX:  INP=BPIC.DAT
specifies the input image file name.
.vari SIZE
SIZE specifies the portion of the input image which is to be processed by 
the program.  It consists of 4 integers:
	SIZE=(SL,SS,NL,NS)
where
	SL = starting line
	SS = starting sample
	NL = number of lines
	NS = number of samples
These can also be specified separately by the parameters SL, SS, NL, and NS.
See also AREA parameter.
.VARIABLE FORMAT
FORMAT specifies the input image data format.  Valid data formats are
BYTE, HALF, FULL, REAL, or DOUB.  If FORMAT is not specified, the data format is
determined from the image label.
.VARIABLE SPIKES
SPIKES=N specifies that the printed histogram will be normalized in frequency 
such that the N-th most frequent value is printed as the full width on 
the page.  All values which occur more frequently will also be represented 
by a full width bar, ranked by number on the right side. 

SPIKES may have any value from 1 to 9  (default=2 for NORMAL width, and 1 for
WIDE width).
.VARIABLE SINC
SINC=N specifies that every N-th sample in the specified area is to be used
in computing the histogram.  Default SINC=1.
Note that if INC is specified, this parameter is ignored.
.VARIABLE LINC
LINC=N specifies that every N-th line in the specified area is to be used
in computing the histogram.  Default LINC=1.
Note that if INC is specified, this parameter is ignored.
.VARIABLE INC
INC=N specifies that every N-th line and every N-th sample in the specified area
is to be used in computing the histogram.  Default INC=1.
This parameter overrides the SINC and LINC parameters.
.VARIABLE AREA
AREA specifies the area(s) of the input image which is/are for which histograms
are to be plotted.  If AREA is not specified, a histogram of the entire image
is computed.  The areas are specified as multiples of four integers:
	AREA=(SL1,SS1,NL1,NS1,SL2,SS2,NL2,NS2,...)
where
	SL1 = starting line of the first image area
        SS1 = starting line of the first image area
        NL1 = number of lines in the first image area
        NS1 = number of samples in the first image area
	ETC...
Up to 150 image areas may be specified.
If the SIZE field is entered as well, the AREA coordinates must be relative
to the subimage described by the SIZE field.
.VARIABLE MODE
MODE specifies the histogram plotting mode.  Valid values are:

    NORMAL - An 80 column wide histogram is printed with a vertical DN axis.
	     NORMAL is the default.
    WIDE   - A 132 column wide histogram is printed with a vertical DN axis.
    SCREEN - A 22 row by (up to) 80 column histogram is printed with a
 	     horizontally DN axis. This plot is designed to fit on a standard
	     80x22 terminal screen. BYTE is the only permissible data format
             for this option.
    SPLOT  - The histogram is plotted using the Regis graphics package with
             a horizontal DN-axis.  The plot is suitable for output to a
	     VT240 compatible terminal.  SPLOT is ignored in batch mode.
    NOHIST - No histogram is printed.  Only the statistical data for the
             image is reported: mean, standard deviation, minimum and maximum
	     DN values, and total number of pixels.
.VARIABLE PPLOT
Keyword specifying histogram output to the HP plotter. PPLOT may be used with
any of the MODE options.  However, PPLOT is specified and MODE is defaulted,
no histogram is output to the terminal or log file.
.VARIABLE TITLE
Specifies a title to be placed beneath pen plots.  TITLE is ignored if PPLOT
is not specified. In addition, two other caption lines are always generated
for pen plots. One line contains the histogram statistics, while the other
lists the file name, area, linc, and sinc.
.vari NOCUM
NOCUM specifies that the percentages printed for each DN-level are the
percentage of the pixels at that DN-level.  The default is to display
the cummulative distribution function, which is the percentage of all pixels
at DN levels up to and including the current one.  Note that this
percentage is only reported in the WIDE format.
.VARIABLE NLINES
Specifies the maximum number of lines to be used in plotting the histogram.
The default is 256.
.VARIABLE LIMITS
LIMITS specifies the lower and upper bounds of the histogram.
If defaulted, these limits are set to the minimum and maximum
DN values in the specified image area, except for byte data,
for which the defaults are 0 and 255.
.VARIABLE EXCLUDE
Exclude 0 DN pixels from the mean and standard deviation values reported
at the end of the histogram. This does not affect the histogram itself.
.VARI PAUSE
PAUSE introduces pauses whenever the plot fills the terminal screen.
This parameter is ignored in batch mode. 
.VARI MEAN
This is an output parameter. When HIST has completed running, this parameter
contains the mean value of the last histogram. This value may be used by
subsequent programs within a procedure by declaring MEAN as a local real
variable in the procedure.
.VARI SIGMA
This is an output parameter. When HIST has completed running, this parameter
contains the value of the standard deviation for the last histogram. This 
value may be used by subsequent programs within a procedure by declaring 
SIGMA as a local real variable in the procedure.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsthist.pdf
procedure
refgbl $echo
refgbl $autousage
LOCAL SD  TYPE=REAL		!Declare SD and AVG
LOCAL AVG TYPE=REAL		!as local TAE variables
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"

! TEST SCRIPT FOR HIST
!
gen g1515 15 15
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist g1515 area=(1,1,3,3,10,10,2,2)
!
hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist g1515 linc=2 sinc=3
!
! test NLINES & LIMITS
HIST g1515 NLINES=10 LIMITS=(5,24)
HIST g1515 NLINES=10 LIMITS=(5,23)
HIST g1515           LIMITS=(5,23) 'wide
!
! TEST SPIKES
HIST g1515 SPIKE=5
HIST g1515 SPIKE=1
!
! test NOHIST keyword
hist g1515 'nohist
!
!  test output to TCL variables.
hist g1515 'nohist SIGMA=SD  MEAN=AVG	!Compute SD and AVG
putmsg "Print average and stdev variables" ""
disp AVG
disp SD
!!  Throw in some extra tests for good coverage.
!
! test SCREEN keyword
hist g1515 'screen spike=1
!
!  make an all zero file.  Hist should not blow up on 'exclude.
gen g 10 10 linc=0 sinc=0
hist g 'exclude
gen g 1024 1000 sinc=0 
write "Should get mean =128.0 because of exclude"
hist g nlines=10 'exclude
!
gen g 10 10 sinc=0
hist g inc=3
write "Mean should be 4.5."
hist g inc=3 'exclude
write "Mean should be 6.0."
!
!
gen g1515 15 15 linc=-1000 sinc=-1000 'half
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist g1515 area=(1,1,3,3,10,10,2,2)
!
hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist g1515 linc=2 sinc=3
!
! test NLINES & LIMITS
HIST g1515 NLINES=10 LIMITS=(-24000, -5000)
HIST g1515 NLINES=10 LIMITS=(-23000, -5000)
HIST g1515           LIMITS=(-23000, -5000) 'wide
!
! TEST SPIKES
HIST g1515 SPIKE=5
HIST g1515 SPIKE=1
!
! test NOHIST keyword
hist g1515 'nohist
!
!  test output to TCL variables.
hist g1515 'nohist SIGMA=SD  MEAN=AVG	!Compute SD and AVG
putmsg "Print average and stdev variables" ""
disp AVG
disp SD
!!
! try a case that HIST used to get wrong.  Should not be any
! > HIGH LIMIT entry.
gen g 10 10 linc=1000 'half
hist g nlines=30 spikes=9
!
gen g1515 15 15 linc=-100000 sinc=-100000 'full
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist g1515 area=(1,1,3,3,10,10,2,2)
!
hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist g1515 linc=2 sinc=3
!
! test NLINES & LIMITS
HIST g1515 NLINES=10 LIMITS=(-2400000, -500000)
HIST g1515 NLINES=10 LIMITS=(-2300000, -500000)
HIST g1515           LIMITS=(-2300000, -500000) 'wide
!
! TEST SPIKES
HIST g1515 SPIKE=5
HIST g1515 SPIKE=1
!
! test NOHIST keyword
hist g1515 'nohist
!
!  test output to TCL variables.
hist g1515 'nohist SIGMA=SD  MEAN=AVG	!Compute SD and AVG
putmsg "Print average and stdev variables" ""
disp AVG
disp SD
!!
!    ! test REAL*4 data
gen g1515 15 15 linc=1.e8 sinc=1.e8 'real4
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist g1515 area=(1,1,3,3,10,10,2,2)
!
hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist g1515 linc=2 sinc=3
!
! test NLINES & LIMITS
HIST g1515 NLINES=10 LIMITS=(5.e+8, 24.E+8) 
HIST g1515 NLINES=10 LIMITS=(5.e+8, 23.E+8)
HIST g1515 'nocum          LIMITS=(5.e+8, 23.E+8) 'wide
!
! TEST SPIKES
HIST g1515 SPIKE=5
HIST g1515 SPIKE=1
!
! test NOHIST keyword
hist g1515 'nohist
!
!  test output to TCL variables.
hist g1515 'nohist SIGMA=SD  MEAN=AVG	!Compute SD and AVG
putmsg "Print average and stdev variables" ""
disp AVG
disp SD
!!
gen g1515 15 15 linc=-123456789.e4 sinc=-1234567890.e4 'real4
write " Try some wild numbers.  3000 bins.  225 are non-empty."
write " Should skip all empty bins and put a * after DN to indicate skip."
hist g1515 nlines=3000
!    ! test REAL*8 data
gen g1515 15 15 linc=-1.e-9 sinc=-1.e-9 'real8
! Do histogram for gen 15 15 output in areas (1,1,3,3) and (10,10,2,2)
!
hist g1515 area=(1,1,3,3,10,10,2,2)
!
hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
!
!  test linc/sinc
hist g1515 linc=2 sinc=3
!
! test NLINES & LIMITS
HIST g1515 NLINES=10 LIMITS= (-24.e-9, -5.E-9)
HIST g1515 NLINES=10 LIMITS=(-23.e-9, -5.E-9)
HIST g1515 'nocum          LIMITS=(-23.e-9, -5.E-9) 'wide
!
! TEST SPIKES
HIST g1515 SPIKE=5
HIST g1515 SPIKE=1
!
! test NOHIST keyword
hist g1515 'nohist
!
!  test output to TCL variables.
hist g1515 'nohist SIGMA=SD  MEAN=AVG	!Compute SD and AVG
putmsg "Print average and stdev variables" ""
disp AVG
disp SD
!
!  test of bands capability
gen g1515 NS=10 NL=10 NB=10
hist g1515
hist g1515 NB=4
!!
! test AR 112483.  Std dev should be exactly 0.  Depends on image size.
gen g1515 337 364 ival=200 linc=0 sinc=0
hist a
!
end-proc
$!-----------------------------------------------------------------------------
$ create session.log.sun-solr
tsthist
let $autousage="none"
gen g1515 15 15
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

          0       1    *************************
          1       2    **************************************************  2
          2       3    **************************************************  1
          3       2    **************************************************
          4       1    *************************

AVERAGE GRAY LEVEL=2.000000
STANDARD DEVIATION=1.154701
NUMBER ELEMENTS=       9
MIN. DN=         0
MAX. DN=         4


         18*      1    **************************************************  2
         19       2    **************************************************  1
         20       1    **************************************************

AVERAGE GRAY LEVEL=19.00000
STANDARD DEVIATION=0.707107
NUMBER ELEMENTS=       4
MIN. DN=        18
MAX. DN=        20

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

          2*      1    *************************
          3       2    **************************************************  2
          4       3    **************************************************  1
          5       2    **************************************************
          6       1    *************************

AVERAGE GRAY LEVEL=4.000000
STANDARD DEVIATION=1.154701
NUMBER ELEMENTS=       9
MIN. DN=         2
MAX. DN=         6


         20*      1    **************************************************  2
         21       2    **************************************************  1
         22       1    **************************************************

AVERAGE GRAY LEVEL=21.00000
STANDARD DEVIATION=0.707107
NUMBER ELEMENTS=       4
MIN. DN=        20
MAX. DN=        22

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
HIST version 15-NOV-05

          0       1    ****************
          2*      1    ****************
          3       1    ****************
          4       1    ****************
          5       1    ****************
          6       2    *********************************
          7       1    ****************
          8       2    *********************************
          9       2    *********************************
         10       2    *********************************
         11       2    *********************************
         12       3    **************************************************  1
         13       2    *********************************
         14       3    **************************************************  2
         15       2    *********************************
         16       2    *********************************
         17       2    *********************************
         18       2    *********************************
         19       1    ****************
         20       2    *********************************
         21       1    ****************
         22       1    ****************
         23       1    ****************
         24       1    ****************
         26*      1    ****************

AVERAGE GRAY LEVEL=13.00000
STANDARD DEVIATION=6.244998
NUMBER ELEMENTS=      40
MIN. DN=         0
MAX. DN=        26

HIST g1515 NLINES=10 LIMITS=(5,24)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      10    ******************
        5.0      18    *********************************
        7.1      17    *******************************
        9.2      21    **************************************
       11.3      25    **********************************************
       13.4      29    **************************************************  1
       15.6      27    **************************************************  2
       17.7      23    ******************************************
       19.8      19    ***********************************
       21.9      15    ***************************
       24.0      15    ***************************
>HIGH LIMIT       6    ***********

AVERAGE GRAY LEVEL=14.00000
STANDARD DEVIATION=6.110101
NUMBER ELEMENTS=     225
MIN. DN=         0
MAX. DN=        28

HIST g1515 NLINES=10 LIMITS=(5,23)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      10    ******************
          5      11    ********************
          7      15    ***************************
          9      19    ***********************************
         11      23    ******************************************
         13      27    **************************************************  2
         15      29    **************************************************  1
         17      25    **********************************************
         19      21    **************************************
         21      17    *******************************
         23      13    ************************
>HIGH LIMIT      15    ***************************

AVERAGE GRAY LEVEL=14.00000
STANDARD DEVIATION=6.110101
NUMBER ELEMENTS=     225
MIN. DN=         0
MAX. DN=        28

HIST g1515           LIMITS=(5,23) 'wide
Beginning VICAR task HIST
HIST version 15-NOV-05

FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ     CDF           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT      15   6.667  ****************************************************************************************************+1
          5       6   9.333  ****************************************+         +         +         +         +         +         +
          6       7  12.444  **********************************************    +         +         +         +         +         +
          7       8  16.000  *****************************************************       +         +         +         +         +
          8       9  20.000  ************************************************************+         +         +         +         +
          9      10  24.444  ******************************************************************    +         +         +         +
         10      11  29.333  *************************************************************************       +         +         +
         11      12  34.667  ********************************************************************************+         +         +
         12      13  40.444  **************************************************************************************    +         +
         13      14  46.667  *********************************************************************************************       +
         14      15  53.333  ****************************************************************************************************+2
         15      14  59.556  *********************************************************************************************       +
         16      13  65.333  **************************************************************************************    +         +
         17      12  70.667  ********************************************************************************+         +         +
         18      11  75.556  *************************************************************************       +         +         +
         19      10  80.000  ******************************************************************    +         +         +         +
         20       9  84.000  ************************************************************+         +         +         +         +
         21       8  87.556  *****************************************************       +         +         +         +         +
         22       7  90.667  **********************************************    +         +         +         +         +         +
         23       6  93.333  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT      15 100.000  ****************************************************************************************************+

AVERAGE GRAY LEVEL=14.00000       STANDARD DEVIATION=6.110101       NUMBER ELEMENTS=     225
MIN. DN=         0    MAX. DN=        28

HIST g1515 SPIKE=5
Beginning VICAR task HIST
HIST version 15-NOV-05

          0       1    ***
          1       2    *******
          2       3    ***********
          3       4    ***************
          4       5    *******************
          5       6    ***********************
          6       7    **************************
          7       8    ******************************
          8       9    **********************************
          9      10    **************************************
         10      11    ******************************************
         11      12    **********************************************
         12      13    **************************************************  4
         13      14    **************************************************  2
         14      15    **************************************************  1
         15      14    **************************************************  3
         16      13    **************************************************  5
         17      12    **********************************************
         18      11    ******************************************
         19      10    **************************************
         20       9    **********************************
         21       8    ******************************
         22       7    **************************
         23       6    ***********************
         24       5    *******************
         25       4    ***************
         26       3    ***********
         27       2    *******
         28       1    ***

AVERAGE GRAY LEVEL=14.00000
STANDARD DEVIATION=6.110101
NUMBER ELEMENTS=     225
MIN. DN=         0
MAX. DN=        28

HIST g1515 SPIKE=1
Beginning VICAR task HIST
HIST version 15-NOV-05

          0       1    ***
          1       2    ******
          2       3    **********
          3       4    *************
          4       5    ****************
          5       6    ********************
          6       7    ***********************
          7       8    **************************
          8       9    ******************************
          9      10    *********************************
         10      11    ************************************
         11      12    ****************************************
         12      13    *******************************************
         13      14    **********************************************
         14      15    **************************************************  1
         15      14    **********************************************
         16      13    *******************************************
         17      12    ****************************************
         18      11    ************************************
         19      10    *********************************
         20       9    ******************************
         21       8    **************************
         22       7    ***********************
         23       6    ********************
         24       5    ****************
         25       4    *************
         26       3    **********
         27       2    ******
         28       1    ***

AVERAGE GRAY LEVEL=14.00000
STANDARD DEVIATION=6.110101
NUMBER ELEMENTS=     225
MIN. DN=         0
MAX. DN=        28

hist g1515 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=14.00000       STANDARD DEVIATION=6.110101       NUMBER ELEMENTS=     225
MIN. DN=         0
MAX. DN=        28

hist g1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=14.00000       STANDARD DEVIATION=6.110101       NUMBER ELEMENTS=     225
MIN. DN=         0
MAX. DN=        28

putmsg "Print average and stdev variables" ""
Print average and stdev variables
disp AVG

AVG=14.0

disp SD

SD=6.11010074615

hist g1515 'screen spike=1
Beginning VICAR task hist
HIST version 15-NOV-05

              1
             ***
             ***
            *****
           *******
          *********
         ***********
         ***********
        *************
       ***************
      *****************
     *******************
     *******************
    *********************
   ***********************
  *************************
 ***************************
 ***************************
*****************************

          1    1    2    2
0    5    0    5    0    5
       225PIXELS   RANGE   0- 28     MEAN  14.000     STD DEV   6.110
gen g 10 10 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g 'exclude
Beginning VICAR task hist
HIST version 15-NOV-05

          0     100    **************************************************  1

EXCLUDING PIXELS OF DN=0
AVERAGE GRAY LEVEL=0.000000
STANDARD DEVIATION=0.000000
NUMBER ELEMENTS=       0
MIN. DN=         0
MAX. DN=         0

gen g 1024 1000 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
write "Should get mean =128.0 because of exclude"
Should get mean =128.0 because of exclude
hist g nlines=10 'exclude
Beginning VICAR task hist
HIST version 15-NOV-05

          0   60000    *************************
         28  112000    ************************************************
         57  112000    ************************************************
         85  116000    **************************************************  1
        113  112000    ************************************************
        142  112000    ************************************************
        170  116000    **************************************************  2
        198  112000    ************************************************
        227  112000    ************************************************
        255   60000    *************************

EXCLUDING PIXELS OF DN=0
AVERAGE GRAY LEVEL=128.0000
STANDARD DEVIATION=73.61160
NUMBER ELEMENTS= 1020000
MIN. DN=         0
MAX. DN=       255

gen g 10 10 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g inc=3
Beginning VICAR task hist
HIST version 15-NOV-05

          0       4    **************************************************  1
          3*      4    **************************************************  2
          6*      4    **************************************************
          9*      4    **************************************************

AVERAGE GRAY LEVEL=4.500000
STANDARD DEVIATION=3.354102
NUMBER ELEMENTS=      16
MIN. DN=         0
MAX. DN=         9

write "Mean should be 4.5."
Mean should be 4.5.
hist g inc=3 'exclude
Beginning VICAR task hist
HIST version 15-NOV-05

          0       4    **************************************************  1
          3*      4    **************************************************  2
          6*      4    **************************************************
          9*      4    **************************************************

EXCLUDING PIXELS OF DN=0
AVERAGE GRAY LEVEL=6.000000
STANDARD DEVIATION=2.449490
NUMBER ELEMENTS=      12
MIN. DN=         0
MAX. DN=         9

write "Mean should be 6.0."
Mean should be 6.0.
gen g1515 15 15 linc=-1000 sinc=-1000 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

      -4000       1    *************************
      -2976*      2    **************************************************  2
      -1952*      3    **************************************************  1
       -928*      2    **************************************************
         96*      1    *************************

AVERAGE GRAY LEVEL=-2000.00
STANDARD DEVIATION=1154.701
NUMBER ELEMENTS=       9
MIN. DN=     -4000
MAX. DN=         0


     -20000       1    **************************************************  2
     -18976*      2    **************************************************  1
     -17952*      1    **************************************************

AVERAGE GRAY LEVEL=-19000.0
STANDARD DEVIATION=707.1068
NUMBER ELEMENTS=       4
MIN. DN=    -20000
MAX. DN=    -18000

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

      -6000       1    *************************
      -4976*      2    **************************************************  2
      -3952*      3    **************************************************  1
      -2928*      2    **************************************************
      -1904*      1    *************************

AVERAGE GRAY LEVEL=-4000.00
STANDARD DEVIATION=1154.701
NUMBER ELEMENTS=       9
MIN. DN=     -6000
MAX. DN=     -2000


     -22000       1    **************************************************  2
     -20976*      2    **************************************************  1
     -19952*      1    **************************************************

AVERAGE GRAY LEVEL=-21000.0
STANDARD DEVIATION=707.1068
NUMBER ELEMENTS=       4
MIN. DN=    -22000
MAX. DN=    -20000

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
HIST version 15-NOV-05

     -26000       1    ****************
     -23952*      1    ****************
     -22928*      1    ****************
     -21904*      1    ****************
     -20880*      1    ****************
     -20112*      2    *********************************
     -19088*      1    ****************
     -18064*      2    *********************************
     -17040*      2    *********************************
     -16016*      2    *********************************
     -14992*      2    *********************************
     -13968*      3    **************************************************  1
     -12944*      2    *********************************
     -11920*      3    **************************************************  2
     -10896*      2    *********************************
      -9872*      2    *********************************
      -9104*      2    *********************************
      -8080*      2    *********************************
      -7056*      1    ****************
      -6032*      2    *********************************
      -5008*      1    ****************
      -3984*      1    ****************
      -2960*      1    ****************
      -1936*      1    ****************
        112*      1    ****************

AVERAGE GRAY LEVEL=-13000.0
STANDARD DEVIATION=6244.998
NUMBER ELEMENTS=      40
MIN. DN=    -26000
MAX. DN=         0

HIST g1515 NLINES=10 LIMITS=(-24000, -5000)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT       6    ***********
     -24000      15    ***************************
     -21889      15    ***************************
     -19778      19    ***********************************
     -17667      23    ******************************************
     -15556      27    **************************************************  2
     -13444      29    **************************************************  1
     -11333      25    **********************************************
      -9222      21    **************************************
      -7111      17    *******************************
      -5000      18    *********************************
>HIGH LIMIT      10    ******************

AVERAGE GRAY LEVEL=-14000.0
STANDARD DEVIATION=6110.101
NUMBER ELEMENTS=     225
MIN. DN=    -28000
MAX. DN=         0

HIST g1515 NLINES=10 LIMITS=(-23000, -5000)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      10    ******************
     -23000      11    ********************
     -21000      15    ***************************
     -19000      19    ***********************************
     -17000      23    ******************************************
     -15000      27    **************************************************  2
     -13000      29    **************************************************  1
     -11000      25    **********************************************
      -9000      21    **************************************
      -7000      17    *******************************
      -5000      13    ************************
>HIGH LIMIT      15    ***************************

AVERAGE GRAY LEVEL=-14000.0
STANDARD DEVIATION=6110.101
NUMBER ELEMENTS=     225
MIN. DN=    -28000
MAX. DN=         0

HIST g1515           LIMITS=(-23000, -5000) 'wide
Beginning VICAR task HIST
HIST version 15-NOV-05

FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ     CDF           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT      15   6.667  ****************************************************************************************************+1
     -23000       6   9.333  ****************************************+         +         +         +         +         +         +

     -22012       7  12.444  **********************************************    +         +         +         +         +         +

     -21024       8  16.000  *****************************************************       +         +         +         +         +

     -19965       9  20.000  ************************************************************+         +         +         +         +

     -18976      10  24.444  ******************************************************************    +         +         +         +

     -17988      11  29.333  *************************************************************************       +         +         +

     -17000      12  34.667  ********************************************************************************+         +         +

     -16012      13  40.444  **************************************************************************************    +         +

     -15024      14  46.667  *********************************************************************************************       +

     -13965      15  53.333  ****************************************************************************************************+2

     -12976      14  59.556  *********************************************************************************************       +

     -11988      13  65.333  **************************************************************************************    +         +

     -11000      12  70.667  ********************************************************************************+         +         +

     -10012      11  75.556  *************************************************************************       +         +         +

      -9024      10  80.000  ******************************************************************    +         +         +         +

      -7965       9  84.000  ************************************************************+         +         +         +         +

      -6976       8  87.556  *****************************************************       +         +         +         +         +

      -5988       7  90.667  **********************************************    +         +         +         +         +         +

      -5000       6  93.333  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT      15 100.000  ****************************************************************************************************+

AVERAGE GRAY LEVEL=-14000.0       STANDARD DEVIATION=6110.101       NUMBER ELEMENTS=     225
MIN. DN=    -28000    MAX. DN=         0

HIST g1515 SPIKE=5
Beginning VICAR task HIST
HIST version 15-NOV-05

     -28000       1    ***
     -26976*      2    *******
     -25952*      3    ***********
     -24928*      4    ***************
     -23904*      5    *******************
     -22880*      6    ***********************
     -22112*      7    **************************
     -21088*      8    ******************************
     -20064*      9    **********************************
     -19040*     10    **************************************
     -18016*     11    ******************************************
     -16992*     12    **********************************************
     -15968*     13    **************************************************  4
     -14944*     14    **************************************************  2
     -13920*     15    **************************************************  1
     -12896*     14    **************************************************  3
     -11872*     13    **************************************************  5
     -11104*     12    **********************************************
     -10080*     11    ******************************************
      -9056*     10    **************************************
      -8032*      9    **********************************
      -7008*      8    ******************************
      -5984*      7    **************************
      -4960*      6    ***********************
      -3936*      5    *******************
      -2912*      4    ***************
      -1888*      3    ***********
      -1120*      2    *******
        -96*      1    ***

AVERAGE GRAY LEVEL=-14000.0
STANDARD DEVIATION=6110.101
NUMBER ELEMENTS=     225
MIN. DN=    -28000
MAX. DN=         0

HIST g1515 SPIKE=1
Beginning VICAR task HIST
HIST version 15-NOV-05

     -28000       1    ***
     -26976*      2    ******
     -25952*      3    **********
     -24928*      4    *************
     -23904*      5    ****************
     -22880*      6    ********************
     -22112*      7    ***********************
     -21088*      8    **************************
     -20064*      9    ******************************
     -19040*     10    *********************************
     -18016*     11    ************************************
     -16992*     12    ****************************************
     -15968*     13    *******************************************
     -14944*     14    **********************************************
     -13920*     15    **************************************************  1
     -12896*     14    **********************************************
     -11872*     13    *******************************************
     -11104*     12    ****************************************
     -10080*     11    ************************************
      -9056*     10    *********************************
      -8032*      9    ******************************
      -7008*      8    **************************
      -5984*      7    ***********************
      -4960*      6    ********************
      -3936*      5    ****************
      -2912*      4    *************
      -1888*      3    **********
      -1120*      2    ******
        -96*      1    ***

AVERAGE GRAY LEVEL=-14000.0
STANDARD DEVIATION=6110.101
NUMBER ELEMENTS=     225
MIN. DN=    -28000
MAX. DN=         0

hist g1515 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=-14000.0       STANDARD DEVIATION=6110.101       NUMBER ELEMENTS=     225
MIN. DN=    -28000
MAX. DN=         0

hist g1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=-14000.0       STANDARD DEVIATION=6110.101       NUMBER ELEMENTS=     225
MIN. DN=    -28000
MAX. DN=         0

putmsg "Print average and stdev variables" ""
Print average and stdev variables
disp AVG

AVG=-14000.0

disp SD

SD=6110.10107422

gen g 10 10 linc=1000 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g nlines=30 spikes=9
Beginning VICAR task hist
HIST version 15-NOV-05

          0       2    *************************
          4       4    **************************************************  1
          8       4    **************************************************  2
       1000*      2    *************************
       1004       4    **************************************************  3
       1008       4    **************************************************  4
       2000*      2    *************************
       2004       4    **************************************************  5
       2008       4    **************************************************  6
       3000*      2    *************************
       3004       4    **************************************************  7
       3008       4    **************************************************  8
       4000*      2    *************************
       4004       4    **************************************************  9
       4008       4    **************************************************
       5000*      2    *************************
       5004       4    **************************************************
       5008       4    **************************************************
       6000*      2    *************************
       6004       4    **************************************************
       6008       4    **************************************************
       7000*      2    *************************
       7004       4    **************************************************
       7008       4    **************************************************
       8000*      2    *************************
       8004       4    **************************************************
       8008       4    **************************************************
       9000*      2    *************************
       9004       4    **************************************************
       9008       4    **************************************************

AVERAGE GRAY LEVEL=4504.500
STANDARD DEVIATION=2872.283
NUMBER ELEMENTS=     100
MIN. DN=         0
MAX. DN=      9009

gen g1515 15 15 linc=-100000 sinc=-100000 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

    -400000       1    *************************
    -299608*      2    **************************************************  2
    -199216*      3    **************************************************  1
    -100392*      2    **************************************************
          0*      1    *************************

AVERAGE GRAY LEVEL=-200000.
STANDARD DEVIATION=115470.1
NUMBER ELEMENTS=       9
MIN. DN=   -400000
MAX. DN=         0


   -2000000       1    **************************************************  2
   -1899608*      2    **************************************************  1
   -1800000*      1    **************************************************

AVERAGE GRAY LEVEL=-1.90E+6
STANDARD DEVIATION=70710.46
NUMBER ELEMENTS=       4
MIN. DN=  -2000000
MAX. DN=  -1800000

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

    -600000       1    *************************
    -499608*      2    **************************************************  2
    -399216*      3    **************************************************  1
    -300392*      2    **************************************************
    -200000*      1    *************************

AVERAGE GRAY LEVEL=-400000.
STANDARD DEVIATION=115470.0
NUMBER ELEMENTS=       9
MIN. DN=   -600000
MAX. DN=   -200000


   -2200000       1    **************************************************  2
   -2099608*      2    **************************************************  1
   -2000000*      1    **************************************************

AVERAGE GRAY LEVEL=-2.10E+6
STANDARD DEVIATION=70709.42
NUMBER ELEMENTS=       4
MIN. DN=  -2200000
MAX. DN=  -2000000

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
HIST version 15-NOV-05

   -2600000       1    ****************
   -2396078*      1    ****************
   -2304314*      1    ****************
   -2202353*      1    ****************
   -2100392*      1    ****************
   -1998431*      2    *********************************
   -1896471*      1    ****************
   -1804706*      2    *********************************
   -1702745*      2    *********************************
   -1600784*      2    *********************************
   -1498823*      2    *********************************
   -1396863*      3    **************************************************  1
   -1294902*      2    *********************************
   -1203137*      3    **************************************************  2
   -1101176*      2    *********************************
    -999216*      2    *********************************
    -897255*      2    *********************************
    -795294*      2    *********************************
    -703529*      1    ****************
    -601569*      2    *********************************
    -499608*      1    ****************
    -397647*      1    ****************
    -295686*      1    ****************
    -203922*      1    ****************
          0*      1    ****************

AVERAGE GRAY LEVEL=-1.30E+6
STANDARD DEVIATION=624499.8
NUMBER ELEMENTS=      40
MIN. DN=  -2600000
MAX. DN=         0

HIST g1515 NLINES=10 LIMITS=(-2400000, -500000)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT       6    ***********
   -2400000      15    ***************************
   -2188889      15    ***************************
   -1977778      19    ***********************************
   -1766667      23    ******************************************
   -1555555      27    **************************************************  2
   -1344444      29    **************************************************  1
   -1133333      25    **********************************************
    -922222      21    **************************************
    -711111      17    *******************************
    -500000      18    *********************************
>HIGH LIMIT      10    ******************

AVERAGE GRAY LEVEL=-1.40E+6
STANDARD DEVIATION=611010.1
NUMBER ELEMENTS=     225
MIN. DN=  -2800000
MAX. DN=         0

HIST g1515 NLINES=10 LIMITS=(-2300000, -500000)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      10    ******************
   -2300000      11    ********************
   -2100000      15    ***************************
   -1900000      19    ***********************************
   -1700000      23    ******************************************
   -1500000      27    **************************************************  2
   -1300000      29    **************************************************  1
   -1100000      25    **********************************************
    -900000      21    **************************************
    -700000      17    *******************************
    -500000      13    ************************
>HIGH LIMIT      15    ***************************

AVERAGE GRAY LEVEL=-1.40E+6
STANDARD DEVIATION=611010.1
NUMBER ELEMENTS=     225
MIN. DN=  -2800000
MAX. DN=         0

HIST g1515           LIMITS=(-2300000, -500000) 'wide
Beginning VICAR task HIST
HIST version 15-NOV-05

FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ     CDF           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT      15   6.667  ****************************************************************************************************+1
   -2300000       6   9.333  ****************************************+         +         +         +         +         +         +

   -2201176       7  12.444  **********************************************    +         +         +         +         +         +

   -2102353       8  16.000  *****************************************************       +         +         +         +         +

   -1996471       9  20.000  ************************************************************+         +         +         +         +

   -1897647      10  24.444  ******************************************************************    +         +         +         +

   -1798823      11  29.333  *************************************************************************       +         +         +

   -1700000      12  34.667  ********************************************************************************+         +         +

   -1601176      13  40.444  **************************************************************************************    +         +

   -1502353      14  46.667  *********************************************************************************************       +

   -1396470      15  53.333  ****************************************************************************************************+2

   -1297647      14  59.556  *********************************************************************************************       +

   -1198823      13  65.333  **************************************************************************************    +         +

   -1100000      12  70.667  ********************************************************************************+         +         +

   -1001176      11  75.556  *************************************************************************       +         +         +

    -902353      10  80.000  ******************************************************************    +         +         +         +

    -796470       9  84.000  ************************************************************+         +         +         +         +

    -697647       8  87.556  *****************************************************       +         +         +         +         +

    -598823       7  90.667  **********************************************    +         +         +         +         +         +

    -500000       6  93.333  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT      15 100.000  ****************************************************************************************************+

AVERAGE GRAY LEVEL=-1.40E+6       STANDARD DEVIATION=611010.1       NUMBER ELEMENTS=     225
MIN. DN=  -2800000    MAX. DN=         0

HIST g1515 SPIKE=5
Beginning VICAR task HIST
HIST version 15-NOV-05

   -2800000       1    ***
   -2701177*      2    *******
   -2602353*      3    ***********
   -2503530*      4    ***************
   -2404706*      5    *******************
   -2294902*      6    ***********************
   -2196079*      7    **************************
   -2097255*      8    ******************************
   -1998431*      9    **********************************
   -1899608*     10    **************************************
   -1800784*     11    ******************************************
   -1701961*     12    **********************************************
   -1603137*     13    **************************************************  4
   -1504314*     14    **************************************************  2
   -1394510*     15    **************************************************  1
   -1295686*     14    **************************************************  3
   -1196863*     13    **************************************************  5
   -1098039*     12    **********************************************
    -999216*     11    ******************************************
    -900392*     10    **************************************
    -801569*      9    **********************************
    -702745*      8    ******************************
    -603922*      7    **************************
    -505098*      6    ***********************
    -395294*      5    *******************
    -296471*      4    ***************
    -197647*      3    ***********
     -98824*      2    *******
          0*      1    ***

AVERAGE GRAY LEVEL=-1.40E+6
STANDARD DEVIATION=611010.1
NUMBER ELEMENTS=     225
MIN. DN=  -2800000
MAX. DN=         0

HIST g1515 SPIKE=1
Beginning VICAR task HIST
HIST version 15-NOV-05

   -2800000       1    ***
   -2701177*      2    ******
   -2602353*      3    **********
   -2503530*      4    *************
   -2404706*      5    ****************
   -2294902*      6    ********************
   -2196079*      7    ***********************
   -2097255*      8    **************************
   -1998431*      9    ******************************
   -1899608*     10    *********************************
   -1800784*     11    ************************************
   -1701961*     12    ****************************************
   -1603137*     13    *******************************************
   -1504314*     14    **********************************************
   -1394510*     15    **************************************************  1
   -1295686*     14    **********************************************
   -1196863*     13    *******************************************
   -1098039*     12    ****************************************
    -999216*     11    ************************************
    -900392*     10    *********************************
    -801569*      9    ******************************
    -702745*      8    **************************
    -603922*      7    ***********************
    -505098*      6    ********************
    -395294*      5    ****************
    -296471*      4    *************
    -197647*      3    **********
     -98824*      2    ******
          0*      1    ***

AVERAGE GRAY LEVEL=-1.40E+6
STANDARD DEVIATION=611010.1
NUMBER ELEMENTS=     225
MIN. DN=  -2800000
MAX. DN=         0

hist g1515 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=-1.40E+6       STANDARD DEVIATION=611010.1       NUMBER ELEMENTS=     225
MIN. DN=  -2800000
MAX. DN=         0

hist g1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=-1.40E+6       STANDARD DEVIATION=611010.1       NUMBER ELEMENTS=     225
MIN. DN=  -2800000
MAX. DN=         0

putmsg "Print average and stdev variables" ""
Print average and stdev variables
disp AVG

AVG=-1400000.0

disp SD

SD=611010.0625

gen g1515 15 15 linc=1.e8 sinc=1.e8 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

0.00000E+00       1    *************************
1.00392E+08*      2    **************************************************  2
2.00784E+08*      3    **************************************************  1
2.99608E+08*      2    **************************************************
4.00000E+08*      1    *************************

AVERAGE GRAY LEVEL=2.000E+8
STANDARD DEVIATION=1.155E+8
NUMBER ELEMENTS=       9
MIN. DN=0.000000
MAX. DN=4.000E+8


1.80000E+09       1    **************************************************  2
1.90039E+09*      2    **************************************************  1
2.00000E+09*      1    **************************************************

AVERAGE GRAY LEVEL=1.900E+9
STANDARD DEVIATION=7.071E+7
NUMBER ELEMENTS=       4
MIN. DN=1.800E+9
MAX. DN=2.000E+9

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

2.00000E+08       1    *************************
3.00392E+08*      2    **************************************************  2
4.00784E+08*      3    **************************************************  1
4.99608E+08*      2    **************************************************
6.00000E+08*      1    *************************

AVERAGE GRAY LEVEL=4.000E+8
STANDARD DEVIATION=1.155E+8
NUMBER ELEMENTS=       9
MIN. DN=2.000E+8
MAX. DN=6.000E+8


2.00000E+09       1    **************************************************  2
2.10039E+09*      2    **************************************************  1
2.20000E+09*      1    **************************************************

AVERAGE GRAY LEVEL=2.100E+9
STANDARD DEVIATION=7.071E+7
NUMBER ELEMENTS=       4
MIN. DN=2.000E+9
MAX. DN=2.200E+9

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
HIST version 15-NOV-05

0.00000E+00       1    ****************
2.03922E+08*      1    ****************
2.95686E+08*      1    ****************
3.97647E+08*      1    ****************
4.99608E+08*      1    ****************
6.01569E+08*      2    *********************************
7.03529E+08*      1    ****************
7.95294E+08*      2    *********************************
8.97255E+08*      2    *********************************
9.99216E+08*      2    *********************************
1.10118E+09*      2    *********************************
1.20314E+09*      3    **************************************************  1
1.30510E+09*      2    *********************************
1.39686E+09*      3    **************************************************  2
1.49882E+09*      2    *********************************
1.60078E+09*      2    *********************************
1.70275E+09*      2    *********************************
1.80471E+09*      2    *********************************
1.89647E+09*      1    ****************
1.99843E+09*      2    *********************************
2.10039E+09*      1    ****************
2.20235E+09*      1    ****************
2.30431E+09*      1    ****************
2.39608E+09*      1    ****************
2.60000E+09*      1    ****************

AVERAGE GRAY LEVEL=1.300E+9
STANDARD DEVIATION=6.245E+8
NUMBER ELEMENTS=      40
MIN. DN=0.000000
MAX. DN=2.600E+9

HIST g1515 NLINES=10 LIMITS=(5.e+8, 24.E+8)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      10    ******************
5.00000E+08      18    *********************************
7.11111E+08      17    *******************************
9.22222E+08      21    **************************************
1.13333E+09      25    **********************************************
1.34444E+09      29    **************************************************  1
1.55556E+09      27    **************************************************  2
1.76667E+09      23    ******************************************
1.97778E+09      19    ***********************************
2.18889E+09      15    ***************************
2.40000E+09      15    ***************************
>HIGH LIMIT       6    ***********

AVERAGE GRAY LEVEL=1.400E+9
STANDARD DEVIATION=6.110E+8
NUMBER ELEMENTS=     225
MIN. DN=0.000000
MAX. DN=2.800E+9

HIST g1515 NLINES=10 LIMITS=(5.e+8, 23.E+8)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      10    ******************
5.00000E+08      11    ********************
7.00000E+08      15    ***************************
9.00000E+08      19    ***********************************
1.10000E+09      23    ******************************************
1.30000E+09      27    **************************************************  2
1.50000E+09      29    **************************************************  1
1.70000E+09      25    **********************************************
1.90000E+09      21    **************************************
2.10000E+09      17    *******************************
2.30000E+09      13    ************************
>HIGH LIMIT      15    ***************************

AVERAGE GRAY LEVEL=1.400E+9
STANDARD DEVIATION=6.110E+8
NUMBER ELEMENTS=     225
MIN. DN=0.000000
MAX. DN=2.800E+9

HIST g1515 'nocum          LIMITS=(5.e+8, 23.E+8) 'wide
Beginning VICAR task HIST
HIST version 15-NOV-05

FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ  PCTILE           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT      15   6.667  ****************************************************************************************************+1
5.00000E+08       6   2.667  ****************************************+         +         +         +         +         +         +

5.98824E+08       7   3.111  **********************************************    +         +         +         +         +         +

6.97647E+08       8   3.556  *****************************************************       +         +         +         +         +

8.03529E+08       9   4.000  ************************************************************+         +         +         +         +

9.02353E+08      10   4.444  ******************************************************************    +         +         +         +

1.00118E+09      11   4.889  *************************************************************************       +         +         +

1.10000E+09      12   5.333  ********************************************************************************+         +         +

1.19882E+09      13   5.778  **************************************************************************************    +         +

1.29765E+09      14   6.222  *********************************************************************************************       +

1.40353E+09      15   6.667  ****************************************************************************************************+2

1.50235E+09      14   6.222  *********************************************************************************************       +

1.60118E+09      13   5.778  **************************************************************************************    +         +

1.70000E+09      12   5.333  ********************************************************************************+         +         +

1.79882E+09      11   4.889  *************************************************************************       +         +         +

1.89765E+09      10   4.444  ******************************************************************    +         +         +         +

2.00353E+09       9   4.000  ************************************************************+         +         +         +         +

2.10235E+09       8   3.556  *****************************************************       +         +         +         +         +

2.20118E+09       7   3.111  **********************************************    +         +         +         +         +         +

2.30000E+09       6   2.667  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT      15   6.667  ****************************************************************************************************+

AVERAGE GRAY LEVEL=1.400E+9       STANDARD DEVIATION=6.110E+8       NUMBER ELEMENTS=     225
MIN. DN=0.000000      MAX. DN=2.800E+9

HIST g1515 SPIKE=5
Beginning VICAR task HIST
HIST version 15-NOV-05

0.00000E+00       1    ***
9.88235E+07*      2    *******
1.97647E+08*      3    ***********
2.96471E+08*      4    ***************
3.95294E+08*      5    *******************
5.05098E+08*      6    ***********************
6.03922E+08*      7    **************************
7.02745E+08*      8    ******************************
8.01569E+08*      9    **********************************
9.00392E+08*     10    **************************************
9.99216E+08*     11    ******************************************
1.09804E+09*     12    **********************************************
1.19686E+09*     13    **************************************************  4
1.29569E+09*     14    **************************************************  2
1.40549E+09*     15    **************************************************  1
1.50431E+09*     14    **************************************************  3
1.60314E+09*     13    **************************************************  5
1.70196E+09*     12    **********************************************
1.80078E+09*     11    ******************************************
1.89961E+09*     10    **************************************
1.99843E+09*      9    **********************************
2.09725E+09*      8    ******************************
2.19608E+09*      7    **************************
2.29490E+09*      6    ***********************
2.40471E+09*      5    *******************
2.50353E+09*      4    ***************
2.60235E+09*      3    ***********
2.70118E+09*      2    *******
2.80000E+09*      1    ***

AVERAGE GRAY LEVEL=1.400E+9
STANDARD DEVIATION=6.110E+8
NUMBER ELEMENTS=     225
MIN. DN=0.000000
MAX. DN=2.800E+9

HIST g1515 SPIKE=1
Beginning VICAR task HIST
HIST version 15-NOV-05

0.00000E+00       1    ***
9.88235E+07*      2    ******
1.97647E+08*      3    **********
2.96471E+08*      4    *************
3.95294E+08*      5    ****************
5.05098E+08*      6    ********************
6.03922E+08*      7    ***********************
7.02745E+08*      8    **************************
8.01569E+08*      9    ******************************
9.00392E+08*     10    *********************************
9.99216E+08*     11    ************************************
1.09804E+09*     12    ****************************************
1.19686E+09*     13    *******************************************
1.29569E+09*     14    **********************************************
1.40549E+09*     15    **************************************************  1
1.50431E+09*     14    **********************************************
1.60314E+09*     13    *******************************************
1.70196E+09*     12    ****************************************
1.80078E+09*     11    ************************************
1.89961E+09*     10    *********************************
1.99843E+09*      9    ******************************
2.09725E+09*      8    **************************
2.19608E+09*      7    ***********************
2.29490E+09*      6    ********************
2.40471E+09*      5    ****************
2.50353E+09*      4    *************
2.60235E+09*      3    **********
2.70118E+09*      2    ******
2.80000E+09*      1    ***

AVERAGE GRAY LEVEL=1.400E+9
STANDARD DEVIATION=6.110E+8
NUMBER ELEMENTS=     225
MIN. DN=0.000000
MAX. DN=2.800E+9

hist g1515 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=1.400E+9       STANDARD DEVIATION=6.110E+8       NUMBER ELEMENTS=     225
MIN. DN=0.000000
MAX. DN=2.800E+9

hist g1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=1.400E+9       STANDARD DEVIATION=6.110E+8       NUMBER ELEMENTS=     225
MIN. DN=0.000000
MAX. DN=2.800E+9

putmsg "Print average and stdev variables" ""
Print average and stdev variables
disp AVG

AVG=1400000000.0

disp SD

SD=611010112.0

gen g1515 15 15 linc=-123456789.e4 sinc=-1234567890.e4 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
write " Try some wild numbers.  3000 bins.  225 are non-empty."
 Try some wild numbers.  3000 bins.  225 are non-empty.
write " Should skip all empty bins and put a * after DN to indicate skip."
 Should skip all empty bins and put a * after DN to indicate skip.
hist g1515 nlines=3000
Beginning VICAR task hist
HIST version 15-NOV-05

-1.9012E+14       1    *************************
-1.8892E+14*      1    *************************
-1.8765E+14*      1    *************************
-1.8645E+14*      1    *************************
-1.8518E+14*      1    *************************
-1.8397E+14*      1    *************************
-1.8271E+14*      1    *************************
-1.8150E+14*      1    *************************
-1.8023E+14*      1    *************************
-1.7903E+14*      1    *************************
-1.7776E+14*      2    **************************************************  1
-1.7656E+14*      2    **************************************************  2
-1.7529E+14*      2    **************************************************
-1.7408E+14*      2    **************************************************
-1.7282E+14*      2    **************************************************
-1.7161E+14*      1    *************************
-1.7034E+14*      1    *************************
-1.6914E+14*      1    *************************
-1.6787E+14*      1    *************************
-1.6667E+14*      1    *************************
-1.6546E+14*      2    **************************************************
-1.6419E+14*      2    **************************************************
-1.6299E+14*      2    **************************************************
-1.6172E+14*      2    **************************************************
-1.6052E+14*      2    **************************************************
-1.5925E+14*      1    *************************
-1.5805E+14*      1    *************************
-1.5678E+14*      1    *************************
-1.5557E+14*      1    *************************
-1.5430E+14*      1    *************************
-1.5310E+14*      2    **************************************************
-1.5183E+14*      2    **************************************************
-1.5063E+14*      2    **************************************************
-1.4936E+14*      2    **************************************************
-1.4816E+14*      2    **************************************************
-1.4689E+14*      1    *************************
-1.4568E+14*      1    *************************
-1.4442E+14*      1    *************************
-1.4321E+14*      1    *************************
-1.4201E+14*      1    *************************
-1.4074E+14*      2    **************************************************
-1.3953E+14*      2    **************************************************
-1.3827E+14*      2    **************************************************
-1.3706E+14*      2    **************************************************
-1.3579E+14*      2    **************************************************
-1.3459E+14*      1    *************************
-1.3332E+14*      1    *************************
-1.3212E+14*      1    *************************
-1.3085E+14*      1    *************************
-1.2964E+14*      1    *************************
-1.2838E+14*      2    **************************************************
-1.2717E+14*      2    **************************************************
-1.2590E+14*      2    **************************************************
-1.2470E+14*      2    **************************************************
-1.2343E+14*      2    **************************************************
-1.2223E+14*      1    *************************
-1.2096E+14*      1    *************************
-1.1975E+14*      1    *************************
-1.1855E+14*      1    *************************
-1.1728E+14*      1    *************************
-1.1608E+14*      2    **************************************************
-1.1481E+14*      2    **************************************************
-1.1360E+14*      2    **************************************************
-1.1234E+14*      2    **************************************************
-1.1113E+14*      2    **************************************************
-1.0986E+14*      1    *************************
-1.0866E+14*      1    *************************
-1.0739E+14*      1    *************************
-1.0619E+14*      1    *************************
-1.0492E+14*      1    *************************
-1.0372E+14*      2    **************************************************
-1.0245E+14*      2    **************************************************
-1.0124E+14*      2    **************************************************
-9.9975E+13*      2    **************************************************
-9.8770E+13*      2    **************************************************
-9.7502E+13*      1    *************************
-9.6298E+13*      1    *************************
-9.5030E+13*      1    *************************
-9.3826E+13*      1    *************************
-9.2621E+13*      1    *************************
-9.1353E+13*      2    **************************************************
-9.0149E+13*      2    **************************************************
-8.8881E+13*      2    **************************************************
-8.7676E+13*      2    **************************************************
-8.6408E+13*      2    **************************************************
-8.5204E+13*      1    *************************
-8.3936E+13*      1    *************************
-8.2731E+13*      1    *************************
-8.1463E+13*      1    *************************
-8.0259E+13*      1    *************************
-7.8991E+13*      2    **************************************************
-7.7786E+13*      2    **************************************************
-7.6519E+13*      2    **************************************************
-7.5314E+13*      2    **************************************************
-7.4046E+13*      2    **************************************************
-7.2842E+13*      1    *************************
-7.1574E+13*      1    *************************
-7.0369E+13*      1    *************************
-6.9165E+13*      1    *************************
-6.7897E+13*      1    *************************
-6.6692E+13*      2    **************************************************
-6.5424E+13*      2    **************************************************
-6.4220E+13*      2    **************************************************
-6.2952E+13*      2    **************************************************
-6.1747E+13*      2    **************************************************
-6.0479E+13*      1    *************************
-5.9275E+13*      1    *************************
-5.8007E+13*      1    *************************
-5.6802E+13*      1    *************************
-5.5535E+13*      1    *************************
-5.4330E+13*      2    **************************************************
-5.3062E+13*      2    **************************************************
-5.1858E+13*      2    **************************************************
-5.0590E+13*      2    **************************************************
-4.9385E+13*      2    **************************************************
-4.8117E+13*      1    *************************
-4.6913E+13*      1    *************************
-4.5708E+13*      1    *************************
-4.4440E+13*      1    *************************
-4.3236E+13*      1    *************************
-4.1968E+13*      2    **************************************************
-4.0763E+13*      2    **************************************************
-3.9495E+13*      2    **************************************************
-3.8291E+13*      2    **************************************************
-3.7023E+13*      2    **************************************************
-3.5819E+13*      1    *************************
-3.4551E+13*      1    *************************
-3.3346E+13*      1    *************************
-3.2078E+13*      1    *************************
-3.0874E+13*      1    *************************
-2.9606E+13*      2    **************************************************
-2.8401E+13*      2    **************************************************
-2.7133E+13*      2    **************************************************
-2.5929E+13*      2    **************************************************
-2.4661E+13*      2    **************************************************
-2.3456E+13*      1    *************************
-2.2252E+13*      1    *************************
-2.0984E+13*      1    *************************
-1.9779E+13*      1    *************************
-1.8512E+13*      1    *************************
-1.7307E+13*      2    **************************************************
-1.6039E+13*      2    **************************************************
-1.4835E+13*      2    **************************************************
-1.3567E+13*      2    **************************************************
-1.2362E+13*      2    **************************************************
-1.1094E+13*      1    *************************
-9.8897E+12*      1    *************************
-8.6218E+12*      1    *************************
-7.4173E+12*      1    *************************
-6.1494E+12*      1    *************************
-4.9449E+12*      1    *************************
-3.6769E+12*      1    *************************
-2.4724E+12*      1    *************************
-1.2045E+12*      1    *************************
3.27680E+05*      1    *************************

AVERAGE GRAY LEVEL=-9.5E+13
STANDARD DEVIATION=5.36E+13
NUMBER ELEMENTS=     225
MIN. DN=-1.9E+14
MAX. DN=0.000000

gen g1515 15 15 linc=-1.e-9 sinc=-1.e-9 'real8
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

-4.0000E-09       1    *************************
-2.9961E-09*      2    **************************************************  2
-1.9922E-09*      3    **************************************************  1
-1.0039E-09*      2    **************************************************
0.00000E+00*      1    *************************

AVERAGE GRAY LEVEL=-2.00E-9
STANDARD DEVIATION=1.155E-9
NUMBER ELEMENTS=       9
MIN. DN=-4.00E-9
MAX. DN=0.000000


-2.0000E-08       1    **************************************************  2
-1.8996E-08*      2    **************************************************  1
-1.8000E-08*      1    **************************************************

AVERAGE GRAY LEVEL=-1.90E-8
STANDARD DEVIATION=7.07E-10
NUMBER ELEMENTS=       4
MIN. DN=-2.00E-8
MAX. DN=-1.80E-8

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

-6.0000E-09       1    *************************
-4.9961E-09*      2    **************************************************  2
-3.9922E-09*      3    **************************************************  1
-3.0039E-09*      2    **************************************************
-2.0000E-09*      1    *************************

AVERAGE GRAY LEVEL=-4.00E-9
STANDARD DEVIATION=1.155E-9
NUMBER ELEMENTS=       9
MIN. DN=-6.00E-9
MAX. DN=-2.00E-9


-2.2000E-08       1    **************************************************  2
-2.0996E-08*      2    **************************************************  1
-2.0000E-08*      1    **************************************************

AVERAGE GRAY LEVEL=-2.10E-8
STANDARD DEVIATION=7.07E-10
NUMBER ELEMENTS=       4
MIN. DN=-2.20E-8
MAX. DN=-2.00E-8

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
HIST version 15-NOV-05

-2.6000E-08       1    ****************
-2.3961E-08*      1    ****************
-2.3043E-08*      1    ****************
-2.2024E-08*      1    ****************
-2.1004E-08*      1    ****************
-1.9984E-08*      2    *********************************
-1.8965E-08*      1    ****************
-1.8047E-08*      2    *********************************
-1.7027E-08*      2    *********************************
-1.6008E-08*      2    *********************************
-1.4988E-08*      2    *********************************
-1.3969E-08*      3    **************************************************  1
-1.2949E-08*      2    *********************************
-1.2031E-08*      3    **************************************************  2
-1.1012E-08*      2    *********************************
-9.9922E-09*      2    *********************************
-8.9725E-09*      2    *********************************
-7.9529E-09*      2    *********************************
-7.0353E-09*      1    ****************
-6.0157E-09*      2    *********************************
-4.9961E-09*      1    ****************
-3.9765E-09*      1    ****************
-2.9569E-09*      1    ****************
-2.0392E-09*      1    ****************
0.00000E+00*      1    ****************

AVERAGE GRAY LEVEL=-1.30E-8
STANDARD DEVIATION=6.245E-9
NUMBER ELEMENTS=      40
MIN. DN=-2.60E-8
MAX. DN=0.000000

HIST g1515 NLINES=10 LIMITS= (-24.e-9, -5.E-9)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT       6    ***********
***********      15    ***************************
***********      15    ***************************
***********      19    ***********************************
***********      23    ******************************************
***********      27    **************************************************  2
***********      29    **************************************************  1
***********      25    **********************************************
***********      21    **************************************
***********      17    *******************************
***********      18    *********************************
>HIGH LIMIT      10    ******************

AVERAGE GRAY LEVEL=-1.40E-8
STANDARD DEVIATION=6.110E-9
NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8
MAX. DN=0.000000

HIST g1515 NLINES=10 LIMITS=(-23.e-9, -5.E-9)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      10    ******************
***********      11    ********************
***********      15    ***************************
***********      19    ***********************************
***********      23    ******************************************
***********      27    **************************************************  2
***********      29    **************************************************  1
***********      25    **********************************************
***********      21    **************************************
***********      17    *******************************
***********      13    ************************
>HIGH LIMIT      15    ***************************

AVERAGE GRAY LEVEL=-1.40E-8
STANDARD DEVIATION=6.110E-9
NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8
MAX. DN=0.000000

HIST g1515 'nocum          LIMITS=(-23.e-9, -5.E-9) 'wide
Beginning VICAR task HIST
HIST version 15-NOV-05

FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ  PCTILE           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT      15   6.667  ****************************************************************************************************+1
-2.3000E-08       6   2.667  ****************************************+         +         +         +         +         +         +

-2.2012E-08       7   3.111  **********************************************    +         +         +         +         +         +

-2.1024E-08       8   3.556  *****************************************************       +         +         +         +         +

-1.9965E-08       9   4.000  ************************************************************+         +         +         +         +

-1.8976E-08      10   4.444  ******************************************************************    +         +         +         +

-1.7988E-08      11   4.889  *************************************************************************       +         +         +

-1.7000E-08      12   5.333  ********************************************************************************+         +         +

-1.6012E-08      13   5.778  **************************************************************************************    +         +

-1.5024E-08      14   6.222  *********************************************************************************************       +

-1.3965E-08      15   6.667  ****************************************************************************************************+2

-1.2976E-08      14   6.222  *********************************************************************************************       +

-1.1988E-08      13   5.778  **************************************************************************************    +         +

-1.1000E-08      12   5.333  ********************************************************************************+         +         +

-1.0012E-08      11   4.889  *************************************************************************       +         +         +

-9.0235E-09      10   4.444  ******************************************************************    +         +         +         +

-7.9647E-09       9   4.000  ************************************************************+         +         +         +         +

-6.9765E-09       8   3.556  *****************************************************       +         +         +         +         +

-5.9882E-09       7   3.111  **********************************************    +         +         +         +         +         +

-5.0000E-09       6   2.667  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT      15   6.667  ****************************************************************************************************+

AVERAGE GRAY LEVEL=-1.40E-8       STANDARD DEVIATION=6.110E-9       NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8      MAX. DN=0.000000

HIST g1515 SPIKE=5
Beginning VICAR task HIST
HIST version 15-NOV-05

-2.8000E-08       1    ***
-2.7012E-08*      2    *******
-2.6024E-08*      3    ***********
-2.5035E-08*      4    ***************
-2.4047E-08*      5    *******************
-2.2949E-08*      6    ***********************
-2.1961E-08*      7    **************************
-2.0973E-08*      8    ******************************
-1.9984E-08*      9    **********************************
-1.8996E-08*     10    **************************************
-1.8008E-08*     11    ******************************************
-1.7020E-08*     12    **********************************************
-1.6031E-08*     13    **************************************************  4
-1.5043E-08*     14    **************************************************  2
-1.3945E-08*     15    **************************************************  1
-1.2957E-08*     14    **************************************************  3
-1.1969E-08*     13    **************************************************  5
-1.0980E-08*     12    **********************************************
-9.9922E-09*     11    ******************************************
-9.0039E-09*     10    **************************************
-8.0157E-09*      9    **********************************
-7.0275E-09*      8    ******************************
-6.0392E-09*      7    **************************
-5.0510E-09*      6    ***********************
-3.9529E-09*      5    *******************
-2.9647E-09*      4    ***************
-1.9765E-09*      3    ***********
-9.8824E-10*      2    *******
0.00000E+00*      1    ***

AVERAGE GRAY LEVEL=-1.40E-8
STANDARD DEVIATION=6.110E-9
NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8
MAX. DN=0.000000

HIST g1515 SPIKE=1
Beginning VICAR task HIST
HIST version 15-NOV-05

-2.8000E-08       1    ***
-2.7012E-08*      2    ******
-2.6024E-08*      3    **********
-2.5035E-08*      4    *************
-2.4047E-08*      5    ****************
-2.2949E-08*      6    ********************
-2.1961E-08*      7    ***********************
-2.0973E-08*      8    **************************
-1.9984E-08*      9    ******************************
-1.8996E-08*     10    *********************************
-1.8008E-08*     11    ************************************
-1.7020E-08*     12    ****************************************
-1.6031E-08*     13    *******************************************
-1.5043E-08*     14    **********************************************
-1.3945E-08*     15    **************************************************  1
-1.2957E-08*     14    **********************************************
-1.1969E-08*     13    *******************************************
-1.0980E-08*     12    ****************************************
-9.9922E-09*     11    ************************************
-9.0039E-09*     10    *********************************
-8.0157E-09*      9    ******************************
-7.0275E-09*      8    **************************
-6.0392E-09*      7    ***********************
-5.0510E-09*      6    ********************
-3.9529E-09*      5    ****************
-2.9647E-09*      4    *************
-1.9765E-09*      3    **********
-9.8824E-10*      2    ******
0.00000E+00*      1    ***

AVERAGE GRAY LEVEL=-1.40E-8
STANDARD DEVIATION=6.110E-9
NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8
MAX. DN=0.000000

hist g1515 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=-1.40E-8       STANDARD DEVIATION=6.110E-9       NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8
MAX. DN=0.000000

hist g1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=-1.40E-8       STANDARD DEVIATION=6.110E-9       NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8
MAX. DN=0.000000

putmsg "Print average and stdev variables" ""
Print average and stdev variables
disp AVG

AVG=-1.40000002702e-08

disp SD

SD=6.11010131379e-09

gen g1515 NS=10 NL=10 NB=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515
Beginning VICAR task hist
HIST version 15-NOV-05

          0       1
          1       3    **
          2       6    ****
          3      10    ******
          4      15    **********
          5      21    **************
          6      28    ******************
          7      36    ************************
          8      45    ******************************
          9      55    ************************************
         10      63    ******************************************
         11      69    **********************************************
         12      73    ************************************************
         13      75    **************************************************  1
         14      75    **************************************************  2
         15      73    ************************************************
         16      69    **********************************************
         17      63    ******************************************
         18      55    ************************************
         19      45    ******************************
         20      36    ************************
         21      28    ******************
         22      21    **************
         23      15    **********
         24      10    ******
         25       6    ****
         26       3    **
         27       1

AVERAGE GRAY LEVEL=13.50000
STANDARD DEVIATION=4.974937
NUMBER ELEMENTS=    1000
MIN. DN=         0
MAX. DN=        27

hist g1515 NB=4
Beginning VICAR task hist
HIST version 15-NOV-05

          0       1    *
          1       3    ****
          2       6    ********
          3      10    *************
          4      14    *******************
          5      18    *************************
          6      22    ******************************
          7      26    ************************************
          8      30    *****************************************
          9      34    ***********************************************
         10      36    **************************************************  1
         11      36    **************************************************  2
         12      34    ***********************************************
         13      30    *****************************************
         14      26    ************************************
         15      22    ******************************
         16      18    *************************
         17      14    *******************
         18      10    *************
         19       6    ********
         20       3    ****
         21       1    *

AVERAGE GRAY LEVEL=10.50000
STANDARD DEVIATION=4.213075
NUMBER ELEMENTS=     400
MIN. DN=         0
MAX. DN=        21

gen g1515 337 364 ival=200 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist a
Beginning VICAR task hist
HIST version 15-NOV-05

        200* 122668    **************************************************  1

AVERAGE GRAY LEVEL=200.0000
STANDARD DEVIATION=0.000000
NUMBER ELEMENTS=  122668
MIN. DN=       200
MAX. DN=       200

end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$!-----------------------------------------------------------------------------
$ create session.log.x86-linux
tsthist
let $autousage="none"
gen g1515 15 15
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

          0       1    *************************
          1       2    **************************************************  2
          2       3    **************************************************  1
          3       2    **************************************************
          4       1    *************************

AVERAGE GRAY LEVEL=2.000000
STANDARD DEVIATION=1.154701
NUMBER ELEMENTS=       9
MIN. DN=         0
MAX. DN=         4


         18*      1    **************************************************  2
         19       2    **************************************************  1
         20       1    **************************************************

AVERAGE GRAY LEVEL=19.00000
STANDARD DEVIATION=0.707107
NUMBER ELEMENTS=       4
MIN. DN=        18
MAX. DN=        20

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

          2*      1    *************************
          3       2    **************************************************  2
          4       3    **************************************************  1
          5       2    **************************************************
          6       1    *************************

AVERAGE GRAY LEVEL=4.000000
STANDARD DEVIATION=1.154701
NUMBER ELEMENTS=       9
MIN. DN=         2
MAX. DN=         6


         20*      1    **************************************************  2
         21       2    **************************************************  1
         22       1    **************************************************

AVERAGE GRAY LEVEL=21.00000
STANDARD DEVIATION=0.707107
NUMBER ELEMENTS=       4
MIN. DN=        20
MAX. DN=        22

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
HIST version 15-NOV-05

          0       1    ****************
          2*      1    ****************
          3       1    ****************
          4       1    ****************
          5       1    ****************
          6       2    *********************************
          7       1    ****************
          8       2    *********************************
          9       2    *********************************
         10       2    *********************************
         11       2    *********************************
         12       3    **************************************************  1
         13       2    *********************************
         14       3    **************************************************  2
         15       2    *********************************
         16       2    *********************************
         17       2    *********************************
         18       2    *********************************
         19       1    ****************
         20       2    *********************************
         21       1    ****************
         22       1    ****************
         23       1    ****************
         24       1    ****************
         26*      1    ****************

AVERAGE GRAY LEVEL=13.00000
STANDARD DEVIATION=6.244998
NUMBER ELEMENTS=      40
MIN. DN=         0
MAX. DN=        26

HIST g1515 NLINES=10 LIMITS=(5,24)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      10    ******************
        5.0      18    *********************************
        7.1      17    *******************************
        9.2      21    **************************************
       11.3      25    **********************************************
       13.4      29    **************************************************  1
       15.6      27    **************************************************  2
       17.7      23    ******************************************
       19.8      19    ***********************************
       21.9      15    ***************************
       24.0      15    ***************************
>HIGH LIMIT       6    ***********

AVERAGE GRAY LEVEL=14.00000
STANDARD DEVIATION=6.110101
NUMBER ELEMENTS=     225
MIN. DN=         0
MAX. DN=        28

HIST g1515 NLINES=10 LIMITS=(5,23)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      10    ******************
          5      11    ********************
          7      15    ***************************
          9      19    ***********************************
         11      23    ******************************************
         13      27    **************************************************  2
         15      29    **************************************************  1
         17      25    **********************************************
         19      21    **************************************
         21      17    *******************************
         23      13    ************************
>HIGH LIMIT      15    ***************************

AVERAGE GRAY LEVEL=14.00000
STANDARD DEVIATION=6.110101
NUMBER ELEMENTS=     225
MIN. DN=         0
MAX. DN=        28

HIST g1515           LIMITS=(5,23) 'wide
Beginning VICAR task HIST
HIST version 15-NOV-05

FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ     CDF           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT      15   6.667  ****************************************************************************************************+1
          5       6   9.333  ****************************************+         +         +         +         +         +         +
          6       7  12.444  **********************************************    +         +         +         +         +         +
          7       8  16.000  *****************************************************       +         +         +         +         +
          8       9  20.000  ************************************************************+         +         +         +         +
          9      10  24.444  ******************************************************************    +         +         +         +
         10      11  29.333  *************************************************************************       +         +         +
         11      12  34.667  ********************************************************************************+         +         +
         12      13  40.444  **************************************************************************************    +         +
         13      14  46.667  *********************************************************************************************       +
         14      15  53.333  ****************************************************************************************************+2
         15      14  59.556  *********************************************************************************************       +
         16      13  65.333  **************************************************************************************    +         +
         17      12  70.667  ********************************************************************************+         +         +
         18      11  75.556  *************************************************************************       +         +         +
         19      10  80.000  ******************************************************************    +         +         +         +
         20       9  84.000  ************************************************************+         +         +         +         +
         21       8  87.556  *****************************************************       +         +         +         +         +
         22       7  90.667  **********************************************    +         +         +         +         +         +
         23       6  93.333  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT      15 100.000  ****************************************************************************************************+

AVERAGE GRAY LEVEL=14.00000       STANDARD DEVIATION=6.110101       NUMBER ELEMENTS=     225
MIN. DN=         0    MAX. DN=        28

HIST g1515 SPIKE=5
Beginning VICAR task HIST
HIST version 15-NOV-05

          0       1    ***
          1       2    *******
          2       3    ***********
          3       4    ***************
          4       5    *******************
          5       6    ***********************
          6       7    **************************
          7       8    ******************************
          8       9    **********************************
          9      10    **************************************
         10      11    ******************************************
         11      12    **********************************************
         12      13    **************************************************  4
         13      14    **************************************************  2
         14      15    **************************************************  1
         15      14    **************************************************  3
         16      13    **************************************************  5
         17      12    **********************************************
         18      11    ******************************************
         19      10    **************************************
         20       9    **********************************
         21       8    ******************************
         22       7    **************************
         23       6    ***********************
         24       5    *******************
         25       4    ***************
         26       3    ***********
         27       2    *******
         28       1    ***

AVERAGE GRAY LEVEL=14.00000
STANDARD DEVIATION=6.110101
NUMBER ELEMENTS=     225
MIN. DN=         0
MAX. DN=        28

HIST g1515 SPIKE=1
Beginning VICAR task HIST
HIST version 15-NOV-05

          0       1    ***
          1       2    ******
          2       3    **********
          3       4    *************
          4       5    ****************
          5       6    ********************
          6       7    ***********************
          7       8    **************************
          8       9    ******************************
          9      10    *********************************
         10      11    ************************************
         11      12    ****************************************
         12      13    *******************************************
         13      14    **********************************************
         14      15    **************************************************  1
         15      14    **********************************************
         16      13    *******************************************
         17      12    ****************************************
         18      11    ************************************
         19      10    *********************************
         20       9    ******************************
         21       8    **************************
         22       7    ***********************
         23       6    ********************
         24       5    ****************
         25       4    *************
         26       3    **********
         27       2    ******
         28       1    ***

AVERAGE GRAY LEVEL=14.00000
STANDARD DEVIATION=6.110101
NUMBER ELEMENTS=     225
MIN. DN=         0
MAX. DN=        28

hist g1515 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=14.00000       STANDARD DEVIATION=6.110101       NUMBER ELEMENTS=     225
MIN. DN=         0
MAX. DN=        28

hist g1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=14.00000       STANDARD DEVIATION=6.110101       NUMBER ELEMENTS=     225
MIN. DN=         0
MAX. DN=        28

putmsg "Print average and stdev variables" ""
Print average and stdev variables
disp AVG

AVG=14.0

disp SD

SD=6.11010074615

hist g1515 'screen spike=1
Beginning VICAR task hist
HIST version 15-NOV-05

              1
             ***
             ***
            *****
           *******
          *********
         ***********
         ***********
        *************
       ***************
      *****************
     *******************
     *******************
    *********************
   ***********************
  *************************
 ***************************
 ***************************
*****************************

          1    1    2    2
0    5    0    5    0    5
       225PIXELS   RANGE   0- 28     MEAN  14.000     STD DEV   6.110
gen g 10 10 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g 'exclude
Beginning VICAR task hist
HIST version 15-NOV-05

          0     100    **************************************************  1

EXCLUDING PIXELS OF DN=0
AVERAGE GRAY LEVEL=0.000000
STANDARD DEVIATION=0.000000
NUMBER ELEMENTS=       0
MIN. DN=         0
MAX. DN=         0

gen g 1024 1000 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
write "Should get mean =128.0 because of exclude"
Should get mean =128.0 because of exclude
hist g nlines=10 'exclude
Beginning VICAR task hist
HIST version 15-NOV-05

          0   60000    *************************
         28  112000    ************************************************
         57  112000    ************************************************
         85  116000    **************************************************  1
        113  112000    ************************************************
        142  112000    ************************************************
        170  116000    **************************************************  2
        198  112000    ************************************************
        227  112000    ************************************************
        255   60000    *************************

EXCLUDING PIXELS OF DN=0
AVERAGE GRAY LEVEL=128.0000
STANDARD DEVIATION=73.61160
NUMBER ELEMENTS= 1020000
MIN. DN=         0
MAX. DN=       255

gen g 10 10 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g inc=3
Beginning VICAR task hist
HIST version 15-NOV-05

          0       4    **************************************************  1
          3*      4    **************************************************  2
          6*      4    **************************************************
          9*      4    **************************************************

AVERAGE GRAY LEVEL=4.500000
STANDARD DEVIATION=3.354102
NUMBER ELEMENTS=      16
MIN. DN=         0
MAX. DN=         9

write "Mean should be 4.5."
Mean should be 4.5.
hist g inc=3 'exclude
Beginning VICAR task hist
HIST version 15-NOV-05

          0       4    **************************************************  1
          3*      4    **************************************************  2
          6*      4    **************************************************
          9*      4    **************************************************

EXCLUDING PIXELS OF DN=0
AVERAGE GRAY LEVEL=6.000000
STANDARD DEVIATION=2.449490
NUMBER ELEMENTS=      12
MIN. DN=         0
MAX. DN=         9

write "Mean should be 6.0."
Mean should be 6.0.
gen g1515 15 15 linc=-1000 sinc=-1000 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

      -4000       1    *************************
      -2976*      2    **************************************************  2
      -1952*      3    **************************************************  1
       -928*      2    **************************************************
         96*      1    *************************

AVERAGE GRAY LEVEL=-2000.00
STANDARD DEVIATION=1154.701
NUMBER ELEMENTS=       9
MIN. DN=     -4000
MAX. DN=         0


     -20000       1    **************************************************  2
     -18976*      2    **************************************************  1
     -17952*      1    **************************************************

AVERAGE GRAY LEVEL=-19000.0
STANDARD DEVIATION=707.1068
NUMBER ELEMENTS=       4
MIN. DN=    -20000
MAX. DN=    -18000

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

      -6000       1    *************************
      -4976*      2    **************************************************  2
      -3952*      3    **************************************************  1
      -2928*      2    **************************************************
      -1904*      1    *************************

AVERAGE GRAY LEVEL=-4000.00
STANDARD DEVIATION=1154.701
NUMBER ELEMENTS=       9
MIN. DN=     -6000
MAX. DN=     -2000


     -22000       1    **************************************************  2
     -20976*      2    **************************************************  1
     -19952*      1    **************************************************

AVERAGE GRAY LEVEL=-21000.0
STANDARD DEVIATION=707.1068
NUMBER ELEMENTS=       4
MIN. DN=    -22000
MAX. DN=    -20000

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
HIST version 15-NOV-05

     -26000       1    ****************
     -23952*      1    ****************
     -22928*      1    ****************
     -21904*      1    ****************
     -20880*      1    ****************
     -20112*      2    *********************************
     -19088*      1    ****************
     -18064*      2    *********************************
     -17040*      2    *********************************
     -16016*      2    *********************************
     -14992*      2    *********************************
     -13968*      3    **************************************************  1
     -12944*      2    *********************************
     -11920*      3    **************************************************  2
     -10896*      2    *********************************
      -9872*      2    *********************************
      -9104*      2    *********************************
      -8080*      2    *********************************
      -7056*      1    ****************
      -6032*      2    *********************************
      -5008*      1    ****************
      -3984*      1    ****************
      -2960*      1    ****************
      -1936*      1    ****************
        112*      1    ****************

AVERAGE GRAY LEVEL=-13000.0
STANDARD DEVIATION=6244.998
NUMBER ELEMENTS=      40
MIN. DN=    -26000
MAX. DN=         0

HIST g1515 NLINES=10 LIMITS=(-24000, -5000)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT       6    ***********
     -24000      15    ***************************
     -21889      15    ***************************
     -19778      19    ***********************************
     -17667      23    ******************************************
     -15556      27    **************************************************  2
     -13444      29    **************************************************  1
     -11333      25    **********************************************
      -9222      21    **************************************
      -7111      17    *******************************
      -5000      18    *********************************
>HIGH LIMIT      10    ******************

AVERAGE GRAY LEVEL=-14000.0
STANDARD DEVIATION=6110.101
NUMBER ELEMENTS=     225
MIN. DN=    -28000
MAX. DN=         0

HIST g1515 NLINES=10 LIMITS=(-23000, -5000)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      10    ******************
     -23000      11    ********************
     -21000      15    ***************************
     -19000      19    ***********************************
     -17000      23    ******************************************
     -15000      27    **************************************************  2
     -13000      29    **************************************************  1
     -11000      25    **********************************************
      -9000      21    **************************************
      -7000      17    *******************************
      -5000      13    ************************
>HIGH LIMIT      15    ***************************

AVERAGE GRAY LEVEL=-14000.0
STANDARD DEVIATION=6110.101
NUMBER ELEMENTS=     225
MIN. DN=    -28000
MAX. DN=         0

HIST g1515           LIMITS=(-23000, -5000) 'wide
Beginning VICAR task HIST
HIST version 15-NOV-05

FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ     CDF           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT      15   6.667  ****************************************************************************************************+1
     -23000       6   9.333  ****************************************+         +         +         +         +         +         +

     -22012       7  12.444  **********************************************    +         +         +         +         +         +

     -21024       8  16.000  *****************************************************       +         +         +         +         +

     -19965       9  20.000  ************************************************************+         +         +         +         +

     -18976      10  24.444  ******************************************************************    +         +         +         +

     -17988      11  29.333  *************************************************************************       +         +         +

     -17000      12  34.667  ********************************************************************************+         +         +

     -16012      13  40.444  **************************************************************************************    +         +

     -15024      14  46.667  *********************************************************************************************       +

     -13965      15  53.333  ****************************************************************************************************+2

     -12976      14  59.556  *********************************************************************************************       +

     -11988      13  65.333  **************************************************************************************    +         +

     -11000      12  70.667  ********************************************************************************+         +         +

     -10012      11  75.556  *************************************************************************       +         +         +

      -9024      10  80.000  ******************************************************************    +         +         +         +

      -7965       9  84.000  ************************************************************+         +         +         +         +

      -6976       8  87.556  *****************************************************       +         +         +         +         +

      -5988       7  90.667  **********************************************    +         +         +         +         +         +

      -5000       6  93.333  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT      15 100.000  ****************************************************************************************************+

AVERAGE GRAY LEVEL=-14000.0       STANDARD DEVIATION=6110.101       NUMBER ELEMENTS=     225
MIN. DN=    -28000    MAX. DN=         0

HIST g1515 SPIKE=5
Beginning VICAR task HIST
HIST version 15-NOV-05

     -28000       1    ***
     -26976*      2    *******
     -25952*      3    ***********
     -24928*      4    ***************
     -23904*      5    *******************
     -22880*      6    ***********************
     -22112*      7    **************************
     -21088*      8    ******************************
     -20064*      9    **********************************
     -19040*     10    **************************************
     -18016*     11    ******************************************
     -16992*     12    **********************************************
     -15968*     13    **************************************************  4
     -14944*     14    **************************************************  2
     -13920*     15    **************************************************  1
     -12896*     14    **************************************************  3
     -11872*     13    **************************************************  5
     -11104*     12    **********************************************
     -10080*     11    ******************************************
      -9056*     10    **************************************
      -8032*      9    **********************************
      -7008*      8    ******************************
      -5984*      7    **************************
      -4960*      6    ***********************
      -3936*      5    *******************
      -2912*      4    ***************
      -1888*      3    ***********
      -1120*      2    *******
        -96*      1    ***

AVERAGE GRAY LEVEL=-14000.0
STANDARD DEVIATION=6110.101
NUMBER ELEMENTS=     225
MIN. DN=    -28000
MAX. DN=         0

HIST g1515 SPIKE=1
Beginning VICAR task HIST
HIST version 15-NOV-05

     -28000       1    ***
     -26976*      2    ******
     -25952*      3    **********
     -24928*      4    *************
     -23904*      5    ****************
     -22880*      6    ********************
     -22112*      7    ***********************
     -21088*      8    **************************
     -20064*      9    ******************************
     -19040*     10    *********************************
     -18016*     11    ************************************
     -16992*     12    ****************************************
     -15968*     13    *******************************************
     -14944*     14    **********************************************
     -13920*     15    **************************************************  1
     -12896*     14    **********************************************
     -11872*     13    *******************************************
     -11104*     12    ****************************************
     -10080*     11    ************************************
      -9056*     10    *********************************
      -8032*      9    ******************************
      -7008*      8    **************************
      -5984*      7    ***********************
      -4960*      6    ********************
      -3936*      5    ****************
      -2912*      4    *************
      -1888*      3    **********
      -1120*      2    ******
        -96*      1    ***

AVERAGE GRAY LEVEL=-14000.0
STANDARD DEVIATION=6110.101
NUMBER ELEMENTS=     225
MIN. DN=    -28000
MAX. DN=         0

hist g1515 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=-14000.0       STANDARD DEVIATION=6110.101       NUMBER ELEMENTS=     225
MIN. DN=    -28000
MAX. DN=         0

hist g1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=-14000.0       STANDARD DEVIATION=6110.101       NUMBER ELEMENTS=     225
MIN. DN=    -28000
MAX. DN=         0

putmsg "Print average and stdev variables" ""
Print average and stdev variables
disp AVG

AVG=-14000.0

disp SD

SD=6110.10107422

gen g 10 10 linc=1000 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g nlines=30 spikes=9
Beginning VICAR task hist
HIST version 15-NOV-05

          0       2    *************************
          4       4    **************************************************  1
          8       4    **************************************************  2
       1000*      2    *************************
       1004       4    **************************************************  3
       1008       4    **************************************************  4
       2000*      2    *************************
       2004       4    **************************************************  5
       2008       4    **************************************************  6
       3000*      2    *************************
       3004       4    **************************************************  7
       3008       4    **************************************************  8
       4000*      2    *************************
       4004       4    **************************************************  9
       4008       4    **************************************************
       5000*      2    *************************
       5004       4    **************************************************
       5008       4    **************************************************
       6000*      2    *************************
       6004       4    **************************************************
       6008       4    **************************************************
       7000*      2    *************************
       7004       4    **************************************************
       7008       4    **************************************************
       8000*      2    *************************
       8004       4    **************************************************
       8008       4    **************************************************
       9000*      2    *************************
       9004       4    **************************************************
       9008       4    **************************************************

AVERAGE GRAY LEVEL=4504.500
STANDARD DEVIATION=2872.283
NUMBER ELEMENTS=     100
MIN. DN=         0
MAX. DN=      9009

gen g1515 15 15 linc=-100000 sinc=-100000 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

    -400000       1    *************************
    -299608*      2    **************************************************  2
    -199216*      3    **************************************************  1
    -100392*      2    **************************************************
          0*      1    *************************

AVERAGE GRAY LEVEL=-200000.
STANDARD DEVIATION=115470.1
NUMBER ELEMENTS=       9
MIN. DN=   -400000
MAX. DN=         0


   -2000000       1    **************************************************  2
   -1899608*      2    **************************************************  1
   -1800000*      1    **************************************************

AVERAGE GRAY LEVEL=-1.90E+6
STANDARD DEVIATION=70710.68
NUMBER ELEMENTS=       4
MIN. DN=  -2000000
MAX. DN=  -1800000

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

    -600000       1    *************************
    -499608*      2    **************************************************  2
    -399216*      3    **************************************************  1
    -300392*      2    **************************************************
    -200000*      1    *************************

AVERAGE GRAY LEVEL=-400000.
STANDARD DEVIATION=115470.1
NUMBER ELEMENTS=       9
MIN. DN=   -600000
MAX. DN=   -200000


   -2200000       1    **************************************************  2
   -2099608*      2    **************************************************  1
   -2000000*      1    **************************************************

AVERAGE GRAY LEVEL=-2.10E+6
STANDARD DEVIATION=70710.68
NUMBER ELEMENTS=       4
MIN. DN=  -2200000
MAX. DN=  -2000000

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
HIST version 15-NOV-05

   -2600000       1    ****************
   -2396078*      1    ****************
   -2304314*      1    ****************
   -2202353*      1    ****************
   -2100392*      1    ****************
   -1998431*      2    *********************************
   -1896471*      1    ****************
   -1804706*      2    *********************************
   -1702745*      2    *********************************
   -1600784*      2    *********************************
   -1498824*      2    *********************************
   -1396863*      3    **************************************************  1
   -1294902*      2    *********************************
   -1203137*      3    **************************************************  2
   -1101177*      2    *********************************
    -999216*      2    *********************************
    -897255*      2    *********************************
    -795294*      2    *********************************
    -703529*      1    ****************
    -601569*      2    *********************************
    -499608*      1    ****************
    -397647*      1    ****************
    -295686*      1    ****************
    -203922*      1    ****************
          0*      1    ****************

AVERAGE GRAY LEVEL=-1.30E+6
STANDARD DEVIATION=624499.8
NUMBER ELEMENTS=      40
MIN. DN=  -2600000
MAX. DN=         0

HIST g1515 NLINES=10 LIMITS=(-2400000, -500000)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT       6    ***********
   -2400000      15    ***************************
   -2188889      15    ***************************
   -1977778      19    ***********************************
   -1766667      23    ******************************************
   -1555556      27    **************************************************  2
   -1344444      29    **************************************************  1
   -1133333      25    **********************************************
    -922222      21    **************************************
    -711111      17    *******************************
    -500000      18    *********************************
>HIGH LIMIT      10    ******************

AVERAGE GRAY LEVEL=-1.40E+6
STANDARD DEVIATION=611010.1
NUMBER ELEMENTS=     225
MIN. DN=  -2800000
MAX. DN=         0

HIST g1515 NLINES=10 LIMITS=(-2300000, -500000)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      10    ******************
   -2300000      11    ********************
   -2100000      15    ***************************
   -1900000      19    ***********************************
   -1700000      23    ******************************************
   -1500000      27    **************************************************  2
   -1300000      29    **************************************************  1
   -1100000      25    **********************************************
    -900000      21    **************************************
    -700000      17    *******************************
    -500000      13    ************************
>HIGH LIMIT      15    ***************************

AVERAGE GRAY LEVEL=-1.40E+6
STANDARD DEVIATION=611010.1
NUMBER ELEMENTS=     225
MIN. DN=  -2800000
MAX. DN=         0

HIST g1515           LIMITS=(-2300000, -500000) 'wide
Beginning VICAR task HIST
HIST version 15-NOV-05

FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ     CDF           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT      15   6.667  ****************************************************************************************************+1
   -2300000       6   9.333  ****************************************+         +         +         +         +         +         +

   -2201176       7  12.444  **********************************************    +         +         +         +         +         +

   -2102353       8  16.000  *****************************************************       +         +         +         +         +

   -2003529       9  20.000  ************************************************************+         +         +         +         +

   -1897647      10  24.444  ******************************************************************    +         +         +         +

   -1798824      11  29.333  *************************************************************************       +         +         +

   -1700000      12  34.667  ********************************************************************************+         +         +

   -1601176      13  40.444  **************************************************************************************    +         +

   -1502353      14  46.667  *********************************************************************************************       +

   -1403529      15  53.333  ****************************************************************************************************+2

   -1297647      14  59.556  *********************************************************************************************       +

   -1198823      13  65.333  **************************************************************************************    +         +

   -1100000      12  70.667  ********************************************************************************+         +         +

   -1001176      11  75.556  *************************************************************************       +         +         +

    -902353      10  80.000  ******************************************************************    +         +         +         +

    -803529       9  84.000  ************************************************************+         +         +         +         +

    -697647       8  87.556  *****************************************************       +         +         +         +         +

    -598823       7  90.667  **********************************************    +         +         +         +         +         +

    -500000       6  93.333  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT      15 100.000  ****************************************************************************************************+

AVERAGE GRAY LEVEL=-1.40E+6       STANDARD DEVIATION=611010.1       NUMBER ELEMENTS=     225
MIN. DN=  -2800000    MAX. DN=         0

HIST g1515 SPIKE=5
Beginning VICAR task HIST
HIST version 15-NOV-05

   -2800000       1    ***
   -2701176*      2    *******
   -2602353*      3    ***********
   -2503529*      4    ***************
   -2404706*      5    *******************
   -2294902*      6    ***********************
   -2196078*      7    **************************
   -2097255*      8    ******************************
   -1998431*      9    **********************************
   -1899608*     10    **************************************
   -1800784*     11    ******************************************
   -1701961*     12    **********************************************
   -1603137*     13    **************************************************  4
   -1504314*     14    **************************************************  2
   -1405490*     15    **************************************************  1
   -1295686*     14    **************************************************  3
   -1196863*     13    **************************************************  5
   -1098039*     12    **********************************************
    -999216*     11    ******************************************
    -900392*     10    **************************************
    -801569*      9    **********************************
    -702745*      8    ******************************
    -603921*      7    **************************
    -505098*      6    ***********************
    -395294*      5    *******************
    -296470*      4    ***************
    -197647*      3    ***********
     -98823*      2    *******
          0*      1    ***

AVERAGE GRAY LEVEL=-1.40E+6
STANDARD DEVIATION=611010.1
NUMBER ELEMENTS=     225
MIN. DN=  -2800000
MAX. DN=         0

HIST g1515 SPIKE=1
Beginning VICAR task HIST
HIST version 15-NOV-05

   -2800000       1    ***
   -2701176*      2    ******
   -2602353*      3    **********
   -2503529*      4    *************
   -2404706*      5    ****************
   -2294902*      6    ********************
   -2196078*      7    ***********************
   -2097255*      8    **************************
   -1998431*      9    ******************************
   -1899608*     10    *********************************
   -1800784*     11    ************************************
   -1701961*     12    ****************************************
   -1603137*     13    *******************************************
   -1504314*     14    **********************************************
   -1405490*     15    **************************************************  1
   -1295686*     14    **********************************************
   -1196863*     13    *******************************************
   -1098039*     12    ****************************************
    -999216*     11    ************************************
    -900392*     10    *********************************
    -801569*      9    ******************************
    -702745*      8    **************************
    -603921*      7    ***********************
    -505098*      6    ********************
    -395294*      5    ****************
    -296470*      4    *************
    -197647*      3    **********
     -98823*      2    ******
          0*      1    ***

AVERAGE GRAY LEVEL=-1.40E+6
STANDARD DEVIATION=611010.1
NUMBER ELEMENTS=     225
MIN. DN=  -2800000
MAX. DN=         0

hist g1515 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=-1.40E+6       STANDARD DEVIATION=611010.1       NUMBER ELEMENTS=     225
MIN. DN=  -2800000
MAX. DN=         0

hist g1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=-1.40E+6       STANDARD DEVIATION=611010.1       NUMBER ELEMENTS=     225
MIN. DN=  -2800000
MAX. DN=         0

putmsg "Print average and stdev variables" ""
Print average and stdev variables
disp AVG

AVG=-1400000.0

disp SD

SD=611010.0625

gen g1515 15 15 linc=1.e8 sinc=1.e8 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

0.00000E+00       1    *************************
1.00392E+08*      2    **************************************************  2
1.99216E+08*      3    **************************************************  1
2.99608E+08*      2    **************************************************
4.00000E+08*      1    *************************

AVERAGE GRAY LEVEL=2.000E+8
STANDARD DEVIATION=1.155E+8
NUMBER ELEMENTS=       9
MIN. DN=0.000000
MAX. DN=4.000E+8


1.80000E+09       1    **************************************************  2
1.89961E+09*      2    **************************************************  1
2.00000E+09*      1    **************************************************

AVERAGE GRAY LEVEL=1.900E+9
STANDARD DEVIATION=7.071E+7
NUMBER ELEMENTS=       4
MIN. DN=1.800E+9
MAX. DN=2.000E+9

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

2.00000E+08       1    *************************
3.00392E+08*      2    **************************************************  2
3.99216E+08*      3    **************************************************  1
4.99608E+08*      2    **************************************************
6.00000E+08*      1    *************************

AVERAGE GRAY LEVEL=4.000E+8
STANDARD DEVIATION=1.155E+8
NUMBER ELEMENTS=       9
MIN. DN=2.000E+8
MAX. DN=6.000E+8


2.00000E+09       1    **************************************************  2
2.09961E+09*      2    **************************************************  1
2.20000E+09*      1    **************************************************

AVERAGE GRAY LEVEL=2.100E+9
STANDARD DEVIATION=7.071E+7
NUMBER ELEMENTS=       4
MIN. DN=2.000E+9
MAX. DN=2.200E+9

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
HIST version 15-NOV-05

0.00000E+00       1    ****************
2.03922E+08*      1    ****************
2.95686E+08*      1    ****************
3.97647E+08*      1    ****************
4.99608E+08*      1    ****************
6.01569E+08*      2    *********************************
7.03529E+08*      1    ****************
7.95294E+08*      2    *********************************
8.97255E+08*      2    *********************************
9.99216E+08*      2    *********************************
1.10118E+09*      2    *********************************
1.20314E+09*      3    **************************************************  1
1.30510E+09*      2    *********************************
1.39686E+09*      3    **************************************************  2
1.49882E+09*      2    *********************************
1.60078E+09*      2    *********************************
1.70275E+09*      2    *********************************
1.80471E+09*      2    *********************************
1.89647E+09*      1    ****************
1.99843E+09*      2    *********************************
2.10039E+09*      1    ****************
2.20235E+09*      1    ****************
2.30431E+09*      1    ****************
2.39608E+09*      1    ****************
2.60000E+09*      1    ****************

AVERAGE GRAY LEVEL=1.300E+9
STANDARD DEVIATION=6.245E+8
NUMBER ELEMENTS=      40
MIN. DN=0.000000
MAX. DN=2.600E+9

HIST g1515 NLINES=10 LIMITS=(5.e+8, 24.E+8)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      10    ******************
5.00000E+08      18    *********************************
7.11111E+08      17    *******************************
9.22222E+08      21    **************************************
1.13333E+09      25    **********************************************
1.34444E+09      29    **************************************************  1
1.55556E+09      27    **************************************************  2
1.76667E+09      23    ******************************************
1.97778E+09      19    ***********************************
2.18889E+09      15    ***************************
2.40000E+09      15    ***************************
>HIGH LIMIT       6    ***********

AVERAGE GRAY LEVEL=1.400E+9
STANDARD DEVIATION=6.110E+8
NUMBER ELEMENTS=     225
MIN. DN=0.000000
MAX. DN=2.800E+9

HIST g1515 NLINES=10 LIMITS=(5.e+8, 23.E+8)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      10    ******************
5.00000E+08      11    ********************
7.00000E+08      15    ***************************
9.00000E+08      19    ***********************************
1.10000E+09      23    ******************************************
1.30000E+09      27    **************************************************  2
1.50000E+09      29    **************************************************  1
1.70000E+09      25    **********************************************
1.90000E+09      21    **************************************
2.10000E+09      17    *******************************
2.30000E+09      13    ************************
>HIGH LIMIT      15    ***************************

AVERAGE GRAY LEVEL=1.400E+9
STANDARD DEVIATION=6.110E+8
NUMBER ELEMENTS=     225
MIN. DN=0.000000
MAX. DN=2.800E+9

HIST g1515 'nocum          LIMITS=(5.e+8, 23.E+8) 'wide
Beginning VICAR task HIST
HIST version 15-NOV-05

FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ  PCTILE           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT      15   6.667  ****************************************************************************************************+1
5.00000E+08       6   2.667  ****************************************+         +         +         +         +         +         +

5.98824E+08       7   3.111  **********************************************    +         +         +         +         +         +

6.97647E+08       8   3.556  *****************************************************       +         +         +         +         +

8.03529E+08       9   4.000  ************************************************************+         +         +         +         +

9.02353E+08      10   4.444  ******************************************************************    +         +         +         +

1.00118E+09      11   4.889  *************************************************************************       +         +         +

1.10000E+09      12   5.333  ********************************************************************************+         +         +

1.19882E+09      13   5.778  **************************************************************************************    +         +

1.29765E+09      14   6.222  *********************************************************************************************       +

1.40353E+09      15   6.667  ****************************************************************************************************+2

1.50235E+09      14   6.222  *********************************************************************************************       +

1.60118E+09      13   5.778  **************************************************************************************    +         +

1.70000E+09      12   5.333  ********************************************************************************+         +         +

1.79882E+09      11   4.889  *************************************************************************       +         +         +

1.89765E+09      10   4.444  ******************************************************************    +         +         +         +

2.00353E+09       9   4.000  ************************************************************+         +         +         +         +

2.10235E+09       8   3.556  *****************************************************       +         +         +         +         +

2.20118E+09       7   3.111  **********************************************    +         +         +         +         +         +

2.30000E+09       6   2.667  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT      15   6.667  ****************************************************************************************************+

AVERAGE GRAY LEVEL=1.400E+9       STANDARD DEVIATION=6.110E+8       NUMBER ELEMENTS=     225
MIN. DN=0.000000      MAX. DN=2.800E+9

HIST g1515 SPIKE=5
Beginning VICAR task HIST
HIST version 15-NOV-05

0.00000E+00       1    ***
9.88235E+07*      2    *******
1.97647E+08*      3    ***********
2.96471E+08*      4    ***************
3.95294E+08*      5    *******************
5.05098E+08*      6    ***********************
6.03922E+08*      7    **************************
7.02745E+08*      8    ******************************
8.01569E+08*      9    **********************************
9.00392E+08*     10    **************************************
9.99216E+08*     11    ******************************************
1.09804E+09*     12    **********************************************
1.19686E+09*     13    **************************************************  4
1.29569E+09*     14    **************************************************  2
1.40549E+09*     15    **************************************************  1
1.50431E+09*     14    **************************************************  3
1.60314E+09*     13    **************************************************  5
1.70196E+09*     12    **********************************************
1.80078E+09*     11    ******************************************
1.89961E+09*     10    **************************************
1.99843E+09*      9    **********************************
2.09725E+09*      8    ******************************
2.19608E+09*      7    **************************
2.29490E+09*      6    ***********************
2.40471E+09*      5    *******************
2.50353E+09*      4    ***************
2.60235E+09*      3    ***********
2.70118E+09*      2    *******
2.80000E+09*      1    ***

AVERAGE GRAY LEVEL=1.400E+9
STANDARD DEVIATION=6.110E+8
NUMBER ELEMENTS=     225
MIN. DN=0.000000
MAX. DN=2.800E+9

HIST g1515 SPIKE=1
Beginning VICAR task HIST
HIST version 15-NOV-05

0.00000E+00       1    ***
9.88235E+07*      2    ******
1.97647E+08*      3    **********
2.96471E+08*      4    *************
3.95294E+08*      5    ****************
5.05098E+08*      6    ********************
6.03922E+08*      7    ***********************
7.02745E+08*      8    **************************
8.01569E+08*      9    ******************************
9.00392E+08*     10    *********************************
9.99216E+08*     11    ************************************
1.09804E+09*     12    ****************************************
1.19686E+09*     13    *******************************************
1.29569E+09*     14    **********************************************
1.40549E+09*     15    **************************************************  1
1.50431E+09*     14    **********************************************
1.60314E+09*     13    *******************************************
1.70196E+09*     12    ****************************************
1.80078E+09*     11    ************************************
1.89961E+09*     10    *********************************
1.99843E+09*      9    ******************************
2.09725E+09*      8    **************************
2.19608E+09*      7    ***********************
2.29490E+09*      6    ********************
2.40471E+09*      5    ****************
2.50353E+09*      4    *************
2.60235E+09*      3    **********
2.70118E+09*      2    ******
2.80000E+09*      1    ***

AVERAGE GRAY LEVEL=1.400E+9
STANDARD DEVIATION=6.110E+8
NUMBER ELEMENTS=     225
MIN. DN=0.000000
MAX. DN=2.800E+9

hist g1515 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=1.400E+9       STANDARD DEVIATION=6.110E+8       NUMBER ELEMENTS=     225
MIN. DN=0.000000
MAX. DN=2.800E+9

hist g1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=1.400E+9       STANDARD DEVIATION=6.110E+8       NUMBER ELEMENTS=     225
MIN. DN=0.000000
MAX. DN=2.800E+9

putmsg "Print average and stdev variables" ""
Print average and stdev variables
disp AVG

AVG=1400000000.0

disp SD

SD=611010112.0

gen g1515 15 15 linc=-123456789.e4 sinc=-1234567890.e4 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
write " Try some wild numbers.  3000 bins.  225 are non-empty."
 Try some wild numbers.  3000 bins.  225 are non-empty.
write " Should skip all empty bins and put a * after DN to indicate skip."
 Should skip all empty bins and put a * after DN to indicate skip.
hist g1515 nlines=3000
Beginning VICAR task hist
HIST version 15-NOV-05

-1.9012E+14       1    *************************
-1.8892E+14*      1    *************************
-1.8765E+14*      1    *************************
-1.8645E+14*      1    *************************
-1.8518E+14*      1    *************************
-1.8397E+14*      1    *************************
-1.8271E+14*      1    *************************
-1.8150E+14*      1    *************************
-1.8023E+14*      1    *************************
-1.7903E+14*      1    *************************
-1.7776E+14*      2    **************************************************  1
-1.7656E+14*      2    **************************************************  2
-1.7529E+14*      2    **************************************************
-1.7408E+14*      2    **************************************************
-1.7282E+14*      2    **************************************************
-1.7161E+14*      1    *************************
-1.7034E+14*      1    *************************
-1.6914E+14*      1    *************************
-1.6787E+14*      1    *************************
-1.6667E+14*      1    *************************
-1.6546E+14*      2    **************************************************
-1.6419E+14*      2    **************************************************
-1.6299E+14*      2    **************************************************
-1.6172E+14*      2    **************************************************
-1.6052E+14*      2    **************************************************
-1.5925E+14*      1    *************************
-1.5805E+14*      1    *************************
-1.5678E+14*      1    *************************
-1.5557E+14*      1    *************************
-1.5430E+14*      1    *************************
-1.5310E+14*      2    **************************************************
-1.5183E+14*      2    **************************************************
-1.5063E+14*      2    **************************************************
-1.4936E+14*      2    **************************************************
-1.4816E+14*      2    **************************************************
-1.4689E+14*      1    *************************
-1.4568E+14*      1    *************************
-1.4442E+14*      1    *************************
-1.4321E+14*      1    *************************
-1.4201E+14*      1    *************************
-1.4074E+14*      2    **************************************************
-1.3953E+14*      2    **************************************************
-1.3827E+14*      2    **************************************************
-1.3706E+14*      2    **************************************************
-1.3579E+14*      2    **************************************************
-1.3459E+14*      1    *************************
-1.3332E+14*      1    *************************
-1.3212E+14*      1    *************************
-1.3085E+14*      1    *************************
-1.2964E+14*      1    *************************
-1.2838E+14*      2    **************************************************
-1.2717E+14*      2    **************************************************
-1.2590E+14*      2    **************************************************
-1.2470E+14*      2    **************************************************
-1.2343E+14*      2    **************************************************
-1.2223E+14*      1    *************************
-1.2096E+14*      1    *************************
-1.1975E+14*      1    *************************
-1.1855E+14*      1    *************************
-1.1728E+14*      1    *************************
-1.1608E+14*      2    **************************************************
-1.1481E+14*      2    **************************************************
-1.1360E+14*      2    **************************************************
-1.1234E+14*      2    **************************************************
-1.1113E+14*      2    **************************************************
-1.0986E+14*      1    *************************
-1.0866E+14*      1    *************************
-1.0739E+14*      1    *************************
-1.0619E+14*      1    *************************
-1.0492E+14*      1    *************************
-1.0372E+14*      2    **************************************************
-1.0245E+14*      2    **************************************************
-1.0124E+14*      2    **************************************************
-9.9975E+13*      2    **************************************************
-9.8770E+13*      2    **************************************************
-9.7502E+13*      1    *************************
-9.6298E+13*      1    *************************
-9.5093E+13*      1    *************************
-9.3826E+13*      1    *************************
-9.2621E+13*      1    *************************
-9.1353E+13*      2    **************************************************
-9.0149E+13*      2    **************************************************
-8.8881E+13*      2    **************************************************
-8.7676E+13*      2    **************************************************
-8.6408E+13*      2    **************************************************
-8.5204E+13*      1    *************************
-8.3936E+13*      1    *************************
-8.2731E+13*      1    *************************
-8.1463E+13*      1    *************************
-8.0259E+13*      1    *************************
-7.8991E+13*      2    **************************************************
-7.7786E+13*      2    **************************************************
-7.6519E+13*      2    **************************************************
-7.5314E+13*      2    **************************************************
-7.4046E+13*      2    **************************************************
-7.2842E+13*      1    *************************
-7.1574E+13*      1    *************************
-7.0369E+13*      1    *************************
-6.9165E+13*      1    *************************
-6.7897E+13*      1    *************************
-6.6692E+13*      2    **************************************************
-6.5424E+13*      2    **************************************************
-6.4220E+13*      2    **************************************************
-6.2952E+13*      2    **************************************************
-6.1747E+13*      2    **************************************************
-6.0479E+13*      1    *************************
-5.9275E+13*      1    *************************
-5.8007E+13*      1    *************************
-5.6802E+13*      1    *************************
-5.5535E+13*      1    *************************
-5.4330E+13*      2    **************************************************
-5.3062E+13*      2    **************************************************
-5.1858E+13*      2    **************************************************
-5.0590E+13*      2    **************************************************
-4.9385E+13*      2    **************************************************
-4.8117E+13*      1    *************************
-4.6913E+13*      1    *************************
-4.5708E+13*      1    *************************
-4.4440E+13*      1    *************************
-4.3236E+13*      1    *************************
-4.1968E+13*      2    **************************************************
-4.0763E+13*      2    **************************************************
-3.9495E+13*      2    **************************************************
-3.8291E+13*      2    **************************************************
-3.7023E+13*      2    **************************************************
-3.5819E+13*      1    *************************
-3.4551E+13*      1    *************************
-3.3346E+13*      1    *************************
-3.2078E+13*      1    *************************
-3.0874E+13*      1    *************************
-2.9606E+13*      2    **************************************************
-2.8401E+13*      2    **************************************************
-2.7133E+13*      2    **************************************************
-2.5929E+13*      2    **************************************************
-2.4661E+13*      2    **************************************************
-2.3456E+13*      1    *************************
-2.2252E+13*      1    *************************
-2.0984E+13*      1    *************************
-1.9779E+13*      1    *************************
-1.8512E+13*      1    *************************
-1.7307E+13*      2    **************************************************
-1.6039E+13*      2    **************************************************
-1.4835E+13*      2    **************************************************
-1.3567E+13*      2    **************************************************
-1.2362E+13*      2    **************************************************
-1.1094E+13*      1    *************************
-9.8897E+12*      1    *************************
-8.6218E+12*      1    *************************
-7.4173E+12*      1    *************************
-6.1494E+12*      1    *************************
-4.9449E+12*      1    *************************
-3.6769E+12*      1    *************************
-2.4724E+12*      1    *************************
-1.2045E+12*      1    *************************
5.80813E+06*      1    *************************

AVERAGE GRAY LEVEL=-9.5E+13
STANDARD DEVIATION=5.36E+13
NUMBER ELEMENTS=     225
MIN. DN=-1.9E+14
MAX. DN=0.000000

gen g1515 15 15 linc=-1.e-9 sinc=-1.e-9 'real8
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515 area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

-4.0000E-09       1    *************************
-2.9961E-09*      2    **************************************************  2
-1.9922E-09*      3    **************************************************  1
-1.0039E-09*      2    **************************************************
-1.5439E-16*      1    *************************

AVERAGE GRAY LEVEL=-2.00E-9
STANDARD DEVIATION=1.155E-9
NUMBER ELEMENTS=       9
MIN. DN=-4.00E-9
MAX. DN=0.000000


-2.0000E-08       1    **************************************************  2
-1.8996E-08*      2    **************************************************  1
-1.8000E-08*      1    **************************************************

AVERAGE GRAY LEVEL=-1.90E-8
STANDARD DEVIATION=7.07E-10
NUMBER ELEMENTS=       4
MIN. DN=-2.00E-8
MAX. DN=-1.80E-8

hist g1515 size= (2,2,14,14) area=(1,1,3,3,10,10,2,2)
Beginning VICAR task hist
HIST version 15-NOV-05

-6.0000E-09       1    *************************
-4.9961E-09*      2    **************************************************  2
-3.9922E-09*      3    **************************************************  1
-3.0039E-09*      2    **************************************************
-2.0000E-09*      1    *************************

AVERAGE GRAY LEVEL=-4.00E-9
STANDARD DEVIATION=1.155E-9
NUMBER ELEMENTS=       9
MIN. DN=-6.00E-9
MAX. DN=-2.00E-9


-2.2000E-08       1    **************************************************  2
-2.0996E-08*      2    **************************************************  1
-2.0000E-08*      1    **************************************************

AVERAGE GRAY LEVEL=-2.10E-8
STANDARD DEVIATION=7.07E-10
NUMBER ELEMENTS=       4
MIN. DN=-2.20E-8
MAX. DN=-2.00E-8

hist g1515 linc=2 sinc=3
Beginning VICAR task hist
HIST version 15-NOV-05

-2.6000E-08       1    ****************
-2.3961E-08*      1    ****************
-2.3043E-08*      1    ****************
-2.2024E-08*      1    ****************
-2.1004E-08*      1    ****************
-1.9984E-08*      2    *********************************
-1.8965E-08*      1    ****************
-1.8047E-08*      2    *********************************
-1.7027E-08*      2    *********************************
-1.6008E-08*      2    *********************************
-1.4988E-08*      2    *********************************
-1.3969E-08*      3    **************************************************  1
-1.3051E-08*      2    *********************************
-1.2031E-08*      3    **************************************************  2
-1.1012E-08*      2    *********************************
-9.9922E-09*      2    *********************************
-8.9725E-09*      2    *********************************
-7.9529E-09*      2    *********************************
-7.0353E-09*      1    ****************
-6.0157E-09*      2    *********************************
-4.9961E-09*      1    ****************
-3.9765E-09*      1    ****************
-2.9569E-09*      1    ****************
-2.0392E-09*      1    ****************
3.19189E-16*      1    ****************

AVERAGE GRAY LEVEL=-1.30E-8
STANDARD DEVIATION=6.245E-9
NUMBER ELEMENTS=      40
MIN. DN=-2.60E-8
MAX. DN=0.000000

HIST g1515 NLINES=10 LIMITS= (-24.e-9, -5.E-9)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT       6    ***********
***********      15    ***************************
***********      15    ***************************
***********      19    ***********************************
***********      23    ******************************************
***********      27    **************************************************  2
***********      29    **************************************************  1
***********      25    **********************************************
***********      21    **************************************
***********      17    *******************************
***********      18    *********************************
>HIGH LIMIT      10    ******************

AVERAGE GRAY LEVEL=-1.40E-8
STANDARD DEVIATION=6.110E-9
NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8
MAX. DN=0.000000

HIST g1515 NLINES=10 LIMITS=(-23.e-9, -5.E-9)
Beginning VICAR task HIST
HIST version 15-NOV-05

< LOW LIMIT      15    ***************************
***********       6    ***********
***********      15    ***************************
***********      19    ***********************************
***********      23    ******************************************
***********      27    **************************************************  2
***********      29    **************************************************  1
***********      25    **********************************************
***********      21    **************************************
***********      17    *******************************
***********      13    ************************
>HIGH LIMIT      15    ***************************

AVERAGE GRAY LEVEL=-1.40E-8
STANDARD DEVIATION=6.110E-9
NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8
MAX. DN=0.000000

HIST g1515 'nocum          LIMITS=(-23.e-9, -5.E-9) 'wide
Beginning VICAR task HIST
HIST version 15-NOV-05

FREQUENCY DISTRIBUTION     SL=    1     SS=    1     NL=   15     NS=   15     LINC=  1    SINC=  1

       GRAY    FREQ  PCTILE           10        20        30        40        50        60        70        80        90       100
                             +         +         +         +         +         +         +         +         +         +         +
< LOW LIMIT      15   6.667  ****************************************************************************************************+1
-2.3000E-08       6   2.667  ****************************************+         +         +         +         +         +         +

-2.2012E-08       7   3.111  **********************************************    +         +         +         +         +         +

-2.1024E-08       8   3.556  *****************************************************       +         +         +         +         +

-1.9965E-08       9   4.000  ************************************************************+         +         +         +         +

-1.8976E-08      10   4.444  ******************************************************************    +         +         +         +

-1.7988E-08      11   4.889  *************************************************************************       +         +         +

-1.7000E-08      12   5.333  ********************************************************************************+         +         +

-1.6012E-08      13   5.778  **************************************************************************************    +         +

-1.5024E-08      14   6.222  *********************************************************************************************       +

-1.4035E-08      15   6.667  ****************************************************************************************************+2

-1.2976E-08      14   6.222  *********************************************************************************************       +

-1.1988E-08      13   5.778  **************************************************************************************    +         +

-1.1000E-08      12   5.333  ********************************************************************************+         +         +

-1.0012E-08      11   4.889  *************************************************************************       +         +         +

-9.0235E-09      10   4.444  ******************************************************************    +         +         +         +

-7.9647E-09       9   4.000  ************************************************************+         +         +         +         +

-6.9765E-09       8   3.556  *****************************************************       +         +         +         +         +

-5.9882E-09       7   3.111  **********************************************    +         +         +         +         +         +

-5.0000E-09       6   2.667  ****************************************+         +         +         +         +         +         +
>HIGH LIMIT      15   6.667  ****************************************************************************************************+

AVERAGE GRAY LEVEL=-1.40E-8       STANDARD DEVIATION=6.110E-9       NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8      MAX. DN=0.000000

HIST g1515 SPIKE=5
Beginning VICAR task HIST
HIST version 15-NOV-05

-2.8000E-08       1    ***
-2.7012E-08*      2    *******
-2.6024E-08*      3    ***********
-2.5035E-08*      4    ***************
-2.4047E-08*      5    *******************
-2.2949E-08*      6    ***********************
-2.1961E-08*      7    **************************
-2.0973E-08*      8    ******************************
-1.9984E-08*      9    **********************************
-1.8996E-08*     10    **************************************
-1.8008E-08*     11    ******************************************
-1.7020E-08*     12    **********************************************
-1.6031E-08*     13    **************************************************  4
-1.5043E-08*     14    **************************************************  2
-1.3945E-08*     15    **************************************************  1
-1.2957E-08*     14    **************************************************  3
-1.1969E-08*     13    **************************************************  5
-1.0980E-08*     12    **********************************************
-9.9922E-09*     11    ******************************************
-9.0039E-09*     10    **************************************
-8.0157E-09*      9    **********************************
-7.0275E-09*      8    ******************************
-6.0392E-09*      7    **************************
-5.0510E-09*      6    ***********************
-3.9529E-09*      5    *******************
-2.9647E-09*      4    ***************
-1.9765E-09*      3    ***********
-9.8824E-10*      2    *******
-2.0123E-16*      1    ***

AVERAGE GRAY LEVEL=-1.40E-8
STANDARD DEVIATION=6.110E-9
NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8
MAX. DN=0.000000

HIST g1515 SPIKE=1
Beginning VICAR task HIST
HIST version 15-NOV-05

-2.8000E-08       1    ***
-2.7012E-08*      2    ******
-2.6024E-08*      3    **********
-2.5035E-08*      4    *************
-2.4047E-08*      5    ****************
-2.2949E-08*      6    ********************
-2.1961E-08*      7    ***********************
-2.0973E-08*      8    **************************
-1.9984E-08*      9    ******************************
-1.8996E-08*     10    *********************************
-1.8008E-08*     11    ************************************
-1.7020E-08*     12    ****************************************
-1.6031E-08*     13    *******************************************
-1.5043E-08*     14    **********************************************
-1.3945E-08*     15    **************************************************  1
-1.2957E-08*     14    **********************************************
-1.1969E-08*     13    *******************************************
-1.0980E-08*     12    ****************************************
-9.9922E-09*     11    ************************************
-9.0039E-09*     10    *********************************
-8.0157E-09*      9    ******************************
-7.0275E-09*      8    **************************
-6.0392E-09*      7    ***********************
-5.0510E-09*      6    ********************
-3.9529E-09*      5    ****************
-2.9647E-09*      4    *************
-1.9765E-09*      3    **********
-9.8824E-10*      2    ******
-2.0123E-16*      1    ***

AVERAGE GRAY LEVEL=-1.40E-8
STANDARD DEVIATION=6.110E-9
NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8
MAX. DN=0.000000

hist g1515 'nohist
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=-1.40E-8       STANDARD DEVIATION=6.110E-9       NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8
MAX. DN=0.000000

hist g1515 'nohist SIGMA=SD  MEAN=AVG
Beginning VICAR task hist
HIST version 15-NOV-05


AVERAGE GRAY LEVEL=-1.40E-8       STANDARD DEVIATION=6.110E-9       NUMBER ELEMENTS=     225
MIN. DN=-2.80E-8
MAX. DN=0.000000

putmsg "Print average and stdev variables" ""
Print average and stdev variables
disp AVG

AVG=-1.40000002702e-08

disp SD

SD=6.11010086971e-09

gen g1515 NS=10 NL=10 NB=10
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist g1515
Beginning VICAR task hist
HIST version 15-NOV-05

          0       1
          1       3    **
          2       6    ****
          3      10    ******
          4      15    **********
          5      21    **************
          6      28    ******************
          7      36    ************************
          8      45    ******************************
          9      55    ************************************
         10      63    ******************************************
         11      69    **********************************************
         12      73    ************************************************
         13      75    **************************************************  1
         14      75    **************************************************  2
         15      73    ************************************************
         16      69    **********************************************
         17      63    ******************************************
         18      55    ************************************
         19      45    ******************************
         20      36    ************************
         21      28    ******************
         22      21    **************
         23      15    **********
         24      10    ******
         25       6    ****
         26       3    **
         27       1

AVERAGE GRAY LEVEL=13.50000
STANDARD DEVIATION=4.974937
NUMBER ELEMENTS=    1000
MIN. DN=         0
MAX. DN=        27

hist g1515 NB=4
Beginning VICAR task hist
HIST version 15-NOV-05

          0       1    *
          1       3    ****
          2       6    ********
          3      10    *************
          4      14    *******************
          5      18    *************************
          6      22    ******************************
          7      26    ************************************
          8      30    *****************************************
          9      34    ***********************************************
         10      36    **************************************************  1
         11      36    **************************************************  2
         12      34    ***********************************************
         13      30    *****************************************
         14      26    ************************************
         15      22    ******************************
         16      18    *************************
         17      14    *******************
         18      10    *************
         19       6    ********
         20       3    ****
         21       1    *

AVERAGE GRAY LEVEL=10.50000
STANDARD DEVIATION=4.213075
NUMBER ELEMENTS=     400
MIN. DN=         0
MAX. DN=        21

gen g1515 337 364 ival=200 linc=0 sinc=0
Beginning VICAR task gen
GEN Version 6
GEN task completed
hist a
Beginning VICAR task hist
HIST version 15-NOV-05

        200* 122668    **************************************************  1

AVERAGE GRAY LEVEL=200.0000
STANDARD DEVIATION=0.000000
NUMBER ELEMENTS=  122668
MIN. DN=       200
MAX. DN=       200

end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
