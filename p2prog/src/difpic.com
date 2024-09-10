$!****************************************************************************
$!
$! Build proc for MIPL module difpic
$! VPACK Version 1.9, Tuesday, June 29, 2004, 14:48:11
$!
$! Execute by entering:		$ @difpic
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
$ write sys$output "*** module difpic ***"
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
$ write sys$output "Invalid argument given to difpic.com file -- ", primary
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
$   if F$SEARCH("difpic.imake") .nes. ""
$   then
$      vimake difpic
$      purge difpic.bld
$   else
$      if F$SEARCH("difpic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake difpic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @difpic.bld "STD"
$   else
$      @difpic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create difpic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack difpic.com -mixed -
	-s difpic.f -
	-p difpic.pdf -
	-i difpic.imake -
	-t tstdifpic.pdf tstdifpic.log_solos tstdifpic.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create difpic.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C    REVISION HISTORY
C       1-85  SP   EXPANDED BUFFER SIZES TO 60000 AND REMOVED MESSAGE ABOUT
C                  PROCESSING FIRST 10000 BYTES IF IMAGE LINE TOO BIG. NOW
C                  USES 2 BUFFERS INSTEAD OF 3.
C       1-85  SP   MOVED COUNTING OF DIFFERENCES BEFORE COMPARING AGAINST
C                  MINDN BECAUSE MINDN IS 0 FOR BYTE DATA.
C       1-85  SP   ADDED MOD PARAMETER FOR BYTE DATA.
C       1-85  SP   ADDED WCHECK CALL AFTER WRITES.
C       1-85  SP   ADDED CODE TO AVOID INTEGER OVERFLOW ON EXTREME HALFWORD
C                  VALUES.
C       1-85  SP   CONVERTED TO VICAR2 SUBROUTINE CALLS.  ( U_FORMAT and 
C                  optional parameters in XVREAD and XVWRIT avoided because
C                  of apparent speed problems.)
C       1-85  SP   CHANGED MESSAGE 'NUMBER OF NONZERO PIXELS' TO 'NUMBER OF
C                  DIFFERENT PIXELS'.
C       1-85  SP   CHANGED TO IGNORE FORMAT PARAMETER BECAUSE VICAR2 USES
C                  ONLY THE FORMAT IN LABEL.
C       1-85  SP   MADE OUTPUT FILE OPTIONAL TO ALLOW GREATER SPEED.
C      12-91  SP   REPLACED PRNT CALLS WITH CALLS TO PRNINT AND PRNTREAL
C                  FOR SIMPLICITY.
C      12-91  SP   PORTED TO RUN ON BOTH UNIX AND VMS.
C       9-92  SP   Made buffer size 200000 bytes. Modified to handle 
C                  all data formats.  CHANGED AVE VALS TO DISPLAY AS FLOAT.
C                  CORRECTED "AVE DN OF PIX" TO "AVE VAL OF DIFFS"
C       3-93  SP   Modified to not use -2147483648 to work around Sun compiler.
C                  Added ability to handle 3d files if SIZE field defaulted
C                  and no output file specified.
C       7-94  SP   Allowed format="WORD" as alternative to HALF.
C       8-03  lwk  removed restrictions on NB, added SB parameter;  use of
C		   optionals in XVREAD/WRIT is no longer a speed issue.
C      12-03  lwk  added checks on size/format of input files
C       6-04  lwk  allow for deviant Format types WORD & COMPLEX; removed 
c		mabend at BIP check because SIT objected to it(!)  (I don't
c		agree, and have retained all the other mabend calls, but
c		there's no need to make an issue of it)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      integer   MAXBYTESPAR
      parameter (MAXBYTESPAR=200000)
      BYTE      BUF1(MAXBYTESPAR),BUF2(MAXBYTESPAR)

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2,NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2,NO,OUTFILE,SL,SS,EL,ES,SB,EB

      CHARACTER*12 FORM1, FORM2
      CHARACTER*8 ORG1, ORG2

C============================================================================
C
C  OPEN DATA SETS

      call xvmessage('DIFPIC version 29jun04', ' ')

      CALL XVUNIT( INFILE1, 'INP', 1, IND, ' ' )
      CALL XVOPEN( INFILE1,IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     . 'IO_ACT', 'SA', ' ')
      CALL XVGET( INFILE1, IND, 'FORMAT', FORM1, 'PIX_SIZE', IPIXSIZE,
     . 'ORG', ORG1, 'NL', NL1, 'NS', NS1, 'NB', NB1, ' ')
      IF (NS1*IPIXSIZE .GT. MAXBYTESPAR) call mabend(
     . 'Record too big for buffer, notify cognizant programmer.',
     . ' ')
      if (nb1.gt.1 .and. org1.eq.'BIP') then
	call xvmessage(
     .  ' BIP files not supported, use program TRAN to convert to BSQ',
     .  ' ')
	return
      endif
      if (form1.eq.'WORD') form1 = 'HALF'

      CALL XVUNIT( INFILE2, 'INP', 2, IND, ' ' )
      CALL XVOPEN( INFILE2, IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     . 'IO_ACT', 'SA', ' ')
      CALL XVGET( INFILE2, IND, 'FORMAT', FORM2, 'ORG', ORG2,
     . 'NL', NL2, 'NS', NS2, 'NB', NB2, ' ')
      if (nb2.gt.1 .and. org2.eq.'BIP') then
	call xvmessage(
     .  ' BIP files not supported, use program TRAN to convert to BSQ',
     .  ' ')
	return
      endif
      if (form2.eq.'WORD') form1 = 'HALF'

      ! just look at 4 bytes to make COMP same as COMPLEX
      if (form1(1:4).ne.form2(1:4)) call mabend(
	1'Files must have same formats')

      if (org1.eq.'BIL' .or. org2.eq.'BIL') call xvmessage(
     . ' Warning: BIL format may cause performance degradation',' ')

      CALL XVSIZE( SL, SS, NLO, NSO, NLI, NSI )   ! GET SIZE PARAMETER.
      CALL XVBANDS( SB, NBO, NBI)
      ! nbi is from 1st input label
      ! nbo is from param NB or BANDS, whichever is non-zero;  else zero
      if ((sb+nbo-1).gt.nbi) then
        call xvmessage(' NB too large, reduced to fit input',' ')
        nbo = nbi-sb+1
      endif
      if (nbo.le.0) nb = nbi-sb+1

      if ( (nl1.ne.nl2.and.sl.eq.1.and.nlo.eq.nli) .or.
     .     (ns1.ne.ns2.and.ss.eq.1.and.nso.eq.nsi) .or.
     .     (nb1.ne.nb2.and.sb.eq.1.and.nbo.eq.nbi) ) call mabend(
     .' Files have different dimensions, specify SIZE/BANDS parameter!')

      EL=SL+NLO-1
      ES=SS+NSO-1
      EB=SB+NBO-1

      CALL XVPCNT( 'OUT', NO )     ! NUMBER OF OUTPUT FILES.
      IF ( NO .NE. 0)  THEN
        CALL XVUNIT( OUTFILE, 'OUT', 1, IND, ' ' )
        CALL XVOPEN( OUTFILE, IND, 'OP', 'WRITE', 'OPEN_ACT', 'SA',
     .      'IO_ACT', 'SA', 'U_NL', NLO, 'U_NS', NSO, 'U_NB', NBO, ' ' )
      END IF

      IF (FORM1(1:4) .EQ. 'BYTE')  THEN
           CALL DIFPICB(BUF1, BUF2)
      ELSEIF (FORM1(1:4) .EQ. 'HALF' .OR. FORM1(1:4) .EQ. 'WORD') THEN
           CALL DIFPICH(BUF1, BUF2)
      ELSEIF (FORM1(1:4) .EQ. 'FULL') THEN
           CALL DIFPICF(BUF1, BUF2)
      ELSEIF (FORM1(1:4) .EQ. 'REAL') THEN
           CALL DIFPICR(BUF1, BUF2)
      ELSEIF (FORM1(1:4) .EQ. 'DOUB') THEN
           CALL DIFPICD(BUF1, BUF2)
      ELSEIF (FORM1(1:4) .EQ. 'COMP') THEN
           CALL DIFPICC(BUF1, BUF2)
      ELSE
          CALL XVMESSAGE('ERROR: INVALID DATA FORMAT FROM XVGET', ' ')
          CALL ABEND
      END IF

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICB(BUF1, BUF2)

      include 'fortport'

      BYTE        BUF1(*),BUF2(*)


C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     IPTOT, INTOT, NPIX, NPOS, NNEG, MAXDN, MINDN
      LOGICAL MODFLAG
      LOGICAL XVPTST

C==================================================================
      MODFLAG = XVPTST( 'MOD' )     ! MOD ONLY FOR BYTE DATA.

      IPTOT=0
      INTOT=0
      NPIX=0
      NPOS=0
      NNEG=0
      MAXDN=255
      MINDN=0
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
	1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
	1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
             K= BYTE2INT(BUF1(J)) - BYTE2INT(BUF2(J)) 
                           ! BYTE2INT CONVERTS UNSIGNED BYTE TO INTEGER.

             IF(K.NE.0)THEN
               IF(K.GT.0)THEN  !COUNT UP THE NUMBER OF POSITIVE 
                                                        !DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+K
               ENDIF
               IF(K.LT.0)THEN !COUNT UP THE NUMBER OF NEGATIVE 
                                                    !DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+K
               ENDIF	
             ENDIF

             IF (MODFLAG)    THEN
                 IF (K .LT. MINDN)  K = K + 256
             ELSE
               IF(K.GT.MAXDN) K=MAXDN
               IF(K.LT.MINDN) K=MINDN
             END IF

             BUF1(II) =INT2BYTE(K)               !LOW ORDER BYTE.
             II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
	1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF(NPOS.NE.0)
     .  CALL PRNTREAL(FLOAT(IPTOT)/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)
     .  CALL PRNTREAL(FLOAT(INTOT)/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL(FLOAT(IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICH(BUF1, BUF2)

      INTEGER*2     BUF1(*),BUF2(*)

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG, MAXDN, MINDN
      REAL        IPTOT, INTOT
C==================================================================

      IPTOT=0.
      INTOT=0.
      NPIX=0
      NPOS=0
      NNEG=0
      MAXDN=32767
      MINDN=-32768
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
	1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
	1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            K=BUF1(J)
            K = K - BUF2(J)       ! AVOID INTEGER OVERFLOW ON HALFWORD DATA.

            IF(K.NE.0)THEN
              IF(K.GT.0)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+K
              ENDIF
              IF(K.LT.0)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+K
              ENDIF	
            ENDIF

            IF(K.GT.MAXDN)THEN
              BUF1(II)=MAXDN
            ELSE IF(K.LT.MINDN)THEN
              BUF1(II)=MINDN
            ELSE
              BUF1(II)=K
            ENDIF
 
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
	1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF(NPOS.NE.0)
     .   CALL PRNTREAL(IPTOT/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)
     .   CALL PRNTREAL(INTOT/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL((IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICF(BUF1, BUF2)

      INTEGER*4     BUF1(*),BUF2(*)

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG, MAXDN, MINDN
      REAL        IPTOT, INTOT, RMAX, RMIN
C==================================================================

      IPTOT=0.
      INTOT=0.
      NPIX=0
      NPOS=0
      NNEG=0
      MAXDN=2147483647
      MINDN=-MAXDN-1
      RMAX = MAXDN
      RMIN = MINDN
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
	1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
	1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            K=BUF1(J)
            L=BUF2(J)
            IF (K .GE. 0 .AND. L .GE. 0) THEN   ! MOST COMMON CASE
              K = K - L
              R = FLOAT(K)
            ELSE 
              R = FLOAT(K) - FLOAT(L)    ! CHECK IF K-L IS OUT OF RANGE.
              IF ( R .LT. RMIN ) THEN
                K = MINDN
              ELSE IF ( R .GT. RMAX ) THEN
                K = MAXDN
              ELSE 
                K = K-L
              END IF
            END IF

            IF(K.NE.0)THEN
              IF(K.GT.0)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(K.LT.0)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF	
            ENDIF

            BUF1(II)=K
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
	1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF(NPOS.NE.0)
     .  CALL PRNTREAL(IPTOT/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)
     .  CALL PRNTREAL(INTOT/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL((IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICR(BUF1, BUF2)

      REAL*4     BUF1(*),BUF2(*)

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG
      REAL        IPTOT, INTOT
C==================================================================

      IPTOT=0.
      INTOT=0.
      NPIX=0
      NPOS=0
      NNEG=0
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
	1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
	1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            R=BUF1(J) - BUF2(J)

            IF(R.NE.0)THEN
              IF(R.GT.0.)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(R.LT.0.)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF	
            ENDIF
            BUF1(II) = R
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
	1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF(NPOS.NE.0)CALL PRNTREAL(IPTOT/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)CALL PRNTREAL(INTOT/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL((IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICD(BUF1, BUF2)

      REAL*8     BUF1(*),BUF2(*)

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG
      REAL*8      R, IPTOT, INTOT
C==================================================================

      IPTOT=0.
      INTOT=0.
      NPIX=0
      NPOS=0
      NNEG=0
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
	1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
	1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            R=BUF1(J) - BUF2(J)

            IF(R.NE.0)THEN
              IF(R.GT.0.)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(R.LT.0.)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF	
            ENDIF
            BUF1(II) = R
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
	1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF(NPOS.NE.0)
     .  CALL PRNTREAL(SNGL(IPTOT)/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)
     .  CALL PRNTREAL(SNGL(INTOT)/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL(SNGL(IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICC(BUF1, BUF2)

      COMPLEX*8   BUF1(*),BUF2(*)

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG
      COMPLEX*8   R, IPTOT, INTOT
C==================================================================

      IPTOT=(0., 0.)
      INTOT=(0., 0.)
      NPIX=0
      NPOS=0
      NNEG=0
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
	1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
	1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            R=BUF1(J) - BUF2(J)

            IF(BUF1(J) .NE. BUF2(J) )THEN
              IF(REAL(R).GE.0.)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(REAL(R).LT.0.)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF	
            ENDIF
            BUF1(II) = R
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
	1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF(NPOS.NE.0)CALL PRNT(10,1,IPTOT/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)CALL PRNT(10,1,INTOT/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNT(10,1,(IPTOT+INTOT)/(NB*NL*NS),' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################

      SUBROUTINE PRNTINT( IVAL, TITLE )
C
C     PURPOSE: PRNTINT prints the INTEGER value IVAL on the same line
C              and to the right of the description string TITLE.
C
C     REVISION HISTORY
C       12-91   SP  ORIGINAL VERSION PATTERNED AFTER PRNT FOR A SINGLE VALUE.
C
      INTEGER*4     IVAL
      CHARACTER*(*) TITLE
      CHARACTER*132 BUF
      INTEGER       L, N

C==============START OF EXECUTABLE CODE================================

      L = LEN( TITLE)
      L = MIN( 100, L)              ! NO SPACE FOR MORE THAN ABOUT 100 CHARS.
      BUF(1:L) = TITLE

C  IF 4 DIGITS ARE ENOUGH USE 4 DIGITS; ELSE USE 11.

      IF (-999 .LE. IVAL .AND. IVAL .LE. 9999)  THEN
         WRITE( BUF(L+1:L+4), 9040) IVAL
         N = L+4
      ELSE
         WRITE( BUF(L+1:L+11), 9110) IVAL
         N = L+11
      END IF

      CALL XVMESSAGE( BUF(1:N), ' ')
      RETURN

9040  FORMAT( I4 )
9110  FORMAT( I11)              ! MAXIMUM LENGTH FOR 32-BIT INTEGER
      END
      SUBROUTINE PRNTREAL( RVAL, TITLE )
C
C     PURPOSE: PRNTREAL prints the REAL value RVAL on the same line
C              and to the right of the description string TITLE.
C
C     REVISION HISTORY
C       12-91   SP  ORIGINAL VERSION PATTERNED AFTER PRNT FOR A SINGLE VALUE.
C
      REAL*4        RVAL
      CHARACTER*(*) TITLE
      CHARACTER*132 BUF
      INTEGER       L, N

C==============START OF EXECUTABLE CODE================================

      L = LEN( TITLE)
      L = MIN( 100, L)              ! NO SPACE FOR MORE THAN ABOUT 100 CHARS.
      BUF(1:L) = TITLE

      WRITE( BUF(L+1:L+13), 9130) RVAL
      N = L+13

      CALL XVMESSAGE( BUF(1:N), ' ')
      RETURN

9130  FORMAT( G13.6)              ! 6 SIGNIFICANT DIGITS IN FIXED POINT
                                  ! FOR MAGNITUDES FROM .1 TO 999999.
                                  ! OTHERWISE IN EXPONENTIAL FORMAT.
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create difpic.pdf
process help=*
PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING COUNT=(0:1)  DEFAULT=--
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM BANDS TYPE=INTEGER COUNT=2 DEFAULT=(1,0)
PARM SB TYPE=INTEGER DEFAULT=1
PARM NB TYPE=INTEGER DEFAULT=0
PARM MOD  TYPE=KEYWORD COUNT=0:1 VALID=MOD  DEFAULT=--
END-PROC
!# annot icon = difpic
.TITLE
DIFPIC
.HELP
 PURPOSE:

Program DIFPIC is used to find the difference between two images.  DIFPIC is
used primarily for picture comparison.  

 EXECUTION:

The input images may have any VICAR data format: BYTE, HALF, FULL, REAL, DOUB,
or COMP.  Both input images must have the same format.  The data format is taken
from the VICAR label of the first input file.  The optional output 
image has the same data format as the input images.  

Both input images must have the same dimensions, or, if they are of different 
sizes, the SIZE parameter must be specified.  If the images are three-dimensional,
they must be of BSQ or BIL file organization;  BIP files are not supported.
.PAGE
When an output file is produced, the output DN (data number) for a given line and 
sample is found by subtracting the DN from the second image from the DN from the 
first image. The resulting data numbers are then checked for being valid for the 
data type (byte, halfword, or fullword) of the image and are adjusted if invalid.  
For byte data, data numbers less than 0 are set to 0 (or are added to 256 if MOD 
is specified), and data numbers greater than 255 are set to 255. 
For halfword data, data numbers less than -32768 are set to -32768, and data numbers 
greater than 32767 are set to 32767. 
For fullword data, data numbers less than -2147483648 are set to -2147483648, and 
data numbers greater than 2147483647 are set to 2147483647. 
For formats REAL, DOUB, and COMP, no checking for floating point overflow or 
underflow is performed by the program.  Thus it is possible for DIFPIC to terminate 
with such a floating point error.  If this happens, the user can either run DIFPIC 
without on output file or divide both input images through by 1.0E5 and run DIFPIC 
on the results.
NOTE that program F2 can also be used to compare images, with somewhat greater
generality and robustness, but that program is not as simple to use and does not
give as much supplementary information as does DIFPIC.
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      difpic INP=(a1,a2) OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      difpic INP=(a1,a2) OUT=b  SL=sl SS=ss NL=nl NS=ns optional parameters
      difpic (a1,a2) b (sl,ss,nl,ns) optional parameters
      difpic (a1,a2) b optional parameters
      difpic INP=(a1,a2) SIZE=(sl,ss,nl,ns) optional parameters
      difpic INP=(a1,a2) SL=sl SS=ss NL=nl NS=ns optional parameters
      difpic (a1,a2) (sl,ss,nl,ns) optional parameters
      difpic (a1,a2) optional parameters

       Here 'a1' and 'a2' represent the input image file names, and
       'b' represents the output image file name.
.PAGE
EXAMPLES

1.    difpic INP=(A1,A2) OUT=B 'MOD 

      In this example the difference between the byte images A1 and A2 is
      written to the output file B.  Where the DN for image A1 is greater
      than the DN for image A2, the arithmetic difference is less than 0.
      The output DN is determined by taking the arithmetic difference modulo
      256.

2.    difpic INP=(A1,A2) 

      In this example the number of differences (pixels) between the 
      images A1 and A2 is printed and no output file is produced.
.PAGE
STATISTICS
Statistics on the number of differences are displayed by default.  Fuller 
statistics are produced if an output file is specified.  

Some of the statistics are average values for the differences found by DIFPIC.
These are computed in floating point and may involve numerous additions and 
thus may result in slightly different values on different computers.

The value displayed for AVE VAL OF DIFFS includes all pixels: those with 
positive, negative or zero differences.  This is a floating point value based 
on the real differences between the pixels and not on the value of the output 
pixel, which might be coerced to the numeric limits for the data type of the 
pixel.
.PAGE
 PRECISION: 
  When an output file is specified, DIFPIC computes the average difference
as well as the average positive and negative differences.  These are computed
in a straightforward way, adding up the differences and then dividing by the
number of differences.  (These computations are intended to be fast but not
necessarily highly precise.)  To accommodate the large differences that are
possible in the worst cases, the adding up takes place in single precision
floating point for all data formats except for BYTE and DOUBLE.  If there are a
large number of differences, this can result in a large number of floating
point operations and in some cases can result in a noticeable amount of
round-off error.  The test procedure contains such a case.  Thus the precision
of the average differences is not guaranteed.  In normal use the variation in 
these values that can be expected on different MIPS-supported machines should
not differ by more than 1 in six significant digits.  In contrast, the NUMBER
OF DIFFERENCES is a precise integer value.
.PAGE
RESTRICTIONS
1. The input files must both have the same data format. 
2. The maximum number of bytes per line is 200000.
3. BIP file organization is not supported.

 Ported to UNIX by:      Steve Pohorsky               4 Apr 1992

 COGNIZANT PROGRAMMER:   Lucas Kamp

 REVISIONS:
    12-91  SP   PORTED TO RUN ON BOTH UNIX AND VMS.
     9-92  SP   Made buffer size 200000 bytes. Modified to handle 
                all data formats.  CHANGED AVE VALS TO DISPLAY AS FLOAT.
                CORRECTED "AVE DN OF PIX" TO "AVE VAL OF DIFFS"
     3-93  SP   Modified to not use -2147483648 to work around Sun compiler.
                Added ability to handle 3D files if SIZE field defaulted
                and no output file specified.
     7-94  SP   Allowed format="WORD" as alternative to HALF.
     8-03  lwk  removed restrictions on NB, added SB parameter
    11-03  lwk  added checks on size/format of input files
.LEVEL1
.VARIABLE INP
Input file names
.VARIABLE OUT
Output file name (optional)
.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
You can enter SL,SS,NL,
and NS together as SIZE, OR
enter the SL,SS,NL, and NS
parameters separately.
By default, the entire input
image is used if these
parameters are not entered.
.VARIABLE SL
Starting line number
.VARIABLE SS
Starting sample number
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE BANDS
Standard Vicar Bands field:
  (SB,NB)
You can enter SB and NB together
as BANDS, OR enter the SB and NB
parameters separately.
By default, the entire input
image is used if these
.VARIABLE SB
Starting band number
.VARIABLE NB
Number of bands
.VARIABLE MOD
Specifies for byte images that
the difference will be taken
modulo 256.
.LEVEL2
.VARIABLE OUT
If no output file is specified, DIFPIC operates in a faster mode and only
prints the number of different pixels.
.VARIABLE NB
Number of bands: This may be used for multi-band (3D) images.  The default is
the number of bands in the input image label.
.VARIABLE MOD
MOD is an option for byte images that is useful when one wants to see in the 
output file the location of all pixels for which the input DNs are not the 
same.  When MOD is specified for byte images, all pixels with the same input
DNs will have an output DN of 0, and all pixels that do not have the same input
DNs will have an output DN that is not 0.  MOD has no effect except for BYTE 
images.  MOD is recommended for byte images when an output file is produced.

For byte data if the difference computed is less than 0, the output DN is set
to 0 if MOD is not specified, but is set to 256 plus the difference if MOD is
specified.  This amounts to taking the difference modulo 256.
.END
$ Return
$!#############################################################################
$Imake_File:
$ create difpic.imake
#define PROGRAM difpic

#define MODULE_LIST difpic.f

#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/*#define DEBUG		/* remove on delivery */
/*#define LIB_LOCAL	/* remove on delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tstdifpic.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
gen difpica 100 100 'byte
gen difpicb 100 100 ival=5 'byte
difpic (difpica,difpicb) difpicc 'mod
list difpicc (1,1,10,20)
difpic (difpicb,difpica) difpicc 
list difpicc (1,1,10,20)
difpic (difpicb,difpica) size=(2,2,9,9)
!
gen difpica 100 100 'half
gen difpicb 100 100 ival=5 'half
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,20)
difpic (difpicb,difpica) difpicc 
list difpicc (1,1,10,20)
copy difpica difpicd
difpic (difpica,difpicd) size=(2,2,9,9)
!
gen difpica 10 10 linc=2 sinc=2
gen difpicb 10 10 ival=6
difpic (difpica,difpicb) difpicc size=(2,3,8,6) 'mod
list difpicc
difpic (difpica,difpicb) size=(2,3,8,6) 
!
gen difpica 100 100 'full
gen difpicb 100 100 ival=5 'full
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5)
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
copy difpica difpicd
difpic (difpica,difpicd) 
!
gen difpica 100 100 'real4
gen difpicb 100 100 ival=5 'real4
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5)
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
copy difpica difpicd
difpic (difpica,difpicd) 
!
!
gen difpica 100 100 'real8
gen difpicb 100 100 ival=5 'real8
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5)
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
copy difpica difpicd
difpic (difpica,difpicd) 
!
!
gen difpica 100 100 'comp
gen difpicb 100 100 ival=5 'comp
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5)
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
copy difpica difpicd
difpic (difpica,difpicd) 
!
!check for handling of max and min DNs
!
gen difpica 100 100 ival=0 linc=0 sinc=0 'byte
gen difpicb 100 100 ival= 255 linc=0 sinc=0 'byte
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5) 'zero
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
!
gen difpica 100 100 ival=-32768 'half
gen difpicb 100 100 ival= 32500 'half
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5)
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
!
gen difpica 100 100 ival=-2147483648 'full
gen difpicb 100 100 ival= 2147483000 'full
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5)
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
!
write "Test difpic on multiband images"
gen a 10 20 30 ORG="BSQ"
gen b 10 20 30 ORG="BSQ" ival=1
write "Should get 6000 differences."
difpic (a b)
!
gen a 10 20 30 ORG="BIL" 'HALF
gen b 10 20 30 ORG="BIL" 'HALF ival=1
write "Should get 3000 differences."
difpic (a b) NB=15
!
gen a 10 20 30 ORG="BIP" 'REAL
gen b 10 20 30 ORG="BIP" 'REAL ival=1
write "Should get error message about BIP files"
difpic (a b) NB=15
!
write "Test difpic on multiband images with output file"
!
gen difpica 40 40 10 'byte
gen difpicb 40 40 10 ival=5 'byte
difpic (difpica,difpicb) difpicc 'mod
list difpicc (1,1,10,20) nb=2
!
gen difpica 40 40 10 'half
gen difpicb 40 40 10 ival=5 'half
difpic (difpicb,difpica) difpicc 
list difpicc (1,1,10,20) nb=2
!
gen difpica 40 40 10 'full
gen difpicb 40 40 10 ival=5 'full
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5) nb=2
!
gen difpica 40 40 10 'real4
gen difpicb 40 40 10 ival=5 'real4
difpic (difpica,difpicb) difpicc nb=4 sb=3
list difpicc (1,1,10,5) nb=2
!
gen difpica 40 40 10 'real8
gen difpicb 40 40 10 ival=5 'real8
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5) nb=2
!
gen difpica 40 40 10 'comp
gen difpicb 40 40 10 ival=5 'comp
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5) nb=2
!
! test that COMPLEX format treated properly:
gen difpica 40 40 'comp
gen difpicb 40 40 ival=5 'comp
ccomp difpica (cr ci) 'rect 'forw
ccomp (cr ci) difpicb 'rect 'inv   
label-l difpica
label-l difpicb
difpic (difpica,difpicb) difpicc
!
end-proc
$!-----------------------------------------------------------------------------
$ create tstdifpic.log_solos
tstdifpic
gen difpica 100 100 'byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival=5 'byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc 'mod
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,20)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:05 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:05 2004
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      2     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      3     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      4     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      5     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      6     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      7     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      8     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      9     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
     10     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
difpic (difpicb,difpica) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.00000
 NUMBER OF POS DIFF=      10000
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS=  5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,20)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:05 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:06 2004
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      2       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      3       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      4       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      5       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      6       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      7       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      8       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      9       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
     10       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
difpic (difpicb,difpica) size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =  81
gen difpica 100 100 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival=5 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,20)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:07 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:07 2004
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      2        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      3        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      4        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      5        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      6        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      7        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      8        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      9        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
     10        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:07 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:07 2004
     Samp      16    17    18    19    20
   Line
      1        -5    -5    -5    -5    -5
      2        -5    -5    -5    -5    -5
      3        -5    -5    -5    -5    -5
      4        -5    -5    -5    -5    -5
      5        -5    -5    -5    -5    -5
      6        -5    -5    -5    -5    -5
      7        -5    -5    -5    -5    -5
      8        -5    -5    -5    -5    -5
      9        -5    -5    -5    -5    -5
     10        -5    -5    -5    -5    -5
difpic (difpicb,difpica) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.00000
 NUMBER OF POS DIFF=      10000
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS=  5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,20)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:07 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:08 2004
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      2         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      3         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      4         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      5         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      6         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      7         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      8         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      9         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
     10         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:07 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:08 2004
     Samp      16    17    18    19    20
   Line
      1         5     5     5     5     5
      2         5     5     5     5     5
      3         5     5     5     5     5
      4         5     5     5     5     5
      5         5     5     5     5     5
      6         5     5     5     5     5
      7         5     5     5     5     5
      8         5     5     5     5     5
      9         5     5     5     5     5
     10         5     5     5     5     5
copy difpica difpicd
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
difpic (difpica,difpicd) size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
gen difpica 10 10 linc=2 sinc=2
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 10 10 ival=6
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc size=(2,3,8,6) 'mod
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  4.05263
 NUMBER OF POS DIFF=  38
 AVE VAL OF NEG DIFFS= -1.66667
 NUMBER OF NEG DIFFS=   6
 TOTAL NUMBER OF DIFFERENT PIXELS=  44
 AVE VAL OF DIFFS=  3.00000
 % DIFF PIXELS=  91.6667
list difpicc
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:09 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:09 2004
     Samp     1       3       5
   Line
      1     253 254 255   0   1   2
      2     254 255   0   1   2   3
      3     255   0   1   2   3   4
      4       0   1   2   3   4   5
      5       1   2   3   4   5   6
      6       2   3   4   5   6   7
      7       3   4   5   6   7   8
      8       4   5   6   7   8   9
difpic (difpica,difpicb) size=(2,3,8,6)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =  44
gen difpica 100 100 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival=5 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5)
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:10 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:10 2004
     Samp            1          2          3          4          5
   Line
      1             -5         -5         -5         -5         -5
      2             -5         -5         -5         -5         -5
      3             -5         -5         -5         -5         -5
      4             -5         -5         -5         -5         -5
      5             -5         -5         -5         -5         -5
      6             -5         -5         -5         -5         -5
      7             -5         -5         -5         -5         -5
      8             -5         -5         -5         -5         -5
      9             -5         -5         -5         -5         -5
     10             -5         -5         -5         -5         -5
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.00000
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS=  5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:10 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:11 2004
     Samp            1          2          3          4          5
   Line
      1              5          5          5          5          5
      2              5          5          5          5          5
      3              5          5          5          5          5
      4              5          5          5          5          5
      5              5          5          5          5          5
      6              5          5          5          5          5
      7              5          5          5          5          5
      8              5          5          5          5          5
      9              5          5          5          5          5
copy difpica difpicd
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
difpic (difpica,difpicd)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
gen difpica 100 100 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival=5 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5)
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:12 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:13 2004
     Samp             1           2           3           4           5
   Line
      1      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      2      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      3      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      4      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      5      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      6      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      7      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      8      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      9      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
     10      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.00000
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS=  5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:12 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:13 2004
     Samp             1           2           3           4           5
   Line
      1       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      2       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      3       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      4       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      5       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      6       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      7       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      8       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      9       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
copy difpica difpicd
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
difpic (difpica,difpicd)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
gen difpica 100 100 'real8
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival=5 'real8
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5)
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:14 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:15 2004
     Samp             1           2           3           4           5
   Line
      1      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      2      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      3      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      4      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      5      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      6      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      7      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      8      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      9      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
     10      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.00000
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS=  5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:15 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:15 2004
     Samp             1           2           3           4           5
   Line
      1       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      2       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      3       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      4       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      5       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      6       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      7       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      8       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      9       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
copy difpica difpicd
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
difpic (difpica,difpicd)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
gen difpica 100 100 'comp
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival=5 'comp
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.000E+00  0.000E+00
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -5.000E+00  0.000E+00
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5)
Beginning VICAR task list

   COMP     samples are interpreted as COMPLEX  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:16 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:17 2004
     Samp                         1                       2                       3                       4                       5
   Line
      1      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      2      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      3      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      4      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      5      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      6      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      7      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      8      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      9      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
     10      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.000E+00  0.000E+00
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS=  5.000E+00  0.000E+00
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   COMP     samples are interpreted as COMPLEX  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:17 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:18 2004
     Samp                         1                       2                       3                       4                       5
   Line
      1       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      2       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      3       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      4       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      5       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      6       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      7       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      8       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      9       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
copy difpica difpicd
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
difpic (difpica,difpicd)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
gen difpica 100 100 ival=0 linc=0 sinc=0 'byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival= 255 linc=0 sinc=0 'byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -255.000
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -255.000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5) 'zero
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:19 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:19 2004
     Samp     1       3       5
   Line
      1       0   0   0   0   0
      2       0   0   0   0   0
      3       0   0   0   0   0
      4       0   0   0   0   0
      5       0   0   0   0   0
      6       0   0   0   0   0
      7       0   0   0   0   0
      8       0   0   0   0   0
      9       0   0   0   0   0
     10       0   0   0   0   0
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  255.000
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS=  255.000
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:19 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:20 2004
     Samp     1       3       5
   Line
      1     255 255 255 255 255
      2     255 255 255 255 255
      3     255 255 255 255 255
      4     255 255 255 255 255
      5     255 255 255 255 255
      6     255 255 255 255 255
      7     255 255 255 255 255
      8     255 255 255 255 255
      9     255 255 255 255 255
gen difpica 100 100 ival=-32768 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival= 32500 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -65273.8
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -65273.8
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:20 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:21 2004
     Samp       1     2     3     4     5
   Line
      1    -32768-32768-32768-32768-32768
      2    -32768-32768-32768-32768-32768
      3    -32768-32768-32768-32768-32768
      4    -32768-32768-32768-32768-32768
      5    -32768-32768-32768-32768-32768
      6    -32768-32768-32768-32768-32768
      7    -32768-32768-32768-32768-32768
      8    -32768-32768-32768-32768-32768
      9    -32768-32768-32768-32768-32768
     10    -32768-32768-32768-32768-32768
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  65268.0
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS=  65268.0
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:20 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:21 2004
     Samp       1     2     3     4     5
   Line
      1     32767 32767 32767 32767 32767
      2     32767 32767 32767 32767 32767
      3     32767 32767 32767 32767 32767
      4     32767 32767 32767 32767 32767
      5     32767 32767 32767 32767 32767
      6     32767 32767 32767 32767 32767
      7     32767 32767 32767 32767 32767
      8     32767 32767 32767 32767 32767
      9     32767 32767 32767 32767 32767
gen difpica 100 100 ival=-2147483648 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival= 2147483000 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS=-0.429497E+10
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS=-0.429497E+10
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5)
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:22 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:22 2004
     Samp            1          2          3          4          5
   Line
      1    -2147483648-2147483648-2147483648-2147483648-2147483648
      2    -2147483648-2147483648-2147483648-2147483648-2147483648
      3    -2147483648-2147483648-2147483648-2147483648-2147483648
      4    -2147483648-2147483648-2147483648-2147483648-2147483648
      5    -2147483648-2147483648-2147483648-2147483648-2147483648
      6    -2147483648-2147483648-2147483648-2147483648-2147483648
      7    -2147483648-2147483648-2147483648-2147483648-2147483648
      8    -2147483648-2147483648-2147483648-2147483648-2147483648
      9    -2147483648-2147483648-2147483648-2147483648-2147483648
     10    -2147483648-2147483648-2147483648-2147483648-2147483648
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS= 0.429497E+10
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS= 0.429497E+10
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:22 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:23 2004
     Samp            1          2          3          4          5
   Line
      1     2147483647 2147483647 2147483647 2147483647 2147483647
      2     2147483647 2147483647 2147483647 2147483647 2147483647
      3     2147483647 2147483647 2147483647 2147483647 2147483647
      4     2147483647 2147483647 2147483647 2147483647 2147483647
      5     2147483647 2147483647 2147483647 2147483647 2147483647
      6     2147483647 2147483647 2147483647 2147483647 2147483647
      7     2147483647 2147483647 2147483647 2147483647 2147483647
      8     2147483647 2147483647 2147483647 2147483647 2147483647
      9     2147483647 2147483647 2147483647 2147483647 2147483647
write "Test difpic on multiband images"
Test difpic on multiband images
gen a 10 20 30 ORG="BSQ"
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen b 10 20 30 ORG="BSQ" ival=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
write "Should get 6000 differences."
Should get 6000 differences.
difpic (a b)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =6000
gen a 10 20 30 ORG="BIL" 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen b 10 20 30 ORG="BIL" 'HALF ival=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
write "Should get 3000 differences."
Should get 3000 differences.
difpic (a b) NB=15
Beginning VICAR task difpic
DIFPIC version 29jun04
 Warning: BIL format may cause performance degradation
 NUMBER OF DIFFERENCES =3000
gen a 10 20 30 ORG="BIP" 'REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen b 10 20 30 ORG="BIP" 'REAL ival=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
write "Should get error message about BIP files"
Should get error message about BIP files
difpic (a b) NB=15
Beginning VICAR task difpic
DIFPIC version 29jun04
 BIP files not supported, use program TRAN to convert to BSQ
write "Test difpic on multiband images with output file"
Test difpic on multiband images with output file
gen difpica 40 40 10 'byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 10 ival=5 'byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc 'mod
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      16000
 TOTAL NUMBER OF DIFFERENT PIXELS=      16000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,20) nb=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:25 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:26 2004
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      2     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      3     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      4     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      5     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      6     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      7     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      8     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      9     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
     10     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:25 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:26 2004
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      2     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      3     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      4     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      5     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      6     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      7     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      8     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      9     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
     10     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
gen difpica 40 40 10 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 10 ival=5 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpicb,difpica) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.00000
 NUMBER OF POS DIFF=      16000
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=      16000
 AVE VAL OF DIFFS=  5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,20) nb=2
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:27 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:27 2004
 ***********
 Band =     1
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      2         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      3         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      4         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      5         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      6         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      7         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      8         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      9         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
     10         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:27 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:27 2004
 ***********
 Band =     2
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      2         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      3         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      4         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      5         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      6         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      7         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      8         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      9         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
     10         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:27 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:27 2004
 ***********
 Band =     1
 ***********
     Samp      16    17    18    19    20
   Line
      1         5     5     5     5     5
      2         5     5     5     5     5
      3         5     5     5     5     5
      4         5     5     5     5     5
      5         5     5     5     5     5
      6         5     5     5     5     5
      7         5     5     5     5     5
      8         5     5     5     5     5
      9         5     5     5     5     5
     10         5     5     5     5     5


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:27 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:27 2004
 ***********
 Band =     2
 ***********
     Samp      16    17    18    19    20
   Line
      1         5     5     5     5     5
      2         5     5     5     5     5
      3         5     5     5     5     5
      4         5     5     5     5     5
      5         5     5     5     5     5
      6         5     5     5     5     5
      7         5     5     5     5     5
      8         5     5     5     5     5
      9         5     5     5     5     5
     10         5     5     5     5     5
gen difpica 40 40 10 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 10 ival=5 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      16000
 TOTAL NUMBER OF DIFFERENT PIXELS=      16000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5) nb=2
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:27 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:28 2004
 ***********
 Band =     1
 ***********
     Samp            1          2          3          4          5
   Line
      1             -5         -5         -5         -5         -5
      2             -5         -5         -5         -5         -5
      3             -5         -5         -5         -5         -5
      4             -5         -5         -5         -5         -5
      5             -5         -5         -5         -5         -5
      6             -5         -5         -5         -5         -5
      7             -5         -5         -5         -5         -5
      8             -5         -5         -5         -5         -5
      9             -5         -5         -5         -5         -5
     10             -5         -5         -5         -5         -5


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:27 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:28 2004
 ***********
 Band =     2
 ***********
     Samp            1          2          3          4          5
   Line
      1             -5         -5         -5         -5         -5
      2             -5         -5         -5         -5         -5
      3             -5         -5         -5         -5         -5
      4             -5         -5         -5         -5         -5
      5             -5         -5         -5         -5         -5
      6             -5         -5         -5         -5         -5
      7             -5         -5         -5         -5         -5
      8             -5         -5         -5         -5         -5
      9             -5         -5         -5         -5         -5
     10             -5         -5         -5         -5         -5
gen difpica 40 40 10 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 10 ival=5 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc nb=4 sb=3
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=6400
 TOTAL NUMBER OF DIFFERENT PIXELS=6400
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5) nb=2
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:28 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:29 2004
 ***********
 Band =     1
 ***********
     Samp             1           2           3           4           5
   Line
      1      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      2      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      3      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      4      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      5      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      6      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      7      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      8      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      9      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
     10      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:28 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:29 2004
 ***********
 Band =     2
 ***********
     Samp             1           2           3           4           5
   Line
      1      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      2      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      3      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      4      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      5      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      6      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      7      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      8      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      9      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
     10      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
gen difpica 40 40 10 'real8
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 10 ival=5 'real8
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      16000
 TOTAL NUMBER OF DIFFERENT PIXELS=      16000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5) nb=2
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:30 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:30 2004
 ***********
 Band =     1
 ***********
     Samp             1           2           3           4           5
   Line
      1      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      2      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      3      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      4      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      5      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      6      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      7      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      8      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      9      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
     10      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:30 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:30 2004
 ***********
 Band =     2
 ***********
     Samp             1           2           3           4           5
   Line
      1      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      2      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      3      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      4      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      5      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      6      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      7      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      8      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      9      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
     10      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
gen difpica 40 40 10 'comp
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 10 ival=5 'comp
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.000E+00  0.000E+00
 NUMBER OF NEG DIFFS=      16000
 TOTAL NUMBER OF DIFFERENT PIXELS=      16000
 AVE VAL OF DIFFS= -5.000E+00  0.000E+00
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5) nb=2
Beginning VICAR task list

   COMP     samples are interpreted as COMPLEX  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:31 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:31 2004
 ***********
 Band =     1
 ***********
     Samp                         1                       2                       3                       4                       5
   Line
      1      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      2      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      3      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      4      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      5      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      6      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      7      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      8      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      9      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
     10      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:43:31 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:43:31 2004
 ***********
 Band =     2
 ***********
     Samp                         1                       2                       3                       4                       5
   Line
      1      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      2      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      3      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      4      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      5      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      6      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      7      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      8      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      9      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
     10      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
gen difpica 40 40 'comp
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 ival=5 'comp
Beginning VICAR task gen
GEN Version 6
GEN task completed
ccomp difpica (cr ci) 'rect 'forw
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
ccomp (cr ci) difpicb 'rect 'inv
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
label-l difpica
Beginning VICAR task label
************************************************************
 
        ************  File difpica ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in COMP format from a SUN-SOLR host
                1 bands
                40 lines per band
                40 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Tue Jun 29 14:43:32 2004 ----
IVAL=(0.0, 0.0)
SINC=(1.0, 1.0)
LINC=(1.0, 1.0)
BINC=(1.0, 1.0)
MODULO=(0.0, 0.0)
 
************************************************************
label-l difpicb
Beginning VICAR task label
************************************************************
 
        ************  File difpicb ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in COMPLEX format from a SUN-SOLR host
                1 bands
                40 lines per band
                40 samples per line
                0 lines of binary header of type t
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Tue Jun 29 14:43:32 2004 ----
IVAL=(0.0, 0.0)
SINC=(1.0, 1.0)
LINC=(1.0, 1.0)
BINC=(1.0, 1.0)
MODULO=(0.0, 0.0)
---- Task: CCOMP -- User: lwk -- Tue Jun 29 14:43:32 2004 ----
---- Task: CCOMP -- User: lwk -- Tue Jun 29 14:43:32 2004 ----
 
************************************************************
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=   0
 AVE VAL OF DIFFS=  0.000E+00  0.000E+00
 % DIFF PIXELS=       0.
end-proc
disable-log
$!-----------------------------------------------------------------------------
$ create tstdifpic.log_linux
tstdifpic
gen difpica 100 100 'byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival=5 'byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc 'mod
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,20)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      2     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      3     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      4     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      5     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      6     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      7     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      8     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      9     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
     10     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
difpic (difpicb,difpica) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.00000
 NUMBER OF POS DIFF=      10000
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS=  5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,20)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      2       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      3       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      4       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      5       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      6       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      7       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      8       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
      9       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
     10       5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5   5
difpic (difpicb,difpica) size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =  81
gen difpica 100 100 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival=5 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,20)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      2        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      3        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      4        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      5        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      6        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      7        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      8        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
      9        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5
     10        -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5    -5

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
     Samp      16    17    18    19    20
   Line
      1        -5    -5    -5    -5    -5
      2        -5    -5    -5    -5    -5
      3        -5    -5    -5    -5    -5
      4        -5    -5    -5    -5    -5
      5        -5    -5    -5    -5    -5
      6        -5    -5    -5    -5    -5
      7        -5    -5    -5    -5    -5
      8        -5    -5    -5    -5    -5
      9        -5    -5    -5    -5    -5
     10        -5    -5    -5    -5    -5
difpic (difpicb,difpica) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.00000
 NUMBER OF POS DIFF=      10000
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS=  5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,20)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      2         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      3         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      4         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      5         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      6         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      7         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      8         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      9         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
     10         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
     Samp      16    17    18    19    20
   Line
      1         5     5     5     5     5
      2         5     5     5     5     5
      3         5     5     5     5     5
      4         5     5     5     5     5
      5         5     5     5     5     5
      6         5     5     5     5     5
      7         5     5     5     5     5
      8         5     5     5     5     5
      9         5     5     5     5     5
     10         5     5     5     5     5
copy difpica difpicd
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
difpic (difpica,difpicd) size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
gen difpica 10 10 linc=2 sinc=2
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 10 10 ival=6
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc size=(2,3,8,6) 'mod
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  4.05263
 NUMBER OF POS DIFF=  38
 AVE VAL OF NEG DIFFS= -1.66667
 NUMBER OF NEG DIFFS=   6
 TOTAL NUMBER OF DIFFERENT PIXELS=  44
 AVE VAL OF DIFFS=  3.00000
 % DIFF PIXELS=  91.6667
list difpicc
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
     Samp     1       3       5
   Line
      1     253 254 255   0   1   2
      2     254 255   0   1   2   3
      3     255   0   1   2   3   4
      4       0   1   2   3   4   5
      5       1   2   3   4   5   6
      6       2   3   4   5   6   7
      7       3   4   5   6   7   8
      8       4   5   6   7   8   9
difpic (difpica,difpicb) size=(2,3,8,6)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =  44
gen difpica 100 100 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival=5 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5)
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
     Samp            1          2          3          4          5
   Line
      1             -5         -5         -5         -5         -5
      2             -5         -5         -5         -5         -5
      3             -5         -5         -5         -5         -5
      4             -5         -5         -5         -5         -5
      5             -5         -5         -5         -5         -5
      6             -5         -5         -5         -5         -5
      7             -5         -5         -5         -5         -5
      8             -5         -5         -5         -5         -5
      9             -5         -5         -5         -5         -5
     10             -5         -5         -5         -5         -5
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.00000
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS=  5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
     Samp            1          2          3          4          5
   Line
      1              5          5          5          5          5
      2              5          5          5          5          5
      3              5          5          5          5          5
      4              5          5          5          5          5
      5              5          5          5          5          5
      6              5          5          5          5          5
      7              5          5          5          5          5
      8              5          5          5          5          5
      9              5          5          5          5          5
copy difpica difpicd
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
difpic (difpica,difpicd)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
gen difpica 100 100 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival=5 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5)
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
     Samp             1           2           3           4           5
   Line
      1      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      2      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      3      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      4      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      5      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      6      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      7      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      8      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      9      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
     10      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.00000
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS=  5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:45 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
     Samp             1           2           3           4           5
   Line
      1       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      2       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      3       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      4       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      5       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      6       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      7       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      8       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      9       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
copy difpica difpicd
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
difpic (difpica,difpicd)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
gen difpica 100 100 'real8
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival=5 'real8
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5)
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
     Samp             1           2           3           4           5
   Line
      1      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      2      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      3      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      4      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      5      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      6      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      7      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      8      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      9      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
     10      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.00000
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS=  5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
     Samp             1           2           3           4           5
   Line
      1       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      2       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      3       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      4       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      5       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      6       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      7       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      8       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
      9       5.000E+00   5.000E+00   5.000E+00   5.000E+00   5.000E+00
copy difpica difpicd
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
difpic (difpica,difpicd)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
gen difpica 100 100 'comp
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival=5 'comp
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.000E+00  0.000E+00
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -5.000E+00  0.000E+00
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5)
Beginning VICAR task list

   COMP     samples are interpreted as COMPLEX  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
     Samp                         1                       2                       3                       4                       5
   Line
      1      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      2      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      3      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      4      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      5      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      6      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      7      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      8      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      9      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
     10      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.000E+00  0.000E+00
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS=  5.000E+00  0.000E+00
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   COMP     samples are interpreted as COMPLEX  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
     Samp                         1                       2                       3                       4                       5
   Line
      1       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      2       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      3       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      4       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      5       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      6       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      7       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      8       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
      9       5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00   5.000E+00   0.000E+00
copy difpica difpicd
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
difpic (difpica,difpicd)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
gen difpica 100 100 ival=0 linc=0 sinc=0 'byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival= 255 linc=0 sinc=0 'byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -255.000
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -255.000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5) 'zero
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
     Samp     1       3       5
   Line
      1       0   0   0   0   0
      2       0   0   0   0   0
      3       0   0   0   0   0
      4       0   0   0   0   0
      5       0   0   0   0   0
      6       0   0   0   0   0
      7       0   0   0   0   0
      8       0   0   0   0   0
      9       0   0   0   0   0
     10       0   0   0   0   0
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  255.000
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS=  255.000
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
     Samp     1       3       5
   Line
      1     255 255 255 255 255
      2     255 255 255 255 255
      3     255 255 255 255 255
      4     255 255 255 255 255
      5     255 255 255 255 255
      6     255 255 255 255 255
      7     255 255 255 255 255
      8     255 255 255 255 255
      9     255 255 255 255 255
gen difpica 100 100 ival=-32768 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival= 32500 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -65273.8
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS= -65273.8
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
     Samp       1     2     3     4     5
   Line
      1    -32768-32768-32768-32768-32768
      2    -32768-32768-32768-32768-32768
      3    -32768-32768-32768-32768-32768
      4    -32768-32768-32768-32768-32768
      5    -32768-32768-32768-32768-32768
      6    -32768-32768-32768-32768-32768
      7    -32768-32768-32768-32768-32768
      8    -32768-32768-32768-32768-32768
      9    -32768-32768-32768-32768-32768
     10    -32768-32768-32768-32768-32768
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  65268.0
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS=  65268.0
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
     Samp       1     2     3     4     5
   Line
      1     32767 32767 32767 32767 32767
      2     32767 32767 32767 32767 32767
      3     32767 32767 32767 32767 32767
      4     32767 32767 32767 32767 32767
      5     32767 32767 32767 32767 32767
      6     32767 32767 32767 32767 32767
      7     32767 32767 32767 32767 32767
      8     32767 32767 32767 32767 32767
      9     32767 32767 32767 32767 32767
gen difpica 100 100 ival=-2147483648 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 100 100 ival= 2147483000 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS=-0.429497E+10
 NUMBER OF NEG DIFFS=      10000
 TOTAL NUMBER OF DIFFERENT PIXELS=      10000
 AVE VAL OF DIFFS=-0.429497E+10
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5)
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
     Samp            1          2          3          4          5
   Line
      1    -2147483648-2147483648-2147483648-2147483648-2147483648
      2    -2147483648-2147483648-2147483648-2147483648-2147483648
      3    -2147483648-2147483648-2147483648-2147483648-2147483648
      4    -2147483648-2147483648-2147483648-2147483648-2147483648
      5    -2147483648-2147483648-2147483648-2147483648-2147483648
      6    -2147483648-2147483648-2147483648-2147483648-2147483648
      7    -2147483648-2147483648-2147483648-2147483648-2147483648
      8    -2147483648-2147483648-2147483648-2147483648-2147483648
      9    -2147483648-2147483648-2147483648-2147483648-2147483648
     10    -2147483648-2147483648-2147483648-2147483648-2147483648
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS= 0.429497E+10
 NUMBER OF POS DIFF=  81
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=  81
 AVE VAL OF DIFFS= 0.429497E+10
 % DIFF PIXELS=  100.000
list difpicc (1,1,9,5)
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
     Samp            1          2          3          4          5
   Line
      1     2147483647 2147483647 2147483647 2147483647 2147483647
      2     2147483647 2147483647 2147483647 2147483647 2147483647
      3     2147483647 2147483647 2147483647 2147483647 2147483647
      4     2147483647 2147483647 2147483647 2147483647 2147483647
      5     2147483647 2147483647 2147483647 2147483647 2147483647
      6     2147483647 2147483647 2147483647 2147483647 2147483647
      7     2147483647 2147483647 2147483647 2147483647 2147483647
      8     2147483647 2147483647 2147483647 2147483647 2147483647
      9     2147483647 2147483647 2147483647 2147483647 2147483647
write "Test difpic on multiband images"
Test difpic on multiband images
gen a 10 20 30 ORG="BSQ"
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen b 10 20 30 ORG="BSQ" ival=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
write "Should get 6000 differences."
Should get 6000 differences.
difpic (a b)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =6000
gen a 10 20 30 ORG="BIL" 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen b 10 20 30 ORG="BIL" 'HALF ival=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
write "Should get 3000 differences."
Should get 3000 differences.
difpic (a b) NB=15
Beginning VICAR task difpic
DIFPIC version 29jun04
 Warning: BIL format may cause performance degradation
 NUMBER OF DIFFERENCES =3000
gen a 10 20 30 ORG="BIP" 'REAL
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen b 10 20 30 ORG="BIP" 'REAL ival=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
write "Should get error message about BIP files"
Should get error message about BIP files
difpic (a b) NB=15
Beginning VICAR task difpic
DIFPIC version 29jun04
 BIP files not supported, use program TRAN to convert to BSQ
write "Test difpic on multiband images with output file"
Test difpic on multiband images with output file
gen difpica 40 40 10 'byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 10 ival=5 'byte
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc 'mod
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      16000
 TOTAL NUMBER OF DIFFERENT PIXELS=      16000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,20) nb=2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 ***********
 Band =     1
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      2     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      3     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      4     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      5     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      6     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      7     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      8     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      9     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
     10     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 ***********
 Band =     2
 ***********
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      2     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      3     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      4     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      5     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      6     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      7     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      8     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
      9     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
     10     251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251 251
gen difpica 40 40 10 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 10 ival=5 'half
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpicb,difpica) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 AVE VAL OF POS DIFFS=  5.00000
 NUMBER OF POS DIFF=      16000
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=      16000
 AVE VAL OF DIFFS=  5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,20) nb=2
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 ***********
 Band =     1
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      2         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      3         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      4         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      5         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      6         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      7         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      8         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      9         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
     10         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 ***********
 Band =     2
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      2         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      3         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      4         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      5         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      6         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      7         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      8         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
      9         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5
     10         5     5     5     5     5     5     5     5     5     5     5     5     5     5     5

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 ***********
 Band =     1
 ***********
     Samp      16    17    18    19    20
   Line
      1         5     5     5     5     5
      2         5     5     5     5     5
      3         5     5     5     5     5
      4         5     5     5     5     5
      5         5     5     5     5     5
      6         5     5     5     5     5
      7         5     5     5     5     5
      8         5     5     5     5     5
      9         5     5     5     5     5
     10         5     5     5     5     5


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:46 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 ***********
 Band =     2
 ***********
     Samp      16    17    18    19    20
   Line
      1         5     5     5     5     5
      2         5     5     5     5     5
      3         5     5     5     5     5
      4         5     5     5     5     5
      5         5     5     5     5     5
      6         5     5     5     5     5
      7         5     5     5     5     5
      8         5     5     5     5     5
      9         5     5     5     5     5
     10         5     5     5     5     5
gen difpica 40 40 10 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 10 ival=5 'full
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      16000
 TOTAL NUMBER OF DIFFERENT PIXELS=      16000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5) nb=2
Beginning VICAR task list

   FULL     samples are interpreted as FULLWORD data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 ***********
 Band =     1
 ***********
     Samp            1          2          3          4          5
   Line
      1             -5         -5         -5         -5         -5
      2             -5         -5         -5         -5         -5
      3             -5         -5         -5         -5         -5
      4             -5         -5         -5         -5         -5
      5             -5         -5         -5         -5         -5
      6             -5         -5         -5         -5         -5
      7             -5         -5         -5         -5         -5
      8             -5         -5         -5         -5         -5
      9             -5         -5         -5         -5         -5
     10             -5         -5         -5         -5         -5


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 ***********
 Band =     2
 ***********
     Samp            1          2          3          4          5
   Line
      1             -5         -5         -5         -5         -5
      2             -5         -5         -5         -5         -5
      3             -5         -5         -5         -5         -5
      4             -5         -5         -5         -5         -5
      5             -5         -5         -5         -5         -5
      6             -5         -5         -5         -5         -5
      7             -5         -5         -5         -5         -5
      8             -5         -5         -5         -5         -5
      9             -5         -5         -5         -5         -5
     10             -5         -5         -5         -5         -5
gen difpica 40 40 10 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 10 ival=5 'real4
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc nb=4 sb=3
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=6400
 TOTAL NUMBER OF DIFFERENT PIXELS=6400
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5) nb=2
Beginning VICAR task list

   REAL     samples are interpreted as  REAL*4  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 ***********
 Band =     1
 ***********
     Samp             1           2           3           4           5
   Line
      1      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      2      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      3      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      4      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      5      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      6      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      7      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      8      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      9      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
     10      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 ***********
 Band =     2
 ***********
     Samp             1           2           3           4           5
   Line
      1      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      2      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      3      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      4      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      5      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      6      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      7      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      8      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      9      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
     10      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
gen difpica 40 40 10 'real8
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 10 ival=5 'real8
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.00000
 NUMBER OF NEG DIFFS=      16000
 TOTAL NUMBER OF DIFFERENT PIXELS=      16000
 AVE VAL OF DIFFS= -5.00000
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5) nb=2
Beginning VICAR task list

   DOUB     samples are interpreted as  REAL*8  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 ***********
 Band =     1
 ***********
     Samp             1           2           3           4           5
   Line
      1      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      2      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      3      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      4      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      5      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      6      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      7      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      8      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      9      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
     10      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 ***********
 Band =     2
 ***********
     Samp             1           2           3           4           5
   Line
      1      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      2      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      3      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      4      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      5      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      6      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      7      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      8      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
      9      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
     10      -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00  -5.000E+00
gen difpica 40 40 10 'comp
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 10 ival=5 'comp
Beginning VICAR task gen
GEN Version 6
GEN task completed
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 AVE VAL OF NEG DIFFS= -5.000E+00  0.000E+00
 NUMBER OF NEG DIFFS=      16000
 TOTAL NUMBER OF DIFFERENT PIXELS=      16000
 AVE VAL OF DIFFS= -5.000E+00  0.000E+00
 % DIFF PIXELS=  100.000
list difpicc (1,1,10,5) nb=2
Beginning VICAR task list

   COMP     samples are interpreted as COMPLEX  data
 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 ***********
 Band =     1
 ***********
     Samp                         1                       2                       3                       4                       5
   Line
      1      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      2      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      3      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      4      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      5      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      6      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      7      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      8      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      9      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
     10      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00


 Task:GEN       User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 Task:DIFPIC    User:lwk       Date_Time:Tue Jun 29 14:38:47 2004
 ***********
 Band =     2
 ***********
     Samp                         1                       2                       3                       4                       5
   Line
      1      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      2      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      3      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      4      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      5      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      6      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      7      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      8      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
      9      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
     10      -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00  -5.000E+00   0.000E+00
gen difpica 40 40 'comp
Beginning VICAR task gen
GEN Version 6
GEN task completed
gen difpicb 40 40 ival=5 'comp
Beginning VICAR task gen
GEN Version 6
GEN task completed
ccomp difpica (cr ci) 'rect 'forw
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
ccomp (cr ci) difpicb 'rect 'inv
Beginning VICAR task ccomp
CCOMP version 02-MAY-94
label-l difpica
Beginning VICAR task label
************************************************************
 
        ************  File difpica ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in COMP format from a X86-LINUX host
                1 bands
                40 lines per band
                40 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Tue Jun 29 14:38:47 2004 ----
IVAL=(0.0, 0.0)
SINC=(1.0, 1.0)
LINC=(1.0, 1.0)
BINC=(1.0, 1.0)
MODULO=(0.0, 0.0)
 
************************************************************
label-l difpicb
Beginning VICAR task label
************************************************************
 
        ************  File difpicb ************
                3 dimensional IMAGE file
                File organization is BSQ
                Pixels are in COMPLEX format from a X86-LINUX host
                1 bands
                40 lines per band
                40 samples per line
                0 lines of binary header
                0 bytes of binary prefix per line
---- Task: GEN -- User: lwk -- Tue Jun 29 14:38:47 2004 ----
IVAL=(0.0, 0.0)
SINC=(1.0, 1.0)
LINC=(1.0, 1.0)
BINC=(1.0, 1.0)
MODULO=(0.0, 0.0)
---- Task: CCOMP -- User: lwk -- Tue Jun 29 14:38:47 2004 ----
---- Task: CCOMP -- User: lwk -- Tue Jun 29 14:38:47 2004 ----
 
************************************************************
difpic (difpica,difpicb) difpicc
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF POS DIFF=   0
 NUMBER OF NEG DIFFS=   0
 TOTAL NUMBER OF DIFFERENT PIXELS=   0
 AVE VAL OF DIFFS=  0.000E+00  0.000E+00
 % DIFF PIXELS=  0.00000
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
