$!****************************************************************************
$!
$! Build proc for MIPL module stats
$! VPACK Version 1.9, Thursday, May 30, 2002, 11:33:10
$!
$! Execute by entering:		$ @stats
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
$ write sys$output "*** module stats ***"
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
$ write sys$output "Invalid argument given to stats.com file -- ", primary
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
$   if F$SEARCH("stats.imake") .nes. ""
$   then
$      vimake stats
$      purge stats.bld
$   else
$      if F$SEARCH("stats.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake stats
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @stats.bld "STD"
$   else
$      @stats.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create stats.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack stats.com -mixed -
	-s stats.f -
	-i stats.imake -
	-p stats.pdf -
	-t tststats.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create stats.f
$ DECK/DOLLARS="$ VOKAGLEVE"
                                                                 
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C     PROGRAM STATS
C      6 MAY  1977 ...JDA... INITIAL RELEASE
C     28 JUNE 1978 ...JDA... CHANGE CALL LABELC TO CALL LABELB
C     19 SEPT 1981 ...REA... FIX BUG THAT EATS CORE, AND
C                            CHANGE LABELB TO LABELC
C     10 APR  1983 ...REA... EXPAND INPUT BUFFER TO 19072 BYTES
C      1 OCT  1983 ...AJR... MODIFY PARAMETERS AND VAX CONVERSION
C     25 JUL  1985 ...REA... FIX BUGS TO MAKE THE 'ALL' FEATURE OF
C				HIST AND SPEC WORK
C     25 FEB  1986 ...SP.... CONVERTED TO VICAR2 CALLS.
C     25 FEB  1986 ...SP.... CHANGED TO ALLOW UP TO 12 INPUT FILES.
C     25 FEB  1986 ...SP.... RENAMED ROUTINE FORMAT AS HEADING TO AVOID CONFLICT
C     25 FEB  1986 ...SP.... CORRECTED BUG IN PFIELD FOR LISTING OF VERTICES.
C     11 OCT  1988 ...SP.... CORRECTED BUG IN PFIELD PRINTING MORE THAN 4 DIGITS
C     31 OCT  1994 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C     10 JUL  1995 ...CRI... CHANGED FIRST OUTPUT FILE FORMAT TO ISTATFILE
C     30 MAY  2002 ...AXC... MODIFIED VICAR RTL CALL.  (AR-106777)
C
C        'STATS'   STATISTICS PROCESSOR PROGRAM
C
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'fortport'

      character*80 msg
      CHARACTER*10 PNAME
      CHARACTER*8 FORMAT
      CHARACTER*40 INST
      LOGICAL*4 XVPTST
      CHARACTER*132 BUFFER,BUFFER1
      INTEGER PARM(2000)
      INTEGER EXCLUD(10),nclasses,nbands
      INTEGER LINE2(12),SPLOT(50)
      INTEGER PTRBF1(12),PTRBUF(12)
      INTEGER BND(12)
      INTEGER VERTS(50)
      INTEGER HIST(12)
      INTEGER SPEC(12)
      INTEGER IDN(12),CLSPTS(50),NPIX
      INTEGER HISBUF(256,12)
      INTEGER IUNIT(12),OUNIT(2)
      INTEGER*2 LNBUF(602),SSBUF(600),NSBUF(600)
      INTEGER*2 LXBUF(1202),S1BUF(1200),NS1BUF(1200)
      BYTE    BUF(7200),SBUF(19072),OBUF(2000),DNBUF(600)
      LOGICAL MSS,NOPRNT,ADD
      LOGICAL STAT,SCRIBE
      CHARACTER*8 CNAME(50)
      REAL    MEAN(12,50),DEV(12,50),COV(78),COR(78),SMEAN
      REAL    MEANS(1000),COVARIANCE(1000)
      REAL*8 XMEAN(12),XCOV(78),PTS1,PTS2
      EQUIVALENCE (BUF(1),SBUF(1),OBUF(1))
      COMMON /IOSTFF/ IOLINE
      COMMON /C1/ DEV,MEAN,CNAME,SBUF
      COMMON /C2/ PARM,LNBUF,SSBUF,NSBUF,HISBUF
C
      DATA BND/1,2,3,4,5,6,7,8,9,10,11,12/
      DATA VERTS/50*1/
      DATA HIST/1,2,3,4,5,6,7,8,9,10,11,12/
      DATA SPEC/1,2,3,4,5,6,7,8,9,10,11,12/
C
C
      CALL IFMESSAGE('STATS version 30-MAY-02')
C
C
C        ZERO THE LARGE ARRAYS
      CALL ZIA(DEV,6068)
      CALL ZIA(PARM,5973)       
C
C
C
C        DEFAULT PARAMETERS
      MAXFLD= 600
      NHIST= 0
      NSPEC= 0
      NPLOT= 0
      MSS= .FALSE.
      NOPRNT= .FALSE.
      NEXCL= 0
      SCRIBE= .FALSE.
      ADD= .TRUE.
      STAT= .TRUE.
      ISCRIBE= 1
      FORM= 1
      XDN= 255
      XDS= 2
      DO J=1,50
      SPLOT(J)= J
      END DO
C

C        GET NUMBER OF INPUT AND OUTPUT DATA SETS
      CALL XVPCNT('INP',NI)
      CALL XVPCNT('OUT',NO)

C        GET UNIT NUMBERS FOR ALL DATA SETS
      DO I=1,NI
         CALL XVUNIT(IUNIT(I),'INP',I,ISTAT,' ')
         IRETURN = 1
         CALL XVSIGNAL(IUNIT(I), ISTAT, IRETURN)
      END DO
      DO I=1,NO
         CALL XVUNIT(OUNIT(I),'OUT',I,ISTAT,' ')
         IRETURN = 1
         CALL XVSIGNAL(OUNIT(I), ISTAT, IRETURN)
      END DO

C        GET SIZE AND FORMAT OF PRIMARY INPUT AND SIZE FIELD INFO

      CALL XVOPEN(IUNIT(1),ISTAT, 'OP', 'READ',
     +                 'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL XVGET(IUNIT(1),ISTAT,'FORMAT',FORMAT,'NL',NLI,'NS',
     +                 NSI,' ')
      CALL XVCLOSE(IUNIT(1),ISTAT,' ')
      IF(FORMAT.NE.'BYTE') THEN
         CALL XVMESSAGE('STATS ACCEPTS BYTE DATA ONLY',' ')
         CALL ABEND
      END IF

C        GET PARAMETERS
      NUSE= NI                   ! NUSE IS THE NUMBER OF BANDS TO BE USED.

C        'MSS'
      CALL XVPARM('MSS',PARM,ICOUNT,IDEF,32)
      IF(PARM(1) .GT. 0) THEN
         MSS= .TRUE.
         NCHAN= PARM(1)          ! NCHAN IS THE NUMBER OF IMAGES MSSED.
         NUSE= NCHAN

C        'BAND'
        CALL XVPARM('BAND',PARM,ICOUNT,IDEF,32)
        IF(PARM(1) .GT. 0) THEN
           NUSE= ICOUNT
           DO J=1,NUSE
              BND(J)= PARM(J)
           END DO
        END IF
      END IF

C        'HIST'
      CALL XVPARM('HIST',PARM,ICOUNT,IDEF,32)
      IF(PARM(1) .EQ. 0)   THEN
        NHIST=NUSE
      ELSE IF (PARM(1) .NE. -1)  THEN
         NHIST= ICOUNT
         DO J=1,NHIST
            HIST(J)= PARM(J)
         END DO
      END IF

C        'SPEC'
      CALL XVPARM('SPEC',PARM,ICOUNT,IDEF,32)
      IF(PARM(1) .EQ. 0)   THEN
        NSPEC=NUSE
      ELSE IF (PARM(1) .NE. -1)  THEN
         NSPEC= ICOUNT
         DO J=1,NSPEC
            SPEC(J)= PARM(J)
         END DO
      END IF

C        'NOPR'
      IF(XVPTST('NOPRINT')) NOPRNT= .TRUE.
C        'EXCL'
      CALL XVPARM('EXCLUDE',PARM,ICOUNT,IDEF,32)
      IF (ICOUNT .GT. 0) THEN
         NEXCL= ICOUNT
         IF(NEXCL.GT.10)  NEXCL= 10
         DO J=1,NEXCL
            EXCLUD(J)= PARM(J)
         END DO
      END IF
     
C        'SPLOT'
      CALL XVPARM('SPLOT',PARM,ICOUNT,IDEF,32)
      IF(PARM(1).GT.0) THEN      
         NPLOT= ICOUNT
         DO J=1,NPLOT
            SPLOT(J)= PARM(J)
         END DO
      END IF

C        'SCRIBE'
      CALL XVPARM('SCRIBE',PARM,ICOUNT,IDEF,1)
      IF (PARM(1) .GT. 0) THEN
         SCRIBE=.TRUE.
         ISCRIBE=PARM(1)
      END IF

C        'VERT'
      CALL XVPARM('VERT',PARM,ICOUNT,IDEF,50)
      IF (PARM(1) .GT. 0) THEN
         DO J=1,ICOUNT
            IF(PARM(J).LE.50 .AND. PARM(J).GE.1) VERTS(PARM(J))=2
         END DO
      END IF

C        'DN'
      CALL XVPARM('DN',PARM,ICOUNT,IDEF,1)
      IF (ICOUNT .GT. 0) THEN
         XDN= PARM(1)
         ADD= .FALSE.
      END IF

100   CONTINUE
      NSCHAN= NSI
      IF(MSS) NSCHAN= NSI/NCHAN
      MTRX= (NUSE*(NUSE+1))/2
      NSO = 4*(NUSE+MTRX+3)
      IF(NO.GE.2)  SCRIBE= .TRUE.

      IF(SCRIBE .AND. NO.LT.2)  XDS= 1
      IF(NO.EQ.0 .OR. XDS.EQ.1)  STAT= .FALSE.
      OSCRIBE = OUNIT(XDS)
      STATUNIT= OUNIT(1)

      IF(NHIST.GT.0 .AND. SCRIBE)  THEN
         BUFFER1(1:47)='*** HISTOGRAMS & SCRIBING MUST BE PROCESSED SEP'
         BUFFER1(48:77)='ARATELY ... HISTOGRAMS DELETED'
         CALL XVMESSAGE(BUFFER1,' ')
         NHIST= 0
      END IF
C
C        OPEN DATA SETS

      DO I=1,NI
        CALL XVOPEN(IUNIT(I),ISTAT, 'OP', 'READ',
     +               'OPEN_ACT','SA','IO_ACT','SA',' ')
      END DO

      IF(STAT) THEN
         NCLASSES=100
         NBANDS=NUSE
         INST=' '
         CALL ISTAT_FILE_OPEN(STATUNIT,'WRITE',NCLASSES,NBANDS,INST,
     &                        ISTAT)
         IF (ISTAT.LT.0) CALL ISTAT_SIGNAL(STATUNIT,ISTAT,1)
      END IF

      IF (SCRIBE)  THEN

C        COPY THE SCRIBE PICTURE

      CALL XVOPEN(OSCRIBE, ISTAT, 'OP', 'WRITE', 'U_NL',NLI,
     .           'U_NS',NSCHAN,'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL ITLA(XDN,DNBUF,MAXFLD)
      PTR= 1
      IF(MSS)  PTR= NSCHAN*(ISCRIBE-1)+1
      DO 250 II=1,NLI
         IF(MSS)  THEN
            CALL XVREAD(IUNIT(1), SBUF, ISTAT,' ')
         ELSE
            CALL XVREAD(IUNIT(ISCRIBE), SBUF, ISTAT,' ')
         END IF
         IF(ADD)  CALL HSTGNB(NSCHAN,SBUF(PTR),HISBUF(1,1))
         CALL XVWRIT(OSCRIBE, SBUF(PTR), ISTAT,' ')
250   CONTINUE
      XMEAN(1)= 0.
      DO J=1,256
         XMEAN(1)= DFLOAT(J-1)*DFLOAT(HISBUF(J,1))+XMEAN(1)
      END DO
      SMEAN= XMEAN(1)/(DFLOAT(NSCHAN)*DFLOAT(NLI))

      CALL XVCLOSE(OSCRIBE, ISTAT,' ')
      CALL XVOPEN(OSCRIBE, ISTAT, 'OP', 'UPDATE', 'U_NL',NLI,
     .           'U_NS',NSCHAN,'OPEN_ACT','SA','IO_ACT','SA',' ')

      END IF
C
C        ZERO BUFFERS
255   CALL ZIA(CLSPTS,50)
      NCLS= 0
      PARNXT= 0
      CLSCNT= 0
C
C        LOAD POINTER BUFFERS
      K= MAXFLD

      IF(MSS)  K= NSCHAN
      DO J=1,NUSE
         PTRBF1(J)= K*(BND(J)-1)+1
         IF(.NOT.MSS)  PTRBUF(J)=PTRBF1(J)
      END DO

      IF(NEXCL.GT.0)  CALL PRNT(4,NEXCL,EXCLUD,
     & 'THE FOLLOWING DN ARE EXCLUDED FROM STATISTICS:.')

C    LOOP THROUGH THE CLASSnn PARAMETERS AND HANDLE THE SPECIFIED CLASSES.

      PNAME='CLASS'
      DO 800 ICLASS = 1,50

         IO = ICLASS
         IF ( IO .LT. 10 ) THEN
           write(msg,421) io
421        format('CLASS',i1)
         ELSE IF ( IO .LT. 100 ) THEN
             write(msg,422) io
422          format('CLASS',i2)
         ELSE
             write(msg,423) io
423          format('CLASS',i3)
         END IF

         CALL XVPARM( MSG, PARM, ICOUNT, IDEF,10)


         IF (ICOUNT .LE. 2)    GOTO 800
C
C        GET TRAINING FIELD ADDRESS
C
309   FLDNUM=0
      CLSCNT=CLSCNT+1

C  GET THE CLASS NAME
310   CLS=CLSCNT
      WRITE(CNAME(CLS),'(A,I3.3)') 'CLASS',CLSCNT

C  CHECK IF CLASS IS RECT (FORM=1) OR VERT (FORM=2)  

      FORM = VERTS(ICLASS)
      IF(NHIST.GT.0)  CALL ZIA(HISBUF(1,1),3072)
      CALL ZIA(XMEAN,24)
      CALL ZIA(XCOV,156)
      NCLS=CLS

      IF(.NOT.NOPRNT)  CALL HEADING(FORM,CLS,CNAME(CLS))

C...LOOP THROUGH THE TRAINING FIELDS FOR THIS TRAINING AREA.

      PAR = 1
      DO WHILE (PAR+3 .LE. ICOUNT)    ! THERE IS ANOTHER TRAINING FIELD IF
                                      ! THERE ARE AT LEAST 4 VALUES LEFT.
        FLDNUM= FLDNUM+1                ! FLDNUM = NUMBER OF FIELDS IN TR. AREA.
        NPNTS= 1
        IF(FORM.EQ.1)  GO TO 350
        PTR = PAR
      DO 325 I=1,(ICOUNT/2) - 1
        PTR= PTR+2         
        IF(PARM(PTR).EQ.PARM(PAR) .AND. PARM(PTR+1).EQ.PARM(PAR+1)) 
     .      GO TO 330                 ! MAKE SURE WE HAVE CLOSURE.

325   CONTINUE
      GO TO 960
330   NPNTS= I
C
350   CALL AREA(FORM,PAR,NPNTS,PERPTS)
      IF(.NOT.NOPRNT) CALL PFIELD(FORM,PAR,NPNTS)
      IF(SCRIBE)  CALL OUTLIN(PERPTS,FLDNUM,NLI,NSCHAN)
      PARNXT= 2*(NPNTS+1)
C
C        READ FIELD,   GATHER STATS
      II= 0
400   II= II+1
      IF(LNBUF(II).EQ.0)  GO TO 7777
      SL= LNBUF(II)
      SS= SSBUF(II)
      NS= NSBUF(II)
      IF(NS.GT.MAXFLD)  NS= MAXFLD
      DO J=1,NI
         LINE2(J)= SL
      END DO
      CLSPTS(CLS)= CLSPTS(CLS)+NS
      IF(MSS)  THEN
         CALL XVREAD( IUNIT(1), SBUF, ISTAT, 'LINE',LINE2(1),' ')
         DO J=1,NUSE
            PTRBUF(J)= PTRBF1(J)+SS-1
         END DO
      ELSE
         DO J=1,NUSE
            PTR= PTRBUF(J)
            CALL XVREAD( IUNIT(J), BUF(PTR), ISTAT, 'LINE',LINE2(J),
     .                   'SAMP',SS, 'NSAMPS', NS,' ')
         END DO
      END IF
      IF(NHIST.EQ.0)  GO TO 435
      DO J=1,NHIST
         K= HIST(J)
         PTR= PTRBUF(K)
         CALL HSTGNB(NS,BUF(PTR),HISBUF(1,K))
      END DO
C
435   DO 460 J=1,NS
      JV= J-1
      DO K=1,NUSE
         PTR= PTRBUF(K)+JV
         IDN(K)= BYTE2INT(BUF(PTR))
      END DO
      IF(NEXCL.EQ.0)  GO TO 445
           DO 444 NX=1,NEXCL
              NXC= EXCLUD(NX)
              DO KK=1,NUSE
                 IF(IDN(KK).EQ.NXC)  GO TO 443
              END DO
              GO TO 444
443           CLSPTS(CLS)= CLSPTS(CLS)-1
              GO TO 460
444        CONTINUE
445   KL=0
      DO K=1,NUSE
         XMEAN(K)= XMEAN(K) + IDN(K)
         DO L=1,K
            KL=KL+1
            XCOV(KL)= XCOV(KL)+IDN(K)*IDN(L)
         END DO
      END DO
460   CONTINUE
      GO TO 400
C
C
C        DO THE SCRIBING ON THE PICTURE
7777  CONTINUE
      IF(SCRIBE)  THEN
C
C        SET OUTLINE DN
      IF(.NOT.ADD)  GO TO 739
      XDN= 0
      IF(XMEAN(ISCRIBE)/CLSPTS(CLS).LT.SMEAN)  XDN= 255
      CALL ITLA(XDN,DNBUF,MAXFLD)
739   CONTINUE
C
C        SCRIBE FIELD ON PICTURE
      II= 0
740   II= II+1
      LX= LXBUF(II)
      IF(LX.EQ.0)  GO TO 750
      CALL XVREAD( OSCRIBE, OBUF, ISTAT, 'LINE', LX,' ')
      J1= S1BUF(II)
      J2= NS1BUF(II)
      IF(J1.LT.0.OR.J2.LT.0)  GO TO 740

      CALL MVE(1,J2,DNBUF,OBUF(J1),1,1)
      CALL XVWRIT( OSCRIBE, OBUF, ISTAT, 'LINE', LX,' ')
      GO TO 740
750   CONTINUE

      END IF

      PAR = PAR + PARNXT

      END DO

      IF (PAR .NE. ICOUNT+1)  GOTO 960  
C
C
C        COMPUTE STATS
600   CONTINUE
      PTS1= CLSPTS(CLS)
      PTS2= PTS1-1.
      IF(PTS1.GE.2.)  GO TO 610
      PTS1= 1.
      PTS2= 1.
610   PTS1= 1./PTS1
      PTS2= 1./PTS2
      KL= 0
      DO 630 J=1,NUSE
      DO 620 L=1,J
      KL= KL+1
      XCOV(KL)= (XCOV(KL)-XMEAN(J)*XMEAN(L)*PTS1)*PTS2
      COV(KL)= XCOV(KL)
620   CONTINUE
      IF (XCOV(KL).GT.0.0) THEN
	      DEV(J,CLS) = DSQRT(XCOV(KL))
	  ELSE
	      DEV(J,CLS) = 0.0
      END IF
  630 CONTINUE
      KL= 0
      DO J=1,NUSE
	  MEAN(J,CLS) = XMEAN(J)*PTS1
	  DO L=1,J
	      KL= KL+1
	      IF (DEV(J,CLS).EQ.0.0 .OR. DEV(L,CLS).EQ.0.0) THEN
		      COR(KL)= 0.
		  ELSE
		      COR(KL)= COV(KL)/(DEV(J,CLS)*DEV(L,CLS))
	      END IF
	  END DO
      END DO
      IF(NOPRNT)  GO TO 735
C
      WRITE(BUFFER,1003) CLSPTS(CLS)
      CALL XVMESSAGE(BUFFER,' ')
      call xvmessage(' ',' ')
      WRITE(BUFFER,1004) CLS,CNAME(CLS)
      CALL XVMESSAGE(BUFFER,' ')
      WRITE(BUFFER,1005) (I,I=1,NUSE)
      CALL XVMESSAGE(BUFFER,' ')
      WRITE(BUFFER,1006) (MEAN(I,CLS),I=1,NUSE)
      CALL XVMESSAGE(BUFFER,' ')
      WRITE(BUFFER,1007) (DEV(I,CLS),I=1,NUSE)
      CALL XVMESSAGE(BUFFER,' ')
      call xvmessage(' ',' ')
      WRITE(BUFFER,1008)
      CALL XVMESSAGE(BUFFER,' ')
      KL1= 1
      DO 730 J=1,NUSE
      KL1= KL1+J-1
      KL2= KL1+J-1
      WRITE(BUFFER,1009) (COV(I),I=KL1,KL2)
      CALL XVMESSAGE(BUFFER,' ')
730   CONTINUE
      call xvmessage(' ',' ')
      WRITE(BUFFER,1011)
      CALL XVMESSAGE(BUFFER,' ')
      KL1= 1
      DO 732 J=1,NUSE
      KL1= KL1+J-1
      KL2= KL1+J-1
      WRITE(BUFFER,1015) (COR(I),I=KL1,KL2)
      CALL XVMESSAGE(BUFFER,' ')
732   CONTINUE
C
735   IF(NHIST.GT.0)  CALL HISTGM(HISBUF,HIST,NHIST,CNAME(CLS),
     &                            NEXCL,EXCLUD)
      IF (STAT)  THEN
C
C        WRITE MEANS & COVARIANCE MATRIX ON STAT DATA SET
 
         NN= 4*NUSE
         NM= 4*MTRX

         CALL MVE(7,NUSE,MEAN(1,CLS),MEANS,1,1)
         CALL MVE(7,1,CLSPTS(CLS),NPIX,1,1)
         CALL MVE(7,MTRX,COV(1),COVARIANCE,1,1)
         CALL ISTAT_RECORD_WRITE(STATUNIT,ICLASS,CNAME(CLS),NPIX,NBANDS,
     &                           MEANS,COVARIANCE,ISTAT)
         IF (ISTAT.LT.0) CALL ISTAT_SIGNAL(STATUNIT,ISTAT,1)
      END IF

800   CONTINUE

      IF (NCLS .EQ. 0) GOTO 970        ! IF NO CLASSES.
C
C  PRINT THE CLASS TABLE
      call xvmessage(' ',' ')
      CALL XVMESSAGE(' CLASS NUMBERS ASSIGNED',' ')
      CALL XVMESSAGE(' ----------------------',' ')
      DO 805 J=1,NCLS
      WRITE(BUFFER,6002) J,CNAME(J)
6002  FORMAT(I6,' = ',A)
      CALL XVMESSAGE(BUFFER,' ')
805   CONTINUE
C
      IF(NPLOT.EQ.0)  NPLOT= NCLS
      IF(NSPEC.GT.0)  CALL SPECTL(SPEC,SPLOT,NSPEC,NPLOT)

      IF (SCRIBE)  CALL XVCLOSE(OSCRIBE, ISTAT,' ')
C
      IF(STAT)  THEN
         CALL ISTAT_FILE_INFO (STATUNIT,NCLASSES,NBANDS,IBIS)
         CALL IBIS_FILE_SET (IBIS,'NR',NCLS,ISTAT)
         IF (ISTAT.NE.1) CALL IBIS_SIGNAL(IBIS,ISTAT,1)
         CALL ISTAT_FILE_CLOSE(STATUNIT,ISTAT,1)
         IF (ISTAT.LT.0) CALL ISTAT_SIGNAL(STATUNIT,ISTAT,1)
      END IF

      RETURN

960   WRITE(BUFFER,1010) ICLASS
      CALL XVMESSAGE(BUFFER,' ')
      CALL ABEND 
970   CALL XVMESSAGE('*** NO TRAINING AREAS SPECIFIED',' ')
      CALL ABEND 
C
C
C
1000  FORMAT('THE FOLLOWING DN ARE EXCLUDED FROM STATISTICS ',
     &    10(I5,','))
1003  FORMAT(5X,'TOTAL POINTS =',I7)
      call xvmessage(' ',' ')
1004  FORMAT(' STATISTICS FOR CLASS # ',I2,'   "',A,'"')
1005  FORMAT('   CHANNEL',4X,12(I2,7X))
1006  FORMAT('    MEAN ',12F9.2)
1007  FORMAT('   ST DEV',12F9.2)
      call xvmessage(' ',' ')
1008  FORMAT('   COVARIANCE MATRIX')
1009  FORMAT(9X,12F9.2)
1010  FORMAT('*** PARAMETER ERROR FOR CLASS',I3)
      call xvmessage(' ',' ')
1011  FORMAT('   CORRELATION MATRIX')
1012  FORMAT('EXTERNAL PARAMETERS TO BE SUPPLIED')
1013  FORMAT('*** KEYWORD -TRAIN- NOT SPECIFIED')
1014  FORMAT('... WARNING---HISTOGRAMS AND SCRIBING CANNOT BOTH BE DONE
     & IN THE SAME EXECUTION',/'              HISTOGRAMS DELETED')
1015  FORMAT(9X,12F9.4)
C
      END
C
      SUBROUTINE HEADING(FORM,CLS,CNAME)
      IMPLICIT INTEGER (A-Z)
      COMMON /IOSTFF/ IOLINE
      CHARACTER*132 BUFFER
      INTEGER PARM(2000)
      CHARACTER*8 CNAME
      COMMON /C2/ PARM,FIL(3973)
C
C        PRINT THE CLASS HEADING
C
      call xvmessage(' ',' ')
      WRITE(BUFFER,1001) CLS,CNAME
      CALL XVMESSAGE(BUFFER,' ')
      IF(FORM.EQ.1)  RETURN
      WRITE(BUFFER,1003)
      CALL XVMESSAGE(BUFFER,' ')
      WRITE(BUFFER,1004)
      CALL XVMESSAGE(BUFFER,' ')
      RETURN
C
C
C
      ENTRY PFIELD(FORM,PAR,NPNTS)
      GO TO (100,200,300),FORM
C
100   SL= PARM(PAR)
      SS= PARM(PAR+1)
      NL= PARM(PAR+2)
      NS= PARM(PAR+3)
      WRITE(BUFFER,1002) SL,SS,NL,NS
      CALL XVMESSAGE(BUFFER,' ')
      RETURN
C
200   NPT2= 2*(NPNTS+1)
      PAR1= PAR-1
      DO 250 K=1,NPT2,14
         KEND = MIN( K+13, NPT2 )
         WRITE(BUFFER,1005) (PARM(PAR1+I),PARM(PAR+I),I=K,KEND,2)
         CALL XVMESSAGE(BUFFER(2:132),' ')
250   CONTINUE
      RETURN
C
300   RETURN
C
C
1001  FORMAT('TRAINING AREAS FOR CLASS # ',I2,'   "',A,'"')
1002  FORMAT(9X,'SL=',I5,'   SS=',I5,'   NL=',I5,'   NS=',I5)
1003  FORMAT('IRREGULAR AREA VERTICES')
1004  FORMAT(7('LINE SAMPLE     '))
1005  FORMAT(7(2I6,4X))
      END
C
      SUBROUTINE AREA(FORM,PAR,NOPTS,PERPTS)
      IMPLICIT INTEGER (A-Z)
      INTEGER PARM(2000)
      INTEGER DUMMY(1300)
      INTEGER*2 LNBUF(602),SSBUF(600),NSBUF(600)
      INTEGER*2 LN2(1204),SAM2(1204),TEST(1204),PTBUF(100,2)
      INTEGER*2 LN(300),SAMP(300),TEMP,TEMP2
      LOGICAL SAMEL,LPOS
      REAL DEL,A,DEL1
      INTEGER MAXLN, MAXPTS
      COMMON /IOSTFF/ IOLINE
      COMMON /C1/ FIL(2562),DUMMY,LN2,SAM2,TEST,PTBUF,LN,SAMP
      COMMON /C2/ PARM,LNBUF,SSBUF,NSBUF,FIL2(3072)

      DATA MAXLN/600/
      DATA MAXPTS/300/
C
C
C        TRAINING AREA FORMATS
C     FORM=1   RECTANGULAR FORMAT
C     FORM=2   VERTICES FORMAT
C
      GO TO (100,400),FORM
C
C        RECTANGULAR FORMAT
100   SL= PARM(PAR)
      SS= PARM(PAR+1)
      NL= PARM(PAR+2)
      NS= PARM(PAR+3)
      IF(NS.GT.MAXLN .OR. NL.GT.MAXLN)  GO TO 901
      DO 150 I=1,NL
      LNBUF(I)= SL+I-1
      SSBUF(I)= SS
150   NSBUF(I)= NS
      LNBUF(NL+1)= 0
C
C     GENERATE PERIMETER POINTS FOR OUTLIN
C
      LN2(1)=SL
      SAM2(1)=SS
      TEST(1)=1
      QQQ=NL+1
      DO 1 K=2,QQQ
      LN2(K)=SL+K-2
      SAM2(K)=SS+NS-1
1     TEST(K)=2
      XXX= NL
      DO 2 K=1,XXX
      SAM2(K+QQQ)=SS
      TEST(K+QQQ)=1
2     LN2(K+QQQ)=NL+SL-K
      PERPTS=2*NL+1
      RETURN
C
C
400   JJ= PAR-2
      IF(NOPTS.GT.MAXPTS)  GO TO 902
      DO 410 J=1,NOPTS
      JJ= JJ+2
      LN(J)= PARM(JJ)
410   SAMP(J)= PARM(JJ+1)
C
C
C        THE FOLLOWING CODE WRITTEN BY BOB BEGGS   8/74
C        (AREA COMPUTATION FOR ARBITRARY POLYGON)
      I=1
      CALL ZIA(TEST,601)
      DO 550 J=1,NOPTS
      LPOS=.FALSE.
      SAMEL=.FALSE.
      IF(J.EQ.NOPTS)GO TO 420
      GO TO 430
420   LN(J+1)=LN(1)
      SAMP(J+1)=SAMP(1)
C
C     PUT THE INPUT POINTS INTO THE PERIMETER ARRAYS
C
430   LN2(I)=LN(J)
      SAM2(I)=SAMP(J)
      IF(LN(J+1).EQ.LN(J))GO TO 450
C
C     TEST IS EITHER 1 (START OF A LINE SEGMENT) , 2 (END OF A LINE
C     SEGMENT) , OR 3 ( A SINGLE POINT)
C
C     SEE IF LINE IS GOING UP OR DOWN
C
      IF(LN(J+1).LT.LN(J))GO TO 440
C
C     TNUM IS THE TEST NUMBER FOR THE NEXT SET OF INTERPOLATED POINTS
      TNUM=2
      LPOS=.TRUE.
      GO TO 470
C
440   TNUM=1
      GO TO 470
C
450   SAMEL=.TRUE.
      IF(SAMP(J).LT.SAMP(J+1))GO TO 460
      TNUM=2
      TEST(I)=2
      GO TO 470
C
460   TNUM=1
      TEST(I)=1
C
470    I=I+1
C
C
      IF(SAMEL)GO TO 550
C
C
C     INTERPOLATION
C
480   DEL2=LN(J+1)-LN(J)
C     DEL2 IS THE DIFFERENCE BETWEEN INPUT POINTS
C
      IF(DEL2.LT.0)DEL2=0-DEL2
      A=SAMP(J+1)-SAMP(J)
      DEL=A/(LN(J+1)-LN(J))
C
490   INC=1
      IF(.NOT.LPOS)GO TO 500
      GO TO 510
C
C     SET UP INITIAL INCREMENTS
500   INC=-1
      DEL=0-DEL
510   DEL1=DEL
      INC1=INC
C
C     CHECK FOR PEAKS AND VALLEYS IN THE FIGURE
C     THEY WILL BE SINGLE PIXELS AND SO WILL HAVE A TEST OF 3
C
      IF(J.EQ.1)GO TO 535
      FF=LN(J-1)-LN(J)
      IF(FF.LT.0)FF=0-FF
      COMPR=FF*DEL+SAMP(J)+.5
      IF(LN(J).LT.LN(J+1).AND.LN(J).LT.LN(J-1))GO TO 520
      IF(LN(J).GT.LN(J+1).AND.LN(J).GT.LN(J-1))GO TO 525
      GO TO 530
520   IF(SAMP(J-1).LE.COMPR)GO TO 535
      GO TO 530
525   IF(SAMP(J-1).GE.COMPR)GO TO 535
530   TEST(I-1)=TNUM
      GO TO 540
535   TEST(I-1)=3
C
540   IF(DEL2.LT.2)GO TO 550
      DO 545 L=2,DEL2
      LN2(I)=LN(J)+INC1
      SAM2(I)=SAMP(J)+DEL1+.5
      INC1=INC1+INC
      DEL1=DEL+DEL1
      TEST(I)=TNUM
      I=I+1
545   CONTINUE
550   CONTINUE
C
C     SET UP SORTING ARRAYS
C
C     THIS IS ALSO THE  MAIN LOOP
C
      H=0
      DO 680 B=1,MAXLN
C     SORT THINGS ONE LINE AT A TIME
C
      D=0
      LINE=LN(1)+B-1
C
C     FIND PERIMETER POINTS IN EACH LINE AND PUT THEM IN A 2 DIMENSIONAL
C     ARRAY
C
      PERPTS=I
      DO 570 N=1,I
      IF(LN2(N).NE.LINE)GO TO 570
      D=D+1
      PTBUF(D,1)=TEST(N)
      PTBUF(D,2)=SAM2(N)
570   CONTINUE
C
C     IF THERE ARE NO POINTS LEFT BREAK OUT OF LOOP AND END
C
      IF(D.EQ.0)GO TO 690
      IF(D.EQ.1)GO TO 630
C
C     SINKING SORT
C     PUT THE PERIMETER POINTS IN SAMPLE NUMBER ORDER
C
      G=D
      DO 620 SSS=2,G
      E=2
580   IF(PTBUF(E,2).GT.PTBUF(E-1,2))GO TO 610
      IF(PTBUF(E,2).EQ.PTBUF(E-1,2))GO TO 590
      TEMP=PTBUF(E-1,2)
      TEMP2=PTBUF(E-1,1)
      PTBUF(E-1,2)=PTBUF(E,2)
      PTBUF(E-1,1)=PTBUF(E,1)
      PTBUF(E,2)=TEMP
      PTBUF(E,1)=TEMP2
      GO TO 610
C
C     IF TWO POINTS ARE THE SAME MAKE THEM INTO ONE SINGLE
590   PTBUF(E-1,1)=3
      D=D-1
      IF(E.GT.D)GO TO 620
      DO 600 R=E,D
      PTBUF(R,2)=PTBUF(R+1,2)
600   PTBUF(R,1)=PTBUF(R+1,1)
610   E=E+1
      IF(E.LE.D)GO TO 580
620   CONTINUE
C
C     SET UP OUTPUT ARRAYS
C
C     STRT IS THE NUMBER OF STARTS(1) ENCOUNTERED
630   STRT=0
      ENDCNT=0
      DO 675 F=1,D
      IF(PTBUF(F,1).EQ.1)GO TO 650
      IF(PTBUF(F,1).EQ.2)GO TO 670
      IF(STRT.EQ.0)GO TO 640
C
C     IF A START AND END HAVE BEEN FOUND BEFORE A SINGLE PUT THE LINE
C     SEGMENT IN THE OUTPUT ARRAYS
C
      LNBUF(H)=LINE
      NSBUF(H)=PTBUF(KEEP,2)-SSBUF(H)+1
      STRT=0
      ENDCNT=0
C
C     PUT THE SINGLE IN THE OUTPUT ARRAYS
C
640   H=H+1
      SSBUF(H)=PTBUF(F,2)
      LNBUF(H)=LINE
      NSBUF(H)=1
      GO TO 675
C
650   STRT=STRT+1
      IF(STRT.LT.2)GO TO 660
      IF(ENDCNT.EQ.0)GO TO 675
      NEXT1=PTBUF(KEEP,2)+1
      IF(NEXT1.EQ.PTBUF(F,2))GO TO 831
C
C     IF A SECOND START IS FOUND AND THERE IS AN END BEFORE IT PUT THE
C     SEGMENT IN THE OUTPUT ARRAYS
      LNBUF(H)=LINE
      NSBUF(H)=PTBUF(KEEP,2)-SSBUF(H)+1
      STRT=1
C     KEEP THE START,S LOCATION
      H=H+1
      SSBUF(H)=PTBUF(F,2)
831   ENDCNT=0
      GO TO 675
C
660   H=H+1
      SSBUF(H)=PTBUF(F,2)
      GO TO 675
C
C     KEEP THE LAST END,S LOCATION
C
670   ENDCNT=ENDCNT+1
      KEEP=F
675   CONTINUE
      IF(STRT.EQ.0)GO TO 680
      LNBUF(H)=LINE
      NSBUF(H)=PTBUF(KEEP,2)-SSBUF(H)+1
680   CONTINUE
690   LNBUF(H+1)= 0
C
C
700   RETURN
C
901   CALL XVMESSAGE('MAXIMUM OF 600 LINES OR SAMPLES EXCEEDED',' ')
      GO TO 999
902   CALL XVMESSAGE('MAXIMUM OF 300 CONTOUR POINTS EXCEEDED',' ')
999   CALL ABEND   !   RETURN 2
      END
C
      SUBROUTINE OUTLIN(NPTS,FLDNUM, NL, NS)
      IMPLICIT INTEGER (A-Z)
      COMMON /IOSTFF/ IOLINE
      INTEGER*2 LN2(1204),SAM2(1204),TEST(1204)
      INTEGER*2 LXBUF(1202),S1BUF(1200),NS1BUF(1200)
      INTEGER DUMMY(1300),DUMMY2(2901)
      COMMON /C1/ FIL(2962),DUMMY,LN2,SAM2,TEST
      COMMON /C2/ FIL2(1271),DUMMY2,LXBUF,S1BUF,NS1BUF
C
C     "OUTLIN" USES THE BUFFERS LOADED BY "AREA" WHICH CONTAIN THE
C     PERIMETER POINTS IN ORDER CLOCKWISE AROUND THE FIGURE
C
C
C     BASICALLY, OUTLIN DOES A LOT OF TESTS AND FIGURES OUT WHAT
C     THE PERIMETER OF THE CONTOUR IS DOING.   THEN IT PUTS THE
C     OUTLINE POINT(S) FOR THAT PERIMETER POINT INTO LXBUF, S1BUF,
C     AND NS1BUF
C
      K=1
C
C     MAIN LOOP
C
4     NPTS=NPTS-1
C     CLOSE CONTOUR
      LN2(NPTS+1)=LN2(1)
      SAM2(NPTS+1)=SAM2(1)
      TEST(NPTS+1)=TEST(1)
      DO 10 J=1,NPTS
      L=TEST(J)
      GO TO (11,12,13),L
C
C     DO THE STARTS OF LINE SEGMENTS
C     MOST OF THE SPECIAL CASES ARE HANDLED IN THIS SECTION
C
11    LXBUF(K)=LN2(J)
      IF(J.EQ.1)GO TO 6
      IF(J.EQ.2)GO TO 50
      IF(TEST(J-1).EQ.2)GO TO 20
      IF(TEST(J-2).EQ.2.AND.LN2(J-2).NE.LN2(J).AND.SAM2(J-2).LT.SAM2(J))
     *GO TO 41
50    S1BUF(K)=SAM2(J-1)-1
      NS1BUF(K)=SAM2(J)-SAM2(J-1)+1
      IF(J.EQ.2)GO TO 19
      IF(TEST(J-2).NE.2.OR.SAM2(J).LT.SAM2(J-2).OR.TEST(J-1).EQ.3)GO TO
     *91
      S1BUF(K)=SAM2(J-2)+1
      NS1BUF(K)=SAM2(J)-SAM2(J-2)-1
C
91    IF(J.LE.4)GO TO 19
      IF(TEST(J-4).NE.2.OR.LN2(J-4).NE.LN2(J).OR.TEST(J-2).EQ.3)GO TO 19
      S1BUF(K)=SAM2(J-4)+1
      NS1BUF(K)=SAM2(J)-SAM2(J-4)-1
19    IF(TEST(J+1).EQ.2)GO TO 16
C
C     SEE IF SLOPE IS NEGATIVE OR POSITIVE
C
      IF(SAM2(J).LE.SAM2(J+1).AND.SAM2(J).GE.SAM2(J-1))GO TO 15
      IF(SAM2(J).LT.SAM2(J-1).AND.SAM2(J).LT.SAM2(J+1))GO TO 101
      S1BUF(K)=SAM2(J+1)-1
      NS1BUF(K)=SAM2(J)-SAM2(J+1)+1
      IF(LN2(J).EQ.LN2(J+1).AND.TEST(J+1).EQ.1)LXBUF(K)=LN2(J)+1
      IF(J+4.GT.NPTS)GO TO 15
      IF(SAM2(J+1).NE.SAM2(J+3).OR.LN2(J).NE.LN2(J+4))GO TO 15
      S1BUF(K)=SAM2(J+4)+1
      NS1BUF(K)=SAM2(J)-SAM2(J+4)-1
      GO TO 15
C
C     DO FIRST POINT
C
6     S1BUF(K)=SAM2(NPTS)-1
      NS1BUF(K)=SAM2(J)-SAM2(NPTS)+1
      IF(SAM2(1).GE.SAM2(NPTS))GO TO 16
      S1BUF(K)=SAM2(J)-1
      NS1BUF(K)=1
      GO TO 16
C
16    IF(LN2(J+1).NE.LN2(J))GO TO 32
C
C     OVERLINE THE SEGMENT
C
      K=K+1
      LXBUF(K)=LN2(J)-1
      S1BUF(K)=SAM2(J)-1
      NS1BUF(K)=SAM2(J+1)-SAM2(J)+3
      GO TO 15
C
C     UNDERLINE AN INTERIOR SEGMENT
C
32    I=J
      IF(LN2(J+1).EQ.LN2(J+2))I=I+1
      S1BUF(K)=SAM2(I+2)+1
      NS1BUF(K)=SAM2(J)-SAM2(I+2)-1
      GO TO 15
C
20    IF(TEST(J+1).EQ.2.AND.LN2(J+1).EQ.LN2(J))GO TO 102
      IF(LN2(J-1).NE.LN2(J))GO TO 15
C
C     UNDERLINE THE SEGMENT
C
      LXBUF(K)=LN2(J)+1
      S1BUF(K)=SAM2(J)-1
      NS1BUF(K)=SAM2(J-1)-SAM2(J)+3
      K=K+1
      LXBUF(K)=LN2(J)
      GO TO 50
C
C     OVERLINE INTERIOR SEGMENTS
C
41    LXBUF(K)=LN2(J-2)
      S1BUF(K)=SAM2(J-2)+1
      NS1BUF(K)=SAM2(J+1)-SAM2(J-2)-1
      GO TO 15
C
C     DO THE CASE WHERE THE OUTLINE GOES LIKE THIS:  <
C
101   S1BUF(K)=SAM2(J)-1
      NS1BUF(K)=1
      GO TO 15
C
102   LXBUF(K)=LN2(J)-1
      S1BUF(K)=SAM2(J-1)+1
      NS1BUF(K)=SAM2(J+1)-SAM2(J-1)+1
C
15    IF(NS1BUF(K).LT.1)GO TO 10
      GO TO 111
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     DO ENDS
C
12    LXBUF(K)=LN2(J)
C
C     SKIP CASES ALREADY HANDLED IN FIRST SECTION
C
      IF(TEST(J+1).EQ.1.AND.LN2(J).NE.LN2(J+1))GO TO 10
      IF(TEST(J-1).EQ.1.AND.LN2(J).GT.LN2(J-1))GO TO 10
      IF(J.LE.2)GO TO 90
      IF(TEST(J-2).EQ.1.AND.TEST(J-1).NE.3.AND.SAM2(J-2).GE.SAM2(J))GO T
     *O 10
      IF(J.LE.3)GO TO 90
      IF(TEST(J-3).EQ.1.AND.TEST(J-2).NE.3.AND.SAM2(J-3).GT.SAM2(J).AND.
     *LN2(J).EQ.LN2(J-3))GO TO 10
90    S1BUF(K)=SAM2(J)+1
      NS1BUF(K)=SAM2(J+1)-SAM2(J)+1
      IF(J+5.GT.NPTS)GO TO 351
      IF(TEST(J+4).EQ.1.AND.LN2(J).EQ.LN2(J+4).AND.SAM2(J+5).GT.SAM2(J-1
     *))NS1BUF(K)=NS1BUF(K)-1
C
C     SEE IF SLOPE IS NEGATIVE OR POSITIVE
C
351   IF(SAM2(J).LE.SAM2(J+1).AND.SAM2(J).GE.SAM2(J-1))GO TO 115
      S1BUF(K)=SAM2(J)+1
      NS1BUF(K)=SAM2(J-1)-SAM2(J)+1
      IF(J.LT.5)GO TO 352
      IF(TEST(J-3).EQ.1.AND.LN2(J).EQ.LN2(J-4))NS1BUF(K)=NS1BUF(K)-1
352   IF(LN2(J).EQ.LN2(J-1).AND.TEST(J-1).EQ.2)LXBUF(K)=LN2(J)+1
C
C     ACCOUNT FOR THE CASE WHERE THE OUTLINE GOES LIKE THIS:  >
      IF(SAM2(J).GT.SAM2(J-1).AND.SAM2(J).GT.SAM2(J+1))NS1BUF(K)=1
115   IF(NS1BUF(K).LE.0)GO TO 10
      GO TO 111
C
C
C     DO SINGLE POINTS
C     IN THESE CASES THE OUTLINE WILL LOOK LIKE THIS:  V
C     OR WILL LOOK LIKE IT UPSIDE DOWN
C
13    LXBUF(K)=LN2(J)
      IF(LN2(J).GT.LN2(J+1))GO TO 70
      IF(J.EQ.1)GO TO 75
      S1BUF(K)=SAM2(J-1)-1
      NS1BUF(K)=SAM2(J)-SAM2(J-1)+1
      GO TO 76
C
75    S1BUF(K)=SAM2(NPTS)-1
      NS1BUF(K)=SAM2(J)-SAM2(NPTS)+1
      IF(SAM2(NPTS).LE.SAM2(J))GO TO 79
      GO TO 77
C
76    IF(SAM2(J-1).LE.SAM2(J))GO TO 79
77    S1BUF(K)=SAM2(J)-1
      NS1BUF(K)=1
79    K=K+1
      LXBUF(K)=LN2(J)
72    S1BUF(K)=SAM2(J)+1
      NS1BUF(K)=SAM2(J+1)-SAM2(J)+1
      IF(SAM2(J+1).LT.SAM2(J))NS1BUF(K)=1
      K=K+1
      LXBUF(K)=LN2(J)-1
      GO TO 71
C
70    S1BUF(K)=SAM2(J+1)-1
      NS1BUF(K)=SAM2(J)-SAM2(J+1)+1
      IF(SAM2(J+1).LE.SAM2(J))GO TO 92
      S1BUF(K)=SAM2(J)-1
      NS1BUF(K)=1
92    K=K+1
      LXBUF(K)=LN2(J)
      S1BUF(K)=SAM2(J)+1
      NS1BUF(K)=SAM2(J-1)-SAM2(J)+1
      IF(SAM2(J-1).GE.SAM2(J))GO TO 93
      S1BUF(K)=SAM2(J)+1
      NS1BUF(K)=1
93    K=K+1
      LXBUF(K)=LN2(J)+1
71    S1BUF(K)=SAM2(J)-1
      NS1BUF(K)=3
111   K=K+1
10    CONTINUE
      LXBUF(K+1)= 0
      S1BUF(K+1)= 0
      NS1BUF(K+1)= 0
C
C
C        MAKE SURE THE SCRIBE IS WITHIN THE PICTURE BOUNDARY
      K= 0
1001  K= K+1
      IF(LXBUF(K).EQ.0 .AND. S1BUF(K).EQ.0 .AND. NS1BUF(K).EQ.0)
     &                                                        GO TO 1020
      IF(LXBUF(K).LE.0)  LXBUF(K)= 1
      IF(LXBUF(K).GT.NL)  LXBUF(K)= NL
      IF(S1BUF(K).GT.0)  GO TO 1003
      NS1BUF(K)= NS1BUF(K)+S1BUF(K)-1
      S1BUF(K)= 1
1003  IF(S1BUF(K).LE.NS)  GO TO 1005
      S1BUF(K)= 0
      GO TO 1007
1005  IF(S1BUF(K)+NS1BUF(K).LE.NS+1)  GO TO 1007
      NS1BUF(K)= 2*NS1BUF(K)+S1BUF(K)-NS-1
1007  CONTINUE
      GO TO 1001
C
1020  CONTINUE
      RETURN
      END
C
      SUBROUTINE SPECTL(SPEC,SPLOT,NSPEC,NPLOT)
      IMPLICIT INTEGER (A-Z)
      INTEGER SPEC(12),SPLOT(50)
      REAL DEV(12,50),MEAN(12,50)
      CHARACTER*132 BUFFER
      BYTE BLANK(132),BAR,DASH
      COMMON /C1/ DEV,MEAN,FIL(4868)
C
      DATA BLANK/132*' '/
      DATA BAR/'|'/
      DATA DASH/'-'/
C
      CALL XVMESSAGE('                                                  
     +   SPECTRAL PLOT',' ')
      LINE_COUNT = 1
C					plot each requested spectral band
      DO I=1,NSPEC
	  LINE_COUNT = LINE_COUNT+NPLOT+12
	  IF (LINE_COUNT .GT. 60) THEN
	      CALL XVMESSAGE('1',' ')
	      LINE_COUNT = NPLOT+13
	  END IF
	  II = SPEC(I)
	  CALL XVMESSAGE(' ',' ')
	  CALL XVMESSAGE(' ',' ')
	  CALL XVMESSAGE(' ',' ')
	  CALL XVMESSAGE(' ',' ')
          BUFFER(1:12) = ' C     BAND '
          WRITE (BUFFER(13:14),'(I2)') II
          CALL XVMESSAGE(BUFFER(2:14),' ')
	  CALL XVMESSAGE('L            +/- 1 STANDARD DEVIATION',' ')
	  CALL XVMESSAGE('A',' ')
	  CALL XVMESSAGE('S',' ')
          buffer(1:46)='S 0   10   20   30   40   50   60   70   80   '
          buffer(47:91)='90   100  110  120  130  140  150  160  170  '
          buffer(92:131)='180  190  200  210  220  230  240  250  '
          call xvmessage(buffer,' ')
c	  CALL XVMESSAGE('S 0   10   20   30   40   50   60   70   80   90
c     +   100  110  120  130  140  150  160  170  180  190  200  210  220
c     +  230  240  250  ',' ')

          buffer(1:46)='--|----|----|----|----|----|----|----|----|---'
          buffer(47:88)='-|----|----|----|----|----|----|----|----|'
          buffer(89:131)='----|----|----|----|----|----|----|----|---'
          call xvmessage(buffer,' ')
c	  CALL XVMESSAGE('--|----|----|----|----|----|----|----|----|----|
c     +----|----|----|----|----|----|----|----|----|----|----|----|----|-
c     +---|----|----|---',' ')
C
C						plot each requested class
C
	  DO J=1,NPLOT
	      JJ = SPLOT(J)
C              CALL MVLC(BLANK,BUFFER(1:132),132)
              BUFFER = ' '
              WRITE (BUFFER(1:3),'(I3)') J
	      LEFT = (MEAN(II,JJ)-DEV(II,JJ)+9)/2.0
	      RIGHT = (MEAN(II,JJ)+DEV(II,JJ)+9)/2.0
	      LEFT = MIN(132,MAX(4,LEFT))
	      RIGHT = MIN(132,MAX(4,RIGHT))
	      DO K=LEFT+1,RIGHT-1
                  BUFFER(K:K) = '-'
	      END DO
              BUFFER(LEFT:LEFT) = '|'
              BUFFER(RIGHT:RIGHT) = '|'
	      IF (JJ.LT.10) THEN
		      CENTER = (MEAN(II,JJ)+9.0)/2.0
                      WRITE (BUFFER(CENTER-0:CENTER),'(I1)') JJ
		  ELSE
		      CENTER = (MEAN(II,JJ)+10.0)/2.0
                      WRITE (BUFFER(CENTER-1:CENTER),'(I2)') JJ
	      END IF
              CALL XVMESSAGE(BUFFER(2:132),' ')
	  END DO
          buffer(1:46)='--|----|----|----|----|----|----|----|----|---'
          buffer(47:88)='-|----|----|----|----|----|----|----|----|'
          buffer(89:131)='----|----|----|----|----|----|----|----|---'
          call xvmessage(buffer,' ')
c	  CALL XVMESSAGE('--|----|----|----|----|----|----|----|----|----|
c     +----|----|----|----|----|----|----|----|----|----|----|----|----|-
c     +---|----|----|---',' ')

          buffer(1:46)='  0   10   20   30   40   50   60   70   80   '
          buffer(47:91)='90   100  110  120  130  140  150  160  170  '
          buffer(92:131)='180  190  200  210  220  230  240  250  '
          call xvmessage(buffer,' ')
c	  CALL XVMESSAGE('   0   10   20   30   40   50   60   70   80   90
c     +   100  110  120  130  140  150  160  170  180  190  200  210  220
c     +  230  240  250  ',' ')
      END DO
      RETURN
      END
C
      SUBROUTINE HISTGM(TALLY,HISVEC,NOHIST,TTL,NEXCL,EXCLUD)
      IMPLICIT INTEGER (A-Z)
      include 'fortport'
      COMMON /IOSTFF/ IOLINE
      character*132 string
      CHARACTER*132 BUFFER
      CHARACTER*8 TTL
      INTEGER TALLY(256,12),indx
      character*244 hisbuf
      INTEGER HISVEC(12),EXCLUD(10)
      REAL XSCALE,XSHFT
      INTEGER*2 XAXIS(14)
      character*4 char
      character*1 sym
      character*1 tempbuf
      byte sbyte
      DATA MOVE/'00000041'X/
C
C
C        INITIALIZE
      PAGSIZ= 70
      YSIZ= 15
      INC= 2
      XSIZ= 121
      XLOW= 0
      XHGH= XLOW+INC*(XSIZ-1)
      XSIZ2= INC*(XSIZ-1)+1
C
      JPTCNT= PAGSIZ/(YSIZ+9)
      DSIZ= (XSIZ+8)/10+1
      XSCALE= FLOAT(XLOW-XHGH)/(XSIZ-1)
      XSHFT= FLOAT(XSIZ*XHGH-XLOW)/(XSIZ-1)
      DO 90 I=1,DSIZ
      K= DSIZ-I+1
90    XAXIS(K)= (10*I-9)*XSCALE+XSHFT+0.501
C
C
      JCNT= JPTCNT
      DO 900 JF=1,NOHIST
      JFEAT= HISVEC(JF)
      IF(JCNT.LT.JPTCNT)  GO TO 300
      call xvmessage(' ',' ')
      WRITE(BUFFER,1002) TTL
1002  FORMAT('HISTOGRAM FOR:  "',A,'"')
      CALL XVMESSAGE(BUFFER,' ')
      JCNT= 0
C
C        SCALE & PRINT THE HISTOGRAM
300   MAX= 0
      JJ1= XHGH+3
      DO 350 J=(JJ1+1),256
      TALLY(JJ1,JFEAT)= TALLY(JJ1,JFEAT)+TALLY(J,JFEAT)
350   TALLY(J,JFEAT)= 0
      IF(NEXCL.EQ.0)  GO TO 375
      DO 360 J=1,NEXCL
      JJ= EXCLUD(J)+1
360   TALLY(JJ,JFEAT)= 0
375   YSCALE= 1
      JCNT= JCNT+1
      DO 400 K=1,(XSIZ2+INC),INC
      J= XLOW+K
      JK= TALLY(J,JFEAT)+TALLY(J+1,JFEAT)
400   IF(JK.GT.MAX)  MAX= JK
      IF(MAX.GT.YSIZ)  YSCALE= (MAX+(YSIZ-1))/YSIZ
      WRITE(BUFFER,1004)  JFEAT
1004  FORMAT('CHANNEL ',I2)
      CALL XVMESSAGE(BUFFER,' ')
      WRITE(BUFFER,6004)  YSCALE
6004  FORMAT('   EACH * REPRESENTS',I3,'  POINT(S).')
      CALL XVMESSAGE(BUFFER,' ')
C
      DO 600 JY=1,YSIZ
      JH= (YSIZ-(JY-1))*YSCALE
      IK= JH-YSCALE
      DO 500 I=1,(XSIZ2+INC),INC
      HISBUF(I:i)= ' '
      IZ= XLOW+I
      JK= TALLY(IZ,JFEAT)+TALLY(IZ+1,JFEAT)
      SYM= '*'
      IF(JK.GE.JH)  GO TO 490
      IF(JK.LE.IK)  GO TO 500
      JK= JK-IK
      NUMIC= JK
C      CALL BINBCD(NUMIC,CHAR)
      write(char(4:4),'(i1)') numic
      
      sym(1:1)=char(4:4)
      IF(JK.LT.10)  GO TO 490
      JK= JK-10
      jk = jk+move
      sbyte = int2byte(jk)
      jk = jk - move
      write(sym,'(a1)') sbyte
      if(JK.LT.100)then
        write(char(1:2),'(I2)')JK
      else 
        write(char(1:3),'(I3)')JK
      end if 
         
      IF(JK.LT.26)  GO TO 490
      SYM(1:1)= '$'
490   continue
      HISBUF(I:i)= SYM(1:1)
      write(tempbuf,'(a1)') sym
500   CONTINUE
      indx = 1
      do 505 i = 1,122
         string(i:i) = hisbuf(indx:indx)
         indx = indx + 2
505   continue
C
      write(buffer,8700) jh,string(1:122)
8700  format(i4,' I',1x,a122)
      call xvmessage(buffer,' ')

c      WRITE(BUFFER,5002)  JH,(HISBUF(I),I=1,(XSIZ2+INC),INC)
c5002  FORMAT(1X,I4,' I',1X,124A1)
c      CALL XVMESSAGE(buffer,' ')
600   CONTINUE
      WRITE(BUFFER,6001)
6001  FORMAT(7X,12('+---------'),'+=>')
      CALL XVMESSAGE(BUFFER,' ')
      WRITE(BUFFER,6002)  (XAXIS(I),I=1,DSIZ)
6002  FORMAT(5X,12(I3,7X),I3)
      CALL XVMESSAGE(BUFFER,' ')
900   CONTINUE
      RETURN
      END
C********************************************************
      SUBROUTINE HSTGNB(NSAMP,PIXLIN,HIST)
      INCLUDE 'fortport'
C
      BYTE PIXLIN(*)
      INTEGER*4 HIST(256)
      DO I=1,NSAMP
        INDEX=1 + BYTE2INT(PIXLIN(I))
        HIST(INDEX)=HIST(INDEX)+1
      ENDDO
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create stats.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM stats

   To Create the build file give the command:

		$ vimake stats			(VMS)
   or
		% vimake stats			(Unix)


************************************************************************/


#define PROGRAM	stats
#define R2LIB

#define MODULE_LIST stats.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_MATH77
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define  DEBUG       /* comment out before delivery */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create stats.pdf
process help=*
PARM INP      TYPE=STRING   COUNT=(1:12)
PARM OUT      TYPE=STRING   COUNT=(1:2)
PARM SIZE     TYPE=INTEGER  COUNT=4	      DEFAULT=(1,1,0,0)
PARM MSS      TYPE=INTEGER		      DEFAULT=0
PARM BAND     TYPE=INTEGER  COUNT=(1:12)    DEFAULT=0
PARM SCRIBE   TYPE=INTEGER		      DEFAULT=0
PARM DN       TYPE=INTEGER  COUNT=(0:1)     DEFAULT=--
PARM HIST     TYPE=INTEGER  COUNT=(1:12)    DEFAULT=-1
PARM SPEC     TYPE=INTEGER  COUNT=(1:12)    DEFAULT=-1
PARM SPLOT    TYPE=INTEGER  COUNT=(1:12)    DEFAULT=-1
PARM EXCLUDE  TYPE=INTEGER  COUNT=(0:10)    DEFAULT=--
PARM NOPRINT  TYPE=KEYWORD  COUNT=(0:1) VALID="NOPRINT" DEFAULT=--
PARM VERT     TYPE=INTEGER  COUNT=(1:50)    DEFAULT=(0,0)
PARM CLASS1   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS2   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS3   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS4   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS5   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS6   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS7   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS8   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS9   TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS10  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS11  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS12  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS13  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS14  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS15  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS16  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS17  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS18  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS19  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS20  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS21  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS22  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS23  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS24  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS25  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS26  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS27  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS28  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS29  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS30  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS31  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS32  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS33  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS34  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS35  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS36  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS37  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS38  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS39  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS40  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS41  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS42  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS43  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS44  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS45  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS46  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS47  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS48  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS49  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASS50  TYPE=INTEGER  COUNT=(2:200)   DEFAULT=(0,0)
PARM CLASSx   TYPE=INTEGER                  DEFAULT=0
END-PROC
.TITLE
   Program stats
.HELP
PURPOSE:  stats computes the statistics of specified training areas on
 multi-spectral data.  The output consists of page printer output, a
 statistics data set compatible with the multispectral classifier
 FASTCLAS, and (optionally) an image containing the scribed training
 areas.  Input multispectral data must be in separate VICAR data sets or
 in MSS format.  Refer to the help for program MSS.
EXECUTION:
The following is the execution statement format for stats:
	stats INP OUT PARAMS
where INP, OUT, PARAMS are parameters discussed in their respective
parameter sections.
 The training areas for the classes may be specified either in rectangular
or vertices format.  stats no longer allows some training fields in a given
training area to be in vertices format and some to be in rectangular
format.  Rectangular format is the default and is thus not 
specified explicitly.   The RECT parameter (which was present in the IBM
version of stats), for expliciltly specifying rectangular format, is no
longer used.  The VERT parameter is used to list any and every class using the
vertices format. 
  The training area for the class is composed of one or more training fields.
Each training field is a closed region of the image.  The format must be 
the same for all fields within a class.  In rectangular format the training 
fields are defined by starting line, starting sample, number of lines,
and number of samples.
  In vertices format, only the vertices (line number and sample number) of an
irregular area need be stated.  
.page
EXAMPLES

1.    stats INP=(A, B, C, D)   OUT=ST  HIST=0 SPEC=0 EXCLUDE=0 VERT=2 +
            CLASS1=( 50,60,20,20  85,100,10,15 )                      +
            CLASS2=( 125,50  140,60  150,40  130,30  125,50 )

In this example four spectral bands are input in separate VICAR images.
Histograms and spectral plots of all four bands and both classes are
produced.  Any pixel with a DN of 0 is excluded from the statistics.
The class 1 statistics come from two rectangular training fields.  The 
class 2 statistics come from an irregular training field.
.page

2.    stats INP=MS OUT=(ST,SC) MSS=5  SPEC=0 'NOPRINT SCRIBE=2 VERT=1     +
            CLASS1=( 25,20  35,30  40,50  55,30  30,15  25,20             +
                     85,40 100,55  70,55  85,40 )                         +
            CLASS2=( 200,100,20,30 )                                      +
            CLASS3=( 150,140,5,10 )

In this example five spectral bands are input in MSS format (in one file).
Statistics are computed on all five bands.  Spectral plots of all five bands 
and all three classes are produced, but all other printer (terminal) output is
suppressed.  The second spectral band (in this case, extracted from the MSS
format input) is copied to the second output file and scribed with the training
areas.  Class 1 statistics are gathered from two irregular training fields.
Classes 2 and three both have one rectangular training field.
.page
3.  The last example is the test procedure for stats.  This is
    a complete example that could be run by the user and that 
    demonstrates uses of the possible parameters.

     gen gen1 nl=128 ns=128
     gen gen2 nl=128 ns=128 sinc=2 linc=2
     gen gen3 nl=128 ns=128 sinc=4 linc=4
     !
     !  First tests will check standard image format files
     !
     ! Copy first band and scribe training areas on this image
     stats (gen1,gen2,gen3) (stat1,stat2) 'noprint +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32)
     ! Print Histograms for the DN values in each training area
     stats (gen1,gen2,gen3) stat1 hist=0 +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32)
     ! Print Spectral Plot for all bands and classes
     stats (gen1,gen2,gen3) stat1 spec=0 +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32)
     ! Try out Vertice format
     stats (gen1,gen2,gen3) stat1 hist=0 vert=5 +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32) class5=(48,48,48,70,70,48,48,48)
     !
     !  Now check MSS format images
     !
     mss (gen1,gen2,gen3) mss (1,1,128,128)
     ! Copy second band and scribe training areas on this image
     stats mss (stat1,stat2) mss=3 scribe=2 +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32)
     ! Gather stats for first and third bands
     stats mss stat1 mss=3 band=(1,3) hist=0 'noprint +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32)
     ! Print spectral plots for second and third band for first and fourth class
     stats mss stat1 mss=3 splot=(1,4) spec=(2,3) +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32)
     ! Try out Vertice format
     stats mss (stat1,stat2) mss=3 vert=5 +
     class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
     class4=(1,96,32,32) class5=(48,48,48,70,70,48,48,48)
.page
OPERATION:  If the scribe option is requested, stats begins by copying
 the specified input picture to the secondary output data set.  Then the first
 training area parameters are the processed and the training area is
 read from all input bands.  Statistics on this area are compiled and the
 area is scribed on the scribed picture.  After compiling statistics for
 all training fields for a given class, the statistics are written on the
 output statistics data set.  (The format of the file is now type istatfile,
 based on "IBIS" formatting)  The record number on which they were written is
 the class number of that class.  The record contains the
 means, the number of pixels, and the covariance matrix for that class.  The
 record length is a function of the number of spectral bands used.  The
 maximum record length is 372 bytes which corresponds to 12 spectral bands.
 Maximum number of records is 50 since class numbers must range from 1 to
 50.  If desired, histograms of each training class in each band are
 printed.   Spectral plots of the spectral signatures
 can also be printed.  The 'EXCLUDE' option allows the user to specify
 certain DNs for which statistics are ignored.  This is useful, for
 instance, if the training areas lie at the edge of a skewed picture.  To
 prevent statistics from including pixels in the background skew,
 'EXCLUDE,0' could be specified.
      Page printer output includes the training areas for the class,
 means, and standard deviations for each band, and the covariance matrix.
 The covariance matrix is simply the covariance taken between all combina-
 tions of spectral band pairs.  The keyword NOPRINT supresses the page
 printer option.
RESTRICTIONS:  stats can handle up to 12 spectral bands in MSS format or
 in separate data set format, and compute statistics on 50 training
 classes.  The maximum size for any training field is 600 lines by 600
 samples.  The input files must have byte data format.
      Histograms and scribing must be performed in separate executions.
 If both are specified then the histograms will be deleted.
.page
WRITTEN BY: J. D. Addington               30 September 1974

CONVERTED TO VAX BY:  A. J. Runkle	  22 Dec. 1983

CURRENT COGNIZANT PROGRAMMER: Steve Pohorsky

MADE PORTABLE FOR UNIX: CRI               31 OCT 1994
CHANGED OUTPUT TO ISTAT TYPE AMS (CRI)    10 JUL 1995

30 May 2002  AXC  Modified Vicar RTL call.  (AR-106777)

TIMING:  Execution time depends on the number and size of the training
 areas and the number of spectral bands.  Typical running time is between
 1 and 2 minutes.
.LEVEL1
.VARIABLE INP
STRING - Contains the input data
set name(s)
.VARIABLE OUT
STRING - Contains output data   
set name(s)
.VARIABLE SIZE
Standard Vicar size field 
(Not used - can be defaulted).
.VARIABLE MSS
Specifies the number of spectral
bands in MSS format.
.VARIABLE BAND
Denotes that bands in MSS format
to be used as input.
.VARIABLE SCRIBE
Denotes which input band is to
be copied to output  data set.
.VARIABLE DN
Specifies the outline DN value.
.VARIABLE HIST
Denotes the bands for which
histograms will be printed.
.VARIABLE SPEC
Denotes which spectral plots are
to be printed.
.VARIABLE SPLOT
Specifies the limits  of 
spectral plots for classes.
.VARIABLE EXCLUDE
Ignore pixels with given DN
values.
.VARIABLE NOPRINT
Specifies whether training area
information is to be printed.
.VARIABLE VERT
Specifies the training
areas that are in vertex format.
.VARIABLE CLASS
Dummy argument do NOT use.
.VARIABLE CLASSx
Dummy arguement do NOT use.
.VARIABLE CLASS1
Training area coordinates.
.VARIABLE CLASS2
Training area coordinates.
.VARIABLE CLASS3
Training area coordinates.
.VARIABLE CLASS4
Training area coordinates.
.VARIABLE CLASS5
Training area coordinates.
.VARIABLE CLASS6
Training area coordinates.
.VARIABLE CLASS7
Training area coordinates.
.VARIABLE CLASS8
Training area coordinates.
.VARIABLE CLASS9
Training area coordinates.
.VARIABLE CLASS10
Training area coordinates.
.VARIABLE CLASS11
Training area coordinates.
.VARIABLE CLASS12
Training area coordinates.
.VARIABLE CLASS13
Training area coordinates.
.VARIABLE CLASS14
Training area coordinates.
.VARIABLE CLASS15
Training area coordinates.
.VARIABLE CLASS16
Training area coordinates.
.VARIABLE CLASS17
Training area coordinates.
.VARIABLE CLASS18
Training area coordinates.
.VARIABLE CLASS19
Training area coordinates.
.VARIABLE CLASS20
Training area coordinates.
.VARIABLE CLASS21
Training area coordinates.
.VARIABLE CLASS23
Training area coordinates.
.VARIABLE CLASS24
Training area coordinates.
.VARIABLE CLASS25
Training area coordinates.
.VARIABLE CLASS26
Training area coordinates.
.VARIABLE CLASS27
Training area coordinates.
.VARIABLE CLASS28
Training area coordinates.
.VARIABLE CLASS29
Training area coordinates.
.VARIABLE CLASS30
Training area coordinates.
.VARIABLE CLASS31
Training area coordinates.
.VARIABLE CLASS32
Training area coordinates.
.VARIABLE CLASS33
Training area coordinates.
.VARIABLE CLASS34
Training area coordinates.
.VARIABLE CLASS35
Training area coordinates.
.VARIABLE CLASS36
Training area coordinates.
.VARIABLE CLASS37
Training area coordinates.
.VARIABLE CLASS38
Training area coordinates.
.VARIABLE CLASS39
Training area coordinates.
.VARIABLE CLASS40
Training area coordinates.
.VARIABLE CLASS41
Training area coordinates.
.VARIABLE CLASS42
Training area coordinates.
.VARIABLE CLASS43
Training area coordinates.
.VARIABLE CLASS44
Training area coordinates.
.VARIABLE CLASS45
Training area coordinates.
.VARIABLE CLASS46
Training area coordinates.
.VARIABLE CLASS47
Training area coordinates.
.VARIABLE CLASS48
Training area coordinates.
.VARIABLE CLASS49
Training area coordinates.
.VARIABLE CLASS50
Training area coordinates.
.LEVEL2
.VARIABLE INP
STRING - Input data sets used by stats.  If input data is in MSS format
then only one data set is required.  Otherwise there must be one data set
per spectral band examined.
.VARIABLE OUT
STRING - Output data sets used by STATS.  First data set will contain the
statistics output.  The second output data set (optional) will contain
the input picture with training areas scribed.  (If scribing is requested
and their is only one output file, the output file is assumed to be the
scribed image, and no statistics output file is generated.)

The statistics output file is a classification statistics file (statistics
data set).  Each record (excluding labels) in a classification statistics file
contains statistics for a particular class of pixels.  The statistics are
derived from the multispectral (multi-channel) data.  The size of records in
the file is based on the number of channels (bands).  If 'N' is used to denote
the number of channels, then each record contains the following data in the
order shown. Now in the new ISTATFILE format.

   *  1 column with class name in character (ASCII) format.  This
      takes the form 'CLASSnnn' where nnn is the class number.

   *  1 column, stored in INTEGER*4 format, containing the number of pixels 
      in this class.

   *  1 column, stored in INTEGER*4 format, containing the number of bands 
      in this class.

   *  N words for mean values for this class for each of the N channels.
      Each mean value is stored in REAL*4 format.

   *  N*(N+1)/2 words containing the variances and covariances for the N
      channels for this class.  Each variance and covariance is stored in
      REAL*4 format.

The VICAR system label contains the number of records in the number of lines
field and the number of bytes per record in the number of samples field.
.VARIABLE SIZE
INTEGER - Standard Vicar size field (Not used - can be defaulted).
.VARIABLE MSS
INTEGER - Denotes that input data is in MSS format and contains the
supplied number of spectral bands.
.VARIABLE BAND
INTEGER - Specifies which MSS format bands are to be used as input (The
default is for all bands specified by the MSS parameter).
Syntax is:   BAND = (b1,...)   where b1,... is a list of bands.
.VARIABLE DN
INTEGER - Denotes that the outline will be the given DN in scribing the
training areas.  The default is to use 0 or 255 depending on which contrasts
better with the image.
.VARIABLE SCRIBE - Denotes which input band is going to be copied with
the training classes scribed on the image.
.VARIABLE HIST
INTEGER - Denotes that histograms of the training classes from the
specified bands are to be printed on the line printer.  If the value of
0 is used, histograms for all bands are printed.
Syntax is:   HIST = (b1,...)   where b1,... is a list of bands.

The histogram output uses vertical bars to show the number of pixels in the
DN bins.  The bars are made of a column of  * symbols with a number (or other
symbol) on top.  The number of pixels represented by each * is shown at the
top of the histogram.  The number on top of a column is an additional number of
pixels in the bin beyond those represented by *s.  The letters A through Z are
used to represent the values 10 through 35, respectively.  The $ symbol
represents a value greater than 35. 
.VARIABLE SPEC
INTEGER - Denotes that a spectral plot of training classes from the given
bands is to be printed.  If the value of 0 is used, all the bands are 
included.
Syntax is:   SPEC = (b1,...)   where b1,... is a list of bands.
.VARIABLE SPLOT
INTEGER - Limits spectral plots to the specified classes (default is that
all classes are included).
Syntax is:   SPLOT = (c1,...)   where c1,... is a list of classes.
.VARIABLE EXCLUDE
INTEGER - Ignore any pixel which has the same DN value as one specified.
There is a maximum of 10 DN values that can be specified.
Syntax is:   EXCLUDE = (d1,...)   where d1,... is a list of DNs.
.VARIABLE NOPRINT
KEYWORD - Controls the printing of means, standard deviations, covariance
matrices, and training area coordinates on the line printer (default is
to PRINT this information).
.VARIABLE VERT
INTEGER - Specifies which  training areas defined by the CLASSx keyword
are to be read in the vertices format.
 The training areas for the classes may be specified either in rectangular
or vertices format.  stats no longer allows some training fields in a given
training area to be in vertices format and some to be in rectangular
format.  Rectangular format is the default and is thus not 
specified explicitly.   The RECT parameter (which was present in the IBM
version of stats), for expliciltly specifying rectangular format, is no
longer used.  The VERT parameter is used to list any and every class using the
vertices format. 
.VARIABLE CLASS
Actual keyword is of the form CLASSx where x is a number from 1 to 50.
The number defines which class number this training set belongs.
e.g.  CLASS69 SL,SS,NL,NS    or    CLASS32  L1,S1,L2,S2,...
See CLASSx for a more complete definition.
.VARIABLE CLASSx
INTEGER - Denotes the training area information in either rectangular or
vertices formats (default is rectangular).  Rectangular coordinates are
of the form: SL,SS,NL,NS   SL,SS,NL,NS ...  (the standard Vicar size field) 
while vertices format is of the form: L1,S1,L2,S2,... etc.  The range of values
for 'x' is from 1 to 50 such that the valid keywords would be of the form:
CLASS1, ... CLASS50. 
  The training area for the class is composed of one or more training fields.
Each training field is a closed region of the image.  The format must be 
the same for all fields within a class.  In rectangular format the training 
fields are defined by starting line, starting sample, number of lines,
and number of samples.
  In vertices format, only the vertices (line number and sample number) of an
irregular area need be stated.  The program interpolates between each vertex to
determine the perimeter of the training field.  Several rules must be adhered
to in using the vertices format:

1)  The first vertex entered must be the topmost point of the area; that is, 
    the lowest line number.
2)  The vertices must follw in a clockwise order. 
3)  The last vertex must be the same as the first; that is, close the area
    for each training field.

  The maximum size for any training field, whether rectangular or vertices,
is 600 lines by 600 samples.
.VARIABLE CLASS1
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS2
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS3
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS4
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS5
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS6
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS7
CINTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS8
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS9
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS10
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS11
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS12
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS13
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS14
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS15
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS16
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS17
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS18
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS19
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS20
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS21
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS22
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS23
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS24
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS25
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS26
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS27
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS28
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS29
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS30
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS31
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS32
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS33
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS34
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS35
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS36
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS37
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS38
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS39
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS40
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS41
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS42
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS43
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS44
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS45
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS46
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS47
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS48
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS49
INTEGER - See CLASSx for a proper definition.
.VARIABLE CLASS50
INTEGER - See CLASSx for a proper definition.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tststats.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!  Test script for Vicar routine STATS
!
gen stats.gn1 nl=128 ns=128
gen stats.gn2 nl=128 ns=128 sinc=2 linc=2
gen stats.gn3 nl=128 ns=128 sinc=4 linc=4
!
!  First tests will check standard image format files
!
! Copy first band and scribe training areas on this image
stats (stats.gn1,stats.gn2,stats.gn3) (stats.st1,stats.st2) 'noprint +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32)
! Print Histograms for the DN values in each training area
stats (stats.gn1,stats.gn2,stats.gn3) stats.st1 hist=0 +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32)
! Print Spectral Plot for all bands and classes
stats (stats.gn1,stats.gn2,stats.gn3) stats.st1 spec=0 +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32)
! Try out Vertice format
stats (stats.gn1,stats.gn2,stats.gn3) stats.st1 hist=0 vert=5 +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32) class5=(48,48,48,70,70,48,48,48)
!
!  Now check MSS format images
!
mss (stats.gn1,stats.gn2,stats.gn3) stats.mss (1,1,128,128)
! Copy second band and scribe training areas on this image
stats stats.mss (stats.st1,stats.st2) mss=3 scribe=2 +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32)
! Gather stats for first and third bands
stats stats.mss stats.st1 mss=3 band=(1,3) hist=0 'noprint +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32)
! Print spectral plots for second and third band for first and forth class
stats stats.mss stats.st1 mss=3 splot=(1,4) spec=(2,3) +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32)
! Try out Vertice format
stats stats.mss (stats.st1,stats.st2) mss=3 vert=5 +
class1=(1,1,32,32) class2=(96,1,32,32) class3=(96,96,32,32) +
class4=(1,96,32,32) class5=(48,48,48,70,70,48,48,48)
!
end-proc
$ Return
$!#############################################################################
