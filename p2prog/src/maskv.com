$!****************************************************************************
$!
$! Build proc for MIPL module maskv
$! VPACK Version 1.9, Wednesday, August 30, 2000, 16:31:04
$!
$! Execute by entering:		$ @maskv
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
$ write sys$output "*** module maskv ***"
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
$ write sys$output "Invalid argument given to maskv.com file -- ", primary
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
$   if F$SEARCH("maskv.imake") .nes. ""
$   then
$      vimake maskv
$      purge maskv.bld
$   else
$      if F$SEARCH("maskv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake maskv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @maskv.bld "STD"
$   else
$      @maskv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create maskv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack maskv.com -
	-s maskv.f flt_lab_maskv.c -
	-i maskv.imake -
	-p maskv.pdf -
	-t tstmaskv.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create maskv.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM MASKV:
C    MASKV  INP  OUT  PARS
C or MASKV  (INP,H1,H2,H3,...) OUT  PARS
C
      SUBROUTINE MAIN44
      IMPLICIT INTEGER(A-Z)
      EXTERNAL MAIN
      COMMON /C1/ SL,SS,NLO,NBO,NLI,NBI,NI,BYTPIX,STRTCH,INMIN,INMAX,
     &            COMP,BEFOR,LFACT,NSW,MARGIN,SLPIC,SSPIC,ELPIC,ESPIC,
     &            NLR,NLRO,ILINE,SLTIC,TICBUF,LUT
      COMMON /C2/ NOLAB,LABG,LABH,LABP,LABU,LABA,LABE,LABD,
     &            ILOGO,LOGO1,LOGO2,SQUEZ,NOID,LAB
      COMMON /C3/ HIST,HSIZE,HWIDTH,NROW,NCOL,NSPIKE,TICS,TICL,LOGFLG,
     &            PHIST,NIHIST,PHLAB
      COMMON /C4/ TASKS,INSTANCES,NLABS,TYPE,INUNIT,OUTUNIT,CNT,FORMAT
      COMMON /C6/ HISTNO,GLL
      COMMON /INIT/ SLLAB,ELLAB,SSLAB,NUM,NDIGIT,CEN,SLNUM,ELNUM,ITIC,
     &              IDIGIT,NSLICE,ICHAR,LSLICE

      BYTE TICBUF(100,10),LUT(256)
      CHARACTER*7200 LAB
      CHARACTER*72 PHLAB
      CHARACTER*49 MSG2
      CHARACTER*22 MSG3
      CHARACTER*32 TYPE
      CHARACTER*8 TASKS(100)
      CHARACTER*7 FORMAT,MISSION
      CHARACTER*5 IBM_LABEL
      CHARACTER*3 SENSOR
      INTEGER*4 INSTANCES(100),NLABS,STATUS,INUNIT,OUTUNIT,CNT,GLL

      CALL IFMESSAGE('MASKV version 29-Aug-2000')
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA',' ') 
      CALL GETPAR		!Process parameters
C
C     ...Generate tic mark pattern
      SIZE=MAX0(NLO,NBO)
      CALL TICGEN(TICBUF,SIZE)
C
C     ...Calculate margin
      MARGIN=0
      MIN=500*LFACT

      RWIDTH=45		!Number of samples for gray wedge and histograms
      IF(HIST.GT.0) RWIDTH=NCOL*(HWIDTH+24*LFACT)
      NSW=5+6*LFACT+10+1+NBO+1+10+6*LFACT+5+RWIDTH+4
      IF(NSW.LT.MIN) MARGIN=(MIN-NSW)/2
      IF(NSW.LT.MIN) NSW=MIN
C       *** MDA CAN ONLY HANDLE THE SAMPLES IN MULTIPLE OF 4 ***
      INSW=MOD(NSW,4)
      NSW=NSW+4-INSW
C
C     ....Determine (starting line,starting sample) and
C     ....(ending line, ending sample) coordinates of input image in output
C     ....display.
      SLPIC=5+7*LFACT+10+1+1
      SSPIC=MARGIN+5+6*LFACT+10+1+1
      ELPIC=SLPIC+NLO-1
      ESPIC=SSPIC+NBO-1
C     ....Open output file with 200 more lines than input to add room for
C     ....tick marks and labels.  VICAR will automatically allocate more
C     ....if needed.
      NLEXTRA = 200
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','OPEN_ACT','SA',
     +'IO_ACT','SA','U_NL',NLO+NLEXTRA,'U_NS',NSW,'O_FORMAT','BYTE',
     +'U_FORMAT','BYTE',' ')

C     ....Get the task names and instances for output history label
      CNT=100
      CALL XLHINFO(INUNIT,TASKS,INSTANCES,CNT,STATUS,' ')

C     ....Determine if image is a GALILEO SSI image
       CALL XLGET(INUNIT,'HISTORY','MISSION',MISSION,STATUS,'HIST',
     +TASKS(1),'INSTANCE',INSTANCES(1),'FORMAT','STRING',' ')
       CALL XLGET(INUNIT,'HISTORY','SENSOR',SENSOR,STATUS,'HIST',
     +TASKS(1),'INSTANCE',INSTANCES(1),'FORMAT','STRING',' ') 
      GLL = 0
      IF (MISSION .EQ. 'GALILEO' .AND. SENSOR .EQ. 'SSI')  GLL = 1    

C     ....Process VICAR1 labels
      CALL XLGET(INUNIT,'HISTORY','NLABS',NLABS,STATUS,
     &    'HIST',TASKS(1),'INSTANCE',INSTANCES(1),'FORMAT','INT',' ')
      IF (STATUS .EQ.-38 .OR. NLABS.EQ.0) GO TO 111
      NLRO=NLABS/5+1
      J=1
      IBM_LABEL='LAB01'
C     ....Move VICAR1 label into LAB
      DO I=1,NLABS
         CALL XLGET(INUNIT,'HISTORY',IBM_LABEL,LAB(J:J+71),STATUS,
     +   'HIST',TASKS(1),'INSTANCE',INSTANCES(1),'FORMAT','STRING',' ')
         J=J+72
         CALL KEYINCC(IBM_LABEL)
      END DO
C
C     ....If output is DISK (not tape), update NL, NS in system label
111   CALL XVTPMODE(OUTUNIT,MODE)
      IF (MODE .NE. 0) THEN
           CALL XVCLOSE(OUTUNIT,STATUS,' ')
           CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','OPEN_ACT','SA',
     +     'IO_ACT','SA','U_NL',NLO+800,'U_NS',NSW,'O_FORMAT',
     +     'BYTE','U_FORMAT','BYTE','COND','NOLABELS NOBLOCK',' ')
      END IF 
C
C     ....Call MAIN routine via STACKA
C     ....(INBUF only needed for halfword input)
      LIN=2
      IF(BYTPIX.EQ.2) LIN=2*NBO
      LHIS=4*HWIDTH*(NIHIST+PHIST)+4
      LOUT=NSW
      LCHECK=NSW
      CALL STACKA(7,MAIN,3,LIN,LHIS,LOUT,LCHECK,IND)
      IF(IND.NE.0) GO TO 990
C
C     ....Print output picture size and stretch
      OLINE=ILINE-1
      WRITE (MSG2,9900) OLINE,NSW
9900  FORMAT ('*****  OUTPUT:  ',I4,' LINES - ',I4,' SAMPLES  *****')
      CALL XVMESSAGE(MSG2,' ')
      IF(STRTCH.EQ.0) GO TO 90
      WRITE (MSG3,9910) INMIN,INMAX
9910  FORMAT ('STRETCH',I6,' -',I6)
      CALL XVMESSAGE(MSG3,' ')
C
C        UPDATE SYSTEM LABEL (DISK OUTPUT)
   90 IF(MODE.NE.0) GO TO 900
      CALL XLDEL(OUTUNIT,'SYSTEM','NL',STATUS,' ')
      CALL XLADD(OUTUNIT,'SYSTEM','NL',OLINE,STATUS,'FORMAT','INT',' ')
      IF (BYTPIX .EQ. 2) THEN
         CALL XLDEL(OUTUNIT,'SYSTEM','FORMAT',STATUS,' ')
         CALL XLADD(OUTUNIT,'SYSTEM','FORMAT','BYTE',STATUS,'FORMAT',
     &   'STRING',' ')
      END IF
C
  900 CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN
C
C        ERROR RETURNS
  990 GO TO (991,992,993),IND
      CALL ABEND
  991 CALL XVMESSAGE('*** INSUFFICIENT CORE ***',' ')
      CALL ABEND
  992 CALL XVMESSAGE('*** READ ERROR ***',' ')
      CALL ABEND
  993 CALL XVMESSAGE('*** ERROR IN HMASK ***',' ')
      CALL ABEND
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Parameter processor
C
      SUBROUTINE GETPAR
      IMPLICIT INTEGER(A-Z)
      COMMON /C1/ SL,SS,NLO,NBO,NLI,NBI,NI,BYTPIX,STRTCH,INMIN,INMAX,
     &            COMP,BEFOR,LFACT,NSW,MARGIN,SLPIC,SSPIC,ELPIC,ESPIC,
     &            NLR,NLRO,ILINE,SLTIC,TICBUF,LUT
      COMMON /C2/ NOLAB,LABG,LABH,LABP,LABU,LABA,LABE,LABD,
     &            ILOGO,LOGO1,LOGO2,SQUEZ,NOID,LAB
      COMMON /C3/ HIST,HSIZE,HWIDTH,NROW,NCOL,NSPIKE,TICS,TICL,LOGFLG,
     &            PHIST,NIHIST,PHLAB
      COMMON /C4/ TASKS,INSTANCES,NLABS,TYPE,INUNIT,OUTUNIT,CNT,FORMAT
      COMMON /C6/ HISTNO,GLL
      COMMON /C7/ HISTORY,TASK,USERS

      CHARACTER*32 INP(10),TYPE
      CHARACTER*8 TASKS(100)
      CHARACTER*7 FORMAT
      INTEGER*4 STRE(2),NLABS,INSTANCES(100),INUNIT,OUTUNIT,CNT
      INTEGER*4 DEF,NOHIST
      BYTE TICBUF(100,10),LUT(256)
      CHARACTER*7200 LAB
      CHARACTER*72 PHLAB
      REAL*4    A,B
C
      PHLAB=' '
C     ....Determine input image data format
      CALL XVGET(INUNIT,STAT,'PIX_SIZE',BYTPIX,'FORMAT',FORMAT,'TYPE',
     +           TYPE,' ')
      IF (XVPTST('HALF')) BYTPIX=2
      IF(BYTPIX.GT.2) CALL MABEND('WRONG DATA FORMAT')
      IF (BYTPIX .EQ. 2) STRTCH=1 

      CALL XVSIZE(SL,SS,NLO,NBO,NLI,NBI)
      IF(NBI.LT.SS+NBO-1) NBO=NBI-SS+1
      IF(NLI.LT.SL+NLO-1) NLO=NLI-SL+1
      CALL XVPARM('INP',INP,NI,DEF,10)	!Get number of inputs NI

C     ....Stretch parameters
      CALL XVPARM('STRETCH',STRE,COUNT,DEF,2)
      IF (DEF .EQ. 0) THEN
         STRTCH=1
         INMIN=STRE(1)
         INMAX=STRE(2)
      ELSE
         INMIN=0
         IF (BYTPIX .EQ. 2) THEN
             INMAX=32767
         ELSE
             INMAX=255 
         END IF
      END IF

      COMP = XVPTST('COMP')		!Complement image
      HIST = XVPTST('HIST')		!Display histograms

      LOGFLG=0
      IF (XVPTST('LOG')) THEN
          LOGFLG=1		!Use log scale for histograms
          HIST=1
      END IF

      IF (HIST.EQ.1) THEN
         PHIST=1
         IF (XVPTST('NOPHIST')) PHIST=0
      END IF
      BEFOR = XVPTST('BEFORE')		!Obtain output hist before COMP

      CALL XVPARM('SPIKES',NSPIKE,COUNT,DEF,1)	!# of histogram spikes
      CALL XVPARM('HSIZE',HSIZE,COUNT,DEF,1)	!Height of histogram in lines
      CALL XVPARM('HWIDTH',HWIDTH,COUNT,DEF,1)	!Width of histogram in samples
      CALL XVPARM('NROW',NROW,COUNT,DEF,1)	!# of rows of histograms
      IF (DEF .EQ. 1) NROW=NI
      CALL XVPARM('NCOL',NCOL,COUNT,DEF,1)	!# of columns of histograms
      CALL XVPARM('TICS',TICS,COUNT,DEF,1)	!Spacing of short hist tic marks
      CALL XVPARM('TICL',TICL,COUNT,DEF,1)	!Spacing of long hist tic marks
      CALL XVPARM('HTITLE',PHLAB,COUNT,DEF,1)	!Histogram title

C     ....Determine histogram location for color image
      HISTNO=0
      IF (XVPTST('RED')) HISTNO=1
      IF (XVPTST('GREEN')) HISTNO=2
      IF (XVPTST('BLUE')) HISTNO=3

C     ....Default values for label display
      NOLAB=0		!Display labels
      LABH=1		!Display history labels
      LABU=1		!Display U labels
      LABE=1		!Display E labels
      LABG=0		!Suppress G labels
      LABP=0		!Supress P labels
      LABA=0		!Supress A labels
      LABD=0		!Suppress D labels

      IF (XVPTST('NOLA')) NOLAB=1	!Suppress display of all VICAR labels
      IF (XVPTST('NOHL')) LABH=0	!Suppress history labels
      IF (XVPTST('NOUL')) LABU=0	!Suppress U labels
      IF (XVPTST('NOEL')) LABE=0	!Suppress E labels
      IF (XVPTST('LABG')) LABG=1	!Display G labels
      IF (XVPTST('LABP')) LABP=1	!Display P labels
      IF (XVPTST('LABA')) LABA=1	!Display A labels
      IF (XVPTST('LABD')) LABD=1	!Display D labels

      TASK=1		!Display VICAR2 task names
      IF (XVPTST('NOTASK')) TASK=0	!Suppress VICAR2 task names
C     ....Supress system labels except task, user, date-time      
      HISTORY=1		!Display system labels
      IF (XVPTST('NOHISTOR')) HISTORY=0

      USER = XVPTST('USER')		!Display user & date/time
      SQUEZ = XVPTST('SQUEEZE')		!Squeeze labels together

      CALL XVPARM('EXPAND',LFACT,COUNT,DEF,1)
      IF(LFACT.GT.10) LFACT=10		!Text size scale factor

C     ....JPL logo
      ILOGO=0		!Default is no logo
      IF  (XVPTST('LOGO')) THEN
          LOGO1=1	!Display logo on left
          LOGO2=1	!and right
          ILOGO=1
      ELSE
          CALL XVPARM('LLOGO',LOGO1,COUNT,DEF,1)
          IF (DEF .EQ. 0) THEN
              ILOGO=1		!Display logo on left
          ELSE
              LOGO1=0
          END IF
          CALL XVPARM('RLOGO',LOGO2,COUNT,DEF,1)
          IF (DEF .EQ. 0) THEN
              ILOGO=1		!Display logo on right
          ELSE
              LOGO2=0
          END IF
      END IF

      NOID=0		!Default is to display JPL ID
      IF (XVPTST('NOID')) NOID=1	!No JPL ID

      IF(HIST.EQ.0) GO TO 500
      NIHIST=NI-1
      NOHIST=NIHIST+PHIST
      IF(NROW.GT.NOHIST) NROW=NOHIST
      IF(NCOL.GT.NOHIST) NCOL=NOHIST
      IF (HISTNO.NE.0) THEN
        IF (NI.GT.1) THEN
            CALL XVMESSAGE('***SECONDARY INPUTS NOT ALLOWED',' ')
            CALL XVMESSAGE('WHEN RED, GREEN OR BLUE IS SPECIFIED',' ')
            CALL ABEND
        ENDIF
        NROW=3
        NCOL=1
      ELSE
        IF (NCOL.GT.NOHIST) NCOL=NOHIST
        IF (NCOL.GT.1) THEN
           NROW=(NOHIST-1)/NCOL + 1
        ELSE
           NCOL=(NOHIST-1)/NROW + 1
        ENDIF
      ENDIF

  120 NLHIST=NROW*(HSIZE+45*LFACT)
      IF(30+NLHIST.LE.NLO) GO TO 500
      HSIZE=HSIZE-10
      CALL XVMESSAGE('HSIZE REDUCED BY 10 LINES',' ')
      IF(HSIZE.LT.10) CALL MABEND('HSIZE TOO SMALL')
      GO TO 120

C     ....Generate lookup table for byte input
500   IF(BYTPIX.EQ.2) GO TO 900
      ISPAN=INMAX-INMIN
      IF(ISPAN.EQ.0) ISPAN=1
      A=255./ISPAN
      B=-A*INMIN
      DO 505 J=1,256
      IDN=A*(J-1)+B
      IDN=MIN0(255,MAX0(0,IDN))
      CALL ITLA(IDN,LUT(J),1)
  505 CONTINUE
C
900   RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Generates 1 cycle of tic marks
C
      SUBROUTINE TICGEN(TICBUF,SIZE)
      IMPLICIT INTEGER(A-Z)
      BYTE TICBUF(100,10),BLK
      DATA BLK/255/

      CALL ZIA(TICBUF,250)
C        PUT BOX ON EVERY 100TH TIC
      DO 10 I=2,4
         TICBUF(99,I)=BLK
         TICBUF(1,I)=BLK
   10 CONTINUE
C
C        TIC MARK AT 50 AND 100
      DO 30 I=2,10
         TICBUF(100,I)=BLK
         TICBUF(50,I)=BLK
   30 CONTINUE
C
C        TIC MARK EVERY 10
      DO 100 INT=10,40,10
      DO 90 I=4,8
         TICBUF(INT,I)=BLK
         TICBUF(INT+50,I)=BLK
   90 CONTINUE
  100 CONTINUE
C
      IF(SIZE.GT.2000) RETURN
C
C        TIC MARK EVERY 2
      DO 200 INT=2,92,10
         DO 140 I=7,8
            TICBUF(INT,I)=BLK
            TICBUF(INT+6,I)=BLK
  140    CONTINUE
         DO 150 I=6,8
            TICBUF(INT+2,I)=BLK
            TICBUF(INT+4,I)=BLK
  150    CONTINUE
  200 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE MAIN(INBUF,LIN,HISBUF,LHIS,OUTBUF,LOUT,LCHECK,IND)
      IMPLICIT INTEGER(A-Z)
      INCLUDE 'fortport'
      COMMON /C1/ SL,SS,NLO,NBO,NLI,NBI,NI,BYTPIX,STRTCH,INMIN,INMAX,
     &            COMP,BEFOR,LFACT,NSW,MARGIN,SLPIC,SSPIC,ELPIC,ESPIC,
     &            NLR,NLRO,ILINE,SLTIC,TICBUF,LUT
      COMMON /C2/ NOLAB,LABG,LABH,LABP,LABU,LABA,LABE,LABD,
     &            ILOGO,LOGO1,LOGO2,SQUEZ,NOID,LAB
      COMMON /C3/ HIST,HSIZE,HWIDTH,NROW,NCOL,NSPIKE,TICS,TICL,LOGFLG,
     &            PHIST,NIHIST,PHLAB
      COMMON /C4/ TASKS,INSTANCES,NLABS,TYPE,INUNIT,OUTUNIT,CNT,FORMAT
      COMMON /C5/ NTIME,REDUCE,NSCHAR
      COMMON /C6/ HISTNO,GLL
      COMMON /INIT/ SLLAB,ELLAB,SSLAB,NUM,NDIGIT,CEN,SLNUM,ELNUM,ITIC,
     &              IDIGIT,NSLICE,ICHAR,LSLICE
      COMMON /C8/ REPEAT,INUM
      CHARACTER*6 INUM
      CHARACTER*32 TYPE
      CHARACTER*8 TASKS(100)
      CHARACTER*7 FORMAT
      INTEGER*4 INSTANCES(100),NLABS,STATUS,INUNIT,OUTUNIT,CNT
      REAL*4 GAIN,OFFSET
      INTEGER*4 PHBUF(259)
      INTEGER*2 INBUF(LIN)
      BYTE TICBUF(100,10),LUT(256)
      CHARACTER*7200 LAB
      CHARACTER*72 PHLAB,PICBUF
      BYTE OUTBUF(LOUT),HISBUF(LHIS)
C
      IND=0
      IF(LOUT.LT.LCHECK) GO TO 991
C
C     ....Get histogram of output image and format histograms
      IF(HIST.EQ.0) GO TO 5
      IF(PHIST.GT.0) CALL GETHIS(INBUF,OUTBUF,PHBUF,IND)
      IF(IND.NE.0) GO TO 992
      SSHIST=ESPIC+1+10+6*LFACT+5+1
      SLHIST=SLPIC+31
      IF (HISTNO .EQ. 2) SLHIST=SLHIST+HSIZE+45
      IF (HISTNO .EQ. 3) SLHIST=SLHIST+2*HSIZE+90
      NREPET=0
      HSTIND=0
      CALL HMASK(HISBUF,LHIS,NIHIST,LFACT,NCOL,HWIDTH,
     &     TICS,TICL,NSPIKE,PHBUF,PHLAB,PHIST,HSIZE,LOGFLG)
C
C     ....Determine location of gray wedge
    5 SSWEDG=ESPIC+1+10+6*LFACT+5+1
      IF(HIST.GT.0) SSWEDG=SSHIST+6*LFACT
      NSWEDG=28
      IF(HIST.GT.0) NSWEDG=(NCOL*HWIDTH+(NCOL-1)*24*LFACT)/16*16+2
      SLWEDG=SLPIC
      NLGLEV=(NLO-2)/16
      ELWEDG=SLWEDG+16*NLGLEV+1
      IF(HIST.GT.0) ELWEDG=SLWEDG+25
C     ....For small image (NLO<18) insure program does not bomb in gwedge
      IF(NLGLEV.EQ.0) NLGLEV=1
C
C     ....Write upper border, sample labels, and tic marks
      ILINE=1
      SLTIC=7*LFACT+6
      CALL ITLA(0,OUTBUF,NSW)
      CALL WLINE(ILINE,NSW,OUTBUF,5)
      CALL SAMLAB(OUTBUF,NSW,ILINE,SSPIC,ESPIC,LFACT,NBO)
      CALL SAMTIC(1,OUTBUF)
C
C     ....Process image
      EL=SL+NLO-1
      ESBORD=SSPIC-1
      ESTIC=SSPIC-2
      IF(BYTPIX.EQ.2) GO TO 120
C
C     ....Here if byte image  
      IF (COMP.EQ.1) THEN
         DO J=1,256
            IDN=255-BYTE2INT(LUT(J))
            CALL ITLA(IDN,LUT(J),1)	!Create look-up table for COMP
         ENDDO
      ENDIF
C
C
      DO 100 LINE=SL,EL
      CALL XVREAD(INUNIT,OUTBUF(SSPIC),STATUS,'LINE',LINE,'SAMP',SS,
     +            'NSAMPS',NBO,' ')
      IF (STATUS .NE. 1) GO TO 992
      CALL TBL(OUTBUF(SSPIC),LUT,NBO)
C     ....Tic marks
      CALL ITLA(0,OUTBUF,ESTIC)
      CALL LINTIC(OUTBUF)
C     ....Gray wedge
      CALL GWEDGE(OUTBUF,HIST,ILINE,SSWEDG,NSWEDG,SLWEDG,ELWEDG,NLGLEV)
C     ....Histograms
      IF(HIST.EQ.0) GO TO 80
      IF(ILINE.LT.SLHIST) GO TO 80
      IF(NREPET.GT.0) GO TO 60
      IF(HSTIND.EQ.1) GO TO 80
      CALL HLINE(OUTBUF(SSHIST),NREPET,HSTIND,HISBUF)
      IF(HSTIND.EQ.-1) GO TO 993
   60 NREPET=NREPET-1
   80 CALL WLINE(ILINE,NSW,OUTBUF,1)
  100 CONTINUE
      GO TO 300
C
C     ....Here if halfword image
120   GAIN=255.0/(INMAX-INMIN)
      OFFSET=-255.0*INMIN/(INMAX-INMIN)
      IF(COMP.EQ.1) GAIN=-GAIN
      IF(COMP.EQ.1) OFFSET=255-OFFSET
C
      DO 200 LINE=SL,EL
      CALL XVREAD(INUNIT,INBUF,STATUS,'LINE',LINE,'SAMP',SS,
     +'NSAMPS',NBO,' ')
      IF(STATUS .NE. 1) GO TO 992
      DO 125 SAMP=1,NBO
      DN=GAIN*INBUF(SAMP)+OFFSET+.5
      IF(DN.LT.0) DN=0
      IF(DN.GT.255) DN=255
      CALL ITLA(DN,OUTBUF(ESBORD+SAMP),1)
  125 CONTINUE
C     ....Tic marks
      CALL ITLA(0,OUTBUF,SSPIC-2)
      CALL LINTIC(OUTBUF)
C     ....Gray wedge
      CALL GWEDGE(OUTBUF,HIST,ILINE,SSWEDG,NSWEDG,SLWEDG,ELWEDG,NLGLEV)
C     ....Histograms
      IF(HIST.EQ.0) GO TO 180
      IF(ILINE.LT.SLHIST) GO TO 180
      IF(NREPET.GT.0) GO TO 160
      IF(HSTIND.EQ.1) GO TO 180
      CALL HLINE(OUTBUF(SSHIST),NREPET,HSTIND,HISBUF)
      IF(HSTIND.EQ.-1) GO TO 993
  160 NREPET=NREPET-1
  180 CALL WLINE(ILINE,NSW,OUTBUF,1)
  200 CONTINUE
C
C     ....Write lower tic marks and sample labels
300   CALL SAMTIC(0,OUTBUF)
      CALL SAMLAB(OUTBUF,NSW,ILINE,SSPIC,ESPIC,LFACT,NBO)
C     ....Write VICAR2 system labels
      CALL SYSLABEL(OUTBUF)
C
C     ....Write VICAR1 labels
      CALL LABELS(OUTBUF)
C
C     ....Write VICAR2 labels
      CALL NEWLABEL(OUTBUF)
C     ....Add space between history labels and ipl labels in case
C     ....'NOTASK is specified
      CALL WLINE(ILINE,NSW,OUTBUF,NTIME*2)
C     ....Write ipllab and logos
      IF (NOID.EQ.0) THEN
         CALL IPLLAB(LFACT,ILOGO,LOGO1,LOGO2,ILINE,NSW,OUTBUF,PICBUF)
         CALL XVMESSAGE(PICBUF,' ')
      ENDIF
      RETURN
C
C        INSUFFICIENT CORE RETURN
  991 IND=1
      RETURN
C        READ ERROR RETURN
  992 IND=2
      RETURN
C        HMASK ERROR RETURN
  993 IND=3
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute histogram of the output image
C
      SUBROUTINE GETHIS(INBUF,OUTBUF,PHBUF,IND)
      IMPLICIT INTEGER(A-Z)
      COMMON /C1/ SL,SS,NLO,NBO,NLI,NBI,NI,BYTPIX,STRTCH,INMIN,INMAX,
     &            COMP,BEFOR,LFACT,NSW,MARGIN,SLPIC,SSPIC,ELPIC,ESPIC,
     &            NLR,NLRO,ILINE,SLTIC,TICBUF,LUT
      COMMON /C4/ TASKS,INSTANCES,NLABS,TYPE,INUNIT,OUTUNIT,CNT,FORMAT
C
      CHARACTER*32 TYPE
      CHARACTER*8 TASKS(100)
      CHARACTER*7 FORMAT
      INTEGER*4 INSTANCES(100),NLABS,STATUS,INUNIT,OUTUNIT,CNT
      REAL*8 MEAN,SIGMA
      REAL*4 GAIN,OFFSET
      INTEGER*4 PHBUF(259)
      INTEGER*2 INBUF(*)
      BYTE OUTBUF(*),LUT(256),TICBUF(100,10)
C
      MEAN=0.0
      SIGMA=0.0
      CALL ZIA(PHBUF,259)
      EL=SL+NLO-1
      IF(BYTPIX.EQ.2) GO TO 120
C
C     ....Here if byte image
      DO 100 LINE=SL,EL
      CALL XVREAD(INUNIT,OUTBUF,STATUS,'LINE',LINE,'SAMP',SS,'NSAMPS',
     +NBO,' ')
      IF(STATUS .NE. 1) GO TO 999
      CALL TBL(OUTBUF,LUT,NBO)
      IF(COMP.EQ.1.AND.BEFOR.EQ.0) CALL XORV(1,NBO,-1,OUTBUF,0,1)
      CALL HSUB(1,NBO,OUTBUF,PHBUF(4),0,255)
  100 CONTINUE
      GO TO 300
C
C     ....Here if halfword image
120   GAIN=255.0/(INMAX-INMIN)
      OFFSET=-255.0*INMIN/(INMAX-INMIN)

      DO 200 LINE=SL,EL
      CALL XVREAD(INUNIT,INBUF,STATUS,'LINE',LINE,'SAMP',SS,
     +'NSAMPS',NBO,' ')
      IF(STATUS .NE. 1) GO TO 999

      DO 150 SAMP=1,NBO
      DN=GAIN*INBUF(SAMP)+OFFSET+.5
      IF(DN.LT.0) DN=0
      IF(DN.GT.255) DN=255
      CALL ITLA(DN,OUTBUF(SAMP),1)
  150 CONTINUE
C
      IF(COMP.EQ.1.AND.BEFOR.EQ.0) CALL XORV(1,NBO,-1,OUTBUF,0,1)
      CALL HSUB(1,NBO,OUTBUF,PHBUF(4),0,255)
  200 CONTINUE
C
C        CALCULATE MEAN AND STANDARD DEVIATION
300   NPTS=NLO*NBO
      DO 350 J=1,255
      MEAN=MEAN+DFLOAT(J)*DFLOAT(PHBUF(J+4))
      SIGMA=SIGMA+DFLOAT(PHBUF(J+4))*DFLOAT(J)*DFLOAT(J)
  350 CONTINUE
      PHBUF(1)=256
      MEAN=MEAN/NPTS
      PHBUF(2)=1000.0*MEAN+0.5
      SIGMA=DSQRT(SIGMA/NPTS-MEAN*MEAN)
      PHBUF(3)=1000.0*SIGMA+0.5
C
      RETURN
C
999   IND=2
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Put 'LINE' label, numbers, and tics on sides
C
      SUBROUTINE LINTIC(BUF)
      IMPLICIT INTEGER(A-Z)
      COMMON /C1/ SL,SS,NLO,NBO,NLI,NBI,NI,BYTPIX,STRTCH,INMIN,INMAX,
     &            COMP,BEFOR,LFACT,NSW,MARGIN,SLPIC,SSPIC,ELPIC,ESPIC,
     &            NLR,NLRO,ILINE,SLTIC,TICBUF,LUT
      COMMON /INIT/ SLLAB,ELLAB,SSLAB,NUM,NDIGIT,CEN,SLNUM,ELNUM,ITIC,
     &              IDIGIT,NSLICE,ICHAR,LSLICE
      COMMON /C8/  REPEAT,INUM
C
      CHARACTER*80 TMPBUF
      CHARACTER*6  LABEL,INUM
      BYTE TICBUF(100,10),BUF(*),BLK,LUT(256)
      DATA BLK/255/
C
      LABEL='LINE'
C
      IF(ILINE.GT.SLTIC) GO TO 10
C
C       INITIALIZE
      SLLAB=40
      ELLAB=SLLAB+4*8*LFACT-1
      SSLAB=SSPIC-11-6*LFACT
      NUM=0
      IF(LFACT.GT.2) NUM=200
      NDIGIT=1
      IF(LFACT.GT.2) NDIGIT=3
      CEN=5+7*LFACT+10+1
      IF(LFACT.GT.2) CEN=CEN+200
      SLNUM=CEN-4*NDIGIT*LFACT+1
      ELNUM=CEN+4*NDIGIT*LFACT
      ITIC=99
C
C        LABELING
   10 IF(ILINE.LT.SLLAB.OR.ILINE.GT.ELLAB) GO TO 40
      IF(ILINE.GT.SLLAB) GO TO 20
      REPEAT=LFACT
      LSLICE=0
      ICHAR=1
   20 CALL TEXT(LABEL(ICHAR:ICHAR),1,LSLICE,BUF(SSLAB),6,0)
      IF(LFACT.GT.1) CALL EXPAND(BUF(SSLAB),6,LFACT)
      CALL MVE(1,6*LFACT,BUF(SSLAB),BUF(ESPIC+12),1,1)
      REPEAT=REPEAT-1
      IF(REPEAT.EQ.0) LSLICE=LSLICE+1
      IF(REPEAT.EQ.0) REPEAT=LFACT
      IF(LSLICE.EQ.8) ICHAR=ICHAR+1
      IF(LSLICE.EQ.8) LSLICE=0
      GO TO 80
C
C        NUMBERING
   40 IF(ILINE.LT.SLNUM.OR.ILINE.GT.ELNUM) GO TO 80
      IF(ELNUM.GT.ELPIC+12) GO TO 80
      IF(ILINE.GT.SLNUM) GO TO 50
      WRITE(TMPBUF,'(''(I'',I4.4,'')'')') NDIGIT
      WRITE(INUM(1:NDIGIT),TMPBUF) NUM
      REPEAT=LFACT
      NSLICE=0
      IDIGIT=1
   50 CALL TEXT(INUM(IDIGIT:IDIGIT),1,NSLICE,BUF(SSLAB),6,0)
      IF(LFACT.GT.1) CALL EXPAND(BUF(SSLAB),6,LFACT)
      CALL MVE(1,6*LFACT,BUF(SSLAB),BUF(ESPIC+12),1,1)
      REPEAT=REPEAT-1
      IF(REPEAT.EQ.0) NSLICE=NSLICE+1
      IF(REPEAT.EQ.0) REPEAT=LFACT
      IF(NSLICE.EQ.8) IDIGIT=IDIGIT+1
      IF(NSLICE.EQ.8) NSLICE=0
      IF(IDIGIT.GT.NDIGIT) GO TO 60
      GO TO 80
   60 NUM=NUM+100
      IF(LFACT.GT.2) NUM=NUM+100
      CEN=CEN+100
      IF(LFACT.GT.2) CEN=CEN+100
      IF(CEN.GT.ELPIC) GO TO 80
      NDIGIT=3
      IF(NUM.GE.1000) NDIGIT=4
      IF(NUM.GE.10000) NDIGIT=5
      SLNUM=CEN-4*NDIGIT*LFACT+1
      ELNUM=CEN+4*NDIGIT*LFACT
C
   80 IF(ILINE.LT.SLPIC-2) RETURN
      IF(ILINE.EQ.ELPIC+1) CALL ITLA(0,BUF(ESPIC+1),12)
      IF(ILINE.GT.ELPIC) RETURN
C
C        TIC MARKS FOR LEFT AND RIGHT COLUMNS
      DO 110 L=1,10
         BUF(SSPIC-12+L)=TICBUF(ITIC,L)
         BUF(ESPIC+12-L)=TICBUF(ITIC,L)
  110 CONTINUE
C       WHITE BORDER LINES ON LEFT AND RIGHT OF PICTURE
      BUF(SSPIC-1)=BLK
      BUF(ESPIC+1)=BLK
      ITIC=ITIC+1
      IF(ITIC.GT.100) ITIC=1
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Put tic mark on sample scale
C
      SUBROUTINE SAMTIC(TOP,OUTBUF)
      IMPLICIT INTEGER(A-Z)
      COMMON /C1/ SL,SS,NLO,NBO,NLI,NBI,NI,BYTPIX,STRTCH,INMIN,INMAX,
     &            COMP,BEFOR,LFACT,NSW,MARGIN,SLPIC,SSPIC,ELPIC,ESPIC,
     &            NLR,NLRO,ILINE,SLTIC,TICBUF,LUT
C
      BYTE OUTBUF(*),TICBUF(100,10),LUT(256)
C
C
      IF(TOP.EQ.1) GO TO 35
C        DRAW WHITE BORDER ALONG BOTTOM OF PICTURE
      CALL LINTIC(OUTBUF)
      CALL ITLA(255,OUTBUF(SSPIC-1),NBO+2)
      CALL WLINE(ILINE,NSW,OUTBUF,1)
C
C
35    DO 70 K=1,10
      L=K
      IF(TOP.EQ.0) L=11-K
      CALL ITLA(0,OUTBUF,NSW)
      CALL LINTIC(OUTBUF)
      CALL MVE(1,2,TICBUF(99,L),OUTBUF(SSPIC-2),1,1)
      NPIX=100
      JS=SSPIC
   40   IF(JS+NPIX-1.GT.ESPIC) NPIX=ESPIC-JS+1
        CALL MVE(1,NPIX,TICBUF(1,L),OUTBUF(JS),1,1)
        IF(NPIX.LT.100) GO TO 70
        JS=JS+100
        GO TO 40
   70 CALL WLINE(ILINE,NSW,OUTBUF,1)
      CALL ITLA(0,OUTBUF,NSW)
C
C
      IF(TOP.EQ.0) GO TO 100
C        DRAW WHITE BORDER LINE ACROSS TOP OF PICTURE
      CALL LINTIC(OUTBUF)
      CALL ITLA(255,OUTBUF(SSPIC-1),NBO+2)
      CALL WLINE(ILINE,NSW,OUTBUF,1)
      CALL ITLA(0,OUTBUF,NSW)
C
100   RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Write 'SAMPLE' label and numbers along top and bottom
C
      SUBROUTINE SAMLAB(BUF,NSW,ILINE,SSPIC,ESPIC,LFACT,NBO)
      IMPLICIT INTEGER(A-Z)
      BYTE BUF(*)
      CHARACTER*6 LABEL,LNUM
      CHARACTER*80 TMPBUF
C
      LABEL='SAMPLE'
      DO 500 K=1,7
      CALL ITLA(0,BUF,NSW)
      NUM=0
      CEN=SSPIC-1
   90 IF(CEN.GT.ESPIC) GO TO 300
      NDIGIT=1
      IF(NUM.GE.100) NDIGIT=3
      IF(NUM.GE.1000) NDIGIT=4
      IF(NUM.GE.10000) NDIGIT=5
      LOC=CEN-3*LFACT*NDIGIT
      WRITE(TMPBUF,'(''(I'',I4.4,'')'')') NDIGIT
      WRITE(LNUM(1:NDIGIT),TMPBUF) NUM
      CALL TEXT(LNUM,NDIGIT,K-1,BUF(LOC),6,0)
      IF(LFACT.GT.1) CALL EXPAND(BUF(LOC),6*NDIGIT,LFACT)
      CEN=CEN+100
      IF(LFACT.GT.2) CEN=CEN+100
      NUM=NUM+100
      IF(LFACT.GT.2) NUM=NUM+100
      GO TO 90
C
C        'SAMPLE'
  300 IF(NBO.LT.100) GO TO 350
      NCHAR=6
      IF(LFACT.GT.1) NCHAR=4
      CALL TEXT(LABEL,NCHAR,K-1,BUF(SSPIC+20),6,0)
      IF(LFACT.GT.1) CALL EXPAND(BUF(SSPIC+20),6*NCHAR,LFACT)
C
  350 DO 400 L=1,LFACT
      CALL WLINE(ILINE,NSW,BUF,1)
  400 CONTINUE
  500 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Process the system labels
C
      SUBROUTINE SYSLABEL(OUTBUF)
      IMPLICIT INTEGER(A-Z)
      COMMON /C1/ SL,SS,NLO,NBO,NLI,NBI,NI,BYTPIX,STRTCH,INMIN,INMAX,
     &            COMP,BEFOR,LFACT,NSW,MARGIN,SLPIC,SSPIC,ELPIC,ESPIC,
     &            NLR,NLRO,ILINE,SLTIC,TICBUF,LUT
      COMMON /C2/ NOLAB,LABG,LABH,LABP,LABU,LABA,LABE,LABD
     &           ,ILOGO,LOGO1,LOGO2,SQUEZ,NOID,LAB
      COMMON /C4/ TASKS,INSTANCES,NLABS,TYPE,INUNIT,OUTUNIT,CNT,FORMAT
      COMMON /C5/ NTIME,REDUCE,NSCHAR
      BYTE TICBUF(100,10),LUT(256)
      CHARACTER*7200 LAB
      CHARACTER*32 TYPE
      CHARACTER*8 TASKS(100)
      CHARACTER*7 FORMAT
      INTEGER*4 INSTANCES(100),NLABS,INUNIT,OUTUNIT,CNT
      BYTE OUTBUF(*)
      CHARACTER*70 OUTLAB
C
      OUTLAB(1:51)='FORMAT=XXXXXXX NS=XXXXX NL=XXXXX TYPE=XXXXXXXXXXXXX'
      OUTLAB(52:70)='XXXXXXXXXXXXXXXXXXX'
      IF (NOLAB .EQ. 1) RETURN
      REDUCE=0
      NSCHAR=6*LFACT
      NTIME=LFACT
C
C        CHECK EXPAND PARAMETER
   10 NSLINE=70*NSCHAR
      IF (NSLINE .LT. NSW) GO TO 30
      CALL XVMESSAGE('LABEL SIZE REDUCED',' ')
      REDUCE=REDUCE+1
      NSCHAR=6*(LFACT-REDUCE)
      NTIME=LFACT-REDUCE
      GO TO 10
   30 LMARG=(NSW-72*NSCHAR)/2+1
      CALL ITLA(0,OUTBUF,NSW)
      CALL WLINE(ILINE,NSW,OUTBUF,NTIME*4)
      OUTLAB(39:70)=TYPE
      OUTLAB(8:14)=FORMAT
      WRITE(OUTLAB(19:23),'(I5)') NBI
      WRITE(OUTLAB(28:32),'(I5)') NLI
      DO 250 L=1,7
      CALL TEXT(OUTLAB,70,L-1,OUTBUF(LMARG),6,0)
      IF (LFACT-REDUCE .GT. 1) CALL EXPAND(OUTBUF(LMARG),6*70,
     +LFACT-REDUCE)
      CALL WLINE(ILINE,NSW,OUTBUF,NTIME)
  250 CONTINUE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Process the VICAR1 labels
C
      SUBROUTINE LABELS(OUTBUF)
      IMPLICIT INTEGER(A-Z)
      COMMON /C1/ SL,SS,NLO,NBO,NLI,NBI,NI,BYTPIX,STRTCH,INMIN,INMAX,
     &            COMP,BEFOR,LFACT,NSW,MARGIN,SLPIC,SSPIC,ELPIC,ESPIC,
     &            NLR,NLRO,ILINE,SLTIC,TICBUF,LUT
      COMMON /C2/ NOLAB,LABG,LABH,LABP,LABU,LABA,LABE,LABD,
     &            ILOGO,LOGO1,LOGO2,SQUEZ,NOID,LAB
      COMMON /C5/ NTIME,REDUCE,NSCHAR
      COMMON /C6/ HISTNO,GLL
C
      CHARACTER*7200 LAB
      CHARACTER*360 OUTLAB
      CHARACTER*9   LABTYP
      CHARACTER*40  ERMS
      CHARACTER*80  LAB2
      BYTE OUTBUF(*),LUT(256), TICBUF(100,10)
      
C
      LAB2='STRETCH XXXXX - XXXXX'
      LABTYP='GPADHEU S'
      ERMS='UNRECOGNIZABLE CHARACTER IN COL 71 (   )'
C
      IF(NOLAB.EQ.1) RETURN
C
C
      NUM=1
      COUNT=0
C
C        DETERMINE HOW MANY LABELS WILL FIT ON ONE OUTPUT LINE
   20 IF(SQUEZ.EQ.0) GO TO 30
      NUM=NSW/(72*NSCHAR)
      IF(NUM.GT.5) NUM=5
      CALL PRNT(4,1,NUM,'NUMBER OF LABELS PER LINE =.')
      IF(NUM.LT.2) CALL XVMESSAGE('NOT ENOUGH SPACE FOR SQUEEZE',' ')
      IF(NUM.LT.2) SQUEZ=0
C
   30 LMARG=(NSW-72*NUM*NSCHAR)/2+1
      ILR=0
C
      OUTLAB=' '
      CALL ITLA(0,OUTBUF,NSW)
      CALL WLINE(ILINE,NSW,OUTBUF,NTIME*4)
C
      DO 400 ILR=1,5*NLRO
C
      DO 100 I=1,9
      IF (LAB(71+(ILR-1)*72:71+(ILR-1)*72).EQ.LABTYP(I:I))
     &        GO TO (101,102,103,104,105,106,107,107,300),I
  100 CONTINUE
      ERMS(38:38)=LAB(71+(ILR-1)*72:71+(ILR-1)*72)
      CALL XVMESSAGE(ERMS,' ')
      GO TO 300
C
  101 IF(LABG.EQ.1) GO TO 200
      GO TO 300
  102 IF(LABP.EQ.1) GO TO 200
      GO TO 300
  103 IF(LABA.EQ.1) GO TO 200
      GO TO 300
  104 IF(LABD.EQ.1) GO TO 200
      GO TO 300
  105 IF(LABH.EQ.1) GO TO 200
      GO TO 300
  106 IF(LABE.EQ.1) GO TO 200
      GO TO 300
  107 IF(LABU.EQ.1) GO TO 200
      GO TO 300
C
C
  200 COUNT=COUNT+1
      OUTLAB(72*(COUNT-1)+1:72*(COUNT-1)+72)=
     *       LAB(1+(ILR-1)*72:1+(ILR-1)*72+71)
      IF(COUNT.LT.NUM) GO TO 300
C
C        WRITE ONE LINE OF LABELS
      DO 250 L=1,7
      CALL TEXT(OUTLAB,72*NUM,L-1,OUTBUF(LMARG),6,0)
      NBYT=NUM*6*72
      IF(LFACT-REDUCE.GT.1) CALL EXPAND(OUTBUF(LMARG),NBYT,LFACT-REDUCE)
      CALL WLINE(ILINE,NSW,OUTBUF,NTIME)
  250 CONTINUE
      CALL ITLA(0,OUTBUF,NSW)
      CALL WLINE(ILINE,NSW,OUTBUF,2*NTIME)
      COUNT=0
C
C        CHECK FOR LAST LABEL[B
  300 IF (LAB(72*ILR:72*ILR).EQ. 'L') GO TO 500
C
  350 CONTINUE
  400 CONTINUE
C
C        WRITE STRETCH LIMITS
500   IF(STRTCH.EQ.0) GO TO 900
      OUTLAB=' '
      IF ((HISTNO .EQ. 0) .OR. (HISTNO .EQ. 1)) THEN
         WRITE (LAB2(9:14),'(I6)') INMIN
         WRITE (LAB2(17:22),'(I6)') INMAX
         IF (HISTNO .EQ. 0) THEN
            OUTLAB(72*COUNT+1:72*COUNT+22)=LAB2(1:22)
         ELSE
            OUTLAB(72*COUNT+1:72*COUNT+23)=LAB2(1:23)
         END IF 
      ELSE IF (HISTNO .EQ. 2) THEN
         WRITE (LAB2(24:29),'(I6)') INMIN
         WRITE (LAB2(32:37),'(I6)') INMAX
C               CALL MVL(' ',LAB2,23)
C               CALL ITLA(LAB2,23)
         OUTLAB(72*COUNT+1:72*COUNT+38)=LAB2(1:38)
      ELSE 
         WRITE (LAB2(39:44),'(I6)') INMIN
         WRITE (LAB2(47:52),'(I6)') INMAX
         OUTLAB(72*COUNT+1:72*COUNT+52)=LAB2(1:52)
      END IF 
  510 DO 550 L=1,7
      CALL TEXT(OUTLAB,72*(COUNT+1),L-1,OUTBUF(LMARG),6,0)
      NBYT=(COUNT+1)*6*72
      IF(LFACT-REDUCE.GT.1) CALL EXPAND(OUTBUF(LMARG),NBYT,LFACT-REDUCE)
      CALL WLINE(ILINE,NSW,OUTBUF,NTIME)
  550 CONTINUE
C
  900 CALL ITLA(0,OUTBUF,NSW)
      CALL WLINE(ILINE,NSW,OUTBUF,2*NTIME)
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Process the VICAR2 labels
C
      SUBROUTINE NEWLABEL(OUTBUF)
      IMPLICIT INTEGER(A-Z)
      INCLUDE 'fortport'
      COMMON /C1/ SL,SS,NLO,NBO,NLI,NBI,NI,BYTPIX,STRTCH,INMIN,INMAX,
     &            COMP,BEFOR,LFACT,NSW,MARGIN,SLPIC,SSPIC,ELPIC,ESPIC,
     &            NLR,NLRO,ILINE,SLTIC,TICBUF,LUT
      COMMON /C2/ NOLAB,LABG,LABH,LABP,LABU,LABA,LABE,LABD,
     &            ILOGO,LOGO1,LOGO2,SQUEZ,NOID,LAB
      COMMON /C4/ TASKS,INSTANCES,NLABS,TYPE,INUNIT,OUTUNIT,CNT,FORMAT
      COMMON /C5/ NTIME,REDUCE,NSCHAR
      COMMON /C6/ HISTNO,GLL
      COMMON /C7/ HISTORY,TASK,USERS
      BYTE TICBUF(100,10),LUT(256)
      CHARACTER*7200 LAB
      CHARACTER*70 OUTLABS
      CHARACTER*59 OUTLAB
      CHARACTER*61 VALUES
      CHARACTER*32 TYPE
      CHARACTER*28 DAT_TIM
      CHARACTER*8 TASKS(100),USER
      CHARACTER*32 KEY
      CHARACTER*7 FORMAT
      INTEGER*4 INSTANCES(100),NLABS,STATUS,INUNIT,OUTUNIT,CNT,VALUEI
      INTEGER*4 MAXLENGTH
      BYTE OUTBUF(*)
C      BYTE OUTLAB1(59),OUTLABS1(70),KEY1(32)
      REAL*4 VALUER
C      EQUIVALENCE (OUTLAB,OUTLAB1),(OUTLABS,OUTLABS1),(KEY,KEY1)
C
      OUTLABS(1:50)='XXXXXXXX=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
      OUTLABS(51:70)='XXXXXXXXXXXXXXXXXXXX'
      OUTLAB(1:50)='XXXXXXXX     XXXXXXXXXXXXXXXXXXXXXXXXXXXX     USER'
      OUTLAB(51:59)='=XXXXXXX '
      IF (NOLAB .EQ. 1) RETURN
      LMARG=(NSW-72*NSCHAR)/2+1
      CALL ITLA(0,OUTBUF,NSW)
      CALL WLINE(ILINE,NSW,OUTBUF,NTIME*4)
C     SUM=LABG+LABP+LABA+LABD+LABH+LABE+LABU      
      DO 1 I=1,CNT
      IF (TASK .EQ. 1) THEN
          IF (USERS .EQ. 1)THEN
              CALL XLGET(INUNIT,'HISTORY','DAT_TIM',DAT_TIM,STATUS,
     &             'HIST',TASKS(I),'INSTANCE',INSTANCES(I),
     &             'FORMAT','STRING',' ')
              CALL XLGET(INUNIT,'HISTORY','USER',USER,STATUS,
     &             'HIST',TASKS(I),'INSTANCE',INSTANCES(I),
     &             'FORMAT','STRING',' ')
              OUTLAB(1:8)=TASKS(I)
              OUTLAB(14:41)=DAT_TIM
              OUTLAB(52:59)=USER
              LENGTH=59
              START=1
          ELSE
              OUTLAB(1:8)=TASKS(I)
              LENGTH=8
              START=1
          END IF
      ELSE IF (USERS .EQ. 1) THEN
              CALL XLGET(INUNIT,'HISTORY','DAT_TIM',DAT_TIM,STATUS,
     &             'HIST',TASKS(I),'INSTANCE',INSTANCES(I),
     &             'FORMAT','STRING',' ')
              CALL XLGET(INUNIT,'HISTORY','USER',USER,STATUS,
     &             'HIST',TASKS(I),'INSTANCE',INSTANCES(I),
     &             'FORMAT','STRING',' ')
              OUTLAB(14:41)=DAT_TIM
              OUTLAB(52:59)=USER
              LENGTH=46
              START=14
      ELSE
          GO TO 100
      END IF
      DO J=1,7
         CALL TEXT(OUTLAB(START:START+LENGTH),LENGTH,J-1,
     &          OUTBUF(LMARG),6,0)
         IF (LFACT-REDUCE .GT. 1) CALL EXPAND(OUTBUF(LMARG),6*LENGTH,
     &   LFACT-REDUCE)
      CALL WLINE(ILINE,NSW,OUTBUF,NTIME)
      END DO
      CALL ITLA(0,OUTBUF,NSW)

 100  CALL WLINE(ILINE,NSW,OUTBUF,NTIME*2)
      IF (GLL.NE.1) GO TO 101
      IF (I .EQ. 1 .AND. GLL .EQ. 1)  THEN
        CALL XVMESSAGE(' USING GALILEO LABEL',' ')
        CALL FLT_LAB_MASKV(INUNIT,LMARG,LFACT,REDUCE,ILINE,NSW,
     +            NTIME,OUTBUF)
        GO TO 1
      END IF
      IF (GLL .EQ. 1 .AND. TASKS(I) .EQ. 'GALSOS') GO TO 1

 101  IF (HISTORY .EQ. 0) GO TO 1
      CALL XLINFO(INUNIT,'HISTORY','TASK',FORMAT,MAXLENGTH,NELEMENTS,
     &STATUS,'HIST',TASKS(I),'INSTANCE',INSTANCES(I),' ')

      DO 10 WHILE(.TRUE.)
      CALL XLNINFO(INUNIT,KEY,FORMAT,MAXLENGTH,NELEMENTS,STATUS,' ')
      IF (STATUS .EQ. -57) GO TO 30
      IF (KEY .EQ. 'TASK') GO TO 1
      IF (KEY(1:3) .EQ. 'LAB') THEN
          IKEY=BYTE2INT(ICHAR(KEY(4:4)))
          IF ((IKEY .GE. 48) .AND. (IKEY .LE. 57)) GO TO 10
      END IF
      IF ((KEY .EQ. 'DAT_TIM') .OR. (KEY .EQ. 'USER') .OR. 
     &(KEY .EQ. 'NLABS')) GO TO 10
      IF (FORMAT .EQ. 'STRING') THEN
         CALL XLGET(INUNIT,'HISTORY',KEY,VALUES,STATUS,'HIST',
     &        TASKS(I),'INSTANCE',INSTANCES(I),'FORMAT','STRING',' ')
         OUTLABS(1:8)=KEY(1:8)
         OUTLABS(10:MAXLENGTH+9)=VALUES(1:MAXLENGTH)
         LENGTH=9+MAXLENGTH
         GO TO 400
      END IF
      IF (FORMAT .EQ. 'INT') THEN
         CALL XLGET(INUNIT,'HISTORY',KEY,VALUEI,STATUS,'HIST',
     &        TASKS(I),'INSTANCE',INSTANCES(I),'FORMAT','INT',' ')
         OUTLABS(1:8)=KEY(1:8)
         WRITE (OUTLABS(10:19),'(I10)') VALUEI
         LENGTH=19
         GO TO 400
      END IF
      IF (FORMAT .EQ. 'REAL') THEN
         CALL XLGET(INUNIT,'HISTORY',KEY,VALUER,STATUS,'HIST',
     &        TASKS(I),'INSTANCE',INSTANCES(I),'FORMAT','REAL',' ')
         OUTLABS(1:8)=KEY(1:8)
         WRITE (OUTLABS(10:23),'(E14.8)') VALUER
         LENGTH=23
         GO TO 400
      END IF
C     THE FOLLOWING "IF" STATEMENT PREVENTS THE PROGRAM ABENDING IF
C     THE LENGTH OF VICAR2 HISTORY LABEL IS OVER 70 BYTES
400   IF (LENGTH .GT. 70) LENGTH=70

      DO J=1,7
      CALL TEXT(OUTLABS,LENGTH,J-1,OUTBUF(LMARG),6,0)
      IF (LFACT-REDUCE .GT. 1) CALL EXPAND(OUTBUF(LMARG),6*LENGTH,
     &LFACT-REDUCE)
      CALL WLINE(ILINE,NSW,OUTBUF,NTIME)
      END DO

      CALL ITLA(0,OUTBUF,NSW)
      CALL WLINE(ILINE,NSW,OUTBUF,NTIME*2)
   10 CONTINUE

    1 CONTINUE
   30 RETURN
      END      
      SUBROUTINE GWEDGE(BUF,HIST,ILINE,SSWEDG,NSWEDG,SLWEDG,ELWEDG,NLGL)
      IMPLICIT INTEGER(A-Z)
      INTEGER*4 DN(16)
      BYTE BUF(*)
      DATA DN/0,17,34,51,68,85,102,119,136,153,170,187,204,221,238,255/
C
      IF(ILINE.GT.ELWEDG+1) RETURN
      IF(ILINE.EQ.ELWEDG+1) GO TO 900
      IF(ILINE.EQ.SLWEDG.OR.ILINE.EQ.ELWEDG) GO TO 800
C
      IF(HIST.GT.0) GO TO 10
C       VERTICAL GRAY WEDGE
      I=16-(ILINE-SLWEDG-1)/NLGL
      CALL ITLA(DN(I),BUF(SSWEDG+1),NSWEDG-2)
      RETURN
C
C       HORIZONTAL GRAY WEDGE
10    IF(ILINE.GT.SLWEDG+1) RETURN
      NSGLEV=(NSWEDG-2)/16
      K=SSWEDG+1
      DO 50 I=1,16
      CALL ITLA(DN(I),BUF(K),NSGLEV)
      K=K+NSGLEV
   50 CONTINUE
      RETURN
C
C       WHITE BORDER AROUND GRAY WEDGE
  800 CALL ITLA(255,BUF(SSWEDG),NSWEDG)
      RETURN
C
  900 CALL ITLA(0,BUF(SSWEDG),NSWEDG)
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Write display line (BUF) NLINE times
C
      SUBROUTINE WLINE(ILINE,NSW,BUF,NLINE)
      IMPLICIT INTEGER(A-Z)
      COMMON /C4/TASKS,INSTANCES,NLABS,TYPE,INUNIT,OUTUNIT,CNT,FORMAT
      CHARACTER*32 TYPE
      CHARACTER*8 TASKS(100)
      CHARACTER*7 FORMAT
      INTEGER*4 STATUS,INSTANCES(100),NLABS,INUNIT,OUTUNIT,CNT
      BYTE BUF(*)
C
      DO 150 I=1,NLINE
      CALL XVWRIT(OUTUNIT,BUF,STATUS,'NSAMPS',NSW,' ')
  150 CONTINUE
      ILINE=ILINE+NLINE
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Display the IPL LOGO
C
      SUBROUTINE IPLLAB(LFACT,ILOGO,LOGO1,LOGO2,ILINE,NSW,BUF,PICBUF)
C
C        LFACT  EXPANSION FACTOR FOR CHARACTERS
C        ILOGO  LOGO FLAG  (0 = NO LOGOS,  1 = LOGOS)
C        LOGO1  LEFT LOGO NUMBER
C        LOGO2  RIGHT LOGO NUMBER
C        ILINE  NUMBER OF NEXT LINE TO BE WRITTEN
C        NSW    NUMBER OF OUTPUT SAMPLES
C        BUF    OUTPUT BUFFER
C        PICBUF  72 BYTES BUFFER WITH PICTURE ID,PROGRAMMER ID,JOBNAME
C
      IMPLICIT INTEGER(A-Z)
      INTEGER WHT, BGR, IDUMMY
      INTEGER*2 DATFLAG
      BYTE BUF(*)
      CHARACTER*17 DATE
      CHARACTER*45 ID
      CHARACTER*72 PICBUF
      CHARACTER*8  IDENT
      CHARACTER*31 JPL
      CHARACTER*6  TMPTIME
      CHARACTER*4  MONTH(12)
C
      DATA MONTH/'JAN ','FEB ','MAR ','APR ','MAY ','JUN ','JUL ',
     &           'AUG ','SEP ','OCT ','NOV ','DEC '/
      DATA WHT/255/,BGR/0/
C
      DATE=' SUN FEB 29, 1984'
      ID=' MIPL PIC ID YEAR/MO/DY/HRMNSC   DJL/        '
      JPL='JPL IMAGE PROCESSING LABORATORY'
C        *** FILL IN ID BUFFER ***
C
C        GET TIME FOR ID
      IND=0
      DATFLAG=2
C        *** CHANGE ID(N) TO ID(N+1) BY CHANGING IPL TO MIPL ***
      CALL XTIME(TMPTIME)
      ID(25:30)=TMPTIME
C     IF(IND.NE.0) CALL XVMESSAGE('** ERROR IN CALLING TIME',' ')
C        GET DATE AND JOBNAME FOR ID
      CALL XVPARM('IDENT',IDENT,COUNT,DEF,1)
      CALL DATFMT(DATFLAG,DATE,IDUMMY)
      IF (DEF .EQ. 0) THEN
          ID(38:45) = IDENT
      ELSE
          CALL GTPRCS(ID(38:45))
      END IF
      ID(14:17) = DATE(14:17)
      DO 5 I=1,12
      IF(DATE(6:9).EQ.MONTH(I)) GO TO 10
    5 CONTINUE
      CALL XVMESSAGE('** ERROR IN CALLING DATE',' ')
      GO TO 30
   10 WRITE (ID(19:20),'(I2)') I
      IF(I.LT.10) ID(19:19)='0'
      ID(22:23)= DATE(10:11)
      IF(ID(22:22).EQ.' ') ID(22:22)='0'
C        GET USERID FOR ID
C  30 CALL SYSACT(DUMJBN,ACCT,IFLDS,&35)
C     CALL MVL(ACCT(2),ID(32),3)
C        *** USER IS A NEW INTERNAL SUBROUTINE ***
C        *** BECAUSE OSDATE DOESN'T RETURN USE NAME ANY MORE ***
C  30 CALL USER(UN,LEN)  
   30 ID(34:36)= ID(38:40)
C      GO TO 50
C   35 CALL XVMESSAGE('** NO ACCOUNTING DATA WAS AVAILABLE',' ')
C      ID(30:32)='   '
C
C        FIND LENGTH OF JOB NAME
      DO 55 M=1,7
      IF(ID(38+M:38+M).EQ.' ') GO TO 60
   55 CONTINUE
C       CHECK SIZE AND FIND STARTING POINT FOR ID
60    NCID=38+M
      NSCHAR=6*LFACT
      NTIME=LFACT
   80 IPLST=(NSW-NSCHAR*NCID)/2+1
      IF(IPLST.GT.0) GO TO 90
      CALL XVMESSAGE('PIC ID SIZE REDUCED',' ')
      NSCHAR=NSCHAR-6
      NTIME=NTIME-1
      GO TO 80
C
C        CHECK FOR LOGO
90    IF(ILOGO.GE.1.AND.IPLST.LT.100*NTIME)
     &      CALL XVMESSAGE('PICTURE SIZE IS TOO SMALL FOR LOGO',' ')
      IF(IPLST.LT.100*NTIME) ILOGO=0
      IF(ILOGO.EQ.0) GO TO 100
      LOGLIN=0
      S=NSW-20-64*LFACT
C
C        *** START OF WRITING TO OUTPUT ***
C
C        BLANK LINES
100   CALL ITLA(0,BUF,NSW)
      CALL WLINE(ILINE,NSW,BUF,2*NTIME)
C        TOP PORTION OF LOGOS
      IF(ILOGO.EQ.0) GO TO 110
      DO 105 L=1,20
      LOGLIN=LOGLIN+1
      if (logo1.ne.0)CALL LOGO(WHT,LOGLIN,LOGO1,BGR,BUF(20))
      if (logo2.ne.0)CALL LOGO(WHT,LOGLIN,LOGO2,BGR,BUF(S))
      IF(NTIME.GT.1) CALL EXPAND(BUF(20),64,NTIME)
      IF(NTIME.GT.1) CALL EXPAND(BUF(S),64,NTIME)
      CALL WLINE(ILINE,NSW,BUF,NTIME)
      CALL ITLA(0,BUF,NSW)
  105 CONTINUE
C
C        TOP OF BOX AROUND PIC ID
C        *** CHANGE 6*26 TO 6*27 BY CHANGING IPL TO MIPL ***
C        *** CHANGE 6*27 TO 6*29 BY CHANGING 2-DIGIT YEAR TO 4-DIGIT ***
110   CALL ITLA(WHT,BUF(IPLST+2),4+6*29+4)
C     IF(NTIME.GT.1) CALL EXPAND(BUF(IPLST),28*6,NTIME)
      IF(NTIME.GT.1) CALL EXPAND(BUF(IPLST),29*6,NTIME)
      IF(ILOGO.EQ.0) GO TO 112
      LOGLIN=LOGLIN+1
      if (logo1.ne.0)CALL LOGO(WHT,LOGLIN,LOGO1,BGR,BUF(20))
      if (logo2.ne.0)CALL LOGO(WHT,LOGLIN,LOGO2,BGR,BUF(S))
      IF(NTIME.GT.1) CALL EXPAND(BUF(20),64,NTIME)
      IF(NTIME.GT.1) CALL EXPAND(BUF(S),64,NTIME)
112   CALL WLINE(ILINE,NSW,BUF,NTIME)
      CALL ITLA(0,BUF,NSW)
C
C        UPPER SIDES OF BOX
      ILOC1=IPLST+2
      ILOC2=IPLST+6+6*29+3
      CALL ITLA(WHT,BUF(ILOC1),1)
      CALL ITLA(WHT,BUF(ILOC2),1)
C     IF(NTIME.GT.1) CALL EXPAND(BUF(IPLST),28*6,NTIME)
      IF(NTIME.GT.1) CALL EXPAND(BUF(IPLST),29*6,NTIME)
      DO 120 K=1,2
      IF(ILOGO.EQ.0) GO TO 115
      LOGLIN=LOGLIN+1
      CALL ITLA(0,BUF(20),64*NTIME)
      CALL ITLA(0,BUF(S),64*NTIME)
      if (logo1.ne.0)CALL LOGO(WHT,LOGLIN,LOGO1,BGR,BUF(20))
      if (logo2.ne.0)CALL LOGO(WHT,LOGLIN,LOGO2,BGR,BUF(S))
      IF(NTIME.GT.1) CALL EXPAND(BUF(20),64,NTIME)
      IF(NTIME.GT.1) CALL EXPAND(BUF(S),64,NTIME)
  115 CALL WLINE(ILINE,NSW,BUF,NTIME)
  120 CONTINUE
C
C        WRITE ID
      DO 130 K=1,7
      CALL TEXT(ID,NCID,K-1,BUF(IPLST),6,WHT)
      CALL ITLA(WHT,BUF(ILOC1),1)
      CALL ITLA(WHT,BUF(ILOC2),1)
      IF(NTIME.GT.1) CALL EXPAND(BUF(IPLST),6*NCID,NTIME)
      IF(ILOGO.EQ.0) GO TO 124
      LOGLIN=LOGLIN+1
      CALL ITLA(0,BUF(20),64*NTIME)
      CALL ITLA(0,BUF(S),64*NTIME)
      if (logo1.ne.0)CALL LOGO(WHT,LOGLIN,LOGO1,BGR,BUF(20))
      if (logo2.ne.0)CALL LOGO(WHT,LOGLIN,LOGO2,BGR,BUF(S))
      IF(NTIME.GT.1) CALL EXPAND(BUF(20),64,NTIME)
      IF(NTIME.GT.1) CALL EXPAND(BUF(S),64,NTIME)
  124 CALL WLINE(ILINE,NSW,BUF,NTIME)
      CALL ITLA(0,BUF,NSW)
  130 CONTINUE
C
C        LOWER SIDES OF BOX
      CALL ITLA(WHT,BUF(ILOC1),1)
      CALL ITLA(WHT,BUF(ILOC2),1)
C     IF(NTIME.GT.1) CALL EXPAND(BUF(IPLST),28*6,NTIME)
      IF(NTIME.GT.1) CALL EXPAND(BUF(IPLST),29*6,NTIME)
      DO 140 L=1,2
      IF(ILOGO.EQ.0) GO TO 135
      LOGLIN=LOGLIN+1
      CALL ITLA(0,BUF(20),64*NTIME)
      CALL ITLA(0,BUF(S),64*NTIME)
      if (logo1.ne.0)CALL LOGO(WHT,LOGLIN,LOGO1,BGR,BUF(20))
      if (logo2.ne.0)CALL LOGO(WHT,LOGLIN,LOGO2,BGR,BUF(S))
      IF(NTIME.GT.1) CALL EXPAND(BUF(20),64,NTIME)
      IF(NTIME.GT.1) CALL EXPAND(BUF(S),64,NTIME)
  135 CALL WLINE(ILINE,NSW,BUF,NTIME)
  140 CONTINUE
C
C        LOWER BORDER OF BOX
      CALL ITLA(0,BUF,NSW)
      CALL ITLA(WHT,BUF(IPLST+2),4+6*29+4)
C     IF(NTIME.GT.1) CALL EXPAND(BUF(IPLST),28*6,NTIME)
      IF(NTIME.GT.1) CALL EXPAND(BUF(IPLST),29*6,NTIME)
      IF(ILOGO.EQ.0) GO TO 145
      LOGLIN=LOGLIN+1
      if (logo1.ne.0)CALL LOGO(WHT,LOGLIN,LOGO1,BGR,BUF(20))
      if (logo2.ne.0)CALL LOGO(WHT,LOGLIN,LOGO2,BGR,BUF(S))
      IF(NTIME.GT.1) CALL EXPAND(BUF(20),64,NTIME)
      IF(NTIME.GT.1) CALL EXPAND(BUF(S),64,NTIME)
  145 CALL WLINE(ILINE,NSW,BUF,NTIME)
C
C        BLANK LINES BETWEEN ID AND JPL LABEL
      CALL ITLA(0,BUF,NSW)
      DO 150 K=1,4
      IF(ILOGO.EQ.0) GO TO 148
      LOGLIN=LOGLIN+1
      if (logo1.ne.0)CALL LOGO(WHT,LOGLIN,LOGO1,BGR,BUF(20))
      if (logo2.ne.0)CALL LOGO(WHT,LOGLIN,LOGO2,BGR,BUF(S))
      IF(NTIME.GT.1) CALL EXPAND(BUF(20),64,NTIME)
      IF(NTIME.GT.1) CALL EXPAND(BUF(S),64,NTIME)
  148 CALL WLINE(ILINE,NSW,BUF,1)
      CALL ITLA(0,BUF,NSW)
  150 CONTINUE
C
C        JPL IMAGE PROCESSING
      JPLST=(NSW-NSCHAR*31)/2+1
      DO 160 K=1,7
      CALL TEXT(JPL,31,K-1,BUF(JPLST),6,WHT)
      IF(NTIME.GT.1) CALL EXPAND(BUF(JPLST),31*6,NTIME)
      IF(ILOGO.EQ.0) GO TO 155
      LOGLIN=LOGLIN+1
      CALL ITLA(0,BUF(20),64*NTIME)
      CALL ITLA(0,BUF(S),64*NTIME)
      if (logo1.ne.0)CALL LOGO(WHT,LOGLIN,LOGO1,BGR,BUF(20))
      if (logo2.ne.0)CALL LOGO(WHT,LOGLIN,LOGO2,BGR,BUF(S))
      IF(NTIME.GT.1) CALL EXPAND(BUF(20),64,NTIME)
      IF(NTIME.GT.1) CALL EXPAND(BUF(S),64,NTIME)
  155 CALL WLINE(ILINE,NSW,BUF,NTIME)
  160 CONTINUE
      CALL ITLA(0,BUF,NSW)
C
C        FINISH UP LOGO
      IF(ILOGO.EQ.0) GO TO 180
      LOGLIN=LOGLIN+1
      DO 170 L=LOGLIN,64
      if (logo1.ne.0)CALL LOGO(WHT,L,LOGO1,BGR,BUF(20))
      if (logo2.ne.0)CALL LOGO(WHT,L,LOGO2,BGR,BUF(S))
      IF(NTIME.GT.1) CALL EXPAND(BUF(20),64,NTIME)
      IF(NTIME.GT.1) CALL EXPAND(BUF(S),64,NTIME)
      CALL WLINE(ILINE,NSW,BUF,NTIME)
      CALL ITLA(0,BUF,NSW)
  170 CONTINUE
C
C        FILL PICBUF
  180 PICBUF=' '
      PICBUF(1:45)=ID
      PICBUF(72:72)='C'
C
C        BLANK BORDER LINES
      CALL ITLA(0,BUF,NSW)
      CALL WLINE(ILINE,NSW,BUF,5)
C
      RETURN
      END
C	SUBROUTINE USER(UN,LON)
C         THIS IS THE SUBROUTINE TO FIND OUT THE USER NAME
C         ON RETURN, UN WILL CONTAIN THE USER NAME UP TO 
C         12 CHARACTERS, AND LON WILL HAVE THE ACTUAL LENGTH
C         OF THE NAME
C	implicit integer (a-z)
C	include '($jpidef)'
C	byte un(12)
C	common/cuser/s,j,ausr,alon
C	integer*2 s,j
C	character*12 usr
c
C        data s/4/
C	ausr=%loc(usr)
C	alon=%loc(lon)
C	j=jpi$_username
C	k=sys$getjpi(,,,s,,,)
c
C	call mvl(%ref(usr),un,lon)
C	return
C	end

      SUBROUTINE KEYINCC(KEY)
C
C	KEYINCC -- SUBROUTINE TO INCREMENT CHARACTER STRING KEY
C	PASSED VARIABLES:
C
C       KEY   -- CHARACTER STRING CONTAINING DIGITS
C
C	LOCAL VARIABLES:
C
C       J,K -- ONE'S, AND TEN'S DIGITS
      BYTE J,K
      CHARACTER*5 KEY
C
      J = ICHAR(KEY(5:5)) + 1
C
C     UPDATE KEY NAME... J INDICATES ONE'S DIGIT, K THE TEN'S DIGIT
      IF (J .GT. ICHAR('9')) THEN
          J = ICHAR('0')
          K = ICHAR(KEY(4:4)) + 1
          KEY(4:4) = CHAR(K)
      ENDIF
      KEY(5:5) = CHAR(J)
      RETURN
      END

      SUBROUTINE CHECK_STAT(NAME,STAT,AABEND)
C
C       STATUS CHECKING SUBROUTINE
C       Passed variables:
C       NAME -- Name of the offending routine
C       STAT -- Status passed back from that routine
C       AABEND -- Flag indicating whether or not to ABEND
      CHARACTER*(*) NAME
      INTEGER STAT
      LOGICAL AABEND

C
C	LOCAL VARIABLE:
C
C       MESSAGE -- OUTPUT MESSAGE ARRAY
      CHARACTER*80 MESSAGE
      IF (STAT .EQ. 1) RETURN
C
      WRITE(MESSAGE,10) STAT,NAME
   10 FORMAT(' Error ',I4,' in ',A20)
      CALL XVMESSAGE(MESSAGE,' ')
      IF (AABEND) THEN
	  CALL MABEND(' ABEND called',' ')
      ENDIF
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     HMASK generates histogram displays for use in masking programs.
C     The histograms are formatted by a call to HMASK. The entry HLINE
C     is then called repeatedly, returning one display line for each 
C     call.  HMASK can be used for DN values ranging from 0 to 32767.
C
C Operation
C
C  HMASK begins by reading in any histogram data sets.  These are data
C  sets created by the program HISTGEN and supplied to the masking pro-
C  gram as secondary inputs.  As they are being read in, the DN levels 
C  are compressed to a number of levels equal to HWIDTH.  The second to
C  last label of each histogram data set is saved for use as annotation
C  for that histogram.  If a histogram is supplied by the masking program
C  for the primary input, it is then compressed to HWIDTH levels also.
C
C  Each histogram is separately scaled by saturating the NSPIKE largest
C  values and then using the NSPIKE + 1 largest value to normalize all 
C  remaining values. This scaling puts the histogram values in terms of
C  image lines, the NSPIKE + 1 largest value being set to NLHIST.  HMASK
C  then returns to the calling program from which the entry is then called
C  repeatedly to generate the image lines for the histogram displays. The
C  image area which the histograms will occupy should be computed as follows:
C
C             NL= NROW * [NLHIST + 45 * LFACT]
C             NS= NCOL * [HWIDTH + 24 * LFACT]
C
C  Hmask will not work for negative DN values
C BUFSIZ should be at least 4*(No. of histograms) * HWIDTH

      SUBROUTINE HMASK(HBUF,BUFSIZ,NIHIST,ENLARGX,NCOLX,HWIDTHX,
     &      TICSX,TICLX,NSPIKE,HISTBF,PLAB,PHIST,NLHISTX,LOGFLG)
      INTEGER HBUF(*)	!Storage buffer for histograms
      INTEGER BUFSIZ	!Length of HBUF in bytes
      INTEGER NIHIST	!Number of input histogram files
      INTEGER ENLARGX	!Label magnification factor
      INTEGER NCOLX	!Number of histograms displayed horizontally
      INTEGER HWIDTHX	!Number of samples for each histogram
      INTEGER TICSX	!DN spacing of small tic mark along the DN axis
      INTEGER TICLX	!DN spacing of large tic marks along the DN axis
      INTEGER NSPIKE	!Number of spikes in the histogram normalization
      INTEGER HISTBF(259)	!Histogram of input image
      CHARACTER*72 PLAB !Histogram annotation for input image
      INTEGER PHIST	! 1=HISTBUF contains valid data, =0 otherwise
      INTEGER NLHISTX	!Number of lines to be used to display DN level freqs.
      INTEGER LOGFLG	! 1=logrithmic scale, 0=linear scale

      CHARACTER*1   LITL
      CHARACTER*5 MEANLT,SIGLT
      CHARACTER*9 LOGLIT
      REAL*4 MEAN(10),SIGMA(10)
      INTEGER INBUF(1024),SSTART
      INTEGER*4 HLOC,BUFLIM,INSTANCES(100)
      INTEGER*4 CNT,STATUS
      INTEGER*4 INIBIS
      CHARACTER*8 TASKS(100)
      CHARACTER*4 BLNK

      COMMON/SV/CFACT(10),HSTART(10),NLEV(10),NOHIST,NCOL
      COMMON/SV/HWIDTH,NLHIST,NSHIST,LFTEDG,NSO,BARWID
      COMMON/SV/TICS,TICL,NLTIC,SPACEH,SPACEA,SPIKEH,ENLARG
      COMMON/SV/NLA,NCHAR,NLCHAR,NSCHAR,RESTRT,NABEND,MBUF
      INTEGER*4 CFACT,HSTART,NLEV,NOHIST,NCOL,HWIDTH,NLHIST,NSHIST,
     &  LFTEDG,NSO,BARWID,TICS,TICL,NLTIC,SPACEH,SPACEA,SPIKEH,
     &  ENLARG,NLA,NCHAR,NLCHAR,NSCHAR,RESTRT,NABEND
      CHARACTER*970 MBUF
C
C
      MEANLT='m= '
      SIGLT(1:2)=']='
      LOGLIT='*LOG*'

      BLNK=' '
      LITL='L'

      ENLARG=ENLARGX
      NCOL=NCOLX
      HWIDTH=HWIDTHX
      TICS=TICSX
      TICL=TICLX
      NLHIST=NLHISTX		!Number of lines per hist

      NOHIST = NIHIST + PHIST	!Total number of histograms
      IF(NOHIST.LT.1) GO TO 992

      NABEND=0
      RESTRT = 1		!1=restart, 0=continue
      DECADE=3.0
      BARWID = 1		!Number of samples per dn level
      SPACEH = 3*ENLARG		!# of spacer lines between histogram rows
      SPACEA = ENLARG		!# of spacer lines between annotation
      SPIKEH = 2*ENLARG		!# of lines for spike display
      do i=1,970
        MBUF=' '
      enddo
      CALL ZIA(HBUF,BUFSIZ/4)
C
C     ....Check for log option
      LOGMOV=0
      IF (LOGFLG.EQ.1) LOGMOV=9
C
      NLTIC = 3*ENLARG		!Height in lines of minor tics
      NLCHAR = 7*ENLARG		!Height (in lines) of character annotation
      NSCHAR = 6*ENLARG		!Width (in samples) of 1 character
      NCHAR = HWIDTH/NSCHAR	!Number of characters per line annotation

      NLA=(20+68+NCHAR-1)/NCHAR	!Number of lines of annnotation
      IF (NLA.EQ.1) NLA=2
      IF (NLA.GT.3) NLA=3
C
C       CALCULATE LEFT EDGE AND SPACE BETWEEN HISTOGRAM COLUMNS
      LFTEDG=1+NSCHAR
      NSHIST=HWIDTH+4*NSCHAR
      NSO=NSHIST*NCOL
      HLOC=1
      BUFLIM=BUFSIZ/4
      IF(NIHIST.LT.1) GO TO 101
C
C     ....Read in histogram files
      DO 100 I=1,NIHIST
      CALL XVUNIT(INUNIT2,'INP',1+I,STATUS,' ')
      CALL IBIS_FILE_OPEN(INUNIT2,INIBIS,'READ',' ',' ',' ',' ',STATUS)
      IF(STATUS.NE.1) CALL IBIS_SIGNAL_U(INUNIT2,STATUS,1)
      MSTART=97*(I-1)
      CALL IBIS_FILE_GET(INIBIS,'NR',NBI,1,1)
      CNT=100		!Get the task names in order of occurrence
      CALL XLHINFO(INUNIT2,TASKS,INSTANCES,CNT,STATUS,' ')
      MBUF(MSTART+26+LOGMOV:MSTART+33+LOGMOV)=TASKS(CNT-1)
C
C     ....Get histogram data for current file
      IF(LOGFLG.EQ.1)MBUF(MSTART+26:MSTART+25+LOGMOV)=LOGLIT
      CALL IBIS_COLUMN_READ(INIBIS,INBUF,1,1,NBI,STATUS)
      IF(STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
      CALL XLGET(INUNIT2,'PROPERTY','NUMBER_OF_LEVELS_IN_HISTOGRAM',
     &        NLEV(I),STATUS,'FORMAT','INT','PROPERTY','STATISTICS',' ')
      CALL XLGET(INUNIT2,'PROPERTY','MEAN_VALUE',MEAN(I),STATUS,
     &           'FORMAT','REAL','PROPERTY','STATISTICS',' ')
      CALL XLGET(INUNIT2,'PROPERTY','STANDARD_DEVIATION_VALUE',SIGMA(I),
     &           STATUS,'FORMAT','REAL','PROPERTY','STATISTICS',' ')

      CFACT(I) = NLEV(I)/HWIDTH	!Compression factor (# of dn levels per sample)
      IF (CFACT(I).LT.1) CFACT(I)=1

      ILOC=4
      INLEV=NLEV(I)
      ICFACT=CFACT(I)
      HSTART(I)=HLOC

C     ....Copy histograms to hbuf, compressing CFACT dn levels into 1.
      DO J=1,INLEV,ICFACT
         IF (HLOC.GT.BUFLIM) GOTO 995
         DO K=1,ICFACT
            HBUF(HLOC)=HBUF(HLOC)+INBUF(ILOC)
            ILOC=ILOC+1
            IF (ILOC.GT.NBI) THEN
               ILOC=1
               CALL IBIS_COLUMN_READ(INIBIS,INBUF,1,1,NBI,STATUS)
               IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
            ENDIF
         ENDDO
         HLOC = HLOC + 1
      ENDDO
C
      CALL IBIS_FILE_CLOSE(INIBIS,' ',STATUS)
      IF(STATUS.NE.1) CALL IBIS_SIGNAL_U(INUNIT2,STATUS,1)
  100 CONTINUE
C
C     ....Process primary histogram (HISTBF)
  101 IF(PHIST.EQ.0) GO TO 120
      NLEV(NOHIST)=HISTBF(1)
      MEAN(NOHIST)=HISTBF(2)/1000.0
      SIGMA(NOHIST)=HISTBF(3)/1000.0
      CFACT(NOHIST)=NLEV(NOHIST)/HWIDTH
      IF (CFACT(NOHIST) .LT. 1) CFACT(NOHIST)=1
      HSTART(NOHIST)=HLOC
      ILOC=4
      ICFACT=CFACT(NOHIST)
      DO J=1,256,ICFACT
         DO K=1,ICFACT
            HBUF(HLOC)=HBUF(HLOC)+HISTBF(ILOC)
            ILOC=ILOC+1
         ENDDO
         HLOC=HLOC+1
         IF(HLOC.GT.BUFLIM) GO TO 995
      ENDDO

C     ....Add annotation for primary histogram
      MBUF(97*NIHIST+26+LOGMOV:97*NIHIST+26+LOGMOV+71-LOGMOV)=
     &     PLAB(1:71-LOGMOV)
      IF(LOGFLG.EQ.1)MBUF(97*NIHIST+26:97*NIHIST+25+LOGMOV)=LOGLIT
  120 CONTINUE
C
C     ....Scale and normalize histograms
      DO 150 I=1,NOHIST
      MSTART=97*(I-1)+1		!Add mean and sigma to histogram annotation
      MBUF(MSTART:MSTART+2)=MEANLT
      M=MEAN(I)+0.5
      N=4
      IF(M.GE.10000) THEN
         WRITE(MBUF(MSTART+2:MSTART+9),'(F8.2)') MEAN(I)
      ELSE IF(M.GE.1000) THEN
         WRITE(MBUF(MSTART+2:MSTART+8),'(F7.2)') MEAN(I)
      ELSE IF(M.GE.100) THEN
         WRITE(MBUF(MSTART+2:MSTART+7),'(F6.2)') MEAN(I)
      ELSE IF(M.GE.10) THEN
         WRITE(MBUF(MSTART+2:MSTART+6),'(F5.2)') MEAN(I)
      ELSE IF(M.LT.10) THEN
         WRITE(MBUF(MSTART+2:MSTART+5),'(F4.2)') MEAN(I)
      END IF
      SSTART=MSTART+12
      MBUF(SSTART-1:SSTART)=SIGLT(1:2)
      M=SIGMA(I)+0.5
      IF(M.GE.10000) THEN
         WRITE(MBUF(SSTART+2:SSTART+9),'(F8.2)') SIGMA(I)
      ELSE IF(M.GE.1000) THEN
         WRITE(MBUF(SSTART+2:SSTART+8),'(F7.2)') SIGMA(I)
      ELSE IF(M.GE.100) THEN
         WRITE(MBUF(SSTART+2:SSTART+7),'(F6.2)') SIGMA(I)
      ELSE IF(M.GE.10) THEN
         WRITE(MBUF(SSTART+2:SSTART+6),'(F5.2)') SIGMA(I)
      ELSE IF(M.LT.10) THEN
         WRITE(MBUF(SSTART+2:SSTART+5),'(F4.2)') SIGMA(I)
      END IF
C
C     ....Flag the nspike largest values with -1
C     ....and normalize the histogram to the next largest value
      NSTART=HSTART(I)-1
      DO J=1,NSPIKE
         MAXFRQ=0
         DO K=1,HWIDTH
            IF (HBUF(NSTART+K).GE.MAXFRQ) THEN
               MAXFRQ = HBUF(NSTART+K)
               IDNMAX = K
            ENDIF
         ENDDO
         IF (MAXFRQ.EQ.0) GO TO 150
         HBUF(NSTART+IDNMAX)=-1
      ENDDO

      MAXFRQ=0
      DO K=1,HWIDTH
         IF(HBUF(NSTART+K).GT.MAXFRQ) MAXFRQ=HBUF(NSTART+K)
      ENDDO
C
      IF(MAXFRQ.EQ.0) GO TO 150

      IF (LOGFLG.EQ.0) THEN	!Linear scaling
         DO J=1,HWIDTH
            IF (HBUF(NSTART+J).NE.-1) 
     &	       HBUF(NSTART+J)=NLHIST*HBUF(NSTART+J)/MAXFRQ
         ENDDO
      ELSE			!LOG scaling
         AMAXHT = NLHIST/DECADE
         ASCALE = ALOG10(FLOAT(MAXFRQ))
         DO J=1,HWIDTH
            IF (HBUF(NSTART+J).GT.0) THEN
               T = HBUF(NSTART+J)
               JJ = (ALOG10(T)+DECADE-ASCALE)*AMAXHT+0.5
               IF (JJ.GT.NLHIST) JJ=NLHIST
               IF (JJ.LT.0) JJ=0
               HBUF(NSTART+J) = JJ
            ENDIF
         ENDDO
      ENDIF

C     ....HBUF entries are now in terms of lines rather than freq.
  150 CONTINUE
C
      RETURN
C
C     ....Error returns
992   CALL XVMESSAGE('***No histograms requested',' ')
      NABEND=-1
      RETURN
995   CALL XVMESSAGE('***HBUF too small',' ')
      NABEND=-1
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Generate a histogram display line in BUF of length (24*LFACT+HWIDTH)*NCOL
C bytes.

      SUBROUTINE HLINE(BUF,NREPET,IRET,HBUF)
      IMPLICIT NONE
      BYTE BUF(*)
      INTEGER*4 HBUF(*)	!Histograms strung end-to-end
      INTEGER NREPET	!Number of times to repeat this display line
      INTEGER IRET	!1=last line, 0=not, -1=error

      COMMON/SV/CFACT(10),HSTART(10),NLEV(10),NOHIST,NCOL
      COMMON/SV/HWIDTH,NLHIST,NSHIST,LFTEDG,NSO,BARWID
      COMMON/SV/TICS,TICL,NLTIC,SPACEH,SPACEA,SPIKEH,ENLARG
      COMMON/SV/NLA,NCHAR,NLCHAR,NSCHAR,RESTRT,NABEND,MBUF
      INTEGER CFACT	!Compression factor (# of DNs per sample)
      INTEGER HSTART	!Index to start of each histogram in HBUF
      INTEGER NLEV	!Number of DN levels in histogram
      INTEGER NOHIST	!Total number of histograms
      INTEGER NCOL	!Number of histograms displayed horizontally
      INTEGER HWIDTH	!Number of samples for each histogram
      INTEGER NLHIST	!Number of lines used to display DN level freqs.
      INTEGER NSHIST	!Number of samps to display histogram (HWIDTH+4*NSCHAR)
      INTEGER LFTEDG	!Sample offset to left edge (1+NSCHAR)
      INTEGER NSO	!Total number of samps in hist display (NSHIST*NCOL)
      INTEGER BARWID	!Number of samples per dn level
      INTEGER TICS	!DN spacing of small tic mark along the DN axis
      INTEGER TICL	!DN spacing of large tic marks along the DN axis
      INTEGER NLTIC	!Height in lines of minor tics
      INTEGER SPACEH	!# of spacer lines between histogram rows
      INTEGER SPACEA	!# of spacer lines between annotation
      INTEGER SPIKEH 	!# of lines for spike display
      INTEGER ENLARG	!Label magnification factor
      INTEGER NLA	!Number of lines of annotation
      INTEGER NCHAR	!Number of characters per line annotation
      INTEGER NLCHAR	!Height (in lines) of character annotation
      INTEGER NSCHAR	!Width (in samples) of 1 character
      INTEGER RESTRT	!1=restart, 0=no
      INTEGER NABEND	!0=normal, -1=error
      CHARACTER*970 MBUF	!Task names
C     ....These values local to HLINE but saved in COMMON for re-entry
      COMMON/SV2/KLOW,KHIGH,NUMB,NUMC,NUMD,NUME,NUMF,NOLEFT,LINCUR,
     &	LBUF

      INTEGER*4 KLOW,KHIGH,NUMB,NUMC,NUMD,NUME,NUMF,NOLEFT,LINCUR
      CHARACTER*360 LBUF
      COMMON/SV3/ I,J,K,L,M,K1,KK,NUMWRT,NUM,LINDIF,NSP,ISAMP,NTICS,INC,
     &NTOT,NTOT3,IDN,NPOS,NDIGIT,NSLICE,NBYT,MSTART,NSTART,NSERCH
      INTEGER I,J,K,L,M,K1,KK,NUMWRT,NUM,LINDIF,NSP,ISAMP,NTICS,INC
      INTEGER NTOT,NTOT3,IDN
      INTEGER NPOS,NDIGIT,NSLICE,NBYT,MSTART,NSTART,NSERCH

      IRET=0
      IF (NABEND.EQ.-1) THEN
         IRET = -1
         RETURN
      ENDIF

C     ....If RESTRT=1, procedure is restarted from first line.
C     ....Otherwise the next line is generated.
      IF (RESTRT.NE.0) THEN
         RESTRT=0
         NREPET=1
         NUMB = 1+SPACEH+SPIKEH+SPIKEH	!Start of a row
         NUMC = NUMB+NLHIST-1		!Start of axis and tic marks for a row
         NUMD = NUMC+2*NLTIC+1		!Start of axis annotation for a row
         NUME = NUMD+SPACEA+NLCHAR	!Start of hist. annotation
         NUMF = NUME+NLA*(NLCHAR+SPACEA)!End of a row
         KLOW = 1
         KHIGH = NCOL
         LINCUR = 1			!Current line within 1 row of histograms
      ENDIF
C
   10 IF (LINCUR.GE.NUMB) GO TO 700
      LINDIF=LINCUR-1

C     ....Space between rows of histograms
      IF (LINDIF.LT.SPACEH) THEN
         CALL ITLA(0,BUF(1),NSO)
         NREPET=SPACEH
         GOTO 5000
      ENDIF

C     ....Spike dots
      IF (LINDIF.LT.SPACEH+SPIKEH) THEN
         NREPET=SPIKEH
         NSP=-NSHIST
         DO I=KLOW,KHIGH
            NSTART=HSTART(I)-1
            NSP=NSP+NSHIST
            DO K=1,HWIDTH
               IF (HBUF(NSTART+K).EQ.-1) THEN
                  ISAMP = LFTEDG+NSP+K*BARWID
                  CALL ITLA(255,BUF(ISAMP),BARWID)
                  HBUF(NSTART+K) = NLHIST
               ENDIF
            ENDDO
         ENDDO
         GOTO 5000
      ENDIF
C
C     ....Space between spike dots and highest histogram level
      NREPET=SPIKEH
      CALL ITLA(0,BUF(1),NSO)
      GO TO 5000
C
C     ....Histogram display
  700 IF (LINCUR.LT.NUMC) THEN
         LINDIF=LINCUR-NUMB
         NREPET=1
         NSP=-NSHIST
         DO I=KLOW,KHIGH
            NSP=NSP+NSHIST
            NSTART=HSTART(I)-1
            DO J=1,HWIDTH
               IF (HBUF(NSTART+J).EQ.(NLHIST-LINDIF)) THEN
                  ISAMP=LFTEDG+NSP+J*BARWID
                  CALL ITLA(255,BUF(ISAMP),BARWID)
               ENDIF
            ENDDO
         ENDDO
         GO TO 5000
      ENDIF
C
      IF (LINCUR.GE.NUMF) GO TO 2000
      LINDIF = LINCUR - NUMC

C     ....Histogram DN axis
C     ....Increment LFTEDG to account for the fact that DN values are
C     ....shifted by one. LFTEDG is decremented by same amount at the
C     ....end of this histogram column
      IF (LINDIF.LE.0) THEN
         LFTEDG=LFTEDG+BARWID
         ISAMP=LFTEDG-NSHIST
         DO I=KLOW,KHIGH
            ISAMP=ISAMP+NSHIST
            CALL ITLA(255,BUF(ISAMP),BARWID*HWIDTH)
         ENDDO
         NREPET=1
         GO TO 5000
      ENDIF

C     ....Tic marks along dn axis
      IF (LINCUR.GE.NUMD) GO TO 800
      CALL ITLA(0,BUF(1),NSO)
      KK=LFTEDG-NSHIST-1

      DO J=KLOW,KHIGH
         NTICS = NLEV(J)/TICS			!Minor tics
         INC = TICS/CFACT(J)*BARWID
         IF (LINDIF.GT.1) THEN			!Major tics
            NTICS = NLEV(J)/TICL
            INC = TICL/CFACT(J)*BARWID
         ENDIF
         KK=KK+NSHIST
         DO I=1,NTICS
            K = KK+I*INC+1
            CALL ITLA(255,BUF(K),BARWID)
         ENDDO
         DO I=1,2				!Low and high range tics
            K = KK+(I-1)*HWIDTH*BARWID+1
            CALL ITLA(255,BUF(K),BARWID)
         ENDDO
      ENDDO

      NREPET=NLTIC
      GO TO 5000

C
  800 IF (LINCUR.GE.NUME) GO TO 1000
      LINDIF=LINCUR-NUMD
      IF (LINDIF.LE.0) THEN	!Spacer lines between tics and DN numbering
         CALL ITLA(0,BUF(1),NSO)
         NREPET=SPACEA
         GO TO 5000
      ENDIF

C     ....Number long tics with DN value
      IF (LINDIF.GT.SPACEA) GO TO 850
      NTOT=NSO/NSCHAR
      do j=1,360
         LBUF(j:j)=' '
      enddo
      KK=LFTEDG-NSHIST-1

      DO I=KLOW,KHIGH
         NTICS = NLEV(I)/TICL
         INC = TICL/CFACT(I)*BARWID
         KK = KK + NSHIST
         K = KK

         DO J=1,NTICS
            K = K + INC
            IDN = J*TICL
            NPOS = (K+NSCHAR-1)/NSCHAR
            IF (IDN.GT.10) NPOS=NPOS+1
            IF (IDN.GT.1000) NPOS=NPOS+1
            NDIGIT = ALOG10(FLOAT(IDN))+1
            IF (NDIGIT.EQ.1) THEN
               WRITE (LBUF(NPOS:NPOS),'(I1)') IDN
            ELSE IF (NDIGIT.EQ.2) THEN
               WRITE (LBUF(NPOS-1:NPOS),'(I2)') IDN
            ELSE IF (NDIGIT.EQ.3) THEN
               WRITE (LBUF(NPOS-2:NPOS),'(I3)') IDN
            ELSE IF (NDIGIT.EQ.4) THEN
               WRITE (LBUF(NPOS-2:NPOS+1),'(I4)') IDN
            END IF
         ENDDO
         NPOS = (KK+NSCHAR-1)/NSCHAR
         LBUF(NPOS:NPOS) = '0'		!Write zero
      ENDDO

  850 NSLICE=(LINDIF-SPACEA)/ENLARG
      CALL TEXT(LBUF,NTOT,NSLICE,BUF,6,0)
      NBYT=6*NTOT
      IF(ENLARG.GT.1) CALL EXPAND(BUF,NBYT,ENLARG)
      NREPET=ENLARG
      GO TO 5000
C
C     ...Label annotation
 1000 IF (LINCUR.GT.NUME) GO TO 1500
      LFTEDG=LFTEDG-BARWID	!Restore to its non-incremented value
      NTOT=NSO/NSCHAR
      NTOT3=3*(NSO/NSCHAR)
      DO I=1,NTOT3
         LBUF(I:I)=' '
      ENDDO
      KK=-NSHIST+LFTEDG

      DO 1200 I=KLOW,KHIGH
      KK=KK+NSHIST		!Calculate line start
      K=(KK+NSCHAR-1)/NSCHAR
      K1=K
      MSTART=97*(I-1)
      NOLEFT=97

      DO 1100 J=1,NLA
      NSERCH=5
      IF(NCHAR.GT.20) NSERCH=10
      M=0
      DO L=1,NSERCH		!Check if line can be broken at a blank
         IF (MBUF(MSTART+NCHAR-L+2:MSTART+NCHAR-L+2).EQ.' ') THEN
            M=L-1
            GO TO 1031
         ENDIF
      ENDDO
C     ....Move annotation for this line and calculate number of characters left
 1031 NUMWRT=NCHAR-M
      IF(NOLEFT.LT.NUMWRT)NUMWRT=NOLEFT
      NOLEFT=NOLEFT-NUMWRT
      IF(NUMWRT.LT.1)GO TO 1200
      LBUF(NTOT*(J-1)+K:NTOT*(J-1)+K+NUMWRT-1)=
     &     MBUF(MSTART+1:MSTART+NUMWRT)
      MSTART=MSTART+NCHAR-M
1100  CONTINUE
1200  CONTINUE

1500  LINDIF=LINCUR-NUME
C     ....Spacer lines between lines of annotation
      IF (MOD(LINDIF,SPACEA+NLCHAR).LE.0) THEN
         CALL ITLA(0,BUF(1),NSO)
         NREPET=SPACEA
         GO TO 5000
      ENDIF
      NSLICE=MOD(LINDIF/ENLARG,(SPACEA+NLCHAR)/ENLARG)
      NSLICE=NSLICE-1
      NUM=(LINDIF+SPACEA+NLCHAR-1)/(SPACEA+NLCHAR)
      NUM=1+(NUM-1)*NTOT
      CALL TEXT(LBUF(NUM:360),NTOT,NSLICE,BUF,6,0)
      NBYT=6*NTOT
      IF (ENLARG.GT.1) CALL EXPAND(BUF,NBYT,ENLARG)
      NREPET=ENLARG
      GO TO 5000
C
C     ....Check to see if done
 2000 IF (KHIGH.GE.NOHIST) THEN
         CALL ITLA(0,BUF,NSO)
         IRET = 1
         RETURN
      ENDIF

      KLOW=KLOW+NCOL		!Reset counters for next row
      KHIGH=MIN0(NOHIST,KHIGH+NCOL)
      LINCUR=1
      GOTO 10
C
C     ....Return one line
 5000 LINCUR = LINCUR + NREPET
      IRET=0
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create flt_lab_maskv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include "xvmaininc.h"
#include "ftnbridge.h"

struct  node { char * str;  /* LINK list for output buffer */
	       struct node * next;
	     };
typedef struct node NODE;

void FTN_NAME(flt_lab_maskv)(unit,lmarg,lfact,reduce,iline,nsw,ntime,outbuf)
int *unit;	
int *lmarg, *lfact, *reduce, *iline, *nsw, *ntime;
char *outbuf; 
{ zflt_lab_maskv(unit,lmarg,lfact,reduce,iline,nsw,ntime,outbuf);
}

void  zwline (iline,nsw,buf,nline)  
int   *iline,*nsw,*nline;
char  *buf;
{
FTN_NAME(wline) (iline,nsw,buf,nline) ;
}

zflt_lab_maskv( unit,lmarg,lfact,reduce,iline,nsw,ntime,outbuf)
int *unit;		/* Unit number of file whose label is listed	      */
int *lmarg, *lfact, *reduce, *iline, *nsw, *ntime;
char * outbuf;
{NODE * list;
 int j,ntime2,length,expnd;
   expnd = *lfact - * reduce;
   ntime2 = *ntime * 2;
   list = (NODE*) flight_label(*unit);
if (list != NULL)
   while (list != NULL){
          for (j=0; j<7; j++) {
	    length = strlen(list->str);
            ztext(list->str,length,j,&outbuf[*lmarg],6,0);
	    if (*lfact - *reduce > 1) {
	             length = length * 6;
		     zexpand(&outbuf[*lmarg],length,expnd);
		                      }
	   zwline(iline,nsw,outbuf,ntime);
	                      }
	  memset(outbuf,0,*nsw);
	  zwline(iline,nsw,outbuf,&ntime2);
	  list = list->next;
	}
else return -1;
}


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create maskv.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM maskv

   To Create the build file give the command:

		$ vimake maskv			(VMS)
   or
		% vimake maskv			(Unix)


************************************************************************/
#define PROGRAM	maskv
#define R2LIB
#define MODULE_LIST maskv.f flt_lab_maskv.c 
#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define USES_C
#define FTNINC_LIST fortport
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/* #define LIB_LOCAL   /* Comment out upon delivery */

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create maskv.pdf
process help=*
 PARM INP         TYPE=STRING          COUNT=(1:10)
 PARM OUT         TYPE=STRING
 PARM SIZE        TYPE=INTEGER         COUNT=4          DEFAULT=(1,1,0,0)
 PARM SL          TYPE=INTEGER         COUNT=1          DEFAULT=1
 PARM SS          TYPE=INTEGER         COUNT=1          DEFAULT=1
 PARM NL          TYPE=INTEGER         COUNT=1          DEFAULT=0
 PARM NS          TYPE=INTEGER         COUNT=1          DEFAULT=0
 PARM FORMAT	  TYPE=KEYWORD COUNT=0:1 VALID=HALF DEFAULT=--
 PARM COMP        TYPE=KEYWORD COUNT=0:1 VALID=COMP DEFAULT=--
 PARM STRETCH     TYPE=INTEGER         COUNT=2          DEFAULT=(0,255)
 PARM EXPAND      TYPE=INTEGER         DEFAULT=1        VALID=1:4
 PARM HIST        TYPE=KEYWORD COUNT=0:2 VALID=(HIST,NOPHIST) DEFAULT=--
 PARM ID          TYPE=KEYWORD COUNT=0:1 VALID=(NOID,LOGO) DEFAULT=--
 PARM LLOGO       TYPE=INTEGER         VALID=(1:4)      DEFAULT=1
 PARM RLOGO       TYPE=INTEGER         VALID=(1:4)      DEFAULT=1
 PARM COLOR       TYPE=KEYWORD COUNT=0:1 VALID=(RED,GREEN,BLUE) DEFAULT=--
 PARM NROW        TYPE=INTEGER         DEFAULT=1 VALID=(1:9)
 PARM NCOL        TYPE=INTEGER         DEFAULT=1 VALID=(1:9)
 PARM HSIZE       TYPE=INTEGER         DEFAULT=100
 PARM HWIDTH      TYPE=INTEGER valid=(8,16,32,64,128,256) DEFAULT=128
 PARM TICS        TYPE=INTEGER valid=(2:256)   DEFAULT=16
 PARM TICL        TYPE=INTEGER valid=(40:256)  DEFAULT=128
 PARM LOG         TYPE=KEYWORD COUNT=0:1 VALID=LOG DEFAULT=--
 PARM SPIKES      TYPE=INTEGER         DEFAULT=3
 PARM BEFORE      TYPE=KEYWORD COUNT=0:1 VALID=BEFORE DEFAULT=--
 PARM LABELS      TYPE=KEYWORD COUNT=0:8 DEFAULT=-- +
	VALID=(NOLA,SQUEEZE,LABG,LABP,LABA,LABD,NOHL,NOUL,NOEL)
 PARM TASK        TYPE=KEYWORD COUNT=0:1 VALID=(TASK,NOTASK) DEFAULT="TASK" 
 PARM USER        TYPE=KEYWORD COUNT=0:1 VALID=(USER,NOUSER) DEFAULT="NOUSER" 
 PARM HISTORY     TYPE=KEYWORD COUNT=0:1 VALID=(HISTORY,NOHISTOR) +
        DEFAULT="HISTORY"  
 PARM HTITLE      TYPE=(STRING,38)     COUNT=0:1 +
        DEFAULT=MASKV        
 PARM IDENT       TYPE=(STRING,8)      COUNT=0:1   DEFAULT=--
!# parm inp(2-10) hints=default
 
!# annot function="Image Display"
!# annot keywords=(image,Mark,histogram,label,"optional annotation")
 END-PROC

.TITLE
Create an image display for film recording
.HELP
PURPOSE:

MASKV creates a display of a B/W image in a format suitable for film recording
(see program BARNE).  The display consists of the image, bordered by reference
marks, historgrams, VICAR label information, and other optional annotation.

EXECUTION:

    MASKV  INP=A  OUT=B  userparameters....
or
    MASKV  INP=(A,H1,H2,H3,...,H9)  OUT=B
where

    A is the input image (byte or halfword),
    B is the output (masked) image (byte),
    H1, H2, H3,... are up to 9 optional histogram files to be displayed with
       the image.

If histogram files are included, they must be in an IBIS format as created by
programs HISTGEN or HICCUP.

.page
 OPERATION

The MASKV output is a mask suitable for film recording.  The mask consists of
an image display, up to 9 optional histograms, VICAR labels, grey wedge, and
optional logos.  For example,
     MASKV  INP=(GRS.IMG,H1,H2,H3)  OUT=GRS.MSK  'HIST  NCOL=2  'LOGO
would create a mask with the following general layout:


 --------------------------------------  ----------------------------------
 |                                    |  |           grey wedge           |
 |                                    |  ----------------------------------
 |                                    |  ----------------  ----------------
 |                                    |  |              |  |              |
 |                                    |  |  histogram   |  |  histogram   |
 |                                    |  |     H1       |  |     H2       |
 |               image                |  |              |  |              |
 |              display               |  ----------------  ----------------
 |                                    |  ----------------  ----------------
 |                                    |  |              |  |              |
 |                                    |  |  histogram   |  |  histogram   |
 |                                    |  |     H3       |  |  of output   |
 |                                    |  |              |  |    image     |
 |                                    |  ----------------  ----------------
 -------------------------------------- 
           VICAR2 system label
           VICAR1 labels
           VICAR2 history labels

  JPL                 MIPL PIC ID 96/10/08/111223  GMY/GMY059        JPL


.page
IMAGE DISPLAY PARAMETERS:

The displayed image is bordered by reference marks, numbered every 100 pixels
(200 pixels for large character sizes).

A linear stretch may be applied to the image by specifying the STRETCH
parameter.  STRETCH=(50,200) will apply a linear transformation to the DNs so
that 50 DN is output as 0 DN, 200 DN is output as 255 DN, and all DN values
outside the range are saturated.

Specifying 'COMP will complement the image (reverse the sence of the DNs).
I.E., 'COMP has the same effect as STRETCH=(255,0).  If the STRETCH parameter
is also specified, the 'COMP is applied after the stretch.

.page
HISTOGRAM DISPLAY PARAMETERS:

The 'HIST keyword is required to display histograms.  The histogram of the
input image, together with up to 9 histograms input via histogram files may
be displayed.

The NROW and NCOL parameters control how the histograms are arranged in rows
and columns.  For example,
    MASKV  (IMG,H1,H2,H3,H4,H5) OUT  'HIST NROW=2 NCOL=3
causes the histograms to be displayed in 2 rows and 3 columns:

          H1  H2  H3
          H4  H5  IMG

The mean DN and standard deviation are displayed below the histogram.

If histogram files are supplied, these are displayed first.  The second-to-last
task in the history label of each histogram file is displayed as the title.

The histogram of the input image is computed AFTER applying any stretch and/or
COMP parameters.  If 'BEFORE is specified, the histogram is computed after
applying the stretch, but before applying COMP.  The HTITLE parameter may be
used to provide a title.  The keyword 'NOPHIST may be used to suppress the
histogram display of the input image.

HSIZE and HWIDTH specify the length of the frequency and DN axes of the
histogram (default is 100 x 128).

Specifying 'LOG causes the frequency axis to be displayed using a logrithmic
scale.

The SPIKES parameter is used to normalize the frequency scale by specifying the
number of values to saturate.  Saturation is indicated by a "dot" above the
plotted value.

TICL and TICS specify the DN separation between the long and short tic marks
along the DN axis (default is 128 and 16).

.page
VICAR LABEL DISPLAY PARAMETERS:

The VICAR labels are displayed in the following order: (1) VICAR2 system
label, (2) VICAR1 labels, (3) VICAR2 task and user labels.  The 'NOLA
keyword suppresses all label displays.

VICAR1 labels are obsolete IBM VICAR labels and are described in the next
section.

All VICAR2 labels beginning with the keyword TASK are processed, displaying
the task name and processing parameters.  The keyword 'NOHISTOR will suppress
the display of processing parameters.  The keyword 'NOTASK will suppress the
display of all TASK labels completely.

The keyword 'USER will enable the display of user labels.

The keyword 'SQUEEZE will attempt to squeeze the labels together to decrease
the amount of real estate it takes up on film.

Labels are normally displayed using characters which are a 7 lines by 6 samples
in size.  The EXPAND=n parameter can be used to magnify the characters by a
factor of n.  If the character size specified using the EXPAND parameter does
not allow a full label on one line, the character size will be reduced.
Note that ALL annotation in the mask is effected by this parameter.

Any label that exceeds 70 characters will be truncated to 70 characters
on the hard copy.

Finally, the IPL ID and optional logos are displayed.  The ID contains the
date and time of processing as well as the user id and job name.  The keyword
'NOID suppresses the IPL ID display.  The keywords 'LOGO, 'LLOGO, and 'RLOGO
enable the display of both, left, or right logos.

.page
VICAR1* LABELS:

VICAR1* labels mimic the format of old IBM Vicar labels, and occur only if the
image was read in from an IBM tape using program CONVIM.  VICAR1 labels consist
of one or more 72 character labels as in the following example:
 
LAB01 '77                  800 800 800 800 L1       SC'
LAB02 'C       10 27 81 13:47:26   FFM931           GC'
LABO3 'IN HALF RANGE 0 1 OUT BYTE RANGE 0 1         PC'
LAB04 'ZFILL: NLW=1 NSW=1 REPLACE=1                  L'

The first label is the IBM system label which is never displayed by MASKV.
(MASKV only displays the Vicar2 system label).

VICAR1 labels conform to the following rules:
  
If character #72 is a C, then another VICAR1 label line follows:
If character #72 is an L, it indicates the last label line.
 
the keywords 'LABG, 'LABP, 'LABA, 'LABD, 'NOHL, 'NOUL, and 'NOEL enable or
suppress the display of one or more types of VICAR1 labels.  Character 71
of each VICAR1 label identifies the label type:

CHAR.71   TYPE                MASKV ACTION
 ----------------------------------------------------------------------
  BLANK   ANY                 Displayed unless "NOUL" specified
  S       SYSTEM              Never displayed
  G       HISTORY1            Displayed if "LABG" specified
  H       HISTORY2            Displayed unless "NOHL" specified
  P       PARAMETERS          Displayed if "LABP" specified
  U       USER ANNOTATION     Displayed unless "NOUL" specified
  A       USER ANNOTATION     Displayed if "LABA" specified
  E       PROGRAM GENERATED   Displayed unless "NOEL" specified
           (CHARACTER)
  D       PROGRAM GENERATED   Displayed if "LABD" specified
           (NON-CHARACTER)

.page
 RESTRICTIONS:

 "LAB" followed by any digits is a reserved keyword for Vicar1*
 history labels (LAB01, LAB02, etc.); see below for further
 desription.  Any Vicar2 user or program history labels using a
 keyword of this form will not be displayed by MASKV.

.page
 EXAMPLE:

 The following text displays a typical Vicar2 label, as output by
 program LABEL-LIST, and then shows how it will appear in the
 output of MASKV.

 LABEL-LIST A.DAT

       ********** FILE A.DAT ************
             2 DIMENSIONAL IMAGE FILE
             FILE ORGANIZATION IS BSQ
             PIXELS ARE IN BYTE FORMAT  (VICAR2 SYSTEM LABEL)
             800 LINES
             800 SAMPLES PER LINE

-------- TASK:CONVIM -- USER: FFM059 -- WED FEB 6 1:10:00 1985 ---
LAB01
'77                  800 800 800 800 L1       SC' (IBM SYSTEM LABEL)
LAB02
'C       10 27 81 13:47:26   FFM931           GC' (VICAR1 G CATEGORY LABEL)
LABO3
'IN HALF RANGE 0 1 OUT BYTE RANGE 0 1         PC' (VICAR1 P CATEGORY LABEL)
LAB04
'ZFILL: NLW=1 NSW=1 REPLACE=1                  L'
-------- TASK:COPY -- USER: FFM059 -- FRI FEB 6 0:00:00 1985 ---
COMMENT='THIS IS A TEST FILE'          (VICAR2 HISTORY LABEL)
COMMENT='HOW ARE YOU'

.page
The following execution of MASKV will be used:

    maskv A B 'LABP 'NOTASK 'USER 

The labels will be displayed as follows:

       FORMAT=BYTE NS=800 NL=800 TYPE=IMAGE
       IN HALF RANGE 0 1 OUT BYTE RANGE 0 1 
       WED FEB 6 1:10:00 1985 USER=FFM059
       FRI FEB 6 0:00:00 1985 USER=FFM059
       COMMENT=THIS IS A TEST FILE
       COMMENT=HOW ARE YOU

.page
NOTE:  MASKV may allocate more disk space than necessary if there are not many
labels in the input data.  The user can release the unused blocks by the 
DCL command "copy" or USH command "cp" after MASKV is completed.

.page
USING MASKV TO DISPLAY COLOR IMAGES:

The following sequence of executions will create masks of a color triplet:

	   MASKV  RED  MRED  'HIST  'RED
	   MASKV  GRN  MGRN  'HIST  'GREEN
	   MASKV  BLU  MBLU  'HIST  'BLUE

The keywords 'RED, 'GREEN, and 'BLUE control the position of the histogram in
the output display.  'RED will display the histogram in the first row,  'GREEN
will display the histogram in the second row, and 'BLUE will display the
histogram in the third row (see NROW parameter).

If 'RED, 'GREEN, and 'BLUE are specified, secondary histogram files should not
be input (i.e. no H1, H2, etc.), and the parameters NROW and NCOL should not be
used.

.page
 History:

 ORIGINAL PROGRAMMER:  John H. Reimer,  10 MAY 1982
 CONVERTED TO VAX BY:  F. F. Moss,  20 JAN 1984
 CURRENT COGNIZANT PROGRAMMER: F. F. Moss.

 08/30/2000 ..AXC....  Removed redundant call to subroutine DATFMT.
                       Modified subroutines DATFMT & GTPRCS. (AR-104622)
 06/30/1998 ..TXH....  Modified outputs to be have 4-digit year dates.
 12/12/96   ..OAM....  Fixed code to return the complement of an image 
                       only when COMP is specified. (FR 89921).
                       Included valid ranges for HTITLE , TICS and TICL 
                       in maskv.pdf.  (FR 89868).  
 10/10/96   ..OAM....  In HLINE, declared INSTANCES as an array of integers to 
                       avoid memory overwriting. Included a condition to
                       handle the case in which NCOL or NROW > number of 
                       available histograms.(FR89852). Updated help file. 
 8/14/96    ..OAM....  Modified the format of the histogram annotations.
                       Included COMMON statements to keep the value of static
                       variables after each call of HLINE (FR89844). 
                       Modified to call ibis_signal_u instead of ibis_signal 
                       after the open statement (FR89846). 
                       Included valid values for HWIDTH in the pdf. 
                       Modified to print none, one or two logos at the time
                       (FR89845). Modified the control flow of the
                       program when processing GLL images and NOTASK is 
                       specified (FR89847).  
 8/2/96     ..OAM....  Declared variables REPEAT and INUM using the
                       COMMON statement to  save their values between
                       LINTIC calls.  Required to work on the SGIs.(FR89838).
 7/19/96    ..0AM....  Modified flt_lab_maskv to be an inteface with 
                       the subroutine flight_label for Galileo SSI images. 
                       Corrected Voyager related code to handle text 
                       wrap around correctly. (FR 89325). 
 6/19/96    ..OAM....  Included flt_lab_maskv code to reformat the  
                       VICAR label information.(FR 89376).
                       Modified the Fortran bridge of ztext.c to
                       pass the entire string even if padded with             
                       blanks on the right. 
 3/6/1995           -  AS (CRI) Made portable for UNIX
 6/7/1994  FR 85164 -  F. Moss Remove link to DATATRIEVE & VRDI.
 6/25/1993 FR 81727 -- M. O'Shaughnessy. Added valid range to EXPAND parm
	               to prevent too-large text from vanishing from the
		       NL/NS annotation.
.LEVEL1
.VARIABLE INP
 INPUT DATA SET
.VARIABLE OUT
 OUTPUT DATA SET
.VARIABLE SIZE
 IMAGE SIZE
.VARIABLE SL
 STARTING LINE
.VARIABLE SS
 STARTING SAMPLE
.VARIABLE NL
 NUMBER OF LINES
.VARIABLE NS
 NUMBER OF SAMPLES
.VARIABLE FORMAT
 DATA FORMAT IDENTIFIER
 VALID: HALF
.VARIABLE COMP
 COMPLEMENT THE INPUT IMAGE
 VALID: COMP
.VARIABLE STRETCH
 APPLY A LINEAR STRETCH
 TO THE INPUT IMAGE
.VARIABLE EXPAND
 ENLARGE THE CHARACTERS
 IN THE OUTPUT
.VARIABLE HIST
 HISTOGRAMS DISPLAYED
 VALID: HIST,NOPHIST
.VARIABLE ID
 ID'S DISPLAYED
 VALID: NOID,LOGO
.VARIABLE LLOGO
 DISPLAY LOGO ON THE LEFT
 SIDE IN THE OUTPUT
.VARIABLE RLOGO
 DISPLAY LOGO ON THE RIGHT
 SIDE IN THE OUTPUT
.VARIABLE COLOR
 Specifies the color of an
input image (one of a color
triplet).
.VARIABLE NROW
 NUMBER OF HISTOGRAMS DISPLAYED
 VERTICALLY
.VARIABLE NCOL
 NUMBER OF HISTOGRAMS DISPLAYED
 HORIZONTALLY
.VARIABLE HSIZE
 NUMBER OF LINES TO DISPLAY DN
 LEVEL FREQUENCIES FOR HISTOGRAM
.VARIABLE HWIDTH
 THE WIDTH OF EACH HISTOGRAM
.VARIABLE TICS
 THE INTERVAL OF SMALL TIC MARKS
 ALONG THE HISTOGRAM DN AXIS
.VARIABLE TICL
 THE INTERVAL OF LARGE TIC MARKS
 ALONG THE HISTOGRAM DN AXIS
.VARIABLE LOG
 LOGARITHMIC SCALE OF DN LEVEL
 FREQUENCIES
 VALID: LOG
.VARIABLE SPIKES
 TO SATURATE SOME HIGHEST DN
 LEVEL FREQUENCIES
.VARIABLE BEFORE
 COMPUTE THE HISTOGRAM BEFORE
 COMPLEMENTING THE IMAGE
 VALID: BEFORE
.VARIABLE LABELS
 VICAR1 LABEL DISPLAY 
 VALID: NOLA,LABG,LABP,LABA,
 LABD,NOHL,NOUL,NOEL,SQUEEZE
 (SPECIFY 0-8)
.VARIABLE TASK
 DISPLAY TASK NAME FROM THE
 VICAR2 LABEL
.VARIABLE USER
 DISPLAY USER NAME AND DATE-
 TIME FROM THE VICAR2 LABEL
.VARIABLE HISTORY
 DISPLAY VICAR2 HISTORY
 LABEL
.VARIABLE HTITLE
 HISTOGRAM TITLE
.VARIABLE IDENT
 USER-SPECIFIED HARDCOPY
 IDENTIFIER

.LEVEL2
.variable inp
  Ex:  INP=A
    or INP=(A,H1,H2,H3,...,H9)

Input files, consisting of the input image A, optionally followed by up to 9
histogram files (H1, H2, H3,...) to be displayed with the image.

If specified, the histogram files must be an IBIS file as created by programs
HISTGEN or HICCUP.

.variable out
The output image display.  The file is in byte data format, and is suitable
for film recording (via program BARNE).

NOTE:  MASKV may allocate more disk space than necessary if there are 
few labels in the input data. The user can release the unused blocks by the
DCL command "copy" or USH "cp" after MASKV is completed.

.variable format
Keyword: valid value = HALF.
'HALF specifies that the input image is halfword. (Default is a byte)

.variable comp
COMP specifies that the input image is to be complemented (i.e. the sense of
light and dark pixels is reversed).  For byte images, for example, 0 dn becomes
255 dn, 1 dn becomes 254 dn, etc.

.variable stretch
 STRETCH=(N1,N2) specifies that a linear stretch will be applied to the input
image so that N1 dn maps to 0 dn and N2 dn maps to 255 dn, and all dn values
outside this range is saturated.

.variable expand
EXPAND=N scales the size of displayed annotation by N.  Characters normally have
a height of 7 lines and a width of 5 samples.  EXPAND=2 would output 14x10
characters.

NOTE:  To compute how much image area is required to display text, add 2N
samples spacing between characters and between text lines.

.variable hist
 Keyword: valid values are HIST and NOPHIST.
 (Zero, one, or both may be specified)

The 'HIST keyword is required to display histograms.  The histogram of the
input image, together with up to 9 histograms input via histogram files may
be displayed.  For example,
    MASKV  (IMG,H1,H2,H3,H4,H5) OUT  'HIST NROW=2 NCOL=3
causes the histograms to be displayed in 2 rows and 3 columns:

          H1  H2  H3
          H4  H5  IMG

The 'NOPHIST keyword suppresses the display of the input image (IMG).

MASKV will compute the histogram of the input image after applying any
STRETCH or COMP to the image.  If 'BEFORE is specified, the histogram is
calculated before the COMP is applied.

.variable id
 Keyword: valid values are NOID and LOGO.
 (Zero or one may be specified)

.vari NOID 
 NOID specifies that IPL id labels will not be displayed in the
 output. (Default is to display them)

.vari LOGO 
 LOGO specifies that the JPL Logo will be displayed to both sides
 of the IPL id labels. (Default is not to display the logo)

.variable llogo
 LLOGO=I1 specifies a left logo. Values for I1 and corresponding
 logos are:
                  1 = JPL logo
                  2 = Mickey Mouse logo
                  3 = Goddard logo
                  4 = NASA logo
 Default is that no logos are displayed
.variable rlogo
 RLOGO=I2 is similar to keyword LLOGO except that this keyword
 specifies a right logo. Default is that no logos are displayed.
.variable color
The following sequence of executions will create masks of a color triplet:

	   MASKV  RED  MRED  'HIST  'RED
	   MASKV  GRN  MGRN  'HIST  'GREEN
	   MASKV  BLU  MBLU  'HIST  'BLUE

The keywords 'RED, 'GREEN, and 'BLUE control the position of the histogram in
the output display.  'RED will display the histogram in the first row,  'GREEN
will display the histogram in the second row, and 'BLUE will display the
histogram in the third row (see NROW parameter).

If 'RED, 'GREEN, and 'BLUE are specified, secondary histogram files should not
be input (i.e. no H1, H2, etc.), and the parameters NROW and NCOL should not be
used.
.variable nrow
 NROW=N4 specifies that histograms are to be displayed in N4 rows.
 (Default is N4 = number of histograms.)

 NCOL * NROW <= NI
.variable ncol
 NCOL=N5 specifies that histograms are to be displayed in N5 columns.
 (Default is N5=1)

 NCOL * NROW <= NI
.variable hsize
 HSIZE=N6 specifies that each histograms will use N6 lines to display
 DN level frequencies. (Default is n6=100)
.variable hwidth
 HWIDTH=N7 specifies that each histogram will be N7 samples wide.
 (Default is N7=128)
.variable tics
 TICS=N8 specifies that small tic marks will be drawn every N8 DN
 along the histogram dn axis.
 (Default is N8=16)
.variable ticl
 TICL=N9 specifies that large tic marks will be drawn every N9 DN
 along the histogram dn axis and labeled with
 the corresponding dn value. (Default is N9=128)
.variable log
 Keyword: valid value is LOG.
 
 This specifies that logarithmic scaling is to be used in scaling
 the DN level frequencies.  (Default is to use linear scaling.)
.variable spikes
 SPIKES=N11 specifies that the N11 greatest DN level
 frequencies of each histogram are to be saturated.
 (Default is N11=3)
.variable before
 Keyword: valid value is BEFORE.
 
 This specifies that the histogram of the input image is to be
 computed prior to complementing the image. This keyword
 is only applicable if COMP is also specified.
 
 (Default is to compute the histogram after complementing the image)
.variable labels
 Keyword: valid values: NOLA,LABG,LABP,LABA,LABD,NOHL,NOUL,NOEL,SQUEEZE.
 (Zero through eight keywords may be specified)

 This keyword primarily controls display of Vicar1 labels.  For a
 description of Vicar1 label types and formats, see HELP MASKV.

 NOLA:  Specifies that none of the Vicar1 or Vicar2 labels will be
 displayed. This keyword works independently of keyword NOID.

 The remaining keywords control the display of Vicar1 label only:

 LABG:  specifies that Vicar1 labels of category 'G' are displayed.
 LABP:  specifies that Vicar1 labels of category 'P' are displayed. 
 LABA:  specifies that Vicar1 labels of category 'A' are displayed.
 LABD:  specifies that Vicar1 labels of category 'D' are displayed. 
 NOHL:  specifies that Vicar1 labels of category 'H' are not
   displayed.  Default is to display Vicar1 'H' category labels.
 NOUL:  specifies that Vicar1 labels of category 'U' are not displayed.
 NOEL:  specifies that Vicar1 labels of category 'E' are not displayed.
 SQUEEZE:  specifies that as many Vicar1 labels as possible will be
 displayed on a single line.  (Default is to display one Vicar1 label
 per line)
.VARIABLE TASK
 If TASK is specified, MASKV searches for all occurrences of
 keyword 'TASK' in the Vicar2 history labels and then displays
 the values of 'TASK'.
 
 This parameter does not affect the Vicar1 label display, see
 parameter LABELS.
 
 TASK is independent of the keywords USER and HISTORY.
 NOTASK or default supresses the display of the task name.
.VARIABLE USER
 If USER is specified, MASKV searches for all occurrences of
 keywords 'DATETIME' and 'USER' in the Vicar2 history labels
 and then displays all of their respective values.
 
 This parameter does not affect the Vicar1 label display, see
 parameter LABELS.
 
 USER is independent of the keywords TASK and HISTORY.
 NOUSER or default supresses the display of the user name and
 date-time.
.VARIABLE HISTORY 
 If HISTORY is specified, then all Vicar2 history labels will be
 printed in the order in which they are found, excepting those
 with keywords 'TASK', 'USER', and 'DATETIME'. 
 
 This parameter does not affect the Vicar1 label display, see
 parameter LABELS.
.VARIABLE HTITLE
 Title for histograms.
.VARIABLE IDENT
 This parameter allows the user to supply an identifier for the hardcopy
 product.  If it is defaulted, the process name is used.

 In the current (August 1985) implementation of TAE/Vicar, the process
 name of a batch job is of the form "_JOBxxxx1", e.g. _JOB20191.  (The
 "_" character will be translated to "$" in the hardcopy product.) As
 this is not very informative, the user will want to supply something
 using this parameter.  A planned enhancement to TAE will make the 
 process name be identical to the proc name, i.e., the name of the PDF
 which was submitted to batch.

 For interactive jobs, the process name under TAE is the userid followed
 by a "1", e.g. FFM0591.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmaskv.pdf
procedure
refgbl $autousage
refgbl $echo
refgbl $syschar
Local  path1 TYPE=STRING init="wms_test_work:[testdata.mipl.vgr]"
Local  path2 TYPE=STRING init="wms_test_work:[testdata.gll]"
Local  path3 TYPE=STRING init="wms_test_work:[testdata.y2k]"

body

let $autousage="none"
let _onfail="continue"
let $echo="yes"
if  ($syschar(1) = "UNIX")
       let path1 = "/project/test_work/testdata/mipl/vgr/"
       let path2 = "/project/test_work/testdata/gll/"
       let path3 = "/project/test_work/testdata/y2k/"
end-if

write "THIS IS A TEST OF PROGRAM maskv"
! THIS IS A TEST SCRIPT FOR THE PROGRAM - maskv
! TEST DEFAULT PARAMETERS
gen out=aa.img NL=500 NS=200
label-list aa.img

maskv aa.img cc.img
list cc.img (19,119,20,15)  

! TEST THE PARAM 'SIZE'
maskv cc.img c1.img SIZE=(1,1,100,100)

! GENERATE 3 HISTOGRAMS FOR THE SECOND,THIRD AND FOURTH INPUTS
hiccup aa.img h1.img
!the next test should call abend.
maskv (aa.img, h1.img, h2.img, h3.img) c1_1.img +
	Expand=1 'hist nrow=2 ncol=2 hsize=150 tics=16 +
        ticl=128 'log spikes=1

ibis-copy h1.img h2.img
ibis-copy h2.img h3.img
label-list h1.img
label-list h2.img
maskv (aa.img, h1.img, h2.img, h3.img) c1_1.img +
	Expand=1 'hist nrow=2 ncol=2 hsize=150 tics=16 +
        ticl=128 'log spikes=1

!TEST FR89852
maskv aa.img c1_2.img EXPAND=1 'HIST NROW=2 NCOL=2 HSIZE=150 TICS=16

! TEST THE PARMS 'COMP' AND 'HIST' WHILE 'NOPHIST' AND 'BEFORE' 
! ARE NOT SPECIFIED
maskv (aa.img, h1.img, h2.img, h3.img) c2.img 'HIST 'COMP STRETCH=(52,102)

! TEST THE PARMS 'COMP' AND 'HIST' WITH THE 'BEFORE' SPECIFIED
maskv (aa.img, h1.img, h2.img, h3.img) c3.img 'HIST 'COMP 'BEFORE +
   STRETCH=(-10,75)

! TEST THE PARMS 'COMP' AND 'HIST' WITH THE 'NOPHIST' SPECIFIED
maskv (aa.img, h1.img, h2.img, h3.img) c4.img 'COMP '(HIST,NOPHIST) +
   STRETCH=(102,51)

! TEST THE HISTOGRAM PARAMTERS
maskv (aa.img, h1.img, h2.img, h3.img) c5.img EXPAND=1 'HIST NROW=2 +
   NCOL=2 HSIZE=150 TICS=16 TICL=128 'LOG SPIKES=1

! TEST PARMS 'NOID' AND 'NOLA'
maskv aa.img c6.img 'HIST 'NOID 'NOLA

! TEST HALFWORD IMAGE
! GENERATE AN 800 BY 800 HALFWORD IMAGE TO ENSURE THAT
! maskv ALLOWS FULL RESOLUTION DICO PLAYBACK OF VOYAGER 
! FRAME WITH LABELS AND HISTOGRAMS
gen out=bb.img NL=800 NS=800 'HALF
label-list bb.img
maskv (bb.img, h1.img) c7.img 'HIST STRETCH=(0,1000) 'HALF

! TEST LABEL PARAMETERS
label-list &"path1"f2048013.byt
maskv &"path1"f2048013.byt  c8.img '(LABP,LABA,LABD,LABG)

maskv  &"path2"test_image_ict.udr ict.udr
LABEL-LIST  &"path2"test_image_ict.udr	   

maskv  &"path2"test_image_barc.udr barc.udr
LABEL-LIST  &"path2"test_image_barc.udr	   

maskv  &"path2"test_image_lossless.udr  lossless.udr 
LABEL-LIST  &"path2"test_image_lossless.udr  

!placing a valid value for filter
label-list &"path2"s1677721400.3
label-replace &"path2"s1677721400.3 test.out "FILTER=3" TASK="TASK"
maskv test.out img.out

!adding stars
label-add test.out  star5.out "NSTARS = 5,+
STAR1=(231,165,12,894),STAR2=(21,32,187,23),STAR3=(5,10,15,20),+
STAR4=(1,2,3,5),STAR5=(9,4,8,3)" TASK="TASK"
maskv star5.out  img5.out
label-list star5.out

label-replace star5.out star4.out "NSTARS = 4" TASK="TASK"
maskv star4.out img4.out rlogo=2 'squeeze

label-replace star5.out star3.out "NSTARS = 3" TASK="TASK"
maskv star3.out img3.out rlogo=1 llogo=3

label-replace star5.out star2.out "NSTARS = 2" TASK="TASK"
maskv star2.out  img2.out 'logo  'hist

label-replace star5.out star1.out "NSTARS = 1" TASK="TASK"
maskv star1.out  img1.out 'user llogo=4

label-add star5.out  star.out "RAD = 10.22" TASK="TASK"
label-replace star.out stars.out "TARGET = RING" TASK="TASK"
maskv stars.out img6.out 'notask htitle="this is a title" +
      ident="12345678" 'hist

write "The followings are prepared for the Year 2000 tests."

maskv &"path3"scet_jan_01_2000.img scet_jan_01_2000.maskv +
   htitle="Jan. 01, 2000 Test Output" 'hist

maskv &"path3"scet_mar_01_2000.img scet_mar_01_2000.maskv +
   htitle="Mar. 01, 2000 Test Output" 'hist

maskv &"path3"scet_dec_31_2000.img scet_dec_31_2000.maskv +
   htitle="Dec. 31, 2000 Test Output" 'hist

maskv &"path3"scet_jan_01_2001.img scet_jan_01_2001.maskv +
   htitle="Jan. 01, 2001 Test Output" 'hist

maskv &"path3"scet_mar_01_2001.img scet_mar_01_2001.maskv +
   htitle="Mar. 01, 2001 Test Output" 'hist

maskv &"path3"scet_dec_31_2001.img scet_dec_31_2001.maskv +
   htitle="Dec. 31, 2001 Test Output" 'hist

write "Please verify the output images."

if  ($syschar(1) = "UNIX")
   ush rm *.img
   ush rm *.udr
   ush rm *.out
else
   dcl del *.img;*
   dcl del *.udr;*
   dcl del *.out;*
end-if

end-proc

$ Return
$!#############################################################################
