$!****************************************************************************
$!
$! Build proc for MIPL module otf1
$! VPACK Version 1.9, Wednesday, February 02, 2005, 10:28:52
$!
$! Execute by entering:		$ @otf1
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
$ write sys$output "*** module otf1 ***"
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
$ write sys$output "Invalid argument given to otf1.com file -- ", primary
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
$   if F$SEARCH("otf1.imake") .nes. ""
$   then
$      vimake otf1
$      purge otf1.bld
$   else
$      if F$SEARCH("otf1.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake otf1
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @otf1.bld "STD"
$   else
$      @otf1.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create otf1.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack otf1.com -mixed -
	-s otf1.f -
	-i otf1.imake -
	-p otf1.pdf -
	-t tstotf1.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create otf1.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C-------PROGRAM OTF1
C 05 AUG 1997 ...TXH... Ported from VAX to MIPS Env.  Merged changes
C                       made by CCA and the previous MIPS version.
C 10 JUL 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
         COMMON/C1/R,A,REALV,AIMAGV,WTS,AMPL9,WSFT9,F29,PPLOT,
     &             CPLOT,AMPL10,PARMS,OBUF

         COMPLEX*8 A(2050)

         REAL*4 R(1026),F(1026),PARMS(512),WTS(512),AMPL9(131)
         REAL*4 CPLOT(131),PPLOT(131),AMPL10(131),WSFT9(129)
         REAL*4 F29(129),REALV(129),AIMAGV(129),C1,C2,X,Y,RINL
         REAL*4 AMPLSUM,NORMSUM,DENM,CENTR,CEN,SUM1,SUM,PHASE
         REAL*4 VAL,PI,FTNORM,OBUF(400)

         INTEGER*2 IBUF(4096)
         INTEGER CNT,IUNI,STAT,OUNI,OSTAT,ZOOMFLAG
         INTEGER NUMBER_OF_LINES_WITHOUT_EDGE,NOPRT,STATUS

         CHARACTER*256 PLOT_DS
         CHARACTER*80  HEADR(70),IMESS
         CHARACTER*40  INFILE,OUTFILE,TBL
         CHARACTER*121 LO
         CHARACTER*140 ZPBUF
         CHARACTER*9   BL
         CHARACTER*101 LOG
         CHARACTER*1   LOGR,LOGI,LOGA,LOGW,BLNK,ONE,TAB
         CHARACTER*31  MPHASE
         CHARACTER*37  MCYCLE

         LOGICAL XVPTST

         DATA PI/3.1415926536E0/

         DO I = 1,101
            LOG(I:I) = '1'
         END DO

         DO I= 1,121
            LO(I:I) = '1'
         END DO
         LO(1:20) = '                    ' 

         BL=' '
         LOGR='R'
         LOGI='I'
         LOGA='A'
         LOGW='W'
         BLNK=' '
         ONE='1'
         IMESS='INTEGRATED MTF AMPLITUDE FROM .25 TO .48 = '
         MPHASE='PHASE (RAD) multiplied *'
         MCYCLE='CYCLES PER SAMPLE multiplied *' 
         CALL IFMESSAGE('OTF1 version 05-AUG-1997')

         CALL XVTRANS_SET(TRANBUF,'HALF','REAL',STAT)
         IF (STAT .NE. 1) CALL MABEND('BUFFER SETUP UNSUCCESSFUL')

C**************************
C     OPEN INPUT IF THERE, AND GET PROCESSING AREA BRANCH TO 200 IF INPUT 
C     IS LSF OR DATA OPEN OUTPUT IF NEEDED
C**************************
         IPIC=0
         ISL=0
         ISS=0
         INL=0
         INS=0
         INLINES=0
         INSAMPS=0
         LINLAS=0
         LINO=0
         CALL XVUNIT (IUNI,'INP',1,INSTAT,' ')
         IF (INSTAT .NE. 1) GOTO 200
         CALL XVP('INP',INFILE,ICNT)
         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &            'U_FORMAT','HALF',' ')
         IPIC=1
         CALL XVSIZE(ISL,ISS,INL,INS,INLINES,INSAMPS)
         IF (INL .GT. INLINES)
     &      CALL XVMESSAGE ('INL TOO LARGE!!!!',' ')
         IF (INL .GT. INLINES) INL=INLINES
         IF(INS .GT. INSAMPS)
     &      CALL XVMESSAGE ('INS TOO LARGE!!!!',' ')
         IF(INS .GT. INSAMPS) INS=INSAMPS
         INSM1=INS-1      !THIS IS NEEDED FOR LSF COUNTS
         LINO=ISL
         LINLAS=ISL+INL-1
         IF (LINLAS .GT. INLINES)
     &      CALL MABEND ('TOO MANY LINES!!!!')
         IF (INS-ISS .GT. INSAMPS)
     &      CALL MABEND ('TOO MANY SAMPS!!!!')
         ISSO=ISS
9        CONTINUE
200      CONTINUE               !BRANCH TO HERE FOR NON IMAGE INPUT

C***************************
C     SET DEFAULTS AND INIT BUFFERS
C***************************
         DO 1 I=1,129
            REALV(I)=0.0E0
1           AIMAGV(I)=0.0E0      
         DO 2 I=1,4096
2           IBUF(I)=0
         DO 3 I=1,4488
3           R(I)=0.0E0      
         AMPLSUM=0.0E0
         NORMSUM=0.0E0
         COUNT=0.0
         NPTSIN=0
         KEY=0         !KEY=1=REFLECT DATA/KEY=2=MEAN FILLIN
         KEY4=1        !KEY4=1=NORMALIZE /KEY4=0= DON'T
         ILSF=0
         IDATA=0
         IPRINT=0
         IPLOT=0
         ITST=0
         NUMBER_OF_LINES_WITHOUT_EDGE=0
         CALL XVP('INTERVAL',TIMINT,CNT)
         CALL XVP('NOISE',NOISE,CNT)

C*************************************
C     VICAR PARAM PROCESSOR
C*************************************
         IF (XVPTST('DIAG')) IPRINT=1
         CALL XVP ('PLOT',PLOT_DS,CNT)
         CALL PLOTFN (PLOT_DS)
         IF (CNT .NE. 0) IPLOT=1
         CALL XVPARM ('PZOOM',PZOOM,CNT,IDEF,1)
         IF (IDEF .EQ. 0) THEN
             ZOOMFLAG=1
         ELSE
             ZOOMFLAG=0
         ENDIF
         CALL XVP('LSF',PARMS,CNT)     !INPUTS LSF REAL*4 DATA
         IF (CNT .EQ. 0) GOTO 7
         NPTSIN=CNT
         DO 4 I=3,12
            I1=I
            IF (NPTSIN .LE. 2**I) GOTO 5
4        CONTINUE
5        NTOT=2**I1
         DO 6 I=1,NPTSIN
6           R(I)=PARMS(I)
         IPOWER=I1
         ILSF=1
         LINO=1
7        CALL XVP ('DATA',PARMS,CNT)    !INPUTS RAW REAL*4 DATA
         IF (CNT .EQ. 0) GOTO 107
         NPTSIN=CNT
         DO 104 I=3,12
            I1=I
            IF (NPTSIN .LE. 2**I) GOTO 105
104      CONTINUE
105      NTOT=2**I1
         INS=NPTSIN
         INSM1=INS-1
         IPOWER=I1
         IDATA=1
         LINO=1
         DO 106 I=1,NPTSIN
106         R(I)=PARMS(I)
         LEFTL=0
         IF (R(1) .GT. R(INS)) LEFTL=1  !FIND DIRECTION OF GRADIENT
         IF (LEFTL .EQ. 1) GOTO 500
C****************************
C     DIFFERENTIATE TO GET LSF
C****************************
         IF (IPRINT .EQ. 1)
     &      CALL PRNT(4,1,INSM1,'NUMBER OF ELEMENTS IN LSF= ') 
         DO 501 I=1,INSM1
501         R(I) = R(I+1)-R(I)
         GOTO 503
500      DO 502 I = 1,INSM1
502         R(I) = R(I)-R(I+1)
503      CONTINUE
107      IF (XVPTST('REFLECT')) KEY=1
         IF (XVPTST('MEAN')) KEY=2
         IF (XVPTST('NONORMAL')) KEY4=0
         IF (XVPTST('SINCTST')) ITST=1
         IF (ITST .EQ. 0) GOTO 504        !TEST WITH SINC
         LINO=1
         NPTSIN=60
         INS=NPTSIN
         INSM1=INS-1
         ILSF=1
         NTOT=64
         IPOWER=6
         DO 505 I=1,60
            DENM=(2.0E0*PI*(FLOAT(I)-30.5E0)/5.0E0)
            R(I)=(SIN(DENM)/DENM)**2   !TAB LSF
505      CONTINUE 
504      CONTINUE
C******************************
C     BEGIN DATA ACQUISITION
C******************************
         IF (IPIC .EQ. 1) GOTO 10        !IMAGE INTEGER INPUT
         IF (IDATA .EQ. 1) GOTO 24       !PAR STRING REAL INPUT
         IF (ILSF .EQ. 1) GOTO 24        !PAR STRING LSF INPUT
10       CONTINUE
11       CONTINUE
         CALL XVREAD(IUNI,IBUF,STAT,'LINE',LINO,'SAMP',ISSO,
     &               'NSAMPS',INS,' ')
         IF (IPRINT .EQ. 0) GOTO 99
         CALL PRNT(2,INS,IBUF,'IBUF=. ')
         DO 100 I=1,INS                                
         CALL PRNT(4,1,I,'I=. ')
100      CALL PRNT(2,1,IBUF(I),'IBUF(I)=. ')
99       CONTINUE
C***************************
C     FIND LINE SPREAD FUNCTION
C***************************
         LEFTL=0
         IF (IBUF(1) .GT. IBUF(INS)) LEFTL=1  !FIND DIRECTION OF GRADIENT
         IF (LEFTL .EQ. 1) GOTO 13
C****************************
C     DIFFERENTIATE TO GET LSF
C****************************
         IF (IPRINT .EQ. 1) 
     &      CALL PRNT(4,1,INSM1,'NUMBER OF ELEMENTS IN LSF= ') 
         DO 12 I = 1,INSM1
12       IBUF(I)=IBUF(I+1)-IBUF(I)
         GOTO 15
13       DO 14 I=1,INSM1
14          IBUF(I)=IBUF(I)-IBUF(I+1)
15       CONTINUE
         IF (IPRINT .EQ. 0) GOTO 98
         DO 101 I=1,INSM1                               
            CALL PRNT(4,1,I,'I=. ')                       
101         CALL PRNT(2,1,IBUF(I),'DIFF IBUF(I)=. ')      
98       CONTINUE
         MAX=IBUF(1)
C*************************
C     LOCATE MAX IN LSF
C*************************
         DO 16 I=1,INSM1
            IF (IBUF(I) .LT. MAX) GOTO 16
            MAX=IBUF(I)
            I1=I
16       CONTINUE
         INSM2=INSM1-1
         IF (IPRINT .EQ. 0) GOTO 97
         CALL PRNT (4,1,I1,'INDEX FOR MAX I1=. ')      
         CALL PRNT (4,1,MAX,'MAX =. ')            
         CALL PRNT (4,1,INSM2,'INSM2 IS NUM IN LSF-1=. ')  
97       CONTINUE
         IBAD=0
C************************
C     FIND EDGES OF LSF
C************************
         DO 17 I=I1,INSM2
            IRT=I
            IF (IBUF(I) .LT. 1) IBUF(I)=0   !TRUNCATES NEGATIVES
            IF (IBUF(I) .LT. 1) GOTO 18
            IF ((IBUF(I) .LT. IBUF(I+1)) .AND. 
     &          (IBUF(I) .LT. NOISE)) GOTO 18
            IF (IBUF(I) .LT. IBUF(I+1)) IBAD=IBAD+1   !COUNTS BAD PIXELS
17       CONTINUE
         IF (IBAD .NE. 0)
     &      CALL PRNT(4,1,IBAD,'NUMBER OF BAD PIXELS ON RIGHT= ')
         IF (IBAD .NE. 0)
     &      CALL PRNT(4,1,LINO,'LINE NUMBER=. ')
18       CONTINUE
         IF (IPRINT .EQ. 0) GOTO 96
         CALL XVMESSAGE ('FIND EDGE OF LSF',' ')           
         DO 102 I=1,INSM1                              
            CALL PRNT (4,1,I,'I=. ')                      
102         CALL PRNT (2,1,IBUF(I),'IBUF(I)=. ')          
96       CONTINUE
         DO 19 I=2,I1
            I2=I1-I+2
            ILEFT=I2
            IF (IPRINT .EQ. 1)
     &         CALL PRNT(4,1,I2,'INDEX FOR LEFT EDGE LSF I2=. ')        
            IF (IBUF(I2) .LT. 1) IBUF(I2)=0
            IF (IBUF(I2) .LT. 1) GOTO 20
            IF ((IBUF(I2) .LT. IBUF(I2-1)) .AND.
     &          (IBUF(I2) .LT. NOISE)) GOTO 20
            IF (IBUF(I2) .LT. IBUF(I2-1)) IBAD=IBAD+1   !COUNTS BAD PIXELS
19       CONTINUE
         IF (IBAD .NE. 0)
     &      CALL PRNT(4,1,IBAD,'NUMBER OF BAD PIXELS ON LEFT=. ')
         IF (IBAD .NE. 0)
     &      CALL PRNT(4,1,LINO,'LINE NUMBER=. ')
20       CONTINUE
         IF(IPRINT .EQ. 0) GOTO 95
         DO 103 I=1,INSM1                                
            CALL PRNT(4,1,I,'INDEX FOR LSF EDGE I=. ')
103         CALL PRNT(2,1,IBUF(I),'IBUF(I)=. ')
95       CONTINUE
         IF((I1-ILEFT) .GT. 30) ILEFT=I1-30   !SETS MAXIMUM WIDTH OF LSF
         IF((IRT-I1) .GT. 30) IRT=I1+30       !TO MAX +- 30 ELEMENTS
         I1=0
         IF (IPRINT .EQ. 1)
     &      CALL PRNT(4,1,ILEFT,'ILEFT=. ')          
         IF (IPRINT .EQ. 1)
     &      CALL PRNT(4,1,IRT,'IRT=. ')
         DO 21 I=ILEFT,IRT
            I1=I1+1
            CALL XVTRANS(TRANBUF,IBUF(I),R(I1),1)
            IF (IPRINT .EQ. 1)
     &         CALL PRNT(7,1,R(I1),'LOAD R(I1)=. ')     
21       CONTINUE
         NPTSIN=I1
         IF(IPRINT .EQ. 1)
     &      CALL PRNT(4,1,NPTSIN,'NPTSIN=. ')        
         DO 22 I=3,12
            I1=I
            IF (NPTSIN .LE. 2**I) GOTO 23
22       CONTINUE
23       NTOT=2**I1
         IPOWER=I1
C****************************
C     PROCESS LSF NOW
C****************************
24       CONTINUE
         NTOT=2**IPOWER
         NTOTOL=NTOT
         NZERO=NPTSIN+1
         N1=NTOT/2
         N2=N1+1
         N3=N1+2
         N4=NTOT+2
         IF (TIMINT .LT. 1.0E-20) TIMINT=1.0
         TIME1=TIMINT*NTOT
         IF(NPTSIN .EQ. NTOT) GOTO 31    !NO FILL INS NEEDED
         IF(KEY .EQ. 1) GOTO 26          !FILL IN BY REFLECTING LSF
         IF(KEY .EQ. 2) GOTO 28          !FILL IN BY REPLECATING MEAN LSF
         DO 25 I=NZERO,NTOT             !SUPPLY BLANKS IF NO FILL INS
25          R(I)=0.0E0
         GOTO 31
26       J=0                            !REFLECTION OF LSF
         DO 27 I=NZERO,NTOT
            J=J+2
27          R(I)=R(I-J)
         GOTO 31
28       SUM=0.0E0                      !MEAN LSF
         DO 29 I=1,NPTSIN
29          SUM=SUM+R(I)
         SUM=SUM/NPTSIN
         DO 30 I=NZERO,NTOT
30          R(I)=SUM
31       CONTINUE
         IF (KEY4 .EQ. 0) GOTO 34         !NO NORMALIZE MTF
         SUM=0.0E0
         DO 32 I=1,NTOT
32          SUM=SUM+R(I)
         IF (SUM .EQ. 0) THEN
            CALL XVMESSAGE ('No Edge Found',' ')
            CALL PRNT(4,1,LINO,'LINE  NUMBER=. ')
            NUMBER_OF_LINES_WITHOUT_EDGE =
     &                            NUMBER_OF_LINES_WITHOUT_EDGE+1
            GOTO 481
         ENDIF
         IF (IPRINT .EQ. 1) THEN
            CALL XVMESSAGE ('NORMALIZING LSF',' ')
            CALL PRNT(7,1,SUM,'SUM OF LSF=. ')
         ENDIF
         DO 33 I=1,NTOT                 !DIVIDE BY SUM OF LSF
33          R(I)=R(I)/SUM                  !TO NORMALIZE MTF
         SUM=1.0E0
34       CONTINUE
C****************************
C     COMPUTE FIRST MOMENT FOR WEIGHT CENTER OF LSF BY EQU (2)
C****************************
         SUM=0.0E0
         SUM1=0.0E0 
         DO 35 I=1,NTOT         
            SUM=SUM+R(I)
            CEN=FLOAT(I-1)*R(I)
35          SUM1=SUM1+CEN                  
         IF (SUM .EQ. 0) THEN
            CALL XVMESSAGE ('No Edge Found',' ')
            CALL PRNT (4,1,LINO,'LINE  NUMBER=. ')
            GOTO 481
         ENDIF
         CENTR=SUM1/SUM 
         IF (IPRINT .EQ. 1) THEN
            CALL PRNT(7,1,SUM1,'SUM1=. ')
            CALL PRNT(7,1,SUM,'SUM=. ')
            CALL PRNT(7,1,CENTR,'CENTR=. ')      
         ENDIF
C****************************
C     SAMPLING THEOREM TO PRODUCE 256 WIDE LSF FOR FFTT
C     LSF AFTER RESAMPLING IS SPLIT WITH MAXIMUM AT R(1),
C     MINIMUM=0 AT R(128) AND MAXIMUM AT R(256).
C****************************
         IF (IPRINT .EQ. 1)
     &      CALL PRNT(7,NTOT,R,'AT SAMPLING THEORM R(I)=. ')
         NT2=NTOT/2
         I=CENTR+1.0E-5                 !WANT FRACTIONAL PART OF CENTR
         CEN=128.0E0+NT2+CENTR-I        !WANT FRACTIONAL PART OF CENTR
         IF (IPRINT .EQ. 1)
     &      CALL PRNT(7,1,CEN,'FRACTIONAL OFFSET CEN=. ') 
         SUM1=0.0E0
         DO 39 I=1,256+NTOT              !REDEFINE PER NATHAN
            VAL=(CEN-I)*PI                  !"EQU 3" 
            IF (ABS(CEN-I) .LT. 1.0E-10) GOTO 38   
            WTS(I)=SIN(VAL)/VAL             !"EQUATION 3"
            GOTO 39
38          WTS(I)=1.0E0
39          SUM1=SUM1+WTS(I)                !SUM OF WEIGHTS
         IF (IPRINT .EQ. 1) THEN
            CALL PRNT(7,1,SUM1,'SUM1 OF WTS=. ')   
            CALL PRNT(7,256+NTOT,WTS,'WTS(I)=. ')  
         ENDIF
         J=256+1                         !REDEFINE PER NATHAN
         DO 42 I=1,J                     !DOES THE RESAMPLING
            SUM1=0.0E0
            DO 41 K=1,NTOT
41             SUM1=SUM1+WTS(I+K-1)*R(K)       !"INTX" EQU 3.
            F(I)=SUM1
42       CONTINUE
         IF (IPRINT .EQ. 1) CALL PRNT(7,256,F,'INT F(I)=. ')  
         I=1+(CENTR+1.0E-5)              !FRACTIONAL PART OF CENTR
         MIDPT=128+NT2-I+1     
         I=0
         IF (IPRINT .EQ. 1) CALL PRNT(4,1,MIDPT,'MIDPT=. ')   
         DO 43 K=MIDPT,256               !CENTERS LSF, SPLITS
            I=I+1                           !AND FOLDS WITH MAX AT END
43          R(I)=F(K)                       !POINTS
         IF (IPRINT .EQ. 1) CALL PRNT(7,256,R,'R(I)=. ') 
         I=257
         LLT=MIDPT-1
         IF (IPRINT .EQ. 1) CALL PRNT(4,1,LLT,'LLT=. ')    
         DO 44 K=1,LLT                   !LEFT JUSTIFIES LSF
            J=LLT-K+1
            I=I-1
44          R(I)=F(J)
         NTOT=256
         IPOWER=8
         IF (IPRINT .EQ. 1) THEN
            CALL XVMESSAGE ('RESAMPLED DATA FOR TRANSFORM',' ')
            CALL PRNT(7,256,R,'R=. ')
         ENDIF
         NTOTOL=NTOT
         N1=NTOT/2
         N2=N1+1
         N3=N1+2
         N4=NTOT+2
         TIME1=TIMINT*NTOT    !INITIAL PHASE SHIFT 

C********************
C     DIRECT FORWARD FFT 
C********************
         DO 46 I=1,NTOT
46          A(I)=CMPLX(R(I),0.0E0)
         CALL FFTT(IPOWER,-1,A)       !REALLY ONLY THREE ARGUMENTS!!!!

C*********************
C     PROCESS FFTT OUTPUT FOR EACH LINE NORMALIZE ONLY ONCE AFTER F.T. 
C     TO AVOID ROUND OFF AND TRUNC. CONSULTATION WITH R. NATHAN
C     REMOVE FFTT FACTOR OF "2" AND NORMALIZE FT COMPONENTS BY "/REAL(A(1))"
C     ASSUMES AIMAG(A(1))=0.0 ALWAYS!!
C*********************
         FTNORM=REAL(A(1))
         DO 48 I=1,N2                          !SUM UP FT ELEMENTS
            REALV(I)=REALV(I)+(REAL(A(I))/FTNORM) !FOR EACH LINE
            AIMAGV(I)=AIMAGV(I)+(AIMAG(A(I))/FTNORM)
48       CONTINUE
481      LINO=LINO+1
         IF ((ILSF .EQ. 1) .OR. (IDATA .EQ. 1)) GOTO 49
         IF (LINO .LE. LINLAS) GOTO 11     !GO READ ANOTHER LINE
49       CONTINUE
         IF (ISL .NE. 1) THEN
            LINES_PRO = LINLAS-ISL+1-NUMBER_OF_LINES_WITHOUT_EDGE
            RINL=FLOAT(LINES_PRO)      !NUMBER OF LINES PROCESSED
            IF (ITST .EQ. 0)
     &         CALL PRNT
     &              (4,1,LINES_PRO,'NUMBER OF LINES PROCESSED=. ')
            ELSE
               LINES_PRO = LINO-1-NUMBER_OF_LINES_WITHOUT_EDGE 
               RINL=FLOAT(LINES_PRO)    !NUMBER OF LINES PROCESSED
               CALL PRNT 
     &              (4,1,LINES_PRO,'NUMBER OF LINES PROCESSED=. ')
            ENDIF

C     PRINT OUT TRANSFORM VALUES
C***************************
C-----IF OUTPUT ASCII FILE REQUIRED?
	    CALL XVPARM('TABLE',TBL,ICNT,IDEF,1)
	    IF(IDEF .EQ. 0) THEN
	       ITBL = 1
 	       TAB=CHAR(9)
	       OPEN(11,NAME=TBL,STATUS='UNKNOWN',IOSTAT=JST,ERR=999)
               IF (XVPTST('COLHDR')) THEN
                  IF (XVPTST('PHASE')) 
     &               WRITE(11,904)
     &                  ' FREQUENCY',TAB,'AMPLITUDE',TAB,'PHASE'
                  IF (XVPTST('NOPHASE')) 
     &               WRITE(11,905) ' FREQUENCY',TAB,'AMPLITUDE'
               ENDIF
	    ENDIF

904	    FORMAT(1X,A10,A1,A9,A1,A5)
905	    FORMAT(1X,A10,A1,A9)

C-------IS FULL PRINTOUT REQUIRED?
	    NOPRT=0
	    IF (XVPTST('NOPRINT')) NOPRT=1
	    IF (NOPRT .EQ. 0) THEN
            ZPBUF=' '
            CALL XVMESSAGE(' ',' ')
            ZPBUF(1:51)=
     &         '   CYCLES/     FREQUENCY    REAL(FT)     IMAG.(FT)'
            ZPBUF(55:102)=
     &         'INTENSITY    AMPLITUDE     PHASE     WAVE LENGTH'
            CALL XVMESSAGE(ZPBUF,' ')
            ZPBUF=' '
            ZPBUF(4:9)='SAMPLE'
            ZPBUF(81:87)='RADIANS'
            ZPBUF(95:99)='SHIFT'
            CALL XVMESSAGE(ZPBUF,' ')
            ZPBUF=' '
	 ENDIF

         K=1

         DO 54 I=1,N2                   !LOOP TO PRINTOUT OTF VS FREQUENCY
            C1=REALV(I)/RINL               !AVERAGE THE SUMED F.T. COMPONENTS
            C2=AIMAGV(I)/RINL
            IF (ABS(C1) .LT. 1.0E-8) C1=0.0E0 !SUPRESS ERROR VALUES
            IF (ABS(C2) .LT. 1.0E-8) C2=0.0E0
            F2=(I-1)/TIME1
            AMPL2=C1*C1+C2*C2
            AMPL=SQRT(AMPL2)

            AMPL9(I)=AMPL             !LOAD FOR PLOTTING
            AMPL10(I)=AMPL

C            AMPL9(I)=-AMPL             !LOAD FOR PLOTTING
C            AMPL10(I)=-AMPL
            IF (AMPL .LT. 0.0005E0) GOTO 60 !SUPRESS PHASE ERRORS

C<<<<<< **** NOTE TO DEVELOPER ****
C       The following change was made as a work-around for the subroutine
C       FFTT.  The problem with FFTT is its inconsistency between platforms on 
C       calculation of small floating values for both real and imaginary parts 
C       of the complex number.  The problem may be caused by hardware round-off.
C       In the case of this program, it affected the imaginary part of the 
C       complex number, which has direct impact on the calculation of PHASE. 
C       REF:  FR90520
C       -- Thomas Huang (txh)
C>>>>>> ***************************

            IF (ABS(C2) .LT. (1E-3 * ABS(C1))) THEN
               IF (C1 .GT. 0.0) THEN
                  PHASE = 0.0
               ELSE
                  PHASE = PI
               ENDIF
            ELSE
               PHASE=ATAN2(C2,C1)
            ENDIF

C<<<<<<< **** ORIGINAL CODE ****
C           PHASE=ATAN2(C2,C1)
C>>>>>>> ***********************

            GOTO 61
60          PHASE=0.0E0
61          CONTINUE      
            F3=(I-1)/FLOAT(NTOTOL)         !DEFINES FREQUENCY

C            CPLOT(I)=F3                   !LOAD FOR PLOTTING

C<<<<<< Changed to use the INTERVAL input value.  TXH

            CPLOT(I)=F3*TIMINT         !LOAD FOR PLOTTING

            IF (ZOOMFLAG .EQ. 1) THEN

               PPLOT(I)=PHASE*PZOOM     !LOAD FOR PLOTTING
C               PPLOT(I)=-PHASE*PZOOM     !LOAD FOR PLOTTING

            ELSE

               PPLOT(I)=PHASE
C               PPLOT(I)=-PHASE

            ENDIF
            IF((F3 .LT. 0.2500) .OR.
     &         (F3 .GT. 0.484)) GOTO 51
            AMPLSUM=AMPLSUM+AMPL           !INTEGERATES AMPL
51          CONTINUE
            IF(ABS(F2) .LT. 1.0E-20) GOTO 52
            WSFT=PHASE/(2.0E0*PI*F2)
            GOTO 53
52          WSFT=0.0E0
53          CONTINUE
            F29(I)=F2
            WSFT9(I)=WSFT
	    IF (NOPRT .EQ. 0) THEN
               WRITE (ZPBUF(1:10),'(F10.5)') F3
               WRITE (ZPBUF(14:23),'(F10.5)') F2
               WRITE (ZPBUF(27:36),'(F10.5)') C1
               WRITE (ZPBUF(40:49),'(F10.5)') C2
               WRITE (ZPBUF(53:62),'(F10.5)') AMPL2
               WRITE (ZPBUF(66:75),'(F10.5)') AMPL
               WRITE (ZPBUF(79:88),'(F10.5)') PHASE
               WRITE (ZPBUF(92:101),'(F10.5)') WSFT
               CALL XVMESSAGE(ZPBUF,' ')
	    ENDIF

	    IF ((ITBL .EQ. 1) .AND. XVPTST('PHASE')) 
     &         WRITE(11,903) F3,TAB,AMPL,TAB,PHASE
	    IF (ITBL .EQ. 1 .AND. XVPTST('NOPHASE')) 
     &         WRITE(11,902) F3,TAB,AMPL

            OBUF(K)=F3
            OBUF(K+1)=C1
            OBUF(K+2)=C2
            K=K+3
54       CONTINUE

903	 FORMAT(1x,F10.5,A1,F10.5,A1,F10.5)
902	 FORMAT(1x,F10.5,A1,F10.5)
	 IF (ITBL .EQ. 1) CLOSE(11)

         CALL PRNT(4,1,K,'NUMBER OUTPUT = . ')

	 CALL XVP('OUT',OUTFILE,OSTAT)
         IF (OSTAT .EQ. 1) THEN
            CALL XVUNIT(OUNI,'OUT',1,OSTAT,' ')
            CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT',
     &                  'SA','U_FORMAT','REAL','O_FORMAT',
     &                  'REAL','U_NL',1,'U_NS',372,'OP',
     &                  'WRITE',' ')
            CALL XVWRIT(OUNI,OBUF,STAT,'NSAMPS',K,' ')
            CALL XVCLOSE(OUNI,STAT,' ')
         ENDIF

         WRITE (IMESS(48:56),'(F9.5)') AMPLSUM
         CALL XVMESSAGE(IMESS,' ')

         IF(IPLOT .EQ. 1) GOTO 57
C************************
C     LINE PRINTER PLOTS
C************************
         ZPBUF=' '
         ZPBUF(2:10)='FREQUENCY'
         ZPBUF(20:21)='-1'
         ZPBUF(70:70)='0'
         ZPBUF(119:120)='+1'
         CALL XVMESSAGE(ZPBUF,' ')
         CALL XVMESSAGE(LO,' ')
         DO 56 I=1,N2                     
            DO 55 J=1,101
55             LOG(J:J)=BLNK
            LOG(51:51)=ONE
            INLO=(REALV(I)+1.)*51.
            IF (INLO .LT. 0) INLO=0
            IF (INLO .GT. 101) INLO=101
            LOG(INLO:INLO)=LOGR
            INLO=(AIMAGV(I)+1.)*51.
            IF (INLO .LT. 0) INLO=0
            IF (INLO .GT. 101) INLO=101
            LOG(INLO:INLO)=LOGI
            INLO=(-AMPL10(I)+1.)*51.
            IF(INLO .LT. 0) INLO=0
            IF(INLO .GT. 101) INLO=101
            LOG(INLO:INLO)=LOGA
            INLO=(WSFT9(I)+1.)*51.
            IF (INLO .LT. 0) INLO=0
            IF (INLO .GT. 101) INLO=101
            LOG (INLO:INLO)=LOGW
            WRITE (ZPBUF(1:10),'(F10.5)') F29(I)
            ZPBUF(20:120)=LOG(1:101)
            ZPBUF(10:18)=BL
56          CALL XVMESSAGE(ZPBUF,' ')
57       CONTINUE

         IF (IPLOT .EQ. 0) GOTO 995

C*******************
C     PRINTRONIX PLOTS 
C*******************
         CALL XRTBEGIN(STATUS)
         IF (STATUS .NE. 1) CALL MABEND('Unable to OPEN plotter')
         CALL DISPLAYAXES(1,1,1)   ! x,y1 and y2 axes displayed 1=yes 0=no
         CALL SETACTIVESET(0)      ! 0= no triangles (default), 1= triangles
         X=0.22                         !PRINT HEADER ON PLOT
         Y=3.5

C-------PRINT HEADER ON PLOT
C-------PRINT INPUT FILENAME AND PLOTNAME ON PLOT
C-------GET AND PLOT ALL LABELS
         HEADR(1)='OTF1 MODULATION TRANSFER FUNCTION'
         IF (INSTAT .EQ. 1) THEN
            HEADR(2)='INPUT = '//INFILE
            I=3
         ELSE
            I=2
         END IF
         HEADR(I)='PLOT  = '//PLOT_DS
         I=I+1

C-------PRINT INPUT FILENAME ON PLOT
         IF (INSTAT .EQ. 1) THEN
            CALL OTFLABS(IUNI,HEADR,I,*994)
            I=I+1
         ENDIF
         HEADR(I)(1:80)=IMESS
         CALL HEADER (HEADR,I,0) ! 0=left justify, 1=center justify, 2=right

         WRITE (MCYCLE(31:37), '(F7.2)') TIMINT 
         IF (ZOOMFLAG .EQ. 0) THEN
            CALL AXESTITLES(MCYCLE,'AMPLITUDE',90,
     &                   'PHASE (PI=1)',90)
         ELSE
            WRITE (MPHASE(25:31),'(F7.2)') PZOOM
            CALL AXESTITLES
     &           (MCYCLE,'AMPLITUDE',90,MPHASE,90)
         ENDIF
 
         AMPL9(130)=0.00000
         AMPL9(131)=1.00000
         CPLOT(130)=0.00000
         CPLOT(131)=1.00000
         PPLOT(130)=0.00000
         PPLOT(131)=1.00000
         CALL LINE(CPLOT,AMPL9,N2,1,0,0)

C     plot the "PHASE" curve
         do ii=1,n2  ! scale phase to lie between -1 and 1 .
            pplot(ii)=pplot(ii)/3.14159
         enddo
         CALL LINE(CPLOT,PPLOT,N2,1,0,0)
         CALL PLOT(0,0,999)

995      IF (INSTAT .EQ. 1) CALL XVCLOSE(IUNI,STAT,' ')
         RETURN
999      CALL XVMESSAGE ('ERROR OPENNING TABLE FILE',' ')
         CALL PRNT(4,1,J,'IOSTAT=.')
994      CALL ABEND
         RETURN
      END


      SUBROUTINE OTFLABS(INU,HEADR,ICNT,*)
         IMPLICIT INTEGER*4 (A-Z)
         INTEGER*4 LV2(60),LPL(60),ICNT
         CHARACTER*72 V1(60),V2(60),PL(60),HT(60)
         CHARACTER*80 HEADR(70)
 
         CALL LABPROC(INU,V1,NV1,V2,LV2,NV2,PL,LPL,NPP,HT,NH)
 
         IF (NV1 .GT. 0) THEN
            DO I=1,NV1
               HEADR(ICNT)=V1(I)
               ICNT=ICNT+1
            ENDDO
         ELSE IF (NPP .GT. 0) THEN
            DO I=1,NPP
               HEADR(ICNT)=PL(I)
               ICNT=ICNT+1
            ENDDO
         ELSE IF (NV2 .GT. 0) THEN
            DO I=1,NV2
               HEADR(ICNT)=V2(I)
               ICNT=ICNT+1
            ENDDO
         ELSE
            CALL XVMESSAGE('LABEL ERROR',' ')
            RETURN 1
         ENDIF

         IF (NH .GT. 0) THEN
            DO I=1,NH
               HEADR(ICNT)=HT(I)
               ICNT=ICNT+1
            ENDDO
         ENDIF
 
         RETURN
      END


      SUBROUTINE KVPAIR(IU,PAIR,LNG,NUM)
C-------EXTRACTS KEYWORD=VALUE STRINGS FROM FIRST PROPERTY LABEL
         IMPLICIT INTEGER*4 (A-Z)
         INTEGER*4 LNG(60)
         character*72 pair(60)
         character*2048 buf

         max=2048
         call xlgetlabel(iu,buf,max,ist)
         i=0
         b = index(buf(1:),'PROPERTY')
         IF (B .EQ. 0) GOTO 100

50       L=INDEX(BUF(B+1:),' ')
         L=B+L
         Q=INDEX(BUF(B+1:),'''')
         Q=B+Q
         IF (Q .LT. L) THEN		!ARE THERE QUOTES IN THE VALUE?
            Q2=INDEX(BUF(Q+1:),'''')
            Q2=Q+Q2
            L=INDEX(BUF(Q2+1:),' ')
            L=Q2+L
         ENDIF
         E=L-1
C-------STOP WHEN REACH A 'TASK' OR NEXT PROPERTY LABEL
         IF (BUF(B:B+3) .EQ. 'TASK') GOTO 100
         IF ((I .GT. 1) .AND. 
     &       (BUF(B:B+3) .EQ. 'PROP')) GOTO 100
         I=I+1
         KK=E-B+1
         LNG(I) = KK			!STORE LENGTH
         PAIR(I) = BUF(B:E)        !STORE STRING
         IF (E .EQ. MAX) GOTO 100        !END OF LABEL?
         DO J=E+1,MAX			!LOOK FOR NEXT STRING
            B=J
            IF(BUF(J:J) .NE. ' ') GOTO 50
         END DO
100      NUM=I
         RETURN
      END
c ************************************************************


      subroutine V2PAIR(IU,PAIR,LNG,NUM)
C-------RETURN THE KEYWORD=VALUE PAIRS FROM THE FIRST V2 TASK
         IMPLICIT INTEGER*4 (A-Z)
         INTEGER*4 LNG(60)
         character*72 pair(60)
         character*2048 buf

         max=2048
         call xlgetlabel(iu,buf,max,ist)
         i=0
         IF (index(buf(1:),'PROPERTY') .GT. 0) GOTO 100
         B = index(buf(1:),'TASK')
         IF (B .EQ. 0) goto 100

50       L=INDEX(BUF(B+1:),' ')
         L=B+L
         Q=INDEX(BUF(B+1:),'''')
         Q=B+Q
         IF (Q .LT. L) THEN
            Q2=INDEX(BUF(Q+1:),'''')
            Q2=Q+Q2
            L=INDEX(BUF(Q2+1:),' ')
            L=Q2+L
         ENDIF
         E=L-1
C-------STOP WHEN ENCOUNTER THE SECOND TASK
         IF ((I .GT. 1) .AND. 
     &       (BUF(B:B+3).EQ. 'TASK')) GOTO 100
         I=I+1
         KK=E-B+1
         LNG(I) = KK			!STORE LENGTH
         PAIR(I) = BUF(B:E)	!STORE STRING
         IF (E .EQ. MAX) GOTO 100	!END OF LABEL?
         DO J=E+1,MAX			!LOOK FOR NEXT STRING
            B=J
            IF(BUF(J:J) .NE. ' ') GOTO 50
         END DO
100      NUM=I
         RETURN
      END


      SUBROUTINE LABPROC(IUNI,LABV1,NV1,VPAIR,LV2,NV2,PPAIR,LPL,
     &                   NPP,HSTRY,NH)
         IMPLICIT INTEGER(A-Z)
         INTEGER INSTANCES(20),LV2(60),LPL(60)
         CHARACTER*8 TASKS(20)
         CHARACTER*12 UNAME
         CHARACTER*28 TIME
         CHARACTER*72 HBUF,BL
         CHARACTER*72 LABV1(60),HSTRY(60),VPAIR(60),PPAIR(60)
         CHARACTER*4320 LABS

c-------get all vicar1 labels
         NV1=40
         CALL VIC1LAB(IUNI,STAT,NV1,LABS,0)
         DO J=1,NV1
            LABV1(J)=LABS(72*(J-1)+1:72*(J-1)+72)
         END DO

c-------get keyword=value pairs from vicar property labels
         NPP=0
         IF (NV1 .EQ. 0) call kvpair(IUNI,ppair,LPL,npp)

c-------get keyword=value pairs from first vicar2 task
         NV2=0
         IF ((NV1 .EQ. 0) .AND. (NPP .EQ. 0)) 
     &      call v2pair(IUNI,vpair,LV2,nv2)

c-------get user and date for each vicar2 history task
         BL = '----TASK:-------------USER:------------------------'

         nh=20                             !EXTRACT VIC*2 LAB
         CALL XLHINFO(IUNI,TASKS,INSTANCES,NH,STAT,' ')

         DO 801 J=1,NH
            CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')
            CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')

            HBUF(1:72) = BL(1:72)
            HBUF(10:22) = TASKS(J)
            HBUF(28:39) = UNAME
            HBUF(40:68) = TIME
801         HSTRY(J) = HBUF

         RETURN
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create otf1.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM otf1

   To Create the build file give the command:

		$ vimake otf1			(VMS)
   or
		% vimake otf1			(Unix)


************************************************************************/


#define PROGRAM	otf1
#define R2LIB

#define MODULE_LIST otf1.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_MOTIF
#define LIB_XRT_GRAPH

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create otf1.pdf
process help=*
 
PARM INP      TYPE=STRING  COUNT=(0:1)                DEFAULT=--
PARM OUT      TYPE=STRING  COUNT=(0:1)                DEFAULT=--
PARM SIZE     TYPE=INTEGER COUNT=(0:4)                DEFAULT=(1,1,0,0)
PARM DIAG     TYPE=KEYWORD COUNT=(0:1) VALID=DIAG     DEFAULT=--
PARM SINCTST  TYPE=KEYWORD COUNT=(0:1) VALID=SINCTST  DEFAULT=--
PARM NOISE    TYPE=INTEGER COUNT=(0:1)                DEFAULT=3
PARM PLOT     TYPE=STRING  COUNT=(0:1)                DEFAULT=--
PARM REFLECT  TYPE=KEYWORD COUNT=(0:1) VALID=REFLECT  DEFAULT=--
PARM MEAN     TYPE=KEYWORD COUNT=(0:1) VALID=MEAN     DEFAULT=--
PARM NONORMAL TYPE=KEYWORD COUNT=(0:1) VALID=NONORMAL DEFAULT=--
PARM NOPRINT  TYPE=KEYWORD COUNT=(0:1) VALID=NOPRINT  DEFAULT=--
PARM INTERVAL TYPE=REAL    COUNT=(0:1) VALID=(0:9999.99)  DEFAULT=1.0
PARM DATA     TYPE=REAL    COUNT=(0:512)              DEFAULT=--
PARM LSF      TYPE=REAL    COUNT=(0:512)              DEFAULT=--
PARM PZOOM    TYPE=REAL    COUNT=(0:1) VALID=(0:9999.99)  DEFAULT=--
PARM TABLE    TYPES=STRING  COUNT=(0:1)                DEFAULT=--
PARM COLUMNS  TYPE=KEYWORD COUNT=(0:1) VALID=(COLHDR,NOCOLHDR) DEFAULT=COLHDR
PARM PHASE    TYPE=KEYWORD COUNT=(0:1) VALID=(PHASE,NOPHASE) DEFAULT=PHASE
PARM NODISP   TYPE=KEYWORD COUNT=(0:1) VALID=NODISP   DEFAULT=--
END-PROC
.TITLE
VICAR program OTF1 -- perform 1-D FFT to obtain the
Optical Transfer Function and the MTF.
.HELP
 
PURPOSE:
 
   OTF1 is a VICAR applications program which performs one-dimensional
Fast Fourier Transformations in order to compute Optical Transfer
Functions (OTF) from degraded edges in images or from either a tabulated
real function, or a line spread function using parameter inputs. OTF1 is
able to compute the entire optical transfer function (not just the MTF)
from digital picture data with greater accuracy and ease than other methods
available. This technique for computing imaging system MTF's is preferable
to previously used techniques which invloved the imaging of sine wave targets.
 
   The input to OTF1 may be either image data or data specified by parameters.
OTF1 allows OTF's to be computed from such operational image data as planet
limbs or other step functions. OTF1 will accept a real string of values (DATA)
for which the Fourier transform is desired, or a line spread function (LSF)
for which the OTF is desired. A self test routine is invoked by SINCTST which
creates a SINC function and branches to the LSF processor.
 
   To aid in the preparation of filter weights for MTF enhancement, a provision
has been added to output the Real and Imaginary Fourier Transform components.
The output is 124 triplets of Real*4 numbers organized as [Frequency,Real,
Imaginary], [Frequency,Real,Imaginary], from 1.0 cycles/sample to 0.484
cycles/sample. The output does not extend beyond 0.484 cycles/sample due to
limitations of the Sampling Theorem used for LSF resampling (see last para-
graph of Method below).
.page
 EXECUTION:
 
   OTF1 INP  OUT  PLOT  PARAMS
 
 where: INP is an optional input image.
 
        OUT is an optional output file with fourier transform data.
 
        PLOT is an optional plot of the output.
 
        PARMS are optional parameter, which are described below and
             under Tutor.
.page
METHOD:
 
   The data are normally in an input image and are processed line by line.
Each line is left adjusted into an array of dimension equal to the smallest
power of two which contain an entire input line. The data is then different-
iated to produce a Line Spread Function (LSF). For direct input of a Line
Spread Function, the parameters LSF is followed by the LSF values (Real*4).
Processing will commence with the LSF ready for fillin to the nearest power
of two using the MEAN, REFLECT, or FILLIN parameters. For direct input of
a theortical "real" edge or mathematical function, the parameter DATA is
followed by a string of values (Real*4) and will be processed as thought it
was a real one-line image input. SINCTST will cause a SINC function to be
formed and processed as though it were the LSF of an edge.
 
  The Maximum point in the LSF is located and the LSF truncated to 30 points
on either side of the LSF Maximum. If there are less than 30 points to either
side the option exists to fill in extra points by replecating the LSF mean,
by reflection, or with zeros. This LSF is then left justified and the Sampling
Theorem applied to produce a 256 point power spectrum. In addition the
resampled LSF is folded about the maximum, with points from the maximum to the
upper end placed from i=1 to the midpoint, and points from the maximum to the
lower end placed from near the midpoint to 1=256. This array is then in the
proper form upon which the subroutine FFTT can be used to perform an inverse
transform. The real and imaginary components of the transform for each line
are accumulated element by element, and these operations continued until all
the image lines have been processed. From the accumulated real and imaginary
transform components the following are computed and printed.
.page
   Given a complex FT element "a+ib" the output will consist of:
 
(1) The spatial frequency in cycles/sample.
(2) The actual frequency in cycles/unit of time (INTERVAL).
(3) a=Real part of FT.
(4) b=Imaginary part of FT.
(5) SQRT(a**2+b**2)=The amplitude of the FT. This is also the MTF if the
    input data is an LSF.
(6) (a**2+b**2)=The power spectrum or intensity of the FT.
(7) The Normalized amplitude of the MTF which will be plotted.
(8) ACRTAN(b/a)=The phase angle Omega of the FT measured in radians
    which will also be plotted
(9) W=Omega/2*pi*F=The displacement in the same units as INTERVAL
    (the default of which is samples) of each sine wave component in
    the image from its true position in the object being imaged. F is
    in units of 1/INTERVAL. This quantity is meaningful if the input
    data is an LSF and if OTF has been specified.
.page
If PLOT was specified the Normalized MTF Amplitude and Phase are processed
into an output data set with Fortran I/O which can be printed to produce a
Printronix like plot. In this case the line printer plot is disabled.
 
The default is to generate on the line printer a display of the pertinent
components of the OTF in a crude but rapidly analyzable format. Each
curve's data points are represented by a letter with the following code:
 
   A   Amplitude of the FT
   R   Real portion of the FT
   I   Imaginary portion of the FT
   W   Wavelength shift of the FT.
.page
The FT is computed from
 
           N-1
    FT =SUM    [DATA *Exp(-2*PI*inm/N)]                   (1)
           n=0      n
 
where N is the data string length and  0<m<(N-1).
 
   The OTF section of the program is designed to generate an LSF positioned
in such a way as to eliminate both the effects of sampling and the bias
introduced into the phase OMEGA due to the arbitrary positioning of the LSF
data in the input array. Users of FFTT have been forced to obtain clean
OTF's by using symmetrical LSF's positoned half at the beginning of the
data string and half at the end. This was inconvenient and impossible if
the LSF was asymmetrical. In the latter event all they could do was settle
for the MTF, never knowing what the OTF was like. In a situation where the
OTF has gone negative, the MTF is dangerous to use as a restoration function
because it will boost the negative amplitudes the wrong way. The OTF allows
the user to visualize the scrambling of spatial frequencies which occur when
the LSF is asymmetrical, and also to see the sign of the OTF (given by the
real part of the FT).
 
   There are two problems associated with generating unbiased OTF's. They
can be solved simultaneously by use of the Sampling Theorem. If the LSF is
computed from a degraded edge composed of digital data, it is clear that
(particularly for narrow LSF's) the position of the sampled points is going
to influence the symmetry of the LSF. Clearly, trying to dissect several
LSF's into two halves (each different) and store them at opposite ends of
the data string before the FT operation, is going to produce different OTF's
each time. Because the data is band limited, at least at the Nyquist Frequency,
the MTF's generated in the above case will all be about the same. The problem
is to eliminate the effects of sampling. The Sampling Theorem is used by OTF1
to resample the acquired LSF about its center of weight. At this point, if the
LSF is asymmetrical, the effect is real.
.page
   In computing the weight center of the LSF a first moments algorithm is
used:
 
              n=N                  n=N
    CENTER=SUM   [(n-1)*DATA ]/ SUM   [DATA ]               (2)
              n=1           n      n=1     n
 
The Data are then resampled about the CENTER position using
 
            n=+INF
    INT =SUM      [DATA *((SIN(x-n)*PI)/((x-n)*PI))]          (3)
       x    n=-INF     n
 
where x is some integer distance away from CENTER. The data can then be
split about the located center and positioned properly in the array to be
transformed.
 
   The use of many LSF's to produce one OTF is done by averaging the
transform components. Averaging MTF's (if that's what you want) can be
erroneous because even OTF's, which are in reality positive, can be
caused to go negative if one of the LSF's is afflicted with noise. Since
the MTF is the modulus of the OTF, the MTF is always positive. Great care
should be taken in inspecting the LSF's computed from pictures before the
resampling step to be sure they look decent. Averaged runs of 50 or more
LSF's should yield OTF's which are accurate to within a few percent. This
is an order of magnitude better than the sine wave methods used in the past.
 
   Because the Sampling Theorem summation limits must be finite (in this
case +-128) the theorem is blind to the spatial frequencies above 0.484
cycles per sample. The OTF values above this frequency are best obtained
by extrapolating from values below 0.484 to the Nyquest limit.
.page
 RESTRICTIONS:
 
  1. Input picture record size <= 2048 samples.
 
  2. LSF or DATA input elements <= 256.
.page
 PROGRAM HISTORY:

 05  AUG   1997...T.Huang.......Ported from VAX/VMS to UNIX and ALPHA/VMS.
                                Combined the changes made by C. Avis with
                                the existing MIPS version of OTF1. 
                                Fixed problems caused by subroutine FFTT, by
                                reducing the precision in calculation with 
                                imaginary part of a complex number for PHASE.
                                (REF: fr90520) 
 28  SEP   1995...C.C.AVIS......Added option for no Phase to table
 28  JUN   1995...C.C.AVIS......Added header to ASCII table output
 27  JUN   1995...C.C.AVIS......Modified test file to use perfect ramp case
 23  FEB   1994...C.C.Avis......1.Add tabular output, property label support,
                                2.Improved test and help files.
              ?...J.J.Lorre.....1.Scaling of phase plots.
 11  AUG   1988...F.M.MOSS......1.Add warning message for the line without edge.
                                2.Fourier Transform will not apply to the line
                                  without edge.
                                3.Print out the total number of lines processed.
                                4.Fix the bug to avoid calling XVWRIT if there
                                  is no output required. (FR 35605)
  7  MAY   1987...F.M.MOSS......1.modify the code to handle size field correctly
                                2.modify output's XVOPEN and XVWRIT calling
                                  parameters
                                3.put "NONORMAL" keyword in the source if
                                  the input is a line spread function
  15 NOV   1985...F.M.MOSS......1.modify the plot for phase angle...
                                (the axis range from -1 to 1
                                 instead of from -3 to 3)
                                2.add a new param "PZOOM" so the
                                  user can either zoom up (PZOOM >1.)
                                  or zoom down (PZOOM <1.) the phase
                                  angle
  06 MAY   1985...M.E.MORRILL...ADD OUTPUT FOR F.T. COMP.
  02 MAY   1985...M.E.MORRILL...REMOVE VAX/FFT FACTOR OF 2
                                & CLEAN UP NORMALIZATIONS
  30 APR   1985...M.E.MORRILL...SAMPLING THEOREM "BUGS"
  17 APR   1985...M.E.MORRILL...MINOR FIXES TO ADD QUANTITAVE
                                ESTIMATE OF OTF CURVE.
  06 DEC   1984...M.E.MORRILL...CODE MODIFIED TO CONFORM TO
                                THEORY FROM R.NATHAN AND TO
                                REMOVE FILTER OPERATIONS.
  06 NOV   1984...M.E.MORRILL...CONVERTED TO VAX-VICAR*2.
  02 FEB   1983...E.P.KORMSO....REVISION TO PRINTRONIX PLOTING.
  10 DEC   1982...E.P.KORSMO....PRINTRONIX PLOT PACKAGE ADDED.
  27 JUN   1975...D.A.HASS......CONVERSION TO 360/OS.
  10 MAR   1975...J.J.LORRE.....RE-RELEASE.
  16 MAY   1974...J.J.LORRE.....ORIGINAL RELEASE.
 
CURRENT COGNIZANT PROGRAMMER:  J J Lorre
 
.LEVEL1
.VARIABLE INP
 An input image.
.VARIABLE OUT
 Output of Real and
 Imaginary Fourier
 Transform components.
.VARIABLE SIZE
 INTEGER-OPTIONAL
 SL,SS,NL,NS
.VARIABLE DIAG
 KEYWORD-OPTIONAL
 diaganostic printout.
.VARIABLE SINCTST
 KEYWORD-OPTIONAL
 Creates SINC function
 and branches to LSF processing.
.VARIABLE NOISE
 INTEGER-OPTIONAL
 Allowable noise level in LSF.
.VARIABLE PLOT
 STRING-OPTIONAL
 Turns on PLOT.
.VARIABLE REFLECT
 KEYWORD-OPTIONAL
 Reflects data about last
 input point.
.VARIABLE MEAN
 KEYWORD-OPTIONAL
 Causes missing data to be set
 to mean of input data.
.VARIABLE NONORMAL
 KEYWORD-OPTIONAL
 Do not normalize data.
.VARIABLE NOPRINT
 KEYWORD-OPTIONAL
 Do not print full tabular
 output.
.VARIABLE INTERVAL
 INTEGER-OPTIONAL
 Time interval between
 data points.
.VARIABLE LSF
 REAL-OPTIONAL
 Up to 256 elements from which
 the OTF will be determined.
.VARIABLE DATA
 REAL-OPTIONAL
 Real numbers of which the FT
 will be taken.
.VARIABLE PZOOM
 REAL-OPTIONAL
 Scaling factor for phase
 in plots.
.VARIABLE TABLE
 STRING-OPTIONAL
 File to receive tabular output.
.VARIABLE COLUMNS
 keyword - OPTIONAL
 Specifies whether to write
 column headers in the
 ASCII table.
.VARIABLE PHASE
 keyword - optional
 Specifies whether to print
 the Phase to the Ascii table
 or not.
.VARIABLE NODISP
 If present, no display
 is shown
 
.LEVEL2
.VARIABLE INP
 An input image  which the user wishes to Fourier Transform.
 The FT will be taken of each line and the components then averaged.
 Restriction on input record length is that it not exceed 2048 samples.
 
 If no input image is specified, data may be specified through the
 DATA or LSF parameters.  If no data are specified, OFT1 will run an
 internal sinc-function test case.
.VARIABLE OUT
 Optional output of Real and Imaginary Fourier Transform components.
 Output is 124 triplets of Real*4 numbers organized as:
   [Frequency,Real,Imaginary],
 from 1.0 cycles/sample to 0.484 cycles/sample.
.VARIABLE SIZE
 INTEGER-OPTIONAL
 Specifies the SL,SS,NL,NS of the image for which the FT will be taken
 of each line separately and then the transforms will be averaged.
.VARIABLE DIAG
 KEYWORD-OPTIONAL
 Turns on diaganostic printout.
.VARIABLE SINCTST
 KEYWORD-OPTIONAL
 Creates SINC function and branches to LSF processing.
.VARIABLE NOISE
 INTEGER-OPTIONAL
 Specifies allowable noise level in LSF for finding end points.
.VARIABLE PLOT
 STRING-OPTIONAL
 Specifies the filename to receive the output plot data.
 This must be printed out with the VMS/DCL command:
 
   PRINT/NOFEED filename
 
.VARIABLE REFLECT
 KEYWORD-OPTIONAL
 Cauese the input data to be reflected about the last input data
 point in order to fill in unspecified locations. The program must
 eventually have a data string which is a power of 2 in length. The
 default is to fill with zeros.
.VARIABLE MEAN
 KEYWORD-OPTIONAL
 Causes missing data to be set equal to the mean of input data.
 Default is to fill with zeros.
.VARIABLE NONORMAL
 KEYWORD-OPTIONAL
 Causes the data to not be normalized so that its sum is unity.
 Default is to normalize the input LSF.
.VARIABLE NOPRINT
 KEYWORD-OPTIONAL
 Suppresses the printout of the full tabular listing.  This listing
 normally includes FOR EACH POINT IN FREQUENCY:
 CYCLES/SAMPLE, FREQUENCY, REAL, IMAGINARY, INTENSITY, AMPLITUDE,
 PHASE and WAVELENGTH SHIFT.
 The default is to print the table.
.VARIABLE INTERVAL
 INTEGER-OPTIONAL
 Defines the time interval between data points. Default is 1.0 and defines
 the frequency in cycles per sample.
.VARIABLE LSF
 REAL-OPTIONAL
 Input string of real Line Spread Function values for which an OTF is
 desired. This is limited to 256 element.
.VARIABLE DATA
 REAL-OPTIONAL
 Inputs a string of real arguments which one desires to take the FT of.
 This is limited to 256 elements.
.VARIABLE PZOOM
 REAL-OPTIONAL
 Specifies the scale factor used to plot the phase values so that the
 values are easier to read on the plot.
 The scale factor will be printed on the plot
 (e.g., PHASE (RAD) MULTIPLY *   2.00  if PZOOM =2.0)
.VARIABLE TABLE
 STRING-OPTIONAL
 Specifies the name of a file to receive tabular output.  This file
 contains ASCII text in tab-delimited form.  The columns of the table
 contain Frequency, Amplitude and Phase.
.VARIABLE COLUMNS
 keyword - OPTIONAL
 Specifies whether to include column headers in the ASCII table.
 COLHDR specifies to include the headers.  NOCOLHDR specifies no headers.
 The headers are "FREQUENCY", "AMPLITUDE" and "PHASE".
 Default is to put headers in the table.
.VARIABLE PHASE
 keyword - OPTIONAL - Valid=(phase,nophase)
 Specifies whether to include a column for Phase in the ASCII table.
 Frequency and Amplitude are always included.  Default is to include it.
.VARIABLE NODISP
 Keyword--Optional
 If present, no display is shown in interactive mode and output plot files
 are automatically saved.  When not present, plot is displayed and files are
 save is an option.
.end

$ Return
$!#############################################################################
$Test_File:
$ create tstotf1.pdf
procedure
refgbl $autousage
refgbl $echo
refgbl $syschar
body
let _onfail="continue"
let $echo="yes"

local dir type=string
local mtfv type=string

if ($syschar(1)="UNIX")
   let dir ="/project/test_work/testdata/cassini/iss/"
else
   let dir="wms_test_work:[testdata.cassini.iss]"
end-if

let mtfv="&dir"//"mtf_v.byte"
 
! INTERNAL TEST WITH SINC FUNCTION
otf1 'diag 'sinctst plot=sinctst.psf table=t0.tbl
typetext t0.tbl
 
! MAKE STEP FUNCTION IMAGE
gen out=bk.dat nl=40 ns=40 linc=0.0 sinc=0.0 ival=0.0
gen out=wt.dat nl=40 ns=40 linc=0.0 sinc=0.0 ival=218
insect inp=(bk.dat,wt.dat) out=edge.dat size=(1,1,40,40) +
   insect=(1,1,40,20,1,21)
list edge.dat (1,11,40,30)
 
!MAKE LINEAR RAMP FOR EDGE, AND EXPAND INTO A 20X16 IMAGE
genthis a.img nl=1 ns=16 +
   dn=(0,0,0,0,24,48,72,96,120,144,168,192,216,216,216,216)
size a.img s1edge.dat 'noin lzoom=20 szoom=1
list s1edge.dat
 
!SMOOTH THE RAMP WITH A BOX FILTER
boxflt2 inp=s1edge.dat out=s2edge.dat nlw=9 nsw=9
list s2edge.dat
 
otf1 inp=edge.dat out=l.dat table=t1.tbl
typetext t1.tbl
list l.dat 'nofeed
 
otf1 inp=s1edge.dat size=(1,1,35,40) plot=edge1.psf 'noprint
 
otf1 inp=s1edge.dat plot=edge2.psf 'reflect
 
otf1 inp=s1edge.dat plot=edge3.psf 'mean table=t2.tbl 'noprint
 
otf1 inp=s1edge.dat plot=edge4.psf 'nonormal interval=0.5 table=t3.tbl
 
otf1 inp="&mtfv" size=(200,200,200,256) plot=edge5.psf +
   noise=5 table=t4.tbl 'noprint
 
f2 inp="&mtfv" out=half.dat +
   size=(200,200,200,256) 'half func=IN1*10
 
otf1 inp=half.dat plot=edge7.psf
 
otf1 inp=s2edge.dat plot=edge8.psf pzoom=4 'noprint
 
otf1 table=t5.tbl +
   data=(100,100,100,100,99,90,66,33,20,18,17,17,17,17,17,17)
typetext t5.tbl

!Testing PHASE option
otf1 table=t6.tbl phase=nophase +
   data=(100,100,100,100,99,90,66,33,20,18,17,17,17,17,17,17)
typetext t6.tbl
 
!Testing COLUMNS option
otf1 table=t7.tbl columns=nocolhdr +
   data=(100,100,100,100,99,90,66,33,20,18,17,17,17,17,17,17)
typetext t7.tbl

!Testing NODISP option
otf1 table=t8.tbl nodisp=nodisp +
   data=(100,100,100,100,99,90,66,33,20,18,17,17,17,17,17,17)
typetext t8.tbl

if ($syschar(1)="UNIX")
   ush rm *.dat
   ush rm *.img
else
   dcl del *.dat;*
   dcl del *.img;*
end-if
 
end-proc

$ Return
$!#############################################################################
