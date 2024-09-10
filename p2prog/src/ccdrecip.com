$!****************************************************************************
$!
$! Build proc for MIPL module ccdrecip
$! VPACK Version 1.9, Wednesday, February 02, 2005, 10:41:51
$!
$! Execute by entering:		$ @ccdrecip
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
$ write sys$output "*** module ccdrecip ***"
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
$ write sys$output "Invalid argument given to ccdrecip.com file -- ", primary
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
$   if F$SEARCH("ccdrecip.imake") .nes. ""
$   then
$      vimake ccdrecip
$      purge ccdrecip.bld
$   else
$      if F$SEARCH("ccdrecip.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake ccdrecip
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @ccdrecip.bld "STD"
$   else
$      @ccdrecip.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ccdrecip.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ccdrecip.com -mixed -
	-s ccdrecip.f -
	-i ccdrecip.imake -
	-p ccdrecip.pdf -
	-t tstccdrecip.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ccdrecip.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM CCDRECIP
C
C Radiometric calibration routine:  Determines the camera sensitivity
C (in DN per foot-lambert-milliseconds or in DN per picoamp-milliseconds
C and shutter offset (in msec) for a CCD or vidicon
C camera system.
      SUBROUTINE MAIN44
         COMMON/CP/VMES,ARMES,RMSG,LABEL

         REAL*8 DC(400),DN(30,400),AO(400),TOS(400)
         REAL*8 W(30),AVDN(30),DENOM,DNSUM
         REAL*8 SWX(400),SWXT(400),SX2(400)
         REAL*8 SW,SWT,SWT2
         REAL*4 LIGHT(30),LYAV(40),LXAV(40),YTOS(402),XTOS(402)
         REAL*4 SHUTTER_OFFSETS(1024), LORS_NUMBERS(1024)
         REAL*4 AVTOS(40),COLU(30,40),COLV(30,40),LT,LTC
         REAL*4 OUT(4320), OUTM(2,2000), EXPOS(30), BEXPO

         INTEGER ATBL,OTBL,CTBL,VTBL,ASIZ
         INTEGER SL,SS,SLI,SSI,NLI,NSI,NLI2,NSI2  ! size variables
         INTEGER OUNI,STAT,CNT,GAREA,SIGTOL,BAD(400)
         INTEGER AREA(4,400), NI, STATUS
         INTEGER NPTS,NEXP

         CHARACTER*256 PLOT_DS,OFFSET_FILE
         CHARACTER*40 CORFIL(2),ATFIL,OFFIL,AVFIL,LFILE
         CHARACTER*4 DIRECTN
         CHARACTER*8 UNITS
         CHARACTER*70 MS1, MS2
         CHARACTER*39 LUMMSG
         CHARACTER*34 RADMSG
         CHARACTER*1 TAB

C        global variables
         CHARACTER*54 VMES
         CHARACTER*4320 LABEL
         CHARACTER*51 ARMES
         CHARACTER*41 RMSG  

         TAB = CHAR(9)
      
         VMES(1:54)=' '
         ARMES = 'NUMBER OF GOOD AREAS=**** OUT OF**** AREAS SAMPLED'
         RMSG = 'NUMBER REJECTED FOR               =     '

         MS1 = ' '
         MS2 = ' '
         MS2(1:26)='AREA   SL   SS   NL   NS  '
         MS2(27:47)='A0 (DN/ENERGY UNIT)  '
         MS2(48:65)='TOS (MILLISECONDS)'
         LUMMSG(1:39)='ENERGY UNIT = FOOT-LAMBERT-MILLISECONDS'
         RADMSG(1:34)='ENERGY UNIT = PICOAMP-MILLISECONDS'

         CALL IFMESSAGE ('CCDRECIP VERSION 25-MAR-1997')
         CALL XVP('SIGTOL',SIGTOL,CNT)
         CALL XVPARM('PLOT',PLOT_DS,IPLOT,IDEF,1)
         IF (IDEF.EQ.0) THEN
            CALL PLOTFN(PLOT_DS)
            CALL XRTBEGIN(STATUS)
            IF (STATUS.NE.1) CALL MABEND('Unable to OPEN plotter')
            CALL DISPLAYAXES(1,1,0)! x,y1,y2 axes displayed 1=yes 0=no
            CALL SETACTIVESET(1)   ! endpts on lines 0=no triangles, 1=triangles
            IPLOT = 1
         ELSE
            IPLOT = 0
         ENDIF

         CALL XVP('REJECT',IREJCT,CNT)

C-------get light data from file or parameters
	 CALL XVPARM('LTFILE',LFILE,ICNT,IDEF,1)
	 IF (ICNT .EQ. 1) THEN
	    OPEN(15,FILE=LFILE,STATUS='OLD',IOSTAT=JST,ERR=996)
	    DO J=1,30
	       READ(15,7,END=6) LIGHT(J)
	       NLUM=J
	    END DO
            CLOSE(15)
    6       CALL PRNT(4,1,NLUM,' LIGHT FROM FILE=.')
	 ELSE
	    CALL XVP('LIGHT',LIGHT,LCNT)
            NLUM = LCNT
	 END IF
	
    7    FORMAT(G20.12)

C-------any light entered?
	 IF (LCNT .EQ. 0 .AND. NLUM .EQ. 0) THEN
	    CALL XVMESSAGE(' NO LIGHT DATA INPUT',' ')
	    GO TO 999
	 END IF
	
	 CALL XVPARM('DIRECTIO',DIRECTN,ICNT,IDEF,1) 
         CALL XVPARM('ARRAYSIZ',ASIZ,ICNT,IDEF,1)
	 CALL XVPARM('AREATBL', ATFIL,ATBL,IDEF,1)
	 CALL XVPARM('OFFTBL',  OFFIL,OTBL,IDEF,1)
	 CALL XVPARM('AVOFFTBL',AVFIL,VTBL,IDEF,1)
	 CALL XVPARM('CORRTBL',CORFIL,CTBL,IDEF,2)
	 CALL XVPARM('UNITS',UNITS,ICNT,IDEF,1)

         CALL XVUNIT(IUNI,'INP',1,STAT,' ')
         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
         CALL XVSIZE(SLI,SSI,NLI,NSI,NLI2,NSI2)
         CALL LABPROC(IUNI,NLAB,LABEL)

C        Read in area size fields...
         CALL XLGET (IUNI,'HISTORY','NUM_AREAS',NAREA,STAT,
     &               'FORMAT','INT','HIST','LTGEN',' ')
         CALL XLGET (IUNI,'HISTORY','AREAS',AREA,STAT,'NELEMENT',
     &               4*NAREA,'FORMAT','INT','HIST','LTGEN',' ')

C        Rectangular array of areas is expected, so determine number of columns
         DO 111 I=2,NAREA
            if (area(2,i) .lt. area(2,i-1) ) then
               ncol=i-1
               go to 112
            end if
  111    CONTINUE

  112    CONTINUE   ! exit the loop

         NROW = NAREA/NCOL

         IF (NCOL*NROW.NE.NAREA) GOTO 998
         SW=0.0D0
         SWT=0.0D0
         SWT2=0.0D0

         DO I=1,NAREA
            SWX(I)=0.0D0
            SWXT(I)=0.0D0
            SX2(I)=0.0D0
         ENDDO

         CALL XLGET (IUNI,'HISTORY','NUM_EXPOS',NEXP,STAT,
     &               'FORMAT','INT','HIST','LTGEN',' ')
         CALL XLGET (IUNI,'HISTORY','EXPOSURES',EXPOS,STAT,
     &               'NELEMENT',NEXP,'FORMAT','REAL', 
     &               'HIST','LTGEN',' ')


C        Read data and compute weighted sums...
         DO 50 L = 1,NLI                     !Loop through each exposure...
            CALL XVREAD (IUNI,OUT,STAT,'LINE',L,'NSAMPS',NSI,' ')
            NI = NINT (OUT(1))
            IF (NI .EQ. 0) THEN
               BEXPO = EXPOS(L)
               GOTO 970
            ENDIF
            IF (L .GT. 1) THEN
               W(L-1) = DBLE(LIGHT(L)**2)          ! weights
               SW = SW + W(L-1)                         ! sum weights
               SWT = SWT + W(L-1)*DBLE(EXPOS(L))        ! sum weights * time
               SWT2 = SWT2 + W(L-1)*DBLE(EXPOS(L)**2)
            ENDIF
            IB = 0

            DO 50 K = 1,NAREA                    !Area loop
               SL = AREA(1,K)
               SS = AREA(2,K)
               NL = AREA(3,K)
               NS = AREA(4,K)
               N = NI*NL*NS                   !Total # pixels area*NI
               DNSUM = 0.0D0
               DO I = 1,NI
                  DNSUM = DNSUM + DBLE(OUT(IB+I+1))    !Sum dn across inputs
               ENDDO


               IF (L .EQ. 1) THEN
                  DC(K) = DNSUM/N             !Average Dark Current
               ELSE
                  DN(L-1,K) = DNSUM/N - DC(K)   !Average DN
                  X = DN(L-1,K)/LIGHT(L)
                  SWX(K) = SWX(K) + W(L-1)*DBLE(X)
                  SWXT(K) = SWXT(K) + W(L-1)*DBLE(X)*DBLE(EXPOS(L))
                  SX2(K) = SX2(K) + DBLE(X)**2
               ENDIF
   50    IB = IB + 3*NI

         CALL XVMESSAGE (' ',' ')
         DENOM = SWT**2 - SW*SWT2

C        Compute shutter offset and sensitivity for each area....
         IF (UNITS .EQ. 'RADIANCE') THEN
            CALL XVMESSAGE (RADMSG,' ')
         ELSE
            CALL XVMESSAGE (LUMMSG,' ')
         ENDIF
         CALL XVMESSAGE (' ',' ')
         CALL XVMESSAGE (MS2,' ')
         DO K=1,NAREA
            SL = AREA(1,K)
            SS = AREA(2,K)
            NL = AREA(3,K)
            NS = AREA(4,K)

C           calculate camera sensitivity  
            AO(K) = (SWX(K)*SWT-SWXT(K)*SW)/DENOM          

C           calculate shutter-offset 
            TOS(K) = (AO(K)*SWT-SWX(K))/(AO(K)*SW)

C           output results 
            WRITE (MS1,
     &         '(I4,A1,I4,A1,I4,A1,I4,A1,I4,A5,F12.5,A9,F12.5)')
     &         K,' ',SL,' ',SS,' ',NL,' ',NS,'     ',AO(K),
     &         '         ',TOS(K)
            CALL XVMESSAGE (MS1,' ')
         ENDDO

C        Weed out bad areas...
         CALL ZIA(BAD,NAREA)
         IF (NAREA.GT.1) THEN
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE ('Global value for A0...',' ')
            CALL IMEAN(AO,1,NAREA,BAD,SIGTOL)
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE ('Global shutter offset...',' ')
            CALL IMEAN(TOS,2,NAREA,BAD,SIGTOL)
         ENDIF
         CALL XVMESSAGE (' ',' ')

C        Print out all bad areas...
         DO 100 K=1,NAREA
            IF (BAD(K).EQ.0) GOTO 100
            SL = AREA(1,K)
            SS = AREA(2,K)
            NL = AREA(3,K)
            NS = AREA(4,K)
            WRITE (MS1,'(A5,I3,A16,I4,A1,I4,A1,I4,A1,I4,A1)') 'AREA ',
     &             K,' (SL,SS,NL,NS)=(',SL,',',SS,',',NL,',',NS,')'
            IF (BAD(K).EQ.1) MS1(47:70)='***BAD SENSITIVITY******'
            IF (BAD(K).EQ.2) MS1(47:70)='***BAD SHUTTER OFFSET***'
            IF (BAD(K).EQ.4) MS1(47:70)='***BOTH BAD FIT*********'
            CALL XVMESSAGE (MS1,' ')
  100    CONTINUE

C        Get average DN at each exposure level (using only the
C        good areas)....
C         NPTS = NLI - 2		!Number of exposures (excluding DC)
         NPTS = NLI - 1                !Number of exposures (excluding DC)
         GAREA = 0			!Number of good areas
         CALL ZIA(AVDN,2*NPTS)	!Zero out averages
C
         DO 105 K=1,NAREA
            IF ((IREJCT .EQ. 1) .AND. 
     &          ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 105
            IF ((IREJCT .EQ. 2) .AND. 
     &          ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 105
            IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 105
            GAREA = GAREA + 1
            DO J=1,NPTS
               AVDN(J) = AVDN(J) + DN(J,K)
            ENDDO
  105    CONTINUE

         DO J=1,NPTS
            AVDN(J) = AVDN(J)/GAREA
         ENDDO

         CALL XVMESSAGE (' ',' ')
         IF (IREJCT.EQ.1) RMSG(21:34)='SENSITIVITY   '
         IF (IREJCT.EQ.2) RMSG(21:34)='SHUTTER OFFSET'
         IF (IREJCT.EQ.3) RMSG(21:34)='BOTH          '
         WRITE (RMSG(38:40),'(I3)') NAREA-GAREA
   	 IF (IREJCT.EQ.0) THEN
            RMSG(1:41)='NO REJECTION CRITERIA APPLIED            '
	 ENDIF
         CALL RTIMES(NPTS,LIGHT(2),EXPOS(2),AVDN,IPLOT,NLAB,
     &               NAREA,GAREA,UNITS,ATBL,ATFIL,*999)
C-------------------------------------------------------
         IF (DIRECTN .EQ. 'LINE') THEN
C-----------    LINE DIRECTION    ---------
C              Get average offset for each row...
            NGR = 0			!Number of good rows

            DO 120 I=1,NROW
               AVGOS = 0.0		!Average offset for row I
               NGOOD = 0		!Number of good areas on row

               DO 110 J=1,NCOL
                  K = NCOL*(I-1) + J
                  IF ((IREJCT .EQ. 1) .AND. 
     &                ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 110
                  IF ((IREJCT .EQ. 2) .AND. 
     &                ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 110
                  IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 110
                  SL = AREA(1,K)
                  SS = AREA(2,K)
                  NL = AREA(3,K)
                  NS = AREA(4,K)
                  XTOS(K) = SL + NL/2
                  YTOS(K) = TOS(K)
                  AVGOS = AVGOS + TOS(K)
                  NGOOD = NGOOD + 1
  110          CONTINUE

               IF (NGOOD.GT.0) THEN
                  NGR = NGR + 1
                  LXAV(NGR) = SL + NL/2
                  LYAV(NGR) = AVGOS/NGOOD
               ENDIF
  120       CONTINUE
	 ELSE
C-----------    SAMPLE DIRECTION    ---------
C           Get average offset for each column...
            NGR = 0                     !Number of good cols

            DO 140 I=1,NCOL
               AVGOS = 0.0              !Average offset for col I
               NGOOD = 0                !Number of good areas on col

               DO 130 J=1,NROW
                  K = NCOL*(J-1) + I
                  IF ((IREJCT .EQ. 1) .AND. 
     &                ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 130
                  IF ((IREJCT .EQ. 2) .AND. 
     &                ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 130
                  IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 130
                  SL = AREA(1,K)
                  SS = AREA(2,K)
                  NL = AREA(3,K)
                  NS = AREA(4,K)
                  XTOS(K) = SS + NS/2
                  YTOS(K) = TOS(K)
                  AVGOS = AVGOS + TOS(K)
                  NGOOD = NGOOD + 1
  130          CONTINUE
               IF (NGOOD.GT.0) THEN
                  NGR = NGR + 1
                  LXAV(NGR) = SS + NS/2
                  LYAV(NGR) = AVGOS/NGOOD
                  AVTOS(I) = LYAV(NGR)      !save by column number
               ENDIF
  140       CONTINUE
	 ENDIF

         IF (CTBL .EQ. 2) THEN
C-------open table of uncorrected sensitivity values by grid column
            open(66,file=corfil(1),status='UNKNOWN',IOSTAT=JST,ERR=999)
C-------open table of corrected sensitivity values by grid column
            open(67,file=corfil(2),status='UNKNOWN',IOSTAT=JST,ERR=999)
            IF (DIRECTN .EQ. 'SAMP') THEN

C-----------  averaging grid columns    ---------
               num = ncol
               do ix=2,nlum
                  lt=light(ix)*expos(ix)
                  do i=1,ncol
                     ltc=light(ix)*(expos(ix) - avtos(i))
                     do j=1,nrow
       	                k=ncol*(j-1)+i
        	        u=dn(ix-1,k)/lt
       	                v=dn(ix-1,k)/ltc
       	                colu(ix-1,i) = colu(ix-1,i) + u
       	                colv(ix-1,i) = colv(ix-1,i) + v
                     enddo
                     colu(ix-1,i) = colu(ix-1,i)/nrow
                     colv(ix-1,i) = colv(ix-1,i)/nrow
                  enddo
                  write(66,661) expos(ix),(TAB,colu(ix-1,i),i=1,ncol)
                  write(67,661) expos(ix),(TAB,colv(ix-1,i),i=1,ncol)
               enddo
            ELSE
C------- averaging grid rows
	       num = nrow
               do ix=2,nlum
                  lt=light(ix)*expos(ix)
                  do i=1,nrow
                     ltc=light(ix)*(expos(ix) - avtos(i))
                     do j=1,ncol
                        k=nrow*(j-1)+i
                        u=dn(ix-1,k)/lt
                        v=dn(ix-1,k)/ltc
                        colu(ix-1,i) = colu(ix-1,i) + u
                        colv(ix-1,i) = colv(ix-1,i) + v
                     enddo
                     colu(ix-1,i) = colu(ix-1,i)/ncol
                     colv(ix-1,i) = colv(ix-1,i)/ncol
                  enddo
                  write(66,661) expos(ix),(TAB,colu(ix-1,i),i=1,nrow)
                  write(67,661) expos(ix),(TAB,colv(ix-1,i),i=1,nrow)
               enddo
            ENDIF 

  661       format(1x,F5.0,40(A1,F11.6)) ! the 40 is # of cols in COLU and COLV

            close(66)
            close(67)
         ENDIF
C-------------------------------------------------------
C-----Note:  Now, the xtos and ytos are 0.0 for bad areas
C-------------------------------------------------------
         IF (IPLOT.EQ.1) THEN
            CALL TPLOT(NLAB,YTOS,XTOS,GAREA,LYAV,LXAV,NGR,directn)
            CALL PLOT(0,0,999)
            CALL XVCLOSE(IUNI,STAT,' ')
         ENDIF

C-------Write out tables of 1) line/samp number vs. offset (all good areas)
C-------                    2) avg line/samp num. vs. avg. offset 
         if (otbl .eq. 1)
     &      call tbloff(xtos,ytos,narea,offil,directn,*999)
         if (vtbl .eq. 1)
     &      call tblav(lxav,lyav,ngr,avfil,directn,*999)
C
C	Calculate shutter offsets for each line or samp...
C
         CALL XVPARM('OFFSETS',OFFSET_FILE,CNT,IDEF,1)
         IF (IDEF.EQ.0) THEN
	    CALL XVMESSAGE ('Saving shutter offsets...',' ')
	    CALL LINSHUT( NGR, LXAV, LYAV, 1.0, 1.0, ASIZ,
     &                    LorS_Numbers, Shutter_Offsets )
            CALL XVUNIT(ISO,'X',1,IND,'U_NAME',OFFSET_FILE,' ')
            IF (IND.NE.1) GOTO 997
            CALL XVOPEN(ISO,IND,'OP','WRITE','U_NL',1,'U_NS',ASIZ,
     &                  'U_FORMAT','REAL','O_FORMAT','REAL',
     &                  'OPEN_ACT','SA','IO_ACT','SA',' ')
            CALL XLADD(ISO,'HISTORY','FILE','SHUTTER-OFFSET',
     &                 STAT,'FORMAT','STRING',' ')
            CALL XLADD(ISO,'HISTORY','SO_TYPE',DIRECTN//'-DEPENDENT',
     &                 STAT,'FORMAT','STRING',' ')
            CALL XVWRIT(ISO,Shutter_Offsets,IND,' ')
            CALL XVCLOSE(ISO,IND,' ')
         ENDIF

C        Output centers of all bad areas in MARK format file...
         CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
         IF (STAT.NE.1) GOTO 250            !Skip if output file not specified

C        Put the centers into output buffer (OUTM)...
         I = 0
         DO K = 1,NAREA
            IF ((IREJCT .NE. 0) .AND. (BAD(K) .NE. 0) .AND.
     &          ((IREJCT .EQ. BAD(K)) .OR. (IREJCT .EQ. 3) .OR. 
     &           (BAD(K) .EQ. 4))) THEN
               SL = AREA(1,K)
               SS = AREA(2,K)
               NL = AREA(3,K)
               NS = AREA(4,K)
               I = I + 1
               OUTM(1,I)=SL+NL/2
               OUTM(2,I)=SS+NS/2
            ENDIF
         ENDDO

         NSO = 2*(NAREA-GAREA)
         NLO = 1
         CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &               'OP','WRITE','U_NL',NLO,'U_NS',NSO,'U_FORMAT',
     &               'REAL','O_FORMAT','REAL',' ')
         IF(IREJCT .EQ. 1)
     &      CALL XLADD(OUNI,'HISTORY','CCDRECIP',
     &                 ' REJECTED FOR SENSITIVITY',STAT,'FORMAT',
     &                 'STRING',' ')
         IF(IREJCT .EQ. 2)
     &      CALL XLADD(OUNI,'HISTORY','CCDRECIP',
     &                 ' REJECTED FOR SHUTTER OFFSET',STAT,'FORMAT',
     &                 'STRING',' ')
         IF(IREJCT .EQ. 3)
     &      CALL XLADD(OUNI,'HISTORY','CCDRECIP',
     &                 ' REJECTED FOR BOTH',STAT,'FORMAT','STRING',' ')
         CALL XVWRIT(OUNI,OUTM,STAT,'NSAMPS',NSO,' ')
         CALL XVCLOSE(OUNI,STAT,' ')

  250    CALL XVMESSAGE ('CCDRECIP task completed',' ')
         RETURN

C	 Error conditions...
  970    CALL PRNT(7,1,EXPO,' ***No data for exposure=.')
         CALL XVMESSAGE (
     &      '***Run MOMGEN on this exposure and try again.',' ')
         GOTO 999
  996    CALL XVMESSAGE (' ***Error opening light file',' ')
         CALL PRNT(4,1,JST,' IOSTAT=.')
         GOTO 999
  997    CALL XVMESSAGE ('***Error allocating offset file',' ')
         GOTO 999
  998    CALL XVMESSAGE ('***Error in grid',' ')
         CALL PRNT(4,1,NAREA,' ***Narea=.')
  999    CALL XVMESSAGE ('***CCDRECIP task cancelled',' ')
         CALL ABEND
      END


C Routine to flag all areas which differ from mean by more than SIGTOL
C sigma...
C Inputs: BUF(N) = input samples
C         TYPE = 1 for sensitivity, =2 for offset
C         N = number of samples
C         SIGTOL = 1 or 2 (sigma)
C Outputs: BAD(N) = TYPE if sample is bad type
C                 = 4 if both sensitivity and offset terms are bad
      SUBROUTINE IMEAN(BUF,TYPE,N,BAD,SIGTOL)

         REAL*4 MEAN
         REAL*8 BUF(N),SUM,SSUM
         INTEGER BAD(N),TYPE,SIGTOL
         CHARACTER*44 MSG
     &      /' N=**** MEAN=******.***** SIGMA=******.*****'/

         NLOOP = 4 - SIGTOL

         DO 50 I=1,NLOOP
            NS = 0
            SUM = 0.0D0
            SSUM = 0.0D0

            DO K=1,N
               IF ((BAD(K) .NE. TYPE) .AND. (BAD(K) .NE. 4)) THEN 
                  SAMP = BUF(K)
                  NS = NS+1
                  SUM = SUM+SAMP
                  SSUM = SSUM+SAMP*SAMP
               ENDIF
            ENDDO

            MEAN = SUM/NS
            SIGMA = SSUM/NS-MEAN*MEAN
            SIGMA = SQRT(SIGMA)
            WRITE (MSG,'(A2,I4,A6,F12.5,A7,F12.5)') 'N=',NS,
     &             ' MEAN=',MEAN,' SIGMA=',SIGMA
            IF (I .EQ. 1) 
     &         CALL XVMESSAGE ('Raw mean and sigma are...',' ')
            IF (I .EQ. 2) 
     &         CALL XVMESSAGE (
     &            'After throwing out samples differing by 2 sigma',' ')
            IF (I .EQ. 3) 
     &         CALL XVMESSAGE (
     &            'After throwing out samples differing by 1 sigma',' ')
            CALL XVMESSAGE (MSG,' ')
            IF (I .EQ. NLOOP) GOTO 50

C-------To avoid rejecting all points when perfect data is input
            if (sigma .eq. 0.0) sigma=1.0e-06

            EPS=(3-I)*SIGMA

            DO 20 K=1,N		!Weed out bad samples
               IF ((BAD(K) .EQ. TYPE) .OR. (BAD(K) .EQ. 4)) GOTO 20
               SAMP = BUF(K)
               SAMP = ABS(SAMP-MEAN)
               IF (SAMP .GE. EPS) THEN
                  IF (BAD(K) .NE. 0) THEN
                     BAD(K) = 4
                  ELSE
                     BAD(K) = TYPE
                  ENDIF
               ENDIF
   20       CONTINUE

   50    CONTINUE

         RETURN
      END         



      SUBROUTINE RTIMES(NPTS,L,T,D,IPLOT,NLAB,NAREA,GAREA,
     &                  UNITS,atbl,atfil,*)
         REAL*4 LT,TMT0,L(30),T(30)
         REAL*8 D(30)
         REAL*8 X(30),X2(30),X3(30),DL1(30),DL2(30),DL3(30)
         REAL*8 TOS,AO,STOS,SAO,DY,DZ,XYZ
         REAL*8 SW,SWT,SWX,SWXT,SWT2,W,RX,SX,SX2,SDY

         INTEGER NAREA,GAREA
         integer*4 atbl

         COMMON/CP/VMES,ARMES,RMSG,LABEL
         CHARACTER*4320 LABEL,RMSG*41,VMES*54,ARMES*51

         CHARACTER*19 YT1, YT2, YT3, YT4
         CHARACTER*132 MSG1
         CHARACTER*9 HD1, HD4
         CHARACTER*12 HD2
         CHARACTER*7 HD3
         CHARACTER*13 HD5
         CHARACTER*19 HD6
         CHARACTER*8 HD7
         CHARACTER*14 HD8
         CHARACTER*6 HD9L, HD9R, HD12B
         CHARACTER*10 HD12A
         CHARACTER*4 HD13, HD16
         CHARACTER*3 HD14, HD15
         CHARACTER*40 atfil
         CHARACTER*8 UNITS
         CHARACTER*1 TAB

         YT1 = '(DN-DC)/L'
         YT2 = '(DN-DC)/(L*T)'
         YT3 = '(DN-DC)/(L*(T-TOS))'
         YT4 = ' RESIDUAL FIT'
         HD1 = 'COMMANDED'
         HD2 = 'ILLUMINATION'
         HD3 = '(DN-DC)'
         HD4 = '(DN-DC)/L'
         HD5 = '(DN-DC)/(L*T)'
         HD6 = '(DN-DC)/[L*(T-TOS)]'
         HD7 = 'RESIDUAL'
         HD8 = 'EXPOSURE T(MS)'
         HD9L = 'L(LUM)'
         HD9R = 'L(RAD)'
         HD12A = '(DN/ENERGY'
         HD12B = ' UNIT)'
         HD13 = 'TOS='
         HD14 = 'SD='
         HD15 = 'AO='
         HD16 = 'RMS='

         TAB = CHAR(9)
         CALL XVMESSAGE (' ',' ')
         MSG1 = ' '
         MSG1(4:12)=HD1
         MSG1(17:28)=HD2
         MSG1(33:39)=HD3
         MSG1(46:54)=HD4
         MSG1(60:67)=HD7
         MSG1(74:86)=HD5
         MSG1(90:97)=HD7
         MSG1(100:118)=HD6
         MSG1(121:129)=HD7
         CALL XVMESSAGE (MSG1(2:132),' ')

         MSG1 = ' '
         MSG1(2:15)=HD8
         IF (UNITS .EQ. 'RADIANCE') THEN
           MSG1(19:24)=HD9R
         ELSE
           MSG1(19:24)=HD9L
         ENDIF
         MSG1(45:54)=HD12A
         MSG1(59:68)=HD12A
         MSG1(75:84)=HD12A
         MSG1(89:98)=HD12A
         MSG1(104:113)=HD12A
         MSG1(120:129)=HD12A
         CALL XVMESSAGE (MSG1(2:129),' ')

         MSG1 = ' '
         MSG1(47:52)=HD12B
         MSG1(61:66)=HD12B
         MSG1(77:82)=HD12B
         MSG1(91:96)=HD12B
         MSG1(106:111)=HD12B
         MSG1(122:127)=HD12B
         CALL XVMESSAGE (MSG1(2:127),' ')

         SX = 0.0D0
         SX2 = 0.0D0
         SW = 0.0D0
         SWX = 0.0D0
         SWT = 0.0D0
         SWXT = 0.0D0
         SWT2 = 0.0D0

         DO I=1,NPTS
             W = L(I)**2
             X(I) = D(I)/L(I)   ! calculate (DN-DC)/L
             SX = SX + X(I)
             SX2 = SX2 + X(I)**2
             SW = SW + W
             SWX = SWX + W*X(I)
             SWT = SWT + W*T(I)
             SWXT = SWXT + W*DBLE(T(I))*DBLE(X(I))
             SWT2 = SWT2 + W*DBLE(T(I))**2
         ENDDO

C        calculate camera sensitivity
         AO = (SWX*SWT-SWXT*SW)/(SWT**2-SW*SWT2)

C        calculate shutter-offset
         TOS = (AO*SWT-SWX)/(AO*SW)
         MSG1 = ' '
         RX = 0.0D0
         DZ = 0.0D0
         SDY = 0.0D0

C------OPEN OUTPUT TABLE IF NEEDED
	 if (atbl .eq. 1) THEN
            open(11,file=atfil,status='UNKNOWN',IOSTAT=JST,ERR=999)
 	    IF (UNITS .EQ. 'RADIANCE') THEN
               WRITE(11,10) 'MEAN_DN.aka.D',TAB,'RAD.aka.L',TAB,
     &               'EXP.aka.T',TAB,'L.times.T',TAB,'T.minus.T0.aka.Q',
     &               TAB,'D.over.L',TAB,'D.over.L.times.T',TAB,
     &               'D.over.L.times.Q'
            ELSE
               WRITE(11,10) 'MEAN_DN.aka.D',TAB,'LUM.aka.L',TAB,
     &               'EXP.aka.T',TAB,'L.times.T',TAB,'T.minus.T0.aka.Q',
     &               TAB,'D.over.L',TAB,'D.over.L.times.T',TAB,
     &               'D.over.L.times.Q'
            ENDIF
         ENDIF

   10    FORMAT(1x,A13,A1,A9,A1,A9,A1,A9,A1,A16,A1,A8,A1,A16,A1,A16)

C	 Get residuals
         DO I=1,NPTS

C           calculate (DN-DC)/(L*T)
            X2(I) = D(I)/(DBLE(L(I))*DBLE(T(I)))
C           calculate residual
            DL2(I) = X2(I) - AO

C           calculate (DN-DC)/[L*(T-TO)]
            X3(I) = D(I)/(DBLE(L(I))*(DBLE(T(I))-TOS))
C           calculate residual
            DL3(I) = X3(I) - AO

            RX = RX + DL2(I)**2
            DZ = DZ + DL3(I)**2
            DL1(I) = X(I) - AO*(DBLE(T(I))-TOS)
            SDY = SDY + DL1(I)**2

C           output results in table format
            WRITE (MSG1(3:10),'(F8.4)') T(I)
            WRITE (MSG1(17:24),'(F8.4)') L(I)
            WRITE (MSG1(31:38),'(F8.3)') D(I)
            WRITE (MSG1(45:52),'(F8.4)') X(I)
            WRITE (MSG1(59:66),'(F8.4)') DL1(I)
            WRITE (MSG1(75:82),'(F8.4)') X2(I)
            WRITE (MSG1(89:96),'(F8.4)') DL2(I)
            WRITE (MSG1(104:111),'(F8.4)') X3(I)
            WRITE (MSG1(120:127),'(F8.4)') DL3(I)
            CALL XVMESSAGE (MSG1,' ')

C           write to table file if AREATBL is specified 
            IF (ATBL .EQ. 1) THEN
	       LT = L(I)*T(I)
	       TMT0 = T(I)-TOS
               WRITE(11,9) D(I),TAB,L(I),TAB,T(I),TAB,LT,TAB,TMT0,
     &               TAB,X(I),TAB,X2(I),TAB,X3(I)
            ENDIF
C
         ENDDO

	 IF (ATBL .EQ. 1) CLOSE(11)

    9    FORMAT(1x,2(F12.4,A1),F10.1,A1,3(F16.4,A1),2(F16.6,A1))

         MSG1 = ' '

C        calculate standard deviations of the residuals
         RX = DSQRT(RX)
         DZ = DSQRT(DZ)
         DY = DSQRT(SDY/(NPTS-2.0D0))

         XYZ = NPTS*SX2 - SX**2
         SAO = DY*DSQRT(NPTS/XYZ)
         STOS = DY*DSQRT(SX2/XYZ)

         MSG1(55:58)=HD16
         MSG1(85:88)=HD16
         MSG1(116:119)=HD16
         WRITE (MSG1(60:66),'(F7.4)') DY
         WRITE (MSG1(90:96),'(F7.4)') RX
         WRITE (MSG1(121:127),'(F7.4)') DZ
         CALL XVMESSAGE (MSG1,' ')

         CALL XVMESSAGE (' ',' ')
         MSG1 = ' '
         MSG1(1:4)=HD13
         MSG1(15:17)=HD14
         MSG1(29:31)=HD15
         MSG1(43:45)=HD14
         WRITE (MSG1(5:11),'(F7.4)') TOS
         WRITE (MSG1(18:24),'(F7.4)') STOS
         WRITE (MSG1(32:38),'(F7.4)') AO
         WRITE (MSG1(46:52),'(F7.4)') SAO
         VMES=MSG1
         CALL XVMESSAGE (VMES,' ')
         CALL XVMESSAGE (RMSG,' ')
         WRITE (ARMES(22:25),'(I4)') GAREA
         WRITE (ARMES(33:36),'(I4)') NAREA
         CALL XVMESSAGE (ARMES,' ')

         IF (IPLOT.EQ.0) RETURN

C        plot (DN-DC)/L .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT1,X,T,0,NPTS,AO)
C        plot residual .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT4,DL1,T,3,NPTS,AO)

C        plot (DN-DC)/(L*T) .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT2,X2,T,1,NPTS,AO)
C        plot residual .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT4,DL2,T,2,NPTS,AO)

C        plot (DN-DC)/[L*(T-TO)] .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT3,X3,T,1,NPTS,AO)
C        plot residual .vs. SHUTTER TIME
         CALL RPLOT(NLAB,YT4,DL3,T,2,NPTS,AO)

         RETURN

999	 CALL xvmessage(' ERROR OPENING AREA TABLE FILE',' ')
	 CALL PRNT(4,1,JST,' IOSTAT =.')
	 RETURN 1
      END


C     This subroutine generates a plot of the inputs DY and DX
      SUBROUTINE RPLOT(NLAB,YTITLE,DY,DX,IFLAG,NPTS,DAO)

         COMMON/CP/VMES,ARMES,RMSG,LABEL
         COMMON/PLT1/LXT,LYT,LT
         COMMON/PLT2/XLEN,YLEN,XPAGE,YPAGE,XORIG,YORIG,XDELTA,YDELTA

         INTEGER STATUS, ICNT 
 
         REAL*4 Y(30),X(30),DX(30),AO,ABUF(30),ZBUF(30),XC,YC
         REAL*8 DY(30),DAO

         CHARACTER*19 YTITLE 
         CHARACTER*4320 LABEL
         CHARACTER*41 RMSG
         CHARACTER*54 VMES
         CHARACTER*51 ARMES
         CHARACTER*80 TITLE(80)
         CHARACTER*16 XTITLE

         TITLE(1) = 'CCD RECIPROCITY ANALYSIS'
         XTITLE = 'SHUTTER TIME(MS)'

         LXT = 17
         LYT = 19
         LT = 25
         YC = 12.50           ! set up for HEADER
         XC = 1.50
         YC = YC - 0.25       ! set up for LABEL
         N = MIN0(NLAB,25)

         ICNT = 2

C        insert TASK labels into TITLE
         DO I = 2,N
            TITLE(ICNT)=LABEL(1+(I-1)*72:1+(I-1)*72+71)
            ICNT = ICNT + 1
         ENDDO

C        insert more labels into TITLE
         TITLE(ICNT)=VMES
         ICNT = ICNT + 1
         TITLE(ICNT)=RMSG
         ICNT = ICNT + 1
         TITLE(ICNT)=ARMES

C        display plot header and axis-titles
         CALL HEADER (TITLE,ICNT,0) ! 0=left justify, 1=center justify, 2=right
         CALL AXESTITLES(XTITLE,YTITLE,270,' ',0)

         AO = SNGL(DAO)
         N2 = NPTS + 2

         DO I = 1,N2
            Y(I) = DY(I)
            X(I) = DX(I)
         ENDDO

         DO I = 1,NPTS
            ZBUF(I) = 0.0000
            ABUF(I) = AO
         ENDDO

         XLEN = 8.00
         YLEN = 10.00
         XORIG = 0.0000
         XDELTA = 15.0000
         X(NPTS+1) = XORIG
         X(NPTS+2) = XDELTA

         IF (IFLAG .EQ. 3) GOTO 30
         IF (IFLAG .EQ. 2) GOTO 20
         IF (IFLAG .EQ. 1) GOTO 10
         YORIG = 0.0000
         YDELTA = 2.0000
         Y(NPTS+1) = YORIG
         Y(NPTS+2) = YDELTA
         GOTO 40

   10    YORIG = 0.120
         YDELTA = 0.0100
         Y(NPTS+1) = YORIG
         Y(NPTS+2) = YDELTA
         ABUF(NPTS+1) = YORIG
         ABUF(NPTS+2) = YDELTA
         GOTO 40

   20    YORIG =  -0.100
         YDELTA = 0.020
         Y(NPTS+1) = YORIG
         Y(NPTS+2) = YDELTA
         ZBUF(NPTS+1) = YORIG
         ZBUF(NPTS+2) = YDELTA
         GOTO 40

   30    YORIG =  -1.000
         YDELTA = 0.200
         Y(NPTS+1) = YORIG
         Y(NPTS+2) = YDELTA
         ZBUF(NPTS+1) = YORIG
         ZBUF(NPTS+2) = YDELTA

   40    CONTINUE

         X(NPTS+1) = 0.0
         X(NPTS+2) = 1.0
         Y(NPTS+1) = 0.0
         Y(NPTS+2) = 1.0
         ABUF(NPTS+1) = 0.0
         ABUF(NPTS+2) = 1.0
         ZBUF(NPTS+1) = 0.0
         ZBUF(NPTS+2) = 1.0
         CALL LINE(X,Y,NPTS,1,1,1)
         IF (IFLAG .EQ. 1) CALL LINE(X,ABUF,NPTS,1,1,11)
         IF (IFLAG .EQ. 3) CALL LINE(X,ZBUF,NPTS,1,0,0)
         IF (IFLAG .EQ. 2) CALL LINE(X,ZBUF,NPTS,1,0,0)
         CALL XRTPAGE (STATUS)
         IF (STATUS .NE. 1) GOTO 960
         RETURN

  960    CALL XVMESSAGE ('*** Incomplete Ploting Operation.',' ')
         CALL XVMESSAGE ('XRT Window Button Definitions:',' ')
         CALL XVMESSAGE ('SAVE = write to output PostScript file.',' ')
         CALL XVMESSAGE ('PAGE = display next graph.',' ')
         CALL XVMESSAGE ('EXIT = terminate application.',' ')
         CALL MABEND('***CCDRECIP task cancelled',' ')

      END



      SUBROUTINE TPLOT(NLAB,Y,X,NPTS,LYAV,LXAV,NRGD,DIR)
         REAL*4 Y(402),X(402),LYAV(30),LXAV(30)
         INTEGER*4 ICNT,P

         CHARACTER*4320 LABEL
         CHARACTER*41 RMSG
         CHARACTER*54 VMES
         CHARACTER*51 ARMES
         CHARACTER*47 LMESG
         CHARACTER*80 TITLE(80)
         CHARACTER*11 XTITLE
         CHARACTER*26 YTITLE
         CHARACTER*4 DIR

         COMMON/PLT1/LXT,LYT,LT
         COMMON/PLT2/XLEN,YLEN,XPAGE,YPAGE,XORIG,YORIG,XDELTA,YDELTA
         COMMON/CP/VMES,ARMES,RMSG,LABEL


         LMESG = 'LINE NUMBER=**** SHUTTER OFFSET=*********** MS'
         TITLE(1) = 'CCD RECIPROCITY ANALYSIS'
         XTITLE = 'LINE NUMBER'
         YTITLE = 'GLOBAL SHUTTER OFFSET (MS)'

         IF (DIR(1:4) .EQ. 'SAMP') THEN
	      LMESG(1:4) = DIR(1:4)
	      XTITLE(1:4) = DIR(1:4)
         END IF

         ICNT = 2
         LXT = 11
         LYT = 26
         LT = 25
         YC = 12.50           !SET UP FOR HEADER
         XC = 1.50
C         CALL SYMBOL(XC,YC,0.21,TITLE,0,0.0,LT)
         YC = YC-0.25         !SET UP FOR LABEL
         N = MIN0(NLAB,25)

         DO P=2,N
            TITLE(ICNT)=LABEL(1+(P-1)*72:1+(P-1)*72+71)
            ICNT = ICNT + 1
         ENDDO

         TITLE(ICNT)=VMES
         ICNT = ICNT + 1
         TITLE(ICNT)=RMSG
         ICNT = ICNT + 1
         TITLE(ICNT)=ARMES
         CALL HEADER (TITLE,ICNT,0)
         CALL AXESTITLES(XTITLE,YTITLE,270,' ',0)
 
         N2 = NPTS + 2
         I = 1
         K = 1
         YAV = 0.0E0
         CALL XVMESSAGE (' ',' ')

         DO N=1,NRGD
            WRITE (LMESG(13:16),'(I4)') NINT(LXAV(N))
            WRITE (LMESG(33:43),'(F11.6)') LYAV(N)
            CALL XVMESSAGE (LMESG,' ')
         ENDDO

         X(NPTS+1) = 0.0
         X(NPTS+2) = 1.0
         Y(NPTS+1) = 0.0
         Y(NPTS+2) = 1.0

         CALL LINE(X,Y,NPTS,1,-1,3)

         LXAV(NRGD+1) = 0.0
         LXAV(NRGD+2) = 1.0
         LYAV(NRGD+1) = 0.0
         LYAV(NRGD+2) = 1.0

         CALL LINE(LXAV,LYAV,NRGD,1,0,0)

         RETURN
      END



C     Performs a piece-wise linear interpolation of the calculated
C     shutter offsets to calculate all offsets.  The first two
C     and last two points are use to extrapolate for the first and
C     last few offsets.
      Subroutine LinShut( NIn, XIn, YIn, XStart, DX, NOut, XOut, YOut)
         Implicit None
         Integer  NIn, NOut
         Real XIn(NIn), YIn(NIn), XStart, DX, XOut(NOut), YOut(NOut)
      
         Integer I, IX1, IX2
         Real X, M, B
      
         IX1 = 0
         IX2 = 1
         X = XStart
      
         DO I = 1, NOut
	    IF ( (IX1 .EQ. 0) .OR.		!) THEN
     &	         ( (X .GE. XIn(IX2)) .AND. (IX2 .LT. NIn) ) ) THEN
               IX1 = IX2
               IX2 = IX2 + 1
               M = (YIn(IX1) - YIn(IX2)) / (XIn(IX1) - XIn(IX2))
               B = YIn(IX1) - M*XIn(IX1)
            ENDIF
            XOut(I) = X
            YOut(I) = M*XOut(I) + B
            X = X + DX
         END DO
      
         RETURN
      END


C     This subroutine retrives the VICAR 1 and history label.  It will 
C     display the TASK, USER, and TIME within each TASK entry.
      SUBROUTINE LABPROC(IUNI,NLAB,LABEL)
         IMPLICIT NONE 
         INTEGER INSTANCES(20)
         INTEGER NHIST, J, I             ! loop control variables
         INTEGER NLAB, STAT              ! store # of labels and status
         INTEGER IUNI                    ! store file id number

         CHARACTER*8 TASKS(20)           ! store task nume
         CHARACTER*4320 LABEL            ! store retrived VIC1 label
         CHARACTER*132 MSG               ! store message
         CHARACTER*12 UNAME              ! store user name
         CHARACTER*28 TIME               ! store time
         CHARACTER*65 HBUF

C        initialize display and storage buffers
         HBUF = '----TASK:------------USER:------------------------'
         MSG = ' '
         LABEL = ' '

         CALL VIC1LAB(IUNI,STAT,NLAB,LABEL,0) ! retrive VICAR 1 label
                                              ! NLAB=0 if no VIC1LAB
         NHIST = 20                           ! max # of tasks to retrive

C        retirve history tasks
         CALL XLHINFO(IUNI,TASKS,INSTANCES,NHIST,STAT,' ')

C        for each task, extract the USER name and the DATE_TIME, 
C        write the TASK name, USER name, and DATE_TIME onto buffer (HBUF)
         DO J=1,NHIST
            UNAME = ' '
            TIME = ' '
            CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')  ! retrive user name from task entry
            CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')  ! retrive DATE_TIME from task entry
            DO I=1,8                             !BLANKS NULLS
               IF(TASKS(J)(I:I) .LT. '0') TASKS(J)(I:I) = ' '
            ENDDO
            HBUF(10:17) = TASKS(J)
            HBUF(27:38) = UNAME
            HBUF(39:63) = TIME
            LABEL(1+(NLAB+J-1)*72:(1+(NLAB+J-1)*72)+64) = HBUF
         ENDDO

         NLAB=NLAB+NHIST   ! calculate number of labels to be display 

C        output history tasks retrived from the label
         DO I=1,NLAB
            MSG(1:72) = LABEL(1+(I-1)*72:(1+(I-1)*72)+71)
            CALL XVMESSAGE (MSG,' ')
         ENDDO

         RETURN
      END



      subroutine tbloff(nums,offs,npts,table_ds,directn,*)
         
         REAL*4 NUMS(NPTS),OFFS(NPTS)

         CHARACTER*40 TABLE_DS
         CHARACTER*4 DIRECTN
         CHARACTER*1 TAB

        TAB = CHAR(9)

C-------NUMS IS THE SET OF LINE OR SAMPLE NUMBERS FOR THE AREAS
C-------OFFS IS THE SET OF SHUTTER OFFSETS FOR THE AREAS
C-------both NUMS and OFFS are 0.0 for bad areas
         OPEN(12,FILE=TABLE_DS,STATUS='UNKNOWN',IOSTAT=JST,ERR=999)

         WRITE(12,10) 
     &      DIRECTN//'_OF_AREA',TAB,'CALCULATED.SHUTTER_OFFSET'

   10    FORMAT(1x,A12,A1,A25)

         DO L=1,NPTS
	   IF (NUMS(L) .NE. 0.0) 
     &        WRITE(12,9) NUMS(L),TAB,OFFS(L)
         END DO

         CLOSE(12)
         RETURN

    9    FORMAT(1x,F8.2,A1,F12.4)

  999    CALL xvmessage(' ERROR OPENING OFFSET TABLE FILE',' ')
         CALL PRNT(4,1,JST,' IOSTAT =.')
         RETURN 1
      END



      subroutine tblav(nums,offs,npts,table_ds,directn,*)
 
         REAL*4 NUMS(NPTS),OFFS(NPTS)

         CHARACTER*40 TABLE_DS
         CHARACTER*4 DIRECTN
         CHARACTER*1 TAB


        TAB = CHAR(9)
C-------NUMS IS THE SET OF AVG LINE OR SAMPLE NUMBERS FOR THE ROWS OR COLS
C-------OFFS IS THE SET OF AVG SHUTTER OFFSETS FOR THE ROWS OR COLS

         OPEN(13,FILE=TABLE_DS,STATUS='UNKNOWN',IOSTAT=JST,ERR=999)

         WRITE(13,10) DIRECTN,TAB,'MEAN.SHUTTER_OFFSET'
   10    FORMAT(1x,A4,A1,A19)

         DO L=1,NPTS
            WRITE(13,9) NUMS(L),TAB,OFFS(L)
         END DO

         CLOSE(13)
         RETURN

    9    FORMAT(1x,F8.2,A1,F12.4)

  999    CALL xvmessage(' ERROR OPENING AVERAGE TABLE FILE',' ')
	 CALL PRNT(4,1,JST,' IOSTAT =.')
	 RETURN 1
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create ccdrecip.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM ccdrecip

   To Create the build file give the command:

		$ vimake ccdrecip			(VMS)
   or
		% vimake ccdrecip			(Unix)


************************************************************************/


#define PROGRAM	ccdrecip
#define R2LIB

#define MODULE_LIST ccdrecip.f

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
$ create ccdrecip.pdf
PROCESS HELP=*
PARM INP        TYPE=STRING     COUNT=1
PARM OUT        TYPE=STRING     COUNT=(0:1)     DEFAULT=--
PARM PLOT       TYPE=STRING     COUNT=(0:1)     DEFAULT=--
PARM SIGTOL     TYPE=INTEGER    COUNT=(0:1)     DEFAULT=2       VALID=(1,2)
PARM REJECT     TYPE=INTEGER    COUNT=(0:1)     DEFAULT=1
PARM UNITS      TYPE=KEYWORD    COUNT=(0:1)     DEFAULT=RADIANCE +
                                            VALID=(RADIANCE,LUMINANC)
PARM LIGHT      TYPE=REAL       COUNT=(0:30)    DEFAULT=--
PARM LTFILE     TYPE=STRING     COUNT=(0:1)     DEFAULT=--
PARM DIRECTIO   TYPE=KEYWORD    COUNT=(0:1) VALID=(LINE,SAMP) DEFAULT=LINE
PARM ARRAYSIZ   TYPE=INTEGER    COUNT=(0:1)     DEFAULT=1024
PARM AREATBL    TYPE=STRING     COUNT=(0:1)     DEFAULT=--
PARM OFFTBL     TYPE=STRING     COUNT=(0:1)     DEFAULT=--
PARM AVOFFTBL   TYPE=STRING     COUNT=(0:1)     DEFAULT=--
PARM CORRTBL    TYPE=STRING     COUNT=(0,2)     DEFAULT=--
PARM OFFSETS    TYPE=STRING     COUNT=(0:1)     DEFAULT=--
END-PROC
.TITLE
VICAR Application Program CCDRECIP
.HELP
PURPOSE:

CCDRECIP determines the shutter offset (in msec) and sensitivity
(in DN per foot-lambert-milliseconds (UNITS = LUMINANC) or
in DN per picoamp-milliseconds (UNITS = RADIANCE)) for a
camera system.  The program is one of a series of programs originally
developed to support radiometric calibration of the Galileo SSI camera system.
(UNITS should be LUMINANC for Galileo and RADIANCE for Cassini).

Reference:
    D-4264  MIPL Software Structural Design for the Instrument
            Calibration of GLL SSI Science Processing.
    D-tbd   Software Design Document for Instrument Calibration -
            Cassini ISS

.PAGE
Vicar Plotting

The Vicar Plotting system can produce plots on a display device if required.
The plotting system always generates a postscript file suitable for plotting on 
either a hard copy device or a display device. The postscript file can be 
defaulted to a system derived name or can be explicitly named by the user.  

The user can run plotting programs either interactively or in batch.  Whether 
or not plots are required, a plotting device (node) must be specified. This may
or may not be the plotting device the user requires.  To override the system 
defaulted device, the user must select a display device.  This is implemented 
from an Alpha using the following following:
           set display/create/trans=tcpip/node=poconos.  
This allocates a UNIX workstation named "poconos". If the user is on an Alpha
and wishes to plot to a UNIX workstation, the user first type: 
           xhost coda2 
where coda2 is the name of the Alpha from which plots will be sent.  
.PAGE
To plot on a UNIX workstation, the user needs to type on the UNIX workstation
           setenv DISPLAY poconos:0.0
which allocates the UNIX workstation named "poconos".      
Note: poconos and coda2 are examples of nodes.  The user may send and direct 
plots from and to any node he chooses.  

Interactively, either on the command line or in a pdf, the user may decide 
decide whether or not to plot the graphics.   If the user requires only a hard
copy, he must add 'nodisp to the program command line.   This will inhibit the 
plotting of the graphics on a display device.  

In batch, the user may not request plots on a display device.  Only postscript
files will be produced.  In order to generate postscript files in batch mode , 
the user MUST allocate a display device from WITHIN the batch script.  If the 
user fails to do this, no postscript files will be produced.    

.PAGE
EXECUTION:
                CCDRECIP INP=RCP.DAT OUT=MARK.DAT PARAMS

The input is a Reciprocity File (RCP) containing statistical data for
specified areas in the image for each exposure of a reciprocity sequence.
The RCP must have been previously initialized via LTGEN and loaded with
data via MOMGEN.

The output is an optional MARK-format tiepoint data set containing the
centers of all areas rejected for producing values for SENSITIVITY or
SHUTTER OFFSET or either which differ by more than 2 sigma from the mean
values for all the areas.

.PAGE
MATHEMATICAL BACKGROUND:

The output camera signal is proportional to exposure as follows:

      DN-DC = A*L*(T-To)
where
      DN-DC is the output signal minus the dark current,
      A is the camera sensitivity (DN/foot-lambert-milliseconds (LUMINANC)) or
                                   DN/picoamp-milliseconds (RADIANCE))
      L is the light cannon setting (foot-lamberts (LUMINANC)) or
        is the spectral radiance of the source (picoamp (RADIANCE)),
      T is the commanded exposure time (milliseconds), and
      To is the shutter-offset (milliseconds).

CCDRECIP solves for the sensitivity A and shutter-offset To, given data
points DN acquired by varying the light cannon setting (or spectral radiance of
the source) and exposure time:
         i
                DN - DC = A*L *(T -To)
                  i          i   i

.PAGE
OPERATION:

CCDRECIP performs the following steps:

  1) Read data from the Reciprocity File.
  2) Compute the sensitivity and shutter offset for each area.
  3) Compute mean values for sensitivity and shutter-offset (by averaging
     the values extracted from each area) and flag all areas deviating
     by more than 1 or 2 sigma from the mean.
  4) Re-compute the mean value for sensitivity and shutter-offset,
     ignoring all flagged values as specified by the REJECT parameter.

If the REJECT parameter is specified (default=2), areas may be rejected
because of a bad value for sensitivity (REJECT=1), shutter-offset (REJECT=2),
or either (REJECT=3).  If REJECT=0, no area rejection is performed.

CCDRECIP prints out the following:

  1) Sensitivity and shutter-offset for each area.
  2) Summary of all areas with bad values for sensitivity or shutter-offset.
  3) Mean sensitivity as a function of exposure time.
  4) Global value for sensitivity and shutter-offset, obtained by combining
     data from all good areas.
  5) Shutter-offset as a function of image line or sample number.

Note that the sensitivity and offset are listed as AO and TOS in the printout.

If the PLOT keyword is specified, CCDRECIP produces the following plots:

  1) (DN-DC)/L vs SHUTTER TIME
  2) (DN-DC)/(L*T) vs SHUTTER TIME
  3) (DN-DC)/[L*(T-To)] vs SHUTTER TIME
  4) To vs image line or sample number.  The raw points are plotted with
     "+" and the average shutter offset at a given line or sample number
     is plotted as a solid line.

The resulting plot file should be printed out using the NOFEED qualifier
(see example below).

Four types of tabular output data are also available.  The AREATBL
parameter produces a tab-delimitted ASCII text file containing:
if UNITS = LUMINANC, MEAN_DN(D), LUM(L), EXP(T), L*T, ACTUAL(T-To), D/L,
D/L*T, and D/L*(T-To), and if UNITS = RADIANCE, MEAN_DN(D), RAD(L), EXP(T),
L*T, ACTUAL(T-To), D/L, D/L*T, and D/L*(T-To), for each exposure level.

The OFFTBL parameter produces a tab-delimitted ASCII text file containing:
LINE or SAMPLE and calculated SHUTTER_OFFSET for all good areas.

The AVOFFTBL parameter produces a tab-delimitted ASCII text file containing:
LINE or SAMPLE and mean SHUTTER_OFFSET for each row or column of grid areas.

The CORRTBL parameters produces two files tabulating the correction
achieved as a result of using the derived shutter-offset.  The first
of the two files holds the uncorrected sensitivity values averaged over
each column or row (see DIRECTIO parameter) of grid points.  The second
file holds the corrected values.  If DIRECTIO is LINE, then the values
of the grid rows are averaged.  If SAMP, then the values of the grid
columns are averaged.

If an output file is specified, the centers of all flagged areas (as
specified by the REJECT parameter) are stored in a MARK-format tiepoint
data set.  These areas can be subsequently displayed (see example below)
to indicate the spatial distribution of regions which give rise to bad
sensitivity or shutter-offset constants.

If an output shutter-offset file is specified via the OFFSET parameter,
then a file containing shutter offsets for each image line or sample is
generated.  These offsets are calculated by using the average shutter-offsets
as found above and then performing a piece-wise linear interpolation for the
lines or samples that fall between data points.  A linear extrapolation is
done at each end using the first and last two points.  These shutter-offsets
can later be used as inputs to GALGEN, see GALGEN's TUTOR and HELP files.

Instead of entering the light values of the exposure levels as a
multivalued parameter, they can be contained in an ASCII file.  This file
merely contains one light value per record (see procedure MOMGEN2 for
the format details).  The file is specified to CCDRECIP using the LTFILE
parameter.

NOTE:  The first value in the LIGHT parameter set or the LTFILE
       file should be 0.0 to correspond with EXPO=0.0 for the dark
       current frames.

The parameter DIRECTIO is used to tell CCDRECIP to derive a Line- or a
Sample-dependent shutter-offset.

Because CCDRECIP is dealing with the Light Transfer File and not the raw
images, it doesn't know how big they are.  Therefore, the user must tell
CCDRECIP how many elements the shutter-offset should contain.  This is
done with the ARRAYSIZ parameter.

NOTE:  CCDRECIP CANNOT be used with reciprocity sequences that contain
       extended dark current or extended exposure frames.

.PAGE
EXAMPLE:

    PLOTTING DEVICE=PRINT               !Specify plotting device
    CCDRECIP RCP.DAT MRK.DAT PLOT=RCP.PLT OFFSETS=OFFSETS.DAT
    MARK (PIC,MRK.DAT) OUT              !Scribe boxes around bad centers
    JDISP OUT                           !Display bad centers
    DCL PRINT/NOFEED RCP.PLT

.PAGE
ORIGINAL PROGRAMMER: Mike Morrill, Oct 84
COGNIZANT PROGRAMMER: Gary Yagi
REVISION HISTORY:

 27 Apr 99  gmy  Declared P as integer to avoid compiler error on SGI
 25 Mar 97...T.Huang........Ported from VAX to UNIX to support both
                            Cassini and Galileo data.
  1 Jan 97...c.c.avis.......allow rectangular grids
 16 Jul 96...c.c.avis.......added correction tables by row or column
 29 APR 96...c.c.avis.......changed decimal places in output table
 24 APR 96...c.c.avis.......change f12.5 to g20.12 in reading LTFILE
 22 AUG 95...c.c.avis.......Added tests involving noise
 02 JAN 95...J.R.YOSHIMIZU..Changed LUMINANC to LIGHT and LUMFILE to LTFILE.
                            Added UNITS
 21 DEC 94...C.C.Avis.......Clarified Help on Reject parameter.
 20 JUN 94...C.C.Avis.......Fixed xladd to SO file not mark file (bug),
                            added table outputs, added use of LUMFILE,
                            added sample-dependent shutter-offset
 26 APR 88...G.M.Yagi.......Added more documentation to help file.
 04 Nov 87...G.M.Yagi.......Shutter offset file changed to Image format.
 01 Nov 87...G.M.Yagi.......Convert to new CPLT plotting routines.
 14 JAN 87...G.M.Yagi.......Fix so plot is optional.
  1 AUG 86...G.M.Yagi.......Code and documentation clean-up.
 29 OCT 85...R.A.MORTENSEN..Added output of all 800 shutter offsets.
 26 FEB 85...M.E.MORRILL....ADD PLOT OF GLOBAL SHUTTER OFFSET.
 15 FEB 85...M.E.MORRILL....ADD SIGMA TOLERANCE PARAMETER.
 26 JAN 85...M.E.MORRILL....VERSION 1*A RELEASED FOR USE.
 14 JAN 85...M.E.MORRILL....ENLARGED BUFFERS FOR 400 AREAS.
  7 JAN 85...M.E.MORRILL....MARK OUTPUT FOR REJECTED AREAS.
 21 DEC 84...M.E.MORRILL....PLOTING PACKAGES ADDED.
 13 DEC 84...M.E.MORRILL....USES GOOD AREAS TO TABULATE RESULTS.
  2 NOV 84...M.E.MORRILL....TRACKS REJECTED AREAS WITH
                              3 CLASSES: AO,TOS, BOTH.
  8 OCT 84...M.E.MORRILL....INITIAL RELEASE.

.LEVEL1
.VARIABLE INP
The Reciprocity File
created by LTGEN/MOMGEN
.VARIABLE OUT
A MARK-format file
contining centers of
rejected areas.
.VARIABLE PLOT
Output plot file
.VARIABLE SIGTOL
Specifies 1 or 2 Sigma
rejection from mean values.
.VARIABLE REJECT
Specifies whether to
reject areas based on
bad sensitivity,
bad shutter-offset, or
either, or no rejection.
.VARIABLE UNITS
Specifies whether the
illumination values are
RADIANCE or LUMINANC
.VARIABLE LIGHT
Illumination values in
Relative Foot-Lamberts
or picoamp)
First value=0.0 for DC.
.VARIABLE LTFILE
Name of file containing
list of illumination
values in Relative
Foot-Lamberts or picoamp).
.VARIABLE DIRECTIO
Direction of shutter
movement.
.VARIABLE ARRAYSIZ
Number of pixels in
the direction of
shutter movement.
.VARIABLE AREATBL
File to receive table
of stats for each
area.
.VARIABLE OFFTBL
File to receive table
of pixel number vs.
calculated offset.
.VARIABLE AVOFFTBL
File to receive table
of pixel number vs.
calculated offset
averaged by row or
column.
.VARIABLE CORRTBL
Files to receive the
uncorrected and corrected
sensitivity values by
grid row or column.
.VARIABLE OFFSETS
Specifies the name
of an output file to
receive the shutter
offsets for each image
line or sample.
.LEVEL2
.VARIABLE INP
The Reciprocity File created by LTGEN and MOMGEN
containing area statistics for calculating the sensitivity
and shutter-offset.
.VARIABLE OUT
A MARK formatted data set containing rejected area locations.
.VARIABLE SIGTOL
Specifies the number of standard deviations from the mean which
signifies a bad area.  See REJECT parameter.
.VARIABLE REJECT
REJECT=0  No area rejection performed
      =1  Reject areas with bad sensitivity
      =2  Reject areas with bad shutter-offset
      =3  Reject areas with either bad sensitivity or offset
Areas with values differing from the mean by more than SIGTOL sigma are
rejected.
.VARIABLE UNITS
Specifies whether the illumination values are in LUMINANC (Relative-Foot-
Lamberts ) or RADIANCE (picoamp).
(UNITS should be LUMINANC for Galileo and RADIANCE for Cassini).
.VARIABLE LIGHT
The illumination (in Relative Foot Lamberts or picoamp)
for each exposure level of the reciprocity sequence.  The first entry should
be 0.0, corresponding to EXPO=0.0 in the reciprocity file for the Dark Current
frames.
.VARIABLE LTFILE
Name of file containing list of illumination values in Relative Foot-Lamberts
or picoamp).  This is an ASCII text file containing one
record for each exposure level from dark-current (record 1) to the highest
exposure level (the last record).  Each record contains one floating point
value denoting the illumination value for that exposure level.

.VARIABLE DIRECTIO
Specifies whether to derive a line-dependent or a sample-dependent shutter-
offset.  This corresponds to the direction of shutter movement (LINE or
SAMP).

.VARIABLE ARRAYSIZ
Specifies how many elements to calculate for the output shutter-offset file.
This correspondes to the number of image pixels in the direction of shutter
movement (i.e., the number of image lines or the number of image samples).

.VARIABLE PLOT
Specifies the name of a file to contain the plot data when the plotting
device is specified as the printer (i.e., PLOTTING 'PRINTRONX) or other
devices besides the display monitor.  For the printer, the file must be
printed using the /NOFEED qualifier.

.VARIABLE AREATBL
The AREATBL parameter produces a tab-delimitted ASCII text file containing:
MEAN_DN(D), LUM(L), EXP(T), L*T, (T-To), D/L, D/L*T, and D/L*(T-To)
for each exposure level.

.VARIABLE OFFTBL
The OFFTBL parameter produces a tab-delimitted ASCII text file containing:
For line-dependent shutter-offsets: Line number and calculated SHUTTER_OFFSET
for all good areas.
For sample-dependent shutter-offsets: Sample number and calculated SHUTTER_
OFFSET for all good areas.

.VARIABLE AVOFFTBL
The AVOFFTBL parameter produces a tab-delimitted ASCII text file containing:
For line-dependent shutter-offsets: Line number and mean SHUTTER_OFFSET for
each row of grid areas.
For sample-dependent shutter-offsets: Sample number and mean SHUTTER_OFFSET for
 each column of grid areas.

.VARIABLE CORRTBL
STRING - COUNT= 0:2 OPTIONAL
Specifies the two files two contain the uncorrected and the corrected
sensitivity values averaged by grid column or row.  The DIRECTIO parameter
specifies whether averaging is done by row or column.  If DIRECTIO is LINE,
then the values of the grid rows are averaged.  If SAMP, then the values
of the grid columns are averaged.

.VARIABLE OFFSETS
Specifies the name of an output file that will receive the shutter-offsets for
each image line or sample from 1 to ARRAYSIZ.  The file is in standard VICAR
image format and is used as an input to programs CCDSLOPE, GALGEN, and GALSOS.
.END

$ Return
$!#############################################################################
$Test_File:
$ create tstccdrecip.pdf
procedure
refgbl $autousage
refgbl $echo
refgbl $syschar
body
local dir string
let $autousage="none"
let _onfail="continue"
let $echo="yes"

if ($syschar(1)="UNIX")
   let dir = "/project/test_work/testdata/cassini/iss/"
   defcmd-replace typeit "ush cat"
else
   let dir = "wms_test_work:[testdata.cassini.iss]"
   defcmd-replace typeit "dcl type"
end-if 

!=================================================================
!First, check the results from simulated data to verify algoritm.
!Second, see that the sample- and line-dependent modes get the
!same answer.
!=================================================================
 
!CASSINI TEST:
 
!---------------------------
! Make TEST.LTF a test reciprocity file which has exposure levels of
! 0,10,20,40 and each input frame was 10, 910, 960, 985 dn, with
! radiances of 0,100,50,25 respectively.
!
! A sequence with a shutter-offset of zero would have DNs of exactly
! Radiance*EXP + DC or (10, 1010, 1010, 1010) assuming a sensitivity of 1.0.
! The use of uniform images of (10, 910, 960, 985) DN should give a
! shutter-offset of 1.0.
!---------------------------
 
!Set dns to 10 and replicate - exposure = 0.0
f2 inp=&"dir"sum2.1 out=l1.a func=10
copy l1.a l1.b
copy l1.a l1.c
 
!Set dns to 910 and replicate - set exposure to 10, radiance to 100
f2 inp=&"dir"sum2.1 out=l2.a func=910
label-rep l2.a 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=10. +
            RADIANCE=100."
copy l2.a l2.b
copy l2.a l2.c
 
!Set dns to 960 and replicate - set exposure to 20, radiance to 50
f2 inp=&"dir"sum2.1 out=l3.a func=960
label-rep l3.a 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=20. +
            RADIANCE=50."
copy l3.a l3.b
copy l3.a l3.c
 
!Set dns to 985 and replicate - set exposure to 40, radiance to 25
f2 inp=&"dir"sum2.1 out=l4.a func=985
label-rep l4.a 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=40. +
            RADIANCE=25."
copy l4.a l4.b
copy l4.a l4.c
 
!Create list of the files created
createfile l.list
addtofile l.list "NEXT FILE=0001"
addtofile l.list "l1.a"
addtofile l.list "l1.b"
addtofile l.list "l1.c"
addtofile l.list "l2.a"
addtofile l.list "l2.b"
addtofile l.list "l2.c"
addtofile l.list "l3.a"
addtofile l.list "l3.b"
addtofile l.list "l3.c"
addtofile l.list "l4.a"
addtofile l.list "l4.b"
addtofile l.list "l4.c"
reset l.list
typeit l.list 
 
!Initialize Light Transfer File
ltgen l1.a out=test.ltf list=l.list 'grid
 
!Fill Light Transfer File with stats
momgen2 list=l.list ltfrcp=test.ltf
 
ccdrecip test.ltf 'SAMP  light=(0,100,50,25) 'RADIANCE +
 arraysiz=512  offset=so.file  areatbl=area.tbl +
 offtbl=off.tbl  avofftbl=meanoff.tbl

typeit area.tbl
typeit off.tbl
typeit meanoff.tbl

!---------------------------
! Repeat with noise-added input data
!---------------------------
!Set dns to 10 and replicate - exposure = 0.0
 
gausnois a.dat mean=0 sigma=3 format=half seed=13 nl=512 ns=512
f2 (&"dir"sum2.1, a.dat) l1.a func=10+in2
gausnois a.dat mean=0 sigma=3 format=half seed=17 nl=512 ns=512
f2 (&"dir"sum2.1, a.dat) l1.b func=10+in2
gausnois a.dat mean=0 sigma=3 format=half seed=19 nl=512 ns=512
f2 (&"dir"sum2.1, a.dat) l1.c func=10+in2
 
!Set dns to 910 and replicate - set exposure to 10, radiance to 100
gausnois a.dat mean=0 sigma=30 format=half seed=13 nl=512 ns=512
f2 (&"dir"sum2.1, a.dat) l2.a func=910+in2
label-rep l2.a 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=10. +
         RADIANCE=100."
gausnois a.dat mean=0 sigma=30 format=half seed=17 nl=512 ns=512
f2 (&"dir"sum2.1, a.dat) l2.b func=910+in2
label-rep l2.b 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=10. +
         RADIANCE=100."
gausnois a.dat mean=0 sigma=30 format=half seed=19 nl=512 ns=512
f2 (&"dir"sum2.1, a.dat) l2.c func=910+in2
label-rep l2.c 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=10. +
         RADIANCE=100."
 
!Set dns to 960 and replicate - set exposure to 20, radiance to 50
gausnois a.dat mean=0 sigma=30 format=half seed=13 nl=512 ns=512
f2 (&"dir"sum2.1, a.dat) l3.a func=960+in2
label-rep l3.a 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=20. +
         RADIANCE=50."
gausnois a.dat mean=0 sigma=30 format=half seed=17 nl=512 ns=512
f2 (&"dir"sum2.1, a.dat) l3.b func=960+in2
label-rep l3.b 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=20. +
         RADIANCE=50."
gausnois a.dat mean=0 sigma=30 format=half seed=19 nl=512 ns=512
f2 (&"dir"sum2.1, a.dat) l3.c func=960+in2
label-rep l3.c 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=20. +
         RADIANCE=50."
 
!Set dns to 985 and replicate - set exposure to 40, radiance to 25
gausnois a.dat mean=0 sigma=30 format=half seed=13 nl=512 ns=512
f2 (&"dir"sum2.1, a.dat) l4.a func=985+in2
label-rep l4.a 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=40. +
         RADIANCE=25."
gausnois a.dat mean=0 sigma=30 format=half seed=17 nl=512 ns=512
f2 (&"dir"sum2.1, a.dat) l4.b func=985+in2
label-rep l4.b 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=40. +
         RADIANCE=25."
gausnois a.dat mean=0 sigma=30 format=half seed=19 nl=512 ns=512
f2 (&"dir"sum2.1, a.dat) l4.c func=985+in2
label-rep l4.c 'prop property="CASSINI-ISS" item="EXPOSURE_DURATION=40. +
         RADIANCE=25."
 
!Create list of the files created
createfile l.list
addtofile l.list "NEXT FILE=0001"
addtofile l.list "l1.a"
addtofile l.list "l1.b"
addtofile l.list "l1.c"
addtofile l.list "l2.a"
addtofile l.list "l2.b"
addtofile l.list "l2.c"
addtofile l.list "l3.a"
addtofile l.list "l3.b"
addtofile l.list "l3.c"
addtofile l.list "l4.a"
addtofile l.list "l4.b"
addtofile l.list "l4.c"
reset l.list 
typeit l.list
 
!Initialize Light Transfer File
ltgen l1.a out=test.ltfn list=l.list 'GRID
 
!Fill Light Transfer File with stats
momgen2 list=l.list ltfrcp=test.ltfn
 
ccdrecip test.ltfn 'SAMP  light=(0,100,50,25) 'RADIANCE +
 arraysiz=512  offset=so.filen  areatbl=area.tbln +
 offtbl=off.tbln  avofftbl=meanoff.tbln

typeit area.tbln
typeit off.tbln
typeit meanoff.tbln
 
!---------------------------
! Deriving the sample-dependent shutter-offset
!---------------------------
!Introduce a shutter offset in the sample direction by creating a
! ramp in DN in the sample direction.  At each exposure level, use
! a different amount of ramping across the 512 samples.  A ramp of101 DN
! for exp=100, 51 for exp=50 and 26 for exp=26 will be used to generate
! a shutter-offset which goes from 1.0 at sample 1 to 2.0 at sample 512.
! (i.e., an average offset of 1.5)
!---------------------------
 
gen x.dat nl=512 ns=256 ival=0 linc=0 sinc=1
 
copy x.dat a.dat (1,1,512,101)
size a.dat xa.dat (1,1,512,512)
f2 (l2.a, xa.dat) l6.a func=in1-in2
maxmin l6.a (1,1,1,512)
copy l6.a l6.b
copy l6.a l6.c
 
copy x.dat a.dat (1,1,512,51)
size a.dat xa.dat (1,1,512,512)
f2 (l3.a, xa.dat) l7.a func=in1-in2
maxmin l7.a (1,1,1,512)
copy l7.a l7.b
copy l7.a l7.c
 
copy x.dat a.dat (1,1,512,26)
size a.dat xa.dat (1,1,512,512)
f2 (l4.a, xa.dat) l8.a func=in1-in2
maxmin l8.a (1,1,1,512)
copy l8.a l8.b
copy l8.a l8.c
 
!Create list of the files created
createfile l.list
addtofile l.list "NEXT FILE=0001"
addtofile l.list "l1.a"
addtofile l.list "l1.b"
addtofile l.list "l1.c"
addtofile l.list "l6.a"
addtofile l.list "l6.b"
addtofile l.list "l6.c"
addtofile l.list "l7.a"
addtofile l.list "l7.b"
addtofile l.list "l7.c"
addtofile l.list "l8.a"
addtofile l.list "l8.b"
addtofile l.list "l8.c"
reset l.list
typeit l.list
 
!Initialize Light Transfer File
ltgen l1.a out=test.ltf list=l.list 'GRID
 
!Fill Light Transfer File with stats
momgen2 list=l.list ltfrcp=test.ltf
 
ccdrecip test.ltf 'SAMP  light=(0,100,50,25) 'RADIANCE +
 arraysiz=512  offset=so.file  areatbl=area.tbl +
 offtbl=off.tbl  avofftbl=meanoff.tbl reject=0

typeit area.tbl
typeit off.tbl
typeit meanoff.tbl
 
if ($syschar(1) = "UNIX")
   ush rm l1.*
   ush rm l2.*
   ush rm l3.*
   ush rm l4.*
   ush rm l.list
   ush rm l6.*
   ush rm l7.*
   ush rm l8.*
else
   dcl del l1.*;*
   dcl del l2.*;*
   dcl del l3.*;*
   dcl del l4.*;*
   dcl del l.list;*
   dcl del l6.*;*
   dcl del l7.*;*
   dcl del l8.*;* 
end-if
 
!=================================================================
 
!make galileo style (line-dependent shutter-offset) RCP file
ltgen &"dir"rcp_5.byte rcp.tst 'GRID ni=3 +
   exp=(0,4,6,8,12,17,25,33,50,67,100)
 
momgen +
  (&"dir"rcp_1.byte,&"dir"rcp_2.byte,&"dir"rcp_3.byte) +
  rcp.tst exp=0
momgen +
  (&"dir"rcp_4.byte,&"dir"rcp_5.byte,&"dir"rcp_6.byte) +
  rcp.tst exp=4
momgen +
  (&"dir"rcp_7.byte,&"dir"rcp_8.byte,&"dir"rcp_9.byte) +
  rcp.tst exp=6
momgen +
  (&"dir"rcp_10.byte,&"dir"rcp_11.byte,&"dir"rcp_12.byte) +
  rcp.tst exp=8
momgen +
  (&"dir"rcp_13.byte,&"dir"rcp_14.byte,&"dir"rcp_15.byte) +
  rcp.tst exp=12
momgen +
  (&"dir"rcp_16.byte,&"dir"rcp_17.byte,&"dir"rcp_18.byte) +
  rcp.tst exp=17
momgen +
  (&"dir"rcp_19.byte,&"dir"rcp_20.byte,&"dir"rcp_21.byte) +
  rcp.tst exp=25
momgen +
  (&"dir"rcp_22.byte,&"dir"rcp_23.byte,&"dir"rcp_24.byte) +
  rcp.tst exp=33
momgen +
  (&"dir"rcp_25.byte,&"dir"rcp_26.byte,&"dir"rcp_27.byte) +
  rcp.tst exp=50
momgen +
  (&"dir"rcp_28.byte,&"dir"rcp_29.byte,&"dir"rcp_30.byte) +
  rcp.tst exp=67
momgen +
  (&"dir"rcp_31.byte,&"dir"rcp_32.byte,&"dir"rcp_33.byte) +
  rcp.tst exp=100
 
 
!make cassini style (sample-dependent shutter-offset) RCP file
!make sure the flotted pixels wind up in the proper place
!for the new grid to use them
 
 
flot &"dir"rcp_1.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_2.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_3.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
 
ltgen b.dat rcps.tst 'GRID ni=3 exp=(0,4,6,8,12,17,25,33,50,67,100)
 
momgen (a.dat,b.dat,c.dat) rcps.tst exp=0
flot &"dir"rcp_4.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_5.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_6.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=4
flot &"dir"rcp_7.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_8.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_9.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=6
flot &"dir"rcp_10.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_11.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_12.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=8
flot &"dir"rcp_13.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_14.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_15.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=12
flot &"dir"rcp_16.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_17.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_18.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=17
flot &"dir"rcp_19.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_20.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_21.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=25
flot &"dir"rcp_22.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_23.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_24.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=33
flot &"dir"rcp_25.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_26.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_27.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=50
flot &"dir"rcp_28.byte ax.dat 'coun
fastmos ax.dat a.dat OFF1=(-5,1)
flot &"dir"rcp_29.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_30.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=67
flot &"dir"rcp_31.byte ax.dat 'coun
fastmos ax.dat a.dat off1=(-5,1)
flot &"dir"rcp_32.byte ax.dat 'coun
fastmos ax.dat b.dat off1=(-5,1)
flot &"dir"rcp_33.byte ax.dat 'coun
fastmos ax.dat c.dat off1=(-5,1)
momgen (a.dat,b.dat,c.dat) rcps.tst exp=100
 
!RUN CCDRECIP IN LINE- AND SAMPLE-DEPENDENT CASES AND COMPARE
 
!plotting 'print
 
!THE OUTPUT OF THE LINE-DEPENDENT RUNS AND THE SAMPLE-DEPENDENT RUNS
!MAY BE DIRECTLY COMPARED EXCEPT FOR THE AREA NUMBERS.  BECAUSE THE
!INPUT DATA HAS BEEN FLOTTED FOR THE SAMPLE-DEPENDENT CASE, THE PIXELS
!THAT WERE IN AREA X ARE NOW IN AREA Y.  SO THE RESULTS FOR AREAS X AND Y
!CAN BE COMPARED.
 
!            LINE CASE                   SAMP CASE
!      X = grid_row,grid_col  -->    11-grid_col,grid_row = Y
! e.g.
!      AREA 19 = (2,9)        -->    (2,2) = AREA 12
 
!MAKE THE LIGHT FILE FOR INPUT LATER
createfile lt.dat 
addtofile lt.dat "0.0"
addtofile lt.dat "74.5"
addtofile lt.dat "50.5"
addtofile lt.dat "38.7"
addtofile lt.dat "26.0"
addtofile lt.dat "20.0"
addtofile lt.dat "12.8"
addtofile lt.dat "9.9"
addtofile lt.dat "6.8"
addtofile lt.dat "5.2"
addtofile lt.dat "3.6"

typeit lt.dat
 
!RUN IN LINE-DEPENDENT MODE
! ---- rcp.tst is Recprocity file from ltgen/momgen containing
! ---- a line-dependent shutter-offset
ccdrecip rcp.tst rej=0 array=800 ltfile=lt.dat 'radiance +
 areatbl=al.tbl offtbl=ol.tbl avofftbl=vl.tbl
ccdrecip rcp.tst rej=1 array=800 ltfile=lt.dat 'radiance +
 areatbl=al1.tbl offtbl=ol1.tbl avofftbl=vl1.tbl
ccdrecip rcp.tst REJ=2 array=800 ltfile=lt.dat 'radiance +
 areatbl=al2.tbl offtbl=ol2.tbl avofftbl=vl2.tbl
ccdrecip rcp.tst REJ=3 areatbl=al3.tbl offtbl=ol3.tbl avofftbl=vl3.tbl +
 light=(0,74.5,50.5,38.7,26,20,12.8,9.9,6.8,5.2,3.6) 'radiance
 
!RUN IN SAMPLE-DEPENDENT MODE
! ---- rcps.tst is Recprocity file from ltgen/momgen containing
! ---- a sample-dependent shutter-offset.  It used flotted versions of the
! ---- inputs used in rcp.tst
ccdrecip rcps.tst 'samp ltfile=lt.dat 'radiance +
   rej=0 array=800 areatbl=as.tbl offtbl=os.tbl avofftbl=vs.tbl
ccdrecip rcps.tst  'samp ltfile=lt.dat 'radiance +
   rej=1 array=800 areatbl=as1.tbl offtbl=os1.tbl avofftbl=vs1.tbl
ccdrecip rcps.tst  'samp ltfile=lt.dat 'radiance +
   rej=2 array=800 areatbl=as2.tbl offtbl=os2.tbl avofftbl=vs2.tbl
ccdrecip rcps.tst  'samp +
   rej=3 areatbl=as3.tbl offtbl=os3.tbl avofftbl=vs3.tbl +
   light=(0,74.5,50.5,38.7,26,20,12.8,9.9,6.8,5.2,3.6) 'radiance
 
!COMPARE SOME TABLES BETWEEN THE MODES
if ($syschar(1) = "UNIX")
   ush diff al.tbl as.tbl
   ush diff vl.tbl vs.tbl
else
   dcl diff/para al.tbl,as.tbl
   dcl diff/para vl.tbl,vs.tbl
end-if
 
!TRY MARK OUTPUT AND PLOTS FOR BOTH MODES
ccdrecip rcp.tst  out=ml.dat offset=sol.dat 'line plot=l.plt rej=3 +
  ltfile=lt.dat array=1024 'radiance
ccdrecip rcps.tst  out=ms.dat offset=sos.dat 'samp plot=s.plt REJ=3 +
  ltfile=lt.dat array=1024 'radiance
 
!The plot files will have differences in the titles
!The mark files will have differences because the areas are flotted
!with respect to each other
 
!The output image files should be identical except for maybe
!roundoff error
difpic (sol.dat, sos.dat) sd
 
end-proc

$ Return
$!#############################################################################
