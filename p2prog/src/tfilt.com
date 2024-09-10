$!****************************************************************************
$!
$! Build proc for MIPL module tfilt
$! VPACK Version 1.9, Saturday, July 08, 2000, 17:51:16
$!
$! Execute by entering:		$ @tfilt
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
$ write sys$output "*** module tfilt ***"
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
$ write sys$output "Invalid argument given to tfilt.com file -- ", primary
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
$   if F$SEARCH("tfilt.imake") .nes. ""
$   then
$      vimake tfilt
$      purge tfilt.bld
$   else
$      if F$SEARCH("tfilt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tfilt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tfilt.bld "STD"
$   else
$      @tfilt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tfilt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tfilt.com -
	-s tfilt.f -
	-i tfilt.imake -
	-p tfilt.pdf -
	-t tsttfilt.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tfilt.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      Include   'VICMAIN_FOR'
      Subroutine  Main44
      External    FiltB
      Common/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
c
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      INTEGER totlen
c
      call xvmessage(' *** program TFILT version 8-Jul-2000 ***',' ')

      call FILTA
      L1 = Totlen * 4
      L2 = Totlen * 4
      L3 = Totlen * 2
      L4 = Totlen * 2
      L5 = Totlen * 2
      L6 = NPIX * 2
      LTOT = L1 + L2 + L3 + L4 + L5 + L6
      call STACKA(9,FiltB,6,L1,L2,L3,L4,L5,L6,LTOT)
c
999   Continue
c
      Return
      End
c
c********************************************************************
c
      Subroutine FiltB(SUMDN,L1,COUNT,L2,IN,L3,LAST,L4,NEXT,L5,OUT,L6,
     *LTOT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      INTEGER    SUMDN(L1/4), COUNT(L2/4)
      INTEGER*2  IN(L3/2), LAST(L4/2), NEXT(L5/2), OUT(NPIX)
      INTEGER DNSUM,SLIN,TOTLEN,SSAM,THRESH,TOTL1,TOTL2,UPPER,status,
     * wunit1,RUNIT,wunit2,wunit3

      If (L1+L2+L3+L4+L5+L6 .NE. LTOT)  then
        call xvmessage('*** STACKA Error ***', ' ')
	call Abend
      Endif
c
c   Copy RUNIT to wunit1 and wunit2.  Rearranging such that Line I of wunit1
c   Contins the Next (Highest) Line in the Filter Window for Line I of RUNIT,
c   and Line I of wunit2 the Last (Lowest) Line in the Window for Line I-1

c  band loop, over entire subroutine (not reflected in indentation!):
      do ib=1,nb

c  for multi-band case, re-initialize wunit2 and wunit1:
      if (ib.gt.1) then
        call xvclose( wunit2, status, ' ')
        call xvopen( wunit2, status, 'OP','WRITE','U_FORMAT','HALF',
     *   'O_FORMAT','HALF','U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT',
     *   'SA','IO_ACT','SA', ' ')
        call xvclose( wunit1, status, ' ')
        call xvopen( wunit1, status, 'OP','WRITE','U_FORMAT','HALF',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
      endif

      Do  LINE=2,NLW2
          IREC2 = NLW2+SLIN+1-LINE
          call xvread( RUNIT, IN, status, 'LINE',IREC2, 'SAMP',SSAM,
     &	   'NSAMPS',NSO, 'band', ib, ' ')
          call xvwrit( wunit2, IN, status, ' ')
      enddo

      NEND = NLO-NLW1
      call xvread( RUNIT, IN , status, 'LINE',SLIN, 'SAMP',SSAM,
     &	'NSAMPS',NSO, 'band', ib, ' ')
      LINE=1
      IF (LINE.LE.NEND) call xvwrit( wunit2, IN, status,' ')  ! USU. DONE
      IF (LINE.GT.NLW2) call xvwrit( wunit1,  IN, status,' ')  ! USU. SKIPPED
c
      Do  LINE=2,NLO
	 call xvread( RUNIT, IN, status, 'SAMP',SSAM, 'NSAMPS',NSO,
     &    'line', slin+line-1, 'band', ib, ' ')
	 If (LINE.LE.NEND) call xvwrit( wunit2, IN, status, ' ')
	 If (LINE.GT.NLW2) call xvwrit( wunit1, IN, status, ' ')
      enddo

      Do  I=1,NLW2-1
          LINE=NLO-1+SLIN-I
          call xvread( RUNIT, IN, status, 'LINE',LINE, 'SAMP',SSAM,
     &	  'NSAMPS',NSO, 'band', ib, ' ')
          call xvwrit( wunit1, IN, status, ' ')
      enddo
c
C  Close  Scratch DISK & Re-Open for INPUT:
c
      call xvclose( wunit2, status, ' ')
      call xvopen( wunit2, status, 'OP','READ','U_FORMAT','HALF',
     *   'O_FORMAT','HALF','U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT',
     *   'SA','IO_ACT','SA', ' ')
c
C  Close  OUTPUT File  & Re-Open  for UPDATE
c
      call xvclose( wunit1, status, ' ')
      call xvopen( wunit1, status, 'OP','UPDATE', 'U_FORMAT','HALF',
     * 'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     * ' ')
c
c   Compute the Initial SUMDN and Count buffers from the 
c   first NLW  lines 
c   SUMDN = SUM of DN'S  GT  THRESH in each column 
c   COUNT = SUM of PIXELS GT THRESH in each column 

      call  ZIA(SUMDN, L1/4)
      call  ZIA(COUNT, L2/4)
      Do  LINE=1,NLW
	  call xvread( wunit2, IN(NSW2), status, 'LINE',LINE,
     &	 'NSAMPS',NSO, ' ')
	call  RFLCT(IN)
	Do  J=1,TOTLEN
	  If (IN(J) .GT. THRESH)  Then
            SUMDN(J) = SUMDN(J) + IN(J)
            COUNT(J) = COUNT(J) + 1
	  Endif
	enddo
      enddo
c
c    Initialize the xvreads:
c
      call xvread( wunit1,NEXT(NSW2),status,'LINE',1,'NSAMPS',NSO,' ')
      call  RFLCT(NEXT)
      call xvread( wunit2,LAST(NSW2),status,'LINE',1,'NSAMPS',NSO,' ')
      call  RFLCT(LAST)
      call xvread( RUNIT,IN(NSW2),status,'LINE',SLIN,'NSAMPS',NSO,
     &      'SAMP',SSAM, 'band', ib, ' ')
      call  RFLCT(IN)
c
C   MAIN  LINE Loop for  OUTPUT Picture
c
      DO  LINE = 1,NLO
          DNSUM = 0
          NSUM  = 0
c
c    Add up the First NSW DN:
c 
          DO J = 1,NSW
            DNSUM = DNSUM + SUMDN(J)
            NSUM  = NSUM  + COUNT(J)
 	  enddo
c
c    Apply the appropriate Filter to Current LINE 
c
         If (MODE .EQ. 1) call  HPFILT(IN,OUT,SUMDN,COUNT)
         If (MODE .EQ. 2) call  SDFILT(IN,OUT,SUMDN,COUNT)
         If (MODE .EQ. 3) call  LPFILT(IN,OUT,SUMDN,COUNT)
         If (MODE .EQ. 4) call  DFILT (IN,OUT,SUMDN,COUNT)
	 call xvwrit( wunit1, OUT, status, ' ')
         If (LINE .EQ. NLO) Go To 2000
c
c   Update SUMDN & COUNT buffers by deleting LAST line & Adding NEXT:
c
	 DO J = 1,TOTLEN
	   If (LAST(J) .GT. THRESH)  Then
	     SUMDN(J) = SUMDN(J) - LAST(J)
	     COUNT(J) = COUNT(J) - 1
	   EndIf
	   If (NEXT(J) .GT. THRESH)  Then
	     SUMDN(J) = SUMDN(J) + NEXT(J)
	     COUNT(J) = COUNT(J) + 1
	   EndIf
	 enddo

	 If (SSAM .EQ. 1)  Then	   ! READ NEXT LINE
	   call xvread( RUNIT, IN(NSW2), status,'NSAMPS',NSO,
     &      'line', slin+line, 'band', ib,  ' ')
	 Else
	   call xvread( RUNIT, IN(NSW2), status, 'SAMP',SSAM,
     &	    'NSAMPS',NSO, 'line', slin+line, 'band', ib, ' ')
	 EndIf
c
         call RFLCT(IN)
c
c    LAST   Line in Window  
c
         call xvread( wunit2,LAST(NSW2),status, 'NSAMPS',NSO, ' ')
         call RFLCT(LAST)
c
c    NEXT  Line in Window  
c
         call xvread( wunit1,NEXT(NSW2),status,'NSAMPS',NSO, ' ')
         call RFLCT(NEXT)
c
      enddo
c
C   Close Files and Exit
c
2000  Continue

c  multi-band case:  copy wunit1 to band IB of wunit3
      if (nb.gt.1) then
        do il=1,nlo
          call xvread( wunit1, in, status, 'line', il, 'nsamps',nso,
     &     ' ')
          call xvwrit( wunit3, in, status, 'line', il, 'nsamps',nso,
     &     'band', ib, ' ')
        enddo
      endif

      enddo   ! END OF BAND LOOP

      call xvclose( RUNIT, status, ' ')
      call xvclose( wunit2, status, ' ')
      call xvclose( wunit1, status, ' ')
c
      Return 
      End
c
c*************************************************************
c
      Subroutine RFLCT(BUF)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      INTEGER*2 BUF(1)
      INTEGER DNSUM,SLIN,TOTLEN,SSAM,THRESH,TOTL1,TOTL2,UPPER
c
      call  MVE(2,NSW1,BUF(NSW3),BUF(NSW1),1,-1)
      call  MVE(2,NSW1,BUF(TOTL2),BUF(TOTL1),-1,1)
c
      Return
      End
c
c******************************************************************
c
      Subroutine FILTA
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
      INTEGER DNSUM,SLIN,TOTLEN,SSAM,THRESH,TOTL1,TOTL2,UPPER,status,
     * wunit1,RUNIT,wunit2,wunit3
      LOGICAL xvptst,HIGH,LOW,SCENE,DIVIDE

      call xvunit( RUNIT,'INP',1,status, ' ')
      call xvopen( RUNIT, status,'U_FORMAT','HALF','OPEN_ACT','SA',
     * 'IO_ACT','SA', ' ')
      call xvsize(SLIN, SSAM,NLO,NSO,NX,NS, ' ')
      call xvbands(sb,nb,idum)
      call xvget( RUNIT,status,'PIX_SIZE',N, ' ')
      if (n.gt.2) call mabend(' Input must be byte or halfword!')

      call xvpcnt('OUT',i,j)
      if (nb.gt.1 .and.	i.lt.3) call mabend(
     *	 ' 3 output files required when input is multi-band')

      if (nb.eq.1) then
	call xvunit( WUNIT1,'OUT',1,status,' ')
	call xvopen( WUNIT1, status, 'OP','WRITE','U_FORMAT','HALF',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
      else
	call xvunit( WUNIT3, 'OUT', 1, status, ' ')
	call xvopen( WUNIT3, status, 'OP','WRITE','U_FORMAT','HALF',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',NB,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
        call xvunit( WUNIT1, 'OUT', 3, status, ' ')
        call xvopen( WUNIT1, status, 'OP','WRITE','U_FORMAT','HALF',
     *   'U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT','SA','IO_ACT','SA',
     *   ' ')
      endif

      call xvunit( WUNIT2, 'OUT', 2, status, ' ')
      call xvopen( WUNIT2, status,'OP','WRITE','U_FORMAT','HALF',
     * 'O_FORMAT','HALF','U_NL',NLO,'U_NS',NSO,'U_NB',1,'OPEN_ACT',
     * 'SA','IO_ACT','SA', ' ')

      KEY = 2-N
      call Prnt(4,1,N,' #BYTES/PXL=.')
      MODE = 4
      NLW  = NLO/10
      NSW  = NLW
      call xvparm('NLW', NLW, NNLW, NLWDF, 0)
      call xvparm('NSW', NSW, NNSW, NSWDF, 0)
      call xvparm('THRESH', THRESH, NTHRE, THREDF, 0)
      call xvparm('SCALE' ,SCALE, NSCAL, SCALDF, 0)
      call xvparm('DCTRAN', DCTRAN, NDCTR, DCTRDF, 0)
      call xvparm('BOOST', BOOST, NBOOS, BOOSDF, 0)
      call xvparm('DCLEVEL', DCLVL, NDCLE, DCLEDF, 0)
      call xvparm('OFFSET', OFFSET, NOFFS, OFSDF, 0)
      HIGH  =  xvptst('HIGH')
      LOW   =  xvptst('LOW')
      SCENE =  xvptst('SCENE')
      DIVIDE=  xvptst('DIVIDE')

      If (HIGH)   MODE = 1
      If (LOW)    MODE = 3
      If (SCENE)  MODE = 2
      If (DIVIDE) MODE = 4

      If ((NLW/2)*2 .EQ. NLW) NLW = NLW + 1
      If ((NSW/2)*2 .EQ. NSW) NSW = NSW + 1

      If (KEY .EQ. 0)  Then
	NPIX   =    NSO
	LOWER  = -32768
	UPPER  =  32767
      Else
	NPIX   =   NSO
	LOWER  =     0
	UPPER  =   255
      EndIf

      NLW2 = (NLW+1) / 2
      NLW1 = NLW2 - 1
      NSW2 = (NSW+1) / 2
      NSW1 = NSW2 - 1
      TOTLEN = NPIX +  2*NSW1
      NSW3 = NSW2 + 1
      TOTL1 = TOTLEN - NSW1 + 1
      TOTL2 = TOTLEN - NSW2
      call  PRNT(4,1,NLW,' NLW=.')
      call  PRNT(4,1,NSW,' NSW=.')
c
c     Get  Number  Pixels/Line
c   
      Return
      End
c
c******************************************************************
c
      Subroutine HPFILT(IN,OUT,SUMDN,COUNT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
c
c
      INTEGER*2 IN(1),OUT(1)
      INTEGER SUMDN(1),COUNT(1),UPLIM,DNSUM,THRESH,UPPER
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2
c
      If (IN(NSW2) .LE. THRESH) Go To 90
      If (NSUM .NE. 0) X = Float(DNSUM) / Float(NSUM)
      If (NSUM .EQ. 0) X = Float(DNSUM)
      X = (IN(NSW2) - X) * BOOST + DCTRAN*IN(NSW2) + DCLVL
      If (X .GT. UPPER)  Then
	OUT(1) = UPPER
      ElseIf (X .LE. LOWER)  Then
	OUT(1) = LOWER
      Else
	OUT(1) = X
      EndIf
      Go  To 100
90    OUT(1)=0
100   Continue
c
      JJ = 1
      LOWLIM = NSW2 + 1
      UPLIM  = NPIX + NSW1
c
      Do 200  J = LOWLIM,UPLIM
         JJ  = JJ + 1
         JK1 = J - NSW2
         JK2 = J + NSW1
         DNSUM = DNSUM - SUMDN(JK1) + SUMDN(JK2)
         NSUM  = NSUM - COUNT(JK1) + COUNT(JK2)
         If (IN(J) .LE. THRESH) Go To 160
         If (NSUM .NE. 0) X = Float(DNSUM)/Float(NSUM)
         If (NSUM .EQ. 0) X = Float(DNSUM)
         X = (IN(J) - X) * BOOST + DCTRAN * IN(J) + DCLVL
         If (X .GT. UPPER)   Then
	   OUT(JJ)=UPPER
         ElseIf (X .LE. LOWER)  Then
	   OUT(JJ)=LOWER
         Else
	   OUT(JJ) = X
         EndIf
c
         Go To  200
160      OUT(JJ)=0
200   Continue
c
      Return
      End
c
c******************************************************************
c
      Subroutine DFILT(IN,OUT,SUMDN,COUNT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
c
c
      INTEGER*2 IN(1),OUT(1)
      INTEGER SUMDN(1),COUNT(1),UPLIM,DNSUM,THRESH,UPPER
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2
      IF(IN(NSW2).LE.THRESH)GO TO 90
      IF(DNSUM.NE.0)X=FLOAT(NSUM)/FLOAT(DNSUM)
      IF(NSUM.EQ.0)X=FLOAT(NSUM)
      X = IN(NSW2)*(SCALE*X+DCTRAN)+OFFSET
      IF (X.GT.UPPER) THEN
	OUT(1)=UPPER
      ELSEIF (X.LE.LOWER) THEN
	OUT(1)=LOWER
      ELSE
	OUT(1) = X
      ENDIF
      GO TO 100
 90   OUT(1)=0
 100  CONTINUE
      JJ=1
      LOWLIM=NSW2+1
      UPLIM =NPIX+NSW1
      DO 200 J=LOWLIM,UPLIM
      JJ=JJ+1
      JK1=J-NSW2
      JK2=J+NSW1
      DNSUM=DNSUM-SUMDN(JK1)+SUMDN(JK2)
      NSUM=NSUM-COUNT(JK1)+COUNT(JK2)
      IF(IN(J).LE.THRESH)GO TO 160
      IF(DNSUM.NE.0)X=FLOAT(NSUM)/FLOAT(DNSUM)
      IF(DNSUM.EQ.0)X=FLOAT(NSUM)
      X=IN(J)*(SCALE*X+DCTRAN)+OFFSET
      IF (X.GT.UPPER) THEN
	OUT(JJ)=UPPER
      ELSEIF (X.LE.LOWER) THEN
	OUT(JJ)=LOWER
      ELSE
	OUT(JJ) = X
      ENDIF
      GO TO 150
 160  OUT(JJ)=0
 150  CONTINUE
 200  CONTINUE
      RETURN
      END
c
c******************************************************************
c
      Subroutine SDFILT(IN,OUT,SUMDN,COUNT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
C
C
      INTEGER*2 IN(1),OUT(1)
      INTEGER SUMDN(1),COUNT(1),UPLIM,DNSUM,THRESH,UPPER
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2
c
      call MVE(2,NPIX,IFIX(DCLVL),OUT,0,1)
      If (IN(NSW2) .LE. THRESH) Go To  90
      If (NSUM .NE. 0) X = Float(DNSUM) / Float(NSUM)
      If (NSUM .EQ. 0) X = Float(DNSUM)
      If (X .NE. 0) X = (IN(NSW2)-X)*SCALE/X + DCTRAN*IN(NSW2) + DCLVL
      If (X .GT. UPPER) Then
	OUT(1) = UPPER
      ElseIf (X .LE. LOWER)  Then
	OUT(1)=LOWER
      Else
	OUT(1) = X
      EndIf
      Go  To  100
90    OUT(1) = 0
100   Continue
c
      JJ = 1
      LOWLIM = NSW2 + 1
      UPLIM  = NPIX + NSW1
      Do 200  J = LOWLIM,UPLIM
      JJ = JJ + 1
      JK1 = J - NSW2
      JK2 = J + NSW1
      DNSUM = DNSUM - SUMDN(JK1) + SUMDN(JK2)
      NSUM = NSUM - COUNT(JK1) + COUNT(JK2)
      If (IN(J) .LE. THRESH) Go To  160
      If (NSUM .NE. 0) X = Float(DNSUM) / Float(NSUM)
      If (NSUM .EQ. 0) X = Float(DNSUM)
      If (X .NE. 0) X = (IN(J)-X)*SCALE / X + DCTRAN*IN(J) + DCLVL
      If (X .GT. UPPER) Then
	OUT(JJ)=UPPER
      ElseIf (X .LE. LOWER) Then
	OUT(JJ)=LOWER
      Else
	OUT(JJ) = X
      EndIf
      Go  To  150
160   OUT(JJ) = 0
150   Continue
200   Continue
c
      RETURN
      END
c
c******************************************************************
c
      Subroutine  LPFILT(IN,OUT,SUMDN,COUNT)
      COMMON/C1/RUNIT,wunit3,wunit1,NLO,NSO,NX,NS,THRESH,SLIN,SSAM,
     * MODE,NLW,NSW,K,KEY,NSUM,DNSUM,nb,wunit2,
     * LOWER,UPPER,NLW1,NLW2,NSW1,NSW2,NSW3,TOTLEN,NPIX,TOTL1,TOTL2
      COMMON/C2/SCALE,DCTRAN,OFFSET,BOOST,DCLVL
c
      INTEGER*2 IN(1),OUT(1)
      INTEGER SUMDN(1),COUNT(1),UPLIM,DNSUM,THRESH,UPPER
      INTEGER SLIN,TOTLEN,SSAM,TOTL1,TOTL2
c
c
      If (IN(NSW2) .LE. THRESH) Go To 90
      If (NSUM .NE. 0) X = Float(DNSUM) / Float(NSUM)
      If (NSUM .EQ. 0) X = Float(DNSUM)
      X = BOOST*X + OFFSET
      If (X .GT. UPPER)  Then
	OUT(1) = UPPER
      ElseIf  (X .LE. LOWER)  Then
	OUT(1) = LOWER
      Else
	OUT(1) = X
      EndIf
      Go To  100
90    OUT(1) = 0
100   Continue
c
c
      JJ=1
      LOWLIM=NSW2+1
      UPLIM =NPIX+NSW1
      Do 200  J = LOWLIM,UPLIM
      JJ = JJ + 1
      JK1 = J - NSW2
      JK2 = J + NSW1
      DNSUM = DNSUM - SUMDN(JK1) + SUMDN(JK2)
      NSUM = NSUM - COUNT(JK1) + COUNT(JK2)
      If (IN(J) .LE. THRESH) Go To 160
      If (NSUM .NE. 0) X = Float(DNSUM) / Float(NSUM)
      If (NSUM .EQ. 0) X = Float(DNSUM)
      X = BOOST*X + OFFSET
      If (X .GT. UPPER)  Then
	OUT(JJ) = UPPER
      ElseIf (X .LE. LOWER)  Then
	OUT(JJ) = LOWER
      Else
	OUT(JJ) = X
      EndIf
      Go to 150 
160   OUT(JJ) = 0
150   Continue
200   Continue
c
      Return
      End
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tfilt.imake
#define  PROGRAM   tfilt

#define MODULE_LIST tfilt.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create tfilt.pdf
process help=*
PARM INP      TYPE = (STRING,80)
PARM OUT      TYPE = (STRING,80) count=2:3
PARM SIZE     TYPE = INTEGER  COUNT = 4  DEFAULT = (1,1,0,0)
PARM SL       TYPE=INTEGER DEFAULT=1
PARM SS       TYPE=INTEGER DEFAULT=1
PARM NL       TYPE=INTEGER DEFAULT=0
PARM NS       TYPE=INTEGER DEFAULT=0
PARM NLW      TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM NSW      TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM THRESH   TYPE=INTEGER  DEFAULT=-1
PARM FILTER TYPE=KEYWORD COUNT=1 VALID=(HIGH,LOW,SCENE,DIVIDE) DEFAULT=DIVIDE
PARM SCALE    TYPE=REAL  DEFAULT=100.0
PARM DCTRAN   TYPE=REAL  DEFAULT=0.0
PARM BOOST    TYPE=REAL DEFAULT=1.0
PARM DCLEVEL  TYPE=REAL  DEFAULT=128.0
PARM OFFSET   TYPE=REAL DEFAULT=0.5
END-PROC
.TITLE
 tfilt  --  threshold filtering.
.HELP
PURPOSE:

tfilt is a VICAR program which performs various boxfilter type convolutional
filters.  It is a commonly used, especially in planetary image processing. 
tfilt is often used to bring out detail that is not noticeable in the original
image.  tfilt is able to ignore DN's below a threshold when computing the low
pass average and is thus free of ringing in certain cases.  (See the threshold
effects section below.)   Scaling allows the dynamic range of high pass filters
to be improved before integer truncation. 
.PAGE
EXECUTION:

tfilt is executed as a standard VICAR program.  The following 
VICAR command line formats show the most common usages:

      tfilt INP=a OUT=(b,x) SIZE=(sl,ss,nl,ns) optional parameters
      tfilt INP=a OUT=(b,x) SL=sl SS=ss NL=nl NS=ns optional parameters
      tfilt a (b,x) (sl,ss,nl,ns) optional parameters
      tfilt a (b,x) optional parameters

       Here 'a' represents the input image file name, 
       'x' represents a scratch file name, and
       'b' represents the output image file name.

If the primary input is a multi-band image, then three output files
are required, of which the last two are scratch files, e.g.:

      tfilt a (b,x,y) optional parameters

.PAGE
USER INFORMATION:

The size of the output image is determined by the the SIZE field if the SIZE
field is entered by the user.  If the SIZE field is not entered, the output
file is the same size as the input file. 

The data type of the input image may either be byte or halfword data. The data
type is obtained from the VICAR label of the input image.
The output image has the same data format (byte or halfword) as the
input image. 

.page
Available Filter Types:

tfilt can be used to apply a high pass, a low pass, a scene dependent, or
a divide filer to an image.  Each type of filter can be thought of as 
emphasizing one aspect of the image and de-emphasizing another aspect of
the image.  tfilt has a variety of uses in image restoration and
image enhancement.  The major charactistics of each of these filters are
described below.

1)  The high pass filter emphasizes high frequency information and
    de-emphasizes low frequency information.  (High frequency means
    brightness changing rapidly as a function of distance.  Low frequency
    means brightness changing slowly as a function of distance.)  The high
    pass filter can be used to compensate for variations in illumination
    between different areas of the image due to sun angle differences.  It
    can also be used to make the picture sharper, to emphasize edges of
    features, and to enhance fine detail.  Often the high pass filter will
    bring out small scale structure that is not noticeable in the original
    image.  The high pass filter tends to remove variations due to color
    differences (albedo variations). The high pass filter can emphasize
    noise if present, so it is best to remove noise before performing a
    high pass filter.  If there are reseau marks in the image, they should
    be removed with program RESSAR77 or a similar program before a high
    pass filter is performed. 
    
2)  The low pass filter emphasizes low frequency information and
    de-emphasizes high frequency information.  The low pass filter is used
    mainly for smoothing the brightness variations in the image.  It can be
    used to smooth noise in a noisy or grainy image to make it less
    noticeable.  The low pass filter makes edges and features blurred or
    smoother. 
    
3)  The scene dependent filter is a variant of the high pass filter that
    can be useful in dealing with images having nonhomogeneous illumination
    or images with mixtures of areas with little high frequency detail and
    areas with a lot of high frequency detail.  It is called a scene
    dependent filter because it compensates for dark areas by dividing the
    high frequency information by the average brightness of the surrounding
    area.  Thus it emphasizes high frequency information especially in
    darker areas of the image, and de-emphasizes low frequency information
    especially in brighter areas of the image. The scene dependent filter
    has similar properties to the high pass filter (enhancing detail), but
    compensates more for the darker areas of the image. The MIPL facility
    at JPL has a display showing the use of the scene dependent filter on
    an image of Mercury with only part of the planet illumined by the sun. 
    The scene dependent filter improves feasture discriminability near the
    edge of the illumined portion of the planet. 
    
4)  The divide filter amplifies darker parts of the image and scales down
    brighter parts of the image, preserving the contrast and evening out
    the overall intensity across the image.  It is useful for bringing out
    subtle detail, especially in darker parts of the image. The divide
    filter can be used to compensate for variations in illumination between
    different areas of the image due to sun angle differences.  The divide
    filter tends to remove variations due to color differences (albedo
    variations). The divide filter tends to pass random noise without
    adding emphasis. 
.page
Selection of Filter Type:

The most commonly used of the four filter types is the high pass filter. This is
partly because people like to see a lot of detail and imaging systems often
attenuate higher frequency information.  (See reference 2.) The high pass
filter also compensates for variations in illumination in the image. The divide
filter is also commonly used.  It compensates for variations in illumination in
the image without emphasizing high frequency information.  It can bring out
detail in the image and in some cases produces a result quite similar to the
high pass filter.  The scene dependent filter is less commonly used.  It is
recommended when the results of the high pass filter are not satisfactory
because of inhomogeneity in the image.  The low pass filter has a very
different purpose from the other three filter types, namely smoothing the data.
VICAR program BOXFLT2 (currently in R3LIB) is also used for low pass filtering
and has some different methods for handling the edges of the image.
.page
Advantages and Disadvantages:

tfilt has both good and bad features.  It is fast for a filter program, 
If the threshold is chosen well, tfilt can do a good filtering job without 
causing noticeable ringing near edges.  It is not an optimal filter, though, 
and sometimes it can generate ringing problems.  This is because it uses a 
uniform set of filter weights.(See reference 1.)  The VICAR program FILTER is 
recommended for users wishing to do more optimal filtering.  FILTER can be 
much slower than tfilt, though. FILTER requires the user specify the filter 
weights.  There is, however, a default set of weights in FILTER.  VICAR 
procedure FILTER2 can be used to generate the weights alternatively.
 
.page
Threshold Effects:

Linear filtering with a uniform set of weights can often cause ringing
in the output image, especially around a sharp edge.  The threshold (THRESH)
parameter allows tfilt to filter without causing noticeable ringing in many
cases.  In planetary image processing, the most distinct edge in a picture
is often the boundary (limb) between a planet or moon (satelite) and the
black of outer space.  For such an image the threshold should be set 
slightly below the brightness of the planet so that basically all of the
planet is brighter than the threshold and the black of outer space is
darker than the threshold. Unfortunately, finding a threshold that minimizes
ringing can be a matter of trial and error.  A histogram may be of value in
picking an initial value. 
.page
Effects of Box Size:

For all filter types the box size (parameters NLW and NSW) affects the
output.  For a low pass filter a small box (3 by 3 to 11 by 11) is usually used.
For the other filter types a large box is generally used since this
produces a less grainy image and less noise enhancement than a smaller box.
A large box can cause greater ringing than a small box, though, especially
if the threshold parameter is not set optimally.  For an 800 by 800 
Voyager image a box size in the range 15 by 15 to 75 by 75 is common.
The exact size is not too important.  A square is commonly used for
symmetry.  Because of the algorithm used by tfilt, the box size does not affect
the execution time significantly.  (A 75 by 75 box took five percent more time
than a 5 by 5 box.) 
.page
Effects of the DC Transmission Factor (DCTRAN):

The DCTRAN parameter is used to soften the effect of the selected filter by
making the output image a combination of the filter output and the input
image.  DCTRAN can be used with all of the filter types except low pass.
DCTRAN is commonly set to make the output image about 70 percent from
the filter and 30 percent from the input image.  (For the high pass filter
you can try DCTRAN = 0.1 to 0.2.)  The DC in DCTRAN stands for direct
current.  This stems from the connection of filtering with signal processing
theory.  DCTRAN reflects the amount of the original signal that is
transmitted by the filter.  When DCTRAN is used with the high pass filter or 
scene dependent filter, the output image will retain some low 
frequency information, and albedo and illumination variations will still be
present to some degree. 
.page
Control of Dynamic Range:

Each of the filter types uses some parameters to control the dynamic
range of DNs in the output image.  The default values for these
parameters can sometimes produce a narrow dynamic range, especially
with byte data, resulting in image degradation when integer truncation
takes place.  Some recommendations on these parameters are given below
for byte data, but a lot depends on the input image, and some trial
and error may be appropriate until satisfactory results are produced.
(For trial and error, try tfilt on a small portion of the 
image and run VICAR program HIST with BINS=20 to check the dynamic
range of the output image.)

For the high pass filter, try BOOST=4, and default DCLEVEL.  Default DCTRAN if a
straight high pass filter is desired or try DCTRAN = .1 to .2 to soften 
the filter.

For the scene dependent filter, try SCALE in the range 100 to 300 and default
DCLEVEL. Default DCTRAN if a straight scene dependent filter is desired or try
DCTRAN = .1 to .2 to soften the filter. 

For the low pass filter the defaults for BOOST and OFFSET should be
satisfactory.

For the divide filter, try OFFSET in the range 0 to 50, and default DCLEVEL.  
Default DCTRAN if a straight divide filter is desired or try 
DCTRAN = .1 to .2 to soften the filter.
.page
EXAMPLES:

1.     tfilt (A,X) HIGH.IMG  'HIGH BOOST=4 THRESH=25  NLW=21 NSW=21 DCTRAN=.1

In this example tfilt is used to apply a high pass filter to image A, 
producing image HIGH.IMG.  A 21 by 21 box is used.

2.     tfilt (MIRANDA.IMG,X) DIV.IMG   'DIV SCALE=150 THRESH=25  NLW=21 NSW=21

In this example tfilt is used to apply a divide filter to image MIRANDA.IMG, 
producing image DIV.IMG.  A 21 by 21 box is used.
.PAGE
3.     tfilt (MIRANDA.IMG,X) SCENE.IMG 'SCENE SCALE=250 THRESH=25 NLW=21 NSW=21

In this example tfilt is used to apply a scene dependent filter to image 
MIRANDA.IMG, producing image SCENE.IMG.  A 21 by 21 box is used.

4.     tfilt (MIRANDA.IMG,X) LOW.IMG   'LOW THRESH=25  NLW=3 NSW=3

In this example tfilt is used to apply a low pass filter to image MIRANDA.IMG, 
producing image LOW.IMG.  A 3 by 3 box is used.

5.  The last example is the test procedure for tfilt.  This is
    a complete example that could be run by the user and that 
    demonstrates uses of the possible parameters.

    GEN OUT=GEN NL=20 NS=17 ! generate a picture with byte format
    GEN OUT=HST NL=20 NS=22 'HALF ! generate a scratch file with halfword format
    LIST INP=GEN 'ZEROES ! check input data
    tfilt INP=GEN OUT=(TEST1,IST)   !run tfilt - default all optional params.
    LIST INP=TEST1 'ZEROES
    tfilt INP=GEN OUT=(TEST2,IST)  ! run tfilt with byte
    LIST INP=TEST2 'ZEROES
    ! run tfilt with byte and nlw
    tfilt INP=GEN OUT=(TEST4,IST) NLW=3
    LIST INP=TEST4  'ZEROES
    ! run tfilt with byte, nls and nsw
    tfilt INP=GEN OUT=(TEST6,IST) NLW=2 NSW=3
    LIST INP=TEST6 'ZEROES
    ! run tfilt with byte, nls, nsw and thresh
    tfilt INP=GEN OUT=(TEST7,IST) NLW=2 NSW=3 THRESH=3
    LIST INP=TEST7 'ZEROES
    ! run tfilt with nlw, nsw, thresh and high
    tfilt INP=GEN OUT=(TEST8,IST) NLW=3 NSW=3 THRESH=3 'HIGH
    LIST INP=TEST8 'ZEROES
    ! run tfilt with thresh, scene
    tfilt INP=GEN OUT=(TEST9,IST) THRESH=2 'SCENE
    LIST INP=TEST9 'ZEROES
    tfilt INP=GEN OUT=(TEST10,IST) NLW=4 'LOW !run tfilt with nlw and low
    LIST INP=TEST10 'ZEROES
    tfilt INP=GEN OUT=(TEST11,IST) 'DIVIDE ! run tfilt with divide
    LIST INP=TEST11 'ZEROES
    ! run tfilt with nlw and high - define boost,dctran, dclevel
    tfilt INP=GEN OUT=(TEST12,IST) NLW=2 'HIGH BOOST=2 DCTRAN=0.1 DCLEVEL=50.0
    LIST INP=TEST12 'ZEROES
    !run tfilt with nlw, nsw, and low - define boost  and offset
    tfilt INP=GEN OUT=(TEST14,IST) NLW=2 NSW=4 'LOW BOOST=2.00 OFFSET=20.
    LIST INP=TEST14 'ZEROES
    ! run tfilt with halfword and nsw
    GEN OUT=HGEN NL=20 NS=22 'HALF !generate a picture with halfword format
    LIST INP=HGEN 'ZEROES
    tfilt INP=(HGEN,HST) OUT=(TEST3,IST)  ! run tfilt with halfword
    LIST INP=TEST3 'ZEROES
    tfilt INP=(HGEN,HST) OUT=(TEST5,IST) NSW=2
    LIST INP=TEST5 'ZEROES
    !run tfilt with halfword,nlw,nsw,thresh,scene,scale,dctran and dclevel
    tfilt INP=(HGEN,HST) OUT=(TEST13,IST) NLW=2 NSW=2 THRESH=3 'SCENE+
     SCALE=200.0 DCTRAN=0.3 DCLEVEL=10
    LIST INP=TEST13 'ZEROES
.page
 OPERATION
    
tfilt operates as a convolutional filter.  At each pixel it computes the
average of all DN's greater than the THRESH parameter within an area of
dimensions NLW by NSW centered on that pixel and performs the filter using this
value.  Values of pixels outside the image are obtained by reflection about the
image edges. 

If the local average is ADN, the pixel at the center of the averaged area is
DN, and the output pixel DN is OUT, then the four filters are: 
 
 HIGH     OUT=(DN-ADN)*BOOST+DCTRAN*DN+DCLEVEL
 SCENE    OUT=((DN-ADN)/ADN)*SCALE+DCTRAN*DN+DCLEVEL
 LOW      OUT=ADN*BOOST+OFFSET
 DIVIDE   OUT=(DN/ADN)*SCALE+DCTRAN*DN+OFFSET

Default values for the constants are:
 
 SCALE = 100.0,  DCTRAN = 0.0,  BOOST = 1.0,
 DCLEVEL= 128.0,  OFFSET = 0.5

The calculations are performed in floating-point arithmetic and then truncated
to integer to give the OUT value in the above equations.  For SCENE and DIVIDE,
1.0 is used in the denominator if ADN is 0.  The results of the calculations
are checked for being in the valid range of DNs for the data type (byte or
halfword) of the image and are adjusted if invalid.  For byte data, DNs less
than 0 are set to 0, and DNs greater than 255 are set to 255.  For halfword
data, DNs less than -32768 are set to -32768, and DNs greater than 32767 are
set to 32767. 
.PAGE
RESTRICTIONS:

1. The input and output images must be byte or halfword data.

TIMING: 

tfilt takes about 23 CPU seconds on the VAX 8600 for a high pass filter 
on an 800 by 800 byte image.  Because of the algorithm used by tfilt,
the box size does not affect the execution time significantly.  (A 75 by
75 box took five percent more time than a 5 by 5 box.)
.page 

  REFERENCES:

   1.   K. R. Castleman, "Digital Image Processing",
        Prentice Hall, Inc., 1979, p. 199.

   2.   J. G. Moik, "Digital Processing of Remotely Sensed Images",
        NASA Publication SP-431, 1980, p. 130.


 WRITTEN BY: J. J. Lorre,                   Sept. 22, 1980
 COGNIZANT PROGRAMMER:  L W.Kamp
 REVISIONS:
  05MAR00 ...LWK...  Revised to allow multispectral files
  Feb 21 96 ...FFM...  Renamed FILTER0 as FILTER. Obsolete programs FILTERAP &
                       FILTER0.(There is no need to have procedure FILTER
                       because there is no AP on any platform).
                       Modified HELP & TEST slightly, retested on alpha, andes,
                       solaris, & sunos.
  AUG 06 89 ...GMY...  Fix bug in SDFILT (given SIZE field specification)
  MAY-85   ...LWK... RENAMED tfilt FOR PROC TFILT
  OCT. 84  ...LWK... BUG FIXES & SPEED UP I/O BY OMITTING OPTIONALS
  OCT. 84  ...BXG... CONVERTED TO VICAR2
  JAN. 84  ...DFS... CONVERTED TO VAX
  NOV 24 82 ...JAM... THIS IS TFILT REWRITTEN IN FORTRAN. THE ORIGINAL 
		     TFILT WAS WRITTEN IN PL/I BY J.J.LORRE
.LEVEL1
.VARIABLE INP
input file
.VARIABLE OUT
output file and one or two 
(if multi-band) scratch files
.VARIABLE SIZE
 FOUR INTEGERS -
 VICAR size field
.VARIABLE SL
 INTEGER - starting line in
 input picture
.VARIABLE SS
 INTEGER - starting sample in
 input picture
.VARIABLE NL
 INTEGER - number of lines in
 input picture to process
.VARIABLE NS
 INTEGER - number of samples per
 line in input picture 
 to process
.VARIABLE NLW
 INTEGER - number of lines 
 in filter
.VARIABLE NSW
 INTEGER - number of pixels 
 per line in filter
.VARIABLE THRESH
 INTEGER - pixel threshold level
.VARIABLE FILTER
 KEYWORD - filter type
 Valid: HIGH,LOW,SCENE,
 DIVIDE
.VARIABLE SCALE
 REAL - DN scale factor
.VARIABLE DCTRAN
 REAL - DC transmission factor
.VARIABLE BOOST
 REAL - amplitude boost factor
.VARIABLE DCLEVEL
 REAL - additive constant
.VARIABLE OFFSET
 REAL - additive constant
.LEVEL2
.VARIABLE INP
 input file
.VARIABLE OUT
 output file and one or two scratch files

 If the primary input is multi-band, then three output files are required
 of which the last two are scratch files.  Otherwise, only one scratch
 file is required, which is the second output file.

 The first output file is always the output image.

.VARIABLE SIZE
 (number of lines,number of samples,
 starting line number,starting sample number)
 Specifies area of input image to process.
.VARI SL
Starting line.
.VARI SS
Starting sample.
.VARI NL
Number of lines.
.VARI NS
Number of samples per line.
.VARIABLE NLW
 NLW is an integer which specifies the number of lines dimension
 of the convolution window.

 The default is NLW=NL/10 where NL is the number of lines in the size field.
 If NLW is an even number (whether user-specified or default), then 1 is
 added to get an odd number.  E.g., for a 100x100 image, the default is
 NLW=11.
.VARIABLE NSW
 NSW is an integer which specifies the number of pixels dimension of the
 convolutional window.  The default is NL/10.

 If NLW is an even number (whether user-specified or default), then 1 is
 added to get an odd number.  
.VARIABLE THRESH
 THRESH is an integer which specifies a threshold
 level above which pixels will be accepted into
 the average (low pass).  When a pixel lies at or
 below THRESH the output picture pixel will
 always be zero.  The default is THRESH=-1,
 which, at least for byte, converts tfilt into
 a linear boxfilter.
.VARIABLE FILTER
 This parameter specifies the type of filter to be used.
 
 HIGH: performs a high pass filter using the following equation:
 	OUT = (DN-AVGDN)*BOOST+DCTRAN*DN+DCLEVEL
 
 SCENE: performs a SCENE filter using the following equation:
	OUT = (DN-AVGDN)/ADVDN*SCALE+DCTRAN*DN+DCLEVEL
 
 LOW:  performs a low pass filter using the following equation:
	OUT = AVGDN*BOOST+OFFSET
 
 DIVIDE: performs a divide filter using the following equation:
	OUT = DN/AVGDN*SCALE+DCTRAN*DN+OFFSET
	 This is the default filter.
.VARIABLE SCALE
 SCALE is a real number, used to scale output for SCENE and
 DIVIDE filters only.  The default is 100.
.VARIABLE DCTRAN
 DCTRAN is a real, DC transmission. Default = 0.0  (See the section on 
 DCTRAN in the main help section for tfilt.)
.VARIABLE BOOST
 BOOST is a real number, used to scale output for HIGH and LOW
 filters only.   Default is 1.0
.VARIABLE DCLEVEL
 DCLEVEL is a real additive factor in SCENE and HIGH filters only.
 Default is 128.0
.VARIABLE OFFSET
 OFFSET is a real additive factor used with
 LOW and DIVIDE filters only. (Default = 0.5)
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttfilt.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
GEN OUT=GEN NL=20 NS=17 ! generate a picture with byte format
LIST INP=GEN 'ZEROES ! check input data
tfilt INP=GEN OUT=(TEST1,X)  ! run tfilt - default all optional parameters
LIST INP=TEST1 'ZEROES
tfilt INP=GEN OUT=(TEST2,X) ! run tfilt with byte
LIST INP=TEST2 'ZEROES
! run tfilt with byte and nlw
tfilt INP=GEN OUT=(TEST4,X) NLW=3
LIST INP=TEST4  'ZEROES
! run tfilt with byte, nls and nsw
tfilt INP=GEN OUT=(TEST6,X) NLW=2 NSW=3
LIST INP=TEST6 'ZEROES
! run tfilt with byte, nls, nsw and thresh
tfilt INP=GEN OUT=(TEST7,X) NLW=2 NSW=3 THRESH=3
LIST INP=TEST7 'ZEROES
! run tfilt with nlw, nsw, thresh and high
tfilt INP=GEN OUT=(TEST8,X) NLW=3 NSW=3 THRESH=3 'HIGH
LIST INP=TEST8 'ZEROES
! run tfilt with thresh, scene
tfilt INP=GEN OUT=(TEST9,X) THRESH=2 'SCENE
LIST INP=TEST9 'ZEROES
tfilt INP=GEN OUT=(TEST10,X) NLW=4 'LOW !run tfilt with nlw and low
LIST INP=TEST10 'ZEROES
tfilt INP=GEN OUT=(TEST11,X) 'DIVIDE ! run tfilt with divide
LIST INP=TEST11 'ZEROES
! run tfilt with nlw and high - define boost,dctran, dclevel
tfilt INP=GEN OUT=(TEST12,X) NLW=2 'HIGH BOOST=2 DCTRAN=0.1 DCLEVEL=50.0
LIST INP=TEST12 'ZEROES
!run tfilt with nlw, nsw, and low - define boost  and offset
tfilt INP=GEN OUT=(TEST14,X) NLW=2 NSW=4 'LOW BOOST=2.00 OFFSET=20.
LIST INP=TEST14 'ZEROES
! run tfilt with halfword and nsw
GEN OUT=HGEN NL=20 NS=22 'HALF !generate a picture with halfword format
LIST INP=HGEN 'ZEROES
tfilt INP=HGEN OUT=(TEST3,X) ! run tfilt with halfword
LIST INP=TEST3 'ZEROES
tfilt INP=HGEN OUT=(TEST5,X) NSW=2
LIST INP=TEST5 'ZEROES
!run tfilt with halfword, nlw, nsw, thresh, scene, scale, dctran and dclevel
tfilt INP=HGEN OUT=(TEST13,X) NLW=2 NSW=2 THRESH=3 'SCENE +
 SCALE=200.0 DCTRAN=0.3 DCLEVEL=10
LIST  INP=TEST13  'ZEROES
! test multispectral option
GEN OUT=MGEN NL=20 NS=22 NB=10 'HALF ! generate a halfword multispectral file 
tfilt INP=MGEN OUT=(TEST15,X,Y) NLW=2 NSW=2 THRESH=3 'LOW
LIST  INP=TEST15 'ZEROES sinc=2 binc=2
!
! TEST FOR LARGE HALFWORD FILE, FULL DATA RANGE
!
Gen hgen_1 nl=500 ns=8000 Linc=20 Sinc=5 'Half
tfilt hgen_1 (test1_1,x) 'LOW
List test1_1 (1,1,10,10)
List test1_1 (1,6545,20,20)
end-proc
$ Return
$!#############################################################################
