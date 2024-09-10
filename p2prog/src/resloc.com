$!****************************************************************************
$!
$! Build proc for MIPL module resloc
$! VPACK Version 1.8, Friday, March 07, 1997, 14:30:44
$!
$! Execute by entering:		$ @resloc
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
$ write sys$output "*** module resloc ***"
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
$ write sys$output "Invalid argument given to resloc.com file -- ", primary
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
$   if F$SEARCH("resloc.imake") .nes. ""
$   then
$      vimake resloc
$      purge resloc.bld
$   else
$      if F$SEARCH("resloc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake resloc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @resloc.bld "STD"
$   else
$      @resloc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create resloc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack resloc.com -
	-s resloc.f -
	-i resloc.imake -
	-p resloc.pdf -
	-t tstresloc.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create resloc.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C   REVISION HISTORY
C
C     2-96  BAM added in new IBIS reseau location file format
C     7-95  AS  (CRI) MSTP S/W CONVERSION (VICAR PORTING)
C     3-89  SP  Prevent divide by zero.  Finish off lwk's fix to FILLOC where 
C               MIN set to 0.


      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

C  VICAR PROGRAM TO LOCATE RESEAUX ON VOYAGER PICTURES
C          B,1,1616,1,*,ORES
C          B,1,1616,3,*,GEOM
C          E,RESLOC,(PIC,RES),(ORES,GEOM)
C          E,RESLOC,(PIC,file),(ORES,GEOM)

      IMPLICIT INTEGER(A-Z)
      INTEGER*4 LAB(18,1)
      REAL*8 COEF,MOM,UMOM,A
      INTEGER IWORK(1)
      CHARACTER*144 LLAB
      EQUIVALENCE (PTS,LAB,IWORK),(LBL,LOC2)
      COMMON/C1/IFRM,ICAM,IFIL,IYEAR,IDreslocAY
      COMMON/C1/DBUG,PRINT,NRES,NLRI,REDO,IFIT,TOL
      COMMON/C1/SL,SSAMP,NL,NS,NLI,NSI,NI,NO,FMT,NPAR
      COMMON/C3/PTS(4,202),COEF(20),MOM(28),UMOM(20),A(100),LOC2(2,202)
      COMMON/C2/FRM,CAMERA,FILTER,YEAR,DAY,OLOC(2,202),LOC(2,202)
      COMMON/CLAB/NLAB,NLA,NSA,DCODE,BUF(1800)
      real loc,oloc      ! bam 
      real oloc2(2216)
      integer iret    ! subroutine return variable
      integer istat


c for porting     BAM
	integer ibis
        character *6 format(409) /5*'FULL',404*'REAL'/
        integer status
        integer nrows

 
      common/units/ in1,in2,out1,out2,ibis  ! added ibis - bam

c*******************************************************************


      CALL IFMESSAGE('RESLOC version 3/96')

      call xvunit( in1, 'INP', 1, istat ,0)   ! open input image
      if ( istat .ne. 1 ) then
          call xvmessage ( ' Error getting unit for image file',0)
          call xvmessage ( ' Program terminated.',0)
      end if
      call xvopen( in1, istat, 'OPEN_ACT', 'SA', 'IO_ACT', 'SA',
     .'U_FORMAT', 'HALF',0)
      if ( istat .ne. 1 ) then
          call xvmessage ( ' Error opening image file',0)
          call xvmessage ( ' Program terminated.',0)
      end if

C     get size of input and output
      call xvsize(sl,ssamp,nl,ns,nli,nsi,0)

C     get number of inputs, outputs
      call xvpcnt('INP',ni)
      call xvpcnt('OUT',no)

      iret = 0                     !initialize return variable
      CALL RESPAR(iret)            ! get resloc parameters
      if ( iret .eq. 1 ) go to 990 ! bad camera # - error

      CALL GETRES(LOC,ICAM)        ! given the camera, get loc data

      IF (NI.ne.1) then            ! image + reseau location file
                                   ! open the reseau location file
          call xvunit( in2, 'INP', 2, istat, 0)  
          if ( istat .ne. 1 ) then   
              call xvmessage ( ' Error getting unit for reseau file',0)
              call xvmessage ( ' Program terminated.',0)
              return
          end if

          call xvopen( in2, istat, 0)     ! get the label from the file
          if ( istat .ne. 1 ) then   
              call xvmessage ( ' Error opening reseau file',0)
              call xvmessage ( ' Program terminated.',0)
              return
          end if

          call vic1lab( in2, istat, j, llab, 2)  
          call chkstat(istat,'Error reading Reseau location header',
     -                 0,0,0)
	  call xvclose( in2, istat, 0)   ! close as a normal vicar file

          call xvunit( in2, 'INP', 2, istat, 0)  
          if ( istat .ne. 1 ) then   
              call xvmessage ( ' Error getting unit for reseau file',0)
              call xvmessage ( ' Program terminated.',0)
              return
          end if
          call ibis_file_open(in2,ibis,'read',409,99999,
     -                        format,0,status)
          if ( status .ne. 1 ) call ibis_signal_u(in2,status,1)


          icount = ibis_file_get(ibis,'nr',nrows,1,1) ! get nrows
          if ( nrows .lt. 0 ) call ibis_signal(ibis,icount,1)

          CALL GETRS2(MODE,nrows,iret)     ! find reseaus
          IF(MODE.EQ.0) GOTO 50            ! reseaus found 
      end if

C     FIND RESEAU BY CORRELATION

      CALL MVE(7,2*NRES,-99.,OLOC,0,1)
      IF(DBUG.ne.0) then
          CALL XVMESSAGE('NOMINAL LOCATIONS',0)
          CALL PMJS(LOC,1)
      end if

      iret = 0                     !initialize return variable
      CALL RCOR(iret)              ! resloc correlation routine
      if ( iret .eq. 1 ) go to 990 ! unexpected EOF encountered - error

      CALL RESFIT(OLOC,LOC,LOC2,PTS,COEF,MOM,UMOM,A,IFIT,TOL,DBUG)

   50 continue            ! so far all is ok
      IF(PRINT.EQ.1) then
          CALL XVMESSAGE('OUTPUT RESEAU LOCATIONS',0)
          CALL PMJS(OLOC,1)
      end if
C
C-----------------------------------------------------------------------

C-----if required, write reslocs in reseau file

      IF (NI .eq. 2) THEN    ! some sort of reseau file was input

	if (mode.eq.1) then  ! nominals were located
	  call putrs2(nrows) ! write reseau
	endif

      ENDIF


C-----------------------------------------------------------------
C
      call xvclose ( in1, istat,0 )

      if ( no .eq. 0 ) return

      if ( no .ge. 1 ) then     ! WRITE RESLOCS IN OUTPUT DATA SET
	call xvunit( out1, 'OUT', 1, istat, 0)
        if ( istat .ne. 1 ) then
            call xvmessage ( ' Error getting unit for reseau file',0)
            call xvmessage ( ' Program terminated.',0)
        end if
	call xvopen( out1, istat,'OP','WRITE', 0 )
        if ( istat .ne. 1 ) then
            call xvmessage ( ' Error getting unit for image file',0)
            call xvmessage ( ' Program terminated.',0)
        end if
	call xladd( out1, 'HISTORY', 'TITLE', '**RESLOC COORDINATES**',
     .	 i, 'FORMAT', 'STRING',0)
	call xvclose( out1, i, 0 )

        
      	call xvunit( out1, 'OUT', 1, istat, 0)
        nrows = 1
        call ibis_file_open(out1,ibis,'write',409,nrows,
     -                        format,0,status)
        if ( status .ne. 1 ) call ibis_signal_u(out1,status,1)

        call putrs1(nrows)       ! write a single resloc
        call ibis_file_close(ibis,' ',status)
      end if


      if ( no .eq. 2 ) then      ! GEOMA PARAMETERS
	call xvunit( out2, 'OUT', 2, istat ,0)
	call xvopen( out2, istat,'OP','WRITE', 0 )
	call xladd( out2, 'HISTORY', 'TITLE',
     .	 '**RESLOC GEOMA PARAMETERS**', i, 'FORMAT', 'STRING',0)
	call xvclose( out2, istat, 0)   ! close as a normal vicar file

	call xvunit( out2, 'OUT', 2, istat ,0)
        IWORK(1) = 2216
        CALL GEOMAV(oloc2,ICAM,OLOC)
        call xvmessage ( ' calling iwrite ',' ')
        call iwrite_tiepoints ( out2,23,22,0,oloc2(9),4)
      end if

      RETURN
C
C          UNEXPECTED EOF
  990 CALL xvmessage('**RESLOC TASK CANCELLED',0)
      return
      END


C
C**************************************************************
      SUBROUTINE RESPAR(iret)

C RESLOC PARAMETER PROCESSOR
      IMPLICIT INTEGER(A-Z)
      COMMON/CP/IBUG
      COMMON/C1/IFRM,ICAM,IFIL,IYEAR,IDAY
      COMMON/C1/DBUG,PRINT,NRES,NLRI,REDO,IFIT,TOL
      COMMON/C1/SL,SSAMP,NL,NS,NLI,NSI,NI,NO,FMT,NPAR
C      COMMON/C1/LAB(18,20)
      COMMON/CPAR/NLW,NSW,NHOR,NVER,INTERP,SIGMA,CTHRE,DNTHRE
      REAL*4 SIGMA,CTHRE,TOL
      INTEGER APAR(11)
      LOGICAL XVPTST
      common/units/ in1,in2,out1,out2,ibis  ! added ibis - bam
      integer bad
      data bad/-999/
C
      IBUG = 0
      DBUG = 0
      PRINT = 0
      NRES=202
C
      APAR(1) = 11
      CALL ABLE77v2(IND,in1,APAR,LAB)
      if (apar(2).ne.bad)  IFRM = APAR(2)
      if (apar(4).ne.bad)  IFIL = APAR(4)
      if (apar(6).ne.bad)  ICAM = APAR(6)
      if (apar(10).ne.bad) IYEAR = APAR(10)
      if (apar(11).ne.bad) IDAY = APAR(11)
C
C        'NLW - height of correlation'
      call xvparm('NLW',nlw,count,def,1)
C
C        'NSW - width of correlation'
      call xvparm('NSW',nsw,count,def,1)
C
C        'NVER - height of the search area'
      call xvparm('NVER',nver,count,def,1)
C
C        'NHOR - width of the search area'
      call xvparm('NHOR',nhor,count,def,1)
C
C        'DBUG - print diagnostics'
      if (xvptst('DBUG')) then
          dbug=1
          print=1
      end if 
C
C        'CAMERA - camera serial number override'
      call xvparm('CAMERA',icamm,count,def,1)
      if (def .eq. 0) icam=icamm
C
C        'PRINT - generates listing of reseaus and GEOMA parameters'
      if (xvptst('PRINT')) print=1
C
C        'NOIN - suppresses the interpolation routine'
      if (xvptst('NOIN')) then
          interp=0
      else
          interp=1
      end if
C
C        'SIGMA - standard deviation constant for reseau shape function'
      call xvparm('SIGMA',sigma,count,def,1)
C
C        'REDO - ignore entry in the VGR reseau location master file'
      if (xvptst('REDO')) then
          redo=1
      else
          redo=0
      end if 
C
C        'FRAME - frame number override'
      call xvparm('FRAME',ifrmm,count,def,1)
      if (def .eq. 0) ifrm=ifrmm
C
C        'CTHRESH - correlation threshold'
      call xvparm('CTHRESH',cthre,count,def,1)
C
C        'FIT - select fit type'
      call xvparm('FIT',ifit,count,def,1)
C
C        'TOLER - max location error'
      call xvparm('TOLER',tol,count,def,1)
C
C        'DNTHRESH - DN threshold in candidate reseau centers'
      call xvparm('DNTHRESH',dnthre,count,def,1)
C
      IF(ICAM.LT.1.OR.ICAM.GT.8) THEN
         CALL XVMESSAGE('INVALID CAMERA S/N',0)
         iret = 1
      END IF 

      IF(DBUG.EQ.1) CALL PRNT(4,11,IFRM,'IFRM=.')
      RETURN
      END
C


C*********************************************************

      SUBROUTINE GETRS2(MODE,nrows,iret)
C ROUTINE TO PUT RESLOCS OR NOMINALS IN OLOC
C          MODE  = 0 RESLOCS FounD
C                = 1 NOMINALS FounD
C                = 2 NOT RESLOC FILE, NOMINALS ASSUMED
c          nrows = rows in the interface file
c          iret  = 0 passed subroutine ok
c                = 1 error

c
c     history - rewritten for porting  bam 3/96
c
      IMPLICIT INTEGER(A-Z)
      COMMON/C1/IFRM,ICAM,IFIL,IYEAR,IDAY
      COMMON/C1/DBUG,PRINT,NRES,NLRI,REDO,IFIT,TOL
      COMMON/C1/SL,SSAMP,NL,NS,NLI,NSI,NI,NO,FMT,NPAR
      COMMON/C5/DSRN,LRW,NLM,FTBL(8098),CTBL(8098)


! this is the imformation passed into getlocv2      

      COMMON/C2/FRM,CAMERA,FILTER,YEAR,DAY,OLOC(2,202),LOC(2,202)
      real *4 loc,oloc      ! bam 

      common/units/ in1,in2,out1,out2,ibis  ! added ibis - bam

      CHARACTER*4 NOM(8),nomu(8)
! for alphas
      DATA NOM/'NOM1','NOM2','NOM3','NOM4','NOM5','NOM6','NOM7','NOM8'/
! for unix
      DATA nomu/'1MON','2MON','3MON','4MON','5MON','6MON','7MON','8MON'/
C
C     yes, folks, the above is a cludge to get around the byte swapping
c     under unix
C


      dsrn = in2
      FRM = IFRM
      CAMERA = ICAM
      iret = 0               ! return flag set to ok
      
      call testos(ios)       ! check to swap bytes for UNIX

      if (redo.eq.0) then    ! check master reseau file

          call getlocv2(ibis,nrows,frm,oloc,iret)

          if ( iret .eq. 0 ) then    ! frame located
              mode = 0
          else                       ! frame not found
                                     ! check for nominals
              call xvmessage(' Frame not found in Master Reseau File',0)
              call xvmessage(' Checking nominals.',0)
              call xvmessage(' ',0)
  
              if (ios .eq. 0) then
                  call xvmessage('The OS is VMS',' ')
                  call mvcl(nom(icam),frm,4) ! change char to integer
              else if (ios .eq. 1) then
                  call xvmessage('The OS is UNIX',' ')
                  call mvcl(nomu(icam),frm,4) ! change char to integer
              end if

              call getlocv2(ibis,nrows,frm,oloc,iret)

              if ( iret .eq. 0 ) then    ! found nominals
                  call xvmessage ( ' Nominals located.',0)
                  call xvmessage ( ' ',0)
                  CALL MVE(7,2*NRES,OLOC,LOC,1,1)
                  mode = 1 
              else                   ! still nothing
                  go to 980  
              end if

          end if

      else if (redo.eq.1) then    ! ignore master reseau file

          if (ios .eq. 0) then
              call xvmessage('The OS is VMS',' ')
              call mvcl(nom(icam),frm,4) ! change char to integer
          else if (ios .eq. 1) then
              call xvmessage('The OS is UNIX',' ')
              call mvcl(nomu(icam),frm,4) ! change char to integer
          end if

          call getlocv2(ibis,nrows,frm,oloc,iret)

          if ( iret .eq. 0 ) then    ! found nominals
              CALL MVE(7,2*NRES,OLOC,LOC,1,1)
              mode = 1 
          else                   ! still nothing
              go to 980  
          end if

      else                        ! read nominals file

          MODE = 2
          call xvread(dsrn,loc,i,'LINE',1,'NSAMPS',2*NRES,0)

      end if              

      return
C
  980 CALL mabend('**NOMINALS MISSING')

      return
      end

C----------------------------------------------------------

      subroutine PUTRS2(nrows)
C ROUTINE TO PUT RESLOCS INTO MASTER RESLOC FILE
c     history - rewritten for porting  bam 3/96
c
      IMPLICIT INTEGER(A-Z)
      COMMON/C1/IFRM,ICAM,IFIL,IYEAR,IDAY
      COMMON/C1/DBUG,PRINT,NRES,NLRI,REDO,IFIT,TOL
      COMMON/C1/SL,SSAMP,NL,NS,NLI,NSI,NI,NO,FMT,NPAR
      COMMON/C5/DSRN,LRW,NLM,FTBL(8098),CTBL(8098)
      COMMON/C2/FRM,CAMERA,FILTER,YEAR,DAY,OLOC(2,202),LOC(2,202)
      COMMON/CLAB/NLAB,NLA,NSA,DCODE,BUF(1800)
      real *4 loc,oloc      ! bam 

      common/units/ in1,in2,out1,out2,ibis  ! added ibis - bam


      CALL MVE(4,5,IFRM,FRM,1,1)
      CALL PUTLOCv2(in2,ibis,nrows,frm,oloc)

      call ibis_file_close(ibis,' ',status)

      call prnt ( 4, 1, frm,' Frame ' )
      call xvmessage ( ' Stored in Master Reseau File.',0 )

      return
      end
C----------------------------------------------------------

      subroutine putrs1(nrows)
C ROUTINE TO PUT A SINGLE RESLOC INTO AN OUTPUT FILE
c     history - rewritten for porting  bam 3/96
c
      IMPLICIT INTEGER(A-Z)
      COMMON/C1/IFRM,ICAM,IFIL,IYEAR,IDAY
      COMMON/C1/DBUG,PRINT,NRES,NLRI,REDO,IFIT,TOL
      COMMON/C1/SL,SSAMP,NL,NS,NLI,NSI,NI,NO,FMT,NPAR
      COMMON/C5/DSRN,LRW,NLM,FTBL(8098),CTBL(8098)
      COMMON/C2/FRM,CAMERA,FILTER,YEAR,DAY,OLOC(2,202),LOC(2,202)
      COMMON/CLAB/NLAB,NLA,NSA,DCODE,BUF(1800)
      real *4 loc,oloc      ! bam 

      common/units/ in1,in2,out1,out2,ibis  ! added ibis - bam



      CALL MVE(4,5,IFRM,FRM,1,1)
      CALL PUTLOCv2(out1,ibis,nrows,frm,oloc)

      call ibis_file_close(ibis,' ',status)

      call prnt ( 4, 1, frm,' Frame ' )
      call xvmessage ( ' Stored in Output Reseau File.',0 )

      RETURN
      END

C
C**************************************************************
      SUBROUTINE RCOR(iret)

C RESLOC CORRELATION ROUTINE
      IMPLICIT INTEGER(A-V)
      COMMON/C1/IFRM,ICAM,IFIL,IYEAR,IDAY
      COMMON/C1/DBUG,PRINT,NRES,NLRI,REDO,IFIT,TOL
      COMMON/C1/SL,SSAMP,NL,NS,NLI,NSI,NI,NO,FMT,NPAR
      COMMON/C4/ILINE(2,202)
      COMMON/C2/FRM,CAMERA,FILTER,YEAR,DAY,OLOC(2,202),LOC(2,202)
      COMMON/CPAR/NLW,NSW,NHOR,NVER,INTERP,SIGMA,CTHRE,DNTHRE
      INTEGER*4 MAXBUF
      INTEGER*2 ILINE
      REAL*4 LOC,OLOC
      BYTE WORK(54000)
      CHARACTER*39 MSG
      integer iret, jret

C
      MAXBUF=54000
      MSG='NLW=** NSW=** NHOR=** NVER=** INTERP=*'
      CALL ZIA(WORK,MAXBUF/4)
      NLIC = NVER + NLW - 1
      NSIC = NHOR + NSW - 1

C     GET INITIAL LINE (ILINE) OF EACH CORRELATION AREA WHILE
C     INSURING THAT THE AREA REMAINS WITHIN THE PICTURE

      DO LRES=1,NRES
          L = LOC(1,LRES) - NLIC/2 + .5
          L = MAX0(L,1)
          L = MIN0(L,NLI-NLIC+1)
          ILINE(1,LRES) = L
          ILINE(2,LRES) = LRES
      end do

C     ELEMINATE MARKS ALONG EDGES OF PICTURE

      DO LRES=1,12
          ILINE(1,LRES) = NLI
          ILINE(1,LRES+189) = NLI
      end do

      DO LRES=47,152,15
          ILINE(1,LRES) = NLI
          ILINE(1,LRES+3) = NLI
      end do

      ILINE(1,24) = NLI
      ILINE(1,35) = NLI
      ILINE(1,167) = NLI
      ILINE(1,178) = NLI
      CALL SORTX(ILINE,NRES)
      WRITE (MSG(5:6),'(I2)') NLW
      WRITE (MSG(12:13),'(I2)') NSW
      WRITE (MSG(20:21),'(I2)') NHOR
      WRITE (MSG(27:28),'(I2)') NVER
      WRITE (MSG(38:38),'(I1)') INTERP
      CALL XVMESSAGE(MSG,0)
C          ASSIGN WORK SPACE
      I = 1
      IRHO = I
      I = I + 4*NHOR*NVER
      IAP = I
      I = I + 4*NSW*NLW
      IS2 = I
      I = I + 4*NSIC
      IS = I
      I = I + 2*NSIC
      IB = I
      I = I + 2*NSI*NLIC
      IA = I
      I = I + 2*NSW*NLW
      CALL PRNT(4,1,I,' TOT BUF=.')

      IF(I.GT.MAXBUF) GOTO 990

      jret = 0
      CALL RCOR3(WORK(IA),WORK(IB),WORK(IS),WORK(IS2),WORK(IAP),
     &WORK(IRHO),NHOR,NVER,NLW,NSW,NSI,SIGMA,CTHRE,DNTHRE,INTERP,jret)
      if ( jret .eq. 1 ) then
          iret = 1
          return  ! error
      end if

      CALL FILLOC(LOC,OLOC)
      RETURN


  990 CALL XVMESSAGE('BUFFER REQUIREMENTS EXCEED CORE ALLOCATION',0)
      CALL XVMESSAGE('TRY REDUCING NHOR, NLW, OR NSW',0)
      iret = 1
      return
C
      END
C
C**************************************************************
      SUBROUTINE RCOR3(A,B,JSUM,JMOM,AP,R,NHOR,NVER,NLW,NSW,NSIN,SIGMA,
     &CTHRE,DNTHRE,INTERP,jret)
      IMPLICIT INTEGER(A-Z)
      COMMON/C1/IFRM,ICAM,IFIL,IYEAR,IDAY
      COMMON/C1/DBUG,PRINT,NRES,NLRI,REDO,IFIT,TOL
      COMMON/C1/SL,SSAMP,NL,NS,NLI,NSI,NI,NO,FMT,NPAR
      COMMON/C4/ILINE(2,202)
      COMMON/C2/FRM,CAMERA,FILTER,YEAR,DAY,OLOC(2,202),LOC(2,202)
      common/units/ in1,in2,out1,out2,ibis  ! added ibis - bam
      INTEGER*2 A(NSW,NLW),B(NSIN,1),JSUM(1),ILINE
      INTEGER*4 JMOM(1)
      REAL*4 AP(NSW,NLW),R(NHOR,NVER),RMAX,SCALE,CTHRE,CMIN,CSUM
      REAL*4 LOC,OLOC,X,Y,DX,DY
      CHARACTER*18 MSG
      CHARACTER*29 MSG1
      CHARACTER*46 MSG2

      MSG='(SL,SS)=(***,***)'
      MSG1='RES MARK *** CANNOT BE Found'
      MSG2='CORRELATION MATRIX FOR MARK *** (***.*,***.*)'
C
      IF(DBUG.EQ.1) CALL XVMESSAGE('RCOR3 HERE',0)
      NLIC = NVER + NLW - 1
      NSIC = NHOR + NSW - 1
      NCOR = NHOR*NVER
      NLWH = NLW/2
      NSWH = NSW/2
      CALL GAUSS(0.,0.,SIGMA,A,NLW,NSW)
      CALL OPCON(NLW,NSW)
      CALL DTREND(A,AP,NLW,NSW)

      IF(DBUG.ne.0) then
          DO J=1,NLW
             CALL PRNT(2,NSW,A(1,J),'0.')
          end do
          DO J=1,NLW
             CALL PRNT(7,NSW,AP(1,J),0)
          end do
      end if

      KounT = 0
      CSUM = 0.
      CMIN = 1.
      BI = 0
      IRES = 0
      LLIC = 0
C          LLIC = LAST LINE IN CORE
C
C
   10 IRES = IRES + 1
      L = ILINE(1,IRES)
      IF(L.EQ.NLI) GOTO 100
      LRES = ILINE(2,IRES)
      BLINE = L
      ELINE = L + NLIC - 1


      IF(ELINE.EQ.LLIC) GOTO 15


C          READ IN REQUIRED LINES

      IF(BLINE.LE.LLIC) BLINE=LLIC+1

      DO LLIC=BLINE,ELINE
        BI = MOD(BI,NLIC) + 1
	call xvread( in1, b(1,bi), i, 'LINE', llic, 0)
      end do

	LLIC = ELINE
C
C          GET LOCATION OF LEFTMOST SAMPLE IN CORRELATION AREA
   15 S = LOC(2,LRES) - NSIC/2 + .5
      BJ = MOD(BI,NLIC) + 1
      BK = MOD(BJ+NLW-2,NLIC) + 1
C
C          COMPUTE VERTICAL MOMENTS
      CALL MVE(2,NSIC,0,JSUM,0,1)
      CALL ZIA(JMOM,NSIC)
      J = BJ
C
      DO K=1,NLW
          CALL MOMGEN(B(S,J),JSUM,JMOM,K,NSIC)
          J = MOD(J,NLIC) + 1
      end do


C
C     IF(DBUG.EQ.0) GOTO 19
C     CALL OUTCON(L,MSG(13),3)
C     CALL OUTCON(S,MSG(17),3)
C     CALL QPRINT(MSG,18)
C     J = BJ
C     DO 18 I=1,NLIC
C     CALL PRNT(2,NSIC,B(S,J),'0.')
C  18 J = MOD(J,NLIC) + 1
C     CALL PRNT(2,NSIC,JSUM,' JSUM=.')
C     CALL PRNT(4,NSIC,JMOM,' JMOM=.')
C  19 CONTINUE


      CALL ZIA(R,NCOR)
      I = BK
      J = BJ
C
C
C          COMPUTE CORRELATION MATRIX R(NHOR,NVER)
      DO 40 JJ=1,NVER
          CALL CONVLD(AP,B(S,1),NLW,NSW,JSUM,JMOM,J,
     -                R(1,JJ),NHOR,NSIN,NLIC)
          IF(JJ.EQ.NVER) GOTO 40
          I = MOD(I,NLIC) + 1   ! UPDATE VERTICAL MOMENTS
          CALL UPMOMV(B(S,I),B(S,J),JSUM,JMOM,NSIC,NLW)
          J = MOD(J,NLIC) + 1
D         IF(DBUG.EQ.0) GOTO 40
D         CALL PRNT(2,NSIC,B(S,I),'0.')
D         CALL PRNT(2,NSIC,JSUM,' JSUM=.')
D         CALL PRNT(4,NSIC,JMOM,' JMOM=.')
   40 CONTINUE
C
      IJ = BJ
      IJ = MOD(IJ+NSW/2,NLIC)
      CALL MAXR( R, NHOR, NVER, B(S+NLW/2,1), IJ, NSIN,
     & CTHRE, DNTHRE, NLIC, RMAX, K, J)
      IF(RMAX.LT.CMIN) CMIN=RMAX
      KounT = KounT + 1
      CSUM = CSUM + RMAX

      IF(DBUG.EQ.0) GOTO 42
          call prnt ( 4, 1, lres,' Res Mark ' )
          call prnt ( 7, 1, rmax,' Corr=0. ' )

   42 IF(RMAX.LT.CTHRE) GOTO 992
      DX = K
      DY = J
      IF(INTERP.EQ.0) GOTO 60
      IF(K.EQ.1.OR.J.EQ.1) GOTO 990
      IF(K.EQ.NHOR.OR.J.EQ.NVER) GOTO 990

      kret = 0
      CALL QXLOC(R(K-1,J-1),X,Y,NHOR,kret)
      if ( kret .eq. 1 ) go to 55

      IF(ABS(X).GT.1.OR.ABS(Y).GT.1.) GOTO 55
      DX = DX + X
      DY = DY + Y
      GOTO 60

   55 CALL PRNT(4,1,LRES,'FIT ERROR AT RESEAU MARK =.')
C
   60 DY = L + DY + (NLWH-1)
      DX = S + DX + (NSWH-1)
      OLOC(1,LRES) = DY
      OLOC(2,LRES) = DX
      GOTO 10
C
  100 CALL PRNT(7,1,CMIN,'MINIMUM CORRELATION VALUE=.')
      CALL PRNT(7,1,CSUM/KounT,'AVERAGE VALUE=.')
      RETURN
C
  990 WRITE (MSG1(10:12),'(I3)')LRES
      CALL XVMESSAGE(MSG1,0)
      CALL XVMESSAGE
     &         ('CORRELATION MAX LIES ON MARGIN OF SEARCH AREA',0)
      GOTO 998
C
  992 WRITE (MSG1(10:12),'(I3)')LRES
      CALL XVMESSAGE(MSG1,0)
      CALL PRNT(7,1,RMAX,'CORRELATION MAX TOO LOW=.')
C
  998 IF(DBUG.EQ.0) GOTO 10
      WRITE (MSG2(29:31),'(I3)')LRES
      WRITE (MSG2(34:38),'(F5.1)')DY
      WRITE (MSG2(40:44),'(F5.1)')DX
      CALL XVMESSAGE(MSG2,0)
      CALL PRNT(7,1,RMAX,'RMAX=.')
      CALL PRNT(4,1,J,'J=.')
      CALL PRNT(4,1,K,'K=.')
      IF(RMAX.LT.1.E-08) GOTO 10
      SCALE = 255./RMAX
      CALL SPRNT(R,JMOM,SCALE,NHOR,NVER)
      GOTO 10
C
      END
C
C**************************************************************
      SUBROUTINE QXLOC(Q,X,Y,NHOR,kret)
C          ROUTINE TO APPROXIMATE A 3X3 AREA BY A QUADRATIC AND
C          RETURN THE LOCATION OF THE EXTREME VALUE.
C  QUADRATIC IS OF THE FORM
C          Q(X,Y) = DXX*X**2+DXY*X*Y+DYY*Y**2+EX*X+EY*Y+F
      REAL*4 Q(NHOR,*)
C
      DXX = 0.
      DYY = 0.
C          COMPUTE 2ND DERIVATIVES
      DO 10 I=1,3
      DXX = DXX + Q(1,I) - 2.*Q(2,I) + Q(3,I)
   10 DYY = DYY + Q(I,1) - 2.*Q(I,2) + Q(I,3)
C
      DXX = DXX/6.
      DYY = DYY/6.
      DXY = .25*(Q(1,1)+Q(3,3) - Q(1,3)-Q(3,1))
      EX = (Q(3,1)+Q(3,3) - Q(1,1)-Q(1,3))/6.
      EY = (Q(1,3)+Q(3,3) - Q(1,1)-Q(3,1))/6.
C          LOCATE EXTREMUM
      D = 4.*DXX*DYY - DXY*DXY

      IF(D.EQ.0.) then
         kret = 1
         return
      end if

      X = (EY*DXY-2.*EX*DYY)/D
      Y = (EX*DXY-2.*EY*DXX)/D
      RETURN
      END
C
C**************************************************************
      SUBROUTINE RESFIT(LOC,REF,WORK,PTS,COEF,MOM,UMOM,A,IFIT,TOL,IBUG)
C ROUTINE TO FIT RESLOCS (LOC) TO NOMINALS (REF)
      REAL*4 LOC(2,202),REF(2,202),WORK(2,202),PTS(4,202)
      REAL*8 COEF(20),MOM(28),UMOM(20),A(100)
      CHARACTER*90 MSG
      CHARACTER*33 MSG1
      CHARACTER*10 RMSG

      MSG(1:54)='RESEAU MARK XXX HAD THE LARGEST RESIDUE OF **.* PIXELS'
      MSG(55:90)=' (****.*,****.*) NOM=(****.*,****.*)'
      MSG1='MARK *** CHANGED TO (***.*,***.*)'
      RMSG='RMS=***.**'
      NRES = 202
      NPOW = MAX0(IFIT-7,1)
      IMAX0 = 0
      ILOOP = 0
      IFLAG = 0
C
C
    5 ILOOP = ILOOP + 1
C
      DO 10 I=1,NRES
      PTS(2,I) = LOC(1,I)
      PTS(1,I) = LOC(2,I)
      PTS(4,I) = REF(1,I)
      PTS(3,I) = REF(2,I)
      IF(PTS(1,I).NE.-99.) GOTO 10
      IFLAG = 1
      CALL MVE(7,2,PTS(3,I),PTS,1,1)
   10 CONTINUE
C
      IF(IFLAG.EQ.1) GOTO 50
C
      CALL LFIT(IFIT,PTS,NRES,COEF,MOM,UMOM,A,*100)
      CALL RMSFIT(NPOW,PTS,NRES,COEF,RMS,RMAX,IMAX,1)
      IF(IMAX.EQ.IMAX0) GOTO 50
      IMAX0 = IMAX
      WRITE (RMSG(5:10),'(F6.2)') RMS
      CALL XVMESSAGE(RMSG,0)
      WRITE (MSG(13:15),'(I3)') IMAX
      WRITE (MSG(44:47),'(F4.1)') RMAX
      WRITE (MSG(57:62),'(F6.1)') LOC(1,IMAX)
      WRITE (MSG(64:69),'(F6.1)') LOC(2,IMAX)
      WRITE (MSG(77:82),'(F6.1)') REF(1,IMAX)
      WRITE (MSG(84:89),'(F6.1)') REF(2,IMAX)
      CALL XVMESSAGE(MSG,0)
      IF(RMAX.LT.TOL) GOTO 90
      LOC(1,IMAX) = -99.
      LOC(2,IMAX) = -99.
      CALL FILLOC(REF,LOC)
      WRITE (MSG1(6:8),'(I3)') IMAX
      WRITE (MSG1(22:26),'(F5.1)') LOC(1,IMAX)
      WRITE (MSG1(28:32),'(F5.1)') LOC(2,IMAX)
      CALL XVMESSAGE(MSG1,0)
      GOTO 5
C
C
   50 CALL XVMESSAGE
     &        ('**WARNING--MAXIMUM OUTPUT ERROR EXCEEDS TOLERANCE',0)
C
   90 IF(IBUG.EQ.0) RETURN
C
      DO I=1,NRES
          WORK(1,I) = PTS(4,I) - REF(1,I)
          WORK(2,I) = PTS(3,I) - REF(2,I)
      end do

      CALL XVMESSAGE('DIFFERENCES FROM NOMINALS',0)
      CALL PMJS(WORK,1)
C
C
  100 RETURN
      END
C
C**************************************************************
      SUBROUTINE FILLOC(ILOC,OLOC)
C          ROUTINE TO FILL IN MISSING LOCATIONS
      REAL*4 ILOC(2,1),OLOC(2,1)
      INTEGER*2 L1(3,8),L2(3,6),L3(2,2),IBUFR(6,2),IBUFS(4,9),IBUF(7)
      INTEGER   L4(2,2)
      DATA L1/25,13,36,14,2,25,2,13,13,37,25,25,168,156,179,
     *        180,168,191,191,179,179,157,168,168/
      DATA L2/48,36,37,51,47,48,46,34,35,49,45,46,50,46,46,47,36,36/
      DATA L3/52,48,60,49/,L4/13,1,166,154/
      DATA IBUFR/179,167,168,180,191,190,23,12,11,22,34,35/
      DATA IBUFS/1,2,13,24,24,13,25,36,12,11,23,35,202,43,58,59,190,167,
     *           179,191,201,178,189,200,35,23,34,46,167,156,168,179,
     *           178,166,177,189/

      LOOP = 0
      MIN = 2

    1 CONTINUE                  ! SCAN FOR MISSING LOCS

      DO 3 I=1,202
          IF(ILOC(1,I).EQ.-99)  GOTO 3
          IF(OLOC(1,I).EQ.-99.) GOTO 5
    3 continue

      RETURN

    5 LOOP = LOOP + 1
c      IF(LOOP.GT.5)  RETURN	!***lwk: this can cause incomplete fillin
      IF(LOOP.GT.2) MIN=1

c***lwk:  replace above with:
      if (loop.gt.5) then  ! if all reference OLOC's are -99, set OLOC = ILOC
	min = 0
	call xvmessage
     *   ('5 iterations in FILLOC:  set remaining locs to defaults',0)
      endif

      INC = -1
      M = 9
C
      DO 10 J=1,8
      IBUF(1) = L1(1,J)
      IBUF(2) = L1(2,J)
      IBUF(3) = L1(3,J)
      IBUF(4) = IBUF(1) + 1
      IBUF(5) = IBUF(2) + 1
      IBUF(6) = IBUF(3) + 1
      IBUF(7) = IBUF(1) - 1
      INC = - INC
      M = M + INC
   10 CALL FILL1(IBUF,7,M,ILOC,OLOC,MIN)
C
      INC = 1
C
      DO 15 J=1,6
      INC = -INC
      IBUF(1) = L2(1,J)
      IBUF(2) = L2(2,J)
      IBUF(3) = L2(3,J)
      IBUF(4) = IBUF(1) + INC
      IBUF(5) = IBUF(2) + 15
      IBUF(6) = IBUF(3) + 15
      DO 15 I=1,8
      CALL FILL1(IBUF,6,1,ILOC,OLOC,MIN)
   15 CALL ADDV(-6,6,15,IBUF,0,1)
C
      DO 17 J=1,2
      IBUF(1) = L3(1,J)
      IBUF(2) = L3(2,J)
      IBUF(3) = IBUF(1) - 1
      IBUF(4) = IBUF(1) + 1
      IBUF(5) = IBUF(2) + 15
      DO 17 I=1,7
      CALL FILL1(IBUF,5,1,ILOC,OLOC,MIN)
   17 CALL ADDV(-6,5,15,IBUF,0,1)
C
C
      DO 20 I0=53,143,15
      IBUF(1) = I0
      IBUF(2) = I0 - 15
      IBUF(3) = I0 + 15
      IBUF(4) = I0 - 1
      IBUF(5) = I0 + 1
   20 CALL FILL1(IBUF,5,7,ILOC,OLOC,MIN)
C
C
      INC = -1
C
      DO 23 J=1,2
      INC = - INC
      IBUF(1) = L4(1,J)
      IBUF(2) = L4(2,J)
      IBUF(3) = IBUF(2) + 1
      IBUF(4) = IBUF(1) + INC
      IBUF(5) = IBUF(2) + 23
      IBUF(6) = IBUF(3) + 23
      DO 23 I=1,2
      CALL FILL1(IBUF,6,1,ILOC,OLOC,MIN)
   23 CALL ADDV(-6,6,23,IBUF,0,1)
C
C
      DO 25 J=1,9
   25 CALL FILL1(IBUFS(1,J),4,1,ILOC,OLOC,MIN)
C
      DO 27 J=1,2
   27 CALL FILL1(IBUFR(1,J),6,1,ILOC,OLOC,MIN)
C
      GOTO 1
C
      END
C
C**************************************************************
      SUBROUTINE FILL1(IBUF,N,M,ILOC,OLOC,MIN)

c  this routine fills OLOC(1,IBUF(1)) thru OLOC(1,IBUF(1)+M-1)
c  with offsets found from locs offset by IBUF(2), ... IBUF(N).
c  ... lwk...

      REAL*4 ILOC(2,1),OLOC(2,1)
      INTEGER*2 IBUF(1)
C
      DO 15 JJ=1,M
      I0 = IBUF(1) + JJ - 1
      IF(OLOC(1,I0).NE.-99.) GOTO 15
      KNT = 0
      DX = 0.
      DY = 0.
C
      DO 10 J=2,N
      I = IBUF(J) + JJ - 1

c      IF(OLOC(1,I).LT.0.) GOTO 10	!***(lwk) try replacing with:
      IF(OLOC(1,I).EQ.-99.) GOTO 10

      KNT = KNT + 1
      DY = DY + OLOC(1,I) - ILOC(1,I)
      DX = DX + OLOC(2,I) - ILOC(2,I)
   10 CONTINUE
C
      IF(KNT.LT.MIN) GOTO 15

c...prevent divide by zero.  Finish off lwk's fix to FILLOC where MIN set to 0.

      IF (KNT .EQ. 0)  THEN
         OLOC(1,I0) = ILOC(1,I0) 
         OLOC(2,I0) = ILOC(2,I0) 
      ELSE
         OLOC(1,I0) = ILOC(1,I0) + DY/KNT
         OLOC(2,I0) = ILOC(2,I0) + DX/KNT
      END IF

   15 CONTINUE
C
      RETURN
      END
C
C**************************************************************
      SUBROUTINE GAUSS(X0,Y0,SIGMA,A,NLW,NSW)
      INTEGER*2 A(NSW,NLW)
C
      S = -1./(2.*SIGMA*SIGMA)
      NLWH = NLW/2 + 1
      NSWH = NSW/2 + 1
C
      DO 10 J=1,NLW
      Y = Y0 + (J-NLWH)
      Y2 = Y*Y
      DO 10 I=1,NSW
      X = X0 + (I-NSWH)
      X2 = X*X
   10 A(I,J) = 255.*(1.-EXP(S*(X2+Y2)))
C
      RETURN
      END
C
C**************************************************************
      SUBROUTINE SPRNT(R,BUF,SCALE,NHOR,NVER)
      BYTE BUF(1)
      INTEGER S,SI,SS
      REAL*4 R(NHOR,NVER)
      CHARACTER*8 LMSG
      CHARACTER*133 SMSG
C
      LMSG='      .'
      SMSG='     SAMP'
C          LIST OUT CORRELATION MATRIX IN VERTICAL STRIPS, 30 ELEMENTS
C   PER LINE
      DO 74 SI=1,NHOR,30
      SS = SI - 1
      NS = MIN0(NHOR-SS,30)
      IF (NS.LT.30) THEN
         SMSG(11:59)='@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'
         SMSG(60:107)='@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'
         SMSG(108:123)='@@@@@@@@@@@@@@@@'
      END IF
C          PRINT SAMPLE HEADING
      DO 72 S=1,NS
   72 WRITE (SMSG(4*S+10:4*S+11),'(I2)') SS+S
      CALL XVMESSAGE(SMSG,0)
C
      DO 74 L=1,NVER
      WRITE (LMSG(5:6),'(I2)') L
      CALL SFIX(1,NS,R(SI,L),BUF,SCALE,.5)
      CALL XVMESSAGE(0,0)
   74 CALL PRNT(1,NS,BUF,LMSG)
C
      RETURN
      END
C
C**************************************************************
      SUBROUTINE LFIT(IFIT,PTS,N,COEF,MOM,UMOM,A,*)
      COMMON/CP/IBUG
      REAL*4 PTS(4,1)
      REAL*8 COEF(1),MOM(1),UMOM(1),A(1),EPS
C
      EPS=1.D-15
      NPOW = MAX0(IFIT-7,1)
      NU = (NPOW+1)*(NPOW+2)/2
      IF(NU.GT.N) GOTO 999
      CALL MOMENT(NPOW,PTS,N,MOM,UMOM,0)
      IF(IBUG.EQ.0) GOTO 10
      CALL XVMESSAGE('MOMENTS=',0)
      CALL PMOM(2*NPOW,MOM)
      CALL XVMESSAGE('UMOMENTS=',0)
      CALL PMOM(NPOW,UMOM)
      CALL PMOM(NPOW,UMOM(NU+1))
   10 IF(IFIT.LT.8) CALL CLFIT(IFIT,COEF,MOM,UMOM,IER)
      IF(IFIT.GE.8) CALL LSPFIT(NPOW,COEF,MOM,UMOM,A,EPS,IER)
      IF(IER.NE.0) GOTO 998
      RETURN
C
  998 CALL PRNT(4,1,IER,'**FIT ERR=.')
  999 CALL XVMESSAGE('REQUESTED FIT IS UNDERCONSTRAINED',0)
      CALL XVMESSAGE('REDUCE FIT OR ADD MORE POINTS',0)
      RETURN1
      END
C
C**************************************************************
      SUBROUTINE PMOM(NPOW,MOM)
      REAL*8 MOM(1)
      CHARACTER*132 MSG
C
      MSG(1:54)='@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'
      MSG(55:106)='@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'
      MSG(107:132)='@@@@@@@@@@@@@@@@@@@@@@@@@@'
      L = 1
      WRITE (MSG(1:9),'(F9.1)') MOM(1)
      CALL XVMESSAGE(MSG,0)
C
      DO 20 I=1,NPOW
      M = 1
      IP1 = I + 1
      DO 10 J=1,IP1
      L = L + 1
      M = M + 9
   10 WRITE (MSG(M-9:M-1),'(F9.1)') MOM(L)
   20 CALL XVMESSAGE(MSG,0)
C
      RETURN
      END
C
C**************************************************************
      SUBROUTINE MOMENT(NPOW,PTS,N,MOM,UMOM,MODE)
      IMPLICIT REAL*8 (R-Z)
      REAL*4 PTS(4,1)
      REAL*8 MOM(1),UMOM(1)
C
      MPOW = 2*NPOW
      NU = (NPOW+1)*(NPOW+2)/2
      NM = (MPOW+1)*(MPOW+2)/2
      U = 0.D0
      V = 0.D0
      IF(MODE.NE.0) GOTO 5
      CALL MVE(8,NM,U,MOM,0,1)
      CALL MVE(8,2*NU,U,UMOM,0,1)
    5 CONTINUE
C
      DO 10 K=1,N
      XI = PTS(2,K)
      YI = PTS(1,K)
      UI = PTS(4,K)
      VI = PTS(3,K)
      U = U + UI
      V = V + VI
      L = 1
C
      DO 10 I=1,MPOW
      IP1 = I + 1
      IPOW = I
      Z = XI**I
      DO 10 J=1,IP1
      L = L + 1
      IF(J.EQ.1) GOTO 8
      IF(J.EQ.IP1) GOTO 7
      Z = XI**IPOW*YI**(I-IPOW)
      GOTO 8
    7 Z = YI**I
    8 MOM(L) = MOM(L) + Z
      IF(I.GT.NPOW) GOTO 10
      UMOM(L) = UMOM(L) + UI*Z
      UMOM(NU+L) = UMOM(NU+L) + VI*Z
   10 IPOW = IPOW - 1
C
      MOM(1) = MOM(1) + N
      UMOM(1) = UMOM(1) + U
      UMOM(NU+1) = UMOM(NU+1) + V
      RETURN
      END
C
C**************************************************************
      SUBROUTINE LSPFIT(NPOW,COEF,MOM,UMOM,A,EPS,IER)
      REAL*8 COEF(1),MOM(1),UMOM(1),A(1)
C
C          GENERATE A-ARRAY FROM MOMENTS
      NU = (NPOW+1)*(NPOW+2)/2
      CALL MVE(8,NU,MOM,A,1,1)
      II = NU
      KX = 1
C
C
      DO 20 K=1,NPOW
      KP1 = K + 1
      KPOW = K
      LPOW = 0
      DO 20 L=1,KP1
      KX = KX + 1
      II = II + 1
      A(II) = MOM(KX)
      IBEG = II
C
      DO 10 I=1,NPOW
      IP1 = I + 1
      IPOW = I
      JPOW = 0
      DO 10 J=1,IP1
      IX = IPOW + KPOW
      IY = JPOW + LPOW
      IX = IX + IY
      IX = (IX*(IX+1))/2 + 1 + IY
      II = II + 1
      A(II) = MOM(IX)
      IPOW = IPOW - 1
   10 JPOW = JPOW + 1
C
      KPOW = KPOW - 1
   20 LPOW = LPOW + 1
C
      CALL MVE(8,2*NU,UMOM,COEF,1,1)
      CALL DGELG(COEF,A,NU,2,EPS,IER)
      RETURN
      END
C
C**************************************************************
      SUBROUTINE CLFIT(IFIT,COEF,MOM,UMOM,IER)
      IMPLICIT REAL*8 (A-F,R-Z)
      REAL*8 COEF(6),MOM(6),UMOM(6),N
      REAL*8 EPS
      INTEGER ROTATE,SCALE,OFFSET
      DATA EPS/1.D-15/
C
C LEAST SQUARE LINEAR FIT ROUTINE
C FINDS COEFFICIENTS A,B,E,F SUCH THAT
C          U = A*X + B*Y + E
C          V =-B*X + A*Y + F
C
C         IFIT  ROTATE SCALE  OFFSET
C          1      0      0      1
C          2      0      1      0
C          3      0      1      1
C          4      1      0      0
C          5      1      0      1
C          6      1      1      0
C          7      1      1      1
      IER = -1
      IF(IFIT.LT.0.OR.IFIT.GT.7) RETURN
      N = MOM(1)
      IF(N.LE..9) RETURN
      X = MOM(2)
      Y = MOM(3)
      X2 = MOM(4)
      Y2 = MOM(6)
      U = UMOM(1)
      XU = UMOM(2)
      YU = UMOM(3)
      V = UMOM(4)
      XV = UMOM(5)
      YV = UMOM(6)
C          INITIALIZE WITH IDENTITY TRANSORMATION
      A = 1.D0
      B = 0.D0
      E = 0.D0
      F = 0.D0
      OFFSET = MOD(IFIT,2)
      SCALE = MOD(IFIT/2,2)
      ROTATE = IFIT/4
      R1 = XU + YV
      R2 = YU - XV
      R = X2 + Y2
      IF(OFFSET.EQ.0) GOTO 5
      R1 = N*R1 - (X*U+Y*V)
      R2 = N*R2 - (Y*U-X*V)
      R = N*R - (X*X+Y*Y)
    5 IF(SCALE.EQ.0) R=DSQRT(R1*R1+R2*R2)
      IF(ROTATE+SCALE.EQ.0) GOTO 10
      IF(DABS(R).LE.EPS) RETURN
      A = R1/R
      IF(ROTATE.EQ.1) B=R2/R
   10 IF(OFFSET.EQ.0) GOTO 15
      E = (U - A*X - B*Y)/N
      F = (V + B*X - A*Y)/N
   15 COEF(1) = E
      COEF(2) = A
      COEF(3) = B
      COEF(4) = F
      COEF(5) = -B
      COEF(6) = A
      IER = 0
      RETURN
      END
C
C**************************************************************
      SUBROUTINE RMSFIT(NPOW,PTS,N,COEF,RMS,DMAX,IMAX,MODE)
      REAL*4 PTS(4,1)
      REAL*8 DRMS,COEF(1)
C
      DRMS = 0.D0
      DMAX = 0.D0
      IMAX = 0
C
C
      DO 20 K=1,N
      X = PTS(2,K)
      Y = PTS(1,K)
      CALL PTRAN(NPOW,COEF,X,Y,U,V)
      DU = U - PTS(4,K)
      DV = V - PTS(3,K)
      DU = DU*DU + DV*DV
      IF(DMAX.GE.DU) GOTO 18
      DMAX = DU
      IMAX = K
   18 DRMS = DRMS + DU
      IF(MODE.NE.1) GOTO 20
      PTS(4,K) = U
      PTS(3,K) = V
   20 CONTINUE
C
      DMAX = SQRT(DMAX)
      RMS = DSQRT(DRMS/N)
      RETURN
      END
C
C**************************************************************
      SUBROUTINE PTRAN(NPOW,COEF,X0,Y0,U0,V0)
      REAL*8 COEF(1),X,Y,U,V,Z
C
      NU = (NPOW+1)*(NPOW+2)/2
      X = X0
      Y = Y0
      U = COEF(1)
      V = COEF(NU+1)
      L = 1
C
      DO 10 I=1,NPOW
      IP1 = I + 1
      IPOW = I
      Z = X**I
      DO 10 J=1,IP1
      L = L + 1
      IF(J.EQ.1) GOTO 8
      IF(J.EQ.IP1) GOTO 7
      Z = X**IPOW*Y**(I-IPOW)
      GOTO 8
    7 Z = Y**I
    8 U = U + COEF(L)*Z
      V = V + COEF(NU+L)*Z
   10 IPOW = IPOW - 1
C
      U0 = U
      V0 = V
      RETURN
      END
C
C**************************************************************
	SUBROUTINE SFIX(DCODE,NS,R,IBUF,SCALE,OFFSET)
C-----THIS ROUTINE WAS WRITTEN ESSENTIALLY FROM THE COMMENTS
C-----IN GARY'S ASSEMBLER CODE.
	REAL*4 R(NS)
	BYTE   IBUF(NS)
	INTEGER LOW,HIGH
	DATA LOW/0/,HIGH/255/
C
	DO 10 I=1,NS
	K = SCALE * R(I) + OFFSET
	IF(K .LT. LOW) K = LOW
	IF(K .GT. HIGH) K = HIGH
10	CALL MVE(-5,1,K,IBUF(I),1,1)
C
	RETURN
	END
C
C**************************************************************
	SUBROUTINE MOMGEN(BUF,SUM,MOM,J,NS)
C-----THIS ROUTINE WAS WRITTEN FROM THE COMMENTS IN GARY'S 
C-----ASSEMBLER CODE.
	INTEGER*2 BUF(NS),SUM(NS)
	INTEGER*4 MOM(NS)
C
	DO 16 I=1,NS
	SUM(I) = SUM(I) + BUF(I)
	MOM(I) = MOM(I) + BUF(I)*J
16	CONTINUE
C
	RETURN
	END
C
C**************************************************************
	SUBROUTINE UPMOMV(BI,BJ,JSUM,JMOM,NS,NLW)
C-----THIS ROUTINE HAS BEEN WRITTEN FROM THE COMMENTS
C-----IN GARY'S ASSEMBLER CODE.
	INTEGER*2 BI(NS),BJ(NS),JSUM(NS)
	INTEGER*4 JMOM(NS)
C
	DO 10 I=1,NS
	IB = BI(I)
	ISUM = JSUM(I)
	JSUM(I) = JSUM(I) + BI(I) - BJ(I)	
	JMOM(I) = JMOM(I) + BI(I)*NLW - ISUM
10	CONTINUE
C
	RETURN
	END
C
C**************************************************************
	SUBROUTINE MAXR( RBUF, NX, NY, B, JY, NBX, THRE, IDTHRE, NL,
     &	 RMAX, KK, JJ)

C  86-2-20 ...LWK... REVISED TO CHECK DN'S IF MORE THAN 1 PEAK
C                    ABOVE THRESHOLD.

C-----THIS ROUTINE WILL FIND THE MAXIMUM VALUE (RMAX) IN A
C-----R*4 BUFFER (RBUF) OF N ELEMENTS.  IT WILL RETURN 
C-----THAT VALUE AND ITS INDEX.
C-----THIS SUBROUTIONE WAS WRITTEN FROM GARY'S
C-----COMMENTS IN THE ASSEMBLER CODE.

	REAL*4 RBUF( NX, NY)
	INTEGER*2 B( NBX, 1)

	RMAX = RBUF( 1, 1)	!CORRELATION VALUE PEAK
	RMAX1 = -1		!PEAK ABOVE 'THRE' WITH DN < IDTHRE
	IF (RMAX.GT.THRE .AND. B(1,JY).LT.IDTHRE) RMAX1 = RMAX
	JJ = 1
	KK = 1
	JJ1 = 1
	KK1 = 1
	L = JY
C
	DO J = 1,NY
	  DO K = 1,NX
	    IF (RBUF(K,J) .GT. RMAX) THEN
	      RMAX = RBUF(K,J)
	      JJ = J
	      KK = K
	    ENDIF
	    IF (RBUF(K,J).GT.RMAX1 .AND. RBUF(K,J).GT.THRE
     &	     .AND. B(K,L).LT.IDTHRE) THEN
	      RMAX1 = RBUF(K,J)
	      JJ1 = J
	      KK1 = K
	    ENDIF
	  ENDDO
	  L = MOD(L,NL)+1
	ENDDO

C  REPLACE RMAX IF ANOTHER PEAK HAS DN < IDTHRE AND IS NOT ADJACENT:
	IF (RMAX1.GT.0. .AND. (IABS(JJ-JJ1).GT.1 .OR.
     &	 IABS(KK-KK1).GT.1)) THEN
	  RMAX = RMAX1
	  JJ = JJ1
	  KK = KK1
	ENDIF
C
	RETURN
	END
C
C**************************************************************
      SUBROUTINE OPCON(NLW,NSW)
C ROUTINE TO INITIALIZE CONVOLD
	IMPLICIT REAL*8 (D-F)
	COMMON/CP/IBUG
	COMMON/CONV/D1,D2,D3,D4,FNLWP1,FNLWM1,FNSWP1,FNSWM1,SSUMA2
        REAL*8 SSUMA2
C
      AREA = NLW*NSW
      D1 = 6./(AREA*(NSW*NSW-1))
      D2 = 6./(AREA*(NLW*NLW-1))
      D3 = -1./(AREA*(NSW-1)*(NLW-1))
      D4 = 7.*AREA - (NLW+NSW+5)
      FNLWP1 = NLW + 1
      FNSWP1 = NSW + 1
      FNLWM1 = NLW - 1
      FNSWM1 = NSW - 1
C
      RETURN
      END
C
C**************************************************************
      SUBROUTINE DTREND(A,AP,NLW,NSW)
C ROUTINE TO DETREND AREA A
	IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/CONV/D1,D2,D3,D4,FNLWP1,FNLWM1,FNSWP1,FNSWM1,SSUMA2
	INTEGER*2 A(NSW,NLW)
	REAL*4 AP(NSW,NLW)
C
      SSUM = 0.D0
      ZI = 0.D0
      ZJ = 0.D0
C
      DO 16 J=1,NLW
	N = 0
	MOM = 0
	DO 10 I=1,NSW
	N = N + A(I,J)
10   	MOM = MOM + I * A(I,J)
      SSUM = SSUM + N
      ZI = ZI + MOM
   16 ZJ = ZJ + J * N
C
      C1 = D1*(2.*ZI - FNSWP1*SSUM)
      C2 = D2*(2.*ZJ - FNLWP1*SSUM)
      C3 = D3*(6.*(FNLWM1*ZI+FNSWM1*ZJ) - D4*SSUM)
      SSUMA2 = 0.D0
C
C          AP(I,J) = A(I,J) - (C1*I+C2*J+C3)
      DO 17 J=1,NLW
      C3 = C3 + C2
      C4 = C3
      DO 17 I=1,NSW
      C4 = C4 + C1
      C5 = A(I,J) - C4
      AP(I,J) = C5
   17 SSUMA2 = SSUMA2 + C5*C5
C
      RETURN
      END
C
C**************************************************************
      SUBROUTINE CONVLD(AD,B,NLW,NSW,JSUM,JMOM,BK,R,NHOR,NSIN,NLIC)
C ROUTINE TO DETREND AREA B AND CONVOLVE IT WITH AREA A (ALREADY DETREND
	IMPLICIT REAL*8 (A-H,O-Z)
      COMMON/CONV/D1,D2,D3,D4,FNLWP1,FNLWM1,FNSWP1,FNSWM1,SSUMA2
	REAL*4 AD(NSW,NLW),BP,R(NHOR)
	INTEGER*2 JSUM(NSIN),B(NSIN,1)
	INTEGER*4 JMOM(NSIN),BK
      SSUM = 0.D0
      ZI = 0.D0
      ZJ = 0.D0
C
      DO 20 I=1,NSW
      SSUM = SSUM + JSUM(I)
      ZI = ZI + I*JSUM(I)
   20 ZJ = ZJ + JMOM(I)
C
C
C
      DO 30 II=1,NHOR
C          DETREND RIGHT AREA
      C1 = D1*(2.*ZI - FNSWP1*SSUM)
      C2 = D2*(2.*ZJ - FNLWP1*SSUM)
      C3 = D3*(6.*(FNLWM1*ZI+FNSWM1*ZJ) - D4*SSUM)
      SSUMAB = 0.D0
      SSUMB2 = 0.D0
      LL = BK
C           BP = B(I,J) - (C1*I+C2*J+C3)
      DO 25 J=1,NLW
      C3 = C3 + C2
      C4 = C3
      DO 24 I=1,NSW
      C4 = C4 + C1
      BP = B(II+I-1,LL) - C4
      SSUMAB = SSUMAB + AD(I,J)*BP
   24 SSUMB2 = SSUMB2 + BP*BP
   25 LL = MOD(LL,NLIC) + 1
C
      IF(SSUMB2.GT.1.D-10.AND.SSUMAB.GT.0.D0) R(II) = (SSUMAB*SSUMAB)/
     &                                                (SSUMA2*SSUMB2)
      ZI = ZI + NSW*JSUM(II+NSW) - SSUM
      ZJ = ZJ + JMOM(II+NSW) - JMOM(II)
   30 SSUM = SSUM + JSUM(II+NSW) - JSUM(II)
C
      RETURN
      END
C
C**************************************************************
	SUBROUTINE SORTX(BUF,N)
C-----THIS ROUTINE WILL SWAP THE HALFWORDS OF THE FULLWORD BUFFER
C-----SO THAT VAX WILL SORT LIKE THE IBM.
	INTEGER*2 BUF(2,N),J
C
        CALL TESTOS(IOS)
        IF (IOS .EQ. 0) THEN ! IF VMS 
	DO 100 I=1,N
	J = BUF(1,I)
	BUF(1,I) = BUF(2,I)
	BUF(2,I) = J
100	CONTINUE
        END IF
C
	CALL SORTIN(BUF,N)
C
        IF (IOS .EQ. 0) THEN
	DO 200 I=1,N
	J = BUF(1,I)
	BUF(1,I) = BUF(2,I)
	BUF(2,I) = J
200	CONTINUE
        END IF
C
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create resloc.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM resloc

   To Create the build file give the command:

		$ vimake resloc			(VMS)
   or
		% vimake resloc			(Unix)


************************************************************************/


#define PROGRAM	resloc
#define R2LIB

#define MODULE_LIST resloc.f 

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
#define DEBUG
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create resloc.pdf
process help=*
PARM INP     TYPE=STRING  COUNT=1:2
PARM OUT     TYPE=STRING  COUNT=1:2
PARM CAMERA  TYPE=INTEGER COUNT=0:1 DEFAULT=-- VALID=4:7 
PARM DBUG    TYPE=KEYWORD COUNT=0:1 DEFAULT=-- VALID=DBUG
PARM PRINT   TYPE=KEYWORD COUNT=0:1 DEFAULT=-- VALID=PRINT
PARM NOIN    TYPE=KEYWORD COUNT=0:1 DEFAULT=-- VALID=NOIN
PARM NVER    TYPE=INTEGER                DEFAULT=19
PARM NHOR    TYPE=INTEGER                DEFAULT=19
PARM NLW     TYPE=INTEGER                DEFAULT=5
PARM NSW     TYPE=INTEGER                DEFAULT=5
PARM SIGMA   TYPE=REAL                   DEFAULT=1.
PARM FRAME   TYPE=INTEGER COUNT=0:1      DEFAULT=--
PARM REDO    TYPE=KEYWORD COUNT=0:1      DEFAULT=-- VALID=REDO
PARM NOMS    TYPE=KEYWORD COUNT=0:1      DEFAULT=-- VALID=NOMS
PARM CTHRESH TYPE=REAL                   DEFAULT=.5
PARM DNTHRESH TYPE=INTEGER               DEFAULT=10
PARM FIT     TYPE=INTEGER VALID=1:8      DEFAULT=8
PARM TOLER   TYPE=REAL                   DEFAULT=4.
END-PROC
.TITLE
 "resloc" -- Locates Voyager image reseau marks.
.HELP
 PURPOSE:

 "RESLOC" will locate the reseaux on pictures recorded by Voyager
 cameras.  It will also output those locations as a solitary file.
 of "geoma" parameters.

 EXECUTION:

   The following is the execution statement for "resloc":

	resloc INP=(PIC,[RES]) OUT=(RES,[GEOM]) PARAMS

 where PIX is an input image, RES is a reseau location file, and
 PARAMS are other user parameters.  Items in square brackets []
 are optional.   

 Further details are given in the respective parameter sections in
 TUTOR mode.
.page
 OPERATION:

 If the VGR reseau location master file is included as a secondary
 input, "RESLOC" will first check it to see if there is an entry for
 the frame PIC.  If an entry already exists, the reseau locations
 are retrieved from the VGR reseau location master file and output
 to RES.  If no entry exists, or if the keyword REDO is specified,
 then the reseau will be located on the input picture PIC, and stored
 in the VGR reseau location master file. The location of the reseau
 requires a set of nominal locations to be used as starting values.
 If the VGR reseau location master file is not present, these nominals
 are retrieved from tables built into the program. In general, it is
 desirable to use the nominals in the VGR reseau location master file
 because these will be more current and accurate. Optionally, the
 reseau master file may be replaced.
.page
 "RESLOC" will search for each reseau mark in an NHOR by NVER area centered
 about its nominal location. At present, marks near the boundaries of the
 picture are not located.  Coordinates for these marks are calculated by
 interpolating over its nearest neighbors. Each mark is found by using an
 NLW by NSW correlation window.  As this window scans over the search area,
 the underlying area is compared with a reseau shape function (constant for
 all the marks) of the form:
   	f(x,y) = 255(1 - exp(-(x**2 + y**2)/2s**2)
 For a 9 by 9 window, the shape function is:
	254 254 254 254 254 254 254 254 254 254
	254 254 254 253 252 253 254 254 254 254
	254 254 250 234 220 234 250 254 254 254
	254 253 234 161 100 161 234 253 254 254
	254 252 220 100   0 100 220 252 254 254
	254 253 234 161 100 161 234 253 254 254
	254 254 250 234 220 234 250 254 254 254
	254 254 254 253 252 253 254 254 254 254
	254 254 254 254 254 254 254 254 254 254
.page
 Let A(i,j) represent the shape function, and B(i,j) be the area of
 the picture being compared.  Let uA, uB, tA, tB be the corresponding
 means and standard deviations.  The correlation function is a normalized
 variance of the form:
     p = (SIGMA(Aij-uA)(Bij-uB))/(N*tA*tB)
 where N = NLW*NSW and the summation is performed
 over an NLW by NSW window.

 If a correlation fails to give a valid result for some point, then
 the reseau location is set to the nominal location, plus an offset
 computed as an average of the offsets of a set of nearby locations.
 In a case where a large area of the image contains invalid data, and
 therefore no correlations can be found, reseau locations will simply be
 set equal to the nominals, and a warning message is issued from
 subroutine FILLOC.
.page
 EXAMPLES:
	
	resloc INP=(A,VGR:RESFIL.TST) OUT=RES
	ressar77 INP=(A,RES) OUT=B
 "RESLOC" is used to store the reseaux in the frame stored in data set A.
 The resulting locations are input to "ressar77" to remove the marks.

	resloc INP=(A,VGR:RESFIL.TST) OUT= (RES,GEOM)
	geoma INP=(A,GEOM) OUT=C SIZE=(1,1,1000,1000)
 "RESLOC" is used to generate parameters for "geoma". "geoma" is then executed
 to geometrically correct the frame stored in data set A.  Note the
 current object space (henceforth refered to as funny space) is 1000
 by 1000.  Since funny space is not the officially approved object space,
 all pictures transformed to funny space should be properly labeled as
 such, and the result derived from such a space should be marked as being
 of dubious value. (See Gary Yagi for questions regarding funny space.)
.page
 RESTRICTIONS:
   The constants NLW, NSW, NHOR, NVER must satisfy
   the following:
     1. Each must be odd;
     2. 2*NHOR*(NVER+2) + NLW*(NSW + 4) + 800*(NLW+NVER) <= 58086
        If NLW = NSW and NHOR = NVER, then the following
        simplified formula may be used:
                NLW + NVER <= 42
.page
 TIMING:
 The execution time of "resloc" is directly proportional to the window and
 search dimensions NLW, NSW, NHOR, NVER.  For the default values,
 the program takes 1 min. 20 sec. per frame on an IBM360, and 2 min. 30 sec.
 per frame on a VAX 11/780. 
.PAGE
HISTORY

 WRITTEN BY: 	GARY YAGI		23 JULY 1977

 CONVERTED TO VAX BY:  C. C. AVIS	27 MAY 1983

 REVISIONS:

 16-NOV-85 - L.W.KAMP - CONVERTED I/O TO VICAR2
 24-JAN-86 - L.W.KAMP - REPLACED LOCK MANAGEMENT CODE WITH VICAR2
                         OPEN STATUS CHECK + CALL WAIT.
 26-FEB-86 - L.W.KAMP - MODIFIED SUBR.MAXR TO CHECK DN<DNTHRESH (NEW PARM)
 20-JUN-86 - L.W.KAMP - REVISED SUBR.FILLOC TO FILL IN ALL LOCATIONS
 27-JUN-86 - F.F.MOSS - CONVERTED PARAM I/O TO VICAR2
 10-JUL-95 - A.SCOP   - (CRI) Made portable for UNIX
 27-MAR-96 - B.A.McGuffie   - modified to accept new IBIS reseau location
                              files

COGNIZANT PROGRAMMER: L. W, Kamp
.page
For purposes of testing, a small sample master reseau location file
named RESFIL.TST is in the current MIPL test directory.  Please check
with Integration and Test to determine where this directory is located.
This file should not be randomly modified: copy it to a scratch directory 
before tampering with it.
.LEVEL1
.VARIABLE INP
 1 image file and either a
 solitary nominal reseau
 location file, or the
 Voyager reseau location
 master file.
.VARIABLE OUT
 1 solitary reseau location
 file and one GEOMA
 parameter file (opt.).
.VARIABLE CAMERA
 integer - camera serial number
 override
.VARIABLE DBUG
 Keyword - Valid value = DBUG.
 Causes diagnostics to be
 printed.
.VARIABLE PRINT
 Keyword - Valid value = PRINT
 Generates listing of reseaus
 and GEOMA parameters.
.VARIABLE NOIN
 Keyword - Valid value =NOIN
 Suppresses the interpolation
 routine
.VARIABLE NVER
 integer - height of the search
 area
.VARIABLE NHOR
 integer - width of the search
 area
.VARIABLE NLW
 integer - height of correlation
 area   
.VARIABLE NSW
 integer - width of the
 correlation area
.VARIABLE SIGMA
 real - standard deviation
 constant for reseau shape
 function.
.VARIABLE FRAME
 integer - frame number override
.VARIABLE REDO
 Keyword - Valid value = REDO
 Ignore entry in the VGR reseau
 location master file
.VARIABLE NOMS
 Keyword - Valid value = NOMS
 Use nominals file instead of
 VGR reseau location master file.
.VARIABLE CTHRESH
 REAL - Optional -CORRELATION THRESHOLD
.VARI DNTHRESH
 INTEGER - optional - DN threshold
 in candidate reseau centers
.VARIABLE FIT
 INTEGER - Optional-SELECT FIT TYPE
.VARIABLE TOLER
 REAL - Optional - MAX LOCATION ERROR
.LEVEL2
.VARIABLE INP
 One picture file generated by one of the Voyager
 logging program.  This frame should be an
 unprocessed  byte image (i.e. No stretching, filtering
 or geom allowed).  Followed by either:
   1. the Voyager reseau location master file;
   2. a solitary reseau location file of nominal reseau locations;
   3. neither.

 "resloc" checks the file type of the second input file if it 
 is present.  If a solitary nominal reseau location file
 is used in "resloc" as the second input file, this code may type a message
 'INVALID "resloc" FILE'. This error message should be ignored.
.VARIABLE OUT
 A  solitary reseau file containing the coordinates (LINE,SAMPLE)
 for each reseau mark in a format suitable for input to the programs
 "res77" and "ressar77". (RES is of size 1616 bytes by 1 line.)

 Optionally, an additional file of size 3600 bytes by 3 lines
 containing GEOMA parameters. 

 Both outputs have VICAR labels.
.VARIABLE CAMERA
 CAMERA specifies the camera serial number for the
 input picture.  This parameter may be used to
 override the camera information in the Voyager
 label.
.VARIABLE DBUG
 Causes diagnostic messages to be printed.
 (Default is no messages printed.)
.VARIABLE PRINT
 Causes a listing of the reseau locations and GEOMA
 parameters (if present) to be generated.
 (Default is that no such listing is printed.)
.VARIABLE NOIN
 The program will normally attempt to calculate
 the reseau locations to sub-pixel accuracy.  This
 keyword suppresses the interpolation routine. 
.VARIABLE NVER
 NVER is an integer specifying the height of the
 search area. (must be odd) (Default =19)
.VARIABLE NHOR
 NHOR  is an integer specifying the width of the
 search area. (must be odd) (Default = 19)
.VARIABLE NLW
 NLW is an integer specifying the height of the
 correlation area. (must be odd) (Default = 5)
.VARIABLE NSW
 NSW is an integer specifying the width of the
 correlation area. (must be odd) (Default = 5)
.VARIABLE SIGMA
 SIGMA is a real number specifying the standard
 deviation constant used in the reseau shape function.
 (Default = 1.0)
.VARIABLE FRAME
 FRAME  is an integer specifying the frame number for
 the input picture.  This parameter will override
 the frame number in the Voyager label.
.VARIABLE REDO
 REDO  specifies the location of the reseau is to
 be redone, i.e., any prior entry for the frame
 in the VGR reseau location master file will be overwritten.
.VARIABLE NOMS
 NOMS indicates that nominals are being used instead of the
 VGR master reseau location file.
.VARIABLE CTHRESH
 A THRESHOLD ON CORRELATION VALUES.
 Default is CTHRESH=0.5.
.vari DNTHRESH
 DNTHRESH is a threshold DN value that is used only in discriminating
 between correlation peaks in a search area:  if several peaks exceed
 CTHRESH, then the peak with DN < DNTHRESH is selected.

 If none of the DN values are < DNTHRESH, or if several peaks have DN
 values < DNTHRESH, then the highest correlation peak is used.
 The default for DNTHRESH is 10.
.VARIABLE FIT
 VALID = 1:8
 Default is FIT = 8 (unconstrained linear fit).
 FIT specifies the type of fit to be performed.  Meanings are :

         IFIT  ROTATE SCALE  OFFSET
          1      0      0      1
          2      0      1      0
          3      0      1      1
          4      1      0      0
          5      1      0      1
          6      1      1      0
          7      1      1      1
.VARIABLE TOLER
 Default is TOLER = 4.0.
 MAXIMUM ALLOWED ERROR IN CALCULATED LOCATION RELATIVE TO
 THE NOMINAL LOCATION.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstresloc.pdf
procedure
! TO RUN ON VMS, TYPE tstresloc
! TO RUN ON AXP or UNIX MACHINE, MOVE THE TEST FILES TO THE MACHINE FROM THE
! VAX IF NOT AVAILABLE ON THAT MACHINE, AND TYPE
!   tstresloc DIR=dirname
!     where dirname = pathname of directory containing file with trailing / OR
!                   = "" if in current directory.
refgbl $echo
refgbl $autousage
refgbl $syschar
body
local PATH string init="WMS_TEST_WORK:[TESTDATA.MIPL.VGR]"
local DIR  string init="WMS_TEST_WORK:[TESTDATA.VGR]"
LOCAL INPIC1 TYPE=STRING
LOCAL INPIC2 TYPE=STRING
let $autousage="none"
let _onfail="continue"
let $echo="no"
if ($syschar(1) = "UNIX")
  let PATH="/project/test_work/testdata/mipl/vgr/"
  let DIR="/project/test_work/testdata/vgr/"
  ush cp /project/test_work/testdata/mipl/vgr/reseau.test reseau.test
end-if
if ($syschar(1) = "VAX_VMS")
  dcl copy WMS_TEST_WORK:[TESTDATA.MIPL.VGR]reseau.test reseau.test
end-if
let INPIC1 = "f1636832.raw"
let INPIC2 = "resfil.tst"
write "THIS IS A TEST OF MODULE RESLOC"
write "MAKE A TEST COPY OF the Reseau File"
!ibis-copy &"PATH"reseau.test tf
write "List contents of original input file"
label-list reseau.test
ibis-list reseau.test nr=7 nc=9 cols=(1,2,3,4,5,6,7,8,9) formats=formats +
intcols=(1,2,3,4,5) sr=19
write "DELETE the data for frame 1636832"
rowop reseau.test out1 keycol=1 range=(1636832,1636832) mode=delete
write "List contents of file after delete"
ibis-list out1 nr=7 nc=9 cols=(1,2,3,4,5,6,7,8,9) formats=formats +
intcols=(1,2,3,4,5) sr=19
write "Add 1636832 to file out1 and file r"
resloc (&"PATH"&INPIC1,out1) (R,G) 
write "List contents of file after add"
label-list out1
ibis-list out1 nr=8 nc=9 cols=(1,2,3,4,5,6,7,8,9) formats=formats +
intcols=(1,2,3,4,5) sr=19
write "List contents of file r - only one row"
label-list R
ibis-list R nr=1 nc=9 cols=(1,2,3,4,5,6,7,8,9) formats=formats +
intcols=(1,2,3,4,5)
write "List contents of GEOMA file"
label-list G
ibis-list G nr=10 nc=4 cols=(1,2,3,4) 
write "HOW ABOUT IF WE JUST WANT TO EXTRACT WHAT IS IN THE FILE?"
resloc (&"PATH"&INPIC1,reseau.test) R1 'print
write "THE NEXT RUN WILL TEST THE REST OF THE KEYWORDS."
write "IT WILL STORE THE FRAME IN THE FILE AS FRAME 1."
!ibis-copy &"PATH"reseau.test reseau.test
if ($syschar(1) = "UNIX")
  ush cp /project/test_work/testdata/mipl/vgr/reseau.test reseau.test
end-if
if ($syschar(1) = "VAX_VMS")
  dcl copy WMS_TEST_WORK:[TESTDATA.MIPL.VGR]reseau.test reseau.test
end-if

resloc (&"PATH"&INPIC1,reseau.test) R3 sigma=1.2 fit=7 tole=5. frame=1 'noin +
cthr=0.6 nver=21 nhor=21 nlw=7 nsw=7
write "WHAT IS IN THE FILE NOW?"
ibis-list reseau.test nr=26 nc=9 cols=(1,2,3,4,5,6,7,8,9) formats=formats +
intcols=(1,2,3,4,5)
ibis-list R3 nr=1 nc=9 cols=(1,2,3,4,5,6,7,8,9) formats=formats +
intcols=(1,2,3,4,5)
write "WE DON'T HAVE TO GIVE RESLOC ANYTHING BUT THE IMAGE."
write "THE RESULTS SHOULD BE THE SAME."
resloc &"PATH"&INPIC1 R4 'print

WRITE ""
WRITE "THE FOLLOWING TEST CASE WAS GIVEN WITH FR89337."
WRITE ""
resloc (&"DIR"1134710.cln,&"DIR"cresn.fil) (xr,1134710.geo)
ficor77 (&"DIR"1134710.cln,&"DIR"v2_wa_ora.calib,&"DIR"dc.wa_5to1) +
       ficor.dat scf=&"DIR"vgrscf.dat
geoma (ficor.dat,1134710.geo) +
      neptune.dat nl=1000  ns=1000 format="HALF"

end-proc
$ Return
$!#############################################################################
