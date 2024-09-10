$!****************************************************************************
$!
$! Build proc for MIPL module omcor2
$! VPACK Version 1.9, Friday, July 14, 2006, 13:54:34
$!
$! Execute by entering:		$ @omcor2
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
$ write sys$output "*** module omcor2 ***"
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
$ write sys$output "Invalid argument given to omcor2.com file -- ", primary
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
$   if F$SEARCH("omcor2.imake") .nes. ""
$   then
$      vimake omcor2
$      purge omcor2.bld
$   else
$      if F$SEARCH("omcor2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake omcor2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @omcor2.bld "STD"
$   else
$      @omcor2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create omcor2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack omcor2.com -mixed -
	-s omcor2.f -
	-i omcor2.imake -
	-p omcor2.pdf -
	-t tstomcor2.pdf tstomcor2.log_solos tstomcor2.log_linux
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create omcor2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C 10 JUL 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44

	INTEGER	maxpix,maxpts
	PARAMETER (maxpix=100,maxpts=768)

        real*8 angles(3,maxpix),om(9,maxpix),loc(3)
        real*8 p(maxpix*3+1,maxpix*3),y(maxpix*3+1)
        real*8 pr(maxpix*3),prr(maxpix*3),pbar(maxpix*3)
        real*8 ftol,delta_t

        real*4 scr(128),zvp
        real*4 radpol(maxpix),eqrad(maxpix),focal(maxpix)
        real*4 optaxl(maxpix),optaxs(maxpix)
        real*4 rsvec(3,maxpix),line,samp
        real*4 left_line(maxpts),left_samp(maxpts)
        real*4 right_line(maxpts),right_samp(maxpts)
        real*4 xleft_over(maxpts),xright_over(maxpts)
        real*4 conv(3600),omangl(maxpix,3)

        integer*4 unito,unit1,unit4,count,def,ground,status
        integer*4 left_over(maxpts),right_over(maxpts),camera
        integer*4 nzvp,times(6),ref_time(6),scet(maxpix,6)
        integer*4 iscr(128),inibiso,inibis1

        logical xvptst
 
        character*5 project

c       data used in the FUNK objective function.
        common/funkcom/om,loc,nsolutions,npts,npict,rsvec,radpol,
     +                 eqrad,focal,optaxl,optaxs,left_line,
     +                 left_samp,right_line,right_samp,left_over,
     +                 right_over
        common/zvpt/delta_t(maxpix)
        common/zvp/nzvp,zvp(2,1000)
	common/fit2/angle3(maxpix),mode

        call ifmessage('OMCOR2 version OCT-96 jjl')
        call xveaction('SA',' ')

        nzvp=0


c Read in the SEDR.INT file
        call xvunit(UNITO,'INP',1,STATUS,' ')
        if (STATUS.NE.1) call mabend('INPUT FILE INITIALIZATION ERROR')
        call ibis_file_open(UNITO,INIBISO,'UPDATE',0,0,' ',' ',STATUS)
        if (STATUS.NE.1) call ibis_signal_U(UNITO,STATUS,1)
        call ibis_file_get(INIBISO,'NR',NPICT,1,1)
        call ibis_file_get(INIBISO,'NC',NCOL1,1,1)
        call prnt(4,1,npict,'# images in SEDR file=.')
        if(ncol1.lt.23) then
           call xvmessage('First input file not a SEDR.INT file',' ')
           call prnt(4,1,ncol1,'Number of columns=.')
           call abend
        endif
        if(ncol1.lt.31) then
            call xvmessage('SEDR.INT file not created by IBISNAV',' ')
            call prnt(4,1,ncol1,'Number of columns=.')
            call abend
        endif
        if(npict.gt.maxpix)then
          call xvmessage('Too many pictures',' ')
          call prnt(4,1,npict,'# images input=.')
          call prnt(4,1,maxpix,'# images permitted=.')
          call abend
        endif
        do i=1,31
           call ibis_column_set(INIBISO,'FORMAT','REAL',i,STATUS)
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           call ibis_column_set(INIBISO,'U_FORMAT','REAL',i,STATUS)
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
        end do
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)

        call ibis_column_read(INIBISO,scr,4,1,npict,status) ! camera
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
        camera=nint(scr(1))
        do i=1,npict
          if(camera.ne.nint(scr(i)))then
             call xvmessage('Only one camera id permitted',' ')
             call abend
          endif
        enddo

        call ibis_column_read(INIBISO,scr,5,1,npict,status) ! rs vect 1
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
        do i=1,npict
          rsvec(1,i)=scr(i)
        enddo
        call ibis_column_read(INIBISO,scr,6,1,npict,status) ! rs vect 2
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
        do i=1,npict
          rsvec(2,i)=scr(i)
        enddo
        call ibis_column_read(INIBISO,scr,7,1,npict,status) ! rs vect 3
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
        do i=1,npict
          rsvec(3,i)=scr(i)
        enddo
        if(xvptst('OLD'))then
           call ibis_column_read(INIBISO,scr,8,1,npict,status)
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           call mve(7,npict,scr,omangl(1,1),1,1)
           call ibis_column_read(INIBISO,scr,9,1,npict,status)
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           call mve(7,npict,scr,omangl(1,2),1,1)
           call ibis_column_read(INIBISO,scr,10,1,npict,status)
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           call mve(7,npict,scr,omangl(1,3),1,1)
        else
           call ibis_column_read(INIBISO,scr,21,1,npict,status)
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           call mve(7,npict,scr,omangl(1,1),1,1)
           call ibis_column_read(INIBISO,scr,22,1,npict,status)
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           call mve(7,npict,scr,omangl(1,2),1,1)
           call ibis_column_read(INIBISO,scr,23,1,npict,status)
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           call mve(7,npict,scr,omangl(1,3),1,1)
        endif
        do i=1,npict
          angles(1,i)=omangl(i,1)
          angles(2,i)=omangl(i,2)
          angles(3,i)=omangl(i,3)
          call fromeuler(angles(1,i),angles(2,i),angles(3,i),
     +                   om(1,i))
c         save kappa into common block - ARV
          angle3(i)=angles(3,i)
        enddo
        call ibis_column_read(INIBISO,radpol,26,1,npict,status) ! radpol
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
        call ibis_column_read(INIBISO,eqrad,27,1,npict,status) ! eqrad
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
        call ibis_column_read(INIBISO,focal,28,1,npict,status) ! focal
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
        call ibis_column_read(INIBISO,optaxl,29,1,npict,status) ! optaxl
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
        call ibis_column_read(INIBISO,optaxs,30,1,npict,status) ! optaxs
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
        call ibis_column_read(INIBISO,scr,31,1,npict,status) ! scale
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
        do i=1,npict
           focal(i)=focal(i)*scr(i)
        enddo
c       get the spececraft event times
        if (xvptst('ZONAL')) then
           do i=32,37
              call ibis_column_set(INIBISO,'FORMAT','FULL',i,STATUS)
              if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
              call ibis_column_set(INIBISO,'U_FORMAT','FULL',i,STATUS)
              if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           end do
           call ibis_column_read(INIBISO,iscr,32,1,npict,status) ! scet year
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           call mve(4,npict,iscr,scet(1,1),1,1)
           call ibis_column_read(INIBISO,iscr,33,1,npict,status) ! scet day
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           call mve(4,npict,iscr,scet(1,2),1,1)
           call ibis_column_read(INIBISO,iscr,34,1,npict,status) ! scet hour
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           call mve(4,npict,iscr,scet(1,3),1,1)
           call ibis_column_read(INIBISO,iscr,35,1,npict,status) ! scet min
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           call mve(4,npict,iscr,scet(1,4),1,1)
           call ibis_column_read(INIBISO,iscr,36,1,npict,status) ! scet sec
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           call mve(4,npict,iscr,scet(1,5),1,1)
           call ibis_column_read(INIBISO,iscr,37,1,npict,status) ! scet milsec
           if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
           call mve(4,npict,iscr,scet(1,6),1,1)
        endif
        call prnt(4,1,camera,'Camera # in ibis file= .')

C		Get the parameters
        call xvparm('PROJECT',project,count,def,5)
        call xvparm('GROUND',ground,nground,def,1)
        call xvparm('CAMERA',camera,count,def,1)
        call xvparmd('TOLERANC',ftol,count,def,1)


c set ground control 
        if(nground.eq.0) ground=npict
        if(ground.gt.npict.or.ground.lt.1)then
           call xvmessage('Ground outside bounds',' ')
           ground=npict
        endif
        call prnt(4,1,ground,'GROUND= .')
        nsolutions=ground-1

c compute time difference from ground control reference for each frame
c so that zonal flow can be added to the tiepoints.
        if(xvptst('ZONAL')) then
          call xvmessage('reading zonal velocity profile file',' ')
          call get_zvp
c         reset ground control to last image.
          ground=npict
          nsolutions=ground-1
          do j=1,6
            ref_time(j)=scet(ground,j)
          enddo
          do i=1,nsolutions
            do j=1,6
              times(j)=scet(i,j)
            enddo
            call time_diff(ref_time,times,delta_t(i))
          enddo
          delta_t(ground)=0.d0
        endif

c read in tiepoints file
        call xvunit(UNIT1,'INP',2,STATUS,' ')
        if (STATUS.NE.1) call mabend('INPUT FILE INITIALIZATION ERROR')
        call ibis_file_open(UNIT1,INIBIS1,'READ',0,0,' ',' ',STATUS)
        if (STATUS.NE.1) call ibis_signal_U(UNIT1,STATUS,1)
        call ibis_file_get(INIBIS1,'NR',NPTS,1,1)
        call ibis_file_get(INIBIS1,'NC',NCOL1,1,1)
        if(ncol1.ne.12)then
          call xvmessage('Input #2 not a tiepoints file',' ')
          call prnt(4,1,ncol1,'# columns= .')
          call abend
        endif
        if(npts.gt.maxpts)then
          call xvmessage('Too many tiepoints',' ')
          call prnt(4,1,npts,'# points input=.')
          call prnt(4,1,maxpts,'# points permitted=.')
          call abend
        endif
        call ibis_column_read(INIBIS1,xleft_over,1,1,npts,status)
        if (STATUS.NE.1) call ibis_signal(INIBIS1,STATUS,1)
        call ibis_column_read(INIBIS1,xright_over,2,1,npts,status)
        if (STATUS.NE.1) call ibis_signal(INIBIS1,STATUS,1)
        call ibis_column_read(INIBIS1,left_line,3,1,npts,status)
        if (STATUS.NE.1) call ibis_signal(INIBIS1,STATUS,1)
        call ibis_column_read(INIBIS1,left_samp,4,1,npts,status)
        if (STATUS.NE.1) call ibis_signal(INIBIS1,STATUS,1)
        call ibis_column_read(INIBIS1,right_line,5,1,npts,status)
        if (STATUS.NE.1) call ibis_signal(INIBIS1,STATUS,1)
        call ibis_column_read(INIBIS1,right_samp,6,1,npts,status)
        if (STATUS.NE.1) call ibis_signal(INIBIS1,STATUS,1)
        do i=1,npts
          left_over(i)=nint(xleft_over(i))
          right_over(i)=nint(xright_over(i))
        enddo
        call prnt(4,1,npts,'# tiepoints located = .')

c read in the geoma correction file.
        call xvpcnt('INP',inp_count)
        if(.not.xvptst('OBJECT'))then
           if(inp_count.lt.3)then
              iflag=0
              if(project.ne.'GLL  ')call xvmessage('Using nominals',' ')
           else
              iflag=1
              call xvunit(unit4,'INP',inp_count,status,' ')
              call xvopen(unit4,status,' ')
           endif
           call getgeom(unit4,project,camera,iflag,conv,conv,nah,
     +                  nav,ind)
        endif


c convert tiepoints from image to object space
        if(xvptst('OBJECT'))then
           call xvmessage('Treating images as OBJECT space images',' ')
        else
           call xvmessage('Treating images as IMAGE space images',' ')
           call xvmessage('Converting coordinates to object space',' ')
           do i=1,npts
              line=left_line(i)
              samp=left_samp(i)
              call convisos(project,camera,line,samp,
     +         left_line(i),left_samp(i),1,conv(9),nah+1,nav+1,ind)
c              First 8 words of CONV have parameter names, etc.
              line=right_line(i)
              samp=right_samp(i)
              call convisos(project,camera,line,samp,
     +         right_line(i),right_samp(i),1,conv(9),nah+1,nav+1,ind)
           enddo
        endif         

c solve for the Euler angles
        call xvmessage('Beginning solution search',' ')
	if(xvptst('FIT2'))then
          mode=2
          call pointing2(nsolutions,angles,p,y,pr,prr,pbar,ftol,ind)
        else
          mode=1
          call pointing3(nsolutions,angles,p,y,pr,prr,pbar,ftol,ind)
        endif
        if(ind.ne.0)then
           call xvmessage('POINTING: no solution',' ')
           return
        endif

c Update input SEDR file
        do i=1,nsolutions
           omangl(i,1)=angles(1,i)
           omangl(i,2)=angles(2,i)
           omangl(i,3)=angles(3,i)
        enddo
        call mve(7,npict,omangl(1,1),scr,1,1)
        call ibis_column_write(INIBISO,scr,21,1,npict,status)
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
        call mve(7,npict,omangl(1,2),scr,1,1)
        call ibis_column_write(INIBISO,scr,22,1,npict,status)
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)
        call mve(7,npict,omangl(1,3),scr,1,1)
        call ibis_column_write(INIBISO,scr,23,1,npict,status)
        if (STATUS.NE.1) call ibis_signal(INIBISO,STATUS,1)

        return
        end

c ***************************************************************
C returns the zonal velocity profile in common array ZVP
      subroutine get_zvp
      COMMON/ZVP/NZ,ZVP(2,1000)
      REAL*4 ZVP
      CHARACTER*256 FNAME

      CALL XVPARM('ZVP',fname,icnt,idef,1)	!Get file name
      CALL XVUNIT(iunit,'Z',1,IND,'U_NAME',FNAME,' ')
      CALL XVSIGNAL(IUNIT,IND,.TRUE.)
      CALL XVOPEN(IUNIT,IND,' ')
      CALL XVGET(IUNIT,IND,'NL',nl,'NS',ns,' ')
      IF (NL.eq.1) then
        NZ = NS/2
        CALL XVREAD(IUNIT,zvp,ind,' ')
        CALL XVCLOSE(IUNIT,ind,' ')
      else
        CALL MABEND('***Invalid Zonal Velocity Profile file format')
      endif
      RETURN
      END

c *************************************************************
c TIME_DIFF returns the time difference in seconds for time1-time2.
c INPUT:
c time1(1-6) year, day, hour, minute, second, millisecond (input integer*4)
c time2(1-6) year, day, hour, minute, second, millisecond (input integer*4)
c delta      time1-time2 in seconds  (returned real*8).
      subroutine time_diff(time1,time2,delta)
      integer*4 time1(6),time2(6)
      real*8 t1,t2,delta
      t1=dble(time1(6))/1000.d0 + dble(time1(5)) +
     +   60.d0*(dble(time1(4)) + 60.d0*(dble(time1(3)) +
     +   24.d0*(dble(time1(2)) + 365.25d0*dble(time1(1))  )))
      t2=dble(time2(6))/1000.d0 + dble(time2(5)) +
     +   60.d0*(dble(time2(4)) + 60.d0*(dble(time2(3)) +
     +   24.d0*(dble(time2(2)) + 365.25d0*dble(time2(1))  )))
      delta=t1 - t2
      return
      end

c *************************************************************
C Given planetocentric latitude RLATdeg in degrees
c Given time interval delta in seconds
c Given planet radii r_pole, r_equator in km.
c compute zonal velocity U from zonal velocity profile in meters/sec.
c compute and distance travelled in longitude in meters.
c compute the distance travelled in longitude in degrees.
C
      SUBROUTINE ZONAL_movement(RLATdeg,delta,r_pole,r_equator,u,
     +                 travel_m,travel_deg)
      COMMON/ZVP/NZ,ZVP(2,1000)
      real*8 delta
      REAL*4 ZVP,pi

      pi = acos(-1.0)
      IF (RLATdeg.EQ.-999.0) GOTO 990

c     convert geocentric latitude to radians
      rlatgc=RLATdeg*pi/180.

c     convert to geodetic latitude in radians.
      if(r_pole.eq.r_equator)then
         rlat=rlatgc
      else
        if(RLATdeg.lt.-89.999)then
           rlat=-89.99*pi/180.
        else if(RLATdeg.gt.89.999)then
           rlat=89.99*pi/180.
        else
           rlat=sngl(datan( ((dble(r_equator))/(dble(r_pole)))**2 *
     +                dble(tan(rlatgc) )))
        endif
      endif

      I = NZ/2
C     ....Search so that  ZVP(i) < RLAT < ZVP(i+1)
   10 IF (RLAT.GE.ZVP(1,I).AND.RLAT.LE.ZVP(1,I+1)) GOTO 20
      I0 = I
      I = I + (RLAT-ZVP(1,I))/(ZVP(1,I+1)-ZVP(1,I))
      IF (I.LT.1) I=1
      IF (I.GE.NZ-1) I=NZ-2
      IF (I.EQ.I0) GOTO 990
      GOTO 10
C     ....Interpolate between points
   20 U = ZVP(2,I) + (ZVP(2,I+1)-ZVP(2,I))*(RLAT-ZVP(1,I))/
     &		(ZVP(1,I+1)-ZVP(1,I))
      travel_m = u*sngl(delta)
      if(r_pole.eq.r_equator)then
        r=r_pole
      else
        r=sngl(dble(r_pole)*dble(r_equator)/dsqrt(
     +  (dble(r_pole))**2 *(dcos(dble(rlatgc)))**2   +
     +  (dble(r_equator))**2 *(dsin(dble(rlatgc)))**2   ))
      endif
      travel_deg=(travel_m * 180.)/(r * 1000. * cos(rlatgc) * pi)
      RETURN

C     ....All latitudes outside range of table are set to zero
  990 U = 0.0
      travel_m=0.0
      travel_deg=0.0
      RETURN
      END


c ***********************************************************
      subroutine myippcov (error, line, samp, lat, lon, om, rs, focal,
     &                     eqradius, polradius, opticaxline,
     &                     opticaxsamp, loc)
c
c        function -
c
c          calculates latitude and longitude for specified data
c          point on an inverse perspective projection.
c	   the data point (line,sample) is assumed to be in object space.
c	   uses oblate spheroid model for planet.
c	   all arithmetic is done in double precision.
c
c        parameters -
c
c	   error = 0 if no error,   1 if point off planet
c          line = line value of input point  r*4
c          samp = sample value of input point r*4
c          lat = calculated latitude (degrees)  r*8   output
c          lon = calculated west longitude (degrees)  r*8   output
c          om = rotation matrix planet to camera - om matrix  r*8
c          rs = position of spacecraft in planet coordinates - rs vector  r*4
c          focal = focal length of camera in object space pixels  r*4
c          sign = +- 1 used for off planet errors
c          loc = output point in planet coords  r*8
c          
      integer error
      integer i,sign

      real*4 line, samp
      real*8 loc(3)
      real*4 rs(3), focal
      real*4 eqradius, polratio, opticaxline, opticaxsamp, polradius

      real*8 lat, lon
      real*8 om(3,3)
      real*8 t(3), u(3), v(3), aa, bb, cc
      real*8 denom, d, xx, zz, r2d


c
c        initialization
c
      r2d=180.0d0/dacos(-1.0d0)  !radian to degree conversion 180/PI
      polratio=eqradius/polradius
      sign = -1
      error = 0
      aa = 0.d0
      bb = 0.d0
      cc = 0.d0
      xx = dble(samp-opticaxsamp)
      zz = dble(line-opticaxline)
c
c        calculate quadratic coefficients
c          aa = (norm(u))**2
c          bb = 2 * dot(u,v)
c          cc = (norm(v))**2 - r**2
c              where
c          t = r * (xx,zz,focal)
c          u = e * t
c          v = e * rs
c
c        note -- r is rotation from planet to camera coordinates,
c          so multiply in transpose sense
c
      do i = 1,3
          t(i) = om(1,i)*xx + om(2,i)*zz + om(3,i)*dble(focal)
      enddo
c	eccentricity matrix:  e(1,1)=1  e(2,2)=1  e(3,3)=re/rp
      u(1) = t(1)
      u(2) = t(2)
      u(3) = dble(polratio)*t(3)
      v(1) = dble(rs(1))
      v(2) = dble(rs(2))
      v(3) = dble(polratio)*dble(rs(3))
c
      aa =        u(1)*u(1) + u(2)*u(2) + u(3)*u(3)
      bb = 2.d0*( u(1)*v(1) + u(2)*v(2) + u(3)*v(3) )
      cc =        v(1)*v(1) + v(2)*v(2) + v(3)*v(3) 
      cc = cc - dble(eqradius)*dble(eqradius)
c
c        calculate depth factor 'd'.  negative discriminant means
c        point off planet.  use -sqrt to insure visible side of
c        planet.
c
      d = bb*bb - 4.d0*aa*cc
      if (d .lt. 0.d0) then
c the point is off planet; but we need a value for lat and lon
c set the discriminant to its absolute value, set sign = +1 to take
c the root on the far side of the planet
        d = dabs(d)
         sign = +1      
          error = 1
c          return
      endif
c sign = -1 for normal problem;
c sign = +1 for off planet point
      d = (-bb + dble(sign) * dsqrt(d))/(2.d0*aa)
c
c        find lat and lon
c
      do i = 1,3
          t(i) = d * t(i) + dble(rs(i))
          loc(i) = sngl(t(i))            ! save the intercept pt. in plan. 
      enddo
      denom = dsqrt(t(1)*t(1) + t(2)*t(2))
      lat = datan(t(3)/denom)*r2d
      lon = datan2(t(2),t(1))*r2d + 360.0d0
      lon = 360.0d0 - dmod(lon,360.0d0)
c
      return
      end

c***********************************************************

      function funk(anglesin)
c The objective function to minimize

        parameter (maxpix=100, maxpts=768)

        real*4 radpol(maxpix),eqrad(maxpix),focal(maxpix)
        real*4 optaxl(maxpix),optaxs(maxpix)
        real*4 rsvec(3,maxpix),zvp,rlat
        real*4 left_line(maxpts),left_samp(maxpts)
        real*4 right_line(maxpts),right_samp(maxpts)

        integer*4 left_over(maxpts),right_over(maxpts),nzvp

        real*8 anglesin(300)
        real*8 om(9,maxpix),angles(3,maxpix),lat,lon,loc(3)
        real*8 funk,x1,y1,z1,x2,y2,z2,delta_t,d2r

        common/funkcom/om,loc,nsolutions,npts,npict,rsvec,radpol,
     +                 eqrad,focal,optaxl,optaxs,left_line,
     +                 left_samp,right_line,right_samp,left_over,
     +                 right_over
        common/zvpt/delta_t(maxpix)
        common/zvp/nzvp,zvp(2,1000)
        common/fit2/angle3(100),mode

        d2r=dacos(-1.0d0)/180.0d0  !degree to radian conversion PI/180

c build angles array depending on mode - ARV
        if (mode.eq.1) then
          do i=1,nsolutions
            angles(1,i)=anglesin((i-1)*3+1)
            angles(2,i)=anglesin((i-1)*3+2)
            angles(3,i)=anglesin((i-1)*3+3)
          enddo
        else
          do i=1,nsolutions
            angles(1,i)=anglesin((i-1)*2+1)
            angles(2,i)=anglesin((i-1)*2+2)
            angles(3,i)=angle3(i)
          enddo
        endif
 
c compute om matrices but not those of the ground control.
        do i=1,nsolutions
           call fromeuler(angles(1,i),angles(2,i),angles(3,i),om(1,i))
        enddo

        funk=0.d0
c loop over all points
        do i=1,npts

c          Left image coords -> lat,lon
           j=left_over(i)
           call myippcov(ind,left_line(i),left_samp(i),lat,lon,
     +                   om(1,j),rsvec(1,j),focal(j),eqrad(j),
     +                   radpol(j),optaxl(j),optaxs(j),loc)
           if(ind.ne.0)then
              call xvmessage('off planet',' ')
              funk=1.d0+10.d0
              return
           endif
           lon=360.d0-lon

c          convert longitudes to ground control reference time
           if(nzvp.gt.0)then
             rlat=sngl(lat)
             call zonal_movement(rlat,delta_t(j),radpol(j),eqrad(j),
     +                           zonal_vel,travel_m,travel_deg)
             lon=lon - dble(travel_deg)
             if(lon.lt.0.d0) lon=lon+360.d0
           endif        
c          left image lat,lon -> xyz
           x1=dcos(lat*d2r)*dcos(lon*d2r)
           y1=dcos(lat*d2r)*dsin(lon*d2r)
           z1=dsin(lat*d2r)

c          Right image coords -> lat,lon
           j=right_over(i)
           call myippcov(ind,right_line(i),right_samp(i),lat,lon,
     +                   om(1,j),rsvec(1,j),focal(j),eqrad(j),
     +                   radpol(j),optaxl(j),optaxs(j),loc)
           if(ind.ne.0)then
              call xvmessage('off planet',' ')
              funk=1.d0+10.d0
              return
           endif
           lon=360.d0-lon

c          convert longitudes to ground control reference time
           if(nzvp.gt.0)then
             rlat=sngl(lat)
             call zonal_movement(rlat,delta_t(j),radpol(j),eqrad(j),
     +                           zonal_vel,travel_m,travel_deg)
             lon=lon - dble(travel_deg)
             if(lon.lt.0.d0) lon=lon+360.d0
           endif

c          right image lat,lon -> xyz
           x2=dcos(lat*d2r)*dcos(lon*d2r)
           y2=dcos(lat*d2r)*dsin(lon*d2r)
           z2=dsin(lat*d2r)

           funk=funk+dsqrt((x1-x2)**2+(y1-y2)**2+(z1-z2)**2)
        enddo
        funk=funk*dble(radpol(1))/dble(npts) ! mean error in KM.

        return
        end

c *************************************************************

      subroutine pointing2(nsolutions,angles,p,y,pr,prr,
     +                    pbar,ftol,ind)
c COMPUTES THE CORRECT OM MATRIX EULER ANGLES
c uses Numerical Recipes routine AMOEBA (simplex method)
c fits only two angles

      real*8 pr(nsolutions*2),prr(nsolutions*2)
      real*8 pbar(nsolutions*2),funk
      real*8 angles(3,nsolutions),p(nsolutions*2+1,nsolutions*2)
      real*8 y(nsolutions*2+1),ftol,error,errkm
      character*80 msg
      common/fit2/angle3(100),mode

      mp=nsolutions*2+1
      np=nsolutions*2
      ndim=nsolutions*2

c set up the nsolutions*2+1 simplex vertices.
      k=0
      do i=1,nsolutions
         do j=1,2
            k=k+1
            p(1,k)=angles(j,i)
         enddo
      enddo
      k=0
      do i=2,nsolutions*2+1
         do j=1,nsolutions*2
            p(i,j)=p(1,j)
         enddo
         k=k+1
         p(i,k)=p(i,k)+.01d0
      enddo

c print inputs
         msg=' '
      call xvmessage('Input Euler angles (degrees)',' ')
      do i=1,nsolutions
         write (msg,9000)angles(1,i),angles(2,i),angles(3,i)
         call xvmessage(msg,' ')
         msg=' '
      enddo
9000  format(9x,F11.6,F11.7,F11.6)

c compute residuals at each vertex
      do i=1,nsolutions*2+1
         do j=1,nsolutions*2
            pr(j)=p(i,j)
         enddo
         y(i)=funk(pr)
      enddo
      call prnt(8,1,y(1),'Mean error/tiepoint in km= .')
      call xvmessage('   ',' ')

c produce first solution
      call amoeba(p,y,pr,prr,pbar,mp,np,ndim,ftol,iter)
      if(iter.eq.0)then
        call xvmessage('No iterations possible, redo tiepoints',' ')
        ind=1
        return
      endif

c compute error in degrees
      call mve(8,nsolutions,0.d0,prr,0,1)
      do i=2,nsolutions*2+1
         k=-1
         do j=1,nsolutions
            k=k+2
            prr(j)=prr(j)+dsqrt((p(1,k)-p(i,k))**2+
     +                          (p(1,k+1)-p(i,k+1))**2)
         enddo
      enddo
      do j=1,nsolutions
         prr(j)=prr(j)/(nsolutions*2)
      enddo
      error=0.d0
      do j=1,nsolutions
         error=error+prr(j)
      enddo
      error=error/nsolutions

c compute error in KM
      do j=1,nsolutions*2
         pr(j)=p(1,j)
      enddo
      errkm=funk(pr)

c print first solution
      k=0
      call xvmessage
     +        ('First solution for Euler angles, and error(deg)',' ')
      do i=1,nsolutions
         do j=1,2
            k=k+1
            pr(j)=p(1,k)
         enddo
         pr(3)=prr(i)
         write (msg,9010)pr(1),pr(2),pr(3)
         call xvmessage(msg,' ')
      enddo
9010  format(9x,F11.6,F11.7,F11.8)
      msg=' '
      call prnt(4,1,iter,'# iterations:.')
      write (msg,9015)error
9015  format('Mean error in degrees=',F12.8)
      call xvmessage(msg,' ')
      msg=' '
      call prnt(8,1,errkm,'Mean error/tiepoint in km= .')
      call xvmessage('   ',' ')

c Second solution...
c set up the nsolutions*2+1 simplex vertices.
      k=0
      do i=2,nsolutions*2+1
         do j=1,nsolutions*2
            p(i,j)=p(1,j)
         enddo
         k=k+1
         p(i,k)=p(i,k)+error
      enddo

c compute residuals at each vertex
      do i=1,nsolutions*2+1
         do j=1,nsolutions*2
            pr(j)=p(i,j)
         enddo
         y(i)=funk(pr)
      enddo

c produce second solution
      call amoeba(p,y,pr,prr,pbar,mp,np,ndim,ftol,iter)
      if(iter.eq.0)then
        call xvmessage('No iterations possible, redo tiepoints',' ')
        ind=1
        return
      endif

c compute error in degrees
      call mve(8,nsolutions,0.d0,prr,0,1)
      do i=2,nsolutions*2+1
         k=-1
         do j=1,nsolutions
            k=k+2
            prr(j)=prr(j)+dsqrt((p(1,k)-p(i,k))**2+
     +                          (p(1,k+1)-p(i,k+1))**2)
         enddo
      enddo
      do j=1,nsolutions
         prr(j)=prr(j)/(nsolutions*2)
      enddo
      error=0.d0
      do j=1,nsolutions
         error=error+prr(j)
      enddo
      error=error/nsolutions

c compute error in KM
      do j=1,nsolutions*2
         pr(j)=p(1,j)
      enddo
      errkm=funk(pr)

c print second solution
      k=0
      call xvmessage
     +       ('Second solution for Euler angles, and error(deg)',' ')
      do i=1,nsolutions
         do j=1,2
            k=k+1
            pr(j)=p(1,k)
         enddo
         pr(3)=prr(i)
         write (msg,9020)pr(1),pr(2),pr(3)
         call xvmessage(msg,' ')
      enddo
9020  format(9x,F11.6,F11.7,F11.8)
      msg=' '
      call prnt(4,1,iter,'# iterations:.')
      write (msg,9025)error
9025  format('Mean error in degrees=',F12.8)
      call xvmessage(msg,' ')
      call prnt(8,1,errkm,'Mean error/tiepoint in km= .')
      call xvmessage('   ',' ')
      msg=' '
c copy to output
      k=0
      do i=1,nsolutions
         do j=1,2
            k=k+1
            angles(j,i)=p(1,k)
         enddo
         angles(3,i)=angle3(i)
      enddo

      ind=0
      return
      end

c **************************************************************

c Function to minimize an expression, see Numerical Recipes.
      SUBROUTINE AMOEBA(P,Y,PR,PRR,PBAR,MP,NP,NDIM,FTOL,ITER)
      PARAMETER (ALPHA=1.0,BETA=0.5,GAMMA=2.0,ITMAX=500)
      real*8 P(MP,NP),Y(MP),PR(NP),PRR(NP),PBAR(NP)
      real*8 funk
      real*8 rtol,ypr,yprr,ftol

      MPTS=NDIM+1
      ITER=0
1     ILO=1
      IF(Y(1).GT.Y(2))THEN
        IHI=1
        INHI=2
      ELSE
        IHI=2
        INHI=1
      ENDIF
      DO 11 I=1,MPTS
        IF(Y(I).LT.Y(ILO)) ILO=I
        IF(Y(I).GT.Y(IHI))THEN
          INHI=IHI
          IHI=I
        ELSE IF(Y(I).GT.Y(INHI))THEN
          IF(I.NE.IHI) INHI=I
        ENDIF
11    CONTINUE
      RTOL=2.d0*DABS(Y(IHI)-Y(ILO))/(DABS(Y(IHI))+DABS(Y(ILO)))
      IF(RTOL.LT.FTOL)RETURN
c      IF(ITER.EQ.ITMAX) 
c     +     call xvmessage('Amoeba exceeding maximum iterations.',' ')
      ITER=ITER+1
      DO 12 J=1,NDIM
        PBAR(J)=0.d0
12    CONTINUE
      DO 14 I=1,MPTS
        IF(I.NE.IHI)THEN
          DO 13 J=1,NDIM
            PBAR(J)=PBAR(J)+P(I,J)
13        CONTINUE
        ENDIF
14    CONTINUE
      DO 15 J=1,NDIM
        PBAR(J)=PBAR(J)/DBLE(NDIM)
        PR(J)=(1.d0+DBLE(ALPHA))*PBAR(J)-DBLE(ALPHA)*P(IHI,J)
15    CONTINUE
      YPR=FUNK(PR)
      IF(YPR.LE.Y(ILO))THEN
        DO 16 J=1,NDIM
          PRR(J)=DBLE(GAMMA)*PR(J)+(1.d0-DBLE(GAMMA))*PBAR(J)
16      CONTINUE
        YPRR=FUNK(PRR)
        IF(YPRR.LT.Y(ILO))THEN
          DO 17 J=1,NDIM
            P(IHI,J)=PRR(J)
17        CONTINUE
          Y(IHI)=YPRR
        ELSE
          DO 18 J=1,NDIM
            P(IHI,J)=PR(J)
18        CONTINUE
          Y(IHI)=YPR
        ENDIF
      ELSE IF(YPR.GE.Y(INHI))THEN
        IF(YPR.LT.Y(IHI))THEN
          DO 19 J=1,NDIM
            P(IHI,J)=PR(J)
19        CONTINUE
          Y(IHI)=YPR
        ENDIF
        DO 21 J=1,NDIM
          PRR(J)=DBLE(BETA)*P(IHI,J)+(1.d0-DBLE(BETA))*PBAR(J)
21      CONTINUE
        YPRR=FUNK(PRR)
        IF(YPRR.LT.Y(IHI))THEN
          DO 22 J=1,NDIM
            P(IHI,J)=PRR(J)
22        CONTINUE
          Y(IHI)=YPRR
        ELSE
          DO 24 I=1,MPTS
            IF(I.NE.ILO)THEN
              DO 23 J=1,NDIM
                PR(J)=0.5d0*(P(I,J)+P(ILO,J))
                P(I,J)=PR(J)
23            CONTINUE
              Y(I)=FUNK(PR)
            ENDIF
24        CONTINUE
        ENDIF
      ELSE
        DO 25 J=1,NDIM
          P(IHI,J)=PR(J)
25      CONTINUE
        Y(IHI)=YPR
      ENDIF
      GO TO 1
      END

c***********************************************************

      subroutine pointing3(nsolutions,angles,p,y,pr,prr,
     +                    pbar,ftol,ind)
c COMPUTES THE CORRECT OM MATRIX EULER ANGLES
c uses Numerical Recipes routine AMOEBA (simplex method)

      real*8 pr(nsolutions*3),prr(nsolutions*3)
      real*8 pbar(nsolutions*3),funk
      real*8 angles(3,nsolutions),p(nsolutions*3+1,nsolutions*3)
      real*8 y(nsolutions*3+1),ftol,error,errkm
      character*80 msg

      mp=nsolutions*3+1
      np=nsolutions*3
      ndim=nsolutions*3

c set up the nsolutions*3+1 simplex vertices.
      k=0
      do i=1,nsolutions
         do j=1,3
            k=k+1
            p(1,k)=angles(j,i)
         enddo
      enddo
      k=0
      do i=2,nsolutions*3+1
         do j=1,nsolutions*3
            p(i,j)=p(1,j)
         enddo
         k=k+1
         p(i,k)=p(i,k)+.01d0
      enddo

c print inputs
         msg=' '
      call xvmessage('Input Euler angles (degrees)',' ')
      do i=1,nsolutions
         write (msg,9000)angles(1,i),angles(2,i),angles(3,i)
         call xvmessage(msg,' ')
         msg=' '
      enddo
9000  format(9x,F11.6,F11.7,F11.6)

c compute residuals at each vertex
      do i=1,nsolutions*3+1
         do j=1,nsolutions*3
            pr(j)=p(i,j)
         enddo
         y(i)=funk(pr)
      enddo
      call prnt(8,1,y(1),'Mean error/tiepoint in km= .')
      call xvmessage('   ',' ')

c produce first solution
      call amoeba(p,y,pr,prr,pbar,mp,np,ndim,ftol,iter)
      if(iter.eq.0)then
        call xvmessage('No iterations possible, redo tiepoints',' ')
        ind=1
        return
      endif

c compute error in degrees
      call mve(8,nsolutions,0.d0,prr,0,1)
      do i=2,nsolutions*3+1
         k=-2
         do j=1,nsolutions
            k=k+3
            prr(j)=prr(j)+dsqrt((p(1,k)-p(i,k))**2+
     +                          (p(1,k+1)-p(i,k+1))**2+
     +                          (p(1,k+2)-p(i,k+2))**2)
         enddo
      enddo
      do j=1,nsolutions
         prr(j)=prr(j)/(nsolutions*3)
      enddo
      error=0.d0
      do j=1,nsolutions
         error=error+prr(j)
      enddo
      error=error/nsolutions

c compute error in KM
      do j=1,nsolutions*3
         pr(j)=p(1,j)
      enddo
      errkm=funk(pr)

c print first solution
      k=0
      call xvmessage
     +        ('First solution for Euler angles, and error(deg)',' ')
      do i=1,nsolutions
         do j=1,3
            k=k+1
            pr(j)=p(1,k)
         enddo
         pr(4)=prr(i)
         write (msg,9010)pr(1),pr(2),pr(3),pr(4)
         call xvmessage(msg,' ')
      enddo
9010  format(9x,F11.6,F11.7,F11.6,F11.8)
      msg=' '
      call prnt(4,1,iter,'# iterations:.')
      write (msg,9015)error
9015  format('Mean error in degrees=',F12.8)
      call xvmessage(msg,' ')
      msg=' '
      call prnt(8,1,errkm,'Mean error/tiepoint in km= .')
      call xvmessage('   ',' ')

c Second solution...
c set up the nsolutions*3+1 simplex vertices.
      k=0
      do i=2,nsolutions*3+1
         do j=1,nsolutions*3
            p(i,j)=p(1,j)
         enddo
         k=k+1
         p(i,k)=p(i,k)+error
      enddo

c compute residuals at each vertex
      do i=1,nsolutions*3+1
         do j=1,nsolutions*3
            pr(j)=p(i,j)
         enddo
         y(i)=funk(pr)
      enddo

c produce second solution
      call amoeba(p,y,pr,prr,pbar,mp,np,ndim,ftol,iter)
      if(iter.eq.0)then
        call xvmessage('No iterations possible, redo tiepoints',' ')
        ind=1
        return
      endif

c compute error in degrees
      call mve(8,nsolutions,0.d0,prr,0,1)
      do i=2,nsolutions*3+1
         k=-2
         do j=1,nsolutions
            k=k+3
            prr(j)=prr(j)+dsqrt((p(1,k)-p(i,k))**2+
     +                          (p(1,k+1)-p(i,k+1))**2+
     +                          (p(1,k+2)-p(i,k+2))**2)
         enddo
      enddo
      do j=1,nsolutions
         prr(j)=prr(j)/(nsolutions*3)
      enddo
      error=0.d0
      do j=1,nsolutions
         error=error+prr(j)
      enddo
      error=error/nsolutions

c compute error in KM
      do j=1,nsolutions*3
         pr(j)=p(1,j)
      enddo
      errkm=funk(pr)

c print second solution
      k=0
      call xvmessage
     +       ('Second solution for Euler angles, and error(deg)',' ')
      do i=1,nsolutions
         do j=1,3
            k=k+1
            pr(j)=p(1,k)
         enddo
         pr(4)=prr(i)
         write (msg,9020)pr(1),pr(2),pr(3),pr(4)
         call xvmessage(msg,' ')
      enddo
9020  format(9x,F11.6,F11.7,F11.6,F11.8)
      msg=' '
      call prnt(4,1,iter,'# iterations:.')
      write (msg,9025)error
9025  format('Mean error in degrees=',F12.8)
      call xvmessage(msg,' ')
      call prnt(8,1,errkm,'Mean error/tiepoint in km= .')
      call xvmessage('   ',' ')
      msg=' '
c copy to output
      k=0
      do i=1,nsolutions
         do j=1,3
            k=k+1
            angles(j,i)=p(1,k)
         enddo
      enddo

      ind=0
      return
      end

c **************************************************************

	subroutine toeuler (c, alpha, delta, kappa, error)
	implicit none
	real*8	c(3,3)      ! output - derived rotation matrix 
	real*8	alpha       ! input  - ra of z axis (degrees)
	real*8	delta	    ! input  - declination z axis (degrees)
	real*8	kappa	    ! input  - rotation angle around z axis
 			    !          (3rd euler angle) (degrees)

c  this routine performs the functional inverse of routine fromeuler.
c  this routine takes an input rotation matrix, and computes the three euler
c  angles representing the matrix.  (these 3 angles are called alpha, delta,
c  and kappa by mert davies etc.)  if the matrix is not a valid rotation
c  matrix (i.e. if the length of the row and column vectors is not within
c  0.0001 of unity) then error is returned true.
c
c  the 9 elements of the matrix are stored in order of increasing address as
c
c                  |  1   3   7  |     | c(1,1)  c(1,2)  c(1,3) |
c                  |  2   5   8  |     | c(2,1)  c(2,2)  c(2,3) |    
c                  |  3   6   9  |     | c(3,1)  c(3,2)  c(3,3) |
c

	real*8	collength, rowlength, d2r, r2d
	integer i, j
	logical	error

        d2r=dacos(-1.0d0)/180.0d0  !degree to radian conversion PI/180
        r2d=180.0d0/dacos(-1.0d0)

	error = .false.
	do i = 1, 3
	    collength = 0.d0
	    rowlength = 0.d0
	    do j = 1, 3
		collength = collength + c(i,j)**2
		rowlength = rowlength + c(j,i)**2
	    enddo
	    if (dabs(collength-1.d0) .gt. 0.0001d0) error = .true.
	    if (dabs(rowlength-1.d0) .gt. 0.0001d0) error = .true.
	enddo

	if (.not. error) then
	    delta = dasin(c(3,3))*r2d
	    if (dabs(c(3,1)) .gt. 1e-10) then
		alpha = datan2(c(3,2), c(3,1))*r2d
	    else if (dabs(c(3,2)) .lt. 1e-10) then
		alpha = 0.d0
	    else
		alpha = sign(dble(90.0),c(3,2))
	    endif
	    if (alpha .lt. 0.d0) alpha = alpha + 360.d0
	    if (dabs(c(2,3)) .gt. 1e-10) then
		kappa = datan2(c(1,3), c(2,3))*r2d
	    else if (dabs(c(1,3)) .lt. 1e-10) then
		kappa = datan2( -c(1,1), -c(2,1) )*r2d
	    else
		kappa = sign(dble(90.0),c(1,3))
	    endif
	    if (kappa .lt. 0.d0) kappa = kappa + 360.d0
	    if (dabs(dcos(delta*d2r)) * dcos(kappa*d2r) - c(2,3) .gt.
     +		0.0001d0) kappa = 180.d0 - kappa
	endif

	return
        end

c ****************************************************************
	subroutine fromeuler (alpha, delta, kappa, c)
	implicit none
	real*8	alpha       ! input  - ra of z axis (degrees)
	real*8	delta	    ! input  - declination z axis (degrees)
	real*8	kappa	    ! input  - rotation angle around z axis
 			    !          (3rd euler angle) (degrees)
	real*8	c(3,3)      ! output - derived rotation matrix 

c  this routine performs the functional inverse of routine toeuler.  the
c  three euler angles defining the orientation of the rotation matrix are input,
c  and the resultant rotation matrix is output.
c
c  the 9 elements of the matrix are stored in order of increasing address as
c
c                  |  1   3   7  |     | c(1,1)  c(1,2)  c(1,3) |
c                  |  2   5   8  |     | c(2,1)  c(2,2)  c(2,3) |    
c                  |  3   6   9  |     | c(3,1)  c(3,2)  c(3,3) |
c

	real*8	cos_delta, sin_delta, cos_alpha, sin_alpha
	real*8	cos_kappa, sin_kappa, d2r

        d2r=dacos(-1.0d0)/180.0d0  !degree to radian conversion PI/180

	sin_alpha = dsin(alpha*d2r)
	cos_alpha = dcos(alpha*d2r)
	sin_delta = dsin(delta*d2r)
	cos_delta = dcos(delta*d2r)
	sin_kappa = dsin(kappa*d2r)
	cos_kappa = dcos(kappa*d2r)

	c(1,1) = -sin_alpha * cos_kappa - cos_alpha * sin_delta
     &           * sin_kappa
	c(1,2) =  cos_alpha * cos_kappa - sin_alpha * sin_delta
     &           * sin_kappa
	c(1,3) =  cos_delta * sin_kappa
	c(2,1) =  sin_alpha * sin_kappa - cos_alpha * sin_delta
     &           * cos_kappa
	c(2,2) = -cos_alpha * sin_kappa - sin_alpha * sin_delta
     &           * cos_kappa
	c(2,3) =  cos_delta * cos_kappa
	c(3,1) =  cos_alpha * cos_delta
	c(3,2) =  sin_alpha * cos_delta
	c(3,3) =  sin_delta

	return
        end


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create omcor2.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM omcor2

   To Create the build file give the command:

		$ vimake omcor2			(VMS)
   or
		% vimake omcor2			(Unix)


************************************************************************/


#define PROGRAM	omcor2
#define R2LIB

#define MODULE_LIST omcor2.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create omcor2.pdf
PROCESS HELP=*
 PARM INP      TYPE=STRING  COUNT=(2:4)
 PARM PROJECT  TYPE=(STRING,5) COUNT=1 +
    VALID=("VGR-1","VGR-2","MAR10","MAR-9","VIKOR","GLL  ","CASSI")
 PARM OBJECT   TYPE=KEYWORD VALID=(IMAGE,OBJECT) DEFAULT=IMAGE
 PARM FIT2     TYPE=KEYWORD COUNT=0:1 VALID=FIT2 DEFAULT=--
 PARM CAMERA   TYPE=INTEGER COUNT=(0:1) DEFAULT=--
 PARM GROUND TYPE=INTEGER COUNT=(0:1) DEFAULT=-- +
               VALID=(1:50)
 PARM TOLERANC TYPE=REAL COUNT=1 VALID=(.00000001:.01) DEFAULT=.000001
 PARM OLD TYPE=KEYWORD VALID=(OLD,NEW) DEFAULT=NEW
 PARM ZONAL TYPE=KEYWORD VALID=(ZONAL,NOZONAL) DEFAULT=NOZONAL
 PARM ZVP TYPE=STRING COUNT=(0:1) DEFAULT=jupiter.zvp
END-PROC

.TITLE
VICAR/IBIS Program "omcor2"

.HELP
PURPOSE
This program updates the first input IBIS SEDR file by changing 
the OM Euler angles in columns 21,22,23 which define camera
pointing via the OM matrix. "omcor2" performs a global function
minimization of tiepoint residuals by determining the OM
matrices which cause the tiepoints to disagree between image
pairs by a minimum.
 
The GLL flight projects permit very little error in north angle. We suggest
that you use the 'FIT2 parameter to reflect this. Your solutions will improve.

"omcor2" differs from omcor in two ways:
1. Control images are not permitted to move (their OM matrices
   are considered known).
2. "omcor2" permits the use of a zonal velocity flow model to shift
   tiepoints to the common time of the ground control image.
   This permits a global solution for the navigation for non solid
   objects like jupiter. See the ZONAL keyword.
3. The FIT2 keyword permits the north angle to remain fixed for all images.

EXECUTION
omcor2 inp=(SEDR.INT,TIEPOINTS.INT,GEOM.COR)
or-
omcor2 inp=(SEDR.INT,TIEPOINTS.INT)

EXAMPLE: (Galileo)
See mosaicking procedure MANUAL4 in MANMATCH.PDF (Help file).

omcor2 inp=(jupsedr.int,jupmatch.int) +
    'zonal project=GLL camera=1 'fit2

PRECISION
The differences between the unported and ported VAX test logs and the 
differences between the VAX and UNIX test logs are due to the image
space mode being invoked with object space data.  The discrepency is
probably due to two factors:
1. There is no clear solution (bogus test case) so the solution is 
probably very sensitive to precision.
2. Many transformations are made from image to object space resulting in
small errors which get fed into the minimization scheme.

HISTORY
27 AUG 96  ARV  (CIT)  Added FIT2 capability
10 JUL 95  AMS  (CRI)  Made portable for UNIX

.LEVEL1
.VARIABLE INP
First input= SEDR.INT produced
 by program "ibisnav"
Second input=TIEPOINTS.INT
 by program "manmatch"
Third input=
A "geoma" distortion file.

.VARIABLE PROJECT
Specifies  the 
project. 

.VARIABLE OBJECT
Causes images to be 
considered geometrically
corrected.

.VARIABLE FIT2
Causes kappa (north angle)
to be kept fixed.

.VARIABLE CAMERA
The camera serial number.

.VARIABLE GROUND
Ground control frames

.VARIABLE TOLERANC
fractional convergence
tolerance.

.VARIABLE OLD
Source of Euler
angles in SEDR.

.VARIABLE ZONAL
Use zonal flow.

.VARIABLE ZVP
Name of zonal
flow file.

.LEVEL2
.VARIABLE INP
First input= SEDR.INT produced by program "ibisnav"
On output columns 21-23 are recomputed to reflect the updated 
OM matrix angles. Columns 21-23 will NOT be changed for
ground control frames.
NOTE: Place those frames which will become ground control frames
at the END of the SEDR.INT file. See GROUND parameter.
There can be up to 100 sedr file entries (frames).
Note: The scet columns 32-37 must be filled for zonal correction.
This can be assured by using a FILENAME file in "ibisnav".


Second input= TIEPOINTS.INT produced by program "manmatch".
This has 9 columns.
There can be up to 768 tiepoints.

A geom correction file can be entered as the third input
if the images are in image space (un geometrically corrected).
If the project is not GLL and no geom file is present then
nominals will be generated using the PROJECT and CAMERA
parameters.
Not necessary if project is GLL.

.VARIABLE PROJECT
Specifies the project . 
Valid values are: VGR-1 VGR-2 GLL MAR-9 MAR10 VIKOR
Required.

.VARIABLE OBJECT
Causes images to be considered geometrically corrected.
The geoma file will be ignored if present.

.VARIABLE FIT2
Fits two of the three angles, keeping kappa (north angle)
fixed at the input value.

.VARIABLE CAMERA
The camera serial number. Only used in 'IMAGE mode.
Defaults to the entry in the 4th ibis column of the sedr ibis file.
Only one camera is permitted per execution of omcor2.

Voyager:
Used to assist in reading the GEOM parameters for geometric correction.

Galileo:
Used to determine the correct image space correction 
depending upon if an image is summation mode or not.
Normal images must have CAMERA=1.
Summation mode images must have CAMERA=2.

.VARIABLE GROUND
Ground control frames. The number given defines those
images in the SEDR input file which are defined
as ground control frames. 
GROUND specifies the first ground control frame.
All frames after GROUND will ALSO be considered to be
ground control frames.
It is assumed that the
navigation contained in the first input ibis file is
rigorously correct for these frames and will not be altered.
Numbering begins at 1 for the first input, 2 for the
second input and so on. 
If you default GROUND the last frame (last SEDR record) will
become the ground control.
Note: If ZONAL is specified then GROUND is ignored and defaults to
      the last frame.

.VARIABLE TOLERANC
fractional convergence tolerance.
Controls the precision and iteration length of the function
minimization scheme. Is defined as the ratio:
2*(max_error - min_error)/(max_error + min_error)
Cannot be less than the machine real*8 precision.
Defaults to 1.0e-6

.VARIABLE OLD
Source of Euler angles in input SEDR.INT file.
If you specify 'OLD then the euler angles will come from 
columns 8,9,10. The default is they come from columns 21,22,23.
Columns 21,22,23 are initially copies of columns 8,9,10 but are updated
in-place with the program solutions.

.VARIABLE ZONAL
Use the zonal flow profile to shift each tiepoint by the amount of
differential rotation suffered over the interval between when that
image was obtained and when the ground control frame was obtained.
Only use for non solid planets like Jupiter. The images must be
obtained over a small time difference ( minutes not hours ) or else
the zonal drift will introduce large errors.

.VARIABLE ZVP
Name of the zonal flow file.
Defaults to jupiter.zvp

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstomcor2.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
refgbl $syschar
if ($syschar(1) = "UNIX")
 ush cp /project/test_work/testdata/gll/jupiter.zvp jupiter.zvp
 ush cp /project/test_work/testdata/gll/omcor2.nav omcor2.nav          
 ush cp /project/test_work/testdata/gll/omcor2.tpt omcor2.tpt          
 ush cp /project/test_work/testdata/cassini/iss/dione.nav dione.nav
 ush cp /project/test_work/testdata/cassini/iss/dione.tp dione.tp
else
 dcl copy  wms_test_work:[testdata.gll]omcor2.nav omcor2.nav
 dcl copy  wms_test_work:[testdata.gll]omcor2.tpt omcor2.tpt
 dcl copy  wms_test_work:[testdata.gll]jupiter.zvp jupiter.zvp
end-if
!
ibis-list omcor2.nav sc=21 sr=1 nc=3 nr=6 cform="%10.5f %10.5f %10.5f"
omcor2 inp=(omcor2.nav,omcor2.tpt) project=GLL 'fit2 'old camera=1
omcor2 inp=(omcor2.nav,omcor2.tpt) project=GLL 'fit2 camera=1
ibis-list omcor2.nav sc=21 sr=1 nc=3 nr=6 cform="%10.5f %10.5f %10.5f"
!
!ibis-list xxsedr.int sc=21 sr=1 nc=3 nr=4 cform="%10.5f %10.5f %10.5f"
!omcor2 inp=(xxsedr.int,xxmatch.int) project=VGR-1 'OBJECT
!ibis-list xxsedr.int sc=21 sr=1 nc=3 nr=4 cform="%10.5f %10.5f %10.5f"
!
! begin at 'OLD position & re-execute iteratively
!ibis-list xxsedr.int sc=8 sr=1 nc=3 nr=4 cform="%10.5f %10.5f %10.5f"
!omcor2 inp=(xxsedr.int,xxmatch.int) project=VGR-1 'OBJECT +
!     camera=7 'OLD 'fit2
!omcor2 inp=(xxsedr.int,xxmatch.int) project=VGR-1 'OBJECT +
!     camera=7 'fit2
!omcor2 inp=(xxsedr.int,xxmatch.int) project=VGR-1 'OBJECT +
!     camera=7 'fit2
!ibis-list xxsedr.int sc=21 sr=1 nc=3 nr=4 cform="%10.5f %10.5f %10.5f"
!
! pretend is image space data
!omcor2 inp=(xxsedr.int,xxmatch.int) project=VGR-1 +
!     camera=7 'OLD 'fit2
!
! test zonal option
!omcor2 inp=(jup1sedr.int,jup1match.int) +
!    'zonal project=VGR-1 'new 'object 'fit2
!
! Cassini test (files supplied by C.Avis)
omcor2 (dione.nav, dione.tp) proj=CAS 'object ground=12
!
if ($syschar(1) = "UNIX")
  ush rm jupiter.zvp
  ush rm omcor2.nav
  ush rm omcor2.tpt
  ush rm dione.nav
  ush rm dione.tp
end-if
end-proc
$!-----------------------------------------------------------------------------
$ create tstomcor2.log_solos
tstomcor2
refgbl $syschar
if ($syschar(1) = "UNIX")
 ush cp /project/test_work/testdata/gll/jupiter.zvp jupiter.zvp
 ush cp /project/test_work/testdata/gll/omcor2.nav omcor2.nav
 ush cp /project/test_work/testdata/gll/omcor2.tpt omcor2.tpt
 ush cp /project/test_work/testdata/cassini/iss/dione.nav dione.nav
 ush cp /project/test_work/testdata/cassini/iss/dione.tp dione.tp
else
end-if
ibis-list omcor2.nav sc=21 sr=1 nc=3 nr=6 cform="%10.5f %10.5f %10.5f"
Beginning VICAR task ibis
 
Number of Rows:6  Number of Columns: 37      
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:6
+----------+----------+---------
       C:21       C:22      C:23
+----------+----------+---------
 236.56863    2.25451  356.67090
 237.06033    2.21574  356.63950
 236.08849    1.85556  357.12939
 236.33672    1.83802  357.11758
 236.58223    1.81517  357.11282
 236.81427    2.23617  356.65546
omcor2 inp=(omcor2.nav,omcor2.tpt) project=GLL 'fit2 'old camera=1
Beginning VICAR task omcor2
OMCOR2 version OCT-96 jjl
# images in SEDR file=          6
Camera # in ibis file=        -999
GROUND=           6
# tiepoints located =          17
Treating images as IMAGE space images
Converting coordinates to object space
Beginning solution search
Input Euler angles (degrees)
          236.566895  2.2561262 356.670898
          237.060669  2.2150457 356.639496
          236.085068  1.8573743 357.129395
          236.336441  1.8385832 357.117584
          236.581787  1.8175324 357.112823
Mean error/tiepoint in km=   53.061478

First solution for Euler angles, and error(deg)
          236.568621  2.2545006 0.00000038
          237.060331  2.2157438 0.00000005
          236.088492  1.8555718 0.00000028
          236.336715  1.8380243 0.00000001
          236.582229  1.8151803 0.00000021
# iterations:        422
Mean error in degrees=  0.00000019
Mean error/tiepoint in km=   15.741596

Second solution for Euler angles, and error(deg)
          236.568629  2.2545095 0.00000139
          237.060331  2.2157438 0.00000003
          236.088483  1.8555663 0.00000114
          236.336715  1.8380242 0.00000001
          236.582226  1.8151743 0.00000095
# iterations:        135
Mean error in degrees=  0.00000071
Mean error/tiepoint in km=   15.741028

omcor2 inp=(omcor2.nav,omcor2.tpt) project=GLL 'fit2 camera=1
Beginning VICAR task omcor2
OMCOR2 version OCT-96 jjl
# images in SEDR file=          6
Camera # in ibis file=        -999
GROUND=           6
# tiepoints located =          17
Treating images as IMAGE space images
Converting coordinates to object space
Beginning solution search
Input Euler angles (degrees)
          236.568634  2.2545094 356.670898
          237.060333  2.2157438 356.639496
          236.088486  1.8555663 357.129395
          236.336716  1.8380243 357.117584
          236.582230  1.8151743 357.112823
Mean error/tiepoint in km=   15.742326

First solution for Euler angles, and error(deg)
          236.568631  2.2545052 0.00000019
          237.060331  2.2157438 0.00000004
          236.088487  1.8555635 0.00000015
          236.336715  1.8380243 0.00000001
          236.582228  1.8151740 0.00000012
# iterations:        332
Mean error in degrees=  0.00000010
Mean error/tiepoint in km=   15.740941

Second solution for Euler angles, and error(deg)
          236.568631  2.2545052 0.00000003
          237.060331  2.2157438 0.00000001
          236.088487  1.8555635 0.00000003
          236.336715  1.8380243 0.00000000
          236.582228  1.8151740 0.00000003
# iterations:         11
Mean error in degrees=  0.00000002
Mean error/tiepoint in km=   15.740941

ibis-list omcor2.nav sc=21 sr=1 nc=3 nr=6 cform="%10.5f %10.5f %10.5f"
Beginning VICAR task ibis
 
Number of Rows:6  Number of Columns: 37      
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:6
+----------+----------+---------
       C:21       C:22      C:23
+----------+----------+---------
 236.56863    2.25451  356.67090
 237.06033    2.21574  356.63950
 236.08849    1.85556  357.12939
 236.33672    1.83802  357.11758
 236.58223    1.81517  357.11282
 236.81427    2.23617  356.65546
omcor2 (dione.nav, dione.tp) proj=CAS 'object ground=12
Beginning VICAR task omcor2
OMCOR2 version OCT-96 jjl
# images in SEDR file=         12
Camera # in ibis file=           1
GROUND=          12
# tiepoints located =          34
Treating images as OBJECT space images
Beginning solution search
Input Euler angles (degrees)
            9.474711  1.8202074 270.012390
            9.208696  1.5760661 269.919250
            8.940827  1.3379499 269.911133
            8.909019  1.3958033 269.913269
            8.631347  1.7373493 269.951965
            8.355293  2.0821910 270.008911
            8.076267  2.4326472 270.005615
            7.821219  2.2863350 269.974854
            7.531661  2.0835867 269.956482
            7.240105  1.8866909 269.983826
            7.034562  1.6866417 269.979370
Mean error/tiepoint in km=  0.12126750

First solution for Euler angles, and error(deg)
            9.474707  1.8202129 270.007622 0.00001493
            9.208687  1.5760709 269.924818 0.00001271
            8.940829  1.3379399 269.910576 0.00000121
            8.909024  1.3957872 269.911660 0.00000776
            8.631342  1.7373381 269.950809 0.00000657
            8.355292  2.0821778 270.008563 0.00000229
            8.076267  2.4326347 270.004826 0.00000714
            7.821215  2.2863205 269.975457 0.00000390
            7.531647  2.0835724 269.958061 0.00001107
            7.240097  1.8866735 269.984662 0.00000371
            7.034544  1.6866363 269.973770 0.00002050
# iterations:       5404
Mean error in degrees=  0.00000835
Mean error/tiepoint in km=  0.12093864

Second solution for Euler angles, and error(deg)
            9.474707  1.8202130 270.007640 0.00000039
            9.208687  1.5760707 269.924812 0.00000015
            8.940829  1.3379401 269.910566 0.00000034
            8.909024  1.3957873 269.911665 0.00000013
            8.631342  1.7373376 269.950822 0.00000051
            8.355292  2.0821778 270.008568 0.00000017
            8.076267  2.4326346 270.004843 0.00000035
            7.821215  2.2863205 269.975440 0.00000040
            7.531647  2.0835721 269.958060 0.00000006
            7.240097  1.8866735 269.984658 0.00000020
            7.034543  1.6866363 269.973772 0.00000006
# iterations:       1309
Mean error in degrees=  0.00000025
Mean error/tiepoint in km=  0.12091475

if ($syschar(1) = "UNIX")
  ush rm jupiter.zvp
  ush rm omcor2.nav
  ush rm omcor2.tpt
  ush rm dione.nav
  ush rm dione.tp
end-if
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
$ create tstomcor2.log_linux
tstomcor2
refgbl $syschar
if ($syschar(1) = "UNIX")
 ush cp /project/test_work/testdata/gll/jupiter.zvp jupiter.zvp
 ush cp /project/test_work/testdata/gll/omcor2.nav omcor2.nav
 ush cp /project/test_work/testdata/gll/omcor2.tpt omcor2.tpt
 ush cp /project/test_work/testdata/cassini/iss/dione.nav dione.nav
 ush cp /project/test_work/testdata/cassini/iss/dione.tp dione.tp
else
end-if
ibis-list omcor2.nav sc=21 sr=1 nc=3 nr=6 cform="%10.5f %10.5f %10.5f"
Beginning VICAR task ibis
 
Number of Rows:6  Number of Columns: 37      
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:6
+----------+----------+---------
       C:21       C:22      C:23
+----------+----------+---------
 236.56929    2.25522  356.50473
 237.06029    2.21586  356.67090
 236.08824    1.85651  356.98654
 236.33652    1.83822  357.05688
 236.58206    1.81508  357.11832
 236.81427    2.23617  356.65546
omcor2 inp=(omcor2.nav,omcor2.tpt) project=GLL 'fit2 'old camera=1
Beginning VICAR task omcor2
OMCOR2 version OCT-96 jjl
# images in SEDR file=          6
Camera # in ibis file=        -999
GROUND=           6
# tiepoints located =          17
Treating images as IMAGE space images
Converting coordinates to object space
Beginning solution search
Input Euler angles (degrees)
          236.566895  2.2561262 356.670898
          237.060669  2.2150457 356.639496
          236.085068  1.8573743 357.129395
          236.336441  1.8385832 357.117584
          236.581787  1.8175324 357.112823
Mean error/tiepoint in km=   53.061489

First solution for Euler angles, and error(deg)
          236.568622  2.2544983 0.00000017
          237.060331  2.2157438 0.00000003
          236.088493  1.8555654 0.00000016
          236.336715  1.8380243 0.00000001
          236.582228  1.8151791 0.00000018
# iterations:        428
Mean error in degrees=  0.00000011
Mean error/tiepoint in km=   15.741589

Second solution for Euler angles, and error(deg)
          236.568622  2.2544983 0.00000003
          237.060331  2.2157438 0.00000001
          236.088493  1.8555654 0.00000003
          236.336715  1.8380243 0.00000000
          236.582228  1.8151791 0.00000003
# iterations:         11
Mean error in degrees=  0.00000002
Mean error/tiepoint in km=   15.741589

omcor2 inp=(omcor2.nav,omcor2.tpt) project=GLL 'fit2 camera=1
Beginning VICAR task omcor2
OMCOR2 version OCT-96 jjl
# images in SEDR file=          6
Camera # in ibis file=        -999
GROUND=           6
# tiepoints located =          17
Treating images as IMAGE space images
Converting coordinates to object space
Beginning solution search
Input Euler angles (degrees)
          236.568619  2.2544982 356.670898
          237.060333  2.2157438 356.639496
          236.088486  1.8555654 357.129395
          236.336716  1.8380243 357.117584
          236.582230  1.8151791 357.112823
Mean error/tiepoint in km=   15.743096

First solution for Euler angles, and error(deg)
          236.568620  2.2544981 0.00000018
          237.060331  2.2157438 0.00000005
          236.088486  1.8555657 0.00000025
          236.336715  1.8380243 0.00000001
          236.582226  1.8151796 0.00000012
# iterations:        326
Mean error in degrees=  0.00000012
Mean error/tiepoint in km=   15.741633

Second solution for Euler angles, and error(deg)
          236.568620  2.2544983 0.00000004
          237.060331  2.2157438 0.00000002
          236.088486  1.8555657 0.00000007
          236.336715  1.8380243 0.00000000
          236.582226  1.8151794 0.00000008
# iterations:         33
Mean error in degrees=  0.00000004
Mean error/tiepoint in km=   15.741585

ibis-list omcor2.nav sc=21 sr=1 nc=3 nr=6 cform="%10.5f %10.5f %10.5f"
Beginning VICAR task ibis
 
Number of Rows:6  Number of Columns: 37      
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:6
+----------+----------+---------
       C:21       C:22      C:23
+----------+----------+---------
 236.56862    2.25450  356.67090
 237.06033    2.21574  356.63950
 236.08849    1.85557  357.12939
 236.33672    1.83802  357.11758
 236.58223    1.81518  357.11282
 236.81427    2.23617  356.65546
omcor2 (dione.nav, dione.tp) proj=CAS 'object ground=12
Beginning VICAR task omcor2
OMCOR2 version OCT-96 jjl
# images in SEDR file=         12
Camera # in ibis file=           1
GROUND=          12
# tiepoints located =          34
Treating images as OBJECT space images
Beginning solution search
Input Euler angles (degrees)
            9.474703  1.8202107 270.016785
            9.208652  1.5760316 269.930847
            8.940825  1.3379033 269.917542
            8.909021  1.3957828 269.917450
            8.631331  1.7373420 269.954865
            8.355282  2.0822046 270.003998
            8.076267  2.4326501 270.005005
            7.821192  2.2863357 269.974640
            7.531627  2.0836051 269.964264
            7.240085  1.8866845 269.982056
            7.034535  1.6866310 269.979004
Mean error/tiepoint in km=  0.12186796

First solution for Euler angles, and error(deg)
            9.474712  1.8202199 270.015288 0.00001213
            9.208654  1.5760322 269.928532 0.00000509
            8.940819  1.3379059 269.916199 0.00000389
            8.909022  1.3957815 269.915414 0.00000581
            8.631335  1.7373451 269.954183 0.00000251
            8.355286  2.0822048 270.004540 0.00000128
            8.076271  2.4326513 270.005123 0.00000083
            7.821203  2.2863329 269.972668 0.00000574
            7.531634  2.0836038 269.963064 0.00000457
            7.240088  1.8866862 269.982049 0.00000206
            7.034534  1.6866374 269.976560 0.00000660
# iterations:       3639
Mean error in degrees=  0.00000459
Mean error/tiepoint in km=  0.12170417

Second solution for Euler angles, and error(deg)
            9.474712  1.8202207 270.015288 0.00000013
            9.208654  1.5760323 269.928532 0.00000006
            8.940819  1.3379060 269.916193 0.00000011
            8.909023  1.3957848 269.915421 0.00000041
            8.631335  1.7373450 269.954182 0.00000017
            8.355286  2.0822048 270.004537 0.00000011
            8.076271  2.4326513 270.005124 0.00000020
            7.821203  2.2863328 269.972665 0.00000028
            7.531634  2.0836038 269.963060 0.00000019
            7.240088  1.8866862 269.982053 0.00000006
            7.034534  1.6866374 269.976566 0.00000017
# iterations:       1001
Mean error in degrees=  0.00000017
Mean error/tiepoint in km=  0.12168599

if ($syschar(1) = "UNIX")
  ush rm jupiter.zvp
  ush rm omcor2.nav
  ush rm omcor2.tpt
  ush rm dione.nav
  ush rm dione.tp
end-if
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
