$!****************************************************************************
$!
$! Build proc for MIPL module mosplot
$! VPACK Version 1.9, Sunday, December 22, 2002, 12:29:25
$!
$! Execute by entering:		$ @mosplot
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
$ write sys$output "*** module mosplot ***"
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
$ write sys$output "Invalid argument given to mosplot.com file -- ", primary
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
$   if F$SEARCH("mosplot.imake") .nes. ""
$   then
$      vimake mosplot
$      purge mosplot.bld
$   else
$      if F$SEARCH("mosplot.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mosplot
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mosplot.bld "STD"
$   else
$      @mosplot.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mosplot.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mosplot.com -mixed -
	-s mosplot.f -
	-i mosplot.imake -
	-p mosplot.pdf -
	-t tstmosplot.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mosplot.f
$ DECK/DOLLARS="$ VOKAGLEVE"
        INCLUDE 'VICMAIN_FOR'
        SUBROUTINE MAIN44
        IMPLICIT NONE
        INTEGER	  NCOL,maxpix,maxpts,maxground,maxborder
        integer   PASS, TRUE
        PARAMETER (NCOL = 6,maxpix=200,maxpts=1000)
        parameter (maxground=100,maxborder=25)
        parameter (TRUE = 1, PASS = 1)

        integer left_image,right_image,pair,nground
        integer npict,noverlap,ncol1,ncol3,ind
        integer fds(maxpix),npoly(maxpix),overlap(maxpts,2)
        integer unito,unit3,unit4,file,count,idground
        integer idatal(40),idatar(40),idgeom,idpoints
        integer status,nofile,col,i,j,k,m,n,tiep,ns,nl,incr
        integer line,samp,ibisbuf(maxpts,ncol),iground(maxground,5)
        integer idata(40),idef,icam
        integer inibiso, inibis3
        integer outibis

        real*8 data8l(20),data8r(20)

        REAL rbisbuf(maxpts,NCOL),ground(maxground,5)
        real realover(maxpts,2)
        real x(maxborder+3),y(maxborder+3)
        real corner(2,maxborder,maxpix),xy(2)
        real radpol(maxpix),eqpol(maxpix),focal(maxpix)
        real optaxl(maxpix),optaxs(maxpix),scale(maxpix)
        real rsvec(maxpix,3),omangl(maxpix,3),conv(3600)
        real datal(40),datar(40),xx(4),yy(4)
        real latlon(2),data(40),area(4),save_lat,save_lon
        real exag,rline,rsamp,lat,lon,bignum,cam_real(maxpix)
        real xscale, yscale
        character*5  project
        character*80 string
        character*80 plotout
        character*4  ibisformat(2)

        character*132 msg
        character*132 msg1

        LOGICAL	new,plotit,xvptst,print,in,inside,stereographic
        logical use_area

        !! The equivalence statements below have been determined to be
        !! OK as is when porting to UNIX platforms.  The equivalence 
        !! statment as used, is a 'FORTRAN' way of building a C structure.
        equivalence (ibisbuf,rbisbuf),(data8l,datal,idatal)
        equivalence (data8r,datar,idatar),(ground,iground)
        equivalence (overlap,realover),(idata,data)

        !! Specification for the two columns of the 'overlap' output file
        ibisformat(1) = 'REAL'
        ibisformat(2) = 'REAL'
        do i = 1,3600
           conv(i) = 0.0
        end do
        do i = 1,40
           datal(i) = 0.0
        end do
        rline = 0.0
        rsamp = 0.0
        lat = 0.0
        lon = 0.0
        xscale = 1.0
        yscale = 1.0

        CALL IFMESSAGE('MOSPLOT version Dec 22 2002')
        bignum=1.1e+30

C Get the parameters
        call xvp('EXAG',exag,count)
        new= xvptst('NEW')
        call xvparm('PROJECT',project,n,idef,5)
        call xvpcnt('OUT',count)
        if(count.eq.0) plotit=.true.
        print=xvptst('PRINT')
        call xvp('INCR',incr,count)
        call xvp('NS',ns,count)
        call xvp('NL',nl,count)
        call xvp('LATLON',latlon,count)
        if(count.eq.2)then
          stereographic=.true.
        else
          stereographic=.false.
        endif
        use_area=.false.
        call xvp('AREA',area,count)
        if(count.eq.4) use_area=.true.

        if(new)then
            call xvmessage('Using updated SEDR, columns: 21-23',' ')
        else
            call xvmessage('Using original SEDR, columns: 8-10',' ')
        endif

c Obtain output PostScript filename, or default filename
        call xvparm('PLOTOUT',plotout,n,idef,80)

C       Open output PostScript file  
        call plotfn (plotout)

c initialize XRT/graph
        call xrtbegin (status)
        if (status .ne. 1) then
           call mabend('MOSPLOT failed to initialize XRT/graph')
        endif 

c set word 39 of convev buffer
        if(xvptst('OBJECT'))then
           call xvmessage('Treating images as OBJECT space images',' ')
           idatal(39)=8
           idatar(39)=8
        else
           call xvmessage('Treating images as IMAGE space images',' ')
           idatar(39)=7
           idatal(39)=7
        endif         

c Set the DATA buffer for a stereographic projection for convev.
c we use this to convert from lat,lon to line,samp in a
c stereographic projection.
        if(stereographic)then
           data(1)=500.
           data(2)=500.
           data(3)=latlon(1)
           data(4)=latlon(1)
           data(5)=latlon(1)
           data(6)=latlon(2)
           data(7)=1.0
           data(8)=1.0
           if(latlon(1).lt.0.0) data(8)=-1.0
           data(9)=0.0
           data(25)=1000.
           data(26)=1000.
           idata(39)=4
        endif

c Read in the SEDR.INT file
        call rdfil_readonly(unito,inibiso,1,npict,ncol1,nofile)
        call xvmessage('   ',' ')
        write (msg( 1:23),'(a)') '# images in SEDR file= '
        write (msg(24:26),'(i3)') npict
        call xvmessage (msg,' ')
        if(npict.gt.maxpix)then
           call xvmessage('Too many SEDR entries',' ')
           write (msg( 1:14),'(a)') 'upper limit = '
           write (msg(15:17),'(i3)') maxpix
           call xvmessage (msg,' ')
           call abend
        endif
        if(ncol1.lt.23) then
           call xvmessage('First input file not a SEDR.INT file',' ')
           write (msg( 1:19),'(a)') 'Number of columns= '
           write (msg(20:22),'(i3)') ncol1
           call xvmessage (msg,' ')
           call abend
        endif
        call ibis_column_read(inibiso,fds,1,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        if(ncol1.lt.31) then
           call xvmessage('SEDR.INT file not created by VGRIBIS',' ')
           write (msg( 1:19),'(a)') 'Number of columns= '
           write (msg(20:22),'(i3)') ncol1
           call xvmessage (msg,' ')
           call abend
        endif
        call ibis_column_read(inibiso,cam_real, 4,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,rsvec(1,1),5,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,rsvec(1,2),6,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,rsvec(1,3),7,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        if(new)then    ! new sedr updated by omcor.
          call ibis_column_read(inibiso,omangl(1,1),21,1,npict,status)
          if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
          call ibis_column_read(inibiso,omangl(1,2),22,1,npict,status)
          if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
          call ibis_column_read(inibiso,omangl(1,3),23,1,npict,status)
          if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        else           ! original sedr unmodified.
            call ibis_column_read(inibiso,omangl(1,1),8,1,npict,status)
            if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
            call ibis_column_read(inibiso,omangl(1,2),9,1,npict,status)
            if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
            call ibis_column_read(inibiso,omangl(1,3),10,1,npict,status)
            if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        endif
        call ibis_column_read(inibiso,radpol,26,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,eqpol,27,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,focal,28,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,optaxl,29,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,optaxs,30,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_column_read(inibiso,scale,31,1,npict,status)
        if (status .NE. PASS) call IBIS_SIGNAL (inibiso,status,true)
        call ibis_file_close (INIBISO, 'UDELETE',status)
        if (status.NE.PASS) call IBIS_SIGNAL(inibiso,status,true)

        if(print)then
          do i=1,npict
             msg = ' '
             write (msg( 1:26),'(a)') 'rsvector,omangle, point#= '
             write (msg(27:44),'(i)') i
             call xvmessage (msg,' ')
             write (msg(1:),'(6e12.4)') rsvec(i,1),rsvec(i,2),
     +              rsvec(i,3), omangl(i,1),omangl(i,2),omangl(i,3)
             call xvmessage(msg,' ')
             msg = ' '
             call xvmessage('polr,eqr,foc,optal,optas,scal',' ')
             write (msg(1:),'(6f12.4)') radpol(i),eqpol(i),focal(i),
     +                   optaxl(i),optaxs(i),scale(i)
             call xvmessage(msg,' ')
          enddo 
        endif

c Determine the file IDs of files 2,3 and 4 are (if present).
c idgeom=INP# of geom file, 0 means not there.
c idpoints=INP# of tiepoints file, 0 means not there.
c idground=INP# of ground control points, 0 means not there.

      call getinput_id(idgeom,idpoints,idground)

c check consistency.
      if(project.ne.'GLL  ')then
        if((idatal(39).eq.7).and.(idgeom.eq.0))then
             call xvmessage('IMAGE space requires GEOMA file',' ')
             call abend
        endif
      endif


c Read tiepoints file as input if present.
c ibisbuf(n,1)=left image ID #.
c ibisbuf(n,2)=right image ID #.
c rbisbuf(n,3)=line # of left image.
c rbisbuf(n,4)=sample # of left image.
c rbisbuf(n,5)=line # of right image.
c rbisbuf(n,6)=sample # of right image.
      if(idpoints.gt.0)then
           call rdfil_readonly(unit3,inibis3,idpoints,tiep,ncol3,nofile)
           call xvmessage('   ',' ')
           msg = ' '
           write (msg( 1:28),'(a)') 'points located in tiep file='
           write (msg(29:),'(i)') tiep
           call xvmessage (msg,' ')
           if(tiep.gt.maxpts)then
              call xvmessage('Too many tiepoints',' ')
              write (msg( 1:13),'(a)') 'upper limit ='
              write (msg(14:),'(i)') maxpts
              call xvmessage (msg,' ')
              call abend
           endif
           if(ncol3.ne.12)then
              call xvmessage('Incorrect # cols in tiepoints file',' ')
              call abend
           endif
           do col=1,ncol
              call ibis_column_read (inibis3,rbisbuf(1,col),
     +             col,1,tiep,status)
              if (status.NE.PASS) call IBIS_SIGNAL(inibis3,status,true)
           enddo
           call ibis_file_close (INIBIS3, 'UDELETE',status)
           if (status.NE.PASS) call IBIS_SIGNAL(inibis3,status,true)
           do i=1,tiep
              !! Convert columns 1 & 2 to integer.  The column 1 & 2 values
              !! were originally converted to real*4 before storing into the
              !! file.
              ibisbuf(i,1)=nint(rbisbuf(i,1))
              ibisbuf(i,2)=nint(rbisbuf(i,2))
           enddo
           if(print)then
              msg1 = ' '
              write (msg1( 1:22),'(a)') 'left   right      line'
              write (msg1(23:40),'(a)') '         samp     '
              write (msg1(41:62),'(a)') '   line          samp'
              call xvmessage(msg1, ' ')
              do i=1,tiep
                 write(string,4)ibisbuf(i,1),ibisbuf(i,2),
     +                  rbisbuf(i,3),rbisbuf(i,4),
     +                  rbisbuf(i,5),rbisbuf(i,6)
                 call xvmessage(string,' ')
              enddo
           endif
4          format(1x,2i5,4f12.3)
      endif

c Read ground control file as input if present.
c iground(n,1)=image ID #.
c ground(n,2)=line.
c ground(n,3)=sample.
c ground(n,4)=latitude.
c ground(n,5)=longitude.
      if(idground.gt.0)then
           call rdfil_readonly(unit3,inibis3,idground,
     +                         nground,ncol3,nofile)
           call xvmessage('   ',' ')
           msg = ' '
           write (msg(1:31),'(a)') 'ground control points located= '
           write (msg(32:),'(i)') nground
           call xvmessage (msg,' ')
           if(nground.gt.maxground)then
              call xvmessage(' Too many ground points',' ')
              write (msg( 1:14),'(a)') 'upper limit = '
              write (msg(15:17),'(i3)') maxground
              call xvmessage (msg,' ')
              call abend
           endif
           if(ncol3.ne.5)then
              call xvmessage('Incorrect # cols in ground file',' ')
              call abend
           endif
           do i=1,5
              call ibis_column_read(inibis3,ground(1,i),
     +             i,1,nground,status)
              if (status.NE.PASS) call IBIS_SIGNAL(inibis3,status,true)
           enddo
           do i=1,nground    ! convert to integer
              iground(i,1)=nint(ground(i,1))
           enddo
           call ibis_file_close (INIBIS3, 'UDELETE', STATUS)
           if (status.NE.PASS) call IBIS_SIGNAL(inibis3,status,true)
           if(print)then
              msg1 = ' '
              write (msg1( 1:25),'(a)') 'ID#      line           '
              write (msg1(25:34),'(a)') 'sample   '
              write (msg1(35:59),'(a)') ' latitude      longitude'
              call xvmessage(msg1, ' ')
              do i=1,nground
                 write(string,5) iground(i,1),ground(i,2),
     +                  ground(i,3),ground(i,4),ground(i,5)
                 call xvmessage(string,' ')
              enddo
           endif
5          format(1x,i5,4f12.3)
      endif

c read in the geoma correction file ... an IBIS file.
      if(idgeom.gt.0)then
           call xvunit(unit4,'INP',idgeom,status,' ')
           call geoma2conv (unit4, conv)
           call xvmessage
     +          ('GEOMA image space correction file read ok',' ')
           if(xvptst('OBJECT'))then
              call xvmessage
     +             ('WARNING: geoma file will be ignored',' ')
           endif
      endif

c put 'GLL' into CONV if project is galileo.
      if(project.eq.'GLL  ')then
        call mvcl('GLL  ',conv(1),5)
        DO I=2,npict
          IF (cam_real(I).NE.cam_real(1)) THEN
            call mabend('Images are not taken with the same SSI mode')
          END IF  
        END DO
        icam=cam_real(1)
        IF (icam.EQ.2) THEN !Summation Mode Images, change default NS, NL, INCR
          ! check to see that all NS, NL and INCR are not changed by the user
          call xvp('INCR',incr,count)
          IF (count.EQ.0) THEN
            call xvp('NS', ns, count)
            IF (count.EQ.0) THEN
              call xvp('NL', nl, count)
              IF (count.EQ.0) THEN
                call xvmessage ('Summation Mode images detected', ' ')
                call xvmessage ('  changing parameter NS=>400', ' ')
                call xvmessage ('                     NL=>400', ' ')
                call xvmessage ('                     INCR=>66',' ')
                NS = 400
                NL = 400
                INCR = 66
              END IF
            END IF
          END IF
        END IF
      CALL MVE(4,1,2,CONV(3),icam,1)

      else
         call xvmessage('assuming project is not Galileo',' ')
      endif

c Create footprint plots ************************************
c Loop over images.
      do 30 file=1,npict

c setup SEDR for the latest image.
         call setnav1(file,maxpix,omangl,rsvec,
     +      radpol,eqpol,focal,optaxl,optaxs,scale,data8l,
     +      datal)

c Locate points every INCR around picture border.
         line=1
         samp=1
         k=0
31       if(line.eq.1.and.samp.lt.ns)then
            samp=samp+incr
            if(samp.gt.ns) samp=ns
         else if(samp.eq.ns.and.line.lt.nl)then
            line=line+incr
            if(line.gt.nl) line=nl
         else if(line.eq.nl.and.samp.gt.1)then
            samp=samp-incr
            if(samp.lt.1) samp=1
         else if(samp.eq.1.and.line.gt.1)then
            line=line-incr
            if(line.lt.1) line=1
         else
            call xvmessage('Not supposed to be here',' ')
         endif

c convert from line,samp to lat,lon
         rline=line
         rsamp=samp
         call convev(ind,datal,datal,rline,rsamp,
     +               lat,lon,2,conv)
c        if(ind.ne.0) point is off planet

c convert from lat,lon to line,sample in stereographic proj.
         if((ind.eq.0).and.stereographic)then
            call convev(ind,data,data,rline,rsamp,lat,lon,
     +                  1,conv)
c           if(ind.ne.0)then      !point not visible.
         endif

c Save coordinates for all files.
         if(ind.eq.0)then
           if(stereographic)then
              k=k+1
              corner(1,k,file)=rsamp   ! horizontal
              corner(2,k,file)=rline   ! vertical
           else
              k=k+1
              corner(1,k,file)=lon
              corner(2,k,file)=lat
           endif
           if(k.gt.maxborder)then
              call xvmessage('Too many vertices for routine INSIDE',' ')
              write (msg( 1:14),'(a)') 'upper limit = '
              write (msg(15:17),'(i3)') maxborder
              call xvmessage (msg,' ')
              call xvmessage('increase parameter INCR',' ')
              call abend
           endif
         endif

         if(.not.(line.eq.1.and.samp.eq.1)) goto 31

         npoly(file)=k       ! save # polygon vertices.

30    continue  !! End of DO 30 ... create footprint plots

c     compute the scale for the plots & draw axes.
c     xx(4) & yy(4) contain the limits and scales.
      call plot_scale(corner,npoly,npict,maxborder
     +               ,maxpix,xx,yy,stereographic,
     +                use_area,area,xscale,yscale)
c     draw footprint plots only.
      call plot_foot(corner,x,y,npoly,npict,maxborder
     +              ,maxpix,xx,yy,xscale,yscale)

c compute the tiepoint residuals ********************************
      if(idpoints.gt.0)then
         left_image=0
         right_image=0
         if(print)then
            call xvmessage('   ',' ')
            call xvmessage('Tiepoint residuals',' ')
            call xvmessage
     +           ('point#  left   right    del_lat   del_lon',' ')
         endif
         do 50 pair=1,tiep

c setup sedr for the image pair.
            if(left_image.eq.ibisbuf(pair,1).and.
     +         right_image.eq.ibisbuf(pair,2))then
            else
               left_image=ibisbuf(pair,1)
               right_image=ibisbuf(pair,2)
               call setnav(left_image,right_image,
     +         maxpix,omangl,rsvec,
     +         radpol,eqpol,focal,optaxl,optaxs,scale,data8l,data8r,
     +         datal,datar)
            endif

c compute lat,lon for each image

c left image
            rline=rbisbuf(pair,3)
            rsamp=rbisbuf(pair,4)
            ind = 0
            call convev(ind,datal,datal,rline,rsamp,
     +               lat,lon,2,conv)
            if(ind.ne.0)then
               write (msg(1:),'(a)') 'Left pnt off planet, pt#='
               write (msg(26:),'(i)') pair
               call xvmessage (msg,' ')
               rbisbuf(pair,3)=bignum
               goto 50
            else
               rbisbuf(pair,3)=lat
               rbisbuf(pair,4)=lon
            endif
            save_lat=lat
            save_lon=lon

c convert to projection in line,sample
            if(stereographic)then
               call convev(ind,data,data,rline,rsamp,lat,lon,
     +                     1,conv)
               if(ind.ne.0)then
                  write (msg(1:),'(a)') 'Left pnt not visbl, pt#='
                  write (msg(26:),'(i)') pair
                  call xvmessage (msg,' ')
                  rbisbuf(pair,3)=bignum
                  goto 50
               else
                  rbisbuf(pair,3)=rline
                  rbisbuf(pair,4)=rsamp
               endif
            endif

c right image
            rline=rbisbuf(pair,5)
            rsamp=rbisbuf(pair,6)
            call convev(ind,datar,datar,rline,rsamp,
     +               lat,lon,2,conv)
            if(ind.ne.0)then
               write (msg(1:),'(a)') 'Right pnt off planet, pt#='
               write (msg(28:),'(i)') pair
               call xvmessage (msg,' ')
               rbisbuf(pair,3)=bignum
               goto 50
            else
               rbisbuf(pair,5)=lat
               rbisbuf(pair,6)=lon
            endif

c convert to projection in line,sample
            if(stereographic)then
               call convev(ind,data,data,rline,rsamp,lat,lon,
     +                     1,conv)
               if(ind.ne.0)then
                  write (msg(1:),'(a)') 'Right pnt not visbl, pt#='
                  write (msg(27:),'(i)') pair
                  call xvmessage (msg,' ')
                  rbisbuf(pair,3)=bignum
                  goto 50
               else
                  rbisbuf(pair,5)=rline
                  rbisbuf(pair,6)=rsamp
               endif
            endif

           if(print)then
                 write(string,7)pair,left_image,right_image,
     +                  save_lat - lat,
     +                  save_lon - lon
                 call xvmessage(string,' ')
           endif
7          format(1x,3i5,2f12.3)

50       continue

c Convert the ground control points from line,sample
c to lat,lon  to  compare with the given lat,lon.
         if(idground.gt.0)then
           file=0
           if(print)then
              call xvmessage('   ',' ')
              call xvmessage('Ground control residuals',' ')
              call xvmessage('file#   del_lat   del_lon',' ')
           endif
           do 70 i=1,nground

c setup sedr for frame numbers
              if(file.ne.iground(i,1))then
                file=iground(i,1)
                call setnav1(file,maxpix,omangl,rsvec,
     +            radpol,eqpol,focal,optaxl,
     +            optaxs,scale,data8l,
     +            datal)
              endif

c convert from line,samp to lat,lon
              rline=ground(i,2)
              rsamp=ground(i,3)
              ind = 0
              call convev(ind,datal,datal,rline,rsamp,
     +                    lat,lon,2,conv)
              if(ind.ne.0)then
                  write (msg(1:),'(a)') 'Ground pt off planet, pt#='
                  write (msg(27:),'(i)') i
                  call xvmessage (msg,' ')
                 ground(i,2)=bignum
                 goto 70
              else
                 ground(i,2)=lat  ! replace line/samp by lat/lon
                 ground(i,3)=lon
              endif
              save_lat=lat
              save_lon=lon

c convert to projection in line,sample
              if(stereographic)then
                 call convev(ind,data,data,rline,rsamp,lat,lon,
     +                     1,conv)
                 if(ind.ne.0)then
                    write (msg(1:),'(a)') 'Ground pt not visbl, pt#='
                    write (msg(27:),'(i)') i
                    call xvmessage (msg,' ')
                    ground(i,2)=bignum
                    goto 70
                 else
                    ground(i,2)=rline
                    ground(i,3)=rsamp
                 endif
              endif

c convert given lat,lon to projection in line,sample
              if(stereographic)then
                 lat=ground(i,4)
                 lon=ground(i,5)
                 call convev(ind,data,data,rline,rsamp,lat,lon,
     +                     1,conv)
                 if(ind.ne.0)then
                    write (msg(1:),'(a)') 'Ground pt not visbl, pt#='
                    write (msg(27:),'(i)') i
                    call xvmessage (msg,' ')
                    ground(i,2)=bignum
                    goto 70
                 else
                    ground(i,4)=rline
                    ground(i,5)=rsamp
                 endif
              endif

              if(print)then
                 write(string,8)file,
     +                  save_lat - lat,
     +                  save_lon - lon
                 call xvmessage(string,' ')
              endif
8             format(1x,i5,2f12.3)

70          continue
         endif

         call plot_resid(rbisbuf,maxpts,ncol,tiep,xx,yy,
     +                   exag,idground,ground,maxground,
     +                   nground,xscale,yscale)

      endif              

c Compute overlap & write output file *****************************
      call xvpcnt('OUT',count)
      if(count.gt.0) then

c Create the overlap buffer
        m=0
        do 40 i=1,npict
           do 41 j=1,npict
              if(i.eq.j)goto 41
              do 42 k=1,npoly(j)
                 xy(1)=corner(1,k,j)
                 xy(2)=corner(2,k,j)
                 in=inside(xy,corner(1,1,i),npoly(i))
                 if(in)then
                    do 43 n=1,m  ! reject redundancies
                       if((overlap(n,1).eq.j).and.(overlap(n,2)
     +                       .eq.i)) goto 41
43                  continue
                    if(m.eq.maxpts)then
                       call xvmessage('Overlap buffer exceeded',' ')
                       call abend
                    endif
                    m=m+1
                    overlap(m,1)=i
                    overlap(m,2)=j
                    goto 41
                 endif
42            continue
41         continue
40      continue
        noverlap=m
        if(print)then
           call xvmessage('   ',' ')
           call xvmessage('Overlap file contents:',' ')
           call xvmessage('Left-file#   Right-file#',' ')
           do i=1,noverlap
              write(string,6) overlap(i,1),overlap(i,2)
              call xvmessage(string,' ')
           enddo
        endif
6       format(1x,2i10)

c  WRITE OUT OVERLAP FILE.
        CALL XVUNIT(UNITO,'OUT',1,STATUS,' ')
        IF (STATUS.NE.1) THEN
           msg1 = ' '
           write (msg1( 1:31),'(a)') 'OUTPUT INITIALIZATION ERROR - '
           write (msg1(32:62),'(a)') '            PROGRAM TERMINATED'
           call xvmessage(msg1, ' ')
           CALL ABEND
        END IF
        CALL IBIS_FILE_OPEN (UNITO,OUTIBIS,'WRITE',2,noverlap,
     +                       ibisformat,' ',STATUS)
        IF (STATUS .NE. PASS) CALL IBIS_SIGNAL_U(UNITO,STATUS,TRUE)
        DO COL = 1, 2
          do i=1,noverlap                  ! convert to real
             realover(i,col)=overlap(i,col)
          enddo
          call ibis_column_write (OUTIBIS, realover(1,col), col,
     +       1,noverlap,status)
       ENDDO
        CALL IBIS_FILE_CLOSE (OUTIBIS,'UDELETE',STATUS)
        IF (STATUS .NE. PASS) CALL IBIS_SIGNAL_U(OUTIBIS,STATUS,TRUE)
      endif

c close XRT/graph
      call plot(0,0,999)

      return
      end


c *********************************************************

      SUBROUTINE geoma2conv (unit, conv)
      integer unit
      real    conv(*)

      integer  colcount
      character*8 nahstr, navstr, tiestr 

      nahstr = 'NAH'
      navstr = 'NAV'
      tiestr = 'TIEPOINT'
      colcount = 4

      call mvcl (nahstr, conv(1), 8)
      call mvcl (navstr, conv(4), 8)
      call mvcl (tiestr, conv(7), 8)

!!  FORTRAN Calling Sequence:  CALL IREAD_TIEPOINTS(UNIT,NAH,NAV,MAXPTS,
!!                                                  TIEPOINTS,COLCOUNT)

      conv(6) = 0.0
      call iread_tiepoints (unit, conv(3), conv(6), 3600,
     +                      conv(9), colcount)

      return
      end 

c *********************************************************
	SUBROUTINE RDFIL_READONLY (UNIT,INIBIS,INST,CLEN,NCOL,NOFILE)
c IBIS open
C Version for no update mode (read only).

	INTEGER UNIT,CLEN,NCOL,INST
	LOGICAL NOFILE
c	INTEGER DEF,STATUS,inibis
	INTEGER STATUS,inibis
	CHARACTER*200 FILENAME

      character*132 msg1

      CALL XVPONE('INP',FILENAME,INST,200)
C
      CALL XVUNIT (UNIT,'XXX',INST,STATUS,'U_NAME',FILENAME,' ')
      IF (STATUS.NE.1) THEN
        msg1 = ' '
        write (msg1( 1:35),'(a)') ' INPUT FILE INITIALIZATION ERROR - '
        write (msg1(36:66),'(a)') '             PROGRAM TERMINATED'
        call xvmessage(msg1, ' ')
        call abend
      END IF
      CALL IBIS_FILE_OPEN (UNIT,INIBIS,'READ',0,0,' ',' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(INIBIS,STATUS,1)
      CALL IBIS_FILE_GET(INIBIS,'NR',CLEN,1,1)
      CALL IBIS_FILE_GET(INIBIS,'NC',NCOL,1,1)
	  RETURN
	  END


c*****************************************************************

      subroutine plot_resid(rbisbuf,maxpts,ncol,tiep,xx,yy,
     +                      exag,idground,ground,maxground,
     +                      nground,xscale,yscale)
c  plot residuals for each tiepoint & ground cntl pt.
c  NOTE: if stereographic is true then all latitudes are really lines
c       and all longitudes are really samples.
        integer*4 tiep
        real*4 ground(maxground,5)
        real*4 rbisbuf(maxpts,ncol),xx(4),yy(4)
        real   xscale, yscale
        character*1 text

        flag=1.0e+30

c rbisbuf(n,3)=left pix latitude
c rbisbuf(n,4)=left pix longitude
c rbisbuf(n,5)=right pix latitude
c rbisbuf(n,6)=right pix longitude
!### jj
c plot tiepoint residuals
        do 10 j=1,tiep
          if(rbisbuf(j,3).gt.flag)goto 10
          x1=rbisbuf(j,4)/xscale
          y1=rbisbuf(j,3)/yscale
          call number(x1,y1,.14,real(j),0.0,-1)

          !! Put circle on plot 
          call symbolanchor (9) !! XRT_ANCHOR_HOME
          text = 'o'
          call symbol(x1,y1,.14,text,1,0.0,-1)
          call symbolanchor (8) !! XRT_ANCHOR_BEST

          call plot(x1,y1,3)
          x=rbisbuf(j,6)/xscale
          y=rbisbuf(j,5)/yscale
          x=exag*(x-x1)+x1
          y=exag*(y-y1)+y1
          call plot(x,y,2)
10      continue 

c ground(n,2)=computed latitude using SEDR
c ground(n,3)=computed longitude using SEDR
c ground(n,4)=given latitude 
c ground(n,5)=given longitude 

c plot ground control point residuals
        if(idground.gt.0)then
         do 20 j=1,nground
          if(ground(j,2).gt.flag)goto 20
          x1=ground(j,3)/xscale
          y1=ground(j,2)/yscale

          call number(x1,y1,.14,real(j),0.0,-1)

          !! Put 'x' on plot 
          call symbolanchor (9) !! XRT_ANCHOR_HOME
          text = 'x'
          call symbol(x1,y1,.14,text,2,0.0,-1)
          call symbolanchor (8) !! XRT_ANCHOR_BEST
          call plot(x1,y1,3)
          x=ground(j,5)/xscale
          y=ground(j,4)/yscale
          x=exag*(x-x1)+x1
          y=exag*(y-y1)+y1
          call plot(x,y,2)
20       continue 
        endif

        call plot( 0.0, 0.0,-3)
        return
        end

c****************************************************************

      subroutine plot_scale(corner,npoly,npict,
     +      maxborder,maxpix,xx,yy,stereographic,
     +      use_area,area,xscale,yscale)
c  Compute scale and draw axes of plots..
        logical stereographic,use_area
        real*4 xx(4),yy(4),corner(2,maxborder,maxpix),area(4)
        real   firstv, xscale,yscale,temp
        integer*4 npoly(maxpix)
        character*20 datestr
        integer*2 flag
        integer   II

        character*15 line
        character*15 sample
        character*8 timestr
        character*44 banner
        integer status
        integer valscale
        
        status = 1
c Establish the axis lengths
       xrange=1.0 
       yrange=1.0 

c title
        banner  = ' '
        timestr = ' '
        flag = 2
        II = 0
        call datfmt (flag, datestr, II)
        call time(timestr)
        call xvmessage (datestr,' ')
        call xvmessage (timestr,' ')
        write (BANNER( 1:15),'(a15)') 'FOOTPRINT PLOT '
        write (BANNER(16:33),'(a17)')  datestr 
        write (BANNER(34:41),'(a8 )')  timestr
        call header (banner, 1, 1) !! Title string, 1 line, adjust center
        call xvmessage(banner,' ')

c compute plot limits
        if (stereographic) then
           xx(1)=1.e+30
           xx(2)=-1.e+30
           yy(1)=1.e+30
           yy(2)=-1.e+30
           if (use_area) then
             xx(1)=area(3)
             xx(2)=area(4)
             yy(1)=area(1)
             yy(2)=area(2)
           else
             do i=1,npict
               do j=1,npoly(i)
                 if(corner(1,j,i).lt.xx(1)) xx(1)=corner(1,j,i)
                 if(corner(2,j,i).lt.yy(1)) yy(1)=corner(2,j,i)
                 if(corner(1,j,i).gt.xx(2)) xx(2)=corner(1,j,i)
                 if(corner(2,j,i).gt.yy(2)) yy(2)=corner(2,j,i)
               enddo
             enddo
           endif
           temp=yy(1)
           yy(1)=yy(2)
           yy(2)=temp
        else
           xx(1)=360.
           xx(2)=0.0
           yy(1)=-90.
           yy(2)=90.
        endif

c compute scales
        xx(3)=xx(1)
        xx(4)=(xx(2)-xx(1))
        yy(3)=yy(1)
        yy(4)=(yy(2)-yy(1))

c plot axes & set scales
        n=0
        if(stereographic)then
           firstv = xx(4)
           line   = ' '
           write (line(1:4),'(a)') 'line'
c          find the scaling for the annotation values
           valscale = int(alog10(abs(firstv)))
           if (valscale .ge. -1  .and.  valscale .le. 1) valscale = 0

c          if the axis labels are scaled then print the exponent value 
           xscale = 1.0
           if (valscale .ge. 3) then
              xscale = 10**valscale
              write (line( 5:10), '(a)') ' *10**'   
              write (line(11:11 ),'(i1)') valscale   
           endif

           firstv = yy(4)
           sample = ' '
           write (sample(1:6),'(a)') 'sample'
c          find the scaling for the annotation values
           valscale = int(alog10(abs(firstv)))
           if (valscale .ge. -1  .and.  valscale .le. 1) valscale = 0

c          if the axis labels are scaled then print the exponent value 
           yscale = 1.0
           if (valscale .ge. 3) then
              yscale = 10**valscale
              write (sample( 7:12), '(a)') ' *10**'   
              write (sample(13:13 ),'(i1)') valscale   
           endif

           temp = xscale
           xscale = yscale
           yscale = temp

           call axestitles (sample,line,90,' ',0)
           !! Reverse X axis annotation ... increase from right to left
           call axesreverse (0, 1)  
           !! Set origins to cross at XRT_ORIGIN_MIN, Y=XRT_ORIGIN_MAX
           call axesoriginplacement (2,3)
        else
           !! Display all three axes ... X, Y, & Y2 axes
           call displayaxes (1, 1, 1)
           !! Reverse X axis annotation ... increase from right to left
           call axesreverse (1, 0)  
           !! Set X axis at XRT_ORIGIN_ZERO & set Y axis at XRT_OTIGIN_MIN
           call axesoriginplacement (1,2)
           !! Label the X, Y, & Y2 axes
           call axestitles ('West Longitude','latitude',270,
     &                      'latitude',90)
           call plot(  0.0, 0.0, -3)
           call plot(  0.0, 90.0, 3)
           call plot(  0.0,-90.0, 3)
           call plot(  0.0,  0.0, 3)
           call plot(360.0,  0.0, 3)
           call setaxesmaximums (360.0,  90.0,  90.0)
           call setaxesminimums (  0.0, -90.0, -90.0)
        endif
       
        return
        end

c*****************************************************************
        subroutine plot_foot(corner,xbuf,ybuf,npoly,npict,
     +      maxborder,maxpix,xx,yy,xscale,yscale)
c  plot footprints for each frame.
        real*4 xbuf(maxborder+3),ybuf(maxborder+3)
        real*4 xx(4),yy(4),corner(2,maxborder,maxpix)
        real   xscale,yscale
        integer*4 npoly(maxpix)

c plot each footprint.
        do i=1,npict
          n=npoly(i)
          do j=1,n                      ! copy data from each footprint
             xbuf(j)=corner(1,j,i)
             ybuf(j)=corner(2,j,i)
          enddo
          val=i
        
          x=(xbuf(n))/xscale
          y=(ybuf(n))/yscale
          call number(x,y,.14,val,0.0,-1)  ! plot file number
          n=n+1
          xbuf(n)=xbuf(1)     ! repeat last point to close polygon
          ybuf(n)=ybuf(1)
          xbuf(n+1)= 0.0      ! add scale & offset
          xbuf(n+2)= 1.0
          ybuf(n+1)= 0.0
          ybuf(n+2)= 1.0
          if (xscale .ne. 1.0) then
             do II = 1, n
                xbuf(II) = xbuf(II)/xscale
             enddo
          endif
          if (yscale .ne. 1.0) then
             do II = 1, n
                ybuf(II) = ybuf(II)/yscale
             enddo
          endif
          call line(xbuf,ybuf,n,1,0,3)  ! plot polygon
        enddo

        call plot(0.0,0.0,-3)
        return
        end

c **************************************************************
        subroutine setnav(left,right,
     +          maxpix,omangl,rsvec,
     +          radpol,eqpol,focal,optaxl,optaxs,scale,data8l,data8r,
     +          datal,datar)
c Fills the DATAL & DATAR for convev subroutine.
        real*8 r81,r82,r83,data8l(20),data8r(20)
        integer*4 left,right
        real*4 omangl(maxpix,3),rsvec(maxpix,3),radpol(1),eqpol(1)
        real*4 focal(1),optaxl(1),optaxs(1),scale(1),datal(40),datar(40)

c left image
        i=left
        r81=omangl(i,1)
        r82=omangl(i,2)
        r83=omangl(i,3)
        call fromeuler(r81,r82,r83,data8l)
        data8l(10)=rsvec(i,1)
        data8l(11)=rsvec(i,2)
        data8l(12)=rsvec(i,3)
        datal(25)=radpol(i)
        if(datal(25).eq.0.)call xvmessage('WARNING: SEDR is blank',' ')
        datal(26)=eqpol(i)
        datal(27)=focal(i)
        datal(28)=optaxl(i)
        datal(29)=optaxs(i)
        datal(30)=scale(i)         
c right image
        i=right
        r81=omangl(i,1)
        r82=omangl(i,2)
        r83=omangl(i,3)
        call fromeuler(r81,r82,r83,data8r)
        data8r(10)=rsvec(i,1)
        data8r(11)=rsvec(i,2)
        data8r(12)=rsvec(i,3)
        datar(25)=radpol(i)
        if(datar(25).eq.0.)call xvmessage('WARNING: SEDR is blank',' ')
        datar(26)=eqpol(i)
        datar(27)=focal(i)
        datar(28)=optaxl(i)
        datar(29)=optaxs(i)
        datar(30)=scale(i)         
        return
        end

c **************************************************************
        subroutine setnav1(file,maxpix,omangl,rsvec,
     +          radpol,eqpol,focal,optaxl,optaxs,scale,data8l,
     +          datal)
c Fills the DATAL for convev subroutine.
        real*8 r81,r82,r83,data8l(20)
        integer*4 file
        real*4 omangl(maxpix,3),rsvec(maxpix,3),radpol(1),eqpol(1)
        real*4 focal(1),optaxl(1),optaxs(1),scale(1),datal(40)

        i=file
        r81=omangl(i,1)
        r82=omangl(i,2)
        r83=omangl(i,3)
        call fromeuler(r81,r82,r83,data8l)
        data8l(10)=rsvec(i,1)
        data8l(11)=rsvec(i,2)
        data8l(12)=rsvec(i,3)
        datal(25)=radpol(i)
        if(datal(25).eq.0.)call xvmessage('WARNING: SEDR is blank',' ')
        datal(26)=eqpol(i)
        datal(27)=focal(i)
        datal(28)=optaxl(i)
        datal(29)=optaxs(i)
        datal(30)=scale(i)         
        return
        end

c *************************************************************

      subroutine getinput_id(idgeom,idpoints,idground)
c returns the INP # of the geom & tiepoints files if present
      integer count,unit,status,inibis,nah

      character*132 msg
      nah = 0
      call xvpcnt('INP',count)
      idgeom=0
      idpoints=0
      idground=0
      if(count.lt.2)return
      do 9999 i=2,count
        call xvunit(unit,'INP',i,status,' ')
        CALL IBIS_FILE_OPEN (UNIT,INIBIS,'READ',0,0,' ',' ',STATUS)

        if (status .ne. 1) then
           write (msg( 1:36),'(a)')
     +            'neither an IBIS or GEOMA file, inp# '
           write (msg(37:39),'(i3)') i
           call xvmessage (msg,' ')
           call abend
        end if
 
C      Get area size fields...
       NAH = 0       ! 0 IF NOT IN LABEL.
       CALL XLGET(UNIT,'PROPERTY','NUMBER_OF_AREAS_HORIZONTAL',
     &        NAH,STATUS,'FORMAT','INT','PROPERTY','TIEPOINT',' ')

       if (status .eq. 1) then
          idgeom=i
       else 
          if(idpoints.eq.0)then
             idpoints=i
          else
             idground=i
          endif
       endif
       call xvclose(unit,status,' ')
       call ibis_file_close (INIBIS, 'UDELETE',status)
9999   enddo
       return
       end


c **************************************************************

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
	real*8	cos_kappa, sin_kappa,dtr
        dtr = 3.141592653589793D0/180.d0
	sin_alpha = sin(alpha*dtr)
	cos_alpha = cos(alpha*dtr)
	sin_delta = sin(delta*dtr)
	cos_delta = cos(delta*dtr)
	sin_kappa = sin(kappa*dtr)
	cos_kappa = cos(kappa*dtr)
	c(1,1) = -sin_alpha * cos_kappa - cos_alpha * sin_delta * sin_kappa
	c(1,2) =  cos_alpha * cos_kappa - sin_alpha * sin_delta * sin_kappa
	c(1,3) =  cos_delta * sin_kappa
	c(2,1) =  sin_alpha * sin_kappa - cos_alpha * sin_delta * cos_kappa
	c(2,2) = -cos_alpha * sin_kappa - sin_alpha * sin_delta * cos_kappa
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
$ create mosplot.imake
#define PROGRAM  mosplot

#define MODULE_LIST mosplot.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN
#define FTN_STRING
#define LIB_MATH77
#define LIB_MOTIF
#define LIB_XRT_GRAPH
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/* #define DEBUG      /* Remove upon deliver */
/* #define LIB_LOCAL  /* Remove upon delivery */
$ Return
$!#############################################################################
$PDF_File:
$ create mosplot.pdf
PROCESS HELP=*
 PARM INP      STATUS=STRING  COUNT=(1:4)
 PARM OUT      STATUS=STRING  COUNT=(0:1) DEFAULT=--
 PARM PLOTOUT  STATUS=STRING  COUNT=(0:1) DEFAULT="mosplot.psf"
 PARM PROJECT  STATUS=(STRING,5) COUNT=(0:1) DEFAULT=-- +
     VALID=("VGR-1","VGR-2","MAR10","MAR-9","GLL  ","VIKOR")
 PARM EXAG     STATUS=REAL    DEFAULT=10.
 PARM INCR     STATUS=INTEGER DEFAULT=133
 PARM NL       STATUS=INTEGER DEFAULT=800
 PARM NS       STATUS=INTEGER DEFAULT=800
 PARM NEW      STATUS=KEYWORD VALID=(NEW,OLD) DEFAULT=OLD
 PARM OBJECT   STATUS=KEYWORD VALID=(OBJECT,IMAGE) DEFAULT=IMAGE
 PARM PRINT    STATUS=KEYWORD VALID=(PRINT,NOPRINT) DEFAULT=NOPRINT
 PARM LATLON   STATUS=REAL COUNT=(0,2) DEFAULT=--
 PARM AREA     STATUS=REAL    COUNT=(0,4) DEFAULT=--
 PARM NODISP   STATUS=KEYWORD COUNT=(0,1) VALID=NODISP DEFAULT=--
END-PROC

.TITLE
VICAR/IBIS Program MOSPLOT

.HELP
PURPOSE:
  MOSPLOT is a program which performs three functions to assist
  the user in the generation of a mosaic. These functions are:
1. To plot the footprints of all the frames in a mosaic.
2. To create an overlap file which contains all of the 
   overlapping frame pairs in the mosaic.
   This file is needed by MANMATCH to acquire tiepoints.
3. To plot the residuals of tiepoints before or after the
   global SEDR has been corrected using program OMCOR.
   Also to plot ground control points.

.page

   A graphic image is displayed automatically.  To suppress the automatic
   graphics image display, the keyword 'NODISP must appear on the VICAR
   command line. When the 'NODISP keyword is included in the VICAR command
   line, the output graphics image is automatically output in PostScript
   format to a file specified by PLOTOUT. If an output PostScript filename
   is not specified via PLOTOUT, the filename will default to 'MOSPLOT.PSF'.
   To print the PostScript file; enter 'qpr mosplot.psf' on the system
   command line.


.page
EXECUTION

To produce a footprint plot showing entire planet:
Image space:
      MOSPLOT INP=(SEDR.INT,GEOM.IMG) 
Object space:
      MOSPLOT INP=SEDR.INT 'OBJECT 

To produce a footprint plot showing only area of interest:
(Oblique stereographic projection )
Image space:
      MOSPLOT INP=(SEDR.INT,GEOM.IMG) LATLON=(45.2,167.0)
Object space:
      MOSPLOT INP=SEDR.INT 'OBJECT LATLON=(45.2,167.0)

To generate an overlap file:
Image space:
      MOSPLOT INP=(SEDR.INT,GEOM.IMG) OUT=OVER.INT 
Object space:
      MOSPLOT INP=SEDR.INT OUT=OVER.INT 'OBJECT 
NOTE: You may prefer to use the Stereographic projection mode
to create the overlap file because it does not suffer from
ambiguities when the poles or the prime meridian is contained
within the mosaic. ie:
      MOSPLOT INP=(SEDR.INT,GEOM.IMG) OUT=OVER.INT LATLON=(45.2,167.0) 

To produce a tiepoint residuals plot:
Image space:
      MOSPLOT INP=(SEDR.INT,MATCH.INT,GEOM.IMG) 
Object space:
      MOSPLOT INP=(SEDR.INT,MATCH.INT) 'OBJECT

To produce a residuals plot including ground control:
Image space:
      MOSPLOT INP=(SEDR.INT,MATCH.INT,GROUND.INT,GEOM.IMG) 
Object space:
      MOSPLOT INP=(SEDR.INT,MATCH.INT,GROUND.INT) 'OBJECT

Note: All the files are IBIS tabular files.

.page
EXAMPLE: 
  The following example creates an updated SEDR which can then 
be used by MAP2 to create an accurate mosaic. The mosaic 
consists of 4 Ganymede frames from voyager.          

(create the ibis sedr file)
  vgribis out=sedr.int fds=(2063559,2063602,2063611,2063614) +
         camera=5 enc=JUPITER

(make first footprint plot to get center of projection)
  mosplot inp=sedr.int 'object nl=1000 ns=1000

(make stereographic plot and create overlap file)
  mosplot inp=sedr.int out=over.int 'object +
         nl=1000 ns=1000 latlon=(-18.,184.)

(generate tiepoints file interactively)
  manmatch inp=(sedr.int,over.int) out=match.int +
         dir=ud3:[cca314] 'sedr 'object

(create ground control points)
  getgcp out=ground.int linc=250 sinc=250 +
         enc=JUPITER fds=2063559 camera=5 id=1 +
         sedr=sedr

(plot tiepoints & ground control using old sedr)
  mosplot inp=(sedr.int,match.int,ground.int) 'object +
         nl=1000 ns=1000 exag=10. latlon=(-18.,184.)

(iteratively correct the ibis sedr file)
  omcor inp=(sedr.int,match.int,ground.int) planet=JUPITER +
         mission=VOYAGER maxiter=(4,3) weight=0 gcpfac=0.99 +
         omcol=8 outomcol=21
  omcor inp=(sedr.int,match.int,ground.int) planet=JUPITER +
         mission=VOYAGER maxiter=(4,3) weight=0 gcpfac=0.8 +
         omcol=21 outomcol=21
  omcor inp=(sedr.int,match.int,ground.int) planet=JUPITER +
         mission=VOYAGER maxiter=(4,3) weight=0 gcpfac=0.7 +
         omcol=21 outomcol=21
  omcor inp=(sedr.int,match.int,ground.int) planet=JUPITER +
         mission=VOYAGER maxiter=(4,3) weight=0 gcpfac=0.6 +
         omcol=21 outomcol=21
  omcor inp=(sedr.int,match.int,ground.int) planet=JUPITER +
         mission=VOYAGER maxiter=(4,3) weight=0 gcpfac=0.5 +
         omcol=21 outomcol=21
  omcor inp=(sedr.int,match.int,ground.int) planet=JUPITER +
         mission=VOYAGER maxiter=(4,3) weight=0 gcpfac=0.4 +
         omcol=21 outomcol=21
  omcor inp=(sedr.int,match.int,ground.int) planet=JUPITER +
         mission=VOYAGER maxiter=(4,3) weight=0 gcpfac=0.3 +
         omcol=21 outomcol=21
  omcor inp=(sedr.int,match.int,ground.int) planet=JUPITER +
         mission=VOYAGER maxiter=(4,3) weight=0 gcpfac=0.2 +
         omcol=21 outomcol=21
  omcor inp=(sedr.int,match.int,ground.int) planet=JUPITER +
         mission=VOYAGER maxiter=(4,3) weight=0 gcpfac=0.1 +
         omcol=21 outomcol=21
  omcor inp=(sedr.int,match.int,ground.int) planet=JUPITER +
         mission=VOYAGER maxiter=(4,3) weight=0 gcpfac=0.0 +
         omcol=21 outomcol=21

(plot the tiepoints & ground control using the corrected sedr)
  mosplot inp=(sedr.int,match.int,ground.int) 'new 'object +
         nl=1000 ns=1000 exag=100. latlon=(-18.,184.)

(correct the archival sedr from the ibis sedr)
  omupdate inp=sedr.int 'update encountr=jupiter camera=5

  *** place the MAP2 and mosaic operations here ***


.PAGE
ON THE PLOT FORMATS:
  The footprint plots are closed polygons from points
taken around the border of each frame. A number is printed
at the line=1, sample=1 corner of each frame. This is the
frame number, numbered from 1 to n in the same order of
the FDS times input to VGRIBIS.
  If the user specifies the LATLON keyword then the plot will
be an oblique stereographic projection. If LATLON is not
specified then the plot will show the entire planet.
The line and samples in the stereographic projection are
arbitrarily based upon a scale of one degree/pixel.
The stereographic projection does not suffer from problems
if the poles or prime meridian is in the mosaic.
  The residuals plots are superimposed on the footprint plots.
They represent tiepoint vectors drawn from the left image lat,lon
to the right image lat,lon and are exaggerated by EXAG in
the direction of the right image point. By left & right I mean
the order of the frames input to MANMATCH in pairs.
A CIRCLE symbol is printed at the base of each vector
along with the number of the point.
(left image position).
  If ground control file is present then the ground control
points are added to the above as vectors starting at the
computed lat,lon and extending to the given lat,lon.
They are also exaggerated. A TRIANGLE symbol is placed at the
base of each vector (computed lat,lon position)
along with the number of the point.

.PAGE
FILE STRUCTURE:
  The files are IBIS format tabular files. They consist
of 512 byte records where each column of data is written as
sequential records until exhausted. The next column begins at the
start of the next record etc. Record #1 contains the number of points
per column. All data is real*4 binary.

.page
COGNIZANT PROGRAMMER:  J J Lorre

REVISION HISTORY:
WHO  WHEN      WHAT
--- ---------  ----------------------------------------------------------------
GMY 22 Dec 02  Made portable to Linux
SMC 10 Oct 96  Added Summation mode capability, FR89818
JCT  7 Jul 95  (CRI) Made portable for UNIX and added XRT/graph interface
RGD 10 may 91  Removed r3lib in link
JJL 24 sep 90  Axes reversed, Display device selectble
JJL 26 may 90  Conversion to GLL, test file update
JJL  7 mar 90  Corrected date & time in plot
JJL  6 sep 89  Added area keyword.

.LEVEL1
.VARIABLE INP
All inputs are IBIS tabular files.

First input= SEDR.INT made
 by program VGRIBIS
Second input= MATCH.INT made
 by program MANMATCH
( optional )
Third input= GROUND.INT made
 by program GETGCP
( optional )
Fourth input=GEOM.IMG made
for program GEOMA.
( optional )

.VARIABLE OUT
The overlap ibis file. 
(optional) two columns
output: col1=first frame
number of a pair
col2=second frame number
of a pair

.VARIABLE PROJECT
Specifies the project as
GLL, VIKOR, MAR10, VGR-1,
VGR-2, or MAR-9. Only used
if the input images are in
image space and project is
GLL.

.VARIABLE EXAG
The exaggeration factor
used in plotting tiepoint
residuals.

.VARIABLE PLOTOUT
to specify the output
PostScript filename
for plot images

.VARIABLE NEW
To select the new SEDR
OMmatrix. Default is to
select the  OLD SEDR
OMmatrix

.VARIABLE PRINT
To print the contents of
all files read and written

.VARIABLE INCR
The spacing in pixels
between  border points for
plots.

.VARIABLE NL
the number of lines 
in each image

.VARIABLE NS
the number of samples 
in each image

.VARIABLE OBJECT
the images going into 
the mosaic are
geometrically corrected 

.VARIABLE LATLON
Center of projection
for stereographic
plot.

.VARIABLE AREA
Specifies a portion of 
the stereographic 
projection to plot. AREA
is followed by 4  numbers
indicating: upper line,
lower line,  left sample,
right sample.

.VARIABLE NODISP
Keyword used to state that
plots are not to be 
displayed. However plots
will be saved in the
output PostScript 
file.

.LEVEL2

.VARIABLE INP
All inputs are IBIS tabular files.

SEDR.INT made by program VGRIBIS
This is an IBIS file containing the SEDR for
each frame in the mosaic.
( required as first input )

MATCH.INT made by program MANMATCH
This is an IBIS file containing the tiepoints
connecting the frames in the mosaic.
( optional )

GROUND.INT made by program GETGCP
This is an IBIS file containing the locations and
lat,lon coordinates of ground control points.
NOTE: if ground.int is provided it must follow
match.int.
( optional )

GEOM.IMG made for program GEOMA.
This is a geometric correction file. It is
required if the input files are IMAGE space
(un-geometric-corrected) images.
( optional )

NOTE on the order of the inputs:
   All the inputs but SEDR.INT are optional. They may 
occur in any order provided that:
1. SEDR.INT is first.
2. If GROUND.INT is provided it must follow MATCH.INT.

.VARIABLE OUT
The overlap ibis file.( optional )
two columns output:
col1=first frame number of a pair
col2=second frame number of a pair
The frame numbers run from 1 to N beginning with the
first entry in the SEDR.INT file created by program
VGRIBIS and ending with the Nth entry.

.VARIABLE PLOTOUT
This variable is used to specify the name of the output PostScript filename.
If a filename is not specufued, the the filename will default to 'mosplot.psf'.

.VARIABLE PROJECT
Specifies the project as GLL,VIKOR,MAR10,VGR-1,VGR-2,MAR-9
Only used if the input images are
in image space and the project is GLL.

.VARIABLE EXAG
The exaggeration factor used in plotting tiepoint 
residuals. The default is to exaggerate the length
of the tiepoint residuals by a factor of 10, 
ie: exag=10.0

.VARIABLE NEW
To select the new SEDR OMmatrix which was created by program
OMCOR and stored in columns 21-23 in the SEDR.INT file.
Default is to select the OLD SEDR OMmatrix stored in 
columns 8-10 of SEDR.INT.

.VARIABLE PRINT
To print the contents of all files read and written
except for the geom file.

.VARIABLE INCR
Specifies the spacing in pixels between points selected
at equal intervals around the border of the pictures
for generaton of the footprint plots. NOTE: the routine
INSIDE has a limit of 25 such points.  Default is 133 , 
and should the user not change any of INCR, NL nor NS while 
processing GLL summation mode, then INCR will be defaulted 
to 66.

.VARIABLE NL
Specifies the number of lines in each image going into
the mosaic. Default is 800, and should the user not change
any of INCR, NL nor NS while processing GLL summation mode, 
then NL will be defaulted to 400.

.VARIABLE NS
Specifies the number of samples in each image going into
the mosaic. Default is 800, and should the user not change
any of INCR, NL nor NS while processing GLL summation mode,
then NS will be assigned to 400.

.VARIABLE OBJECT
Specifies that the images going into the mosaic are
geometrically corrected (object space). The default is
IMAGE or un-corrected.

.VARIABLE LATLON
Specifies the center of projection for an oblique
stereographic projection plot of only the area contained
within the mosaic. If LATLON is not specified the plot
will be in latitude/longitude and contain the whole planet.
Use of latlon will avoid problems at the poles and
around the prime meridian. see the EXAMPLE

.VARIABLE AREA
Specifies a portion of the stereographic projection to plot.
The default is to plot all the area containing the images.
AREA is followed by 4 numbers indicating:
upper line, lower line, left sample, right sample.

.VARIABLE NODISP
Keyword used to state that plots are not to be displayed. However plots
will be saved in the output PostScript file.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmosplot.pdf
procedure
  refgbl  $echo
  refgbl  $SysChar
body
  Local Path    STATUS=STRING

  let     $echo="no"
  If ($SysChar(1)="VAX_VMS")
    Let Path="wms_test_work:[testdata.gll]"
  Else
    Let Path="/project/test_work/testdata/gll/"
  End-If

  write "===To Test Summation Mode"
  write "===This call will create a file named summ.over on current directory"
  Let $echo="yes"
 MOSPLOT &"Path"summ.sedr Project=GLL out=summ.over
  Let $echo="no"
  
  write "===The content of the IBIS Overlapping file is"
  Let $echo="yes"
 IBIS-LIST summ.over
  Let $echo="no"
end-proc
$ Return
$!#############################################################################
