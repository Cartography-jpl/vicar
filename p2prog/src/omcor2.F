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


