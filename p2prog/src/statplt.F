      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      implicit none
C     July 10, 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)

      REAL      MEANS(600),COVARIANCE(600),EX(363),EY(363)
      INTEGER   IPARM(40),STATUS
      integer   bands, unit, classes, ibis
      integer   npix,nb,row

      integer   idel, icount, idef, locxs, locys, locxy
      integer   ichan, jchan, I, II, III, L

      real      xscale, xinc, xloc, xi, sigma, xlo, xhi, ylo, yhi
      real      x, xx, yscale, yinc, yloc, y, yy, q
      real      a2, b2, c2, theta, sinsq, cossq, sincos
      real      r, dx, dy
     
      character*80 classname

      CHARACTER*63 plotname
      CHARACTER*12 xtitle, ytitle

      CALL IFMESSAGE('STATPLT version 10 July 95')

C     SET DEFAULT VALUES
      xscale    = 0.0
      xinc      = 0.0
      xloc      = 0.0
      xi        = 0.0
      sigma     = 0.0
      x         = 0.0
      xloc      = 0.0
      xx        = 0.0
      yscale    = 0.0
      yinc      = 0.0
      yloc      = 0.0
      y         = 0.0
      yy        = 0.0
      q         = 0.0
      a2        = 0.0
      b2        = 0.0
      c2        = 0.0
      theta     = 0.0
      sinsq     = 0.0
      cossq     = 0.0
      sincos    = 0.0
      r         = 0.0
      dx        = 0.0
      dy        = 0.0
      bands     = 0
      unit      = 0
      classes   = 0
      ibis      = 0
      npix      = 0
      nb        = 0
      row       = 0
      idel      = 0
      icount    = 0
      idef      = 0
      locxs     = 0
      locys     = 0
      locxy     = 0
      I         = 0
      II        = 0
      III       = 0
      L         = 0
      XSCALE    = 1.0
      YSCALE    = 1.0
      ICHAN     = 1
      JCHAN     = 2
      SIGMA     = 1.0
      XLO       = 0.0
      XHI       = 255.0
      YLO       = 0.0
      YHI       = 255.0

C                PROCESS PARAMETERS
C        'BANDS'
      CALL XVPARM('BANDS',IPARM,ICOUNT,IDEF,2)
      ICHAN=IPARM(1)
      JCHAN=IPARM(2)

C        'SIGMA'
      CALL XVPARM('SIGMA',SIGMA,ICOUNT,IDEF,1)

C        'PLOTNAME'
      CALL XVPARM('PLOTNAME',plotname,ICOUNT,IDEF,1)

C        'XSCALE'
      CALL XVPARM('XSCALE',IPARM,ICOUNT,IDEF,2)
      XLO=IPARM(1)
      XHI=IPARM(2)

C        'YSCALE'
      CALL XVPARM('YSCALE',IPARM,ICOUNT,IDEF,2)
      YLO=IPARM(1)
      YHI=IPARM(2)

C          OPEN INPUT DATA SET
      CALL XVUNIT(UNIT,'INP',1,STATUS,' ')

      if (status .lt. 0) call xvsignal(unit, status, 1) 

C     SET  POINTERS FOR THE LOCATIONS OF MEANS AND SIGMAS

      LOCXS = (ICHAN*(ICHAN+1))/2
      LOCYS = (JCHAN*(JCHAN+1))/2
      LOCXY = MAX0(LOCXS,LOCYS)-IABS(JCHAN-ICHAN)

      call istat_file_open(unit,'read',0,0,0,status) 

      if (status .lt. 0) call istat_signal(unit, status, 1) 
	
      ! Get file information 
      call istat_file_Info(unit, classes, bands, ibis) 

C     INITIALIZE PLOT
!     Specify output PostScript file */
      call plotfn (plotname)

      call xrtbegin (status)
      if (status .ne. 1) then
         call xvmessage ('Error on XRT/graph initialization',' ')
         goto 9999
      endif

      call setwidgetaspect (800,800)
      call setgraphaspect (800, 800)

      WRITE (XTITLE,'(A4,I2)') 'BAND', ICHAN
      WRITE (YTITLE,'(A4,I2)') 'BAND', JCHAN
      CALL axestitles (xtitle,ytitle,90,' ',0)

      EX(362) = 0.0
      EY(362) = 0.0
      EX(363) = 1.0
      EY(363) = 1.0

      XX = SIGMA/XSCALE
      YY = SIGMA/YSCALE

C     READ A RECORD, PLOT THE MEAN AND THE SIGMA ELLIPSE
      DO III = 1, classes 
         row = III

         call istat_record_read (unit, row, classname, npix,
     +	                         nb,means,covariance,status) 
         if (status .lt. 0) call istat_signal(unit,status,1) 

!        Get elipse center point
         X = means(ichan)
         Y = means(jchan)

         IF(X .GE. XLO .AND. X .LE. XHI .and.
     +      Y .GE. YLO .AND. Y .LE. YHI) THEN
            Q = III
            CALL NUMBER(X,Y+0.1,0.12,Q,0.0,-1)

C           CALCULATE THE SIGMA ELLIPSE POINTS and PUT IN EX & EY ARRAYS

            A2 = covariance(LOCXS)+1E-9
            B2 = covariance(LOCYS)+1E-9
            C2 = covariance(LOCXY)+1E-9
            Q = A2*B2-C2*C2

            DO L=1,180
               THETA = real (L)
               !! Provide a crutch for CODA1.  A High performance arithmetic
               !! trap will occur when result from sin or cos approaches zero
               !! and the result is squared 
               if (L .eq. 180) then
                  sinsq = 0.0
               else
                  SINSQ = SIN(THETA)**2
               endif
               if (L .eq. 90) then
                  cossq = 0.0
               else
                  COSSQ = COS(THETA)**2
               endif
               if (L .eq. 90 .or. L .eq. 180) then
                  sincos = 0.0
               else
                  SINCOS = SIN(THETA)*COS(THETA)
               endif
               R = B2*COSSQ+A2*SINSQ-2.0*C2*SINCOS+1E-15
               if (cossq .eq. 0.0) then
                  dx = 0.0
               else
                  DX = SQRT(COSSQ*Q/R)
               endif
               IF(L.GT.90) DX=-DX
               if (sinsq .eq. 0.0) then
                  dy = 0.0
               else
                  DY = SQRT(SINSQ*Q/R)
               endif
               EX(L) = X+DX*SIGMA
               EY(L) = Y+DY*SIGMA
               EX(L+180) = X-DX*SIGMA
               EY(L+180) = Y-DY*SIGMA
               IF(EX(L).LT. XLO) EX(L)= XLO
               IF(EX(L).GT.XHI) EX(L)=XHI
               IF(EY(L).GT.YHI) EY(L)=YHI
               IF(EX(L+180).GT. XHI) EX(L+180)=XHI
               IF(EX(L+180).LT. XLO) EX(L+180)=XLO
               IF(EY(L+180).LT. YLO) EY(L+180)=YLO
            END DO
            EX(361) = EX(1)
            EY(361) = EY(1)
            CALL LINE(EX,EY,361,1,0,0)
         END IF
      END DO 

C     CLOSE PLOT FILE
      CALL PLOT(0.0,0.0,999)

C     CLOSE INPUT DATA SET
      CALL XVCLOSE(UNIT,STATUS,' ')

9999  continue
      RETURN
      END
