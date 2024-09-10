	include 'VICMAIN_FOR'
c	10-July-95  CRI  MSTP S/W Conversion (VICAR porting)
	subroutine main44

	integer status, def, count,tcount
	integer	pen, axisdef
	real	plotsize, viewelev, viewazi, axlen
	real	theta, phi,  origx, origy
	real	x, y, z,  x2, y2,  plotx, ploty
	real	minx2, miny2, maxx2, maxy2
	real	plotxsize, plotysize, plotoffsets(2)
	real    aspctrx,aspctry,measuredx,measuredy
	real	xrtscalex,xrtscaley
	logical eof, zero,  autoscaling, xvptst
	character*3  dataform
	character*80 plot_ds
	character*60  string
	character*80  outfile,title(60)

	integer	dataformat
        integer rdgr,wrgr,nextgr,putgr,getgr,setgr,clgr
	real	rho, origin(3), zscale
	real	sinth, costh, costhcosph, sinthcosph, sinph
	real	costhsinph, sinthsinph, cosph
	real	scale, plotxoff, plotyoff,d2r
	common /perspeccom/ dataformat, origin, zscale, rho,
     +			sinth, costh, costhcosph, sinthcosph, sinph,
     +			costhsinph, sinthsinph, cosph

	common /scalecom/ scale,plotxoff,plotyoff,xrtscalex,xrtscaley

        call ifmessage('PLOT3D version 10-July-95')

c______________________________________________________________________________
c			start executable code
c------------------------------------------------------------------------------

	tcount = 1

        d2r = acos(-1.0)/180.0     !degree to radian conversion PI/180

	aspctrx = 18.0    ! Max box for scaling purposes using:
	aspctry = 13.50   !  1.5 * default widget aspect ratio (1200 x 900) 
        measuredx = 9.0
	if (xvptst('TITLE')) then
		measuredy = 6.375
	else
		measuredy = 6.5625
	endif
	xrtscalex = aspctrx/measuredx
	xrtscaley = aspctry/measuredy

	call xvp ('DATAFORM', dataform, count)
	if (dataform(1:3) .eq. 'XYZ') then
	    dataformat = 1
	else if (dataform(1:3) .eq. 'YXZ') then
	    dataformat = 2
	else if (dataform(1:3) .eq. 'LSZ') then
	    dataformat = 3
	else
	    dataformat = 1
	endif

c		get the scaling parameters
	call xvp ('PLOTSIZE', plotsize, count )
	call xvp ('ZSCALE', zscale, count )

	call xvparm ('SCALE', scale, count, def,1 )
	autoscaling = (def .eq. 1)
	if (scale .ne. 0.0)  scale = 1.0/scale
	call xvp ('PLOTOFFS', plotoffsets, count )

	call xvp ('DISTANCE', rho, count)
	call xvp ('ELEV', viewelev, count)
	call xvp ('AZIMUTH', viewazi, count)
	call xvp ('ORIGIN', origin(1), count)
	phi = 90.0 - viewelev
	theta = 90.0 - viewazi

	sinth = sin (theta * d2r)
	costh = cos (theta * d2r)
	sinph = sin (phi * d2r)
	cosph = cos (phi * d2r)
	costhcosph = costh*cosph
	sinthcosph = sinth*cosph
	costhsinph = costh*sinph
	sinthsinph = sinth*sinph
	    
	call xvp ('PEN', pen, count)

c			Open the input graphics file
        status = rdgr (1,1,3)
        if (status.ne.1) call signalgr(1,status,1)

c			If output exists then output 2-D perspective
	call xvparm ('OUT', outfile, count, def,1 )
	if (def .eq. 0) then
            status = wrgr ( 1, 2, 3 )
            if (status.ne.1) call signalgr(2,status,1)
	    eof = .false.
	    do while (.not. eof)
c			scan for the beginning of a line string
                status = nextgr (1,eof,x,y,z)
                if (status.ne.1) call signalgr(1,status,1)
		if (eof) go to 199

		zero = .false.
		do while (.not. zero .and. .not. eof)
		    call perspective (x, y, z,  x2, y2)
                    status = putgr (2,x2,y2,0.0)
                    if (status.ne.1) call signalgr(2,status,1)
                    status = getgr ( 1,zero,eof,x,y,z)
                    if (status.ne.1) call signalgr(1,status,1)
		enddo
                status = putgr (2,0.0,0.0,0.0)
                if (status.ne.1) call signalgr(2,status,1)
	    enddo
199	    continue
            status = clgr (1)
            if (status.ne.1) call signalgr(1,status,1)
            status = clgr (2)

            if (status.ne.1) call signalgr(2,status,1)
	    return
	endif

	if (autoscaling) then
	    minx2 = 1.0e20
	    miny2 = 1.0e20
	    maxx2 = -1.0e20
	    maxy2 = -1.0e20
	    eof = .false.
	    do while (.not. eof)
                status = getgr (1,zero,eof,x,y,z)
                if (status.ne.1) call signalgr(1,status,1)
		if (.not. zero .and. .not. eof) then
		    call perspective (x, y, z, x2, y2)
		    minx2 = min (minx2, x2)
		    miny2 = min (miny2, y2)
		    maxx2 = max (maxx2, x2)
		    maxy2 = max (maxy2, y2)
		endif
	    enddo
            status = setgr (1,1) ! reset graphics file to its first coordinate
            if (status.ne.1) call signalgr(1,status,1)

	    scale = plotsize/ max(maxx2-minx2, maxy2-miny2)
	    plotxoff = -scale*minx2
	    plotyoff = -scale*miny2
	    plotxsize = scale*(maxx2-minx2) 
	    plotysize = scale*(maxy2-miny2)
	else
	    plotxoff = plotoffsets(1)
	    plotyoff = plotoffsets(2)
	    plotxsize = plotsize
	    plotysize = plotsize
	endif

c		open plotter
      CALL XVPARM('PLOT',PLOT_DS,IPLOT,IDEF,1)
      IF (IDEF.EQ.0) THEN
          CALL PLOTFN(PLOT_DS)
      ENDIF

c	call plots ( 0, 0, 9)
	call xrtbegin(status)
        if (status.ne.1) call mabend('Unable to OPEN Plotter')
        CALL DISPLAYAXES(0,0,0)   ! turn auto axis feature off    
c		make new origin and get right pen
	call plot (0.5, 0.5, -3)
	call newpen (pen)
c  	        scale drawing for paper size
	call plot (0.0, 0.0, 3)
	call plot (aspctrx,aspctry, 3)

c		draw box
	if (xvptst ('BOX')) then
	    plotbx = plotxsize * xrtscalex
	    plotby = plotysize * xrtscaley
	    call plot (0.0, 0.0, 3)
	    call plot (plotbx, 0.0, 2)
	    call plot (plotbx, plotby, 2)
	    call plot (0.0, plotby, 2)
	    call plot (0.0, 0.0, 2)
	endif

c		put on title
	call xvparm ('TITLE', title, count, def,1)
        CALL HEADER (TITLE,tcount,1)

c    XRTP does not provide capability to scale the TITLE on the plot
c      (so the following is commented out 7/8/95)
c	if (def .eq. 0) then
c	    plotx = plotxsize/2. - 0.25*.67*(slength(title)/2.+2)
c	    call symbol(plotx, plotysize+0.15, 0.25, 
c     +			 title, 0, 0.0, slength(title) )
c	endif
c		draw 3-D axis 

	call xvparm ('AXIS', axlen, count, axisdef,1)
	if (axisdef .eq. 0 .and. axlen .ne. 0.0) then
	    call perspective (origin(1), origin(2), origin(3), x2, y2)
	    call scaleplot (x2, y2,  origx, origy)
	    call perspective (origin(1)+axlen,origin(2),origin(3),x2,y2)
	    call scaleplot (x2, y2,  plotx, ploty)
	    call plot ( origx, origy, 3)
	    call plot ( plotx, ploty, 2)
	    call symbol (plotx+0.05, ploty+0.05, 0.15, 
     +					dataform(1:1), 0, 0.0, 1)
	    call perspective (origin(1),origin(2)+axlen,origin(3),x2,y2)
	    call scaleplot (x2, y2,  plotx, ploty)
	    call plot ( origx, origy, 3)
	    call plot ( plotx, ploty, 2)
	    call symbol (plotx+0.05, ploty+0.05, 0.15, 
     +					dataform(2:2), 0, 0.0, 1)
	    call perspective (origin(1), origin(2),
     +                           origin(3)+axlen*zscale,x2,y2)
	    call scaleplot (x2, y2,  plotx, ploty)
	    call plot ( origx, origy, 3)
	    call plot ( plotx, ploty, 2)
	    call symbol (plotx+0.05, ploty+0.05, 0.15, 
     +					dataform(3:3), 0, 0.0, 1)
	endif

c		plot view parameters
	if (xvptst ('PARM')) then
	    if (axisdef .eq. 0) then
		write (string, '(a5,i3,a6,i4,a8,f7.2)') 
     +			'ELEV=', nint(viewelev), 
     +			'  AZI=', nint(viewazi), 
     +			'  AXLEN=', axlen
c		call symbol (0.2, -0.35, 0.15, string, 0, 0.0, 33)
	    else
		write (string, '(a5,i3,a6,i4)') 
     +			'ELEV=', nint(viewelev), 
     +			'  AZI=', nint(viewazi)
c		call symbol (0.2, -0.35, 0.15, string, 0, 0.0, 18)
	    endif
	    call footer(string,1,1)
	endif

	eof = .false.
	do while (.not. eof)
c		scan for the beginning of a line string
            status = nextgr (1,eof,x,y,z)
            if (status.ne.1) call signalgr(1,status,1)
	    if (eof) go to 299

c		Move to the first point with pen up
	    call perspective (x, y, z,  x2, y2)
	    call scaleplot (x2, y2,  plotx, ploty)
	    call plot ( plotx, ploty, 3)

	    zero = .false.
	    do while (.not. zero .and. .not. eof)
		call perspective (x, y, z,  x2, y2)
		call scaleplot (x2, y2,  plotx, ploty)
		call plot ( plotx, ploty, 2)
                status = getgr (1,zero,eof,x,y,z)
                if (status.ne.1) call signalgr(1,status,1)
	    enddo
	enddo

299	continue
        status = clgr (1)
        if (status.ne.1) call signalgr(1,status,1)

c		Close the plotter and be done.
	call plot( 0.0, 0.0, 999 )

	return
	end

C***********************************************************
	subroutine perspective (x, y, z, x2, y2)
	implicit none
	real	x, y, z,  x2, y2
	real	xp, yp, zp, x_eye, y_eye, z_eye, tmp

	integer	dataformat
	real	rho, origin(3), zscale
	real	sinth, costh, costhcosph, sinthcosph, sinph
	real	costhsinph, sinthsinph, cosph

	common /perspeccom/ dataformat, origin, zscale, rho,
     +			sinth, costh, costhcosph, sinthcosph, sinph,
     +			costhsinph, sinthsinph, cosph

	xp = x - origin(1)
	yp = y - origin(2)
	zp = (z - origin(3))/zscale
	if (dataformat .eq. 2) then
	    tmp = xp
	    xp = yp
	    yp = tmp
	else if (dataformat .eq. 3) then
	    tmp = xp
	    xp = yp
	    yp = -tmp
	endif
	
	x_eye = -xp*sinth + yp*costh
	y_eye = -xp*costhcosph - yp*sinthcosph  + zp*sinph
	z_eye = 1.0 - (xp*costhsinph + yp*sinth*sinph + zp*cosph)/rho
	x2 = x_eye/z_eye
	y2 = y_eye/z_eye

	return
	end

C******************************************************************
	subroutine scaleplot (x2, y2, plotx, ploty)
	implicit none
	real	x2, y2,  plotx, ploty,xrtscalex,xrtscaley

	real	scale, plotxoff, plotyoff
	common /scalecom/ scale,plotxoff,plotyoff,xrtscalex,xrtscaley

	plotx = (scale*x2 + plotxoff)*xrtscalex
	ploty = (scale*y2 + plotyoff)*xrtscaley

c	call 
	return
	end

C********************************************************************
c	integer function slength(string)
c	implicit none
c	integer	i
c	character*(*) string
c
c	i = len(string)
c	do while (ichar(string(i:i)) .eq. 32 .and. i .gt. 1)
c	    i = i - 1
c	enddo
c	slength = i
c	return
c	end
