	include 'VICMAIN_FOR'

	subroutine main44

c
c	"pltgraf" plots an IBIS graphics-1 file using Calcomp calls.
c	Axes with labels and a title may be plotted if desired.
c	Three dimensional graphics-1 files can also be plotted.
c	The direction the axes increase can be any of the four
c	corners, so line,sample or lat,long can be plotted.  Note,
c	however, that the first value of the coordinate pair is 
c	plotted in the y direction (i.e. up-down).
c
c
c	Much of the code is taken from POLYCLIP written by L. Bynum
c
c
c	Programmer	Frank Evans
c
c	Revision	New		November 1985
c
c	Revision	A		
c				Put in calls to standard IBIS
c				  graphics-1 file subroutines.
c				Added 3-D option.
c				Frank Evans	February 1986
c
c	Revision	B		
c				Allowed different coordinate systems
c				Modified 3-D option.
c				Frank Evans	March 1986
c	
c	Revision	C
c				Added interface attribute file
c				Frank Evans	June 1986
c	
c	Revision	D
c				Added dataform parameter
c				Frank Evans	January 1987
c	
c	Revision	E
c				Used call to SETGR to avoid opening graphics
c				file twice (done to reset file to its first
c				coordinate)
c				Michael Tschudi	June 1987
c	
c	Revision	F
c				 1. add PEN param
c				 2. add ZTOPEN param
c				 3. add COMMENT param
c				 4. add DATE param
c				 5. add FINALPOS param
c				 6. add NOBOX to the MODE param
c				 7. change formula for centering title from
c					xlen/2.-0.25*.67*(slength(title)/2.+2)
c				    to
c				 	(xlen-0.2490*slength(title))/2.0
c				 8. if xlabel specified along with 'NOAXIS or
c				    'NOBOX, then it is displayed without axis 
c				    numbers; same for ylabel
c				 9. change pen number calculation for scaled z
c				    from
c					pen = int(z)
c				    to
c					pen = nint(z)
c				    to avoid round-off error
c				10. fixed ZRANGE so that the range is checked
c				11. set plot to draw in same location 
c				    regardless of MODE switch value
c				12. fixed default FORMAT value
c				Michael Tschudi	September 17, 1987
c
c	REVISION	G
c				Check that interface file annotation is within
c				the plot area.  Also, center the annotation
c				and add TSIZE parameter.
c				Howard Frieden	 February 1990
c
c       REVISION        H       MSTP S/W CONVERSION (VICAR PORTING)
c                               CRI              July 1995
c
c       REVISION        I       corrected subroutine plottext
c				bam 5/98



	implicit none


	real char_width_1_4, char_width_1_8
	integer max_z_to_pen,idate
	parameter (
     +	    char_width_1_4 = 0.2490,	! avg. width of a 1/4" high character
!     +	    char_width_1_8 = 0.1296,	! avg. width of a 1/8" high character
     +	    char_width_1_8 = 0.06,	! avg. width of a 1/8" high character
     +	    max_z_to_pen = 20)		! max. size of z-to-pen table
					! (definition is repeated in DRAW)

	integer status				! returns status from routines
	integer dim	  		! number of dimensions in input file
	integer slength, slen			! pseudo string length function
	integer count				! dummy count for xvp routine
	integer xrandef, yrandef, zrandef	! default flags for ranges
	integer	dataformat			! code for data file format 
	real	xrange(2), yrange(2), zrange(2)	! window range input parameters
	real	xlen, ylen			! length of the axis
	real	xmin, xmax, ymin, ymax, zmin, zmax	! the window limits
	real	x1, y1, z1, x2, y2, z2		! points defining line segments
	real	x, y, z				! coordinates
	real	prev_x, prev_y			! contains the last pair output
	real	xscale, xoffs, yscale, yoffs    ! scaling factors
	real	firstx, firsty, deltax, deltay	! for AXIS subroutine
	logical inside				! used for "clipping" a point
	logical	eof, zero			! end of file and zero flags
	logical	it_all_fits			! flag indicates auto scaling
	logical xvptst
	integer inpcount, rdgr,setgr,getgr,nextgr,clgr
        integer*2 flag
	character*72 inpfiles(2)
        character*256 plot_ds
	character*2  direction, dataform
	character*60 xlabel, ylabel, title
	character*70 comment

	common / form / dataformat
	common / window / xmin, xmax, ymin, ymax
	common / prev / prev_x, prev_y, dim
	common / scaling / xscale, xoffs, yscale, yoffs, zmin, zmax




c---------------------------------------------------------------------
c	Start of executable code
c---------------------------------------------------------------------

        call IFMESSAGE('PLTGRAF version MAY-27-98')

	comment = ' '
        plot_ds = ' '

	call xvp ('DIM', dim, count)
	

	call xvp ('DATAFORM', dataform, count)
	if (dataform(1:2) .eq. 'XY') then
	    dataformat = 1
	else if (dataform(1:2) .eq. 'YX') then
	    dataformat = 2
	else
	    dataformat = 2
	endif

        CALL XVP('PLOT',PLOT_DS,COUNT)
        if(count.gt.0) CALL PLOTFN(PLOT_DS)

c	    get input parameters

	call xvparm( 'XRANGE', xrange, count, xrandef, 2 )
	call xvparm( 'YRANGE', yrange, count, yrandef, 2 )
	call xvparm( 'ZRANGE', zrange, count, zrandef, 2 )

c	    open input graphics file
	STATUS = RDGR (1, 1, dim)
        if (STATUS.ne.1) call SIGNALGR(1,STATUS,1)

c		if either range is defaulted then find the actual range
	if (xrandef .eq. 1.0 .or. yrandef .eq. 1.0
     +		.or.  (dim .eq. 3 .and. zrandef .eq. 1.0) ) then
	    xmin = +1.0e30
	    xmax = -1.0e30
	    ymin = +1.0e30
	    ymax = -1.0e30
	    zmin = +1.0e30
	    zmax = -1.0e30

	    eof = .false.
	    do while (.not. eof)		! go thru file until eof
		STATUS = GETGR (1, zero, eof, x, y, z)
                if (STATUS.ne.1) call SIGNALGR(1,STATUS,1)
		if (.not. zero .and. .not. eof) then
		    call format (x, y, z)
		    xmin = min( xmin, x)
		    xmax = max( xmax, x)
		    ymin = min( ymin, y)
		    ymax = max( ymax, y)
		    zmin = min( zmin, z)
		    zmax = max( zmax, z)
		endif
	    enddo
	    STATUS = SETGR (1, 1)	! reset the graphics file to its start
            if (STATUS.ne.1) call SIGNALGR(1,STATUS,1)
C	TYPE *,XMIN,XMAX,YMIN,YMAX
	endif


c		if one of the dimensions was not defaulted use the input range
	if (xrandef .eq. 0) then
	    xmin = xrange(1)
	    xmax = xrange(2)
	endif
	if (yrandef .eq. 0) then
	    ymin = yrange(1)
	    ymax = yrange(2)
	endif
	if (zrandef .eq. 0) then
	    zmin = zrange(1)
	    zmax = zrange(2)
	endif
	if (xmin .eq. 0 .and. xmax .eq. 0) then
		call xvmessage('xran cannot be (0,0)',' ')
		call ABEND
c	   call MABEND('xran cannot be (0,0)')
	endif
	if (xmin .eq. xmax) then
	   call MABEND('xrange must vary...')
	endif
	if (ymin .eq. ymax) then
	   call MABEND('yrange must vary...')
	endif

	if (ymin .eq. 0 .and. ymax .eq. 0) then
C		call xvmessage('yran cannot be (0,0)',' ')
C		call ABEND
	   call MABEND('yran cannot be (0,0)')
	endif 

c		if both were defaulted then the whole file fits
	it_all_fits = (xrandef .eq. 1.0 .and. yrandef .eq. 1.0)


	call xvp( 'DIRECT', direction, count)
	call xvp( 'XLEN', xlen, count )
	call xvp( 'YLEN', ylen, count )
c	call xvp( 'NUMPENS', numpens, count )

c			calculate the scaling factors
	if (direction(1:1) .eq. 'T') then
	    yscale = ylen/(ymax-ymin)
	    yoffs = -yscale*ymin
	    firsty = ymin
	else
	    yscale = -ylen/(ymax-ymin)
	    yoffs = ylen - yscale*ymin
	    firsty = ymax
	endif
	if (direction(2:2) .eq. 'R') then
	    xscale = xlen/(xmax-xmin)
	    xoffs = -xscale*xmin
	    firstx = xmin
	else
	    xscale = -xlen/(xmax-xmin)
	    xoffs = xlen - xscale*xmin
	    firstx = xmax
	endif
	deltay = 1.0/yscale
	deltax = 1.0/xscale


c	if (dim .eq. 3) then
c     Get the z-to-pen table
c	    call xvp ('ZTOPEN', z_to_pen, num_z_to_pen)

c     If it isn't specified, set up for scaling of z value to find the pen
c	    if (num_z_to_pen .eq. 0) then
c		zscale = numpens/(zmax-zmin)
c		zoffs =  -zscale*zmin + 1.0
c	    endif
c	endif

c Get the various text strings we'll display
	call xvp( 'XLABEL', xlabel, count)
	call xvp( 'YLABEL', ylabel, count)
	call xvp( 'TITLE', title, count)
	if (xvptst('DATE')) then
            flag=2
	    call datfmt (flag,comment,idate)
	    call xvp ('COMMENT', comment(19:70), count)
	else
	    call xvp ('COMMENT', comment, count)
	endif


c Get the basepen parameter--the basepen is the pen used for 2D plots
c and for 3D plots where the z-value is out of the range of the z-to-pen
c table
c	call xvp ('PEN', basepen, count)




c		Start the plot

	call XRTBEGIN(status)		! initialize plot
        if (status.ne.1) call MABEND('Could not initialize plotter.')

        call displayaxes(0,0,0)   ! turn auto axes off.

c	lastpen = basepen
c	call newpen (lastpen)

c			 make new origin a half inch up and over
	call plot( 0.5,0.5, -3)
	call plot( 7.8,6.0, 3)
	call plot( 0.0,0.0, 3)


c Draw the border around the data to be plotted
c   If AXIS, then draw box plus full axes
	if (xvptst('AXIS')) then

c     Draw the x and y axes 
c	    call axis( 0.0, 0.0, xlabel, -slength(xlabel),
c     +				 xlen, 0.0, firstx, deltax)
	    call axis( 0.0, 0.0, '     ', -slength(xlabel),
     +                           xlen, 0.0, firstx, deltax)
	    call axis( 0.0, 0.0, '     ', slength(ylabel),
     +				 ylen, 90.0, firsty, deltay)

c     Draw the unannotated axes
	    call plot(  0.0, ylen, 3)
	    call plot( xlen, ylen, 2)
	    call plot( xlen, 0.0,  2)
 	    x = 0.0
	    do while (x .le. xlen)			! draw x-axis tic marks
	        call plot( x, ylen, 3)
	        call plot( x, ylen+0.0625, 2)
	        x = x + 1.0
	    enddo
	    y = 0.0
	    do while (y .le. ylen)			! draw y-axis tic marks
	        call plot( xlen, y, 3)
	        call plot( xlen+0.0625, y, 2)
	        y = y + 1.0
	    enddo

c     Plot the title if it exists
	    slen = slength (title)
	    if (slen .gt. 0) then
                call header(title,1,0)
c		call symbol (((xlen - char_width_1_4*slen) / 2.0),
c     +		    (ylen + 0.25), 0.25, title, 0, 0.0, slen)
	    endif

c     Plot the axes labels if present

	    slen = slength(xlabel)
	    if (slen .gt. 0) then
		call symbol(((xlen - char_width_1_8*slen+1.0)/2.0),
     +			-.80,0.125,xlabel,0,0.0,slen)
	    endif
	    slen = slength(ylabel)
	    if (slen .gt. 0) then
		call symbol(-.75,
     +			((ylen - char_width_1_8*slen)/2.0),
     +			0.125, ylabel, 0, 90.0,slen)
	    endif

c     Plot the comment if it exists
	    slen = slength (comment)
	    if (slen .gt. 0) then
		call symbol ((xlen - char_width_1_8*slen), -0.40,
     +		    0.125, comment, 0, 0.0, slen)
	    endif


c   If NOAXIS or NOBOX, then skip the numerical labels on the axes
	else
	call plot( 0.5,0.5, -3)

c     If NOAXIS, then draw the outline box
	    if (xvptst('NOAXIS')) then
		call plot(  0.0, ylen, 2)
		call plot( xlen, ylen, 2)
		call plot( xlen, 0.0,  2)
		call plot(  0.0, 0.0,  2)
	    endif

c     Plot the title if it exists
	    slen = slength (title)
	    if (slen .gt. 0) then
                call header(title,1,0)
c		call symbol (((xlen - char_width_1_4*slen) / 2.0),
c     +		    (ylen + 0.25), 0.25, title, 0, 0.0, slen)
	    endif

c     Plot the axes labels if present
	    slen = slength (xlabel)
	    if (slen .gt. 0) then
		call symbol (((xlen - char_width_1_8*slen)/2.0),
     +		    -0.20, 0.125, xlabel, 0, 0.0, slen)
	    endif
	    slen = slength (ylabel)
	    if (slen .gt. 0) then
C		call symbol (((ylen - char_width_1_8*slen) / 2.0),
C     + 		    -3.0, 0.125, ylabel, 0,90.0,slen)
		call symbol (-.5,
     +		    ((ylen - char_width_1_8*slen) / 2.0),
     +		    .5, ylabel, 0, 90.0, slen)
	    endif

c     Plot the comment if it exists
	    slen = slength (comment)
	    if (slen .gt. 0) then
		call symbol ((xlen - char_width_1_8*slen), -0.40,
     +		    0.125, comment, 0, 0.0, slen)
	    endif
	endif


	eof = .false.
	do while (.not. eof)			! go thru file until eof

	    status = NEXTGR (1, eof, x1, y1, z1)
            if (STATUS.ne.1) call SIGNALGR(1,STATUS,1)
	    if (eof) go to 90
	    call format (x1, y1, z1)

	    status = GETGR (1, zero, eof, x2, y2, z2 )	! get the next pair
            if (STATUS.ne.1) call SIGNALGR(1,STATUS,1)
	    if (eof)  zero = .true.
	    call format (x2, y2, z2)

	
	    if ( zero ) then			! is this a point?
		inside = .true.				! find out if the point
		if ( x1 .lt. xmin ) inside = .false.	! is inside or outside
		if ( x1 .gt. xmax ) inside = .false.
		if ( y1 .lt. ymin ) inside = .false.
		if ( y1 .gt. ymax ) inside = .false.
		if ( inside ) then			! if inside, plot it
		    call draw (x1, y1, z1, x1, y1 , z1 )
		endif

	    else

c	     if we got this far we have a line segment and not a point
		zero = .false.
		do while (.not. zero )
		    if (it_all_fits) then
			call draw ( x1, y1, z1, x2, y2, z2 )
		    else
			call clipper( x1, y1, z1, x2, y2, z2 )
		    endif
		    x1 = x2
		    y1 = y2
		    z1 = z2
		    status = GETGR (1, zero, eof, x2, y2, z2)
                    if (STATUS.ne.1) call SIGNALGR(1,STATUS,1)
	 	    if (eof) go to 90
		    call format (x2, y2, z2)
		enddo				! end of line string

	    endif
	
	enddo					! end of file



90	continue


c		close the input graphics file
	status = CLGR (1)
        if (STATUS.ne.1) call SIGNALGR(1,STATUS,1)

c	if (dim .eq. 3) then
c	    if (lastpen .ne. basepen) call newpen(basepen)
c	endif


	call xvp ('INP', inpfiles, inpcount)
        if (inpcount .gt. 1) call plottext(xlen,ylen)


c Reset the pen for the next plot
	if (xvptst('NEXT')) then
c     Position the pen to the right of this plot
	    call plot (xlen+1.0,-0.5, -3)
	else
c     Position the pen back to the original origin
	    call plot (-0.5, -0.5, -3)
	endif

c	if (basepen .ne. 1)  call newpen (1)

c End the plot
	call plot (0, 0, 999)

	return
	end



c*******************************************************************
	subroutine plottext(xlen,ylen)
c	This subroutine is for plotting text and numbers in interface files
	implicit none

c
c       recoded for new IBIS subroutine calls - 5/98 bam
c

        integer i,k
	integer	status, count, runit, ibis
        integer record1,record2
        integer rk
	integer	ncol, clen
	integer	cols(5)
        integer rcols(5)
        integer acols(5)
	integer	heightdef, angledef, def
	integer	inteq, nchar, row, nelms, nfmts
	real	zmin, zmax
	real	height, angle, x, y, xlen, ylen, arad, sin, cos
	real	xscale, xoffs, yscale, yoffs
        character*4 format(5)   ! for columns

        real rnum(5)
        real val
	character*4 astr(5)
        integer ndec

	common / scaling / xscale, xoffs, yscale, yoffs, zmin, zmax

c*******************************************************************

	angle = 0.0      ! default the angle
        ndec = 2         ! # of places after the decimal point

c       read the user parameters for this file

	call xvp ('XCOL', cols(1), count)  ! x value column
	call xvp ('YCOL', cols(2), count ) ! y value column
					   ! character size value column
	call xvparm ('HEIGHCOL', cols(3), count, heightdef,' '  )
					   ! character angle value column
	call xvparm ('ANGLECOL', cols(4), count, angledef,' ' )
					   ! which columns
	call xvparm ('DATACOLS', cols(5), nelms, def,' ')
					   ! column formats
	call xvparm ('FORMAT', format, nfmts, def,' ')
					   ! default character size 
	call xvp ('TSIZE',height,count) ! default text height


c get a unit for the interface file
        CALL XVUNIT(RUNIT, 'INP', 2, STATUS, ' ')
 

c open the interface file
	call ibis_file_open(runit,ibis,'read',0,0,' ',' ',status)
        if (status.ne.1) call ibis_signal_u(runit,status,1)


c get # of rows and columns
        CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
        CALL IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)
c	call rdfil( infile, 2, clen, ncol, nofile )	! open input


c open records for data columns and ascii text

        rcols(1) = cols(1) 
        rcols(2) = cols(2)


        k = 2
        if (heightdef .eq. 0) then
            rcols(3) = cols(3) 
            k = k + 1
        end if
        if (angledef .eq. 0) then
            rcols(4) = cols(4) 
            k = k + 1
        end if

        rk = 0        
        count = 0         ! count of ascii columns
        do i = 1, nelms   ! loop through datacols
            k = k + 1
            if ( format(k) .eq. 'ASCI' .or.
     -         format(k) .eq. 'asci') then
               count = count + 1
            else			      ! numerical text
                rcols(k) = cols(4+i) 
                rk = 1
            end if

        end do        
c
c       open appropriate IBIS records
c

        if ( rk .eq. 0 ) k = k - 1
        call ibis_record_open(ibis,record1,' ',
     -          rcols,k,'REAL',status)

        if ( count .gt. 0 ) then
            go to ( 1,2,3,4,5), count
 1          call ibis_record_open(ibis,record2,'format:ASCII',
     -          0,0,'a4',status)
            nchar = 4
            go to 6
 2          call ibis_record_open(ibis,record2,'format:ASCII',
     -          0,0,'a8',status)
            nchar = 8
            go to 6
 3          call ibis_record_open(ibis,record2,'format:ASCII',
     -          0,0,'a12',status)
            nchar = 12
            go to 6
 4          call ibis_record_open(ibis,record2,'format:ASCII',
     -          0,0,'a16',status)
            nchar = 16
            go to 6
 5          call ibis_record_open(ibis,record2,'format:ASCII',
     -          0,0,'a20',status)
            nchar = 20

 6          if ( status .ne. 1 ) call ibis_signal(ibis,status,1)
        end if

	do row = 1, clen        ! process each row
            call ibis_record_read(record1,rnum,row,status)
            x = rnum(1)
            y = rnum(2)
            if (heightdef .eq. 0) height = rnum(3)
            if (angledef .eq. 0) angle = rnum(4)
         
	    x = x * xscale + xoffs
            y = y * yscale + yoffs - height/2.
					! check annotation fits in the plot
  	    if(x.lt.0.or.x.gt.xlen.or.y.lt.0.or.y.gt.ylen) goto 50



            if ( format(cols(5)) .ne. 'ASCI'
     -          .and. format(cols(5)) .ne. 'asci' ) then
               val = rnum(3)
               call number (x, y, height, val, angle, ndec)
            end if

            if ( format(cols(5)) .eq. 'ASCI'
     -          .or. format(cols(5)) .eq. 'asci' ) then

                arad=angle*3.1416/180. ! convert angle to radians
                x=x-nchar*height/2.*cos(arad) ! center the string of characters
                y=y-nchar*height/2.*sin(arad)
                call ibis_record_read(record2,astr,row,status)
	        call symbol (x,y,height,astr,inteq,angle,nchar)

                if ( rk .eq. 1 ) then
                    val = rnum(3)
                    y = y - .25
                    call number (x, y, height, val, angle, ndec)
                end if
            end if

 50         continue       ! point outside plot
        end do

9900	CALL IBIS_FILE_CLOSE (IBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(RUNIT,STATUS,1)

	return
	end



c*******************************************************************
	subroutine clipper( a1, b1, z1, a2, b2, z2 )
c
c	This routine is a fortran version of the Cohen-Sutherland
c	algorithm for clipping. A complete explaination of the 
c	workings of the algorithm plus the Pascal code can be
c	found in the book "Fundamentals of Interactive Computer
c	Graphics" by J. D. Foley and A. Van Dam ( 1982 ).
c

	implicit none

	logical outcode1(4),  outcode2(4), accept, reject, done
	logical reject_check, accept_check
	real*4  x1, x2, y1, y2, z1, z2
	real*4  a1, a2, b1, b2
	real*4  xmin, xmax, ymin, ymax
	

	common / window / xmin, xmax, ymin, ymax

	x1 = a1
	x2 = a2
	y1 = b1
	y2 = b2

	accept = .false.
	reject = .false.
	done = .false.

	do while( .not. done )
	  call outcodes( x1, y1, outcode1 )
	  call outcodes( x2, y2, outcode2 )
	  reject = reject_check ( outcode1, outcode2 )
	  if ( reject ) then
	    done = .true.
	  else
	    accept = accept_check( outcode1, outcode2 )
	    if ( accept ) then
	      done = .true.
	    else
	      if (.not.( outcode1(1) .or. outcode1(2) .or. outcode1(3)        
     &		    .or. outcode1(4) ) ) then
		call swap( x1, x2, y1, y2, outcode1, outcode2 )
	      endif

	      if ( outcode1(1) ) then
		x1 = x1 + ( x2 - x1 ) * ( ymax - y1 ) / ( y2 - y1 )
		y1 = ymax
	      else if ( outcode1(2) ) then
		x1 = x1 + ( x2 - x1 ) * ( ymin - y1 ) / ( y2 - y1 )
		y1 = ymin
	      else if ( outcode1(3) ) then
		y1 = y1 + ( y2 - y1 ) * ( xmax - x1 ) / ( x2 - x1 )
		x1 = xmax
	      else if ( outcode1(4) ) then
		y1 = y1 + ( y2 - y1 ) * ( xmin - x1 ) / ( x2 - x1 )
		x1 = xmin
	      endif
	    endif
	  endif
	enddo ! while

	if ( accept ) call draw( x1, y1, z1, x2, y2, z2 )

	return
	end
 


c*******************************************************************
	subroutine outcodes( x, y, outcode )
	implicit none
	real*4 x, y, xmin, xmax, ymin, ymax
	logical outcode(4)
	common /window/ xmin, xmax, ymin, ymax

	if ( x .lt. xmin ) then 
	  outcode(4) = .true.
	else
	  outcode(4) = .false.
	endif

	if ( x .gt. xmax ) then
	  outcode(3) = .true.
	else
	  outcode(3) = .false.
	endif

	if ( y .lt. ymin ) then 
	  outcode(2) = .true.
	else
	  outcode(2) = .false.
	endif

	if ( y .gt. ymax ) then
	  outcode(1) = .true.
	else
	  outcode(1) = .false.
	endif
	return
	end


	logical function reject_check( outcode1, outcode2 )
	implicit none
	logical outcode1(4), outcode2(4)

	reject_check = .false.
	if      ( outcode1(1) .and. outcode2(1) ) then
	  reject_check = .true.
	else if ( outcode1(2) .and. outcode2(2) ) then
	  reject_check = .true.
	else if ( outcode1(3) .and. outcode2(3) ) then
	  reject_check = .true.
	else if ( outcode1(4) .and. outcode2(4) ) then
	  reject_check = .true.
	endif
	return
	end


	logical function accept_check( outcode1, outcode2 )
	implicit none
	logical outcode1(4), outcode2(4)
	integer*4 i

	accept_check = .true.
	do i = 1, 4
	  if ( outcode1(i) .or. outcode2(i) ) then
	    accept_check = .false.
	  endif
	enddo

	return
	end




c*******************************************************************
	subroutine swap( x1, x2, y1, y2, outcode1, outcode2 )
	implicit none
	logical outcode1(4), outcode2(4), ltemp
	real*4  x1, y1, x2, y2, temp
	integer i

	temp = x1
	x1 = x2
	x2 = temp
	temp = y1
	y1 = y2
	y2 = temp
	do i = 1, 4
	  ltemp = outcode1(i)
	  outcode1(i) = outcode2(i)
	  outcode2(i) = ltemp
	enddo
	return
	end



c*******************************************************************
	subroutine draw ( x1, y1, z1, x2, y2, z2 )
	implicit none

	integer	dim
	real	x, y, x1, y1, z1, x2, y2, z2
	real	prev_x, prev_y
	real	xscale, xoffs, yscale, yoffs, zmin, zmax
	common / prev / prev_x, prev_y, dim
	common / scaling / xscale, xoffs, yscale, yoffs, zmin, zmax


c		if pen isn't here then move the pen to the first point
	if ( x1 .ne. prev_x .or. y1 .ne. prev_y ) then 
	    x = xscale*x1 + xoffs			! scale the point
	    y = yscale*y1 + yoffs		
	    call plot(x, y, +3)				
	endif						

	x = xscale*x2 + xoffs
	y = yscale*y2 + yoffs

	if (dim .eq. 3) then
c	    if (num_z_to_pen .eq. 0) then
!        Is this a legitimate point to plot?
c		z = (z1 + z2) / 2.0
c		if (z .lt. zmin  .or.  zmax .lt. z) go to 400

!	 Use scaling to convert z to pen
c		z = zscale*z + zoffs
c		pen = nint(z)

c	    else
!        Is this a legitimate point to plot?
		if (z2 .lt. zmin  .or.  zmax .lt. z2) go to 400

!	 Use table to convert z to pen
c		i_z = nint(z2)
c		if (1 .le. i_z  .and.  i_z .le. max_z_to_pen) then
c		    pen = z_to_pen(i_z)
c		else
c		    pen = basepen
c		endif
c	    endif

c	    if (pen .ne. lastpen) then		! change pens if necessary
c		call newpen (pen)
c		lastpen = pen
c	    endif
	endif
	call plot(x, y, +2)			! draw the line

 400	prev_x = x2
	prev_y = y2

	return 
	end



c*******************************************************************
	integer function slength(string)
	integer	i
	character*(*) string

	i = len(string)
	do while (ichar(string(i:i)) .eq. 32 .and. i .gt. 1)
	    i = i - 1
	enddo
	slength = i
	return
	end




c*******************************************************************
	subroutine format (x, y, z)
c		Formats data from (x,y) to (x,y) or (y,x)
	implicit none
	real	x, y, z
	real	tmp
	integer	dataformat
	common / form / dataformat

	if (dataformat .eq. 2) then
	    tmp = x
	    x = y
	    y = tmp
	endif

	return
	end



