$!****************************************************************************
$!
$! Build proc for MIPL module pltgraf
$! VPACK Version 1.8, Friday, March 19, 1999, 12:49:25
$!
$! Execute by entering:		$ @pltgraf
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
$ write sys$output "*** module pltgraf ***"
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
$ write sys$output "Invalid argument given to pltgraf.com file -- ", primary
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
$   if F$SEARCH("pltgraf.imake") .nes. ""
$   then
$      vimake pltgraf
$      purge pltgraf.bld
$   else
$      if F$SEARCH("pltgraf.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pltgraf
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pltgraf.bld "STD"
$   else
$      @pltgraf.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pltgraf.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pltgraf.com -
	-s pltgraf.f -
	-i pltgraf.imake -
	-p pltgraf.pdf -
	-t tstpltgraf.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pltgraf.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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



$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pltgraf.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM pltgraf

   To Create the build file give the command:

		$ vimake pltgraf			(VMS)
   or
		% vimake pltgraf			(Unix)


************************************************************************/


#define PROGRAM	pltgraf
#define R2LIB

#define MODULE_LIST pltgraf.f 

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
$ create pltgraf.pdf
process      help=*
    parm INP      status=(string,72)  count=1:2

    parm TITLE    status=(string,60) default=""
    parm XLABEL   status=(string,60) default=""
    parm YLABEL   status=(string,12) default=""
    parm COMMENT  status=(string,60) default=""
    parm DATE     status=keyword valid=(NODATE,DATE) default=NODATE

    parm MODE     status=keyword valid=(AXIS,NOAXIS,NOBOX) default=AXIS

    parm XLEN     status=real  valid=(0.5:10.0)  default=4.0
    parm YLEN     status=real  valid=(0.5:10.0)  default=3.0

    parm XRANGE   status=real count=2 default=(0,1)
    parm YRANGE   status=real count=2 default=(0,1)
    parm ZRANGE   status=real count=2 default=(1,8)

    parm DATAFORM status=(string,2) valid=(YX,XY) default="YX"
    parm DIM      status=integer default=2 valid=2:3
    parm DIRECT   status=(string,2) valid=(BR,TR,TL,BL) default="BR"

    parm FINALPOS status=keyword valid=(CURR,NEXT) default=CURR

    parm DATACOLS STATUS=integer valid=(1:100) count=(1:6) default=3
    parm FORMAT   STATUS=(string,4) count=(1:11) default=" "
    parm XCOL     STATUS=integer count=1 valid=(1:40) default=1
    parm YCOL     STATUS=integer count=1 valid=(1:40) default=2
    parm HEIGHCOL STATUS=integer count=1 valid=(1:40) default=1
    parm ANGLECOL STATUS=integer count=1 valid=(1:40) default=1
    parm TSIZE	  STATUS=real valid=(.0375:.6) default=.15
    PARM PLOT     STATUS=STRING  COUNT=(0:1)                DEFAULT=--
    PARM NODISP   STATUS=KEYWORD COUNT=(0:1) VALID=NODISP   DEFAULT=--
end-proc

.title
VICAR/IBIS Program "pltgraf"
.help
PURPOSE
    "PLTGRAF" plots an IBIS graphics file inside a labeled  box.  The
    window size and plot size can be selected or  automatic window sizing
    invoked.  Graphics outside of  the window are clipped.  Three
    dimensional graphics-1  files can also be plotted.  The format of the
    data in the file can be specified (XY or YX), and the direction of the
    axes on the plot can be chosen.   

    "PLTGRAF" can also plot attribute information such as text and numbers 
    from an (optional) IBIS interface file.  

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
EXECUTION
If the display parameters are set to defaults,

	pltgraf INP=file.gra

will plot the graphics file "file.gra"

Display parameters:
                                title
            3.0 +--------|--------|--------|--------|
                |                                   |
            2.0 +                                   +
  y-axis anno.  |                                   |
            1.0 +                                   +
                |                                   |
            0.0 +--------|--------|--------|--------|
               0.0      1.0      2.0      3.0      4.0
                            x-axis anno.      comment

  text labeling data (default for each is no text):
    title="title"		Title (above data)
    xlabel="x-axis anno."	X-axis annotation (below data)
    ylabel="y-axis anno."	Y-axis annotation (to left of data)
    comment="comment"		Comment text (below data, right justified)
    'date			Date, in the form "day-mon-da, year",
                                prepended to comment text (comment not
				necessary to get date) (DATE parameter)

  box and numbers bordering data (MODE parameter; default is 'axis):
    'axis			Draw box, tick marks, and numbers
    'noaxis			Draw box only
    'nobox			No border around data

  plot size (defaults shown):
    xlen=4.0			Width of plot in inches, not counting text 
				and numbers outside of box
    ylen=3.0			Height of plot in inches, not counting text 
				and numbers outside of box

  window into data (default for each is to use min. & max. of data):
    xrange=(lower_x,upper_x)	Only points with x values in the range
				    lower_x <= x <= upper_x
				are displayed.
    yrange=(lower_y,upper_y)	Only points with y values in the range
				    lower_y <= y <= upper_y
				are displayed.
    zrange=(lower_z,upper_z)	Only points with z values in the range
				    lower_z <= z <= upper_z
				are displayed.

  data format (defaults shown):
    dataform="YX"		Data in file is in (y,x) or (y,x,z) format.
				"XY" indicates (x,y) or (x,y,z) format.
    dim=2			Number of dimensions for each data point--here,
				2d. dim=3 indicates 3d.
    direct="BR"			Direction of increasing x and y values--here,
				from top to bottom for y and from left to right
				for x. Other directions are "TR", "TL", "BL".

  control of final pen position (default shown):
    'curr			At the end of the plot, the cursor is positioned
				at the origin of the plot just finished. 'next
				positions the cursor 1 inch to the right of the 
				plot. (FINALPOS parameter)

  special annotation--only used if an interface file is supplied as the 
  second file in the INP parameter (defaults shown):
    datacols=3			A list of 1 to 6 numbers indicating the 
				columns in the interface file that contain
				text and/or numbers to be plotted.
    format="REAL,HALF,ASCI,     The format of the input data to be displayed.
           etc"	                One format for each column. The first two
                                formats must be present and represent the
                                format of the x and y value columns.  The
                                next two columns, if present, represent the
                                angle and size of the text.  Column 5 may 
                                be either a number to display or text columns
                                to display. A maximum of 20 characters 
                                (5 columns can be displayed).  The user may 
                                optionally plot both text and a value at an
                                x,y location.                 
    xcol=1			The number of the column that contains
				x-coordinate of the left edge of the text
				and/or numbers (before rotation).
    ycol=2			The number of the column that contains
				y-coordinate of the bottom edge of the text
				and/or numbers (before rotation).
    tsize=.15			Default text size if heighcol is defaulted.
    heighcol=1			The number of the column that contains the
				height for the text and/or numbers. If the
				default is taken, then a height of tsize is
				used for all rows in the interface file.
    anglecol=1			The number of the column that contains the
				CCW rotation angle--in degrees from horizontal--
				for the text and/or numbers. If the default is
				taken, then an angle of 0.0 degrees is used
				for all rows in the interface file.


EXAMPLES

    pltgraf INP=FILE.GRA  TITLE="Map of an Unknown Land"  +
        XLEN=10  YLEN=5.0  +
	XRANGE=(-120,-115)  YRANGE=(34,36)  +
	XLABEL="LONGITUDE" YLABEL="LATITUDE"  DIRECT="TR" 

In this example the data window is (-120,-115) in longitude and (34,36) in
latitude, and the size of the plot is 10 by 5 inches. The direction of the axes
is set to top-right since this is a latitude-longitude plot. 


    pltgraf  FILE.GRA  XLEN=7 YLEN=3.5 'NOAXIS TITLE="Another Map" +
	XLABEL="south" 'DATE

In this example, the data window is found from the extent of the data so that
the whole file is displayed. Because of the 'NOAXIS keyword, a box is drawn
around the data but the axes are not labeled with numbers and no tick marks
are drawn. A title and the x-axis annotation are displayed; today's date is
drawn in the lower right corner of the plot.


    pltgraf  THREE.GRA  DIM=3  XRANGE=(1000,1400) YRANGE=(600,900) +
	ZRANGE=(0,10000)

This example demonstrates how to plot 3-D graphics files. The first two
dimensions are treated identically as with 2-D graphics files.


    ibis-gen LINES NC=3 NR=8 'IBIS-1 'ROW DATACOL=(1,2,3) DATA=( +
	 1,  1,  1,	 2,  2,  2,	 3,  3,  3,	 4,  4,  4,
	 5,  5,  5,	 6,  6,  6,	 7,  7,  7,	 8,  8,  8)
    pltgraf LINES DIM=3 'NOAXIS XLEN=8 YLEN=8 'DATE COMMENT=" test" +
	ZRANGE=(1,7)

In this plot, a box will be drawn around the data and the comment
"WED SEP 17, 1987 test" (current date) will be drawn in the lower right
corner of the plot just outside of the box. The last line segment,
(7,7) to (8,8), will not be drawn, however, because the second point's z
value--8--is outside of ZRANGE.


.PAGE
Optional Interface File Examples:

    pltgraf (PLOT.GRA,TEXT.INT) XCOL=1 YCOL=3 DATACOLS=(5,6,7,8) +
	FORMAT=("REAL","REAL","HALF","ASCI",'ASCI','ASCI') +
        HEIGHCOL=10 ANGLECOL=11 'NOBOX +
	COMMENT="Test annotation"

This example shows the use of an interface file to plot annotation. First, the
data in the first file will get plotted; no box, tick marks, axes number 
labels, axes annotation, or title will be plotted ('NOBOX + omission of
XLABEL, YLABEL, & TITLE). A comment will be drawn in the lower right corner
of the plot.

Next, the annotation from the interface file will be plotted.  The position of
the text to be plotted is in columns specified by XCOL and YCOL (these
positions are scaled to fit the plot in the same way that data points are
scaled). The DATACOLS specifies the interface columns that hold the text and
numbers to be plotted.  The data in the columns is specified with the 
FORMAT parameter. For this example, the first two columns (1,3) are real 
numbers and contain the x and y location at which the following datacols will
be plotted.  Column 5 contains a half word value to plotted at x,y and columns
6,7,8 contain associated text to be plotted at the same location.

.page
RESTRICTIONS
 
 1. Plotted text must not be longer than 60 characters.
 2. Interface file text may not be longer than 20 characters.

WRITTEN BY:                     Frank Evans
COGNIZANT PROGRAMMER:           Barbara McGuffie
REVISION:                       G (February 23, 1990)
          Made portable for UNIX    A. Scop (CRI) July, 10 1995

          Corrected subroutine PLOTTEXT which had not been ported
	     properly. The subroutine was rewritten.   BAM 5/98

.LEVEL1
.VARIABLE INP
 1. Input IBIS graphics-1 file
 2. Optional interface file with
    special annotation

.VARIABLE TITLE
String for title
.VARIABLE XLABEL
String for x-axis annotation
.VARIABLE YLABEL
String for y-axis annotation
.VARIABLE COMMENT
String for lower-right corner
annotation
.VARIABLE DATE
Switch indicating that the curr.
date should be prepended to 
COMMENT ('DATE) or not ('NODATE)

.VARIABLE MODE
Switch indicating level of 
border detail around data:
  'AXIS: box, ticks, numbers
  'NOAXIS: box only
  'NOBOX: nothing

.VARIABLE XLEN
Length of x-axis in inches
.VARIABLE YLEN
Length of y-axis in inches

.VARIABLE XRANGE
Range for x-variable
.VARIABLE YRANGE
Range for y-variable
.VARIABLE ZRANGE
Range for z-variable

.VARIABLE DATAFORM
File format:  XY, YX.
.VARIABLE DIM
Dimension of graphics
file (2 or 3)
.VARIABLE DIRECT
Increasing direction for axes:
  TR for top right
  BR for bottom right
  TL for top left
  BL for bottom left

.VARIABLE FINALPOS
Position for pen following plot:
  'CURR: at origin of plot (for
    overprinting)
  'NEXT: to right of plot (for
    adjacent plot)

.VARIABLE DATACOLS
Columns (up to 10) that hold the
text or numbers to be plotted
.VARIABLE FORMAT
String containing IBIS FORMAT
statement to use to format the 
data columns (parentheses req'd)
.VARIABLE XCOL
Column number that holds left-
edge x coords for text/numbers
(before rotation)
.VARIABLE YCOL
Column number that holds bottom-
edge y coords for text/numbers
(before rotation)
.VARIABLE TSIZE
Text size in inches when 
HEIGHCOLis defaulted. 
Default is .15 
.VARIABLE HEIGHCOL
Column number that holds the
height of the text in inches.
(Default yields TSIZE inches)
.VARIABLE ANGLECOL
Column number that holds the
angle of the text in degrees.
(Default yields 0 degrees)
.VARIABLE PLOT
 STRING-OPTIONAL
 Turns on PLOT.
.VARIABLE NODISP
 If present, no display
 is shown


.LEVEL2
.VARIABLE INP
    Type:	string, 72 characters
    Count:	1 or 2
    Valid:	any
    Default:	none

.VARIABLE TITLE
    Type:	string, 60 characters
    Count:	1
    Valid:	any
    Default:	""
.VARIABLE XLABEL
    Type:	string, 60 characters
    Count:	1
    Valid:	any
    Default:	""
.VARIABLE YLABEL
    Type:	string, 12 characters
    Count:	1
    Valid:	any
    Default:	""
.VARIABLE COMMENT
    Type:	string, 60 characters
    Count:	1
    Valid:	any
    Default:	""
.VARIABLE DATE
    Type:	keyword
    Count:	1
    Valid:	'NODATE, 'DATE
    Default:	'NODATE

.VARIABLE MODE
    Type:	keyword
    Count:	1
    Valid:	'AXIS, 'NOAXIS, 'NOBOX
    Default:	'AXIS

.VARIABLE XLEN
    Type:	real
    Count:	1
    Valid:	0.5 to 10.0 inches
    Default:	4.0
.VARIABLE YLEN
    Type:	real
    Count:	1
    Valid:	0.5 to 10.0 inches
    Default:	3.0

.VARIABLE XRANGE
    Type:	real
    Count:	2
    Valid:	any
    Default:	1.0 (if defaulted, then the actual range of the data is used)
.VARIABLE YRANGE
    Type:	real
    Count:	2
    Valid:	any
    Default:	1.0 (if defaulted, then the actual range of the data is used)
.VARIABLE ZRANGE
    Type:	real
    Count:	2
    Valid:	any
    Default:	1.0 (if defaulted, then the actual range of the data is used)

.VARIABLE DATAFORM
    The DATAFORM parameter specifies the way to assign the coordinates in
    the file to the x and y axes of the plot.  The default (YX) uses the
    first coordinate for the Y axis and the second for the X axis. 

    Type:	string, 2 characters
    Count:	1
    Valid:	"YX", "XY"
    Default:	"YX"
.VARIABLE DIM
    Type:	integer
    Count:	1
    Valid:	2 or 3
    Default:	2
.VARIABLE DIRECT
    Type:	string, 2 characters
    Count:	1
    Valid:	"BR", "TR", "TL", "BL"
    Default:	"BR"

.VARIABLE FINALPOS
    Type:	keyword
    Count:	1
    Valid:	'CURR or 'NEXT
    Default:	'CURR

.VARIABLE DATACOLS
    Type:	integer
    Count:	1 to 10
    Valid:	1 to 40
    Default:	3
.VARIABLE FORMAT
    Type:	string, 4 characters
    Count:	1:100
    Valid:	IBIS format information enclosed in parentheses
    Default:	none
.VARIABLE XCOL
    Type:	integer
    Count:	1
    Valid:	1 to 40
    Default:	1
.VARIABLE YCOL
    Type:	integer
    Count:	1
    Valid:	1 to 40
    Default:	2
.VARIABLE TSIZE
    Type:	real
    Count:	1
    Valid:	.0075 to 3.0
    Default:	.15
.VARIABLE HEIGHCOL
    Type:	integer
    Count:	1
    Valid:	1 to 40
    Default:	1 (if defaulted, then a height of TSIZE inches is 
		used for all rows)
.VARIABLE ANGLECOL
    Type:	integer
    Count:	1
    Valid:	1 to 40
    Default:	1 (if defaulted, then an angle of 0.0 degrees is
		used for all rows)
.VARIABLE PLOT
 STRING-OPTIONAL
 Specifies the filename to receive the output plot data.
 This must be printed out with the VMS/DCL or UNIX/USH command:

   PRINT/QUE=QMS/NOTIFY filename

 or

   qpr filename

.VARIABLE NODISP
 Keyword--Optional
 If present, no display is shown in interactive mode and output plot files
 are automatically saved.  When not present, plot is displayed and files are
 save is an option.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpltgraf.pdf
procedure
    refgbl $autousage
    refgbl $echo
body
    let $autousage="none"
!    let _onfail="continue"
    let $echo="yes"

write "THIS TSTPDF MUST BE RUN INTERACTIVELY - NOT IN BATCH"

write "In order to create files for plotting, click the left mouse button"
write "on the save button in the upper left hand corner of the plot"
write "display window to save the file.  Then, at the command line,"
write "For UNIX & ALPHA machines type     qpr filename"
write "For VMS          machines type     print/q=qms/notify filename"

!Warning, changing size of plot display window changes hardcopy also.

! Create a polygon consisting of a cube with a diamond to one side; this
! is an attempt to make a shape that will have a recognizable orientation.
    ibis-gen cube.pic NC=3 NR=26 'ibis-1 'row DATACOL=(1,2,3) +
        data=(1,1,1, 1,51,1, 51,51,1, 51,1,1, 1,1,1, 1,1,51, +
	1,51,51, 51,51,51, 51,1,51, 1,1,51, 0,0,0, 1,51,1, 1,51,51, 0,0,0, +
	51,51,1, 51,51,51, 0,0,0, 51,1,1, 51,1,51, 0,0,0, +
	36,41,26, 41,41,36, 36,41,46, 31,41,36, 36,41,26, 0,0,0)

! Plot the cube default style to an output file, then use pltgraf to display
! the file
    plot3d cube.pic cube_perspective azimuth=30 distance=100 origin=(26,26,26)
    ibis-list cube_perspective NC=3 NR=26 GR1DIM=3
    pltgraf cube_perspective title="Perspective view of a cube" dim=3 +
            plot=pgrf1.psf

! Plot the cube as a flat polygon; the z-values are used as pen numbers
! after partitioning into 1 of 3, 4, 5, or 6 groups
    pltgraf cube.pic dim=3 xlabel="x" ylabel="y" xlen=5.0 ylen=5.0 +
	dataform=xy direct=tr 'date plot=pgrf2.psf
    pltgraf cube.pic dim=3 comment="comment" xlen=5.0 ylen=5.0 +
	dataform=yx direct=tr comment="test" plot=pgrf3.psf
    pltgraf cube.pic dim=3 'date xlen=6.0 ylen=3.0 +
	dataform=xy direct=tr comment="test" 'date plot=pgrf4.psf


! Run tests in which the z-value is directly interpreted as a pen number
    ibis-gen latlong.pic NC=3 NR=28 'ibis-1 'row DATACOL=(1,2,3) data=( +
	 159.2450,  152.5256, 1.0000,  176.0350,  788.9391, 1.0000, +
	 183.6950, 1425.4431, 1.0000,  182.2350, 2061.9700, 1.0000, +
	 171.6550, 2698.4524, 1.0000,  151.9450, 3334.8225, 2.0000, +
	 123.1000, 3971.0132, 2.0000,   85.1200, 4606.9551, 2.0000, +
	  37.9850, 5242.5796, 2.0000,    0.0000,    0.0000, 0.0000, +
	4274.6050,   17.6281, 6.0000, 4292.0552,  711.7065, 6.0000, +
	4300.0200, 1405.8488, 6.0000, 4298.5000, 2100.0068, 6.0000, +
	4287.5000, 2794.1338, 6.0000, 4267.0151, 3488.1819, 5.0000, +
	4237.0298, 4182.1030, 5.0000, 4197.5400, 4875.8501, 5.0000, +
	4148.5249, 5569.3721, 5.0000,    0.0000,    0.0000, 0.0000, +
	 159.2450,  152.5256, 8.0000, 2216.9250,   85.0768, 8.0000, +
	4274.6050,   17.6281, 7.0000,    0.0000,    0.0000, 0.0000, +
	  37.9850, 5242.5796, 3.0000, 2093.2550, 5405.9759, 3.0000, +
	4148.5249, 5569.3721, 4.0000,    0.0000,    0.0000, 0.0000)

    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 +
	xlabel="xlabel" ylabel="ylabel" comment="no axis version" +
	title="abcdefghijklmn" dim=3 'noaxis plot=pgrf8.psf

    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 +
	xlabel="xlabel" ylabel="ylabel" 'date +
	title="abcdefghijklmn" dim=3 plot=pgrf9.psf

    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 +
	xlabel="xlabel" ylabel="ylabel" 'date comment="test" +
	title="abcdefghijklmn" dim=3 plot=pgrf10.psf

    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 +
	xlabel="xlabel" ylabel="ylabel" 'date zran=(1,5) +
	title="abcdefghijklmn" dim=3 direct=tr plot=pgrf11.psf

    ibis-gen lines.pic NC=3 NR=8 'ibis-1 'row DATACOL=(1,2,3) data=( +
	 1,  1,  1,	 2,  2,  2,	 3,  3,  3,	 4,  4,  4, +
	 5,  5,  5,	 6,  6,  6,	 7,  7,  7,	 8,  8,  8)
    pltgraf lines.pic dim=3 'noaxis xlen=8 ylen=8 'date comment=" test" +
         zrange=(1,7) plot=pgrf12.psf


! Test that multiple plots are properly aligned when plotted together
! The following three plots should be done on the same page if possible
    ibis-gen latlong.pic NC=2 NR=26 'ibis-1 'row DATACOL=(1,2) data=( +
	 159.2450,  152.5256,  176.0350,  788.9391, +
	 183.6950, 1425.4431,  182.2350, 2061.9700, +
	 171.6550, 2698.4524,  151.9450, 3334.8225, +
	 123.1000, 3971.0132,   85.1200, 4606.9551, +
	  37.9850, 5242.5796,    0.0000,    0.0000, +
	4274.6050,   17.6281, 4292.0552,  711.7065, +
	4300.0200, 1405.8488, 4298.5000, 2100.0068, +
	4287.5000, 2794.1338, 4267.0151, 3488.1819, +
	4237.0298, 4182.1030, 4197.5400, 4875.8501, +
	4148.5249, 5569.3721,    0.0000,    0.0000, +
	 159.2450,  152.5256, 4274.6050,   17.6281, +
	   0.0000,    0.0000,   37.9850, 5242.5796, +
	4148.5249, 5569.3721,    0.0000,    0.0000)  
!	string=("Test text1","Test text2") 


! Test with annotated interface file as second input           
    pltgraf (latlong.pic,test.int) xran=(0,50) yran=(0,50) xlen=6 +
	ylen=6 'curr title="test inferface" datacols=4 +
        format=("REAL","REAL","REAL") plot=pgrf4a.psf
    pltgraf (latlong.pic,text.int) xran=(0,50) yran=(0,50) xlen=6 +
	ylen=6 'curr title="test inferface" datacols=3 +
        format=("REAL","REAL","ASCI") plot=pgrf4b.psf


    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 'curr +
	title=" b " xlabel=" I    noax   I" ylabel=" I    noax I" +
	'noaxis comment=" I    noax   I" plot=pgrf6.psf
    pltgraf latlong.pic xran=(0,5500) yran=(0,5500) xlen=6 ylen=6 'curr +
	title="  c" xlabel=" I       nobxI" ylabel=" I     nobxI" +
	'nobox comment=" I       nobxI" plot=pgrf7.psf
    pltgraf latlong.pic xran=(0,1) yran=(0,1) xlen=6 ylen=6 'curr +
	title="a  " xlabel=" Iaxis       I" ylabel=" Iaxis     I" +
	comment=" Iaxis       I" plot=pgrf5a.psf
    pltgraf latlong.pic xran=(0,0) yran=(0,5500) xlen=6 ylen=6 'curr +
	title="a  " xlabel=" Iaxis       I" ylabel=" Iaxis     I" +
	comment=" Iaxis       I" direct=tr plot=pgrf5.psf

end-proc
$ Return
$!#############################################################################
