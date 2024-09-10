$!****************************************************************************
$!
$! Build proc for MIPL module plot3d
$! VPACK Version 1.9, Monday, March 22, 1999, 13:55:16
$!
$! Execute by entering:		$ @plot3d
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
$ write sys$output "*** module plot3d ***"
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
$ write sys$output "Invalid argument given to plot3d.com file -- ", primary
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
$   if F$SEARCH("plot3d.imake") .nes. ""
$   then
$      vimake plot3d
$      purge plot3d.bld
$   else
$      if F$SEARCH("plot3d.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake plot3d
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @plot3d.bld "STD"
$   else
$      @plot3d.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create plot3d.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack plot3d.com -
	-s plot3d.f -
	-i plot3d.imake -
	-p plot3d.pdf -
	-t tstplot3d.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create plot3d.f
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create plot3d.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM plot3d

   To Create the build file give the command:

		$ vimake plot3d			(VMS)
   or
		% vimake plot3d			(Unix)


************************************************************************/


#define PROGRAM	plot3d
#define R2LIB

#define MODULE_LIST plot3d.f

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
$ create plot3d.pdf
PROCESS HELP=*
 PARM INP      TYPE=(STRING,80)  COUNT=1
 PARM OUT      TYPE=(STRING,80)  COUNT=0:1 DEFAULT=--
 PARM ELEV     TYPE=REAL    DEFAULT=30
 PARM AZIMUTH  TYPE=REAL    DEFAULT=180
 PARM DISTANCE TYPE=REAL    DEFAULT=10000
 PARM ORIGIN   TYPE=REAL    COUNT=3   DEFAULT=(0,0,0)
 PARM PLOTSIZE TYPE=REAL    DEFAULT=4.0
 PARM ZSCALE   TYPE=REAL    DEFAULT=1.0
 PARM SCALE    TYPE=REAL    COUNT=0:1  DEFAULT=--
 PARM PLOTOFFS TYPE=REAL    COUNT=2  DEFAULT=(0,0)
 PARM DATAFORM TYPE=(STRING,3)  VALID=(XYZ,YXZ,LSZ) DEFAULT=XYZ
 PARM TITLE    TYPE=(STRING,60)  DEFAULT=""
 PARM AXIS     TYPE=REAL    COUNT=0:1  DEFAULT=--
 PARM PARMPLOT TYPE=KEYWORD VALID=(PARM,NOPARM) DEFAULT=PARM
 PARM BOXPLOT  TYPE=KEYWORD VALID=(BOX,NOBOX) DEFAULT=BOX
 PARM PEN      TYPE=INTEGER DEFAULT=1
 PARM PLOT     TYPE=STRING  COUNT=(0:1)     DEFAULT=--

END-PROC
.TITLE
VICAR/IBIS Program "plot3d"
.HELP
PURPOSE

    "plot3d" plots 3-D IBIS graphics-1 files in true 2-D perspective on 
the plotting devices.  The plot may be automatically or manually scaled.
The perspective may be from any angle or distance.  The plot may be 
annotated in a variety of ways.  Optionally, the perspective transformation
may be performed and data output in an IBIS 2-D graphics file.

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

Examples:

A standard autoscaling 3-D plot:
plot3d  THREE.GRA  PLOTSIZE=6.0 ELEV=30 AZIMUTH=135 DISTANCE=200 

To plot a file in (line,sample,Z) format and scale the Z value down:
plot3d  THREE.GRA  DATAFORM=LSZ PLOTSIZE=3 ZSCALE=100

To plot a 3-D axis and title:
plot3d  THREE.GRA  PLOTSIZE=6.0 ELEV=30 AZIMUTH=135  AXIS=10 +
			'NOBOX TITLE='THREE D PLOT'

For manual scaling:
plot3d  THREE.GRA  SCALE=10 PLOTOFFS=(4.5,6)

For 2-D file output:
plot3d  THREE.GRA  TWO.GRA  ZSCALE=10  ORIGIN=(100,100,10)

For the default plot:
plot3d  THREE.GRA  


Since the device independent Calcomp plotting routines are used, the
plotting device must first be allocated with PLOTTING before running
this program.


Original Programmer:	Frank Evans	January 1987
Ported to Unix          Randy Schenk (CRI) 10-July-95

Cognizant Programmer:	Michael Tschudi	June 1987

.LEVEL1
.VARIABLE INP
IBIS 3-D graphics file name.
.VARIABLE OUT
Optional 2-D IBIS graphics file.
No plot produced if output file.
.VARIABLE ELEV
Observer elevation angle
in degrees above horizon.
.VARIABLE AZIMUTH
Observer azimuthial angle
in degrees east of north.
.VARIABLE DISTANCE
Observer distance from origin
(in same units as graphics).
.VARIABLE ORIGIN   
The view origin (observer looks
toward origin) in same format
as 3-D graphics file.
.VARIABLE PLOTSIZE 
The plot size in inches.
.VARIABLE ZSCALE
Divisor to convert scale of
Z values into X,Y units.
.VARIABLE SCALE
Specify for manual scaling.
Divisor to convert graphics
file units to inches on plot.
.VARIABLE PLOTOFFS
Only used for manual scaling.
Offset of view origin (X,Y) 
from plot origin (in inches).
.VARIABLE DATAFORM 
3-D data format:
XYZ for (X,Y,Z)
YXZ for (Y,X,Z)
LSZ for (line,sample,Z)
.VARIABLE TITLE    
Title for top of plot.
.VARIABLE AXIS
3-D axis plotted if specified.
Length of axis in units of
graphics file.
.VARIABLE PARMPLOT
Keyword to plot parameter
values at bottom of plot.
.VARIABLE BOXPLOT  
Keyword to plot box
around plotted data.
.VARIABLE PEN
Number of plotting pen to use.

.LEVEL2
.VARIABLE INP
IBIS 3-D graphics file name.
.VARIABLE OUT
If an output file is specified then a perspective transformation will be
performed and the result output in an 2-D IBIS graphics file.  The output
is in (X,Y) format.  No plot will be produced an output file is specified.  
The ORIGIN and ZSCALE parameters are used in the transformation, but the 
plotting scaling is not.  Thus the output will be in the same units as 
the input 3-D file.
.VARIABLE ELEV
The elevation angle of the observer in degrees above the horizon.
.VARIABLE AZIMUTH
The azimuthial angle of the observer in degrees east of north where 
north is the y direction.  Default is to look from the south (180 degrees).
.VARIABLE DISTANCE
The distance of the observer from origin (in same units as graphics file).  
This controls the amount of perspective but not the size of the plot.
.VARIABLE ORIGIN   
The 3-D location of the origin toward which the observer looks.  
In same format as the 3-D graphics file (e.g. XYZ, LSZ).
.VARIABLE PLOTSIZE 
The maximum size of the plotted data not including the plot annotation.
.VARIABLE ZSCALE
Divisor scale factor to convert scale of Z values same scale as the 
X and Y values.
.VARIABLE SCALE
If specified then the autoscaling is disabled.  
SCALE is a divisor scale factor to convert graphics file units to 
inches on plot.
.VARIABLE PLOTOFFS
Only used for manual scaling.  Offset of view origin (X,Y) from plot 
origin (in inches).
.VARIABLE DATAFORM 
The 3-D graphics-1 file contains triplets of real numbers.  
The DATAFORM parameter specifies the format of the triplet:  
XYZ for (X,Y,Z),  YXZ for (Y,X,Z),  LSZ for (line,sample,Z).
.VARIABLE TITLE    
String for centered title at top of plot.
.VARIABLE AXIS
If the AXIS parameter is specified then the 3-D axis will be plotted.
The 3-D axis consists of three lines starting at the view origin, and
drawn along the coordinate axis, with a length given by the AXIS value
in units of graphics file.
.VARIABLE PARMPLOT
Keyword to plot parameter values at bottom of plot.  The value of the
ELEV and AZIMUTH parameters, and the AXIS if specified, are plotted.
.VARIABLE BOXPLOT  
Keyword to plot a box around the plotted data.
.VARIABLE PEN
Number of plotting pen to use.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstplot3d.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
! Create a polygon consisting of a cube with a diamond to one side; this
! is an attempt to make a shape that will have a recognizable orientation.
ibis-gen cube NC=3 NR=26 'IBIS-1 'ROW DATACOL=(1,2,3) +
    data=(1,1,1, 1,51,1, 51,51,1, 51,1,1, 1,1,1, 1,1,51, +
    1,51,51, 51,51,51, 51,1,51, 1,1,51, 0,0,0, 1,51,1, 1,51,51, 0,0,0, +
    51,51,1, 51,51,51, 0,0,0, 51,1,1, 51,1,51, 0,0,0, +
    36,41,26, 41,41,36, 36,41,46, 31,41,36, 36,41,26, 0,0,0)
!
! Plot the cube default style to the printer and to an output file
!!!! plotting print		! plot to printronix printer
plot3d cube title="A" plot="p3da.psf"  ! minimal perspective, diamond in upper-right corner
plot3d cube cube_perspective title="B" 
ibis-list cube_perspective NC=3 NR=26 GR1DIM=3
pltgraf cube_perspective title="B via PLTGRAF" DIM=3 PLOT="p3db.psf"
!
! Float around the cube, using the center of the cube as the origin
plot3d cube title="C" origin=(26,26,26) distance=1000 plot="p3dc.psf" ! zoom in
plot3d cube title="D" origin=(26,26,26) distance=100 plot="p3dd.psf"  ! zoom in
plot3d cube title="E" origin=(26,26,26) distance=100 +
    azimuth=90 plot="p3de.psf"	! circle to right
plot3d cube title="F" origin=(26,26,26) distance=100 +
    azimuth=30 elev=60 plot="p3df.psf"	! right & up
plot3d cube title="G" origin=(26,26,26) distance=40 +
    azimuth=0 elev=0 plot="p3dg.psf"	! right,in,down
plot3d cube title="H" origin=(26,26,26) distance=100 +
    azimuth=360 elev=90 plot="p3dh.psf"	! above
plot3d cube title="I" origin=(26,26,26) distance=100 +
    azimuth=360 elev=-90 plot="p3di.psf"	! below
!
! Squeeze the cube from top to bottom (half size)
plot3d cube title="J" origin=(26,26,26) distance=100 zscale=2 plot="p3dj.psf"
!
! Try a small paper version of the above (about 1 inch wide & half-inch high)
plot3d cube title="K" origin=(26,26,26) distance=100 zscale=2 scale=50 +
    plot="p3dk.psf"
!
! Again, but shifted to the right 2 inches and up 1 inch
plot3d cube title="L" origin=(26,26,26) distance=100 zscale=2 +
    scale=50 plotoffs=(2,1) plot="p3dl.psf"
!
! Repeat "K", but specifying a bounding box size rather than a plot scale
! factor. Plot should be 1 inch high & wide, not counting annotation.
plot3d cube title="M" origin=(26,26,26) distance=100 zscale=2 plotsize=1 +
    plot="p3dm.psf"
! Make zscale larger than life (use autoscaling for plot)
plot3d cube title="N" origin=(26,26,26) distance=100 zscale=0.5 plot="p3dn.psf"
!
! 10-unit axis & no title
plot3d cube axis=10 azimuth=20 plot="p3do.psf"
!
! Again, but without parameter values or a bounding box as well
plot3d cube axis=10 azimuth=20 'noparm 'nobox plot="p3dp.psf"
!
! Switch pens (can't see results on Printronix, however)
plot3d cube title="O" pen=5 plot="p3dq.psf"
!
! Manipulate the program's interpretation of the data format
plot3d cube title="P: xyz" dataform=xyz	plot="p3dr.psf" ! should be same as default (A)
plot3d cube title="P: yxz" dataform=yxz plot="p3ds.psf"
plot3d cube title="P: lsz" dataform=lsz plot="p3dt.psf"
!
! Print the plots
!!!! dcl print/nofeed/delete printronx.plt;*
!
end-proc
$ Return
$!#############################################################################
