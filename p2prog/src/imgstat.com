$!****************************************************************************
$!
$! Build proc for MIPL module imgstat
$! VPACK Version 1.8, Monday, April 13, 1998, 09:24:14
$!
$! Execute by entering:		$ @imgstat
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
$ write sys$output "*** module imgstat ***"
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
$ write sys$output "Invalid argument given to imgstat.com file -- ", primary
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
$   if F$SEARCH("imgstat.imake") .nes. ""
$   then
$      vimake imgstat
$      purge imgstat.bld
$   else
$      if F$SEARCH("imgstat.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake imgstat
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @imgstat.bld "STD"
$   else
$      @imgstat.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create imgstat.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack imgstat.com -
	-s imgstat.f -
	-p imgstat.pdf -
	-i imgstat.imake -
	-t tstimgstat.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create imgstat.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C	VICAR PROGRAM IMGSTAT
C
C-------------------------------------------------------------------
C Edit History:
c
C        BAM 2/95
c        BAM 3/96
c        BAM 10/96
C        NDR 1/97  -- Added Slope deviation output options.
c
c	TBD: This may need a x and y scaling feature for slopedev.
c
C-------------------------------------------------------------------


	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT NONE  !Dont use implicit integer
	integer MAXIN,MAXOUT,MAXWIND
	parameter (MAXIN=14000,MAXOUT=MAXIN/2,MAXWIND=36) !ndr 3/96

        integer*2 databuf(MAXIN*MAXWIND)
        integer*2 windowbuf(MAXWIND*MAXWIND)
        integer*2 out1(MAXOUT), out2(MAXOUT), out3(MAXOUT), out4(MAXOUT)
        integer*2 sout(MAXOUT) ! for MSSD mode
        integer   out(5),nli,nsi,cur_line,ninput,dcode
	integer*4 imin,imax,ipt,mm,begin,ist
	integer*4 i,j,k,l,m,cmin,cmax,status
	integer*4 nl,ns,nlw,nodset,icount
	integer*4 sl,ss,lines,samps,windowsize
	integer*4 iunit(3)
	real*4    dxbuf(maxout),dybuf(maxout),dx,dy
        integer*4 nn,next
        
        REAL*8 MEAN, SD, SUM, sldev, slerr, center
        LOGICAL XVPTST,KMIN,KMAX,KMEAN,KSD,KMSSD
        integer outs(5)



! PARAMETER PROCESSING 

        call xvpcnt ( 'INP',  ninput )  ! the input data sets

	do i=1,ninput                   ! open the input
	  CALL XVUNIT(IUNIT(i),'INP',i,STATUS,' ')
	  if (status.ne.1) call xvsignal(iunit(i),status,1)
	end do
	
	CALL XVOPEN(IUNIT(1),STATUS,'U_FORMAT','HALF',' ')
	if (status.ne.1) call xvsignal(iunit(1),status,1)
	
	if (ninput.eq.3) then           ! check for derivative inputs
	  CALL XVOPEN(IUNIT(2),STATUS,'U_FORMAT','REAL',' ')
	  if (status.ne.1) call xvsignal(iunit(2),status,1)
	  CALL XVOPEN(IUNIT(3),STATUS,'U_FORMAT','REAL',' ')
	  if (status.ne.1) call xvsignal(iunit(3),status,1)
	end if
	
        call xvpcnt ( 'OUT',  nodset )  ! the output data sets

	CALL XVP('WINDOW',NLW,ICOUNT) ! get window size

	call xvsize( sl, ss, nl, ns, nli,nsi)

	IF(NS .GT. MAXIN) THEN    ! bam 2/95 !! ndr 3/96
	  CALL XVMESSAGE(
     +   'IMAGE HAS JUST TOO MANY SAMPLES AND THATS TOO BIG.',' ')
	  RETURN
	ENDIF

        KMIN  = XVPTST('MIN')    ! SEE WHAT WE WISH TO PROCESS
        KMAX  = XVPTST('MAX')
        KMEAN = XVPTST('MEAN')
        KSD   = XVPTST('SD')
        KMSSD = XVPTST('MSSD')

        lines = nl / nlw                ! get # of lines and samples
        samps = ns / nlw
        windowsize = nlw * nlw
	center = (1.0 + nlw)/2.0

        do i = 1,5                      ! initialize output files required
            outs(i) = 1                 ! default = all on
        end do

        if ( .not. kmean ) outs(1) = 0
        if ( .not. kmin  ) outs(2) = 0
        if ( .not. kmax  ) outs(3) = 0
        if ( .not. ksd   ) outs(4) = 0
        if ( .not. kmssd ) outs(5) = 0


!       check we have enough output files specified for the work required

        sum = outs(1) + outs(2) + outs(3) + outs(4) + outs(5)
        if ( sum .ne. nodset ) then
          CALL XVMESSAGE(
     +   'Outputs requested do not match number of output data sets.'
     +    ,' ')
          RETURN
        ENDIF


        DO I = 1,nodset
	    CALL XVUNIT(out(i),'OUT',i,STATUS,' ')
	    if (status.ne.1) call xvsignal(out(i),status,1)
	    CALL XVOPEN(out(i),STATUS,'OP','WRITE','OPEN_ACT','SA',
     -       'O_FORMAT','HALF', 'U_NL',lines, 'U_NS',samps,' ')
	    if (status.ne.1) call xvsignal(out(i),status,1)
        end do

        dcode = 2
	cur_line = 1

        do i = 1, lines

            do j = 1, nlw              ! read a window of lines - all samples
               ipt = 1 + ( j - 1 ) * ns
               CALL XVREAD(IUNIT(1),databuf(ipt),STATUS,'NSAMPS',NS,
     +              'line',cur_line,' ')
	       cur_line = cur_line+1
	       if (status.ne.1) call xvsignal(iunit(1),status,1)
            end do

	    if (ninput.eq.3) then  ! Read in a line of DX and DY slopes
               CALL XVREAD(iunit(2),dxbuf,STATUS, ' ')
	       if (status.ne.1) call xvsignal(iunit(2),status,1)
               CALL XVREAD(iunit(3),dybuf,STATUS, ' ')
	       if (status.ne.1) call xvsignal(iunit(3),status,1)
	    end if
	    
            do k = 1, samps                     ! loop through the windows
                mm = 1
                begin = ( k - 1 ) * nlw 
                sum = 0.0
                do l = 1, nlw                   ! # of lines
                    ist = begin + ( l - 1 ) * ns
                    do m = 1, nlw               ! # of samples
                        sum = sum + databuf(ist+m)
                        windowbuf(mm) = databuf(ist+m)
                        mm = mm + 1
                    end do                      !Loop for one window-line
                end do                 !Loop for one window over a sample

                out1(k) = sum / windowsize
                call minmax (dcode, windowsize, windowbuf, 
     +            cmin, cmax, imin, imax)
                out2(k) = cmin
                out3(k) = cmax

                mean = out1(k)
                sum = 0.0
                do nn = 1, windowsize
                    sum = sum + (windowbuf(nn) - mean )**2
                end do
                sd = sum / windowsize
                out4(k) = dsqrt(sd)
		
		if (ninput.ne.3) continue  !with k-samp loop
		
		!compute mean slope deviation
		dx = dxbuf(k)  !slope with increasing x (to right)
		dy = dybuf(k)  !slope with increasing y (up)
		sldev = 0.0
		mm = 1
                do l = 1, nlw                   ! # of lines
                    do m = 1, nlw               ! # of samples
                        slerr = windowbuf(mm) 
			slerr = slerr - 
     +				(mean + dx*(m-center) + dy*(center-l))
                        sldev = sldev + slerr*slerr
                        mm = mm + 1
                    end do  !Loop for one window-line
                end do !Loop for one window over a sample
		
		sldev = DSQRT( sldev / (windowsize -1) )   ! standard deviation
                sout(k) = sldev
		
            end do


            next = 1                       ! output files
            if ( outs(1) .eq. 1 ) then     ! mean required
                CALL XVWRIT(out(next),out1,STATUS,' ')
                if (status.ne.1) call xvsignal(out(next),status,1) 
                next = next + 1
            end if
            if ( outs(2) .eq. 1 ) then      ! min required
                CALL XVWRIT(out(next),out2,STATUS,' ')
                if (status.ne.1) call xvsignal(out(next),status,1) 
                next = next + 1
            end if
            if ( outs(3) .eq. 1 ) then ! max required
                CALL XVWRIT(out(next),out3,STATUS,' ')
                if (status.ne.1) call xvsignal(out(next),status,1) 
                next = next + 1
            end if
            if ( outs(4) .eq. 1 ) then ! sd required
                CALL XVWRIT(out(next),out4,STATUS,' ')
                if (status.ne.1) call xvsignal(out(next),status,1) 
                next = next + 1
            end if
            if ( outs(5) .eq. 1 ) then ! sd required
                CALL XVWRIT(out(next),sout,STATUS,' ')
    	        if (status.ne.1) call xvsignal(sout,status,1)
            end if


        end do   	! end of main loop



        do i = i, ninput   ! close input
   	        call xvclose(iunit(i),status,' ')
		if (status.ne.1) call xvsignal(out(i),status,1)
	enddo

	do i=1,nodset      ! and output
		call xvclose(out(i),status,' ')
		if (status.ne.1) call xvsignal(out(i),status,1)
	enddo


	RETURN
 	END

$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create imgstat.pdf
PROCESS help=*
  PARM INP  TYPE=STRING COUNT=0:3 DEFAULT=--
  PARM OUT  TYPE=STRING COUNT=0:5 DEFAULT=--
  PARM SIZE TYPE=INTEGER COUNT=0:4 DEFAULT=--
  PARM SL   TYPE=INTEGER DEFAULT=1
  PARM SS   TYPE=INTEGER DEFAULT=1
  PARM NL   TYPE=INTEGER DEFAULT=0
  PARM NS   TYPE=INTEGER DEFAULT=0
  PARM MEAN TYPE=KEYWORD  COUNT=0:1   VALID=(MEAN)  DEFAULT=-- 
  PARM MIN  TYPE=KEYWORD  COUNT=0:1   VALID=(MIN)   DEFAULT=-- 
  PARM MAX  TYPE=KEYWORD  COUNT=0:1   VALID=(MAX)   DEFAULT=-- 
  PARM SD   TYPE=KEYWORD  COUNT=0:1   VALID=(SD)    DEFAULT=-- 
  PARM MSSD TYPE=KEYWORD  COUNT=0:1   VALID=(MSSD)  DEFAULT=-- 
  PARM WINDOW TYPE=INTEGER DEF=12 
END-PROC
.TITLE 
VICAR Program IMGSTAT
.HELP
PURPOSE

	IMGSTAT is a VICAR applications program for calculating statistical
	quantities in local areas surrounding each pixel in an input image.
	The local mean, minimum, maximum, standard deviations, and optionally
        the mean slope devided by the standard deviation are quantities that 
        are obtained.

.PAGE
EXECUTION FORMAT

	TAE>IMGSTAT IN,OUT, WINDOW

	where

	IN	is the input data set (VICAR labeled image).

	OUT	is the output image of the selected statistic.
                Three output files are required; optionally,
                if four output files are requested, a standard
                deviations output file is created.

	WINDOW	is the number of lines in the local area window.
.PAGE
OPERATION

	IMGSTAT performs a sliding window statistical analysis on an input
	image. An output pixel's position represents the center position of
	the window for the input image and its value represents statistics
	based on data within the window only. The window moves along one sample
	at a time and one line at a time until each pixel in the input has
	been at the center of the window. In other words statistics are 
	compiled for the local area around each and every pixel in the input
	image.

	The edge conditions are handled as follows. Any window positions that
	extend beyond the edge of the image are handled by using data from the
	nearest pixel on the image. This is not a reflection scheme like some
	sliding window algorithms have.

	Byte and alf word input results in half word output.  This is 
        automatic and requires no specification by the user. Half word 
        values that exceed the half word maximum integer value
	(32767) are set to 32767. Input images must have less than 14001 
        samples. There is no limit to the number of lines in the input.
.PAGE
EQUATIONS

	MIN
		The min is a smallest value of all DN values in the 
		window.

	MAX
		The max is a largest value of all DN values in the 
		window.

	MEAN
		The mean is a result of the sum of all DN values in the 
		window divided by the number of pixels in the window.
		(the average of the DNs)
			WINDOWSUM / NPIXWNDO

        STANDARD DEVIAION
                The standard deviation is the square root of the sum 
                of all observations minus the mean devided by the 
                number of observations.     


        MEAN SLOPE / STANDARD DEVIAION
                The mean slope is computed and is then devided by the 
                standard deviaion for all observations.
  

Original Programmer:	Barbara McGuffie    February 1995
Cognizant Programmer:	Barbara McGuffie
Revision	        1

.LEVEL1
.VARI INP
Vicar labeled image file (input)
.VARI OUT
Vicar labeled image file/s
.VARI WINDOW
Number of lines/samples
in the window
.VARI NL
Number of input lines
.VARI NS
Number of input samples (Maximum 12000)
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI SIZE
Standard VICAR size field
.VARI MEAN
If indicated, specifies that the 
first output data set will 
contain mean values
.VARI MIN
If indicated, specifies that 
the next output data set will 
contain minimum values
.VARI MAX
If indicated, specifies that 
the next output data set will 
contain maximum values
.VARI SD
If indicated, specifies that 
the next output data set will 
contain standard deviations
.VARI MSSD
If indicated, specifies that
the next output data set will 
contain mean slope devided by 
the standard deviations
.LEVEL2
.VARI	IN
File name to be used as the input
data set (VICAR labeled image).
.VARI   OUT
File names to be used as the output
data set (VICAR labeled image).
.VARI 	WINDOW
The number of lines in the local area window.
.VARI NL
Number of input lines
.VARI NS
Number of input samples
.VARI SL
Starting line
.VARI SS
Starting sample
.END
$ Return
$!#############################################################################
$Imake_File:
$ create imgstat.imake

/***********************************************************************

                     IMAKE FILE FOR PROGRAM imgstat

   To Create the build file give the command:

		$ vimake imgstat 			(VMS)
   or
		% vimake imgstat	       		(Unix)


************************************************************************/

#define PROGRAM	imgstat
#define R2LIB

#define MODULE_LIST imgstat.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tstimgstat.pdf
!  Procedure to test the procedure IMGSTAT
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
gen a 10 10 
list a
imgstat a b 'mean window=3
list b
imgstat a b 'min window=3
list b
imgstat a b 'min window=5
list b
imgstat a b 'max window=3
list b
imgstat a b 'sd window=3
list b
imgstat a b 'mssd window=3
list b
imgstat a (b,c,d,e,f) 'mean 'min 'max 'sd 'mssd window=3
list b
list c
list d
list e
list f
imgstat a (b,c)  'min 'max window=3
list b
list c
end-proc
$ Return
$!#############################################################################
