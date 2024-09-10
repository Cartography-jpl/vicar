$!****************************************************************************
$!
$! Build proc for MIPL module addnoise
$! VPACK Version 1.9, Wednesday, January 06, 1999, 16:23:23
$!
$! Execute by entering:		$ @addnoise
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
$ write sys$output "*** module addnoise ***"
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
$ write sys$output "Invalid argument given to addnoise.com file -- ", primary
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
$   if F$SEARCH("addnoise.imake") .nes. ""
$   then
$      vimake addnoise
$      purge addnoise.bld
$   else
$      if F$SEARCH("addnoise.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake addnoise
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @addnoise.bld "STD"
$   else
$      @addnoise.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create addnoise.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack addnoise.com -
	-s addnoise.f -
	-i addnoise.imake -
	-p addnoise.pdf -
	-t tstaddnoise.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create addnoise.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	include 'VICMAIN_FOR'
	subroutine main44

	implicit none
	real*4	inbuf(32768)
	real*4	sigma,x,y,u,ranx,rany,gain,pixel_rate
	integer*4 nl,ns,cnt,status,line,samp,i,j,bit_hit
	integer*4 inunit,outunit,seed,bits_per_pixel,rate
	logical add_noise,shot_noise,bit_noise
	character*8 format
	
c set parameters
	call xvp('GAIN',gain,cnt)
	if(cnt.eq.0)shot_noise=.false.
	if(cnt.eq.1)then
	  shot_noise=.true.
	  gain=sqrt(1.0/gain)
	endif
	call xvp('RATE',rate,cnt)
	if(cnt.eq.0)bit_noise=.false.
	if(cnt.eq.1)bit_noise=.true.	
	call xvp('SIGMA',sigma,cnt)
	if(cnt.eq.0)add_noise=.false.
	if(cnt.eq.1)add_noise=.true.
	call xvp('bits',bits_per_pixel,cnt)	
	call xvp('SEED',seed,cnt)
	if (cnt .eq. 0)  then
		call get_seconds( seed)
	end if
	if(add_noise.or.shot_noise.or.bit_noise)then
	else
	  write(*,*)'Must select a noise model'
	  call abend
	endif
	
	if(bit_noise)then
	  pixel_rate=float(rate)/float(bits_per_pixel) ! pixel hit rate
	endif

c open input
	call xvunit(inunit,'INP',1,status,' ')
	call xvopen(inunit,status, 'IO_ACT','SA','OPEN_ACT','SA',
     +		'OP','READ',  'U_FORMAT','REAL',' ')
        call xvget(inunit,status,'NL',nl,'NS',ns,'FORMAT',format,' ')
        write(*,*)'Input format is ',format

c open output
	call xvunit(outunit,'OUT',1,status,' ')
	call xvopen(outunit,status, 'IO_ACT','SA','OPEN_ACT','SA',
     +		'OP','WRITE',  'U_FORMAT','REAL',' ')

c introduce noise
        cnt=0
	do line = 1,nl
	    call xvread(inunit,inbuf,status,' ')
	    do samp = 1,ns
	    
	      if(shot_noise)then
		call rangen(seed,ranx)
		call rangen(seed,rany)
		x = max( ranx, 1.0e-10)
		y = rany
		u = sqrt(-2.*log(x)) * cos(2*3.1415927*y)
		x=max(inbuf(samp),0.0)
		inbuf(samp) = gain*sqrt(x)*u*0.7979 + inbuf(samp)
              endif
              	    
	      if(add_noise)then
		call rangen(seed,ranx)
		call rangen(seed,rany)
		x = max( ranx, 1.0e-10)
		y = rany
		u = sqrt(-2.*log(x)) * cos(2*3.1415927*y)
		inbuf(samp) = sigma*u + inbuf(samp)
              endif
              	    
	      if(bit_noise)then
	        call rangen(seed,ranx)
	        if(ranx*pixel_rate.lt.1.0)then ! probability of pixel hit
	          call rangen(seed,ranx)
	          bit_hit=nint(ranx*bits_per_pixel+0.5)-1 ! the bit # hit
	          i=nint(inbuf(samp))
	          call bits(i,bit_hit,bit_hit,j) ! j is the bit value
	          if(j.eq.0)then
	            inbuf(samp)=i+2**bit_hit
	          else
	            inbuf(samp)=i-2**bit_hit
	          endif
	          cnt=cnt+1
	        endif
              endif
                            
	    enddo
	    if(format.eq.'BYTE')then
	      do samp=1,ns
	        if(inbuf(samp).lt.0.0)inbuf(samp)=0.0
	        if(inbuf(samp).gt.255.0)inbuf(samp)=255.0
	      enddo
	    endif
	    call xvwrit(outunit,inbuf,status,' ')
	enddo
	if(bit_noise)write(*,*)cnt,' pixels hit by bit errors'


	call xvclose(inunit,status,' ')
	call xvclose(outunit,status,' ')
	return
	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create addnoise.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM addnoise

   To Create the build file give the command:

		$ vimake addnoise			(VMS)
   or
		% vimake addnoise			(Unix)


************************************************************************/


#define PROGRAM	addnoise

#define MODULE_LIST addnoise.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create addnoise.pdf
PROCESS HELP=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING
PARM SIGMA TYPE=REAL DEFAULT=-- COUNT=0:1
PARM GAIN TYPE=REAL DEFAULT=-- COUNT=0:1
PARM RATE TYPE=INTEGER DEFAULT=-- COUNT=0:1
PARM BITS TYPE=INTEGER DEFAULT=8 COUNT=0:1
PARM SEED TYPE=INTEGER DEFAULT=-- COUNT=0:1
END-PROC

.TITLE
Create noise in images.

.HELP
PURPOSE:

Addnoise creates three types of noise:
1. Additive gaussian noise.
2. Shot noise (multiplicative ccd type noise).
3. Bit error noise.

EXECUTION:

addnoise inp=a out=b sigma=10.            Additive gaussian noise 
addnoise inp=a out=b gain=30.             ccd shot noise
addnoise inp=a out=b rate=100 bits=8      bit error noise

The SEED parameter is provided so the same random patterns can be generated.
If it is defaulted the seed for the random number generator comes from the
system time.

Note, more than one noise can be simulated at one time. If you specify "sigma"
and "gain" and "rate" for example, you'll get all three. The ordering of
these is:
first  shot noise
second additive noise
third  bit noise

METHOD:

Additive noise:
dn_out= dn_in + sigma

Shot noise:
dn_out = sqrt(1/gain) * sqrt(dn_in) * x + dn_in
where x is a random gaussian distribution with a mean deviation of 1.0

Bit error noise:
A pixel rate is computed from the bit rate.
A random number selects the pixels to hit.
Another random number selects the bit to flip.

Original Programmer :	Jean Lorre  11-1-1998

.LEVEL1
.VARIABLE IN
Input image 
.VARIABLE OUT
Output image
.VARIABLE SIGMA
The standard deviation
of the distribution
for additive noise.
.VARIABLE GAIN
Ccd gain constant
in electrons/dn.
.VARIABLE RATE
Bit error rate.
.VARIABLE BITS
Bits per pixel.
.VARIABLE SEED
The starting seed for the
random number generator

.LEVEL2
.VARIABLE IN
Input image
 
.VARIABLE OUT
Output image

.VARIABLE SIGMA
Triggers additive noise to be created.
The standard deviation of the distribution for additive noise.

.VARIABLE GAIN
Triggers shot noise to be created.
Ccd gain constant in electrons/dn.
Typical values for galileo were gain=30 in gain state 4 and gain=170 in
gain state 2.

.VARIABLE RATE
Triggers bit error noise to be created.
For example a bit error rate of 100 means one bit in 100 is hit on the average.

Note, the algorithm does not permit a pixel to be hit more than once.
Note, see the BITS keyword.

.VARIABLE BITS
Bits per pixel. Defaults to 8. Only used in the bit error "RATE" mode.
This is the lower number of bits which can be changed per word. 

Note: bits does not default to the format type. If you have HALF data but
only 12 bits/pixel in a 16 bit field you must set bits=12 not to 16.
Avoid changing the sign bit by setting bits to one less than the word length.

.VARIABLE SEED
The starting seed for the random number generator. Defaults to time of day.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstaddnoise.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="no"
!
gen out=a.img nl=100 ns=100 ival=100 linc=0 sinc=0
addnoise inp=a.img out=b1.img sigma=10.0
hist b1.img
xvd b1.img
addnoise inp=a.img out=b2.img gain=30.0
hist b2.img
xvd b2.img
addnoise inp=a.img out=b3.img rate=100
hist b3.img
xvd b3.img
!
end-proc
$ Return
$!#############################################################################
