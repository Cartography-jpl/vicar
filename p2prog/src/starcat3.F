C  PROGRAM STARCAT3 - read input image (star field), locate stars, and store
C                     data in catalog for each star.
C
C   Revision History
C    8-97    SP   Changed to round up in computing auto threshold.  Commented
C                 out auto-threshold parameter processing.  (Ray said it did
C                 not work, and I was not able to tweak it to work OK.)
C                 Moved routine LABPROC behind CENTFIND to avoid Alpha compiler
C                 warnings.  This did not eliminate the warnings, which appear
C                 to be a compiler flaw, since ISL is an argument (warning)
C                 as is NL.
C                 Changed calls to CHKSTAT to use 5 arguments, calling ABEND
C                 in most cases.  (Because of OPEN_ACT and IO_ACT, the program
C                 will usually ABEND on errors, even without CHKSTAT.)
C                 Changed ".eq." TO ".ge." in check for saturation in CENTFIND.
C                 Changed MAXI default from 4.0E9 to 1.0E38 to better allow for
C                 saturation in fullword images.
C                 Changed calls to MVE with a first argument (DCODE) of 8,
C                 to use 0d0 (double precision 0) instead of 0.
C                 Corrected last argument in all calls to XVPARM.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
c
	implicit none
c
c	this routine allows for up to maxobjs dimension on the number 
c	of objects detected. That parameter is declared in subroutine
c	centfind.  To increase it reset that parmeter.
c
	integer*4 maxdn,ithr,iback,minpix,maxpix
	integer*4 lospike,hispike,minns,maxns,minnl,maxnl,nods,ibis
	integer*4 saominp,astminp,nlin1,isl,iss,columns,rows
	integer*4 imindn,imaxdn
	integer*4 numobj,astcnt,saocnt,nlimg,nsimg
	integer*4 sl,ss,nl,ns,ouni(3),iuni,stat
	integer*4 fullrecord,realrecord,asciirecord,nfull,nreal,nascii
	logical*4 asteroid,idef,iprnt,iauto,spike
	real*4 sens,saoratio,astratio,astmaxw,astmaxl,maxsum,minsum
	character*8 fmt
	character*35 version
c
	data version/' *** STARCAT3 9-11-97 SXP*** '/
c
	call xvmessage (version,' ')
c
	call parmproc (iuni,ouni,nods,sl,ss,nl,ns,
     1 nsimg,nlimg,fmt,ibis,iprnt,
     2 maxdn,iauto,sens,ithr,idef,iback,maxsum,minsum,
     3 minpix,maxpix,lospike,hispike,spike,minns,maxns,minnl,maxnl,
     4 asteroid,saominp,astminp,saoratio,astratio,astmaxw,astmaxl)
c
	call centfind (iuni,ouni,nods,sl,ss,nl,ns,
     1 nlin1,numobj,ibis,columns,rows,iprnt,fmt,
     2 maxdn,iauto,sens,ithr,idef,
     3 maxsum,minsum,spike,
     4 iback,imindn,imaxdn,minpix,maxpix,lospike,hispike,minns,maxns,
     5 minnl,maxnl,asteroid,saominp,astminp,saoratio,astratio,
     6 astmaxw,astmaxl,fullrecord,realrecord,asciirecord,nfull,
     7 nreal,nascii)
c
	call labproc (ouni,nods,sl,ss,nl,ns,sens,ithr,
     1 nlin1,iss,isl,numobj,astcnt,saocnt,asteroid,
     2 nlimg,nsimg,iback,iauto,idef,version,fmt,imindn,imaxdn,
     3 maxnl,maxns,maxdn)
c
	call putparm (numobj)
	CALL XVCLOSE(IUNI,STAT,' ')
	call ibis_file_set(ibis,'nr',nlin1,stat)
	if (stat.ne.1) call ibis_signal(ibis,stat,1)
	call ibis_file_close (ibis,' ',stat)
	if (stat.ne.1) call ibis_signal_u (ouni(1),stat,1)
	if (nods.gt.1) call xvclose (ouni(2),stat,' ')
	if (nods.eq.3) call xvclose (ouni(3),stat,' ')
	return
	END
c========================================================================
	subroutine parmproc (iuni,ouni,nods,sl,ss,nl,ns,
     1 nsimg,nlimg,fmt,ibis,iprnt,
     2 maxdn,iauto,sens,ithr,idef,iback,maxsum,minsum,
     3 minpix,maxpix,lospike,hispike,spike,minns,maxns,minnl,maxnl,
     4 asteroid,saominp,astminp,saoratio,astratio,astmaxw,astmaxl)
c
c	parameter processor 
c
	implicit none
c
	common /catalog/ttype,ibfmt,tunits
c
	character*5 ibfmt(21)		!column format
	character*8 ttype(21)		!column group
	character*16 tunits(21)		!column units
c
	integer*4 nlimg,nsimg,nods,iback
	integer*4 maxdn,ibis,lospike,hispike,spikeval
	integer*4 ithr,minpix,minint,maxint,threshin
	integer*4 minns,maxns,maxpix,minnl,maxnl,count,def
	integer*4 saominp,astminp,minintg,maxintg
	integer*4 sl,ss,nl,ns,ouni(3),iuni,stat,cnt
c	integer*4 sizefield(4),slparm,ssparm,nlparm,nsparm
	logical*4 xvptst,asteroid,idef,iprnt,iauto,spike
	real*4 saoratio,astratio,astmaxw,astmaxl
	real*4 sens,maxsum,minsum
	character*8 fmt
	character*256 fnam(3)
	character*132 outline
C    1/'12345678901234567890123456789012345678901234567890123456789012345
C    256789012345678901234567890123456789'/
C
	data MININTG/-2147483648/		!MINIMUM INTEGER VALUE (I*4)
	data MAXINTG/2147483647/			!MAXIMUM INTEGER VALUE (I*4)
c
	CALL XVPARM ('OUT',FNAM,nods,DEF,3)
	CALL XVUNIT (IUNI,'INP',1,STAT,' ')
	call chkstat (stat,'XVunit error on 1st input file', 1,0,0)!ABEND if err
	CALL XVOPEN (IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     1 'U_FORMAT','FULL',' ')
	call chkstat(stat,'XVopen error on 1st input file',1,0,0) !ABEND if err
	call xvget (iuni,stat,'NL',nlimg,'NS',nsimg,'FORMAT',fmt,' ')
c				!get size and format of input image 
	if (fmt.eq.'REAL'.or.fmt.eq.'DOUB'.or.fmt.eq.'COMP') then
		write (outline,10020) fmt
10020 format ('Format ',a8,' not supported by STARCAT3')
		call xvmessage (outline,' ')
		call abend
	endif
c
	call xvsize (sl,ss,nl,ns,nlimg,nsimg)
        if (ns .gt. 32767)  call mabend( 
     .     'ERROR: NS of input image > 32767')

C	First open catalog, then, if desired, an output image containing
C	the objects filled in with white DN=255 and an output image containing
C	the flag values, i.e., the object numbers as they ar found.
C
C					!ouni(1) is starcat catalog
	call xvunit(ouni(1),'OUT',1,stat,' ')
	call chkstat(stat,'XVopen error on output catalog file',1,0,0)!ABEND if error.
	if (nods.gt.1) then		!SKIP IF ONLY ONE OUTPUT
c					!ouni(2) is an image file with objects at dn=255
		CALL XVUNIT(ouni(2),'OUT',2,stat,' ')	!OPEN OUTPUT IMAGE
		call chkstat(stat,'XVunit error on 2nd output image',1,0,0)
		CALL XVOPEN(ouni(2),stat,'U_NL',nl,'U_NS',ns,'OPEN_ACT',
     1	'SA','IO_ACT','SA','OP','WRITE','U_FORMAT','BYTE',
     2	'O_FORMAT','BYTE',' ')
		call chkstat (stat,'XVopen error on 2nd output image', 1,0,0)
	endif
	if (nods.eq.3) then		!SKIP IF ONLY ONE OUTPUT
c				!OUNI(3) is flag image file (objects as they are numbered)
		CALL XVUNIT(ouni(3),'OUT',3,stat,' ')	!OPEN OUTPUT IMAGE
		call chkstat (stat,'XVunit error on 3rd output image', 1,0,0)
		CALL XVOPEN(ouni(3),stat,'U_NL',nl,'U_NS',ns,'OPEN_ACT',
     1	'SA','IO_ACT','SA','OP','WRITE','U_FORMAT','HALF',
     2	'O_FORMAT','HALF',' ')
		call chkstat (stat,'XVopen error on 3rd output image', 1,0,0)
	endif
C
C	Saturation value, maxd, allows the user to mark an object as
c	saturated but still count as an object while hispike says if value
c	exceeds hispike then do not count it as an object
c
	if (fmt.eq.'BYTE') maxdn=255
	if (fmt.eq.'HALF') maxdn=32767
	if (fmt.eq.'FULL') maxdn=maxintg
	call xvp('MAXD',maxdn,cnt)		!saturation - still an object
	if (fmt.eq.'HALF') then
		if (maxdn.lt.32767) then
		write (outline,10070) maxdn 
10070 format ('STARCAT3: MAXD (saturation) for a halfword image = ',i8)
		call xvmessage (outline,' ')
		endif
	endif
	if (fmt.eq.'FULL') then
		if (maxdn.lt.maxintg) then
		write (outline,10080) maxdn 
10080 format ('STARCAT3: MAXD (saturation) for a fullword image = ',i10)
		call xvmessage (outline,' ')
		endif
	endif
c
	iauto=.false.		!'AUTO' NO AUTOMATIC THRESHOLDING
CCCCCCCCC	if (xvptst('AUTO'))iauto=.true.	!check for autothresholding 
	sens=1.0					!'SENS' 1 SIGMA
CCCCCCCC	if (iauto) call xvp('SENS',sens,cnt)	!autothresholding sensitivity
C					!Number of sigma
	idef=.true.		!flag to indicate whether threshold is given
	ithr=0				!'THRE' DEFAULT THRESHOLD DN=0 for BYTE
	if (fmt.eq.'HALF') ithr=-32768		!default for 'HALF'
	if (fmt.eq.'FULL') ithr=minintg		!default for 'FULL'
	if (.not.iauto) then
		call xvp('THRE',threshin,cnt)		!THRESHOLD DN
		if(cnt.gt.0) then
			idef=.false.
			ithr=threshin
		endif
	endif
	iback=0				!'BACK' BACKGROUND DN=0
	call xvp('BACK',iback,cnt)	!BACKGROUND DN LEVEL
	iprnt=.false.		!'PRINT' DON'T PRINT CATALOG TO SESSION LOG
	if (xvptst('LIST'))iprnt=.true.	!1= PRINT CATALOG TO SESSION LOG
c
c  the following are single pixel rejection criteria parameters
c	maxint & minint
c
	if (fmt.eq.'BYTE') then
		maxint=0
		if (maxdn.eq.255) then
			spike=.false.
		else
			spike=.true.
			maxint=maxdn+1			!one count above saturation
		endif
	endif
	if (fmt.eq.'HALF') then
		maxint=-32768
		if (maxdn.eq.32767) then
			spike=.false.
		else
			spike=.true.
			maxint=maxdn+1			!one count above saturation
		endif
	endif
	if (fmt.eq.'FULL') then
		maxint=-65536
		if (maxdn.eq.65535) then
			spike=.false.
		else
			spike=.true.
			maxint=maxdn+1			!one count above saturation
		endif
	endif
	call xvp('SPIKE',spikeval,cnt)		!PIXEL MAXIMUM VALUE
	if (cnt.eq.0) then
		hispike=maxint 
	else
		hispike=spikeval
		spike=.true.
	endif
	if (spike) then
		maxdn=hispike-1
		write (outline,10100) hispike,maxdn 
10100 format ('STARCAT3: SPIKE=',i10,' MAXD=',i10,' reset MAXD=SPIKE-1')
		call xvmessage (outline,' ')
	endif
	if (fmt.eq.'BYTE') minint=0
	if (fmt.eq.'HALF') minint=-32768
	if (fmt.eq.'FULL') minint=minintg

	call xvp('LOVAL',lospike,cnt)			!PIXEL MINIMUM INTENSITY

	if (cnt.eq.0) 	then
		lospike=minint
	else
		if (.not.iauto) then
			if (lospike.ge.ithr) then
				if (ithr.eq.minintg) then
					lospike=ithr
				else
					lospike=ithr-1
				endif
			write (outline,10110) lospike,ithr 
10110 format ('STARCAT3: LOVAL=',i10,' THRE=',i10,' reset LOVAL=THRE-1')
			call xvmessage (outline,' ')
			endif
		endif
	endif
c
c  the following are object rejection criteria parameters
c	maxsum & minsum; maxpix & minpix; maxns & minns; maxnl & minnl
c
	maxsum=1.0e38
	call xvp('MAXI',maxsum,cnt)	!object maximum sum, in counts
	minsum=1.0
	call xvp('MINI',minsum,cnt)	!object minimum sum, in counts
	maxpix=maxintg
	call xvp('MAXP',maxpix,cnt)	!OBJECT MAXIMUM AREA, in pixels
	minpix=3
	call xvp('MINP',minpix,cnt)	!OBJECT MINIMUM AREA, in pixels
	minns=1	
	call xvp('MINS',minns,cnt)	!OBJECT MINIMUM SAMPLE DIMENSION
	maxns=1000000	
	call xvp('MAXS',maxns,cnt)	!OBJECT MAXIMUM SAMPLE DIMENSION
	minnl=1
	call xvp('MINL',minnl,cnt)	!OBJECT MINIMUM LINE DIMENSION
	maxnl=1000000
	call xvp('MAXL',maxnl,cnt)	!OBJECT MAXIMUM LINE DIMENSION
C asteroid option
	asteroid=.false.
	asteroid=xvptst('ASTEROID')
	if (asteroid) then
		call xvparm('SAOMINP',saominp,count,def,0)
		call xvparm('ASTMINP',astminp,count,def,0)
		call xvparm('SAORATIO',saoratio,count,def,0)
		call xvparm('ASTRATIO',astratio,count,def,0)
		call xvparm('ASTMAXW',astmaxw,count,def,0)
		call xvparm('ASTMAXL',astmaxl,count,def,0)
	endif
	return
	end
c============================================================================
	subroutine centfind (iuni,ouni,nods,sl,ss,nl,ns,
     1 nlin1,numobj,ibis,columns,rows,iprnt,fmt,
     2 maxdn,iauto,sens,ithr,idef,
     3 maxsum,minsum,spike,
     4 iback,imindn,imaxdn,minpix,maxpix,lospike,hispike,minns,maxns,
     5 minnl,maxnl,asteroid,saominp,astminp,saoratio,astratio,
     6 astmaxw,astmaxl,fullrecord,realrecord,asciirecord,nfull,
     7 nreal,nascii)
c
C*******************
C    Find all objects in field which meet certain criteria specified
C    in input parameters
C*******************
c
	implicit none
	include 'errdefs'
c
	common /catalog/ttype,ibfmt,tunits
c
	integer*4 maxobjs	!if maxobjs > 32767 make sure I*2 variables
	PARAMETER (maxobjs=2000)		!change nbin below
	character*5 ibfmt(21)		!column format
	character*8 ttype(21)		!column group
	character*16 tunits(21)		!column units
	byte outdata(32767)
	integer*2 flag(32767,2)
	integer*2 isls(maxobjs),isss(maxobjs),el(maxobjs),es(maxobjs)
	integer*2 cntr(maxobjs,2)
	integer*4 l,nl,ns,line,iss,isl,iuni,stat
	integer*4 j,ss,nods,numobj,ii
	integer*4 newc,nolc,n1,nbin,oldid,isw,numsat
	integer*4 npix,ithr,i,iback,maxdn,n2,num,newid,nlin1
	integer*4 saocnt,astcnt,sl,ouni(3),minpix
	integer*4 hispike,lospike,maxpix,minns,columns,rows,ibis
	integer*4 maxns,minnl,maxnl,saominp,astminp,imindn,imaxdn
	integer*4 fullrecord,realrecord,asciirecord,abendc,rejobj
	integer*4 nfull,nreal,nascii,scversion,scunum,maxfreq
	integer*4 id(maxobjs),npixs(maxobjs),satcnt(maxobjs),ibuf(7)
	integer*4 in(32767),rejcnt(maxobjs)
	integer*4 ohist(0:255),hist(-32768:32767),fhist(-65536:65535)
	logical*4 asteroid,idef,iprnt,iauto,spike
        real   rthresh
	real*4 saoratio,astratio
	real*4 astmaxw,astmaxl,sens,mean,sigma,median,mode,risump,xsigma
	real*4 ricens,maxsum,minsum
	real*4 rbuf(11),sumdn(maxobjs)
	real*8 l2,ij,iumpjl,iumpj2,iumpl2,rl,r8isump,rsumpl
	real*8 cenx(maxobjs),ceny(maxobjs),sumpjl(maxobjs)
        real*8 sumpj2(maxobjs)
	real*8 sumpl2(maxobjs)
	character*8 fmt
	character*16 openmode,iborg,scunits(50)
	character*20 abuf(3)
	character*150 heading
	character*150 outline
c
C    1/'12345678901234567890123456789012345678901234567890123456789012345
C    256789012345678901234567890123456789'/
C
	data heading
     1/'  NUMBER INTENSITY    AREA       SL       SS       NL       NS C|<--
     2ENTSAMP CENTLINE              MAJ AXIS   MIN AXIS   RATIO  ROTANGL
     3E'/
c
c
c The starcat catalog is an IBIS-2 tabular file which has the following format.
c
c
c	***************************************************************
c	descriptor	description			format	units
c	________________________________________________________
c	entry		catalog entry number	full		<none>		
c	DN-bkg		sum of DNs - background	real		counts
c	tot pix		total pixel count 		full		<none>
c	sl			starting line			full		<none>
c	ss			starting sample		full		<none>
c	nl			number of lines		full		<none>
c	ns			number of samples		full		<none>
c	centsamp		center sample			real		<none>
c	centline		center line			real		<none>
c	maj axis		major axis length		real		<none>
c	min axis		minor axis length		real		<none>
c	ratio		ratio minor/major axis	real		<none>
c	rotangle		rotation angle of ellip	real		degrees
c	calalog		source catalog			ascii-A20	<none>
c	ID			catalog ID			ascii-A16	<none>
c	RA			right ascension		real		hours
c	dec			declination			real		degrees
c	mag calc		calculated magnitude	real		<none>
c	mag ref		catalog magnitude		real		<none>
c	class		classification			ascii-A8	<none>
c	sat pixs		# saturated pixels		full		<none>
c	***************************************************************
c
C	The old STARCAT catalog format was the following
C
C*************************
C     IBUF(1)=SUM(DN-BACK)			I*4
C     IBUF(2)=SUM PIXELS				I*4
C     IBUF(3)=SL  					I*4
C     IBUF(4)=SS  					I*4
C     IBUF(5)=NL  					I*4
C     IBUF(6)=NS  					I*4
C     IBUF(7)=CENTROID SAMPLE  		R*4
C     IBUF(8)=CENTROID LINE  			R*4
C     IBUF(9)=major axis length  		R*4
C     IBUF(10)=minor axis length  		R*4
C     IBUF(11)=object identifier	  	R*4 - ("asteroid" option only)
C     IBUF(12)=rotation angle degrees 	R*4
C**********************
c	
	nbin=2000		!MAX NUMBER OF OBJECTS PER 2 LINES - same as maxobjs
	iborg='ROW'
	columns=21
	rows=maxobjs		!initial value - will be increased if needed
	openmode='WRITE'
	scversion=2			!as of 1/21/95 - 21 column (type 2)
	call scopen (ouni(1),ibis,columns,rows,ibfmt,iborg,ttype,
     1 tunits,scunits,scunum,fullrecord,realrecord,asciirecord,
     2 nfull,nreal,nascii,openmode,scversion)
c
	if (iauto) then
		call autot (iuni,fmt,sl,ss,nl,ns,iback,ohist,
     1	hist,fhist,mean,sigma,maxfreq,imindn,imaxdn,median,mode,
     2	xsigma)
		write (outline,10300) imindn
10300 format ('MIN_DN = ',i7,' for image')
		call xvmessage (outline,' ')
c note: imaxdn has no effect on maxdn specified in parmeters
		write (outline,10310) imaxdn
10310 format ('MAX_DN = ',i7,' for image')
		call xvmessage (outline,' ')
		write (outline,10320) mean
10320 format ('MEAN   = ',f9.2,' for image')
		call xvmessage (outline,' ')
		write (outline,10330) sigma
10330 format ('SIGMA  = ',f9.2,' for image')
		call xvmessage (outline,' ')
		write (outline,10335) median
10335 format ('MEDIAN = ',f9.2,' for image')
		call xvmessage (outline,' ')
		write (outline,10337) mode
10337 format ('MODE   = ',f9.2,' for image')
		call xvmessage (outline,' ')
c	call window into image

		rthresh=mean+sens*xsigma
                if ( rthresh .GT. INT(rthresh) )  then
                   ithr = INT(rthresh) + 1   ! Round up to next integer
                else
                   ithr = INT(rthresh)
                end if
		lospike=ithr-1
		write (outline,10340) sens
10340 format ('SENSITIVITY = ',f9.2,' as given')
		call xvmessage (outline,' ')
		write (outline,10345) xsigma
10345 format ('XSIGMA = ',f9.2,' as computed')
		call xvmessage (outline,' ')
		write (outline,10350) ithr
10350 format ('AUTOTHRESHOLD (MEAN+SENSITIVITY*XSIGMA) = ',i7)
		call xvmessage (outline,' ')
		write (outline,10355) lospike
10355 format ('LOVAL  = ',i8)
		call xvmessage(outline,' ')
	else
		write (outline,10360) ithr
10360 format ('THRESH = ',i6,' as given')
		call xvmessage (outline,' ')
	endif
c
c**			debugging purposes only
c**	if (.not.idef) then
c**		ithr=mean+sigma
c**	endif
c
c	print out heading
	if (iprnt) then
		call xvmessage (heading,' ')    !align with PRINZ.
	endif

	NLIN1=0			!output catalog line counter
	OLDID=1			!initialize ID 
	ISW=1			!initialize flag buffer to buffer #1 
	NPIX=NS			!NUMBER OF PIXELS =  NUMBER OF SAMPLES/LINE
	NUMOBJ=0			!RUNNING COUNTER OF REAL OBJECTS FOUND
	NOLC=0
	NEWC=0
	saocnt=0		!no asteroid candidate
	astcnt=0		!no sao catalog star candidate
	ISL=SL			!input starting line
	ISS=SS			!input starting sample
C*******************
C     CLEAR FLAG BUFFERS
C
C	The FLAG buffer is a 2 line array equal in width to the image
C	which associates an object ID with each sample. ISW indicates
C	the current and previous line in order to find contiguous pixels
C	for each object.
C*******************
	CALL MVE(2,NPIX,0,FLAG(1,1),0,1)
	CALL MVE(2,NPIX,0,FLAG(1,2),0,1)
	CALL MVE(7,NBIN,0.0,sumdn,0,1)
	CALL MVE(4,NBIN,0,NPIXS,0,1)
	CALL MVE(8,NBIN,0d0,CENX,0,1)
	CALL MVE(8,NBIN,0d0,CENY,0,1)
	CALL MVE(8,NBIN,0d0,sumpl2,0,1)
	CALL MVE(8,NBIN,0d0,sumpj2,0,1)
	CALL MVE(8,NBIN,0d0,sumpjl,0,1)
	call mve(4,nbin,0,satcnt,0,1)
	call mve(4,nbin,0,rejcnt,0,1)
	CALL MVE(-6,NBIN,32767,ISLS,0,1)
	CALL MVE(-6,NBIN,32767,ISSS,0,1)
	CALL MVE(2,NBIN,0,EL,0,1)
	CALL MVE(2,NBIN,0,ES,0,1)
	CALL MVE(2,NBIN,0,CNTR(1,1),0,1)
	CALL MVE(2,NBIN,0,CNTR(1,2),0,1)
c
c------> MAIN LOOP: Read input in line by line, find objects <------------
	DO 1000 L=1,NL
c				!print out a summary every 500 lines
	if (mod(L,500).eq.0) then
		write (outline,10410) l,numobj
10410 format ('Line# = ',i7,'        #Objects = ',i7)
		call xvmessage(outline,' ')
	endif
	LINE=L+ISL-1
c	if (line.eq.596) then
c		call xvmessage ('Here',' ')
c	endif
C     read the picture to process.
C	INPUT IMAGE DATA WILL BE IN BUFFER IN(NS)
C
	call xvread(iuni,in,stat,'LINE',line,'SAMP',iss,'NSAMPS',ns,' ')
	if (stat.eq.END_OF_FILE) GO TO 1004
	call chkstat(stat,'XVread error on input image', 1,0,0) !ABEND if error.
	if (nods.gt.1) call mve (1,ns,0,outdata,0,1)    !0's to output
C*******************
C     SEARCH IN(1) to in(npix) FOR AN OBJECT .ge. threshold
C*******************
      NEWC=0
      N1=1				!initialize starting samp position
c  Now start search on the line, l
c--> Find all objects on one line loop (goes to 200)
c
c  I think this algorithm will have some problems if mean is lt 0
c  It was originally written for byte images where lowest dn was 0
c
105	continue
	ricens=0.0			!this initializes centroid function
	risump=0.0			!init sum of dns
	iumpj2=0
	iumpjl=0
	iumpl2=0
	numsat=0			!initialize saturation counter for object
	rejobj=0			!valid object (for minint comparison)
      do 110 j=n1,npix
		if (in(j).ge.ithr) go to 120	!image > threshold?
110   continue
C*******************
C     finished line!
C*******************
      go to 200
120   continue
	N1=J				!mark starting samp position of object
      L2=L*L
      do 130 j=n1,npix
		i=in(j)-iback
      	if (in(j).lt.ithr) go to 140	!image < threshold?
C*******************
C     Have object - Update centroid numerator
C	- 1st moment of marginal distribution in x [sample direction]
C*******************
c	check for low value pixel modification  criteria
		if (in(j).lt.lospike) i=lospike-iback	!if dn lt lospike, then make lospike
		if (in(j).ge.maxdn) then
			i=maxdn
			numsat=numsat+1
		endif
		if (spike) then
			if (in(j).ge.hispike) rejobj=1  !reject object after enclosure
		endif
		ij=dble(i*j)			!1st moment in sample
		ricens=ricens+sngl(ij)
		risump=risump+float(i)		!add new intensity to sum dn's
		iumpj2=iumpj2+ij*j		!2nd moment in sample
		iumpjl=iumpjl+ij*L
		iumpl2=iumpl2+i*L2		!2nd moment in line
130   continue

      J=NPIX+1				!at end of line plus 1
140   N2=J-1				!mark ending position of object
C*******************
C     FILL IN STAR AREA ON 2ND OUTPUT FILE if greater than min ns
C	NOTE:  This occasionally marks certain rejected objects - esp single pix.
C*******************
	if ((n2-n1+1).ge.minns) then
		do ii=n1,n2
			outdata(ii)=255
		enddo
	endif
C*******************
C     FIND ID OF THIS OBJECT FROM PREVIOUS FLAG LINE
C	CURRENT  FLAG LINE = FLAG(NS,ISW)
C	PREVIOUS FLAG LINE = FLAG(NS,3-ISW)
C*******************
c
c**** got to figure out where to check rejobj
c****
      CALL MATCH (FLAG(1,3-ISW),N1,N2,NUM,ID,NPIX)
      NEWID=ID(1)
      IF(NUM.LT.2) GO TO 101
C*******************
C     IF MANY IDs,  CONDENSE TO ONE ID
C*******************
	CALL JJLCONCAT(sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,CENY,
     1               sumpj2,sumpjl,sumpl2,satcnt,rejcnt,NUM,ID)
      GO TO 102
101   continue
C*******************
C     IF NO ID,  CREATE ONE
C     UPDATE NEW FLAG BUFFER
C     UPDATE NEW COUNTER
C     UPDATE THE STATISTICS BUFFERS
C*******************
	if (num.eq.0) then
		CALL FIND (NBIN,NEWID,sumdn,OLDID)
	endif
102	continue
c	now have our id
	CALL MVE (-6,N2-N1+1,NEWID,FLAG(N1,ISW),0,1)
      NEWC=NEWC+1
      CNTR(NEWC,ISW)=NEWID
	sumdn(newid)=sumdn(newid)+risump	!sum of intensities
	NPIXS(NEWID)=NPIXS(NEWID)+N2-N1+1	!number of pixels in area
	CENX(NEWID)=CENX(NEWID)+dble(ricens)	!centroid in x-direction
	r8isump=dble(risump)
	rl=dble(l)
	rsumpl=r8isump*rl
      CENY(NEWID)=CENY(NEWID)+rsumpl		!centroid in y-direction
         SUMPJ2(NEWID) = SUMPJ2(NEWID)+IUMPJ2
         SUMPJL(NEWID) = SUMPJL(NEWID)+IUMPJL
         SUMPL2(NEWID) = SUMPL2(NEWID)+IUMPL2
      IF (ISLS(NEWID).GT.L) ISLS(NEWID)=L
      IF (ISSS(NEWID).GT.N1) ISSS(NEWID)=N1
      IF (ES(NEWID).LT.N2) ES(NEWID)=N2
      EL(NEWID)=L
	satcnt(newid) = satcnt(newid)+numsat	!number of saturated pixels
	rejcnt(newid) = rejcnt(newid)+rejobj
C*******************
C     SEND BACK FOR MORE DATA ON SAME LINE
C*******************
      N1=N2+2			!skip at least two pixels for next object
      if (n1.lt.npix) go to 105	!loop back if not at end of line
C--> FOUND ALL OBJECTS on one line - loop back to 105
200   CONTINUE			!formerly 103
C*******************
C     WRITE OUT 2ND and 3RD OUTPUT FILES
C*******************
	if (nods.gt.1) then
		call xvwrit(ouni(2),outdata,stat,'LINE',l,'NSAMPS',ns,
     1	' ')
		call chkstat (stat,'XVwrit error on 2nd output image', 1,0,0)
	endif
	if (nods.eq.3) then
		call xvwrit(ouni(3),flag(1,isw),stat,'LINE',l,'NSAMPS',
     1	ns,' ')
		call chkstat (stat,'XVwrit error on 3rd output image', 1,0,0)
	endif
C*******************
C     SEARCH BOTH FLAG BUFFERS FOR
C     TERMINATED STARS AND WRITE TO DATA SET
C*******************
      CALL EOL(sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,CENY,
     1 sumpj2,sumpjl,sumpl2,MINPIX,satcnt,rejcnt,rbuf,abuf,IBUF,
     2 NPIX,NLIN1,NL,L,IPRNT,CNTR(1,3-ISW),
     3 CNTR(1,ISW),NOLC,NEWC,numobj,maxdn,
     4 MAXPIX,minsum,maxsum,MINNS,MAXNS,
     5 MINNL,MAXNL,OUNI,ibis,rows,
     6 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     7 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     8 nascii,fmt)
C*******************
C     ZERO THE OLD FLAG LINE
C     AND SWITCH FLAGS
C*******************
      CALL MVE(2,NPIX,0,FLAG(1,3-ISW),0,1)
      ISW=3-ISW
      NOLC=NEWC
1000   CONTINUE
c------> END MAIN LOOP: Read input completely, found objects <------------
C
1004   CONTINUE
C	Update nr of catalog file if greater that opening value
	if (nlin1.gt.rows) then
		rows=rows+1000
		call ibis_file_set(ibis,'nr',rows,stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendc)
	endif
c
	write (outline,10500) numobj
10500 format ('*** Total of ',i10,' stars found ***')
	call xvmessage (outline,' ')
	return
	end
c****************************************************************************
	SUBROUTINE FIND (NBIN,NEWID,sumdn,OLDID)
C*******************
C     TO FIND A NEW BIN POSITION
C     NBIN=LENGTH OF BIN BUFFERS
C     NEWID=NEW BIN VALUE RETURNED
C     sumdn=SUM DN BUFFER
C     OLDID=LAST FOUND I.D.
C*******************
	implicit none
	INTEGER*4 OLDID
	integer*4 j,nbin,newid
	real*4 sumdn(1)
	character*100 outline
c
      DO 30 J=OLDID,NBIN
		if (sumdn(j).eq.0.0) go to 40
30    CONTINUE
      DO 35 J=1,OLDID
		if (sumdn(j).eq.0.0) go to 40
35    CONTINUE
	write (outline,10100) nbin
10100 format ('**SUBROUTINE FIND - All ',i4,' bins filled')
	call xvmessage (outline,' ')
	CALL ABEND
c
40    NEWID=J
      OLDID=J
      RETURN
      END
c****************************************************************************
	SUBROUTINE JJLCONCAT(sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,
     1 CENY,sumpj2,sumpjl,sumpl2,satcnt,rejcnt,NUM,ID)
C*******************
C     ROUTINE TO CONCATENATE MANY I.D. S
C	NUM = Number of objects located
C*******************
	implicit none
	INTEGER*2 ISLS(1),ISSS(1),EL(1),ES(1)
	INTEGER*4 OLDID,NPIXS(1),ID(1),satcnt(1),rejcnt(1)
	integer*4 newid,num,j
	real*4 sumdn(1)
	real*8 CENX(1),CENY(1),sumpj2(1),sumpjl(1),sumpl2(1)
c
      NEWID=ID(1)
      DO 10 J=2,NUM
      OLDID=ID(J)
	sumdn(newid)=sumdn(newid)+sumdn(oldid)
	sumdn(oldid)=0.0
	satcnt(newid)=satcnt(newid)+satcnt(oldid)
	satcnt(oldid)=0
	rejcnt(newid)=rejcnt(newid)+rejcnt(oldid)
	rejcnt(oldid)=0
      NPIXS(NEWID)=NPIXS(NEWID)+NPIXS(OLDID)
      NPIXS(OLDID)=0
      IF(ISLS(NEWID).GT.ISLS(OLDID)) ISLS(NEWID)=ISLS(OLDID)
      ISLS(OLDID)=32767
      IF(ISSS(NEWID).GT.ISSS(OLDID)) ISSS(NEWID)=ISSS(OLDID)
      ISSS(OLDID)=32767
      IF(EL(NEWID).LT.EL(OLDID)) EL(NEWID)=EL(OLDID)
      EL(OLDID)=0
      IF(ES(NEWID).LT.ES(OLDID)) ES(NEWID)=ES(OLDID)
      ES(OLDID)=0
      CENX(NEWID)=CENX(NEWID)+CENX(OLDID)
      CENX(OLDID)=0.0
      CENY(NEWID)=CENY(NEWID)+CENY(OLDID)
      CENY(OLDID)=0.0
          SUMPJ2(NEWID)=SUMPJ2(NEWID)+SUMPJ2(OLDID)
          SUMPJ2(OLDID)=0.0
          SUMPJL(NEWID)=SUMPJL(NEWID)+SUMPJL(OLDID)
          SUMPJL(OLDID)=0.0
          SUMPL2(NEWID)=SUMPL2(NEWID)+SUMPL2(OLDID)
          SUMPL2(OLDID)=0.0
10    continue
      RETURN
      END
c============================================================================
      subroutine labproc (ouni,nods,sl,ss,nl,ns,sens,ithr,nlin1,iss,isl,
     1 numobj,astcnt,saocnt,asteroid,
     2 nlimg,nsimg,iback,iauto,idef,version,fmt,imindn,imaxdn,
     3 maxnl,maxns,maxdn)
c
	implicit none
c
	integer*4 istat,ouni(3),nods,sl,ss,nl,ns,iback,nlin1
	integer*4 imindn,imaxdn,isl,iss,numobj,saocnt,astcnt
	integer*4 ithr,nlimg,nsimg,j,maxnl,maxns,maxdn
	logical*4 idef,iauto,asteroid
	real*4 mean,sigma,sens
	character*8 threshold,autothresh,man,default
	character*8 fmt,area,FULL,SUBAREA
	character*28 filen(3)
	character*35 version
c
	data man/'ENTERED '/,default/'DEFAULT '/
	data autothresh/'AUTO    '/
	data filen	/'STARCAT TYPE   2 CATALOG    ',
     1			 'STARCAT FILLED DISPLAY FILE ',
     2			 'STARCAT FLAGGED OBJECT FILE '/
c
	FULL='FULL    '
	SUBAREA='SUBAREA '

	AREA=FULL
	if (nl.ne.nlimg) AREA=SUBAREA
	if (ns.ne.nsimg) AREA=SUBAREA
	IF (ISL.NE.1) AREA=SUBAREA
	IF (ISS.NE.1) AREA=SUBAREA
	if (iauto) then
		threshold=autothresh
	else
		if (idef) then
			threshold=default
		else
			threshold=man
		endif
	endif
	do j=1,nods
		call xladd(ouni(j),'HISTORY','DATASET',filen(j),istat,
     1 'FORMAT','STRING',' ')
		call xladd(ouni(j),'HISTORY','CREATOR',version,istat,
     1 'FORMAT','STRING',' ')
		call xladd(ouni(j),'HISTORY','IMGFMT',fmt,istat,
     1 'FORMAT','STRING',' ')
		call xladd (ouni(j),'HISTORY','THRESHOLD',threshold,istat,
     1 'FORMAT','STRING',' ')
		call xladd (ouni(j),'HISTORY','THRESHVAL',ithr,istat,
     1 'FORMAT','INT',' ')
		if (iauto) then
			call xladd(ouni(j),'HISTORY','MIN_DN',imindn,istat,
     1 'FORMAT','INT',' ')
			call xladd(ouni(j),'HISTORY','MAX_DN',imaxdn,istat,
     1 'FORMAT','INT',' ')
			call xladd (ouni(j),'HISTORY','MEAN',mean,istat,
     1 		'FORMAT','REAL',' ')
			call xladd (ouni(j),'HISTORY','SIGMA',sigma,istat,
     1 		'FORMAT','REAL',' ')
			call xladd(ouni(j),'HISTORY','SENSITIVITY',sens,istat,
     1		'FORMAT','REAL',' ')
c
		endif
		call xladd (ouni(j),'HISTORY','BKGVALUE',iback,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','SATURAT_VAL',maxdn,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','MAX_LINESIZE',maxnl,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','MAX_SAMPSIZE',maxns,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','TAB_AREA',area,istat,
     1 'FORMAT','STRING',' ')
		call xladd (ouni(j),'HISTORY','SL_IMAGE',SL,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','SS_IMAGE',SS,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','NL_IMAGE',NL,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','NS_IMAGE',NS,istat,
     1 'FORMAT','INT',' ')
		call xladd (ouni(j),'HISTORY','OBJECTS',numobj,istat,
     1 'FORMAT','INT',' ')
c
		if (asteroid) then
			call xladd(ouni(j),'HISTORY','ASTEROID',astcnt,istat,
     1	'FORMAT','INT',' ')
			call xladd(ouni(j),'HISTORY','SAOSTARS',saocnt,istat,
     1	'FORMAT','INT',' ')
		endif
c
	enddo

	return
	end
c****************************************************************************
	SUBROUTINE MATCH (FLAG,N1,N2,NUM,ID,NS)
C*******************
C     RETURN I.D. OF OBJECT FOUND ON PREVIOUS LINE
C     FLAG=OLD FLAG BUFFER
C     N1=BEGIN  N2=END  OF OBJECT
C     NUM = NUMBER OF OBJECTS LOCATED
C     ID=I.D. OF OBJECTS LOCATED
C*******************
	implicit none
	INTEGER*2 FLAG(1)
	INTEGER*4 ID(1)
	integer*4 m1,m2,n1,n2,ns,num,j,k,n
c
      M1=N1-1				!point 1 pixel to left of object
      M2=N2+1				!point 1 pixel to right of object
      IF(M1.LT.1) M1=1
      IF(M2.GT.NS) M2=NS
      NUM=0				!running counter of objects being checked
      K=0
C*******************
C     FIND ALL FLAGS - search for this object # in previous line flag buffer
C*******************
      DO 10 J=M1,M2
      	IF(FLAG(J).EQ.K) GO TO 10
      	IF(FLAG(J).EQ.0) GO TO 10
      	K=FLAG(J)
      	NUM=NUM+1
      	ID(NUM)=K
10    CONTINUE
      IF(NUM.LT.2) RETURN
C*******************
C     REJECT DUPLICATES
C*******************
	call isort (id,1,num)
      N=1
      K=ID(1)
      DO 20 J=2,NUM
      	IF(K.EQ.ID(J)) GO TO 20
      	N=N+1
      	ID(N)=ID(J)
      	K=ID(J)
20    CONTINUE
      NUM=N
      RETURN
      END
c****************************************************************************
	SUBROUTINE EOL(sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,CENY,
     1 sumpj2,sumpjl,sumpl2,MINPIX,satcnt,rejcnt,rbuf,abuf,
     2 IBUF,NS,NLIN1,NL,LINE,IPRNT,OLCNTR,NECNTR,NOLC,
     3 NEWC,numobj,maxdn,MAXPIX,minsum,maxsum,MINNS,
     4 MAXNS,MINNL,MAXNL,OUNI,ibis,rows,
     5 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     6 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     7 nascii,fmt)
C*******************
C     SEARCH FOR FINISHED OBJECTS IN JUST COMPLETED LINE
C*******************
	implicit none
	INTEGER*2 OLCNTR(1),NECNTR(1)
	INTEGER*2 ISLS(1),ISSS(1),EL(1),ES(1)
	integer*4 j,k,nlin1,ibis,rows,maxdn
	integer*4 newc,newid,nolc,nl,ns,numobj
	integer*4 line,maxpix,minpix
	integer*4 maxnl,minnl,maxns,minns
	integer*4 saominp,astminp,saocnt,astcnt
	integer*4 fullrecord,realrecord,asciirecord,nfull,nreal,nascii
	integer*4 ibuf(nfull)
	integer*4 npixs(1),satcnt(1),rejcnt(1)
	integer*4 ouni(3)
	logical*4 asteroid,iprnt
	real*4 astmaxl,astmaxw,astratio,saoratio,maxsum,minsum
	real*4 rbuf(nreal),sumdn(1)
	real*8 CENX(1),CENY(1),sumpj2(1),sumpjl(1),sumpl2(1)
	character*8 fmt
	character*20 abuf(3)
c
C*******************
C     SEARCH OLCNTR FOR IDs NOT IN NECNTR
C*******************
      IF(LINE.EQ.NL) GO TO 51
      IF(NOLC.EQ.0) RETURN
      IF(NEWC.EQ.0) GO TO 51
      DO 10 J=1,NOLC
 	     NEWID=OLCNTR(J)
      	DO 20 K=1,NEWC
	     	 IF(NEWID.EQ.NECNTR(K)) GO TO 10
20    CONTINUE
C*******************
C     STAR NOT CONTINUED
C*******************
	CALL FIN(NEWID,sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,CENY,
     1 sumpj2,sumpjl,sumpl2,ibis,MINPIX,satcnt,rejcnt,rows,abuf,
     2 IBUF,rbuf,NLIN1,IPRNT,numobj,maxdn,
     3 MAXPIX,minsum,maxsum,MINNS,MAXNS,MINNL,MAXNL,OUNI,
     4 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     5 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     6 nascii,fmt)
10    CONTINUE
      RETURN
C
C*******************
C     FINISH OFF EVERYTHING - JUST FINISHED LINE
C*******************
51    IF(NOLC.EQ.0) RETURN
      DO 50 J=1,NOLC
      NEWID=OLCNTR(J)
	CALL FIN(NEWID,sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,CENY,
     1 sumpj2,sumpjl,sumpl2,ibis,MINPIX,satcnt,rejcnt,rows,abuf,
     2 IBUF,rbuf,NLIN1,IPRNT,numobj,maxdn,
     3 MAXPIX,minsum,maxsum,MINNS,MAXNS,MINNL,MAXNL,OUNI,
     4 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     5 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     6 nascii,fmt)
50    CONTINUE
	RETURN
	END
c****************************************************************************
	SUBROUTINE FIN (NEWID,sumdn,NPIXS,ISLS,ISSS,EL,ES,CENX,CENY,
     1 sumpj2,sumpjl,sumpl2,ibis,
     2 MINPIX,satcnt,rejcnt,rows,abuf,ibuf,rbuf,NLIN1,IPRNT,numobj,
     3 maxdn,MAXPIX,minsum,maxsum,MINNS,MAXNS,MINNL,MAXNL,OUNI,
     4 saominp,astminp,saoratio,astratio,astmaxw,astmaxl,asteroid,
     5 saocnt,astcnt,fullrecord,realrecord,asciirecord,nfull,nreal,
     6 nascii,fmt)
C*******************
C     ROUTINE TO TERMINATE AN OBJECT AND WRITE TO CATALOG
C     ibuf=catalog I*4 data
c	rbuf=catalog R*4 data
c	abuf=catalog ASCII data
C*******************
	implicit none
	integer*2 ISLS(1),ISSS(1),EL(1),ES(1)
	integer*4 npixs(1),satcnt(1),rejcnt(1)
	integer*4 ouni(3),stat
	integer*4 il,ind,is,ibis,abendc,rows,maxdn
	integer*4 maxnl,minnl,maxns,minns,maxpix,minpix
	integer*4 newid,nlin1,nrot,numobj,nfull,nreal,nascii
	integer*4 saominp,astminp,saocnt,astcnt
	integer*4 fullrecord,realrecord,asciirecord
	integer*4 ibuf(nfull)
	logical*4 asteroid,iprnt
	real*4 rbuf(nreal),sumdn(1)
	real*4 mu20,mu02,mu11,rad,ratio,minsum,maxsum
	real*4 r,astratio,saoratio,theta,astmaxw,astmaxl
	real*4 amatrx(2,2),dmatrx(2),vmatrx(2,2)
	real*8 CENX(1),CENY(1),sumpj2(1),sumpjl(1),sumpl2(1)
	real*8 sump
	character*8 fmt
	character*16 abuf1
	character*20 abuf2,abuf(3)
c
	data rad/57.29577/
	data abuf1/'                '/
	data abuf2/'                    '/
c
c  first, check out object rejection criteria
      IF (NPIXS(NEWID).EQ.0) RETURN		!no object if no area
      IF (NPIXS(NEWID).LT.MINPIX) GO TO 100	!no object if lt min # pixels
      IF (NPIXS(NEWID).GT.MAXPIX) GO TO 100	!no object if gt max pixels
      IL=EL(NEWID)-ISLS(NEWID)+1
      IS=ES(NEWID)-ISSS(NEWID)+1
      IF (IL.LT.MINNL) GO TO 100		!no object if lt min lines
      IF (IL.GT.MAXNL) GO TO 100		!no object if gt max line
      IF (IS.LT.MINNS) GO TO 100		!no onject if lt min samples
      IF (IS.GT.MAXNS) GO TO 100		!no object if gt max samples
	if (fmt.eq.'BYTE') then
		if (sumdn(newid).le.0.0) go to 100		!may have to worry about
	endif
c**      IF (ISUM(NEWID).LT.MININT) GO TO 100		!dn sums lt 0.0
c**      IF (ISUM(NEWID).GT.MAXINT) GO TO 100
	if (sumdn(newid).lt.minsum) go to 100	!no obj if lt min sumdn
	if (sumdn(newid).gt.maxsum) go to 100	!no obj if gt max sumdn
	if (rejcnt(newid).gt.1) go to 100		!no object if 1 dn gt maxint
c
c	OBJECT IS REAL!!
c
	numobj=numobj+1
	sump=dble(sumdn(newid))
C					!Catalog entries
	ibuf(1)=nlin1+1			!1 - entry number
	rbuf(1)=sumdn(newid)		!2 - sum of DNs of object (intensity)
      IBUF(2)=NPIXS(NEWID)		!3 - number of pixels in area
      IBUF(3)=ISLS(NEWID)		!4 - SL of object from size field
      IBUF(4)=ISSS(NEWID)		!5 - SS of object from size field
      IBUF(5)=IL			!6 - NL of object from size field
      IBUF(6)=IS			!7 - NS of object from size field
      RBUF(2)=CENX(NEWID)/sump		!8 - centroid sample position
      RBUF(3)=CENY(NEWID)/sump		!9 - centroid line position
	ibuf(7)=satcnt(newid)		!21 - # of saturated pixels
c
      MU20=SUMPJ2(NEWID)/SUMP-(CENX(NEWID)/(SUMP))**2.
      MU02=SUMPL2(NEWID)/SUMP-(CENY(NEWID)/(SUMP))**2.
      MU11=SUMPJL(NEWID)/SUMP-CENY(NEWID)*CENX(NEWID)/(SUMP)**2.
c
      amatrx(1,1)=mu20
      amatrx(2,2)=mu02
      amatrx(1,2)=mu11
      amatrx(2,1)=mu11
      call JACOBI(amatrx,2,2,dmatrx,vmatrx,nrot,ind)
      if (ind.eq.0) then
        call EIGSRT(dmatrx,vmatrx,2,2)
        theta=-rad*atan2(vmatrx(2,1),vmatrx(1,1))
c						!finish catalog entries
        RBUF(4) = sqrt(abs(dmatrx(1)))		!10 - major axis of object
        RBUF(5) = sqrt(abs(dmatrx(2)))		!11 - minor axis of object
	  ratio=rbuf(5)/rbuf(4)		   	! minor/major axes
        RBUF(6) = ratio				!12 - minor/major ratio
        RBUF(7) = THETA				!13 - rotation of major axis
      else
         numobj=numobj-1
         goto 100
C*** from an earlier incarnation of this program
c        IF ((MU20.NE.MU02).OR.(MU11.NE.0)) THEN
c           THETA=-ATAN2(2.*MU11,MU20-MU02)*rad/2
c        ELSE
c           THETA=-9999.
c        ENDIF
c        RBUF(3) = MU20
c        RBUF(4) = MU02
c        RBUF(5) = MU11
c        RBUF(6) = THETA
c***
      endif

      if (asteroid) then  			! logic for asteroid program
		if (rbuf(4).eq.0.0) rbuf(4)=.00001
		ratio=rbuf(5)/rbuf(4)  	 	! minor/major axes
      	   if((int(rbuf(1)).ge.saominp).and.
     1 		(ratio.gt.saoratio)) then	!if intensity & area > then
			goto 180    			! SAO star candidate
         	   else if((int(rbuf(1)).ge.astminp).and.
     1        (ratio.lt.astratio).and.   	! asteroid candidates
     2        (rbuf(5).le.astmaxw)) then
		  	  r=sqrt(float(ibuf(5)**2+ibuf(6)**2)) 	        	! length
		if (r.lt.astmaxl) goto 190  		! accept object
         endif
         	numobj=numobj-1   				! reject object
         	goto 190
180		continue
c**	RBUF(ISTART+11)=1.0			!11- #1=SAO candidate
		abuf(1)='starcat3'
		abuf(3)='star'
		saocnt=saocnt+1
		go to 195
190		continue
c**	RBUF(ISTART+11)=2.0			!11- #2=asteroid candidate
		abuf(1)='starcat3'
		abuf(3)='asteroid'
		astcnt=astcnt+1
195		continue
      endif
200   continue
c 	skip printing if 'LIST' not given
	if (iprnt) CALL PRINZ(IBUF,RBUF,nfull,nreal)
C*******************
C     LOGIC FOR STATISTICS DATA SET
C*******************
	NLIN1=NLIN1+1		!bump catalog line counter
c
c	go write out catalog
c
	if (nlin1.gt.rows) then
		rows=rows+1000
		call ibis_file_set(ibis,'nr',rows,stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendc)
	endif
c				!catalog
	call ibis_record_write (fullrecord,ibuf,nlin1,stat)
	if (stat.ne.1) call ibis_signal (ibis,stat,abendc)
	call ibis_record_write (realrecord,rbuf,nlin1,stat)
	if (stat.ne.1) call ibis_signal (ibis,stat,abendc)
	call ibis_record_write (asciirecord,abuf,nlin1,stat)
	if (stat.ne.1) call ibis_signal (ibis,stat,abendc)
C*******************
C     CLEAR OUT BINS
C*******************
100	CONTINUE
	sumdn(newid)=0.0
	NPIXS(NEWID)=0
	ISLS(NEWID)=32767
	ISSS(NEWID)=32767
	EL(NEWID)=0
	ES(NEWID)=0
	CENX(NEWID)=0.0
	CENY(NEWID)=0.0
	SUMPJ2(NEWID)=0.0
	SUMPL2(NEWID)=0.0
	SUMPJL(NEWID)=0.0
	satcnt(newid)=0
	rejcnt(newid)=0
	RETURN
	END
c****************************************************************************
      SUBROUTINE PRINZ(IBUF,RBUF,nfull,nreal)
c
c	prints out catalog to log and terminal if 'LIST' keyword given
c
	implicit none
	integer*4 nfull,nreal
	INTEGER*4 IBUF(nfull),i,j
	REAL*4 RBUF(nreal)
	character*150 outline
c
	write (outline,10100) ibuf(1),rbuf(1),(ibuf(i),i=2,6),
     1	(rbuf(j),j=2,7)   !include rotangle after ratio.
10100 format(i8,f9.0,5i9,2f9.2,13x,4f9.2)
	call xvmessage(outline,' ')
      RETURN
      END
c****************************************************************************
      SUBROUTINE JACOBI(A,N,NP,D,V,NROT,ind)
c Computes eigenvalues & eigenvectors.
c A=n by n matrix of real symmetric values in an np by np matrix.
c D=returns n eigenvalues of A.
c V=returns normalized eigenvectors of A in NP by NP matrix.
c NROT=# jacobi rotations required.
c ind= 0(normal) 1(abnormal)
	implicit none
	integer*4 i,ind,ip,iq,j,n,np,nrot
	real*4 A(NP,NP),D(NP),V(NP,NP),B(100),Z(100)
	real*4 c,g,h,s,sm,t,tau,theta
	real*4 tresh
c
c	integer*4 nmax
c	PARAMETER (NMAX=100)
c
      ind=0
      DO 12 IP=1,N
        DO 11 IQ=1,N
          V(IP,IQ)=0.
11      CONTINUE
        V(IP,IP)=1.
12    CONTINUE
      DO 13 IP=1,N
        B(IP)=A(IP,IP)
        D(IP)=B(IP)
        Z(IP)=0.
13    CONTINUE
      NROT=0
      DO 24 I=1,50
        SM=0.
        DO 15 IP=1,N-1
          DO 14 IQ=IP+1,N
            SM=SM+ABS(A(IP,IQ))
14        CONTINUE
15      CONTINUE
        IF(SM.EQ.0.)RETURN
        IF(I.LT.4)THEN
          TRESH=0.2*SM/N**2
        ELSE
          TRESH=0.
        ENDIF
        DO 22 IP=1,N-1
          DO 21 IQ=IP+1,N
            G=100.*ABS(A(IP,IQ))
            IF((I.GT.4).AND.(ABS(D(IP))+G.EQ.ABS(D(IP)))
     *         .AND.(ABS(D(IQ))+G.EQ.ABS(D(IQ))))THEN
              A(IP,IQ)=0.
            ELSE IF(ABS(A(IP,IQ)).GT.TRESH)THEN
              H=D(IQ)-D(IP)
              IF(ABS(H)+G.EQ.ABS(H))THEN
                T=A(IP,IQ)/H
              ELSE
                THETA=0.5*H/A(IP,IQ)
                T=1./(ABS(THETA)+SQRT(1.+THETA**2))
                IF(THETA.LT.0.)T=-T
              ENDIF
              C=1./SQRT(1+T**2)
              S=T*C
              TAU=S/(1.+C)
              H=T*A(IP,IQ)
              Z(IP)=Z(IP)-H
              Z(IQ)=Z(IQ)+H
              D(IP)=D(IP)-H
              D(IQ)=D(IQ)+H
              A(IP,IQ)=0.
              DO 16 J=1,IP-1
                G=A(J,IP)
                H=A(J,IQ)
                A(J,IP)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
16            CONTINUE
              DO 17 J=IP+1,IQ-1
                G=A(IP,J)
                H=A(J,IQ)
                A(IP,J)=G-S*(H+G*TAU)
                A(J,IQ)=H+S*(G-H*TAU)
17            CONTINUE
              DO 18 J=IQ+1,N
                G=A(IP,J)
                H=A(IQ,J)
                A(IP,J)=G-S*(H+G*TAU)
                A(IQ,J)=H+S*(G-H*TAU)
18            CONTINUE
              DO 19 J=1,N
                G=V(J,IP)
                H=V(J,IQ)
                V(J,IP)=G-S*(H+G*TAU)
                V(J,IQ)=H+S*(G-H*TAU)
19            CONTINUE
              NROT=NROT+1
            ENDIF
21        CONTINUE
22      CONTINUE
        DO 23 IP=1,N
          B(IP)=B(IP)+Z(IP)
          D(IP)=B(IP)
          Z(IP)=0.
23      CONTINUE
24    CONTINUE
      ind=1				!1 = error
      RETURN
      END
c****************************************************************************
      SUBROUTINE EIGSRT(D,V,N,NP)
c sorts the eigenvalues into ASCENDING order & moves the
c eigenvector columns in the same order.
	implicit none
	integer*4 i,j,k,n,np 
	real*4 D(NP),V(NP,NP),p
c
      DO 13 I=1,N-1
        K=I
        P=D(I)
        DO 11 J=I+1,N
          IF(D(J).GE.P)THEN
            K=J
            P=D(J)
          ENDIF
11      CONTINUE
        IF(K.NE.I)THEN
          D(K)=D(I)
          D(I)=P
          DO 12 J=1,N
            P=V(J,I)
            V(J,I)=V(J,K)
            V(J,K)=P
12        CONTINUE
        ENDIF
13    CONTINUE
      RETURN
      END
c****************************************************************************
	subroutine autot (iunit,fmt,sl,ss,nl,ns,iback,ohist,
     1 hist,fhist,mean,sigma,maxfreq,mindn,maxdn,median,mode,xsigma)
c	subroutine autot (iunit,iunitb,nids,fmt,sl,ss,nl,ns,iback,ohist,
c 	1 hist,fhist,mean,sigma,maxfreq,mindn,maxdn,median,mode,xsigma)
c
c	computes a general threshold for image
c	
	implicit none
	integer*4 inpbuf(32767)
	integer*4 iunit,sl,ss,nl,ns,maxfreq,mindn,maxdn,npts,i
	integer*4 sumcts,iback,npixels,jmaxdn,jmean
	integer*4 ohist(0:255),hist(-32768:32767),fhist(-65538:65537)
	real*4 mean,sigma,median,mode,xsigma
	logical*4 medianfound
	character*8 fmt
	character*100 outline
c
	medianfound=.false.
	npts=nl*ns	
	if (fmt.eq.'BYTE') then
		call comphist(iunit,sl,ss,nl,ns,iback,ohist,
     1	inpbuf)
		call histat(ohist,npts,mean,sigma,maxfreq)
		mode=0.0
		sumcts=ohist(0)
		median=0.0
		do i=1,255
			if (ohist(i).eq.maxfreq) mode=i
			if (.not.medianfound) then
				sumcts=sumcts+ohist(i)
				if (sumcts.gt.npts/2) then
					medianfound=.true.
					median=i
				endif
			endif
			if (ohist(i).ne.0) go to 10
		enddo
		write (outline,10100)
10100 format ('**SUBROUTINE AUTOT -  All values zero in minimum search')
		call xvmessage (outline,' ')
		call abend
c
10	continue
		mindn=i
		do i=255,1,-1
			if (ohist(i).ne.0) go to 20
		enddo	
		write (outline,10100)
		call xvmessage (outline,' ')
		call abend
c
20	continue
		maxdn=i
		jmean=nint(mean)
		jmaxdn=jmean-mindn-1
		call newsigma1 (ohist,mindn,jmean,jmaxdn,npixels,xsigma)
		return
	endif
c
	if (fmt.eq.'HALF') then
		call comphist2(iunit,sl,ss,nl,ns,iback,hist,
     1	inpbuf)
		call histat2(hist,npts,mean,sigma,mindn,maxdn,maxfreq)
		mode=hist(-32768)
		sumcts=hist(-32768)
		median=0.0
		do i=-32768,32767
			if (hist(i).eq.maxfreq) mode=i
			if (.not.medianfound) then
				sumcts=sumcts+hist(i)
				if (sumcts.gt.npts/2) then
					medianfound=.true.
					median=i
				endif
			endif
		enddo
		jmean=nint(mean)
		jmaxdn=jmean-mindn-1
		call newsigma2 (hist,mindn,jmean,jmaxdn,npixels,xsigma)
	endif
	if (fmt.eq.'FULL') then
		call comphist4(iunit,sl,ss,nl,ns,iback,fhist,
     1	inpbuf)
		call histat4(fhist,npts,mean,sigma,mindn,maxdn,maxfreq)
		mode=fhist(-65536)
		sumcts=fhist(-65536)
		median=0.0
		do i=-65538,65537
			if (fhist(i).eq.maxfreq) then
				mode=i
			endif
			if (.not.medianfound) then
				sumcts=sumcts+fhist(i)
				if (sumcts.gt.npts/2) then
					medianfound=.true.
					median=i
				endif
			endif
		enddo
		jmean=nint(mean)
		jmaxdn=jmean-mindn-1
		call newsigma4 (fhist,mindn,jmean,jmaxdn,npixels,xsigma)
	endif
	return	
	end
c=============================================================================
	SUBROUTINE COMPHIST(iunit,SL,SS,NL,NS,iback,ohist,inpbuf)
C Compute a histogram of a byte image.
C
c*** like standard library routine except inpbuf is integer*4 - not integer*2
c	and subtracts background if required
c
	implicit none
      INTEGER*4 OHIST(256)
      INTEGER*4 INPBUF(32768)		!only difference from library routine
      INTEGER*4 SL,SS,EL,nl,ns,iunit,iback
	integer*4 line,ind

      CALL ZIA(OHIST,256)
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         CALL XVREAD(IUNIT,inpbuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         CALL HSUB(1,NS,INPBUF,OHIST)
      ENDDO

      RETURN
      END
c============================================================================
	SUBROUTINE COMPHIST2(iunit,SL,SS,NL,NS,iback,hist,inpbuf)
C Compute a histogram of a halfword image.
C
c*** like standard library routine except inpbuf is integer*4 - not integer*2
c
	implicit none
      INTEGER*4 HIST(-32768:32767)
      INTEGER*4 INPBUF(32768)		!only difference from library routine
      INTEGER*4 SL,SS,EL,nl,ns,iunit,iback,line,ind,j,idn

      CALL ZIA(HIST,65536)
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         CALL XVREAD(IUNIT,inpbuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         DO J=1,NS

            IDN = INPBUF(J)
            HIST(IDN) = HIST(IDN) + 1
         ENDDO
      ENDDO

      RETURN
      END
c=============================================================================
	SUBROUTINE COMPHIST4(iunit,SL,SS,NL,NS,iback,hist,inpbuf)
C
C Compute a histogram of a halfword image.
c	modeled after comphist2 in standard vicar subroutine library
c	limited to +/- 65535 for unsigned 16-bit CCD work
C
	implicit none
      INTEGER*4 HIST(-65536:65535)
      INTEGER*4 INPBUF(32767)
      INTEGER*4 SL,SS,nl,ns,EL,line,j,iunit,ind,idn,iback
c
      CALL ZIA(HIST,131072)
      EL = SL+NL-1
C     ....Generate histogram
      DO LINE=SL,EL
         CALL XVREAD(IUNIT,inpbuf,ind,'LINE',LINE,'SAMP',SS,'NSAMPS',
     &               NS,' ')
         DO J=1,NS
            IDN = INPBUF(J)
            HIST(IDN) = HIST(IDN) + 1
         ENDDO
      ENDDO

      RETURN
      END
c============================================================================
      SUBROUTINE histat4(FHIST,NPTS,mean,sigma,mindn,maxdn,maxfreq)
C
C   Compute mean and standard deviation (for halfword inputs only).
C
c	modified standard routine histat2
c
	implicit none
	integer*4 mindn,maxdn,maxfreq,npts,j,npixels
      INTEGER*4 FHIST(-65536:65535)
      REAL*4 MEAN,SIGMA,dn
      REAL*8 DMEAN,DSIGMA
c
      DN = 0.0			!DN value
      DMEAN = 0.0D0		!Mean DN of input image
      DSIGMA = 0.0D0		!Standard deviation of input image
      MINDN = 65535
      MAXFREQ = 0

      IF (NPTS.EQ.0) THEN	!Test number of pixels against 0
	MEAN = DMEAN		!Set mean, std. dev. & max. freq.
	SIGMA = DSIGMA		!to 0; set max. and min. DN to 
	MAXDN = MINDN		!32767; then exit program.
        GOTO 15
      ENDIF 

      DO 10 J=-65536,65535
      NPIXELS = FHIST(J)
      IF (NPIXELS.EQ.0) GOTO 10
      DN = J
      DMEAN = DMEAN + NPIXELS*DN
      DSIGMA = DSIGMA + NPIXELS*DN**2
      IF (J.LT.MINDN) MINDN=J
      IF (J.NE.-65536.AND.J.NE.65535.AND.NPIXELS.GT.MAXFREQ)
     &		 MAXFREQ=NPIXELS
   10 CONTINUE

      MAXDN = DN + 0.5
      DMEAN = DMEAN/NPTS
      MEAN = DMEAN
      SIGMA = DSQRT(DSIGMA/NPTS-DMEAN*DMEAN)
   15 RETURN
      END
c==========================================================================
	subroutine newsigma1 (ohist,lowval,midval,highval,npixels,xsigma)
c
c	routine to symmetrize (?) the data around the mean value
c	and compute a new sigma for sky background
c	modelled on subroutine histat
c
	implicit none
	integer*4 lowval,midval,highval,npixels,i,j,index
	integer*4 ohist(256),temphist(256)
	real*4 xsigma
	real*8 dsigma,dn,dmean,sumdn,sum2dn
c
	npixels=0
	dsigma=0.0d0
	sumdn=0.0d0
	sum2dn=0.0d0
	dmean=dble(midval)
	do i=lowval,midval
		temphist(i)=ohist(i)
	enddo
	do i=midval+1,highval
		index=midval-i
		temphist(i)=ohist(midval+index)
	enddo

	do 10 j=lowval,highval
		npixels=npixels+temphist(j)
		if (npixels.eq.0) go to 10
		dn=dble(j)
		sumdn=sumdn+dn
		sum2dn=sum2dn+dn*dn
10	continue
	dsigma=dsqrt((dble(npixels)*sum2dn)-(sumdn*sumdn))
	xsigma=sngl(dsigma)
	return
	end
c==========================================================================
	subroutine newsigma2 (hist,lowval,midval,highval,npixels,xsigma)
c
c	routine to symmetrize (?) the data around the mean value
c	and compute a new sigma for sky background
c	modelled on subroutine histat
c
	implicit none
	integer*4 lowval,midval,highval,npixels,i,j,index
	integer*4 hist(-32768:32767),temphist(-32768:32767)
	real*4 xsigma
	real*8 dsigma,dn,dmean,sumdn,sum2dn
c
	npixels=0
	dsigma=0.0d0
	sumdn=0.0d0
	sum2dn=0.0d0
	dmean=dble(midval)
	do i=lowval,midval
		temphist(i)=hist(i)
	enddo
	do i=midval+1,highval
		index=midval-i
		temphist(i)=hist(midval+index)
	enddo

	do 10 j=lowval,highval
		npixels=npixels+temphist(j)
		if (npixels.eq.0) go to 10
		dn=dble(j)
		sumdn=sumdn+dn
		sum2dn=sum2dn+dn*dn
10	continue
	dsigma=dsqrt((dble(npixels)*sum2dn)-(sumdn*sumdn))
	xsigma=sngl(dsigma)
	return
	end
c==========================================================================
	subroutine newsigma4 (fhist,lowval,midval,highval,npixels,xsigma)
c
c	routine to symmetrize (?) the data around the mean value
c	and compute a new sigma for sky background
c	modelled on subroutine histat
c
	implicit none
	integer*4 lowval,midval,highval,npixels,i,j,index
	integer*4 fhist(-65536:65535),temphist(-65536:65535)
	real*4 xsigma
	real*8 dsigma,dn,dmean,sumdn,sum2dn
c	character*80 outline
c
	npixels=0
	dsigma=0.0d0
	sumdn=0.0d0
	sum2dn=0.0d0
	dmean=dble(midval)
	do i=lowval,midval
		temphist(i)=fhist(i)
	enddo
	do i=midval+1,highval
		index=i-midval
		index=midval-index
		temphist(i)=fhist(index)
c		write (outline,10010) i, index, midval+index
c10010 format (i6,2x,i6,2x,i6)
c		call xvmessage(outline,' ')
	enddo
	do 10 j=lowval,highval
		npixels=npixels+temphist(j)
		if (npixels.eq.0) go to 10
		dn=dble(j)
		sumdn=sumdn+dn
		sum2dn=sum2dn+dn*dn
10	continue
	dsigma=dsqrt((dble(npixels)*sum2dn)-(sumdn*sumdn))
	xsigma=sngl(dsigma)
	return
	end
c===========================================================================
	subroutine putparm (numobj)
c
	implicit none
	integer*4 numobj
	integer*4 parb(500),xcont,xadd,stat
c
	call xqini (parb,500,xcont)
	call xqintg (parb,'OBJECTS',1,numobj,xadd,stat)
	call xvqout (parb,stat)
	call chkstat (stat,' xvqout error', 1,0,0)    !ABEND if error.
c
	return
	end
c===========================================================================
	subroutine scopen (unit,ibis,columns,rows,ibfmt,iborg,ttype,
     1 tunits,scunits,scunum,fullrecord,realrecord,asciirecord,
     2 nfull,nreal,nascii,openmode,scversion)
c
c	version - 1/21/95
c	a general routine to open starcat catalog in ibis2 format
c	for 'READ', 'WRITE, or 'UPDATE'
c
c The starcat catalog is an IBIS-2 tabular file which has the following format.
c	for type 1
c
c	starcat catalog type is declared in variable scversion
c
c	type 1 has 20 columns
c	***************************************************************
c	descriptor	description		format		units
c	entry		catalog entry number	full(1)		<none>		
c	DN-bkg		sum of DNs - background	real(1)		counts
c	tot pix		total pixel count 	full(2)		<none>
c	sl		starting line		full(3)		<none>
c	ss		starting sample		full(4)		<none>
c	nl		number of lines		full(5)		<none>
c	ns		number of samples	full(6)		<none>
c	centsamp	center sample		real(2)		<none>
c	centline	center line		real(3)		<none>
c	maj axis	major axis length	real(4)		<none>
c	min axis	minor axis length	real(5)		<none>
c	ratio		ratio minor/major axis	real(6)		<none>
c	rotangle	rotation angle of ellip	real(7)		degrees
c	catalog		source catalog		ascii(1)-'A16'	<none>
c	ID		catalog ID		ascii(2)-'A20'	<none>
c	RA		right ascension		real(8)		hours
c	dec		declination		real(9)		degrees
c	mag calc	calculated magnitude	real(10)	<none>
c	mag ref		catalog magnitude	real(11)	<none>
c	class		classification		ascii(3)-'A8'	<none>
c	***************************************************************
c
c	type 2 adds 1 more column:
c	sat pix	# saturated pixels		full(7)   <none>
c	***************************************************************
c
c	type 3 adds 4 more columns:
c	***************************************************************
c	descriptor 	description		format		units
c	inten		intensity		full(8)		<none>		
c	eccent		eccentricity		real(12)	<none>
c	stddev		standard deviation	real(13)	<none>
c	amplitud	amplitude		real(14)	<none>	
c	***************************************************************
c
	implicit none
c
	integer*4 unit,ibis,columns,rows,stat,count,abendcode
	integer*4 fullrecord,realrecord,asciirecord,ibis_file_get
	integer*4 ibis_group_new,scversion,i,j,k,l
	integer*4 ibis_column_find,collist(50),num,scunum,sctnum
	integer*4 nfull,nreal,nascii,iscptr
	character*5 ibfmt(columns)
	character*8 ttype(columns),openmode,sctypes(50)
	character*16 iborg,tunits(columns),scunits(50)
	character*16 cattype,catfile,tempunit
	character*80 outline
c
	data catfile/'STARCAT TYPE   1'/
c
	abendcode=0
	iborg='ROW'
c
	if (openmode(1:4).eq.'READ'.or.openmode(1:6).eq.'UPDATE') then
		call ibis_file_open (unit,ibis,openmode,columns,rows,ibfmt,
     1	 iborg,stat)
		if (stat.ne.1) call ibis_signal_u(unit,stat,1)
		count=ibis_file_get(ibis,'nr',rows,1,1)
		if (count.lt.0) call ibis_signal(ibis,count,abendcode)
		count=ibis_file_get(ibis,'nc',columns,1,1)
		if (count.lt.0) call ibis_signal(ibis,count,abendcode)
		count=ibis_file_get(ibis,'type',cattype,1,1)
		if (count.lt.0) call ibis_signal(ibis,count,abendcode)
		if (catfile(1:7).ne.cattype(1:7)) then
			write (outline,10100) cattype
10100 format ('Catalog of type ',a16,' not starcat catalog')
			call xvmessage (outline,' ')
			call abend
		endif
		read (cattype(14:16),10110) scversion
10110 format (i3)
		count=ibis_file_get(ibis,'formats',ibfmt,1,columns)
		if (count.lt.0) call ibis_signal(ibis,count,abendcode)
		scunum=ibis_file_get(ibis,'units',scunits,1,columns)
		if (scunum.lt.0) call ibis_signal(ibis,scunum,abendcode)
		do i=1,scunum
			num=ibis_column_find(ibis,'unit',scunits(i),
     1		collist,1,50)
			if (num.lt.0) call ibis_signal(ibis,num,abendcode)
			do j=1,num
				tunits(collist(j))=scunits(i)
			enddo
		enddo
		count=ibis_file_get(ibis,'groups',ttype,1,columns)
		if (count.lt.0) call ibis_signal(ibis,count,abendcode)
		count=ibis_file_get(ibis,'org',iborg,1,1)
		if (count.lt.0) call ibis_signal(ibis,count,abendcode)
		nfull=0
		nreal=0
		nascii=0
		nfull=ibis_column_find(ibis,'format','FULL',0,0,0)
		if (nfull.lt.0) call Ibis_signal (ibis,stat,abendcode)
		call ibis_record_open(ibis,fullrecord,'format:full',0,0,
     1	 'full',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
		nreal=ibis_column_find(ibis,'format','REAL',0,0,0)
		if (nreal.lt.0) call Ibis_signal (ibis,stat,abendcode)
		call ibis_record_open(ibis,realrecord,'format:real',0,0,
     1	 'real',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
		nascii=ibis_column_find(ibis,'format','ASCII',0,0,0)
		if (nascii.lt.0) call Ibis_signal (ibis,stat,abendcode)
		call ibis_record_open(ibis,asciirecord,'format:ascii',0,0,
     1	 'a20',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
	endif
c
	if (openmode(1:5).eq.'WRITE') then
		call ibis_file_open(unit,ibis,'WRITE',columns,rows,
     1	ibfmt,iborg,stat)
		if (stat.ne.1) call ibis_signal_u (unit,stat,1)
		if (scversion.ne.1) then
			write(outline,10110) scversion
			catfile(14:16)=outline(1:3)
		endif
		call ibis_file_set(ibis,'type',catfile,stat)
		if (stat.lt.0) call ibis_signal(ibis,stat,abendcode)
		nfull=0
		nreal=0
		nascii=0
		nfull=ibis_column_find(ibis,'format','FULL',0,0,0)
		if (nfull.lt.0) call Ibis_signal (ibis,stat,abendcode)
		call ibis_record_open(ibis,fullrecord,'format:full',0,0,
     1	 'full',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
		nreal=ibis_column_find(ibis,'format','REAL',0,0,0)
		if (nreal.lt.0) call Ibis_signal (ibis,stat,abendcode)
		call ibis_record_open(ibis,realrecord,'format:real',0,0,
     1	 'real',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
		nascii=ibis_column_find(ibis,'format','ASCII',0,0,0)
		if (nascii.lt.0) call Ibis_signal (ibis,stat,abendcode)
		call ibis_record_open(ibis,asciirecord,'format:ascii',0,0,
     1	 'a20',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abendcode)
c
c  check if inherit properties of existing catalog
c
		sctnum=ibis_file_get(ibis,'groups',sctypes,1,columns)
		if (sctnum.lt.0) call ibis_signal(ibis,sctnum,abendcode)
		if (sctnum.eq.0) then		!only do if no inherit
c**			write (outline,10150) sctnum
c**10150 format ('Subroutine SCOPEN - Number of starcat types = ',i3)
c**			call xvmessage (outline,' ')
c**			call abend
			do i=1,columns
			    count=ibis_group_new(ibis,'group',ttype(i),i,
     1			1,' ')
			    if (count.lt.0) call ibis_signal(ibis,count,
     1			abendcode)
			enddo
		endif
		scunum=ibis_file_get(ibis,'units',scunits,1,columns)
		if (scunum.lt.0) call ibis_signal(ibis,scunum,abendcode)
c
c   The following test is put in here to set if new catalog inherits
c   properties of an existing catalog, i.e., INP=<old catalog>,
c   OUT=<new catalog> if xvselpi is not done.
c
		if (scunum.gt.0) then
			do i=1,scunum
				num=ibis_column_find(ibis,'unit',scunits(i),
     1			collist,1,50)
				if (num.lt.0) call ibis_signal(ibis,num,
     1			abendcode)
				do j=1,num
					tunits(collist(j))=scunits(i)
				enddo
			enddo
		elseif (scunum.eq.0) then
		    	tempunit=tunits(1)
			i=1
			iscptr=1
50	continue
			scunits(iscptr)=tempunit
			collist(1)=i
			num=1
			do k=i+1,columns
			    if (scunits(iscptr).eq.tunits(k)) then 
				num=num+1
				collist(num)=k
			    endif
			enddo	
			count=ibis_group_new(ibis,'unit',scunits(iscptr),
     1		collist,num,' ') 
			if (count.lt.0) call ibis_signal(ibis,count,abendcode)
			do l=1,columns
			    collist(l)=0
			enddo
70	continue
			i=i+1
			if (i.gt.columns) go to 80
			do j=1,iscptr
			    if (tunits(i).eq.scunits(j)) go to 70
			    do k=j,iscptr
				if (tunits(i).eq.scunits(k)) go to 70
			    enddo
			enddo
			tempunit=tunits(i)
			iscptr=iscptr+1
			go to 50
		endif
80	continue
	endif
c
	return
	end
c**************************************************************************
	block data
c
c	catalog in ibis-2 format
c	type 1 has 20 columns - type 2 has 21
c
	common /catalog/ttype,ibfmt,tunits
c
	character*5 ibfmt(21)		!column format
	character*8 ttype(21)		!column group
	character*16 tunits(21)		!column units
c
	data ttype(1) /'Entry   '/
	data ttype(2) /'DN-bkg  '/
	data ttype(3) /'tot pix '/
	data ttype(4) /'sl      '/
	data ttype(5) /'ss      '/
	data ttype(6) /'nl      '/
	data ttype(7) /'ns      '/
	data ttype(8) /'centsamp'/
	data ttype(9) /'centline'/
	data ttype(10)/'maj axis'/
	data ttype(11)/'min axis'/
	data ttype(12)/'ratio   '/
	data ttype(13)/'rotangle'/
	data ttype(14)/'Catalog '/
	data ttype(15)/'ID      '/
	data ttype(16)/'RA      '/
	data ttype(17)/'dec     '/
	data ttype(18)/'mag calc'/
	data ttype(19)/'mag ref '/
	data ttype(20)/'class   '/
	data ttype(21)/'sat pix '/
c
	data ibfmt(1) /'FULL'/			!entry
	data ibfmt(2) /'REAL'/			!sum (DN-bkg)
	data ibfmt(3) /'FULL'/			!sum pixels
	data ibfmt(4) /'FULL'/			!sl
	data ibfmt(5) /'FULL'/			!ss
	data ibfmt(6) /'FULL'/			!nl
	data ibfmt(7) /'FULL'/			!ns
	data ibfmt(8) /'REAL'/			!centroid sample
	data ibfmt(9) /'REAL'/			!centroid line
	data ibfmt(10)/'REAL'/			!major axis length
	data ibfmt(11)/'REAL'/			!minor axis length
	data ibfmt(12)/'REAL'/			!minor/major axis ratio
	data ibfmt(13)/'REAL'/			!rotation angle
	data ibfmt(14)/'A16 '/			!catalog
	data ibfmt(15)/'A20 '/			!catalog identifier
	data ibfmt(16)/'REAL'/			!right ascension
	data ibfmt(17)/'REAL'/			!declination
	data ibfmt(18)/'REAL'/			!calculated magnitude
	data ibfmt(19)/'REAL'/			!reference magnitude
	data ibfmt(20)/'A8  '/			!class (star,galaxy,asteroid,
						!planet,moon,stdstar,comet,tstdstar)
	data ibfmt(21)/'FULL'/			!# of saturated pixels
c
	data tunits(1) /'<none>          '/
	data tunits(2) /'counts          '/
	data tunits(3) /'<none>          '/
	data tunits(4) /'<none>          '/
	data tunits(5) /'<none>          '/
	data tunits(6) /'<none>          '/
	data tunits(7) /'<none>          '/
	data tunits(8) /'<none>          '/
	data tunits(9) /'<none>          '/
	data tunits(10)/'<none>          '/
	data tunits(11)/'<none>          '/
	data tunits(12)/'<none>          '/
	data tunits(13)/'degrees         '/
	data tunits(14)/'<none>          '/
	data tunits(15)/'<none>          '/
	data tunits(16)/'hours           '/
	data tunits(17)/'degrees         '/
	data tunits(18)/'<none>          '/
	data tunits(19)/'<none>          '/
	data tunits(20)/'<none>          '/
	data tunits(21)/'<none>          '/
c
	end
