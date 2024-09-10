$!****************************************************************************
$!
$! Build proc for MIPL module fitsin
$! VPACK Version 1.9, Friday, October 08, 2004, 11:51:19
$!
$! Execute by entering:		$ @fitsin
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
$ write sys$output "*** module fitsin ***"
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
$ write sys$output "Invalid argument given to fitsin.com file -- ", primary
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
$   if F$SEARCH("fitsin.imake") .nes. ""
$   then
$      vimake fitsin
$      purge fitsin.bld
$   else
$      if F$SEARCH("fitsin.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake fitsin
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @fitsin.bld "STD"
$   else
$      @fitsin.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create fitsin.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack fitsin.com -mixed -
	-s fitsin.f -
	-i fitsin.imake -
	-p fitsin.pdf -
	-t tstfitsin.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create fitsin.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	include 'VICMAIN_FOR'
	SUBROUTINE MAIN44
c
c	program to transfer FITS files to vicar files
c
	implicit none
c
	common /fitskeys/ keyword
c
	character*8 keyword(35)
c
	integer*4 stat,tabbits,tabax,tabcols,tabrows,tabdepth
	integer*4 pcount,gcount,tfields,extver,extlevel,catrec
	integer*4 ouni1(10),ouni2(10),outunit,excnt,ibis,nods,nhdrs
	integer*4 iuni,npix,nlin,nband,btpix,bfctr,n,hdrunit
	integer*4 fullrecord,realrecord,asciirecord,doubrecord
	integer*4 tbcol(999),fitsrec,tabrec,fitstart
	logical*4 iexcise,xtens,iscl,izero,tlist,table,xtend
	logical*4 xfer,prfits,ihst,icmt,inul,idat,ipass,jpass
	logical*4 imgxfer,tbxfer,btxfer,ineof,u16
	real*4 bzero,bscale
	character*4 ibfmt(999)
	character*8 passtart,unlikely,excludst(10),ttype(999),passover
	character*10 tform(999)
	character*16 extname
	character*35 version
	character*80 outline
	character*132 hdrfilename(10)
c
	data version/'*** FITSIN Version 12-May-2003 '/
	data excludst/10*'        '/
	data unlikely/'}#%@^!~('/	!an unlikely combination of characters
c					!in a FITS header
	call xvmessage(version,' ')
        passover=unlikely
	stat=0				!return status for xv routines
c					!process FITS parameters

	call parmproc (iuni,ouni1,ouni2,bfctr,excnt,xfer,prfits,
	1 ihst,icmt,inul,idat,ipass,jpass,iexcise,passtart,excludst,
	2 unlikely,tlist,nods,nhdrs,hdrfilename,u16,passover)
c
	fitsrec=0
	fitstart=1
	if (nods.gt.0) then
	   do n=1,nods
		outunit=ouni1(n)
		CALL XVUNIT(outunit,'OUT',n,STAT,' ')
		call chkstat (stat,'XVunit err on output file',1,0,0)
		if (stat.eq.1) xfer=.true.	!got output #1!
c					!process FITS header
		if (nhdrs.gt.0) then
	   	   hdrunit=ouni2(n)
	   	   CALL XVUNIT(hdrunit,'HEADER',n,stat,'U_NAME',
	1	   hdrfilename(n),' ')
		   call chkstat(stat,'XVunit err HDR file ',1,0,0)
		   if (stat.eq.1) prfits=.true.	!output FITS header file
		endif
		call hdrproc (bfctr,hdrunit,excnt,catrec,tabrec,nods,
	1	fitsrec,fitstart,iuni,npix,nlin,nband,btpix,bzero,bscale,
	2	xfer,prfits,ihst,icmt,inul,iscl,izero,ipass,jpass,iexcise,
	3	xtens,excludst,passtart,imgxfer,tbxfer,btxfer,table,ineof,
	4	tabbits,tabax,tabcols,tabrows,tabdepth,pcount,gcount,
	5	tfields,extname,extver,extlevel,ttype,tbcol,tform,ibfmt)

		if (ineof) then
			write (outline,10100) n-1,nods
10100 format ('**FITSIN - Only ',i3,' data sets found out of ',i3,
	1 	' requested')
			call xvmessage (outline,' ')
			call xvclose (iuni,stat,' ')
			return
		endif
	   if (imgxfer) then
		call imgopen (bfctr,iuni,outunit,npix,nlin,nband,idat,
	1	izero,iscl,inul,xtens,btpix,bzero,bscale,fitsrec,
	2	fitstart,passtart,iexcise,excnt,excludst,u16,icmt,ipass,jpass)

 		if (btpix.eq.-64) call dfitsi (iuni,outunit,npix,nlin,
	1	nband,bfctr,bzero,bscale,fitsrec,idat)	!IEEE double
		if (btpix.eq.-32) call rfitsi (iuni,outunit,npix,nlin,
	1	nband,bfctr,bzero,bscale,fitsrec,idat)	!IEEE real
		if (btpix.eq.32) call ifitsi (iuni,outunit,npix,nlin,
	1	nband,bfctr,bzero,bscale,fitsrec,idat)	!input fullword
		if (btpix.eq.16) then
			if (u16) then
				call ufitsi (iuni,outunit,npix,nlin,
	1			nband,bfctr,fitsrec)
			else			!input/output halfword
				call hfitsi (iuni,outunit,npix,nlin,
	1			nband,bfctr,bzero,bscale,fitsrec,idat)
			endif
		endif
		if (btpix.eq.8) call bfitsi (iuni,outunit,npix,nlin,
	1	nband,bfctr,bzero,bscale,fitsrec,idat)	!input byte

		CALL XVCLOSE(outunit,STAT,' ')
		fitstart=fitsrec+1
	   endif
c
	   if (tbxfer) then
	     call tablopen  (bfctr,iuni,outunit,tabcols,tabrows,tabdepth,
	1	 pcount,gcount,tfields,extname,extver,extlevel,ttype,
	2	 tbcol,tform,tabbits,catrec,fitsrec,ibis,ibfmt,passtart,
	3	 excludst,fullrecord,realrecord,asciirecord,doubrecord,
	4	 xtend,icmt)
	     call fitsibis  (iuni,outunit,tabcols,tabrows,tabdepth,tfields,
	1	bfctr,tabrec,ibfmt,tform,ttype,tbcol,fullrecord,realrecord,
	2	asciirecord,doubrecord,ibis)
		call ibis_file_close(ibis,' ',stat)
		if (stat.ne.1) call ibis_signal_u(ouni1,stat,1)
		fitstart=fitsrec+1
	   endif
c
	   if (table.and.tlist) then
		call tablelist (ttype,tbcol,tform,tfields,tabrows,
	1	 extname,extver,iuni,bfctr,tabcols,tabrec)
	   endif
c
	   if (btxfer) then
		write (outline,10120) 
10120 format (' FITS BINARY TABLES to IBIS2 files not yet supported')
		call xvmessage (outline,' ') 
c		call bintablopen     		!future expansion of function
c		call bintablproc		!future expansion of function
	   endif
	   enddo
	else
		fitsrec=0
		fitstart=1
		outunit=0
		if (nhdrs.gt.0) then
	   	   hdrunit=ouni2(1)
	   	   CALL XVUNIT(hdrunit,'HEADER',1,stat,'U_NAME',
	1	   hdrfilename(1),' ')
		   call chkstat(stat,'XVunit err HDR file ',1,0,0)
		   if (stat.eq.1) prfits=.true.	!output FITS header file
		endif
c					!process FITS header
		call hdrproc (bfctr,hdrunit,excnt,catrec,tabrec,nods,
	1	fitsrec,fitstart,iuni,npix,nlin,nband,btpix,bzero,bscale,
	2	xfer,prfits,ihst,icmt,inul,iscl,izero,ipass,jpass,iexcise,
	3	xtens,excludst,passtart,imgxfer,tbxfer,btxfer,table,ineof,
	4	tabbits,tabax,tabcols,tabrows,tabdepth,pcount,gcount,
	5	tfields,extname,extver,extlevel,ttype,tbcol,tform,ibfmt)
c
	   if (imgxfer) then
		call imgopen (bfctr,iuni,outunit,npix,nlin,nband,idat,
	1	izero,iscl,inul,xtens,btpix,bzero,bscale,fitsrec,
	2	fitstart,passtart,iexcise,excnt,excludst,u16,icmt,ipass,jpass)
		fitsrec=fitsrec+1

		if (btpix.eq.-64) call dfitsi (iuni,outunit,npix,nlin,
	1	nband,bfctr,bzero,bscale,fitsrec,idat)	!IEEE double
		if (btpix.eq.-32) call rfitsi (iuni,outunit,npix,nlin,
	1	nband,bfctr,bzero,bscale,fitsrec,idat)	!input IEEE real format
		if (btpix.eq.32) call ifitsi (iuni,outunit,npix,nlin,
	1	nband,bfctr,bzero,bscale,fitsrec,idat)	!input fullword
		if (btpix.eq.16) then
			if (u16) then		!output unsigned
				call ufitsi (iuni,outunit,npix,nlin,
	1			nband,bfctr,fitsrec)
			else			!input/output halfword
				call hfitsi (iuni,outunit,npix,nlin,
	1			nband,bfctr,bzero,bscale,fitsrec,idat)
			endif
		endif
		if (btpix.eq.8) call bfitsi (iuni,outunit,npix,nlin,
	1	nband,bfctr,bzero,bscale,fitsrec,idat)	!input byte
	   endif
c
	   if (tbxfer) then
	     call tablopen  (bfctr,iuni,outunit,tabcols,tabrows,tabdepth,
	1	 pcount,gcount,tfields,extname,extver,extlevel,ttype,
	2	 tbcol,tform,tabbits,catrec,fitsrec,ibis,ibfmt,passtart,
	3	 excludst,fullrecord,realrecord,asciirecord,doubrecord,
	4	 xtend,icmt)
	     call fitsibis  (iuni,outunit,tabcols,tabrows,tabdepth,tfields,
	1	bfctr,tabrec,ibfmt,tform,ttype,tbcol,fullrecord,realrecord,
	2	asciirecord,doubrecord,ibis)
	   endif
c
	   if (table.and.tlist) then
		call tablelist (ttype,tbcol,tform,tfields,tabrows,
	1	 extname,extver,iuni,bfctr,tabcols,tabrec)
	   endif
c
	   if (btxfer) then
		write (outline,10120) 
		call xvmessage (outline,' ') 
c		call bintablopen	!future expansion of function
c		call bintablproc	!future expansion of function
	   endif
c	close all files and exit
c
	endif
	CALL XVCLOSE(IUNI,STAT,' ')
	return
	end
C=========================================================================
	SUBROUTINE PARMPROC (iuni,ouni1,ouni2,bfctr,excnt,xfer,prfits,
	1 ihst,icmt,inul,idat,ipass,jpass,iexcise,passtart,excludst,
	2 unlikely,tlist,nods,nhdrs,filename,u16,passover)
C
C	ROUTINE TO PROCESS fitsin PARAMETERS
C
	implicit none
c
	common /fitskeys/ keyword
c
	integer*4 ouni1(10),ouni2(10),iuni,excnt,bfctr
	character*8 keyword(35),passtart,unlikely,excludst(10),passover
        character*10 exclud(10)
	integer*4 stat,nods,cnt,kk,lookcnt,nhdrs,fitsns,def
	logical*4 xfer,prfits,ihst,icmt,inul,idat,ipass,jpass,iexcise
	logical*4 tlist,u16
	character*8 curr_host,intfmt,realfmt
	character*9 hist,comt,nul,dtype,blktype,tablist
	character*80 outline
	character*132 filename(10)
c
C	data exclud/120*' '/
	data curr_host/'HOSTNAME'/
c
	xfer=.false.			!Output not to be done
	idat=.false.			!Default is "FITS" data type out
	ipass=.false.
	iexcise=.false.
	call xvpcnt('OUT',nods)		!get number of output data sets
	prfits=.false.			!no FITS header output file
	call xvpcnt('HEADER',nhdrs)	!get number of HEADER data sets
	call xvp ('HEADER',filename,nhdrs)

	bfctr=1				!blocking factor = 1

        CALL XVP('BLOCK',BLKTYPE,CNT)
	if (blktype.eq.'BLOCK') then
		call xvp('FACTOR',bfctr,cnt)
		if (bfctr.gt.10) then
			write (outline,10100) bfctr
10100	format (' **fitsin - Blocking factor = ',i4,' - greater than 10')
			call xvmessage(outline,' ')
			call abend
		endif
	endif

        CALL XVPARM('PASS',passover,lookcnt,def,1) !BEGIN EXTRACTING LABEL WORD
	jpass=.true.
	if(passover.eq.' ') jpass=.false. !indicate that PASS= was not given
	if(passover.eq.unlikely) jpass=.false.

        if (lookcnt.ne.0) then
          call mvlc(passover,passtart,8)
          call uprcase(passtart)
        endif
c
c	need to read in label item in 'PASS' and replace each null
c	with a space since user probably will not fill 'PASS' parameter
c	with blanks if keyword less than 8 characters
c
	if (jpass) then
		write (outline,10110) passtart
10110 format ('passtart = ',a8)
		call xvmessage (outline,' ')
	endif
c
c note: exclud is passed over as a byte string since excluded items are all
c  passed over as lower case
c

	CALL XVPARM('EXCISE',exclud,excnt,def,10)	!WORDS TO DROP FROM LABELS
        do kk=1,excnt
            call mvlc(exclud(kk),excludst(kk),10)
            call uprcase(excludst(kk))
        enddo

	if (excnt.ge.1) iexcise=.true.
        
	if (.not.iexcise) go to 5
c
5	continue
C	IHST=.TRUE. FOR "HISTORY", .FALSE. FOR "NOHISTORY"
	ihst=.true.
	CALL XVP('HISTORY',HIST,CNT)
	if (hist.eq.'NOHISTORY') ihst=.false.
C	ICMT=.TRUE. FOR "COMMENT", .FALSE. FOR "NOCOMMENT"
	icmt=.true.
	CALL XVP('COMMENT',COMT,CNT)
	if (comt.eq.'NOCOMMENT') icmt=.false.
c	
	inul=.true.			!pass null lines
	CALL XVP('NULL',NUL,CNT)
	IF (NUL.EQ.'NONULL') INUL=.FALSE.
C	IDAT=.TRUE. FOR "TRUE" DATA, .FALSE. FOR "FITS" TAPE DATA
	idat=.false.
	u16=.false.
	CALL XVP('DATA',DTYPE,CNT)
	IF (DTYPE.EQ.'TRUE') IDAT=.TRUE.
	if (dtype.eq.'U16') u16=.true.
c	tlist=.true. for list tabular file
	tlist=.false.
	call xvp ('TABLIST',tablist,cnt)
	if (tablist.eq.'LIST') tlist=.true.
c
	fitsns=2880*bfctr
C
C	OPEN FITS (INPUT) DATA SET
C
	CALL XVUNIT(IUNI,'INP',1,STAT,' ')
	CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','I_FORMAT',
	1 'BYTE','OP','READ','COND','NOLABELS,NOBLOCKS','U_FORMAT','BYTE',
	2 'U_NS',fitsns,'CONVERT','OFF',' ')
	call chkstat (stat,'XVopen error on FITS file',1,0,0)
	call xvhost (curr_host,intfmt,realfmt,stat)
	call chkstat (stat,'XVhost error',1,0,0)
c**	write (outline,10050) intfmt
c**10050 format (' XVhost Integer format = ',a8)
c**	call xvmessage (outline,' ')

	return
	end
C=========================================================================
	subroutine hdrproc (bfctr,ouni2,excnt,catrec,tabrec,nods,
	1 fitsrec,fitstart,iuni,npix,nlin,nband,btpix,bzero,bscale,
	2 xfer,prfits,ihst,icmt,inul,iscl,izero,ipass,jpass,iexcise,
	3 xtens,excludst,passtart,imgxfer,tbxfer,btxfer,table,ineof,
	4 tabbits,tabax,tabcols,tabrows,tabdepth,pcount,gcount,
	5 tfields,extname,extver,extlevel,ttype,tbcol,tform,ibfmt)
c
c	routine to process FITS headers into VICAR labels
c
	implicit none
c
	include 'errdefs'
c
	common /fitskeys/ keyword
c
	byte data(28800)
	integer*4 ouni2,excnt,totlab,catrec,tabrec,nods
	integer*4 iuni,npix,nlin,nband,btpix,bfctr
	integer*4 fitsns,fitsrec,stat,nax,fitstart
	integer*4 tabbits,tabax,tabcols,tabrows,tabdepth,pcount
	integer*4 gcount,tfields,extver,extlevel,tbcol(999)
	logical*4 xfer,prfits,ihst,icmt,inul,iscl,izero,ipass,jpass
	logical*4 xend,imgxfer,tbxfer,btxfer,valid
	logical*4 iexcise,iend,xtens,table,oneblock,xtend,group
	logical*4 hierarch,bintable,ineof
	real*4 bzero,bscale
	character*4 ibfmt(999)
	character*8 ttype(999)
	character*8 keyword(35),passtart,excludst(10)
	character*8 xtype
	character*10 tform(999)
	character*16 extname
	character*80 outline
c
	data xtype/'        '/
c
C**************************
C     EXTRACT EACH HEADER RECORD INTO ARRAY "DATA"(28800) AND EXAMINE.
C	Header is 2880 bytes (36 80-byte card images) times the tape
C	blocking factor (bfctr)
C**************************
	fitsns=2880*bfctr
	fitsrec=fitstart
 	CALL XVREAD(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',fitsns,' ')
C					!CHECK FOR VALID HEADER KEYWORD
	if (stat.eq.END_OF_FILE) then
		ineof=.true.
		return
	endif
	call chkstat (stat,'XVRead error - hdrproc',1,0,0)
	iend=.false.
	xtens=.false.
	table=.false.
	oneblock=.false.
	hierarch=.false.
	group=.false.
	bintable=.false.
	valid=.false.

	CALL VALHDR  (data,xfer,iend,oneblock,xtens,table,xtype,
	1 bintable,imgxfer,tbxfer,btxfer,valid)
	if (.not.valid) then
		write (outline,10060)
10060 format ('**ERROR - INVALID HEADER KEYWORD')
		call xvmessage (outline,' ')
		write (outline,10065)
10065 format ('ONE HEADER BLOCK LISTED - BUT NO PROCESSING')
		call xvmessage (outline,' ')
		xfer=.false.
		iend=.true.
		oneblock=.true.		!list only 1st block of header
	endif
C
C	GET OUTPUT DATA SET CHARACTERISTICS
C
	if (xfer) then
		imgxfer=.true.
		tbxfer=.true.
		btxfer=.true.
	endif
	bzero=0.0
	bscale=1.0
	totlab=1
11	continue
c					!compare FITS with desired output 
	call fitschk (data,bfctr,xfer,iscl,izero,xtend,group,totlab,
	1 hierarch,iend,bscale,bzero,btpix,nlin,npix,nband,nax,
	2 imgxfer,tbxfer,btxfer)
	if (oneblock) then
		call fitslist (data,bfctr,iend)
		xfer=.false.
		imgxfer=.false.
		tbxfer=.false.
		btxfer=.false.
		go to 150
	endif					!c***
	if (.not.xfer) call fitslist (data,bfctr,iend)
	if (iend) go to 20
	fitsrec=fitsrec+1
	CALL XVREAD(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',fitsns,' ')
	call chkstat (stat,'XVRead error - hdrproc',1,0,0)
	go to 11
c
20	continue
	if (.not.xtend) then
		tbxfer=.false.
		btxfer=.false.
	endif
c						!see if XTENSIONS on end
	if (xtend) then
		fitsrec=fitsrec+1
		catrec=fitsrec
 		CALL XVREAD(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',
	1	fitsns,' ')
		call chkstat (stat,'XVRead error - hdrproc',1,0,0)
		valid=.false.
C					!CHECK FOR VALID HEADER KEYWORD
		CALL VALHDR  (data,xfer,iend,oneblock,xtens,table,xtype,
	1 	bintable,imgxfer,tbxfer,btxfer,valid) 
	endif
22	continue
	if (xtens) then
		xend=.false.
25		continue
c
		if (table) then
			call tablchk (data,bfctr,totlab,xend,tabbits,
	1		tabax,tabcols,tabrows,tabdepth,pcount,gcount,
	2		tfields,extname,extver,extlevel,ttype,tbcol,
	3		tform,ibfmt)
		endif
		if (.not.xfer) call fitslist (data,bfctr,iend)
		if (xend) go to 31
		fitsrec=fitsrec+1
		call xvread(iuni,data,stat,'LINE',fitsrec,'NSAMPS',
	1	fitsns,' ')
		call chkstat (stat,'XVRead error - hdrproc',1,0,0)
		if (stat.eq.END_OF_FILE) go to 140
		call chkstat (stat,'XVread err in XTENSION checks',1,0,0)
		go to 25
c
c **** what about multiple extension sets in same file??
c	may have to have another test for "EXTENSION ="??
c
	endif
c
c *** if writing header file do the following
c
31	continue
	tabrec=fitsrec
	if (xfer.and..not.table) tbxfer=.false.
	if (xfer.and..not.bintable) btxfer=.false.
	if (prfits) then
c
		call xvopen(ouni2,stat,'OPEN_ACT','SA','IO_ACT','SA',
	1	'U_FORMAT','BYTE','O_FORMAT','BYTE','OP','WRITE','COND',
	2	'NOLABELS','U_ORG','BSQ','U_NL',totlab,'U_NS',81,
	3	'U_NB',1,' ')
		call chkstat(stat,'xvopen err HDR File ',1,0,0)
		fitsrec=fitstart
 		CALL XVREAD(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',
	1	fitsns,' ')
		call chkstat (stat,'XVRead error - hdrproc',1,0,0)
35		continue
		iend=.false.
c					!will write to output file
		call fitsprt (data,bfctr,ouni2,iend,totlab)
		if (iend) go to 38
		if (oneblock) go to 40
		fitsrec=fitsrec+1
		CALL XVREAD(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',
	1	fitsns,' ')
		call chkstat (stat,'XVRead error - hdrproc',1,0,0)
		GO TO 35
38		continue
		if (xtens) then
			xend=.false.
			call fitsprt  (data,bfctr,ouni2,xend,totlab)
			if (xend) go to 40
			fitsrec=fitsrec+1
			CALL XVREAD(IUNI,DATA,STAT,'LINE',fitsrec,
	1		'NSAMPS',fitsns,' ')
			call chkstat (stat,'XVRead error - hdrproc',1,0,0)
		endif
		call xvclose (ouni2,stat,' ')
	endif
40	CONTINUE
	go to 150
c	
140	continue
	call abend
c
150	continue
	return
	end
C=========================================================================
	subroutine imgopen (bfctr,iuni,ouni1,npix,nlin,nband,idat,izero,
	1 iscl,inul,xtens,btpix,bzero,bscale,fitsrec,fitstart,
	2 passtart,iexcise,excnt,excludst,u16,icmt,ipass,jpass)
c
	implicit none
c
C	OPEN VICAR (OUTPUT) IMAGE DATA SET
c
	byte data(28800),lab(80,144)
	integer*4 btpix,nlin,npix,nband,nlab,totlab,trlab,fitsrec
	integer*4 i,iuni,ouni1,stat,fitsns,bfctr,excnt,ilab,fitstart
	logical*4 idat,iscl,izero,iend,ipass,jpass,xtend,icmt
	logical*4 ihst,inul,iexcise,xtens,xend,xfer,u16
	real*4 bscale,bzero
	character*4 xmat
	character*5 key
	character*8 passtart,excludst(10)
	character*80 outline,flabel
C 
	data KEY/'VF000'/
c
	fitsns=2880*bfctr
	xmat='BYTE'
	if (BTPIX.EQ.16) xmat='HALF'
	if (btpix.eq.16.and.u16) xmat='FULL'
	if (BTPIX.EQ.32) xmat='FULL'
	if (btpix.eq.-32) xmat='REAL'
	if (btpix.eq.-64) xmat='DOUB'
	if (.not.idat) go to 80		!BRANCH IF FITS
	xmat='REAL'	
80	continue
	CALL XVOPEN(OUNI1,STAT,'OPEN_ACT','SA','IO_ACT','SA',
	1 'U_FORMAT',xmat,'O_FORMAT',xmat,'OP','WRITE','U_NL',
	2 nlin,'U_NS',npix,'U_NB',nband,'U_ORG','BSQ',' ')
	call chkstat (stat,'XVopen error on VICAR file',1,0,0)
	if (.not.idat) go to 90		!BRANCH IF FITS DATA
C MAY HAVE TO SOME DAY TREAT CASE WHERE "XMAT" HAS TO BE COMPLEX OR DOUBLE
	IF (ISCL.AND.IZERO) GO TO 90	!BRANCH IF BSCALE AND BZERO FOUND
	if (.not.izero)	then
		write (outline,10100)
10100	format('**fitsin - KEYWORD BZERO = NOT FOUND FOR DATA=TRUE')
		call xvmessage(outline,' ')
	endif
	if (.NOT.ISCL) then
		write (outline,10200)
10200	format ('**fitsin - KEYWORD BSCALE = NOT FOUND FOR DATA=TRUE')
		call xvmessage (outline,' ')
	endif
	if (.NOT.ISCL .OR. .NOT.IZERO) then
		write (outline,10300)
10300	format ('**fitsin - FITS DATA TRANSFERRED AS VICAR REAL DATA')
		call xvmessage (outline,' ')
	endif
90	continue
	nlab=1			!INITIALIZE OUTPUT LABEL PTR
	totlab=0		!INITIALIZE TOTAL FITS KEYWORDS
	trlab=0			!initialize FITS keywords to be transferred

C
C	REINITIALIZE POINTER TO START OF FITS FILE
C
	iend=.false.
	fitsrec=fitstart
 	CALL XVREAD(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',fitsns,' ')
	call chkstat (stat,'XVRead error - imgopen',1,0,0)
100	continue
	if (.not.jpass) ipass=.true.
c				!EXAMINE/LIST REMAINING CARD IMAGES
	CALL FITSKEY (data,bfctr,xfer,npix,nlin,nband,jpass,
	1 ipass,bscale,bzero,iscl,izero,xtend,iend,icmt,ihst,
	2 inul,iexcise,excnt,xtens,nlab,trlab,totlab,
	3 excludst,passtart,lab)
C
C	WRITE OUT FITS LABELS TO VICAR FILE
C
	do i=1,nlab
		CALL KEYINC2(KEY)	!Increment "VFxxx" VICAR Label prefix
 		ilab=trlab-nlab+i
		call mvlc (lab(1,i),flabel,80,' ')
		CALL XLADD (ouni1,'HISTORY',key,flabel,STAT,'FORMAT',
	1	'STRING','ULEN',80,'ERR_ACT','S',' ')
		call chkstat(stat,'xladd err LABEL # = ',0,ilab,1)
	ENDDO
125	continue
	if (iend.and..not.xtens) go to 150	!Branch if done with headers
	if (xend) go to 150
C***************************
C     READ REST OF ANNOTATION RECORDS.
C***************************
	fitsrec=fitsrec+1
	CALL XVREAD(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',fitsrec,
	1 ' ')					!READ NEXT HEADER
	call chkstat (stat,'XVRead error - imgopen',1,0,0)
	nlab=1					!INITIALIZE OUTPUT LABEL PTR
	go to 100
c
150	continue
	return
	end
C=========================================================================
	subroutine tablopen (bfctr,iuni,ouni1,tabcols,tabrows,tabdepth,
	1 pcount,gcount,tfields,extname,extver,extlevel,ttype,tbcol,tform,
	2 tabbits,catrec,fitsrec,ibis,ibfmt,passtart,excludst,fullrecord,
	3 realrecord,asciirecord,doubrecord,xtend,icmt)
c
	implicit none
c
C	OPEN IBIS TABLE (OUTPUT) DATA SET
c
	byte data(28800),lab(80,144)
	integer*4 i,j,nlab,totlab,trlab,fitsrec,catrec
	integer*4 tabbits,tabcols,tabrows,tabdepth,pcount,gcount
	integer*4 tfields,extver,extlevel,ibis,count,abend
	integer*4 fullrecord,realrecord,asciirecord,doubrecord
	integer*4 nfull,nreal,nascii,ndoub
	integer*4 tbcol(999)
	integer*4 iuni,ouni1,stat,fitsns,bfctr,excnt,ilab
	integer*4 ibis_column_find,ibis_group_new
	logical*4 iend,ipass,jpass,xtend,icmt,idat
	logical*4 xfer,xtens,xend,iscl,izero,ihst,inul
	logical*4 iexcise
	real*4 bscale,bzero
	character*4 ibfmt(999)
	character*5 key
	character*8 passtart,excludst(10)
	character*8 ttype(999)
	character*10 tform(999)
	character*16 extname,iborg
	character*30 fitstabfile
	character*80 outline,flabel
C 
	data KEY/'VF000'/,iborg/'ROW'/
	data fitstabfile/'FITS TABLE CONVERSION'/
c
	fitsns=2880*bfctr
	idat=.false.			!Default "FITS"
	ipass=.true.
	jpass=.false.
	iexcise=.false.
	ihst=.true.			!pass HISTORY= lines
	icmt=.true.			!pass COMMENT= lines
	inul=.true.			!pass null lines (all blanks)
	bscale=1.0
	bzero=0.0
80	continue
	write (outline,10100) tfields,tabcols,tabrows
10100 format ('tfields = ',i5,' tabcols = ',i5,' tabrows = ',i5)
	call xvmessage (outline,' ')
	write (outline,10105) tfields
10105 format ('Column format: 1 - ',i4) 
	call xvmessage (outline,' ')
	write (outline,10110) (ibfmt(i), i=1,tfields)
10110 format (12(1x,a4))
	call xvmessage (outline,' ') 
c
	call ibis_file_open (ouni1,ibis,'WRITE',tfields,tabrows,ibfmt,
	1 iborg,stat)
	if (stat.ne.1) call ibis_signal_u (ouni1,stat,1)
c
	call ibis_file_set(ibis,'type',fitstabfile,stat)
	if (stat.lt.0) call ibis_signal(ibis,stat,abend)
	nfull=0
	nreal=0
	nascii=0
	ndoub=0
c
	nfull=ibis_column_find(ibis,'format','FULL',0,0,0)
	if (nfull.lt.0) call ibis_signal (ibis,nfull,abend)
	nreal=ibis_column_find(ibis,'format','REAL',0,0,0)
	if (nreal.lt.0) call ibis_signal (ibis,nreal,abend)
	nascii=ibis_column_find(ibis,'format','ASCII',0,0,0)
	if (nascii.lt.0) call ibis_signal (ibis,nascii,abend)
	ndoub=ibis_column_find(ibis,'format','DOUB',0,0,0)
	if (ndoub.lt.0) call ibis_signal (ibis,ndoub,abend)

	if (nfull.gt.0) then
		call ibis_record_open (ibis,fullrecord,'format:FULL',
	1	0,0,'full',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abend)
		call chkstat (stat,'IBIS_record_open error on FULLgroup',1,0,0)
	endif
	if (nreal.gt.0) then
		call ibis_record_open (ibis,realrecord,'format:REAL',
	1	0,0,'real',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abend)
	endif
	if (nascii.gt.0) then
		call ibis_record_open (ibis,asciirecord,'format:ASCII',
	1	0,0,'a40',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abend)
	endif
	if (ndoub.gt.0) then
		call ibis_record_open (ibis,doubrecord,'format:DOUB',
	1	0,0,'doub',stat)
		if (stat.ne.1) call ibis_signal(ibis,stat,abend)
	endif
	do j=1,tfields
		count=ibis_group_new (ibis,'group',ttype(j),j,1,' ')
		if (count.lt.0) call ibis_signal (ibis,stat,abend)
	enddo
c
90	continue
	nlab=1			!INITIALIZE OUTPUT LABEL PTR
	totlab=0		!INITIALIZE TOTAL FITS KEYWORDS
	trlab=0			!initialize FITS keywords to be transferred
C
C	REINITIALIZE POINTER TO START OF FITS FILE
C
	iend=.false.
	fitsrec=catrec
 	CALL XVREAD(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',fitsns,' ')
	call chkstat (stat,'XVRead error - tablopen',1,0,0)
100	continue
c				!EXAMINE/LIST REMAINING CARD IMAGES
	CALL FITSKEY (data,bfctr,xfer,tabcols,tabrows,tabdepth,jpass,
	1 ipass,bscale,bzero,iscl,izero,xtend,xend,icmt,ihst,
	2 inul,iexcise,excnt,xtens,nlab,trlab,totlab,
	3 excludst,passtart,lab)
C
C	WRITE OUT FITS LABELS TO IBIS FILE
C
	do i=1,nlab
		CALL KEYINC2(KEY)	!Increment "VFxxx" VICAR Label prefix
 		ilab=trlab-nlab+i
		call mvlc (lab(1,i),flabel,80,' ')
		CALL XLADD (ouni1,'HISTORY',key,flabel,STAT,'FORMAT',
	1	'STRING','ULEN',80,'ERR_ACT','S',' ')
		call chkstat(stat,'xladd err LABEL # = ',0,ilab,1)
	ENDDO
125	continue
	if (iend.and..not.xtens) go to 150	!Branch if done with headers
	if (xend) go to 150
C***************************
C     READ REST OF ANNOTATION RECORDS.
C***************************
	fitsrec=fitsrec+1
	CALL XVREAD(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',
	1 fitsrec,' ')				!READ NEXT HEADER
	call chkstat (stat,'XVRead error - tablopen',1,0,0)
	nlab=1					!INITIALIZE OUTPUT LABEL PTR
	go to 100
c
150	continue
	return
	end
C=========================================================================
	subroutine bfitsi (iuni,ouni,npix,nlin,nband,bfctr,bzero,
	1 bscale,fitsrec,idat)
c
c	subroutine to write out VICAR byte data from from FITS byte data
c
	implicit none
c
	byte bdata(28800),data(28800)
	integer*4 il,stat,fitsns,i,j,iuni,ouni,npix,nlin,nband
	integer*4 bfctr,fitsrec
	integer*4 vicptr,vicrow,vicband,fitsptr
	logical*4 idat
	real*4 bscale,bzero
	real*4 rdata(28800)
c
	fitsns=bfctr*2880		!samples(bytes)/record
	il=0
	fitsptr=fitsns+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns) go to 20
	fitsrec=fitsrec+1
	CALL XVREAD(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',fitsrec,' ')
	if (stat.eq.-30 .and. vicrow.ge.(nlin-1)) then
	  call xvmessage(' Warning: last 2880-byte block is incomplete!',' ')
	else
	  call chkstat (stat,'XVRead error - bfitsi',1,0,0)
	endif
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(npix-vicptr,fitsns-fitsptr)
	do i=1,il
		bdata(vicptr+i)=data(fitsptr+i)
	enddo
C
 	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (vicptr.lt.npix) go to 10
c
	if (idat) then
C
C	NEED TO CONVERT HERE USING FORMULA
C	ODATA = VAX*BSCALE+BZERO
C	IF YOU WANT "TRUE" VALUES STORED IN VICAR FILE
C
		do j=1,npix
			RDATA(J)=BDATA(J)*BSCALE+BZERO
		enddo
		call xvwrit (ouni,rdata,stat,'NSAMPS',npix,'LINE',
	1	vicrow,'BAND',vicband,' ')
		call chkstat (stat,'XVwrit error - bfitsi',1,0,0)
	else
		call xvwrit(ouni,bdata,stat,'NSAMPS',npix,'LINE',
	1	vicrow,'BAND',vicband,' ')
		call chkstat (stat,'XVwrit error - bfitsi',1,0,0)
	endif
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.nlin) go to 10
	vicband=vicband+1
	vicrow=1
	if (vicband.le.nband) go to 10
	return
	end
c=========================================================================
	subroutine hfitsi (iuni,ouni,npix,nlin,nband,bfctr,bzero,
	1 bscale,fitsrec,idat)
c
c	Routine to transfer FITS integer*2 data to VICAR image file
c
	implicit none
c
	byte data(28800)
	integer*2 hdata(14400),odata(14400)
	integer*4 il,i,j,stat,fitsns,iuni,ouni,npix,nlin,nband,bfctr
	integer*4 vicptr,vicrow,vicband,fitsptr,fitsns2,fitsrec
	integer*4 transhalf(12)
	logical*4 idat
	real*4 bzero,bscale
	real*4 rdata(7200)
c
c	modified algorithm from reference 1
c
	call xvtrans_in (transhalf,'HALF','HALF','HIGH','IEEE',stat)
	call chkstat (stat,'XVtrans_in error for transhalf',1,0,0)
C
	fitsns=bfctr*2880		!samples(bytes)/record
	fitsns2=fitsns/2		!halfs/record
	il=0
	fitsptr=fitsns2+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row 
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns2) go to 20
	fitsrec=fitsrec+1
	CALL XVREAD(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',fitsrec,' ')
	if (stat.eq.-30 .and. vicrow.ge.(nlin-1)) then
	  call xvmessage(' Warning: last 2880-byte block is incomplete!',' ')
	else
	  call chkstat (stat,'XVRead error - hfitsi',1,0,0)
	endif
	call xvtrans (transhalf,data,hdata,fitsns2)
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(npix-vicptr,fitsns2-fitsptr)
	do i=1,il
		odata(vicptr+i)=hdata(fitsptr+i)
	enddo
	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (vicptr.lt.npix) go to 10
	if (idat) then
C
C	NEED TO CONVERT HERE USING FORMULA
C	RDATA = ODATA*BSCALE+BZERO
C	IF YOU WANT "TRUE" VALUES STORED IN VICAR FILE
C
		do j=1,npix
			RDATA(J)=ODATA(J)*BSCALE+BZERO
		enddo
		CALL XVWRIT(OUNI,RDATA,STAT,'NSAMPS',npix,'LINE',vicrow,
	1	'BAND',vicband,' ')		!Output data to VICAR
		call chkstat (stat,'XVwrit error - hfitsi',1,0,0)
	else
		CALL XVWRIT(OUNI,ODATA,STAT,'NSAMPS',npix,'LINE',vicrow,
	1	'BAND',vicband,' ')		!Output data to VICAR
		call chkstat (stat,'XVwrit error - hfitsi',1,0,0)
	endif
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.nlin) go to 10		!loop if not finished band
	vicband=vicband+1			!bump band count
	vicrow=1				!reinit row ctr
	if (vicband.le.nband) go to 10		!loop if want to start new band
	return
	end
c=========================================================================
	subroutine ufitsi (iuni,ouni,npix,nlin,nband,bfctr,fitsrec)
c
c	Routine to transfer unsigned FITS integer*2 data to a
c	fullword VICAR image file since VICAR 16-bit data is signed
c
	implicit none
c
	byte data(28800)
	integer*2 hdata(14400)
	integer*4 fdata(14400)
	integer*4 il,i,stat,fitsns,iuni,ouni,npix,nlin,nband,bfctr
	integer*4 vicptr,vicrow,vicband,fitsptr,fitsns2,fitsrec,temp
	integer*4 transhalf(12)
c
c	modified algorithm from reference 1
c
	call xvtrans_in (transhalf,'HALF','HALF','HIGH','IEEE',stat)
	call chkstat (stat,'XVtrans_in error for transhalf',1,0,0)
C
	fitsns=bfctr*2880		!samples(bytes)/record
	fitsns2=fitsns/2		!halfs/record
	il=0
	fitsptr=fitsns2+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row 
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns2) go to 20
	fitsrec=fitsrec+1
	CALL XVREAD(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',fitsrec,' ')
	if (stat.eq.-30 .and. vicrow.ge.(nlin-1)) then
	  call xvmessage(' Warning: last 2880-byte block is incomplete!',' ')
	else
	  call chkstat (stat,'XVRead error - ufitsi',1,0,0)
	endif
	call xvtrans (transhalf,data,hdata,fitsns2)
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(npix-vicptr,fitsns2-fitsptr)
	do i=1,il
		fdata(vicptr+i)=hdata(fitsptr+i)
		if (fdata(vicptr+i).lt.0) then
			temp=hdata(fitsptr+i)
			fdata(vicptr+i)=65536+temp	
		endif
	enddo
	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (vicptr.lt.npix) go to 10
	CALL XVWRIT(OUNI,fdata,STAT,'NSAMPS',npix,'LINE',vicrow,
	1	'BAND',vicband,' ')		!Output data to VICAR
	call chkstat (stat,'XVwrit error - ufitsi',1,0,0)
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.nlin) go to 10		!loop if not finished band
	vicband=vicband+1			!bump band count
	vicrow=1				!reinit row ctr
	if (vicband.le.nband) go to 10		!loop if want to start new band
	return
	end
C=========================================================================
	subroutine ifitsi (iuni,ouni,npix,nlin,nband,bfctr,bzero,
	1 bscale,fitsrec,idat)
c
c	subroutine to write out VAX integer*4 data from FITS integer*4  data
c
	implicit none
c
	byte data(28800)
	integer*4 full(7200),vax(7200)
	integer*4 transfull(12)
	integer*4 il,stat,fitsns,i,j,iuni,ouni,npix,nlin,nband,bfctr
	integer*4 vicptr,vicrow,vicband,fitsptr,fitsns4,fitsrec
	logical*4 idat
	real*4 bzero,bscale
	real*4 rdata(28800)
c
	call xvtrans_in (transfull,'FULL','FULL','HIGH','IEEE',stat)
	call chkstat (stat,'XVtrans_in error for transreal',1,0,0)
C
	fitsns=bfctr*2880		!samples(bytes)/record
	fitsns4=fitsns/4		!fulls/record
	il=0
	fitsptr=fitsns4+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns4) go to 20
	fitsrec=fitsrec+1
	CALL XVREAD(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',fitsrec,' ')
	if (stat.eq.-30 .and. vicrow.ge.(nlin-1)) then
	  call xvmessage(' Warning: last 2880-byte block is incomplete!',' ')
	else
	  call chkstat (stat,'XVRead error - ifitsi',1,0,0)
	endif
	call xvtrans (transfull,data,full,fitsns4)
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(npix-vicptr,fitsns4-fitsptr)
C
	do i=1,il
		vax(vicptr+i)=full(fitsptr+i)
	enddo
 	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (vicptr.lt.npix) go to 10
c
	if (idat) then
C
C	NEED TO CONVERT HERE USING FORMULA
C	ODATA = VAX*BSCALE+BZERO
C	IF YOU WANT "TRUE" VALUES STORED IN VICAR FILE
C
		do J=1,npix
			RDATA(J)=VAX(J)*BSCALE+BZERO
		enddo
		call xvwrit (ouni,rdata,stat,'NSAMPS',npix,'LINE',
	1	vicrow,'BAND',vicband,' ')
		call chkstat (stat,'XVwrit error - ifitsi',1,0,0)
	else
		call xvwrit(ouni,vax,stat,'NSAMPS',npix,'LINE',
	1	vicrow,'BAND',vicband,' ')
		call chkstat (stat,'XVwrit error - ifitsi',1,0,0)
	endif
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.nlin) go to 10
	vicband=vicband+1
	vicrow=1
	if (vicband.le.nband) go to 10
	return
	end
C=========================================================================
	subroutine rfitsi (iuni,ouni,npix,nlin,nband,bfctr,bzero,
	1 bscale,fitsrec,idat)
c
c	subroutine to write out VAX real*4 data from ieee real*4 from FITS
c
	implicit none
c
	byte data(28800)
	integer*4 transreal(12)
	integer*4 il,stat,fitsns,i,j,iuni,ouni,npix,nlin,nband,bfctr
	integer*4 vicptr,vicrow,vicband,fitsptr,fitsns4,fitsrec
	logical*4 idat
	real*4 vax(7200),rdata(28800)
	real*4 ieee(7200)
	real*4 bzero,bscale
c
	call xvtrans_in (transreal,'REAL','REAL','HIGH','IEEE',stat)
	call chkstat (stat,'XVtrans_in error for transreal',1,0,0)
C
	fitsns=bfctr*2880		!samples(bytes)/record
	fitsns4=fitsns/4		!reals/record
	il=0
	fitsptr=fitsns4+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns4) go to 20
	fitsrec=fitsrec+1
	CALL XVREAD(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',fitsrec,' ')
	if (stat.eq.-30 .and. vicrow.ge.(nlin-1)) then
	  call xvmessage(' Warning: last 2880-byte block is incomplete!',' ')
	else
	  call chkstat (stat,'XVRead error - rfitsi',1,0,0)
	endif
	call xvtrans (transreal,data,ieee,fitsns4)
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(npix-vicptr,fitsns4-fitsptr)
C
	do i=1,il
		vax(vicptr+i)=ieee(fitsptr+i)
	enddo
 	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (vicptr.lt.npix) go to 10
c
	if (idat) then
C
C	NEED TO CONVERT HERE USING FORMULA
C	RDATA = ODATA*BSCALE+BZERO
C	IF YOU WANT "TRUE" VALUES STORED IN VICAR FILE
C
		do J=1,npix
			RDATA(J)=VAX(J)*BSCALE+BZERO
		enddo
		call xvwrit (ouni,rdata,stat,'NSAMPS',npix,'LINE',
	1	vicrow,'BAND',vicband,' ')
		call chkstat (stat,'XVwrit error - rfitsi',1,0,0)
	else
		call xvwrit(ouni,vax,stat,'NSAMPS',npix,'LINE',
	1	vicrow,'BAND',vicband,' ')
		call chkstat (stat,'XVwrit error - rfitsi',1,0,0)
	endif
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.nlin) go to 10
	vicband=vicband+1
	vicrow=1
	if (vicband.le.nband) go to 10
	return
	end
C=========================================================================
	subroutine dfitsi (iuni,ouni,npix,nlin,nband,bfctr,bzero,
	1 bscale,fitsrec,idat)
c
c	subroutine to write out VAX real*8 data from ieee real*8 from FITS
c
	implicit none
c
	byte data(28800)
	integer*4 transdble(12)
	integer*4 il,stat,fitsns,i,j,iuni,ouni,npix,nlin,nband,bfctr
	integer*4 vicptr,vicrow,vicband,fitsptr,fitsns8,fitsrec
	logical*4 idat
	real*8 vax(3600),rdata(28800)
	real*8 ieee8(3600)
	real*4 bzero,bscale
c
	call xvtrans_in (transdble,'DOUB','DOUB','HIGH','IEEE',stat)
	call chkstat (stat,'XVtrans_in error for transdble',1,0,0)
C
	fitsns=bfctr*2880		!samples(bytes)/record
	fitsns8=fitsns/8		!doubles/record
	il=0
	fitsptr=fitsns8+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns8) go to 20
	fitsrec=fitsrec+1
	CALL XVREAD(IUNI,DATA,STAT,'NSAMPS',fitsns,'LINE',fitsrec,' ')
	if (stat.eq.-30 .and. vicrow.ge.(nlin-1)) then
	  call xvmessage(' Warning: last 2880-byte block is incomplete!',' ')
	else
	  call chkstat (stat,'XVRead error - dfitsi',1,0,0)
	endif
	call xvtrans (transdble,data,ieee8,fitsns8)
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(npix-vicptr,fitsns8-fitsptr)
C
	do i=1,il
		vax(vicptr+i)=ieee8(fitsptr+i)
	enddo
 	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (vicptr.lt.npix) go to 10
c
	if (idat) then
C
C	NEED TO CONVERT HERE USING FORMULA
C	RDATA = ODATA*BSCALE+BZERO
C	IF YOU WANT "TRUE" VALUES STORED IN VICAR FILE
C
		do j=1,npix
			RDATA(J)=VAX(J)*dble(BSCALE)+dble(BZERO)
		enddo
		call xvwrit (ouni,rdata,stat,'NSAMPS',npix,'LINE',
	1	vicrow,'BAND',vicband,' ')
		call chkstat (stat,'XVwrit error - dfitsi',1,0,0)
	else
		call xvwrit(ouni,vax,stat,'NSAMPS',npix,'LINE',
	1	vicrow,'BAND',vicband,' ')
		call chkstat (stat,'XVwrit error - dfitsi',1,0,0)
	endif
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.nlin) go to 10
	vicband=vicband+1
	vicrow=1
	if (vicband.le.nband) go to 10
	return
	end
C=========================================================================
	subroutine fitsibis (iuni,ouni,tabcols,tabrows,tabdepth,tfields,
	1 bfctr,tabrec,ibfmt,tform,ttype,tbcol,fullrecord,realrecord,
	2 asciirecord,doubrecord,ibis)
c
c	subroutine to write out IBIS data from from FITS Table data
c
c npix=tabcols  nlin=tabrows   nband=tabdepth
c
	implicit none
c
	byte bdata(28800),data(28800)
	integer*4 il,stat,fitsns,i,field,iuni,ouni,tabcols,tabrows
	integer*4 tabdepth,bfctr,tabrec,fitsrec,tfields,ibis
	integer*4 vicptr,vicrow,vicband,fitsptr,abend
	integer*4 fullindex,realindex,asciiindex,doubindex
	integer*4 fullrecord,realrecord,asciirecord,doubrecord
	integer*4 tbcol(999)
	integer*4 fullval(999)
	real*4 realval(999)
	real*8 doubval(999)
	character*4 ibfmt(999)
	character*8 ttype(999)
	character*10 tform(999)
	character*20 asciival(999)
	character*2880 fitstable
c
	realindex=0
	fullindex=0
	asciiindex=0
	doubindex=0
	fitsns=bfctr*2880		!samples(bytes)/record
	fitsrec=tabrec
	il=0
	fitsptr=fitsns+1		!pointer to fits data
	vicptr=0			!pointer to vicar data
	vicrow=1			!pointer to vicar row
	vicband=1			!pointer to vicar band
10	continue
	if (fitsptr.lt.fitsns) go to 20
	fitsrec=fitsrec+1
	CALL XVREAD(IUNI,DATA,STAT,'LINE',fitsrec,'NSAMPS',fitsns,' ')
	call chkstat (stat,'XVRead error - fitsibis',1,0,0)
	fitsptr=0			!reset fits data pointer
20	continue
	il=min(tabcols-vicptr,fitsns-fitsptr)
	do i=1,il
		bdata(vicptr+i)=data(fitsptr+i)
	enddo
C
 	vicptr=vicptr+il
	fitsptr=fitsptr+il
	if (vicptr.lt.tabcols) go to 10
	call mvlc (bdata,fitstable,tabcols,' ')
	do field=1,tfields
		call fitsconv (field,fitstable,tfields,tform,tbcol,ibfmt,
	1	realindex,fullindex,asciiindex,doubindex,realval,
	2	fullval,asciival,doubval,tabcols)
	enddo
	if (fullindex.gt.0) then
		call ibis_record_write (fullrecord,fullval,vicrow,stat)
		if (stat.ne.1) call ibis_signal (ibis,stat,abend)
	endif
	if (realindex.gt.0) then
		call ibis_record_write (realrecord,realval,vicrow,stat)
		if (stat.ne.1) call ibis_signal (ibis,stat,abend)
	endif
	if (asciiindex.gt.0) then
		call ibis_record_write (asciirecord,asciival,vicrow,stat)
		if (stat.ne.1) call ibis_signal (ibis,stat,abend)
	endif
	if (doubindex.gt.0) then
		call ibis_record_write (doubrecord,doubval,vicrow,stat)
		if (stat.ne.1) call ibis_signal (ibis,stat,abend)
	endif
	realindex=0
	fullindex=0
	asciiindex=0
	doubindex=0
	vicptr=0
	vicrow=vicrow+1
	if (vicrow.le.tabrows) go to 10
	vicband=vicband+1
	vicrow=1
	if (vicband.le.tabdepth) go to 10
	return
	end
C=========================================================================
	subroutine fitsconv (field,fitstable,tfields,tform,tbcol,ibfmt,
	1 realindex,fullindex,asciiindex,doubindex,realval,
	2 fullval,asciival,doubval,tabcols)
c
c	routine to pack data into ibis buffers
c
	implicit none
c
	integer*4 field,tfields,realindex,fullindex,asciiindex,doubindex
	integer*4 colbeg,colend,tabcols
	integer*4 fullval(999),tbcol(999)
	real*4 realval(999)
	real*8 doubval(999)
	character*4 ibfmt(999)
	character*10 tform(999)
	character*20 asciival(999)
	character*2880 fitstable
c
	if (ibfmt(field).eq.'FULL') then
		fullindex=fullindex+1
		colbeg=tbcol(field)
		colend=tbcol(field+1)-1
		if (field.eq.tfields) colend=tabcols
		read (fitstable(colbeg:colend),tform(field)) 
	1		fullval(fullindex)
	else if (ibfmt(field).eq.'REAL') then
		realindex=realindex+1
		colbeg=tbcol(field)
		colend=tbcol(field+1)-1
		if (field.eq.tfields) colend=tabcols
		read (fitstable(colbeg:colend),tform(field)) 
	1		realval(realindex)
	else if (ibfmt(field).eq.'DOUB') then
		doubindex=doubindex+1
		colbeg=tbcol(field)
		colend=tbcol(field+1)-1
		if (field.eq.tfields) colend=tabcols
		read (fitstable(colbeg:colend),tform(field)) 
	1		doubval(doubindex)
	else if (ibfmt(field)(1:1).eq.'A') then
		asciiindex=asciiindex+1
		colbeg=tbcol(field)
		colend=tbcol(field+1)-1
		if (field.eq.tfields) colend=tabcols
		read (fitstable(colbeg:colend),tform(field)) 
	1		asciival(asciiindex)
	endif
c
	return
	end
C=========================================================================
	SUBROUTINE VALHDR (data,xfer,end,oneblock,xtens,table,xtype,
	1 bintable,imgxfer,tbxfer,btxfer,valid) 
C
C	Routine to examine FIRST CARD IMAGE OF HEADER and FIRST CARD
C	IMAGE OF XTENSION HEADER
C
	implicit none
c
	common /fitskeys/ keyword
c
	byte data(28800)
	integer*4 j
	logical*4 xfer,end,simple,xtens,table,bintable,oneblock
	logical*4 imgxfer,tbxfer,btxfer,valid
	character*8 keyword(35)
	character*8 hdrword,xtype
	character*80 outline
C
C Check first keyword
C
	call mvlc (data(1),hdrword,8,' ')
	if (keyword(1).NE. HDRWORD) GO TO 10020		!SIMPLE =
	simple=.true.
	valid=.true.
	if (xfer) imgxfer=.true.
	if (DATA(30).EQ.ichar('T')) GO TO 10100
	if (DATA(30).NE.ichar('F')) GO TO 10005
	simple=.false.
	write (outline,100)
100	format ('** SIMPLE = F, TAPE DOES NOT CONFORM TO FITS
	1 STANDARD')
	call xvmessage (outline,' ')
	write (outline,105)
105	format ('PROCESSING WILL BE ATTEMPTED ANYWAY...')
	call xvmessage (outline,' ')
	GO TO 10100
C
C	LOOP THROUGH FIELDS 11-30 TO SEE IF 'T' OR 'F' IN NONSTANDARD POSITION
C
10005 CONTINUE
	DO 10010 J=11,30
	IF (DATA(J).NE.ichar('T')) GO TO 10008
	write (outline,110)
110	format ('**ERROR - SIMPLE = T, T FOUND IN NONSTANDARD POSITION')
	call xvmessage (outline,' ')
	GO TO 10100
c
10008 CONTINUE
	IF (DATA(J).NE.ichar('F')) GO TO 10010
	write (outline,115)
115	format ('**ERROR - SIMPLE = F, F FOUND IN NONSTANDARD POSITION')
	call xvmessage (outline,' ')
	write (outline,100)
	call xvmessage (outline,' ')
	write (outline,105)
	call xvmessage (outline,' ')
	GO TO 10100
c
10010 CONTINUE
	write (outline,120)
120	format ('**ERROR - NO VALID T OR F FOUND FOR SIMPLE =')
	call xvmessage (outline,' ')
	write (outline,125)
125	format ('ONE HEADER BLOCK LISTED - BUT NO PROCESSING')
	call xvmessage (outline,' ')
	xfer=.false.			!LIST BUT NO PROCESSING
	oneblock=.true.			!list only 1st block of header
	GO TO 10100
C
C	CHECK FOR HDRWORD = XTENSION
C	common xtensions are '3DTABLE ', 'A3DTABLE', 'TABLE   ', 'BINTABLE'
C
10020 CONTINUE						!EXTENSION =
	if (keyword(15).NE.HDRWORD) GO TO 10030
	xtens=.true.
	valid=.true.
	call mvlc (data(12),xtype,8,' ')
	if (xtype.eq.'TABLE   ') then
		table=.true.
		if (xfer) tbxfer=.true.
	endif
	if (xtype.eq.'BINTABLE') then
		bintable=.true.
		if (xfer) btxfer=.true.
	endif
c**	write (outline,127) xtype
c**127	format ('**************     XTENSION = ',a8,' KEYWORD FOUND')
c**	call xvmessage (outline,' ')
	if (xtype.eq.'3DTABLE ') then
		write (outline,128)
128	format ('**************    That FITS data type not supported')
		call xvmessage (outline,' ')
		xfer=.false.
	endif
	if (xtype.eq.'A3DTABLE') then
		write (outline,128)
		call xvmessage (outline,' ')
		xfer=.false.
	endif
	GO TO 10100
C
C	CHECK FOR HDRWORD = TABNAME - OLDER AIPS FILES
C
10030 CONTINUE
	IF (keyword(27).NE.HDRWORD) GO TO 10050			!TABNAME =
	table=.true.
	valid=.true.
	write (outline,130)
130	format ('**ERROR - TABNAME = OLDER AIPS KEYWORD FOUND')
	call xvmessage (outline,' ')
	write (outline,125)
	call xvmessage (outline,' ')
	write (outline,135)
135	format ('*** SEE RESPONSIBLE PROGRAMMER ***')
	call xvmessage (outline,' ')
	xfer=.false.			!LIST BUT NO PROCESSING
	go to 10100
C
C	INVALID HEADER KEYWORD
C
10050 CONTINUE
	valid=.false.
10100 CONTINUE
	return
	end
C=========================================================================
	subroutine fitschk  (data,bfctr,xfer,iscl,izero,xtend,group,totlab,
	1 hierarch,end,bscale,bzero,btpix,nlin,npix,nband,nax,
	2 imgxfer,tbxfer,btxfer)
C
C	ROUTINE TO EXAMINE EACH HEADER RECORD OF 2880 BYTES,
C	TO COMPARE FITS DATA WITH DESIRED VICAR OUTPUT IMAGE OR TABLE,
C	AND PASS OUT APPROPRIATE WARNING MESSAGES
C
	implicit none
c
	common /fitskeys/ keyword
c
	character*8 keyword(35)
c
	byte data(28800),CARD(80,360)
	integer*4 i,k,m,one,two,three,four,nax,btpix,bfctr
	integer*4 npix,nlin,nband,totlab
	logical*4 end,idat,xtend,group,imgxfer,tbxfer,btxfer
	logical*4 xfer,iscl,izero,hierarch
	real*4 bzero,bscale
	character*8 fkey(360)
	character*80 outline,cardimg
c
	data one,two,three,four/1,2,3,4/
c
	do k=1,36*bfctr
	do i=1,80
		card(i,k)=data(80*(k-1)+i)
	enddo
	enddo
c
	do 10510 M=1,36*bfctr
	call mvlc (card(1,m),fkey(m),8,' ')
	call mvlc (card(1,m),cardimg,80,' ')
	totlab=totlab+1
	if (fkey(m).eq.keyword(1)) go to 10510		!SIMPLE
	if (fkey(m).eq.keyword(18)) go to 10510		!'        '
	if (fkey(m).eq.keyword(11)) go to 10510		!COMMENT
	if (fkey(m).eq.keyword(12)) go to 10510		!HISTORY
	if (FKEY(M).NE.KEYWORD(2)) GO TO 10010		!BITPIX 
	read (cardimg(27:30),20110) btpix
20110 format (i4)
	if (btpix.eq.8) go to 10510
	if (btpix.eq.16) go to 10510
	if (btpix.eq.32) go to 10510
	if (btpix.eq.-32) go to 10510
	if (btpix.eq.-64) go to 10510
	write (outline,105) btpix
105	format ('**ERROR - BITPIX =',i6,' a non-standard value')
	call xvmessage (outline,' ')
	write (outline,110)
110	format ('HEADER BLOCK LISTED - BUT NO PROCESSING')
	call xvmessage (outline,' ')
	xfer=.false.
	imgxfer=.false.
	tbxfer=.false.
	btxfer=.false.
	GO TO 10510
c
10010 CONTINUE
	if (FKEY(M).NE.KEYWORD(3)) GO TO 10020		!NAXIS - NUMBER OF AXES
	read (cardimg(27:30),20110) nax
c**	if (nax.eq.0) then
c**		write (outline,115)
c**115	format ('** NAXIS = 0, NO DATA ARRAY PRESENT')
c**		call xvmessage (outline,' ')
c**		if (xfer) imgxfer=.false.
c**		go to 10510
c**	endif

	if (nax.gt.3) then
		write (outline,120) nax
120	format ('**ERROR - NAXIS = ',i6,' NUMBER OF AXES GREATER THAN 3')
		call xvmessage (outline,' ')
		write (outline,110)
		call xvmessage (outline,' ')
		write (outline,125)
125	format ('*** SEE RESPONSIBLE PROGRAMMER ***')
		call xvmessage (outline,' ')
		imgxfer=.false.
		tbxfer=.false.
		btxfer=.false.
		xfer=.false.	!LIST BUT NO PROCESSING whether image or table
		GO TO 10510
	endif
	go to 10510
c
10020 CONTINUE
	IF (FKEY(M).NE.KEYWORD(4)) GO TO 10030	!NAXIS1 - Number of Pixels
	read (cardimg(27:30),20110) npix
	if (npix.eq.0) then
		write (outline,127) one
127	format ('** NAXIS',i1,' = 0 - NO DATA ARRAY PRESENT')
		call xvmessage (outline,' ')
		if (xfer) imgxfer=.false.
		GO TO 10510
	endif
	go to 10510
c
10030 CONTINUE
	IF (FKEY(M).NE.KEYWORD(5)) GO TO 10040	!NAXIS2 - Number of Lines
	read (cardimg(27:30),20110) nlin
	if (nlin.eq.0) then
		write (outline,130) two,two
130	format ('** NAXIS',i1,' = 0, ,DATA IN AXIS ',i1,
	1	' NOT AN ARRAY, OR')
		call xvmessage (outline,' ')
		write (outline,117)
117	format ('DATA IS POSSIBLY AN IRREGULARLY-SPACED ARRAY')
		call xvmessage(outline,' ')
		if (xfer) imgxfer=.false.
		GO TO 10510
	endif
	go to 10510
c
10040 CONTINUE
	IF (FKEY(M).NE.KEYWORD(6)) GO TO 10050	!NAXIS3 - Number of Bands
	read (cardimg(27:30),20110) nband
	if (nband.eq.0) then
		write (outline,130) three,three
		call xvmessage (outline,' ')
		write (outline,117)
		call xvmessage(outline,' ')
		GO TO 10510
	endif
	go to 10510
c
10050 CONTINUE
	IF (FKEY(M).NE.KEYWORD(7)) GO TO 10060		!BSCALE - 
c	CALL INCON (NWORD,CARD(11,M),BSCALE,20)
	read (cardimg(11:30),20120) bscale
20120 format (e20.5)
	if (bscale.gt.1.0.and.idat) then
		write (outline,150)
150	format ('**ERROR - Warning: DATA CANNOT BE RESTORED WITH
	1 ORIGINAL ACCURACY')
		call xvmessage (outline,' ')
		write (outline,155) bscale
155	format ('**Note that BSCALE = ',f9.2)
		call xvmessage (outline,' ')
	endif
	iscl=.true.
	GO TO 10510
c
10060 CONTINUE
	IF (FKEY(M).NE.KEYWORD(8)) GO TO 10070		!BZERO - 
	read (cardimg(11:30),20120) bzero
	izero=.true.
	GO TO 10510
c
10070 CONTINUE
	IF (FKEY(M).NE.KEYWORD(13)) GO TO 10080		!BLOCKED - 
	IF (CARD(30,m).eq.ichar('T')) then
C
C	DON'T KNOW ABOUT FOLLOWING YET ....
C
		write (outline,170)
170	format ('** BLOCKED = T, TAPE RECORDS MAY BE BLOCKED')
		call xvmessage (outline,' ')
		write (outline,175)
175	format ('PROCESSING WILL BE ATTEMPTED ANYWAY...')
		call xvmessage (outline,' ')
		GO TO 10510
	endif
	go to 10510
c
10080 CONTINUE
	IF (FKEY(M).NE.KEYWORD(14)) GO TO 10100		!EXTEND -
	IF (CARD(30,m).eq.ichar('T')) then
		write (outline,180)
180	format ('** EXTEND = T, TAPE MAY CONTAIN FITS EXTENSIONS')
		call xvmessage (outline,' ')
		XTEND=.TRUE.
		GO TO 10510
	endif
	go to 10510
c
10100 CONTINUE
	IF (FKEY(M).NE.KEYWORD(20)) GO TO 10110		!GROUPS -
	IF (CARD(30,m).eq.ichar('T')) then
		write (outline,190)
190	format ('** GROUPS = T, TAPE CONTAINS A GROUP FORMAT FILE')
		call xvmessage (outline,' ')
		write (outline,110)
		call xvmessage (outline,' ')
		group=.true.
		xfer=.false.
		imgxfer=.false.
		tbxfer=.false.
		btxfer=.false.
		GO TO 10510
	endif
	go to 10510
c
10110 continue
	IF (FKEY(M).NE.KEYWORD(30)) GO TO 10120		!HIERARCH -
	if (CARD(30,m).eq.ichar('T')) then
		write (outline,200)
200	format ('**ERROR - HIERARCH = T, NO ABILITY TO PROCESS HIERARCH
	1 DATA TYPE')
		call xvmessage (outline,' ')
		write (outline,110)
		call xvmessage (outline,' ')
		hierarch=.true.
		xfer=.false.
		imgxfer=.false.
		tbxfer=.false.
		btxfer=.false.
		GO TO 10510
	endif
	go to 10510
c
10120	continue
	IF (FKEY(M).NE.KEYWORD(19)) GO TO 10510		!END - 
	end=.true.
	GO TO 10515
c
10510	CONTINUE
10515	CONTINUE
	if (nax.eq.0.and.xfer.and.xtend) imgxfer=.false.  !Probably table
	return
	end
C=========================================================================
	SUBROUTINE FITSPRT (data,bfctr,ouni,end,totlab)
C
C	ROUTINE TO PRINT OUT FITS HEADER TO FILE
C
	implicit none
c
	common /fitskeys/ keyword
c
	character*8 keyword(35)
c
	byte data(28800),card(81,360)
	integer*4 bfctr
	integer*4 stat,i,k,m,ouni,totlab
	logical*4 end
	character*8 fkey(360)
C
	do k=1,36*bfctr
	do i=1,80
		card(i,k)=data(80*(k-1)+i)
	enddo
	card(81,k)=10
	enddo
c
	do m=1,36*bfctr
		call mvlc (card(1,m),fkey(m),8,' ')
		call xvwrit(ouni,card(1,m),stat,' ')  !Output data to HDR file
		call chkstat(stat,'xvwrit err HDR File record = ',0,totlab,1)
	if (FKEY(m).eq.KEYWORD(19)) then		!END - 
		end=.true.
		return
	endif
	enddo
c
	return
	end
C=========================================================================
	SUBROUTINE FITSLIST (data,bfctr,end)
C
C	ROUTINE TO PRINT OUT FITS HEADER TO FILE
C
	implicit none
c
	common /fitskeys/ keyword
c
	character*8 keyword(35)
c
	byte data(28800),card(80,360)
	integer*4 i,k,m,bfctr
	logical*4 end
	character*80 outline,cardimg
C
	do k=1,36*bfctr
	do i=1,80
		card(i,k)=data(80*(k-1)+i)
	enddo
	enddo
c
	do m=1,36*bfctr
		call mvlc (card(1,m),cardimg,80,' ')
		write (outline,10000) cardimg
10000 format (a80)
		call xvmessage (outline,' ')
		if (cardimg(1:8).eq.KEYWORD(19)) then		!END - 
			end=.true.
			return
		endif
	enddo
c
	return
	end
C=========================================================================
	SUBROUTINE FITSKEY (data,bfctr,xfer,npix,nlin,nband,jpass,
	1 ipass,bscale,bzero,iscl,izero,xtend,end,icmt,ihst,
	2 inul,iexcise,excnt,xtens,nlab,trlab,totlab,
	3 excludst,passtart,lab)
C
C	ROUTINE TO EXAMINE EACH HEADER OF 2880 BYTES
C	AND TO INCLUDE OR EXCLUDE FITS KEYWORDS FROM TRANSFER
C	TO VICAR LABEL
C
	implicit none
c
	common /fitskeys/ keyword
c
	byte data(28800),lab(80,144)
	logical*4 ihst,icmt,inul,iscl,izero,ipass,jpass
	logical*4 iexcise,end,xtens,xtend,xfer
	integer*4 excnt,trlab,nlab,totlab
	integer*4 npix,nlin,nband,bfctr
	real*4 bzero,bscale
	character*8 keyword(35),passtart,excludst(10)
c
	byte card(80,360)
	integer*4 i,ii,j,k,m,nbt,nax
	character*8 FKEY(360)
	character*80 outline,cardimg
c
	do k=1,36*bfctr
	do i=1,80
		card(i,k)=data(80*(k-1)+i)
	enddo
	enddo
 
	DO 10510 M=1,36*bfctr
	call mvlc (card(1,m),fkey(m),8,' ')
	call mvlc (card(1,m),cardimg,80,' ')

	write (outline,10000) (card(ii,m), ii=1,80)
10000 format (80a1)
	call xvmessage (outline,' ')
	IF (FKEY(M).EQ.KEYWORD(1)) GO TO 10400	!SIMPLE 
	IF (FKEY(M).EQ.KEYWORD(15)) GO TO 10160	!XTENSION - Always store
	IF (FKEY(M).EQ.KEYWORD(9)) GO TO 10400	!BUNIT - Always store 
	IF (FKEY(M).EQ.KEYWORD(10)) GO TO 10400	!BLANK - Always store
	IF (FKEY(M).EQ.KEYWORD(16)) GO TO 10400	!DATAMAX - Always store
	IF (FKEY(M).EQ.KEYWORD(17)) GO TO 10400	!DATAMIN - Always store
	IF (FKEY(M).EQ.KEYWORD(20)) GO TO 10400	!GROUPS - Always store 
	IF (FKEY(M).EQ.KEYWORD(21)) GO TO 10400	!GCOUNT - Always store 
	IF (FKEY(M).EQ.KEYWORD(22)) GO TO 10400	!PCOUNT - Always store 
	IF (FKEY(M).EQ.KEYWORD(23)) GO TO 10400	!EXTNAME - Always store
	IF (FKEY(M).EQ.KEYWORD(24)) GO TO 10400	!EXTVER - Always store
	IF (FKEY(M).EQ.KEYWORD(25)) GO TO 10400	!EXTLEVEL - Always store
	IF (FKEY(M).EQ.KEYWORD(26)) GO TO 10400	!TFIELDS - Always store
	IF (FKEY(M).EQ.KEYWORD(27)) GO TO 10400	!TABNAME - Always store
	IF (FKEY(M).EQ.KEYWORD(28)) GO TO 10400	!OBJECT - Always store
	IF (FKEY(M).EQ.KEYWORD(29)) GO TO 10400	!DATE - Always store 
	IF (FKEY(M).EQ.KEYWORD(34)) GO TO 10400	!ORIGIN - Always store 
	IF (FKEY(M).NE.KEYWORD(2)) GO TO 10010	!BITPIX
	read (cardimg(27:30),20110) nbt
20110 format (i4)
	GO TO 10400				!Always store in VICAR label
10010 CONTINUE
	IF (FKEY(M).NE.KEYWORD(3)) GO TO 10020	!NAXIS - NUMBER OF AXES
	read (cardimg(27:30),20110) nax
	if (nax.eq.0) then
		call mvlc (card(1,m+1),passtart,8,' ')
		ipass=.true.
	endif
	GO TO 10400				!Always store in VICAR label
10020 CONTINUE
	IF (FKEY(M).NE.KEYWORD(4)) GO TO 10030	!NAXIS1 - Number of Pixels
	read (cardimg(27:30),20110) npix
	GO TO 10400				!Always store in VICAR label
10030 CONTINUE
	IF (FKEY(M).NE.KEYWORD(5)) GO TO 10040	!NAXIS2 - Number of Lines
	read (cardimg(27:30),20110) nlin
c**	if (nax.gt.2) go to 10505		!Do following only if naxis<3
	if (jpass) go to 10505			!Skip following if PASS= given
	call mvlc (card(1,m+1),passtart,8,' ')
	ipass=.true.
	GO TO 10400				!Always store in VICAR label
10040 CONTINUE
	IF (FKEY(M).NE.KEYWORD(6)) GO TO 10050	!NAXIS3 - Number of Bands
	read (cardimg(27:30),20110) nband
	if (jpass) go to 10505			!Skip following if PASS= given
	call mvlc (card(1,m+1),passtart,8,' ')
	ipass=.true.
	GO TO 10400				!Always store in VICAR label
10050 CONTINUE
	IF (FKEY(M).NE.KEYWORD(7)) GO TO 10060	!BSCALE - 
	read (cardimg(11:30),20120) bscale
20120 format (e20.5)
	ISCL=.TRUE.
	GO TO 10400				!Always store in VICAR label
10060 CONTINUE
	IF (FKEY(M).NE.KEYWORD(8)) GO TO 10070	!BZERO - 
	read (cardimg(11:30),20120) bzero
	IZERO=.TRUE.
	GO TO 10400				!Always store in VICAR label
10070 CONTINUE
	IF (FKEY(M).NE.KEYWORD(13)) GO TO 10080	!BLOCKED - 
	IF (CARD(30,M).NE.ichar('T')) GO TO 10300
	GO TO 10400				!Always store in VICAR label
10080 CONTINUE
	IF (FKEY(M).NE.KEYWORD(14)) GO TO 10090	!EXTEND - 
	IF (CARD(30,M).NE.ichar('T')) GO TO 10300
	xtend=.true.
	GO TO 10400				!Always store in VICAR label
10090 CONTINUE
c	IF (DATA(30).NE.ichar('F')) GO TO 10500
10100 CONTINUE
	IF (FKEY(M).NE.KEYWORD(19)) GO TO 10110	!END - 
	END=.TRUE.
	GO TO 10400				!Always store in VICAR label
10110 CONTINUE
	IF (FKEY(M).NE.KEYWORD(11)) GO TO 10120	!COMMENT =
	IF (.NOT.ICMT) GO TO 10505		!Don't store
	GO TO 10400
10120 CONTINUE
	IF (FKEY(M).NE.KEYWORD(12)) GO TO 10130	!HISTORY =
	IF (.NOT.IHST) GO TO 10505		!Don't store
	GO TO 10400
10130 CONTINUE
	IF (FKEY(M).NE.KEYWORD(18)) GO TO 10140	!'        ' =
	IF (.NOT.INUL) GO TO 10505		!Don't store
	GO TO 10400
10140 CONTINUE
	IF (IPASS) GO TO 10150			!Started "PASS"?
	IF (FKEY(M).NE.passtart) GO TO 10505	!If not - skip
	IPASS=.TRUE.				!Indicate we've found "PASS="
	GO TO 10400
10150 CONTINUE
	if (.not.iexcise) go to 10400
	do i=1,excnt
	IF (FKEY(M).EQ.EXCLUDST(i)) GO TO 10505	!Check to see if "EXCISE="
	enddo
	go to 10400
10160 continue
	xtens=.true.
	go to 10400
c
c	check for PASS= for things always entered but but not specifically
c	checked in PASS= loop
c
10300	continue
	IF (IPASS) GO TO 10400			!Started "PASS"?
	IF (FKEY(M).NE.passtart) GO TO 10505	!If not - skip
	IPASS=.TRUE.				!Indicate we've found "PASS="
C***************************
C     MOVE OBJECT INFO TO LABEL.
C***************************
10400 CONTINUE

	do i=1,80
		lab(i,nlab)=card(i,m)
	enddo
	do j=1,80
		IF(LAB(J,NLAB).EQ.39)LAB(J,NLAB)=ichar('"') !replace ' w "
        enddo
	nlab=nlab+1
C
10500 CONTINUE
	trlab=trlab+1
	if (trlab.eq.1000) then
		write (outline,500)
500	format (' WARNING - Total FITS keywords to be passed exceed 999')
		call xvmessage (outline,' ')
	endif
10505	continue
	totlab=totlab+1				!INCREMENT TOTAL LABELS COUNTER
	if (end) go to 10600
10510	continue
	nlab=nlab-1				!keep index counter correct
	return
c
10600 CONTINUE
	nlab=nlab-1
	write (outline,600) totlab
600	format (' Total FITS keywords found       = ',i6)
	call xvmessage (outline,' ')
	write (outline,605) trlab
605	format (' Total FITS keywords to transfer = ',i6)
	call xvmessage (outline,' ')
	if (.not.ipass) then
		write (outline,610) passtart
610	format ('Did not find indicated FITS keyword for PASS = ',a8)
		call xvmessage (outline,' ')
	endif
	return
	end
C============================================================================
	subroutine tablchk (data,bfctr,totlab,end,tabbits,tabax,tabcols,
	1 tabrows,tabdepth,pcount,gcount,tfields,extname,extver,extlevel,
	2 ttype,tbcol,tform,ibfmt)
c
	implicit none
c
	common /fitskeys/ keyword
c
	byte data(28800),card(80,360)
	integer*4 i,k,m,totlab
	integer*4 tabbits,tabax,tabcols,tabrows,tabdepth,pcount,gcount
	integer*4 tfields,extver,extlevel,index,bfctr
	integer*4 tbcol(999)
	logical*4 end
	character*1 blank1,lparen,rparen
	character*2 blank2
	character*3 blank3
	character*4 ibfmt(999)
	character*8 keyword(35)
	character*8 fkey(360),ttype(999)
	character*10 tform(999)
	character*16 extname
	character*80 cardimg,outline
c
	data blank1/' '/,blank2/'  '/,blank3/'   '/
	data lparen/'('/,rparen/')'/
c
c
	do k=1,36*bfctr
	do i=1,80
		card(i,k)=data(80*(k-1)+i)
	enddo
	enddo
c
	DO 10510 m=1,36*bfctr
	call mvlc (card(1,m),fkey(m),8,' ')
	call mvlc (card(1,m),cardimg,80,' ')
	totlab=totlab+1
	if (fkey(m).NE.KEYWORD(2)) GO TO 10110		!BITPIX 
	read (cardimg(27:30),20100) tabbits
20100 format (i4)
	go to 10510
10110 continue
	if (fkey(m).ne.KEYWORD(3)) go to 10120		!NAXIS -
	read (cardimg(27:30),20100) tabax
	go to 10510
10120 continue
	if (fkey(m).ne.KEYWORD(4)) go to 10130		!NAXIS1 -
	read (cardimg(27:30),20100) tabcols
	go to 10510
10130 continue
	if (fkey(m).ne.KEYWORD(5)) go to 10140		!NAXIS2 -
	read (cardimg(27:30),20100) tabrows
	go to 10510
10140 continue
	if (fkey(m).ne.KEYWORD(6)) go to 10150		!NAXIS3 -
	read (cardimg(27:30),20100) tabdepth
	go to 10510
10150 continue
	if (fkey(m).ne.KEYWORD(22)) go to 10160		!PCOUNT -
	read (cardimg(27:30),20100) pcount
	go to 10510
10160 continue
	if (fkey(m).ne.KEYWORD(21)) go to 10170		!GCOUNT - 
	read (cardimg(27:30),20100) gcount
	go to 10510
10170 continue
	if (fkey(m).ne.KEYWORD(26)) go to 10180		!TFIELDS -
	read (cardimg(27:30),20100) tfields
	go to 10510
10180 continue
	if (fkey(m).ne.KEYWORD(23)) go to 10190		!EXTNAME -
	read (cardimg(12:27),20180) extname
20180 format (a16)
	go to 10510
10190 continue
	if (fkey(m).ne.KEYWORD(24)) go to 10200		!EXTVER -
	read (cardimg(27:30),20100) extver
	go to 10510
10200 continue
	if (fkey(m).ne.KEYWORD(25)) go to 10210		!EXTLEVEL -
	read (cardimg(27:30),20100) extlevel
	go to 10510
10210 continue
	if (tfields.lt.0) go to 10510
	if (fkey(m)(1:5).ne.KEYWORD(31)(1:5)) go to 10300	!TTYPExxx -
	if (fkey(m)(6:8).ne.blank3) go to 10220
	read (cardimg(12:19),20210) ttype(1)
20210 format (a8)
	go to 10510
10220 continue
	if (fkey(m)(7:8).ne.blank2) go to 10230
	read (cardimg(6:6),20220) index
20220 format (i1)
	read (cardimg(12:19),20210) ttype(index)
	go to 10510
10230 continue
	if (fkey(m)(8:8).ne.blank1) go to 10240
	read (cardimg(6:7),20230) index
20230 format (i2)
	read (cardimg(12:19),20210) ttype(index)
	go to 10510
10240 continue
	read (cardimg(6:8),20240) index
20240 format (i3)
	read (cardimg(12:19),20210) ttype(index)
	go to 10510
c
10300 continue
	if (fkey(m)(1:5).ne.KEYWORD(32)(1:5)) go to 10400	!TBCOLxxx -
	if (fkey(m)(6:8).ne.blank3) go to 10320
	read (cardimg(27:30),20100) tbcol(1)
	go to 10510
10320 continue
	if (fkey(m)(7:8).ne.blank2) go to 10330
	read (cardimg(6:6),20220) index
	read (cardimg(27:30),20100) tbcol(index)
	go to 10510
10330 continue
	if (fkey(m)(8:8).ne.blank1) go to 10340
	read (cardimg(6:7),20230) index
	read (cardimg(27:30),20100) tbcol(index)
	go to 10510
10340 continue
	read (cardimg(6:8),20240) index
	read (cardimg(27:30),20100) tbcol(index)
	go to 10510
c
10400 continue
	if (fkey(m)(1:5).ne.KEYWORD(33)(1:5)) go to 10500	!TFORMxxx -
	if (fkey(m)(6:8).ne.blank3) go to 10420
	index=1
	read (cardimg(12:19),20210) tform(index)(2:9)
	go to 10445
10420 continue
	if (fkey(m)(7:8).ne.blank2) go to 10430
	read (cardimg(6:6),20220) index
	read (cardimg(12:19),20210) tform(index)(2:9)
	go to 10445
10430 continue
	if (fkey(m)(8:8).ne.blank1) go to 10440
	read (cardimg(6:7),20230) index
	read (cardimg(12:19),20210) tform(index)(2:9)
	go to 10445
10440 continue
	read (cardimg(6:8),20240) index
	read (cardimg(12:19),20210) tform(index)(2:9)
c
10445 continue
	tform(index)(1:1)=lparen
	tform(index)(10:10)=rparen
	do i=9,30
		if (cardimg(i:i).eq.'A') then
			ibfmt(index)=cardimg(i:i+3)
			go to 10510
		endif
		if (cardimg(i:i).eq.'D') then
			ibfmt(index)='DOUB'
			go to 10510
		endif
		if (cardimg(i:i).eq.'E') then
			ibfmt(index)='REAL'
			go to 10510
		endif
		if (cardimg(i:i).eq.'F') then
			ibfmt(index)='REAL'
			go to 10510
		endif
		if (cardimg(i:i).eq.'I') then
			ibfmt(index)='FULL'
			go to 10510
		endif
	enddo
	write (outline,21000) cardimg(1:30)
21000 format (' ERROR - Did not find proper format for ',a30)
	call xvmessage(outline,' ')
	call abend 
c
10500 continue
	if (fkey(m).ne.keyword(19)) go to 10510
	end=.true.
	go to 10515
c
10510 continue	
c
10515 continue
	return
	end
c===========================================================================
	subroutine tablelist (ttype,tbcol,tform,tfields,tabrows,extname,
	1 extver,unit,bfctr,tabcols,catrec)
c
	implicit none
c
	byte data(28800),tabdata(28800)
	integer*4 tfields,tabrows,extver,i,ii,unit,stat,bfctr,fitsns
	integer*4 fitsptr,tabptr,tabcols,il,row,catrec,temprec
	integer*4 tbcol(999)
	character*8 ttype(999)
	character*10 tform(999)
	character*16 extname
	character*100 outline
c
	write (outline,10100) extname,extver
10100 format ('Listing of FITS data set ',a16,' Version ',i4)
	call xvmessage (outline,' ')
c
	write (outline,10110) (ttype(ii), ii=1,tfields)
10110 format (10(a8,2x))
	call xvmessage (outline,' ')
c
	temprec=catrec
	fitsns=2880*bfctr
	fitsptr=fitsns+1
	tabptr=0
	row=0
10	continue
	if (fitsptr.lt.fitsns) go to 20
	catrec=catrec+1
	call xvread (unit,data,stat,'LINE',catrec,'NSAMPS',fitsns,' ')
	call chkstat (stat,'XVRead error - tablelist',1,0,0)
	fitsptr=0
20	continue
	il=min(tabcols-tabptr,fitsns-fitsptr)
	do i=1,il
		tabdata(tabptr+i)=data(fitsptr+i)
	enddo
c
	tabptr=tabptr+il
	fitsptr=fitsptr+il
	if (tabptr.lt.tabcols) go to 10
	write (outline,10200) (tabdata(ii), ii=1,80)
10200 format (80a1)
c
	call xvmessage (outline,' ')
	tabptr=0
	row=row+1
	if (row.le.tabrows) go to 10
c
	write (outline,10210) row-1
10210 format (i5,' Rows written')
	call xvmessage (outline,' ')
	catrec=temprec
	return
	end
c==========================================================================
      SUBROUTINE KEYINC2(KEY)
C
C	KEYINC2 -- SUBROUTINE TO INCREMENT CHARACTER*5 STRING KEY
C		ALLOWS UP TO 999 INCREMENTS
C
C	SEE KEYINC FOR CHARACTER*5 STRING TO ALLOW UP TO 99 INCREMENTS
C 
C	PASSED VARIABLES:
C
C KEY   -- CHARACTER*5 STRING CONTAINING VICAR LABEL KEY
C		SHOULD BE OF FORMAT AAXXX WHERE "A" IS ASCII ALPHABETIC
C		AND "X" IS ASCII NUMERIC.
C		FOR COMPATIBILITY WITH OLDER VICAR SUBROUTINE KEYINC 
C		KEYINC2 WILL ACCEPT FORMAT AABXX WHERE BOTH "A" AND "B"
C		ARE ALPHABETIC. WHEN IT DETECTS "B" ON AN INCREMENT INTO
C		THE HUNDREDS DIGIT IT WILL CHANGE "B" TO A "1"  
C
C	LOCAL VARIABLES:
C
C J,K,L -- ONE'S, TEN'S AND HUNDREDS DIGITS OF ASCII KEY
c
	implicit none
c
	BYTE J,K,L
	CHARACTER*5 KEY
	character*80 outline
C
C
C-- FOLLOWING PUT IN TO KEEP COMPATIBLE WITH OLDER ROUTINE
C-- RESETS HUNDREDS DIGIT TO A NUMBER
C
	L=ICHAR(KEY(3:3))
	IF (L.LT.ichar('0')) L=ichar('0')
	IF (L.GT.ichar('9')) L=ichar('0')

	J = ICHAR(KEY(5:5))
	IF (J.LT.ichar('0')) then
		write (outline,10100) 
10100 format (' KEYINC2 - Final character of key is not a digit')
		call xvmessage (outline,' ')
		RETURN
	ENDIF
	if (j.gt.ichar('9')) then
		write (outline,10100) 
		call xvmessage (outline,' ')
		RETURN
	ENDIF
	j=j+1
C
C-- UPDATE KEY NAME... J INDICATES ONE'S DIGIT, K THE TEN'S DIGIT
C-- L THE HUNDREDS DIGIT
C
      IF (J .GT. ichar('9')) THEN
	J = ichar('0')
	K = ICHAR(KEY(4:4)) + 1
		IF (K.GT.ichar('9')) THEN
		K=ichar('0')
		L=L+1
			IF (L.GT.ichar('9')) THEN
			write (outline,10200) 
10200 format (' KEYINC2 - Key exceeds xx999')
			call xvmessage (outline,' ')
			RETURN
			ENDIF
		KEY(3:3) = CHAR(L)
		ENDIF
	KEY(4:4) = CHAR(K)
      ENDIF
      KEY(5:5) = CHAR(J)
      RETURN
      END
C============================================================================
	BLOCK DATA
c
	common /fitskeys/ keyword
c
	character*8 keyword(35)
c
	DATA KEYWORD(1)  /'SIMPLE  '/		!LOGICAL
	DATA KEYWORD(2)  /'BITPIX  '/		!INTEGER
	DATA KEYWORD(3)  /'NAXIS   '/		!INTEGER
	DATA KEYWORD(4)  /'NAXIS1  '/		!INTEGER
	DATA KEYWORD(5)  /'NAXIS2  '/		!INTEGER
	DATA KEYWORD(6)  /'NAXIS3  '/		!INTEGER
	DATA KEYWORD(7)  /'BSCALE  '/		!FLOATING
	DATA KEYWORD(8)  /'BZERO   '/		!FLOATING
	DATA KEYWORD(9)  /'BUNIT   '/		!CHARACTER
	DATA KEYWORD(10) /'BLANK   '/		!INTEGER
	DATA KEYWORD(11) /'COMMENT '/		!CHARACTER
	DATA KEYWORD(12) /'HISTORY '/		!CHARACTER
	DATA KEYWORD(13) /'BLOCKED '/		!LOGICAL
	DATA KEYWORD(14) /'EXTEND  '/		!LOGICAL
	DATA KEYWORD(15) /'XTENSION'/		!CHARACTER
	DATA KEYWORD(16) /'DATAMAX '/		!FLOATING
	DATA KEYWORD(17) /'DATAMIN '/		!FLOATING
	DATA KEYWORD(18) /'        '/		!BLANKS ARE COMMENTS
	DATA KEYWORD(19) /'END     '/
	DATA KEYWORD(20) /'GROUPS  '/		!LOGICAL
	DATA KEYWORD(21) /'GCOUNT  '/		!INTEGER
	DATA KEYWORD(22) /'PCOUNT  '/		!INTEGER
	DATA KEYWORD(23) /'EXTNAME '/		!CHARACTER
	DATA KEYWORD(24) /'EXTVER  '/		!INTEGER
	DATA KEYWORD(25) /'EXTLEVEL'/		!INTEGER
	DATA KEYWORD(26) /'TFIELDS '/		!INTEGER
	DATA KEYWORD(27) /'TABNAME '/		!CHARACTER
	DATA KEYWORD(28) /'OBJECT  '/		!CHARACTER
	DATA KEYWORD(29) /'DATE    '/		!CHARACTER
	DATA KEYWORD(30) /'HIERARCH'/		!LOGICAL
	DATA KEYWORD(31) /'TTYPE   '/		!CHARACTER
	DATA KEYWORD(32) /'TBCOL   '/		!INTEGER
	DATA KEYWORD(33) /'TFORM   '/		!CHARACTER
	DATA KEYWORD(34) /'ORIGIN  '/		!CHARACTER
	DATA KEYWORD(35) /'        '/
	end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create fitsin.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM fitsin

   To Create the build file give the command:

		$ vimake fitsin			(VMS)
   or
		% vimake fitsin			(Unix)


************************************************************************/


#define PROGRAM	fitsin
#define R2LIB

#define MODULE_LIST fitsin.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define FTNINC_LIST fortport errdefs

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB

/* #define DEBUG */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create fitsin.pdf
process help=*
PARM INP  TYPE=STRING
PARM OUT  TYPE=STRING  COUNT=(0:10) DEFAULT=--
PARM HEADER TYPE=STRING COUNT=(0:10) DEFAULT=--
PARM DATA TYPE=STRING COUNT=(0:1) VALID=(TRUE,FITS,U16) DEFAULT=FITS
PARM BLOCK TYPE=STRING COUNT=(0:1) VALID=(BLOCK,NOBLOCK) DEFAULT=NOBLOCK
PARM FACTOR TYPE=INTEGER COUNT=(0:1) DEFAULT=1
PARM PASS TYPE=STRING  COUNT=(0:1) DEFAULT=--
PARM EXCISE TYPE=STRING COUNT=(0:10) DEFAULT=--
PARM COMMENT TYPE=STRING COUNT=(0:1) VALID=(COMMENT,NOCOMMENT) 	DEFAULT=COMMENT
PARM HISTORY TYPE=STRING COUNT=(0:1) VALID=(HISTORY,NOHISTORY) DEFAULT=HISTORY
PARM NULL TYPE=STRING COUNT=(0:1) VALID=(NULL,NONULL) DEFAULT=NULL
PARM TABLIST TYPE=STRING COUNT=(0:1) VALID=(LIST,NOLIST) DEFAULT=NOLIST
END-PROC
.TITLE
FITSIN  --  log FITS format data.
.HELP
PURPOSE:
       
   FITSIN is a VICAR*2 Applications program to convert FITS formatted
Astronomical data into VICAR*2 formatted image files. It is based on 
documentation by Don Wells of NRAO.

OPERATION:

	Flexible Image Transport System (FITS) tapes are a general interchange
data format used by radio and optical astronomy observatories for passing
multidimensional data.  In the original definition the tape format consisted
of fixed length records of 2880 bytes with at least one header block of ASCII
information that preceeds the binary (data) information. The fixed length
record of 2880 bytes was chosen because it was divisible by the word lengths
of the most common computer systems of the late 1970's. However, since the
packing density of 1/2-inch tape was pretty poor when newer tape drives
became available in the 1980's the standard was revised to allow up to 10
logical 2880 byte records to be packed into up to 28800 bytes in a physical
block. The FITS keyword BLOCKED = T was added to denote that perhaps the tape
was blocked at greater than 2880 bytes. Note that it does not demand that
the tape be blocked only that it might be. 

	The data records from the instrument are packed into these
fixed length records after the FITS header.

	FITSIN reads the FITS header block of 2880 bytes and extracts
information from it. The entire header block is printed out on the log. All
or portions of the FITS ASCII header can be passed to the Vicar-2 history
label. They will be stored as comments inside the VICAR header by using the
keyword "VFnnn=" where nnn are three digits starting with 001. A maximum
of 999 label entries are allowed.

	FITS tapes may or may not contain ANSI standard labels.  If you 
process an ANSI FITS tape as an unlabeled tape then the real FITS images
files numbered 1, 2, 3, 4,... will be file numbers 2, 5, 8, 11,... on the
tape. File numbers 1, 3, 4, 6, 7, etc will be ANSI header records.


FITS HEADER 

	Each header block consists of 36 80-byte records (card images).
The first 10 columns of each card image contains an 8-character keyword
followed by a blank and an equals sign. Columns 11 through 30 contain a
value or string in ASCII characters which is right-justified. Columns 31
through 80 are a comment field. The first character of a comment field
(column 31) is a slash, /.

	The first FITS header record at a minimum consists of the following
information:
	The first card contains "SIMPLE    =" in columns 1 through 10  
followed by a logical "T" or "F" in column 30. "T" indicates that the
tape format follows the basic rules of FITS format. "F" implies that
it does not.
	The second card contains "BITPIX    =" which gives the number of bits
per pixel in each data record.  The valid values are 8, 16, 32, -32 and -64.
All axes of data must have the same number of bits per pixel. Note that
integer data types 8, 16 and 32 are byte-reversed from the VAX and that 
BITPIX=-32 is IEEE floating point format, and -64 is IEEE double precision
floating point formate, not VAX floating and double, respectively.
	The third card contains "NAXIS     =" which gives the number of
dimensions (axes) of the data.  FITSIN currently only processes three
dimensions or less.  When a sample tape of four or more dimensions is
received then it should be possible to convert the data to IBIS tabular
files although the current program does not do this.
	The fourth and subsequent cards (up to the number of dimensions)
contain "NAXIS1    =", "NAXIS2     =", "NAXIS3    =",...,"NAXISn    ="
which define the number of pixels along each dimension, n.
	Any number of optional header cards may follow.
	The last card is always the "END" card.

For example, the simplest FITS header sequence would look like:

	'         111111111122222222223'  <--card column number
	'123456789012345678901234567890'	
	'SIMPLE   =               T'		!LOGICAL
	'BITPIX   =              16'		!INTEGER
	'NAXIS    =               2'		!INTEGER
	'NAXIS1   =             512'		!INTEGER
	'NAXIS2   =             512'		!INTEGER
	'END                       '

	FITS allows many optional header cards.  These optional header cards
vary from one sensor to another and one observatory to another. Some of 
the more important ones, as far as FITSIN is concerned, are BZERO, BSCALE,
BUNIT, OBJECT, DATE, DATAMAX, DATAMIN, COMMENT, HISTORY, and BLANK. For
example, the following FITS keywords are alwasy passed to the VICAR label:
	SIMPLE
	BITPIX
	NAXIS
	NAXISm	(where m is an integer)
	END

	
The following FITS keywords are always passed to the VICAR label if found:
	BZERO
	BSCALE
	BUNIT
	OBJECT
	DATE
	EXTEND
	XTENSION 
	TABNAME 
	BLANK
	DATAMAX 
	DATAMIN 
	EXTNAME 
	EXTVER 
	EXTLEVEL 
	TFIELDS 

An example of a more complete FITS header is one from the 200 inch
telescope at Palomar.

SIMPLE  =                    T /                                                
BITPIX  =                   16 /                                                
NAXIS   =                    2 /                                                
NAXIS1  =                  800 /                                                
NAXIS2  =                   60 /                                                
BSCALE  =         8.175754E-01 /                                                
BZERO   =         2.538707E+04 /                                                
OBJECT  = 'L637              ' /                                                
FRAME   =                  233 /                                                
NIGHT   =                    2 /                                                
DATE    = '27-APR-1989       ' /                                                
TELESCOP=                  200 /                                                
PORT    =                    1 /                                                
DEWAR   =                    8 /                                                
CHIP    = 'TI432             ' /                                                
ETIME   =                  300 /                                                
TIME    =                  300 /                                                
DECS    = '+                 ' /                                                
EPOCH   =                 1950 /                                                
HAS     = '-                 ' /                                                
SECZ    =         1.109078E+00 /                                                
TEMP    =        -1.147000E+02 /                                                
ERASE   =         1.228969E+03 /                                                
FILTER  = 'NONE              ' /                                                
SATURATE=                    0 /                                                
GRATING =                  600 /                                                
SLIT    = '2.0S              ' /                                                
CRVAL1  =  5.9551000976563E+03 / Angstroms                                      
CDELT1  =  1.1090113863032E+00 /                                                
CRPIX1  =         1.000000E+00 /                                                
CTYPE1  = 'WAVELENGTH        ' /                                                
CRVAL2  =  1.0000000000000E+00 /                                                
CDELT2  =  1.0000000000000E+00 /                                                
CRPIX2  =         1.000000E+00 /                                                
CTYPE2  = 'COLUMN #          ' /                                                
END                                                                             

CONVERSION OF FITS HEADERS INTO VICAR LABELS

	FITS header records are converted into VICAR label items using
the VFxxx keyword. Up to 999 labels can be transferred.  For example,
FITS keyword   

CRVAL1  =  5.9551000976563E+03 / Angstroms                                      

will transfer as

VF023='CRVAL1  =  5.9551000976563E+03 / Angstroms                   '


Many of the FITS labels have strings in them. By convention a string
begins with a single quote (') and ends with another quote.  Transfer
of single quotes to VICAR labels cause problems so single quotes are
converted into double quotes (") into VICAR labels.  For example,
FITS keyword

OBJECT  = 'L637              ' /                                                

This would be converted into the VICAR label item

VF003='OBJECT  = "L637              " /                                '       



FITS DATA

	Data on fits tape are packed into 2880 byte fixed length records.
The label items BITPIX, NAXIS, NAXIS1, NAXIS2, ..., NAXISn describe how
the data is packed. BITPIX which can only take on the values 8, 16, 32,
-32 and -64 tell how many pits there are per pixel.  NAXIS tells how many
dimensions the data has. FITSIN in its current implementation can
process only values of 1, 2 and 3. NAXIS1, NAXIS2 and NAXIS3 give
the number of pixels per axis (sample, line, band) in VICAR nomenclature.
It is common to use BITPIX of 8 and NAXIS=0 for FITS tabular data.

PARAMETERS

	The OUT=filename parameter is not required.  If not given then
only the header of the INP= file is scanned and printed out to the terminal
and/or session log.  The scan will also state the number of FITS keywords
found and the number. Up to 10 output files can be given for multiple
FITS data sets per FITS file.

	The HEADER= parameter is not required. It saves each FITS label 
in a file which has 80 byte fixed length records. No FITS labels are 
created or omitted in this file and no other parameter controls the
contents. The HEADER file does not have VICAR labels.  Up to 10 HEADER
files are allowed for multiple data sets per FITS file.

	The parameter DATA="FITS"/"TRUE"/"U16" refers to the data format to
pass to the VICAR image. The default is FITS tape format which means that 
it will pass to the output image the same data format that is stored in the FITS
image. If the FITS data is 8 bit then the output will be VICAR format
'BYTE', if 16 bit then the output will be VICAR format 'HALF', etc.
In this mode it will thus ignore any BSCALE and BZERO FITS labels if
found in the FITS label. Consequently, if the original experimenter
normalized the data using BSCALE and BZERO to pack the data in 8 or 16 bit
format the VICAR image will retain this packing.  For some applications
this is entirely acceptable, for others it is not.

	If DATA="TRUE" then FITSIN will transform the FITS data into a VICAR
'REAL'format data set regardless of the FITS data values. It will then use the
FITS BSCALE and BZERO values stored in the FITS label to convert the data
(whatever the format, 8 bit, 16 bit or 32 bit integers or 32-bit IEEE
FITS floating point formats) into VAX 'REAL' format by the formula

			REAL = (FITS*BSCALE)+BZERO
where,

REAL is the R*4 output value in the VICAR image,
FITS is the FITS stored data value in 8 bit, 16 bit, 32 bit or -32 bit format,
BSCALE is the value stored in the FITS BSCALE header record,
BZERO is the value stored in the FITS BZERO header record.
					
If no BSCALE or BZERO values are found in the label when you give this option
then FITSIN will warn you and then use default values of BSCALE=1.0 and
BZERO=0.0 to do the conversion. Thus, use of the "DATA=TRUE" option will
force the output to be VICAR 'REAL' no matter what format the FITS data is in.

	FITS data assumes that 8-bit and 16-bit data are unsigned. VICAR
uses unsigned values for 8-bit data but signed for 16-bit data. Consequently,
there will be problems for displaying data that go from 65535 to 0 in FITS.
It will show up as -32768 to +32767 in VICAR. To get around this you have to
force 16-bit data into a 32-bit integer. Use DATA="U16" to do this.

	The BLOCK= parameter is for reading of blocked FITS tapes. Valid entries
are "BLOCK" or "NOBLOCK".  If "BLOCK" is entered then enter a value of from one
to 10 for "FACTOR".  

Other parameters are geared toward limiting the FITS header keywords from
being passed to the VICAR label since only 999 are allowed. The options for
selecting which FITS labels to pass to the VICAR label are the following:

	PASS="FITS KEYWORD" - Indicates at which FITS keyword to begin passing
	the FITS label records to VICAR label records.  By default, FITS 
	keywords starting after FITS keyword "NAXIS3 =" are passed or
	"NAXIS =" if NAXIS = 0 (in the case of tables).
	
	EXCISE="FITS KEYWORD" - Indicates which FITS keywords not to pass to
	the VICAR label records.  Up to 10 keywords may be EXCISEd. By default
	no FITS keywords are excised.
	
	COMMENT="COMMENT"/"NOCOMMENT" - Pass/Don't Pass FITS "COMMENT =" records
	to the VICAR label.  By default, FITS "COMMENT =" keywords are passed.
	
	HISTORY="HISTORY"/"NOHISTORY" - Pass/Don't Pass FITS "HISTORY =" records
	to the VICAR label.  By default, FITS "HISTORY =" keywords are passed.
	
	NULL="NULL"/"NONULL" - Pass/Don't Pass FITS blank (null) records to
	the VICAR label.  By default, null keyword records are passed.

Note that by specifying PASS and EXCISE keywords along with COMMENT="NOCOMMENT",
HISTORY="NOHISTORY", and/or NULL="NONULL" that FITS "COMMENT=", "HISTORY=" and
null records will not be passed to the VICAR label. This technique is normally
used after a tape scan which detects pages of FITS label information which is
of little use to the VICAR user.

	TABLIST="LIST"/"NOLIST" - List FITS tables to screen


FITS files can transfer text data. This is usually done by the following:
SIMPLE  =                    T / Standard FITS format
BITPIX  =                    8 / Character Information
NAXIS   =                    0 / No image data array present
EXTEND  =                    T / Extension exists
 .
 .
 .
END
XTENSION= 'TABLE   '           / Table Extension
BITPIX  =                    8 / Character Information
NAXIS   =                    2 / Two-dimensional table
NAXIS1  =                   80 / Number of characters per line
NAXIS2  =                  560 / Number of rows
PCOUNT  =                    0 / No Random Parameters
GCOUNT  =                    1 / Only one group
TFIELDS =                    1 / One field per row

EXTNAME = 'REV_1_1 '           / Generic Comments
EXTVER  =                    1 / Integer Version Number

TTYPE1  = 'TEXT    '           / Free text
TBCOL1  =                    1 / Start in column 1
TFORM1  = 'A80     '           / 80 Character Field

END

You can list such text files to screen by using the TABLIST=LIST option


LIMITATIONS

	1. The program has not been implemented for multi-dimensional files
	greater than 3 dimensions. (NAXIS < 4)
	2. Up to 999 FITS keywords can be passed to the VICAR label.
	3. FITS "Blocked" tape formats are not supported although it will copy
	data as best it can.
	4. Changes ' (single quote) in FITS labels to " (double quotes)
	in VICAR labels
	5. Cannot process FITS 'BINTABLE' or '3DTABLE' files
	6. Allows up to 10 FITS data sets to be embedded in one FITS file
	7. Program has been run on a variety of FITS tapes as well as disk
	data sets but non were in BLOCKED format.
	8. Need yet to create a TEXT output file

PROGRAM HISTORY:

      12 MAY 2003   LWKamp     Added check for incomplete last block in FITS file;
				fixed chkstat calls (now require all arguments)
      14 NOV 2002...A.C.Chen   ...Converted FITS keywords from "PASS" and 
                                  "EXCISE" parameters to upper case so FITS
                                  label keywords can be passed or excluded
                                  correctly. Modified test pdf.  
       2 MAR 1995...R.J.Bambery...Fixed text file printing to screen
							(was truncated to 45 characters)
	 9 NOV 1994...R.J.Bambery...Delivered to Gloria Conner
	14 AUG 1994...R.J.Bambery...Added U16 parameter to pass unsigned 16-bit
				    data to VICAR fullword images
	11 AUG 1994...R.J.Bambery...Added ability to process FITS -64 data
	23 MAR 1994...R.J.Bambery...Added ability to process up to 10
				    FITS data sets per FITS file
	18 MAR 1994...R.J.Bambery...Fixed a "PASS=" parameter bug
				    Changed default NULL=NULL from
				    NULL=NONULL
	 4 DEC 1993...R.J.Bambery...Incorporation of IBIS-2 tabular
				    file routines
	26 JUN 1993...R.J.Bambery...Allow for listing of FITS table files
	23 JUN 1993...R.J.Bambery...Fixed bugs in FITSCHK (nproc to noproc)
	10 MAY 1993...R.J.Bambery...Made portable to UNIX
				    Removed IEEE specific floating pt code
				    because of new VICAR executive
	10 APR 1991...R.J.BAMBERY...Added blocking factor and bands
	18 MAR 1991...R.J.BAMBERY...Fixed bugs and prevent ASCII dump of
					entire file when invalid header found
	20 FEB 1991...R.J.BAMBERY...ADDED IEEE FLOATING POINT CODE
	28 SEP 1990...R.J.BAMBERY...ADDED HEADER OPTION
	17 SEP 1990...R.J.BAMBERY...EXPANDED FITS LABELS TO 999 FROM 99
        25 AUG 1990...R.J.BAMBERY...EXPANDED OPTIONS,
				    MULTIPLE FITS HEADER RECORDS
	30 JUN 1987...G.W.GARNEAU...Updated and expanded
	12 DEC 1984...M.E.MORRILL...VAX/VICAR-2 Conversion
	17 APR 1982...M.E.MORRILL...IBM Version, initial release.

EXAMPLES:

       FITSIN INP=FITS.DAT  PARMS   (for scanning headers)
                   --or--
       FITSIN INP=FITS.DAT  OUT=IMAGE.DAT PARAMS (for converting data)

       FITSIN INP=FITS.DAT  HEADER=FITS.HDR (for saving FITS labels)
		  --or--
       FITSIN INP=FITS.DAT  OUT=IMAGE.DAT HEADER=FITS.HDR PARAMS (for
				 saving FITS labels and converting data)
      
Parameters are defined above and in the TUTOR mode. SIZE field is computed
from FITS information.

REFERENCE

Donald C. Wells, "FITS: A Flexible Image Transport System", National Optical
Astronomy Observatories (NOAO) publication, 1979

Flexible Image Transport System (FITS) Draft Standard, NSDSSO 100-1.0,
June, 1993. Obtainable from NSSDC.

A User's Guide for the Flexible Image Transport System (FITS), Version 3.1,
May 2, 1994. Obtainable from NSSDC.

Help on FITS is available through NASA/Office of Standards and Technology,
Greenbelt MD.
anonymous ftp: nssdca.gsfc.nasa.gov
WWW: http://fits.cv.nrao.edu

Usenet newsgroup: sci.astro.fits

.LEVEL1
.VARIABLE INP
 STRING
 A FITS tape file number or
 disk file.
.VARIABLE OUT
 STRING--OPTIONAL
 A Vicar formated output
 image filename.
 (Up to 10 embedded data
 sets can be converted 
 into separate VICAR files)
.VARIABLE HEADER
 STRING-OPTIONAL
 A file name for outputting
 FITS labels
 (Up to 10 embedded data
 sets FITS labels can be
 listed into separate
 VICAR files)
.VARIABLE DATA
 STRING-OPTIONAL
 Data format to pass to VICAR
 image
 FITS/TRUE/U16
 DEFAULT="FITS"
.VARIABLE BLOCK
 STRING-OPTIONAL
 FITS image is blocked/not
 blocked,
 (NOT supported yet)
 BLOCK/NOBLOCK
 DEFAULT=BLOCK
.VARIABLE FACTOR
 INTEGER-OPTIONAL
 Blocking factor when 
 BLOCK=BLOCK is selected.
 Must be integer.
 DEFAULT=1
.VARIABLE PASS
 STRING-OPTIONAL
 Pass to VICAR history label,
 FITS label records beginning
 with PASS="FITS KEYWORD".
 DEFAULT=-- (All FITS labels)
.VARIABLE EXCISE
 STRING-OPTIONAL
 Do not pass to VICAR history
 label FITS label records
 beginning with
 EXCISE="FITS KEYWORD".
 DEFAULT="        " (null)
.VARIABLE COMMENT
 STRING-OPTIONAL
 Pass/Don't Pass FITS
 "COMMENT =" labels
 COMMENT/NOCOMMENT
 DEFAULT=COMMENT
.VARIABLE HISTORY
 STRING-OPTIONAL
 Pass/Don't Pass FITS
 "HISTORY =" labels
 HISTORY/NOHISTORY
 DEFAULT=HISTORY
.VARIABLE NULL
 STRING-OPTIONAL
 Pass/Don't Pass FITS
 "           " (null) labels
 NULL/NONULL
 DEFAULT=NONULL
.VARIABLE TABLIST
 STRING-OPTIONAL
 List/Nolist of FITS tables
 to screen
 DEFAULT=NOLIST
.LEVEL2
.VARIABLE INP
 A FITS formatted data file on tape or disk.
.VARIABLE OUT
 A Vicar formatted output image filename. Output can be 'BYTE', 'HALF', 'FULL'
 or 'REAL' depending on FITS format or the FITSIN parameter "DATA="
.VARIABLE HEADER
 An output file containing FITS label information in ASCII 80 byte fixed
 length records.
.VARIABLE DATA
 STRING-OPTIONAL
 Data format to pass to VICAR image. Default is "FITS" tape format which
 can be 'BYTE', 'HALF', 'FULL'  or 'REAL'.
 
 If "TRUE" then will pass R*4 format data to VICAR using FITS parameters
 BSCALE and BZERO keywords in FITS labels based on the formula 

	"TRUE" = (FITS*BSCALE)+BZERO.
 
 If DATA is set to "TRUE" and no BSCALE or BZERO values are not found then
 the program will warn you, but will create an output data set of type
 real*4 by using the default settings of BSCALE=1.0 and BZERO=0.0.
 
 When DATA=TRUE output file will always be 'REAL'.
 Use U16 to transform unsigned FITS data into unsigned VICAR FULL data since
 VICAR uses 16-bit signed data.
 FITS/TRUE/U16
 DEFAULT="FITS"
.VARIABLE BLOCK
 STRING-OPTIONAL
 FITS image tape is blocked/not blocked,
 BLOCK/NOBLOCK
 DEFAULT=BLOCK
.VARIABLE FACTOR
 INTEGER-OPTIONAL
 Blocking factor when BLOCK=BLOCK is selected. Maximum permitted value is 10.
 Must be integer. 
 Find Blocking factor from MIPL TAPES utility.
 DEFAULT=1
.VARIABLE PASS
 STRING-OPTIONAL
 Pass to VICAR history label the FITS label records beginning
 with PASS="FITS KEYWORD". Normally this is done after a tape scan.
 
 "BSCALE =", "BZERO =", "BUNIT =", "OBJECT =", "COMMENT =" and "DATE ="
 keywords will always be passed to the VICAR label even if found before the
 FITS keyword you choose for PASS=.
 
 DEFAULT=-- (All FITS labels)
.VARIABLE EXCISE
 STRING-OPTIONAL
 Don't pass to VICAR history label the FITS label records beginning
 with EXCISE="FITS KEYWORD". This gives you the option of eliminating other
 less helpful FITS  keywords. Normally this is done after a tape scan.
 Up to 10 keywords may be excised in addition to those specified with the
 COMMENT, HISTORY, and NULL keywords.
.VARIABLE COMMENT
 STRING-OPTIONAL
 Pass/Don't Pass FITS "COMMENT =" labels
 COMMENT/NOCOMMENT
 DEFAULT=COMMENT
.VARIABLE HISTORY
 STRING-OPTIONAL
 Pass/Don't Pass FITS "HISTORY =" labels
 HISTORY/NOHISTORY
 DEFAULT=HISTORY
.VARIABLE NULL
 STRING-OPTIONAL
 Pass/Don't Pass FITS "           "(null) labels
 NULL/NONULL
 DEFAULT=NONULL
.VARIABLE TABLIST
 STRING-OPTIONAL
 List/Nolist of FITS tables to screen
 DEFAULT=NOLIST
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfitsin.pdf
procedure
refgbl $echo
refgbl $syschar
body

local path string

IF ($syschar(1)="VAX_VMS")
   let path="wms_test_work:[testdata.general]" 
ELSE
   let path="/project/test_work/testdata/gll/"
END-IF

let _onfail="continue"
write "system = &$syschar(1)"
let $echo=("yes","no")
! New exhaustive test of FITSIN - Nov 8, 1994
!
! Uses files l637b.fits,p1.fits,pcf230w_a.fits
! 		1980ly1.fits,iras_12b1.fits
!		af784.fits,neat668.fits,0001.gsc
!
! does not test parameters "BLOCK" or "FACTOR" since data not on tape
! (They have no meaning for disk files)
!

!fitsin inp=c3d_961228_151603.fts out=a.img
!xvd a.img

! --> test listing of fits header only
fitsin &"path"1980ly1.fits

! --> translate 16-bit data to HALF vicar
fitsin &"path"1980ly1.fits &"path"1980ly1.vic
label-list &"path"1980ly1.vic
list &"path"1980ly1.vic sl=1 ss=1 nl=10 ns=10
! --> translate 16-bit data to HALF vicar
fitsin &"path"1980ly1.fits &"path"1980ly1.vic
label-list &"path"1980ly1.vic
list &"path"1980ly1.vic sl=1 ss=1 nl=10 ns=10

! --> translate 16-bit data to HALF vicar with embedded BSCALE
fitsin &"path"l637.fits &"path"l637.vic
label-list &"path"l637.vic
list &"path"l637.vic sl=1 ss=1 nl=10 ns=10

! --> pass label information from telescope onward
fitsin &"path"l637.fits &"path"l637.vic pass="telescop"
label-list &"path"l637.vic

! --> do not pass following label entries
fitsin &"path"l637.fits &"path"l637.vic excise=("telescop","chip","has","secz",+
    "port","dewar","bscale","bzero")
label-list &"path"l637.vic

! --> translate BITPIX=16 with BSCALE parameter to REAL vicar
fitsin &"path"l637.fits &"path"l637.vic data="true"
label-list &"path"l637.vic
list &"path"l637.vic sl=1 ss=1 nl=10 ns=10

! --> create header file only
fitsin &"path"l637.fits header=l637.hdr
if ($syschar(1)="VAX_VMS") 
	dcl type &"path"l637.hdr
else
	ush cat &"path"l637.hdr
end-if

! --> translate file and create header file
fitsin &"path"l637.fits &"path"l637.vic header=&"path"l637.hdr
if ($syschar(1)="VAX_VMS") 
	dcl type &"path"l637.hdr
else
	ush cat &"path"l637.hdr
end-if
list &"path"l637.vic sl=1 ss=1 nl=10 ns=10

! --> do not pass history= and comment= labels
fitsin &"path"l637.fits &"path"l637.vic history=nohistory comment=nocomment 
label-list &"path"l637.vic

! --> list header of a file with a very long fits header
fitsin &"path"pcf230w_a.fits

! --> create fits header file of a FITS image with a long fits header
fitsin &"path"pcf230w_a.fits header=&"path"pcf230w_a.hdr
if ($syschar(1)="VAX_VMS") 
	dcl type &"path"pcf230w_a.hdr
else
	ush cat &"path"pcf230w_a.hdr
end-if

! --> translate BITPIX=16 fits file with a long header -- gets slower
fitsin &"path"pcf230w_a.fits &"path"pcf230w_a.vic
label-list &"path"pcf230w_a.vic 
list &"path"pcf230w_a.vic sl=1 ss=1 nl=10 ns=10

! --> list FITS header for BITPIX=-32
fitsin &"path"p1.fits

! --> create FITS header file for BITPIX=-32 data
fitsin &"path"p1.fits header=&"path"p1.hdr
if ($syschar(1)="VAX_VMS") 
	dcl type &"path"p1.hdr
else
	ush cat &"path"p1.hdr
end-if

! --> translate BITPIX=-32 (IEEE real) to REAL vicar
fitsin &"path"p1.fits &"path"p1.vic 
label-list &"path"p1.vic
list &"path"p1.vic sl=1 ss=1 nl=6 ns=6

! --> translate BITPIX=-64 (IEEE double) to DOUB vicar
fitsin &"path"af784.fits  &"path"af784.vic 
label-li &"path"af784.vic
list &"path"af784.vic sl=1 ss=1 nl=5 ns=5

! --> translate BITPIX=16 to HALF vicar
fitsin &"path"iras_12b1.fits &"path"iras_12b1.vic
label-li &"path"iras_12b1.vic
list &"path"iras_12b1.vic sl=293 ss=480 nl=6 ns=6

! --> translate BITPIX=16 to HALF vicar
fitsin &"path"iras_12b1.fits &"path"iras_12b1.vic data=true
label-li &"path"iras_12b1.vic
list &"path"iras_12b1.vic sl=293 ss=480 nl=6 ns=6

! --> translate BITPIX=16 to FULL vicar since data is unsigned 16-bit data
fitsin &"path"neat668.fits &"path"neat668.vic data=u16
label-li &"path"neat668.vic 
list &"path"neat668.vic sl=100 ss=100 nl=6 ns=6

! --> create ibis-2 tabular file from FITS table 
!fitsin 0001.gsc 0001.ibis
!label-li 0001.ibis
!ibis-list 0001.ibis nr=15 nc=20 units=units groups=groups formats=formats +
!	screen=120

! --> create ibis-2 tabular file from FITS table with listing 
!fitsin 0001.gsc 0001.ibis tablist=list 
!label-li 0001.ibis
!ibis-list 0001.ibis nr=15 nc=20 units=units groups=groups formats=formats +
!	screen=120

let $echo="no"
end-proc
$ Return
$!#############################################################################
