$!****************************************************************************
$!
$! Build proc for MIPL module plotit
$! VPACK Version 1.9, Friday, July 27, 2001, 11:05:55
$!
$! Execute by entering:		$ @plotit
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
$ write sys$output "*** module plotit ***"
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
$ write sys$output "Invalid argument given to plotit.com file -- ", primary
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
$   if F$SEARCH("plotit.imake") .nes. ""
$   then
$      vimake plotit
$      purge plotit.bld
$   else
$      if F$SEARCH("plotit.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake plotit
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @plotit.bld "STD"
$   else
$      @plotit.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create plotit.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack plotit.com -mixed -
	-s plotit.f -
	-i plotit.imake -
	-p plotit.pdf -
	-t tstplotit.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create plotit.f
$ DECK/DOLLARS="$ VOKAGLEVE"

c program plotit

      include 'VICMAIN_FOR'
      subroutine main44
      parameter (maxsize=1024, maxdata=100)
      integer*4 status,count,pgbeg,def,pgcurs
      real*4 buf(maxsize*maxsize),record(maxsize*8)
      character*20 device
      character*80 xlabel,ylabel,title
      character*1 key
      character*30 msg/'                              '/ 
      real*4 spliney(maxdata),spliney2(maxdata)
      real*4 splinex(maxdata),xbox(4),ybox(4)                               
      real*4 xdata(maxdata),ydata(maxdata),tr(6)
      logical logx,logy,xvptst,dot,lines,splines
      
c get number of input files.
      call xvpcnt('INP',nids)

c open output device
      call xvparm('DEVICE',device,count,def,1)
      IER = PGBEG(0,device,1,1)
      IF (IER.NE.1) then
        call xvmessage('PGBEG: cannot start PGPLOT',' ')
        call abend()
      endif

c open input
      if(nids.gt.0)then
        call xvunit(inunit,'INP',1,status,' ')
        call xvsignal(inunit,status,1)
        call xvopen(inunit,status,'U_FORMAT','REAL',' ')
        call xvsignal(inunit,status,1)
        call xvget(inunit,status,'NL',nl,'NS',ns,' ')
        call xvsignal(inunit,status,1)
        inc=max((nl+1)/maxsize+1,(ns+1)/maxsize+1)
        if(inc.gt.1)write(*,*)'Image compressed by ',inc
        k=0
        nlnew=0
        nsnew=0
        do line=1,nl,inc
          nlnew=nlnew+1
          call xvread(inunit,record,status,'LINE',line,' ')
          call xvsignal(inunit,status,1)
          do i=1,ns,inc
            k=k+1
            buf(k)=record(i)
          enddo
        enddo
        do i=1,ns,inc
            nsnew=nsnew+1
        enddo
        call xvpone('INP',title,1,80)

c compute data ranges
        xmin_plot=1.
        xmax_plot=ns
        ymin_plot=nl
        ymax_plot=1.
        CALL PGENV(xmin_plot,xmax_plot,ymin_plot,ymax_plot,1,-1)
        call pgbox('BCINST',0.,0,'BCINST',0.,0)
        call pglab('sample','line',title)
        tr(1)=0.
        tr(2)=inc
        tr(3)=0.
        tr(4)=0.
        tr(5)=0.
        tr(6)=inc
        call xvparm('PERCENT',percent,count,def,1)
        call datarange(buf,nsnew*nlnew,dnmin,dnmax,percent)
        write(*,*)'stretched from ',dnmin,' to ',dnmax

c draw image
        call pggray(buf,nsnew,nlnew,1,nsnew,1,nlnew,dnmax,dnmin,tr)
        call pgwedg('RG',1.,3.,dnmax,dnmin,'dn')

c draw "exit" box
        call pgqcs(4,xheight,yheight)
        call pgsci(2)
        call pgptxt(0.0,nl-yheight*2.0,0.0,0.0,'EXIT')
        call pgqtxt(0.0,nl-yheight*2.0,0.0,0.0,'EXIT',xbox,ybox)
c        call pgsci(3)
c        call pgsfs(2)
c        call pgrect(xbox(1),xbox(3),ybox(3),ybox(1))
c        call pgpoly(4,xbox,ybox)

c read cursor
        write(*,*)'To exit click on EXIT'
        write(*,*)'Click to see image coordinates'
        x=nsnew/2.
        y=nlnew/2.
90      status=pgcurs(x,y,key)
        if(key.eq.'e')goto 100
        if((x.gt.xbox(1)).and.(x.lt.xbox(3)).and.
     +     (y.gt.ybox(3)).and.(y.lt.ybox(1)))goto 100
        call pgsci(0)
        call pgmtxt('T',1.,0.,0.,msg)
        write(msg,98)y,x
98      format(' line=',f6.1,' sample=',f6.1)
        call pgsci(1)
        call pgmtxt('T',1.,0.,0.,msg)
        goto 90
100     call pgend

        return
      endif
     
c get plot data
      call xvparm('XDATA',xdata,count,def,maxdata)
      npts=count
      write(*,*)'located ',npts,' data points'
      call xvparm('YDATA',ydata,count,def,maxdata)
      if(npts.lt.1)then
        call xvmessage('No points found',' ')
        call abend()
      endif
      if(npts.ne.count)then
        call xvmessage('Number of data points in X & Y disagree',' ')
        call abend()
      endif
      call xvparm('XLABEL',xlabel,count,def,1)
      call xvparm('YLABEL',ylabel,count,def,1)
      call xvparm('TITLE',title,count,def,1)

c log options
      logx= xvptst('LOGX')
      logy= xvptst('LOGY')
      istyle=1
      if(logx)then
        istyle=10
        do i=1,npts
          xdata(i)=log10(xdata(i))
        enddo
      endif
      if(logy)then
        istyle=20
        do i=1,npts
          ydata(i)=log10(ydata(i))
        enddo
      endif
      if(logx.and.logy)istyle=30

c compute data ranges
      call datarange(ydata,npts,ymin,ymax,0.0)
      call datarange(xdata,npts,xmin,xmax,0.0)
      call pgrnge(xmin,xmax,xmin_plot,xmax_plot)
      call pgrnge(ymin,ymax,ymin_plot,ymax_plot)

c compute splines if requested
      splines=xvptst('SPLINE')
      if(splines)then
        call spline(xdata,ydata,npts,2.0e+30,2.0e+30,spliney2)
        i=0
        do xc=xdata(1),xdata(npts),(xdata(npts)-xdata(1))/maxdata
          i=i+1
          if(i.le.maxdata)then
            call splint(xdata,ydata,spliney2,npts,xc,yc)
            splinex(i)=xc
            spliney(i)=yc
            nspline=i
          endif
        enddo
      endif
 
c plot data 
      dot=xvptst('DOT')
      lines=xvptst('LINE')
      if((.not.dot).and.(.not.lines).and.(.not.splines))then
        dot=.true.
        lines=.true.
      endif  
      CALL PGENV(xmin_plot,xmax_plot,ymin_plot,ymax_plot,0,istyle)
      CALL PGLAB(xlabel, ylabel, title)
      if(dot) CALL PGPT(npts,xdata,ydata,9)
      if(lines) call pgline(npts,xdata,ydata)
      if(splines) call pgline(nspline,splinex,spliney)
      CALL PGEND
 
      return
      end

      subroutine datarange(buf,n,dnmin,dnmax,percent)
      real*4 buf(n)
      integer*4 hist(10000)
      dnmin=buf(1)
      dnmax=buf(1)
      do i=1,n
        if(dnmin.gt.buf(i))dnmin=buf(i)
        if(dnmax.lt.buf(i))dnmax=buf(i)
      enddo
      if(percent.eq.0.0)return
      slope=(10000-1)/(dnmax-dnmin)
      offset=1-slope*dnmin
      do i=1,10000
        hist(i)=0
      enddo
      do i=1,n
        j=nint(buf(i)*slope+offset)
        hist(j)=hist(j)+1
      enddo
      sum=0.
      target=n*percent/100.
      do i=1,10000
        sum=sum+hist(i)
        if(sum.ge.target)then
          dnmin=(i-offset)/slope
          goto 100
        endif
      enddo
100   sum=0.
      do i=10000,1,-1
        sum=sum+hist(i)
        if(sum.ge.target)then
          dnmax=(i-offset)/slope
          goto 200
        endif
      enddo
200   continue
      return
      end

      SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)
      PARAMETER (NMAX=100)
      DIMENSION X(N),Y(N),Y2(N),U(NMAX)
      IF (YP1.GT..99E30) THEN
        Y2(1)=0.
        U(1)=0.
      ELSE
        Y2(1)=-0.5
        U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE
      IF (YPN.GT..99E30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE
      RETURN
      END

      SUBROUTINE SPLINT(XA,YA,Y2A,N,X,Y)
      DIMENSION XA(N),YA(N),Y2A(N)
      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.) PAUSE 'Bad XA input.'
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     *      ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
      RETURN
      END

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create plotit.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM plotit

   To Create the build file give the command:

		$ vimake plotit			(VMS)
   or
		% vimake plotit			(Unix)


************************************************************************/


#define PROGRAM	plotit
#define R2LIB

#define MODULE_LIST plotit.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define R2LIB

#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MOTIF
/*#define LIB_LOCAL */
/*#define LOCAL_LIBRARY /usr/local/pgplot/libpgplot.a */
#define LIB_PGPLOT

/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create plotit.pdf
process help=*
PARM INP TYPE=STRING COUNT=(0:1) DEFAULT=--
PARM DEVICE TYPE=KEYWORD +
 VALID=(/GIF,/VGIF,/PPM,/VPPM,/PS,/VPS,/CPS,/VCPS,/XSERVE,/XMOTIF, +
 /GTERM,/XTERM,/XWINDOW) DEFAULT=/XSERVE
PARM DOT TYPE=KEYWORD VALID=(DOT,NONE1) DEFAULT=NONE1
PARM LINE TYPE=KEYWORD VALID=(LINE,NONE2) DEFAULT=NONE2
PARM SPLINE TYPE=KEYWORD VALID=(SPLINE,NONE3) DEFAULT=NONE3
PARM LOGX TYPE=KEYWORD VALID=(LINEAR,LOGX) DEFAULT=LINEAR
PARM LOGY TYPE=KEYWORD VALID=(LINEA,LOGY) DEFAULT=LINEA
PARM XDATA TYPE=REAL COUNT=(0:100) DEFAULT=--
PARM YDATA TYPE=REAL COUNT=(0:100) DEFAULT=--
PARM XLABEL TYPE=STRING COUNT=(0:1) DEFAULT='x'
PARM YLABEL TYPE=STRING COUNT=(0:1) DEFAULT='y'
PARM TITLE TYPE=STRING COUNT=(0:1) DEFAULT='title'
PARM PERCENT TYPE=REAL COUNT=(0:1) DEFAULT=2.
END-PROC
.TITLE
VICAR2 Program PLOTIT a plotting and image display program.

.HELP
PURPOSE
1. To plot graphs.
2. To display images.

EXECUTION
See http://www-astro-theory.fnal.gov/Documentation/Software/pgplot/
         index.html

*************to plot a graph********************
plotit +
xdata=(0.,2.,4.,6.,8.) ydata=(1.,2.,4.,8.,16.) +
xlabel="x axis" ylabel="y axis" title="a title"

************to display a vicar image *******************
plotit inp=ref.img


.LEVEL1

.VARIABLE INP
 input image
.VARIABLE DEVICE
Output device type.
.VARIABLE XDATA
X data to plot
.VARIABLE YDATA
Y data to plot
.VARIABLE XLABEL
The X axis label
.VARIABLE YLABEL
The Y axis label
.VARIABLE TITLE
Plot title
.VARIABLE LOGX
Log x axis
.VARIABLE LOGY
Log y axis
.VARIABLE DOT
Plot points as symbols.
.VARIABLE LINE
Plot lines connecting
the dots.
.VARIABLE SPLINE
Plot spline interpolated
data between the dots.

.LEVEL2

.VARIABLE INP
 input image to be displayed

.VARIABLE PERCENT
image histogram saturation at each end.

.VARIABLE DEVICE
Device type to plot or display to. Options are:
/GIF,    gif format landscape
/VGIF,   gif format portrait
/PPM,    portable pixel map landscape 
/VPPM,   portable pixel map portrait
/PS,     postscript monochrome landscape
/VPS,    postscript monochrome portrait
/CPS,    postscrip color landscape
/VCPS,   postscrip color portrait
/XTERM,  tektronix terminal
/XWINDOW workstations running x windows
/XSERVE, persistent X window
/XMOTIF  X motif

The default is: device=/XSERVE

.VARIABLE XDATA
X data to plot

.VARIABLE YDATA
Y data to plot

.VARIABLE XLABEL
The X axis label

.VARIABLE YLABEL
The Y axis label

.VARIABLE TITLE
Plot title

.VARIABLE LOGX
Log x axis.

.VARIABLE LOGY
Log y axis.

.VARIABLE DOT
Plot points only as symbols.
If neither DOT, LINE, or SPLINE are specified then DOT and LINE become
the default.

.VARIABLE LINE
Plot lines connecting the dots.
If neither DOT, LINE, or SPLINE are specified then DOT and LINE become
the default.

.VARIABLE SPLINE
Plot spline interpolated data between the dots.
If neither DOT, LINE, or SPLINE are specified then DOT and LINE become
the default.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstplotit.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
! display an image
plotit inp=/project/it/testdata/gll/earth.red
!
! plot data
plotit +
xdata=(1.,2.,4.,6.,8.) ydata=(1.,3.,7.,13.,25.) +
xlabel="x axis" ylabel="y axis" title="a title" +
'logx 'logy
!
end-proc
$ Return
$!#############################################################################
