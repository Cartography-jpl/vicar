
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

