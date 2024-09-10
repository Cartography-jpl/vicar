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

