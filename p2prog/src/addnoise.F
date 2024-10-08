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
