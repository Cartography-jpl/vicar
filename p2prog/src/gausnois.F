	include 'VICMAIN_FOR'
	subroutine main44

	implicit none
	real*4	buffer(32768)
	real*4	mean,sigma
	real*4	x,y,u
	real*4	ranx,rany
	integer*4	nl,ns
	integer*4	cnt,status
	integer*4	line,samp
	integer*4	unit,seed
	character*8 format

	call xvp('NL',nl,cnt)
	call xvp('NS',ns,cnt)
	call xvp('FORMAT',format,cnt)

	call xvunit(unit,'OUT',1,status,' ')
	call xvopen(UNIT,STATUS, 'IO_ACT','SA','OPEN_ACT','SA',
     +		'OP','WRITE',  'U_FORMAT','REAL', 'O_FORMAT',format,
     +		'U_NL',NL, 'U_NS',NS,' ')

	call xvp('MEAN',mean,cnt)
	call xvp('SIGMA',sigma,cnt)
	call xvp('SEED',seed,cnt)
	if (cnt .eq. 0)  then
		call get_seconds( seed)
	end if

	do line = 1,nl
	    do samp = 1,ns
		call rangen(seed,ranx)
		call rangen(seed,rany)
		x = max( ranx, 1.0e-10)
		y = rany
		u = sqrt(-2.*log(x)) * cos(2*3.1415927*y)
		buffer(samp) = sigma*u + mean
	    enddo
	    call xvwrit(unit,buffer,status,' ')
	enddo

	call xvclose(unit,status,' ')

	return
	end
