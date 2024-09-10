      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C---- VICAR PROGRAM "PLOTINT".
C     PURPOSE: TO PLOT  DATA CONTAINED IN  COLUMNS OF AN
C               IBIS INTERFACE FILE.
C
C     PROGRAMMER: BORIS GOKHMAN, JULY 1981.
C
C	MODIFIED:   FRANK EVANS 	NOVEMBER 1985  
C			FOR F77 CALCOMP, GENERAL MODERNIZATION
C
C       MODIFIED:   EJB    SEPT 87 FOR MULTIPLE PLOTS AS PER
C                          CONTROL COLUMN ON SPECIFIED OUTPUT DEVICES
C
C       MODIFIED:   BAM   MSTP PORTING  12/95
C
C	MODIFIED:   PXA   JAN 97 CONVERTED CALCOMP CALLS TO XRTPS
C			  CALLS AND REWROTE PLOT PROCEDURES

c       aug2006  -lwk-  fixed bug (Linux only) when YCOLSTR defaulted;
c			added NODISP and PLOTOUT parameters to support
c			output to file instead of display (no code needed
c			for NODISP as it is parsed by xrtps routines)


      IMPLICIT INTEGER (A-Z)
      INTEGER	NPOINT(400), NSTART(400), NCOLY(20),nystr
      INTEGER	INTEQ(20),LINTYP(20),SYMTYP(20)
      INTEGER	COUNT, FREQ,DEF,RUNIT,ibis_in,
     *	 	XRANDF,YRANDF, SLENGTH
      REAL	COLX(2000),COLY(2000,20),CONTRL(100000)
      REAL	COLMIN, COLMAX
      REAL	XRAN(2),YRAN(2), XRANG1,XRANG2, YRANG1,YRANG2
      REAL	FIRSTX,DELTAX, FIRSTY,DELTAY
      REAL	HEIGHT, Y, XLEN,YLEN,YMIN,YMAX,XMIN,XMAX
      REAL	ARRMIN,ARRMAX
      INTEGER	YSTRPTR(20), YSTRLEN(20)
      INTEGER   status
      BYTE	YSTRTMP(1200)
      CHARACTER*60  TITLE, XLABEL, YLABEL,label(1),plotout
      CHARACTER*20 YCOLSTR(20)
      REAL	SCALER(4),SCAX(4)
      EQUIVALENCE(SCALER(1),YMIN),(SCALER(2),YMAX),
     *           (SCALER(3),FIRSTY),(SCALER(4),DELTAY)
      EQUIVALENCE(SCAX(1),XMIN),(SCAX(2),XMAX),
     *           (SCAX(3),FIRSTX),(SCAX(4),DELTAX)
C& MOIFIED EJB SEPT 87 ************************
      CHARACTER*7 DEVICE

C&
C& GET THE VALUE OF DEVICE OR SET DEFAULT
C&   
        CALL XVP('DEVICE',DEVICE,COUNT)
C& ********************************************
C
      call xvp('PLOTOUT',plotout,count)
      call plotfn(plotout)

C---- READ PARAMETERS 
C
C---- GET THE NUMBERS OF THE X-COL AND THE Y-COL AND CONTROL COL
C
      CALL XVP('YCOL',NCOLY,MYCOL)
      CALL XVP('XCOL',NCOLX,COUNT)
C
      CALL XVP('CONTROL',NCONTR,COUNT)
C--  GET THE SIZE OF THE PLOT.
      CALL XVP('XLEN',XLEN,COUNT)
      CALL XVP('YLEN',YLEN,COUNT)
      IF(XLEN .EQ. 0) THEN
	CALL MABEND('X-AXIS LENGTH MUST BE GREATER THAN ZERO...')
      ENDIF
      IF(YLEN .EQ. 0) THEN
	CALL MABEND('Y-AXIS LENGTH MUST BE GREATER THAN ZERO...')
      ENDIF
      IF(XLEN .GT. 6.0) THEN
	CALL MABEND('X-AXIS LENGTH CANNOT EXCEED 5 INCHES (DEFAULT)...')
      ENDIF
      IF(YLEN .GT. 6.0) THEN
	CALL MABEND('Y-AXIS LENGTH CANNOT EXCEED 5 INCHES ...')
      ENDIF		
C
C--  GET THE TEXT/TITLE INFO
      CALL XVP('TITLE',TITLE,COUNT)
      CALL XVP('XLABEL',XLABEL,COUNT)
      CALL XVP('YLABEL',YLABEL,COUNT)
      CALL XVP('HEIGHT',HEIGHT,COUNT)
      CALL XVP('YCOLSTR',YSTRTMP,NYSTR)
      if (nystr.gt.0) then
        CALL XVSPTR(YSTRTMP,NYSTR,YSTRPTR,YSTRLEN)
        DO L = 1 , NYSTR
	    DO K = 1, YSTRLEN(L)
	        YCOLSTR(L)(K:K) = CHAR(YSTRTMP(YSTRPTR(L)+K-1))
	    ENDDO
        ENDDO
      endif
C
C---- READ THE AXIS LABELS AND SYMBOL STUFF
      CALL XVP('FREQ',FREQ,COUNT)
      CALL XVPARM ('SYMTYPE',SYMTYP,COUNT, DEF,20)
      IF (DEF .EQ. 1) THEN
          DO I = 1,MYCOL
	    SYMTYP(I) = 0
	  ENDDO
      ENDIF
      DO I = 1,MYCOL
          LINTYP(I) = FREQ*(SYMTYP(I)-1)
      ENDDO
      DO IYCOL = 1,MYCOL
         INTEQ(IYCOL) = MOD(IYCOL-1,15)
      ENDDO
C--  READ THE X AND Y RANGE IF PROVIDED
      CALL XVPARM('XRANGE',XRAN,COUNT,XRANDF,2)
      CALL XVPARM('YRANGE',YRAN,COUNT,YRANDF,2)
      print *, 'xrandf = ',XRANDF
      print *, 'yrandf = ',YRANDF
      XRANG1 = XRAN(1)
      XRANG2 = XRAN(2)
      YRANG1 = YRAN(1)
      YRANG2 = YRAN(2)
      IF (XRANG1 .EQ. XRANG2) THEN
	CALL MABEND('XRANGE MUST VARY...')
      ENDIF
      IF (YRANG1 .EQ. YRANG2) THEN
	CALL MABEND('YRANGE MUST VARY...')
      ENDIF
C
C--- OPEN THE INTERFACE FILE
	CALL XVUNIT(RUNIT,'INP',1,STATUS,' ')
	IF (STATUS .NE. 1) THEN
		CALL XVMESSAGE('ERROR ON IBIS UNIT CALL',' ')
		CALL XVMESSAGE('PROGRAM TERMINATED',' ')
		RETURN
	END IF
       call ibis_file_open(RUNIT,ibis_in,'read',0,0,' ',' ',status)
       if (status .ne. 1) call ibis_signal_u(RUNIT,status,1)
       call ibis_file_get(ibis_in,'nc',ncol,1,1)
       call ibis_file_get(ibis_in,'nr',CLEN,1,1)
       call ibis_column_read(ibis_in,CONTRL,ncol,1,clen,status)
       if (status .ne. 1) call ibis_signal_u(RUNIT,status,1)	
      CALL FINDCO(ibis_in,CONTRL,NCONTR,CLEN,NSTART,NPOINT,NPLOTS)
C
C--   START THE PLOTTER
      LDEV = 9
C
c      CALL PLOTS(0,0,LDEV)
      call xrtbegin(status)
      print *,'xrt status (non-zero, success) = ',status
C
C	Turn the autoaxes off. Otherwise an extra set of axes appear.
      call displayaxes(0,0,0)
      call displayaxes(1,1,0)
C
C
C---- BEGIN PLOTS. THIS LOOP MAKES COMPLETE PLOTS(TOTAL # =NPLOTS)
C     ACCORDING TO THE CONTROL COLUMN.
C
      print *,'nplots = ',nplots
      DO 100 IPLOT = 1,NPLOTS
C---- READ "X" AND "Y" COLUMNS.
C  Rewrite GETSET calls with ibis column read calls
      print *,'CLEN = ',clen
      call ibis_column_read(ibis_in,colx,ncolx,1,clen,status)
      if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)
      do l=1,mycol
	call ibis_column_read(ibis_in,coly(1,l),ncoly(l),1,clen,status)
	if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)
      end do
      LASTPT = NPOINT(IPLOT)
      print *,'LASTPT = ',lastpt
C
C---- SCALE AND DRAW X-AXIS.
C
c   code to check if y case applies to x case
	 IF (XRANDF .NE. 0) THEN
         COLMIN = ARRMIN(COLX,LASTPT)
         COLMAX = ARRMAX(COLX,LASTPT)
         IF (XMIN.GT.COLMIN) XMIN=COLMIN
         IF (XMAX.LT.COLMAX) XMAX=COLMAX
	 ELSE
         COLMIN = ARRMIN(COLX,LASTPT)
         COLMAX = ARRMAX(COLX,LASTPT)
         IF (XMIN.GT.COLMIN) XMIN=COLMIN
         IF (XMAX.LT.COLMAX) XMAX=COLMAX
	 ENDIF
c   cccccccccccccccccccccccccccccccccccccccccccc
      print *,'xrang1 = ',xrang1
      print *,'xrang2 = ',xrang2
      print *,'xmin = ',xmin
      print *,'xmax = ',xmax
      IF(XRANDF .NE. 0) THEN
        CALL SCALE(COLX,XLEN,LASTPT,1)
        FIRSTX = COLX(LASTPT+1)
        DELTAX = COLX(LASTPT+2)
      ELSE
c        CALL SCALE(COLX,XLEN,LASTPT,1)
        FIRSTX = XRANG1
        DELTAX = (XRANG2-XRANG1)/XLEN
        COLX(LASTPT+1) = FIRSTX
        COLX(LASTPT+2) = DELTAX
      ENDIF
      LXPNT = INT((XRANG2-XRANG1)/DELTAX)
      print *,'firstx = ',firstx
      print *,'deltax = ',deltax
      print *,'lxpnt = ',lxpnt
      print *,'FIRSTX AND DELTAX BEFORE AXIS...',firstx,deltax
      NCHAR = MAX( SLENGTH(XLABEL),1)
      call axestitles(xlabel,ylabel,270,' ',0)
C
C---- SCALE AND DRAW Y-AXIS.
C
      YMIN = 1.E+19
      YMAX = -1.E+19

      DO L = 1, MYCOL
	 IF (YRANDF .NE. 0) THEN
         COLMIN = ARRMIN(COLY(1,L),LASTPT)
         COLMAX = ARRMAX(COLY(1,L),LASTPT)
         IF (YMIN.GT.COLMIN) YMIN=COLMIN
         IF (YMAX.LT.COLMAX) YMAX=COLMAX
	 ELSE
         COLMIN = ARRMIN(COLY(1,L),LASTPT)
         COLMAX = ARRMAX(COLY(1,L),LASTPT)
         IF (YMIN.GT.COLMIN) YMIN=COLMIN
         IF (YMAX.LT.COLMAX) YMAX=COLMAX
	 ENDIF
      ENDDO
      print *,'ymin = ',ymin
      print *,'ymax = ',ymax
      IF (YRANDF .NE. 0) THEN
        CALL SCALE(SCALER,YLEN,2,1)
      ELSE
        FIRSTY = YRANG1
        DELTAY = (YRANG2-YRANG1)/YLEN
      ENDIF
      LYPNT = INT((YRANG2-YRANG1)/DELTAY)
      print *,'lypnt = ',lypnt
      print *,'yrang1 = ',yrang1
      print *,'yrang2 = ',yrang2
      print *,'FIRSTY AND DELTAY BEFORE AXIS...',firsty,deltay
C
      NCHAR = MAX( SLENGTH(TITLE), 1)
      label(1) = TITLE
      print *,'label(1) = ',label(1)
      call header(label(1),1,center)
C
C---- SET THE SCALING PARAMETERS.
C
      PRINT *,'LASTPT = ',LASTPT
      DO L = 1,MYCOL
         COLY(LASTPT+1,L) = FIRSTY
         COLY(LASTPT+2,L) = DELTAY
      ENDDO
C
C---- MAKE THE PLOT AND PLOT SYMBOLS 
C
      COLX(LASTPT+1) = FIRSTX
      COLX(LASTPT+2) = DELTAX
      Y = 0.0
      DO L = 1,MYCOL
	  call setactiveset(l)
          Y = Y + 2.0*0.15
          IF (YSTRLEN(L) .GT. 0)  THEN
	      IF (SYMTYP(L) .NE. 1) THEN 
     	        CALL SYMBOL(XLEN+0.5,Y,0.15,YCOLSTR(L),0,0.0,YSTRLEN(L))
	      ENDIF
	  ENDIF
	  IF (YRANDF .NE. 0) THEN
c       call setaxesminimums(xrang1,yrang1,yrang1)
       call setaxesmaximums(xrang2,yrang2,yrang2)
          CALL LINE(COLX,COLY(1,L), LASTPT,1, LINTYP(L), INTEQ(L))
	  ELSE
       call setaxesminimums(xrang1,yrang1,0)
       call setaxesmaximums(xrang2,yrang2,0)
c       CALL LINE(COLX,COLY(1,L),LASTPT,1,LINTYP(L),INTEQ(L))
        call line(colx,coly(1,l),lastpt,1,lintyp(l),inteq(l))
	  ENDIF 
      ENDDO
C
C& MODIFIED BY EJB SEPT 87 *********************
C&  BRANCH
       IF(DEVICE .EQ. 'CALCOMP') THEN
C& RESET (MOVE) ORIGIN AS FOR CALCOMP PLOTTER 
C& BUT CONTINUE THE SAME 'PLOT FILE'
        IF (IPLOT .LT. NPLOTS) CALL PLOT(0.,0.,  -3)
       ELSEIF(DEVICE .EQ. 'IMAGE') THEN
        IF (IPLOT .LT. NPLOTS) CALL NEWPLT
C&
       ELSE
C& ASSUME PRINTRONIX/REGIS/TEK OUTPUT DEVICE;
C& CLOSE THIS 'PLOT FILE' AND OPEN A NEW ONE FOR THE NEXT PLOT
       IF(IPLOT.LT.NPLOTS) CALL PLOT(0.,0.,999)
       IF(IPLOT.LT.NPLOTS) CALL PLOTS(0,0,LDEV)
       ENDIF
C& *********************************************
C
  100 CONTINUE
C
      CALL PLOT(0.,0.,999)
	call ibis_file_close(ibis_in,' ',status)
	if (status .ne. 1) call ibis_signal_u(RUNIT,status,1)

      RETURN
      END
C
C**************************************
	INTEGER FUNCTION SLENGTH(STRING)
	INTEGER	I
	CHARACTER*(*) STRING

	I = LEN(STRING)
	DO WHILE (ICHAR(STRING(I:I)) .EQ. 32 .AND. I .GT. 1)
	    I = I - 1
	ENDDO
	SLENGTH = I
	RETURN
	END
C
C*************************************
      REAL FUNCTION ARRMIN(ARRAY,NUMBER)
      REAL  ARRAY(1), DUMMY
      DUMMY = ARRAY(1)
      DO I = 2,NUMBER
        DUMMY = AMIN1(DUMMY,ARRAY(I))
      ENDDO
      ARRMIN = DUMMY
      RETURN
      END
C*************************************
      REAL FUNCTION ARRMAX(ARRAY,NUMBER)
      REAL  ARRAY(1), DUMMY
      DUMMY = ARRAY(1)
      DO I = 2,NUMBER
        DUMMY = AMAX1(DUMMY,ARRAY(I))
      ENDDO
      ARRMAX = DUMMY
      RETURN
      END


	SUBROUTINE FINDCO (ibis_in,CONTRL,NCONTR,LENGTH,NSTART,
     *		     NPOINT,NSETS)
	INTEGER*4 NPOINT(1), NSTART(1)
	INTEGER ibis_in,status 
C	REAL CONTRL(100000)
	DIMENSION CONTRL(1)
c	DIMENSION COLUMN(1)
C	NROWS = (LENGTH-1)/128+1
C	NRECTR = NROWS*(NCONTR-1)+2
C
C---- READ CONTROL COLUMN AND FIND START POINTS AND LENGTHS
C     FOR THE SETS.
C
C	CALL GETCOL (UNIT,NCONTR,LENGTH,CONTRL)
C	call ibis_column_read(unit,ncontr,length,contrl,status)
	call ibis_column_read(ibis_in,contrl,ncontr,1,length,status)
	if (status .ne. 1) call ibis_signal_u(ibis_in,status,1)
	NSTART(1) = 1
	NPT = 0
	NSETS = 0
	LENGTH=100
	L = LENGTH-1
	DO NELEM=1,L
	  NPT = NPT+1
	  IF (CONTRL(NELEM).NE.CONTRL(NELEM+1)) THEN
	    NSETS = NSETS+1
	    NSTART(NSETS+1) = NELEM+1
	    NPOINT(NSETS) = NPT
	    NPT = 0
	  ENDIF
	ENDDO
	NSETS = NSETS+1
	NPOINT(NSETS) = NPT+1
	RETURN
	END
C     ******************************

