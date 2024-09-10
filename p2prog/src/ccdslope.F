      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM CCDSLOPE
C
C Radiometric calibration routine:  Determines light transfer curve
C slope and offset for a linear camera system.
C
      SUBROUTINE MAIN44
         COMMON/C2/UNITS,LUMMSG,RADMSG

         REAL*8 SDN,SDN2,SXDN,SX,SX2,DENOM,DC(400),EDC(400)
         REAL*8 DN(30,400),W(30,400),DNSUM,DLTX
         REAL*4 LIGHT,DNS(6400),OUTM(2,2000)
         REAL*4 EXPOS(30,400),SLOPE(400)
         REAL*4 OFFS(400),CEXPO(30),SHUTTER_OFFSETS(1024),BEXPO

         INTEGER*4 AREA(4,400), NI
         INTEGER SL,SS,SLI,SSI,OUNI,STAT,CNT,BAD(400),SIGTOL
         INTEGER EFNUM,STATUS

         LOGICAL XVPTST

         CHARACTER*4320 LABEL
         CHARACTER*4 DIRECTN
         CHARACTER*92 MS1
         CHARACTER*14 VAL
         CHARACTER*256 PLOT_DS,OFFSET_FILE
         CHARACTER*51 ARMES
         CHARACTER*41 RMSG
         CHARACTER*82 MS2
         CHARACTER*40 MS3
         CHARACTER*62 MS4
         CHARACTER*40 LUMMSG
         CHARACTER*34 RADMSG
         CHARACTER*8  UNITS

         CALL IFMESSAGE ('CCDSLOPE version 28-JAN-05')

         ARMES = 'NUMBER OF GOOD AREAS=**** OUT OF**** AREAS SAMPLED'
         RMSG = 'NUMBER REJECTED FOR               =     '
         MS3 = 'DELTA CORRECTION=****.***** ENERGY UNIT'

         MS1(1:41)=' AREA XXX (SL,SS,NL,NS)=(XXX,XXX,XXX,XXX)'
         MS1(42:92)=' '                
         MS2(1:58)=' '                
         MS4(1:49)='AREA    SL   SS   NL   NS  SLOPE (DN/ENERGY UNIT)'
         MS4(50:62)='  OFFSET (DN)'
         LUMMSG='ENERGY UNIT = FOOT-LAMBERT-MILLISECONDS'
         RADMSG='ENERGY UNIT = PICOAMP-MILLISECONDS'

         CNTRL = 0

         IF (XVPTST('DELTAX')) CNTRL=1

         CALL XVPARM('PLOT',PLOT_DS,IPLOT,IDEF,1)
         IF (IDEF.EQ.0) THEN
            CALL PLOTFN(PLOT_DS)
            CALL XRTBEGIN(STATUS)
            IF (STATUS.NE.1) CALL MABEND('Unable to OPEN plotter')
            CALL DISPLAYAXES(1,1,0) ! x,y1,y2 axes displayed 1=yes 0=no
            CALL SETACTIVESET(1)   ! endpts on lines 0=no triangles, 1=triangles
         ELSE
            IPLOT = 0
         ENDIF

         CALL XVP('REJECT',IREJCT,CNT)
         CALL XVP('SIGTOL',SIGTOL,CNT)
         CALL PRNT(4,1,SIGTOL,' SIGMA REJ TOL=. ')
         CALL XVPARM('MOFSET',TOS,CNT,IDEF,1)
         CALL XVPARM('UNITS',UNITS,ICOUNT,IDEF,1)
         CALL XVPARM('OFFSETS',OFFSET_FILE,ICOUNT,IDEF,1)

C-----If shutter-offset file is supplied, read it and calculate
c-----mean shutter-offset
         IF (IDEF.EQ.0) THEN
            CALL XVUNIT(ISO,'X',1,IND,'U_NAME',OFFSET_FILE,' ')
            IF (IND.NE.1) GOTO 998
            CALL XVOPEN(ISO,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')
	    CALL XVGET(ISO,IND,'NS',NSL,' ')

            CALL XLGET(ISO,'HISTORY','FILE',VAL,IST,'FORMAT',
     &                 'STRING',' ')
	    IF (VAL .NE. 'SHUTTER-OFFSET') THEN
	       CALL XVMESSAGE 
     &            ('No SHUTTER-OFFSET in label of offset file',' ')
	       CALL XVMESSAGE(' Continuing anyway',' ')
	    END IF
            CALL XVREAD(ISO,SHUTTER_OFFSETS,IND,' ')
            TOS = 0.0
            DO I=1,NSL
               TOS = TOS + SHUTTER_OFFSETS(I)
            ENDDO
            TOS = TOS/FLOAT(NSL)

c-----While we're at it, check the label to see if the SO is Line-
c-----or Sample-dependent.  If it doesn't say, assume Line-
            CALL XLGET (ISO,'HISTORY','SO_TYPE',VAL,IST, 
     &                  'FORMAT','STRING',' ')
	    IF (IST .NE. 1 .OR. INDEX(VAL,'LINE') .NE. 0) THEN
               DIRECTN(1:4) = 'LINE'
	    ELSE
               DIRECTN(1:4) = 'SAMP'
            END IF
            CALL XVCLOSE(ISO,IND,' ')

         ELSE

c-----No SO file supplied, so fill buffer with mean value from parameters
	    CALL MVE(7,1024,TOS,SHUTTER_OFFSETS,0,1)
         ENDIF

	 CALL PRNT(7,1,TOS,'Mean shutter-offset=.')
	 CALL XVMESSAGE 
     &      ('Shutter-offset is '//DIRECTN(1:4)//'-dependent',' ')

         IF (XVPTST('SUBDC')) THEN
            IDCSUB = 1
            CALL XVMESSAGE ('DC subtraction mode',' ')
         ELSE
            IDCSUB = 0
            CALL XVMESSAGE ('DC included as data point on curve',' ')
         ENDIF

         CALL XVP('EXTEXPO',EFNUM,IEDC)
         IF (EFNUM .EQ. 0) IEDC=0     !Check for EDC Data (10 - OCT - 85)
         IF (IEDC .EQ. 1) 
     &      CALL XVMESSAGE ('EXTENDED EXPOSURE MODE DATA',' ')
         IF ((IDCSUB .EQ. 0) .AND. (IEDC .EQ. 1))
     &      EFNUM=EFNUM+1  !10 POINT CURVE!!

C-----Open the input Light Transfer File
         CALL XVUNIT(IUNI,'INP',1,STAT,' ')
         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &             'U_FORMAT','REAL',' ')
         CALL XVSIZE(SLI,SSI,NLI,NSI,NLO,NSO)
         CALL LABPROC(IUNI,LABEL,NLAB)
C             Get area size fields...
         CALL XLGET(IUNI,'HISTORY','NUM_AREAS',NAREA,STAT,
     &              'FORMAT','INT',' ')
         CALL XLGET(IUNI,'HISTORY','AREAS',AREA,STAT,'NELEMENT',4*NAREA,
     &              'FORMAT','INT',' ')

c-----Get the radiance value for this test from the LTF label
         CALL XLGET(IUNI,'PROPERTY','RADIANCE',LIGHT,NLUM,
     &             'PROPERTY','CASSINI-ISS','FORMAT','REAL',' ')

c-----Override with parameter
         CALL XVP('LIGHT',LIGHT,LCNT)

C-------any luminances entered?
	 IF ((LCNT .EQ. 0) .AND. (NLUM .NE. 1)) THEN
	    CALL XVMESSAGE('NO LIGHT VALUE INPUT',' ')
	    GOTO 999
	 END IF

         NPTS = NLI-IDCSUB-IEDC  !Number of points on light transfer curve
         J = -(IDCSUB+IEDC)      !J is index for point on curve
C
C        Get average DN for each area and at each exposure and
C        remove dark current if specified...

         CALL XLGET(IUNI,'HISTORY','NUM_EXPOS',NEXP,STAT,
     &              'FORMAT','INT',' ')
         CALL XLGET(IUNI,'HISTORY','EXPOSURES',EXPOS,STAT,
     &              'NELEMENT',NEXP,'FORMAT','REAL',' ')
         DO 50 L=1,NLI			!Exposure loop
            CALL XVREAD(IUNI,DNS,STAT,'LINE',L,'NSAMPS',NSI,' ')
            NI = NINT(DNS(1))
            IF (NI .EQ. 0) THEN
               BEXPO=EXPOS(L,1)
               GOTO 970
            END IF
            J = J + 1
            IF (J .GT. 0) CEXPO(J)=EXPOS(L,1)  !Store commanded exposure
            IB = 0

            DO 50 K=1,NAREA            	!Area loop
               SL=AREA(1,K)             !Get area size field
               SS=AREA(2,K)
               NL=AREA(3,K)
               NS=AREA(4,K)
               N = NI*NL*NS             !Total number of pixels in area*inputs
               DNSUM = 0.0D0
               DO I=1,NI
                  DNSUM = DNSUM + DBLE(DNS(IB+I+1))  !Sum of DNs across inputs
               ENDDO

               IF (IDCSUB .EQ. 0) THEN		!Do not subtract dark current...
                  IF (IEDC .EQ. 0) THEN
                     DN(J,K) = DNSUM/N	!Average DN for area
                  ELSE
                     IF (L .EQ. 1) THEN
                        EDC(K) = DNSUM/N	!Average extended DC for area
                        GOTO 50
                     ENDIF
                     IF (J .LT. EFNUM) THEN
                        DN(J,K) = DNSUM/N
                     ELSE
                        DN(J,K) = DNSUM/N - EDC(K) + DN(1,K)
                     ENDIF
                  ENDIF
               ELSE           !Subtract DC
                  IF (IEDC .EQ. 0) THEN
                     IF (L .EQ. 1) THEN
                        DC(K) = DNSUM/N	!Average dark current
                        GOTO 50
                     ENDIF
                     DN(J,K) = DNSUM/N - DC(K)
                  ELSE
                     IF (L .EQ. 1) EDC(K)=DNSUM/N
                     IF (L .EQ. 2) DC(K)=DNSUM/N
                     IF (L .LE. 2) GOTO 50
                     IF (J .LT. EFNUM) THEN
                        DN(J,K) = DNSUM/N - DC(K)
                     ELSE
                        DN(J,K) = DNSUM/N - EDC(K)
                     ENDIF
                  ENDIF
               ENDIF
   50    IB = IB + 3*NI

	 CALL XVCLOSE(IUNI,IST,' ')
C
C        Correct exposure for shutter offset and convert to energy units...
C
         MAXL = 0
         MAXS = 0

         DO K=1,NAREA
            SL=AREA(1,K)
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)

	    IF (K .EQ. NAREA) THEN
               MAXL = SL + NL/2       !finding last (l,s) of last area
               MAXS = SS + NS/2       !for defining reticle areas later
	    ENDIF

c---------Find line or sample of center of this area
	    IF (DIRECTN(1:4) .EQ. 'LINE') THEN
               L = SL + NL/2
	    ELSE
               L = SS + NS/2
	    ENDIF

c---------Correct exposure time for shutter-offset
            DO J=1,NPTS
               EXPOS(J,K) = AMAX1(CEXPO(J)-SHUTTER_OFFSETS(L),0.0)
               W(J,K) = LIGHT*EXPOS(J,K)
            ENDDO
         ENDDO

         LOOP = 0
         DLTX = 0.0D0

C           Main control loop: Repeat once if DLTX energy correction
C           has been specified (DELTX).

   60    IF (LOOP.EQ.1) THEN
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE 
     &         ('-----------------------------------------',' ')
            CALL XVMESSAGE 
     &         ('Processing repeated with DELTA correction',' ')
            CALL XVMESSAGE 
     &         ('-----------------------------------------',' ')
         ENDIF
C    Find slope and offsets for each area...
         DO K=1,NAREA
            SDN = 0.0D0
            SX = 0.0D0
            SDN2 = 0.0D0
            SX2 = 0.0D0
            SXDN = 0.0D0
            DO J=1,NPTS
               SDN = SDN + DN(J,K)             !SUM DNS
               SX = SX + W(J,K)	  	       !SUM ENERGY
               SDN2 = SDN2 + DN(J,K)**2        !SUM DN**2
               SX2 = SX2 + W(J,K)**2	       !SUM ENERGY**2
               SXDN = SXDN + W(J,K)*DN(J,K)    !SUM DN*ENERGY
            ENDDO
            DENOM = DBLE(NPTS)*SX2 - SX**2
            IF (DENOM .EQ. 0.0) GOTO 996 
            SLOPE(K) = (DBLE(NPTS)*SXDN-SX*SDN)/DENOM
            OFFS(K)  =  (SX2*SDN-SX*SXDN)/DENOM
         ENDDO
C    Weed out bad areas...
         CALL ZIA(BAD,NAREA)
         IF (NAREA.GT.1) THEN
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE ('GLOBAL VALUE FOR SLOPE...',' ')
            CALL IMEAN(SLOPE,1,NAREA,BAD,SIGTOL)
            CALL XVMESSAGE (' ',' ')
            CALL XVMESSAGE ('GLOBAL VALUE FOR OFFSET...',' ')
            CALL IMEAN(OFFS,2,NAREA,BAD,SIGTOL)
         ENDIF
C    Print out bad areas
         IBA = 0                  !flag for any bad areas
         CALL XVMESSAGE (' ',' ')
         CALL XVMESSAGE ('Summary of bad areas....',' ')
         DO 65 K=1,NAREA
            IF (BAD(K) .EQ. 0) GOTO 65
            SL=AREA(1,K)
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)

            MS1 = ' '
            WRITE (MS1,'(A4,I4,A16,I4,A1,I4,A1,I4,A1,I4,A1)')
     &        'AREA',K,'(SL,SS,NL,NS)=(',SL,',',SS,',',NL,',',NS,')'
            IF (BAD(K).EQ.1) MS1(47:74)='****BAD SLOPE FIT********'
            IF (BAD(K).EQ.2) MS1(47:74)='****BAD OFFSET FIT*******'
            IF (BAD(K).EQ.4) MS1(47:74)='****BOTH BAD FIT*********'
            CALL XVMESSAGE (MS1,' ')
            IBA = 1
   65    CONTINUE

         CALL XVMESSAGE (' ',' ')
         IF (IBA .eq. 0) CALL XVMESSAGE ('    No rejected areas',' ')
         CALL XVMESSAGE (' ',' ')
         CALL XVMESSAGE ('Slopes and offsets for each area...',' ')
         MS1(43:80) = ' '

         CALL XVMESSAGE (' ',' ')
         IF (UNITS .EQ. 'RADIANCE') THEN
            CALL XVMESSAGE (RADMSG,' ')
         ELSE
            CALL XVMESSAGE (LUMMSG,' ')
         ENDIF
         CALL XVMESSAGE (' ',' ')
         CALL XVMESSAGE (MS4,' ')

C-----Print out slope and offset for each area...
         DO 100 K=1,NAREA
            SL=AREA(1,K)
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)
            WRITE 
     &         (MS2,'(I4,2X,I4,1X,I4,1X,I4,1X,I4,7X,F10.5,9X,F10.5)')
     &         K,SL,SS,NL,NS,SLOPE(K),OFFS(K)
            CALL XVMESSAGE (MS2,' ')
  100    CONTINUE

         CALL LTCPLOT(AREA,BAD,CEXPO,DN,EXPOS,W,LABEL,SLOPE,OFFS,
     &                ARMES,RMSG,NPTS,NAREA,IREJCT,LIGHT,TOS,LOOP,
     &                DLTX,IDCSUB,NLAB,IPLOT,MAXL,MAXS)
         IF ((CNTRL .EQ. 0) .OR. (LOOP .EQ. 1)) GOTO 200
         WRITE (MS3,'(A17,F10.5,A12)')
     &          'DELTA CORRECTION=',DLTX,' ENERGY UNIT'
         CALL XVMESSAGE (MS3,' ')
         LOOP = 1

c-----Adjust energy terms by DLTX and loop back
         DO K=1,NAREA
            DO J=1,NPTS
               W(J,K) = W(J,K) - DLTX
            ENDDO
         ENDDO
         GOTO 60 


  200    CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
         IF (STAT .NE. 1) GOTO 300
         CALL ZIA(OUTM,2*NAREA)
         I = 0

         DO 220 K=1,NAREA
            IF ((IREJCT .EQ. 0) .OR. (BAD(K) .EQ. 0) .OR.
     &          (IREJCT .NE. 3) .AND. (IREJCT .NE. BAD(K)) .AND.
     &          (BAD(K) .NE. 4)) GOTO 220
            SL=AREA(1,K)
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)
            I = I + 1
            OUTM(1,I) = SL + NL/2
            OUTM(2,I) = SS + NS/2
  220    CONTINUE

         NSO = 2*I
         NLO = 1
         CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &        'OP','WRITE','U_NL',NLO,'U_NS',NSO,'U_FORMAT','REAL',
     &        'O_FORMAT','REAL',' ')

         IF (IREJCT .EQ. 1) 
     &      CALL XLADD (OUNI,'HISTORY','CCDSLOPE',
     &                  ' REJECTED FOR SLOPE ',STAT,'FORMAT',
     &                  'STRING',' ')

         IF (IREJCT .EQ. 2) 
     &      CALL XLADD(OUNI,'HISTORY','CCDSLOPE',
     &                 ' REJECTED FOR OFFSET ',STAT,'FORMAT',
     &                 'STRING',' ')

         IF (IREJCT .EQ. 3)
     &      CALL XLADD(OUNI,'HISTORY','CCDSLOPE',
     &                 ' REJECTED FOR BOTH ',STAT,'FORMAT',
     &                 'STRING',' ')

         CALL XVWRIT(OUNI,OUTM,STAT,'NSAMPS',NSO,' ')
         CALL XVCLOSE(OUNI,STAT,' ')

  300    IF (IPLOT .EQ. 0) RETURN
         CALL PLOT(0.00,0.00,999)
         CALL XVMESSAGE ('CCDSLOPE task completed',' ')
         RETURN

  970    CALL PRNT(7,1,BEXPO,'***No data for exposure=.')
         CALL XVMESSAGE 
     &      ('***Run MOMGEN on this exposure and try again.',' ')
         GOTO 999
  996    CALL XVMESSAGE ('*** Error: SUM ENERGY = 0.0',' ')
         GOTO 999
  998    CALL XVMESSAGE ('***Error opening shutter offset file',' ') 
  999    CALL XVMESSAGE ('***CCDSLOPE task cancelled',' ')
         CALL ABEND
      END


C Routine to flag all areas which differ from mean by more than 2 sigma.
C Inputs: BUF(N) = input samples
C         TYPE = 1 for slope, 2 for offset
C         N = number of samples
C Outputs: BAD(N) = TYPE if sample is bad type
C                 = 4 if sample has both bad slope and offset
C
      SUBROUTINE IMEAN(BUF,TYPE,N,BAD,SIGTOL)

         REAL*4 BUF(1)
         INTEGER BAD(1),TYPE,SIGTOL

         REAL*4 MAXDIFF
         REAL*8 SUM,SSUM,MEAN
         CHARACTER*44 MSG

         MSG = 'N=**** MEAN=******.***** SIGMA=******.***** '
    
         NS = 0			!Count of number of good samples
         SUM = 0.0D0
         SSUM = 0.0D0

C        Get sums and sums of squares of all good areas...
         DO K=1,N
            SAMP = BUF(K)
            NS = NS+1
            SUM = SUM+DBLE(SAMP)
            SSUM = SSUM+DBLE(SAMP*SAMP)
         ENDDO

C        ...and use these to compute mean and standard deviation.
         MEAN = SUM/NS
         SIGMA = DSQRT(SSUM/NS-MEAN*MEAN)
         WRITE (MSG,'(A2,I4,A6,F12.5,A7,F12.5)')
     &          'N=',NS,' MEAN=',MEAN,' SIGMA=',SIGMA
         CALL XVMESSAGE ('Raw mean and sigma are...',' ')
         CALL XVMESSAGE (MSG,' ')
         IF (SIGMA .EQ. 0.0) RETURN
C
C         Weed out all samples differing by more than 2 sigma from mean...
   10    MAXDIFF = 0.

C        First find sample with largest difference...
         DO 20 K=1,N
            IF ((BAD(K) .EQ. TYPE) .OR. (BAD(K) .EQ. 4)) GOTO 20
            SAMP = ABS(BUF(K)-SNGL(MEAN))
            IF (SAMP .LE. MAXDIFF) GOTO 20
            MAXDIFF = SAMP
            IMAX = K          
   20    CONTINUE

C        Continue if remaining samples are good.
         IF (MAXDIFF .LE. SIGTOL*SIGMA) GOTO 50	
         SAMP = BUF(IMAX)
         SUM = SUM - SAMP			!Delete worst sample from sums,
         SSUM = SSUM - SAMP**2
         NS = NS - 1
         IF (BAD(IMAX) .NE. 0) THEN		!and flag area as bad.
            BAD(IMAX) = 4
         ELSE
            BAD(IMAX) = TYPE
         ENDIF
         GOTO 10

   50    MEAN = SUM/NS
         SIGMA = DSQRT(SSUM/NS-MEAN**2)
         WRITE (MSG,'(A2,I4,A6,F12.5,A7,F12.5)')
     &          'N=',NS,' MEAN=',MEAN,' SIGMA=',SIGMA
         IF (SIGTOL .EQ. 1) THEN
            CALL XVMESSAGE 
     &         ('After throwing out samples differing by 1 sigma',' ')
         ELSE
            CALL XVMESSAGE 
     &         ('After throwing out samples differing by 2 sigma',' ')
         ENDIF
         CALL XVMESSAGE (MSG,' ')
         RETURN
      END         


C Routine to plot the light-transfer curve (signal vs exposure) for each of
C the 5 reticles...
C All arguments are inputs.
C
      SUBROUTINE LTCPLOT (AREA,BAD,CEXPO,DN,EXPOS,W,LABEL,SLOPE,
     &                    OFFS,ARMES,RMSG,NPTS,NAREA,IREJCT,LIGHT,
     &                    TOS,LOOP,DLTX,IDCSUB,NLAB,IPLOT,MAXL,MAXS)

         INTEGER*4 AREA(4,400)	  !Area size fields: (sl,ss,nl,ns)
         INTEGER*4 BAD(400)	  !1=bad slope, 2=bad offset, 4=both bad

         REAL*4 CEXPO(30)         !Commanded exposure times (msec)
         REAL*8 DN(30,400)        !Mean DN of each (exposure,area)
         REAL*4 EXPOS(30,400)     !Exposure of each (exposure,area)
         REAL*8 W(30,400)         !Energy of each (exposure,area)
         REAL*4 LIGHT             !Light values for each exposure
         REAL*4 SLOPE(400),OFFS(400)

         CHARACTER*4320 LABEL
         CHARACTER*51 ARMES
         CHARACTER*41 RMSG  !labels & annotation
         CHARACTER*40 TBLFILE

         REAL*8 DLTX,DX(30),WX(30),COMPDNX(30),SMEAN
         REAL*8 AVDN(30,5),AVW(30,5),COMPDN(30,5),ASLOPE,AOFFS
         REAL*8 SSLOPE(5),SOFF(5)      !Sums of slopes and offsets 
         REAL*8 S2SLOPE(5),S2OFF(5)    !Sums of sqrs of slopes & offsets 
         REAL*8 SLOPESIG(5),OFFSIG(5)  !Sigmas for each reticle
         REAL*8 TSLOPESIG,TOFFSIG      !Framewide sigmas
         REAL*8 TSSLOPE,TSOFF,TS2SLOPE,TS2OFF   !framewide sums of areas
         REAL*4 AVEXPO(30,5),TX(30),RETICLES(2,5)
         INTEGER*4 NG(5)		  !number of good areas in each reticle
         INTEGER*4 NA(5)		  !number of areas near reticle
         INTEGER SL,SS
         LOGICAL LASTFLAG

         LASTFLAG = .FALSE.
         CALL ZIA(NG,5)
         CALL ZIA(NA,5)
         CALL ZIA(SSLOPE,10)
         CALL ZIA(SOFF,10)
         CALL ZIA(S2SLOPE,10)
         CALL ZIA(S2OFF,10)
         CALL ZIA(SLOPESIG,10)
         CALL ZIA(OFFSIG,10)

C-------NPTS is the number of exposure levels

         DO I=1,5
           CALL ZIA(AVDN(1,I),NPTS*2)
           CALL ZIA(AVEXPO(1,I),NPTS)
           CALL ZIA(AVW(1,I),NPTS*2)
           CALL ZIA(COMPDN(1,I),NPTS*2)
         ENDDO

C-----Set Reticle points based upon the last line/samp used
         RETICLES(1,1) = 1           !upper left
         RETICLES(2,1) = 1
         RETICLES(1,2) = 1           !upper right
         RETICLES(2,2) = maxs
         RETICLES(1,3) = maxl        !lower left
         RETICLES(2,3) = 1
         RETICLES(1,4) = maxl        !lower right
         RETICLES(2,4) = maxs
         RETICLES(1,5) = maxl/2      !center
         RETICLES(2,5) = maxs/2
C
C           Compute signal, exposure and energy at each reticle...
         DO 10 K=1,NAREA
            SL = AREA(1,K)
            SS = AREA(2,K)
            NL = AREA(3,K)
            NS = AREA(4,K)
C           Find nearest reticle...
            DMIN = 99999.**2

            DO I=1,5
               D = (RETICLES(1,I)-SL)**2 + (RETICLES(2,I)-SS)**2
               IF (D .LT. DMIN) THEN
                  DMIN = D
                  IMIN = I
               ENDIF
            ENDDO

            NA(IMIN) = NA(IMIN) + 1
            IF ((IREJCT .EQ. 1) .AND. 
     &          ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 10
            IF ((IREJCT .EQ. 2) .AND. 
     &          ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 10
            IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 10
C           Add data to that reticle...
            NG(IMIN) = NG(IMIN) + 1      !number of good areas in reticle

C-----Sum the DN, EXP and ENERGY of area into reticle sums for each
C-----exposure level
            DO J=1,NPTS
               AVDN(J,IMIN) = AVDN(J,IMIN) + DN(J,K)
               AVEXPO(J,IMIN) = AVEXPO(J,IMIN) + EXPOS(J,K)
               AVW(J,IMIN) = AVW(J,IMIN) + W(J,K)
            ENDDO

c-----Accumulate the sums and squares to calculate the sigmas 
            SSLOPE(IMIN) = SSLOPE(IMIN) + SLOPE(K)
            S2SLOPE(IMIN) = S2SLOPE(IMIN) + SLOPE(K)*SLOPE(K)
            SOFF(IMIN) = SOFF(IMIN) + OFFS(K)
            S2OFF(IMIN) = S2OFF(IMIN) +OFFS(K)*OFFS(K)

  10     CONTINUE

         IF (IREJCT .EQ. 1) RMSG(22:35)='    SLOPE     '
         IF (IREJCT .EQ. 2) RMSG(22:35)='    OFFSET    '
         IF (IREJCT .EQ. 3) RMSG(22:35)='    BOTH      '

         CALL ZIA(TX,NPTS)
         CALL ZIA(DX,2*NPTS)
         CALL ZIA(COMPDNX,2*NPTS)
         CALL ZIA(WX,2*NPTS)
         NGOOD = 0             !number of good areas framewide
         TSSLOPE = 0.D0
         TSOFF = 0.D0
         TS2SLOPE = 0.D0
         TS2OFF = 0.D0

C-----Process and plot data for each reticle
C-----Also, accumulate data to enable whole image processing
         DO 20 I=1,5
            IF (NG(I) .EQ. 0) THEN
               CALL XVMESSAGE 
     &            ('***No good areas in this part of frame',' ')
               GOTO 20
            ENDIF
            NGOOD = NGOOD + NG(I)      !increment number of good areas
            DO J=1,NPTS
               DX(J) = DX(J) + AVDN(J,I)      !Framewide sums
               TX(J) = TX(J) + AVEXPO(J,I)
               WX(J) = WX(J) + AVW(J,I)
               AVDN(J,I) = AVDN(J,I)/NG(I)    !Reticle means
               AVEXPO(J,I) = AVEXPO(J,I)/NG(I)
               AVW(J,I) = AVW(J,I)/NG(I)
            ENDDO

            WRITE (RMSG(38:40),'(I3)') NA(I)-NG(I)

            IF (IREJCT.EQ.0) THEN
               RMSG(1:41)='NO REJECTION CRITERIA APPLIED            '
            END IF

            WRITE (ARMES(22:25),'(I4)') NG(I)
            WRITE (ARMES(33:36),'(I4)') NA(I)

C-------Find mean of slopes and offsets of all areas allocated to
c-------this reticle.  This is necessary in order to derive a
c-------sigma for them.  The means are derived from the already-
c-------calculated slopes and offsets for each area.
            SMEAN = SSLOPE(I)/DBLE(NG(I))
            SLOPESIG(I) = DSQRT(S2SLOPE(I)/DBLE(NG(I)) - SMEAN*SMEAN)
            CALL PRNT(4,1,I,'Reticle .')
            CALL XVMESSAGE ('Means from averaging values of areas',' ')
            CALL PRNT(8,1,SMEAN,' SLOPE  =.')
            SMEAN = SOFF(I)/DBLE(NG(I))
            OFFSIG(I) = DSQRT(S2OFF(I)/DBLE(NG(I)) - SMEAN*SMEAN)
            CALL PRNT(8,1,SMEAN,' OFFSET =.')
            CALL XVMESSAGE (' ',' ')

C-------Accumulate the sums and squares of sums of all areas in
c-------order to derive the frame-wide sigma later.
            TSSLOPE = TSSLOPE + SSLOPE(I)
            TSOFF = TSOFF + SOFF(I)
            TS2SLOPE = TS2SLOPE + S2SLOPE(I)
            TS2OFF = TS2OFF + S2OFF(I)

C------Derive the slope and offset for each reticle by using the
c------exposure, dn and energy values at each exposure level accumulated
c------from all areas allocated to this reticle.  Does not just
c------average the slopes and offsets for each allocated area that have
c------already been calculated.

            CALL RTIMES(NPTS,LIGHT,CEXPO,AVEXPO(1,I),AVDN(1,I),AVW(1,I),
     &                  SLOPESIG(I),OFFSIG(I),IPLOT,I,LABEL,NLAB,ARMES,
     &                  RMSG,TOS,LOOP,ASLOPE,AOFFS,COMPDN(1,I),
     &                  DLTX,IDCSUB,LASTFLAG)

   20    CONTINUE

C-------Find mean of slopes and offsets of all areas.
c-------This is necessary in order to derive a
c-------sigma for the framewide values.  The means are derived from 
c-------the already-accumulated sums of slopes and offsets for each area.
         SMEAN = TSSLOPE/DBLE(NGOOD)
         TSLOPESIG = DSQRT(TS2SLOPE/DBLE(NGOOD) - SMEAN*SMEAN)
         CALL XVMESSAGE ('Framewide',' ')
         CALL XVMESSAGE ('Means from averaging values of areas',' ')
         CALL PRNT(8,1,SMEAN,' SLOPE=.')
         SMEAN = TSOFF/DBLE(NGOOD)
         TOFFSIG = DSQRT(TS2OFF/DBLE(NGOOD) - SMEAN*SMEAN)
         CALL PRNT(8,1,SMEAN,' OFFSET=.')
         CALL XVMESSAGE (' ',' ')

c-----Make the framewide means to use in deriving the slope and
c-----offset for the entire frame.
         DO J=1,NPTS
            DX(J) = DX(J)/DBLE(NGOOD)
            TX(J) = TX(J)/DBLE(NGOOD)
            WX(J) = WX(J)/DBLE(NGOOD)
         ENDDO

C-----Process and plot framewide data, returning the best fit
c-----slope and offset in ASLOPE and AOFFS.
         LASTFLAG = .TRUE.
         CALL RTIMES(NPTS,LIGHT,CEXPO,TX,DX,WX,TSLOPESIG,TOFFSIG,
     &               IPLOT,6,LABEL,NLAB,ARMES,RMSG,TOS,LOOP,ASLOPE,
     &               AOFFS,COMPDNX,DLTX,IDCSUB,LASTFLAG)

         DLTX = -AOFFS/ASLOPE

         CALL XVPARM('TABLE',TBLFILE,ICNT,IDEF,1)
         IF (ICNT .EQ. 0) RETURN

C-----Use reticle and whole frame data to create output table

         CALL GENTBL(AVDN,COMPDN,AVW,DX,COMPDNX,WX,NPTS,TBLFILE,*999)

999	 CONTINUE
         RETURN
      END



C Routine to plot:
C	1) Light transfer curve (DN vs Energy)
C       2) Residuals to curve
C       3) Percent residuals
C The light transfer curve has the form: DN = SLOPE*ENERGY + OFFSET
C
C Inputs: NPTS=Number of data points on curve
C            L=light intensity in relative foot lamberts or nanowatts/cm**2/sr/
C              nanometer
C	     CEXPO=commanded exposure time (msec)
C            T=corrected exposure time (msec)
C            D=DN value
C	     W=energy 
C            SLOPESIG=simga of slope
C            OFFSSIG=sigma of offset
C
C Outputs: SLOPE,OFFS=slope and offset of light transfer curve
C
      SUBROUTINE RTIMES(NPTS,L,CEXPO,T,D,W,SLOPESIG,OFFSSIG,IPLOT,
     &                  IRETICLE,LABEL,NLAB,ARMES,RMSG,TOS,LOOP,
     &                  SLOPE,OFFS,DNP,DLTX,IDCSUB,LASTFLAG)

         REAL*4 CEXPO(30),T(30),L
         REAL*8 D(30),W(30)
         REAL*8 SLOPE,OFFS,DLTX
         REAL*8 PCDX(30),DNP(30),DEL(30)
         REAL*8 SLOPESIG,OFFSSIG,DENOM,RMS,RX
         REAL*8 SDN,SDN2,SXDN,SDNP,SX,SX2,XC1,XC2

         LOGICAL LASTFLAG,TEMPFLAG

         CHARACTER*12 MSG(6)
         CHARACTER*34 MSG2
         CHARACTER*62 GMS2
         CHARACTER*132 MSG1
         CHARACTER*9 HD1, HD16
         CHARACTER*6 HD2, HD7, HD12
         CHARACTER*8 HD3, HD4, HD5
         CHARACTER*18 DC8, HD8, HD44, HD45
         CHARACTER*12 HD6
         CHARACTER*7 HD14
         CHARACTER*5 HD9
         CHARACTER*6 HD10L, HD10R
         CHARACTER*13 HD11
         CHARACTER*3 HD13
         CHARACTER*4 HD15

         CHARACTER*4320 LABEL 
         CHARACTER*51 ARMES 
         CHARACTER*41 RMSG
         CHARACTER*8  UNITS
         CHARACTER*40 LUMMSG
         CHARACTER*34 RADMSG
         COMMON/C2/UNITS,LUMMSG,RADMSG
 
C Assigning values to array MSG  (Also start of executable code)

         TEMPFLAG = LASTFLAG
         LASTFLAG = .FALSE.
 
         HD1 = 'COMMANDED'
         HD2 = 'ACTUAL'
         HD3 = 'BEST FIT'
         HD4 = 'RESIDUAL'
         HD44 = 'RESIDUAL (DN)     '
         HD45 = 'RESIDUAL (PERCENT)'     
         HD5 = 'EXPOSURE'
         HD6 = 'ILLUMINATION'
         HD7 = 'ENERGY'
         DC8 = '(DN-DC)           '
         HD8 = '   DN             '
         HD9 = 'T(MS)'
         HD10L = 'L(LUM)'
         HD10R = 'L(RAD)'
         HD11 = '(ENERGY UNIT)'
         HD12 = 'SLOPE='
         HD13 = 'SD='
         HD14 = 'OFFSET='
         HD15 = 'RMS='
         HD16 = 'CORRECTED'

         MSG(1)='=UPPER-LEFT '
         MSG(2)='=UPPER-RIGHT'
         MSG(3)='=LOWER-LEFT '
         MSG(4)='=LOWER-RIGHT'
         MSG(5)='=CENTER     '
         MSG(6)='=FULL FRAME '   

         GMS2(1:35)='GALGEN:C1=****.***** ENERGY UNIT/DN'
         GMS2(36:62)= ' C2=****.***** ENERGY UNIT '
      
C-----Print which reticle is being done
         MSG2='STATISTICS FOR UPPER-RIGHT CORNER'
         MSG2(16:26)=MSG(IRETICLE)(2:12)
         IF ((IRETICLE .EQ. 5) .OR. (IRETICLE .EQ. 6))
     &      MSG2(28:33) = ' '
         CALL XVMESSAGE (MSG2,' ')

C-----Construct line 1 of header
         MSG1 = ' '
         MSG1(1:9)=HD1
         MSG1(13:18)=HD2
         IF (LOOP .EQ. 1) MSG1(39:47)=HD16
         MSG1(60:67)=HD3
         MSG1(71:78)=HD4 
         CALL XVMESSAGE (' ',' ')
         CALL XVMESSAGE (MSG1,' ')        !print header line 1

C-----Construct line 2 of header
         MSG1 = ' '
         MSG1(1:8)=HD5
         MSG1(12:19)=HD5
         MSG1(22:33)=HD6
         MSG1(39:44)=HD7
         IF (IDCSUB .EQ. 0) THEN
            MSG1(49:55)=HD8(1:7)
            MSG1(60:66)=HD8(1:7)
            MSG1(71:77)=HD8(1:7)
         ELSE
            MSG1(49:55)=DC8(1:7)
            MSG1(60:66)=DC8(1:7)
            MSG1(71:77)=DC8(1:7)
         ENDIF
         CALL XVMESSAGE (MSG1,' ')        !print header line 2

C-----Construct line 3 of header
         MSG1 = ' '
         MSG1(2:6)=HD9
         MSG1(13:17)=HD9
         IF (UNITS .EQ. 'RADIANCE') THEN
            MSG1(25:30)=HD10R
         ELSE
            MSG1(25:30)=HD10L
         ENDIF
         MSG1(35:47)=HD11 
         CALL XVMESSAGE (MSG1,' ')        !print header line 3

c-----Perform least squares fit to derive slope and offset
         SX=0.0D0
         SX2=0.0D0
         SDN=0.0D0
         SDN2=0.0D0
         SXDN=0.0D0
         SDNP=0.0D0

         DO I=1,NPTS
            SX = SX + W(I)
            SDN = SDN + D(I)
            SX2 = SX2 + W(I)**2
            SXDN = SXDN + W(I)*D(I)
            SDN2 = SDN2 + D(I)**2
         ENDDO

         DENOM = DBLE(NPTS)*SX2 - SX**2
         SLOPE = (DBLE(NPTS)*SXDN-SX*SDN)/DENOM
         OFFS =  (SX2*SDN-SX*SXDN)/DENOM

C-----Set up residuals...
         RX=0.0D0
         MSG1 = ' '

         DO I=1,NPTS
            DNP(I) = W(I)*SLOPE + OFFS
            DEL(I) = D(I) - DNP(I)		!Residual error in DN
            PCDX(I) = 0.
            IF (D(I) .NE. 0.) PCDX(I)=100.0D0*DEL(I)/D(I) !Percentage error
            RX = RX + DEL(I)**2			!Accumulate RMS error
            WRITE (MSG1,'(F10.4, 2F11.4, F14.4, 2F11.4, F10.4)')
     &             CEXPO(I),T(I),L,W(I),D(I),DNP(I),DEL(I)
            CALL XVMESSAGE (MSG1,' ')
         ENDDO

c------Commenting all this out 'cause noone can figure it out and
c------have now input SLOPESIG and OFFSSIG from calling routine.

         RMS = DSQRT(RX/DBLE(NPTS))
         XC1 = 1.0D0/SLOPE          !GALGEN COEFICIENT C1
         XC2 = -(OFFS/SLOPE)        !GALGEN COEFICIENT C2
         MSG1 = ' '
         MSG1(65:68)=HD15
         WRITE (MSG1(69:78),'(F10.4)') RMS !print RMS
         CALL XVMESSAGE (MSG1,' ')

         MSG1 = ' '
         MSG1(1:6)=HD12
         WRITE (MSG1(7:17),'(F11.6)') SLOPE !calc'd slope
         MSG1(19:21)=HD13
         WRITE (MSG1(22:30),'(F9.6)') SLOPESIG !std dev of slope
         MSG1(32:38)=HD14      
         WRITE (MSG1(39:49),'(F11.6)') OFFS !calc'd offset
         MSG1(51:53)=HD13      
         WRITE (MSG1(54:62),'(F9.6)') OFFSSIG !std dev of offset
         CALL XVMESSAGE (MSG1,' ')

c-----Number of good areas and rejection reasons
         CALL XVMESSAGE (ARMES,' ')
         CALL XVMESSAGE (RMSG,' ')
         WRITE (GMS2(11:20),'(F10.5)') XC1 !GALGEN COEF.
         WRITE (GMS2(40:49),'(F10.5)') XC2 !GALGEN COEF.
         CALL XVMESSAGE (GMS2,' ')

         IF (IPLOT .EQ. 0) RETURN
         IF (LOOP .EQ. 1) RETURN

         IF (IDCSUB .EQ. 0) THEN
            CALL SPLOT(LABEL,NLAB,HD8,D,DNP,W,0,NPTS,ARMES,
     &                 MSG(IRETICLE),RMSG,MSG1,GMS2,TOS,DLTX,
     &                 IRECTICLE,RMS,LASTFLAG)
         ELSE
            CALL SPLOT(LABEL,NLAB,DC8,D,DNP,W,0,NPTS,ARMES,
     &                 MSG(IRETICLE),RMSG,MSG1,GMS2,TOS,DLTX,
     &                 IRETICLE,RMS,LASTFLAG)
         ENDIF

         CALL SPLOT(LABEL,NLAB,HD44,DEL,DEL,W,2,NPTS,ARMES,
     &              MSG(IRETICLE),RMSG,MSG1,GMS2,TOS,DLTX,
     &              IRETICLE,RMS,LASTFLAG)

         IF (TEMPFLAG) LASTFLAG=.TRUE.

         CALL SPLOT(LABEL,NLAB,HD45,PCDX,PCDX,W,2,NPTS,ARMES,
     &              MSG(IRETICLE),RMSG,MSG1,GMS2,TOS,DLTX,
     &              IRETICLE,RMS,LASTFLAG)
         RETURN
      END



      SUBROUTINE SPLOT(LABEL,NLAB,YTITLE,DY,DY2,DX,IFLAG,NPTS,
     &                 ARMES,MSG,RMSG,MS2,GMS2,TOS,DLTX,
     &                 IRETICLE,RMS,LASTFLAG)

         COMMON/PLT1/LXT,LYT,LT
         COMMON/C2/UNITS,LUMMSG,RADMSG

         INTEGER ICNT,STATUS,P

         REAL*4 Y(30),X(30),Y2(30),ZBUF(30)
         REAL*8 DY(30),DX(30),DY2(30),DLTX,RMS

         CHARACTER*8  UNITS
         CHARACTER*40 LUMMSG
         CHARACTER*34 RADMSG
         CHARACTER*18 YTITLE
         CHARACTER*4320 LABEL
         CHARACTER*51 ARMES
         CHARACTER*12 MSG, LMSG(7)
         CHARACTER*41 RMSG
         CHARACTER*63 MS2
         CHARACTER*62 GMS2
         CHARACTER*80 CMS1
         CHARACTER*80 TITLE(70)
         CHARACTER*6 XTITLE
         CHARACTER*28 OMES
         CHARACTER*30 DLTAX
         CHARACTER*30 RESMG

         ICNT = 2
         CMS1 = ' '
         TITLE(1) = 'CCD LIGHT TRANSFER ANALYSIS'
         XTITLE = 'ENERGY'
         LUMMSG = 'ENERGY UNIT = FOOT-LAMBERT-MILLISECONDS'
         RADMSG = 'ENERGY UNIT = PICOAMP-MILLISECONDS'

         IMIN=0
         IMAX=0
         LXT=6
         LYT=7
         IF (IFLAG .EQ. 2) LYT=18
         LT=31

         NN = MIN0(NLAB,25)

         DO P=2,NN
            TITLE(ICNT)=LABEL(1+(P-1)*72:1+(P-1)*72+71)
            ICNT = ICNT + 1
         ENDDO

         WRITE(RESMG,'(A17,F9.4,A3)') 'RMS RESIDUAL FIT=',RMS,' DN'
         WRITE(DLTAX,'(A8,F9.5,A12)') 'DELTA X=',DLTX,' ENERGY UNIT'
         CMS1(31:60)=DLTAX
         WRITE(OMES,'(A15,F9.5,A3)') 'SHUTTER OFFSET=',TOS,' MS'
         IF (UNITS .EQ. 'RADIANCE') THEN
            TITLE(ICNT)=RADMSG
            ICNT = ICNT + 1
         ELSE
            TITLE(ICNT)=LUMMSG
            ICNT = ICNT + 1
         ENDIF
         CMS1(1:27)=OMES
         TITLE(ICNT)=CMS1
         ICNT = ICNT + 1
         TITLE(ICNT)=RMSG
         ICNT = ICNT + 1
         TITLE(ICNT)=ARMES
         ICNT = ICNT + 1
         TITLE(ICNT)=MS2
         ICNT = ICNT + 1
         TITLE(ICNT)=RESMG
         ICNT = ICNT + 1
         TITLE(ICNT)=GMS2
         CALL HEADER (TITLE,ICNT,0) ! 0=left justify, 1=center justify, 2=right
         CALL AXESTITLES(XTITLE,YTITLE,270,' ',0)

         DO I=1,NPTS
            Y(I)  = DY(I)
            Y2(I) = DY2(I)
            X(I)  = DX(I)
            ZBUF(I)=0.0000
         ENDDO

         X(NPTS+1)=0.0
         X(NPTS+2)=1.0

         IF (IFLAG .EQ. 2) THEN
            DO I=1,NPTS
               IF (Y(I) .GT. 1.00) IMAX=1
               IF (Y(I) .LT. -1.00) IMIN=1
            ENDDO
            IF ((IMIN .EQ. 1) .OR. (IMAX .EQ. 1)) THEN
               ZBUF(NPTS+1)=0.0
               ZBUF(NPTS+2)=1.0
            ELSE
               Y(NPTS+1)=0.0 
               Y(NPTS+2)=1.0
               ZBUF(NPTS+1)=0.0
               ZBUF(NPTS+2)=1.0
            ENDIF
         ELSE
            Y(NPTS+1)=0.00
            Y(NPTS+2)=1.0
            Y2(NPTS+1)=0.0
            Y2(NPTS+2)=1.0
         ENDIF

         DO II=1,7
            LMSG(II)=' '
         ENDDO
         LMSG(IRETICLE+1)=MSG
         call setlabel (LMSG,7,1,2) ! 7=# of symbols 1=verticle 2=position east
         CALL SETACTIVESET(IRETICLE)
 
         CALL LINE(X,Y,NPTS,1,1,1)             !DATA LINE
         IF (IFLAG .EQ. 2) CALL LINE(X,ZBUF,NPTS,1,0,0) !RESIDUAL
         IF (IFLAG .EQ. 0) CALL LINE(X,Y2,NPTS,1,1,4)   !RESIDUAL
         IF (.NOT. LASTFLAG) THEN
            CALL XRTPAGE(STATUS)
            IF (STATUS .NE. 1) THEN
               CALL XVMESSAGE ('*** Incomplete Ploting Operation.',' ')
               CALL XVMESSAGE ('XRT Window Button Definitions:',' ')
               CALL XVMESSAGE
     &            ('SAVE = write to output PostScript file.',' ')
               CALL XVMESSAGE ('PAGE = display next graph.',' ')
               CALL XVMESSAGE ('EXIT = terminate application.',' ')
               CALL MABEND('***Unable to PAGE plotter',' ')
            ENDIF
         ENDIF
         RETURN
      END



      SUBROUTINE LABPROC(IUNI,LABEL,NLAB)

         IMPLICIT NONE 

         INTEGER INSTANCES(20)
         INTEGER NHIST, J, I             ! loop control variables
         INTEGER NLAB, STAT              ! store # of labels and status
         INTEGER IUNI                    ! store file id number
 
         CHARACTER*8 TASKS(20)
         CHARACTER*4320 LABEL 
         CHARACTER*132 MSG
         CHARACTER*12 UNAME
         CHARACTER*28 TIME
         CHARACTER*65 HBUF

C        initialize display and storage buffers
         HBUF = '----TASK:------------USER:------------------------'
         MSG = ' '
         LABEL = ' '

         CALL VIC1LAB(IUNI,STAT,NLAB,LABEL,0)   ! retrive VICAR 1 label
                                                ! NLAB=0 if no VIC1LAB

         NHIST = 20                             ! EXTRACTS VIC*2 LAB

C        retirve history tasks
         CALL XLHINFO(IUNI,TASKS,INSTANCES,NHIST,STAT,' ')
 
C        for each task, extract the USER name and the DATE_TIME,
C        write the TASK name, USER name, and DATE_TIME onto buffer (HBUF)
         DO J=1,NHIST
            UNAME = ' '
            TIME = ' '
            CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')
            CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')
            DO I=1,8                             !BLANKS NULLS
               IF (TASKS(J)(I:I) .LT. '0') TASKS(J)(I:I)=' '
            ENDDO
            HBUF(10:17)=TASKS(J)
            HBUF(27:38)=UNAME
            HBUF(39:63)=TIME
            LABEL(1+(NLAB+J-1)*72:(1+(NLAB+J-1)*72)+64)=HBUF
         ENDDO

         NLAB=NLAB+NHIST   ! calculate number of labels to be display

C        output history tasks retrived from the label
         DO I=1,NLAB
            MSG(1:72) = LABEL((1+(I-1)*72):(1+(I-1)*72)+71)
            CALL XVMESSAGE (MSG,' ')
            MSG = ' '
         ENDDO
         RETURN
      END



      subroutine GENTBL(DN,CDN,W,DNX,CDNX,WX,npts,table_ds,*)

         INTEGER*4 NPTS

         REAL*8 DN(30,5),CDN(30,5),W(30,5)
         REAL*8 DNX(NPTS),CDNX(NPTS),WX(NPTS)

         CHARACTER*40 TABLE_DS
         CHARACTER*1 TAB
C
C-------use last point of full frame buffer to calculate format
c        val = max(dnx(npts),wx(npts),cdnx(npts))
c        lod = ifix(log10(val)) + 1  !number of places left of decimal
c        nc = lod + 5      !add 1 for pad, 1 for sign, 1 for point,
C                          ! and 2 for places right of decimal
c  VARIABLES IN FORMAT STATEMENTS DON'T WORK ON LINUX (SEE STATEMENT 9,
C  BELOW), SO ABOVE CODE HAS BEEN DISABLED.  -LWK / 28JAN05

C** Note:
C   RECL should be ignored for sequential file in FORTRAN 77.  
C   However, it will crash on VMS if the output string is too long.  
C   Therefore, RECL is specified to patch this problem.  UNIX will 
C   generate a Warrning message when it sees RECL, but it does 
C   not affect the output and the numerical computation of this program.
C   A notice has been summited to DEC in regard to this problem. 
C   -- TXH
C**
c        OPEN(12,FILE=TABLE_DS,STATUS='UNKNOWN',RECL=300,
c    &        IOSTAT=JST,ERR=999)
c   The above form causes a crash on Linix, replace with:
        OPEN(12,FILE=TABLE_DS,STATUS='UNKNOWN',
     &        IOSTAT=JST,ERR=999)

         TAB = CHAR(9)

         WRITE(12,10)
     &      'UL_MEAN_DN',TAB,'UL_COMPUTED_DN',TAB,'UL_ENERGY',TAB,
     &      'UR_MEAN_DN',TAB,'UR_COMPUTED_DN',TAB,'UR_ENERGY',TAB,
     &      'LL_MEAN_DN',TAB,'LL_COMPUTED_DN',TAB,'LL_ENERGY',TAB,
     &      'LR_MEAN_DN',TAB,'LR_COMPUTED_DN',TAB,'LR_ENERGY',TAB,
     &      'CENT_MEAN_DN',TAB,'CENT_COMPUTED_DN',TAB,'CENT_ENERGY',TAB,
     &      'FRAME_MEAN_DN',TAB,'FRAME_COMPUTED_DN',TAB,'FRAME_ENERGY'
10       FORMAT(1X,4(A10,A1,A14,A1,A9,A1),A12,A1,A16,A1,A11,A1,
     &          A13,A1,A17,A1,A12)
	
         DO L=1,NPTS
            WRITE(12,9) 
     &         DN(L,1),TAB,CDN(L,1),TAB,W(L,1),TAB,
     &         DN(L,2),TAB,CDN(L,2),TAB,W(L,2),TAB,
     &         DN(L,3),TAB,CDN(L,3),TAB,W(L,3),TAB,
     &         DN(L,4),TAB,CDN(L,4),TAB,W(L,4),TAB,
     &         DN(L,5),TAB,CDN(L,5),TAB,W(L,5),TAB,
     &         DNX(L),TAB, CDNX(L), TAB,WX(L)
	 ENDDO

	 CLOSE(12)
	 RETURN

c9        FORMAT(18(F<nc>.2,A1))
9        FORMAT(18(F8.2,A1))

999      CALL XVMESSAGE('ERROR OPENING TABLE FILE',' ')
         CALL PRNT(4,1,JST,' IOSTAT =.')
         RETURN 1
      END

