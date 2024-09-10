      INCLUDE 'VICMAIN_FOR'
C VICAR PROGRAM CCDNOISE
C
C Radiometric calibration routine:  Determines system gain constant (in
C electrons per DN) and the read-noise floor (DN) for a CCD camera system.
C
C         CCDNOISE LTF MARK
C
C If no extended dark currents are present, the records (L) in the LTF
C contain the following:
C   L=1    LTF header record.
C   L=2    Data record for exposure level 0 (dark current)
C   L=3    Data record for exposure level 1
C   L=NLI  Data record for hightest exposure level
C If extended dark currents are present, then:
C   L=1    LTF header record.
C   L=2    Data record for exposure level -1 (extended dark current)
C   L=3    Data record for exposure level 0 (normal dark current)
C   L=4    Data record for exposure level 1
C   L=NLI  Data record for hightest exposure level
C
      SUBROUTINE MAIN44
   
         REAL*8 RMEAN,SIGMA,SSUM(10),SSUM2(10),SSUMXY(10)
         REAL*8 EPDNMEAN,RDNMEAN,UI,UI2,SI2,UISI2
         REAL*4 EXP(2),reticle(2,5),OUT(6400),OUTM(2,2000)
         REAL*4 RDNBUF(400),EXPOS(30),EPDBUF(400)
         REAL*4 RSIGNAL(30,5),RNOISE(30,5),REPDN(5),RRDN(5)
         REAL*4 EDC(400),U(30,400),S(30,400),bexpo

         INTEGER AREA(4,400)
         INTEGER SL,SS,SLI,SSI,STAT,CNT,BAD(400),GAREA,EFNUM
         INTEGER*4 NG(5),TABLE,NI

         LOGICAL DBUG, XVPTST

         CHARACTER*132 MSG
         CHARACTER*4320 LABEL
         CHARACTER*51 ARMES
         CHARACTER*41 RMSG
         CHARACTER*85 MS1
         CHARACTER*40 MS2,table_ds
         CHARACTER*256 PLOT_DS

         ARMES = 'NUMBER OF GOOD AREAS=**** OUT OF**** AREAS SAMPLED'
         RMSG = 'NUMBER REJECTED FOR               ='
         MS2 = 'K=XXXXXX.XXXXX E/DN RDN=******.***** DN'
         MS1 = 'AREA XXX (SL,SS,NL,NS)=(XXXX,XXXX,XXXX,XXXX)'

         CALL IFMESSAGE('CCDNOISE version 22-APR-1997',' ')
C        IREJECT=0  Do not reject areas
C               =1  Reject areas with gain terms 2 sigma from average
C               =2  Reject areas with noise terms 2 sigma from average
C	        =3  Reject areas with both bad gain and noise terms
         DBUG = XVPTST('DBUG')

         CALL XVPARM('PLOT',PLOT_DS,IPLOT,IDEF,1)
         IF (IPLOT .GT. 0) THEN
            CALL PLOTFN(PLOT_DS)
         ELSE
            IPLOT = 0
         ENDIF

	 call xvparm('TABLE',TABLE_DS,TABLE,IDEF,1)

         CALL XVP('REJECT',IREJCT,CNT)
         CALL XVP('LIMIT',EXP,CNT)
         IF (CNT .EQ. 2) THEN
            EXP1 = EXP(1)
            EXP2 = EXP(2)
         ELSE
            EXP1 = 0.
            EXP2 = 999999.
         ENDIF
         CALL XVP('EXTEXPO',EFNUM,IEDC)
         IF (EFNUM .EQ. 0) IEDC=0   !Check for extended dark current

         CALL XVUNIT(IUNI,'INP',1,STAT,' ')
         CALL XVOPEN(IUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',' ')
         CALL XVSIZE(SLI,SSI,NLI,NSI,dummy1,dummy2)
         CALL LABPROC(IUNI,LABEL,NLAB)
C        Read in area size fields...
         CALL XLGET(IUNI,'HISTORY','NUM_AREAS',NAREA,STAT,
     &              'FORMAT','INT',' ')
         CALL XLGET(IUNI,'HISTORY','AREAS',AREA,STAT,'NELEMENT',4*NAREA,
     &              'FORMAT','INT',' ')
         CALL XLGET(IUNI,'HISTORY','NUM_EXPOS',NEXP,STAT,
     &              'FORMAT','INT',' ')
         CALL XLGET(IUNI,'HISTORY','EXPOSURES',EXPOS,STAT,
     &              'NELEMENT',NEXP,'FORMAT','REAL',' ')
         L1 =1 
         L2 = NLI
         NPTS = NLI - IEDC	!Number of points of light transfer curve
C
C  Compute mean DN (U) and sigma (S) for each area...
C      U(1,K) = mean DC for area K
C  For L=2,3,4,...,NLI
C      U(L,K) = mean signal at exposure L-2 for area K (dark current subtracted)
C      S(L,K) = mean noise at exposure L-2
C  Note that U(2,K) = mean signal at exposure 0 = DC - DC = 0 
C
         DO 50 L=1,NLI        !Loop through each exposure record...
            CALL XVREAD(IUNI,OUT,STAT,'LINE',L,'NSAMPS',NSI,' ')
            NI=NINT(OUT(1))
            IF (NI .EQ. 0) THEN
               BEXPO=OUT(L)
               GOTO 970
            END IF
            IF (EXPOS(L) .LT. EXP1) L1=L+1
            IF (EXPOS(L) .LE. EXP2) L2=L
            IB=1

            DO 50 K=1,NAREA	   !Loop through each area...
               SL=AREA(1,K)                      !Get area size field
               SS=AREA(2,K)
               NL=AREA(3,K)
               NS=AREA(4,K)
               CALL MVE(9,NI,OUT(IB+1),SSUM,1,1)
               CALL MVE(9,NI,OUT(IB+NI+1),SSUM2,1,1)
               CALL MVE(9,NI,OUT(IB+2*NI+1),SSUMXY,1,1)
               N = NL*NS
               SIGMA = 0.D0
               RMEAN = SSUM(1)

               DO I=2,NI
                  RMEAN = RMEAN+SSUM(I)
                  SIGMA = SIGMA+(SSUM2(I)+SSUM2(I-1)-2*SSUMXY(I-1))/N
     &                    -((SSUM(I)-SSUM(I-1))/N)**2
               ENDDO

               RMEAN = RMEAN/(NI*N)
               IF (NI .GT. 1) SIGMA=SIGMA/(NI-1)
               IF (SIGMA .LT. 0.D0) SIGMA=0.D0

               IF (IEDC .EQ. 0) THEN
                  IF (L .EQ. 1) U(1,K)=RMEAN		!mean DC
                  U(L+1,K) = RMEAN - U(1,K)		!signal = DN - DC
                  S(L+1,K) = DSQRT(SIGMA/2.D0)
               ELSE
                  IF (L .EQ. 1) THEN
                     EDC(K) = RMEAN			!extended DC
                     GOTO 50
                  ENDIF
                  IF (L .EQ. 2) U(1,K)=RMEAN		!normal DC
                  IF (L .LT. EFNUM+3) THEN
                     U(L,K) = RMEAN - U(1,K)
                  ELSE
                     U(L,K) = RMEAN - EDC(K)
                  ENDIF
                  S(L,K) = DSQRT(SIGMA/2.D0)
               ENDIF
           
   50    IB = IB+3*NI

         IF (L1 .GE. L2) GOTO 998
         IF (IEDC .EQ. 1) THEN
             L1 = L1 - 1		!EDC is not stored in U or S
             L2 = L2 - 1
         ENDIF

         MAXL = 0			!Maximum line number encountered
         MAXS = 0			!Maximum sample number encountered
         MAXL2 = 0			!Maximum last line number encountered
         MAXS2 = 0			!Maximum last sample number encountered
C
C          Compute system gain constant in electrons per DN (EPDN)
C          and read noise...
C
         DO 100 K=1,NAREA
            SL=AREA(1,K)                      !Get area size field
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)
            MAXL = MAX0(MAXL,SL)
            MAXS = MAX0(MAXS,SS)
            MAXL2 = MAX0(MAXL2,SL+NL-1)
            MAXS2 = MAX0(MAXS2,SS+NS-1)
            N = L2 - L1 + 1		!Number of light levels in fit
            UI = 0.D0
            UI2 = 0.D0
            SI2 = 0.D0
            UISI2 = 0.D0
C           Least squares fit over signal/noise curve...
            DO L=L1,L2
               RMEAN = U(L+1,K)	!mean signal (DN)
               SIGMA = S(L+1,K)	!noise (DN)
               UI = UI + RMEAN
               UI2 = UI2 + RMEAN**2
               SI2 = SI2 + SIGMA**2
               UISI2 = UISI2 + RMEAN*SIGMA**2
            ENDDO

            D = N*UI2 - UI**2			!Calculate read noise and
            IF (D .ne. 0.) THEN
               RDN = (SI2*UI2-UISI2*UI)/D	!system gain constant (in
            else
               rdn = 0.
            endif
            IF (RDN .GT. 0.) RDN=SQRT(RDN)	!electrons per DN) via least
            EPDN = D/(N*UISI2-UI*SI2)		!squares fit...
            RDNBUF(K) = RDN
            EPDBUF(K) = EPDN

            WRITE (MS1(6:8),'(I3)') K
            WRITE (MS1(25:28),'(I4)') SL
            WRITE (MS1(30:33),'(I4)') SS
            WRITE (MS1(35:38),'(I4)') NL
            WRITE (MS1(40:43),'(I4)') NS
            WRITE (MS2(3:14),'(F12.5)') EPDN
            WRITE (MS2(25:36),'(F12.5)') RDN
            MS1(46:85) = MS2
            IF (DBUG) CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE(MS1,' ')
            IF (DBUG)
     &         CALL AREADATA(EXPOS(2),U(2,K),S(2,K),NPTS,EPDN,RDN)
  100    CONTINUE

         CALL ZIA(BAD,NAREA)
C        Calculate global system gain and noise constants and
C        weed out bad areas...
         IF (NAREA .GT. 1) THEN
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE('Global value for K...',' ')
            CALL IMEAN(EPDBUF,1,NAREA,BAD)
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE('Global noise floor...',' ')
            CALL IMEAN(RDNBUF,2,NAREA,BAD)
         ENDIF

         CALL XVMESSAGE(' ',' ')
         MS1(1:70) = ' '
         I = 0
C        Report all bad areas...              
         DO 200 K=1,NAREA
            SL=AREA(1,K)                      !Get area size field
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)

            MS1 = ' '
            WRITE (MS1,'(A4,I4,A16,I4,A1,I4,A1,I4,A1,I4,A1)')
     &        'AREA',K,'(SL,SS,NL,NS)=(',SL,',',SS,',',NL,',',NS,')'
            IF (BAD(K).EQ.0) GOTO 200
            IF (BAD(K).EQ.1) MS1(47:71) = '*****BAD SYSTEM GAIN*****'
            IF (BAD(K).EQ.2) MS1(47:71) = '*****BAD NOISE FLOOR*****'
            IF (BAD(K).EQ.4) MS1(47:71) = '****BOTH CONSTANTS BAD***'
            CALL XVMESSAGE(MS1,' ')

            IF (IREJCT .EQ. 0) GOTO 200
            IF ((IREJCT .NE. 3) .AND.
     &          (IREJCT .NE. BAD(K)) .AND.
     &          (BAD(K) .NE. 4)) GOTO 200
            I = I + 1
            OUTM(1,I)=SL+NL/2   !Store center of bad area in MARK output record
            OUTM(2,I)=SS+NS/2
  200    CONTINUE
C
C          Compute means of all good areas
C
         GAREA = 0         !number of good areas
         EPDNMEAN = 0.D0   !mean system gain constant
         RDNMEAN = 0.D0	   !mean read noise

         DO 210 K=1,NAREA
            IF ((IREJCT .GT. 0 .AND. IREJCT .EQ. BAD(K)) .OR.
     &          (IREJCT .EQ. 3 .AND. BAD(K) .GT. 0) .OR.
     &          (IREJCT .GT. 0 .AND. BAD(K) .EQ. 4)) GOTO 210
            GAREA = GAREA + 1
            EPDNMEAN = EPDNMEAN + EPDBUF(K)
            RDNMEAN = RDNMEAN + RDNBUF(K)
  210    CONTINUE

         EPDNMEAN = EPDNMEAN/GAREA
         RDNMEAN = RDNMEAN/GAREA
         NREJCT = NAREA - GAREA

         MSG(43:91)= ' '
         CALL XVMESSAGE(' ',' ')
         IF (IREJCT.EQ.1) RMSG(21:34)=' SYSTEM GAIN  '
         IF (IREJCT.EQ.2) RMSG(21:34)=' NOISE FLOOR  '
         IF (IREJCT.EQ.3) RMSG(21:34)=' NOISE OR GAIN'
	 if (irejct .eq. 0) then
            call xvmessage('No rejection criteria applied',' ')
	 else
            WRITE (RMSG(39:41),'(I3)') NREJCT
            CALL XVMESSAGE(RMSG,' ')
	 end if
         WRITE (ARMES(22:25),'(I4)') GAREA
         WRITE (ARMES(33:36),'(I4)') NAREA
         CALL XVMESSAGE(ARMES,' ')
         WRITE (MS2(3:14),'(F12.5)') EPDNMEAN
         WRITE (MS2(25:36),'(F12.5)') RDNMEAN
         CALL XVMESSAGE(MS2,' ')
         CALL XVMESSAGE(' ',' ')
C
C          Write centers of bad areas into MARK format file...
C
         CALL XVUNIT(OUNI,'OUT',1,STAT,' ')
         IF (STAT.NE.1) GOTO 300		!Skip if no output specified...
         NSO = 2*NREJCT
         NLO = 1
         CALL XVOPEN(OUNI,STAT,'OPEN_ACT','SA','IO_ACT','SA',
     &               'OP','WRITE','U_NL',NLO,'U_NS',NSO,'U_FORMAT',
     &               'REAL','O_FORMAT','REAL',' ')
         IF (IREJCT .EQ. 1)
     &      CALL XLADD(OUNI,'HISTORY','CCDNOISE',
     &                 ' REJECTED FOR SYSTEM GAIN',STAT,'FORMAT',
     &                 'STRING',' ')      
         IF (IREJCT .EQ. 2)
     &      CALL XLADD(OUNI,'HISTORY','CCDNOISE',
     &                 ' REJECTED FOR NOISE FLOOR',STAT,'FORMAT',
     &                 'STRING',' ')      
         IF (IREJCT .EQ. 3)
     &      CALL XLADD(OUNI,'HISTORY','CCDNOISE',
     &                 ' REJECTED FOR BOTH',STAT,'FORMAT',
     &                 'STRING',' ')
         CALL XVWRIT(OUNI,OUTM,STAT,'NSAMPS',NSO,' ')
         CALL XVCLOSE(OUNI,STAT,' ')

C        Plot or table ....
  300    IF ((IPLOT .EQ. 0) .and. (table .eq. 0)) GOTO 400
C
C-----Set Reticle points based upon the last line/samp used
	 reticle(1,1) = 1            !upper left
	 reticle(2,1) = 1
	 reticle(1,2) = 1            !upper right
	 reticle(2,2) = maxs2
	 reticle(1,3) = maxl2        !lower left
	 reticle(2,3) = 1
	 reticle(1,4) = maxl2        !lower right
	 reticle(2,4) = maxs2
	 reticle(1,5) = maxl2/2      !center
	 reticle(2,5) = maxs2/2
c
C        Compute signal and noise at each of the reticle points...
         DO 305 K=1,NAREA
            IF ((IREJCT .EQ. 1) .AND.
     &          ((BAD(K) .EQ. 1) .OR. (BAD(K) .EQ. 4))) GOTO 305
            IF ((IREJCT .EQ. 2) .AND.
     &          ((BAD(K) .EQ. 2) .OR. (BAD(K) .EQ. 4))) GOTO 305
            IF ((IREJCT .EQ. 3) .AND. (BAD(K) .NE. 0)) GOTO 305
            SL=AREA(1,K)                      !Get area size field
            SS=AREA(2,K)
            NL=AREA(3,K)
            NS=AREA(4,K)
C           Find nearest reticle...
            DMIN = 99999.**2
            DO I=1,5
               D = (RETICLE(1,I)-SL)**2 + (RETICLE(2,I)-SS)**2
               IF (D .LT. DMIN) THEN
                  DMIN = D
                  IMIN = I
               ENDIF
            ENDDO
C           Add signal and noise data to that reticle...
            NG(IMIN) = NG(IMIN) + 1
            DO J=1,NPTS
               RSIGNAL(J,IMIN) = RSIGNAL(J,IMIN) + U(J+1,K)
               RNOISE(J,IMIN) = RNOISE(J,IMIN) + S(J+1,K)
            ENDDO
  305    CONTINUE

         DO I=1,5
            IF (NG(I) .GT. 0) THEN
               DO J=1,NPTS
                  RSIGNAL(J,I) = RSIGNAL(J,I)/NG(I)
                  RNOISE(J,I) = RNOISE(J,I)/NG(I)
               ENDDO
               RSIGNAL(1,I) = 0.	!Just to be sure (for plot)
            ENDIF
         ENDDO
C
C        Compute system gain constant and read noise at reticles...
         DO 350 I=1,5
            N = L2 - L1 + 1		!Number of light levels in fit
            UI = 0.D0
            UI2 = 0.D0
            SI2 = 0.D0
            UISI2 = 0.D0
C           Least squares fit over signal/noise curve...
            DO L=L1,L2
               RMEAN = RSIGNAL(L,I)  !mean signal (DN)
               SIGMA = RNOISE(L,I)   !noise (DN)
               UI = UI + RMEAN
               UI2 = UI2 + RMEAN**2
               SI2 = SI2 + SIGMA**2
               UISI2 = UISI2 + RMEAN*SIGMA**2
            ENDDO

            D = N*UI2 - UI**2			!Calculate read noise and
            RDN = (SI2*UI2-UISI2*UI)/D	!system gain constant (in
            IF (RDN .GT. 0.) RDN=SQRT(RDN)	!electrons per DN) via least
            EPDN = D/(N*UISI2-UI*SI2)		!squares fit...
            RRDN(I) = RDN
  350    REPDN(I) = EPDN

         if (iplot .ne. 0) then
            CALL NPLOT(NPTS,EXPOS(IEDC+1),RSIGNAL,RNOISE,REPDN,RRDN,
     &                 NG,LABEL,NLAB,ARMES,RMSG,MS2,*997)
         end if

         if (table .eq. 1) then
            call wrttbl(npts,expos(iedc+2),rsignal,rnoise,repdn,rrdn,
     &             table_ds,*999)
         end if

         CALL XVCLOSE(IUNI,STAT,' ')

  400    CALL XVMESSAGE('CCDNOISE task completed',' ')
         RETURN
CCCCCCCCCCCCCCCC
C          ERROR CONDITIONS
  970    CALL PRNT(7,1,EXPO,'***No data for exposure=.')
         CALL XVMESSAGE('***Run MOMGEN on this exposure and try again.',
     &               ' ')
         GOTO 999
  997    CALL XVMESSAGE('***Plot Error.',' ')
         GOTO 999
  998    CALL XVMESSAGE('***Err in exposure ranges',' ')
  999    CALL XVMESSAGE('***CCDNOISE task cancelled',' ')
         CALL ABEND
      END



C---Routine to write signal and noise tab-delimitted table
	SUBROUTINE WRTTBL(NPTS,EXP,RSIG,RNOI,REPDN,RRDN,TABLE_DS,*)

           REAL*4 RSIG(30,5),RNOI(30,5),REPDN(5),RRDN(5)
           REAL*4 EXP(npts),CN1,CN2,CN3,CN4,CN5

           CHARACTER*1 TAB
           CHARACTER*40 TABLE_DS

C-------RSIG AND RNOI ARE (NPTS,NUMBER OF RETICLE AREAS)
C-------REPDN AND RRDN ARE ELECTRONS/DN AND READ NOISE AND ARE
C-------(NUMBER OF RETICLE AREAS)

           OPEN(11,FILE=TABLE_DS,STATUS='UNKNOWN',RECL=1200,
     &          IOSTAT=JST,ERR=999)
	   TAB=CHAR(9)
	
C-------write header for table identifying columns
	   WRITE(11,8) 'Exp.time',
     &       TAB,'Signal-UL',TAB,'Noise-UL',TAB,'Calc.noise-UL',
     &       TAB,'Signal-UR',TAB,'Noise-UR',TAB,'Calc.noise-UR',
     &       TAB,'Signal-LL',TAB,'Noise-LL',TAB,'Calc.noise-LL',
     &       TAB,'Signal-LR',TAB,'Noise-LR',TAB,'Calc.noise-LR',
     &       TAB,'Signal-CEN',TAB,'Noise-CEN',TAB,'Calc.noise-CEN'

	   DO L=1,NPTS
C-------compute the computed noise value for each area at this exp. level
	      CN1 = SQRT(RSIG(L,1)/REPDN(1) + RRDN(1)**2)
	      CN2 = SQRT(RSIG(L,2)/REPDN(2) + RRDN(2)**2)
	      CN3 = SQRT(RSIG(L,3)/REPDN(3) + RRDN(3)**2)
	      CN4 = SQRT(RSIG(L,4)/REPDN(4) + RRDN(4)**2)
	      CN5 = SQRT(RSIG(L,5)/REPDN(5) + RRDN(5)**2)
	      WRITE(11,9) EXP(L),TAB,RSIG(L,1),TAB,RNOI(L,1),TAB,CN1,
     &          TAB,RSIG(L,2),TAB,RNOI(L,2),TAB,CN2,
     &          TAB,RSIG(L,3),TAB,RNOI(L,3),TAB,CN3,
     &          TAB,RSIG(L,4),TAB,RNOI(L,4),TAB,CN4,
     &          TAB,RSIG(L,5),TAB,RNOI(L,5),TAB,CN5
	   ENDDO

	   CLOSE(11)
	   RETURN

8	   FORMAT(1X,A8,15(A1,A14))
9	   FORMAT(1X,F12.5,15(A1,F12.4))
999	   CALL xvmessage('ERROR OPENING TABLE FILE',' ')
	   CALL PRNT(4,1,JST,' IOSTAT =.')
	   RETURN 1
	END



C Routine to flag all areas which differ from mean by more than 2 sigma.
C Inputs: BUF(N) = input samples
C         TYPE = 1 for system gain constant, 2 for read noise
C         N = number of samples
C Outputs: BAD(N) = TYPE if sample is bad type
C                 = 4 if sample has both bad gain and noise

      SUBROUTINE IMEAN(BUF,TYPE,N,BAD)

         REAL*4 BUF(*),MEAN,MAXDIFF
         REAL*8 SUM,SSUM

         INTEGER BAD(*),TYPE

         CHARACTER*43 MSG

         MSG = 'N=**** MEAN=******.***** SIGMA=******.******'

         NS = 0			!Count of number of good samples
         SUM = 0.0D0
         SSUM = 0.0D0
C	 Get sums and sums of squares of all good areas...
         DO K=1,N
            IF ((BAD(K) .NE. TYPE) .AND. (BAD(K) .NE. 4)) THEN 
               SAMP = BUF(K)
               NS = NS+1
               SUM = SUM+DBLE(SAMP)
               SSUM = SSUM+DBLE(SAMP)*DBLE(SAMP)
            ENDIF
         ENDDO
C          ...and use these to compute mean and standard deviation.
         MEAN = SNGL(SUM)/NS
         SIGMA = SNGL(SSUM)/NS-MEAN*MEAN
         IF (SIGMA .GT. 0.0) SIGMA = SQRT(SIGMA)
         WRITE (MSG(3:6),'(I4)') NS
         WRITE (MSG(13:24),'(F12.5)') MEAN
         WRITE (MSG(32:43),'(F12.5)') SIGMA
         CALL XVMESSAGE('Raw mean and sigma are...',' ')
         CALL XVMESSAGE(MSG,' ')
         IF (SIGMA .EQ. 0.0) RETURN

C        Weed out all samples differing by more than 2 sigma from mean...
   10    MAXDIFF = 0.
C        First find sample with largest difference...
         DO 20 K=1,N
            IF ((BAD(K) .EQ. TYPE) .OR. (BAD(K) .EQ. 4)) GOTO 20
            SAMP = ABS(BUF(K)-MEAN)
            IF (SAMP .LE. MAXDIFF) GOTO 20
            MAXDIFF = SAMP
            IMAX = K          
   20    CONTINUE

C        Continue if remaining samples are good.
         IF (MAXDIFF .LE. 2*SIGMA) GOTO 50  
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

   50    MEAN = SNGL(SUM)/NS
         SIGMA = SNGL(SSUM)/NS-MEAN**2
         IF (SIGMA .GT. 0.0) SIGMA = SQRT(SIGMA)
         WRITE (MSG(3:6),'(I4)') NS
         WRITE (MSG(13:24),'(F12.5)') MEAN
         WRITE (MSG(32:43),'(F12.5)') SIGMA
         CALL XVMESSAGE 
     &      ('After throwing out samples differing by 2 sigma',
     &               ' ')
         CALL XVMESSAGE(MSG,' ')
         RETURN
      END         



C Routine to print out data for area...
C Inputs: EXPOS = exposure times in milliseconds
C         U = mean signal in DN
C         S = RMS noise in DN
C         NPTS = number of points of curve
C         EPDN = system gain constant (electrons/DN)
C         RDN = read noise (DN)
C
      SUBROUTINE AREADATA(EXPOS,U,S,NPTS,EPDN,RDN)

         REAL*4 EXPOS(NPTS),U(NPTS),S(NPTS),EXPO

         CHARACTER*132 MSG

         MSG = ' '
         MSG(1:51) = '    EXPOSURE   MEAN       MEASURED   LOG'          
         MSG(52:132) = 'LOG         COMPUTED       RATIO'
         CALL XVMESSAGE (MSG, ' ')
         MSG = ' '
         MSG(1:51) = '    TIME (MS)  SIGNAL(DN) NOISE(DN)  SIGNAL(DN)'
         MSG(52:132) = 'NOISE(DN)   NOISE(DN)  (NOISE/COMPUTED)'
         CALL XVMESSAGE (MSG, ' ')

         DO L=1,NPTS
            EXPO = EXPOS(L)
            RMEAN = U(L)
            SIGMA = S(L)
            IF ((RMEAN .LT. 0.) .OR. (EPDN .LE. 0)) THEN
               TNOISE = 0.
               RATIO = 0.
            ELSE
               TNOISE = SQRT(RMEAN/EPDN + RDN**2)
               RATIO = SIGMA/TNOISE
            ENDIF
            MSG = ' '
            WRITE (MSG(2:13),'(F12.5)') EXPO !Exposure time (msec)
            WRITE (MSG(15:24),'(F10.4)') RMEAN !Mean signal (DN)
            WRITE (MSG(26:36),'(F11.5)') SIGMA !Noise (DN)
            IF (RMEAN .GT. .1D0) THEN
               R = LOG10(RMEAN)
               WRITE (MSG(38:47),'(F10.5)') R !LOG signal (DN)
            ENDIF
            IF (SIGMA .GT. 0.D0) THEN
               R = LOG10(SIGMA)
               WRITE (MSG(49:58),'(F10.5)') R !LOG noise (DN)
            ENDIF
            WRITE (MSG(60:69),'(F10.5)') TNOISE !Computed noise (DN)
            WRITE (MSG(71:80),'(F10.5)') RATIO !Ratio measured/computed
            CALL XVMESSAGE(MSG,' ')
         ENDDO

         RETURN
      END



C Routine to plot signal vs noise for the 5 reticles...
C All arguments are inputs.
C
      SUBROUTINE NPLOT(NPTS,EXPOS,RSIGNAL,RNOISE,REPDN,RRDN,NG,
     &     LABEL,NLAB,ARMES,RMSG,MS2,*)

         REAL*4 EXPOS(30),RSIGNAL(30,5),RNOISE(30,5),REPDN(5),RRDN(5)
         REAL*4 X(30),Y(30) !Buffers for log signal and log noise

         INTEGER*4 NG(30),ICNT,STATUS

         CHARACTER*4320 LABEL
         CHARACTER*51 ARMES
         CHARACTER*41 RMSG,MS2
         CHARACTER*80 TITLE(80)
         CHARACTER*14 YTITLE
         CHARACTER*15 XTITLE
         CHARACTER*12 MSG(5), LMSG(6)
         CHARACTER*62 MSG2

         MSG2(1:52)='UPPER-RIGHT CORNER  K=*****.* E/DN  RDN=***.*** DN'
         MSG2(53:61) = 'NAREA=***'
         title(1) = 'CCD NOISE ANALYSIS'
         icnt = 2
         ytitle = 'LOG NOISE (DN)'
         xtitle = 'LOG SIGNAL (DN)'
c
         MSG(1)='=UPPER-LEFT '
         MSG(2)='=UPPER-RIGHT'
         MSG(3)='=LOWER-LEFT '
         MSG(4)='=LOWER-RIGHT'
         MSG(5)='=CENTER     '

         CALL XRTBEGIN(STATUS)
         IF (STATUS.NE.1) CALL MABEND('Unable to OPEN plotter')
         CALL DISPLAYAXES(1,1,0)

C        Generate plot for each reticle...
         DO 100 I=1,5
            IF (NG(I) .EQ. 0) GOTO 20
            DO J=1,NPTS
               RMEAN = RSIGNAL(J,I)
               IF (RMEAN .GT. 0.) RMEAN=LOG10(RMEAN)
               X(J) = RMEAN
               SIGMA = RNOISE(J,I)
               IF (SIGMA .GT. 0.) SIGMA=LOG10(SIGMA)
               Y(J) = SIGMA
            ENDDO
   20    CONTINUE
         icnt=2
         MSG2(1:11) = MSG(I)(2:12)
         IF (I .EQ. 5) MSG2(13:19) = ' '
         WRITE (MSG2(23:29),'(F7.1)') REPDN(I)
         WRITE (MSG2(41:47),'(F7.3)') RRDN(I)
         WRITE (MSG2(59:61),'(I3)') NG(I)
         CALL XVMESSAGE(' ',' ')
         CALL XVMESSAGE(MSG2,' ')
         IF (NG(I) .EQ. 0) THEN
            CALL XVMESSAGE(' ',' ')
            CALL XVMESSAGE
     &         ('***No good areas in this part of frame',' ')
            GOTO 100
         ENDIF
         CALL AREADATA(EXPOS(2),RSIGNAL(1,I),RNOISE(1,I),NPTS,
     &          REPDN(I),RRDN(I))

         N = MIN0(NLAB,25)

         DO L=1,N
            TITLE(ICNT)=LABEL((L-1)*72+1:(L-1)*72+72)
            ICNT=ICNT+1
         ENDDO
         TITLE(ICNT)=ARMES
         ICNT=ICNT+1
         TITLE(ICNT)=MS2
 
         CALL HEADER(TITLE,ICNT,0)  !0=left justify 1=center 2=right
         CALL AXESTITLES(XTITLE,YTITLE,270,' ',0)

         DO II=1,6
            LMSG(II)=' '
         ENDDO
         LMSG(I+1)=MSG(I)
 
         CALL SETLABEL (LMSG,6,1,2) ! 6=# of symbols 1=verticle 2=position east
         CALL SETACTIVESET(I)
 
         X(NPTS+1) = 0.0
         X(NPTS+2) = 1.0
         Y(NPTS+1) = 0.0
         Y(NPTS+2) = 1.0
         CALL LINE(X,Y,NPTS,1,1,I)   !Plot the curve
         IF (I .LT. 5) THEN
            CALL XRTPAGE(STATUS)       !Advance page
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
100      continue

C        Rasterize plot file...
         CALL PLOT(0.,0.,999)
         CLOSE(UNIT=9,STATUS='KEEP')   !ALLAN RUNKLE'S RASTER-SCAN-CONVERTER
         RETURN

      END



      SUBROUTINE LABPROC(IUNI,LABEL,NLAB)

         IMPLICIT NONE
 
         INTEGER INSTANCES(20),STAT,NLAB,IUNI,CNT,I,J

         CHARACTER*8 TASKS(20)
         CHARACTER*4320 LABEL
         CHARACTER*72 MSG
         CHARACTER*12 UNAME
         CHARACTER*28 TIME
         CHARACTER*65 HBUF
 
         hbuf='----TASK:------------USER:'
         MSG= ' '
         LABEL= ' '
         CALL VIC1LAB(IUNI,STAT,NLAB,LABEL,60)
         CNT=20                             !EXTRACTS VIC*2 LAB
         CALL XLHINFO(IUNI,TASKS,INSTANCES,CNT,STAT,' ')
         DO 801 J=1,CNT
            UNAME=' '
            TIME =' '
            CALL XLGET(IUNI,'HISTORY','USER',UNAME,STAT,'HIST',TASKS(J),
     &                 'INSTANCE',INSTANCES(J),'FORMAT','STRING',' ')
            CALL XLGET(IUNI,'HISTORY','DAT_TIM',TIME,STAT,'HIST',
     &                 TASKS(J),'INSTANCE',INSTANCES(J),'FORMAT',
     &                 'STRING',' ')
            DO 802 I=1,8                             !BLANKS NULLS
802            IF (TASKS(J)(I:I) .LT. '0') TASKS(J)(I:I)=' '
            HBUF(11:18)  = TASKS(J)(1:8)
            HBUF(28:39) = UNAME(1:12)
            HBUF(40:64) = TIME(1:25)
801      LABEL(1+(NLAB+J-1)*72:65+(NLAB+J-1)*72) =  HBUF(1:65)
         NLAB=NLAB+CNT
         DO 800 I=1,NLAB
            MSG = LABEL(1+(I-1)*72:1+(I-1)*72+71)
            CALL XVMESSAGE(MSG,' ')
800      MSG=' '
         RETURN
      END


