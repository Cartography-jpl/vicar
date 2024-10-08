       PROGRAM  ASTRTCHR
C#######################################################################
C  NAME OF ROUTINE
C      astrtchr ( Automatic STReTCH Real )
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      astrtchr is a VICAR applications program which performs automatic
C      linear stretches on floating point and fullword integer pictures.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    JANUARY 1984
C  FOR
C      EARTH RESOURCES APPLICATIONS
C
C  ORIGINAL ASTRTCHR PROGRAM BY
C      J. J. LORRE
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     1-84  SP   CONVERTED FROM IBM VICAR VERSION: CHANGED FROM PLI TO 
C                FORTRAN-77 AND MISCELLANEOUS CLEANUP.
C     1-84  SP   ADDED LABEL PROCESSING OF INPUT FILE BY USING LABELC
C                TO FIND THE DATA FORMAT AND USING FORMAT IN LABEL AS DEFAULT.
C     1-84  SP   ADDED INFMT AND OUTFMT PARAMETERS TO ALLOW USER TO OVERRIDE
C                IMAGE FORMAT IN VICAR LABEL.
C     1-84  SP   USED STACKA FOR DYNAMIC MEMORY ALLOCATION.
C     1-84  SP   MODIFIED SO THAT RDCHEK & WRCHEK USED FOR I/O ERROR MESSAGES.
C     1-84  SP   CHANGED LABEL PROCESSING OF OUTPUT FILE TO NOT USE JJLLAB.
C     1-84  SP   CHANGED OUTMIN FROM -32766 TO -32768 & OUTMAX FROM 32766 TO
C                32767.
C     1-84  SP   REMOVED CHECKING OF AREA & PROCESS PARAMETERS AGAINST IMAGE
C                SIZE IN VICAR LABEL TO ALLOW USER TO OVERRIDE SIZE IN LABEL.
C     1-84  SP   SIMPLIFIED COMPUTATION OF NL AND NS FOR OUTPUT IMAGE LABEL.
C     1-84  SP   LENGTHENED SOME KEYWORDS: LIMI -> LIMITS,...
C     1-84  SP   CORRECTED ROUNDING OF NEGATIVE OUTPUT DNS TO ROUND TO
C                NEAREST INTEGER. (USED NINT FUNCTION.)
C     1-84  SP   CORRECTED FORMULA FOR FINDING VALLHI FROM
C                I=(1.0-PERHI)*N+1.5  TO  I=(1.0-PERHI)*N+.5
C     5-84  SP   INCREASED BUFFER SIZE FOR LABELC SINCE NLRECOUT NOW IS 0.
C    10-84  SP   CHANGED TO PERFORM RANGE CHECKING ON OUTPUT DNS BEFORE
C                CONVERTING TO INTEGER TO PREVENT INTEGER OVERFLOWS.
C     2-85  JES  CORRECTED CODE THAT CHECKS OUTPUT LIMITS SO THAT ACTUAL
C                "LIMITS" ARE USED, NOT THE LIMIT RANGE OF DATA FORMAT.
C                CORRECTIONS WERE IN THE "LINE-BY-LINE STRETCH LOOP"
C     4-85  JRH  CONVERTED TO VICAR2.
C      		 REPLACED THE PROCESS PARAMETER WITH THE SIZE PARAMETER.
C    		 CHANGED INFMT TO IFORMAT AND OUTFMT TO OFORMAT.
C                FOR IFORMAT, CHANGED THE KEYWORD FROM INTEGER TO FULL.
C                CORRECTED FORMULA FOR FINDING VALLHI ON FULLWORD DATA
C                FROM I=(1.0-PERHI)*N+1.5  TO  I=(1.0-PERHI)*N+.5 
C     4-87  SP   CORRECTED INTEGER OVERFLOW PROBLEM AGAIN.  (THE ABOVE CHANGE
C                FROM 2-85 REMOVED THE CHANGE FROM 10-84.)  ADDED NOCLIP
C                PARAMETER TO ALLOW OUTPUT DN RANGE TO BE HANDLED AS BEFORE
C                2-85.  OUTPUT DN RANGE DEFAULTS TO THE LIMITS PARAMETER AS IT
C                HAS SINCE 2-85.
C     4-87  SP   CORRECTED PROBLEM OF STRETCH PARAMETERS NOT GOING INTO LABEL.
C                (CORRECTED LENGTH OF STRING IN MVL CALL.)
C     5-92  LWK  ADDED 'STREXCL' AND 'REPLACE' PARAMETERS FOR USE IN NIMS
C                PROCESSING
C     1-94  JFM  ADDED TO VICAR HISTORY LABEL THE PDS STANDARD KEYWORDS
C		 STRETCH_MINIMUM AND STRETCH_MAXIMUM AS AN ALTERNATE WAY OF
C		 EXPRESSING INPUT AND OUTPUT STRETCH LIMITS.
C	         (FR 82901 FOR GLL NIMS)	    
C     4-94  CRI  MSTP (S/W CONVERSION) VICAR PORTING
C     8-03  NTT  ADDED 3D IMAGE CAPABILITY, ADDED ORGANIZATION HANDLING
C                BIP FILES ARE NOW PROHIBITED
C
C    CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C
C      astrtchr INP=a OUT=b optional parameters
C      astrtchr a b optional parameters
C
C       Here 'a' represents the input image file name,
C       'b' represents the output image file name.
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file names.
C      OUT    - Output file name.
C      SIZE   - Standard Vicar size field:  
C               SIZE field in terms of fullword pixels for performing stretch.
C      AREA   - SIZE field in terms of fullword pixels for sampling pixels to
C               determine range of input DNs.
C      IFORMAT- The input data format -- REAL or FULL.
C      OFORMAT- The output data format -- BYTE or HALF. 
C      LIMITS - Lower and upper stretch limits (output DNs).
C      NOCLIP - Causes the range of output DNs to be limited by the output data
C               format instead of by the LIMITS parameter.
C      PERCENT- Percentage of sampled pixels to be stretched to or beyond the
C               stretch limits.
C      HPERCENT-Percentage of sampled pixels to be stretched to or beyond the
C               upper stretch limit.
C      LPERCENT-Percentage of sampled pixels to be stretched to or beyond the
C               lower stretch limit.
C      SORT   - The maximum number of pixels which will be sampled in the
C               area specified for AREA.
C      SINC   - If SINC=n, then output file will contain just every nth pixel 
C               in each line.
C      LINC   - If LINC=n, then output file will contain just every nth line.
C      EXCL   - Intervals of input DNs to be excluded from the list of
C               sampled pixels.
C      PRINT  - To print the sorted list of DNs of sampled pixels.
C  OUTPUT PARAMETERS
C      The output image produced is written to the output file.
C  PROGRAM LIMITATIONS
C      1. The input image must be REAL*4 or fullword (INTEGER*4) data.
C      2. The output image must be byte or halfword data.
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      INCLUDE 'VICMAIN_FOR'
C
      SUBROUTINE MAIN44 
C
C#######################################################################
C  NAME OF ROUTINE
C     MAIN44 (name for top level subroutine by VICAR convention)
C
C  PURPOSE
C      MAIN44 processes parameters entered by user and calls STRETCHIT
C      via STACKA to perform stretch.
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JANUARY 1984
C  FOR
C      EARTH RESOURCES APPLICATIONS
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT AND OUTPUT PARAMETERS     
C      SEE UNDER PROGRAM ASTRTCHR.
C      
C  CALLED BY
C      ASTRTCHR
C  SUBROUTINES CALLED
C      The library routines  ABEND, STACKA.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C    17 JULY 79   ...JJL...    INITIAL RELEASE
c    01jul04  -lwk-  added initialization of NOTIORR flag, which causes 
c		problems in Linux
C
      IMPLICIT NONE
      EXTERNAL STRETCHIT
C
C   PARAMETERS
C
      INTEGER      INTCODE_PAR, REALCODE_PAR
      PARAMETER    (INTCODE_PAR = 4)     ! FORMAT CODE FOR INTEGER*4
      PARAMETER    (REALCODE_PAR = 7)    ! FORMAT CODE FOR REAL*4
C
C   GLOBAL VARIABLES
C
      INTEGER*4    EVALS, INCS, INCL, INCODE, LIMLO, LIMHI, NPIXEL
      INTEGER*4    NPIXOUT, NSORT, OUTMIN, OUTMAX, INUNIT, OUTUNIT
      INTEGER*4    STATUS, SLW, SSW, NLW, NSW, SB, NB
      INTEGER*4    AREA(4)
      REAL*4       EXCL(600)
      REAL*4       PERHI, PERLO, REPVAL
      CHARACTER*4  OUTFMT
      LOGICAL      INREAL, IPRNT, OUTHALF, EXCLUD
      COMMON /C1/  AREA, EVALS, INCS, INCODE, INCL, INREAL, IPRNT, 
     +             LIMLO, LIMHI, NPIXEL, NPIXOUT, NSORT, OUTMIN, 
     +             OUTMAX, OUTHALF, PERHI, PERLO, SLW, SSW, NLW, NSW,
     +             INUNIT, OUTUNIT, OUTFMT, EXCL, EXCLUD, REPVAL,
     +             SB, NB
C
C    LOCAL VARIABLES
C
      INTEGER*4    ICNT, ISIZE, NL, NS, NNLI, NNSI, NBI
      INTEGER*4    LIMT(2)
      REAL*4       PERC
      CHARACTER*4  INFMT
      CHARACTER*32 FORMT
      CHARACTER*80 MSG1
      LOGICAL      XVPTST
      LOGICAL      ABORTFLAG, NOTIORR
      CHARACTER*3  ORGIN
C
C
C=================START OF EXECUTABLE CODE===============================
C
C
C  OPEN THE INPUT
C
      CALL IFMESSAGE('ASTRTCHR version 02-MAY-94')
      CALL XVEACTION('SA',' ')
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,' ')
      CALL XVGET(INUNIT,STATUS,'NL',NL,'NS',NS,'FORMAT',FORMT,' ')

c     Check organization of image, prohibit BIP
      CALL XVGET(INUNIT,STATUS,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')

C
C  CHECK IF THE INPUT IMAGE IS REAL OR FULLWORD DATA.
C
      NOTIORR=.FALSE.
      IF (FORMT.EQ.'FULL') THEN
           INREAL = .FALSE.
           INCODE = INTCODE_PAR
      ELSE IF (FORMT.EQ.'REAL') THEN
           INREAL = .TRUE.
           INCODE = REALCODE_PAR
      ELSE
           NOTIORR=.TRUE.
      END IF
C
C  NOW LOOK AT PARAMETERS ENTERED BY USER.
C
      CALL XVP('IFORMAT',INFMT,ICNT)
      IF ((ICNT.EQ.0).AND.NOTIORR) THEN
         GOTO 5700
      ELSE IF ((ICNT.NE.0).AND.(INFMT.EQ.'FULL')) THEN
         INREAL = .FALSE.
         INCODE = INTCODE_PAR
      ELSE IF ((ICNT.NE.0).AND.(INFMT.EQ.'REAL')) THEN
         INREAL = .TRUE.
         INCODE = REALCODE_PAR
      ENDIF
C
      CALL XVP('OFORMAT',OUTFMT,ICNT)
      IF (OUTFMT.EQ.'HALF') THEN
         OUTHALF = .TRUE.
         OUTMIN = -32768
         OUTMAX =  32767
      ELSE
         OUTHALF = .FALSE.
         OUTMIN = 0
         OUTMAX = 255
      ENDIF
C
      CALL XVSIZE(SLW,SSW,NLW,NSW,NNLI,NNSI)
      CALL XVBANDS(SB, NB, NBI)

      IF ( SB .GT. NBI ) CALL MABEND(
     +  'SB is greater than the total number of bands')

      IF ( SB + NB - 1 .GT. NBI) THEN
         CALL XVMESSAGE('***Number of bands truncated', ' ') 
         NB = NBI + 1 - SB
      ENDIF

C
      CALL XVP('AREA',AREA,ICNT)
      IF (ICNT.EQ.0) THEN
         AREA(1) = 1
         AREA(2) = 1
         AREA(3) = NL
         AREA(4) = NS
      ENDIF
C
      CALL XVP('LIMITS',LIMT,ICNT)
      IF (ICNT.NE.0) THEN
         LIMLO = LIMT(1)
         LIMHI = LIMT(2)
      ELSE IF (OUTHALF) THEN
         LIMLO = 0
         LIMHI = 10000
      ELSE
         LIMLO = 0
         LIMHI = 255
      END IF
C
      CALL XVP('PERCENT',PERHI,ICNT)
      PERHI = PERHI/(100.0*2.0)
      PERLO = PERHI
C
      CALL XVP('HPERCENT',PERC,ICNT)
      IF (ICNT.NE.0) PERHI = PERC/100.0
C
      CALL XVP('LPERCENT',PERC,ICNT)
      IF (ICNT.NE.0) PERLO = PERC/100.0
C
      CALL XVP('SORT',NSORT,ICNT)
C
      CALL XVP('SINC',INCS,ICNT)
C
      CALL XVP('LINC',INCL,ICNT)
C
      CALL XVP('EXCL',EXCL,EVALS)
      IF (MOD(EVALS,2).NE.0) GO TO 6300
C
      EXCLUD = XVPTST('STREXCL')
      CALL XVP( 'REPLACE', REPVAL, ICNT)
C
      IPRNT = XVPTST('PRINT')
C
      IF (LIMLO.LT.OUTMIN .OR. LIMLO.GT.OUTMAX) GOTO 6400
      IF (LIMHI.LT.OUTMIN .OR. LIMHI.GT.OUTMAX) GOTO 6400
C
      IF ( .NOT. XVPTST('NOCLIP') )   THEN
           OUTMIN = LIMLO                      ! DEFAULT IS TO USE LIMLO
           OUTMAX = LIMHI                      ! AND LIMHI AS OUTPUT DN RANGE.
      END IF
C
      CALL XVMESSAGE (' ',' ')
      WRITE (MSG1,5000) FLOAT(LIMLO)
5000  FORMAT ('LOWER OUTPUT LIMIT = ',E10.4)
      CALL XVMESSAGE(MSG1,' ')
C
      WRITE (MSG1,5100) FLOAT(LIMHI)
5100  FORMAT ('UPPER OUTPUT LIMIT = ',E10.4)
      CALL XVMESSAGE(MSG1,' ')
C
C  CALL SPECIAL LIBRARY SUBROUTINE STACKA TO ALLOCATE THE NECESSARY BUFFERS
C  AND TO CALL SUBROUTINE STRETCHIT.
C
      NPIXEL = MAX0(AREA(4), NSW)
      ISIZE = 4*NPIXEL
      NPIXOUT = (NSW + INCS - 1) / INCS
C
      CALL STACKA(6, STRETCHIT, 3, ISIZE, NSORT*4, 2*NPIXOUT, ABORTFLAG)
C
      IF (ABORTFLAG) GOTO 7000      ! DID STRETCHIT HAVE AN ERROR IN
                                    ! EXECUTING SUBROUTINE STRETCHIT.
      CALL XVCLOSE(INUNIT,STATUS,' ')   ! IF NO ERROR, THEN DONE.
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN
C
C
CCC ERROR PROCESSING
C
5700  CONTINUE
      CALL XVMESSAGE('INPUT MUST BE FULLWORD OR REAL*4 DATA',' ')
      GOTO 7100            ! CALL ABEND 
C
6300  CONTINUE
      CALL XVMESSAGE(
     .  'ERROR: ODD NUMBER OF EXCL PARAMETER VALUES ENTERED',' ')
      GOTO 7100            ! CALL ABEND 
C
6400  CONTINUE
      CALL XVMESSAGE('ERROR: LIMIT PARAMETER VALUE OUT OF RANGE',' ')
      GOTO 7100            ! CALL ABEND 
C
CCCCCCC
C
7000  CONTINUE
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
7100  CONTINUE
      CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL ABEND                 ! ABNORMAL END. (NO RETURN FROM ABEND.)
C
      END
C
C
      SUBROUTINE STRETCHIT(IN, INL, SORT, SORTL, OUT, OUTL, ABORTFLAG)
C#######################################################################
C  NAME OF ROUTINE
C     STRETCHIT ( STRETCH IT )
C
C  PURPOSE
C      STRETCHIT performs automatic linear stretches on floating point
C      and fullword integer pictures.
C      
C  PREPARED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JANUARY 1984
C  FOR
C      EARTH RESOURCES APPLICATIONS
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR1 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     1-84  SP   CONVERTED FROM IBM VICAR VERSION: MISCELLANEOUS CLEANUP.
C  CALLING SEQUENCE
C      CALLED VIA LIBRARY SUBROUTINE STACKA.
C  INPUT PARAMETERS 
C      IN(J)             - LINE BUFFER FOR THE INPUT IMAGE.
C       array              THE PIXEL INDEX GOES FROM 1 TO NPIXEL.
C                          IN IS AN REAL*4 ARRAY.
C      INL               - NUMBER OF BYTES ALLOCATED BY STACKA FOR IN.
C      SORT(N)           - BUFFER FOR SORTING DNS.  SORT IS A REAL*4
C       array              ARRAY.
C      SORTL             - NUMBER OF BYTES ALLOCATED BY STACKA FOR SORT.
C      OUT(J)            - LINE BUFFER FOR THE OUTPUT IMAGE.
C       array              THE PIXEL INDEX GOES FROM 1 TO NPIXOUT.
C                          OUT IS AN INTEGER*2 ARRAY.
C      OUTL              - NUMBER OF BYTES ALLOCATED BY STACKA FOR OUT.
C
C  OUTPUT PARAMETERS
C      ABORTFLAG - .TRUE. IF STRETCHIT FAILED. .FALSE. IF SUCCESSFUL. ABORTFLAG
C                  IS A LOGICAL*4 VARIABLE.
C  CALLED BY
C      STACKA
C  SUBROUTINES CALLED 
C      The library routines ISORT, PRNT, SSORT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      IMPLICIT NONE

C   GLOBAL VARIABLES
C
      INTEGER*4    EVALS, INCS, INCL, LIMLO, LIMHI, NPIXEL
      INTEGER*4    NPIXOUT, NSORT, OUTMIN, OUTMAX, INUNIT, OUTUNIT
      INTEGER*4    STATUS, SLW, SSW, NLW, NSW, SB, NB, INCODE
      INTEGER*4    AREA(4)
      REAL*4       EXCL(600)
      REAL*4       PERHI, PERLO, REPVAL
      CHARACTER*4  OUTFMT
      LOGICAL      INREAL, IPRNT, OUTHALF, REPLAC, EXCLUD
      COMMON /C1/  AREA, EVALS, INCS, INCODE, INCL, INREAL, IPRNT, 
     +             LIMLO, LIMHI, NPIXEL, NPIXOUT, NSORT, OUTMIN, 
     +             OUTMAX, OUTHALF, PERHI, PERLO, SLW, SSW, NLW, NSW,
     +             INUNIT, OUTUNIT, OUTFMT, EXCL, EXCLUD, REPVAL,
     +             SB, NB
C
C    SUBROUTINE PARAMETERS
C
      REAL*4       IN(NPIXEL), SORT(NSORT)
      INTEGER*2    OUT(NPIXOUT)
      INTEGER*4    OUTL, INL, SORTL
      LOGICAL      ABORTFLAG
C
C    LOCAL VARIABLES
C
      INTEGER*4    I, IE, IEXHI, IEXLO, INTVAL, ISKIP, J, K
      INTEGER*4    L, LINC, LINE, M, N, NLOUT, BAND
      REAL*4       FOUT,     FOUTMAX,  FOUTMIN
      REAL*4       OFFSET, REALVAL, SLOPE, VALLO, VALHI
      REAL*4	   STRETCH_MIN(2), STRETCH_MAX(2)
      CHARACTER*73 LABL
      CHARACTER*80 MSG2, MSG3, MSG4
      INTEGER*4 BANDOUT,LINEOUT
C
C     DATA STATEMENTS
C
      DATA LABL    / ' ' /
C
      EQUIVALENCE (REALVAL, INTVAL)
C
C=================START OF EXECUTABLE CODE===============================     
C
      ABORTFLAG = .FALSE.               ! INITIALIZE TO SUCCESSFUL-SO-FAR.
C
C....CHECK IF STACKA GOT ENOUGH MEMORY. 
C
      IF ( OUTL .LT. 2*NPIXOUT)  THEN
         ABORTFLAG = .TRUE.             ! INDICATE SUBROUTINE FAILURE.
         RETURN				! EXIT SUBROUTINE.
      END IF
C
C    COMPUTE GRID INTERVALS AT WHICH TO SAMPLE FOR SORTING 
C
      ISKIP = SQRT(FLOAT( AREA(3)*AREA(4) ) / NSORT) / INCS + 0.5
      IF (ISKIP .LT. 1)   ISKIP=1
      LINC = INCS*ISKIP
C
C    LOAD PIXELS INTO SORT BUFFER 'SORT'.  THE IDEA IS TO SAMPLE
C    UNIFORMLY OVER THE PICTURE AND STORE THE DNS IN THE SORT BUFFER.
C
      N=0 
      I=0
      DO BAND=SB,SB+NB-1
         DO LINE=AREA(1), AREA(1)+AREA(3)-1, LINC
            CALL XVREAD(INUNIT,IN,STATUS,'NSAMPS',AREA(4),'SAMP',
     +           AREA(2), 'LINE',LINE, 'BAND', BAND, ' ')
            DO J = 1, AREA(4), INCS
               I=I+1
               IF (I .EQ. ISKIP)   THEN 
                  N=N+1
                  SORT(N)=IN(J)
                  IF (N .GE. NSORT)  GOTO 2000
                  I=0
               END IF
            END DO
         END DO
      END DO
2000  CONTINUE
C
C    APPLY EXCLUDING IF ANY 
C
      IF (EVALS .GT. 0) THEN
          M = 0
          DO 2100  K = 1, N
             IF (INREAL) THEN
                DO IE = 1, EVALS, 2
                   IF (EXCL(IE) .LE. SORT(K) .AND.                 
     +               SORT(K) .LE. EXCL(IE+1))  GOTO 2100          
                END DO 
             ELSE                                                 
                DO IE = 1, EVALS, 2
                   IEXLO = NINT(EXCL(IE))
                   IEXHI = NINT(EXCL(IE+1))
                   REALVAL = SORT(K)
                   IF (IEXLO .LE. INTVAL .AND.         
     +               INTVAL .LE. IEXHI) GOTO 2100
                END DO		
             ENDIF      	 
             M = M + 1             ! SKIP THE BAD VALUES AND
             SORT(M) = SORT(K)     ! COPY THE GOOD VALUES
2100      CONTINUE
          N = M
          IF (N .LT. 2)   THEN 
              CALL XVMESSAGE('ENTIRE RANGE EXCLUDED',' ') 
              ABORTFLAG = .TRUE.               ! INDICATE SUBROUTINE FAILURE.
              RETURN			       ! EXIT SUBROUTINE.
          END IF
      END IF
C
      CALL XVMESSAGE (' ',' ')
      WRITE (MSG2, 2110) N
2110  FORMAT ('NUMBER OF SORTED PIXELS = ', I11)
      CALL XVMESSAGE (MSG2,' ')
C
      CALL XVMESSAGE (' ',' ')
      WRITE (MSG3, 2120) LINC
2120  FORMAT ('LINE INTERVAL =   ', I11)
      CALL XVMESSAGE (MSG3,' ')
C
      WRITE (MSG3, 2130) ISKIP*INCS
2130  FORMAT ('SAMPLE INTERVAL = ', I11)
      CALL XVMESSAGE (MSG3,' ')
C
C    PERFORM THE SORT 
C
      IF (INREAL) THEN
         CALL SSORT(SORT,1,N)
      ELSE
         CALL ISORT(SORT,1,N)
      ENDIF
      IF (IPRNT) THEN
         CALL XVMESSAGE (' ',' ')
         CALL PRNT(INCODE,N,SORT,'SORTED INTENSITIES .')
         CALL XVMESSAGE (' ',' ')
      ENDIF
C
C    COMPUTE PERCENT LIMITS 
C
      IF (INREAL) THEN
         I = PERLO*N+1.5
         VALLO = SORT(I)
         I = (1.0-PERHI)*N+.5
         IF (I .GT. N)   I=N
         VALHI = SORT(I)
      ELSE
         I = PERLO*N+1.5
         REALVAL = SORT(I)
         VALLO = INTVAL
         I = (1.0-PERHI)*N+.5
         IF (I .GT. N)   I=N
         REALVAL = SORT(I)
         VALHI = INTVAL
      ENDIF
C
      CALL XVMESSAGE (' ',' ')
      WRITE (MSG4, 2150) VALLO
2150  FORMAT ('LOWER HISTOGRAM BOUND = ', E10.4)
      CALL XVMESSAGE (MSG4,' ')
C
      WRITE (MSG4, 2160) VALHI
2160  FORMAT ('UPPER HISTOGRAM BOUND = ', E10.4)
      CALL XVMESSAGE (MSG4,' ')
C
C   COMPUTE INTENSITY TRANSFORMATION 
C
      IF (VALLO .NE. VALHI)  THEN 
          SLOPE = FLOAT(LIMHI-LIMLO)/(VALHI-VALLO)
          OFFSET = LIMHI - SLOPE*VALHI 
      ELSE
          SLOPE = 1.0 
          OFFSET = 0.0
          CALL XVMESSAGE('NO TRANSFORMATION PERFORMED',' ')
      END IF
C
C   OPEN OUTPUT FILE.
C
      NLOUT = (NLW+INCL-1)/INCL
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','U_NL',NLOUT,'U_NS',
     +            NPIXOUT,'U_NB',NB,'O_FORMAT',OUTFMT,
     +            'U_FORMAT','HALF',' ')
C
      WRITE (LABL, 2170) VALLO, VALHI, FLOAT(LIMLO), FLOAT(LIMHI)
2170  FORMAT ('INPUT LIMITS ',E10.4,' ',E10.4,'  MAP TO OUTPUT LIMITS'
     .        ,F8.1,' ',F8.1)
C
      CALL XLADD(OUTUNIT,'HISTORY','COMMENT',LABL,STATUS,'ERR_ACT',
     +           'S','FORMAT','STRING','ULEN',73,' ')
C
C
C  ADD STRETCH_MINIMUM AND STRETCH_MAXIMUM KEYWORDS TO VICAR LABEL
C
      STRETCH_MIN(1) = VALLO
      STRETCH_MIN(2) = REAL(LIMLO)
      STRETCH_MAX(1) = VALHI
      STRETCH_MAX(2) = REAL(LIMHI)
C
      CALL XLADD(OUTUNIT,'HISTORY','STRETCH_MINIMUM',STRETCH_MIN,
     + 		 STATUS,'ERR_ACT','S','FORMAT','REAL','NELEMENT',2,' ')
      CALL XLADD(OUTUNIT,'HISTORY','STRETCH_MAXIMUM',STRETCH_MAX,
     + 		 STATUS,'ERR_ACT','S','FORMAT','REAL','NELEMENT',2,' ')
C
C  STRETCHING LOOP:  STRETCH LINE BY LINE.
C
      L = 0
      FOUTMIN = OUTMIN
      FOUTMAX = OUTMAX
C
      BANDOUT=0
      DO BAND = SB, SB+NB-1
         LINEOUT=0
         BANDOUT = BANDOUT + 1
         DO LINE = SLW, SLW+NLW-1, INCL
C
            LINEOUT = LINEOUT + 1
            L = L + 1
            CALL XVREAD( INUNIT, IN, STATUS, 'NSAMPS', NSW, 'SAMP', SSW,
     +           'LINE',LINE, 'BAND', BAND, ' ')
C
            N = 0
            IF (INREAL) THEN
               DO J = 1, NSW, INCS
                 N = N + 1
                 REPLAC = .FALSE.
                 IF (EXCLUD .AND. EVALS.GT.0) THEN
                  DO IE = 1, EVALS, 2
                   IF (EXCL(IE).LE.IN(J) .AND. IN(J).LE.EXCL(IE+1)) THEN
                       FOUT = REPVAL
                       REPLAC = .TRUE.
                   ENDIF
                  END DO 
                ENDIF
                IF (.NOT.REPLAC) THEN
                    FOUT = IN(J) * SLOPE + OFFSET 
                    IF (FOUT .LT. FOUTMIN) FOUT = FOUTMIN     
                    IF (FOUT .GT. FOUTMAX) FOUT = FOUTMAX
                ENDIF
                OUT(N) = NINT(FOUT) ! NINT ROUNDS TO NEAREST INTEGER.
              END DO
C
            ELSE
              DO J = 1, NSW, INCS
                 N = N + 1
                 REALVAL = IN(J)
                 REPLAC = .FALSE.
                 IF (EXCLUD .AND. EVALS.GT.0) THEN
                    DO IE = 1, EVALS, 2
                       IEXLO = NINT(EXCL(IE))
                       IEXHI = NINT(EXCL(IE+1))
                       IF (IEXLO.LE.INTVAL .AND. INTVAL.LE.IEXHI) THEN
                          FOUT = REPVAL
                          REPLAC = .TRUE.
                       ENDIF
                    END DO 
                 ENDIF
                 IF (.NOT.REPLAC) THEN
                    FOUT = INTVAL * SLOPE + OFFSET 
                    IF (FOUT .LT. FOUTMIN) FOUT = FOUTMIN     
                    IF (FOUT .GT. FOUTMAX) FOUT = FOUTMAX
                 ENDIF
                 OUT(N) = NINT(FOUT) ! NINT ROUNDS TO NEAREST INTEGER.
              END DO
            ENDIF
C
            CALL XVWRIT(OUTUNIT,OUT,STATUS,'NSAMPS',NPIXOUT,
     +           'LINE',LINEOUT,'BAND',BANDOUT,' ') 
C
         END DO                
      END DO                 ! END OF STRETCHING LOOP.
C
      RETURN
C
C  RETURN TO MAIN44 VIA STACKA.
C
      END

