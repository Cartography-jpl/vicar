C      PROGRAM  AVERAGE
C#######################################################################
C  NAME OF ROUTINE
C      "AVERAGE"  ( AVERAGE images )
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JULY 1983
C  ENVIRONMENT
C      UNIX or VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C     3-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C  CALLING SEQUENCE (TAE COMMAND LINE)
C      The following command line formats show the major allowable forms:
C      average INP=(a...) OUT=b SIZE=(sl,ss,nl,ns) optional parameters
C      average INP=(a...) OUT=b SL=sl SS=ss NL=nl NS=ns optional parameters
C      average (a...) b (sl,ss,nl,ns) optional parameters
C      average (a...) b optional parameters
C
C       Here (a...) represents a list of 2 to 48 file names.
C       b represents an output image file name.
C
C  INPUT PARAMETERS (listed by keyword)
C      INP    - Input file names.
C      OUT    - Output file names.
C      SIZE   - Standard Vicar size field:  (SL,SS,NL,NS)
C               SL = Starting line number.
C               SS = Starting sample number.
C               NL = Number of lines.
C               NS = Number of samples.
C               The same SIZE parameters apply to each of the input
C               image files.
C  SUBROUTINES CALLED
C      MAIN44 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C#######################################################################
C  NAME OF ROUTINE
C      MAIN44   (name of top level subroutine by VICAR convention)
C      
C  CONVERTED FOR USE ON MIPL SYSTEM BY   
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION     JULY 1983
C  CALLING SEQUENCE
C      Standard subroutine call and return.
C  INPUT AND OUTPUT PARAMETERS     
C      SEE UNDER PROGRAM AVERAGE.
C      
C  CALLED BY
C      "average"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT INTEGER (A-Z)
      INCLUDE 'fortport'
      INTEGER TEMP
      BYTE IBUF(120000),OBUF(20000)
      INTEGER IDSN(48), IS(48)
C
C
C
C=================START OF EXECUTABLE CODE===============================     
C
      CALL IFMESSAGE ('AVERAGE version 02-MAY-94')
      CALL XVEACTION ('SA', ' ')
      CALL XVPCNT( 'INP', NI )

         CALL XVUNIT( IDSN(1), 'INP', 1, IND, ' ' )
         CALL XVOPEN( IDSN(1), IND, 'OP', 'READ', ' ' )
         CALL XVSIZE( SL, SS, NL, NS, NLI, NSI )   ! GET SIZE PARAMETER.
C
C  OPEN DATA SETS
      DO I=2,NI
         CALL XVUNIT( IDSN(I), 'INP', I, IND, ' ' )
         CALL XVOPEN( IDSN(I), IND, 'OP', 'READ', ' ' )
      END DO

         CALL XVUNIT( OUTFILE, 'OUT', 1, IND, ' ' )
         CALL XVOPEN( OUTFILE, IND, 'OP', 'WRITE', 
     .         'U_NL', NL, 'U_NS', NS, ' ' )
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCC  MAIN LOOP

      EL = SL+NL-1

      DO 491 II=SL,EL           ! LOOP OVER LINES.
              IF ( II .EQ. SL )   THEN
                 ISP = 1
                 DO I = 1, NI

                  CALL XVREAD( IDSN(I), IBUF(ISP), IND, 'LINE', 
     .                  SL, ' ' )
                  ISP = ISP + NSI
                 END DO
              ELSE
                 ISP = 1
                 DO I = 1, NI

                  CALL XVREAD( IDSN(I), IBUF(ISP), IND, ' ' )
                  ISP = ISP + NSI
                 END DO
              END IF
                 IS(1) = SS
                 DO I = 2,NI
                    IS(I) = IS(I-1) + NSI
                 END DO

                 DO  J = 1, NS              ! THE SAMPLE LOOP.
                     TEMP = 0
                     DO I = 1, NI
                        TEMP = TEMP + BYTE2INT( IBUF(  IS(I) ) )
                        IS(I) = IS(I) + 1
                     END DO
                     TEMP = TEMP / NI              ! AVERAGE
                     OBUF(J) = INT2BYTE(TEMP)      ! LOW ORDER BYTE.   
                 END DO

                 CALL XVWRIT( OUTFILE, OBUF, IND, ' ' )

491   CONTINUE

      RETURN          ! NORMAL END.
      END

