$!****************************************************************************
$!
$! Build proc for MIPL module apodize
$! VPACK Version 1.9, Friday, October 10, 2003, 12:32:43
$!
$! Execute by entering:		$ @apodize
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
$ write sys$output "*** module apodize ***"
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
$ write sys$output "Invalid argument given to apodize.com file -- ", primary
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
$   if F$SEARCH("apodize.imake") .nes. ""
$   then
$      vimake apodize
$      purge apodize.bld
$   else
$      if F$SEARCH("apodize.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake apodize
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @apodize.bld "STD"
$   else
$      @apodize.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create apodize.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack apodize.com -mixed -
	-s apodize.f -
	-i apodize.imake -
	-p apodize.pdf -
	-t tstapodiz.pdf new_3d_session.log old_3d_session.log
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create apodize.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C
C     MODIFIED FOR VAX CONVERSION BY ALAN S MAZER, 27 SEPT 1983
C     JHR: CONVERTED TO VICAR2   1 JULY 1985
C     AS(CRI): MSTP S/W CONVERSION (VICAR PORTING) 1 JULY 1994
C     ENABLED 3D IMAGE CAPABILITY (N TOOLE), 16 SEPT 2003
C
C     PUTS A SINE WAVE AROUND PICTURE EDGES TO STOP FFT2 LEAKAGE.
C     KEYWORD FOR THE EDGE IS 'EDGE', DEFAULT VALUE IS 10.
C
      REAL*4 R1(8192),R2(8192)
      INTEGER*4 OUNIT,STAT,SS,SL,SB,NBO,BAND,BANDOUT,LINEOUT,NBI
      INTEGER*2 HBUF(8192)
      CHARACTER*8 FORMAT
      CHARACTER*3 ORGIN
C
      COMMON/C1/R1,R2,HBUF
C
C        SET DEFAULTS AND INITIALIZE
      NPIX=10
C
      CALL IFMESSAGE('APODIZE version 16-SEP-03')
      CALL XVEACTION('SA',' ')
C          OPEN INPUT DATA SET
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')
C
C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      IF(FORMAT.NE.'BYTE'.AND.FORMAT.NE.'HALF') THEN
         CALL XVMESSAGE
     &        ('APODIZE ACCEPTS BYTE OR HALFWORD DATA ONLY',' ')
         CALL ABEND
      END IF

c     Check organization of image, prohibit BIP
      CALL XVGET(IUNIT,STAT,'ORG',ORGIN, ' ')
      IF (ORGIN.EQ.'BIP') CALL MABEND(
     +  'BIP files not supported, use program TRAN to convert to BSQ')

C
C        GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
      IF(SL+NLO-1 .GT. NLI) THEN
         CALL XVMESSAGE
     &        ('NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE',' ')
         CALL ABEND
      END IF
      IF(SS+NSO-1 .GT. NSI) THEN
         CALL XVMESSAGE
     &        ('NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE',' ')
         CALL ABEND
      END IF
      IF(NSO .GT. 8192) THEN
         CALL XVMESSAGE
     &         ('NUMBER OF SAMPLES EXCEEDS BUFFER SIZE OF 8192',' ')
         CALL ABEND
      END IF

      CALL XVBANDS(SB,NBO,NBI)

      IF ( SB .GT. NBI ) CALL MABEND(
     +  'SB is greater than the total number of bands')
                 
      IF ( SB + NBO - 1 .GT. NBI) THEN
         CALL XVMESSAGE('***Number of bands truncated', ' ')
         NBO = NBI + 1 - SB     
      ENDIF

C
C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_FORMAT','HALF',
     &            'U_NL',NLO,'U_NS',NSO,'U_NB',NBO,' ')
C
C           PROCESS PARAMETERS
C        'EDGE'
      CALL XVPARM('EDGE',NPIX,ICOUNT,IDEF,1)
C      
C        SETUP FOR MAIN PROCESSING
      P=NPIX*2
      RAT=3.14159/P
      II=NPIX*2
C
C        PROCESS LEFT AND RIGHT BORDERS
      BANDOUT = 0
      DO 35 BAND = SB,NBO+SB-1
       BANDOUT = BANDOUT + 1
       LINEOUT = 0
       DO 40 L=1,NLO
        LINE=L+SL-1
        LINEOUT = LINEOUT + 1
        CALL XVREAD(IUNIT,HBUF,STAT,'LINE',LINE,'BAND',
     &              BAND,'SAMP',SS,'NSAMPS',NSO,' ')
        RR=HBUF(NPIX)
        RL=HBUF(NSO-NPIX+1)
        DIFF1=RL-RR
        DIFF=ABS(DIFF1)/2.0
        A=1.0
        IF(DIFF.GT.1.0E-20) A=-DIFF1/ABS(DIFF1)
        DO 50 M=1,II
           LN=M-NPIX
           IF(LN.LE.0) THEN
              HBUF(NSO+LN)=DIFF*SIN(RAT*LN*A)+AMIN1(RL,RR)+DIFF+0.5
           ELSE
              HBUF(LN)=DIFF*SIN(RAT*LN*A)+AMIN1(RL,RR)+DIFF+0.5
           END IF
50      CONTINUE
        CALL XVWRIT(OUNIT,HBUF,STAT,'NSAMPS',NSO,'LINE',LINEOUT,
     +             'BAND',BANDOUT,' ')
40     CONTINUE
35    CONTINUE
C
C        RE-OPEN OUTPUT FOR UPDATE
      CALL XVCLOSE(OUNIT,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','UPDATE','U_FORMAT','HALF',' ')
C
C        PROCESS TOP AND BOTTOM BORDERS
      DO 5 BAND=1,NBO
       LINE=NPIX 
      CALL XVREAD(OUNIT,HBUF,STAT,'LINE',LINE,'BAND',BAND,
     &            'SAMP',SS,'NSAMPS',NSO,' ')
      DO 71 J=1,NSO
         R1(J)=HBUF(J)
71    CONTINUE
      LINE=NLO-NPIX+1
      CALL XVREAD(OUNIT,HBUF,STAT,'LINE',LINE,'BAND',BAND,
     &            'SAMP',SS,'NSAMPS',NSO,' ')
      DO 70 J=1,NSO
         R2(J)=HBUF(J)
70    CONTINUE
      DO 10 L=1,II
        LN=NPIX-L
        DO 20 M=1,NSO
           DIFF1=R2(M)-R1(M)
           DIFF=ABS(DIFF1)/2.0
           A=1.0
           IF(DIFF.GT.1.0E-20) A=DIFF1/ABS(DIFF1)
           HBUF(M)=DIFF*SIN(RAT*LN*A)+AMIN1(R2(M),R1(M))+DIFF+0.5
20      CONTINUE
        IF(LN.GE.0) THEN
           LINE=NLO-LN
        ELSE
           LINE=IABS(LN)
        END IF
        CALL XVWRIT(OUNIT,HBUF,STAT,'LINE',LINE,'NSAMPS',NSO,
     +         'BAND',BAND,' ')
10     CONTINUE
5     CONTINUE
C
C        CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
C
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create apodize.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM apodize

   To Create the build file give the command:

		$ vimake apodize			(VMS)
   or
		% vimake apodize			(Unix)


************************************************************************/


#define PROGRAM	apodize
#define R2LIB

#define MODULE_LIST apodize.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create apodize.pdf
process help=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM SB TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM NB TYPE=INTEGER DEFAULT=0
PARM EDGE TYPE=INTEGER DEFAULT=10
END-PROC
.TITLE
"apodize"
.HELP
PURPOSE:
"apodize" modifies picture borders so that the Fourier transform of the
picture will be free of spikes through the zero-frequency axis, allowing
filters to operate without ringing on the picture peripheries.

 
EXECUTION:

Examples

apodize IN OUT

This command will perform the operation on input image IN, sending the output
to image OUT.  The input image is assumed to be in byte format; a ten-pixel
border will be used. 

apodize IN OUT EDGE=3

This command is similar, but a three-pixel border is used.

OPERATION:
The Fourier transform considers the input picture to really be an infinite
mosaic (checkerboard) of identical pictures.  If the picture borders do not
match but cause abrupt DN displacement, this information is included in the
transform as a broad response on a line through DC both vertically and
horizontally.  When restoration filters are applied, this information (which
has not partaken of the image degradation in question) is altered.  The inverse
Fourier transform no longer contains the information needed to represent a
clean picture border and ringing is the result. 

"apodize" fits a sine wave across the picture border so that abrupt transitions
in the mosaic become continuous.  The EDGE parameter controls the wavelength
such that lambda = 4 * EDGE.  All information within the border strip is lost. 


WRITTEN BY:  J. J. Lorre, 20 May 1974
COGNIZANT PROGRAMMER:  A. S. Mazer
REVISION:  New
Made portable for UNIX    A. Scop (CRI), 1 July 1994
Enabled for 3D images     N. Toole      16 Sept 2003

.LEVEL1
.VARIABLE INP
STRING - Input image file
.VARIABLE OUT
STRING - Output image file
.VARIABLE SIZE
INTEGER - Standard VICAR size field
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE SB
INTEGER - Starting band
.VARIABLE NS
INTEGER - Number of lines
.VARIABLE NL
INTEGER - Number of samples
.VARIABLE NB
INTEGER - Number of bands
.VARIABLE EDGE
INTEGER - Border width
.LEVEL2
.VARIABLE EDGE
EDGE is an integer specifying the pixel-width of the border within the picture
the user wishes to modify. 
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstapodiz.pdf
procedure
refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
! TEST SCRIPT FOR APODIZE
!
!DCL C132
!
! RUN APODIZE ON OUTPUT OF GEN 15 15
!
gen G1515 NL=15 NS=15
apodize G1515 G EDGE=3
list G
!
! RUN APODIZE ON OUTPUT OF GEN 14 14 'HALF
!
gen GH1414 NL=14 NS=14 'HALF
apodize GH1414 G3 EDGE=2
list G3
!
! Test 3d Image
!
gen GH14143D 14 14 3 'HALF
apodize GH14143D G33D EDGE=2 
list G33D
!
!DCL C80
!
! END-SCRIPT
!
end-proc
$!-----------------------------------------------------------------------------
$ create new_3d_session.log
tstapodiz
gen G1515 NL=15 NS=15
Beginning VICAR task gen
GEN Version 6
GEN task completed
apodize G1515 G EDGE=3
Beginning VICAR task apodize
APODIZE version 16-SEP-03
list G
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:42:35 2003
 Task:APODIZE   User:ntt       Date_Time:Wed Oct  8 16:42:35 2003
     Samp     1       3       5       7       9      11      13      15
   Line
      1      10   8   7   8   9  10  11  12  13  14  15  16  16  14  12
      2       8   6   5   6   7   8   9  10  11  12  13  14  14  12  10
      3       7   5   4   5   6   7   8   9  10  11  12  13  13  11   9
      4       8   6   5   6   7   8   9  10  11  12  13  14  14  12  10
      5       9   7   6   7   8   9  10  11  12  13  14  15  15  13  11
      6      10   8   7   8   9  10  11  12  13  14  15  16  16  14  12
      7      11   9   8   9  10  11  12  13  14  15  16  17  17  15  13
      8      12  10   9  10  11  12  13  14  15  16  17  18  18  16  14
      9      13  11  10  11  12  13  14  15  16  17  18  19  19  17  15
     10      14  12  11  12  13  14  15  16  17  18  19  20  20  18  16
     11      15  13  12  13  14  15  16  17  18  19  20  21  21  19  17
     12      16  14  13  14  15  16  17  18  19  20  21  22  22  20  18
     13      16  14  13  14  15  16  17  18  19  20  21  22  22  20  18
     14      14  12  11  12  13  14  15  16  17  18  19  20  20  18  16
     15      12  10   9  10  11  12  13  14  15  16  17  18  18  16  14
gen GH1414 NL=14 NS=14 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
apodize GH1414 G3 EDGE=2
Beginning VICAR task apodize
APODIZE version 16-SEP-03
list G3
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:42:35 2003
 Task:APODIZE   User:ntt       Date_Time:Wed Oct  8 16:42:36 2003
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14
   Line
      1         6     4     5     6     7     8     9    10    11    12    13    14    13    10
      2         4     2     3     4     5     6     7     8     9    10    11    12    11     8
      3         5     3     4     5     6     7     8     9    10    11    12    13    12     9
      4         6     4     5     6     7     8     9    10    11    12    13    14    13    10
      5         7     5     6     7     8     9    10    11    12    13    14    15    14    11
      6         8     6     7     8     9    10    11    12    13    14    15    16    15    12
      7         9     7     8     9    10    11    12    13    14    15    16    17    16    13
      8        10     8     9    10    11    12    13    14    15    16    17    18    17    14
      9        11     9    10    11    12    13    14    15    16    17    18    19    18    15
     10        12    10    11    12    13    14    15    16    17    18    19    20    19    16
     11        13    11    12    13    14    15    16    17    18    19    20    21    20    17
     12        14    12    13    14    15    16    17    18    19    20    21    22    21    18
     13        13    11    12    13    14    15    16    17    18    19    20    21    20    17
     14        10     8     9    10    11    12    13    14    15    16    17    18    17    14
gen GH14143D 14 14 3 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
apodize GH14143D G33D EDGE=2
Beginning VICAR task apodize
APODIZE version 16-SEP-03
list G33D
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:42:36 2003
 Task:APODIZE   User:ntt       Date_Time:Wed Oct  8 16:42:36 2003
 ***********
 Band =     1
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14
   Line
      1         6     4     5     6     7     8     9    10    11    12    13    14    13    10
      2         4     2     3     4     5     6     7     8     9    10    11    12    11     8
      3         5     3     4     5     6     7     8     9    10    11    12    13    12     9
      4         6     4     5     6     7     8     9    10    11    12    13    14    13    10
      5         7     5     6     7     8     9    10    11    12    13    14    15    14    11
      6         8     6     7     8     9    10    11    12    13    14    15    16    15    12
      7         9     7     8     9    10    11    12    13    14    15    16    17    16    13
      8        10     8     9    10    11    12    13    14    15    16    17    18    17    14
      9        11     9    10    11    12    13    14    15    16    17    18    19    18    15
     10        12    10    11    12    13    14    15    16    17    18    19    20    19    16
     11        13    11    12    13    14    15    16    17    18    19    20    21    20    17
     12        14    12    13    14    15    16    17    18    19    20    21    22    21    18
     13        13    11    12    13    14    15    16    17    18    19    20    21    20    17
     14        10     8     9    10    11    12    13    14    15    16    17    18    17    14


 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:42:36 2003
 Task:APODIZE   User:ntt       Date_Time:Wed Oct  8 16:42:36 2003
 ***********
 Band =     2
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14
   Line
      1         7     5     6     7     8     9    10    11    12    13    14    15    14    11
      2         5     3     4     5     6     7     8     9    10    11    12    13    12     9
      3         6     4     5     6     7     8     9    10    11    12    13    14    13    10
      4         7     5     6     7     8     9    10    11    12    13    14    15    14    11
      5         8     6     7     8     9    10    11    12    13    14    15    16    15    12
      6         9     7     8     9    10    11    12    13    14    15    16    17    16    13
      7        10     8     9    10    11    12    13    14    15    16    17    18    17    14
      8        11     9    10    11    12    13    14    15    16    17    18    19    18    15
      9        12    10    11    12    13    14    15    16    17    18    19    20    19    16
     10        13    11    12    13    14    15    16    17    18    19    20    21    20    17
     11        14    12    13    14    15    16    17    18    19    20    21    22    21    18
     12        15    13    14    15    16    17    18    19    20    21    22    23    22    19
     13        14    12    13    14    15    16    17    18    19    20    21    22    21    18
     14        11     9    10    11    12    13    14    15    16    17    18    19    18    15


 Task:GEN       User:ntt       Date_Time:Wed Oct  8 16:42:36 2003
 Task:APODIZE   User:ntt       Date_Time:Wed Oct  8 16:42:36 2003
 ***********
 Band =     3
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14
   Line
      1         8     6     7     8     9    10    11    12    13    14    15    16    15    12
      2         6     4     5     6     7     8     9    10    11    12    13    14    13    10
      3         7     5     6     7     8     9    10    11    12    13    14    15    14    11
      4         8     6     7     8     9    10    11    12    13    14    15    16    15    12
      5         9     7     8     9    10    11    12    13    14    15    16    17    16    13
      6        10     8     9    10    11    12    13    14    15    16    17    18    17    14
      7        11     9    10    11    12    13    14    15    16    17    18    19    18    15
      8        12    10    11    12    13    14    15    16    17    18    19    20    19    16
      9        13    11    12    13    14    15    16    17    18    19    20    21    20    17
     10        14    12    13    14    15    16    17    18    19    20    21    22    21    18
     11        15    13    14    15    16    17    18    19    20    21    22    23    22    19
     12        16    14    15    16    17    18    19    20    21    22    23    24    23    20
     13        15    13    14    15    16    17    18    19    20    21    22    23    22    19
     14        12    10    11    12    13    14    15    16    17    18    19    20    19    16
end-proc
disable-log
$!-----------------------------------------------------------------------------
$ create old_3d_session.log
tstapodiz
gen G1515 NL=15 NS=15
Beginning VICAR task gen
GEN Version 6
GEN task completed
apodize G1515 G EDGE=3
Beginning VICAR task apodize
APODIZE version 1-JUL-94
list G
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 12:11:02 2003
 Task:APODIZE   User:ntt       Date_Time:Tue Sep 16 12:11:02 2003
     Samp     1       3       5       7       9      11      13      15
   Line
      1      10   8   7   8   9  10  11  12  13  14  15  16  16  14  12
      2       8   6   5   6   7   8   9  10  11  12  13  14  14  12  10
      3       7   5   4   5   6   7   8   9  10  11  12  13  13  11   9
      4       8   6   5   6   7   8   9  10  11  12  13  14  14  12  10
      5       9   7   6   7   8   9  10  11  12  13  14  15  15  13  11
      6      10   8   7   8   9  10  11  12  13  14  15  16  16  14  12
      7      11   9   8   9  10  11  12  13  14  15  16  17  17  15  13
      8      12  10   9  10  11  12  13  14  15  16  17  18  18  16  14
      9      13  11  10  11  12  13  14  15  16  17  18  19  19  17  15
     10      14  12  11  12  13  14  15  16  17  18  19  20  20  18  16
     11      15  13  12  13  14  15  16  17  18  19  20  21  21  19  17
     12      16  14  13  14  15  16  17  18  19  20  21  22  22  20  18
     13      16  14  13  14  15  16  17  18  19  20  21  22  22  20  18
     14      14  12  11  12  13  14  15  16  17  18  19  20  20  18  16
     15      12  10   9  10  11  12  13  14  15  16  17  18  18  16  14
gen GH1414 NL=14 NS=14 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
apodize GH1414 G3 EDGE=2
Beginning VICAR task apodize
APODIZE version 1-JUL-94
list G3
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 12:11:03 2003
 Task:APODIZE   User:ntt       Date_Time:Tue Sep 16 12:11:03 2003
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14
   Line
      1         6     4     5     6     7     8     9    10    11    12    13    14    13    10
      2         4     2     3     4     5     6     7     8     9    10    11    12    11     8
      3         5     3     4     5     6     7     8     9    10    11    12    13    12     9
      4         6     4     5     6     7     8     9    10    11    12    13    14    13    10
      5         7     5     6     7     8     9    10    11    12    13    14    15    14    11
      6         8     6     7     8     9    10    11    12    13    14    15    16    15    12
      7         9     7     8     9    10    11    12    13    14    15    16    17    16    13
      8        10     8     9    10    11    12    13    14    15    16    17    18    17    14
      9        11     9    10    11    12    13    14    15    16    17    18    19    18    15
     10        12    10    11    12    13    14    15    16    17    18    19    20    19    16
     11        13    11    12    13    14    15    16    17    18    19    20    21    20    17
     12        14    12    13    14    15    16    17    18    19    20    21    22    21    18
     13        13    11    12    13    14    15    16    17    18    19    20    21    20    17
     14        10     8     9    10    11    12    13    14    15    16    17    18    17    14
gen GH14143D 14 14 3 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
apodize GH14143D G33D EDGE=2
Beginning VICAR task apodize
APODIZE version 1-JUL-94
[VIC2-GENERR] Exception in XVREAD, processing file: GH14143D
[VIC2-STRTREC] Bad starting record for read or write operation; program error.
 Current line in image = 0
 ** ABEND called **
continue
list G33D
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:ntt       Date_Time:Tue Sep 16 12:11:03 2003
 Task:APODIZE   User:ntt       Date_Time:Tue Sep 16 12:11:03 2003
 ***********
 Band =     1
 ***********
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14
   Line

      8         0     0     0     0     0     0     0     0     5 -6608     0     0     0     3
end-proc
disable-log
$ Return
$!#############################################################################
