$!****************************************************************************
$!
$! Build proc for MIPL module boxflt2
$! VPACK Version 1.7, Friday, April 08, 1994, 11:01:33
$!
$! Execute by entering:		$ @boxflt2
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
$ write sys$output "*** module boxflt2 ***"
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
$ write sys$output "Invalid argument given to boxflt2.com file -- ", primary
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
$   if F$SEARCH("boxflt2.imake") .nes. ""
$   then
$      vimake boxflt2
$      purge boxflt2.bld
$   else
$      if F$SEARCH("boxflt2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake boxflt2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @boxflt2.bld "STD"
$   else
$      @boxflt2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create boxflt2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack boxflt2.com -
	-s boxflt2.f boxflt2c.c -
	-i boxflt2.imake -
	-p boxflt2.pdf -
	-t tstboxflt2.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create boxflt2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      INCLUDE 'VICMAIN_FOR'
C**********************************************************************
      SUBROUTINE MAIN44
C
C        MODIFIED FOR VAX CONVERSION BY ALAN MAZER 28-JUL-83
C        CONVERTED TO VICAR2 BY J. REIMER 14-AUG-85
C
C        9-88  SP   MODIFIED BECAUSE DIV HAS BEEN RENAMED TO DIVV.
C        4-94  CRI  MSTP S/W CONVERSION (VICAR PORTING)
C
      EXTERNAL MAIN
      COMMON /C1/ IUNIT,OUNIT,SL,SS,NLO,NSO,FORMAT,HIGH,NLW,NSW,
     &            ICYCLE,JCYCLE,IDC 

      INTEGER*4 OUNIT,STAT,SL,SS,HIGH
      CHARACTER*8 FORMAT
      LOGICAL XVPTST

C        SET DEFAULTS AND INITIALIZE
      HIGH=0
      NLW=11
      NSW=11
      ICYCLE=0
      JCYCLE=0
      IDC=128

C
      CALL IFMESSAGE('BOXFLT2 version 02-MAY-94')
C
C          OPEN INPUT DATA SET
      CALL XVEACTION('SA',' ')
      CALL XVUNIT(IUNIT,'INP',1,STAT,' ')
      CALL XVOPEN(IUNIT,STAT,'U_FORMAT','HALF',' ')
C
C        GET DATA FORMAT AND CHECK
      CALL XVGET(IUNIT,STAT,'FORMAT',FORMAT,' ')
      IF(FORMAT.NE.'BYTE'.AND.FORMAT.NE.'HALF') THEN
         CALL XVMESSAGE('BOXFLT2 ACCEPTS BYTE AND HALFWORD DATA ONLY',
     +                  ' ')
         CALL ABEND
      END IF
C
C        GET SIZE INFORMATION AND CHECK
      CALL XVSIZE(SL,SS,NLO,NSO,NLI,NSI)
      IF(SL+NLO-1 .GT. NLI) THEN
         CALL XVMESSAGE
     +        ('NUMBER OF LINES REQUESTED EXCEEDS INPUT SIZE',' ')
         CALL ABEND
      END IF
      IF(SS+NSO-1 .GT. NSI) THEN
         CALL XVMESSAGE
     +        ('NUMBER OF SAMPLES REQUESTED EXCEEDS INPUT SIZE',' ')
         CALL ABEND
      END IF
C
C        OPEN OUTPUT DATA SET
      CALL XVUNIT(OUNIT,'OUT',1,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','WRITE','U_FORMAT','HALF',
     &            'U_NL',NLO,'U_NS',NSO,' ')

C           PROCESS PARAMETERS
C        'HIGHPASS'
      IF(XVPTST('HIGHPASS')) HIGH=1
C        'NLW'
      CALL XVPARM('NLW',NLW,ICOUNT,IDEF,1)
      IF(NLW/2*2.EQ.NLW) CALL XVMESSAGE
     +                          ('WARNING-NLW IS AN EVEN INTEGER',' ')
C        'NSW'
      CALL XVPARM('NSW',NSW,ICOUNT,IDEF,1)
      IF(NSW/2*2.EQ.NSW) CALL XVMESSAGE
     +                          ('WARNING-NSW IS AN EVEN INTEGER',' ')
C        'CYCLE'
      IF(XVPTST('CYCLE')) THEN
         ICYCLE=1
         JCYCLE=1
      END IF
C        'SCYCLE'
      IF(XVPTST('SCYCLE')) ICYCLE=1
C        'LCYCLE'
      IF(XVPTST('LCYCLE')) JCYCLE=1
C        'DCLEVEL'
      CALL XVPARM('DCLEVEL',IDC,ICOUNT,IDEF,1)

      NS=NSO+NSW
      CALL STACKA(7,MAIN,5,4*NS,4*NS,2*NS,2*NS,2*NS)

C        CLOSE DATA SETS
      CALL XVCLOSE(IUNIT,STAT,' ')
      CALL XVCLOSE(OUNIT,STAT,' ')
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE MAIN(ISUM,LX,TBUF,MX,INBUF,IX,OUTBUF,JX,IDBUF,KX)

      COMMON /C1/ IUNIT,OUNIT,SL,SS,NLO,NSO,FORMAT,HIGH,NLW,NSW,
     &            ICYCLE,JCYCLE,IDC 

      INTEGER*4 ISUM(1),TBUF(1),OUNIT,STAT,SS,SL,HIGH
      INTEGER*2 INBUF(1),OUTBUF(1),IDBUF(1)
      CHARACTER*8 FORMAT

      M=NLW/2+1
      N=NSW/2+1
      L=N-1
      CALL ZIA(ISUM,NSO+NSW-1)   

      DO I=1,NLO
          ILINE=1-M+I
          IF(JCYCLE .EQ. 0) THEN
              IF(ILINE .LT. 1) ILINE=2-ILINE
              IF(ILINE .GT. NLO) ILINE=NLO+NLO-ILINE
          ELSE              	      
              IF(ILINE .LT. 1) ILINE=NLO+ILINE
              IF(ILINE .GT. NLO) ILINE=ILINE-NLO
          END IF
          CALL XVREAD(IUNIT,INBUF(N),STAT,'LINE',SL+ILINE-1,
     &                'SAMP',SS,'NSAMPS',NSO,' ')    
          IF (I.LE.NLW) THEN
              IF(N.GT.1) THEN
                  IF(ICYCLE .EQ. 0) THEN
                      CALL RFLCT(N,NSO,INBUF)
                  ELSE 
                      CALL CYCLE(N,NSO,INBUF)
                  END IF
              END IF
              CALL ADDV(6,NSO+NSW-1,INBUF,ISUM,1,1)
          END IF
          CALL XVWRIT(OUNIT,INBUF(N),STAT,' ')
      END DO
      CALL ZAIRE(TBUF,ISUM,NSO,NSW)
      CALL DIVV(4,NSO,NLW*NSW,TBUF,0,1)
      CALL MVE(-6,NSO,TBUF,OUTBUF,1,1)

C        RE-OPEN OUTPUT FOR UPDATE
      CALL XVCLOSE(OUNIT,STAT,' ')
      CALL XVOPEN(OUNIT,STAT,'OP','UPDATE','U_FORMAT','HALF',' ')
      ILINE=(NLW+1)/2

      DO I=2,NLO
          CALL XVREAD(OUNIT,IDBUF(N),STAT,'LINE',I-1,' ')
          CALL XVWRIT(OUNIT,OUTBUF,STAT,'LINE',I-1,' ')
          IF(N.GT.1) THEN
              IF(ICYCLE .EQ. 0) THEN 
                  CALL RFLCT(N,NSO,IDBUF)
              ELSE 
                  CALL CYCLE(N,NSO,IDBUF)
              END IF
          END IF
          CALL RSUBV(6,NSO+NSW-1,IDBUF,ISUM,1,1)

          JLINE=ILINE+1
          IF(JCYCLE .EQ. 0.AND.JLINE.GT.NLO) JLINE=NLO+NLO-JLINE
          IF(JCYCLE.NE.0.AND.JLINE .GT. NLO) JLINE=JLINE-NLO
          CALL XVREAD(IUNIT,INBUF(N),STAT,'LINE',SL+JLINE-1,
     &                'SAMP',SS,'NSAMPS',NSO,' ')        
          IF(N.GT.1) THEN
              IF(ICYCLE .EQ. 0) THEN 
                  CALL RFLCT(N,NSO,INBUF)
              ELSE 
                  CALL CYCLE(N,NSO,INBUF)
              END IF
          END IF
          CALL ADDV(6,NSO+NSW-1,INBUF,ISUM,1,1)
          CALL ZAIRE(TBUF,ISUM,NSO,NSW)
          CALL DIVV(4,NSO,NLW*NSW,TBUF,0,1)
          CALL MVE(-6,NSO,TBUF,OUTBUF,1,1)
          ILINE=ILINE+1
      END DO

      CALL XVWRIT(OUNIT,OUTBUF,STAT,'LINE',I-1,' ')

      IF(HIGH .NE. 1) RETURN

C        DO HIGHPASS OPERATION
      DO I=1,NLO
          CALL XVREAD(IUNIT,INBUF,STAT,'LINE',SL+I-1,
     &                'SAMP',SS,'NSAMPS',NSO,' ')
          CALL XVREAD(OUNIT,OUTBUF,STAT,'LINE',I,' ')
          CALL RSUBV(2,NSO,OUTBUF,INBUF,1,1)
          CALL ADDV(-6,NSO,IDC,INBUF,0,1)
          IF(FORMAT.EQ.'BYTE') CALL CUTOFF(INBUF,NSO)
          CALL XVWRIT(OUNIT,INBUF,STAT,'LINE',I,' ')
      END DO

      CALL XVMESSAGE('HIGH PASS FILTER PERFORMED.',' ')

20    RETURN
      END
C**********************************************************************
      SUBROUTINE RFLCT(N,NSO,INBUF)
      INTEGER*2 INBUF(1)

      L=N-1
      CALL MVE(2,L,INBUF(N+1),INBUF(N-1),1,-1)
      CALL MVE(2,L,INBUF(NSO+L-1),INBUF(NSO+N),-1,1)

      RETURN
      END
C**********************************************************************
      SUBROUTINE CYCLE(N,NSO,INBUF)
      INTEGER*2 INBUF(1)

      L=N-1
      CALL MVE(2,L,INBUF(NSO+1),INBUF(1),1,1)
      CALL MVE(2,L,INBUF(N),INBUF(N+NSO),1,1)

      RETURN
      END
C**********************************************************************
      SUBROUTINE CUTOFF(INBUF,NSO)
      INTEGER*2 INBUF(1)

      DO I=1,NSO
          IF (INBUF(I).GT.255) INBUF(I)=255
          IF (INBUF(I).LT.0) INBUF(I)=0
      END DO

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create boxflt2c.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	VICAR SUBROUTINE                                            SUBV

	General routine for subtracting arrays.  Array B is replaced with the
	product of subtracting A from B.  A and B can be of different data 
        types as indicated

	DCODE......Data types
	           =1,   A is byte         B is byte
	           =2,   A is halfword     B is halfword
	           =3,   A is byte         B is halfword
                   =4,   A is fullword     B is fullword
	           =5,   A is byte         B is fullword
                   =6,   A is halfword     B is fullword
	           =7,   A is real(single) B is real
	           =8,   A is double       B is double
                   =9,   A is real         B is double
                  negative values -1 to -9 reverse of above
*/    
/*                     ADAPTED FROM ASU VERSION  */
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME (rsubv)(dcode, n, avec, bvec, inca, incb)
     int *dcode, *n, *inca, *incb;
     void *avec, *bvec;
{
   rzsubv( *dcode, *n, avec, bvec, *inca, *incb);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

rzsubv(dcode, n, avec, bvec, inca, incb)
   int dcode, n, inca, incb;
   void *avec, *bvec;
{
  int i;
  
  /* vectors */
  unsigned char *bytein,   *byteout;
  short         *halfin,   *halfout;
  long          *fullin,   *fullout;
  float         *realin,   *realout;
  double        *doublein, *doubleout;
  
  
  switch (dcode) {
  case -1:
  case 1:
    bytein = (unsigned char *) avec;
    byteout = (unsigned char *) bvec;
    for (i=0; i < n; i++, bytein+=inca, byteout+=incb) {
      *byteout = *byteout - *bytein;
    }
    break;
  case -2:
  case 2:
    halfin = (short *) avec;
    halfout = (short *) bvec;
    for (i = 0; i <n; i++, halfin+=inca, halfout+=incb){
      *halfout = *halfout - *halfin;
    }
    break;
  case -3:
    halfin = (short *) avec;
    byteout = (unsigned char *) bvec;
    for (i = 0; i<n ;i++,halfin+=inca, byteout+=incb){
      *byteout = *byteout - *halfin;
    }
    break;
  case 3:
    bytein = (unsigned char *) avec;
    halfout = (short *) bvec;
    for (i = 0; i< n; i++, bytein+=inca,halfout+=incb){
      *halfout = *halfout - *bytein;
    }
    break;
  case -4:
  case  4:
    fullin = (long *) avec;
    fullout = (long *) bvec;
    for (i = 0; i<n ;i++,fullin+=inca,fullout+=incb){
      *fullout = *fullout - *fullin;
    }
    break;
  case -5:
    fullin = (long *) avec;
    byteout = (unsigned char *) bvec;
    for (i = 0; i< n; i++,fullin+=inca,byteout+=incb){
      *byteout = *byteout - *fullin;
    }
    break;
  case 5:
    bytein = (unsigned char *) avec;
    fullout = (long *) bvec;
    for (i = 0; i< n; i++, bytein+=inca,fullout+=incb){
      *fullout = *fullout - *bytein;
    }
    break;
  case -6:
    fullin = (long *) avec;
    halfout = (short *) bvec;
    for (i = 0;i< n; i++, fullin+=inca,halfout+=incb){
        *halfout = *halfout - *fullin;
    }
    break;
  case 6:
    halfin = (short *) avec;
    fullout = (long *) bvec;
    for (i = 0; i< n; i++, halfin+=inca,fullout+=incb){
      *fullout = *fullout - *halfin;
    }
    break;
  case -7:
  case  7:
    realin = (float *) avec;
    realout = (float *) bvec;
    for (i = 0;i< n; i++, realin+=inca,realout+=incb){
      *realout = *realout - *realin;
    }
    break;
  case -8:
  case  8:
    doublein = (double *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< n; i++, doublein+=inca,doubleout+=incb){
      *doubleout = *doubleout - *doublein;
    }
    break;
  case -9:
    doublein = (double *) avec;
    realout = (float *) bvec;
    for (i = 0; i< n; i++, doublein+=inca,realout+=incb){
      *realout = *realout - *doublein;
    }
    break;
  case 9:
    realin = (float *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< n; i++,realin+=inca,doubleout+=incb){
      *doubleout = *doubleout - *realin;
    }
    break;
  default:    
    zvmessage("*** SUBV - Illegal DCODE","");
    zabend();
    break;
  }
}


/************************************************************************/
/* Fortran-Callable ZAIRE                                             */
/************************************************************************/


void FTN_NAME (zaire)(out,in,nsaddr,nswaddr)
      int out[],in[],*nsaddr,*nswaddr;

{
      int ns,nsw,outptr,inptr;
      int tmp,total,i;
      ns = *nsaddr;
      nsw = *nswaddr;
      inptr = 0;
      outptr = 0; 
      tmp = inptr;
/*                                         */
      total = 0;
      for (i=1;i<=nsw;i++) total += in[inptr++];
/*                                         */
      ns--;
      out[outptr++] = total;
/*                                         */
      for (i=1;i<=ns;i++) {
          total = total - in[tmp++] + in[inptr++];
          out[outptr++] = total;
      }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create boxflt2.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM boxflt2

   To Create the build file give the command:

		$ vimake boxflt2			(VMS)
   or
		% vimake boxflt2			(Unix)


************************************************************************/


#define PROGRAM	boxflt2
#define R2LIB

#define MODULE_LIST boxflt2.f boxflt2c.c

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define USES_C
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create boxflt2.pdf
process help=*
PARM INP TYPE=STRING
PARM OUT TYPE=STRING
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM NSW TYPE=INTEGER DEFAULT=11
PARM NLW TYPE=INTEGER DEFAULT=11
PARM FILTER TYPE=KEYWORD VALID=(HIGHPASS,LOWPASS) DEFAULT=LOWPASS
PARM DCLEVEL TYPE=INTEGER DEFAULT=128
PARM EDGE TYPE=KEYWORD VALID=(CYCLE,LCYCLE,SCYCLE,REFLECT) DEFAULT=REFLECT
END-PROC
.TITLE
boxflt2
.HELP
PURPOSE:
boxflt2 applies a low-pass filter to an input image by taking the local
mean of all pixels contained within a prescribed window centered at
each pixel of the input image.  This mean then replaces the input value.
A highpass option is available which replaces each input value with the
difference between the input and the local mean, plus a constant DC-level
offset.

EXECUTION:

Examples
	boxflt2  INP  OUT  NLW=21  NSW=451

	This example performs a lowpass filter of size 451 samples by 21
	lines on the input. Reflection is performed at image boundaries.

	boxflt2  INP  OUT  NLW=101  NSW=1  'HIGHPASS  DCLEVEL=90  'CYCLE

	This examples performs a highpass filter of size 101 lines by 1
	sample with an output DCLEVEL of 90, performing cycling at the
	image boundaries.  (The omitted keywords are FILTER and EDGE,
	respectively.)

.page	
Modes of handling boundaries:
		a = pixel (1,1)		b = pixel (1,NS)
		c = pixel (NL,1)	d = pixel (NL,NS)
	+-------+-------+-------+	+-------+-------+-------+
	| d   c | c   d | d   c |	| a   b | a   b | a   b |
	|       |       |       |	|       |       |       |
	| b   a | a   b | b   a |	| c   d | c   d | c   d |
	|-------|-------|-------|	|-------|-------|-------|
	| b   a | a   b | b   a |	| a   b | a   b | a   b |
	|       |       |       |	|       |       |       |
	| d   c | c   d | d   c |	| c   d | c   d | c   d |
	|-------|-------|-------|	|-------|-------|-------|
	| d   c | c   d | d   c |	| a   b | a   b | a   b |
	|       |       |       |	|       |       |       |
	| b   a | a   b | b   a |	| c   d | c   d | c   d |
	+-------+-------+-------+	+-------+-------+-------+
		RELECTION			 CYCLING
.page
OPERATION:
boxflt2 performs a lowpass filter operation by taking the local mean of
all pixels contained within a prescribed window of NLW by NSW dimensions
centered at each pixel of the input image.  This mean then replaces the 
input value.  If the HIGHPASS option is specified, then the difference
between the input and the local mean plus a constant DC-level offset
replaces the input value. 

boxflt2 provides the user with the choice of using "reflection" or "cycling"
(wrap-around) at image boundaries.  In the default case, image reflection
is used, and may be depicted as above in the EXECUTION section.  If cycling
is desired, where the left boundary of the image is equivalent to the 
right boundary, and the upper boundary is equivalent to the lower boundary,
the reflection is performed as shown above in the CYCLING diagram.

WRITTEN BY:  W. D. Benton, 1 June 1976
COGNIZANT PROGRAMMER:  A. S. Mazer
REVISION:  New
           Made Portable for UNIX   Richardson(CRI)  05-May-94 

.LEVEL1
.VARIABLE INP
STRING - Input dataset
.VARIABLE OUT
STRING - Output dataset
.VARIABLE SIZE
INTEGER - Standard VICAR1 size field
.VARIABLE SL
INTEGER - Starting line
.VARIABLE SS
INTEGER - Starting sample
.VARIABLE NS
INTEGER - Number of lines
.VARIABLE NL
INTEGER - Number of samples
.VARIABLE NSW
INTEGER - Filter width in pixels
.VARIABLE NLW
INTEGER - Filter length in pixels
.VARIABLE FILTER
KEYWORD - Selects type of filtering (LOWPASS, HIGHPASS)
.VARIABLE DCLEVEL
INTEGER - Highpass constant
.VARIABLE EDGE
KEYWORD - Selects method of handling edges (REFLECT, CYCLE, LCYCLE, SCYCLE)
.LEVEL2
.VARIABLE NSW
NSW is the width in pixels of the box filter.  It must be less than
twice the image width in pixels and defaults to 11.
.VARIABLE NLW
NLS is the length in lines of the box filter.  It must be less than
twice the image length in pixels and defaults to 11.
.VARIABLE FILTER
FILTER=HIGHPASS specifies that the output is to be the highpass, rather than
the lowpass, version of the input, i.e., OUT = IN - LOW + DCLEVEL.
The default is lowpass filtering.
.VARIABLE DCLEVEL
Specifies (for highpass filter) the constant to be added to the 
difference (IN-LOW) in the highpass output image.  Default is 128.
.VARIABLE EDGE
Specifies image handling at image boundaries.  Setting EDGE=CYCLE or 'CYCLE
causes the program to treat the image as if it wrapped around at boundaries
in both directions.  'LCYCLE and 'SCYCLE cause wrap-around in the line and
sample direction only, respectively.  The default is for the program to 
reflect the image at the boundaries.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstboxflt2.pdf
procedure
refgbl $echo
refgbl $autousage
body
let _onfail="continue"
let $echo="yes"
let $autousage="none"
!  TEST WITH BYTE IMAGES
!
gen BOXFA 20 20 LINC=10 SINC=4 IVAL=0
list BOXFA
boxflt2 BOXFA BOXFB
list BOXFB
boxflt2 BOXFA BOXFC NLW=1 NSW=1
list BOXFC
boxflt2 BOXFA BOXFD 'HIGHPASS DCLEVEL=100 'CYCLE
list BOXFD
boxflt2 BOXFA BOXFE 'LCYCLE
list BOXFE
boxflt2 BOXFA BOXFF 'SCYCLE
list BOXFF
boxflt2 BOXFA BOXFG 'REFLECT 'HIGHPASS
list BOXFG
!  TEST WITH HALFWORD IMAGES
!
gen BOXFA 20 20 LINC=10 SINC=4 IVAL=-100 'HALF
list BOXFA
boxflt2 BOXFA BOXFB
list BOXFB
boxflt2 BOXFA BOXFC NLW=1 NSW=1
list BOXFC
boxflt2 BOXFA BOXFD 'HIGHPASS DCLEVEL=100 'CYCLE
list BOXFD
boxflt2 BOXFA BOXFE 'LCYCLE
list BOXFE
boxflt2 BOXFA BOXFF 'SCYCLE
list BOXFF
boxflt2 BOXFA BOXFG 'REFLECT 'HIGHPASS
list BOXFG
end-proc
$ Return
$!#############################################################################
