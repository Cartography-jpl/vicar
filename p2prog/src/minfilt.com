$!****************************************************************************
$!
$! Build proc for MIPL module minfilt
$! VPACK Version 1.7, Friday, July 22, 1994, 13:04:22
$!
$! Execute by entering:		$ @minfilt
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
$ write sys$output "*** module minfilt ***"
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
$ write sys$output "Invalid argument given to minfilt.com file -- ", primary
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
$   if F$SEARCH("minfilt.imake") .nes. ""
$   then
$      vimake minfilt
$      purge minfilt.bld
$   else
$      if F$SEARCH("minfilt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake minfilt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @minfilt.bld "STD"
$   else
$      @minfilt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create minfilt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack minfilt.com -
	-s minfilt.f -
	-i minfilt.imake -
	-p minfilt.pdf -
	-t tstminfilt.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create minfilt.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C     PROGRAM minfilt
c        jan 93       jjl      
C
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

      parameter (nsmax=10000)
      implicit integer(a-z)
      INTEGER*4 SL,SS,NL,NS,NLW,NSW
      integer*2 OUT(nsmax), IN(51*nsmax)
      INTEGER OUTUNIT,INUNIT,CNT
C
C  OPEN INPUT & OUTPUT FOR SEQUENTIAL I/O
C
      CALL XVUNIT(INUNIT,'INP',1,STATUS,' ')
      CALL XVOPEN(INUNIT,STATUS,'OPEN_ACT','SA','IO_ACT','SA',
     +            'U_FORMAT','HALF',' ')
      CALL XVSIZE(SL,SS,NL,NS,nlin,nsin)
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      CALL XVOPEN(OUTUNIT,STATUS,'OP','WRITE','OPEN_ACT','SA',
     +          'IO_ACT','SA','U_FORMAT','HALF',' ')
      SL = SL - 1

C
C  GET NLW AND NSW VALUES
      CALL XVP('NLW',NLW,CNT)
      CALL XVP('NSW',NSW,CNT)
      NLW = NLW / 2 * 2 + 1
      NSW = NSW / 2 * 2 + 1

      if(nsmax.lt.ns+nsw-1)then
         call xvmessage('input line too long',' ')
         call abend
      endif
      if(51*nsmax.lt.(ns+nsw-1)*nlw)then
         call xvmessage('insufficient memory available',' ')
         call abend
      endif
C
C  DETERMINE STARTING LINE AND INCREMENT VALUES
      LINE = 1 - NLW / 2
      L = NSW / 2
      L1 = (NSW + 1) / 2
      L2 = L1 + 1
      L3 = L1 - 1
      L4 = L1 + NS - 2
      L5 = L1 + NS
      M = NL + NL
      CURR = (NLW + 1) / 2
      IPOS = 0
      INC = NS + NSW - 1
      IMAX = (NLW) * INC
      NLWM1 = NLW - 1
      IF (NLWM1 .NE. 0) THEN
C
C  READ INITIAL NLW LINES INTO CORE, REFLECTING AT BOUNDARIES
C
         DO 200 I=1,NLWM1
            ILINE=LINE
            LINE=LINE+1
  201       IF (ILINE .LT. 1) ILINE = 2 - ILINE
            IF (ILINE .GT. NL) ILINE = M - ILINE
            IF (ILINE .LT. 1) GO TO 201
            CALL XVREAD(INUNIT,IN(IPOS+L1),STATUS,'NSAMPS',NS,'SAMP',SS,
     &                'LINE',SL+ILINE,' ')
            IF (NSW .NE. 1) THEN
                do j=1,L
                   in(ipos+L3+1-j)=in(ipos+L2-1+j)
                   in(ipos+L5-1+j)=in(ipos+L4+1-j)
                enddo
            ENDIF
            IPOS=IPOS+INC
200      CONTINUE
      ENDIF


      DO 300 I = 1, NL
         ILINE = LINE
         LINE = LINE + 1
301      IF (ILINE .LT. 1) ILINE = 2 - ILINE
         IF(ILINE .GT. NL) ILINE = M - ILINE
         IF (ILINE .LT. 1) GOTO 301
         CALL XVREAD(INUNIT,IN(IPOS+L1),STATUS,'NSAMPS',NS,'SAMP',SS,
     &               'LINE',SL+ILINE,' ')
         IF(NSW .NE. 1) THEN
             do j=1,L
                in(ipos+L3+1-j)=in(ipos+L2-1+j)
                in(ipos+L5-1+j)=in(ipos+L4+1-j)
             enddo
         ENDIF

         CALL MIN2D(IN,OUT,nlw,nsw,ns)

         CALL XVWRIT(OUTUNIT,OUT,STATUS,' ')
         IPOS=IPOS+INC
         IF(IPOS .GE. IMAX) IPOS=0
         CURR=MOD(CURR,NLW)+1
300   CONTINUE
C
C *****         CLOSE DATA SETS
C
999   CALL XVCLOSE(INUNIT,STATUS,' ')
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      RETURN
      END

      subroutine min2d(in,out,nlw,nsw,ns)
      integer*2 in(1),out(1),mindn
      len=ns+nsw-1

c find min dn for left most block
      k=0
      mindn=in(1)
      mincol=1
      do j=1,nlw
        do i=1,nsw
          k=k+1
          if(in(k).lt.mindn)then
             mincol=i
             mindn=in(k)      
          endif
        enddo
        k=j*len
      enddo
      out(1)=mindn

c do the rest
      do i=2,ns
        if(mincol.ge.i)then                ! check only right column
           k=i+nsw-1
           do j=1,nlw
             if(in(k).lt.mindn)then
                mincol=i+nsw-1
                mindn=in(k)      
             endif
             k=k+len
           enddo
           out(i)=mindn
        else                               ! check all columns
           mindn=in(i)
           mincol=i
           k=i
           do j=1,nlw
             do m=i,i+nsw-1
               if(in(k).lt.mindn)then
                  mincol=m
                  mindn=in(k)      
               endif
               k=k+1
             enddo
             k=j*len+i
           enddo
           out(i)=mindn
        endif
      enddo
      return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create minfilt.imake
#define  PROGRAM   minfilt

#define MODULE_LIST minfilt.f

#define MAIN_LANG_FORTRAN
#define R2LIB 

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create minfilt.pdf
process help=*
PARM INP	TYPE = STRING
PARM OUT	TYPE = STRING 	          DEFAULT = MINFILT
PARM SIZE	TYPE = INTEGER 	COUNT=0:4 DEFAULT=--
PARM NL		TYPE = INTEGER  COUNT=0:1 DEFAULT=--
PARM NS         TYPE = INTEGER  COUNT=0:1 DEFAULT=--
PARM NLW        TYPE = INTEGER		  DEFAULT = 3
PARM NSW        TYPE = INTEGER		  DEFAULT = 3
END-PROC
.TITLE
VICAR program MINFILT.

.HELP
PURPOSE:
Selects the minimum DN within a rectangular convolution window for
images in either BYTE or HALF formats.

.PAGE
EXECUTION:

MINFILT may be executed in the following manner:

		MINFILT INP=A OUT=B SIZE=(SL,SS,NL,NS) PARAMS

where INP, OUT, SIZE, AND PARAMS are parameters and are explained in their
respective parameter section.

.PAGE
OPERATION:

MINFILT finds the local minimum value of a rectangular window centered at
each pixel in an image. 
The program takes advantage of the fact that if the minimum of an area
was not in the left column then the next area to the right (one 
pixel over) need only be checked in the right column.

EXAMPLE:

	 MINFILT INP=A OUT=B NLW=5 NSW=7
  The window is 5 lines by 7 samples.

.PAGE
HISTORY:

ORIGINALLY WRITTEN BY: J Lorre 1/30/93
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1
.VARI INP
STRING-input dataset.
.VARI OUT
STRING-output dataset.
.VARI SIZE
4 INTEGERS-VICAR size field,
(SL,SS,NL,NS).
.VARI NL
INTEGER-number of lines in
output file.
.VARI NS
INTEGER-Number of samples in
output file.
.VARI NLW
INTEGER-Size of filter kernel
in lines.
.VARI NSW
INTEGER-Size of filter kernel
in samples.
.LEVEL2
.VARI INP
STRING - INP=A where A is the input dataset name.
.VARI OUT
STRING - OUT=B where B is the output dataset name.
.VARI SIZE
4 INTEGERS - SIZE=(SL,SS,NL,NS) where SL is the starting line, SS is the
 starting sample, NL is the number of lines in the input dataset and NS
 is the number of samples in the input dataset. (SIZE is usually defined
 as SIZE=(1,1,NL,NS)). Default is taken from the VICAR label within the
 program.
.VARI NL
INTEGER - NL=N1 where is N1 is the number of lines in the input dataset.
.VARI NS
INTEGER - NS=N1 where is N1 is the number of samples in the input dataset.
.VARI NLW
INTEGER - NLW=I1 where I1 is an integer and specifies the size of the filter
 kernel in lines. Default is NLW=3.
.VARI NSW
INTEGER - NSW=I2 where I2 is an integer and specifies the size of the filter
 kernel in samples. Default is NSW=3.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstminfilt.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage = "none"
let _onfail="continue"
let $echo="yes"
!This is a test file for minfilt
gen out=a.img nl=10 ns=10
list a.img
minfilt inp=a.img out=b.img nlw=5 nsw=1
list b.img
minfilt inp=a.img out=b.img nlw=1 nsw=5
list b.img
minfilt inp=a.img out=b.img nlw=5 nsw=5
list b.img
gausnois out=a.img nl=10 ns=10
list a.img
minfilt inp=a.img out=b.img nlw=5 nsw=5
list b.img
end-proc
$ Return
$!#############################################################################
