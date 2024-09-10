$!****************************************************************************
$!
$! Build proc for MIPL module aggrg
$! VPACK Version 1.9, Friday, September 17, 1999, 11:40:18
$!
$! Execute by entering:		$ @aggrg
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
$ write sys$output "*** module aggrg ***"
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
$ write sys$output "Invalid argument given to aggrg.com file -- ", primary
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
$   if F$SEARCH("aggrg.imake") .nes. ""
$   then
$      vimake aggrg
$      purge aggrg.bld
$   else
$      if F$SEARCH("aggrg.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake aggrg
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @aggrg.bld "STD"
$   else
$      @aggrg.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create aggrg.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack aggrg.com -
	-s aggrg.f -
	-i aggrg.imake -
	-p aggrg.pdf -
	-t tstaggrg.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create aggrg.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C  IBIS ROUTINE AGGRG
C
C  PURPOSE:  SUM COLUMNS OF NUMBERS IN AN IBIS INTERFACE FILE BASED UPON
C  INDICES IN A SPECIFIED CONTROL COLUMN.
C
C  2 JAN 95 ...CRI...  MSTP S/W CONVERSION (VICAR PORTING)
C
C  17 SEPTEMBER 1999   BAM - BUMPED UP THE BUFFERS TO 2,000,000
C
	IMPLICIT INTEGER (A-Z)
	INTEGER COL1(2000000)
	REAL	COL2(2000000), SUM
	INTEGER SUMCOL(40), TOCOL(40)

        CALL IFMESSAGE('AGGRG version 17-SEPT-99')
	CALL XVP ('AGCOL',AGCOL,COUNT)
	CALL XVPARM ('SUMCOL',SUMCOL,NSUM,DEF,40)
	CALL XVPARM ('TOCOL',TOCOL,NTO,DEF,40)
	IF (DEF.NE.0) THEN
	  DO I=1,NSUM
	    TOCOL(I) = SUMCOL(I)
	  ENDDO
	ENDIF
	CALL XVPARM ('INDEXCOL', INDEXCOL, COUNT,INDEXDEF,1)

        CALL XVUNIT(UNIT, 'INP', 1, STATUS, ' ')

	CALL IBIS_FILE_OPEN(UNIT,IBIS,'UPDATE',0,0,' ',' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)
        CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
        CALL IBIS_FILE_GET(IBIS,'NR',CLEN,1,1)

        CALL IBIS_COLUMN_READ(IBIS,COL1,AGCOL,1,CLEN,STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	CALL CONCOL (COL1,CLEN)

	DO ICOL=1,NSUM
          CALL IBIS_COLUMN_READ(IBIS,COL2,SUMCOL(ICOL),1,CLEN,STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	  IL = 1
	  DO WHILE (IL.LE.CLEN)
	    IU = COL1(IL)
	    SUM = 0.
	    DO I=IL,IU
	      SUM = SUM+COL2(I)
	    ENDDO
	    DO I=IL,IU
	      COL2(I) = SUM
	    ENDDO
	    IL = IU+1
	  ENDDO
          CALL IBIS_COLUMN_WRITE(IBIS,COL2,TOCOL(ICOL),1,CLEN,STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	ENDDO

	IF (INDEXDEF .EQ. 0) THEN
	  IL = 1
	  DO WHILE (IL.LE.CLEN)
	    IU = COL1(IL)
	    N = 1
	    DO I=IL,IU
	      COL2(I) = N
	      N = N + 1
	    ENDDO
	    IL = IU+1
	  ENDDO
          CALL IBIS_COLUMN_WRITE(IBIS,COL2,INDEXCOL,1,CLEN,STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS,STATUS,1)
	ENDIF

	CALL IBIS_FILE_CLOSE(IBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(UNIT,STATUS,1)

        RETURN
	END


	SUBROUTINE CONCOL (COL,LEN)
	IMPLICIT INTEGER(A-Z)
	DIMENSION COL(LEN)
C
	N = 1
	C = COL(1)
	DO I=1,LEN
	  IF (COL(I).NE.C) N = I
	  C = COL(I)
 	  COL(N) = I
	ENDDO
	RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create aggrg.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM aggrg

   To Create the build file give the command:

		$ vimake aggrg			(VMS)
   or
		% vimake aggrg			(Unix)


************************************************************************/


#define PROGRAM	aggrg
#define R2LIB

#define MODULE_LIST aggrg.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create aggrg.pdf
PROCESS		HELP=*
 PARM INP      TYPE=(STRING)
 PARM AGCOL    TYPE=INTEGER DEFAULT=1
 PARM SUMCOL   TYPE=INTEGER COUNT=(1:40)
 PARM TOCOL    TYPE=INTEGER COUNT=(0:40) DEFAULT=0
 PARM INDEXCOL TYPE=INTEGER DEFAULT=0
END-PROC
.TITLE
VICAR/IBIS Program "aggrg"
.HELP
PURPOSE

    "aggrg" aggregates columns in an IBIS tabular file using any designated 
column in the file as a control column.  A column is aggregated by adding 
together items which have the same number contiguously in the control column.
The columns of aggregated data either replaces the original column or is 
placed in a columns specified by the user.  Optionally an index column may
be specified to receive the row index for each group.  The input file is 
modified instead of having an output file; the file retains its original 
length (unlike "aggrg2").

A simple example of this process is shown using four columns:

.PAGE
CONTROL COLUMN	 AGGREGATION COLUMN   RESULTS COLUMN   INDEX COLUMN
	1	   10	(10+10)		20		1
	1	   10	(10+10)		20		2
	2	   11	(11+13+14)	38		1
	2	   13	(11+13+14)	38		2
	2	   14	(11+13+14)	38		3
	3	   17	(17)		17		1
	9	   10	(10+11)		21		1
	9	   11	(10+11)		21		2
	8	   11	(11+12)		23		1
	8	   12	(11+12)		23		2
	1	   15	(15)		15		1

.PAGE
TAE COMMAND LINE FORMAT

	aggrg INP=FILE AGCOL=N SUMCOL=(X,Y,Z) TOCOL=(A,B,C)
	aggrg INP=FILE AGCOL=N SUMCOL=(X,Y,Z)
	aggrg INP=FILE AGCOL=N SUMCOL=(X,Y,Z) TOCOL=(A,B,C) INDEXCOL=K

	FILE is the IBIS interface file, N is the control column, 
X,Y,and Z are the columns to be aggregated, A,B, and C are the columns where 
the results are stored, and K is the column where the index numbers are stored.

EXAMPLE

	aggrg INP=A AGCOL=1 SUMCOL=(5,6,7) TOCOL=(8,9,10)

	This execution indexes on column 1, summing columns 5, 6 and 7; and
replacing columns 8, 9, and 10 with the output.


RESTRICTIONS

    There may be at most 40 columns specified in SUMCOL.
    The maximum column length is 250,000.


	WRITTEN BY		A. L. Zobrist		15 Dec 1976
	COGNIZANT PROGRAMMER	K. F. Evans
	REVISION 		2			June 1987
        Made portable for UNIX  AS (CRI)                Jan  1995

.LEVEL1
.VARIABLE INP
Standard IBIS interface file
.VARIABLE AGCOL
Indexing column.
.VARIABLE SUMCOL
Columns to aggregate(S1,S2,..Sk)
.VARIABLE TOCOL
Columns for output  (T1,T2,..Tk)
Default output columns equal the
input aggregate columns.
.VARIABLE INDEXCOL
Optional column to hold the
index numbers for each group.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstaggrg.pdf
procedure
refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
ibis-gen agg1 nc=5 nr=5 datacol=(1,2,3,4,5) +
              data=(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,9,8,7,6,5,9,8,7,6,5)
ibis-list agg1
aggrg agg1 AGCOL=3 SUMCOL=(4,5) TOCOL=(1,2)
ibis-list agg1

ibis-gen agg2 nc=5 nr=5 datacol=(1,2,3,4,5) +
              data=(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,9,8,7,6,5,9,8,7,6,5)
aggrg agg2 AGCOL=3 SUMCOL=5 TOCOL=2 INDEXCOL=1
ibis-list agg2

ibis-gen agg3 nc=5 nr=5 datacol=(1,2,3,4,5) +
              data=(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,9,8,7,6,5,9,8,7,6,5)
ibis-list agg3
aggrg agg3 AGCOL=3 SUMCOL=(4,5)
ibis-list agg3

end-proc
$ Return
$!#############################################################################
