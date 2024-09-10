$!****************************************************************************
$!
$! Build proc for MIPL module aggrg2
$! VPACK Version 1.8, Wednesday, December 28, 1994, 10:32:45
$!
$! Execute by entering:		$ @aggrg2
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
$ write sys$output "*** module aggrg2 ***"
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
$ write sys$output "Invalid argument given to aggrg2.com file -- ", primary
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
$   if F$SEARCH("aggrg2.imake") .nes. ""
$   then
$      vimake aggrg2
$      purge aggrg2.bld
$   else
$      if F$SEARCH("aggrg2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake aggrg2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @aggrg2.bld "STD"
$   else
$      @aggrg2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create aggrg2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack aggrg2.com -
	-s aggrg2.f -
	-i aggrg2.imake -
	-p aggrg2.pdf -
	-t tstaggrg2.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create aggrg2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C
C  IBIS ROUTINE AGGRG2
C  2-JAN-95 ...CRI...  MSTP S/W CONVERSION (VICAR PORTING)
C
C  PURPOSE:  AGGREGATES COLUMNS OF NUMBERS IN AN IBIS INTERFACE FILE BAS
C  EQUAL INDICES IN A CONTROL COLUMN AND COLLAPSES THE FILE BASED UPON R
C  OF THE SAME INDICES.  OPTIONS ARE AVAILABLE TO PICK MAXIMUM IN COLLAP
C  TO PICK ACCORDING TO A MAXIMUM IN ANOTHER COLUMN.
C
C  USER PARAMETERS:
C
C  AGCOL,N - THE KEYWORD AGCOL SPECIFIES THE COLUMN WHICH WILL BE USED A
C            CONTROL FOR COLUMNS TO BE COMPRESSED.  ANY REPEATED NUMBER
C            THIS COLUMN WILL FORM ONE ROW IN THE OUTPUT FILE.
C  AREA,N -  THE INTEGER N SPECIFIES THE COLUMN TO BE USED FOR AN AREA M
C            KEY.  A MAXIMUM VALUE WILL BE FOUND WITHIN EACH INDEX GROUP
C  BYAR,N1,...,NK - THIS SPECIFIES COLUMNS WHICH WILL BE COLLAPSED CHOOS
C            THE VALUE INDICATED BY THE MAXIMUM AREA ROW INDEX.
C  SUMCOL,N1,...,NL - SPECIFIES COLUMNS WHICH WILL BE COLLAPSED BY AGGRE
C            ALL VALUES FOR AN INDEX GROUP.
C
	IMPLICIT INTEGER(A-Z)
	INTEGER*2 CS1,CS2
	REAL R,FCOL1(250000)
	DIMENSION ACTION(100),BYARCL(100),SUMCL(100)
	COMMON/COM/COL1(250000),CS1(250000),CS2(250000)
	EQUIVALENCE (FCOL1(1),COL1(1))
	DATA AGCO,AREA/1,1/,ACTION/100*0/
C
C  INITIALIZE, GET PARAMETERS, OPEN FILES
C
        CALL IFMESSAGE('AGGRG2 version 2-JAN-95')
        CALL XVUNIT(RUNIT, 'INP', 1, STATUS, ' ')
        CALL XVUNIT(WUNIT, 'OUT', 1, STATUS, ' ')

	CALL IBIS_FILE_OPEN(RUNIT,INIBIS,'READ',0,0,' ',' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(RUNIT,STATUS,1)
        CALL IBIS_FILE_GET(INIBIS,'NC',NCOL,1,1)
        CALL IBIS_FILE_GET(INIBIS,'NR',CLEN,1,1)

	CALL XVPARM ('AGCOL',AGCO,NAGCO,AGCODF,1)
	CALL XVPARM ('AREA',AREA,NAREA,AREADF,1)
	CALL XVPARM ('BYAR',BYARCL,NBYAR,BYARDF,20)
	CALL XVPARM ('SUMCOL',SUMCL,NSUMC,SUMCDF,20)
	IF (BYARDF.EQ.0) THEN
	  DO I=1,NBYAR
	    ACTION(BYARCL(I)) = 1
	  ENDDO
	ENDIF
	IF (SUMCDF.EQ.0) THEN
	  DO I=1,NSUMC
	    ACTION(SUMCL(I)) = 2
	  ENDDO
	ENDIF
C
C  PLACE CONTROL RANGES IN CS1 VECTOR
C
        CALL IBIS_COLUMN_READ(INIBIS,FCOL1,AGCO,1,CLEN,STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
	NWCLEN = 0
	I = 1
	J = I + 1
	DO WHILE (J.LE.CLEN)
	  IF (FCOL1(J).EQ.FCOL1(I)) THEN
	    J = J + 1
	  ELSE
	    NWCLEN = NWCLEN + 1
	    CS1(NWCLEN) = J - I
	    I = J
	    J = J + 1
	  ENDIF
	ENDDO
	NWCLEN = NWCLEN + 1
	CS1(NWCLEN) = J - I
C
C  PLACE MAXIMUM AREAS IN CS2 VECTOR.
C
        CALL IBIS_COLUMN_READ(INIBIS,FCOL1,AREA,1,CLEN,STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
	JU = 0
	DO I=1,NWCLEN
	  JL = JU+1
	  JU = JU+CS1(I)
	  R = -1.
	  DO J=JL,JU
	    CS2(J) = 0
	    IF (ABS(FCOL1(J)).GT.R) THEN
	      JMAX = J
	      R = ABS(FCOL1(J))
	    ENDIF
	  ENDDO
	  CS2(JMAX) = 1
	ENDDO
C
C  FOR EACH COLUMN PERFORM THE APPROPRIATE FUNCTION
C
	NWNRCL = (NWCLEN+127)/128
	CALL IBIS_FILE_OPEN(WUNIT,OUTIBIS,'WRITE',NCOL,NWCLEN,
     *                      ' ',' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(WUNIT,STATUS,1)
	DO IX=1,NCOL
          CALL IBIS_COLUMN_READ(INIBIS,COL1,IX,1,CLEN,STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL(INIBIS,STATUS,1)
	  JU = 0
	  IF (ACTION(IX).EQ.1) THEN
	    DO I=1,CLEN
	      IF (CS2(I).EQ.0) FCOL1(I) = 0.
	    ENDDO
	  ENDIF
	  DO I=1,NWCLEN
	    JL = JU + 1
	    JU = JU + CS1(I)
	    IF (ACTION(IX).LE.1) THEN
	      JMAX = JL
	      DO J=JL,JU
		IF (ABS(FCOL1(J)).GT.ABS(FCOL1(JMAX))) JMAX = J
	      ENDDO
	      FCOL1(I) = FCOL1(JMAX)
	    ELSE
	      R = 0.
	      DO J=JL,JU
	        R = R + FCOL1(J)
	      ENDDO
	      FCOL1(I) = R
	    ENDIF
	  ENDDO
          CALL IBIS_COLUMN_WRITE(OUTIBIS,FCOL1,IX,1,NWCLEN,STATUS)
          IF (STATUS.NE.1) CALL IBIS_SIGNAL(OUTIBIS,STATUS,1)
	ENDDO
C
	CALL PRNT(4,1,NWCLEN,' COLUMN LENGTH.')
	CALL IBIS_FILE_CLOSE(INIBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(RUNIT,STATUS,1)
	CALL IBIS_FILE_CLOSE(OUTIBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(WUNIT,STATUS,1)
        RETURN
	END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create aggrg2.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM aggrg2

   To Create the build file give the command:

		$ vimake aggrg2			(VMS)
   or
		% vimake aggrg2			(Unix)


************************************************************************/


#define PROGRAM	aggrg2
#define R2LIB

#define MODULE_LIST aggrg2.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create aggrg2.pdf
PROCESS		HELP=*
! "aggrg2" PDF/HELP - VICAR/IBIS MOSAIC SOFTWARE
PARM INP TYPE=(STRING)
PARM OUT TYPE=(STRING)
PARM AGCOL TYPE=INTEGER,DEFAULT=1
PARM AREA TYPE=INTEGER,DEFAULT=1
PARM BYAR TYPE=INTEGER,COUNT=(1:20),DEFAULT=0
PARM SUMCOL TYPE=INTEGER,COUNT=(1:20),DEFAULT=0
END-PROC
.TITLE
VICAR/IBIS Program "aggrg2"
.HELP
PURPOSE

	"aggrg2" collapses columns of numbers in an IBIS interface file into
smaller columns using a designated column as the control. Within the control
column each number which is repeated will define one row. A second key or
index, the area key, selects the maximum value from an index group and stores
is row position in the column. The row position is a pointer from which a
sample can be selected for other columns.

	There are three alternative methods of collapsing columns:

1. The first option selects a sample item from a column based on the row
position of the maximum value from the key column. One value is selected
for each group of control values.

2. The second option aggregates all the values in a column based on the
limits defined by the control values.

3. The third option selects a maximum value from the column based on the
limits defined by the control values (default).

.PAGE
	To illustrate the options a sample case is shown:

CONTROL AREA	SAMPLE		CONTROL	OPTION	OPTION	OPTION
COLUMN	KEY	COLUMN		COLUMN	ONE	TWO	THREE
1	10	 2		1	 4	12	 6
1	20	 4		2	10	18	10
1	15	 6		3	16	42	16
2	24	 8		4	18	18	18
2	31	10		5	20	20	20
3	16	12
3	31	14
3	53	16
4	10	18
5	20	20

.PAGE
TAE COMMAND LINE FORMAT

	aggrg2 INP=FILE OUT=FILE2 AGCOL=N AREA=A BYAR=(X,Y,Z) SUMCOL=(A,B,C)
	aggrg2 INP=FILE OUT=FILE2 AGCOL=N SUMCOL=(X,Y,Z)

	FILE represents the interface file, N the control column,
	A the keyword area, X,Y, and Z the columns to be collapsed,
	and A,B, and C the columns to be collapsed by aggregating
	all the values for an index group.

EXAMPLE

	sort INP=A SORTCOL=1
	aggrg2 INP=A OUT=B AGCOL=1

	This example collapses the interface file by replacing multiple
rows with the same value in column 1 by a single row and choosing the
largest value in the other rows to be retained.

RESTRICTIONS

	The interface file must be sorted by the routine SORT on the
column used as a control column (given by the keyword AGCOL).

	The interface file must have a column length less than 250,000.


	WRITTEN BY		A. L. Zobrist		 1 Dec 1975
	COGNIZANT PROGRAMMER	K. F. Evans
	REVISION		1			26 Jul 1978
        Made portable for UNIX  A. Scop (CRI)            2 Jan 1995
.LEVEL1
.VARIABLE INP
Standard IBIS input file.
.VARIABLE OUT
Standard IBIS output file.
.VARIABLE AGCOL
Control column number.
.VARIABLE AREA
Area maximum key.
.VARIABLE BYAR
Columns to be collapsed by the
first option.
.VARIABLE SUMCOL
Columns to be collapsed by
aggregating all values for
an index group.
.LEVEL2
.VARIABLE INP
Standard IBIS interface file
of 512 samples by N lines.
.VARIABLE OUT
Standard IBIS interface file
of 512 samples by N lines.
.VARIABLE AGCOL
This column number is used as
a control for the columns to
be compressed. Each number
repeated in this column will
form 1 row on the output file.
.VARIABLE AREA
This column is used as an area
maximum key. The maximum value
is found in each index group
and stored by row position.
This row position is a pointer
to the same positions in other
columns.
.VARIABLE BYAR
These are the columns to be
collapsed by the first option,
that is select one sample row
position for each index group
by the maximum key area.
.VARIABLE SUMCOL
These are the columns to be
collapsed by aggregating all
values for an index group.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstaggrg2.pdf
procedure
refgbl $autousage
refgbl $echo
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
ibis-gen agg1 nc=5 nr=5 datacol=(1,2,3,4,5) +
              data=(1,2,3,4,5,1,12,13,7,4,1,5,4,3,15,2,8,4,6,15,2,2,17,9,5)
ibis-list agg1
aggrg2 agg1 aggo1 AGCOL=1
ibis-list aggo1

ibis-gen agg2 nc=5 nr=5 datacol=(1,2,3,4,5) +
              data=(1,2,3,4,5,1,12,13,7,4,1,5,4,3,15,2,8,4,6,15,2,2,17,9,5)
aggrg2 agg2 aggo2 AGCOL=1 AREA=4 BYAR=(2,3,4,5) SUMCOL=(2,3,4,5)
ibis-list aggo2

end-proc
$ Return
$!#############################################################################
