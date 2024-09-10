$!****************************************************************************
$!
$! Build proc for MIPL module tristim
$! VPACK Version 1.7, Tuesday, April 19, 1994, 16:38:17
$!
$! Execute by entering:		$ @tristim
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
$ write sys$output "*** module tristim ***"
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
$ write sys$output "Invalid argument given to tristim.com file -- ", primary
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
$   if F$SEARCH("tristim.imake") .nes. ""
$   then
$      vimake tristim
$      purge tristim.bld
$   else
$      if F$SEARCH("tristim.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tristim
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tristim.bld "STD"
$   else
$      @tristim.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tristim.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tristim.com -
	-s tristim.f -
	-i tristim.imake -
	-p tristim.pdf -
	-t tsttristim.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tristim.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  02 MAY 1994 ... CRI ... MSTP S/W CONVERSION (VICAR PORTING)

      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      REAL*4 CMFX(40), CMFY(40), CMFZ(40)
      REAL*4 AX(40), AY(40), AZ(40)
      REAL*4 BX(40), BY(40), BZ(40)
      REAL*4 CX(40), CY(40), CZ(40)
      REAL*4 XENONX(40), XENONY(40), XENONZ(40)
      REAL*4 D55X(40), D55Y(40), D55Z(40)
      REAL*4 D65X(40), D65Y(40), D65Z(40)
      REAL*4 D75X(40), D75Y(40), D75Z(40)
      REAL*4 TX(40),TY(40),TZ(40)
      REAL*4 RAD(40,10),XRAD(40),LAMBDA,CONT(40),BACK(40)
      INTEGER ISET,NSPECT
      CHARACTER*78 MSG1
      CHARACTER*80 MSG2,MSG3,MSG4,MSG5
      LOGICAL XVPTST

      DATA CMFX/.0014,.0042,.0143,.0435,.1344,.2839,.3483,.3362,
     1.2908,.1954,.0956,.032,.0049,.0093,.0633,.1655,.2904,.4334,
     2.5945,.7621,.9163,1.0263,1.0622,1.0026,.8544,.6424,.4479,.2835,
     3.1649,.0874,.0468,.0227,.0114,.0058,.0029,.0014,.0007,.0003,.0002,
     4.0001/
      DATA CMFY/0.,.0001,.0004,.0012,.004,.0116,.023,.038,
     1.06,.091,.139,.208,.323,.503,.71,.862,.954,.995,.995,.952,
     3.87,.757,.631,.503,.381,.265,.175,.107,.061,.032,.017,.0082,
     5.0041,.0021,.001,.0005,.0002,.0001,.0001,0./
      DATA CMFZ/.0065,.0201,.0679,.2074,.6456,1.3856,1.7471,
     11.7721,1.6692,1.2876,.813,.4652,.272,.1582,.0782,.0422,.0203,
     2.0087,.0039,.0021,.0017,.0011,.0008,.0003,.0002,15*0./
      DATA AX/.001,.005,.019,.071,.262,.649,.926,1.031,1.019,
     1.776,.428,.16,.027,.057,.425,1.214,2.313,3.732,5.510,7.571,
     29.719,11.579,12.704,12.669,11.373,8.98,6.558,4.336,2.628,1.448,
     3.804,.404,.209,.11,.057,.028,.014,.006,.004,.002/
      DATA AY/.000,.0,.001,.002,.008,.027,.061,.117,.21,.362,
     1.622,1.039,1.792,3.080,4.771,6.322,7.6,8.568,9.222,9.457,9.228,
     28.54,7.547,6.356,5.071,3.704,2.562,1.637,.972,.53,.292,.146,.075,
     3.04,.019,.01,.006,.002,.002,.0/
      DATA AZ/.006,.023,.093,.34,1.256,3.167,4.647,5.435,5.851,
     15.116,3.636,2.324,1.509,.969,.525,.309,.162,.075,.036,.021,
     2.018,.012,.01,.004,.003,15*0./
      DATA BX/.003,.013,.056,.217,.812,1.983,2.689,2.744,2.454,
     11.718,.87,.295,.044,.081,.541,1.458,2.689,4.183,5.84,
     27.472,8.843,9.728,9.948,9.436,8.14,6.2,4.374,2.815,1.655,.876,
     3.465,.22,.108,.053,.026,.012,.006,.002,.002,.001/
      DATA BY/0.,0.,.002,.006,.024,.081,.178,.31,.506,.8,1.265,
     21.918,2.908,4.36,6.072,7.594,8.834,9.603,9.774,9.334,8.396,
     37.176,5.909,4.734,3.63,2.558,1.709,1.062,.612,.321,.169,.08,
     4.039,.019,.009,.004,.002,.001,.001,0./
      DATA BZ/.014,.06,.268,1.033,3.899,9.678,13.489,14.462,
     214.085,11.319,7.396,4.29,2.449,1.371,.669,.372,.188,.084,.038,
     3.021,.016,.01,.007,.003,.002,15*0./
      DATA CX/.004,.019,.085,.329,1.238,2.997,3.975,3.915,3.362,
     12.272,1.112,.363,.052,.089,.576,1.523,2.785,4.282,5.88,
     37.322,8.417,8.984,8.949,8.325,7.07,5.309,3.693,2.349,1.361,
     4.708,.369,.171,.082,.039,.019,.008,.004,.002,.001,.001/
      DATA CY/0.,0.,.002,.009,.037,.122,.262,.443,.694,1.058,
     11.618,2.358,3.401,4.833,6.462,7.934,9.149,9.832,9.841,
     2 9.147,7.992,6.627,5.316,4.176,3.153,2.19,1.443,.886,.504,.259,
     3.134,.062,.029,.014,.006,.003,.002,.001,.001,.0/
      DATA CZ/.02,.089,.404,1.57,5.949,14.628,19.938,20.638,19.299
     1,14.972,9.461,5.274,2.864,1.52,.712,.388,.195,.086,.039,.02,.016,
     2.01,.007,.002,.002,15*0./
      DATA XENONX/.004,.014,.057,.193,.671,1.506,2.092,2.363,
     12.272,1.895,.803,.270,.040,.078,.553,1.484,2.651,4.045,5.657,
     27.333,9.058,10.257,10.332,9.852,8.768,6.416,4.316,2.848,1.582,
     3.874,.471,.246,.107,.060,.028,.015,.007,.003,.002,.001/
      DATA XENONY/0.,0.,.001,.005,.020,.062,.138,.267,.469,.883,
     11.168,1.756,2.634,4.233,6.203,7.730,8.709,9.287,9.468,9.160,
     28.601,7.565,6.138,4.943,3.910,2.646,1.686,1.075,.586,.320,.171,
     3.089,.039,.022,.009,.005,.002,.001,.001,0./
      DATA XENONZ/.017,.066,.270,.918,3.221,7.349,10.497,12.457,
     113.045,12.489,6.834,3.928,2.219,1.332,.684,.378,.185,.081,.037,
     2.020,.017,.011,.008,.003,.002,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     30.,0.,0.,0.,0./
      DATA D55X/.004,.015,.083,.284,.915,1.834,2.836,3.135,2.781,
     11.857,.935,.299,.047,.089,.602,1.641,2.821,4.248,5.656,7.048,
     28.517,8.925,9.540,9.071,7.658,5.525,3.933,2.398,1.417,.781,.400,
     3.172,.089,.047,.019,.011,.006,.002,.001,.001/
      DATA D55Y/0.0,0.,.002,.008,.027,.075,.187,.354,.574,.865,
     21.358,1.942,3.095,4.819,6.755,8.546,9.267,9.750,9.467,8.804,
     38.087,6.583,5.667,4.551,3.415,2.279,1.537,.9050,.524,.286,
     4.146,.062,.032,.017,.007,.004,.002,.001,0.,0./
      DATA D55Z/.020,.073,.394,1.354,4.398,8.951,14.228,16.523,
     215.960,12.239,7.943,4.342,2.606,1.516,.744,.418,.197,.086,.037,
     3.019,.015,.010,.007,.003,.002,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     40.,0.,0.,0.,0./
      DATA D65X/.006,.022,.112,.377,1.188,2.329,3.456,3.722,
     13.242,2.123,1.049,.33,.051,.095,.627,1.686,2.869,4.267,5.625,
     26.947,8.305,8.613,9.047,8.5,7.091,5.063,3.547,2.147,1.252,
     3.68,.346,.15,.077,.041,.017,.01,.005,.002,.001,.001/
      DATA D65Y/0.,.001,.003,.01,.035,.095,.228,.421,.669,
     1.989,1.525,2.142,3.342,5.131,7.04,8.784,9.425,9.796,9.415,
     28.678,7.886,6.353,5.374,4.265,3.162,2.089,1.386,.81,.463,.249,
     3.126,.054,.028,.015,.006,.003,.002,.001,.0,0./
      DATA D65Z/.031,.104,.531,1.795,5.708,11.365,17.336,19.621,
     118.608,13.995,8.917,4.79,2.815,1.614,.776,.43,.201,.086,.037,
     2.019,.015,.009,.007,.003,.002,15*0./
      DATA D75X/.009,.028,.137,.457,1.424,2.749,3.965,4.2,3.617
     1,2.336,1.139,.354,.054,.099,.646,1.716,2.9,4.271,5.584,6.843,
     28.108,8.387,8.7,8.108,6.71,4.749,3.298,1.992,1.151,.619,.315,
     3.136,.069,.037,.015,.009,.004,.002,.001,0./
      DATA D75Y/0.,.001,.004,.013,.042,.112,.262,.475,.746,
     11.088,1.656,2.302,3.538,5.372,7.249,8.939,9.526,9.804,9.346,
     28.549,7.698,6.186,5.168,4.068,2.992,1.959,1.289,.752,.426,
     3.227,.114,.049,.025,.013,.006,.003,.002,.001,0.,0./
      DATA D75Z/.04,.132,.649,2.18,6.84,13.419,19.889,22.139,
     120.759,15.397,9.683,5.147,2.979,1.69,.799,.437,.203,.086,.037,
     2.019,.015,.009,.007,.003,.001,15*0./
      DATA TX/40*0./,TY/40*0./,TZ/40*0./
      DATA ISET/0/
      DATA MSG1/'WAVELENGTH RADIANCE   XBAR      YBAR      ZBAR      R*X
     &       R*Y       R*Z  '/

      CALL IFMESSAGE ('TRISTIM VERSION 02-MAY-94')
      CALL XVEACTION ('SA',' ')
      IF (XVPTST( 'XENON')) THEN
	CALL XVMESSAGE('XENON ILLUMINANT',' ')
	DO I=1,40
	  TX(I)=XENONX(I)
	  TY(I)=XENONY(I)
	  TZ(I)=XENONZ(I)
	ENDDO
      ENDIF

      IF (XVPTST( 'D55')) THEN
	CALL XVMESSAGE('D55 ILLUMINANT',' ')
	DO I=1,40
	  TX(I)=D55X(I)
	  TY(I)=D55Y(I)
	  TZ(I)=D55Z(I)
	ENDDO
      ENDIF

      IF (XVPTST( 'D65')) THEN
	CALL XVMESSAGE('D65 ILLUMINANT',' ')
	DO I=1,40
	  TX(I)=D65X(I)
	  TY(I)=D65Y(I)
	  TZ(I)=D65Z(I)
	ENDDO
      ENDIF

      IF (XVPTST( 'D75')) THEN
	CALL XVMESSAGE('D75 ILLUMINANT',' ')
	DO I=1,40
	  TX(I)=D75X(I)
	  TY(I)=D75Y(I)
	  TZ(I)=D75Z(I)
	ENDDO
      ENDIF

      IF (XVPTST( 'A')) THEN
	CALLXVMESSAGE('ILLUMINANT A',' ')
	DO I=1,40
	  TX(I)=AX(I)
	  TY(I)=AY(I)
	  TZ(I)=AZ(I)
	ENDDO
      ENDIF

      IF (XVPTST( 'B')) THEN
	CALL XVMESSAGE('ILLUMINANT B',' ')
	DO I=1,40
	  TX(I)=BX(I)
	  TY(I)=BY(I)
	  TZ(I)=BZ(I)
	ENDDO
      ENDIF

      IF (XVPTST( 'C')) THEN
	CALL XVMESSAGE('ILLUMINANT C',' ')
	DO I=1,40
	  TX(I)=CX(I)
	  TY(I)=CY(I)
	  TZ(I)=CZ(I)
	ENDDO
      ENDIF

C     COLOR MATCHING FUNCTIONS
C     USER INPUTS SPECTRAL REFLECTANCE (TRANSMITTANCE) TIMES
C     SPECTRAL POWER
      IF (XVPTST( 'CMF')) THEN
	CALL XVMESSAGE('COLOR MATCHING FUNCTION',' ')
	DO I=1,40
	  TX(I)=CMFX(I)
	  TY(I)=CMFY(I)
	  TZ(I)=CMFZ(I)
	ENDDO
      ENDIF

C RADIANCES:

      NSPECT = 1	! DEFAULT

      CALL XVPARM( 'RADIANCE', RAD, ICNT, IDEF, 400) ! RADIANCES IN ALL SPECTRA
      IF (ICNT.EQ.0) THEN
	DO I = 1,40
	  RAD( I, 1) = 1.
	ENDDO
      ELSE
	NSPECT = ICNT/40
	IF (ICNT.NE.40*NSPECT) THEN
	  CALL XVMESSAGE
     &         ('** MUST SPECIFY 40 RADIANCES PER SPECTRUM **',' ')
	  CALL ABEND
	ENDIF
      ENDIF

      CALL XVPARM( 'ADD', CONST, ICNT, IDEF, 1)  ! ADD A CONSTANT TO RADIANCES
      IF (ICNT.GT.0) ISET = 1

      CALL XVPARM( 'MULT', CONSTM, ICNT, IDEF, 1) ! MULT RADIANCES BY CONSTANT
      IF (ICNT.GT.0) ISET = 1

C MULTIPLY RADIANCES TOGETHER
      DO J=1,40
	XRAD(J)=1.
	DO I=1,NSPECT
	  XRAD(J)=RAD(J,I)*XRAD(J)
	ENDDO
      ENDDO
C ADJUST RADIANCES VALUES
      DO I=1,40
        XRAD(I) = XRAD(I)*CONSTM + CONST
      ENDDO

C  OPTION TO ENTER "ABSOLUTE" INTENSITY VALUES NEEDS CONTINUUM &
C  BACKGROUND VALUES:

      CALL XVPARM( 'CONTIN', CONT, ICNT, IDEF, 40)
      IF (ICNT.GT.0) THEN
	CALL XVPARM( 'BACK', BACK, ICNT, IDEF, 40)
	IF (ICNT.LE.1) THEN
	  DO I=2,40
	    BACK(I) = BACK(1)
	  ENDDO
	ENDIF
	DO I=1,40
	  XRAD(I) = (XRAD(I)-BACK(I))/CONT(I)
	ENDDO
      ENDIF

C  PRINT OUT THE RADIANCES

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG1,' ')

      LAMBDA=380.
      RADSUM=0.
      XSUM=0.
      YSUM=0.
      ZSUM=0.
      DO 300 I=1,40
      X=TX(I)*XRAD(I)
      Y=TY(I)*XRAD(I)
      Z=TZ(I)*XRAD(I)
      XSUM=X+XSUM
      YSUM=Y+YSUM
      ZSUM=Z+ZSUM

      WRITE (MSG2,9900) NINT(LAMBDA),XRAD(I),TX(I),TY(I),TZ(I),X,Y,Z
9900  FORMAT ('   ',I6,F10.5,F10.4,F10.4,F10.4,F10.5,F10.5,F10.5)
      CALL XVMESSAGE(MSG2,' ')
      LAMBDA=LAMBDA+10.
      RADSUM=RADSUM+XRAD(I)
  300 CONTINUE
      WRITE (MSG3,9910) CONSTM,CONST
9910  FORMAT ('OUTPUT RADIANCE=INPUT*',F10.5,'  +     ',F5.2)
      IF(ISET.EQ.1) CALL XVMESSAGE(' ',' ')
      IF(ISET.EQ.1) CALL XVMESSAGE(MSG3,' ')

C  PRINT OUT TRISTIMULUS VALUES

      TOTAL = XSUM + YSUM + ZSUM
      XCHROM=XSUM/TOTAL
      YCHROM=YSUM/TOTAL
      ZCHROM=ZSUM/TOTAL
      WRITE (MSG4,9920) XSUM,YSUM,ZSUM
9920  FORMAT ('X TRISTIM = ',F10.6,' Y TRISTIM =',F10.6,'  Z TRISTIM ',
     +F10.6,'  ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG4,' ')
      WRITE (MSG5,9930) XCHROM,YCHROM,ZCHROM
9930  FORMAT ('X CHROM  =  ',F10.8,' Y CHROM   =',F10.8,'  Z CHROM   ',
     +F10.8,'  ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(MSG5,' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('TRISTIM END',' ')
      RETURN
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tristim.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM tristim

   To Create the build file give the command:

		$ vimake tristim			(VMS)
   or
		% vimake tristim			(Unix)


************************************************************************/


#define PROGRAM	tristim
#define R2LIB

#define MODULE_LIST tristim.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create tristim.pdf
process help=*
PARM ILLUMIN  KEYWORD VALID=(XENON,D55,D65,D75,A,B,C,CMF) DEFAULT=XENON
PARM RADIANCE REAL    COUNT=0:400			  DEFAULT=--
PARM ADD      REAL 					  DEFAULT=0.
PARM MULT     REAL					  DEFAULT=1.
PARM CONTIN   REAL    COUNT=(0,40)			  DEFAULT=--
PARM BACK     REAL    COUNT=(1:40)                        DEFAULT=0.
!PARM BACK REAL COUNT=(1,40) DEFAULT=0.  !!not legal under TAE??
end-proc
.TITLE
VICAR1 Program TRISTIM
.HELP
"tristim" is a Vicar applications program that will calculate the tristimulus
values and chromaticity coordinates for a given spectrum, which may be of
intensity, reflectivity, or transmittivity. Tristimulus values and chromaticity
coordinates are quantities which must be known to do color reconstruction.  
For a complete discussion of colorimetry, see Reference 1.
 
EXECUTION:

tristim PARAMS

where PARAMS are parameters listed in Tutor mode.  (There are no input or
output datasets.)
.page
OPERATION:
 
"tristim" computes tristimulus values for a spectrum by the formulas:
 
  X = k * SUM( F(i) * x'(i) )
  Y = k * SUM( F(i) * y'(i) )
  Z = k * SUM( F(i) * z'(i) )
 
where:   X, Y, and Z are the tristimulus values,
 
         F is the input spectral distribution of intensity, reflectivity,
         or transmittivity.
 
         x', y', and z' are the C.I.E. standard colorimetric observer
         values, also known as the color matching functions, optionally
         weighted by a specified illuminant,
 
         and i is the wavelength.
 
The sums are taken from i = 380 nm to 770 nm.

The constant k is defined by the relation:

                    100
         k = -------------------
             SUM( S(i) * y'(i) )

         where S(i) is the spectral radiance of the illuminant.
 
For standard illuminants D55, D65, D75, A, B, and C, the color matching
functions have been weighted by the relative spectral power distributions
of these illuminants in such a way that k=1.  The XENON illuminant
weighting functions have also been computed so that k=1.  However, XENON
is not a standard of the C.I.E. and refers only to the JPL 12-inch light
cannon on 13 April 1976 (see Reference 2).
 
If the user is using some other illuminant, k will have to be computed
separately.  This can easily be done with another execution of TRISTIM,
inputting the spectral radiance of the illuminant for the 40 radiance
values, and then taking the ratio of 100 and the Y tristimulus value.
 
"tristim" also outputs the chromaticity coordinates of the spectrum in
question.  These are related by the equations:
 
          X                  Y                  Z
    x = ----- ,        y = ----- ,        z = ----- .
        X+Y+Z              X+Y+Z              X+Y+Z
.page
The user-supplied input spectrum (RADIANCE) may be specified as a multiple 
of up to ten separate spectral distributions, of exactly 40 points each 
(R1,...,R400). This can represent a series of filters through which light 
passes.
 
The RADIANCE values R1,...,R40 can also be modified (for whatever reason) 
by the equation:
 
   NEW_RADIANCE(i) = Ri * MULT + ADD

where MULT and ADD are the values specified for the parameters of those
names.
.page
In addition, the user may specify Continuum and Background spectra, CONTIN
and BACK, which will be applied to the input spectrum according to the
following formula:

                        (Ri - BACK(i))
   NEW_RADIANCE(i) = -------------------
                     (CONTIN(i) - BACK(i))

This can be useful to process spectrophotometric tracings of inputs in
certain colors and of a white reference input.
.page
REFERENCES

1. Judd, D.B., and G, Wyszecki, "Color in Business, Science, and Industry",
   Third Edition, 1975, John Wiley and Sons.
 
2. JPL IOM 26 April 1976, To: L. Snyder, From: M. Benesh, New Photometric
   Characteristics of 12" (30-cm) Light Cannon No. 1.
 
 
TIMING
 
TRISTIM takes about 4 CPU seconds to run.
.PAGE 
COGNIZANT PROGRAMMER
 
Written by:  Joel Mosher,  1 Oct. 1977
 
Converted to VAX by:  L. W. Kamp,  10 Jan. 1984
 
Current Cognizant Programmer:  L. W. Kamp

Made portable for UNIX   Alan Scop (CRI) 2 May 1994

.LEVEL1
.vari illumin
KEYWORD: Valid = D55, D65,
D75, A, B, C, CMF, XENON.
.vari radiance
List of Radiances
.vari add
Constant added to RADIANCE.
.vari mult
Constant by which RADIANCE
is multiplied.
.vari CONTIN
Continuum reference spectrum
.vari BACK
Background spectrum
.LEVEL2
.vari illumin
ILLUMINANT: specifies the illuminant by whose spectral radiance the C.I.E.
color matching functions are weighted.
 
Valid values are: D55, D65, D75, A, B, C, XENON, CMF.
 
The first six of these (D55 thru C) are C.I.E. standard illuminants.
Note that D55, D65, and D75 refer to blackbody curves (Planck function)
at 5500, 6500, and 7500 Kelvins, respectively.

XENON specifies that the illuminant is the JPL 12-inch light cannon No. 1.
 
CMF specifies that the unweighted C.I.E. color matching functions are to be
used.
.vari radiance
RADIANCE:  List of numbers which specify the spectral radiance or reflectivity
or transmittivity of the object under study.  
 
This spectrum must be specified at exactly 40 points, starting at 380 nm 
and ending at 770 nm.  Up to 10 separate spectra may be specified, each of
40 points, which will be multiplied together inside the program to obtain
the final spectral distribution.  These separate spectra are simple entered
sequentially under this parameter.  E.g., if RADIANCE contains 80 numbers,
of which the first 40 are 2.0 and the next 40 are 5.0, then the resulting
spectrum values are 10.0 at all 40 points.
 
Note that the user may also cause a linear transformation to be applied to
the input spectrum, using the ADD and MULT parameters.
.vari add
ADD: This is a constant that will be added to each of the 40 spectral 
values specified by the RADIANCE parameter, before computing the tristimulus
values.
.vari mult
MULT: This is a constant that will be multiplied with each of the 40 spectral 
values specified by the RADIANCE parameter, before computing the tristimulus
values.
.vari contin
This specifies 40 points that will be used as a reference (continuum) spectrum
to compute relative radiance values from the values input in RADIANCE.

If this parameter is defaulted, then no relative radiance computation will
be done.
.vari back
This specifies 40 points that will be used a background spectrum if the
parameter CONTIN has been specified (if not, then this parameter is
ignored).

If only one value is specified, then that value will be used for all 40
points.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttristim.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
!
!  test with defaults only:
tristim
!
!  test parm ILLUM:
tristim 'D55
tristim 'A
!
!  test parm RADIANCE with ADD & MULT:
tristim 'CMF ADD=1.0 MULT=0.5 RADI=(+
.2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2+
.2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2 .2+
.4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4+
.4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4 .4)
!
!  test parm RADIANCE with CONTIN & BACK:
tristim 'XENON RADIANCE=( +
 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, +
 42, 42, 42, 47, 59, 83, 125, 181, 245, 308, 364, 418, 470, 510, 547, 580, +
 603, 620, 633, 643, 652, 661, 665, 673, 677, 682) +
 BACK=22 +
 CONT=( 700, 700, 700, 692, 700, 712, 720, 725, 733, 730, 728, 740, 739, 740, +
 740, 740, 739, 738, 741, 739, 738, 738, 738, 737, 736, 734, 734, 733, +
 731, 737, 737, 738, 738, 738, 736, 736, 735, 735, 735, 733)
end-proc
$ Return
$!#############################################################################
