$!****************************************************************************
$!
$! Build proc for MIPL module giaconda
$! VPACK Version 1.7, Thursday, July 28, 1994, 11:52:39
$!
$! Execute by entering:		$ @giaconda
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
$ write sys$output "*** module giaconda ***"
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
$ write sys$output "Invalid argument given to giaconda.com file -- ", primary
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
$   if F$SEARCH("giaconda.imake") .nes. ""
$   then
$      vimake giaconda
$      purge giaconda.bld
$   else
$      if F$SEARCH("giaconda.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake giaconda
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @giaconda.bld "STD"
$   else
$      @giaconda.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create giaconda.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack giaconda.com -
	-s giaconda.c -
	-i giaconda.imake -
	-p giaconda.pdf -
	-t tstgiaconda.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create giaconda.c
$ DECK/DOLLARS="$ VOKAGLEVE"

/*  Welcome to GIACONDA */
/*  (Generator of Images in Accurate Color On Numerous Digital Apparatus). */

/*  Revision History:
    5 September 1994 ... CRI ... MSTP S/W Conversion (VICAR Porting).
                                 In porting GIACONDA, the contents of the 
                                 include file 'colors.h' was incorporated 
                                 in-line within this file.  The two structures
                                 ILLUMINANT & MATCH were defined as static.
                                 
*/

#include <math.h>
#include <ctype.h>
#include <stdio.h>
#include "vicmain_c"


#define SSIZE 50					/*spectrum size*/
#define DELTA_LAMBDA 10					/*lambda increment(nm)*/
#define START_LAMBDA 290				/*first lambda (nm)*/
#define NCOLORS 100					/*colors in file*/
#define LCOLOR 10					/*length of names*/
#define LCOMMENT 52					/*length of comments*/
#define MFILTERS 8					/*max filters*/
#define MCOLORS 10					/*max special colors*/
#define MLINES 2000
#define MSAMPLES 2000
#define EPSILON 1E-36					/*small float*/
#define TVEXP 2.6					/*tv intensity = */
  							/*  dn ** TVEXP  */

#define D55 1						/*ILLUMINANT[D55]=sun*/
#define FLR 2						/*ILLUMINANT[FLR]=   */
  							/* flourescent light */
#define RED 0						/*index to arrays*/
#define GREEN 1
#define BLUE 2
#define BLACK 3

#define mm(i,j) matrix[(i) * size + (j)]		/*for minverse(),mdet()*/
#define mi(i,j) inverse[(i) * size + (j)]		/*for minverse()*/
#define mc(i,j) comatrix[(i) * (size - 1) + (j)]	/*for minverse()*/

struct color {						/*color description*/
  char name[LCOLOR + 1];
  char comment[LCOMMENT + 1];
  int light;
  float intensity[SSIZE];				/*spectrum*/
  float tristim[3];					/*tristimulus values*/
};

int   int_ts;                           /* integer for translation */
float real1_ts;                         /* float for translation */
float real2_ts;                         /* float for translation */
int   int_conv[12];                     /* translation buffer */
int   real_conv[12];                    /* translation buffer */
int   int_size, real_size;              /* Pixel sizes */

static double ILLUMINANT[][SSIZE] = {				/*illuminants*/
  {1,1,1,1,1,1,1,1,1,1,					/*white*/
   1,1,1,1,1,1,1,1,1,1,
   1,1,1,1,1,1,1,1,1,1,
   1,1,1,1,1,1,1,1,1,1,
   1,1,1,1,1,1,1,1,1,1},
  {0,.02,2.1,11.2,20.6,23.9,27.8,30.6,34.3,32.6,	/*D55*/
   38.1,61.0,68.6,71.6,67.9,85.6,98.0,100.5,99.9,102.7,
   98.1,100.7,100.7,100.0,104.2,102.1,103.0,100.0,97.2,97.7,
   91.4,94.4,95.1,94.2,90.4,92.3,88.9,90.3,93.9,90.0,
   79.7,82.8,84.8,70.2,79.3,85.0,71.9,52.8,75.9,71.8},
  {0,0,0,0,0,0,0,0,0,7.5,				/*std flour. cool white*/
   12.5,17.7,23.1,27.4,33.4,38.7,44.3,46.7,50.2,49.7,
   50.0,48.4,45.7,47.4,53.2,64.7,81.6,100.0,115.0,120.8,
   116.6,103.1,84.8,69.8,52.7,40.5,30.5,22.0,15.5,9.5,
   5.0,2.0,.5,.2,.1} 
};

/*CIE standard observer: indices = x/y/z,lamda*/

static double MATCH [3][SSIZE] = {
  { 0,0,0,0,0,0,0,0,0,.0014,
    .0042,.0143,.0435,.1344,.2839,.3483,.3362,.2908,.1954,.0956,
    .032,.0049,.0093,.0633,.1655,.2904,.4334,.5945,.7621,.9163,
    1.0263,1.0622,1.0026,.8544,.6424,.4479,.2835,.1649,.0874,.0468,
    .0227,.0114,.0058,.0029,.0014,.0007,.0003,.0002,.0001,0},
  { 0,0,0,0,0,0,0,0,0,0.,
    .0001,.0004,.0012,.004,.0116,.023,.038,.06,.091,.139,
    .208,.323,.503,.71,.862,.954,.995,.995,.952,.87,
    .757,.631,.503,.381,.265,.175,.107,.061,.032,.017,
    .0082,.0041,.0021,.001,.0005,.0002,.0001,.0001,0.,0},
  { 0,0,0,0,0,0,0,0,0,.0065,
    .0201,.0679,.2074,.6456,1.3856,1.7471,1.7721,1.6692,1.2876,.813,
    .4652,.272,.1582,.0782,.0422,.0203,.0087,.0039,.0021,.0017,
    .0011,.0008,.0003,.0002,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0}
};

#define CAM 5						/*for able77v2()*/
#define FILT 3	
#define SCID 18
#define LSIZE 19

#define BIT0 1

#define max(a,b) ((a) > (b)) ? (a) : (b)		/*maximum*/
#define repeat(index,low,high) for (index = low; index < high; index++)
/* mm(i,j), mi(i,j), & mc(i,j) are defined in colors.h */

int  status;
int  input_unit[MFILTERS];
char colors[110],dev[10];
int  nin,ncolors,ocolor[MCOLORS],lcolor[MCOLORS],nweights,nout,scale;
int  nvalues,def;
int  nlines,nsamples;
char alpha_buf[8 * 133];
float weight[MCOLORS];
double itr[3][MFILTERS],offset[3];

struct color *file;	
struct color *pfile[MCOLORS];
double *response[MFILTERS];
enum {VGR,GLL} project[MFILTERS];
double exponent;

double minvert(),mdet(),mcof(),fabs(),pow();



double V4_FILTERS[][SSIZE] = {				/*SN 4*/
  {0},							
  {0,0,0,0,0,0,0,0,0,0,					/*blue*/
   0,0,2,29,234,522,505,503,492,415,
   334,281,240,198,139,98,24,9,4,3,
   1},
  {0,0,0,0,0,0,0,0,0,5,					/*clear*/
   34,112,243,405,515,580,595,563,529,449,
   363,307,274,250,265,301,314,316,296,253,
   202,153,108,68,37,12,6},
  {0,0,0,0,0,0,0,0,0,4,					/*violet*/
   32,104,222,377,481,415,247,56,13,4},
  {0,0,0,0,0,0,0,0,0,0,					/*methane 6190*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,
   0,0,2,39,8},
  {0,0,0,0,0,0,0,0,0,0,					/*green*/
   0,0,0,0,0,0,0,0,0,3,
   5,15,36,84,206,283,272,275,271,243,
   198,148,103,64,34,11,5},
  {0,0,0,0,0,0,0,0,0,0,					/*methane 5410*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,122,29},
  {0,0,0,0,0,0,0,0,0,0,					/*orange*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,2,4,11,31,61,
   110,134,97,57,31,11,5}
};  



double V5_FILTERS[][SSIZE] = {				/*SN 5*/
  {3,14,41,85,122,154,191,239,275,321,			/*clear*/
   319,358,406,415,450,470,468,447,418,393,
   370,344,321,299,304,307,299,279,247,212,
   172,130,92,63,37,19,11,6},
  {0,0,0,0,0,0,13,71,177,267,				/*violet*/
   298,334,382,392,380,282,129,25,10,4},
  {0,0,0,0,0,0,0,0,0,0,					/*blue*/
   0,0,2,13,73,341,422,368,362,368,
   354,321,284,213,187,100,37,14,6,3,
   1},
  {0,0,0,0,0,0,0,0,0,0,					/*orange*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,1,2,4,9,22,57,126,179,
   158,120,86,61,36,19,11,6},
  {3,14,41,85,122,154,191,239,275,321,			/*clear*/
   319,358,406,415,450,470,468,447,418,393,
   370,344,321,299,304,307,299,279,247,212,
   172,130,92,63,37,19,11,6},
  {0,0,0,0,0,0,0,0,0,0,					/*green*/
   0,0,0,0,0,0,0,0,0,1,
   8,32,59,135,253,281,262,255,238,209,
   171,129,89,60,34,18,11,6},
  {0,0,0,0,0,0,0,0,0,0,					/*green*/
   0,0,0,0,0,0,0,0,0,1,
   7,21,40,105,237,283,274,261,238,207,
   170,128,87,59,36,18,11,6}
};  



double V6_FILTERS[][SSIZE] = {				/*SN 6*/
  {0},
  {0,0,0,0,0,0,0,0,0,0,					/*blue*/
   0,0,4,40,281,558,560,583,555,486,
   451,421,384,343,228,110,43,18,8,3,
   1},
  {0,0,0,0,0,0,0,0,0,5,					/*clear*/
   42,143,295,441,557,636,657,648,595,524,
   487,459,430,430,443,511,567,595,566,506,
   409,317,238,178,122,81,55,36,12},
  {0,0,0,0,0,0,0,0,0,4,					/*violet*/
   39,134,271,410,525,474,318,81,21,
   5},
  {0,0,0,0,0,0,0,0,0,0,					/*methane 6190*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,
   0,0,5,102,27},
  {0,0,0,0,0,0,0,0,0,0,					/*green*/
   0,0,0,0,0,0,0,0,0,0,
   6,19,61,144,339,480,502,532,529,492,
   402,308,227,164,111,75,52,35,12},
  {0,0,0,0,0,0,0,0,0,0,					/*methane 5410*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,655,52},
  {0,0,0,0,0,0,0,0,0,0,					/*orange*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,2,6,18,51,118,
   229,278,213,152,105,74,51,34,11}
};



double V7_FILTERS[][SSIZE] = {				/*SN 7*/
  {3,16,42,70,101,135,178,220,268,293,			/*clear*/
   310,333,369,398,432,449,445,442,415,388,
   361,333,314,299,303,308,300,280,251,214,
   172,138,103,66,44,23},
  {0,0,0,0,0,0,12,65,172,244,				/*violet*/
   289,311,347,377,364,270,122,24,10,4},
  {0,0,0,0,0,0,0,0,0,0,					/*blue*/
   0,0,2,12,76,348,392,362,359,361,
   344,310,275,236,177,95,35,13,5,2,
   1},
  {0,0,0,0,0,0,0,0,0,0,					/*orange*/
   0,0,0,0,0,0,0,0,0,0,
   0,0,0,1,2,5,14,42,108,182,
   167,129,97,63,42,23},
  {3,16,42,70,101,135,178,220,268,293,			/*clear*/
   310,333,369,398,432,449,445,442,415,388,
   361,333,314,299,303,308,300,280,251,214,
   172,138,103,66,44,23},
  {0,0,0,0,0,0,0,0,0,0,					/*green*/
   0,0,0,0,0,0,0,0,0,1,
   4,10,24,60,106,258,260,247,235,209,
   170,137,102,63,43,21},
  {0,0,0,0,0,0,0,0,0,0,					/*green*/
   0,0,0,0,0,0,0,0,0,1,
   5,16,39,93,197,261,255,247,236,212,
   171,137,102,63,41,21}
};



double GLL_FILTERS[][SSIZE] = {
  {0,0,0,0,0,                                             /*CLEAR*/
   0,0,0,0,19730,
    850900,1225000,1206000,1095000,1082000,
   1156000,1209000,1356000,1507000,1650000,
   1836000,2039000,2132000,2268000,2333000,
   2338000,2359000,2357000,2360000,2439000,
   2610000,2774000,2678000,2577000,2508000,
   2474000,2435000,2610000,2786000,2919000,
   3007000,3094000,2709000,2290000,2018000,
   1845000,1678000,1611000,1527000,1501000},
  {0,0,0,0,0,                                             /*5600*/
   0,0,0,0,0,
   0,0,0,14230,0,
   0,0,0,0,0,
   0,0,0,115400,127600,
   1936000,2078000,1962000,1931000,1870000,
   1705000,485900,13420}, 
  {0,0,0,0,0,                                             /*6600*/
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,26850,103500,1826000,
   1848000,2194000,2356000,2352000,2427000,
   988000,345200,13750,6978,4104},
  {0,0,0,0,0,                                            /*4450*/
   0,0,0,0,15070,
   580100,853600,831400,563400,698300,
   93020,16820,4318,0,0,
   0,0,0,0,0,
   0,0,0,2384,0,
   0,0,0,0,0,
   0,0,0,0,0,
   3040,3138,2750,2326},
  {0,0,0,0,0,                                             /*7560*/
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   24420,1420000,1525000,32890,1543},
  {0},                                                    /*9680*/ 
  {0,0,0,0,0,                                             /*7270*/
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,0,0,
   0,0,0,46520,1744000,
   1879},
  {0}                                                     /*8890*/
};

struct camera {
  int sn;						/*serial number*/
  double (*pfilter)[SSIZE];				/*pointer to filters*/
};
struct camera V1_CAMERAS[] = {{6,V6_FILTERS},
  			      {7,V7_FILTERS},
  			      {0}
};
struct camera V2_CAMERAS[] = {{4,V4_FILTERS},
  			      {5,V5_FILTERS},
  			      {0}
};
struct camera GLL_CAMERAS[] = {{1,GLL_FILTERS},
  			       {0}
};

struct {
  int id;						/*"spacecraft id"   */
  struct camera *pcamera;				/*  AS IN ABLE77V2()*/
} CRAFT[] = {{2,V2_CAMERAS},
  	     {1,V1_CAMERAS},
  	     {77,GLL_CAMERAS},
  	     {0}
};



main44()
{

  int  colors_unit, count;
  int  iinput,instance;
  int  nl_latest,ns_latest;
  char format[5];
  char scf_name [80];

  zifmessage ("GIACONDA version 5-Sept-1994");

  status = zvp ("SCFNAME", scf_name, &count);
  check_status(1,0);

  status = zvunit(&colors_unit,"NONE",1,"U_NAME",scf_name,0);
  check_status(1,1);

  status = zvopen(colors_unit,"ADDRESS",&file,0);	/*open color file*/
  if (!(status & BIT0)) {				/*if can't find file*/
    zvmessage ("ASSIGN SPECIAL_COLORS TO A COLOR LIBRARY",0);
    return;
  }
  check_status(1,1);

  status = zvtrans_inu (int_conv, "FULL", "FULL", colors_unit);
  check_status(1,2);
  status = zvtrans_inu (real_conv,"REAL", "REAL", colors_unit);
  check_status(1,2);

  status = get_parms();
  if (status == -1) return;				/*FATAL USER ERROR*/

  status = zvunit(&input_unit[0],"INP",1,0);
  check_status(1,3);

  status = zvopen(input_unit[0],"OPEN_ACT","SA",0);	/*open 1st input*/
  check_status(1,5);

  status = zvget(input_unit[0],				/*get nl,ns,format*/
  	"NL",&nlines,"NS",&nsamples,"FORMAT",format,0);	
  check_status(1,5);

  if (nlines > MLINES || nsamples > MSAMPLES) {		/*if too big*/
    sprintf(alpha_buf,					/*FATAL USER ERROR*/
  	    "IMAGES TOO LARGE -- LIMIT IS %d BY %d",
  	    MLINES,MSAMPLES);
    zvmessage (alpha_buf,0);
    return;
  }

  if (strncmp(format,"HALF",4)) {			/*if not halfword*/
    zvmessage ("ERROR -- INPUTS MUST BE HALFWORD",0);
    return;						/*FATAL USER ERROR*/
  }

  repeat(iinput,1,nin) {				/*for other inputs*/
    instance = iinput + 1;
    status = zvunit(&input_unit[iinput],"INP",instance,0);
    check_status(1,6);
    status = zvopen (input_unit[iinput],"OPEN_ACT","SA",0);/*open the file*/
    status = zvget (input_unit[iinput],    			/*get nl,ns*/
  	  "NL",&nl_latest,"NS",&ns_latest,0);
    check_status(1,8);
    if (nlines != nl_latest || nsamples != ns_latest) {	/*same size as 1st?*/ 
     zvmessage ("ALL INPUT IMAGES ARE NOT THE SAME SIZE",0);/*FATAL USER ERROR*/
     return;
    }
    if (strncmp(format,"HALF",4)) {			/*if not halfword*/
      zvmessage ("ERROR -- INPUTS MUST BE HALFWORD",0);
      return;						/*FATAL USER ERROR*/
    }
  }

  repeat(iinput,0,nin) {				/*for each input*/
    status = get_project(iinput);
    if (status == 0) {					/*not found*/
      return;
    }
    get_response(iinput);
  }

  status = ioverf_to_rgb();
  if (status == -1) return;				/*FATAL USER ERROR*/
  if (nout) {						/*if outputs specified*/
    create_output();
  }
}



get_project(input)
/*This function assigns to project[input], a number describing the project
indicated by the image label.  A 1 is returned if the project is determined, a 0
otherwise.*/ 

int input;
{
  char task[9],craft[133];
  int nhist,instances[1];

  nhist = 1;
  status = zlhinfo(input_unit[input],task,instances,&nhist,0);
  check_status(4,1);
  status = zlget(input_unit[input],"HISTORY",		/*look in LAB02*/
  	"LAB02",craft,"HIST",task,0);
  if (status != CANNOT_FIND_KEY) {
    check_status(4,2);
    if (!strncmp(craft,"VGR",3)) {			/*if VGR image*/
      project[input] = VGR;
      return 1;
    }    
  }

  status = zlget(input_unit[input],"HISTORY",		/*look in LAB01*/
  	"LAB01",craft,"HIST",task,0);
  if (status != CANNOT_FIND_KEY) {
    check_status(3,4);
    if (!strncmp(craft,"GLL",3)) {			/*if GLL image*/
      project[input] = GLL;
      return 1;
    }
  }

  zvmessage("ERROR -- INPUTS MUST BE VGR OR GLL IMAGES",0);/*none of the above*/
  return 0;
}



/*This function assigns to response[input], a pointer to an array describing
the response of the filter described in the image label.*/

get_response(input)
int input;
{
  int isc,icamera;
  int label[LSIZE];
  label[0] = LSIZE;					/*for able77v2()*/
  switch (project[input]) {
    case VGR:
      zable77v2(&status,input_unit[input],label);	/*get label info*/
      status += 1;					/*normal = 0*/
      check_status(3,3);
      for (isc = 0;					/*find spacecraft*/
	   CRAFT[isc].id != label[SCID] && CRAFT[isc].id;
	   isc++);
      for (icamera = 0;					/*find camera*/
	   (CRAFT[isc].pcamera + icamera)->sn != label[CAM] &&
	   (CRAFT[isc].pcamera + icamera)->sn;
	   icamera++);
      response[input] =				/*point to data*/
	(CRAFT[isc].pcamera + icamera)->pfilter[label[FILT]];
      break;

    case GLL:
      zable86(&status,input_unit[input],label);	/*get label info*/
      status += 1;
      check_status(3,5);
      response[input] = GLL_CAMERAS[0].pfilter[label[FILT]];
      break;
    default:
      break;
  }
}



/* The get_parms function gets the input parameters and check for their 
   consistency. get+_parms returns a -1 for a fatal user error, a 1 otherwise.*/

get_parms()
{

  int icolor, ifile;

  zvparm ("INP",alpha_buf,&nin,&def,MFILTERS,133); /* Length */
  zvparm ("OUT",alpha_buf,&nout,&def,3,133); /* Length */
  zvparm ("COLORS",colors,&ncolors,&def,MCOLORS,0);
  zvsptr (colors, ncolors, ocolor, lcolor);
  zvparm ("WEIGHTS",weight,&nweights,&def,MCOLORS,0); /* Length */
  zvparm ("DEVICE",dev,&nvalues,&def,1,0);
  zvparm ("SCALE",&scale,&nvalues,&def,1,0);

  if (nin > ncolors) {
    zvmessage ("SPECIFY AT LEAST AS MANY COLORS AS THERE ARE INPUTS",0);
    return -1;
  }
  if (nweights > ncolors) {
    zvmessage ("TOO MANY WEIGHTS",0);
    return -1;
  }
  repeat(icolor,nweights,ncolors) {		/*default weight = 1*/
    weight[icolor] = 1;
  }


  repeat(icolor,0,ncolors) {			/*for each special color*/
    for (ifile = 0;				/*find in file*/
  	 strcmp(file[ifile].name,
  	        colors + ocolor[icolor] - 1) &&
  	 ifile < NCOLORS;
  	 ifile++);
    pfile[icolor] = file + ifile;			/*save the address*/

    if (ifile == NCOLORS) {				/*if not there*/
      sprintf(alpha_buf,"COLOR \"%s\" DOES NOT EXIST",
  	      colors + ocolor[icolor] - 1);
      zvmessage (alpha_buf,0);				/*tell the user*/
      return -1;
    }
  }
  return 1;
}



ioverf_to_rgb()

/*This function calculates I/F for each special color through each of the
relevant filters, and then gets the matrix which transforms those values to
the corresponding tristimulus values by doing a least-squares solution to the
resulting equations using the special colors as the data points.  The
resulting matrix is then multiplied times the device matrix (transforming
X,Y,Z to R,G,B) and, finally scaled so that the maximum possible image DN
results in a maximum output of 255.  The function returns -1 if there is the
problem is underdetermined.*/ 

{
  int mode,i,j,iinput,jinput,icolor,lamda,rgb;
  double data[MFILTERS],
  	 matrix[MFILTERS * MFILTERS],
  	 inverse[MFILTERS * MFILTERS],
  	 ioverf[MFILTERS][MCOLORS],
  	 itt[3][MFILTERS],
  	 ttr[3][3];
  float con[MFILTERS];
  double intensity,flux;
  int size;
  double determinant,adjustment,out_max[3],max_max;
  float devmat[3][3],devoff[3];
  int ndevmat,ndevoff;
  char label_buf[7200];
  size = nin;						/*for mm(),mi()*/

  repeat(iinput,0,nin) {				/*for each filter*/
    flux = 0;						/*get flux*/
    repeat (lamda,0,SSIZE) {
      flux += response[iinput][lamda] *
  	ILLUMINANT[D55][lamda];
    }
    repeat(icolor,0,ncolors) {		            /*for each special color*/
      intensity = 0;					/*get intensity*/
      repeat (lamda,0,SSIZE) {
        zvtrans (int_conv, &(pfile[icolor]->light), &int_ts, 1);
        zvtrans (real_conv, &(pfile[icolor]->intensity[lamda]), &real1_ts, 1);
        intensity += response[iinput][lamda] *
  	  ILLUMINANT[int_ts][lamda] *
  	  real1_ts;
      }
      ioverf[iinput][icolor] = intensity/flux;		/*I/F*/
    }
  }


  repeat(i,0,3) {					/*for x,y,z*/
    repeat(iinput,0,nin) {				/*get data vector*/	
      data[iinput] = 0;
      repeat(icolor,0,ncolors) {
        zvtrans (real_conv, &(pfile[icolor]->tristim[i]), &real2_ts, 1);
        data[iinput] += weight[iinput] *		
          ioverf[iinput][icolor] *
  	  real2_ts;
      }
    }

    repeat(iinput,0,nin) {				/*get msmt. matrix*/
      repeat(jinput,0,nin) {
  	mm(iinput,jinput) = 0;
        repeat(icolor,0,ncolors) {
  	  mm(iinput,jinput) +=
  	    weight[icolor] *
  	    ioverf[iinput][icolor] *
  	    ioverf[jinput][icolor];
        }
      }
    }

    determinant = minvert(matrix,inverse,nin);		/*invert matrix*/
    if (fabs(determinant) < EPSILON) {			/*no unique solution*/
      zvmessage("NO UNIQUE SOLUTION -- SEE HELP (EXECUTION)",0);
      return -1;
    }
    repeat(iinput,0,nin) {				/*data * inv.-msmt. = */
      itt[i][iinput] = 0;				/* i/f-to-tristim     */
      repeat(jinput,0,nin) {
        itt[i][iinput] +=
  	  mi(iinput,jinput) *
  	  data[jinput];
      }
    }
  }



  zvparm("DEVMAT",devmat,&ndevmat,&def,9,0);		/*user device matrix*/
  if (ndevmat) {					/*if there is one*/
    zvparm("DEVOFF",devoff,&ndevoff,&def,3,0);		/*user offset*/
    repeat (i,0,3) {
      offset[i] = devoff[i];				/*copy offset*/
      repeat (j,0,3) {
        ttr[i][j] = devmat[i][j];			/*copy matrix*/
      }
    }
    exponent = 1;					/*linear response*/
  }
  else {
    status = xyztorgb(dev,ttr,offset);			/*get rgb(x,y,z)*/
    exponent = TVEXP;					/*exponent. response*/
  } 
 
  repeat(rgb,0,3) {				/*mult. by device matrix*/
    out_max[rgb] = 0;
    repeat(iinput,0,nin) {
      itr[rgb][iinput] =				/*i/f-to-rgb*/
        ttr[rgb][0] * itt[0][iinput] +
        ttr[rgb][1] * itt[1][iinput] +
        ttr[rgb][2] * itt[2][iinput];
      out_max[rgb] += itr[rgb][iinput];			/*itr term for I/F=1*/
    }
  }

  max_max = max(out_max[0],out_max[1]);			/*max for I/F=1*/
  max_max = max(max_max,out_max[2]);

  adjustment = pow(255.,exponent) / (max_max * scale);
  repeat(iinput,0,nin) {
    if (project[iinput] == VGR) {
      /*get ficor multiplier*/
      mode = 1;
      zficor (input_unit[iinput], label_buf, &con[iinput], mode);
    }
    else {
      con[iinput] = 1.0;
    }
  }
  zvmessage (" ",0);
  zvmessage ("TRANSFORMATION MATRIX:",0);		/*hdr for xform matrix*/
  zvmessage ("INPUT           RED            GREEN           BLUE",0);
  repeat(iinput,0,nin) {
    repeat(rgb,0,3) {
      itr[rgb][iinput] *=				/*adjust to max out=255*/
        adjustment * con[iinput];
    }
    sprintf(alpha_buf,"%-d    %15.5f %15.5f %15.5f",
  	    iinput + 1,
  	    itr[RED][iinput],
  	    itr[GREEN][iinput],
  	    itr[BLUE][iinput]);
    zvmessage (alpha_buf,0);				/*print xform matrix*/
  }  
}



create_output()
/*This function multiplies the input images by the matrix itt[][], thus
creating the red, green, and blue output images*/ 

{
  int red_unit,green_unit,blue_unit;
  double red,green,blue,max_intensity;
  unsigned char red_output[MSAMPLES],green_output[MSAMPLES],
    blue_output[MSAMPLES];
  unsigned short input[MFILTERS][MSAMPLES],*pinput;
  int iinput,line,sample,lamda;

  status = zvunit(&red_unit,"OUT",1,0);		/*create output files*/
  check_status(2,1);
  status = zvunit(&green_unit,"OUT",2,0);
  check_status(2,2);
  status = zvunit(&blue_unit,"OUT",3,0);
  check_status(2,3);
  status = zvopen(red_unit,"OP","WRITE",
  	 "O_FORMAT","BYTE","U_FORMAT","BYTE",
  	 "OPEN_ACT","SA",0);
  status = zvopen(green_unit,"OP","WRITE",
  	 "O_FORMAT","BYTE","U_FORMAT","BYTE",
  	 "OPEN_ACT","SA",0);
  status = zvopen(blue_unit,"OP","WRITE",
  	 "O_FORMAT","BYTE","U_FORMAT","BYTE",
  	 "OPEN_ACT","SA",0);


  repeat(line,0,nlines) {				/*for each line*/
    repeat(iinput,0,nin) {				/*for each input*/
      status = zvread(input_unit[iinput],input[iinput],0);	/*read a line*/
    }
    repeat(sample,0,nsamples) {				/*for each sample*/

      red = offset[RED];				/*output=itr*input+ */
      green = offset[GREEN];				/*  offset	    */
      blue = offset[BLUE];
      for (iinput = 0, pinput = &input[0][sample];
  	   pinput < input[nin];
           iinput++, pinput += MSAMPLES) {
        red += itr[RED][iinput] * *pinput;
        green += itr[GREEN][iinput] * *pinput;
        blue += itr[BLUE][iinput] * *pinput;
      }

      max_intensity = pow(255.,exponent);			/*max possible intensity*/
      if (red < 0) {
        red_output[sample] = 0;
      }
      else if (red > max_intensity) {
        red_output[sample] = 255;
      }
      else {
       red_output[sample] = pow(red,1 / exponent);		/*CRT response*/
      }

      if (green < 0) {
        green_output[sample] = 0;
      }
      else if (green > max_intensity) {
        green_output[sample] = 255;
      }
      else {
       green_output[sample] = pow(green,1 / exponent);	/*CRT response*/
      }

      if (blue < 0) {
        blue_output[sample] = 0;
      }
      else if (blue > max_intensity) {
        blue_output[sample] = 255;
      }
      else {
       blue_output[sample] = pow(blue,1 / exponent);	/*CRT response*/
      }
    }

    status = zvwrit(red_unit,red_output,0);		/*write the line*/
    check_status(2,7);
    status = zvwrit(green_unit,green_output,0);
    check_status(2,8);
    status = zvwrit(blue_unit,blue_output,0);
    check_status(2,8);
  }

  status = zvclose(red_unit,0);			/*close output files*/
  check_status(2,9);
  status = zvclose(green_unit,0);
  check_status(2,10);
  status = zvclose(blue_unit,0);
  check_status(2,11);

  repeat(iinput,0,nin) {				/*close input files*/
    status = zvclose(input_unit[iinput],0);
    check_status(2,12);
  }
  sprintf(alpha_buf,"OUTPUTS HAVE BEEN CREATED WITH %d LINES AND %d SAMPLES\n",
          nlines,nsamples);
  zvmessage (alpha_buf,0);
}



/*This function prints a message if status is negative or even.*/

check_status(function,location)
int function,location;
{

  if (status < 0 || !(status & BIT0)) {
    sprintf(alpha_buf,
  	    "SYSTEM ERROR %d AT LOCATION %d.%d",
  	    status,function,location);
    zvmessage (alpha_buf,0);
  }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create giaconda.imake
#define PROGRAM giaconda

#define MODULE_LIST giaconda.c

#define MAIN_LANG_C
#define R2LIB
/* #define TEST */
/* #define USES_FORTRAN */
#define USES_C
#define LIB_FORTRAN
#define LIB_RTL
#define LIB_TAE
/* #define LIB_LOCAL */
#define LIB_P2SUB
$ Return
$!#############################################################################
$PDF_File:
$ create giaconda.pdf
process help=*

parm INP	count = 1:8
parm SCFNAME	count = 1:1                     ! Special Colors Filename
parm OUT	count = (0,3) +			!r,g,b or none
  		default = --
parm COLORS	count = 1:10			!special colors
parm WEIGHTS	type = real +			!for least-squares fit
  		count = 0:10 +
  		default = -- 				!1/color
parm DEVICE	type = keyword +		!output device
  		valid = "TV" +
  		default = "TV"
parm SCALE	type = integer +		!max dn
  		default = 10000				!I/F = 1
parm DEVMAT     count = (0,9) +                 !user device matrix
                type = real +                
                default = --
parm DEVOFF	count = 3 +			!user device offset
  		type = real +
  		default = (0,0,0)
end-proc



.title
VICAR2 program GIACONDA

.help
PURPOSE

Welcome to GIACONDA (Generator of Images of Accurate Color On Numerous Digital 
Apparatus).  GIACONDA transforms images taken through several filters into a
color image in which designated spectra ("special colors") are reproduced
accurately.  Special colors are defined, modified and examined using VICAR
program LEONARDO.  At this time, the only output device supported by this
program is  the CONRAC 7211 long persistence color monitor. 

EXECUTION

  The program is executed by specifying up to eight half-word images (registed,
FICOR'ed and VGRFIX'ed, or GALSOS'ed) taken through different filters, up to
ten special colors, and, optionally, three output files (red, blue, and green),
a weight for each special color (applied to a least-square fit), and a scale
factor for the input images, and an image display device.  The program will
display the resulting transformation matrix (input dn to output intensity) and,
if output files have been specified, will generate images which may be input to
the specified device. 

  This program reads the special colors from a file pointed to by the 
logical name, SCFNAME.  The file is a 100-line by 70-sample VICAR image
file, which must be created outside of this program.  The same file is read
and written by LEONARDO to define these colors. 

  If the program indicates "NO UNIQUE SOLUTION", there are several possible
reasons for the failure:
  1) Two of the input images were taken through the same filter.
  2) One of the special colors is a linear combination of the others.
  3) One of the special-color spectra does not overlap any of the filter
  	 responses.
  4) One of the filter responses does not overlap any of the special-color
  	spectra.
  5) One of the filters passes nothing in the visible (e.g., VGR UV).

OPERATION

  GIACONDA uses the classical photo-reproduction solution (with a slight
modification) to accurately reproduce selected ("special") colors.  For a
given pixel,  the I/F value for each of n filters is the sum of the tristimulus
values for that pixel, and, therefore, each of the tristimulus values of a
given pixel may be written as a linear combination of the I/F values obtained
from each filter.  If these equations (with unknown coefficients) are written
down for a n special colors, the we have, for each chromaticity coordinate, n
equations with n unknowns.  Therefore, we can solve for the unknown
coefficients, and we have the tristimulus values of a pixel as a function of
the I/F values from each filter.  And, in fact, any linear combination of
special colors will be accurately reproduced with this solution.  If there are
more special colors than filters, a least-squares fit solution may be applied,
as is done by this program. 

  The next step is to convert the tristimulus values to device inputs.  A
monitor's intesity is equal to the sum of the intensities of the three
primaries, and, therefore, the intensity for each phosphor can be written as a
linear combination of the desired tristimulus values.  Intensity is then
converted to input dn by 

  			DN = I ** 1/2.6,

the 2.6 in the exponent being good for all contemporary CRT's.  (It is assumed 
here that the monitor has been adjusted so the zero dn produces 0+ intensity.)
The final dn values are then normalized so that the highest expected input-
image dn produces a monitor dn of 255 (see SCALE), and out-of-range values are 
set at 0 or 255.  This exponential results in some surprising effects (e.g.,
bright skies due to residual dark current).  Only a proportional stretch on the
output images will maintain the proper hues and saturations.  THIS EXPONENT IS
NOT APPLIED IF PARAMETER, DEVMAT, IS SPECIFIED.

  The device response for a film recorder is a tougher nut to crack.  Although 
this program has hooks for that capability, it is not implemented at this time 
(see memo MSD:384-86-105, "Accurate Color Reproduction", R. Brill).

EXAMPLE

  In the following examples the user creates output files, OUT.*, using input 
images INP.*, and special colors, CLOUDS, VOLCANOES, SULPHUR, and SAND, with 
corresponding weights, 3, 2, 1, and 1, respectively.  He uses wants input dn's 
of 10000 to be the maximum brightness (default).

  VICAR>GIACONDA INP=(INP.ORA,INP.CLR,INP.GRE)+
  VICAR>+ OUT=(OUT.RED,OUT.GRE,OUT.BLU)+
  VICAR>+ SCFNAME=(COLORS.DAT)+
  VICAR>+ COLORS=(CLOUDS,VOLCANOES,SULPHUR,SAND) WEIGHTS=(3.,2.)
 

WRITTEN BY: 		   R. BRILL, 20 JUNE, 1986

COGNIZANT PROGRAMMER:	   R. BRILL, 20 JUNE, 1986

Made portable for UNIX ... J. TURNER (CRI),  5 Sept 1994

!

.level1

.vari INP
input image files
.vari OUT
output image files
.vari COLORS
special colors
.vari SCFNAME
special colors file
.vari WEIGHTS
special color weights
.vari SCALE
input image maximum dn
.vari DEVICE
output device
.vari DEVMAT
user device matrix
.vari DEVOFF
user device offset

!

.level2

.vari INP
INP = (input-1,...,input-n), where the inputs are up to eight input image
filenames of registered, FICOR'ed, and VGRFIX'ed images.  The images must have
been taken with different filters and must all be the same size. 

.vari OUT
[OUT = (red-output,green-output,blue-output)], where the -output's are image 
filenames to be used as inputs to the display device.  If none is specified, 
the program will only compute and display the transformation matrix.

.vari COLORS
COLORS = (color-1,...,color-n), where the colors are up to 10 names of special 
colors defined by the program LEONARDO.  There must be at least as may colors 
named as their are input images, and none of the colors may be a linear 
combination of the others.

.vari SCFNAME
SCFNAME = (input-1), the special colors input file must be specified from
which GIACONDA reads the special colors. The file is a 100-line by 70-sample 
VICAR image file.

.vari WEIGHTS
[WEIGHTS=(weight-1,...,weight-n)], where each weight corresponds to a special 
color (as they are ordered in the COLORS specification).  These weights are 
used in the least-squares-fit solution and specify the recipricol of the 
square of sigma.  The number of weights specified must be at most equal to the 
number of colors specified.  If fewer are specified, the remainder of them 
will be set to 1.

.vari SCALE
[SCALE = max-dn], where max-dn is the maximum input dn that will not be 
saturated on the output.  The default is 10000 (normally corresponding to I/F 
= 1).

.vari DEVICE
[DEVICE = image-device], where image-device specifies the device on which the 
output image files are to be displayed.  The only valid device at this time is 
the default, 'TV (Conrac 7211). 

.vari DEVMAT
[DEVMAT = user-device-matrix], where user-device-matrix is used to convert 
chomaticity cooridinates to devise inputs.  The default value is the matrix
generated by the program based on the parameter DEVICE.  The order of entry is

  	rx, ry, rz, gx, gy, gz, bx, by, bz,

where, for example, rx is the x coefficient in the linear equation for the
red device input.

.vari DEVOFF
[DEVOFF = user-device-offset], where user-device-offset is a vector which is
added to device inputs.  This parameter is ignored if DEVMAT is not entered.
Its default is (0,0,0).  The order of entry is

  	r, g, b,

where, for example, r is the constant coefficient in the linear equation for
the red device input.

.end
$ Return
$!#############################################################################
$Test_File:
$ create tstgiaconda.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
!!!!!!!!!
Write " "
Write " The Test Data are handeled for both VMS and UNIX in this PDF."
Write " At present (May 1994), in order to run this program, the"
Write " following data files MUST be copied to the LOCAL directory"
Write " where the program resides:"
Write " "
Write "                        OLD       NEW  (VMS or UNIX execution)"
Write " mipldisk:[mipl.vgr]colors.dat => colors.dat"
Write " mipldisk:[mipl.vgr]uranus.ora => uranus.ora"
Write " mipldisk:[mipl.vgr]uranus.gre => uranus.gre"
Write " mipldisk:[mipl.vgr]uranus.blu => uranus.blu"
Write " v2$scratch:out.red            => out.red"
Write " v2$scratch:out.gre            => out.gre"
Write " v2$scratch:out.blu            => out.blu"
Write " "
Write " "
let $echo="yes"
!!
!!
giaconda inp=(uranus.ora,uranus.gre,uranus.blu), +
         out=(out.red,out.gre,out.blu), +
         scfname=(colors.dat), +
         colors=(red,blue,gray)
!!
!!
end-proc
$ Return
$!#############################################################################
