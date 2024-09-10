$!****************************************************************************
$!
$! Build proc for MIPL module pixstat
$! VPACK Version 1.9, Tuesday, March 13, 2001, 10:57:45
$!
$! Execute by entering:		$ @pixstat
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
$ write sys$output "*** module pixstat ***"
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
$ write sys$output "Invalid argument given to pixstat.com file -- ", primary
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
$   if F$SEARCH("pixstat.imake") .nes. ""
$   then
$      vimake pixstat
$      purge pixstat.bld
$   else
$      if F$SEARCH("pixstat.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pixstat
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pixstat.bld "STD"
$   else
$      @pixstat.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pixstat.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pixstat.com -mixed -
	-s pixstat.c unit_filter.c unit_filter2.c output.c -
	-i pixstat.imake -
	-p pixstat.pdf -
	-t tstpixstat.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pixstat.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c"
#include <math.h>
#include <stdio.h>

void compute_mean(short *ibuf,float *mean,int *vs);
void compute_sigma(int opcode,short *ibuf,float *mean,double *moment,
        int *vs,double *vs2);
void unit_filter(int *vs,float *mean,int ns,int nsw,double scale);
void unit_filter2(int opcode,int *vs,double *vs2,float *mean,double *moment,
	int ns,int nlw,int nsw);
void output(int ounit,float *mean,int nso,double mindn,double maxdn);
void output2(int ounit,float *mean,double *moment,
	int nso,double mindn,double maxdn,double scale);

int iunit,sl,ss,nl,ns;
int ounit,slo,sso,nlo,nso,elo;
int nlw,nsw;		/* filter window size is nlw x nsw */
int nlwh,nswh;
double scale,mindn,maxdn;

void main44()
{
  short *ibuf;    /* nlw+1 consecutive image lines = ibuf[nlw+1][ns] */
  float *mean;    /* mean of image line = mean[nso] */
  double *moment; /* 2nd moment of image line = moment[nso] */
  int *vs;        /* vs[i] = sum of DN in column i */
  double *vs2;    /* vs2[i] = sum of DN**2 in column i */

  float rscale;
  int sli,ssi,nli,nsi,el,es;
  int icode,ocode;
  int cnt,status;
  int opcode;			/* 1=mean, 2=moment, 3=variance, 4=sdev */
  char format[5],oformat[5];	/* input and output data formats */
  char msg[80];

  zvmessage("pixstat version March 15, 2001",0);

  status = zvunit(&iunit,"INP",1,0);
  zvopen(iunit,"U_FORMAT","HALF","OPEN_ACT","SA","IO_ACT","SA",0);
  status = zvget(iunit,"FORMAT",format,0);
  icode = 0;
  if (!strcmp(format,"BYTE")) icode=1;
  if (!strcmp(format,"HALF")) icode=2;
  if (icode == 0) goto WRONG_FORMAT;
  zvsize(&sli,&ssi,&nlo,&nso,&nli,&nsi);
  sli--;	/* pixstatel coordinates start at (0,0) rather than (1,1) */
  ssi--;

  if (nlo>nli-sli) nlo=nli-sli;		/* Check size of output image */
  if (nso>nsi-ssi) nso=nsi-ssi;

  zvp("FORMAT",oformat,&cnt);
  if (cnt == 0) strcpy(oformat,format);
  if (!strcmp(oformat,"BYTE")) ocode=1;
  if (!strcmp(oformat,"HALF")) ocode=2;
  if (!strcmp(oformat,"FULL")) ocode=4;
  if (!strcmp(oformat,"REAL")) ocode=7;
  status = zvunit(&ounit,"OUT",1,0);
  zvopen(ounit,"OP","WRITE","U_FORMAT","REAL","O_FORMAT",oformat,
        "U_NL",nlo,"U_NS",nso,"OPEN_ACT","SA","IO_ACT","SA",0);

  maxdn = 0.;
  if (ocode==1) {
     mindn = 0;
     maxdn = 255;
  }
  if (ocode==2) {
     mindn = -32768.;
     maxdn = 32767.;
  }
  if (ocode==4) {
     mindn = -32768.*65536.;
     maxdn = 32768.*65536.-1.;
  }

	/* Get size of filter window */
  zvp("NLW",&nlw,&cnt);
  zvp("NSW",&nsw,&cnt);
  nlwh = nlw/2;
  nswh = nsw/2;
  nlw = 2*nlwh + 1;		/* Force window to have odd value dimensions */
  nsw = 2*nswh + 1;
  sprintf(msg,"NLW=%d NSW=%d",nlw,nsw);
  zvmessage(msg,0);

  if (zvptst("MEAN")) opcode=1;
  if (zvptst("MOMENT")) opcode=2;
  if (zvptst("VARIANCE")) opcode=3;
  if (zvptst("SDEV")) opcode=4;

  zvp("SCALE",&rscale,&cnt);
  scale = rscale;

	/* Compute size of input image needed to create output image */
  sl = sli - nlwh;
  ss = ssi - nswh;
  el = sli + nlo + nlwh - 1;
  es = ssi + nso + nswh - 1;
  if (sl < 0) sl=0;
  if (ss < 0) ss=0;
  if (el >= nli) el=nli-1;
  if (es >= nsi) es=nsi-1;
  nl = el - sl + 1;
  ns = es - ss + 1;	/* Final input size is (sl,ss,nl,ns) */

	/* Compute where the output image starts and ends */
  slo = sli - sl;
  sso = ssi - ss;
  elo = slo + nlo - 1;

  ibuf = (short *) malloc((nlw+1)*ns*sizeof(short));
  vs = (int *) malloc(ns*sizeof(int));
  mean = (float *) malloc(ns*sizeof(float));

  if (opcode == 1) compute_mean(ibuf,mean,vs);
  else {
     vs2 = (double *) malloc(ns*sizeof(double));
     moment = (double *) malloc(ns*sizeof(double));
     compute_sigma(opcode,ibuf,mean,moment,vs,vs2);
     free(moment);
     free(vs2);
  }
  free(vs);
  free(mean);
  free(ibuf);
  return;

WRONG_FORMAT:
  zvmessage("***Input image must be in byte format",0);
FATAL_ERROR:
  zvmessage("***pixstat task cancelled",0);
  zabend();
}
/*****************************************************************************/
/* Compute mean image.							     */
/*****************************************************************************/
void compute_mean(
	short *ibuf,	/* nlw+1 consecutive image lines = ibuf[nlw+1][ns] */
	float *mean,	/* output image line = mean[nso] */
	int *vs)	/* vs[i] = sum of DN in column i */
{
  int l,s,n0;
  short *top,*bot,*max;

  scale = scale/(nlw*nsw);
  max = &ibuf[nlw*ns];
  n0 = nlwh + 1;	/* vertical pixstatel distance to middle of window */

	/* Initialize column sums with pixstatels from top margin of image */
  zvread(iunit,ibuf,"LINE",sl+1,"SAMP",ss+1,"NSAMPS",ns,0);
  for (s=0; s<ns; s++) vs[s]=ibuf[s];
  bot = ibuf + ns;
  for (l=1; l<=nlwh; l++) {
     zvread(iunit,bot,"SAMP",ss+1,"NSAMPS",ns,0);
     for (s=0; s<ns; s++) vs[s]+=2*bot[s];
     bot += ns;
  }

	/* Filter top margin */
  top = bot - ns;
  for (l=0; l<nlwh; l++) {
     if (l >= slo) {
        unit_filter(vs,mean,ns,nsw,scale);
        output(ounit,&mean[sso],nso,mindn,maxdn);
     }
     zvread(iunit,bot,"SAMP",ss+1,"NSAMPS",ns,0);
     for (s=0; s<ns; s++) vs[s]=vs[s]-top[s]+bot[s];
     top -= ns;
     bot += ns;
  }

	/* Filter internal lines */
  for (l=nlwh; l<nl-n0; l++) {
     unit_filter(vs,mean,ns,nsw,scale);
     output(ounit,&mean[sso],nso,mindn,maxdn);
     zvread(iunit,bot,"SAMP",ss+1,"NSAMPS",ns,0);
     for (s=0; s<ns; s++) vs[s]=vs[s]-top[s]+bot[s];
     bot = top;
     top += ns;
     if (top > max) top=ibuf;
  }

	/* Point bottom to next to last line in image to start reflection */
/*  l = (nl-2) - (nlw+1)*((nl-2)/(nlw+1));
  bot = &ibuf[l*ns]; */
  bot -= ns;
  if (bot < ibuf) bot=max;
  bot -= ns;
  if (bot < ibuf) bot=max;
  
	/* Filter bottom margin */
  for (l=nl-n0; l<nl; l++) {
     unit_filter(vs,mean,ns,nsw,scale);
     output(ounit,&mean[sso],nso,mindn,maxdn);
     if (l >= elo) break;
     for (s=0; s<ns; s++) vs[s]=vs[s]-top[s]+bot[s];
     top += ns;
     bot -= ns;
     if (bot < ibuf) bot=max;
     if (top > max) top=ibuf;
  }
}
/*****************************************************************************/
/* Compute moment, variance, or sigma image.				     */
/*****************************************************************************/
void compute_sigma(
	int opcode,	/* 2=moment, 3=variance, 4=sigma */
	short *ibuf,    /* nlw+1 consecutive image lines = ibuf[nlw+1][ns] */
	float *mean,    /* mean of image line = mean[nso] */
	double *moment, /* second moment of image line = moment[nso] */
	int *vs,        /* vs[i] = sum of DN in column i */
	double *vs2)    /* vs2[i] = sum of DN**2 in column i */
{
  int l,s,n0,dn,dn0;
  short *top,*bot,*max;

  n0 = nlwh + 1;	/* vertical pixstatel distance to middle of window */
  max = &ibuf[nlw*ns];

	/* Initialize column sums with pixstatels from top margin of image */
  zvread(iunit,ibuf,"LINE",sl+1,"SAMP",ss+1,"NSAMPS",ns,0);
  for (s=0; s<ns; s++) {
     dn = ibuf[s];
     vs[s] = dn;
     vs2[s] = dn*dn;
  }

  bot = ibuf + ns;
  for (l=1; l<=nlwh; l++) {
     zvread(iunit,bot,"SAMP",ss+1,"NSAMPS",ns,0);
     for (s=0; s<ns; s++) {
        dn = bot[s];
        vs[s] += 2*dn;
        vs2[s] += 2*dn*dn;
     }
     bot += ns;
  }

	/* Filter top margin */
  top = bot - ns;
  for (l=0; l<nlwh; l++) {
     if (l >= slo) {
        unit_filter2(opcode,vs,vs2,mean,moment,ns,nlw,nsw);
        output2(ounit,mean,&moment[sso],nso,mindn,maxdn,scale);
     }
     zvread(iunit,bot,"SAMP",ss+1,"NSAMPS",ns,0);
     for (s=0; s<ns; s++) {
         dn0 = top[s];
         dn  = bot[s];
         vs[s] = vs[s] - dn0 + dn;
         vs2[s] = vs2[s] - dn0*dn0 + dn*dn;
     }
     top -= ns;
     bot += ns;
  }

	/* Filter internal lines */
  for (l=nlwh; l<nl-n0; l++) {
     unit_filter2(opcode,vs,vs2,mean,moment,ns,nlw,nsw);
     output2(ounit,mean,&moment[sso],nso,mindn,maxdn,scale);
     zvread(iunit,bot,"SAMP",ss+1,"NSAMPS",ns,0);
     for (s=0; s<ns; s++) {
        dn0 = top[s];
        dn  = bot[s];
        vs[s] = vs[s] - dn0 + dn;
        vs2[s] = vs2[s] - dn0*dn0 + dn*dn;
     }
     bot = top;
     top += ns;
     if (top > max) top=ibuf;
  }

	/* Point bottom to next to last line in image to start reflection */
  bot -= ns;
  if (bot < ibuf) bot=max;
  bot -= ns;
  if (bot < ibuf) bot=max;
  
	/* Filter bottom margin */
  for (l=nl-n0; l<nl; l++) {
     unit_filter2(opcode,vs,vs2,mean,moment,ns,nlw,nsw);
     output2(ounit,mean,&moment[sso],nso,mindn,maxdn,scale);
     if (l >= elo) break;
     for (s=0; s<ns; s++) {
        dn0 = top[s];
        dn  = bot[s];
        vs[s] = vs[s] - dn0 + dn;
        vs2[s] = vs2[s] - dn0*dn0 + dn*dn;
     }
     top += ns;
     bot -= ns;
     if (bot < ibuf) bot=max;
     if (top > max) top=ibuf;
  }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create unit_filter.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*****************************************************************************/
/* 1-D unit filter							     */
/*****************************************************************************/
void unit_filter(
  int *vs,		/* input column sums */
  float *obuf,          /* output image line */
  int ns,	        /* number of samples on image line */
  int nsw,		/* filter window size */
  double scale)		/* dn = scale*dn + offset */
{
  int s,z,nswh,n0,sum;

  nswh = nsw/2;
  n0 = nswh + 1;

	/* Compute sum of first area */
  sum = vs[0];
  for (s=1; s<=nswh; s++) sum+=2*vs[s];

	/* Left margin */
  for (s=0; s<nswh; s++) {
     obuf[s] = sum*scale;
     sum = sum - vs[nswh-s] + vs[s+n0];
  }

	/* Middle pixstatels */
  for (s=nswh; s<ns-n0; s++) {
     obuf[s] = sum*scale;
     sum = sum - vs[s-nswh] + vs[s+n0];
  }

	/* Right margin */
  z = 2;
  for (s=ns-n0; s<ns; s++) {
     obuf[s] = sum*scale;
     sum = sum - vs[s-nswh] + vs[ns-z];
     z++;
  }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create unit_filter2.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*****************************************************************************/
/* Compute mean and second moment of image line.                             */
/*****************************************************************************/
#include <math.h>
void unit_filter2(
  int opcode,		/* 2=moment, 3=variance, 4=sigma */
  int *vs,		/* input column sums */
  double *vs2,		/* input column sums**2 */
  float *mean,          /* mean of image line */
  double *moment,       /* second moment of image line */
  int ns,	        /* number of samples on image line */
  int nlw,		/* filter window height */
  int nsw)		/* filter window width */
{
  int s,z,nswh,n0,sum;
  double sum2,narea;

  narea = nlw*nsw;
  nswh = nsw/2;
  n0 = nswh + 1;

	/* Compute sum of first area */
  sum = vs[0];
  sum2 = vs2[0];
  for (s=1; s<=nswh; s++) {
     sum += 2*vs[s];
     sum2 += 2*vs2[s];
  }

	/* Left margin */
  for (s=0; s<nswh; s++) {
     mean[s] = sum/narea;
     moment[s] = sum2/narea;
     sum = sum - vs[nswh-s] + vs[s+n0];
     sum2 = sum2 - vs2[nswh-s] + vs2[s+n0];
  }

	/* Middle pixstatels */
  for (s=nswh; s<ns-n0; s++) {
     mean[s] = sum/narea;
     moment[s] = sum2/narea;
     sum = sum - vs[s-nswh] + vs[s+n0];
     sum2 = sum2 - vs2[s-nswh] + vs2[s+n0];
  }

	/* Right margin */
  z = 2;
  for (s=ns-n0; s<ns; s++) {
     mean[s] = sum/narea;
     moment[s] = sum2/narea;
     sum = sum - vs[s-nswh] + vs[ns-z];
     sum2 = sum2 - vs2[s-nswh] + vs2[ns-z];
     z++;
  }
  if (opcode == 2) return;
  if (opcode == 3) {
     for (s=0; s<ns; s++) moment[s]=moment[s]-mean[s]*mean[s];
  }
  else {
     for (s=0; s<ns; s++) {
        moment[s]=moment[s]-mean[s]*mean[s];
        if (moment[s] > 0.) moment[s]=sqrt(moment[s]);
        else moment[s]=0.;
     }
  }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create output.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*****************************************************************************/
/* Write output image line.  If output data format is not REAL, round of the */
/* DN value and truncate as necessary to fit output data format.	     */

void output(int ounit,
            float *mean,
            int nso,
            double mindn,
            double maxdn)
{
  int i;
  float dn;

  if (maxdn != 0.) {		/* skip if output data format is REAL */
     for (i=0; i<nso; i++) {
         dn = mean[i];
         if (dn < 0.) {
            dn -= 0.5;		/* round down */
            if (dn < mindn) dn=mindn;
         }
         else {
            dn += 0.5;		/* round up */
            if (dn > maxdn) dn=maxdn;
         }
         mean[i] = dn;
     }
  }
  zvwrit(ounit,mean,0);
}
/*****************************************************************************/
/* Write output image line.  If output data format is not REAL, round of the */
/* DN value and truncate as necessary to fit output data format.	     */

void output2(int ounit,
            float *mean,
            double *moment,
            int nso,
            double mindn,
            double maxdn,
            double scale)
{
  int i;
  double dn;

  if (maxdn == 0.) {
     for (i=0; i<nso; i++) mean[i]=scale*moment[i];
  }
  else {
     for (i=0; i<nso; i++) {
         dn = scale*moment[i];
         if (dn < 0.) {
            dn -= 0.5;		/* round down */
            if (dn < mindn) dn=mindn;
         }
         else {
            dn += 0.5;		/* round up */
            if (dn > maxdn) dn=maxdn;
         }
         mean[i] = dn;
     }
  }
  zvwrit(ounit,mean,0);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pixstat.imake
/* Imake file for pixstat */

#define PROGRAM pixstat

#define MODULE_LIST pixstat.c unit_filter.c unit_filter2.c output.c
#define MAIN_LANG_C
#define USES_ANSI_C
#define R2LIB
#define LIB_P2SUB
#define LIB_MATH77
#define LIB_RTL
#define LIB_TAE
$ Return
$!#############################################################################
$PDF_File:
$ create pixstat.pdf
PROCESS help=*
PARM INP    TYPE=STRING  COUNT=1
PARM OUT    TYPE=STRING  COUNT=1
PARM SIZE   TYPE=INTEGER COUNT=0:4 DEFAULT=--
PARM SL     TYPE=INTEGER COUNT=0:1 DEFAULT=1
PARM SS     TYPE=INTEGER COUNT=0:1 DEFAULT=1
PARM NL     TYPE=INTEGER COUNT=0:1 DEFAULT=0
PARM NS     TYPE=INTEGER COUNT=0:1 DEFAULT=0
PARM NLW    TYPE=INTEGER COUNT=0:1 VALID=(3:129)	DEFAULT=11
PARM NSW    TYPE=INTEGER COUNT=0:1 VALID=(3:129)	DEFAULT=11
PARM SCALE  TYPE=REAL    COUNT=0:1 			DEFAULT=1.
PARM CALC   TYPE=KEYWORD COUNT=0:1 VALID=(MEAN,SDEV,VARIANCE,MOMENT)  DEF=SDEV
PARM FORMAT TYPE=KEYWORD COUNT=0:1 VALID=(BYTE,HALF,FULL,REAL)	DEFAULT=--
END-PROC
.TITLE 
VICAR Program PIXSTAT
.HELP

PURPOSE:

PIXSTAT is a VICAR applications program for computing the mean, second
moment, variance, or standard deviation of the local area surrounding each
pixstatel in an image.

EXECUTION:

  PIXSTAT IN OUT NLW=m NSW=n 'CALC
where
  IN is the input image (byte or halfword),
  OUT is the output image (byte, halfword, fullword, or real),
  NLW and NSW specify the height and width of the local area,
  CALC is a keyword selecting the statistic to be computed.

CALC has the following valid values: MEAN, MOMENT, VARIANCE, SDEV.

.page
OPERATION:

PIXSTAT is a filtering operation.  For example, the following two executions
will result in the same output:

	PIXSTAT  A  B  NLW=3 NSW=3  'MEAN
        FILTER   A  B  NLW=3 NSW=3  WEIGHTS=(1,1,1,1)

However, unlike the general purpose FILTER program, the output of PIXSTAT 
is limited to one of the following statistical quantities:  mean, second
moment, variance, and standard deviation.  The CALC keyword is used to select
the statistic to be computed.  The valid values for CALC are MEAN, MOMENT,
VARIANCE, SDEV.

The pixstatel height and width of the filter window is specified by the NLW and
NSW parameters.  For example,

	PIXSTAT  A  B  NLW=9 NSW=11  'SDEV

will cause each pixstatel to be replaced by the standard deviation of the 9 x 11
pixstatel area centered on that pixstatel.

The input image may be in BYTE or HALFWORD data format.

The FORMAT keyword may be used to specify the output data format.  The valid
output formats are BYTE, HALF, FULL, and REAL.  If the output is in integer
format (BYTE, HALF, or FULL), the values are rounded to the nearest integer
and values outside the output DN range are truncated.

If the output format is not specified, the output will have the same format as
the input image.

If MOMENT or VARIANCE are specified, the output DN range may be larger than the
input DN range.  One or more of the following options can be used to avoid data
truncation:

  1) Increase the number of bits in the output via the FORMAT keyword
  2) Specify a floating point output (FORMAT=REAL)
  3) Scale the output using the SCALE parameter

.page
STATISTICAL EQUATIONS:

Let N be the number of pixstatels in the filter window, i.e. N=NLW*NSW.  The MEAN
is the sum of all DN values in the window divided by N:

		       SUM of DN
		MEAN = ---------
			   N

The SECOND MOMENT is the sum of the squares of all DN values in the window
divided by N:
			 SUM of DN**2
		MOMENT = ------------
			     N

The VARIANCE is the sum of squared differences from the mean divided by N:

		    SUM of (DN - MEAN)**2
	VARIANCE = ----------------------- = MOMENT - MEAN**2
			     N

The STANDARD DEVIATION is the square root of the variance.

		SDEV = VARIANCE**0.5

For pixstatels along the margins of the image, the surrounding area is completed
by reflecting the image at its boundaries.  E.g. if the upper-left pixstatel
is assigned coordinates (0,0), then the surrounding area for this pixstatel is
filled in via the following steps:

	1) DN(-i,j) = DN(i,j)	i=-1,-2,-3,...,-NLW/2, j=0,1,2,...,NSW/2
	2) DN(i,-j) = DN(i,j)   j=-1,-2,-3,...,-NSW/2, i=-NLW/2 to +NLW/2

.page
MEMORY REQUIREMENTS:

Since memory is dynamically allocated by the program, there are no restrictions
on the size of the input image or the size of the filter window.  However, if
either of these is larger than the available physical memory, an excessive
number of page faults will occur, and images which normally take seconds
to process may instead require hours.  The relevant variables are the width of
the image (NS) and the height of the filter (NLW).  Total memory requirements
depend also on which variable is being calculated:

       'MEAN:  number of bytes = 2*(NLW+5)*NS
       OTHER:  number of bytes = 2*(NLW+13)*NS

Note that no extra memory is required to reflect the image at the margins since
this operation is conceptually rather than physically performed.

Under normal operation, it should take no more than 6 seconds to compute the
standard deviation for a 1000 x 1000 image.

.page
PROGRAM HISTORY:

Written by: Gary Yagi, March 15, 2001
Cognizant Programmer: Gary Yagi
Revisions: New

.LEVEL1
.VARI INP
Input image (byte or half)
.VARI OUT
Output image
.VARI NLW
Optional integer
pixstatel height of local area
.VARI NSW
Optional integer
pixstatel width of local area
.VARI SCALE
Optional real
Output DN scale factor
.VARI CALC 
Optional keyword
Valid=MEAN, MOMENT, VARIANCE
  or SDEV
.VARI NL
Number of input lines
.VARI NS
Number of input samples (Maximum 4000)
.VARI SL
Starting line
.VARI SS
Starting sample
.VARI SIZE
Standard VICAR size field
.LEVEL2
.VARI INP
Input image filename.  Input data format must be byte or halfword.
There are no restrictions on image size.  However, see help file for memory
requirements.
.VARI OUT
Output image filename.  Output data format will be the same as input, unless
this is overridden via the FORMAT keyword.
.VARI NLW
The number of lines in the local area window.  If an even value is specified,
the next larger odd value will be used.  Default NLW=11.

There are no restrictions on NLW.  However, see help file for memory
requirements.

.VARI NSW
The number of samples in the local area window.  If an even value is specified,
the next larger odd value will be used.  Default NSW=11.

.VARI SCALE
Optional scaling factor:

	SCALE=r

Each output pixstatel will be multiplied by the scaling factor r.  Default SCALE=1.

.VARI CALC
A keyword selecting the statistic to be calculated.
'MEAN, 'MOMENT, 'VARIANCE and 'SDEV (standard deviation)
are the valid selections. 'SDEV is the default selection.

.VARI FORMAT
Optional keyword specifying data format of output image.  Valid values are
BYTE, HALF, FULL, or REAL.  If defaulted, the output will have the same data
format as the input image.

.VARI	SIZE
The size parameter determines the boundaries in the input
file on which the statistics are to be gathered.  It is specified
as  (SL,SS,NL,NS), where
	SL is the starting line 
	SS is the starting sample
	NL is the number of lines to be copied
	NS is the number of samples (pixstatels) in each line
.VARI NL
Number of input lines
.VARI NS
Number of input samples
.VARI SL
Starting line
.VARI SS
Starting sample
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstpixstat.pdf
procedure
refgbl $echo
refgbl $syschar
body
let _onfail="continue"
let $echo="yes"
local path type=string init="wms_test_work:[testdata.gll]"
if ($syschar(1)="UNIX")
    let path="/project/test_work/testdata/gll/"
end-if

!Use a teeny tiny part of the image for most of the test
copy &"path"7500.spk a (105,495,5,5)
pixstat a mean nlw=5 nsw=5 'mean 'real
pixstat a mom  nlw=5 nsw=5 'mom  'real
pixstat a var  nlw=5 nsw=5 'vari 'real
pixstat a sdev nlw=5 nsw=5 'sdev 'real
list mean
list mom
list var
list sdev

!Compute var2 = mom - mean**2 and compare with program output
f2 (mom,mean) var2 func="in1-in2*in2"
f2 (var,var2) diff func="in1-in2"
list diff	!Differences should be small

!Check different output formats
pixstat a mean nlw=5 nsw=5 'mean 'byte
list mean
pixstat a mean nlw=5 nsw=5 'mean 'half
list mean
pixstat a mean nlw=5 nsw=5 'mean 'full
list mean

!Compare 'MEAN with output from FILTER program
fit &"path"7500.spk a perc=.1 'byte
pixstat a b nlw=5 nsw=5 'mean
filter a c nlw=5 nsw=5 weights=(1,1,1,1,1,1,1,1,1)
difpic (b,c)		!difference should be all zeroes
end-proc
$ Return
$!#############################################################################
