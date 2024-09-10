$!****************************************************************************
$!
$! Build proc for MIPL module featherv
$! VPACK Version 1.9, Wednesday, January 03, 2001, 08:52:38
$!
$! Execute by entering:		$ @featherv
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
$ write sys$output "*** module featherv ***"
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
$ write sys$output "Invalid argument given to featherv.com file -- ", primary
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
$   if F$SEARCH("featherv.imake") .nes. ""
$   then
$      vimake featherv
$      purge featherv.bld
$   else
$      if F$SEARCH("featherv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake featherv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @featherv.bld "STD"
$   else
$      @featherv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create featherv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack featherv.com -mixed -
	-s featherv.c -
	-i featherv.imake -
	-p featherv.pdf -
	-t tstfeatherv.pdf devfeatherv.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create featherv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h"
#include <math.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

#define NUMINFILES 400

int *pstack,*istack,*vstack,dbg;
float fhnl,fhns;
float pi2 = 1.5707963;
float pi2inv = 0.6366197;

void propagate(ival,moore_bix,previx,i,redfeather,redtns,currix)
   int ival,previx,i,redfeather,redtns,currix;
   unsigned char **moore_bix;
{
   int ptr,tp,ti,tv,upix;

   ptr = 1;
   pstack[0] = previx;
   istack[0] = i;
   vstack[0] = ival;
   
   /* only 3 directions needed for this Moore algorithm */
   
   do
      {
      ptr--;
      tp = pstack[ptr];
      ti = istack[ptr];
      tv = vstack[ptr];
      moore_bix[tp][ti] = (unsigned char)tv;
      if (ti+1<redtns)
         {
         if ((int)moore_bix[tp][ti+1]>tv+1)
            {
            pstack[ptr] = tp;
            istack[ptr] = ti+1;
            vstack[ptr++] = tv+1;
            }
         }
      if (ti>0)
         {
         if ((int)moore_bix[tp][ti-1]>tv+1)
            {
            pstack[ptr] = tp;
            istack[ptr] = ti-1;
            vstack[ptr++] = tv+1;
            }
         }
      upix = (tp-1+redfeather)%redfeather;
      if (upix!=currix)
         {
         if ((int)moore_bix[upix][ti]>tv+1)
            {
            pstack[ptr] = upix;
            istack[ptr] = ti;
            vstack[ptr++] = tv+1;
            }
         }
      }
   while (ptr>0);
   return;
}

/*===================================================================

ms_find

ms_find searches string str1 for the substring str2 and returns
a pointer to the first location in string after the substring.

function return : character pointer

arguments :

      1. str1: input, char *str1;

      2. str2: input, char *str2;

Null pointer is returned if substring is not found.

*/

char *ms_find(str1, str2)
   char *str1, *str2;

{
   char *str1c;
   int str2_len = strlen(str2);

   str1c = str1;
   for (; strlen(str1c) >= str2_len; str1c++)
      if (strncmp(str1c, str2, str2_len)==0)
	 return (str1c += str2_len);
   return ((char)0);
}

char *nameget(s)
   char *s;
{
   int ix;
   
   ix = strlen(s)-1;
   while ((s[ix]!='\\')&&(s[ix]!='/')&&(ix>0)) ix--;
   if ((s[ix]=='\\')||(s[ix]=='/')) ix++;
   
   return &s[ix];
}

/*  image mosaic with feathering routine   A. Zobrist    9/14/99   */

/***********************************************************************
mz_alloc1

allocate a one dimensional array of any type

arguments:
     1. buf: output, unsigned char **buf;
	contents set to pointer to array.
     2. d1: input, int d1;
	dimension of the array
     3. w: input, int w;
	number of bytes per array element

memalign is used to align on doubleword boundary,  1 is added
to guarantee a non-zero request.  Type of data in array is
immaterial, but length is given by w parameter.  The space may be
released with the free(buf) statement.
*/
void mz_alloc1(buf,d1,w)
   int d1,w;
   unsigned char **buf;
{
   if ((*buf=(unsigned char *)malloc(1+d1*w))==NULL)
			    zmabend("malloc failed");
   return;
}

/*=========================================================

mz_alloc2

allocate a two dimensional array of any type

arguments:
     1. buf: output, unsigned char ***buf;
	contents set to pointer to array.
     2. d1: input, int d1;
	first dimension of the array
     3. d2: input, int d2;
	second dimension of the array
     4. w: input, int w;
	number of bytes per array element

memalign is used to align on doubleword boundary,  1 is added
to guarantee a non-zero request.  Type of data in array is
immaterial, but length is given by w parameter.  The space cannot
be released with a simple call to free(buf) but must be released
with a call to mz_free2(buf,d1) so all of the parts can be freed
in reverse order.
*/
void mz_alloc2(buf,d1,d2,w)
   int d1,d2,w;
   unsigned char ***buf;
{
   int i;
   if ((*buf=(unsigned char **)malloc(1+d1*4))==NULL)
			    zmabend("malloc failed");
   for (i=0;i<d1;i++)
      if (((*buf)[i]=(unsigned char *)malloc(1+d2*w))==NULL)
			    zmabend("malloc failed");
   return;
}

/*=========================================================

mz_free2

free a two dimensional array created by mz_alloc2

arguments:
     1. buf: output, unsigned char **buf;
	array to be freed (not a pointer)
     2. d1: input, int d1;
	first dimension of the array

The subparts are freed first and then the top part.  Use
the first dimension from the mz_alloc2 call.

*/
void mz_free2(buf,d1)
   int d1; unsigned char **buf;
{
   int i;
   for (i=0;i<d1;i++) free(buf[i]);
   free(buf);
   return;
}

/*=========================================================

mz_alloc3

allocate a three dimensional array of any type

arguments:
     1. buf: output, unsigned char ****buf;
	contents set to pointer to array.
     2. d1: input, int d1;
	first dimension of the array
     3. d2: input, int d2;
	second dimension of the array
     4. d3: input, int d3;
	third dimension of the array
     5. w: input, int w;
	number of bytes per array element

memalign is used to align on doubleword boundary,  1 is added
to guarantee a non-zero request.  Type of data in array is
immaterial, but length is given by w parameter.  The space cannot
be released with a simple call to free(buf) but must be released
with a call to mz_free3(buf,d1,d2) so all of the parts can be freed
in reverse order.
*/
void mz_alloc3(buf,d1,d2,d3,w)
   int d1,d2,w;
   unsigned char ****buf;
{
   int i,j;
   if ((*buf=(unsigned char ***)malloc(1+d1*4))==NULL)
			    zmabend("malloc failed");
   for (i=0;i<d1;i++)
      {
      if (((*buf)[i]=(unsigned char **)malloc(1+d2*4))==NULL)
			    zmabend("malloc failed");
      for (j=0;j<d2;j++)
	 if (((*buf)[i][j]=(unsigned char *)malloc(1+d3*w))==NULL)
			       zmabend("malloc failed");
      }
   return;
}

/*=========================================================

mz_free3

free a three dimensional array created by mz_alloc3

arguments:
     1. buf: output, unsigned char ***buf;
	array to be freed (not a pointer)
     2. d1: input, int d1;
	first dimension of the array
     3. d2: input, int d2;
	second dimension of the array

The subparts are freed first and then the top part.  Use
the first two dimensions from the mz_alloc3 call.

*/
void mz_free3(buf,d1,d2)
   int d1,d2;
   unsigned char ***buf;
{
   int i,j;
   for (i=0;i<d1;i++)
      {
      for (j=0;j<d2;j++) free(buf[i][j]);
      free(buf[i]);
      }
   free(buf);
   return;
}


/*=====================================================================
dgauss

dgauss solves a set of linear equations via gaussian elimination

arguments:

     1. a: input and output, double *a;
	m by m coefficient matrix, destroyed.
     2. r: input and output, double *r;
	input right hand m-vector; output solution.
     3. m: input, int m;
	number of linear equations.
     4. eps: input, double eps;
	gaussian pivot tolerance (usually set to 1.e-14)
     5. ierror: output int *ierror;
	result 0=OK, 1=pivot is zero, K=loss of significance warning
	pivot less than eps times max element of a

The matrix a is stored by column order

*/
void dgauss(a,r,m,eps,ierror)
      double *a,*r;
      int m,*ierror;
      double eps;
{
   double piv,tol,pfac,temp;
   int i,j,k,l,n,kt,lt,m2,ll,lu,il;

   if (m<=0) { *ierror = -1; return; }
   *ierror = 0; piv = 0.; m2 = m*m;
   for (j=1;j<=m2;j++)
      {
      temp = a[j-1]; if (temp<0.) temp = -temp;
      if (temp<=piv) continue;
      piv = temp;
      i = j;
      }
   tol = eps*piv;

   ll = 1;
   for (k=1;k<=m;k++)
     {
      if (piv<=0.) { *ierror = -1; return; }
      if (*ierror==0&&piv<tol) *ierror = k-1;
      pfac = 1./a[i-1];
      j = (i-1)/m; i = i-j*m-k; j = j+1-k;
      kt = k+i;
      temp = pfac*r[kt-1];
      r[kt-1] = r[k-1];
      r[k-1] = temp;
      if (k>=m) break;
      lu = ll+m-k;
      if (j>0)
	 {
	 n = j*m;
	 for (l=ll;l<=lu;l++)
	    {
	    temp = a[l-1];
	    lt = l+n;
	    a[l-1] = a[lt-1];
	    a[lt-1] = temp;
	    }
	 }
      for (l=ll;l<=m2;l+=m)
	 {
	 lt = l+i;
	 temp = pfac*a[lt-1];
	 a[lt-1] = a[l-1];
	 a[l-1] = temp;
	 }
      a[ll-1] = (double)j;
      piv = 0.;
      ll = ll+1;
      j = 0;
      for (n=ll;n<=lu;n++)
	 {
	 pfac = -a[n-1];
	 il = n+m;
	 j = j+1;
	 for (l=il;l<=m2;l+=m)
	    {
	    lt = l-j;
	    a[l-1] = a[l-1]+pfac*a[lt-1];
	    temp = a[l-1]; if (temp<0.) temp = -temp;
	    if (temp<=piv) continue;
	    piv = temp;
	    i = l;
	    }
	 kt = k+j;
	 r[kt-1] = r[kt-1]+pfac*r[k-1];
	 }
      ll = ll+m;
      }

   if (m<=0) *ierror = -1;
   if (m<=1) return;
   il = m2+m;
   ll = m+1;
   for (i=2;i<=m;i++)
      {
      n = ll-i;
      il = il-ll;
      l = il-m;
      l = (int)(a[l-1]+.5);
      temp = r[n-1];
      lt = n;
      for (k=il;k<=m2;k+=m)
	 {
	 lt = lt+1;
	 temp = temp-a[k-1]*r[lt-1];
	 }
      k = n+l;
      r[n-1] = r[k-1];
      r[k-1] = temp;
      }
   return;
}


/*================================================================

ms_dnum

ms_dnum converts a string to a double and moves the pointer, also
allows for positive and negative exponent with e or E or D or d, for
example 123.45E-002

function return : double

argument :
      1. num_ptr: input, char **num_ptr;

*/

double ms_dnum (num_ptr)
   char **num_ptr;

{
   double sign = 1., lvalue = 0.0, rvalue = 0.0,
         decpt = 0.0, powr = -9999.0, powsign = 1.0;

   while (**num_ptr==' ') (*num_ptr)++;
   if (**num_ptr == '-')
   {
      sign = -1.;
      (*num_ptr)++;
   }
   for (;;(*num_ptr)++)
      {
      if (**num_ptr=='e' || **num_ptr=='E' ||
          **num_ptr=='d' || **num_ptr=='D') { powr = 0.0; continue;}
      if (**num_ptr=='+') continue;
      if (**num_ptr=='-') { powsign = -1.0; continue; }
      if (**num_ptr=='.') { decpt = .1; continue;}
      if (**num_ptr < '0' || **num_ptr > '9') break;
      if (powr==0.0) { powr = 10.*powr+(**num_ptr)-'0'; continue; }
      else if (decpt==0.) { lvalue = 10.*lvalue+(**num_ptr)-'0'; continue; }
	 else { rvalue = rvalue+decpt*((**num_ptr)-'0'); decpt *= .1; }
      }
   if (powr!=(-9999.0)) return (sign*(lvalue+rvalue)*pow(10.0,powr*powsign));
   else return (sign*(lvalue+rvalue));
}


int rztrim(buf)
   char *buf;
{
   int len;
   char *p;
   
   len = strlen(buf);
   p = &buf[len-1];
   while (*p=='0'&&*(p-1)!='.') { *p = (char)0; p--; }
   return;
}

void nicelen(hdr,val,buf)
   double val;
   char *hdr,*buf;
{
   int i,flen,len[3];
   char fmtstr[20];
   
   flen = MAX(13-(int)(log10(fabs(val)+.9)),3);
   strcpy(fmtstr,hdr);
   strcat(fmtstr,"%1.*f\0");
   for (i=0;i<3;i++)
      {
      sprintf(buf,fmtstr,flen-2*i,val);
      rztrim(buf);
      len[i] = strlen(buf);
      if (i==0&&len[0]<9) return;
      }
   if ((len[0]-len[2])<9)
      {
      sprintf(buf,fmtstr,flen,val);
      rztrim(buf);
      }
   else if ((len[0]-len[1])>=4)
      {
      sprintf(buf,fmtstr,flen-2,val);
      rztrim(buf);
      }
   
   return;
}

void scalefmt(outbuf,scale1,scale2)
   char *outbuf;
   double scale1,scale2;
{
   /* scale2 must be formatted for GeoTIFF (-1 times) */
   
   char buf[30];

   nicelen("(",scale1,buf);
   strcpy(outbuf,buf);
   nicelen(",",scale2,buf);
   strcat(outbuf,buf);
   strcat(outbuf,",0.0)");
   return;
}

void trnsfmt(outbuf,t)
   char *outbuf;
   double t[6];
{
   /* t must be formatted for GeoTIFF */
   
   char buf[30];
   
   nicelen("(",t[0],buf);
   strcpy(outbuf,buf);
   nicelen(",",t[1],buf);
   strcat(outbuf,buf);
   nicelen(",0,",t[2],buf);
   strcat(outbuf,buf);
   nicelen(",",t[3],buf);
   strcat(outbuf,buf);
   nicelen(",",t[4],buf);
   strcat(outbuf,buf);
   nicelen(",0,",t[5],buf);
   strcat(outbuf,buf);
   strcat(outbuf,",0,0,0,0,0,0,0,1)");
   
   return;
}

int grab(p,c,buf)
   char *p,*buf,c;
{
   int n;
   
   n = 0;
   while (*p!=c&&*p!=0)
      {
      if (*p==0) return 0;
      buf[n++] = *p;
      p++;
      }
   buf[n++] = (char)0;
   return n;
}

/************************************************************************/
/*									*/
/*		open_files()						*/
/*	This routine opens the input and output image files		*/
/*									*/
/************************************************************************/

int i_unit,o_unit;			/* input unit, output unit */
int inline, insamp;			/* size of primary input */
int sline, ssamp, nline, nsamp;		/* User specified size of output */
char fmt_str[10];                       /* format of primary input */
 
void open_files()
{
  int status,tsize[4],sizepcnt,sizedef;
  
  /***********************/
  /* open the input file */
  /***********************/
  status = zvunit( &i_unit, "INP", 1, 0);
  status = zvopen( i_unit, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF",0);
  /* should have checked status to make sure file opened properly. */
  /* Make sure we have either BYTE or HALF */
  zvget(i_unit,"FORMAT",fmt_str,0);
  if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF")) {
    zvmessage("Invalid input data format.  Use BYTE or HALF.","");
    zabend();
  }
  /***************************/
  /* open up the output file */
  /***************************/
  /*zvsize( &sline, &ssamp, &nline, &nsamp, &inline, &insamp);*/
  /*zvsize no good for negative parameters: sline,ssamp*/
  zvget(i_unit,"NL",&inline,"NS",&insamp,0);
  zvparm("SIZE",tsize,&sizepcnt,&sizedef,4,0);
  if (!sizedef)
     {
     sline = tsize[0];
     if (sizepcnt>=2) ssamp = tsize[1];
     if (sizepcnt>=3) nline = tsize[2];
     if (sizepcnt>=4) nsamp = tsize[3];
     }
  else
     {
     zvparm("SL",&sline,&sizepcnt,&sizedef,1,0);
     zvparm("SS",&ssamp,&sizepcnt,&sizedef,1,0);
     zvparm("NL",&nline,&sizepcnt,&sizedef,1,0);
     zvparm("NS",&nsamp,&sizepcnt,&sizedef,1,0);
     }
  if (nline==0) nline = inline;
  if (nsamp==0) nsamp = insamp;
  
  status=zvunit( &o_unit, "OUT", 1, 0);
  /* note that zvopen is intelligent enough to default to the same */
  /* format as the input file.  */
  status=zvopen( o_unit, "U_NL", nline, "U_NS", nsamp, "U_FORMAT","HALF",
		"OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA",
                "O_FORMAT",fmt_str,"TYPE","IMAGE",0);
  return;
}

float rampcalc(tns,tnl,dz,countz,j,q1,q2,zbarj)
   int j,q1,q2,**countz;
   float tns,tnl,**dz,zbarj;
{
   /* faster rectilinear interpolation, see commented code below for angular
   interpolation */
   float x,y,xpy,val0,val1;
   
   x = tns*fhns;
   y = tnl*fhnl;
   xpy = 1.0/(x+y+0.005);
   val0 = y*dz[q1][j];
   val1 = x*dz[q2][j];
   /*if (dbg) printf("B:x,y,val0,val1,zbarj,q1,q2,j %f %f %f %f %f %d %d %d\n",
                      x,y,val0,val1,zbarj,q1,q2,j);*/
   if (countz[q1][j]==0&&countz[q2][j]==0) return zbarj;
   else if (countz[q1][j]==0) return val1+zbarj;
   else if (countz[q2][j]==0) return val0+zbarj;
   else return y*xpy*val0+x*xpy*val1+zbarj;
}
 
/*float rampcalc(tns,tnl,dz,countz,j,q1,q2,zbarj)
   int j,q1,q2;
   float tnl,tns,**dz,zbarj;
{
   ** angular interpolation save this code **
   float x,y,xpy,val0,val1,alpha;
   x = tns*fhns;
   y = tnl*fhnl;
   if (x>0.01&&y>0.01) alpha = atan(y/x);
   else if (x>0.01) alpha = 0.0;
   else alpha = pi2;
   val0 = y*dz[q1][j];
   val1 = x*dz[q2][j];
   return alpha*pi2inv*val0+(pi2-alpha)*pi2inv*val1+zbarj;, etc.
}*/


/*================================================================

int gtgetlab

gtgetlab gets a geotiff label into a string parameter.  It
mallocs a large buffer, reads the geotiff label, then mallocs
the string parameter to the exact size, copies the label, then
frees the large buffer.  A null string is returned for any
failure to read a geotiff label.  The user will usually change
to all caps for speedier key identification.

function return:
     int, 1 if successful, 0 if cannot find info in label

arguments:
      1. inp: char buf[];
	 VICAR parameter for file that contains GeoTIFF label
	 usually "inp"
      2. instance: int instance;
         which instance of the previous parm
      3. labelstr: char **labelstr;
	 (output) pointer to string containing the label; is
	 mallocked to the exact size of the string, plus
	 terminating 0. user will usually change to all caps.
      4. nl: int *nl;
	 (output) nl for case of VICAR image, -1 if not
      5. ns: int *ns;
	 (output) ns for case of VICAR image, -1 if not
*/

int gtgetlab(inp,instance,labelstr,nl,ns)
   char inp[];
   int instance,*nl,*ns;
   char **labelstr;
{
   int i,status,geounit;
   int maxlen,nelement,len;
   char *buf,valformat[9],vformat[9];
   char svalue[133],*p,key[33],header[27];
   
   /* malloc large temporary buffer for reading the string */
   
   mz_alloc1((unsigned char **)&buf,1000001,1);
   
   /* open file */
   
   status = zvunit(&geounit,inp,instance,0);
   status = zvopen(geounit,"OP","READ","OPEN_ACT","SA",
         "LAB_ACT","SA",0);
      
   strcpy(buf,"");
   do
      {
      status=zlninfo(geounit,key,valformat,&maxlen,
         &nelement,"ERR_ACT"," ",0);
      if (status!=1) break;
      if (strcmp(key,"NL")==0)
         {
         status=zlget(geounit,"SYSTEM",key,nl,
            "ERR_ACT","SA","FORMAT","INT",0);
         }
      if (strcmp(key,"NS")==0)
         {
         status=zlget(geounit,"SYSTEM",key,ns,
            "ERR_ACT","SA","FORMAT","INT",0);
         }
      status=zlinfo(geounit,"PROPERTY",key,vformat,
         &maxlen,&nelement,"ERR_ACT"," ",
         "PROPERTY","GEOTIFF",0);
      if (status!=1) continue;
      if (strcmp(key,"PROPERTY")==0) continue;
      /* now concatenate the string values / can be vector */
      
      for (i=1;i<=nelement;i++)
         {
         if (nelement==1)
            status=zlget(geounit,"PROPERTY",key,svalue,
               "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
               "PROPERTY","GEOTIFF","ULEN",133,0);
         else
            status=zlget(geounit,"PROPERTY",key,svalue,"ELEMENT",i,
               "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
               "PROPERTY","GEOTIFF","ULEN",133,0);
         strcat(buf,key);
         strcat(buf,"=");
         strcat(buf,svalue);
         strcat(buf,"\n");
         }
      }
   while (1);
   status = zvclose(geounit,0);
   
   /* resave in smaller buffer */
   
   len = strlen(buf);
   if (((*labelstr)=(char *)malloc(len))==NULL) zmabend("malloc failed");
   strcpy(*labelstr,buf);
   
   free(buf);
   if (strlen(*labelstr)<1) return 0; else return 1;
}

/*================================================================

int invertmap

invertmap calculates the inverse of a six-vector double precision
map.

function return:
     ier from the dgauss call (see dgauss)

arguments:
      1. map: double[6] map;
	 (input) coefficients to convert pixel to coord OR
	 COORD TO PIXEL
      2. invmap: double[6] invmap;
	 (output) coefficients to convert the other way
*/

int invertmap(t,tinv)
   double t[6],tinv[6];
{
   int i,ier;
   double work[9];
   
   for (i=0;i<2;i++)
      {
      work[0] = t[2];
      work[1] = 100.0*t[1]+t[2];
      work[2] = 100.0*t[0]+t[2];
      work[3] = t[5];
      work[4] = 100.0*t[4]+t[5];
      work[5] = 100.0*t[3]+t[5];
      work[6] = 1.;
      work[7] = 1.;
      work[8] = 1.;
      if (i==0)
         {
         tinv[0] = 0.0;
         tinv[1] = 0.0;
         tinv[2] = 100.0;
         }
      else
         {
         tinv[3] = 0.0;
         tinv[4] = 100.0;
         tinv[5] = 0.0;
         }
      dgauss(work,&tinv[i*3],3,1.e-14,&ier);
      }
   
   return ier;
}

/*================================================================

int geofix

geofix translates label coordinates into a linear transformation
vectors that can be used for VICAR pixel-to-map or map-to-pixel
conversions.  If the file is a VICAR image then the mapping of 
the corner points is also returned (the (1,1) and (nline,nsamp)
centers of the corner pixels).

The convention for the transforms is (line,samp) -> (East,North)
for the map and (East,North) -> (line,samp) for the invmap
for convenience in working with VICAR.

Note that VICAR pixel referencing is different than GeoTIFF
pixel referencing (both "area" and "point"/"post" types).

function return:
     int, 1 if successful, 0 if cannot find info in label

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
      2. map: double[6] map;
	 (output) coefficients to convert pixel to coord
      3. invmap: double[6] invmap;
	 (output) coefficients to convert coord to pixel
      4. nl: int nl;
	 (input) number of lines in vicar image to calc corner
      5. ns: int ns;
	 (input) number of samples in vicar image to calc corner
      6. corner: double[4] corner;
	 (output) the mapping of the corners (.5,.5,nl+.5,ns+.5)
	 if the file is a VICAR image, else zero
*/

int geofix(labelstr,map,invmap,nl,ns,corner)
   char *labelstr;
   int nl,ns;
   double map[6],invmap[6],corner[4];
{
   int i,ix,vtype,len,status;
   int maxlen,nelement,ireturn,ier;
   char *p;
   double tie[4],voff,ddummy,scale[2],work[9],tab[6];
   
   for (i=0;i<6;i++) { map[i] = 0.; invmap[i] = 0.; }
   map[0] = 1.; map[5] = 1.;
   invmap[0] = 1.; invmap[5] = 1.;
   ireturn = 1;
   
   vtype = nl!=(-1);
      
   /* read the model transformation or get the scale, etc.  Note
   reversal of matrix from samp-line to line-samp */
   
   p = ms_find(labelstr,"GTRASTERTYPEGEOKEY=2");
   if (p!=0) voff = 1.0; else voff = 0.5;     /* 0.5 is the default also */
   p = ms_find(labelstr,"MODELTRANSFORMATIONTAG=(");
   if (p!=0)
      {
      map[1] = ms_dnum(&p); p++;
      map[0] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[2] = ms_dnum(&p)-(map[0]+map[1])*voff; p++;
      map[4] = ms_dnum(&p); p++;
      map[3] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[5] = ms_dnum(&p)-(map[3]+map[4])*voff;
      }
   else
      {
      p = ms_find(labelstr,"MODELTIEPOINTTAG=(");
      if (p==0) { ireturn = 0; goto closem; }
      tie[0] = ms_dnum(&p); p++;
      tie[1] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      tie[2] = ms_dnum(&p); p++;
      tie[3] = ms_dnum(&p);
      p = ms_find(labelstr,"MODELPIXELSCALETAG=(");
      if (p==0) { ireturn = 0; goto closem; }
      scale[0] = ms_dnum(&p); p++;
      scale[1] = ms_dnum(&p);
     
      map[0] = 0.0;
      map[1] = scale[0];
      map[2] = tie[2]-map[1]*(tie[0]+voff);
      map[3] = -scale[1];
      map[4] = 0.0;
      map[5] = tie[3]-map[3]*(tie[1]+voff);
      }
   
   if (vtype)
      {
      corner[0] = 0.5*map[0]+0.5*map[1]+map[2];
      corner[1] = 0.5*map[3]+0.5*map[4]+map[5];
      corner[2] = ((double)nl+0.5)*map[0]+((double)ns+0.5)*map[1]+map[2];
      corner[3] = ((double)nl+0.5)*map[3]+((double)ns+0.5)*map[4]+map[5];
      }
   else for (i=0;i<4;i++) corner[i] = (double)0.0;
   
   invertmap(map,invmap);
   
   closem:
   
   /*for (i=0;i<6;i++) printf("map trans[%d] %f\n",i+1,map[i]);
   for (i=0;i<6;i++) printf("inv trans[%d] %f\n",i+1,invmap[i]);*/
   
   return ireturn;
}

/*================================================================

int gtrect

gtrect tests whether the mapping is "rectangular".  This means 
that the GeoTIFF label has the keyword MODELPIXELSCALETAG or that
it has the keyword MODELTRANSFORMATIONTAG and the upper left part
of the transformation has two 0.0 in diagonal formation.  To allow
for a slight inaccuracy in a calculated transformation, a 
parameter eps is provided for values very close to 0. It actually
ratios with the main terms, see code below.

function return:
     int, 1 if mapping is rectangular, else 0

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
      2. eps: double eps;
	 (input) tolerance about zero for the off-diagonals
	 to still consider the mapping rectangular. It is a
	 ratio to the largest term.  suggest 1.0e-12.
*/

int gtrect(labelstr,eps)
   char *labelstr;
   double eps;
{
   char *p;
   double map[4],ddummy,largest,thresh;
   
   /* read the model transformation or read if scale */
   
   p = ms_find(labelstr,"MODELPIXELSCALETAG=(");
   if (p!=0)
      {
      p = ms_find(labelstr,"MODELTIEPOINTTAG=(");
      if (p==0) zmabend("Problem with GeoTIFF label");
      return 1;
      }
   p = ms_find(labelstr,"MODELTRANSFORMATIONTAG=(");
   if (p!=0)
      {
      map[0] = fabs(ms_dnum(&p)); p++;
      map[1] = fabs(ms_dnum(&p)); p++;
      ddummy = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[2] = fabs(ms_dnum(&p)); p++;
      map[3] = fabs(ms_dnum(&p));
      largest = MAX(map[0],map[1]);
      largest = MAX(largest,map[2]);
      largest = MAX(largest,map[3]);
      thresh = eps*largest;
      if (map[0]<thresh&&map[3]<thresh) return 1;
      if (map[1]<thresh&&map[2]<thresh) return 1;
      }
   return 0;
}

int gtcompval(p1,p2)
   char *p1,*p2;
{
   int charseq,stcomment;
   char *p,*q;
   double dval1,dval2,epsl,epsu;
   
   epsl = 1.0-1.0e-12;
   epsu = 1.0+1.0e-12;
   p = p1; q = p2;
   charseq = 0;
   stcomment = 0;
   do
      {
      while (*p==' ') p++;
      while (*q==' ') q++;
      if (*p=='\n') return 1;
      else if (*p==(char)0) return 1;
      else if (stcomment&&*p=='(') return 1;
      else if (isalpha(*p)||(isdigit(*p)&&charseq==1))
         {
         charseq = 1;
         stcomment = 1;
         if (*q==*p) { p++; q++; }
         else return 0;
         }
      else if (*p=='('||*p==')'||*p==','||*p==')'||*p=='=')
         {
         charseq = 0;
         if (*q=='('||*q==')'||*q==','||*q==')'||*q=='=') { p++; q++; }
            else return 0;
         }
      else if (isdigit(*p)||*p=='.')
         {
         stcomment = 1;
         dval1 = ms_dnum(&p); 
         dval2 = ms_dnum(&q);
         if (dval1<0.0) { dval1 = -dval1; dval2 = -dval2; }
         if (dval1<dval2*epsl||dval1>dval2*epsu) return 0;
         }
      else return 0;
      }
   while (1);
}

/*================================================================

int gtmapcom

gtmapcom tests whether the two mappings are compatible.  This is
defined as having the same values for a list of attributes that
pertain to mappings.  The list is taken from the GeoTIFF spec
rev 1.0.  If the "value" contains parenthetical material, it is
not required to be the same.  Vector values are required to be
the same.  There is a tolerance of 1 part in 1e-12 on all numeric
values.

If one of the listed attributes is present in one label it must
also be present in the other label.  This prevents default values
from making two labels incompatible, or for alternate keywords
to do the same.

function return:
     int, 1 if mappings are compatible, else 0

arguments:
      1. labelstr1: char *labelstr1;
	 (input) string containing the first GeoTIFF label
      1. labelstr2: char *labelstr2;
	 (input) string containing the second GeoTIFF label
*/

int gtmapcom(labelstr1,labelstr2)
   char *labelstr1,*labelstr2;
{
#define numattrib 45
   int iattrib,status;
   char *p1,*p2;
   char attrib[numattrib][34] = {"GTRASTERTYPEGEOKEY","GTMODELTYPEGEOKEY",
     "GEOGRAPHICTXGEOKEY","GEOGGEODETICDATUMGEOKEY","GEOGPRIMEMERIDIANGEOKEY",
     "GEOGLINEARUNITSGEOKEY","GEOGLINEARUNITSIZEGEOKEY","GEOGANGULARUNITSGEOKEY",
     "GEOGANGULARUNITSIZEGEOKEY","GEOGELLIPSOIDGEOKEY","GEOGSEMIMAJORAXISGEOKEY",
     "GEOGSEMIMINORAXISGEOKEY","GEOGINVFLATTENINGGEOKEY","GEOGAZIMUTHUNITSGEOKEY",
     "GEOGPRIMEMERIDIANLONGGEOKEY","PROJECTEDCSTYPEGEOKEY","PROJECTIONGEOKEY",
     "PROJCOORDTRANSGEOKEY","PROJLINEARUNITSGEOKEY","PROJLINEARUNITSIZEGEOKEY",
     "PROJSTDPARALLEL1GEOKEY","PROJSTDPARALLEL2GEOKEY","PROJNATORIGINLONGGEOKEY",
     "PROJNATORIGINLATGEOKEY","PROJFALSEEASTINGGEOKEY","PROJFALSENORTHINGGEOKEY",
     "PROJFALSEORIGINLONGGEOKEY","PROJFALSEORIGINLATGEOKEY","PROJFALSEORIGINEASTINGGEOKEY",
     "PROJFALSEORIGINNORTHINGGEOKEY","PROJCENTERLONGGEOKEY","PROJCENTERLATGEOKEY",
     "PROJCENTEREASTINGGEOKEY","PROJCENTERNORTHINGGEOKEY","PROJSCALEATNATORIGINGEOKEY",
     "PROJSCALEATCENTERINGEOKEY","PROJAZIMUTHANGLEGEOKEY","PROJSTRAIGHTVERTPOLELONGGEOKEY",
     "PROJSTDPARALLELGEOKEY","PROJORIGINLONGGEOKEY","PROJORIGINLATGEOKEY",
     "PROJSCALEATORIGINGEOKEY","VERTICALCSTYPEGEOKEY","VERTICALDATUMGEOKEY",
     "VERTICALUNITSGEOKEY"};
   /*int numattrib = 45;*/
   
   /* loop over attributes in string 1, finding match in string 2,
   the second part of the loop does the reverse check */
   
   for (iattrib=0;iattrib<numattrib;iattrib++)
      {
      p1 = ms_find(labelstr1,attrib[iattrib]);
      if (p1!=0)
         {
         p2 = ms_find(labelstr2,attrib[iattrib]);
         if (p2==0)
            {
            printf("Missing attribute in label 2: %s\n",attrib[iattrib]);
            return 0;
            }
         status = gtcompval(p1,p2);
         if (status!=1)
            {
            printf("Disagreement in labels for: %s\n",attrib[iattrib]);
            return 0;
            }
         }
      p2 = ms_find(labelstr2,attrib[iattrib]);
      if (p2!=0)
         {
         p1 = ms_find(labelstr1,attrib[iattrib]);
         if (p1==0)
            {
            printf("Missing attribute in label 1: %s\n",attrib[iattrib]);
            return 0;
            }
         status = gtcompval(p1,p2);
         if (status!=1)
            {
            printf("Disagreement in labels for: %s\n",attrib[iattrib]);
            return 0;
            }
         }
      }
   
   return 1;
}

/*================================================================

int gtgetrot

gtgetrot gets the rotation number of the GeoTIFF label.  A standard
VICAR rotation is 1.  See the help PDF for VICAR routine GTLIST for
information on the other rotations.

function return:
     int, a number from 0 to 8 designating the rotation relative to
     an (East,North) coordinate system.

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
*/

int gtgetrot(labelstr)
   char *labelstr;
{
   int rot;
   char *p;
   double map[6],tie[4],scale[2],ddummy,xmain,xcross,xtot,voff;
   
   p = ms_find(labelstr,"GTRASTERTYPEGEOKEY=2");
   if (p!=0) voff = 1.0; else voff = 0.5;     /* 0.5 is the default also */
   p = ms_find(labelstr,"MODELTRANSFORMATIONTAG=(");
   if (p!=0)
      {
      map[1] = ms_dnum(&p); p++;
      map[0] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[2] = ms_dnum(&p)-(map[0]+map[1])*voff; p++;
      map[4] = ms_dnum(&p); p++;
      map[3] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[5] = ms_dnum(&p)-(map[3]+map[4])*voff;
      }
   else
      {
      p = ms_find(labelstr,"MODELTIEPOINTTAG=(");
      if (p==0) zmabend("Problem with GeoTIFF label");
      tie[0] = ms_dnum(&p); p++;
      tie[1] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      tie[2] = ms_dnum(&p); p++;
      tie[3] = ms_dnum(&p);
      p = ms_find(labelstr,"MODELPIXELSCALETAG=(");
      if (p==0) zmabend("Problem with GeoTIFF label");
      scale[0] = ms_dnum(&p); p++;
      scale[1] = ms_dnum(&p);
      
      map[0] = 0.0;
      map[1] = scale[0];
      map[2] = tie[2]-map[1]*(tie[0]+voff);
      map[3] = -scale[1];
      map[4] = 0.0;
      map[5] = tie[3]-map[3]*(tie[1]+voff);
      }
   
   xmain = fabs(map[0])+fabs(map[4]);
   xcross = fabs(map[1])+fabs(map[3]);
   xtot = xmain+xcross;
   
   if (xmain/xtot<1.e-10)
      {
      if (map[1]>0.0&&map[3]>0.0) rot = 5;
      else if (map[1]>0.0&&map[3]<0.0) rot = 1;
      else if (map[1]<0.0&&map[3]>0.0) rot = 3;
      else if (map[1]<0.0&&map[3]<0.0) rot = 7;
      }
   else if (xcross/xtot<1.e-10)
      {
      if (map[0]>0.0&&map[4]>0.0) rot = 0;
      else if (map[0]>0.0&&map[4]<0.0) rot = 6;
      else if (map[0]<0.0&&map[4]>0.0) rot = 4;
      else if (map[0]<0.0&&map[4]<0.0) rot = 2;
      }
   else rot = -1;
   
   return rot;
}

/*================================================================

int gtgetscl

gtgetscl gets the scale factors of the GeoTIFF label.  There are two
types of scale that can be returned:

The first is for the four rotations that can be represented by the
MODELTIEPOINTTAG-MODELPIXELSCALETAG combination.  The geographic 
coordinate X which is usually East will have the first scale number
and the Y (North) will have the second.  The plus and minus signs
on these determine the four rotations which are all "flips".  The
odd thing about this scale is that the increasing "lines" coordinate
to the South is denoted by a positive scale factor.

The second is for the four rotations that have to be represented
by the MODELTRANSFORMATIONTAG combination.  The geographic 
coordinate X which is usually East will have the first scale number
and the Y (North) will have the second.  The plus and minus signs
on these determine the four rotations which are all "flips" of a
single ninety degree rotate.

function return:
     void

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
      2. sctype: int *sctype;
	 (output) type of scale factors (see above)
	 1 - from MODELPIXELSCALETAG
	 2 - from MODELTRANSFORMATIONTAG
      3. scale1: double *scale1;
	 (output) scale factor 1
      4. scale2: double *scale2;
	 (output) scale factor 2
*/

int gtgetscl(labelstr,sctype,scale1,scale2)
   char *labelstr;
   int *sctype;
   double *scale1,*scale2;
{
   char *p;
   double ddummy;
   
   p = ms_find(labelstr,"MODELTRANSFORMATIONTAG=(");
   if (p!=0)
      {
      *sctype = 2;
      ddummy = ms_dnum(&p); p++;
      *scale1 = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      *scale2 = ms_dnum(&p); p++;
     }
   else
      {
      p = ms_find(labelstr,"MODELPIXELSCALETAG=(");
      if (p==0) zmabend("Problem with GeoTIFF label");
      *sctype = 1;
      *scale1 = ms_dnum(&p); p++;
      *scale2 = ms_dnum(&p);
      }
    
   return;
}


/*================================================================

gtreplab

gtreplab writes a GeoTIFF label into the property part of VICAR label

function return : void

arguments:
      1. fileparm: input, char *fileparm;
         name of the file to put the label, usually "INP"
      2. nfile: input, int nfile;
         which file of fileparm, set to 1 if only one file
      3. labstr: input, char *labstr;
         string containing new GeoTIFF label, comma separated,
         see GTGEN document for format details, and TSTGTGEN.PDF
         for examples
      4. add: input, int add;
         0 - then the old label is deleted and labstr
             becomes the new label
         1 - then the old label is kept and added to,
      5. replflag: input, int replflag;
         0 - no processing of coord-pixel mapping
         1 - coord-pixel mapping replaced w/ next three params
         2 - coord-pixel mapping replaced w/ next three params, but
             MODELPIXELSCALETAG and MODELTRANSFORMATIONTAG type labels
             are swapped due to rotation
      6. tinv: input, double tinv[6];
         will recalculate every MODELTIEPOINTTAG using this transf
      7. scalestr: input, char *scalestr;
         will replace an occurrence of MODELPIXELSCALETAG with this
      8. transstr: input, char *transstr;
         will replace an occurrence of MODELTRANSFORMATIONTAG with this
      
*/

void gtreplab(fileparm,nfile,labstr,add,replflag,tinv,scalestr,transstr)
   char *fileparm,*labstr,*scalestr,*transstr;
   int nfile,add,replflag;
   double tinv[6];
{
   int geounit,filcount,nelement,maxlen,status,n;
   char key[33],valformat[9],temp1[100],temp2[133],buf[133];
   char *p,*q,*p1,*q1,tbuf[30];
   double dummy,coord1,coord2,pline,psamp;
   
   status=zvunit(&geounit,fileparm,nfile,0);
   status=zvopen(geounit,"OP","UPDATE","OPEN_ACT","SA",
	"LAB_ACT","SA",0);
   if (!add) do
      {
      /*seems only way to delete properties, note ERR_ACT*/
      status=zlninfo(geounit,key,valformat,&maxlen,
      &nelement,"ERR_ACT"," ",0);
      if (status!=1) break;
      status=zlinfo(geounit,"PROPERTY",key,valformat,
      &maxlen,&nelement,"PROPERTY","GEOTIFF",
      "ERR_ACT"," ",0);
      if (status!=1) continue;
      if (strcmp(key,"PROPERTY")==0) continue;
      status=zldel(geounit,"PROPERTY",key,
      "ERR_ACT","SA","PROPERTY","GEOTIFF",0);
      }
   while (1);
   p = labstr;
   q = p+strlen(labstr);
   do
      {
      n = grab(p,'=',temp1);
      if (n==0) zmabend("syntax error in geotiff parameter");
      p += n;
      n = grab(p,'\n',temp2);
      if (n==0) zmabend("syntax error in geotiff parameter");
      p += n;
      
      if (replflag>0&&strcmp(temp1,"MODELPIXELSCALETAG")==0)
         {
         if (replflag==1) strcpy(temp2,scalestr);
         else
            {
            strcpy(temp1,"MODELTRANSFORMATIONTAG");
            strcpy(temp2,transstr);
            }
         }
      else if (replflag>0&&strcmp(temp1,"MODELTRANSFORMATIONTAG")==0)
         {
         if (replflag==1) strcpy(temp2,transstr);
         else
            {
            strcpy(temp1,"MODELPIXELSCALETAG");
            strcpy(temp2,scalestr);
            }
         }
      else if (replflag>0&&strcmp(temp1,"MODELTIEPOINTTAG")==0)
         {
         p1 = &temp2[1];
         dummy = ms_dnum(&p1); p1++;
         dummy = ms_dnum(&p1); q1 = p1; p1++;
         dummy = ms_dnum(&p1); p1++;
         coord1 = ms_dnum(&p1); p1++;
         coord2 = ms_dnum(&p1);
         psamp = tinv[0]*coord1+tinv[1]*coord2+tinv[2];
         pline = tinv[3]*coord1+tinv[4]*coord2+tinv[5];
         nicelen("(",psamp,buf);
         nicelen(",",pline,tbuf);
         strcat(buf,tbuf);
         strcat(buf,q1);
         strcpy(temp2,buf);
         }
      
      status=zladd(geounit,"PROPERTY",temp1,temp2,
         "ERR_ACT","SA","FORMAT","STRING","MODE","REPLACE",
         "PROPERTY","GEOTIFF",0);
      }
   while (p<q);
   zvclose(geounit,0);
   return;
}

main44()
{
   int i,j,k,kk,q,qq,status,colcount,coldef,finfo,ibis,nrec,adjust;
   int filcount,filedef,notfound,dfeather,dummy,iread,currix,maxmfns;
   int previx,tempnl,tempns,datafound,rightbrk,leftbrk,midns,iout;
   int listix,ix2,ix3,tns,fildef,numcross,maxcross,*bufused;
   int *mfsl,*mfss,*mfnl,*mfns,**nbrj,**countz,*ibisrev,**ibisnbrj;
   int *fileix,*ibisix,*openstat,*vunit,*rotix,*lineread,*bufix;
   int edge,thresh,nibble,lnibble,rnibble,nthresh,lthresh,rthresh;
   int ixl,ixr,ibj,nseq,nincr,cloudout,*firstread,bix,ibix,nbrseq;
   int cols[7],inlist[50],sampoff[50],progress,thrcount,tval;
   int ramp,rdkthr,rdiffthr,rmoore,rcols[10],kq,jq,bixq,ix2q,ix3q;
   int zcnt,iix,loff,soff,rcolcount,rcoldef,jnbr,fmt_byte;
   int kmatch,redfeather,redmfns,moorefac,ired,redtns,curline;
   int ix3red,ix3qred,ibjq,*rereadmemory,rereadline,moorectr,mooreend;
   int geotiff,labnl,labns,len,tolerct,tolerdf,img,rot1,rot2;
   int sctype1,sctype2,*ilcorner,*iscorner,*inl,*ins,isav,moorenbl;
   short int ***footprnt,*bufout,*bufhit,tbufout,pself,pnbr,i2dkthr;
   short int **inbuf;
   unsigned char ***moore,mooretst,mnbr;
   char *fnames,infil[NUMINFILES][99],*name1,*name2;
   char *p,fmt_str2[10];
   char *labelstr1,*labelstr2,scalestr[50],transstr[133];
   double t[6],tinv[6],r[6],rinv[6],corner[4],rcorner[4];
   double tout[6],toutinv[6],scale1,scale2,voff,bcor,dcor;
   double scale11,scale12,scale21,scale22;
   double toler,xcorner,ycorner,lcorner,scorner,dlcorner,dscorner;
   
   double *mffac1,*mffac2,**sumz,ztot;
   float vi,usum,ovi,fcloudout,di,odi,lsum,**dz,*zbar,gorefac;
   float zramp,vself,vnbr,hnl,hns,locnl,locns,**ibisdz,fbigthresh;
   float vdiff;
   
   /* initialize, fetch params */

   zifmessage("featherv version 31-aug-00");
   
   if (zvptst("factor")) adjust = 1; else adjust = 0;
   if (zvptst("add")) adjust = adjust*100+2;
   if (zvptst("addzmask")) adjust = adjust*100+3;
   if (adjust>100) zmabend("Cannot use two adjust keywords");
   
   if (zvptst("geotiff")) geotiff = 1; else geotiff = 0;
   
   zvp("dfeather",&dfeather,&dummy);
   zvp("moorefac",&moorefac,&dummy);
   if (moorefac%2!=1) zmabend("moorefac parm must be an odd integer");
   redfeather = dfeather/moorefac;
   if (redfeather<2) zmabend("dfeather must be at least twice moorefac");
   if (redfeather>255) zmabend("dfeather must be less than 255 times moorefac");
   mooreend = moorefac-1;
   moorectr = mooreend/2;
   dfeather = redfeather*moorefac;
   zvp("moorenbl",&moorenbl,&dummy);
   
   edge = zvptst("edge");
   progress = zvptst("progress");
   status = zvp("thresh",&thresh,&dummy);
   status = zvp("nibble",&nibble,&dummy);
   status = zvp("lnibble",&lnibble,&dummy);
   status = zvp("rnibble",&rnibble,&dummy);
   status = zvp("nthresh",&nthresh,&dummy);
   status = zvp("lthresh",&lthresh,&dummy);
   status = zvp("rthresh",&rthresh,&dummy);
   status = zvp("nseq",&nseq,&dummy);
   status = zvp("nincr",&nincr,&dummy);
   if (lnibble==0&&rnibble==0) { lnibble = nibble; rnibble = nibble; }
   if (lthresh==0&&rthresh==0) { lthresh = nthresh; rthresh = nthresh; }
   
   status = zvpcnt("inp",&filcount);
   filcount -= geotiff;
   zvparm("cols",cols,&colcount,&coldef,7,0);
   status = zvunit(&finfo,"inp",filcount,0);
   status = IBISFileOpen(finfo,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(finfo,status,1);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"A99",cols[0]);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"FULL",cols[1]);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"FULL",cols[2]);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"FULL",cols[3]);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"FULL",cols[4]);
   IBISFileGet(ibis,"nr",&nrec,1,1,0);
   mz_alloc1((unsigned char **)&fnames,100*nrec,1);
   mz_alloc1((unsigned char **)&mfsl,nrec,4);
   mz_alloc1((unsigned char **)&mfss,nrec,4);
   mz_alloc1((unsigned char **)&mfnl,nrec,4);
   mz_alloc1((unsigned char **)&mfns,nrec,4);
   mz_alloc1((unsigned char **)&ibisrev,nrec,4);
   status = IBISColumnRead(ibis,fnames,cols[0],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   for (i=0;i<100*nrec;i++) fnames[i] = toupper(fnames[i]);
   status = IBISColumnRead(ibis,mfsl,cols[1],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   status = IBISColumnRead(ibis,mfss,cols[2],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   status = IBISColumnRead(ibis,mfnl,cols[3],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   status = IBISColumnRead(ibis,mfns,cols[4],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   if (adjust>0)
      {
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[5]);
      mz_alloc1((unsigned char **)&mffac1,nrec,8);
      status = IBISColumnRead(ibis,mffac1,cols[5],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      }
   /*if (keep this code for variance case)
      {
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[6]);
      mz_alloc1((unsigned char **)&mffac2,nrec,8);
      status = IBISColumnRead(ibis,mffac2,cols[6],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      }*/
   
   /* open the files for size parm, and open output */
   
   open_files();
   zvclose(i_unit,"CLOS_ACT","FREE",0);
   if (strcmp(fmt_str,"BYTE")==0) fmt_byte = 1; else fmt_byte = 0;
   
   /* find any missing coverage.  this will be a file in IBIS table
   that has (sl,ss,nl,ns) indicating overlap with the output but
   is not in the INP param. only filename (not path) is checked */

   zvparm("INP",infil,&filcount,&fildef,NUMINFILES,99);
   filcount -= geotiff;
   for (j=1;j<filcount;j++)
      for (i=0;i<99;i++) infil[j-1][i] = toupper(infil[j-1][i]);
   mz_alloc1((unsigned char **)&fileix,filcount,4);
   mz_alloc1((unsigned char **)&ibisix,filcount,4);
   mz_alloc1((unsigned char **)&openstat,filcount,4);
   mz_alloc1((unsigned char **)&vunit,filcount,4);
   mz_alloc1((unsigned char **)&rotix,filcount,4);
   mz_alloc1((unsigned char **)&firstread,filcount,4);
   mz_alloc1((unsigned char **)&lineread,filcount,4);
   mz_alloc1((unsigned char **)&bufix,filcount,4);
   mz_alloc1((unsigned char **)&rereadmemory,filcount,4);
   
   /* geotiff section: reads the last file as a geotiff reference file,
   the (sl,ss,nl,ns) will be in the coordinates of this file, the offsets
   will be calculated for each input image and compared to mfsl and mfss,
   the tolerance parameter is read and all offsets must be within
   <tolerance> of a whole pixel */
   
   if (geotiff)
      {
      zvparmd("toler",&toler,&tolerct,&tolerdf,1,0);
      status = gtgetlab("inp",filcount+1,&labelstr1,&labnl,&labns);
      if (status!=1) zmabend("problem with GeoTIFF file");
      len = strlen(labelstr1);
      for (i=0;i<len;i++) labelstr1[i] = toupper(labelstr1[i]);
      status = geofix(labelstr1,t,tinv,labnl,labns,corner);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, first input");
      mz_alloc1((unsigned char **)&ilcorner,filcount,4);
      mz_alloc1((unsigned char **)&iscorner,filcount,4);
      mz_alloc1((unsigned char **)&inl,filcount,4);
      mz_alloc1((unsigned char **)&ins,filcount,4);
      
      for (img=1;img<filcount;img++)
         {
         status = gtgetlab("inp",img,&labelstr2,&labnl,&labns);
         if (status!=1)
            zmabend("Failed to get mapping from GeoTIFF label, i-th input");
         inl[img] = labnl;
         ins[img] = labns;
         len = strlen(labelstr2);
         for (i=0;i<len;i++) labelstr2[i] = toupper(labelstr2[i]);
         if (img==1) status = gtrect(labelstr1,(double)1.0e-12);
         if (status!=1)
            zmabend("GTMSS is restricted to rectangular mappings when GeoTIFF");
         status = gtrect(labelstr2,(double)1.0e-12);
         if (status!=1)
            zmabend("GTMSS is restricted to rectangular mappings when GeoTIFF");
         status = geofix(labelstr2,r,rinv,labnl,labns,rcorner);
         if (status!=1)
            zmabend("Failed to get mapping from GeoTIFF label, i-th input");
         status = gtmapcom(labelstr1,labelstr2);
         if (status!=1) zmabend("Mappings not compatible");
         rot1 = gtgetrot(labelstr1);
         rot2 = gtgetrot(labelstr2);
         if (rot1!=rot2)
            zmabend("Different rotations for two inputs, use GTROTATE");
         gtgetscl(labelstr1,&sctype1,&scale11,&scale12);
         gtgetscl(labelstr2,&sctype2,&scale21,&scale22);
         if (sctype1!=sctype2) /* this is redundant, see rotation */
            zmabend("Different rotations for two inputs, use GTROTATE");
         if (fabs(scale11-scale21)>toler*fabs(scale11))
            zmabend("Different scales for two inputs, use GTSIZE");
         if (fabs(scale12-scale22)>toler*fabs(scale12))
            zmabend("Different scales for two inputs, use GTSIZE");
         xcorner = r[0]+r[1]+r[2];
         ycorner = r[3]+r[4]+r[5];
         lcorner = tinv[0]*xcorner+tinv[1]*ycorner+tinv[2]-1;
         scorner = tinv[3]*xcorner+tinv[4]*ycorner+tinv[5]-1;
         ilcorner[img] = (int)(lcorner*1.0000001);
         iscorner[img] = (int)(scorner*1.0000001);
         if (fabs(lcorner-(double)ilcorner[img])>toler)
            zmabend("Non-integral offset calculated from GeoTIFF");
         if (fabs(scorner-(double)iscorner[img])>toler)
            zmabend("Non-integral offset calculated from GeoTIFF");
         printf("calculated (sl,ss) for image %d is (%18.10f,%18.10f)\n",
            img,lcorner,scorner);
         }
   }
   
   /* end of geotiff section.  */
   
   maxmfns = 0;
   for (i=0;i<filcount;i++) fileix[i] = 0;
   for (i=0;i<nrec;i++)
      {
      name1 = nameget(&fnames[i*100]);
      notfound = 1;
      isav = i;
      for (j=1;j<filcount;j++) /* (0th) index not used, except for infil */
         {
         name2 = nameget(infil[j-1]);
         if (strcmp(name1,name2)==0)
            {
            notfound = 0;
            fileix[j] = j;
            ibisix[j] = i;
            ibisrev[i] = j; /* sparse OK */
            rotix[j] = -1;
            firstread[j] = 0;
            lineread[j] = 1;
            bufix[j] = -999;
            rereadmemory[j] = -1;
            /* geotiff operations */
            if (geotiff)
               {
               if (mfnl[i]==0)
                  {
                  mfnl[i] = inl[j];
                  mfns[i] = ins[j];
                  mfsl[i] = ilcorner[j];
                  mfss[i] = iscorner[j];
                  maxmfns = MAX(maxmfns,mfns[i]);
                  }
               if (mfnl[i]!=inl[j])
                   zmabend("nl in IBIS file disagrees with GeoTIFF");
               if (mfns[i]!=ins[j])
                   zmabend("ns in IBIS file disagrees with GeoTIFF");
               if (mfsl[i]!=ilcorner[j])
                   zmabend("sl in IBIS file disagrees with GeoTIFF");
               if (mfss[i]!=iscorner[j])
                   zmabend("ss in IBIS file disagrees with GeoTIFF");
               }
            maxmfns = MAX(maxmfns,mfns[i]);
            /*check if overlap*/
            if ((mfsl[i]+mfnl[i])<sline) openstat[j] = -1;
            else if ((mfss[i]+mfns[i])<ssamp) openstat[j] = -1;
            else if (mfsl[i]>=(sline+nline)) openstat[j] = -1;
            else if (mfss[i]>=(ssamp+nsamp)) openstat[j] = -1;
            else openstat[j] = 0;
            break;
            }
         }
      if (notfound)
         {
         /*check if overlap, this feature allows checking of coverage via ibis
         table, missing a file that touches range will cause abort*/
         if (((mfsl[isav]+mfnl[isav])>=sline)&&
               ((mfss[isav]+mfns[isav])>=ssamp)&&
               (mfsl[isav]<(sline+nline))&&(mfss[isav]<(ssamp+nsamp)))
            {
            printf("\nfile %s\n",name1);
            zmabend("is in IBIS table, but not input");
            }
         }
      }
   redmfns = (maxmfns+moorefac-1)/moorefac;
   for (j=1;j<filcount;j++) if (fileix[j]==0)
      {
      name2 = nameget(infil[j-1]);
      printf("\nfile %s\n",name2);
      zmabend("is not in IBIS table");
      }
   
   /* set up for ramp calculations */
   
   if (zvptst("noramp")) ramp = 3;
   else if (zvptst("readramp")) ramp = 2;
   else ramp = 1;
   mz_alloc2((unsigned char ***)&nbrj,4,filcount,4);
   mz_alloc2((unsigned char ***)&countz,4,filcount,4);
   mz_alloc2((unsigned char ***)&dz,4,filcount,4);
   mz_alloc1((unsigned char **)&zbar,filcount,4);
   if (ramp==1||ramp==2)
      {
      mz_alloc2((unsigned char ***)&ibisnbrj,5,nrec,4);
      mz_alloc2((unsigned char ***)&sumz,4,filcount,8);
      
      status = zvp("rdkthr",&rdkthr,&dummy);
      status = zvp("rdiffthr",&rdiffthr,&dummy);
      if (rdkthr<thresh) zmabend("rdkthr must be equal to or greather than thresh");
      i2dkthr = (short int)rdkthr;
      fbigthresh = (float)rdiffthr;
      status = zvp("rmoore",&rmoore,&dummy);
      status = zvp("gorefac",&gorefac,&dummy);
      zvparm("rcols",rcols,&rcolcount,&rcoldef,10,0);
      mooretst = (unsigned char)rmoore;
      for (k=0;k<5;k++)
         {
         status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"FULL",rcols[k]);
         status = IBISColumnRead(ibis,ibisnbrj[k],rcols[k],1,nrec);
         if (status!=1) IBISSignal(ibis,status,0);
         }
      for (j=1;j<filcount;j++)
         {
         ibix = ibisix[j];
         for (k=0;k<4;k++)
            {
            sumz[k][j] = 0.0;
            countz[k][j] = 0;
            nbrseq = ibisnbrj[k+1][ibix];
            for (kk=0;kk<nrec;kk++)
               if (ibisnbrj[0][kk]==nbrseq) nbrj[k][j] = ibisrev[kk];
            }
         }
      }
   if (ramp==2)
      {
      mz_alloc2((unsigned char ***)&ibisdz,5,nrec,4);
      for (k=5;k<10;k++)
         {
         status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"REAL",rcols[k]);
         status = IBISColumnRead(ibis,ibisdz[k-5],rcols[k],1,nrec);
         if (status!=1) IBISSignal(ibis,status,0);
         }
      for (j=1;j<filcount;j++)
         {
         i = ibisix[j];
         zbar[j] = ibisdz[0][i];
         for (k=0;k<4;k++)
            {
            dz[k][j] = ibisdz[k+1][i];
            if (fabs(dz[k][j]+999.0)>0.01) countz[k][j] = 1; /* fake value for 'READRAMP case */
            }
         }
      mz_free2((unsigned char **)ibisdz,5);
      }
   if (ramp==3)
      {
      for (j=1;j<filcount;j++)
         {
         zbar[j] = 0.0;
         for (k=0;k<4;k++)
            {
            dz[k][j] = -999.0;
            nbrj[k][j] = 0;
            countz[k][j] = 0;
            }
         }
      
      }
   
   /* A quick scan over the data sets to find the max number of files open
   simultaneously.  Needs to account for distance DFEATHER */
   
   free(fnames);
   numcross = 0;
   maxcross = 0;
   
   for (iread=1-dfeather,iout=2-2*dfeather;iout<nline;iread++,iout++)
      {
      for (j=1;j<filcount;j++)
         {
         ibj = ibisix[j];
         if (openstat[j]==3) continue;
         if (openstat[j]==2)
            {
            /* terminate footprnt if past Moore distance */
            if ((sline+iread-dfeather+2)>(mfsl[ibj]+mfnl[ibj]))
               {
               openstat[j] = 3;
               numcross--;
               continue;
               }
            }
         if (openstat[j]==0) /* check for top line */
            {
            if (mfsl[ibj]<=(sline+iread))
               {
               openstat[j] = 1;
               numcross++;
               maxcross = MAX(maxcross,numcross);
               }
            }
         if (openstat[j]==1) /* consider this line */
            {
            if ((mfsl[ibj]+mfnl[ibj])<=(sline+iread+1)) /* last line Moore */
               {
               /* file closed but buffer still needed */
               openstat[j] = 2;
               continue;
               }
            }
         } /* j=1..filecount loop */
      } /* the big simultaneous file count loop */
   
   /* Now set up the big buffers for image footprints and Moore distances */
   /* also set up inbuf for actual reading; bufused for tracking usage */
   
   printf("\nMaximum files open simultaneously is %d\n",maxcross);
   printf("Virtual memory needed for Footprints is %d\n\n",
        3*maxcross*redmfns*redfeather);
   mz_alloc3((unsigned char ****)&footprnt,maxcross,redfeather,redmfns,2);
   mz_alloc3((unsigned char ****)&moore,maxcross,redfeather,redmfns,1);
   mz_alloc2((unsigned char ***)&inbuf,maxcross,maxmfns,2);
   mz_alloc1((unsigned char **)&bufused,maxcross,4);
   for (i=0;i<maxcross;i++) bufused[i] = 0;
   for (i=0;i<filcount;i++) if (openstat[i]>0) openstat[i] = 0;
   
   /* prep for main loop, the first file was opened for type */
   
   status = zvp("cloudout",&cloudout,&dummy);
   fcloudout = (float)cloudout;
   mz_alloc1((unsigned char **)&bufout,nsamp,2);
   mz_alloc1((unsigned char **)&bufhit,nsamp,2);
   for (i=0;i<nsamp;i++) bufhit[i] = 0;
   mz_alloc1((unsigned char **)&pstack,redmfns*redfeather,4);
   mz_alloc1((unsigned char **)&istack,redmfns*redfeather,4);
   mz_alloc1((unsigned char **)&vstack,redmfns*redfeather,4);
   
   /* this skips the pass 1 for the 'READRAMP case and the 'NORAMP case */
   
   if (ramp!=1) goto pass2entry;
   
   /* OUTPUT LOOP I: GATHER THE DZ AT MOORE DISTANCE rmoore, NOTE THAT
   THE VICAR FILE CLOSE RELEASES THE UNIT; ignore lines not involved
   because moorefac skips them */
   
   for (iread=1-dfeather,iout=2-2*dfeather;iout<nline;iread++,iout++)
      {
      if (progress&&iout%500==499)
         printf("%d lines completed, pass 1\n",iout+1);
      for (j=1;j<filcount;j++)
         {
         ibj = ibisix[j];
         bix = bufix[j];
         if (openstat[j]==3) continue;
         if (openstat[j]==2)
            {
            curline = sline+iread-mfsl[ibj];
            if ((curline%moorefac)==mooreend)
               {
               currix = (rotix[j]+1)%redfeather;
               rotix[j] = currix;
               }
            /* terminate footprnt if past Moore distance */
            if ((sline+iread-dfeather+2)>(mfsl[ibj]+mfnl[ibj]))
               {
               zvclose(vunit[j],"CLOS_ACT","FREE",0);
               openstat[j] = 3;
               k = bufix[j];
               bufused[k] = 0;
               bufix[j] = -998;
               continue;
               }
            }
           
         if (openstat[j]==0) /* check for top line and open */
            {
            if (mfsl[ibj]<=(sline+iread))
               {
               openstat[j] = 1;
               lineread[j] = sline+iread-mfsl[ibj]+1+moorectr;
               status = zvunit( &vunit[j], "INP", fileix[j], 0);
               status = zvopen( vunit[j], "OPEN_ACT", "SA",
                 "IO_ACT", "SA", "U_FORMAT","HALF",0);
               zvget(vunit[j],"NL",&tempnl,0);
               zvget(vunit[j],"NS",&tempns,0);
               if (tempnl!=mfnl[ibj]||tempns!=mfns[ibj])
                  {
                  printf("file %s has\n",infil[j-1]);
                  zmabend("(nl,ns) disagreement, label vs. IBIS file");
                  }
               zvget(vunit[j],"FORMAT",fmt_str2,0);
               if (strcmp(fmt_str,fmt_str2)!=0)
                  zmabend("Mixed byte and halfword files");
               for (k=0;k<maxcross;k++) if (bufused[k]==0)
                  {
                  bufused[k] = 1;
                  bufix[j] = k;
                  bix = k;
                  break;
                  }
               }
            }
         if (openstat[j]==1) /* read the line and apply Moore */
            {
            curline = sline+iread-mfsl[ibj];
            if ((curline%moorefac)!=mooreend) continue;
            if (firstread[j]==0) firstread[j] = 1;
            previx = rotix[j];
            currix = (rotix[j]+1)%redfeather;
            rotix[j] = currix;
            tns = mfns[ibj];
            redtns = (tns+moorefac-1)/moorefac;
            
            if (moorefac==1)
               {
               status = zvread(vunit[j],footprnt[bix][currix],
                   "LINE",lineread[j]++,"SAMP",1,"NSAMPS",tns,0);
               }
            else
               {
               status = zvread(vunit[j],inbuf[bix],
                   "LINE", lineread[j], "SAMP", 1, "NSAMPS", tns, 0);
               lineread[j] += moorefac;
               if (lineread[j]>mfnl[ibj]) lineread[j] = mfnl[ibj];
               for (i=moorectr,ired=0;i<tns;i+=moorefac,ired++)
                  footprnt[bix][currix][ired] = inbuf[bix][i];
               if (tns%moorefac!=0)
                  footprnt[bix][currix][redtns-1] = inbuf[bix][tns-1];
               }
            
            /*for (i=1;i<redtns;i++)
               printf(" %d",footprnt[bix][currix][i]);
            printf(" A\n");*/
            
            /* nibble operations */
            if (edge)
               {
               for (i=0;i<lnibble;i++) footprnt[bix][currix][i] = 0;
               for (i=1;i<=rnibble;i++) footprnt[bix][currix][redtns-i] = 0;
               if (lthresh>0)
                  {
                  thrcount = 0;
                  for (i=lnibble;i<redtns;i+=nincr)
                     {
                     if (footprnt[bix][currix][i]>=lthresh)
                        {
                        thrcount++;
                        if (thrcount>=nseq)
                           {
                           for (k=lnibble;k<=i;k++)
                              footprnt[bix][currix][k] = 0;
                           break;
                           }
                        }
                     else thrcount = 0;
                     }
                  if (thrcount<nseq) 
                     {
                     for (k=lnibble;k<redtns;k++) footprnt[bix][currix][k] = 0;
                     goto edgedone1;
                     }
                  }
               if (rthresh>0)
                  {
                  thrcount = 0;
                  for (i=redtns-1-rnibble;i>=0;i-=nincr)
                     {
                     if (footprnt[bix][currix][i]>=rthresh)
                        {
                        thrcount++;
                        if (thrcount>=nseq)
                           {
                           for (k=redtns-1-rnibble;k>=i;k--)
                               footprnt[bix][currix][k] = 0;
                           break;
                           }
                        }
                     else thrcount = 0;
                     }
                  if (thrcount<nseq)
                     for (k=lnibble;k<redtns;k++) footprnt[bix][currix][k] = 0;
                  }
               }
            edgedone1:
            
            if (firstread[j]==1) /* first line Moore */
               {
               firstread[j] = 2;
               for (i=0;i<redtns;i++)
                  {
                  if (footprnt[bix][currix][i]<thresh)
                     moore[bix][currix][i] = (unsigned char)0;
                  else moore[bix][currix][i] = (unsigned char)1;
                  }
               continue;
               }
            if ((mfsl[ibj]+mfnl[ibj])<=(sline+iread+1)) /* last line Moore */
               {
               for (i=0;i<redtns;i++)
                  {
                  if (footprnt[bix][currix][i]<thresh)
                     {
                     moore[bix][currix][i] = (unsigned char)0;
                     if (previx>=0&&((int)(moore[bix][previx][i])>1))
                     propagate(1,moore[bix],previx,i,redfeather,redtns,currix);
                     }
                  else
                     {
                     moore[bix][currix][i] = (unsigned char)1;
                     if (previx>=0&&((int)(moore[bix][previx][i])>2))
                     propagate(2,moore[bix],previx,i,redfeather,redtns,currix);
                     }
                  }
               openstat[j] = 2;
               continue;
               }
            
            datafound = 0;
            for (i=0;i<redtns;i++)
               {
               leftbrk = i;
               if (footprnt[bix][currix][i]>=thresh) { datafound = 1; break; }
               moore[bix][currix][i] = (unsigned char)0;
               if (previx>=0&&((int)(moore[bix][previx][i])>1))
                  propagate(1,moore[bix],previx,i,redfeather,redtns,currix);
               }
            if (!datafound) continue;
            for (i=redtns-1;i>=0;i--)
               {
               rightbrk = i;
               if (footprnt[bix][currix][i]>=thresh) break;
               moore[bix][currix][i] = (unsigned char)0;
               if (previx>=0&&((int)(moore[bix][previx][i])>1))
                  propagate(1,moore[bix],previx,i,redfeather,redtns,currix);
               }
            midns = (leftbrk+rightbrk)/2;
            if (leftbrk==0) moore[bix][currix][leftbrk++] = (unsigned char)1;
            if (rightbrk==(redtns-1)) moore[bix][currix][rightbrk--] = (unsigned char)1;
            for (i=leftbrk;i<midns;i++)
               {
               moore[bix][currix][i] = (unsigned char)MIN((int)moore[bix][currix][i-1]+1,
                  MIN((int)moore[bix][previx][i]+1,redfeather));
               }
            for (i=rightbrk;i>=midns;i--)
               {
               moore[bix][currix][i] = (unsigned char)MIN((int)moore[bix][currix][i+1]+1,
                  MIN((int)moore[bix][previx][i]+1,redfeather));
               }
            } /* openstat[j]==1 cases */
         } /* j=1..filecount loop */
      
      /* get tiepoints from the oldest line in footprint; a grid is imposed on the */
      /* mosaic space for the tiepoints so as not to collect too many; this grid will */
      /* center with the grids in the footprints, but they don't register with each */
      /* other anyway; note that the tiepoints are collected at high resolution */
      
      if (iout>=0&&((iout%moorefac)==moorectr))
         {
         /* initialize the start and stop points of the output line in bufhit */
         
         bufhit[0] = 1;
         for (j=1;j<filcount;j++)
            {
            if (openstat[j]>0&&openstat[j]<3)
               {
               ibj = ibisix[j];
               if (iout>=mfsl[ibj]-sline)
                  {
                  ixl = mfss[ibj]-ssamp;
                  ixr = mfss[ibj]+mfns[ibj]-ssamp;
                  if (ixl>=0&&ixl<nsamp) bufhit[ixl] = 1;
                  if (ixr>=0&&ixr<nsamp) bufhit[ixr] = 1;
                  }
               }
            }
         for (i=0;i<nsamp;i++)
            {
            if (bufhit[i]==1)
               {
               listix = 0;
               for (j=1;j<filcount;j++)
                  {
                  ibj = ibisix[j];
                  if ((openstat[j]>0)&&(openstat[j]<3)&&
                     (i>=(mfss[ibj]-ssamp))&&
                     (i<(mfss[ibj]+mfns[ibj]-ssamp))&&
                     (iread>=(mfsl[ibj]-sline+dfeather-1)))
                     {
                     inlist[listix] = j;
                     sampoff[listix++] = mfss[ibj]-ssamp;
                     }
                  }
               bufhit[i] = 0;
               }
            if ((i%moorefac)!=moorectr) continue;
            if (listix>1)                           /* must be overlap */
               {
               /*if (i%250==0&&iout%250==0)
                  {
                  printf("%d %d:",iout,i);
                  for (kq=0;kq<listix;kq++)
                     printf(" %d",ibisnbrj[0][ibisix[inlist[kq]]]);
                  printf("\n");
                  }*/
               for (k=0;k<listix;k++)
                  {
                  j = inlist[k];
                  bix = bufix[j];
                  ix2 = (rotix[j]+1)%redfeather;
                  ix3 = i-sampoff[k];
                  ix3red = ix3/moorefac;
                  if (moore[bix][ix2][ix3red]!=mooretst) continue;
                  if (moorefac==1)
                     {
                     pself = footprnt[bix][ix2][ix3red];
                     }
                  else
                     {
                     ibj = ibisix[j];
                     rereadline = iout-mfsl[ibj]+sline+1+moorectr;
                     if (rereadmemory[j]!=rereadline)
                        {
                        status = zvread(vunit[j],inbuf[bix],
                           "LINE",rereadline,"SAMP",1,"NSAMPS",mfns[ibj],0);
                        rereadmemory[j] = rereadline;
                        }
                     pself = inbuf[bix][ix3];
                     }
                  if (pself<i2dkthr) continue;
                  /* accumulate dz for all nbrs, but have to match to 4 nbrs */
                  for (kq=0;kq<listix;kq++)
                     {
                     if (kq==k) continue;
                     jq = inlist[kq];
                     for (kmatch=0;kmatch<4;kmatch++)
                        {
                        if (jq==nbrj[kmatch][j])
                           {
                           bixq = bufix[jq];
                           ix2q = (rotix[jq]+1)%redfeather;
                           ix3q = i-sampoff[kq];
                           ix3qred = ix3q/moorefac;
                           mnbr = moore[bixq][ix2q][ix3qred];
                           if (mnbr==(unsigned char)0) continue;
                           if (moorefac==1)
                              {
                              pnbr = footprnt[bixq][ix2q][ix3qred];
                              }
                           else
                              {
                              ibjq = ibisix[jq];
                              rereadline = iout-mfsl[ibjq]+sline+1+moorectr;
                              if (rereadmemory[jq]!=rereadline)
                                 {
                                 status = zvread(vunit[jq],inbuf[bixq],
                                    "LINE",rereadline,"SAMP",1,"NSAMPS",mfns[ibjq],0);
                                 rereadmemory[jq] = rereadline;
                                 }
                              pnbr = inbuf[bixq][ix3q];
                              }
                           if (pnbr<i2dkthr) break;
                           vself = (float)pself;
                           switch (adjust)
                              {
                              case 1: vself = vself*mffac1[ibisix[j]]; break;
                              case 2: /* same as case 3 */
                              case 3: vself = vself+mffac1[ibisix[j]]; break;
                              }
                           vnbr = (float)pnbr;
                           switch (adjust)
                              {
                              case 1: vnbr = vnbr*mffac1[ibisix[jq]]; break;
                              case 2: /* same as case 3 */
                              case 3: vnbr = vnbr+mffac1[ibisix[jq]]; break;
                              }
                           /* reverse self-nbr */
                           vdiff = vnbr-vself;
                           if (fabs(vdiff)>fbigthresh) break;
                           sumz[kmatch][j] += (double)vdiff;
                           countz[kmatch][j]++;
                           break;
                           }
                        }
                     }
                  }
               } /* listix>1 case */
            } /* i=0..nsamp */
         } /* iout>=0 output of oldest line in bufffer */
      } /* the big input/output loop */
   
   /* reset for loop II, also process the dz, leave zero dz alone */
   
   for (i=0;i<maxcross;i++) bufused[i] = 0;
   for (j=1;j<filcount;j++)
      {
      if (openstat[j]==1||openstat[j]==2) zvclose(vunit[j],"CLOS_ACT","FREE",0);
      if (openstat[j]>0) openstat[j] = 0;
      rotix[j] = -1;
      firstread[j] = 0;
      lineread[j] = 1;
      bufix[j] = -999;
      rereadmemory[j] = -1;
      ztot = 0.0;
      zcnt = 0;
      for (k=0;k<4;k++)
         {
         if (countz[k][j]==0) continue;
         sumz[k][j] /= (double)countz[k][j];
         ztot += sumz[k][j];
         zcnt++;
         }
      if (zcnt==0) zbar[j] = 0; else zbar[j] = ztot/(double)zcnt;
      /* now apply the "half-to-neighbor" factor to zbar with no "boost" */
      zbar[j] *= 0.5;
      /* deduct the self zbar */
      for (k=0;k<4;k++)
         {
         if (countz[k][j]==0) dz[k][j] = 0.0;
         else dz[k][j] = sumz[k][j]-zbar[j];
         }
      }
   for (j=1;j<filcount;j++)
      {
      /* add the neighbor zbar, prev loop has calculated all the zbar's */
      for (k=0;k<4;k++)
         {
         if (countz[k][j]>0)
            {
            jnbr = nbrj[k][j];
            if (jnbr>0) dz[k][j] = dz[k][j]+zbar[jnbr];
            }
         }
      /* now apply the "gorefac" factor */
      for (k=0;k<4;k++) dz[k][j] *= gorefac;
      }
      
   /* OUTPUT LOOP II: USE THE DZ TO RAMP THE NEIGHBORS CLOSER, NOTE THAT
   THE VICAR FILE CLOSE RELEASES THE UNIT */
   
   /*zprnt(8,filcount,sumz[0],"sumz[0].");
   zprnt(8,filcount,sumz[1],"sumz[1].");
   zprnt(8,filcount,sumz[2],"sumz[2].");
   zprnt(8,filcount,sumz[3],"sumz[3].");*/
   
   pass2entry:
   
   /*zprnt(4,filcount,countz[0],"countz[0].");
   zprnt(4,filcount,countz[1],"countz[1].");
   zprnt(4,filcount,countz[2],"countz[2].");
   zprnt(4,filcount,countz[3],"countz[3].");
   zprnt(7,filcount,dz[0],"dz[0].");
   zprnt(7,filcount,dz[1],"dz[1].");
   zprnt(7,filcount,dz[2],"dz[2].");
   zprnt(7,filcount,dz[3],"dz[3].");
   zprnt(7,filcount,zbar,"zbar.");*/
   
   for (iread=1-dfeather,iout=2-2*dfeather;iout<nline;iread++,iout++)
      {
      if (progress&&iout%500==499)
         printf("%d lines completed, pass 2\n",iout+1);
      for (j=1;j<filcount;j++)
         {
         ibj = ibisix[j];
         bix = bufix[j];
         if (openstat[j]==3) continue;
         if (openstat[j]==2)
            {
            
            curline = sline+iread-mfsl[ibj];
            if ((curline%moorefac)==mooreend)
               {
               currix = (rotix[j]+1)%redfeather;
               rotix[j] = currix;
               }
            /* terminate footprnt if past Moore distance */
            if ((sline+iread-dfeather+2)>(mfsl[ibj]+mfnl[ibj]))
               {
               zvclose(vunit[j],"CLOS_ACT","FREE",0);
               openstat[j] = 3;
               k = bufix[j];
               bufused[k] = 0;
               bufix[j] = -998;
               continue;
               }
            }
           
         if (openstat[j]==0) /* check for top line and open */
            {
            if (mfsl[ibj]<=(sline+iread))
               {
               openstat[j] = 1;
               lineread[j] = sline+iread-mfsl[ibj]+1+moorectr;
               status = zvunit( &vunit[j], "INP", fileix[j], 0);
               status = zvopen( vunit[j], "OPEN_ACT", "SA",
                 "IO_ACT", "SA", "U_FORMAT","HALF",0);
               zvget(vunit[j],"NL",&tempnl,0);
               zvget(vunit[j],"NS",&tempns,0);
               if (tempnl!=mfnl[ibj]||tempns!=mfns[ibj])
                  {
                  printf("file %s has\n",infil[j-1]);
                  zmabend("(nl,ns) disagreement, label vs. IBIS file");
                  }
               for (k=0;k<maxcross;k++) if (bufused[k]==0)
                  {
                  bufused[k] = 1;
                  bufix[j] = k;
                  bix = k;
                  break;
                  }
               }
            }
         if (openstat[j]==1) /* read the line and apply Moore */
            {
            curline = sline+iread-mfsl[ibj];
            if ((curline%moorefac)!=mooreend) continue;
            if (firstread[j]==0) firstread[j] = 1;
            previx = rotix[j];
            currix = (rotix[j]+1)%redfeather;
            rotix[j] = currix;
            tns = mfns[ibj];
            redtns = (tns+moorefac-1)/moorefac;
            
            if (moorefac==1)
               {
               status = zvread(vunit[j],footprnt[bix][currix],
                   "LINE",lineread[j]++,"SAMP",1,"NSAMPS",tns,0);
               }
            else
               {
               status = zvread(vunit[j],inbuf[bix],
                   "LINE", lineread[j], "SAMP", 1, "NSAMPS", tns, 0);
               lineread[j] += moorefac;
               if (lineread[j]>mfnl[ibj]) lineread[j] = mfnl[ibj];
               for (i=moorectr,ired=0;i<tns;i+=moorefac,ired++)
                  footprnt[bix][currix][ired] = inbuf[bix][i];
               if (tns%moorefac!=0)
                  footprnt[bix][currix][redtns-1] = inbuf[bix][tns-1];
               }
            
            /* nibble operations */
            if (edge)
               {
               for (i=0;i<lnibble;i++) footprnt[bix][currix][i] = 0;
               for (i=1;i<=rnibble;i++) footprnt[bix][currix][redtns-i] = 0;
               if (lthresh>0)
                  {
                  thrcount = 0;
                  for (i=lnibble;i<redtns;i+=nincr)
                     {
                     if (footprnt[bix][currix][i]>=lthresh)
                        {
                        thrcount++;
                        if (thrcount>=nseq)
                           {
                           for (k=lnibble;k<=i;k++)
                              footprnt[bix][currix][k] = 0;
                           break;
                           }
                        }
                     else thrcount = 0;
                     }
                  if (thrcount<nseq) 
                     {
                     for (k=lnibble;k<redtns;k++) footprnt[bix][currix][k] = 0;
                     goto edgedone2;
                     }
                  }
               if (rthresh>0)
                  {
                  thrcount = 0;
                  for (i=redtns-1-rnibble;i>=0;i-=nincr)
                     {
                     if (footprnt[bix][currix][i]>=rthresh)
                        {
                        thrcount++;
                        if (thrcount>=nseq)
                           {
                           for (k=redtns-1-rnibble;k>=i;k--)
                               footprnt[bix][currix][k] = 0;
                           break;
                           }
                        }
                     else thrcount = 0;
                     }
                  if (thrcount<nseq)
                     for (k=lnibble;k<redtns;k++) footprnt[bix][currix][k] = 0;
                  }
               }
            edgedone2:
            
            if (firstread[j]==1) /* first line Moore */
               {
               firstread[j] = 2;
               for (i=0;i<redtns;i++)
                  {
                  if (footprnt[bix][currix][i]<thresh)
                     moore[bix][currix][i] = (unsigned char)0;
                  else moore[bix][currix][i] = (unsigned char)1;
                  }
               continue;
               }
            if ((mfsl[ibj]+mfnl[ibj])<=(sline+iread+1)) /* last line Moore */
               {
               for (i=0;i<redtns;i++)
                  {
                  if (footprnt[bix][currix][i]<thresh)
                     {
                     moore[bix][currix][i] = (unsigned char)0;
                     if (previx>=0&&((int)(moore[bix][previx][i])>1))
                     propagate(1,moore[bix],previx,i,redfeather,redtns,currix);
                     }
                  else
                     {
                     moore[bix][currix][i] = (unsigned char)1;
                     if (previx>=0&&((int)(moore[bix][previx][i])>2))
                     propagate(2,moore[bix],previx,i,redfeather,redtns,currix);
                     }
                  }
               openstat[j] = 2;
               continue;
               }
            
            datafound = 0;
            for (i=0;i<redtns;i++)
               {
               leftbrk = i;
               if (footprnt[bix][currix][i]>=thresh) { datafound = 1; break; }
               moore[bix][currix][i] = (unsigned char)0;
               if (previx>=0&&((int)(moore[bix][previx][i])>1))
                  propagate(1,moore[bix],previx,i,redfeather,redtns,currix);
               }
            if (!datafound) continue;
            for (i=redtns-1;i>=0;i--)
               {
               rightbrk = i;
               if (footprnt[bix][currix][i]>=thresh) break;
               moore[bix][currix][i] = (unsigned char)0;
               if (previx>=0&&((int)(moore[bix][previx][i])>1))
                  propagate(1,moore[bix],previx,i,redfeather,redtns,currix);
               }
            midns = (leftbrk+rightbrk)/2;
            if (leftbrk==0) moore[bix][currix][leftbrk++] = (unsigned char)1;
            if (rightbrk==(redtns-1)) moore[bix][currix][rightbrk--] = (unsigned char)1;
            for (i=leftbrk;i<midns;i++)
               {
               moore[bix][currix][i] = (unsigned char)MIN((int)moore[bix][currix][i-1]+1,
                  MIN((int)moore[bix][previx][i]+1,redfeather));
               }
            for (i=rightbrk;i>=midns;i--)
               {
               moore[bix][currix][i] = (unsigned char)MIN((int)moore[bix][currix][i+1]+1,
                  MIN((int)moore[bix][previx][i]+1,redfeather));
               }
            } /* openstat[j]==1 cases */
         } /* j=1..filecount loop */
     
      if (iout>=0) /* write output of oldest line in buffer */
         {
         /* initialize the start and stop points of the output line in bufhit */
         
         bufhit[0] = 1;
         for (j=1;j<filcount;j++)
            {
            if (openstat[j]>0&&openstat[j]<3)
               {
               ibj = ibisix[j];
               if (iout>=mfsl[ibj]-sline)
                  {
                  ixl = mfss[ibj]-ssamp;
                  ixr = mfss[ibj]+mfns[ibj]-ssamp;
                  bufhit[ixl] = 1;
                  bufhit[ixr] = 1;
                  }
               }
            }
         for (i=0;i<nsamp;i++)
            {
            /*if (iout==0&&i==4) dbg = 1; else dbg = 0;*/
            if (bufhit[i]==1)
               {
               listix = 0;
               for (j=1;j<filcount;j++)
                  {
                  ibj = ibisix[j];
                  if ((openstat[j]>0)&&(openstat[j]<3)&&
                     (i>=(mfss[ibj]-ssamp))&&
                     (i<(mfss[ibj]+mfns[ibj]-ssamp))&&
                     (iread>=(mfsl[ibj]-sline+dfeather-1)))
                     {
                     inlist[listix] = j;
                     sampoff[listix++] = mfss[ibj]-ssamp;
                     }
                  }
               bufhit[i] = 0;
               }
            if (listix==1)
               {
               j = inlist[0];
               bix = bufix[j];
               ix2 = (rotix[j]+1)%redfeather;
               ix3 = i-sampoff[0];
               ix3red = ix3/moorefac;
               if (moore[bix][ix2][ix3red]<=moorenbl)
                  { bufout[i] = 0; continue; }  /* moore is lo-res */
               if (moorefac==1)                 /* tval is hi-res */
                  {
                  tval = footprnt[bix][ix2][ix3];
                  }
               else
                  {
                  ibj = ibisix[j];
                  rereadline = iout-mfsl[ibj]+sline+1;  /* no adjust to moorectr */
                  if (rereadmemory[j]!=rereadline)
                     {
                     status = zvread(vunit[j],inbuf[bix],
                        "LINE",rereadline,"SAMP",1,"NSAMPS",mfns[ibj],0);
                     rereadmemory[j] = rereadline;
                     }
                  tval = inbuf[bix][ix3];
                  }           
               
               if (ramp<3&&tval>=thresh)                       /* ramp is hi-res */
                  {
                  iix = ibisix[j];
                  loff = mfsl[iix]-sline;
                  soff = mfss[iix]-ssamp;
                  hnl = (float)(mfnl[iix]+1)*0.5;
                  hns = (float)(mfns[iix]+1)*0.5;
                  fhnl = 1.0/hnl;
                  fhns = 1.0/hns;
                  locnl = (float)(iout-loff+1);
                  locns = (float)(i-soff+1);
                  /*if (dbg) printf("A:locnl,locns,hnl,hns %f %f %f %f\n",
                                     locnl,locns,hnl,hns);*/
                  if (locnl<hnl)
                     {
                     if (locns<hns)
                        zramp = rampcalc(hns-locns,hnl-locnl,dz,countz,j,3,2,zbar[j]);
                     else
                        zramp = rampcalc(locns-hns,hnl-locnl,dz,countz,j,3,0,zbar[j]);
                     }
                  else
                     {
                     if (locns<hns)
                        zramp = rampcalc(hns-locns,locnl-hnl,dz,countz,j,1,2,zbar[j]);
                     else
                        zramp = rampcalc(locns-hns,locnl-hnl,dz,countz,j,1,0,zbar[j]);
                     }
                  }
               else zramp = 0.0;
               /*if (dbg) printf("B:tval,adjust,zbar[j],zramp %d %d %f %f\n",
                                      tval,adjust,zbar[j],zramp);*/
               switch (adjust)
                  {
                  case 0: bufout[i] = tval; break;
                  case 1: bufout[i] = MAX((short int)0,(short int)
                          ((float)tval*mffac1[ibisix[j]]+zramp+0.5)); break;
                  case 2: bufout[i] = MAX((short int)0,(short int)
                          ((float)tval+mffac1[ibisix[j]]+zramp+0.5)); break;
                  case 3: if (tval>0) bufout[i] = MAX((short int)1,(short int)
                          ((float)tval+mffac1[ibisix[j]]-zramp+0.5));
                          else bufout[i] = 0; break;
                  }
               }
            else if (listix==0) bufout[i] = 0;
            else                                 /* listix>1 overlap case */
               {
               usum = 0; lsum = 0; kk = 0;
               for (k=0;k<listix;k++)
                  {
                  j = inlist[k];
                  bix = bufix[j];
                  ix2 = (rotix[j]+1)%redfeather;
                  ix3 = i-sampoff[k];
                  ix3red = ix3/moorefac;
                  di = (float)(MAX((int)
                     (moore[bix][ix2][ix3red]-moorenbl),0));  /* moore is lo-res */
                  
                  if (moorefac==1)                           /* vi is hi-res */
                     {
                     tval = (int)footprnt[bix][ix2][ix3];
                     }
                  else
                     {
                     ibj = ibisix[j];
                     rereadline = iout-mfsl[ibj]+sline+1;  /* no adjust to moorectr */
                     if (rereadmemory[j]!=rereadline)
                        {
                        status = zvread(vunit[j],inbuf[bix],
                           "LINE",rereadline,"SAMP",1,"NSAMPS",mfns[ibj],0);
                        rereadmemory[j] = rereadline;
                        }
                     tval = (int)inbuf[bix][ix3];
                     }
                  if (tval<thresh) continue;
                  vi = (float)tval;
                  
                  if (ramp<3)
                     {
                     iix = ibisix[j];
                     loff = mfsl[iix]-sline;
                     soff = mfss[iix]-ssamp;
                     hnl = (float)(mfnl[iix]+1)*0.5;
                     hns = (float)(mfns[iix]+1)*0.5;
                     fhnl = 1.0/hnl;
                     fhns = 1.0/hns;
                     locnl = (float)(iout-loff+1);
                     locns = (float)(i-soff+1);
                     if (locnl<hnl)
                        {
                        if (locns<hns)
                           zramp = rampcalc(hns-locns,hnl-locnl,dz,countz,j,3,2,zbar[j]);
                        else
                           zramp = rampcalc(locns-hns,hnl-locnl,dz,countz,j,3,0,zbar[j]);
                        }
                     else
                        {
                        if (locns<hns)
                           zramp = rampcalc(hns-locns,locnl-hnl,dz,countz,j,1,2,zbar[j]);
                        else
                           zramp = rampcalc(locns-hns,locnl-hnl,dz,countz,j,1,0,zbar[j]);
                        }
                     }
                  /*if (dbg) printf("C:tval,adjust,zbar[j],zramp %d %d %f %f\n",
                                tval,adjust,zbar[j],zramp);*/
                  switch (adjust)
                     {
                     case 1: vi = vi*mffac1[ibisix[j]]+zramp; break;
                     case 2: /* same as case 3 */
                     case 3: vi = vi+mffac1[ibisix[j]]+zramp; break;
                     }
                  if (cloudout>0)
                     {
                     if (kk>0)
                        {
                        if (vi>=(ovi+fcloudout)) continue;
                        if (ovi>=(vi+fcloudout))
                           {
                           usum = 0;
                           lsum = 0;
                           ovi = vi;
                           odi = di;
                           }
                        }
                     else { ovi = vi; odi = di; }
                     }
                  usum += di*vi;
                  lsum += di;
                  kk++;
                  /*if (dbg)
                     {
                     printf("D:di,vi,lsum,usum %f %f %f %f\n",
                                di,vi,lsum,usum);
                     }*/
                  }
               if (lsum<0.01) bufout[i] = (short int)0;
               else if (adjust!=3) 
                  bufout[i] = MAX((short int)0,(short int)((usum/lsum)+0.5));
               else 
                  bufout[i] = MAX((short int)1,(short int)((usum/lsum)+0.5));
               } /* listix>1 case */
            if (fmt_byte&&(bufout[i]>255)) bufout[i] = 255;
            } /* i=0..nsamp */
         zvwrit(o_unit,bufout,"LINE",iout+1,"SAMP",1,"NSAMPS", nsamp, 0);
         } /* iout>=0 output of oldest line in bufffer */
      } /* the big input/output loop */
   
   /* write the output ibis columns, old values read and preserved with
   new values as update, but has to be the 'RAMP case */
   
   if (ramp==1)
      {
      mz_alloc2((unsigned char ***)&ibisdz,5,nrec,4);
      
      for (i=0;i<5;i++)
         {
         status = IBISColumnSet(ibis,"U_FORMAT","REAL",rcols[5+i]);
         if (status!=1) IBISSignal(ibis,status,1);
         status = IBISColumnRead(ibis,ibisdz[i],rcols[5+i],1,nrec);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      
      for (j=1;j<filcount;j++)
         {
         i = ibisix[j];
         ibisdz[0][i] = zbar[j];
         for (k=0;k<4;k++) if (countz[k][j]==0) ibisdz[k+1][i] = -999.0;
                else ibisdz[k+1][i] = dz[k][j];
         }
   
      for (i=0;i<5;i++)
         {
         status = IBISColumnWrite(ibis,ibisdz[i],rcols[5+i],1,nrec);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      }
   
   /* update the geotiff label, gtreplab reopens for update */
   /* the tout[] solutions are in GeoTIFF coordinates, see size */
   
   zvclose(o_unit,0);
   if (geotiff)
      {
      bcor = t[0]+t[1]+t[2];
      dcor = t[3]+t[4]+t[5];
      p = ms_find(labelstr1,"GTRASTERTYPEGEOKEY=2");
      if (p!=0) voff = 1.0; else voff = 0.5;
      
      toutinv[0] = tinv[3];
      toutinv[1] = tinv[4];
      toutinv[2] = 1.0-voff-toutinv[0]*bcor-toutinv[1]*dcor-ssamp;
      toutinv[3] = tinv[0];
      toutinv[4] = tinv[1];
      toutinv[5] = 1.0-voff-toutinv[3]*bcor-toutinv[4]*dcor-sline;
      
      scale2 = -t[3];
      scale1 = t[1];
      
      invertmap(toutinv,tout);
      scalefmt(scalestr,scale1,scale2);
      trnsfmt(transstr,tout);
      gtreplab("OUT",1,labelstr1,0,1,toutinv,scalestr,transstr);
      }
 
   /* close and exit */
   
   return 0;

/* some useful debugging code
   
   ibj = ibisix[j];
   tns = mfns[ibj];
   redtns = (tns+moorefac-1)/moorefac;
   for (qq=1;qq<=redfeather;qq++) {for (q=0;q<redtns;q++)
    printf("%4d",footprnt[bix][(currix+qq)%redfeather][q]);printf("\n");}
   printf(" rotix %d\n",ix2);
   for (qq=1;qq<=redfeather;qq++)             
   {for (q=0;q<tns;q++)
    printf("%4d",moore[bix][(currix+qq)%redfeather][q]);printf("\n");}
   printf("\n");
                  
*/  
   
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create featherv.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM featherv

   To Create the build file give the command:

		$ vimake featherv			(VMS)
   or
		% vimake featherv			(Unix)


************************************************************************/


#define PROGRAM	featherv

#define MODULE_LIST featherv.c

#define MAIN_LANG_C
#define R2LIB
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create featherv.pdf
process help=*
!  FILE NAMES      
!
PARM INP     TYPE=STRING   COUNT=(2:400)
PARM OUT     TYPE=STRING   COUNT=1
PARM COLS    TYPE=INTEGER  COUNT=(5:7)   DEFAULT=(1,2,3,4,5,6,7)
!
PARM SIZE    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SS      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM NL      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=1       DEFAULT=0
!
PARM ADJUST   TYPE=KEYWORD  COUNT=1   DEFAULT=NOADJ +
      VALID=(NOADJ,FACTOR,ADD,ADDZMASK)
PARM DFEATHER TYPE=INTEGER  COUNT=1   DEFAULT=10     VALID=(2:50000)
PARM MOOREFAC TYPE=INTEGER  COUNT=1   DEFAULT=5      VALID=(1:199)
PARM CLOUDOUT TYPE=INTEGER  COUNT=1   DEFAULT=0      VALID=(0:255)
!
PARM RAMP     TYPE=KEYWORD  COUNT=1   DEFAULT=RAMP +
      VALID=(NORAMP,RAMP,READRAMP)
PARM RDKTHR   TYPE=INTEGER  COUNT=1   DEFAULT=10    VALID=(-32768:32767)
PARM RDIFFTHR TYPE=INTEGER  COUNT=1   DEFAULT=35    VALID=(1:32767)
PARM RMOORE   TYPE=INTEGER  COUNT=1   DEFAULT=5     VALID=(1:255)
PARM GOREFAC  TYPE=REAL     COUNT=1   DEFAULT=0.6   VALID=(0.0:1.0)
PARM RCOLS    TYPE=INTEGER  COUNT=10  DEFAULT=(8,9,10,11,12,13,14,15,16,17)
PARM MOORENBL TYPE=INTEGER  COUNT=1   DEFAULT=0     VALID=(0:255)
!
PARM THRESH  TYPE=INTEGER  COUNT=1       DEFAULT=1        VALID=(-32768:32767)
PARM PROGRESS TYPE=KEYWORD COUNT=(0:1)   DEFAULT=--       VALID=PROGRESS
PARM EDGE    TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--       VALID=EDGE
PARM NTHRESH TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM LTHRESH TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM RTHRESH TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NSEQ    TYPE=INTEGER  COUNT=1       DEFAULT=8        VALID=(1:32767)
PARM NIBBLE  TYPE=INTEGER  COUNT=1       DEFAULT=4        VALID=(0:32767)
PARM LNIBBLE TYPE=INTEGER  COUNT=1       DEFAULT=0        VALID=(0:32767)
PARM RNIBBLE TYPE=INTEGER  COUNT=1       DEFAULT=0        VALID=(0:32767)
PARM NINCR   TYPE=INTEGER  COUNT=1       DEFAULT=1        VALID=(1:32767)
!
PARM GEOTIFF TYPE=KEYWORD COUNT=(0:1)   DEFAULT=--    VALID=GEOTIFF
PARM TOLER   TYPE=REAL    COUNT=1   DEFAULT=0.0000001 VALID=(0.0:1.0)
END-PROC
.TITLE
FEATHERV - Program for mosaicking images with Moore distance feathering, ramp version
.HELP
 PURPOSE:
Program FEATHERV takes input images and mosaics them to form an output image
with a gradual feathering in all directions of all image overlaps.
FEATHERV performs mosaicking by reading all of the input image files and 
combining them to form an output image according to the parameters for
edging, and an IBIS file for image placement.  In a typical application, the
output image is a composite picture made from two or more input images each
of which contains a portion of the overall picture.  There are special 
features for cloud (or glint) removal, for brightness correction, for
colossal mosaics via tiling, and for analyst error checking.

A special feature of this program is that tiepoints between frames that are
declared to be neighbors in the IBIS file are gathered.  The brightness difference
statistics are averaged and result in two additional corrections:

1.  The average difference (halved) to all neighbors is added to each frame as
    a constant.  The constant is reported in a column of the IBIS file.

2.  The remaining differences (halved) to each declared neighbor is added to
    each frame as an angled ramp or half-plane.  The half plane is "hinged",
    has a zero value, down or across the middle of the image.  At the edge
    of the image, it has a ramp height that is reported in the IBIS file.
    
The four ramps are merged in each quadrant of the image by an approximation to
angular interpolation.  For example, the 45 degree diagonal to the upper left
corner of the image would get half of its value from the left=facing ramp and
half from the top-facing ramp.  The approximation is to use

         y/(x+y) to approximate atan(y/x)/90
 and        
         x/(x+y) to approximate (90-atan(y/x))/90

for example, on the diagonal, x=y and all four formulas give .5

The constant and ramp corrections are applied after the ADJUST keyword corrections,
since it is best to apply them as a residual operation.  The feathering and
cloud removal are applied after the constant and ramp corrections.
.PAGE
 EXECUTION:
 
Note that the image files are first in the parameter sequence, and the last
input is an IBIS file.  The IBIS file contains master mosaicking information
on all of the images used in the INP parameter (at the least) and is
recommended to contain information on all images in the application, even if
this numbers in the tens of thousands.  The program limits at present (9/99)
has a limit of 400 inputs, and it opens and closes them as a narrow mosaicking
band passes from top to bottom of the output.  The number of files open over
the band cannot exceed the VICAR limit for number of files open at once.
The IBIS file contains the following information in columns that can be
parameter specified or defaulted.  The default column numbers are given here:

1.  The name of the image file in any order, with or without pathname.
    Only the part of the pathname/filename after the last \ or / is read
    for checking
2.  The offset SL of the image input (see GeoTIFF options below)
3.  The offset SS of the image input (see GeoTIFF options below)
4.  The NL of the image input (cannot be less as in FASTMOS) (see GeoTIFF options below)
5.  The NS of the image input (cannot be less as in FASTMOS) (see GeoTIFF options below)
6.  Input factor for brightness equalization formulas (only one as of 9/99)
7.  Reserved for more complex brightness equalization formulas

GeoTIFF Options:  If the images all have GeoTIFF labels and the 'GEOTIFF
keyword is given, then there are two possibilities:

The first is: if columns 2-5 contain non-zero values (only column 4 is
checked for 0), then the values are compared with values calculated from
the GeoTIFF labels or the nl,ns from the VICAR label.  Since the values
might calculate with a slight inaccuracy, the TOLER parameter is provided
with a default value of 1.e-7.  If a calculated offset exceeds the TOLER
parameter, this indicates that the files have an inaccurate mapping for
the purposes of mosaicking and do not have an integral offset.  All mappings
are checked for consistency (map projection type, zones, meridians,
etc.).

The second is: if columns 2-5 contain zero values (only column 4 is
checked for 0), then the values are calculated from the GeoTIFF labels
or the nl,ns from the VICAR label.  The only check is that the offsets
are integral values (within the parameter TOLER) and that the mappings
are consistent.

When in the GeoTIFF case, a geotiff label is calculated and written
for the output mosaicked image.

The IBIS file is used for two reasons.  First, the input of offsets is 
simplified.  Second, coverage can be checked.  The program will abort with
an error message if a mosaic is performed and an input file cannot be found
in the IBIS file.  The program will also abort with an error message if a
mosaic is performed and an file in the IBIS file touches the output area
and is not included in the INP parameter (this is why NL,NS is in the IBIS
file).  If a file is opened and its (NL,NS) does not agree with the IBIS
file, the program aborts.  In all of these cases, the file is named.

The size of the output image is determined by the number of lines and number 
of samples in the SIZE field if the SIZE field is entered by the user.  If the
SIZE field is not entered, the output file is the same size as the first image
file (see note above, this is the second input). The input images are not
required all to have the same size.  The data type of the input images may
either be byte or halfword data, but all of the input images must have the
same data type.  The data type is obtained from the VICAR label of the first 
input image.

For mosaicking, the program needs to know the locations in the output image 
of each of the input images.  This is done by giving the location in
the output image of each input image.  The locations are given in terms
of line number and pixel number within a line.  This is no longer given
by parameters, but must be placed in the columns of an IBIS file as discussed
above.  The offset value must be in the IBIS file unless GeoTIFF labels are
used.  An offset of (1,1) means that the upper left pixel of the offset image
would line up with the upper left pixel of the output image (assuming that
it also had a (sl,s) of (1,1) (the normal case).

An input image is not required to lie entirely within the boundaries of the
output image.  If the upper left hand corner of an input image is not within 
the boundaries of the output image, the location of the input image is given
by extending the numbering of lines and pixels beyond the boundaries of the
output image.  Thus negative numbers or zero would be used for the locations
of input images beginning to the left or above the boundaries of the output
image.  Input images are allowed to overlap, in fact, that is the motivation
for this program.

FEATHERV only has a single mode of overlap, namely, averaging.  The value at
a pixel will generally be the average of all values above the THRESH value
from images that cover the pixel.  But the averaging is modified by feathering
the edges of data as follows.  For each input, a MOORE DISTANCE ALGORITHM is
applied to data inside of the image so that the image is mirrored by a edge
distance image that gives distance from the edge of data above the THRESH 
value or the absolute edge of the input image (if the data goes fully to the
edge of the input image).  The MOORE DISTANCE ALGORITHM has to be applied
outside of the mosaic output image area, because an image edge some distance
from the mosaic output image area can contribute to a MOORE DISTANCE NUMBER
inside of the mosaic output image area.

for example, the image data

 16  16  16  16  16  16  16 
 16  16  16  16  16  16  16
 16  16  16  16  16  16  16
 16  16  16  16  16  16  16 
 16  16   0   0   0  16  16  
 
has the MOORE DISTANCE NUMBERS

 01  01  01  01  01  01  01 
 01  02  02  02  02  02  01
 01  02  02  02  02  02  01
 01  02  01  01  01  02  01 
 01  01   0   0   0  01  01  

The inputs are then weighted by the MOORE DISTANCE NUMBERS

   output = sum((Ith input pixel value)*(Ith MOORE DISTANCE NUMBER))
                  divided by sum(Ith MOORE DISTANCE NUMBER)
                  
The user inputs a parameter DFEATHER to set the distance that the MOORE
DISTANCE ALGORITHM will work.  Then all remaining inside numbers are set
to that value as a maximum.  If MOOREFAC is greater than one, the
value used is DFEATHER/MOOREFAC in tiles of size MOOREFAC x MOOREFAC, 
and also the DFEATHER will be adjusted slightly to a multiple of
MOOREFAC because of the tiling.  The net effect of MOOREFAC>1 is to
still feather to depth DFEATHER, but in coarser steps.

Increasing DFEATHER has two consequences.  First, the program will run
more slowly, probably sublinear to the size of this parameter.  Second,
the program will consume more memory space.  The virtual memory space in
bytes needed for image buffers and MOORE DISTANCE buffers is:

  3 x (max number of inputs open) x (max input NS) x DFEATHER / MOOREFAC**2

FEATHERV is written entirely in the C programming language and uses dynamic
memory allocation to avoid imposing any restrictions on the size of the
images. 

FEATHERV HAS BOTH THE OLD TYPES OF NIBBLING (SEE NEXT PARAGRAPH) AND
A NEW EDGING CAPABILITY BASED ON THE MOORE ALGORITHM.  The new edging
nibbles in a perpendicular direction to the edge of data in all
directions (the old nibble worked from the righ or left only).  Use 
the parameter MOORENBL to invoke this.  The default of 0 is no nibble,
a value of n nibbles all values where the MOORE distance from the edge
is n, etc.  To give a smooth transition, instead of starting at n+1,
the whole MOORE distance input is reduced by n so the MOORE distance
that is used in feathering still starts at 1.  Keep in mind that
MOOREFAC=3 means that a MOORENBL=1 will nibble 3 pixels from all
edges.

FEATHERV has an option for edging the input images prior to applying the
mosaicking mode.  Edging effectively removes the 'edges' of the input images
so that pixels in the 'edges' are not considered to be contained in their
images as far as the mosaicking process is concerned.  Several parameters
determine the precise effect of edging in any situation, but basically if
edging is selected, then each line of each input image is edged first on the
left and then on the right.  Edging means scanning through a line from one
end or another to the point at which the data numbers are greater than or
equal to a threshold value and then removing the pixels from the end of
the line up to a certain number of pixels beyond the point.  This is 
referred to as finding the edge of the scene data and nibbling-in a 
certain number of pixels beyond the edge.  Because of the line by 
line processing performed by the program, edging is only available
in the horizontal direction.  (Edging the top and bottom of images would
normally require an intermediate data set.)  Edging is typically used
to remove distortion around the edges of pictures that was caused by
interpolation, filtering, or other things.  NOTE THAT EDGING IS INCREASED
BY USE OF MOOREFAC.  A NIBBLE OF TWO AND A MOOREFAC OF THREE WILL RESULT
IN SIX PIXELS BEING REMOVED.

FEATHERV has a parameter named ADJUST that can take a column(s) in the
IBIS file as an adjustment to brightness on a per image basis.  See the
parameter description for more details.

RAMP CALCULATIONS

A special feature of this program is that tiepoints between frames that are
declared to be neighbors in the IBIS file are gathered.  The brightness difference
statistics are averaged and result in two additional corrections:

1.  The average difference (halved) to all neighbors is added to each frame as
    a constant.  The constant is reported in a column of the IBIS file.

2.  The remaining differences (halved) to each declared neighbor is added to
    each frame as an angled ramp or half-plane.  The half plane is "hinged",
    has a zero value, down or across the middle of the image.  At the edge
    of the image, it has a ramp height that is reported in the IBIS file.
    
The four ramps are merged in each quadrant of the image by an approximation to
angular interpolation.  For example, the 45 degree diagonal to the upper left
corner of the image would get half of its value from the left=facing ramp and
half from the top-facing ramp.  The approximation is to use

         y/(x+y) to approximate atan(y/x)/90
 and        
         x/(x+y) to approximate (90-atan(y/x))/90

for example, on the diagonal, x=y and all four formulas give .5

The constant and ramp corrections are applied after the ADJUST keyword corrections,
since it is best to apply them as a residual operation.  The feathering and
cloud removal are applied after the constant and ramp corrections.


The output image has the same data format  (byte or halfword) as the input 
images.  
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      feather INP=(a...,ibis) OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      feather INP=(a...,ibis) OUT=b  SL=sl SS=ss NL=nl NS=ns optional parameters
      feather (a...,ibis) b (sl,ss,nl,ns) optional parameters
      feather (a...,ibis) b optional parameters

      Here '(a...,ibis)' represents a list of one to 399 input image file
      names followed by an ibis file, and 'b' represents the output image file
      name.
The GeoTIFF options are:
      The following command line formats show the major allowable forms:
      feather INP=(a...,ibis,gtref) OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      feather INP=(a...,ibis,gtref) OUT=b  SL=sl SS=ss NL=nl NS=ns optional parameters
      feather (a...,ibis,gtref) b (sl,ss,nl,ns) optional parameters
      feather (a...,ibis,gtref) b optional parameters

      Here '(a...,ibis,gtref)' represents a list of one to 398 input image file
      names followed by an ibis file, and then by a GeoTIFF reference image
      that defines the overall pixel space, and 'b' represents the output image file
      name.
.PAGE
EXAMPLES

See examples under program FASTMOS to learn more about the mosaicking process.
The major difference to note here is that SL,SS can be given for the mosaic
image space.
.PAGE
RESTRICTIONS
1. The input and output images must be byte or halfword data.
2. Plan carefully using the virtual memory limit formula

  3 x (max number of inputs open) x (max input NS) x DFEATHER / MOOREFAC**2

 OPERATION:

FEATHERV performs mosaicking on a line by line basis.  The offsetting of
input images is done at READ time.  However, the program has to read ahead
of the output line by distance DFEATHER to run the MOORE DISTANCE ALGORITHM.
This readahead buffer is called the "footprint" of the input.  The footprint
has to go above, to the right, to the left, and below the image output area
to get the MOORE DISTANCE NUMBERS.  Because the footprint buffers are large,
a rolling index scheme is used on them and on the MOORE DISTANCE BUFFERS.

Data in halfword format may include negative data numbers.  Negative data
numbers that do not meet the threshold criteria are ignored.
.PAGE

 TIMING: 

Will get back with this when some big cases are run.  Expect 3x slower
than FASTMOS.  Expect good behavior in the virtual memory (not a lot of
swapping).  

 ORIGINAL PROGRAMMER:    A. Zobrist          27 Oct 1999
 COGNIZANT PROGRAMMER:   Barbara McGuffie    27 Oct 1999
 
 REVISION HISTORY
  99-10-27    AZ   Initial version, named feather
  99-11-05    AZ   two pass version, named feather2
  99-11-18    AZ   ramps added to feather2
  99-11-18    AZ   halfword MOORE algorithm, named featherh
  99-11-28    AZ   all options rolled into featherh
  99-11-30    AZ   reduced resolution MOORE algorithm, named featherv
  00-04-16    AZ   GeoTIFF label use
    
.LEVEL1
.VARIABLE INP
Input image file names
followed by the controlling
IBIS file, and an optional
GeoTIFF reference image
.VARIABLE OUT
Output image file name
.VARIABLE COLS
Columns of the IBIS file that
contain the mosaic information
.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
You can enter SL,SS,NL,
and NS together as SIZE, OR
enter the SL,SS,NL, and NS
parameters separately.
.VARIABLE SL
Starting line number
(can window the output)
.VARIABLE SS
Starting sample number
(can window the output)
.VARIABLE NL
Number of lines output
.VARIABLE NS
Number of samples output
.VARIABLE THRESH
Threshold used for mosaicking.
.VARIABLE PROGRESS
Enter for progress reporting.
.VARIABLE EDGE
Enter for edging.
.VARIABLE NTHRESH
Threshold for edging on both
left and right.
.VARIABLE LTHRESH
Threshold for edging on left.
.VARIABLE RTHRESH
Threshold for edging on right.
.VARIABLE NSEQ
Number of sequential pixels
which must satisfying edging 
threshold criteria at edge of
scene data.
.VARIABLE NIBBLE
Number of pixels to remove
beyond edge of scene data for
edging on both left and right.
.VARIABLE LNIBBLE
Number of pixels to remove
beyond edge of scene data for
edging on left.
.VARIABLE RNIBBLE
Number of pixels to remove
beyond edge of scene data for
edging on right.
.VARIABLE NINCR
If NINCR=n, then scanning for
edge of scene data will check
every nth pixel.
.VARIABLE ADJUST
Set this to use IBIS columns
as a brightness adjustment
.VARIABLE DFEATHER
Feather width in pixels (div 2)
.VARIABLE MOOREFAC
Factor to reduce resolution of
MOORE algorithm, must be odd
.VARIABLE CLOUDOUT
Brightness difference to
identify clouds or glint
for erasure
.VARIABLE RAMP
'RAMP - apply ramping procedure
'NORAMP - don't apply ramping
'READRAMP - apply previous ramp
      values from file
.VARIABLE RDKTHR
Discard tiepoints with either
image raw value below this
.VARIABLE RDIFFTHR
Discard tiepoints with diff
in adjusted values above this
.VARIABLE RMOORE
Moore distance to use for
gathering tiepoints
.VARIABLE GOREFAC
Means of adjusting ramp for
gores
.VARIABLE RCOLS
The IBIS columns to input 
neighbors for ramping and
to output the ramp parameters
.VARIABLE MOORENBL
Nibble using MOORE distance
up to this value
.VARIABLE GEOTIFF
Use GeoTIFF labels from all
image inputs, including a
master reference as last input
.VARIABLE TOLER
Amount that GeoTIFF calculated
offsets can vary from integral
values
.LEVEL2
.VARIABLE INP
The last required file is an IBIS file giving information on the input
files.  The columns of the file are usually:

1.  The name of the image file in any order, with or without pathname.
    Only the part of the pathname/filename after the last \ or / is read
    for checking
2.  The offset SL of the image input
3.  The offset SS of the image input
4.  The NL of the image input (must be exact, cannot be less as in FASTMOS)
5.  The NS of the image input (must be exact, cannot be less as in FASTMOS)
6.  Input factor for brightness equalization formulas (only one as of 9/99)
7.  Reserved for more complex brightness equalization formulas

the numbering of the columns can be changed by the parameter COLS.  See
parameter ADJUST for an explanation of column six.

The program limits at present (9/99):
1. 400 inputs
2. The number allowed by the system for simultaneous open (somewhere 
   between 67 and 100 according to one expert).
   The files are opened and closed as a narrow mosaicking band passes
   from top to bottom of the output.  The width of this band is DFEATHER.
   The number of files open over the band cannot exceed the VICAR
   or system limit for number of files open at once.  It is possible that
   a UNIX system call can enlarge this.

The optional GeoTIFF reference image, which is placed after the IBIS file,
is used in the following way:
1.  Its pixel referencing is a master for the mosaic.
2.  All other images will have offsets relative to the master.
3.  The SIZE or SL,SS,NL,NS parameters will be relative to the master.
4.  The offsets can be placed in the IBIS file, in which case they are
    checked against the GeoTIFF mappings.
5.  The offsets can be zero'd in the IBIS file, in which case they are
    calculated from the GeoTIFF mappings.  NL,NS columns must be zero'd
    for this to occur and they are also calculated.
6.  For this case, the 'GEOTIFF keyword must be given and all input images
    are required to have GeoTIFF labels.
.VARIABLE OUT
A major difference with program FASTMOS is that (SL,SS) can be used to 
window down into the mosaic.  See HELP level 2 under SL.
.VARIABLE COLS
The last file is an IBIS file giving information on the input files.  The
columns of the file are usually:

1.  The name of the image file in any order, with or without pathname.
    Only the part of the pathname/filename after the last \ or / is read
    for checking
2.  The offset SL of the image input
3.  The offset SS of the image input
4.  The NL of the image input (must be exact, cannot be less as in FASTMOS)
5.  The NS of the image input (must be exact, cannot be less as in FASTMOS)
6.  Input factor for brightness equalization formulas (only one as of 9/99)
7.  Reserved for more complex brightness equalization formulas

By using this parameter, the columns can be renumbered as the user desires.
.VARIABLE SIZE
If the SIZE field is not entered, the output image has the same size as the
first input image.  If the SIZE field is entered, the number of lines and
number of samples refer to the size of the output image.
.VARIABLE SL
The default is 1.  Setting it larger moves the "window" of the mosaic upwards
in line number and is the same as lowering all of the individual input SL's
by an equivalent amount.  This means that the mosaic can conveniently be done
in sections by setting (SL,SS,NL,NS) in a checkerboard fashion and then by
butting together the checkerboard pieces with the VICAR programs APPEND and
MSS.
.VARIABLE SS
The default is 1.  Setting it larger moves the "window" of the mosaic upwards
in sample number and is the same as lowering all of the individual input SS's
by an equivalent amount.  This means that the mosaic can conveniently be done
in sections by setting (SL,SS,NL,NS) in a checkerboard fashion and then by
butting together the checkerboard pieces with the VICAR programs APPEND and
MSS.
.VARIABLE MMODE
The mosaicking mode specifies how the output data number values are determined 
from the input data numbers.   The following rules apply for each of the 
modes.  If none of the input images have a data number value for a 
given pixel that is greater than or equal to the THRESH value, the 
output data number is the data number from the first input image if 
the pixel is contained in the first input image, and the output 
data number is L0 if the pixel is not contained in the first input image, where
L0 is 0 if THRESH is greater than 0, L0 is 0 if THRESH=0 and the data format 
is byte, and L0 is equal to THRESH-1 otherwise.  If
exactly one of the input images has a data number value for a given pixel that
is greater than or equal to the THRESH value, the output data number is the
data number from the one input image.  If more than one of the input images
have a data number value for a given pixel that is greater than or equal to the
THRESH value, the output data number is determined by the mosaicking mode. 

There are currently five modes to choose from.  They are listed by name below.
For each mode a description is given of how the output data number is 
determined when there is more than one input image having a data number value 
that is greater than or equal to the THRESH value for a given pixel.  The
default mode is OVERLAY.

OVERLAY  - The input images are checked against the THRESH value in the order
           in which they are entered by the user.  The first data number value
           found which meets the threshold criteria is used for the output
           image.  This means that the order in which the input files are 
           entered gives a priority to the data in the files.

AVERAGE  - The average of the values meeting the threshold criteria is used.
           The average is found by integer division with no rounding.

MOD      - When there are two values meeting the threshold criteria, the 
           average of the values is used.  When there are more than two 
           values meeting the threshold criteria, the value closest to the
           average is used.  This mode may be particularly useful when
           combining many images with high bit-error rates.

MAX      - The maximum of the values meeting the threshold criteria is used.

MIN      - The minimum of the values meeting the threshold criteria is used.
.VARIABLE THRESH
Only values greater than or equal to the THRESH threshold parameter are used
by the mosaicking mode in determining the data numbers for the output image.
The THRESH value is usually greater than 0 for mosaicking.  THRESH can be
set to 0 for cases such as averaging images.  The default value is 1.
(See Example 5 in the main help for details about the case of THRESH=0
for byte data.  Users may need to convert images to halfword to use THRESH=0
for mosaicking.  Other VICAR programs, such as INSECT may be an alternative.)

For halfword images for which negative DNs are considered valid, a negative
THRESH value may be used.  In this case, 0 is an inappropriate value for
representing the absence of image data.  When THRESH is less than 0, FEATHERV
uses an output DN of THRESH-1 to represent the absence of image data.
(If THRESH = -32768, -32768 is used to represent the absence of image data.)
This value is used as an output DN where the output pixel does not lie in one
of the input images.  (See the MMODE parameter.)
.VARIABLE PROGRESS
If the PROGRESS parameter is specified, FEATHERV prints a comment every 500th
line.  The default is to not print the progress.
.VARIABLE EDGE
If the EDGE parameter is specified, all input images are edged prior to
applying the mosaicking mode. No edging is the default.  'EDGE M U S T
be specified to invoke the edging algorithm.

ALSO NOTE THAT EDGING IS INCREASED BY THE USE OF MOOREFAC.  A NIBBLE OF
TWO AND A MOOREFAC OF THREE WILL RESULT IN SIX PIXELS BEING REMOVED.

If edging is selected, then each line of each input image is edged first on the
left and then on the right.  Edging means scanning through a line from one
end or another to the point at which the data numbers are greater than or
equal to a threshold value and then removing the pixels from the end of
the line up to a certain number of pixels beyond the point.  This is 
referred to as finding the edge of the scene data and nibbling in a 
certain number of pixels beyond the edge.  Because of the line by 
line processing performed by the program, edging is only available
in the horizontal direction.  (Edging the top and bottom of images would
normally require an intermediate data set.)  Edging is typically used
to remove distortion around the edges of pictures that was caused by
interpolation, filtering, or other things.

Several parameters are used to control the way that edging is done.  The 
parameters NTHRESH, LTHRESH, RTHRESH, NSEQ, and NINCR determine the location
of the edge of the scene data for lines of the input images.  The parameters
NIBBLE, LNIBBLE, and RNIBBLE determine how many pixels beyond the edge of 
the scene data are removed.

The edge of the scene data for a line is determined as follows.  The program
scans through the pixels of a line comparing the data numbers against the
edging threshold.  (Separate edging thresholds can be specified for scanning
from the left and scanning from the right using the LTHRESH and RTHRESH
parameters.  The NTHRESH parameter can be used to specify the same threshold
for scanning from the left and scanning from the right.)  The scanning begins
at one end of the line, and it checks successive pixels unless the NINCR
parameter is entered.  If NINCR is entered, the scanning checks only every
NINCRth pixel.  The program scans until it finds a group of NSEQ consecutive
(in terms of NINCR) pixels all of which have a data number greater than or
equal to the edging threshold.  The edge of the scene data is defined as the
first pixel (according to the direction of the scan) of that group.

The nibbling number is the number of pixels, starting with the edge of the
scene data, which are to be removed along with any pixels from the end of
the line to the edge of the scene data.  (If the nibbling number is zero, 
then just the pixels from the end of the line to the edge of the scene data
are removed.)  Separate nibbling numbers can be specified for scanning from 
the left and scanning from the right using the LNIBBLE and RNIBBLE parameters.
The NIBBLE parameter can be used to specify the same nibbling number for 
scanning from the left and scanning from the right.  

If no edge of the scene data is found when scanning, the entire line is 
removed.
.VARIABLE NTHRESH
The default for NTHRESH is THRESH.  (See also under EDGE.)
.VARIABLE LTHRESH
The default for LTHRESH is NTHRESH.  (See also under EDGE.)
.VARIABLE RTHRESH
The default for RTHRESH is NTHRESH.  (See also under EDGE.)
.VARIABLE NSEQ
The default for NSEQ is 8.  (See also under EDGE.)
.VARIABLE NIBBLE
The default for NIBBLE is 4.  (See also under EDGE.)

ALSO NOTE THAT EDGING IS INCREASED BY THE USE OF MOOREFAC.  A NIBBLE OF
TWO AND A MOOREFAC OF THREE WILL RESULT IN SIX PIXELS BEING REMOVED.
.VARIABLE LNIBBLE
The default for LNIBBLE is 4.  (See also under EDGE.)

ALSO NOTE THAT EDGING IS INCREASED BY THE USE OF MOOREFAC.  A NIBBLE OF
TWO AND A MOOREFAC OF THREE WILL RESULT IN SIX PIXELS BEING REMOVED.
.VARIABLE RNIBBLE
The default for RNIBBLE is 4.  (See also under EDGE.)

ALSO NOTE THAT EDGING IS INCREASED BY THE USE OF MOOREFAC.  A NIBBLE OF
TWO AND A MOOREFAC OF THREE WILL RESULT IN SIX PIXELS BEING REMOVED.
.VARIABLE NINCR
The default for NINCR is 1.  (See also under EDGE.)
.VARIABLE ADJUST
This keyword is defaulted to 'NOADJ which does no brightness adjustment.
Three kinds of adjustment can be performed using column 6 of the IBIS
file:

'FACTOR - All input images will be multiplied by the number in column 6
of the IBIS file as a brightness adjustment.  Note that there is a
mathematical risk of a one turing into a zero if the factor is less
than .5.  This would be significant if zero means "no data" for
mosaicking.

'ADD - All input images will be added to by the number in column 6
of the IBIS file as a brightness adjustment.

'ADDZMASK - All input images will be added to by the number in column 6
of the IBIS file as a brightness adjustment except that zeros/nonzeros
are preserved (pixels that were greater than 0 prior to the add are
compared to a minimum of 1, zeros remain at zero).  This is useful for
the case where zero means "no data" for mosaicking.

The adjustment is performed just prior to the output or the averaging for
output, so it does not affect the edging operations.  The cloud erasing
operation is done after this adjustment operation.
.VARIABLE DFEATHER
Width of feathering is the max distance of the MOORE DISTANCE ALGORITHM
measured in pixels.  The effective feathering will be twice this distance
because a large overlap will feather one image to its full MOORE DISTANCE
yielding a 50/50 averaging of the two images.  Then a reverse feather of 
the other image will take place.

A larger number will feather the inputs to a greater degree, but the program 
will use more time and memory.  Plan carefully using the virtual memory limit
formula

  3 x (max number of inputs open) x (max input NS) x DFEATHER / MOOREFAC**2

If MOOREFAC is greater than one, the value used is DFEATHER/MOOREFAC
in tiles of size MOOREFAC x MOOREFAC, and also the DFEATHER will be
adjusted slightly to a multiple of MOOREFAC because of the tiling.
The net effect of MOOREFAC>1 is to still feather to depth DFEATHER,
but in coarser steps.
.VARIABLE MOOREFAC
In the formula for storage:

  3 x (max number of inputs open) x (max input NS) x DFEATHER / MOOREFAC**2

DFEATHER increases storage linearly, and also increases compute time.  The
MOOREFAC reduces the resolution of the MOORE algorithm and decreases storage
quadratically, and also saves compute time.  Further, the ratio of
DFEATHER to MOOREFAC must be less than 256.  

A good suggested value is a ratio of 50 to 100.  This will result in
100 to 200 steps of brightness in feathering from one image to the other.
If ramping is used, the need for feathering is reduced and a smaller
ratio can be used.

Keep in mind that increasing MOOREFAC does not decrease the feathering
distance in the images, it simply coarsens the number of steps in the
MOORE distance function.  The human eye probably cannot see more than
100 steps in the typical mosaicking situation.
.VARIABLE CLOUDOUT
If a pixel comes from multiple inputs and one of the inputs is brighter than
one of the others (which of the others is order dependent for three or more)
by this amount, then it is deleted from the averaging process.  This
correction is applied after the brightness adjustment (see keyword ADJUST).
.VARIABLE RAMP
'RAMP - apply ramping procedure;
   The first pass over the files calculates the ramp values to apply
   to each scene.  The second pass constructs ramps to smooth together
   the scenes.  The ramp values are written to the IBIS file.
'NORAMP - don't apply ramping
   A single pass smoothes together the scenes using the Moore distance
   interpolation only.  No ramp values are calculated. No ramp values
   are written to the IBIS file.
'READRAMP - apply previous ramp values from file
   This option requires an IBIS file with ramp values calculated from
   a previous run with the 'RAMP option.  The previous run can be on
   a reduced image... the ramp values are size-independent.  A single
   pass is performed like the second pass of the 'RAMP option, and
   no values are written to the IBIS file.
.VARIABLE RDKTHR
This is for throwing out unreliable water or shadow pixels in Landsat, or space,
shadow, etc. for planets.
.VARIABLE RDIFFTHR
This allows glimmer, seasonal variation, wetness, etc. to be discarded from
tiepoints.  It is applied after the adjust parameter correction.
.VARIABLE RMOORE
Tiepoints in overlap areas for an image are taken at a particular distance
inside of the neighbor image.  The Moore distance function is used to determine
when a pixel is that distance inside.  The unit of distance is the pixel.
By setting this parameter to 7 (for example) all pixels that are a distance
of 7 from the edge of non-0 data in the neighbor overlap will be used as tiepoints.

The tiepoints from different neighbor frames are kept separate in the tally,
so that brightness ramps can be constructed to each neighbor.  Neighbors are
specified in the IBIS file input (see parameter rcols).
.VARIABLE GOREFAC
The ramps are four half-images hinged horizontally and vertically in the middle 
of an image.  The difference with a neighbor (divided by 2) is the height of the 
ramp at the edge of the image.  But the data may actually start inside the
image with a zero gore of data at the edge.  This factor multiplies the height of
the half plane so that it applies better to the data.
.VARIABLE RCOLS
The ten columns in order are (using their default column numbers, which can be changed)

8.   An index of the images (any unique integer index will do)
9.   Right neighbor to ramp to, or 0 if no neighbor to the right (using the index
     of column 8)
10.  Bottom neighbor to ramp to, or 0 if no neighbor to the bottom (using the index
     of column 8)
11.  Left neighbor to ramp to, or 0 if no neighbor to the left (using the index
     of column 8)
12.  Top neighbor to ramp to, or 0 if no neighbor to the top (using the index
     of column 8)
13.  Constant calculated by the ramping procedure, add to this image
14.  Ramp constant calculated by the ramping procedure for right-facing ramp (a
     value of -999.0 means that there is no ramp in this direction##)
15.  Ramp constant calculated by the ramping procedure for bottom-facing ramp (a
     value of -999.0 means that there is no ramp in this direction##)
16.  Ramp constant calculated by the ramping procedure for left-facing ramp (a
     value of -999.0 means that there is no ramp in this direction##)
17.  Ramp constant calculated by the ramping procedure for top-facing ramp (a
     value of -999.0 means that there is no ramp in this direction##)

## A ramp of height zero is different than no ramp.  The ramp of height zero
would be interpolated with neighboring ramps, causing them to curve towards
zero.  No ramp (-999.0) would not be interpolated with neighboring ramps.
.VARIABLE GEOTIFF
GeoTIFF Options:  If the images all have GeoTIFF labels and the 'GEOTIFF
keyword is given, then there are two possibilities:

The first is: if columns 2-5 contain non-zero values (only column 4 is
checked for 0), then the values are compared with values calculated from
the GeoTIFF labels or the nl,ns from the VICAR label.  Since the values
might calculate with a slight inaccuracy, the TOLER parameter is provided
with a default value of 1.e-7.  If a calculated offset exceeds the TOLER
parameter, this indicates that the files have an inaccurate mapping for
the purposes of mosaicking and do not have an integral offset.  All mappings
are checked for consistency (map projection type, zones, meridians,
etc.).

The second is: if columns 2-5 contain zero values (only column 4 is
checked for 0), then the values are calculated from the GeoTIFF labels
or the nl,ns from the VICAR label.  The only check is that the offsets
are integral values (within the parameter TOLER) and that the mappings
are consistent.
.VARIABLE TOLER
The calculated value, if within TOLER of an integral value, will be converted 
to the integral value for purposes of mosaicking.
.VARIABLE MOORENBL
FEATHERV HAS BOTH THE OLD TYPES OF NIBBLING (SEE PARAM EDGE) AND
A NEW EDGING CAPABILITY BASED ON THE MOORE ALGORITHM.  The new edging
nibbles in a perpendicular direction to the edge of data in all
directions (the old nibble worked from the righ or left only).  Use 
the parameter MOORENBL to invoke this.  The default of 0 is no nibble,
a value of n nibbles all values where the MOORE distance from the edge
is n, etc.  To give a smooth transition, instead of starting at n+1,
the whole MOORE distance input is reduced by n so the MOORE distance
that is used in feathering still starts at 1.  Keep in mind that
MOOREFAC=3 means that a MOORENBL=1 will nibble 3 pixels from all
edges.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfeatherv.pdf
procedure
refgbl $echo
parm version string def="ibis-1"
parm org string def="column"
body
!let _onfail="continue"
let $echo="no"


!   TEST SCRIPT FOR featherv, really simple small case to see offsets
!   also compare with fastmos to see offsets the same

gen xim2 12 10 SINC=0 LINC=0 ival=10
gen xim3 10 12 SINC=0 LINC=0 ival=110

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(6,3,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(4,7,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=1 ss=1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'noramp moorefac=1

list ximmos 'zeroes

fastmos (xim2,xim3) ximmos2 nl=17 ns=15 off1=(6,3) off2=(4,7)

list ximmos2 'zeroes


! case to test two pass ramp corrections, use small overlap, foursquare

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

copy xim3 xim6    ! extra file for out of strip test

list xim6 'zeroes

ibis-gen xxa nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(4,1,10,12,.8,1509,1563,1510,0,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(1,11,12,10,4.0,1563,0,1564,1509,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(12,2,12,10,6.0,1510,1564,0,0,1509) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(11,10,10,12,1.2,1564,0,0,1510,1563) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim5") strcols=(1)

ibis-gen xxf nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(11,31,10,12,1.2,1565,0,0,0,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim6") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-cat (xxb,xxf) xxa
ibis-list xxa 'format

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos sl=3 ss=2 nl=20 ns=18 dfeather=4 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'ramp moorefac=1

list ximmos 'zeroes

ibis-list xxa 'format

! now test the 'READRAMP feature

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos2 sl=3 ss=2 nl=20 ns=18 dfeather=4 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'readramp moorefac=1

difpic (ximmos,ximmos2)

! now test the 'NORAMP feature

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos3 sl=3 ss=2 nl=20 ns=18 dfeather=4 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'noramp moorefac=1

list ximmos3 'zeroes

! now test the size parm feature, also case insensitive filenames

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos4 size=(3,2,20,18) dfeather=4 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'noramp moorefac=1

difpic (ximmos3,ximmos4)


!   TEST SCRIPT FOR featherv, simple small case


gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(11,3,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(9,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=3 ss=2 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'noramp moorefac=1

list ximmos 'zeroes

!   TEST SCRIPT FOR featherv, simple small case, top edge


gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-2,4,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-1,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=3 +
   'factor 'progress  'noramp moorefac=1

list ximmos 'zeroes

!   TEST SCRIPT FOR featherv, simple small case, right edge

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(3,9,10,12,0.88) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(1,8,12,10,1.5) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress  'noramp moorefac=1

list ximmos 'zeroes


!   TEST SCRIPT FOR featherv, simple small case, top edge


gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-2,4,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-1,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=3 +
   'factor 'progress  'noramp moorefac=1

list ximmos 'zeroes



!   TEST SCRIPT FOR featherv, simple small case, left edge

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(3,-2,10,12,0.88) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(1,-4,12,10,1.5) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress  'noramp moorefac=1

list ximmos 'zeroes


!   TEST SCRIPT FOR featherv, simple small case, cloudout

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(3,-2,10,12,0.88) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(1,-4,12,10,1.5) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress cloudout=60 'noramp moorefac=1

list ximmos 'zeroes



!   TEST SCRIPT FOR featherv, simple small case, nibble, nibble left-right 

gen xim1 12 10 SINC=1 LINC=0 ival=1
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=2 LINC=0 ival=2
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(3,1,10,12,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(7,3,12,10,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge nibble=2 'noramp moorefac=1

list ximmos 'zeroes

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge lnibble=2 'noramp moorefac=1

list ximmos 'zeroes

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge rnibble=2 'noramp moorefac=1

list ximmos 'zeroes


!   TEST SCRIPT FOR featherv, simple small case, thresh, thresh left-right 
!   also the nincr param test

gen xim1 12 10 SINC=1 LINC=0 ival=1
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=2 LINC=0 ival=2
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(3,1,10,12,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(7,3,12,10,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge nibble=2 nthresh=6 nseq=2 'noramp moorefac=1

list ximmos 'zeroes

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge nibble=2 lthresh=6 nseq=2 'noramp moorefac=1

list ximmos 'zeroes

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge nibble=2 rthresh=6 nseq=2 'noramp moorefac=1

list ximmos 'zeroes
featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge nibble=2 rthresh=6 nseq=2 nincr=2 'noramp moorefac=1

list ximmos 'zeroes



!   TEST SCRIPT FOR featherv, large case

gen xim1 1200 1000 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,500.5,1.,500.5,        1.,1000.,1.,1000.,+
            1200.,1.,11.9,1.,  1200.,500.5,1200.04,500.5,   1200.,1000.,1100.6,1000.)
gen xim1 1000 1200 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,1200.,0.,1300.,+
            1000.,1.,1100.,0., 1000.,1200.,1100.,1300.)

list xim2 'zeroes linc=100 sinc=100
list xim3 'zeroes linc=100 sinc=100

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(11,3,1000,1200,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(9,2,1200,1000,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=3 ss=2 nl=1700 ns=1500 dfeather=40 +
   'factor 'progress  'noramp moorefac=1

list ximmos 'zeroes  linc=100 sinc=100

! case to test storage regeneration, maxcross=2

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,3,10,12,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-4,5,12,10,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(25,3,12,10,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(27,5,10,12,1.0) datacols=(2,3,4,5,6) +
      string=("./xim5") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-list xxb 'format

featherv (xim2,xim3,xim4,xim5,xxb) ximmos sl=3 ss=2 nl=30 ns=15 dfeather=4 +
   'factor 'progress 'noramp moorefac=1

list ximmos 'zeroes


! test case for 'ADD keyword


gen xim1 12 10 SINC=2 LINC=0 ival=1
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=2 ival=2
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,3,10,12,-1) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-4,5,12,10,-2) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(25,3,12,10,-3) datacols=(2,3,4,5,6) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(27,5,10,12,-4) datacols=(2,3,4,5,6) +
      string=("./xim5") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-list xxb 'format



featherv (xim2,xim3,xim4,xim5,xxb) ximmos sl=3 ss=2 nl=30 ns=15 dfeather=4 +
   'add 'progress 'noramp moorefac=1

list ximmos 'zeroes


! test case for 'ADDZMASK keyword


gen xim1 12 10 SINC=2 LINC=0 ival=1
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=2 ival=2
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,3,10,12,-1) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-4,5,12,10,-2) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(25,3,12,10,-3) datacols=(2,3,4,5,6) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(27,5,10,12,-4) datacols=(2,3,4,5,6) +
      string=("./xim5") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-list xxb 'format



featherv (xim2,xim3,xim4,xim5,xxb) ximmos sl=3 ss=2 nl=30 ns=15 dfeather=4 +
   'addz 'progress 'noramp moorefac=1

list ximmos 'zeroes



!   TEST SCRIPT FOR featherv, MOOREFAC=3, horizontal
!   not a pretty case since overlap is almost total


gen xim1 13 13 SINC=0 LINC=0 ival=10
gen xim2 13 13 SINC=0 LINC=0 ival=110

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,4,13,13,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,18) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes


!   TEST SCRIPT FOR featherv, MOOREFAC=3, vertical
!   not a pretty case since overlap is almost total


gen xim1 13 13 SINC=0 LINC=0 ival=10
gen xim2 13 13 SINC=0 LINC=0 ival=110

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(4,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,16) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes

! case to test two pass ramp corrections with MOOREFAC=3

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

copy xim3 xim6    ! extra file for out of strip test

list xim6 'zeroes

ibis-gen xxa nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(4,1,10,12,.8,1509,1563,1510,0,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(1,8,12,10,4.0,1563,0,1564,1509,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(9,2,12,10,6.0,1510,1564,0,0,1509) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(8,7,10,12,1.2,1564,0,0,1510,1563) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim5") strcols=(1)

ibis-gen xxf nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(11,31,10,12,1.2,1565,0,0,0,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim6") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-cat (xxb,xxf) xxa
ibis-list xxa 'format

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos sl=3 ss=2 nl=20 ns=18 dfeather=6 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'ramp moorefac=3

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos2 sl=3 ss=2 nl=20 ns=18 dfeather=6 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'ramp moorefac=1

list ximmos 'zeroes ns=16
list ximmos2 'zeroes ns=16

f2 inp=(ximmos,ximmos2) out=ximmos3 func="abs(in1-in2)"
list ximmos3 'zeroes ns=16

ibis-list xxa 'format

!   TEST SCRIPT FOR featherv, MOOREFAC=3, horizontal
!   not a pretty case since overlap is almost total
!   now using unique input lines to see if proper lines in footprint
!   have to use standard debug statement in code to see this

gen xim1 13 13 SINC=1 LINC=10 ival=10
gen xim2 13 13 SINC=1 LINC=10 ival=110

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,4,13,13,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,18) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes


!   TEST SCRIPT FOR featherv, simple small case, leave out one input
!   but input doesn't touch output area, see devfeatherv.pdf for case
!   where it does touch output area and causes error condition


gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(11,33,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(9,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xxc) ximmos sl=3 ss=2 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'noramp moorefac=1

list ximmos 'zeroes


!   GeoTIFF case, offsets and nl,ns are in ibis file

gen xim1 nl=1 ns=1
gtgen inp=xim1 out=ximmaster 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,.3,.3,0.0)", +
          "ModelTiePointTag=(10,0,0,.5,.3,0.0)", +
          "ModelTiePointTag=(0,10,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim4 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim5 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

gtgen inp=xim4 out=xim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-2,-9,0,.3,.3,0.0)", +
          "ModelTiePointTag=(8,-9,0,.5,.3,0.0)", +
          "ModelTiePointTag=(-2,1,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gtgen inp=xim5 out=xim3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-7,-11,0,.3,.3,0.0)", +
          "ModelTiePointTag=(3,-11,0,.5,.3,0.0)", +
          "ModelTiePointTag=(-7,-1,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim2) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim3) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

list xim2 'zeroes
list xim3 'zeroes

gtlist ximmaster
gtlist xim2
gtlist xim3

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(11,7,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(9,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc,ximmaster) ximmos sl=3 ss=2 nl=17 ns=15 +
    dfeather=4 +
   'factor 'progress 'noramp moorefac=1 'geotiff toler=0.0000001

list ximmos 'zeroes


!   GeoTIFF case, no offsets in ibis file, standard vicar rotation

gen xim1 nl=1 ns=1
gtgen inp=xim1 out=ximmaster 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,.3,.5,0.0)", +
          "ModelTiePointTag=(10,0,0,.5,.5,0.0)", +
          "ModelTiePointTag=(0,10,0,.3,.3,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim4 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim5 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

gtgen inp=xim4 out=xim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-2,-9,0,.3,.5,0.0)", +
          "ModelTiePointTag=(8,-9,0,.5,.5,0.0)", +
          "ModelTiePointTag=(-2,1,0,.3,.3,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gtgen inp=xim5 out=xim3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-7,-11,0,.3,.5,0.0)", +
          "ModelTiePointTag=(3,-11,0,.5,.5,0.0)", +
          "ModelTiePointTag=(-7,-1,0,.3,.3,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim2) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim3) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

list xim2 'zeroes
list xim3 'zeroes

gtlist ximmaster
gtlist xim2
gtlist xim3

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(0,0,0,0,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(0,0,0,0,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc,ximmaster) ximmos sl=3 ss=2 nl=17 ns=15 +
    dfeather=4 +
   'factor 'progress 'noramp moorefac=1 'geotiff toler=0.0000001

list ximmos 'zeroes
gtlist ximmos

!   GeoTIFF case, no offsets in ibis file, now use area
!   type file different rotation

gen xim1 nl=1 ns=1
gtgen inp=xim1 out=ximmaster 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,.3,.3,0.0)", +
          "ModelTiePointTag=(0,10,0,.5,.3,0.0)", +
          "ModelTiePointTag=(10,0,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim4 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim5 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

gtgen inp=xim4 out=xim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-2,-9,0,.3,.3,0.0)", +
          "ModelTiePointTag=(-2,1,0,.5,.3,0.0)", +
          "ModelTiePointTag=(8,-9,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gtgen inp=xim5 out=xim3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-7,-11,0,.3,.3,0.0)", +
          "ModelTiePointTag=(-7,-1,0,.5,.3,0.0)", +
          "ModelTiePointTag=(3,-11,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim2) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim3) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

list xim2 'zeroes
list xim3 'zeroes

gtlist ximmaster
gtlist xim2
gtlist xim3

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(0,0,0,0,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(0,0,0,0,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc,ximmaster) ximmos sl=3 ss=2 nl=17 ns=15 +
    dfeather=4 +
   'factor 'progress 'noramp moorefac=1 'geotiff toler=0.0000001

list ximmos 'zeroes
gtlist ximmos

! base case for moorenbl, see second output with nibble

gen xim1 12 10 SINC=2 LINC=0 ival=1
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=2 ival=2
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,3,10,12,0) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-4,5,12,10,0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(25,3,12,10,0) datacols=(2,3,4,5,6) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(27,5,10,12,0) datacols=(2,3,4,5,6) +
      string=("./xim5") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-list xxb 'format

featherv (xim2,xim3,xim4,xim5,xxb) ximmos sl=3 ss=2 nl=30 ns=15 dfeather=4 +
   'add 'progress 'noramp moorefac=1
list ximmos 'zeroes sl=20

featherv (xim2,xim3,xim4,xim5,xxb) ximmos sl=3 ss=2 nl=30 ns=15 dfeather=4 +
   'add 'progress 'noramp moorefac=1 moorenbl=1
list ximmos 'zeroes sl=20

end-proc
$!-----------------------------------------------------------------------------
$ create devfeatherv.pdf
procedure
refgbl $echo
parm version string def="ibis-1"
parm org string def="column"
body
!let _onfail="continue"
let $echo="yes"



!   GeoTIFF case, no offsets in ibis file, now use area
!   type file different rotation, negative sl,ss and check results
!   with gtcomp for data offset within mosaic. VERY IMPORTANT TEST.

gen xim1 nl=1 ns=1
gtgen inp=xim1 out=ximmaster 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,.4,.4,0.0)", +
          "ModelTiePointTag=(0,10,0,.6,.4,0.0)", +
          "ModelTiePointTag=(10,0,0,.4,.6,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim4 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,10.,0.,11.,+
            12.,1.,13.,0., 12.,10.,13.,11.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim5 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

gtgen inp=xim4 out=xim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-2,-9,0,.3,.3,0.0)", +
          "ModelTiePointTag=(-2,1,0,.5,.3,0.0)", +
          "ModelTiePointTag=(8,-9,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gtgen inp=xim5 out=xim3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-7,-11,0,.3,.3,0.0)", +
          "ModelTiePointTag=(-7,-1,0,.5,.3,0.0)", +
          "ModelTiePointTag=(3,-11,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim2) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim3) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

list xim2 'zeroes
list xim3 'zeroes

gtlist ximmaster
gtlist xim2
gtlist xim3

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(0,0,0,0,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(0,0,0,0,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc,ximmaster) ximmos sl=-1 ss=-6 nl=12 ns=14 +
    dfeather=4 +
   'factor 'progress 'noramp moorefac=1 'geotiff toler=0.0000001

list ximmos 'zeroes

gtcopy2 ximmos ximsubmos size=(5,3,5,5)

list ximsubmos 'zeroes
gtcomp ximsubmos ximmos
gtcomp ximsubmos xim2


!   TEST SCRIPT FOR featherv, MOOREFAC=3, horizontal
!   not a pretty case since overlap is almost total


gen xim1 13 13 SINC=0 LINC=0 ival=10
gen xim2 13 13 SINC=0 LINC=0 ival=110

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,4,13,13,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,18) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes


!   TEST SCRIPT FOR featherv, MOOREFAC=3, vertical
!   not a pretty case since overlap is almost total


gen xim1 13 13 SINC=0 LINC=0 ival=10
gen xim2 13 13 SINC=0 LINC=0 ival=110

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(4,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,16) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes

! case to test two pass ramp corrections with MOOREFAC=3

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

copy xim3 xim6    ! extra file for out of strip test

list xim6 'zeroes

ibis-gen xxa nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(4,1,10,12,.8,1509,1563,1510,0,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(1,8,12,10,4.0,1563,0,1564,1509,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(9,2,12,10,6.0,1510,1564,0,0,1509) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(8,7,10,12,1.2,1564,0,0,1510,1563) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim5") strcols=(1)

ibis-gen xxf nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(11,31,10,12,1.2,1565,0,0,0,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim6") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-cat (xxb,xxf) xxa
ibis-list xxa 'format

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos sl=3 ss=2 nl=20 ns=18 dfeather=6 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'ramp moorefac=3

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos2 sl=3 ss=2 nl=20 ns=18 dfeather=6 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'ramp moorefac=1

list ximmos 'zeroes ns=16
list ximmos2 'zeroes ns=16

f2 inp=(ximmos,ximmos2) out=ximmos3 func="abs(in1-in2)"
list ximmos3 'zeroes ns=16

ibis-list xxa 'format


! DO NOT DISCARD THE NEXT CASE FROM DEVFEATHERV.PDF SINCCE IT REQUIRES
! DEBUG STATEMENTS

!   TEST SCRIPT FOR featherv, MOOREFAC=3, horizontal
!   not a pretty case since overlap is almost total
!   now using unique input lines to see if proper lines in footprint
!   have to use standard debug statement in code to see this

gen xim1 13 13 SINC=1 LINC=10 ival=10
gen xim2 13 13 SINC=1 LINC=10 ival=110

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,4,13,13,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,18) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes




!   TEST THE 255 MAX FOR BYTE CASE, NO ROLLOVER


gen xim1 13 13 SINC=1 LINC=1 ival=243
gen xim2 13 13 SINC=1 LINC=1 ival=243

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.02) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,4,13,13,1.02) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,18) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes


! CASES THAT CHECK ERROR CONDITIONS FOLLOW


!   TEST SCRIPT FOR featherv, simple small case, leave out one input
!   but input DOES touch output area and causes error condition


gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(11,7,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(9,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xxc) ximmos sl=3 ss=2 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'noramp moorefac=1

list ximmos 'zeroes


!   GeoTIFF case, offsets and nl,ns are in ibis file, toler error

gen xim1 nl=1 ns=1
gtgen inp=xim1 out=ximmaster 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,.3,.3,0.0)", +
          "ModelTiePointTag=(10,0,0,.5,.3,0.0)", +
          "ModelTiePointTag=(0,10,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim4 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim5 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

gtgen inp=xim4 out=xim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-2,-9,0,.3,.3,0.0)", +
          "ModelTiePointTag=(8,-9,0,.5,.3,0.0)", +
          "ModelTiePointTag=(-2,1,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gtgen inp=xim5 out=xim3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-7,-11,0,.30001,.30001,0.0)", +
          "ModelTiePointTag=(3,-11,0,.50001,.30001,0.0)", +
          "ModelTiePointTag=(-7,-1,0,.30001,.50001,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim2) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim3) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

list xim2 'zeroes
list xim3 'zeroes

gtlist ximmaster
gtlist xim2
gtlist xim3

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(12,8,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(10,3,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc,ximmaster) ximmos sl=3 ss=2 nl=17 ns=15 +
    dfeather=4 +
   'factor 'progress 'noramp moorefac=1 'geotiff toler=0.0000001

list ximmos 'zeroes


!   GeoTIFF case, offsets and nl,ns are in ibis file, mismatch err

gen xim1 nl=1 ns=1
gtgen inp=xim1 out=ximmaster 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,.3,.3,0.0)", +
          "ModelTiePointTag=(10,0,0,.5,.3,0.0)", +
          "ModelTiePointTag=(0,10,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim4 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim5 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

gtgen inp=xim4 out=xim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-2,-9,0,.3,.3,0.0)", +
          "ModelTiePointTag=(8,-9,0,.5,.3,0.0)", +
          "ModelTiePointTag=(-2,1,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gtgen inp=xim5 out=xim3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-7,-11,0,.3,.3,0.0)", +
          "ModelTiePointTag=(3,-11,0,.5,.3,0.0)", +
          "ModelTiePointTag=(-7,-1,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim2) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim3) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

list xim2 'zeroes
list xim3 'zeroes

gtlist ximmaster
gtlist xim2
gtlist xim3

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(12,8,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(10,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc,ximmaster) ximmos sl=3 ss=2 nl=17 ns=15 +
    dfeather=4 +
   'factor 'progress 'noramp moorefac=1 'geotiff toler=0.0000001

list ximmos 'zeroes


theend>
end-proc
$ Return
$!#############################################################################
