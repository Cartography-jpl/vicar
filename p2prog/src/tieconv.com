$!****************************************************************************
$!
$! Build proc for MIPL module tieconv
$! VPACK Version 1.9, Monday, April 01, 2002, 14:13:08
$!
$! Execute by entering:		$ @tieconv
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
$ write sys$output "*** module tieconv ***"
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
$ write sys$output "Invalid argument given to tieconv.com file -- ", primary
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
$   if F$SEARCH("tieconv.imake") .nes. ""
$   then
$      vimake tieconv
$      purge tieconv.bld
$   else
$      if F$SEARCH("tieconv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tieconv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tieconv.bld "STD"
$   else
$      @tieconv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tieconv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tieconv.com -mixed -
	-s tieconv.c -
	-i tieconv.imake -
	-p tieconv.pdf -
	-t tsttieconv.pdf devtieconv.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tieconv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
/*#include "applic.h"*/
#include <math.h>
#include <stdio.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

#define VORMAXPOLY      1000   /* MAX EDGES IN A SINGLE VORONOI POLYGON */
#define MAXLSQ          400
#define MAXLSQD         MAXLSQ+1
#define MAXLSQ2         2*MAXLSQD
#define MAXLSQ4         4*MAXLSQD
#define MAXTIE          1000000

/************************************************************************/
/* program tieconv                                                      */
/************************************************************************/
/*  83-10 ...alz... initial version, new algorithm for thiessen         */
/*                  triangulation, converted to c,                      */
/************************************************************************/

short int *vgroup;
int bggset,bgg,ccjj[VORMAXPOLY],ccgrp[VORMAXPOLY];
double cc1x[VORMAXPOLY],cc1y[VORMAXPOLY],
      cc2x[VORMAXPOLY],cc2y[VORMAXPOLY],
      ccrx[VORMAXPOLY],ccry[VORMAXPOLY];
      
typedef unsigned char  byte;



double xzprod(x1,y1,x2,y2)
   double x1,y1,x2,y2;
{
   return(x2*y1-x1*y2);
}

/* signed triangular area, for polygon use xp,yp as fixed point */
/* in (E,N) coordinates,        clockwise is positive area */
/* in (N,E) coordinates, counterclockwise is positive area */

double triarea(xp,yp,x1,y1,x2,y2)
   double xp,yp,x1,y1,x2,y2;
{
   return(0.5*(xp*y1-x1*yp+x1*y2-x2*y1+x2*yp-xp*y2));
}

void lin2(a,b,x,eps)
   double a[4],b[2],x[2],eps;
{
   /* for case where the first equation is not null */
   /* provides a solution for the singular case and causes x[1] =-1 */

   double temp;
   if (fabs(a[0])>=fabs(a[1]))
      {
      a[2] /= a[0]; b[0] /= a[0];
      a[3] -= a[2]*a[1]; b[1] -= b[0]*a[1];
      if (fabs(a[3])>eps)
	 {
	 x[1] = b[1]/a[3];
	 x[0] = b[0]-x[1]*a[2];
	 }
      else { x[1] = -1.; x[0] = b[0]+a[2]; }
      }
   else
      {
      a[3] /= a[1]; b[1] /= a[1];
      a[2] -= a[3]*a[0]; b[0] -= b[1]*a[0];
      if (fabs(a[2])>eps)
	 {
	 x[1] = b[0]/a[2];
	 x[0] = b[1]-x[1]*a[3];
	 }
      else { x[1] = -1.; x[0] = b[1]+a[3]; }
      }
   return;
}

void segxseg(x1,y1,x2,y2,x3,y3,x4,y4,w)
   double x1,y1,x2,y2,x3,y3,x4,y4,*w;
{
   /* The first segment is x1,y1 to x2,y2; the second segment is   */
   /* x3,y3 to x4,y4; the return w[0] is the index of the crossing */
   /* of the first segment; 0. is at x1,y1; 1. is at x2,y2 other   */
   /* values are linearly interpolated or extrapolated; w[1] is    */
   /* the same for the second segment                              */

   double aa[4],bb[2];

   aa[0] = x2-x1; aa[1] = y2-y1;
   aa[2] = x3-x4; aa[3] = y3-y4;
   bb[0] = x3-x1; bb[1] = y3-y1;
   lin2(aa,bb,w,1.e-8);
   return;
}


/*                                                  ALZ
   ct1, ct2, ct4, ct7, ct8, st1, st2, st4, st7, st8

   Subroutines used with tabular data set operations
   for type conversion and storing.  The unsigned char
   is for image handling only.

*/

unsigned char ct1(s) unsigned char *s; { return(*s); }
short int ct2(s) short int *s; { return(*s); }
int ct4(s) int *s; { return(*s); }
float ct7(s) float *s; { return(*s); }
double ct8(s) double *s; { return(*s); }
void st1(v,s) unsigned char v,*s; { *s = v; return; }
void st2(v,s) short int v,*s; { *s = v; return; }
void st4(v,s) int v,*s; { *s = v; return; }
void st7(v,s) float v,*s; { *s = v; return; }
void st8(v,s) double v,*s; { *s = v; return; }


void sort8(buf,ptr,n)
     double *buf;
     int *ptr,n;
{
      /* quick and dirty translation of quicksort with middle pivot
      taken from sortin.com */
      
      int l,m,k,j,iptr;
      double ibuf;
   
      if (n<2) return;
      l = n-1;
      m = n/2-1;

 l10: k = m;
      ibuf = buf[k];
      iptr = ptr[k];

 l20: j = 2*k+1;
      if (j>=n) goto l25;
      if (j<(n-1)&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l25;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l20;

 l25: buf[k] = ibuf;
      ptr[k] = iptr;
      m--;
      if (m>=0) goto l10;

 l30: k = 0;
      ibuf = buf[k];
      iptr = ptr[k];

 l40: j = 2*k+1;
      if (j>l) goto l45;
      if (j<l&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l45;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l40;

 l45: buf[k] = ibuf;
      ptr[k] = iptr;
      ibuf = buf[0];
      iptr = ptr[0];
      buf[0] = buf[l];
      ptr[0] = ptr[l];
      buf[l] = ibuf;
      ptr[l] = iptr;
      l--;
      if (l>0) goto l30;

      return;
}

void sort88(buf,ptr,n)
     double *buf;
     int *ptr,n;
{
      /* quick and dirty translation of quicksort with middle pivot
      taken from sortin.com; sorts a vector (x,y,x,y,x,y...) on x
      then on y */
      
      int l,m,k,j,iptr;
      double ibufx,ibufy;
   
      if (n<2) return;
      l = n-1;
      m = n/2-1;

 l10: k = m;
      ibufx = buf[2*k];
      ibufy = buf[2*k+1];
      iptr = ptr[k];

 l20: j = 2*k+1;
      if (j>=n) goto l25;
      if (j<(n-1)&&((buf[2*j+2]>buf[2*j])||(
         (buf[2*j+2]==buf[2*j])&&(buf[2*j+3]>buf[2*j+1]) ))) j++;
      if ((buf[2*j]<ibufx)||(
         (buf[2*j]==ibufx)&&(buf[2*j+1]<=ibufy) )) goto l25;
      buf[2*k] = buf[2*j];
      buf[2*k+1] = buf[2*j+1];
      ptr[k] = ptr[j];
      k = j;
      goto l20;

 l25: buf[2*k] = ibufx;
      buf[2*k+1] = ibufy;
      ptr[k] = iptr;
      m--;
      if (m>=0) goto l10;

 l30: k = 0;
      ibufx = buf[2*k];
      ibufy = buf[2*k+1];
      iptr = ptr[k];

 l40: j = 2*k+1;
      if (j>l) goto l45;
      if (j<l&&((buf[2*j+2]>buf[2*j])||(
         (buf[2*j+2]==buf[2*j])&&(buf[2*j+3]>buf[2*j+1]) ))) j++;
      if ((buf[2*j]<ibufx)||(
         (buf[2*j]==ibufx)&&(buf[2*j+1]<=ibufy) )) goto l45;
      buf[2*k] = buf[2*j];
      buf[2*k+1] = buf[2*j+1];
      ptr[k] = ptr[j];
      k = j;
      goto l40;

 l45: buf[2*k] = ibufx;
      buf[2*k+1] = ibufy;
      ptr[k] = iptr;
      ibufx = buf[0];
      ibufy = buf[1];
      iptr = ptr[0];
      buf[0] = buf[2*l];
      buf[1] = buf[2*l+1];
      ptr[0] = ptr[l];
      buf[2*l] = ibufx;
      buf[2*l+1] = ibufy;
      ptr[l] = iptr;
      l--;
      if (l>0) goto l30;

      return;
}

void sortrec4(key,ptr,len)
   int *key,*ptr,len;
{
   int i,*temp;
   
   if (len<2) return;
   if ((temp=(int *)malloc(4*len))==NULL)
          zmabend("malloc failed");
   for (i=0;i<len;i++) temp[i] = key[i];
   for (i=0;i<len;i++) key[i] = temp[ptr[i]-1];
   free(temp);
   return;
}

void sortrec88(key,ptr,len)
   int *ptr,len;
   double *key;
{
   int i;
   double *temp;
   
   if (len<2) return;
   if ((temp=(double *)malloc(16*len))==NULL)
          zmabend("malloc failed");
   for (i=0;i<len;i++) 
      {
      temp[i*2] = key[i*2];
      temp[i*2+1] = key[i*2+1];
      }
   for (i=0;i<len;i++)
      {
      key[i*2] = temp[ptr[i]*2-2];
      key[i*2+1] = temp[ptr[i]*2-1];
      }
   free(temp);
   return;
}

/*=====================================================================
lsqfit

lsqfit solves for the minimum least squares fit (for ax=r, the minimum
over all x of L2 norm of r-ax)

The matrix a is stored by column order

arguments:

     1. a: input and output, double *a;
	m by n coefficient matrix, destroyed.
     2. r: input and output, double *r;
	input right hand m-vector.
     3. m: input, int m;
	number of linear equations.
     4. n: input, int n;
	number of independent coords; dimension of x.
     5. x: output, double *x;
	solution vector.
     6. eps: input double eps;
	gaussian pivot tolerance (usually set to 1.e-14)
     7. ierror: output int *ierror;
	result 0=OK; K=singular at kth column
	-1=zero matrix a; -2=m<n

*/

void lsqfit(a,r,m,n,x,eps,ierror)
      double *a,*r,*x,eps;
      int m,n,*ierror;
{
   double *buf; int *ipiv;
   int i,j,k,il,iu,kpiv,id,jl,ii,kl;
   double piv,h,sig,tol,beta;

   if ((buf=(double *)malloc(16*n))==NULL) zmabend("malloc failed");
   if ((ipiv=(int *)malloc(4*n))==NULL) zmabend("malloc failed");

   if (m<n) { *ierror = -2; return; }
   piv = 0.;
   iu = 0;
   for (k=1;k<=n;k++)
      {
      ipiv[k-1] = k;
      h = 0.;
      il = iu+1;
      iu = iu+m;
      for (i=il;i<=iu;i++) h = h+a[i-1]*a[i-1];
      buf[k-1] = h;
      if (h<=piv) continue;
      piv = h;
      kpiv = k;
      }
   if (piv<=0.) { *ierror = -1; return; }
   sig = sqrt(piv);
   tol = sig*fabs(eps);
   
   il = -m;
   for (k=1;k<=n;k++)
      {
      il = il+m+1;
      iu = il+m-k;
      i = kpiv-k;
      if (i>0)
	 {
	 h = buf[k-1];
	 buf[k-1] = buf[kpiv-1];
	 buf[kpiv-1] = h;
	 id = i*m;
	 for (i=il;i<=iu;i++)
	    {
	    j = i+id;
	    h = a[i-1];
	    a[i-1] = a[j-1];
	    a[j-1] = h;
	    }
	 }
      if (k>1)
	 {
	 sig = 0.;
	 for (i=il;i<=iu;i++) sig = sig+a[i-1]*a[i-1];
	 sig = sqrt((double)sig);
	 if (sig<tol) { *ierror = k-1; return; }
	 }
      h = a[il-1];
      if (h<0.) sig = -sig;
      ipiv[kpiv-1] = ipiv[k-1];
      ipiv[k-1] = kpiv;
      beta = h+sig;
      a[il-1] = beta;
      beta = 1./(sig*beta);
      j = n+k;
      buf[j-1] = -sig;
      if (k<n)
	 {
	 piv = 0.;
	 id = 0;
	 jl = k+1;
	 kpiv = jl;
	 for (j=jl;j<=n;j++)
	    {
	    id = id+m;
	    h = 0.;
	    for (i=il;i<=iu;i++)
	       {
	       ii = i+id;
	       h = h+a[i-1]*a[ii-1];
	       }
	    h = beta*h;
	    for (i=il;i<=iu;i++)
	       {
	       ii = i+id;
	       a[ii-1] = a[ii-1]-a[i-1]*h;
	       }
	    ii = il+id;
	    h = buf[j-1]-a[ii-1]*a[ii-1];
	    buf[j-1] = h;
	    if (h<=piv) continue;
	    piv = h;
	    kpiv = j;
	    }
	 }
      h = 0.;
      ii = il;
      for (i=k;i<=m;i++)
	 {
	 h = h+a[ii-1]*r[i-1];
	 ii = ii+1;
	 }
      h = beta*h;
      ii = il;
      for (i=k;i<=m;i++)
	 {
	 r[i-1] = r[i-1]-a[ii-1]*h;
	 ii = ii+1;
	 }
      }

   *ierror = 0;
   piv = 1./buf[2*n-1];
   x[n-1] = piv*r[n-1];
   if (n>1)
      {
      jl = (n-1)*m+n;
      for (j=2;j<=n;j++)
	 {
	 jl = jl-m-1;
	 k = n+n+1-j;
	 piv = 1./buf[k-1];
	 kl = k-n;
	 id = ipiv[kl-1]-kl;
	 il = 2-j;
	 h = r[kl-1];
	 il = il+n;
	 iu = il+j-2;
	 ii = jl;
	 for (i=il;i<=iu;i++)
	    {
	    ii = ii+m;
	    h = h-a[ii-1]*x[i-1];
	    }
	 i = il-1;
	 ii = i+id;
	 x[i-1] = x[ii-1];
	 x[ii-1] = piv*h;
	 }
      }
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


/*=========================================================

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


void insert_seg(jj,ccount,p4max,xbig,ybig,xjbig,yjbig,wchcall)
   int jj,*ccount,wchcall;
   double *p4max,xbig,ybig,xjbig,yjbig;
{
   int ccdel[VORMAXPOLY];
   int i,k,tcross,tdelete,srong,niter,iter,kmin,kc,kcmin,whch,irep;
   int ptr,z12,lcross,sccx[10],srl[10];
   double xm,ym,xbisv,ybisv,xbis,ybis,tw[2],xzprod();
   double xcr,ycr,rcirc,r2big,ccrad,pmax,z2min,rat;
   double z1,z2,sx[10],sy[10],dmin,px,py,dot,hyp,fdet,dist,thet,dx,dy;
   
   /*if(bgg)printf("enter insert_seg jj %d\n",jj);*/
   xm = (xbig+xjbig)*.5;
   ym = (ybig+yjbig)*.5;
   xbisv = ym-yjbig;
   ybisv = xjbig-xm;
   xbis = xbisv+xm; ybis = ybisv+ym;
   /*if(bgg)printf("xm,ym %f %f\n",xm,ym);
   if(bgg)printf("xbis,ybis %f %f  ",xbis,ybis);
   if(bgg)printf("xbisv,ybisv %f %f\n",xbisv,ybisv);*/
   tcross = 0; tdelete = 0;
   for (k=0;k<*ccount;k++)
      {
      segxseg(xm,ym,xbis,ybis,cc1x[k],cc1y[k],cc2x[k],cc2y[k],tw);
      /*if(bgg)printf("tw %f %f\n",tw[0],tw[1]);*/
      if (tw[1]>=0. && tw[1]<=1.) lcross = 1; else lcross = 0.;
      xcr = xbisv*tw[0]+xm;
      ycr = ybisv*tw[0]+ym;
      /*if(bgg)printf("xcr,ycr %f %f\n",xcr,ycr);*/
      z1 = xzprod(cc1x[k]-xm,cc1y[k]-ym,xbisv,ybisv);
      z2 = xzprod(cc2x[k]-xm,cc2y[k]-ym,xbisv,ybisv);
      /*if(bgg)printf("z1,z2 %f %f\n",z1,z2);*/
      srong = 0; z12 = 1;
      if (z1<0.) srong++;
      if (z2<0.) { srong++; z12 = 2; }
      /*if(bgg)printf("a-srong,lcross %d %d\n",srong,lcross);*/
      if (lcross==0 && srong==1) srong = 0;
      if (lcross==1 && srong!=1)
	 {
	 if (fabs(z1)<fabs(z2)) z12 = 1+srong/2; else z12 = 2-srong/2;
	 srong = 1;
	 /*if(bgg)printf("end point enforced\n");*/
	 }
      if (lcross==1)
	 {
	 sx[tcross] = xcr;
	 sy[tcross] = ycr;
	 sccx[tcross] = k;
	 srl[tcross] = z12; tcross++;
	 }
      ccdel[k] = srong;
      if (srong==2) tdelete++;
      /*if(bgg)printf("b-srong,lcross %d %d\n",srong,lcross);*/
      }
   /*if(bgg)printf("tcross,tdelete %d %d\n",tcross,tdelete);*/
   if (tcross<2 && tdelete==0) return;
   if (tcross<2)
      {
      /*if(bgg)printf("add polygon cross\n");*/
      niter = 2-tcross;
      for (iter=0;iter<niter;iter++)
	 {
	 dmin = 1.e35;
	 for (k=0;k<*ccount;k++)
	    {
	    if (ccdel[k]==1) continue;
	    px = cc1x[k]-xbis;
	    py = cc1y[k]-ybis;
	    dist = fabs(px)+fabs(py);
	    if (dist>1.e-7)
	       {
	       z2 = xzprod(cc2x[k]-px,cc2y[k]-py,xbisv,ybisv);
	       dot = px*xbisv+py*ybisv;
	       hyp = sqrt((double)(px*px+py*py))*
		     sqrt((double)(xbisv*xbisv+ybisv*ybisv));
	       rat = dot/hyp; if (rat>1.) rat = 1.; if (rat<(-1.)) rat = -1.;
	       thet = acos((double)rat);
	       dist = hyp*sin((double)thet);
	       }
	    if (dist>dmin) continue;
	    dmin = dist; kmin = k; z2min = z2;
	    }
	 ccdel[kmin] = 1;
	 sx[tcross] = cc1x[kmin];
	 sy[tcross] = cc1y[kmin];
	 sccx[tcross] = kmin;
	 if (z2min<0) srl[tcross] = 2; else srl[tcross] = 1;
	 tcross++;
	 }
      }
   if (tcross>2)
      {
      /*if(bgg)printf("elim polygon cross\n");*/
      niter = tcross-2;
      for (iter=0;iter<niter;iter++)
	 {
	 dmin = 1.e35;
	 for (kc=0;kc<tcross;kc++)
	    {
	    k = sccx[kc];
	    whch = srl[kc];
	    if (whch==1)
	       { px = cc1x[k]-xbis; py = cc1y[k]-ybis; }
	    else
	       { px = cc2x[k]-xbis; py = cc2y[k]-ybis; }
	    dist = fabs(px)+fabs(py);
	    if (dist>1.e-7)
	       {
	       dot = px*xbisv+py*ybisv;
	       hyp = sqrt((double)(px*px+py*py))*
		     sqrt((double)(xbisv*xbisv+ybisv*ybisv));
	       rat = dot/hyp; if (rat>1.) rat = 1.; if (rat<(-1.)) rat = -1.;
	       thet = acos((double)rat);
	       dist = hyp*sin((double)thet);
	       }
	    if (dist>dmin) continue;
	    dmin = dist; kcmin = kc; kmin = k;
	    }
	 ccdel[kmin] = 0;
	 for (kc=kcmin;kc<tcross;kc++)
	    {
	    sx[kc] = sx[kc+1];
	    sy[kc] = sy[kc+1];
	    sccx[kc] = sccx[kc+1];
	    srl[kc] = srl[kc+1];
	    }
	 tcross--;
	 }
      }
   if (tcross!=2) zmabend("tcross.ne.2");
   if (sx[0]==sx[1] && sy[0]==sy[1]) return;
   for (irep=0;irep<2;irep++)
      {
      whch = srl[irep];
      ptr = sccx[irep];
      if (whch==1)
	 {
	 cc1x[ptr] = sx[irep];
	 cc1y[ptr] = sy[irep];
	 }
      else
	 {
	 cc2x[ptr] = sx[irep];
	 cc2y[ptr] = sy[irep];
	 }
      }
   ptr = 0;
   for (k=0;k<*ccount;k++)
      {
      cc1x[ptr] = cc1x[k]; cc1y[ptr] = cc1y[k];
      cc2x[ptr] = cc2x[k]; cc2y[ptr] = cc2y[k];
      ccdel[ptr] = ccdel[k];
      switch(wchcall)
	 {
	 case 3: ccrx[ptr] = ccrx[k]; ccry[ptr] = ccry[k];
		 ccgrp[ptr] = ccgrp[k];
	 case 2: ccjj[ptr] = ccjj[k]; break;
	 /*case 1: break;*/
	 }
      if (ccdel[k]<2) ptr++;
      }
   z1 = xzprod(sx[0]-xbig,sy[0]-ybig,sx[1]-xbig,sy[1]-ybig);
   if (z1<0) whch = 0; else whch = 1;
   cc1x[ptr] = sx[whch]; cc1y[ptr] = sy[whch];
   cc2x[ptr] = sx[1-whch]; cc2y[ptr] = sy[1-whch];
   switch(wchcall)
      {
      case 3: ccrx[ptr] = xjbig; ccry[ptr] = yjbig; ccgrp[ptr] = vgroup[jj];
      case 2: ccjj[ptr] = jj; break;
      /*case 1: break;*/
      }
   *ccount = ptr+1;
   pmax = 0.;
   for (i=0;i<*ccount;i++)
      {
      dx = cc1x[i]-xbig; dy = cc1y[i]-ybig;
      ccrad = dx*dx+dy*dy;
      if (ccrad>pmax) pmax = ccrad;
      }
   *p4max = 4.*pmax;
   return;
}

void thiessen(npoints,nlinret,reject,skinny,abendi,ptx,pty,ntriang,
			   tcon1,tcon2,tcon3)
   int npoints,*nlinret,abendi,*ntriang,**tcon1,**tcon2,**tcon3;
   double reject,skinny,*ptx,*pty;
{

   int *bptr,**hash1,**hash2,**tc,bjj[VORMAXPOLY],blink[VORMAXPOLY];
   double *bufio,*buf;

   int labsiz,lnl,lns,nolab,utag,vtype,np,i,j,npts,linct,ip1,hlen1,hlen2;
   int ipoly,topstop,botstop,dirj,jj,k,ptr,jsave,jdup,trict,ibig;
   int t0,t1,t2,t3,tt3,tt,h12,h123,l,triptr,ttptr,ccount,temp;
   double dx,dy,dx2,dist2,xlink,ylink,low1,low2,upp1,upp2,tstarea,diam;
   double tw[2],p4max,xbig,ybig,xjbig,yjbig;
   double triarea();

   char lab[90];
   bgg = 0; bggset = -1;
   
   
   /* read the data */

   np = npoints*2;
   mz_alloc1((unsigned char **)&bufio,np,8);
   mz_alloc1((unsigned char **)&buf,np,8);
   mz_alloc1((unsigned char **)&bptr,npoints,4);
   for (i=0;i<10000;i++)
      {
      hlen1 = (npoints*4+1000)+i;
      if (hlen1%2==0) continue;
      for (j=3;j<=37;j+=2)
	 {
	 if (hlen1%j==0) break;
	 if (j==37) goto rnd1;
	 }
      }
   rnd1: /*if(bgg)printf("hash length = %d\n",hlen1);*/
   mz_alloc2((unsigned char ***)&hash1,4,hlen1,4);
   for (i=0;i<hlen1;i++) hash1[0][i] = 0;

   low1 = 1.e20; low2 = 1.e20; upp1 = -1.e20; upp2 = -1.e20;
   for (i=0;i<npoints;i++)
      {
      bufio[2*i] = ptx[i];
      bufio[2*i+1] = pty[i];
      low1 = MIN(low1,ptx[i]);
      low2 = MIN(low2,pty[i]);
      upp1 = MAX(upp1,ptx[i]);
      upp2 = MAX(upp2,pty[i]);
      }
   diam = 5*((upp1-low1)+(upp2-low2));
   low1 = low1-diam;
   low2 = low2-diam;
   upp1 = upp1+diam;
   upp2 = upp2+diam;

   /* apply the voronoi routine */

   npts = np/2; ccount = 0; linct = 0; trict = 0;
   for (i=0;i<np;i++) buf[i] = bufio[i];
   for (i=0;i<npts;i++) bptr[i] = i+1;
   
   sort88(buf,bptr,npts);
   sortrec88(bufio,bptr,npts);
   
   ptr = 0;
   for (i=0;i<npts;i++)
      {
      if (bufio[i*2]<low1 || bufio[i*2]>upp1 ||
	 bufio[i*2+1]<low2 || bufio[i*2+1]>upp2)
	 { printf("outside point removed\n"); continue; }
      if (i!=0) if (bufio[i*2]==buf[ptr*2-2] &&
	 bufio[i*2+1]==buf[ptr*2-1])
	    {
	    if (abendi) zmabend("duplicate point abend");
	    printf("duplicate point rejected\n");
	    continue;
	    }
       for (j=1;ptr-j>=0;j++)
	 {
	 if (fabs(bufio[i*2]-buf[(ptr-j)*2])>reject) break;
	 if ((fabs(bufio[i*2]-buf[(ptr-j)*2])+
	    fabs(bufio[i*2+1]-buf[(ptr-j)*2+1]))<reject)
	    {
	    printf("i,j,ptr %d %d %d\n",i,j,ptr);
	    printf("bufio[i*2],buf[(ptr-j)*2] %12.3f %12.3f\n",
	                 bufio[i*2],buf[(ptr-j)*2]);
	    printf("bufio[i*2+1],buf[(ptr-j)*2+1] %12.3f %12.3f\n",
	                 bufio[i*2+1],buf[(ptr-j)*2+1]);
	                 
	    if (abendi) zmabend("close point abend");
	    printf("close point rejected\n");
	    goto clpt;
	    }
	 }
      buf[ptr*2] = bufio[i*2];
      buf[ptr*2+1] = bufio[i*2+1]; bptr[ptr++] = bptr[i];
      clpt: continue;
      }
   free(bufio);
   if (ptr==0) return; npts = ptr;
   for (ibig=0;ibig<npts;ibig++)
      {
      if (ibig%10000==9999) printf("%d pts processed\n",ibig);
      xbig = buf[ibig*2];
      ybig = buf[ibig*2+1];
      /*if (bggset>=0) printf("xbig,ybig %f %f\n",xbig,ybig);*/
      if (ibig==bggset) bgg = 1; else bgg = 0;
      cc1x[0] = upp1; cc1y[0] = upp2; cc2x[0] = low1; cc2y[0] = upp2;
      cc1x[1] = upp1; cc1y[1] = low2; cc2x[1] = upp1; cc2y[1] = upp2;
      cc1x[2] = low1; cc1y[2] = low2; cc2x[2] = upp1; cc2y[2] = low2;
      cc1x[3] = low1; cc1y[3] = upp2; cc2x[3] = low1; cc2y[3] = low2;
      ccjj[0] = ibig; ccjj[1] = ibig;
      ccjj[2] = ibig; ccjj[3] = ibig;
      p4max = upp1+upp2-low1-low2; p4max = p4max*p4max; ccount = 4;
      topstop = 0; botstop = 0;
      for (j=0;j<2*npts;j++)
	 {
	 dirj = j%2;
	 jj = ibig+(dirj*2-1)*((j+2)/2);
	 /*if(bgg)printf("a-ibig,j,jj %d %d %d\n",ibig,j,jj);
	 if(bgg)printf("dirj,tops,bots %d %d %d\n",dirj,topstop,botstop);*/
	 if (jj<0 || jj>=npts) continue;
	 if (dirj==0 && topstop) continue;
	 if (dirj==1 && botstop) continue;
	 xjbig = buf[jj*2];
	 yjbig = buf[jj*2+1];
	 dx = xjbig-xbig; dy = yjbig-ybig; dx2 = dx*dx;
	 dist2 = dx2+dy*dy;
	 /*if(bgg)printf("xjbig,yjbig,dist2 %f %f %f\n",xjbig,yjbig,dist2);*/
	 if (dist2<p4max)
	    insert_seg(jj,&ccount,&p4max,xbig,ybig,xjbig,yjbig,2);
	 /*if(bgg)printf("set-stop %f %f %f\n",xbig,xjbig,p4max);*/
	 if (dx2>p4max)
	    { if (dirj==0) topstop = 1; else botstop = 1; }
	 if (topstop&&botstop) break;
	 }

/* output the polygon in chain order, zero length edges are kept
   but can be thinned by user proc */

      for (i=0;i<ccount;i++) blink[i] = 0;
      ptr = 0;
      xlink = cc2x[0]; ylink = cc2y[0]; jsave = 0;
      for (i=0;i<ccount;i++)
	 {
	 jdup = 0;
	 for (j=0;j<ccount;j++)
	    {
	    if (blink[j]) continue;
	    if (cc1x[j]!=xlink || cc1y[j]!=ylink) continue;
	    if (j==jsave) continue;
	    jdup++;
	    if (jdup>1&&cc1x[jsave]==cc2x[jsave]&&
			cc1y[jsave]==cc2y[jsave]) break;
	    jsave = j;
	    }
	 if (ccjj[jsave]!=ibig) bjj[ptr++] = ccjj[jsave];
	 xlink = cc2x[jsave]; ylink = cc2y[jsave];
	 blink[jsave] = 1;
	 }
      for (i=0;i<ptr;i++)
	 {
	 if (i==(ptr-1)) ip1 = 0; else ip1 = i+1;
	 if (bjj[i]!=ibig && bjj[ip1]!=ibig)
	    {
	    tstarea = triarea((double)(buf[ibig*2]),(double)(buf[ibig*2+1]),
		       (double)(buf[bjj[i]*2]),(double)(buf[bjj[i]*2+1]),
		       (double)(buf[bjj[ip1]*2]),(double)(buf[bjj[ip1]*2+1]));
	    if (tstarea<skinny) continue;
	    /*if (bgg) printf("***************saved\n");*/
	    t1 = bptr[ibig];
	    t2 = bptr[bjj[i]];
	    t3 = bptr[bjj[ip1]];
	    if (t1>t2) { tt=t1; t1=t2; t2=tt; }
	    if (t1>t3) { tt=t1; t1=t3; t3=tt; }
	    if (t2>t3) { tt=t2; t2=t3; t3=tt; }
	    h123 = (t1*7+t2*330+t3*4199)%hlen1-1;
	    for (j=0;j<hlen1;j++)
	       {
	       h123++; if (h123>=hlen1) h123 = 0;
	       if (hash1[0][h123]==0) goto stor1;
	       if (hash1[1][h123]!=t1) continue;
	       if (hash1[2][h123]!=t2) continue;
	       if (hash1[3][h123]!=t3) continue;
	       hash1[0][h123]++; goto done1;
	       }
	    printf("debug:trict %d\n",trict);
	    /*mifcb2 = mi_fopen("hash1dbg","w");
	    lab[0] = '\0';
	    mi_labwr(mifcb2,lab,0,0,"graphics","polytr");
	    mg_put(mifcb2,1,4,hlen1,hash1[0]);
	    mg_put(mifcb2,2,4,hlen1,hash1[1]);
	    mg_put(mifcb2,3,4,hlen1,hash1[2]);
	    mg_put(mifcb2,4,4,hlen1,hash1[3]);
	    fclose(mifcb2);
	    zmabend("hash error 1");*/
	    stor1: hash1[1][h123] = t1;
		   hash1[2][h123] = t2;
		   hash1[3][h123] = t3;
		   hash1[0][h123] = 1;
		   /*if (bgg)
		      {
		      printf("###########saving h123 %d\n",h123);
		      printf("ibig,ptr,i %d %d %d\n",ibig,ptr,i);
		      for (k=0;k<ptr;k++)
			 printf("k,bjj[k],bptr[bjj[k]] %d %d %d\n",
				   k,bjj[k],bptr[bjj[k]]);
		      }*/
		   trict++;
		   continue;
	    done1: continue;
	    }
	 }
      }

   /* condense the triangles, then remove intersecting triangles that
       result from perfect grid squares, prefer higher count duplicated
       triangles; uses a hash table of pairs of points from triangles */

   free(buf); free(bptr);
   mz_alloc2((unsigned char ***)&tc,4,trict,4);
   triptr = 0;

   for (i=3;i>0;i--)
      {
      for (j=0;j<hlen1;j++)
	 {
	 if (hash1[0][j]<i) continue;
	 tc[0][triptr] = hash1[0][j];
	 tc[1][triptr] = hash1[1][j];
	 tc[2][triptr] = hash1[2][j];
	 tc[3][triptr++] = hash1[3][j];
	 hash1[0][j] = 0;
	 }
      }
   mz_free2((unsigned char **)hash1,4);
   if (triptr!=trict) zmabend("hash error 2");
   for (i=0;i<10000;i++)
      {
      hlen2 = (trict*3+1000)+i;
      if (hlen2%2==0) continue;
      for (j=3;j<=37;j+=2)
	 {
	 if (hlen2%j==0) break;
	 if (j==37) goto rnd2;
	 }
      }
   rnd2: /*if(bgg)printf("hash length = %d\n",hlen2);*/
   mz_alloc2((unsigned char ***)&hash2,3,hlen2,4);
   for (i=0;i<hlen2;i++) hash2[0][i] = -1;
   for (i=0;i<trict;i++)
      {
      if (i%10000==9999) printf("%d triangles processed\n",i);
      t0 = tc[0][i];
      for(j=0;j<3;j++)
	 {
	 t1 = tc[j/2+1][i];
	 t2 = tc[(j+8)/3][i];
	 t3 = tc[3-j][i];
	 h12 = (t1*7+t2*330)%hlen2-1;
	 for (k=0;k<hlen2;k++)
	    {
	    h12++; if (h12>=hlen2) h12 = 0;
	    if (hash2[0][h12]==(-1)) goto stor2;
	    if (hash2[1][h12]!=t1) continue;
	    if (hash2[2][h12]!=t2) continue;
	    /* may enter more than once */
	    ttptr = hash2[0][h12];
	    if (tc[0][ttptr]==(-99)) continue;
	    for (l=1;l<4;l++)
	       {
	       tt3 = tc[l][ttptr];
	       if (tt3!=t1&&tt3!=t2) break;
	       }
	    segxseg(ptx[t1-1],pty[t1-1],ptx[t3-1],pty[t3-1],
		    ptx[t2-1],pty[t2-1],ptx[tt3-1],pty[tt3-1],tw);
	    if(tw[0]>=0.01&&tw[0]<=0.99&&tw[1]>=0.01&&tw[1]<=0.99)goto done2;
	    segxseg(ptx[t2-1],pty[t2-1],ptx[t3-1],pty[t3-1],
		    ptx[t1-1],pty[t1-1],ptx[tt3-1],pty[tt3-1],tw);
	    if(tw[0]>=0.01&&tw[0]<=0.99&&tw[1]>=0.01&&tw[1]<=0.99)goto done2;
	    }
	 zmabend("hash error 3");
	 stor2: hash2[0][h12] = i;
		hash2[1][h12] = t1;
		hash2[2][h12] = t2;
		continue;
	 done2: tc[0][i] = -99;
		break;
	 }
      }

   mz_free2((unsigned char **)hash2,3);
   mz_alloc1((unsigned char **)tcon1,triptr,4);
   mz_alloc1((unsigned char **)tcon2,triptr,4);
   mz_alloc1((unsigned char **)tcon3,triptr,4);
   trict = 0;
   for (i=0;i<triptr;i++)
      {
      if (tc[0][i]==(-99)) continue;
      (*tcon1)[trict] = tc[1][i];
      (*tcon2)[trict] = tc[2][i];
      (*tcon3)[trict++] = tc[3][i];
      }

   /* now sort the triangles in vertical order, as required by tiegrid */

   mz_free2((unsigned char **)tc,4);
   mz_alloc1((unsigned char **)&buf,trict,8);
   mz_alloc1((unsigned char **)&bptr,trict,4);
   for (i=0;i<trict;i++)
      {
      buf[i] = ptx[(*tcon1)[i]-1]+ptx[(*tcon2)[i]-1]+ptx[(*tcon3)[i]-1];
      bptr[i] = i+1;
      }
   sort8(buf,bptr,trict);
   sortrec4(*tcon1,bptr,trict);
   sortrec4(*tcon2,bptr,trict);
   sortrec4(*tcon3,bptr,trict);

   /* triangles also have to be clockwise */

   for (i=0;i<trict;i++)
      {
      tstarea = triarea((double)(ptx[(*tcon1)[i]-1]),(double)(pty[(*tcon1)[i]-1]),
		 (double)(ptx[(*tcon2)[i]-1]),(double)(pty[(*tcon2)[i]-1]),
		 (double)(ptx[(*tcon3)[i]-1]),(double)(pty[(*tcon3)[i]-1]));
      if (tstarea>0.0) continue;
      temp = (*tcon3)[i]; (*tcon3)[i] = (*tcon1)[i]; (*tcon1)[i] = temp;
      }

   *ntriang = trict;             /* can check this with Euler 2P-2-CVXHULL*/
   *nlinret = npoints+trict-1;   /* using Euler */
   printf("%d points %d lines %d triangles\n",npoints,*nlinret,trict);
   free(buf); free(bptr);
   return;
}


int insidetri(x,y,x1,y1,x2,y2,x3,y3)
   double x,y,x1,y1,x2,y2,x3,y3;
{
   if ((x-x1)*(y2-y1)-(y-y1)*(x2-x1)>0.) return(0);
   if ((x-x2)*(y3-y2)-(y-y2)*(x3-x2)>0.) return(0);
   if ((x-x3)*(y1-y3)-(y-y3)*(x1-x3)>0.) return(0);
   return(1);
}

main44()
{
   double *rpar;
   int *con1,*con2,*con3;
   
   double tmaxx,tmaxy,tminx,tminy,x,y,xx,yy,*ptx,*pty;
   double clsq[MAXLSQ4], clsqxy[MAXLSQ2], elsqxy[MAXLSQ2];
   double csol[8],*optx,*opty;
   double tab[4],work[16];
   double **coeff,dx,dy,**vout;
   double minx,miny,maxx,maxy;
   float **pout,*rout;
   int cols[4];
   int zgeom,mgeom,abendl,lgeom,inside,geomv,keystone,linear; /*booleans*/
   int plot,noprint, xvptst;
   int found;
   int nah = 30,nav = 30,npoint = 4,nrank = 6,nerr = 0;
   int geoma = 1,lgeomlike = 0;
   char outnam[73];
   
   int i,j,k,i1,n,ier,inpcnt,status,unit,colcount,ibis,clen,ptr,record;
   int irow,ntiepp,icnt,idef,nvt,nklsq,nklsq2,nlret,ntri,nah1,nav1;
   int ttri,ttrj1,minldf,maxldf,minsdf,maxsdf,coldef,tiepdef,nht,tiept;
   int ix,itri,tri,isign,wunit,nl,rec1,p,ibisOut,parmOut,ptr2,nout,outdf;
   int nklsq3;
   double eps,skinny,reject;
        
   zifmessage("tieconv version 28-mar-02");
   
   noprint = zvptst("NOPRINT");
   keystone = zvptst("KEYSTONE");
   linear = zvptst("LINEAR");
   
   /*	if inp file specified then read in tiepoints from
	  the ibis interface file */

   status = zvpcnt("inp",&inpcnt);
   if (inpcnt>0)
      {
      status = zvunit(&unit,"inp",1,0);
     
      zvparm("cols",cols,&colcount,&coldef,4,0);
      status = IBISFileOpen(unit,&ibis,"read",0,0,0,0);
      if (status!=1) IBISSignalU(unit,status,1);
      IBISFileGet(ibis,"nr",&clen,1,1);
      mz_alloc1((unsigned char **)&rpar,colcount*clen,8);
      ptr = 0;
      status = IBISRecordOpen(ibis,&record,0,cols,colcount,IFMT_DOUB);
      if (status!=1) IBISSignal(ibis,status,1);
      for (irow=1;irow<=clen;irow++)
	 {
         status = IBISRecordRead(record,&rpar[ptr],irow);
         if (status!=1) IBISSignal(ibis,status,1);
	 ptr += colcount;
	 }
      status = IBISFileClose(ibis,0);
      if (status!=1) IBISSignal(ibis,status,1);
      ntiepp = ptr;
      }
   else
      zvparmd("tiepoint",&rpar,&ntiepp,&tiepdef,1625,0);
   
   zvparm("nah",&nht,&icnt,&idef,1,0);
   zvparm("nav",&nvt,&icnt,&idef,1,0);


   abendl = zvptst("abend");
   reject = 0.01;
   zvparmd("reject",&reject,&icnt,&idef,1,0);
   reject = reject*reject;
   geoma = zvptst("geoma");
   geomv = zvptst("geomv");
   mgeom = zvptst("mgeom");
   lgeom = zvptst("lgeom");
   lgeomlike = mgeom||lgeom||geomv;
   zgeom = zvptst("geomz");
   plot = zvptst("plot");
   if (plot&&(keystone||linear))
      zmabend("can't plot with linear or keystone options");
   
   if (zgeom) npoint = 3;
   if (zgeom) nrank = 3;
   
   n = ntiepp/npoint;
   if (n<3) zmabend("need 3 tiepoints");
   if (n<4&&keystone) zmabend("need 4 tiepoints for keystone option");
   if (n>MAXTIE) zmabend("maximum input tiepoints exceeded");
   if (lgeomlike)
      {
      for (i=0;i<ntiepp;i+=4)
         {
         dx = rpar[i];
         rpar[i] = rpar[i+2];
         rpar[i+2] = dx;
         dx = rpar[i+1];
         rpar[i+1] = rpar[i+3];
         rpar[i+3] = dx;
         }
      }
 
   tmaxx = 0.0;
   tmaxy = 0.0;
   tminx = 1.e20;
   tminy = 1.e20;
   if (n>MAXLSQ) nklsq = MAXLSQ; else nklsq = n;
   nklsq2 = nklsq*2; nklsq3 = nklsq*3;
   
   mz_alloc1((unsigned char **)&ptx,n+4,8);
   mz_alloc1((unsigned char **)&pty,n+4,8);
   mz_alloc1((unsigned char **)&optx,n+4,8);
   if (!zgeom) mz_alloc1((unsigned char **)&opty,n+4,8);
   
   if (zgeom) ptr = 1; else ptr = 2;
   for (i=0;i<n;i++)
      {
      ptx[i] = rpar[ptr];
      pty[i] = rpar[ptr+1];
      if (!zgeom) {
        optx[i] = rpar[ptr-2];
        opty[i] = rpar[ptr-1];
      }
      else optx[i] = rpar[ptr-1];
      if (ptx[i]>tmaxx) tmaxx = ptx[i];
      if (ptx[i]<tminx) tminx = ptx[i];
      if (pty[i]>tmaxy) tmaxy = pty[i];
      if (pty[i]<tminy) tminy = pty[i];
      ptr += npoint;
      }
   free(rpar);
   
   /* for the large case, the random formula scatters the points to be
   fitted across the area extended; will get duplicates but that is not
   a problem with a sample of 400; the sequence repeats for the the j 
   loop */
   
   for (j=0;j<npoint-2;j++)
      {
      k = 0;
      for (i=0;i<nklsq;i++)
         {
         if (n>((3*MAXLSQ)/2)) k = (k*379+i*i)%n; else k = i;
         clsq[i] = ptx[k];
         clsq[i+nklsq] = pty[k];
         clsq[i+nklsq2] = 1.0;
         clsq[i+nklsq3] = ptx[k]*pty[k];
         clsqxy[i] = optx[k];
         if (!zgeom) clsqxy[i+nklsq] = opty[k];
         elsqxy[i] = optx[k];
         if (!zgeom) elsqxy[i+nklsq] = opty[k];
         }
      eps = 1.e-7;
      if (linear)
         {
         lsqfit(clsq,&clsqxy[j*nklsq],nklsq,3,&csol[j*4],eps,&ier);
         csol[3] = 0.0; csol[7] = 0.0;
         }
      else if (keystone)
         lsqfit(clsq,&clsqxy[j*nklsq],nklsq,4,&csol[j*4],eps,&ier);
      else
         lsqfit(clsq,&clsqxy[j*nklsq],nklsq,3,&csol[j*3],eps,&ier);
      }
   zvparmd("mins",&miny,&icnt,&minsdf,1,0);
   zvparmd("maxs",&maxy,&icnt,&maxsdf,1,0);
   zvparmd("minl",&minx,&icnt,&minldf,1,0);
   zvparmd("maxl",&maxx,&icnt,&maxldf,1,0);
   if (minsdf==1) miny = tminy;
   if (maxsdf==1) maxy = tmaxy;
   if (minldf==1) minx = tminx;
   if (maxldf==1) maxx = tmaxx;
   if (lgeom) nah = 10; else if (geomv) nah = 50; else nah = 30;
   if (lgeom) nav = 10; else if (geomv) nav = 50; else nav = 30;
   if (nht!=0) nah = nht;
   if (nvt!=0) nav = nvt;
   dx = (tmaxx-tminx+tmaxy-tminy)*5.0;
   if (linear||keystone) goto lin_or_key;
   
   if (!noprint)
      {
      zprnt(8,3,csol,"lsq fit x'=ax+by+c.");
      if (!zgeom) zprnt(8,3,&csol[3],"lsq fit y'=dx+ey+f.");
      zprnt(8,nklsq*(npoint-2),elsqxy,"data.");
      }
      
   for (i=0;i<nklsq;i++)
      {
      if (n>((3*MAXLSQ)/2)) k = i*i%n; else k = i;
      elsqxy[i] = elsqxy[i]-ptx[k]*csol[0]-pty[k]*csol[1]-csol[2];
      if (!zgeom) elsqxy[i+nklsq] = elsqxy[i+nklsq]-
             ptx[k]*csol[3]-pty[k]*csol[4]-csol[5];
      }
   if (!noprint) zprnt(8,nklsq*(npoint-2),elsqxy,"residuals.");
     
   ptr = n*npoint;
   if (!zgeom) ptr = ptr+2;
   ptx[n] = tminx-dx;
   pty[n] = (tminy+tmaxy)*0.5;
   ptx[n+1] = (tminx+tmaxx)*0.5;
   pty[n+1] = tmaxy+dx;
   ptx[n+2] = (tminx+tmaxx)*0.5;
   pty[n+2] = tminy-dx;
   ptx[n+3] = tmaxx+dx;
   pty[n+3] = (tminy+tmaxy)*0.5;
      
   for (i=0;i<4;i++)
      {
      optx[n+i] = csol[0]*ptx[n+i]+csol[1]*pty[n+i]+csol[2];
      if (!zgeom) opty[n+i] = csol[3]*ptx[n+i]+csol[4]*pty[n+i]+csol[5];
      }
   n += 4;

   /* ready for the big triangulation routine, con1,con2,con3 are
      mallocked in the subroutine (type is **) */
      
   skinny = 0.0;
   thiessen(n,&nlret,reject,skinny,abendl,ptx,pty,&ntri,
			  &con1,&con2,&con3);
   if (!noprint)
      {
      zprnt(4,1,&n,"nodes.");
      zprnt(4,1,&nlret,"edges.");
      zprnt(4,1,&ntri,"triangles.");
      }
   
   if (!plot)
      {
      /* solve triangles */
         
      mz_alloc2((unsigned char ***)&coeff,nrank,ntri,8);
      for (ix=0;ix<2;ix++)
         {
         if (zgeom&&ix>0) break;
         for (itri=0;itri<ntri;itri++)
	    {
	    work[0] = ptx[con1[itri]-1];
	    work[1] = ptx[con2[itri]-1];
	    work[2] = ptx[con3[itri]-1];
	    work[3] = pty[con1[itri]-1];
	    work[4] = pty[con2[itri]-1];
	    work[5] = pty[con3[itri]-1];
	    work[6] = 1.;
	    work[7] = 1.;
	    work[8] = 1.;
	    if (ix==0)
	       {
	       tab[0] = optx[con1[itri]-1];
	       tab[1] = optx[con2[itri]-1];
	       tab[2] = optx[con3[itri]-1];
	       }
	    else
	       {
	       tab[0] = opty[con1[itri]-1];
	       tab[1] = opty[con2[itri]-1];
	       tab[2] = opty[con3[itri]-1];
	       }
	    dgauss(work,tab,3,1.e-14,&ier);
	    for (j=0;j<3;j++) coeff[j+ix*3][itri] = tab[j];
	    if (ier!=0) coeff[0][itri] = 1.0E35;
	    }
	 }
         
      ntri = ntri-nerr;
      nav1 = nav+1;
      nah1 = nah+1;
      dx = (maxx-minx)/(double)nav;
      dy = (maxy-miny)/(double)nah;
        
      /* start the geom of the grid, geomv goes to ibis file */
      
      if (geomv) mz_alloc2((unsigned char ***)&vout,4,nah1*nav1,8);
      else if (zgeom) mz_alloc1((unsigned char **)&rout,3*nah1*nav1,4);
      else mz_alloc1((unsigned char **)&rout,4*nah1*nav1,4);
      
      ptr = 0;
      ptr2 = 0;
      tri = 0;
      ttrj1 = 0;
      for (i=0;i<nav1;i++)
         {
         x = minx+(double)i*dx;
         if (lgeom) x = (int)x;
         for (j=0;j<nah1;j++)
            {
            y = miny+(double)j*dy;
            if (lgeom) y = (int)y;
            isign = -1;
            found = 0;
            ttri = tri+ntri;
               
            for (k=0;k<ntri;k++)
               {
               tri = (ttri+((k+1)/2)*isign)%ntri;
               isign = -isign;
               if (coeff[0][tri]>1.0E34) continue;
               if (insidetri(x,y,
                       ptx[con1[tri]-1],pty[con1[tri]-1],
                       ptx[con2[tri]-1],pty[con2[tri]-1],
                       ptx[con3[tri]-1],pty[con3[tri]-1]))
                  {
                  found = 1;
                  break;
                  }
               }
            if (!found)
               {
               tri = ttri-ntri;
               if (j==1) tri = ttrj1;
               if (!noprint)
                 printf("grid point %f,%f not in triangle\n",x,y);
               }
            if (j==1) ttrj1 = tri;
               
            xx = coeff[0][tri]*x+coeff[1][tri]*y+coeff[2][tri];
            if (!zgeom) yy = coeff[3][tri]*x+coeff[4][tri]*y+coeff[5][tri];
            if (geomv)
               {
               vout[0][ptr] = x;
               vout[1][ptr] = y;
               vout[2][ptr] = xx;
               vout[3][ptr++] = yy;
               }
            else if (lgeomlike)
               {
               rout[ptr2] = x;
               rout[ptr2+1] = y;
               rout[ptr2+2] = xx;
               rout[ptr2+3] = yy;
               ptr2 += 4;
               }
            else
               {
               if (zgeom)
                  {
                  rout[ptr2] = x;
                  rout[ptr2+1] = y;
                  rout[ptr2+2] = xx;
                  ptr2 += 3;
                  }
               else
                  {
                  rout[ptr2] = xx;
                  rout[ptr2+1] = yy;
                  rout[ptr2+2] = x;
                  rout[ptr2+3] = y;
                  ptr2 += 4;
                  }
               }
            }
         }
     
     
   /* the next section is for output of linear or keystone grid only */
   
   lin_or_key:
   if (linear||keystone)
      {
      if (!noprint)
         {
         printf("lsq fit x' = ax+by+cxy+d:\n  %15.8f %15.8f %15.8f %15.8f\n",
               csol[0],csol[1],csol[3],csol[2]);
         if (!zgeom) printf("lsq fit y' = ex+fy+gxy+h:\n  %15.8f %15.8f %15.8f %15.8f\n",
               csol[4],csol[5],csol[7],csol[6]);
         zprnt(8,nklsq*(npoint-2),elsqxy,"data.");
         }
      
      for (i=0;i<nklsq;i++)
         {
         if (n>((3*MAXLSQ)/2)) k = i*i%n; else k = i;
         elsqxy[i] = elsqxy[i]-ptx[k]*csol[0]-pty[k]*csol[1]-csol[2]-
                      ptx[k]*pty[k]*csol[3];
         if (!zgeom) elsqxy[i+nklsq] = elsqxy[i+nklsq]-ptx[k]*csol[4]-
                      pty[k]*csol[5]-csol[6]-ptx[k]*pty[k]*csol[7];
         }
      if (!noprint) zprnt(8,nklsq*(npoint-2),elsqxy,"residuals.");
      
      ntri = ntri-nerr;
      nav1 = nav+1;
      nah1 = nah+1;
      dx = (maxx-minx)/(double)nav;
      dy = (maxy-miny)/(double)nah;
        
      /* like above except use surface fit for all points */
      
      if (geomv) mz_alloc2((unsigned char ***)&vout,4,nah1*nav1,8);
      else if (zgeom) mz_alloc1((unsigned char **)&rout,3*nah1*nav1,4);
      else mz_alloc1((unsigned char **)&rout,4*nah1*nav1,4);
      
      ptr = 0;
      ptr2 = 0;
      for (i=0;i<nav1;i++)
         {
         x = minx+(double)i*dx;
         if (lgeom) x = (int)x;
         for (j=0;j<nah1;j++)
            {
            y = miny+(double)j*dy;
            if (lgeom) y = (int)y;
            xx = csol[0]*x+csol[1]*y+csol[2]+csol[3]*x*y;
            if (!zgeom) yy = csol[4]*x+csol[5]*y+csol[6]+csol[7]*x*y;
            if (geomv)
               {
               vout[0][ptr] = x;
               vout[1][ptr] = y;
               vout[2][ptr] = xx;
               vout[3][ptr++] = yy;
               }
            else if (lgeomlike)
               {
               rout[ptr2] = x;
               rout[ptr2+1] = y;
               rout[ptr2+2] = xx;
               rout[ptr2+3] = yy;
               ptr2 += 4;
               }
            else
               {
               if (zgeom)
                  {
                  rout[ptr2] = x;
                  rout[ptr2+1] = y;
                  rout[ptr2+2] = xx;
                  ptr2 += 3;
                  }
               else
                  {
                  rout[ptr2] = xx;
                  rout[ptr2+1] = yy;
                  rout[ptr2+2] = x;
                  rout[ptr2+3] = y;
                  ptr2 += 4;
                  }
               }
            }
         }
      }
      
      /* Output array to IBIS file in col_ordr, or tiepoints parm file */
      
      if (geomv)
         {
         clen = (nah+1)*(nav+1);
         
         status = zvunit(&ibisOut,"out",1,0);
         status = IBISFileUnit(ibisOut,&ibis,"write",4,clen,0,0);
         status = IBISFileSet(ibis,"fmt_default","doub");
         status = IBISFileUnitOpen(ibis);
         /*status = IBISFileOpen(ibisOut,&ibis,"write",4,clen,0,0);*/
        
         for (i=0;i<npoint;i++)
	    {
	    status = IBISColumnWrite(ibis,vout[i],i+1,1,clen);
	    if (status!=1) IBISSignal(ibis,status,0);
	    }
	 status = IBISFileClose(ibis,0);
         return 0;
         }
      else
         {
         status = zvunit(&parmOut,"out",1,0);
         status = zvparm("out",outnam,&nout,&outdf,1,0);
         status = zvpopen(outnam,"SA",&tiept);
         status = zvpout("NAH",&nah,"INT",1,0);
         status = zvpout("NAV",&nav,"INT",1,0);
         status = zvpout("TIEPOINT",rout,"REAL",ptr2,0);
         status = zvpclose();
         return 0;
         }
      } 
   else    /* new plot option */
      {
      mz_alloc2((unsigned char ***)&pout,2,ntri*5,4);
      
      p = 0;
      for (i=0;i<ntri;i++)
         {
         for (j=0;j<5;j++)
            {
            for (k=0;k<2;k++)
               {
               switch (j+k*5)
                  {
                  case 0: pout[k][p] = ptx[con1[i]-1]; break;
                  case 1: pout[k][p] = ptx[con2[i]-1]; break;
                  case 2: pout[k][p] = ptx[con3[i]-1]; break;
                  case 3: pout[k][p] = ptx[con1[i]-1]; break;
                  
                  case 5: pout[k][p] = pty[con1[i]-1]; break;
                  case 6: pout[k][p] = pty[con2[i]-1]; break;
                  case 7: pout[k][p] = pty[con3[i]-1]; break;
                  case 8: pout[k][p] = pty[con1[i]-1]; break;
                  
                  default: pout[k][p] = 0.0;
                  }
               }
            p++;
            }
         }
      clen = ntri*5;
      
      status = zvunit(&ibisOut,"out",1,0);
      status = IBISFileOpen(ibisOut,&ibis,"write",2,clen,0,0);
      for (i=0;i<2;i++)
	 {
	 status = IBISColumnWrite(ibis,pout[i],i+1,1,clen);
	 if (status!=1) IBISSignal(ibis,status,0);
	 }
      status = IBISFileClose(ibis,0);
      return 0;
      }
     
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tieconv.imake
#define  PROGRAM   tieconv

#define MODULE_LIST tieconv.c

#define MAIN_LANG_C
#define R2LIB 

/* Comment this out before delivery.
#define DEBUG
*/

#define USES_C

#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL
$ Return
$!#############################################################################
$PDF_File:
$ create tieconv.pdf
PROCESS       HELP=*
PARM INP      TYPE=(STRING,72)
PARM OUT      TYPE=(STRING,72)
PARM TIEPOINT TYPE=REAL  COUNT=(1:400) DEFAULT=0.
PARM NAH      TYPE=INTEGER COUNT=1 DEFAULT=0
PARM NAV      TYPE=INTEGER COUNT=1 DEFAULT=0
PARM MODE     TYPE=KEYWORD VALID=(LGEOM,GEOMA,MGEOM,GEOMV,GEOMZ)
PARM COLS     TYPE=INTEGER COUNT=3:4 DEFAULT=(1,2,3,4)
PARM NOPRINT  TYPE=KEYWORD COUNT=(0:1) VALID=NOPRINT DEFAULT=--
PARM PLOT     TYPE=KEYWORD COUNT=(0:1) VALID=PLOT    DEFAULT=--
PARM ABEND    TYPE=KEYWORD COUNT=(0:1) VALID=ABEND   DEFAULT=--
PARM LINEAR   TYPE=KEYWORD COUNT=(0:1) VALID=LINEAR  DEFAULT=--
PARM KEYSTONE TYPE=KEYWORD COUNT=(0:1) VALID=KEYSTONE DEFAULT=--
PARM MINS     TYPE=REAL COUNT=1 DEFAULT=0.
PARM MAXS     TYPE=REAL COUNT=1 DEFAULT=0.
PARM MINL     TYPE=REAL COUNT=1 DEFAULT=0.
PARM MAXL     TYPE=REAL COUNT=1 DEFAULT=0.
PARM REJECT   TYPE=REAL COUNT=1 DEFAULT=.01
PARM PARMS    TYPE=(STRING,72) COUNT=(0:1) DEFAULT=--
END-PROC
.TITLE
VICAR Program tieconv
.HELP
PURPOSE

     TIECONV prepares a gridded dataset for POLYGEOM, GEOMA, 
     LGEOM,  MGEOM,  GEOMV or  GEOMZ  transformations. Input
     is paired sets of tiepoints with no restrictions. It is
     in principle, a surface generation routine, but creates 
     the  gridded  dataset so as to best interface with  the 
     VICAR routines above.   The sequence GEN,  TIECONV, and 
     GEOMZ can be used to generate a surface in image format 
     through an arbitrary set of points.
     TIECONV uses the finite element method  (triangulation) 
     for  surface  fitting.   It is anticipated  that  other 
     surface  fitting methods will be integrated into  VICAR 
     in  the same fashion as tieconv so that users will have 
     maximum flexibility both in terms of choice of  surface 
     fit and in terms on application.
     The triangulation method is Thiessen triangulation. The
     maximum number of points is  probably  about 1 million.
     The largest case  tried so far is 400,000 points, which
     ran in 4 hours 31 minutes on a SUN SPARCstation 20.
     
     TIECONV can be used to prepare a  linear surface fit or
     a bilinear surface fit.  The corresponding keywords are
     LINEAR and  KEYSTONE.  The latter is  commonly  used to 
     convert a  perspective view  to an  orthographic in  an
     approximate fashion.  The former  requires  three  tie-
     points and the latter requires  four.   More  tiepoints
     will be handled with a least squares fit and the resid-
     uals can be viewed.  All input output formats are valid
     with these options except for PLOT.
      
.PAGE
TAE COMMAND LINE FORMAT

     tieconv OUT=B PARAMS
     tieconv PARMS=parm_file OUT=B PARAMS
     tieconv INP=tiep  OUT=B  PARAMS

     where

     parm_file           is an optional disk parameter dataset 
			   containing the input tiepoints.
     tiep		 is an optional IBIS tabular file containing
			   the input tiepoints.
     B                   is the output parameter dataset.
     PARAMS              is a standard VICAR parameter field.
.PAGE
OPERATION

     tieconv operates in two phases.   In phase 1, the input 
     points  are  fully  triangulated  by  a version  of the 
     Thiessen   algorithm.   This operates by  computing the
     Voronoi polygons (polygons of area  nearest each point)
     and  computing  the  triangles  formed by the bisectors
     of the  polygons.   Four extra points  are  added  five 
     diameters away from the convex hull so that the surface 
     will extend smoothly beyond the input tiepoints.

     In  phase 2,  the output grid is formed by  evaluating 
     the  triangular surface at grid point locations.   That 
     is,  a grid point will fall in some triangle,  and  the 
     GEOM  shift  will  be the linear interpolation  of  the 
     input shifts at the three corners of the triangle.  The 
     user  should  note  that  the  triangular  surface   is 
     continuous but not differentiable and it passes through 
     all  of  the input  points.   Point-surface  generation 
     routines can e compared in the following table.
.PAGE
|       \ PROPERTIES| CONTI-|DIFFEREN-| EVALUATES |   WELL   |
|        \    OF    | NUOUS |TIABLE   | AT INPUT  |  BEHAVE  |
| METHOD  \ SURFACE |       |         |   POINT   | NO MESAS |
|-------------------|-------|---------|-----------|----------|
| Triangulation     |  yes  |    no   |    yes    |   yes    |
|-------------------|-------|---------|-----------|----------|
|                -1 |       |         |           |          |
| Interpolation r   |   no  |    no   |    yes    |   yes    |
|-------------------|-------|---------|-----------|----------|
|                -p |       |         |           |          |
| Interpolation r   |   no  |    no   |    yes    |    no    |
|-------------------|-------|---------|-----------|----------|
| Polynomial Fit    |  yes  |    yes  |    no     |    no    |
|-------------------|-------|---------|-----------|----------|
| Linear            |  yes  |    yes  |    no     |   yes    |
|-------------------|-------|---------|-----------|----------|
| Bilin (keystone)  |  yes  |    yes  |    no     |   yes    |
|-------------------|-------|---------|-----------|----------|

.PAGE
EXAMPLE

     tieconv OUT=B 'GEOMA NAH=44 NAV=24
           TIEPOINT=(   346       432       353      422
                        479       316       482      313
                         .
                         .
                         .
                        723       529       715      527)
     POLYGEOM INP=X PARMS=B OUT=Y

     In this example,  the tiepoints are used to set up a 44 
     x  24 grid for use by POLYGEOM.   The tiepoints can  be 
     scattered  over  the  image  in  any  fashion   whereas 
     POLYGEOM requires a regular grid.
     
     TIECONV can be used to prepare a  linear surface fit or
     a bilinear surface fit.  The corresponding keywords are
     LINEAR and  KEYSTONE.  The latter is  commonly  used to 
     convert a  perspective view  to an  orthographic in  an
     approximate fashion.  The former  requires  three  tie-
     points and the latter requires  four.   More  tiepoints
     will be handled with a least squares fit and the resid-
     uals can be viewed.  All input output formats are valid
     with these options except for PLOT.
      
     
.PAGE

     GEN OUT=X NL=1000 NS=1000 IVAL=0 LINC=0 SINC=0
     tieconv OUT=B 'GEOMZ+
             TIEPOINTS=(
               1   1    0
               1000   1    0
               1    1000     0
               1000  1000      0
               500   500       255)
     GEOMZ INP=X PARMS=B OUT=Y

     this  example constructs a "pyramid" shaped  brightness 
     surface in the image Y.
.PAGE

TIMING

     Timing  is dominated by the triangulation method  which 
     is 0(n*log(n)) where n is the number of input points.  A case 
     with  10000  points  was run in 1.63 minutes  CPU  time, 
     and a case with 400,000 points was run in 4.52 hours on
     a SPARCstation 20.
     
RESTRICTIONS

   The maximum number of input tiepoints is probably about 1 million.
   This value will increase as the machines get more virtual and
   real memory since dynamic memory allocation is used throughout
   the algorithm.
   
   The maximum number of output tiepoints is limited by IBIS table
   size (currently about 10 million?).  Internal to the program,
   dynamic memory allocation is used.


.PAGE
WRITTEN BY:            A. L. Zobrist, 29 August 1979

COGNIZANT PROGRAMMER:  B. A. McGuffie

REVISIONS: 
  PORTED TO UNIX	C. R. Schenk (CRI)  31-Oct 1994
  ALGORITHM UPDATED     A. L. Zobrist 19-Jul 1999

.LEVEL1
.VARIABLE INP
Input IBIS tabular file
.VARIABLE COLS
Columns to use from
IBIS file.
.VARIABLE OUT
Output dataset (type depends
on other parameters, see
detailed help)
.VARIABLE PARMS
Input parameter dataset
.VARIABLE TIEPOINT
Specify tiepoint pairs 
.VARIABLE NAH
Number of grid cells horizontal
.VARIABLE NAV
Number of grid cells vertical
.VARIABLE MINL
Bounds of the output grid
.VARIABLE MINS
Bounds of the output grid
.VARIABLE MAXL
Bounds of the output grid
.VARIABLE MAXS
Bounds of the output grid
.VARIABLE REJECT
Radius for duplicate  points
.VARIABLE MODE
GEOMA for GEOMA or POLYGEOM use 
GEOMZ for GEOMZ  use
LGEOM for LGEOM  use 
MGEOM for MGEOM  use
.VARIABLE PLOT
Gen plot file of triangulation
.VARIABLE NOPRINT
Keyword to suppress printout
.VARIABLE ABEND
ABEND abend if duplicate points
.VARIABLE LINEAR
forget triangulation and do
linear fit
.VARIABLE KEYSTONE
forget triangulation and do
bilinear (keystone) fit

.LEVEL2
.VARIABLE INP
       INP=A		 Input IBIS tabular file containing the
			 input tiepoints.  If INP is specified
			 then the tiepoints will be taken from
			 the IBIS interface file rather than the
			 TIEPOINT parameter or the parameter
			 data set.  
.VARIABLE COLS
    COLS=(C1,C2,C3,C4)   Columns in the IBIS tabular file that
			 contain the tiepoints.  C1 has new line,
			 C2 has new sample, C3 has old line, and
			 C4 has old sample.

.VARIABLE OUT
       OUT=B             Output parameter data set containing
			 gridded tiepoints suitable for the
			 GEOM programs.  If the GEOMV keyword
			 is used then this is in an IBIS file.
			 If the plot option is chosen, then
			 this is a plot file.
.VARIABLE PARMS
       PARMS=parm_file   Optional parameter data set created
                         by routine XVPOUT. This data set con-
                         tains keywords and data for TIEPOINT
                         NAH and NAV and can be used instead
                         of specifying these keywords in the
                         TAE COMMAND LINE.
.VARIABLE TIEPOINT
     TIEPOINT=(NL1,NS1,  these  specify  the input  tiepoint 
       OL1,OS1, . . .,   pairs   for   GEOM    applications.  
       NLk,NSk,OLk,OSk)  Maximum k is 100 (due to TAE).

     TIEPOINT=(NL1,NS1,  this  form  of parameter  specifies 
       DZ1, . . .,NLk,   the input tiepoint pairs for  GEOMZ 
       NSk, DZk)         applications   or   image   surface 
                         generation.   Maximum k is 133 (due 
                         to TAE).
.VARIABLE NAH
     NAH=n               the  integer n specifies the number 
                         of  grid cells horizontally in  the 
                         output  grid (default is 30  except 
                         in the case of LGEOM which is 10).
.VARIABLE NAV
     NAV=m               the integer m specifies the  number 
                         of  grid  cells vertically  in  the 
                         output  grid (default is 30  except 
                         in the case of LGEOM which is 10).

.VARIABLE MINL
     MINL=w              the integers w,  x, y, z define the 
     MINS=x              lower   and  upper  bounds  of  the 
     MAXL=y              output  grid in terms of  line  and 
     MAXS=z              sample.  The default is to make the 
                         grid  exactly  contain  the  convex 
                         hull of the input tiepoints.

.VARIABLE MINS
     MINL=w              the integers w,  x, y, z define the 
     MINS=x              lower   and  upper  bounds  of  the 
     MAXL=y              output  grid in terms of  line  and 
     MAXS=z              sample.  The default is to make the 
                         grid  exactly  contain  the  convex 
                         hull of the input tiepoints.

.VARIABLE MAXL
     MINL=w              the integers w,  x, y, z define the 
     MINS=x              lower   and  upper  bounds  of  the 
     MAXL=y              output  grid in terms of  line  and 
     MAXS=z              sample.  The default is to make the 
                         grid  exactly  contain  the  convex 
                         hull of the input tiepoints.

.VARIABLE MAXS
     MINL=w              the integers w,  x, y, z define the 
     MINS=x              lower   and  upper  bounds  of  the 
     MAXL=y              output  grid in terms of  line  and 
     MAXS=z              sample.  The default is to make the 
                         grid  exactly  contain  the  convex 
                         hull of the input tiepoints.

.VARIABLE REJECT
     REJECT=r            the    floating   point   value   r 
                         specifies  a  radius  within  which 
                         separate points will be  considered 
                         as  duplicate  points  (default  is 
                         .01).

.VARIABLE MODE
     GEOMV               this  keyword  specifies  that  the 
                         output  dataset is to be  formatted 
                         for  GEOMV  use.   The output  disk
                         dataset will be an IBIS file.  Note
                         that GEOMV no longer  requires  the
                         NAH or NAV keywords.

     GEOMA               this  keyword  specifies  that  the 
                         output  dataset is to be  formatted 
                         for  GEOMA or  POLYGEOM  use.   The 
                         output  disk  dataset will  contain 
                         the   proper  GEOMA   or   POLYGEOM 
                         keywords  and  format  so  that  no 
                         addition   parameters  need  to  be 
                         specified unless desired.

     GEOMZ               this  keyword  specifies  that  the 
                         output is to be formatted for GEOMZ 
                         use.   The output disk dataset will 
                         contain  the proper GEOMZ  keywords 
                         and  format so that  no  additional 
                         parameters  need  to  be  specified 
                         unless desired.

     LGEOM               this  keyword  specifies  that  the 
                         output is to be formatted for LGEOM 
                         use.   The output disk dataset will 
                         contain  the proper LGEOM  keywords 
                         and  format so that  no  additional 
                         parameters  need  to  be  specified 
                         unless   desired.    The  user   is 
                         cautioned    to    observe    LGEOM 
                         application size limitations.

     MGEOM               this  keyword  specifies  that  the 
                         output is to be formatted for MGEOM 
                         use.   The output disk dataset will 
                         contain  proper MGEOM keywords  and 
                         format   so   that  no   additional 
                         keywords   need  to  be   specified 
                         unless desired.
.VARIABLE ABEND
    ABEND=ABEND          specifies  that the routine  should 
    or 'ABEND            abend   if  duplicate  points   are 
                         found.
.VARIABLE LINEAR
    LINEAR=LINEAR         
    or 'LINEAR      TIECONV can be used to prepare a  linear
                    surface fit or a bilinear surface fit.
                    The corresponding keywords are LINEAR and
                    KEYSTONE.  The latter is  commonly  used
                    to convert a  perspective view  to an 
                    orthographic in  an approximate fashion.
                    The former  requires  three  tiepoints 
                    and the latter requires  four.   More
                    tiepoints will be handled with a least
                    squares fit and the residuals can be 
                    viewed.  All input output formats are 
                    valid with these options except for PLOT.
.VARIABLE KEYSTONE
    KEYSTONE=KEYSTONE         
    or 'KEYSTONE    TIECONV can be used to prepare a  linear
                    surface fit or a bilinear surface fit.
                    The corresponding keywords are LINEAR and
                    KEYSTONE.  The latter is  commonly  used
                    to convert a  perspective view  to an 
                    orthographic in  an approximate fashion.
                    The former  requires  three  tiepoints 
                    and the latter requires  four.   More
                    tiepoints will be handled with a least
                    squares fit and the residuals can be 
                    viewed.  All input output formats are 
                    valid with these options except for PLOT.
.VARIABLE PLOT
    PLOT=PLOT            bypasses tiepoint generation phase.
    or 'PLOT             creates graphics file of triangula-
                         tion for plotting.
.VARIABLE NOPRINT
    'NOPRINT		 Keyword to suppress printout.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttieconv.pdf
procedure
refgbl $echo
refgbl $autousage
parm version string def="ibis-1"
parm org string def="column"
body
!let _onfail="continue"
let $autousage="none"
let $echo="yes"


!! Note to testers:
!!
!!  differences in "data" values of less than 0.1% between different
!!  platforms are due to roundoff and are not significant;
!!
!!  also, differences in "residuals" when less than about 1.0E-10
!!  are completely insignificant.
!!
!!  (lwk, 2002/03/28)


ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=44
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")

tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV  

! list the IBIS file
ibis-list b 'format

! testing the lgeom parameter output

tieconv INP=a COLS=(1,2,3,4)  OUT=b2 +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'LGEOM  

! now test that the tiepoint data sets are OK in the geom programs

gen mgtest 10 10 SINC=40 LINC=40

geomv (mgtest,b) mgtest2 SIZE=(1,1,10,10)
list mgtest2

lgeom mgtest mgtest3 SIZE=(1,1,10,10) PARMS=b2
list mgtest3

difpic (mgtest2,mgtest3)

! now larger test case

ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=3000
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")

tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=30,NAV=5,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV  

! list the IBIS file
ibis-list b 'format


! list out a plot file just for definite look at triangles

ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=11
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")

tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 'PLOT

! list the IBIS file
ibis-list b 'format

! test linear and keystone keywords

ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=44
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")

tieconv INP=a COLS=(1,2,3,4)  'linear OUT=b +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 
tieconv INP=b COLS=(1,2,3,4)  'linear OUT=b2 +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 

! list the IBIS files, the second should have near-zero residuals

ibis-list b 'format
ibis-list b2 'format

tieconv INP=a COLS=(1,2,3,4)  'keystone OUT=b +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 
tieconv INP=b COLS=(1,2,3,4)  'keystone OUT=b2 +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 

! list the IBIS files, the second should have near-zero residuals

ibis-list b 'format
ibis-list b2 'format

tieconv INP=a COLS=(1,2,3,4)  'keystone OUT=b +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 
tieconv INP=b COLS=(1,2,3,4)  'linear OUT=b2 +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 

! list the IBIS files, the second should have NONzero residuals

ibis-list b 'format
ibis-list b2 'format

!!  NOTE:  the GEOMZ option is not incuded in this proc, because
!!  it is still experimental code.  See Al Zobrist or the Cart lab
!!  for later versions.

end-proc
$!-----------------------------------------------------------------------------
$ create devtieconv.pdf
procedure
refgbl $echo
refgbl $autousage
parm version string def="ibis-1"
parm org string def="column"
body
!let _onfail="continue"
let $autousage="none"
let $echo="yes"


!!!!!!!!!!!! DEVELOPMENT CASES FOR ALZ, ALSO HAS A PLOT CASE
!!!!!!!!!!!! SEE THE TSTTIECONV.PDF FILE FOR THE REGRESSION TEST


!ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
!    nc=4 nr=44
!mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
!mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
!!mf a func=("c1=c1-mod(c1,0.1)","c2=c2-mod(c2,0.1)")
!mf a func=("c3=c1*1.1","c4=c2*1.1")
!ibis-list a cols=(1,2,3,4) csize=12 'format

!!datetime
!tieconv INP=a COLS=(1,2,3,4)  OUT=b +
!      NAH=3,NAV=3,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
!    'GEOMV 'NOPR 
!!datetime


! testing the lgeom parameter output

!tieconv INP=a COLS=(1,2,3,4)  OUT=b2 +
!      NAH=3,NAV=3,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
!    'LGEOM 'NOPR 

! now test that the tiepoint data sets are OK

!gen mgtest 10 10 SINC=40 LINC=40

!geomv (mgtest,b) mgtest2 SIZE=(1,1,10,10)
!list mgtest2

!lgeom mgtest mgtest3 SIZE=(1,1,10,10) PARMS=b2
!list mgtest3

!difpic (mgtest2,mgtest3)

! now large test cases with plot

ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=2000
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
!mf a func=("c1=c1-mod(c1,0.1)","c2=c2-mod(c2,0.1)")
mf a func=("c3=c1*1.1","c4=c2*1.1")
!ibis-list a cols=(1,2,3,4) csize=12 'format

datetime
tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=1,NAV=1,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 'NOPR !'PLOT
datetime

!pltgraf inp=b xrange=(-20.0,120.0) yrange=(-20.0,120.0) xlen=10 ylen=10


! rectangular grid case, hard for voronoi algorithms, see Euler #


ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=8281
mf a func=("c1=1.1*int((index-1)/91.0)","c2=1.1*mod(index-1,91)")
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")


tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 'NOPR 'plot

!pltgraf inp=b xrange=(-20.0,120.0) yrange=(-20.0,120.0) xlen=10 ylen=10

end-proc
$ Return
$!#############################################################################
