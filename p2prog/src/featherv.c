#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <string.h>
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
int inlne, insamp;			/* size of primary input */
int sline, ssamp, nline, nsamp;		/* User specified size of output */
char fmt_str[10];                       /* format of primary input */
 
void open_files()
{
  int status,tsize[4],sizepcnt,sizedef;
  
  /***********************/
  /* open the input file */
  /***********************/
  status = zvunit( &i_unit, "INP", 1, NULL);
  status = zvopen( i_unit, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF",NULL);
  /* should have checked status to make sure file opened properly. */
  /* Make sure we have either BYTE or HALF */
  zvget(i_unit,"FORMAT",fmt_str,NULL);
  if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF")) {
    zvmessage("Invalid input data format.  Use BYTE or HALF.","");
    zabend();
  }
  /***************************/
  /* open up the output file */
  /***************************/
  /*zvsize( &sline, &ssamp, &nline, &nsamp, &inline, &insamp);*/
  /*zvsize no good for negative parameters: sline,ssamp*/
  zvget(i_unit,"NL",&inlne,"NS",&insamp,NULL);
  zvparm("SIZE",tsize,&sizepcnt,&sizedef,4,NULL);
  if (!sizedef)
     {
     sline = tsize[0];
     if (sizepcnt>=2) ssamp = tsize[1];
     if (sizepcnt>=3) nline = tsize[2];
     if (sizepcnt>=4) nsamp = tsize[3];
     }
  else
     {
     zvparm("SL",&sline,&sizepcnt,&sizedef,1,NULL);
     zvparm("SS",&ssamp,&sizepcnt,&sizedef,1,NULL);
     zvparm("NL",&nline,&sizepcnt,&sizedef,1,NULL);
     zvparm("NS",&nsamp,&sizepcnt,&sizedef,1,NULL);
     }
  if (nline==0) nline = inlne;
  if (nsamp==0) nsamp = insamp;
  
  status=zvunit( &o_unit, "OUT", 1, NULL);
  /* note that zvopen is intelligent enough to default to the same */
  /* format as the input file.  */
  status=zvopen( o_unit, "U_NL", nline, "U_NS", nsamp, "U_FORMAT","HALF",
		"OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA",
                "O_FORMAT",fmt_str,"TYPE","IMAGE",NULL);
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
   
   status = zvunit(&geounit,inp,instance,NULL);
   status = zvopen(geounit,"OP","READ","OPEN_ACT","SA",
         "LAB_ACT","SA",NULL);
      
   strcpy(buf,"");
   do
      {
      status=zlninfo(geounit,key,valformat,&maxlen,
         &nelement,"ERR_ACT"," ",NULL);
      if (status!=1) break;
      if (strcmp(key,"NL")==0)
         {
         status=zlget(geounit,"SYSTEM",key,nl,
            "ERR_ACT","SA","FORMAT","INT",NULL);
         }
      if (strcmp(key,"NS")==0)
         {
         status=zlget(geounit,"SYSTEM",key,ns,
            "ERR_ACT","SA","FORMAT","INT",NULL);
         }
      status=zlinfo(geounit,"PROPERTY",key,vformat,
         &maxlen,&nelement,"ERR_ACT"," ",
         "PROPERTY","GEOTIFF",NULL);
      if (status!=1) continue;
      if (strcmp(key,"PROPERTY")==0) continue;
      /* now concatenate the string values / can be vector */
      
      for (i=1;i<=nelement;i++)
         {
         if (nelement==1)
            status=zlget(geounit,"PROPERTY",key,svalue,
               "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
               "PROPERTY","GEOTIFF","ULEN",133,NULL);
         else
            status=zlget(geounit,"PROPERTY",key,svalue,"ELEMENT",i,
               "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
               "PROPERTY","GEOTIFF","ULEN",133,NULL);
         strcat(buf,key);
         strcat(buf,"=");
         strcat(buf,svalue);
         strcat(buf,"\n");
         }
      }
   while (1);
   status = zvclose(geounit,NULL);
   
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
   
   status=zvunit(&geounit,fileparm,nfile,NULL);
   status=zvopen(geounit,"OP","UPDATE","OPEN_ACT","SA",
	"LAB_ACT","SA",NULL);
   if (!add) do
      {
      /*seems only way to delete properties, note ERR_ACT*/
      status=zlninfo(geounit,key,valformat,&maxlen,
      &nelement,"ERR_ACT"," ",NULL);
      if (status!=1) break;
      status=zlinfo(geounit,"PROPERTY",key,valformat,
      &maxlen,&nelement,"PROPERTY","GEOTIFF",
      "ERR_ACT"," ",NULL);
      if (status!=1) continue;
      if (strcmp(key,"PROPERTY")==0) continue;
      status=zldel(geounit,"PROPERTY",key,
      "ERR_ACT","SA","PROPERTY","GEOTIFF",NULL);
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
         "PROPERTY","GEOTIFF",NULL);
      }
   while (p<q);
   zvclose(geounit,NULL);
   return;
}

void main44(void)
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
   status = zvunit(&finfo,"inp",filcount,NULL);
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
   zvclose(i_unit,"CLOS_ACT","FREE",NULL);
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
               zvclose(vunit[j],"CLOS_ACT","FREE",NULL);
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
               status = zvunit( &vunit[j], "INP", fileix[j], NULL);
               status = zvopen( vunit[j], "OPEN_ACT", "SA",
                 "IO_ACT", "SA", "U_FORMAT","HALF",NULL);
               zvget(vunit[j],"NL",&tempnl,NULL);
               zvget(vunit[j],"NS",&tempns,NULL);
               if (tempnl!=mfnl[ibj]||tempns!=mfns[ibj])
                  {
                  printf("file %s has\n",infil[j-1]);
                  zmabend("(nl,ns) disagreement, label vs. IBIS file");
                  }
               zvget(vunit[j],"FORMAT",fmt_str2,NULL);
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
                   "LINE",lineread[j]++,"SAMP",1,"NSAMPS",tns,NULL);
               }
            else
               {
               status = zvread(vunit[j],inbuf[bix],
                   "LINE", lineread[j], "SAMP", 1, "NSAMPS", tns, NULL);
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
                           "LINE",rereadline,"SAMP",1,"NSAMPS",mfns[ibj],NULL);
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
                                    "LINE",rereadline,"SAMP",1,"NSAMPS",mfns[ibjq],NULL);
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
      if (openstat[j]==1||openstat[j]==2) zvclose(vunit[j],"CLOS_ACT","FREE",NULL);
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
               zvclose(vunit[j],"CLOS_ACT","FREE",NULL);
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
               status = zvunit( &vunit[j], "INP", fileix[j], NULL);
               status = zvopen( vunit[j], "OPEN_ACT", "SA",
                 "IO_ACT", "SA", "U_FORMAT","HALF",NULL);
               zvget(vunit[j],"NL",&tempnl,NULL);
               zvget(vunit[j],"NS",&tempns,NULL);
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
                   "LINE",lineread[j]++,"SAMP",1,"NSAMPS",tns,NULL);
               }
            else
               {
               status = zvread(vunit[j],inbuf[bix],
                   "LINE", lineread[j], "SAMP", 1, "NSAMPS", tns, NULL);
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
                        "LINE",rereadline,"SAMP",1,"NSAMPS",mfns[ibj],NULL);
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
                           "LINE",rereadline,"SAMP",1,"NSAMPS",mfns[ibj],NULL);
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
         zvwrit(o_unit,bufout,"LINE",iout+1,"SAMP",1,"NSAMPS", nsamp, NULL);
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
   
   zvclose(o_unit,NULL);
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
