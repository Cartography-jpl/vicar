$!****************************************************************************
$!
$! Build proc for MIPL module geomv
$! VPACK Version 1.9, Thursday, July 19, 2007, 12:34:24
$!
$! Execute by entering:		$ @geomv
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
$ write sys$output "*** module geomv ***"
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
$ write sys$output "Invalid argument given to geomv.com file -- ", primary
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
$   if F$SEARCH("geomv.imake") .nes. ""
$   then
$      vimake geomv
$      purge geomv.bld
$   else
$      if F$SEARCH("geomv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake geomv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @geomv.bld "STD"
$   else
$      @geomv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create geomv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack geomv.com -mixed -
	-s geomv.c -
	-i geomv.imake -
	-p geomv.pdf -
	-t tstgeomv.pdf devgeomv.pdf tstgeomv.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create geomv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "vicmain_c.h"
#include "applic.h"
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

/*  image geom routine   A. Zobrist    7/14/99   */


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
      if (powr!=(-9999.0)) { powr = 10.*powr+(**num_ptr)-'0'; continue; }
      else if (decpt==0.) { lvalue = 10.*lvalue+(**num_ptr)-'0'; continue; }
	 else { rvalue = rvalue+decpt*((**num_ptr)-'0'); decpt *= .1; }
      }
   if (powr!=(-9999.0)) return (sign*(lvalue+rvalue)*pow(10.0,powr*powsign));
   else return (sign*(lvalue+rvalue));
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
      if (strcmp(key,"PROPERTY")==0) continue;
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
   if (((*labelstr)=(char *)malloc(len+1))==NULL) zmabend("malloc failed");
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
   double tie[4],voff,ddummy,scale[2];
   
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
      if (strcmp(key,"PROPERTY")==0) continue;
      status=zlinfo(geounit,"PROPERTY",key,valformat,
      &maxlen,&nelement,"PROPERTY","GEOTIFF",
      "ERR_ACT"," ",0);
      if (status!=1) continue;
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

/************************************************************************/
/*									*/
/*		open_files()						*/
/*	This routine opens the input and output image files		*/
/*									*/
/************************************************************************/

int i_unit,o_unit;			/* input unit, output unit */
int sline, ssamp, nline, nsamp;		/* User specified size of output */
int inpline, inpsamp;			/* size of primary input */


void open_files(filetype)
   int *filetype;
{
  int status,tsize[4],sizepcnt,sizedef;
  char fmt_str[10];

  /***********************/
  /* open the input file */
  /***********************/
  status = zvunit( &i_unit, "INP", 1, 0);
  status = zvopen( i_unit, "OPEN_ACT", "SA", "IO_ACT", "SA",0);
  /* should have checked status to make sure file opened properly. */
  /* Make sure we have either BYTE or HALF */
  zvget(i_unit,"FORMAT",fmt_str,0);
  if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF")) {
    zvmessage("Invalid input data format.  Use BYTE or HALF.","");
    zabend();
  }
  if (strcmp(fmt_str,"BYTE")==0) *filetype = 0; else *filetype = 1;
  /***************************/
  /* open up the output file */
  /***************************/
  /*zvsize( &sline, &ssamp, &nline, &nsamp, &inpline, &inpsamp);*/
  /*zvsize no good for negative parameters: sline,ssamp*/
  zvget(i_unit,"NL",&inpline,"NS",&inpsamp,0);
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
  if (nline==0) nline = inpline;
  if (nsamp==0) nsamp = inpsamp;
  
  status=zvunit( &o_unit, "OUT", 1, 0);
  /* note that zvopen is intelligent enough to default to the same */
  /* format as the input file.  */
  status=zvopen( o_unit, "U_NL", nline, "U_NS", nsamp,
		"OP", "WRITE",
		"OPEN_ACT", "SA",
		"IO_ACT", "SA",0);
  return;
}

void get_extrema(newsl,newss,newnl,newns,nahm1,navm1,grids,ldel,sdel,
      outl,outs,inl,ins,jtop,lmin,lmax,smin,smax)
   int newsl,newss,newnl,newns,nahm1,navm1,grids,jtop;
   double *outl,*outs,*inl,*ins,ldel,sdel;
   double *lmin,*lmax,*smin,*smax;
{
   int iline,jgrp,bx,nloop,ipos,jpos,icell,jcell;
   double fipos,fjpos,ficell,fjcell,fnloop1;
   double fri,frj,fri1,frj1,pi1,pi2,pj1,pj2,pipos,pjpos;
   double fiposq,fjposq,ficellq,fjcellq,piposq,pjposq;
   
   *lmin = 1.e20; *smin = 1.e20;
   *lmax = -1.e20; *smax = -1.e20;
   for (iline=0;iline<newnl;iline++)
      {
      ipos = newsl+iline;
      fipos = (double)ipos+.5;
      ficell = (fipos-outl[0])/ldel;
      icell = MAX(MIN((int)ficell,navm1),0);
      jpos = newss;
      for (jgrp=0;;jgrp++)
	 {
	 if (jpos>=jtop) break;
	 fjpos = (double)jpos+.5;
	 fjcell = (fjpos-outs[0])/sdel;
	 jcell = MAX(MIN((int)fjcell,nahm1),0);
	 fri = ficell-(double)icell;
	 fri1 = 1.-fri;
	 frj = fjcell-(double)jcell;
	 frj1 = 1.-frj;
	 bx = icell*grids+jcell;
	 pi1 = inl[bx]*fri1+inl[bx+grids]*fri;
	 pi2 = inl[bx+1]*fri1+inl[bx+grids+1]*fri;
	 pipos = pi1*frj1+pi2*frj;
	 pj1 = ins[bx]*fri1+ins[bx+grids]*fri;
	 pj2 = ins[bx+1]*fri1+ins[bx+grids+1]*fri;
	 pjpos = pj1*frj1+pj2*frj;
	 nloop = MIN((int)(frj1*sdel+1.),jtop-jpos);
	 fnloop1 = (double)(nloop-1);
	 
	 fiposq = fipos;
	 fjposq = fjpos+fnloop1;
	 ficellq = (fiposq-outl[0])/ldel;
	 fjcellq = (fjposq-outs[0])/sdel;
	 fri = ficellq-(double)icell;
	 fri1 = 1.-fri;
	 frj = fjcellq-(double)jcell;
	 frj1 = 1.-frj;
	 pi1 = inl[bx]*fri1+inl[bx+grids]*fri;
	 pi2 = inl[bx+1]*fri1+inl[bx+grids+1]*fri;
	 piposq = pi1*frj1+pi2*frj;
	 pj1 = ins[bx]*fri1+ins[bx+grids]*fri;
	 pj2 = ins[bx+1]*fri1+ins[bx+grids+1]*fri;
	 pjposq = pj1*frj1+pj2*frj;
	 jpos += nloop;
	 *lmin = MIN(*lmin,pipos); *lmin = MIN(*lmin,piposq);
	 *smin = MIN(*smin,pjpos); *smin = MIN(*smin,pjposq);
	 *lmax = MAX(*lmax,pipos); *lmax = MAX(*lmax,piposq);
	 *smax = MAX(*smax,pjpos); *smax = MAX(*smax,pjposq);
	 }
      }
   return;
}

void process_block(sl,ss,nl,ns,nlp6,nsp6,newsl,newss,
      newnl,newns,outl,outs,inl,ins,jtop,nahm1,navm1,grids,interp,
      znoin,ldel,sdel,istrip,fileptr,newnswrite,filetype)
   int sl,ss,nl,ns,nlp6,nsp6,newsl,newss,newnl,newns;
   int jtop,nahm1,navm1,grids,interp,znoin,istrip,newnswrite,filetype;
   double *outl,*outs,*inl,*ins,ldel,sdel;
   FILE *fileptr;
{
   int i,j,iline,bx,cxl,cxs,nloop,tin,xl,xs,outcount,ipos,jpos;
   int status,icell,jcell,bufpos,jgrp,cubtype;
   unsigned char **bbuf,*boutbuf;
   short int **buf,*outbuf;
   double fslbd,flubd,fssbd,fsubd,fxl,fxs,fxl1,fxs1,fipos,fjpos;
   double ficell,fjcell,rl,ru,fnloop1;
   double fri,frj,fri1,frj1,pi1,pi2,pj1,pj2,pipos,pjpos;
   double fiposq,fjposq,ficellq,fjcellq,piposq,pjposq;
   double fidel,fjdel;
   double t1,t2,t3,c1,c2,c3,c4,r1,r2,r3,r4,div6;
   
   if (interp==2||interp==3)
      {
      cubtype = 1;
      div6 = 1.0/6.0;
      }
   else cubtype = 0;
   
   outcount = 1;
   
   if (filetype)
      {
      mz_alloc2((unsigned char ***)&buf,nlp6,nsp6,2);
      mz_alloc1((unsigned char **)&outbuf,newnswrite,2);
      for (i=1;i<=nl;i++)
         {
         status = zvread(i_unit,&buf[i+1][2],"LINE", i+sl,
  		   "SAMP", ss+1, "NSAMPS", ns,0);
         buf[i+1][0] = 0;
         buf[i+1][1] = 0;
         buf[i+1][ns+2] = 0;
         buf[i+1][ns+3] = 0;
         }
      for (j=0;j<nsp6;j++)
         {
         buf[0][j] = 0;
         buf[1][j] = 0;
         buf[nl+2][j] = 0;
         buf[nl+3][j] = 0;
         }
      }
   else
      {
      mz_alloc2((unsigned char ***)&bbuf,nlp6,nsp6,1);
      mz_alloc1((unsigned char **)&boutbuf,newnswrite,1);
      for (i=1;i<=nl;i++)
         {
         status = zvread(i_unit,&bbuf[i+1][2],"LINE", i+sl,
  		   "SAMP", ss+1, "NSAMPS", ns,0);
         bbuf[i+1][0] = (unsigned char)0;
         bbuf[i+1][1] = (unsigned char)0;
         bbuf[i+1][ns+2] = (unsigned char)0;
         bbuf[i+1][ns+3] = (unsigned char)0;
         }
      for (j=0;j<nsp6;j++)
         {
         bbuf[0][j] = (unsigned char)0;
         bbuf[1][j] = (unsigned char)0;
         bbuf[nl+2][j] = (unsigned char)0;
         bbuf[nl+3][j] = (unsigned char)0;
         }
      }
      
   /* calculate one output line at a time */

   flubd = (double)(sl+nl)-0.499; fsubd = (double)(ss+ns)-0.499;
   fslbd = (double)sl+0.499; fssbd = (double)ss+0.499;
   fxl = 0.; fxs = 0.; fxl1 = 1.; fxs1 = 1.;
   for (iline=0;iline<newnl;iline++)
      {
      ipos = newsl+iline;
      fipos = (double)ipos+.5;
      ficell = (fipos-outl[0])/ldel;
      icell = MAX(MIN((int)ficell,navm1),0);
      jpos = newss;
      bufpos = 0;
      for (jgrp=0;;jgrp++)
	 {
	 if (jpos>=jtop) break;
	 fjpos = (double)jpos+.5;
	 fjcell = (fjpos-outs[0])/sdel;
	 jcell = MAX(MIN((int)fjcell,nahm1),0);
	 fri = ficell-(double)icell;
	 fri1 = 1.-fri;
	 frj = fjcell-(double)jcell;
	 frj1 = 1.-frj;
	 bx = icell*grids+jcell;
	 pi1 = inl[bx]*fri1+inl[bx+grids]*fri;
	 pi2 = inl[bx+1]*fri1+inl[bx+grids+1]*fri;
	 pipos = pi1*frj1+pi2*frj;
	 pj1 = ins[bx]*fri1+ins[bx+grids]*fri;
	 pj2 = ins[bx+1]*fri1+ins[bx+grids+1]*fri;
	 pjpos = pj1*frj1+pj2*frj;
	 nloop = MIN((int)(frj1*sdel+1.),jtop-jpos);
	 fnloop1 = (double)(nloop-1);
	 fiposq = fipos;
	 fjposq = fjpos+fnloop1;
	 ficellq = (fiposq-outl[0])/ldel;
	 fjcellq = (fjposq-outs[0])/sdel;
	 fri = ficellq-(double)icell;
	 fri1 = 1.-fri;
	 frj = fjcellq-(double)jcell;
	 frj1 = 1.-frj;
	 pi1 = inl[bx]*fri1+inl[bx+grids]*fri;
	 pi2 = inl[bx+1]*fri1+inl[bx+grids+1]*fri;
	 piposq = pi1*frj1+pi2*frj;
	 pj1 = ins[bx]*fri1+ins[bx+grids]*fri;
	 pj2 = ins[bx+1]*fri1+ins[bx+grids+1]*fri;
	 pjposq = pj1*frj1+pj2*frj;
	 if (nloop>1)
	    {
	    fidel = (piposq-pipos)/fnloop1;
	    fjdel = (pjposq-pjpos)/fnloop1;
	    }
	 else
	    {
	    fidel = 0.0;
	    fjdel = 0.0;
	    }
	 for (j=0;j<nloop;j++)
	    {
	    tin = interp;
	    xl = (int)(pipos+9.5)-10;
	    xs = (int)(pjpos+9.5)-10;
	    if (pipos<fslbd||pjpos<fssbd||pipos>flubd||pjpos>fsubd)
	       {
	          if (filetype) outbuf[bufpos+j] = 0;
	          else boutbuf[bufpos+j] = (unsigned char)0;
	          goto skip;
	       }
	    fxl = pipos-(double)xl-.5;
	    fxs = pjpos-(double)xs-.5;
	    fxl1 = 1.0-fxl;
	    fxs1 = 1.0-fxs;
	    cxl = xl-sl+2;
	    cxs = xs-ss+2;
	    
	    if (filetype)
               {	    
	       if (znoin)
	          {
	          if (cubtype)
	             {
	             if (buf[cxl-1][cxs-1]==0||buf[cxl+2][cxs-1]==0||
	                 buf[cxl-1][cxs+2]==0||buf[cxl+2][cxs+2]==0)
	                {
	                /* the zero(s) have to participate */
	                if (((buf[cxl-1][cxs-1]==0)&&(fxl1>0.01)&&(fxs1>0.01))||
	                    ((buf[cxl+2][cxs-1]==0)&&(fxl>0.01)&&(fxs1>0.01))||
	                    ((buf[cxl-1][cxs+2]==0)&&(fxl1>0.01)&&(fxs>0.01))||
	                    ((buf[cxl+2][cxs+2]==0)&&(fxl>0.01)&&(fxs>0.01)))
	                   tin = 9;
	                }
	             }
	          else
	             {
	             if (buf[cxl][cxs]==0||buf[cxl+1][cxs]==0||
	                 buf[cxl][cxs+1]==0||buf[cxl+1][cxs+1]==0)
	                {
	                /* the zero(s) have to participate */
	                if (((buf[cxl][cxs]==0)&&(fxl1>0.01)&&(fxs1>0.01))||
	                    ((buf[cxl+1][cxs]==0)&&(fxl>0.01)&&(fxs1>0.01))||
	                    ((buf[cxl][cxs+1]==0)&&(fxl1>0.01)&&(fxs>0.01))||
	                    ((buf[cxl+1][cxs+1]==0)&&(fxl>0.01)&&(fxs>0.01)))
	                   tin = 9;
	                }
	             }
	          }
               switch (tin)
	          {
case 1:           rl = fxl1*buf[cxl][cxs]+fxl*buf[cxl+1][cxs];
	          ru = fxl1*buf[cxl][cxs+1]+fxl*buf[cxl+1][cxs+1];
	          outbuf[bufpos+j] = (short int)(fxs1*rl+fxs*ru+.5);
                  break;

case 2:           /* cubic convolution */
                  t1 = fxs; t2 = t1*t1; t3 = t2*t1;
                  c1 = -0.5*t3+t2-0.5*t1;
                  c2 = 1.5*t3-2.5*t2+1.0;
                  c3 = -1.5*t3+2.0*t2+0.5*t1;
                  c4 = 0.5*t3-0.5*t2;
                  
                  r1 = c1*buf[cxl-1][cxs-1]+c2*buf[cxl-1][cxs]+
                       c3*buf[cxl-1][cxs+1]+c4*buf[cxl-1][cxs+2];
                  r2 = c1*buf[cxl][cxs-1]+c2*buf[cxl][cxs]+
                       c3*buf[cxl][cxs+1]+c4*buf[cxl][cxs+2];
                  r3 = c1*buf[cxl+1][cxs-1]+c2*buf[cxl+1][cxs]+
                       c3*buf[cxl+1][cxs+1]+c4*buf[cxl+1][cxs+2];
                  r4 = c1*buf[cxl+2][cxs-1]+c2*buf[cxl+2][cxs]+
                       c3*buf[cxl+2][cxs+1]+c4*buf[cxl+2][cxs+2];
                  
                  t1 = fxl; t2 = t1*t1; t3 = t2*t1;
                  c1 = -0.5*t3+t2-0.5*t1;
                  c2 = 1.5*t3-2.5*t2+1.0;
                  c3 = -1.5*t3+2.0*t2+0.5*t1;
                  c4 = 0.5*t3-0.5*t2;
                  
                  outbuf[bufpos+j] = (short int)(c1*r1+c2*r2+c3*r3+c4*r4+.5);
                  break;


case 3:           /* cubic spline */
                  t1 = fxs; t2 = t1*t1; t3 = t2*t1;
                  c1 = (-t3+3.0*t2-3.0*t1+1.0)*div6;
                  c2 = (3.0*t3-6.0*t2+4.0)*div6;
                  c3 = (-3.0*t3+3.0*t2+3.0*t1+1.0)*div6;
                  c4 = t3*div6;
                  
                  r1 = c1*buf[cxl-1][cxs-1]+c2*buf[cxl-1][cxs]+
                       c3*buf[cxl-1][cxs+1]+c4*buf[cxl-1][cxs+2];
                  r2 = c1*buf[cxl][cxs-1]+c2*buf[cxl][cxs]+
                       c3*buf[cxl][cxs+1]+c4*buf[cxl][cxs+2];
                  r3 = c1*buf[cxl+1][cxs-1]+c2*buf[cxl+1][cxs]+
                       c3*buf[cxl+1][cxs+1]+c4*buf[cxl+1][cxs+2];
                  r4 = c1*buf[cxl+2][cxs-1]+c2*buf[cxl+2][cxs]+
                       c3*buf[cxl+2][cxs+1]+c4*buf[cxl+2][cxs+2];
                  
                  t1 = fxl; t2 = t1*t1; t3 = t2*t1;
                  c1 = (-t3+3.0*t2-3.0*t1+1.0)*div6;
                  c2 = (3.0*t3-6.0*t2+4.0)*div6;
                  c3 = (-3.0*t3+3.0*t2+3.0*t1+1.0)*div6;
                  c4 = t3*div6;
                  
                  outbuf[bufpos+j] = (short int)(c1*r1+c2*r2+c3*r3+c4*r4+.5);
                  break;

case 9:           if (fxl>=.5) cxl += 1;
	          if (fxs>=.5) cxs += 1;
	          outbuf[bufpos+j] = buf[cxl][cxs];
	          break;
	          }
               }
            else
               {
               if (znoin)
	          {
	          if (cubtype)
	             {
	             if ((int)bbuf[cxl-1][cxs-1]==0||(int)bbuf[cxl+2][cxs-1]==0||
	                 (int)bbuf[cxl-1][cxs+2]==0||(int)bbuf[cxl+2][cxs+2]==0)
	                {
	                /* the zero(s) have to participate */
	                if ((((int)bbuf[cxl-1][cxs-1]==0)&&(fxl1>0.01)&&(fxs1>0.01))||
	                    (((int)bbuf[cxl+2][cxs-1]==0)&&(fxl>0.01)&&(fxs1>0.01))||
	                    (((int)bbuf[cxl-1][cxs+2]==0)&&(fxl1>0.01)&&(fxs>0.01))||
	                    (((int)bbuf[cxl+2][cxs+2]==0)&&(fxl>0.01)&&(fxs>0.01)))
	                   tin = 9;
	                }
	             }
	          else
	             {
	             if ((int)bbuf[cxl][cxs]==0||(int)bbuf[cxl+1][cxs]==0||
	                 (int)bbuf[cxl][cxs+1]==0||(int)bbuf[cxl+1][cxs+1]==0)
	                {
	                /* the zero(s) have to participate */
	                if ((((int)bbuf[cxl][cxs]==0)&&(fxl1>0.01)&&(fxs1>0.01))||
	                    (((int)bbuf[cxl+1][cxs]==0)&&(fxl>0.01)&&(fxs1>0.01))||
	                    (((int)bbuf[cxl][cxs+1]==0)&&(fxl1>0.01)&&(fxs>0.01))||
	                    (((int)bbuf[cxl+1][cxs+1]==0)&&(fxl>0.01)&&(fxs>0.01)))
	                   tin = 9;
	                }
	             }
	          }
               switch (tin)
	          {
case 1:           rl = fxl1*(float)bbuf[cxl][cxs]+fxl*(float)bbuf[cxl+1][cxs];
	          ru = fxl1*(float)bbuf[cxl][cxs+1]+fxl*(float)bbuf[cxl+1][cxs+1];
	          boutbuf[bufpos+j] = (unsigned char)(fxs1*rl+fxs*ru+.5);
                  break;

case 2:           /* cubic convolution */
                  t1 = fxs; t2 = t1*t1; t3 = t2*t1;
                  c1 = -0.5*t3+t2-0.5*t1;
                  c2 = 1.5*t3-2.5*t2+1.0;
                  c3 = -1.5*t3+2.0*t2+0.5*t1;
                  c4 = 0.5*t3-0.5*t2;
                  
                  r1 = c1*(float)bbuf[cxl-1][cxs-1]+c2*(float)bbuf[cxl-1][cxs]+
                       c3*(float)bbuf[cxl-1][cxs+1]+c4*(float)bbuf[cxl-1][cxs+2];
                  r2 = c1*(float)bbuf[cxl][cxs-1]+c2*(float)bbuf[cxl][cxs]+
                       c3*(float)bbuf[cxl][cxs+1]+c4*(float)bbuf[cxl][cxs+2];
                  r3 = c1*(float)bbuf[cxl+1][cxs-1]+c2*(float)bbuf[cxl+1][cxs]+
                       c3*(float)bbuf[cxl+1][cxs+1]+c4*(float)bbuf[cxl+1][cxs+2];
                  r4 = c1*(float)bbuf[cxl+2][cxs-1]+c2*(float)bbuf[cxl+2][cxs]+
                       c3*(float)bbuf[cxl+2][cxs+1]+c4*(float)bbuf[cxl+2][cxs+2];
                  
                  t1 = fxl; t2 = t1*t1; t3 = t2*t1;
                  c1 = -0.5*t3+t2-0.5*t1;
                  c2 = 1.5*t3-2.5*t2+1.0;
                  c3 = -1.5*t3+2.0*t2+0.5*t1;
                  c4 = 0.5*t3-0.5*t2;
                  
                  boutbuf[bufpos+j] = (unsigned char)(c1*r1+c2*r2+c3*r3+c4*r4+.5);
                  break;


case 3:           /* cubic spline */
                  t1 = fxs; t2 = t1*t1; t3 = t2*t1;
                  c1 = (-t3+3.0*t2-3.0*t1+1.0)*div6;
                  c2 = (3.0*t3-6.0*t2+4.0)*div6;
                  c3 = (-3.0*t3+3.0*t2+3.0*t1+1.0)*div6;
                  c4 = t3*div6;
                  
                  r1 = c1*(float)bbuf[cxl-1][cxs-1]+c2*(float)bbuf[cxl-1][cxs]+
                       c3*(float)bbuf[cxl-1][cxs+1]+c4*(float)bbuf[cxl-1][cxs+2];
                  r2 = c1*(float)bbuf[cxl][cxs-1]+c2*(float)bbuf[cxl][cxs]+
                       c3*(float)bbuf[cxl][cxs+1]+c4*(float)bbuf[cxl][cxs+2];
                  r3 = c1*(float)bbuf[cxl+1][cxs-1]+c2*(float)bbuf[cxl+1][cxs]+
                       c3*(float)bbuf[cxl+1][cxs+1]+c4*(float)bbuf[cxl+1][cxs+2];
                  r4 = c1*(float)bbuf[cxl+2][cxs-1]+c2*(float)bbuf[cxl+2][cxs]+
                       c3*(float)bbuf[cxl+2][cxs+1]+c4*(float)bbuf[cxl+2][cxs+2];
                  
                  t1 = fxl; t2 = t1*t1; t3 = t2*t1;
                  c1 = (-t3+3.0*t2-3.0*t1+1.0)*div6;
                  c2 = (3.0*t3-6.0*t2+4.0)*div6;
                  c3 = (-3.0*t3+3.0*t2+3.0*t1+1.0)*div6;
                  c4 = t3*div6;
                  
                  boutbuf[bufpos+j] = (unsigned char)(c1*r1+c2*r2+c3*r3+c4*r4+.5);
                  break;

case 9:           if (fxl>=.5) cxl += 1;
	          if (fxs>=.5) cxs += 1;
	          boutbuf[bufpos+j] = bbuf[cxl][cxs];
	          break;
	          }
               }
            
skip:	    pipos += fidel;
	    pjpos += fjdel;
	    }
	 jpos += nloop;
	 bufpos += nloop;
	 }
      if (istrip<0)
         {
         if (filetype) zvwrit(o_unit,outbuf,"LINE",outcount,
                  "SAMP",1,"NSAMPS", newns, 0);
         else zvwrit(o_unit,boutbuf,"LINE",outcount,
                  "SAMP",1,"NSAMPS", newns, 0);
         outcount++;
         }
      else
         {
         if (filetype) fwrite(outbuf,2,newnswrite,fileptr);
         else fwrite(boutbuf,1,newnswrite,fileptr);
         }
      }
   
   if (filetype)
      {
      mz_free2((unsigned char **)buf,nlp6);
      free(outbuf);
      }
   else
      {
      mz_free2((unsigned char **)bbuf,nlp6);
      free(boutbuf);
      }
   
   return;
}
   
   main44()
{
   int    newsl,newss,newnl,newns,interp,cref,dum,filetype;
   
   double *outl,*outs,*inl,*ins;
   int i,j,labsiz,lnl,lns,ncol,nolab,itype,nrec,tin;
   int gridl,grids,iline,vmemsize,dummy,filect,filedf;
   int sl,ss,nl,ns,nlp6,nsp6,jtop,stripn,writepix;
   int reflabsiz,reflnl,reflns,labrval,bufpos,bufix;
   int labnl,labns,gtfirst,len,istrip,jstrip,outcount;
   int snewsl,snewss,snewnl,snewns,linc,sinc,snewnswrite;
   unsigned char *outbuf;
   long int lbufsiz;
   char *p,*labelstr,scalestr[50],transstr[133],tmpfile[100];
   char tmpfilex[150],tstr[2];
   char *ms_find();
   double gridck,ldel,sdel,rl,ru;
   double lmin,smin,lmax,smax;
   double t[6],tinv[6],tout[6],toutinv[6],corner[4];
   double b,d,bcor,dcor,voff,scale1,scale2;
   FILE *tf0,*tf1,*tf2,*tf3,*tf4,*tf5,*tf6,*tf7,*tf8,*tf9,*tf10,
    *tf11,*tf12,*tf13,*tf14,*tf15,*tf16,*tf17,*tf18,*tf19,*tf20;
   FILE *fileptr;
   
   int status,parmcnt,gridcnt,gridIn,ibis;
   int ntiepp,tiepdef,znoin,count,colcount,coldef,nahm1,navm1;
   int cols[4];
   double *rpar;
   
   /* initialize, fetch params */

   zifmessage("geomv version 16-jul-07");
   
   open_files(&filetype);
   writepix = filetype+1;
   
   status = zvpcnt("inp",&parmcnt);
   zvparm("tmpfile",tmpfile,&filect,&filedf,1,99);
   zvp("vmemsize",&vmemsize,&dummy);
   if (parmcnt>1)
      {
      zvparm("cols",cols,&colcount,&coldef,4,0);
      status = zvunit(&gridIn,"inp",2,0);
      status = IBISFileOpen(gridIn,&ibis,IMODE_READ,0,0,0,0);
      if (status!=1) IBISSignalU(gridIn,status,1);
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[0]);
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[1]);
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[2]);
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[3]);
      IBISFileGet(ibis,"nr",&nrec,1,1,0);
      mz_alloc1((unsigned char **)&outl,nrec,8);
      mz_alloc1((unsigned char **)&outs,nrec,8);
      mz_alloc1((unsigned char **)&inl,nrec,8);
      mz_alloc1((unsigned char **)&ins,nrec,8);
      status = IBISColumnRead(ibis,outl,cols[0],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      status = IBISColumnRead(ibis,outs,cols[1],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      status = IBISColumnRead(ibis,inl,cols[2],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      status = IBISColumnRead(ibis,ins,cols[3],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      }
   else
      {
      mz_alloc1((unsigned char **)&rpar,1625,8);
      zvparmd("tiepoint",rpar,&ntiepp,&tiepdef,1625,0);
      if (ntiepp==0) zmabend("No tiepoint input");
      if (ntiepp>=1625)
        zmabend("Too many grid tiepoints for parm dataset, use IBIS file");
      /* put rpar into the column format */
      nrec = ntiepp/4;
      mz_alloc1((unsigned char **)&outl,nrec,8);
      mz_alloc1((unsigned char **)&outs,nrec,8);
      mz_alloc1((unsigned char **)&inl,nrec,8);
      mz_alloc1((unsigned char **)&ins,nrec,8);
      for (i=0;i<nrec;i++)
         {
         outl[i] = rpar[i*4];
         outs[i] = rpar[i*4+1];
         inl[i] = rpar[i*4+2];
         ins[i] = rpar[i*4+3];
         }
      free(rpar);
      }
   if (parmcnt>2) /* reference image */
      {
      status = gtgetlab("inp",3,&labelstr,&labnl,&labns);
      gtfirst = status==1;
      if (gtfirst)
         {
         len = strlen(labelstr);
         for (i=0;i<len;i++) labelstr[i] = toupper(labelstr[i]);
         status = geofix(labelstr,t,tinv,labnl,labns,corner);
         if (status!=1)
            zmabend("Failed to get mapping from GeoTIFF label, first input");
         }
      }
   
   znoin = zvptst("znoin");
   interp = 1;
   if (zvptst("bilin")) interp = 1;
   if (zvptst("noin")) interp = 9;
   if (zvptst("cubconv")) interp = 2;
   if (zvptst("cubsplin")) interp = 3;
   
   /* adjust grid 0.5 for algorithm */

   for (i=0;i<nrec;i++)
      {
      outl[i] -= 0.5;
      outs[i] -= 0.5;
      inl[i] -= 0.5;
      ins[i] -= 0.5;
      }
   newsl = sline - 1;
   newss = ssamp - 1;
   newnl = nline;
   newns = nsamp;
   lnl = inpline;
   lns = inpsamp;
   itype = 2;
   
   /* determine the grid geometry */
   /* the large loop finds min and max extremes in input */
   /* can't just do cell corners because output might be small 
      subset of large cell */

   gridck = outl[0];
   for (i=0;;i++)
      {
      grids = i;
      if (outl[i]!=gridck) break;
      }
   gridl = nrec/grids;
   if (gridl*grids!=nrec||gridl<2||grids<2)
      zmabend("warp grid not rectangular");
   for (i=1;i<gridl;i++)
      for (j=0;j<grids;j++)
         {
         if (outs[i*grids+j]==outs[j]) continue;
         zmabend("warp grid not rectangular");
         }
   for (i=0;i<gridl;i++)
      for (j=1;j<grids;j++)
         {
         if (outl[i*grids+j]==outl[i*grids]) continue;
         zmabend("warp grid not rectangular");
         }
   if ((double)newsl<(outl[0]-1)||(double)(newsl+newnl)>(outl[nrec-1]+1)||
      (double)newss<(outs[0]-1)||(double)(newss+newns)>(outs[nrec-1]+1))
	 zmabend("warp grid does not cover output");
   ldel = (outl[nrec-1]-outl[0])/(double)(gridl-1);
   sdel = (outs[nrec-1]-outs[0])/(double)(grids-1);
   for (i=1;i<gridl;i++)
      if (fabs(outl[i*grids]-outl[(i-1)*grids]-ldel)>0.002)
         zmabend("warp grid not evenly spaced vertically");
   for (i=1;i<grids;i++)
      {
      if (fabs(outs[i]-outs[i-1]-sdel)>0.002)
         zmabend("warp grid not evenly spaced horizontally");
      }
   printf("Warp grid OK: nah = %d nav = %d\n",grids-1,gridl-1);
   nahm1 = grids-2;
   navm1 = gridl-2;
   
   jtop = newss+newns;
   get_extrema(newsl,newss,newnl,newns,nahm1,navm1,grids,ldel,sdel,
      outl,outs,inl,ins,jtop,&lmin,&lmax,&smin,&smax);
   
   /* read into buf, allow extra space for cubic spline */

   sl = MAX((int)lmin-3,0);
   ss = MAX((int)smin-3,0);
   nl = MIN((int)lmax+3,lnl)-sl;
   ns = MIN((int)smax+3,lns)-ss;
   nlp6 = nl+6; nsp6 = ns+6;
     
   mz_alloc1((unsigned char **)&outbuf,2*newns+200,1); /*extra needed for strips*/
   if (nl<=0||ns<=0)
      {
      printf("input range: (sl,ss,nl,ns)=(%d,%d,%d,%d)\n",sl,ss,nl,ns);
      printf("writing all 0 image\n");
      for (i=0;i<2*newns;i++) outbuf[i] = (unsigned char)0;
      for (i=0;i<newnl;i++)
         {
         zvwrit(o_unit,outbuf,"LINE",i+1,
               "SAMP",1,"NSAMPS", newns, 0);
         }
      goto fin;
      }
   lbufsiz = (long int)nlp6*(long int)nsp6*(long int)writepix;
   if (lbufsiz<(long int)1) zmabend("anomalous value for input area");
   if (lbufsiz>(long int)vmemsize)
      {
      stripn = MIN((int)(1.5*sqrt((double)lbufsiz/(double)vmemsize)+1.0),21);
      printf("requested memory %d, processing in %d strips\n",(int)lbufsiz,stripn);
      strcpy(tstr,"x");
      sinc = newns/stripn+1;
      snewnswrite = sinc;
      outcount = 1;
      for (istrip=0;istrip<stripn;istrip++)
         {
         strcpy(tmpfilex,tmpfile);
         tstr[0] = (char)(istrip+97);
         strcat(tmpfilex,tstr);
         switch (istrip)
            {
            case 0: tf0 = fopen(tmpfilex,"w"); fileptr = tf0; break;
            case 1: tf1 = fopen(tmpfilex,"w"); fileptr = tf1; break;
            case 2: tf2 = fopen(tmpfilex,"w"); fileptr = tf2; break;
            case 3: tf3 = fopen(tmpfilex,"w"); fileptr = tf3; break;
            case 4: tf4 = fopen(tmpfilex,"w"); fileptr = tf4; break;
            case 5: tf5 = fopen(tmpfilex,"w"); fileptr = tf5; break;
            case 6: tf6 = fopen(tmpfilex,"w"); fileptr = tf6; break;
            case 7: tf7 = fopen(tmpfilex,"w"); fileptr = tf7; break;
            case 8: tf8 = fopen(tmpfilex,"w"); fileptr = tf8; break;
            case 9: tf9 = fopen(tmpfilex,"w"); fileptr = tf9; break;
            case 10: tf10 = fopen(tmpfilex,"w"); fileptr = tf10; break;
            case 11: tf11 = fopen(tmpfilex,"w"); fileptr = tf11; break;
            case 12: tf12 = fopen(tmpfilex,"w"); fileptr = tf12; break;
            case 13: tf13 = fopen(tmpfilex,"w"); fileptr = tf13; break;
            case 14: tf14 = fopen(tmpfilex,"w"); fileptr = tf14; break;
            case 15: tf15 = fopen(tmpfilex,"w"); fileptr = tf15; break;
            case 16: tf16 = fopen(tmpfilex,"w"); fileptr = tf16; break;
            case 17: tf17 = fopen(tmpfilex,"w"); fileptr = tf17; break;
            case 18: tf18 = fopen(tmpfilex,"w"); fileptr = tf18; break;
            case 19: tf19 = fopen(tmpfilex,"w"); fileptr = tf19; break;
            case 20: tf20 = fopen(tmpfilex,"w"); fileptr = tf20; break;
            }
         snewss = newss+istrip*sinc;
         snewns = sinc;
         if ((snewss+snewns)>(newss+newns)) snewns = newss+newns-snewss;
         
         linc = newnl/stripn+1;
         for (jstrip=0;jstrip<stripn;jstrip++)
            {
            snewsl = newsl+jstrip*linc;
            snewnl = linc;
            if ((snewsl+snewnl)>(newsl+newnl)) snewnl = newsl+newnl-snewsl;

            jtop = snewss+snewns;
            get_extrema(snewsl,snewss,snewnl,snewns,nahm1,navm1,grids,ldel,sdel,
               outl,outs,inl,ins,jtop,&lmin,&lmax,&smin,&smax);
            sl = MAX((int)lmin-3,0);
            ss = MAX((int)smin-3,0);
            nl = MIN((int)lmax+3,lnl)-sl;
            ns = MIN((int)smax+3,lns)-ss;
            nlp6 = nl+6; nsp6 = ns+6;
            
            if (nl<=0 || ns<=0)
               {
               /* this little segment all 0 -- uncover write for dev case*/
               /*printf("input range: (sl,ss,nl,ns)=(%d,%d,%d,%d)\n",sl,ss,nl,ns);
               printf("writing all 0 piece\n");*/
               for (j=0;j<2*snewnswrite;j++) outbuf[j] = (unsigned char)0;
               for (j=0;j<snewnl;j++)
                  fwrite(outbuf,writepix,snewnswrite,fileptr);
               }
            else process_block(sl,ss,nl,ns,nlp6,nsp6,snewsl,snewss,
               snewnl,snewns,outl,outs,inl,ins,jtop,nahm1,navm1,grids,interp,
               znoin,ldel,sdel,istrip,fileptr,snewnswrite,filetype);
            }
         rewind(fileptr);
         fclose(fileptr);
         }
      /* write strips to vicar file */
      for (istrip=0;istrip<stripn;istrip++)
         {
         strcpy(tmpfilex,tmpfile);
         tstr[0] = (char)(istrip+97);
         strcat(tmpfilex,tstr);
         switch (istrip)
            {
            case 0: tf0 = fopen(tmpfilex,"r"); break;
            case 1: tf1 = fopen(tmpfilex,"r"); break;
            case 2: tf2 = fopen(tmpfilex,"r"); break;
            case 3: tf3 = fopen(tmpfilex,"r"); break;
            case 4: tf4 = fopen(tmpfilex,"r"); break;
            case 5: tf5 = fopen(tmpfilex,"r"); break;
            case 6: tf6 = fopen(tmpfilex,"r"); break;
            case 7: tf7 = fopen(tmpfilex,"r"); break;
            case 8: tf8 = fopen(tmpfilex,"r"); break;
            case 9: tf9 = fopen(tmpfilex,"r"); break;
            case 10: tf10 = fopen(tmpfilex,"r"); break;
            case 11: tf11 = fopen(tmpfilex,"r"); break;
            case 12: tf12 = fopen(tmpfilex,"r"); break;
            case 13: tf13 = fopen(tmpfilex,"r"); break;
            case 14: tf14 = fopen(tmpfilex,"r"); break;
            case 15: tf15 = fopen(tmpfilex,"r"); break;
            case 16: tf16 = fopen(tmpfilex,"r"); break;
            case 17: tf17 = fopen(tmpfilex,"r"); break;
            case 18: tf18 = fopen(tmpfilex,"r"); break;
            case 19: tf19 = fopen(tmpfilex,"r"); break;
            case 20: tf20 = fopen(tmpfilex,"r"); break;
            }
         }
      for (iline=0;iline<newnl;iline++)
         {
         bufix = snewnswrite*writepix;
         switch (stripn-1)    /* no breaks in switch cases */
            {
            case 20: fread(&outbuf[20*bufix],writepix,snewnswrite,tf20);
            case 19: fread(&outbuf[19*bufix],writepix,snewnswrite,tf19);
            case 18: fread(&outbuf[18*bufix],writepix,snewnswrite,tf18);
            case 17: fread(&outbuf[17*bufix],writepix,snewnswrite,tf17);
            case 16: fread(&outbuf[16*bufix],writepix,snewnswrite,tf16);
            case 15: fread(&outbuf[15*bufix],writepix,snewnswrite,tf15);
            case 14: fread(&outbuf[14*bufix],writepix,snewnswrite,tf14);
            case 13: fread(&outbuf[13*bufix],writepix,snewnswrite,tf13);
            case 12: fread(&outbuf[12*bufix],writepix,snewnswrite,tf12);
            case 11: fread(&outbuf[11*bufix],writepix,snewnswrite,tf11);
            case 10: fread(&outbuf[10*bufix],writepix,snewnswrite,tf10);
            case 9: fread(&outbuf[9*bufix],writepix,snewnswrite,tf9);
            case 8: fread(&outbuf[8*bufix],writepix,snewnswrite,tf8);
            case 7: fread(&outbuf[7*bufix],writepix,snewnswrite,tf7);
            case 6: fread(&outbuf[6*bufix],writepix,snewnswrite,tf6);
            case 5: fread(&outbuf[5*bufix],writepix,snewnswrite,tf5);
            case 4: fread(&outbuf[4*bufix],writepix,snewnswrite,tf4);
            case 3: fread(&outbuf[3*bufix],writepix,snewnswrite,tf3);
            case 2: fread(&outbuf[2*bufix],writepix,snewnswrite,tf2);
            case 1: fread(&outbuf[bufix],writepix,snewnswrite,tf1);
            case 0: fread(outbuf,writepix,snewnswrite,tf0);
            }
         zvwrit(o_unit,outbuf,"LINE",outcount,
                  "SAMP",1,"NSAMPS", newns, 0);
         outcount++;
         }
      }
   else
      {
      printf("requested memory %d\n",(int)lbufsiz);
      process_block(sl,ss,nl,ns,nlp6,nsp6,newsl,newss,
         newnl,newns,outl,outs,inl,ins,jtop,nahm1,navm1,grids,interp,
         znoin,ldel,sdel,-1,0,newns,filetype);
      }
   
   /* update the geotiff label, gtreplab reopens for update */
   /* the tout[] solutions are in GeoTIFF coordinates, similar
   to gtsize.  Here only deal with a shift. */
   
fin:
   zvclose(o_unit,0);
   if (parmcnt>2)
      {
      b = (double)sline;
      d = (double)ssamp;
      
      bcor = t[0]*b+t[1]*d+t[2];
      dcor = t[3]*b+t[4]*d+t[5];
      p = ms_find(labelstr,"GTRASTERTYPEGEOKEY=2");
      if (p!=0) voff = 1.0; else voff = 0.5;
   
      toutinv[0] = tinv[3];
      toutinv[1] = tinv[4];
      toutinv[2] = 1.0-voff-toutinv[0]*bcor-toutinv[1]*dcor;
      toutinv[3] = tinv[0];
      toutinv[4] = tinv[1];
      toutinv[5] = 1.0-voff-toutinv[3]*bcor-toutinv[4]*dcor;
      
      scale2 = -t[3];
      scale1 = t[1];
      
      invertmap(toutinv,tout);
      scalefmt(scalestr,scale1,scale2);
      trnsfmt(transstr,tout);
      gtreplab("OUT",1,labelstr,0,1,toutinv,scalestr,transstr);
      zvclose(o_unit,0);
      }
   
   zvclose(i_unit,0);
   return 0;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create geomv.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM geomv

   To Create the build file give the command:

		$ vimake geomv			(VMS)
   or
		% vimake geomv			(Unix)


************************************************************************/


#define PROGRAM	geomv

#define MODULE_LIST geomv.c

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
$ create geomv.pdf
process help=*
 !
 PARM INP     TYPE=STRING COUNT=1:3
 PARM OUT     TYPE=STRING COUNT=1
 PARM COLS    TYPE=INTEGER COUNT=4 DEFAULT=(1,2,3,4)
 PARM SIZE    TYPE=INTEGER COUNT=0:4 DEFAULT=(1,1,0,0)
 PARM SL      TYPE=INTEGER COUNT=0:1 DEFAULT=1
 PARM SS      TYPE=INTEGER COUNT=0:1 DEFAULT=1
 PARM NL      TYPE=INTEGER COUNT=0:1 DEFAULT=0
 PARM NS      TYPE=INTEGER COUNT=0:1 DEFAULT=0
 PARM ZNOIN   KEYWORD COUNT=0:1 VALID=ZNOIN DEFAULT=--
 PARM INTERP  TYPE=KEYWORD COUNT=0:1 +
      VALID=(NOIN,BILIN,CUBCONV,CUBSPLIN) DEFAULT=BILIN
 
 PARM NAH     TYPE=INTEGER,VALID=(1:5000) default=1
 PARM NAV     TYPE=INTEGER,VALID=(1:5000) default=1
 PARM TIEPOINT TYPE=REAL,COUNT=0:600 default=--
 PARM PARMS   TYPE=STRING  COUNT=0:1  DEFAULT=--
 PARM VMEMSIZE TYPE=INTEGER COUNT=0:1 DEFAULT=300000000
 PARM TMPFILE TYPE=STRING COUNT=0:1 DEFAULT="xxxgeomvtmp87"
 !
 END-PROC
!
! HELP TEXT FOR GEOMV
.TITLE
GEOMV - Program for high resolution geometric transformations on images.
.HELP
PURPOSE
     GEOMV is a VICAR applications program which makes geometric
     changes in pictures.  It can be used for many purposes including
     correcting geometric distortion, increasing picture size, reducing 
     picture size, and rotating a picture.  The motive for creating
     GEOMV was to use approximately 1000 x 1000 geometric transformation
     grids to allow modelling terrain, etc.  The following are the
     principal differences with respect to the programs LGEOM and
     GEOMV:

	1. No limits on size of input/output pictures in GEOMV.
	   For really large images the program automatically goes
           into a strip processing mode, see parameters VMEMSIZE
           and TMPFILE.
	2. No limits on size of interpolation grid in GEOMV
	   except due to virtual memory limits.  There is about
	   a thousandfold increase in capacity vs the older geom
	   programs.  The large grids can only be transferred by
	   means of IBIS files.  TIECONV has been designed as a
	   companion program to handle large numbers of irregular
	   tiepoints as input and large grids as output.  A 4000
	   by 4000 image was transformed by a 500 x 500 grid
	   in 99 seconds on a SPARCstation 20.
	3. GEOMV uses double precision throughout.  There are 
	   frequent differences in LGEOM and GEOMV results for
	   cells larger than 150 pixels.  It appears that GEOMV is
	   much more accurate.  Making LGEOM more accurate would
	   probably make it slower than GEOMV.
	4. Preparation of "sharp edges" for mosaicking is
           available using ZNOIN (GEOMV) and NOIZ (LGEOM).  The
           two programs may do it differently, if so there should 
           probably be a standardization of this operation.  In
           GEOMV, any 0 input to the bilinear interpolation (the
           four neighbors) causes nearest neighbor interpolation.
	5. GEOMV is a little slower than LGEOM (249 seconds for an 
	   8000 x 8000 image vs 221 seconds for LGEOM).
	6. GEOMV has no bad cases.  The old restriction of less
	   than 85 degree rotation in MGEOM is gone.  Reading of
	   data for grid cells one at a time is gone.
	7. GEOMV is written entirely in C and uses dynamic allocation
	   for all data dependent arrays.
	8. The parameters NAH and NAV are ignored but are kept for
	   compatibility with the profusion of data sets.  The actual
	   values are calculated from the grid while the grid is
	   checked for rectangularity.
	9. GEOMV precalculates from the grid what part of the input
	   is needed.  For example, if you GEOMV a tiny portion of
	   Australia, then the program will only read in the tiny
	   portion of huge input that is needed.  This precalculation
	   is not affected by the fact that the grid might cover a
	   much larger output.  The result is not only speedup of
	   smaller cases but it also allows truly huge GEOM's to
	   be calculated in the strip mode.
       10. Grid spacing requirements differ from LGEOM.  The output
           points do not have to be integral, but the spacing has
           to be uniform in each direction.  This is easy to meet
           for mathematically generated grids, or for grids that
           come from routine TIECONV.  Incidentally, LGEOM does not
           presently check for non-integral output tiepoints, but
           seems to calculate an erroneous output.
       11. Automatic GeoTIFF labelling is supported.
	   
CALL
     geomv (INPUT,GRID) OUTPUT SIZE '(QUALIFIERS) TIEPOINT-GRID
  WHERE:
     INPUT          is the input data set.
     GRID           is an IBIS file containing a warp grid.
     OUTPUT         is the output data set.
     SIZE           is a VICAR size field for the output file
     QUALIFIERS     consist of any of the following keywords:
          NOIN          no interpolation is to be done.
          ZNOIN         no interpolation for points with 0 DN.
     TIEPOINT-GRID      is an alternative form for the warp grid.

     The two forms of the warp grid will now be described.

     The IBIS-file form of the grid contains four columns of
single or double precision tiepoint records.  The columns in
order are (newline,newsamp,oldline,oldsamp); or the column order
can be user specified.  NAH and NAV will be calculated from the
grid and the grid must be rectangular.

     The TIEPOINT-GRID is a set of points describing the relation
of the output file to that of the input file using the keyword
TIEPOINT and optionally NAH and NAV which are ignored.
   NAH=nah  NAV=nav
     tiepoint=(nl1,ns1,ol1,os1,nl2,ns2,ol2,os2,...
                   ...nlk,nsk,olk,osk)

where the grid is rectangular in the output image space, nah is
the number of grid cells horizontally (across the top in the sample
direction), nav is the number of grid cells vertically in the output
image space, the point pairs (nli,nsi,oli,osi) are line-sample
coordinates in the output (new) and input (old) spaces respectively.
the number of pairs k must equal (nah+1)*(nav+1).  The grid must be
perfectly rectangular in the output image space (the rows and
columns must be perfectly horizontal and vertical respectively).
Each direction must be evenly spaced as well, but all values can
br fractional.  The keywords NAH and NAV are ignored and the true
values are calculated from the grid.

The input image may either be byte or halfword data.  The data format is taken
from the VICAR label of the input file.  The output image has the same data 
format (byte or halfword) as the input image.  

Truly large cases (I envision 100 GB) are done by computing a strip size
that depends on the parameter VMEMSIZE.  VMEMSIZE is presently defaulted to
300 MB under the assumption that a 2 GB memory can handle that pretty easily
without too much paging.  But it can be reset larger or smaller as needed and
should be defaulted larger in the future (say when 10 GB memories are common).
Then, if the warp area of the input image is larger than VMEMSIZE, the program
shifts into a "strip processing" mode using the filename given by parameter
TMPFILE followed by a sequence of digits to create one file for each strip.
Then all of the strips are concatenated to give the final output.  Each piece
of each strip is designed to fit within the user provided or defaulted virtual
memory size.  Do not use a link for tmpfile.  Instead, you can use a path/filename
for the TMPFILE.  This is because multiple files will be created with TMPFILE as a
root.  You must delete the TMPFILE files after the run.
  
OPERATION

GEOMV calculates what part of the input is needed for the output, and
reads that entire amount into (virtual) memory.  Images up to 8000
square can be handled easily by the current (1999) generation of
workstations.  Larger images can be handled by sectioning of the output
into a set of tiles, warping them, and then mosaicking the tiles.
The tiles are guaranteed to butt together perfectly.

Unlike MGEOM, GEOMV does not tile the input for the warp grid cells.
It holds all of the cells for a cell-row of output in memory and holds
all of the input image in memory.  Thus there is little penalty for
having a vast number of grid cells.

PERFORMANCE

A 4000 by 4000 image was transformed by a 500 x 500 grid
in 99 seconds on a SPARCstation 20.  Reducing the grid to 30 x 30
cut the time to 39 seconds.  This shows that the use of a large 
grid doesn't penalize the time too much.

.PAGE
Restrictions
------------

The output grid must cover the output image.  The program gives an
error message and stops if it doesn't.  The output grid can be larger.
There are no restrictions on the input grid. 

THE OUTPUT GRID MUST ALSO BE UNIFORMLY SPACED IN EACH DIRECTION
(MORE RESTRICTIVE THAN LGEOM).  THE SPACING VERTICALLY DOES
NOT HAVE TO EQUAL THE SPACING HORIZONTALLY THOUGH.
ON THE OTHER HAND, THE OUTPUT GRID VALUES CAN BE FRACTIONAL (LGEOM
REQUIRES WHOLE NUMBERS).  THESE REQUIREMENTS ARE EASY TO MEET IF THE
GRID IS GENERATED MATHEMATICALLY OR IF A PROGRAM SUCH AS TIECONV IS
USED.

.PAGE
Original Programmer: A. L. Zobrist, 17 Jul. 1999
Current Cognizant Programmer: A. L. Zobrist
25 April 2007 Change to 2 dimensional arrays: A. L. Zobrist
25 April 2007 Do strips if memory too small, handles huge cases: A. L. Zobrist
25 April 2007 Add cubic convolution and cubic spline: A. L. Zobrist

.LEVEL1
.VARI INP
Input file name, second file
Optional grid (IBIS format)
,third file GeoTIFF reference
.VARI OUT
Output file name
.VARI SIZE
Standard VICAR Size Field
.VARI SL
Starting line for output
.VARI SS
Starting sample for output
.VARI NL
Number of lines for output
* See restrictions
.VARI NS
Number of samples for output
* See restrictions
.VARI INTERP
interpolation options
Valid: NOIN,ZNOIN,BILIN,CUBCONV,CUBSPLIN
.VARI FORMAT
FORMAT is ignored.
.VARI NAH
ignored, will calculate
from grid
.VARI NAV
ignored, will calculate
from grid
.VARIABLE COLS
Columns to use from
optional IBIS file.
.VARI TIEPOINT
grid corner tiepoints in
rows NL1,NS1,OL1,OS1,...
.VARI PARMS
previously saved parameter
dataset
.VARI VMEMSIZE
max allocation for input
image, larger case goes
to "strip process" mode
.VARI TMPFILE
temp filename root for
"strip process" mode
.LEVEL2
.VARI INP
Input file name.  This parameter is input as:
     INP=innam
where "innam" is the input file name.

The second file, if given, is an IBIS file containing a trans-
formation grid in four columns specified by the COLS parameter.
This allows large grids, say 500 x 500 or 1000 x 1000.

The third file, if given, is a VICAR image containing a GeoTIFF
label.  In this case, the output image is assumed to have the
same coordinates as the GeoTIFF labelled image except for the
translation produced by the (sl,ss) coordinates.  This translation
is put into the GeoTIFF coordinate information and the GeoTIFF
label is added to the output.  For an illustration of Automatic
GeoTIFF labelling, see the two PDF's included in the geomv.com
file named gtwarp.pdf and tstgtwarp.pdf.

.VARI OUT
Output and intermediate file names. This parameter is input as:
     OUT=outnam
where:
"outnam" is the output file name, and

.VARI SIZE
The size field is specified with four arguments,
     SIZE=(a,b,c,d)
where:
a is the starting line number of the output picture.
b is the starting sample of the output picture.
c is the number of lines, and
d is the number of samples
For example, SIZE=(1,1,40,50)
would create an output picture of size 40 lines by 50 bytes.
The size field can be thought of as a window relative to the output
grid.  The first two values offset the window down and to the right
causing the features in the image to move up and to the left.
.VARI SL
SL can be used to specify the starting line of the output picture.
This is actually a coordinate relative to the output grid, therefore,
it offsets the output picture by (SL - 1.)  The default for SL is 1.
.VARI SS
SS can be used to specify the starting sample of the output picture.
This is actually a coordinate relative to the output grid, therefore,
it offsets the output picture by (SS - 1.)  The default for SS is 1.
.VARI NL
NL can be used in conjunction with NS in place of the SIZE
parameter to specify the size of the output picture.  It simply
represents the number of lines for output.
* See restrictions for more information
.VARI NS
NS can be used in conjunction with NS in place of the SIZE
parameter to specify the size of the output picture.  It simply
represents the number of bytes for output.
* See restrictions for more information
.VARI INTERP
This parameter has four valid keyword values: NOIN, BILIN,
CUBCONV, and CUBSPLIN

NOIN means no interpolation.   The default method (used when neither
keyword is specified) for computing the
DN values of the output picture is to use a bi-linear interpolation
on the four nearest neighbors in the input picture.  With NOIN, the
value of the nearest point is simply used.
For example, say a point in the output picture was determined
to have come from point (R,P) in the input picture.  Since R and P
are real values, we must somehow calculate a DN value for that
point.  Take IR and IP as the truncated values.  We then have
          VAL1                                 VAL2
           *                                    *
         (IR,IP)                              (IR,IP+1)
                     POINT
                       *
                     (R,P)
          VAL3                                 VAL4
           *                                    *
         (IR+1,IP)                           (IR+1,IP+1)
Here, POINT is the result of a bilinear interpolation using
VAL1, VAL2, VAL3, and VAL4.
If NOIN is specified, then POINT would be VAL1, the nearest
neighbor.

CUBCONV:  (reference Goshtasby on the web)
CUBSPLIN: (reference Goshtasby on the web)

ZNOIN specifies that an interpolation is done except
when one or more of the points used has a value equal to zero. 
In that case the nearest neighbor method is used.
This allows preparation of sharp edges (no interpolation rolloff)
for mosaicking.

.VARI FORMAT
The format is obtained from the input image label. 
.VARI NAH
the nah is number of grid cells horizontally, the number of tiepoints 
across is one larger (nah+1).
.VARI NAV
the nav is number of grid cells vertically, the number of tiepoints
vertically is one larger (nav+1).
.VARIABLE COLS
    COLS=(C1,C2,C3,C4)   Columns in the IBIS tabular file that
			 contain the tiepoints.  C1 has new line,
			 C2 has new sample, C3 has old line, and
			 C4 has old sample.  This parameter is used
			 only if the IBIS file input is given.  The
			 defaults are (1,2,3,4) and they are automat-
			 ically given if a TIECONV type program is 
			 used.

.VARI TIEPOINT
There are four real numbers for each tiepoint , the first two are the
line-sample coordinate in the output, the second two are the line-sample
coordinate in the input which is mapped to the point in the output.  There
must be (nah+1)*(nav+1) tiepoints (quadruple)s aligned in a perfectly
horizontal and vertical grid.  THE OUTPUT GRID MUST ALSO BE UNIFORMLY
SPACED IN EACH DIRECTION.  THE SPACING VERTICALLY DOES NOT HAVE TO
EQUAL THE SPACING HORIZONTALLY THOUGH (MORE RESTRICTIVE THAN LGEOM).
ON THE OTHER HAND, THE OUTPUT GRID VALUES CAN BE FRACTIONAL (LGEOM
REQUIRES WHOLE NUMBERS).  THESE REQUIREMENTS ARE EASY TO MEET IF THE
GRID IS GENERATED MATHEMATICALLY OR IF A PROGRAM SUCH AS TIECONV IS USED.


.VARI PARMS
A parameter data set containing the geom parameters.  This file should
have been written by a program which uses the XVP routines for writing
parameter data sets.  This is the most common means by which the parameters
NAH, NAV, and TIEPOINT are passed.
.VARI TMPFILE
If the warp area of the input image is larger than VMEMSIZE, the program
shifts into a "strip processing" mode using the filename given by parameter
TMPFILE followed by a sequence of digits to create one file for each strip.
Then all of the strips are concatenated to give the final output.  Each piece
of each strip is designed to fit within the user provided or defaulted virtual
memory size.  Do not use a link for TMPFILE.  Instead, you can use a path/filename
for the TMPFILE.  This is because multiple files will be created with TMPFILE as a
root.  You must delete the TMPFILE files after the run.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgeomv.pdf
procedure
refgbl $echo
parm version string def="ibis-1"
parm org string def="column"
body
!let _onfail="continue"
let $echo="yes"


!   TEST SCRIPT FOR GEOMV


! Now generate BYTE input data set
 
gen mgtest 10 10 SINC=40 LINC=40
 
! Verify existence of input file
list mgtest

!  Try some copies.
!  Check case of grid bigger than image.
geomv mgtest mgtest1 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20.,+
            20.,1.,20.,1., 20.,20.,20.,20.)
difpic (mgtest1 mgtest)

 
! Perform simple enlargement to 2X size
geomv mgtest mgenlarg + 
   SIZE=(1,1,20,20)+
   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
                20.,1.,10.,1.,20.,20.,10.,10.)

! Print it out
list mgenlarg

 
! Perform 45 degree rotation clockwise with 1.4 times enlargement
geomv mgtest mgrotat + 
   SIZE=(1,1,20,20)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

! Print it out
list mgrotat


!   SUBSET OF ABOVE CASE, THIS WAS AN ERROR CASE UNTIL FIXED 05/00

gen xxxim1 10 10 SINC=40 LINC=40

! Perform 45 degree rotation clockwise with 1.4 times enlargement

GEOMV xxxim1 xxxim3 sl=7 ss=1 +
         nl=6 ns=6 interp=bilin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)
 
list xxxim3 'zer





! Perform test of size field handling 
geomv mgtest mgrotat1 + 
   SIZE=(1,1,20,10)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
             20.,1.,15.,5.,20.,20.,5.,15.)

geomv mgtest mgrotat2 + 
   SIZE=(1,11,20,1)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

geomv mgtest mgrotat3 + 
   SIZE=(1,12,20,9)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

! Concatenate the three images.  
mss (mgrotat1,mgrotat2,mgrotat3) mgrotatA 
difpic (mgrotatA,mgrotat) 


! Perform the same operation, but without interpolation
geomv mgtest mgrotat + 
   SIZE=(1,1,20,20)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
            20.,1.,15.,5.,20.,20.,5.,15.)+
   INTERP=NOIN

! Print it out
list mgrotat


! Perform the same operation, but without interpolation
gen mgtest 10 10 SINC=64 LINC=64
list mgtest
geomv mgtest mgrotat + 
   SIZE=(1,1,20,20)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)+
   'ZNOIN

! Print it out
list mgrotat
 
! Now generate BYTE input data set
 
gen mgtest2 1000 1000 SINC=1 LINC=1
 
! Verify existence of input file
list mgtest2 SIZE=(1,1,15,15)
 
! DO LONG THIN CASE WITH 45 DEG ROTATION.

geomv mgtest2 mgenthin + 
   SIZE=(1,1,2,1000)+
   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000.,+
            2.,1.,1001.,2.,2.,1000.,2.,1001.)

! Print it out
list mgenthin 'NOEJECT
 

! Now do simple tests for half
! Now generate HALF input data set
 
gen mgtest 10 10 SINC=40 LINC=40 'HALF
 
! Verify existence of input file
list mgtest
 

!  Try some copies.
!  Check case of grid bigger than image.
geomv mgtest mgtest1 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20.,+
            20.,1.,20.,1., 20.,20.,20.,20.)
difpic (mgtest1 mgtest)


! Perform simple enlargement to 2X size
geomv mgtest mgenlarg + 
   SIZE=(1,1,20,20)   +
   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
                20.,1.,10.,1.,20.,20.,10.,10.)
 
! Print it out
list mgenlarg
 
! Perform 45 degree rotation clockwise with 1.4 times enlargement
geomv mgtest mgrotat + 
   SIZE=(1,1,20,20)  +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

! Print it out
list mgrotat


! big case with offsets
gen mgtest2 1000 1000 SINC=3 LINC=7

geomv mgtest2 mgenthin + 
   SIZE=(1,1,1000,1000)+
   TIEPOINT=(1.,1.,7.,7.,1.,1000.,1.,1022.,+
            1000.,1.,970.,22.,1000.,1000.,1050.,1060.)

list mgenthin linc=199 sinc=199

! ibis file grid input


! small grid small image testing ibis file pass to geomv

gen mgtest2 400 400 SINC=3 LINC=7
ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=444
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
mf a func=("c1=mod(c1,0.0001)*4000000","c2=mod(c2,0.0001)*4000000")
mf a func=("c3=c1*1.1","c4=c2*1.1")

tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=30,NAV=30,MINL=1.,MINS=1.,MAXL=400.,MAXS=400. +
    'GEOMV

geomv INP=(mgtest2,b) OUT=mgenlarg + 
   SIZE=(1,1,400,400)

list mgenlarg linc=39 sinc=39

! parms file use by GEOMV... have to use the MGEOM version to
! get a parms file since GEOMV keyword to tieconv produces an IBIS
! file in the output, the LGEOM version produces an unevenly spaced
! grid

ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=44
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")

mf a func=("c3=c1*1.1","c4=c2*1.1")

tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=7,NAV=6,MINL=1.,MINS=1.,MAXL=10.,MAXS=10. +
    'MGEOM

geomv INP=mgtest OUT=mgenlarg PARMS=b + 
   SIZE=(1,1,10,10)
list mgenlarg


! ibis file grid input large grid and large image

gen mgtest2 1000 1000 SINC=3 LINC=7
ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=444
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
mf a func=("c1=mod(c1,0.0001)*10000000","c2=mod(c2,0.0001)*10000000")
mf a func=("c3=c1*1.1","c4=c2*1.1")

tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=200,NAV=200,MINL=1.,MINS=1.,MAXL=1000.,MAXS=1000. +
    'GEOMV

geomv INP=(mgtest2,b) OUT=mgenlarg + 
   SIZE=(1,1,1000,1000)

list mgenlarg linc=199 sinc=199

theend>
end-proc
$!-----------------------------------------------------------------------------
$ create devgeomv.pdf
procedure
refgbl $echo
parm version string def="ibis-1"
parm org string def="column"
body
!let _onfail="continue"
let $echo="yes"


!!!!!!!!!!!! THESE ARE DEVELOPER TESTS, COMPARING WITH LGEOM, FOR 
!!!!!!!!!!!! INSTANCE

goto curr


!   TEST SCRIPT FOR GEOMV
!
!
! Now generate BYTE input data set
! 
!gen mgtest 10 10 SINC=40 LINC=40
! 
! Verify existence of input file
!list mgtest
!
!  Try some copies.
!  Check case of grid bigger than image.
!geomv mgtest mgtest1 NAH=1 NAV=1+ 
!   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20.,+
!            20.,1.,20.,1., 20.,20.,20.,20.)
!difpic (mgtest1 mgtest)

!  Check case of grid smaller than image.
!geomv mgtest mgtest1 NAH=1 NAV=1+ 
!   TIEPOINT=(1.,1.,1.,1.,   1.,2.,1.,2.,+
!             2.,1.,2.,1.,   2.,2.,2.,2.)
!difpic (mgtest1 mgtest)

! 
! Perform simple enlargement to 2X size
!geomv mgtest mgenlarg NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
!                20.,1.,10.,1.,20.,20.,10.,10.)
!mgeom mgtest mgenlarg2 NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
!                20.,1.,10.,1.,20.,20.,10.,10.)
 
! Print it out
!list mgenlarg ss=17 nl=3
!list mgenlarg2 ss=17 nl=3
!list mgenlarg2 nl=3
! also diff it
!difpic (mgenlarg mgenlarg2) 
! 
! Perform 45 degree rotation clockwise with 1.4 times enlargement
!geomv mgtest mgrotat + 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)
!lgeom mgtest mgrotat2 NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)
! Print it out
!list mgrotat
!list mgrotat2
!f2 inp=(mgrotat,mgrotat2) out=mgrotat3 func="abs(in1-in2)"
!list mgrotat3

! also diff it
!difpic (mgrotat mgrotat2)

! Perform test of size field handling 
!geomv mgtest mgrotat1 NAH=1 NAV=1+ 
!   SIZE=(1,1,20,10)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)

!geomv mgtest mgrotat2 NAH=1 NAV=1+ 
!   SIZE=(1,11,20,1)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)

!geomv mgtest mgrotat3 NAH=1 NAV=1+ 
!   SIZE=(1,12,20,9)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)

! Concatenate the three images.  
!mss (mgrotat1,mgrotat2,mgrotat3) mgrotatA 
!difpic (mgrotatA,mgrotat) 

!
! Perform the same operation, but without interpolation
! (NOTE BY ALZ: ZNOIN WAS IN THE GEOMA CASE, NOT THE SAME AS NOIN)
!geomv mgtest mgrotat NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)+
!   INTERP=NOIN
!lgeom mgtest mgrotat2 NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)+
!   INTERP=NOIN
! Print it out
!list mgrotat
!list mgrotat2
!f2 inp=(mgrotat,mgrotat2) out=mgrotat3 func="abs(in1-in2)"
!list mgrotat3
!difpic (mgrotat,mgrotat2)
!
! Perform the same operation, but without interpolation
!gen mgtest 10 10 SINC=64 LINC=64
!list mgtest
!geomv mgtest mgrotat NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)+
!   'ZNOIN
!lgeom mgtest mgrotat2 NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)+
!   INTRPZ=NOIZ
! Print it out
!list mgrotat
!list mgrotat2
!f2 inp=(mgrotat,mgrotat2) out=mgrotat3 func="abs(in1-in2)"
!list mgrotat3
!difpic (mgrotat,mgrotat2)
! 
! Now generate BYTE input data set
! 
!gen mgtest2 1000 1000 SINC=1 LINC=1
! 
! Verify existence of input file
!list mgtest2 SIZE=(1,1,15,15)
! 
! DO LONG THIN CASE WITH 45 DEG ROTATION.
!datetime
!geomv mgtest2 mgenthin NAH=1 NAV=1+ 
!   SIZE=(1,1,2,1000)+
!   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000.,+
!            2.,1.,1001.,2.,2.,1000.,2.,1001.)
!datetime
!lgeom mgtest2 mgenthin2 NAH=1 NAV=1+ 
!   SIZE=(1,1,2,1000)+
!   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000.,+
!            2.,1.,1001.,2.,2.,1000.,2.,1001.)
!datetime
! Print it out
!list mgenthin 'NOEJECT nl=2 ns=2
!list mgenthin 'NOEJECT nl=2 ns=2 ss=999
!list mgenthin2 'NOEJECT nl=2 ns=2
!list mgenthin2 'NOEJECT nl=2 ns=2 ss=999
!difpic (mgenthin,mgenthin2)
!f2 inp=(mgenthin,mgenthin2) out=mgenthin3 func="abs(in1-in2)"
!list mgenthin3 'NOEJECT
! 
!gen a 1000 1000
!  Test bilinear interpolation for FR 87169
!geomv							+
!inp=a					+
!out=b					+
!nl=900 ns=900						+
!nav=9							+
!nah=9							+
!tiepoint=(						+
!001,001,900,900,001,100,800,800,001,200,300,300,001,300,300,300,+
!001,400,400,400,001,500,500,500,001,600,600,600,001,700,700,700,+
!001,800,001,800,001,900,001,900,100,001,100,001,+
!100,100,100,100,100,200,600,200,100,300,700,300,100,400,100,400,+
!100,500,500,500,100,600,600,600,100,700,700,700,100,800,800,800,+
!100,900,100,900,		+
!200,001,200,001,200,100,200,100,200,200,200,200,200,300,200,300,+
!200,400,400,400,200,500,500,500,200,600,600,600,200,700,700,700,+
!200,800,200,800,200,900,200,900,300,001,300,001,+
!300,100,300,100,300,200,300,200,300,300,300,300,300,400,300,400,+
!300,500,300,500,300,600,300,600,300,700,300,700,300,800,300,800,+
!300,900,300,900,		+
!400,001,400,001,400,100,400,100,400,200,400,200,400,300,400,300,+
!400,400,400,400,400,500,500,500,400,600,400,600,400,700,400,700,+
!400,800,400,800,400,900,400,900,500,001,500,001,+
!500,100,500,100,500,200,500,200,500,300,500,300,500,400,500,400,+
!500,500,500,500,500,600,500,600,500,700,500,700,500,800,500,800,+
!500,900,500,900,		+
!600,001,600,001,600,100,600,100,600,200,600,200,600,300,600,300,+
!600,400,600,400,600,500,600,500,600,600,600,600,600,700,600,700,+
!600,800,600,800,600,900,600,900,700,001,700,001,+
!700,100,700,100,700,200,700,200,700,300,700,300,700,400,700,400,+
!70,500,700,500,700,600,700,600,700,700,700,700,700,800,700,800,+
!700,900,700,900,		+
!800,001,800,001,800,100,100,100,800,200,200,200,800,300,300,300,+
!800,400,800,400,800,500,800,500,800,600,800,600,800,700,800,700,+
!800,800,800,800,800,900,800,900,900,001,900,001,+
!900,100,900,100,900,200,900,200,900,300,900,300,900,400,400,400,+
!900,500,900,500,900,600,900,600,900,700,900,700,900,800,800,800,+
!900,900,900,900)
!list b (50,50,850,850) linc=100 sinc=100
!geoma							+
!inp=a					+
!out=b2					+
!nl=900 ns=900						+
!nav=9							+
!nah=9							+
!tiepoint=(						+
!001,001,900,900,001,100,800,800,001,200,300,300,001,300,300,300,+
!001,400,400,400,001,500,500,500,001,600,600,600,001,700,700,700,+
!001,800,001,800,001,900,001,900,100,001,100,001,+
!100,100,100,100,100,200,600,200,100,300,700,300,100,400,100,400,+
!100,500,500,500,100,600,600,600,100,700,700,700,100,800,800,800,+
!100,900,100,900,		+
!200,001,200,001,200,100,200,100,200,200,200,200,200,300,200,300,+
!200,400,400,400,200,500,500,500,200,600,600,600,200,700,700,700,+
!200,800,200,800,200,900,200,900,300,001,300,001,+
!300,100,300,100,300,200,300,200,300,300,300,300,300,400,300,400,+
!300,500,300,500,300,600,300,600,300,700,300,700,300,800,300,800,+
!300,900,300,900,		+
!400,001,400,001,400,100,400,100,400,200,400,200,400,300,400,300,+
!400,400,400,400,400,500,500,500,400,600,400,600,400,700,400,700,+
!400,800,400,800,400,900,400,900,500,001,500,001,+
!500,100,500,100,500,200,500,200,500,300,500,300,500,400,500,400,+
!500,500,500,500,500,600,500,600,500,700,500,700,500,800,500,800,+
!500,900,500,900,		+
!600,001,600,001,600,100,600,100,600,200,600,200,600,300,600,300,+
!600,400,600,400,600,500,600,500,600,600,600,600,600,700,600,700,+
!600,800,600,800,600,900,600,900,700,001,700,001,+
!700,100,700,100,700,200,700,200,700,300,700,300,700,400,700,400,+
!70,500,700,500,700,600,700,600,700,700,700,700,700,800,700,800,+
!700,900,700,900,		+
!800,001,800,001,800,100,100,100,800,200,200,200,800,300,300,300,+
!800,400,800,400,800,500,800,500,800,600,800,600,800,700,800,700,+
!800,800,800,800,800,900,800,900,900,001,900,001,+
!900,100,900,100,900,200,900,200,900,300,900,300,900,400,400,400,+
!900,500,900,500,900,600,900,600,900,700,900,700,900,800,800,800,+
!900,900,900,900)
!difpic (b,b2)
!
! Now do simple tests for half
! Now generate HALF input data set
! 
!gen mgtest 10 10 SINC=40 LINC=40 'HALF
! 
! Verify existence of input file
!list mgtest
! 
!
!  Try some copies.
!  Check case of grid bigger than image.
!geomv mgtest mgtest1 NAH=1 NAV=1+ 
!   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20.,+
!            20.,1.,20.,1., 20.,20.,20.,20.)
!difpic (mgtest1 mgtest)

!  Check case of grid smaller than image.
!geomv mgtest mgtest1 NAH=1 NAV=1+ 
!   TIEPOINT=(1.,1.,1.,1.,   1.,2.,1.,2.,+
!             2.,1.,2.,1.,   2.,2.,2.,2.)
!difpic (mgtest1 mgtest)

! Perform simple enlargement to 2X size
!geomv mgtest mgenlarg NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)   +
!   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
!                20.,1.,10.,1.,20.,20.,10.,10.)
!lgeom mgtest mgenlarg2 NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)   +
!   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
!                20.,1.,10.,1.,20.,20.,10.,10.)
 
! Print it out
!list mgenlarg
!difpic (mgenlarg,mgenlarg2)
! 
! Perform 45 degree rotation clockwise with 1.4 times enlargement
!geomv mgtest mgrotat NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)  +
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)
!
! Print it out
!list mgrotat
! 
! Now generate HALF input data set
! 
!gen mgtest2 1000 1000 SINC=1 LINC=1 'HALF
! 
! Verify existence of input file
!list mgtest2 SIZE=(1,1,15,15)
! 
! DO LONG THIN CASE WITH 45 DEG ROTATION.
!geomv mgtest2 mgenthin NAH=1 NAV=1+ 
!   SIZE=(1,1,2,1000) +
!   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000.,+
!           2.,1.,1001.,2.,2.,1000.,2.,1001.)
!lgeom mgtest2 mgenthin2 NAH=1 NAV=1+ 
!   SIZE=(1,1,2,1000) +
!   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000.,+
!           2.,1.,1001.,2.,2.,1000.,2.,1001.)
!difpic (mgenthin,mgenthin2)           
!
! Print it out
!list mgenthin 'NOEJECT
!
!gen a 1000 1000 'half
!  Test bilinear interpolation for FR 87169
!geomv							+
!inp=a					+
!out=b					+
!nl=900 ns=900						+
!nav=9							+
!nah=9							+
!tiepoint=(						+
!001,001,900,900,001,100,800,800,001,200,300,300,001,300,300,300,+
!001,400,400,400,001,500,500,500,001,600,600,600,001,700,700,700,+
!001,800,001,800,001,900,001,900,100,001,100,001,+
!100,100,100,100,100,200,600,200,100,300,700,300,100,400,100,400,+
!100,500,500,500,100,600,600,600,100,700,700,700,100,800,800,800,+
!100,900,100,900,		+
!200,001,200,001,200,100,200,100,200,200,200,200,200,300,200,300,+
!200,400,400,400,200,500,500,500,200,600,600,600,200,700,700,700,+
!200,800,200,800,200,900,200,900,300,001,300,001,+
!300,100,300,100,300,200,300,200,300,300,300,300,300,400,300,400,+
!300,500,300,500,300,600,300,600,300,700,300,700,300,800,300,800,+
!300,900,300,900,		+
!400,001,400,001,400,100,400,100,400,200,400,200,400,300,400,300,+
!400,400,400,400,400,500,500,500,400,600,400,600,400,700,400,700,+
!400,800,400,800,400,900,400,900,500,001,500,001,+
!500,100,500,100,500,200,500,200,500,300,500,300,500,400,500,400,+
!500,500,500,500,500,600,500,600,500,700,500,700,500,800,500,800,+
!500,900,500,900,		+
!600,001,600,001,600,100,600,100,600,200,600,200,600,300,600,300,+
!600,400,600,400,600,500,600,500,600,600,600,600,600,700,600,700,+
!600,800,600,800,600,900,600,900,700,001,700,001,+
!700,100,700,100,700,200,700,200,700,300,700,300,700,400,700,400,+
!700,500,700,500,700,600,700,600,700,700,700,700,700,800,700,800,+
!700,900,700,900,		+
!800,001,800,001,800,100,100,100,800,200,200,200,800,300,300,300,+
!800,400,800,400,800,500,800,500,800,600,800,600,800,700,800,700,+
!800,800,800,800,800,900,800,900,900,001,900,001,+
!900,100,900,100,900,200,900,200,900,300,900,300,900,400,400,400,+
!900,500,900,500,900,600,900,600,900,700,900,700,900,800,800,800,+
!900,900,900,900)
!list b (50,50,850,850) linc=100 sinc=100


! long case with offsets SHOWS INACCURACY OF LGEOM

!gen mgtest2 10 4000 SINC=3 LINC=7
!datetime
!geomv mgtest2 mgenthin NAH=1 NAV=1+ 
!   SIZE=(1,1,10,4000)+
!   TIEPOINT=(1.,1.,1.,1.,1.,4000.,1.,3900.,+
!            4000.,1.,4000.,1.,4000.,4000.,4000.,3900.)
!datetime
!lgeom mgtest2 mgenthin2 NAH=1 NAV=1+ 
!   SIZE=(1,1,10,4000)+
!   TIEPOINT=(1.,1.,1.,1.,1.,4000.,1.,3900.,+
!            4000.,1.,4000.,1.,4000.,4000.,4000.,3900.)
!datetime
!difpic (mgenthin,mgenthin2)
!list mgenthin nl=1 ss=3288 ns=3    !GEOMV IS CORRECT
!list mgenthin2 nl=1 ss=3288 ns=3   !LGEOM IS INCORRECT

! big case with offsets / timing case

!gen mgtest2 4000 4000 SINC=3 LINC=7
!datetime
!geomv mgtest2 mgenthin NAH=1 NAV=1+ 
!   SIZE=(1,1,4000,4000)+
!   TIEPOINT=(1.,1.,7.,7.,1.,4000.,1.,4022.,+
!            4000.,1.,3970.,22.,4000.,4000.,4050.,4060.)
!datetime
!lgeom mgtest2 mgenthin2 NAH=1 NAV=1+ 
!   SIZE=(1,1,4000,4000)+
!   TIEPOINT=(1.,1.,7.,7.,1.,4000.,1.,4022.,+
!            4000.,1.,3970.,22.,4000.,4000.,4050.,4060.)
!datetime
!difpic (mgenthin,mgenthin2)

! ibis file grid input

! really big case with offsets / timing case

!datetime
!gen mgtest2 8000 8000 SINC=3 LINC=7
!datetime
!geomv mgtest2 mgenthin NAH=1 NAV=1+
!   SIZE=(1,1,8000,8000)+
!   TIEPOINT=(1.,1.,7.,7.,1.,8000.,1.,8022.,+
!            8000.,1.,7970.,22.,8000.,8000.,8050.,8060.)
!datetime
!lgeom mgtest2 mgenthin2 NAH=1 NAV=1+ 
!   SIZE=(1,1,8000,8000)+
!   TIEPOINT=(1.,1.,7.,7.,1.,8000.,1.,8022.,+
!            8000.,1.,7970.,22.,8000.,8000.,8050.,8060.)
!datetime
!difpic (mgenthin,mgenthin2)
!datetime

! small grid small image testing ibis file pass to geomv

!gen mgtest2 400 400 SINC=3 LINC=7
!ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
!    nc=4 nr=444
!mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
!mf a func=("c1=mod(c1,0.0001)*4000000","c2=mod(c2,0.0001)*4000000")
!mf a func=("c1=c1-mod(c1,0.1)","c2=c2-mod(c2,0.1)")
!mf a func=("c3=c1*1.1","c4=c2*1.1")
!!ibis-list a cols=(1,2,3,4) csize=12 'format
!tieconv INP=a COLS=(1,2,3,4)  OUT=b +
!      NAH=1,NAV=1,MINL=1.,MINS=1.,MAXL=400.,MAXS=400. +
!    'GEOMV 'NOPR 
!ibis-list b cols=(1,2,3,4) csize=12  'format

!geomv INP=(mgtest2,b) OUT=mgenlarg NAH=3 NAV=3+ 
!   SIZE=(1,1,400,400)

!list mgenlarg linc=37 sinc=37

! parms file use

!ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
!    nc=4 nr=44
!mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
!mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
!!mf a func=("c1=c1-mod(c1,0.1)","c2=c2-mod(c2,0.1)")
!mf a func=("c3=c1*1.1","c4=c2*1.1")
!ibis-list a cols=(1,2,3,4) csize=12
! INP=a COLS=(1,2,3,4)  OUT=b +
!      NAH=3,NAV=3,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
!    'LGEOM 'NOPR

!datetime
!geomv INP=mgtest OUT=mgenlarg PARMS=b + 
!   SIZE=(1,1,20,20)
!list mgenlarg ns=12 nl=20


! ibis file grid input large grid and large image

!gen mgtest2 4000 4000 SINC=3 LINC=7
!ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
!    nc=4 nr=444
!mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
!mf a func=("c1=mod(c1,0.0001)*40000000","c2=mod(c2,0.0001)*40000000")
!mf a func=("c1=c1-mod(c1,0.1)","c2=c2-mod(c2,0.1)")
!mf a func=("c3=c1*1.1","c4=c2*1.1")
!!ibis-list a cols=(1,2,3,4) csize=12 'format
!tieconv INP=a COLS=(1,2,3,4)  OUT=b +
!      NAH=30,NAV=30,MINL=1.,MINS=1.,MAXL=4000.,MAXS=4000. +
!    'GEOMV 'NOPR 
!ibis-list b cols=(1,2,3,4) csize=12 nr=10 'format

!datetime
!geomv INP=(mgtest2,b) OUT=mgenlarg NAH=3 NAV=3+ 
!   SIZE=(1,1,4000,4000)
!datetime

!list mgenlarg linc=97 sinc=97

! LARGE CASE COMPARISON WITH LGEOM, BE CAREFUL, LGEOM
! REQUIRES INTEGRAL VALUES IN OUTPUT GRID, CHECK IT OUT

gen mgtest2 3000 3000 SINC=3 LINC=7
ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=444
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
mf a func=("c1=mod(c1,0.0001)*30000000","c2=mod(c2,0.0001)*30000000")
mf a func=("c1=c1-mod(c1,0.1)","c2=c2-mod(c2,0.1)")
mf a func=("c3=c1*1.1","c4=c2*1.1")
ibis-list a cols=(1,2,3,4) csize=12 NR=10 'format
tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=30,NAV=30,MINL=1.,MINS=1.,MAXL=3001.,MAXS=3001. +
    'GEOMV 'NOPR 
ibis-list b cols=(1,2,3,4) csize=12 nr=40 'format

datetime
geomv INP=(mgtest2,b) OUT=mgenlarg SIZE=(1,1,3000,3000)
datetime

tieconv INP=a COLS=(1,2,3,4)  OUT=b2 +
      NAH=30,NAV=30,MINL=1.,MINS=1.,MAXL=3000.,MAXS=3000. +
    'LGEOM 'NOPR 

datetime
lgeom INP=mgtest2 OUT=mgenlarg2 SIZE=(1,1,3000,3000) PARMS=b2
datetime

difpic (mgenlarg,mgenlarg2)

! small grid small image testing ibis file pass to geomv

!gen mgtest2 400 400 SINC=3 LINC=7
!ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
!    nc=4 nr=44
!mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
!mf a func=("c1=mod(c1,0.0001)*4000000","c2=mod(c2,0.0001)*4000000")
!mf a func=("c3=c1*1.1","c4=c2*1.1")

!tieconv INP=a COLS=(1,2,3,4)  OUT=b +
!      NAH=30,NAV=30,MINL=1.,MINS=1.,MAXL=421.,MAXS=421. +
!    'GEOMV 'NOPR 
!ibis-list b nr=40 'format

!geomv INP=(mgtest2,b) OUT=mgenlarg + 
!   SIZE=(1,1,400,400)

!list mgenlarg linc=39 sinc=39

! ibis file input single precision case, also test columns
! can't use tieconv since it is always doublew precision

!ibis-gen b version=&version org=&org  datacol=(1,2,3,4,5) +
!    nc=5 nr=961
!mf b func=("c1=int((index-1)/31)*14+1","c2=mod(index+30,31)*14+1")
!mf b func=("c3=c1*1.1","c5=c2*1.1")

!ibis-list b nr=961 'format

!geomv INP=(mgtest2,b) OUT=mgenlarg2 cols=(1,2,3,5)+ 
!   SIZE=(1,1,400,400)

!list mgenlarg2 linc=39 sinc=39

!difpic (mgenlarg,mgenlarg2)



!   THIS CASE SHOWS GEOMV VS LGEOM ACCURACY, THE VALUE AT 18,4
! SHOULD CLEARLY BE 37, BUT LGEOM GETS 36.  IN FACT THE FLOATING
! VALUE CALCULATES TO 37.4, SO IT CAN'T TRUNCATE OR ROUND TO 36.

! FOR THIS CASE, THE GRID IS RECTANGULAR, ALL WHOLE NUMBERS, AND
! EVENLY SPACED

! YOU CAN ALSO USE THIS CASE FOR THE VARIOUS BAD GRID ERROR CHECKS.

! Now generate BYTE input data set
 
!gen mgtest 10 10 SINC=40 LINC=40
!list mgtest

 
! Perform bent enlargement to 2X size
!geomv mgtest mgenlarg + 
!   SIZE=(1,1,21,21)+
!   TIEPOINT=(1.,1.,1.,1.,     1.,11.,1.,2.,      1.,21.,1.,10.,+
!             11.,1.,3.,1.,     11.,11.,4.,2.,      11.,21.,3.,10.,+
!             21.,1.,10.,1.,   21.,11.,10.,2.,    21.,21.,10.,10.)

!lgeom mgtest mgenlarg2 nah=2 nav=2 + 
!   SIZE=(1,1,21,21)+
!   TIEPOINT=(1.,1.,1.,1.,     1.,11.,1.,2.,      1.,21.,1.,10.,+
!             11.,1.,3.,1.,     11.,11.,4.,2.,      11.,21.,3.,10.,+
!             21.,1.,10.,1.,   21.,11.,10.,2.,    21.,21.,10.,10.)

!list mgenlarg ss=1 ns=15

!list mgenlarg2 ss=1 ns=15

gen xxxim1 10 10 SINC=40 LINC=40

! Perform 45 degree rotation clockwise with 1.4 times enlargement

GEOMV xxxim1 xxxim2 sl=7 ss=1 +
         nl=6 ns=6 interp=noin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)


GEOMV xxxim1 xxxim5 sl=7 ss=1 +
         nl=6 ns=6 interp=noin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

GEOMV xxxim1 xxxim3 sl=7 ss=1 +
         nl=6 ns=6 interp=bilin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)
GEOMV xxxim1 xxxim6 sl=7 ss=1 +
         nl=6 ns=6 interp=bilin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)
GEOMV xxxim1 xxxim4 sl=7 ss=1 +
         nl=6 ns=6 +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

GEOMV xxxim1 xxxim7 sl=7 ss=1 +
         nl=6 ns=6 +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

difpic (xxxim2,xxxim3)
difpic (xxxim3,xxxim4)
difpic (xxxim2,xxxim5)
difpic (xxxim3,xxxim6)
difpic (xxxim4,xxxim7)

! this case tests the zero piece write, remove write in program to see in action

ibis-copy xxqqgrid2 devgeomvgrid

mf3 devgeomvgrid f="c7=(c7-33284.24)*450+33284.24$c8=(c8-15462.29)*450+15462.29"

geomv +
 inp=(/home/alz/astapp/tomcrete/ca-aster-quads/lcalnorm56.img, +
 devgeomvgrid,tiny)        out=tiny2 sl=1 ss=1   +
 nl=100 ns=100 interp=BILIN cols=(1,2,7,8) tiepoint=

geomv +
 inp=(/home/alz/astapp/tomcrete/ca-aster-quads/lcalnorm56.img, +
 devgeomvgrid,tiny)        out=tiny3 sl=1 ss=1  vmemsize=40000000 +
 nl=100 ns=100 interp=BILIN cols=(1,2,7,8) tiepoint=

difpic (tiny2,tiny3)
xvd tiny3

!curr>

! this is the first huge case

!label-create xxxgeomvtmp87b xxxa nl=10451 ns=10620
!xvd xxxa
!goto theend

!gtwarp /home/alz/astapp/tomcrete/ca-aster-quads/mos1.img +
!   /home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb.img +
!   ref=/home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master +
!   'coverinp

geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mos1.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_cubspl.img +
 size=(-130710,-196887,6000,6000)  cols=(1,2,7,8) +
 interp=cubsplin vmemsize=800000000

!size=(-150710,-206887,31355,31857)  cols=(1,2,7,8) +
 
!xvd /home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb.img

f2 inp=(/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_cubbil.img,+
  /home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_cubcnv.img) +
  out=xxxa func="abs(in1-in2+128)"

hist xxxa

f2 inp=(/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_cubbil.img,+
  /home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_cubspl.img) +
  out=xxxb func="abs(in1-in2+128)"

hist xxxb


goto theend

!curr>

! now cubic convolution and cubic spline

gen xxxim1 20 20 SINC=40 LINC=40

GEOMV xxxim1 xxxim2 sl=10 ss=4 +
         nl=6 ns=6 interp=bilin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

GEOMV xxxim1 xxxim3 sl=10 ss=4 +
         nl=6 ns=6 interp=cubsplin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

GEOMV xxxim1 xxxim4 sl=10 ss=4 +
         nl=6 ns=6 interp=cubconv +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

difpic (xxxim2,xxxim3)
difpic (xxxim2,xxxim4)
difpic (xxxim3,xxxim4)
list xxxim2
list xxxim3
xvd xxxim2
xvd xxxim3


! TIMIMG TEST

!curr>

ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=700000000
ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=600000000
ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=500000000
ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=400000000
ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=300000000
ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=200000000
ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=cubconv vmemsize=500000000
ush date

! comparison of data with different strip layout

curr>

ush date
/home/alz/ikapp/geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 /home/alz/ikapp/xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_300.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=300000000
ush date
/home/alz/ikapp/geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 /home/alz/ikapp/xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_200.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=200000000
ush date

difpic (/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_300.img, +
        /home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_200.img)

theend>
end-proc
$!-----------------------------------------------------------------------------
$ create tstgeomv.log_solos
tstgeomv
gen mgtest 10 10 SINC=40 LINC=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
list mgtest
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:26:54 2007
     Samp     1       3       5       7       9
   Line
      1       0  40  80 120 160 200 240  24  64 104
      2      40  80 120 160 200 240  24  64 104 144
      3      80 120 160 200 240  24  64 104 144 184
      4     120 160 200 240  24  64 104 144 184 224
      5     160 200 240  24  64 104 144 184 224   8
      6     200 240  24  64 104 144 184 224   8  48
      7     240  24  64 104 144 184 224   8  48  88
      8      24  64 104 144 184 224   8  48  88 128
      9      64 104 144 184 224   8  48  88 128 168
     10     104 144 184 224   8  48  88 128 168 208
geomv mgtest mgtest1  +
   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20., +
            20.,1.,20.,1., 20.,20.,20.,20.)
Beginning VICAR task geomv
geomv version 16-jul-07
difpic (mgtest1 mgtest)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
geomv mgtest mgenlarg  +
   SIZE=(1,1,20,20) +
   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10., +
                20.,1.,10.,1.,20.,20.,10.,10.)
Beginning VICAR task geomv
geomv version 16-jul-07
list mgenlarg
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:26:54 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:26:56 2007
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0  19  38  57  76  95 114 133 152 171 189 208 227 206 104  28  47  66  85 104
      2      19  38  57  76  95 114 133 152 171 189 208 202 163 123  78  47  66  85 104 123
      3      38  57  76  95 114 133 152 171 189 208 227 195  99  40  52  66  85 104 123 142
      4      57  76  95 114 133 152 171 189 208 199 167 126  75  47  66  85 104 123 142 161
      5      76  95 114 133 152 171 189 208 227 186  96  49  56  66  85 104 123 142 161 180
      6      95 114 133 152 171 189 208 198 172 128  71  47  66  85 104 123 142 161 180 199
      7     114 133 152 171 189 208 227 178  95  58  58  66  85 104 123 142 161 180 199 218
      8     133 152 171 189 208 198 178 129  65  47  66  85 104 123 142 161 180 194 175 156
      9     152 171 189 208 227 172  95  65  59  66  85 104 123 142 161 180 199 207 130  53
     10     171 189 208 199 186 128  58  47  66  85 104 123 142 161 180 192 179 159  89  19
     11     189 208 227 167  96  71  58  66  85 104 123 142 161 180 199 198 127  63  50  37
     12     208 202 195 126  49  47  66  85 104 123 142 161 180 190 184 161  85  19  37  56
     13     227 163  99  75  56  66  85 104 123 142 161 180 199 190 126  72  53  37  56  75
     14     206 123  40  47  66  85 104 123 142 161 180 190 190 162  79  19  37  56  75  94
     15     104  78  52  66  85 104 123 142 161 180 199 184 126  79  53  37  56  75  94 113
     16      28  47  66  85 104 123 142 161 180 192 198 161  72  19  37  56  75  94 113 132
     17      47  66  85 104 123 142 161 180 199 179 127  85  53  37  56  75  94 113 132 151
     18      66  85 104 123 142 161 180 194 207 159  63  19  37  56  75  94 113 132 151 170
     19      85 104 123 142 161 180 199 175 130  89  50  37  56  75  94 113 132 151 170 189
     20     104 123 142 161 180 199 218 156  53  19  37  56  75  94 113 132 151 170 189 208
geomv mgtest mgrotat  +
   SIZE=(1,1,20,20) +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
                20.,1.,15.,5.,20.,20.,5.,15.)
Beginning VICAR task geomv
geomv version 16-jul-07
list mgrotat
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:26:54 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:26:57 2007
     Samp     1       3       5       7       9      11      13      15      17      19
   Line

      4       0   0   0   0   0   0   0   0   0  46  46   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0  88  88  88  88   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0 131 131 131 131 131 131   0   0   0   0   0   0   0
      7       0   0   0   0   0   0 173 173 173 173 173 173 173 173   0   0   0   0   0   0
      8       0   0   0   0   0 210 215 215 215 215 215 215 215 215 210   0   0   0   0   0
      9       0   0   0   0 158 132 154 136 149 142 142 149 136 154 132 158   0   0   0   0
     10       0   0   0  43  58  43  54  43  49  43  43  49  43  54  43  58  43   0   0   0
     11       0   0  85  85  85  85  85  85  85  85  85  85  85  85  85  85  85  85   0   0
     12       0   0 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127   0   0
     13       0   0   0 169 169 169 169 169 169 169 169 169 169 169 169 169 169   0   0   0
     14       0   0   0   0 211 186 211 190 203 196 196 203 190 211 186 211   0   0   0   0
     15       0   0   0   0   0  65  93  74  88  82  82  88  74  93  65   0   0   0   0   0
     16       0   0   0   0   0   0  40  40  40  40  40  40  40  40   0   0   0   0   0   0
     17       0   0   0   0   0   0   0  82  82  82  82  82  82   0   0   0   0   0   0   0
     18       0   0   0   0   0   0   0   0 124 124 124 124   0   0   0   0   0   0   0   0
     19       0   0   0   0   0   0   0   0   0 166 166   0   0   0   0   0   0   0   0   0
gen xxxim1 10 10 SINC=40 LINC=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
GEOMV xxxim1 xxxim3 sl=7 ss=1  +
         nl=6 ns=6 interp=bilin  +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
                20.,1.,15.,5.,20.,20.,5.,15.)
Beginning VICAR task GEOMV
geomv version 16-jul-07
list xxxim3 'zer
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:26:58 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:26:58 2007
     Samp     1       3       5
   Line
      1       0   0   0   0   0   0
      2       0   0   0   0   0 210
      3       0   0   0   0 158 132
      4       0   0   0  43  58  43
      5       0   0  85  85  85  85
      6       0   0 127 127 127 127
geomv mgtest mgrotat1  +
   SIZE=(1,1,20,10) +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
             20.,1.,15.,5.,20.,20.,5.,15.)
Beginning VICAR task geomv
geomv version 16-jul-07
geomv mgtest mgrotat2  +
   SIZE=(1,11,20,1) +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
                20.,1.,15.,5.,20.,20.,5.,15.)
Beginning VICAR task geomv
geomv version 16-jul-07
geomv mgtest mgrotat3  +
   SIZE=(1,12,20,9) +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
                20.,1.,15.,5.,20.,20.,5.,15.)
Beginning VICAR task geomv
geomv version 16-jul-07
mss (mgrotat1,mgrotat2,mgrotat3) mgrotatA
Beginning VICAR task mss
* OUTPUT CONTAINS   3INTERLEAVED DATA SETS **
* ACTUAL OUTPUT RECORD LENGTH     20SAMPLES **
difpic (mgrotatA,mgrotat)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
geomv mgtest mgrotat  +
   SIZE=(1,1,20,20) +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
            20.,1.,15.,5.,20.,20.,5.,15.) +
   INTERP=NOIN
Beginning VICAR task geomv
geomv version 16-jul-07
list mgrotat
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:26:54 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:01 2007
     Samp     1       3       5       7       9      11      13      15      17      19
   Line

      4       0   0   0   0   0   0   0   0   0  40  40   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0  80  80  80  80   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0 120 120 120 120 120 120   0   0   0   0   0   0   0
      7       0   0   0   0   0   0 160 160 160 160 160 160 160 160   0   0   0   0   0   0
      8       0   0   0   0   0 200 240 200 200 200 200 200 200 240 200   0   0   0   0   0
      9       0   0   0   0 240  24 240  24 240 240 240 240  24 240  24 240   0   0   0   0
     10       0   0   0  24  64  24  64  24  64  24  24  64  24  64  24  64  24   0   0   0
     11       0   0  64 104  64 104  64 104  64 104 104  64 104  64 104  64 104  64   0   0
     12       0   0 144 104 144 104 144 104 144 144 144 144 104 144 104 144 104 144   0   0
     13       0   0   0 184 144 184 144 184 184 184 184 184 184 144 184 144 184   0   0   0
     14       0   0   0   0 224 184 224 224 224 224 224 224 224 224 184 224   0   0   0   0
     15       0   0   0   0   0   8   8   8   8   8   8   8   8   8   8   0   0   0   0   0
     16       0   0   0   0   0   0  48  48  48  48  48  48  48  48   0   0   0   0   0   0
     17       0   0   0   0   0   0   0  88  88  88  88  88  88   0   0   0   0   0   0   0
     18       0   0   0   0   0   0   0   0 128 128 128 128   0   0   0   0   0   0   0   0
     19       0   0   0   0   0   0   0   0   0 168 168   0   0   0   0   0   0   0   0   0
gen mgtest 10 10 SINC=64 LINC=64
Beginning VICAR task gen
GEN Version 6
GEN task completed
list mgtest
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:02 2007
     Samp     1       3       5       7       9
   Line
      1       0  64 128 192   0  64 128 192   0  64
      2      64 128 192   0  64 128 192   0  64 128
      3     128 192   0  64 128 192   0  64 128 192
      4     192   0  64 128 192   0  64 128 192   0
      5       0  64 128 192   0  64 128 192   0  64
      6      64 128 192   0  64 128 192   0  64 128
      7     128 192   0  64 128 192   0  64 128 192
      8     192   0  64 128 192   0  64 128 192   0
      9       0  64 128 192   0  64 128 192   0  64
     10      64 128 192   0  64 128 192   0  64 128
geomv mgtest mgrotat  +
   SIZE=(1,1,20,20) +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
                20.,1.,15.,5.,20.,20.,5.,15.) +
   'ZNOIN
Beginning VICAR task geomv
geomv version 16-jul-07
list mgrotat
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:02 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
     Samp     1       3       5       7       9      11      13      15      17      19
   Line

      4       0   0   0   0   0   0   0   0   0  64  64   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0 141 141 141 141   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0 192 192 192 192 192 192   0   0   0   0   0   0   0

      8       0   0   0   0   0  88 128  88  64  64  64  64  88 128  88   0   0   0   0   0
      9       0   0   0   0 128 155 128 155 155 155 155 155 155 128 155 128   0   0   0   0
     10       0   0   0 192   0 192   0 192   0 192 192   0 192   0 192   0 192   0   0   0
     11       0   0   0  64   0  64   0  64   0  64  64   0  64   0  64   0  64   0   0   0
     12       0   0 128 101 128 101 128 101 101 101 101 101 101 128 101 128 101 128   0   0
     13       0   0   0 168 128 168 128 168 192 192 192 192 168 128 168 128 168   0   0   0
     14       0   0   0   0   0 192   0   0   0   0   0   0   0   0 192   0   0   0   0   0
     15       0   0   0   0   0  64  64  64  64  64  64  64  64  64  64   0   0   0   0   0
     16       0   0   0   0   0   0 115 115 115 115 115 115 115 115   0   0   0   0   0   0
     17       0   0   0   0   0   0   0 192 192 192 192 192 192   0   0   0   0   0   0   0

     19       0   0   0   0   0   0   0   0   0  64  64   0   0   0   0   0   0   0   0   0
gen mgtest2 1000 1000 SINC=1 LINC=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
list mgtest2 SIZE=(1,1,15,15)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   1   2   3   4   5   6   7   8   9  10  11  12  13  14
      2       1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
      3       2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
      4       3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
      5       4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
      6       5   6   7   8   9  10  11  12  13  14  15  16  17  18  19
      7       6   7   8   9  10  11  12  13  14  15  16  17  18  19  20
      8       7   8   9  10  11  12  13  14  15  16  17  18  19  20  21
      9       8   9  10  11  12  13  14  15  16  17  18  19  20  21  22
     10       9  10  11  12  13  14  15  16  17  18  19  20  21  22  23
     11      10  11  12  13  14  15  16  17  18  19  20  21  22  23  24
     12      11  12  13  14  15  16  17  18  19  20  21  22  23  24  25
     13      12  13  14  15  16  17  18  19  20  21  22  23  24  25  26
     14      13  14  15  16  17  18  19  20  21  22  23  24  25  26  27
     15      14  15  16  17  18  19  20  21  22  23  24  25  26  27  28
geomv mgtest2 mgenthin  +
   SIZE=(1,1,2,1000) +
   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000., +
            2.,1.,1001.,2.,2.,1000.,2.,1001.)
Beginning VICAR task geomv
geomv version 16-jul-07
list mgenthin 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2       0 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp    31      33      35      37      39      41      43      45      47      49      51      53      55      57      59
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp    61      63      65      67      69      71      73      75      77      79      81      83      85      87      89
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp    91      93      95      97      99     101     103     105     107     109     111     113     115     117     119
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   121     123     125     127     129     131     133     135     137     139     141     143     145     147     149
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   151     153     155     157     159     161     163     165     167     169     171     173     175     177     179
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   181     183     185     187     189     191     193     195     197     199     201     203     205     207     209
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   211     213     215     217     219     221     223     225     227     229     231     233     235     237     239
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   241     243     245     247     249     251     253     255     257     259     261     263     265     267     269
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   271     273     275     277     279     281     283     285     287     289     291     293     295     297     299
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   301     303     305     307     309     311     313     315     317     319     321     323     325     327     329
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   331     333     335     337     339     341     343     345     347     349     351     353     355     357     359
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   361     363     365     367     369     371     373     375     377     379     381     383     385     387     389
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   391     393     395     397     399     401     403     405     407     409     411     413     415     417     419
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   421     423     425     427     429     431     433     435     437     439     441     443     445     447     449
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   451     453     455     457     459     461     463     465     467     469     471     473     475     477     479
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   481     483     485     487     489     491     493     495     497     499     501     503     505     507     509
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   511     513     515     517     519     521     523     525     527     529     531     533     535     537     539
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   541     543     545     547     549     551     553     555     557     559     561     563     565     567     569
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   571     573     575     577     579     581     583     585     587     589     591     593     595     597     599
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   601     603     605     607     609     611     613     615     617     619     621     623     625     627     629
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   631     633     635     637     639     641     643     645     647     649     651     653     655     657     659
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   661     663     665     667     669     671     673     675     677     679     681     683     685     687     689
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   691     693     695     697     699     701     703     705     707     709     711     713     715     717     719
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   721     723     725     727     729     731     733     735     737     739     741     743     745     747     749
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   751     753     755     757     759     761     763     765     767     769     771     773     775     777     779
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   781     783     785     787     789     791     793     795     797     799     801     803     805     807     809
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   811     813     815     817     819     821     823     825     827     829     831     833     835     837     839
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   841     843     845     847     849     851     853     855     857     859     861     863     865     867     869
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   871     873     875     877     879     881     883     885     887     889     891     893     895     897     899
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   901     903     905     907     909     911     913     915     917     919     921     923     925     927     929
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   931     933     935     937     939     941     943     945     947     949     951     953     955     957     959
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   961     963     965     967     969     971     973     975     977     979     981     983     985     987     989
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:03 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:04 2007
     Samp   991     993     995     997     999
   Line
      1     231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233   0
gen mgtest 10 10 SINC=40 LINC=40 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list mgtest
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:05 2007
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0    40    80   120   160   200   240   280   320   360
      2        40    80   120   160   200   240   280   320   360   400
      3        80   120   160   200   240   280   320   360   400   440
      4       120   160   200   240   280   320   360   400   440   480
      5       160   200   240   280   320   360   400   440   480   520
      6       200   240   280   320   360   400   440   480   520   560
      7       240   280   320   360   400   440   480   520   560   600
      8       280   320   360   400   440   480   520   560   600   640
      9       320   360   400   440   480   520   560   600   640   680
     10       360   400   440   480   520   560   600   640   680   720
geomv mgtest mgtest1  +
   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20., +
            20.,1.,20.,1., 20.,20.,20.,20.)
Beginning VICAR task geomv
geomv version 16-jul-07
difpic (mgtest1 mgtest)
Beginning VICAR task difpic
DIFPIC version 29jun04
 NUMBER OF DIFFERENCES =   0
geomv mgtest mgenlarg  +
   SIZE=(1,1,20,20)    +
   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10., +
                20.,1.,10.,1.,20.,20.,10.,10.)
Beginning VICAR task geomv
geomv version 16-jul-07
list mgenlarg
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:05 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:07 2007
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         0    19    38    57    76    95   114   133   152   171   189   208   227   246   265
      2        19    38    57    76    95   114   133   152   171   189   208   227   246   265   284
      3        38    57    76    95   114   133   152   171   189   208   227   246   265   284   303
      4        57    76    95   114   133   152   171   189   208   227   246   265   284   303   322
      5        76    95   114   133   152   171   189   208   227   246   265   284   303   322   341
      6        95   114   133   152   171   189   208   227   246   265   284   303   322   341   360
      7       114   133   152   171   189   208   227   246   265   284   303   322   341   360   379
      8       133   152   171   189   208   227   246   265   284   303   322   341   360   379   398
      9       152   171   189   208   227   246   265   284   303   322   341   360   379   398   417
     10       171   189   208   227   246   265   284   303   322   341   360   379   398   417   436
     11       189   208   227   246   265   284   303   322   341   360   379   398   417   436   455
     12       208   227   246   265   284   303   322   341   360   379   398   417   436   455   474
     13       227   246   265   284   303   322   341   360   379   398   417   436   455   474   493
     14       246   265   284   303   322   341   360   379   398   417   436   455   474   493   512
     15       265   284   303   322   341   360   379   398   417   436   455   474   493   512   531
     16       284   303   322   341   360   379   398   417   436   455   474   493   512   531   549
     17       303   322   341   360   379   398   417   436   455   474   493   512   531   549   568
     18       322   341   360   379   398   417   436   455   474   493   512   531   549   568   587
     19       341   360   379   398   417   436   455   474   493   512   531   549   568   587   606
     20       360   379   398   417   436   455   474   493   512   531   549   568   587   606   625

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:05 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:07 2007
     Samp      16    17    18    19    20
   Line
      1       284   303   322   341   360
      2       303   322   341   360   379
      3       322   341   360   379   398
      4       341   360   379   398   417
      5       360   379   398   417   436
      6       379   398   417   436   455
      7       398   417   436   455   474
      8       417   436   455   474   493
      9       436   455   474   493   512
     10       455   474   493   512   531
     11       474   493   512   531   549
     12       493   512   531   549   568
     13       512   531   549   568   587
     14       531   549   568   587   606
     15       549   568   587   606   625
     16       568   587   606   625   644
     17       587   606   625   644   663
     18       606   625   644   663   682
     19       625   644   663   682   701
     20       644   663   682   701   720
geomv mgtest mgrotat  +
   SIZE=(1,1,20,20)   +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
                20.,1.,15.,5.,20.,20.,5.,15.)
Beginning VICAR task geomv
geomv version 16-jul-07
list mgrotat
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:05 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:08 2007
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line

      4         0     0     0     0     0     0     0     0     0    46    46     0     0     0     0
      5         0     0     0     0     0     0     0     0    88    88    88    88     0     0     0
      6         0     0     0     0     0     0     0   131   131   131   131   131   131     0     0
      7         0     0     0     0     0     0   173   173   173   173   173   173   173   173     0
      8         0     0     0     0     0   215   215   215   215   215   215   215   215   215   215
      9         0     0     0     0   257   257   257   257   257   257   257   257   257   257   257
     10         0     0     0   299   299   299   299   299   299   299   299   299   299   299   299
     11         0     0   341   341   341   341   341   341   341   341   341   341   341   341   341
     12         0     0   383   383   383   383   383   383   383   383   383   383   383   383   383
     13         0     0     0   425   425   425   425   425   425   425   425   425   425   425   425
     14         0     0     0     0   467   467   467   467   467   467   467   467   467   467   467
     15         0     0     0     0     0   509   509   509   509   509   509   509   509   509   509
     16         0     0     0     0     0     0   552   552   552   552   552   552   552   552     0
     17         0     0     0     0     0     0     0   594   594   594   594   594   594     0     0
     18         0     0     0     0     0     0     0     0   636   636   636   636     0     0     0
     19         0     0     0     0     0     0     0     0     0   678   678     0     0     0     0

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:05 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:08 2007
     Samp      16    17    18    19    20
   Line

      9       257     0     0     0     0
     10       299   299     0     0     0
     11       341   341   341     0     0
     12       383   383   383     0     0
     13       425   425     0     0     0
     14       467     0     0     0     0
gen mgtest2 1000 1000 SINC=3 LINC=7
Beginning VICAR task gen
GEN Version 6
GEN task completed
geomv mgtest2 mgenthin  +
   SIZE=(1,1,1000,1000) +
   TIEPOINT=(1.,1.,7.,7.,1.,1000.,1.,1022., +
            1000.,1.,970.,22.,1000.,1000.,1050.,1060.)
Beginning VICAR task geomv
geomv version 16-jul-07
list mgenthin linc=199 sinc=199
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:09 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:09 2007
     Samp     1     399     797
   Line
      1      60 146 232  63 149   0
    200     132 245 101 214  71   0
    399     204  87 226 110 248   0
    598      19 185  95  31 172   0
    797      91  28 220 157  94   0
    996     163 126  89   0   0   0
gen mgtest2 400 400 SINC=3 LINC=7
Beginning VICAR task gen
GEN Version 6
GEN task completed
ibis-gen a version=ibis-1 org=column  datacol=(1,2,3,4)  +
    nc=4 nr=444
Beginning VICAR task ibis
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
Beginning VICAR task mf
mf a func=("c1=mod(c1,0.0001)*4000000","c2=mod(c2,0.0001)*4000000")
Beginning VICAR task mf
mf a func=("c3=c1*1.1","c4=c2*1.1")
Beginning VICAR task mf
tieconv INP=a COLS=(1,2,3,4)  OUT=b  +
      NAH=30,NAV=30,MINL=1.,MINS=1.,MAXL=400.,MAXS=400.  +
    'GEOMV
Beginning VICAR task tieconv
tieconv version 28-mar-02
lsq fit x'=ax+by+c
            1.100E+00  9.740E-10 -5.594E-07
lsq fit y'=dx+ey+f
           -4.046E-09  1.100E+00  1.016E-06
data
            1.829E+02  1.604E+01  2.496E+02  4.268E+02  1.580E+02  2.897E+02  4.154E+02  9.504E+01  2.087E+02  3.500E+02
            6.208E+01  2.417E+02  2.574E+01  2.941E+02  1.837E+02  1.343E+02  1.462E+02  2.359E+02  4.204E+02  2.596E+02
            1.935E+02  2.558E+02  3.959E+02  2.411E+02  2.314E+02  3.332E+02  1.737E+02  1.256E+02  2.562E+02  1.254E+02
            1.733E+02  3.998E+02  3.650E+02  6.875E+01  4.248E+02  1.130E+02  1.342E+01  1.261E+02  4.449E+01  1.751E+02
            1.116E+02  2.938E+02  2.817E+02  1.091E+02  1.822E+02  9.465E+01  2.865E+02  3.176E+02  1.881E+02  3.380E+02
            3.608E+02  2.565E+02  4.316E+02  7.312E+01  2.758E+01  3.285E+02  6.242E+01  1.092E+02  9.607E+01  3.958E+02
            1.957E+02  3.420E+02  4.284E+02  4.212E+02  3.542E+02  2.272E+02  4.021E+01  2.669E+02  4.000E+02  1.004E+02
            1.808E+02  2.013E+02  2.290E+02  2.303E+02  2.052E+02  1.538E+02  1.095E+02  3.890E+01  9.055E+00  3.928E+02
            3.438E+02  3.020E+02  3.009E+02  3.071E+02  3.204E+02  3.745E+02  2.936E+01  1.650E+02  3.414E+02  1.186E+02
            3.765E+02  2.688E+02  2.018E+02  2.092E+02  2.909E+02  4.133E+02  1.702E+02  3.487E+01  3.804E+02  3.937E+02
            7.503E+01  2.707E+02  1.006E+02  3.849E+01  1.178E+02  2.715E+02  9.310E+01  2.259E+01  9.355E+01  2.724E+02
            1.192E+02  1.074E+02  2.372E+02  3.480E+01  7.477E+00  8.806E+01  3.437E+02  2.672E+02  3.658E+02  1.658E+02
            1.409E+02  2.575E+02  1.091E+02  1.357E+02  3.374E+02  2.406E+02  3.524E+02  1.657E+02  1.875E+02  3.844E+02
            3.500E+02  5.055E+01  3.662E+02  1.040E+01  3.032E+02  3.647E+02  1.948E+02  1.999E+02  7.182E+00  2.309E+01
            2.476E+02  2.408E+02  3.607E+01  4.001E+01  2.861E+02  3.008E+02  1.513E+02  2.104E+02  3.813E+01  1.416E+02
            4.723E+01  1.951E+02  1.451E+02  3.373E+02  3.652E+02  1.953E+02  3.012E+02  2.093E+02  3.931E+02  3.791E+02
            2.008E+02  3.319E+02  2.651E+02  3.412E+01  7.887E+01  3.994E+02  1.156E+02  1.412E+02  2.542E+00  1.396E+02
            1.460E+02  2.175E+01  1.732E+02  1.605E+02  5.058E+01  2.164E+02  2.516E+02  1.562E+02  3.700E+02  1.314E+01
            4.056E+02  2.610E+02  3.921E+02  4.261E+02  3.294E+02  1.357E+02  2.512E+02  2.696E+02  1.910E+02  4.216E+02
            8.159E+01  1.180E+02  5.733E+01  3.060E+02  1.750E+01  1.055E+02  6.280E+01  3.966E+02  1.932E+02  3.328E+02
            3.752E+02  3.541E+02  2.359E+02  5.418E+01  2.153E+02  3.129E+02  3.134E+02  2.504E+02  1.238E+02  3.737E+02
            8.643E+01  2.092E+02  2.349E+02  2.306E+02  1.292E+02  4.379E+02  2.094E+02  3.909E+02  1.025E+02  1.570E+02
            1.815E+02  1.760E+02  7.346E+01  4.145E+02  2.520E+02  2.590E+01  2.099E+02  3.639E+02  4.790E+01  1.420E+02
            1.725E+02  1.730E+02  1.436E+02  1.178E+02  2.842E+01  3.491E+02  2.333E+02  5.407E+01  3.184E+02  1.127E+02
            3.507E+02  8.510E+01  2.967E+02  4.718E+00  1.564E+02  3.116E+02  4.369E+02  1.257E+02  2.582E+02  3.607E+02
            2.676E+01  1.364E+02  2.162E+02  3.331E+02  4.200E+02  1.040E+02  1.982E+02  2.959E+02  4.307E+02  9.566E+01
            2.377E+02  3.834E+02  9.268E+01  2.791E+02  4.356E+02  2.228E+02  4.200E+02  2.144E+02  4.602E+01  3.212E+02
            1.600E+02  3.589E+01  3.890E+02  3.057E+02  2.596E+02  2.506E+02  2.788E+02  3.106E+02  3.796E+02  7.932E+01
            2.226E+02  4.031E+02  1.808E+02  4.356E+02  2.876E+02  1.767E+02  1.366E+02  1.001E+02  1.343E+02  2.057E+02
            3.143E+02  5.356E+01  2.700E+02  8.365E+01  4.080E+02  3.296E+02  2.883E+02  3.177E+02  4.179E+02  1.152E+02
            3.233E+02  1.622E+02  3.814E+01  4.249E+02  2.339E+00  5.697E+01  1.823E+02  3.785E+02  2.053E+02  1.029E+02
            7.120E+01  1.103E+02  1.865E+02  3.670E+02  1.783E+02  6.029E+01  1.304E+01  3.652E+01  1.307E+02  3.293E+02
            1.585E+02  5.853E+01  2.927E+01  1.043E+02  2.501E+02  2.661E+01  3.474E+02  2.990E+02  3.549E+02  4.147E+01
            2.724E+02  1.340E+02  9.999E+01  1.703E+02  3.113E+02  1.166E+02  2.618E+01  6.531E+00  9.119E+01  2.802E+02
            9.986E+01  5.743E+01  8.575E+01  2.519E+02  4.887E+01  3.901E+02  3.956E+02  6.549E+01  2.796E+02  1.581E+02
            1.409E+02  2.615E+02  1.291E+01  3.422E+02  3.357E+02  4.336E+02  1.958E+02  6.226E+01  6.662E+01  1.753E+02
            3.883E+02  2.991E+02  3.143E+02  2.730E+01  2.846E+02  2.063E+02  2.658E+02  2.320E+01  3.249E+02  2.909E+02
            3.948E+02  1.966E+02  1.026E+02  1.466E+02  3.284E+02  1.745E+02  1.585E+02  2.804E+02  6.659E+01  2.423E+01
            8.616E+01  2.860E+02  1.837E+02  1.857E+02  3.591E+02  1.969E+02  2.060E+02  3.195E+02  1.645E+02  1.137E+02
            2.344E+02  1.945E+01  4.159E+02  7.022E+01  3.024E+02  2.325E+02  3.005E+02  6.629E+01  3.562E+00  7.871E+01
            8.067E+01  1.384E+02  6.188E+01  1.904E+02  1.846E+02  4.174E+02  8.729E+00  3.123E+02  7.935E+00  4.158E+02
            1.822E+02  2.208E+02  9.148E+01  2.008E+02  1.422E+02  3.559E+02  4.016E+02  2.460E+02  3.625E+02  3.111E+02
            9.195E+01  1.449E+02  4.365E+02  1.202E+02  7.603E+01  3.040E+02  3.642E+02  2.565E+02  3.874E+02  3.840E+02
            1.792E+02  2.802E+02  1.797E+02  3.513E+02  3.887E+02  2.247E+02  3.328E+02  2.731E+02  7.911E+01  1.237E+02
            4.376E-01  1.493E+02  1.639E+02  4.171E+02  9.607E+01  1.357E+01  2.368E+02  2.922E+02  1.797E+02  3.394E+02
            3.312E+02  1.888E+02  2.850E+02  2.468E+02  4.085E+01  1.070E+02  5.362E+00  2.094E+02  2.456E+02  1.140E+02
            2.545E+02  2.272E+02  6.555E+01  1.761E+02  1.188E+02  3.672E+02  7.781E+00  3.605E+02  1.390E+02  1.896E+02
            7.231E+01  2.608E+02  2.814E+02  1.342E+02  2.927E+02  2.834E+02  1.398E+02  2.683E+02  2.290E+02  5.539E+01
            1.540E+02  1.182E+02  3.547E+02  1.684E+01  3.912E+02  1.912E+02  2.634E+02  2.013E+02  4.114E+02  4.717E+01
            3.951E+02  1.688E+02  2.146E+02  1.261E+02  3.434E+02  3.928E+02  2.744E+02  5.527E+01  1.083E+02  4.335E+02
            1.844E+02  2.410E+02  1.633E+02  3.578E+02  4.180E+02  3.104E+02  6.848E+01  1.323E+02  6.183E+01  2.635E+02
            3.309E+02  2.641E+02  2.936E+01  1.004E+02  3.710E+01  2.796E+02  3.877E+02  3.281E+02  1.341E+02  2.459E+02
            2.234E+02  3.306E+01  1.484E+02  1.295E+02  4.164E+02  1.289E+02  1.472E+02  4.376E+02  1.873E+02  2.092E+02
            9.675E+01  2.901E+02  3.491E+02  2.739E+02  6.433E+01  1.270E+02  8.889E+01  3.565E+02  1.634E+01  1.543E+01
            2.867E+02  1.721E+01  1.990E+01  3.283E+02  9.602E+01  1.359E+02  7.503E+01  2.863E+02  3.969E+02  3.397E+02
            1.817E+02  2.959E+02  3.094E+02  1.886E+02  3.400E+02  3.906E+02  3.070E+02  8.911E+01  1.769E+02  1.640E+02
            4.233E+02  1.419E+02  1.326E+02  2.259E+01  2.183E+02  2.798E+02  2.069E+02  3.341E+01  1.320E+02  1.299E+02
            4.336E+02  1.629E+02  2.316E+02  1.324E+02  3.725E+02  3.828E+01  9.820E+00  3.207E+02  5.720E+01  6.591E+01
            7.477E+00  2.212E+02  3.342E+02  3.129E+02  1.574E+02  3.412E+02  3.906E+02  3.058E+02  1.203E+02  2.405E+02
            2.265E+02  1.117E+02  3.026E+02  3.593E+02  2.817E+02  1.034E+02  2.644E+02  2.575E+02  1.835E+02  3.816E+02
            3.905E+01  2.214E+00  3.047E+02  3.284E+01  6.674E+01  4.399E+02  2.724E+02  4.106E+02  4.145E+02  3.178E+02
            8.669E+01  1.614E+02  1.353E+02  8.559E+00  1.875E+02  2.658E+02  2.098E+02  1.948E+01  1.685E+02  2.168E+02
            1.308E+02  3.841E+02  6.312E+01  4.787E+01  4.055E+02  1.553E+02  2.779E+02  2.662E+02  1.203E+02  3.137E+02
            4.063E+02  3.647E+02  2.224E+02  3.858E+02  8.449E+00  4.104E+02  2.381E+02  4.051E+02  4.378E+02  3.698E+02
            2.011E+02  3.717E+02  4.080E+02  3.101E+02  1.450E+02  2.856E+02  2.919E+02  2.311E+02  3.607E+01  1.803E+02
            1.902E+02  9.947E+01  3.480E+02  5.581E+01  6.935E+01  4.222E+02  2.343E+02  3.522E+02  3.693E+02  2.857E+02
            1.014E+02  2.565E+02  2.772E+02  1.972E+02  1.655E+01  1.416E+02  1.659E+02  1.231E+02  3.860E+02  7.469E+01
            1.362E+02  6.342E+01  3.299E+02  5.575E+01  1.208E+02  8.524E+01  3.554E+02  8.477E+01  1.870E+02  1.550E+02
            4.287E+02  1.953E+02  3.012E+02  2.728E+02  1.772E+02  3.874E+02  5.688E+01  6.564E+01  4.137E+02  2.210E+02
            3.341E+02  3.800E+02  3.253E+02  1.362E+02  2.864E+02  3.695E+02  3.183E+02  1.665E+02  3.539E+02  3.412E+01
            2.011E+01  3.454E+02  1.300E+02  2.538E+02  2.770E+02  1.994E+02  2.116E+01  1.486E+02  2.089E+02  1.686E+02
            2.746E+01  2.257E+02  3.231E+02  3.199E+02  2.160E+02  1.137E+01  1.460E+02  1.800E+02  1.132E+02  4.193E+02
            1.511E+02  2.223E+02  1.927E+02  9.595E+01  3.049E+02  6.798E+00  4.794E+01  3.948E+02  2.345E+02  4.136E+02
            5.188E+01  2.949E+01  3.800E+02  1.562E+02  2.716E+02  3.200E+02  2.676E+02  1.145E+02  3.008E+02  3.863E+02
            3.711E+02  2.552E+02  7.214E+01  2.284E+02  2.504E+02  2.388E+02  9.289E+01  2.863E+02  4.126E+02  4.046E+02
            3.294E+02  1.872E+02  3.506E+02  4.134E+02  4.090E+02  3.039E+02  9.804E+01  2.651E+02  2.979E+02  2.635E+02
            1.284E+02  3.326E+02  2.969E+01  6.606E+01  1.717E+00  2.767E+02  1.091E+01  1.180E+02  1.244E+02  3.010E+01
            3.086E+02  4.649E+01  1.236E+02  1.001E+02  9.337E+00  2.579E+02  4.058E+02  4.652E+01  2.654E+01  3.459E+02
            1.245E+02  2.759E+02  3.267E+02  3.103E+02  1.932E+02  4.154E+02  9.694E+01  1.513E+02  1.050E+02  3.979E+02
            1.837E+02  3.088E+02  3.668E+02  3.240E+02  1.806E+02  4.100E+02  9.865E+01  1.266E+02  8.749E+01  3.876E+02
residuals
           -7.056E-06 -2.432E-07  3.531E-06 -3.986E-06 -4.325E-07 -8.199E-07 -1.575E-05  2.381E-07 -6.099E-06  3.804E-06
            1.163E-06 -2.520E-06  1.181E-06 -7.372E-07  5.013E-07 -5.343E-06  5.770E-06 -3.926E-06  2.122E-06 -1.372E-05
            3.355E-06  4.832E-06  8.762E-06  3.687E-06 -6.129E-07  1.042E-05  5.504E-07 -1.989E-06  9.184E-06 -1.334E-06
            2.249E-06 -3.402E-06 -2.458E-06 -2.584E-06  8.095E-06  1.401E-06  4.121E-07 -2.778E-06  9.227E-07 -5.376E-06
            3.160E-06  8.473E-06  5.703E-06 -1.994E-07  6.682E-06 -2.540E-06  1.468E-05 -7.499E-06  4.935E-06 -1.414E-05
            5.617E-07  4.775E-06  4.968E-06 -2.217E-06  1.181E-06 -7.603E-06 -5.965E-07  3.033E-06 -4.911E-07 -3.158E-06
           -4.471E-06 -1.414E-05 -3.914E-06  1.142E-05 -2.139E-06  5.337E-06 -5.330E-08  2.842E-06 -6.335E-06 -2.074E-06
           -5.471E-06 -4.615E-06 -2.261E-06  2.415E-06 -4.739E-06  5.688E-06  2.324E-06  2.037E-06  8.019E-07  1.223E-05
            1.136E-06 -6.961E-06 -1.325E-05  1.132E-05  7.605E-06  6.453E-06 -1.316E-08 -2.144E-06  1.317E-05  2.184E-06
           -2.932E-06  9.069E-06 -7.637E-06  5.993E-06 -9.939E-06  5.320E-06 -8.100E-07 -9.179E-07  9.434E-06  2.714E-06
           -6.833E-07 -1.966E-07 -3.582E-06 -3.211E-07  1.110E-06 -4.857E-06 -1.024E-06  2.691E-07  3.548E-06  4.318E-06
           -1.135E-06 -7.870E-07  6.917E-06 -2.581E-09  6.291E-07 -3.376E-06  9.321E-07 -3.241E-06 -8.541E-06 -5.256E-06
           -3.099E-06  1.099E-05  3.090E-06 -7.466E-06  1.022E-05  6.744E-06 -2.121E-06 -8.441E-07 -2.685E-06  9.245E-06
           -5.068E-06  9.687E-07  3.466E-06  4.431E-07 -1.305E-05  9.802E-06 -5.828E-06 -7.716E-06  5.594E-07  3.602E-07
            6.432E-06 -5.368E-06  4.190E-07 -1.477E-06 -3.500E-06  5.259E-06  4.406E-06  1.242E-06 -1.110E-06 -1.664E-07
            3.831E-07  3.137E-06 -6.328E-06  1.042E-05  4.461E-07 -1.531E-06 -1.016E-05  4.497E-06  1.211E-05  1.247E-05
            1.357E-06 -1.382E-05 -9.120E-06  8.309E-08 -2.334E-06  5.764E-06 -2.463E-06  1.141E-07  3.563E-07  8.305E-07
            7.271E-06  6.440E-07 -6.951E-06 -3.498E-06 -1.012E-06  5.892E-06 -4.102E-06  5.596E-06  9.734E-06  3.691E-07
            2.800E-06 -1.062E-05 -2.096E-07  2.026E-06  4.537E-06 -3.074E-06 -1.378E-06  4.348E-06 -1.185E-06  2.200E-06
            3.696E-06 -1.674E-06  4.095E-07 -1.032E-05  6.432E-07  1.692E-06  3.091E-07 -9.407E-06  2.296E-07  4.257E-06
           -5.637E-06 -1.424E-05 -9.260E-07  1.489E-06  5.894E-06  1.641E-06 -1.275E-06  1.512E-07 -3.610E-06  2.578E-07
            2.938E-06 -3.195E-06  2.301E-06 -6.637E-06  3.324E-06  4.831E-06  5.913E-06 -2.954E-06  1.707E-06 -1.962E-06
           -2.487E-06 -2.577E-06  2.514E-06 -3.557E-06  4.695E-06 -2.140E-07  1.263E-06 -8.611E-06  1.947E-06 -1.678E-06
           -7.087E-06  3.617E-06  2.940E-06  1.140E-06  1.189E-06 -2.272E-06  7.222E-07  1.747E-08  4.561E-06 -2.477E-07
           -1.433E-05 -1.859E-06  2.068E-06  7.366E-08 -6.460E-06  1.811E-06  1.094E-05  3.195E-07  1.250E-05 -5.405E-06
            1.069E-06 -5.168E-06  4.099E-06  4.539E-06 -6.763E-06 -8.435E-07 -2.988E-06  2.138E-06 -7.293E-06  3.298E-06
            3.787E-06  1.228E-05  1.853E-06  1.184E-05  8.159E-06 -1.985E-06  1.146E-05  2.814E-06 -1.294E-06 -7.394E-06
            2.615E-06  7.663E-07  1.208E-05 -6.997E-06 -1.355E-05 -2.618E-06 -6.602E-06  5.064E-06 -1.503E-05 -1.526E-06
            2.342E-06 -3.569E-07  6.535E-06  1.829E-06 -6.659E-06  1.981E-06 -7.423E-06 -4.304E-07  6.050E-06 -3.161E-06
            4.754E-06  5.226E-08  1.043E-05  2.962E-06 -1.582E-05 -1.396E-05  1.456E-05  1.397E-05 -1.612E-05 -7.747E-07
           -1.045E-05 -3.728E-06 -1.108E-07 -9.430E-07  3.998E-07 -1.398E-06 -3.937E-06  2.900E-07 -1.615E-06  3.224E-06
            3.126E-07  3.756E-06  2.717E-07  9.574E-06 -2.484E-06  5.947E-07  7.775E-07  2.662E-07  4.878E-06 -1.095E-05
           -3.467E-06  4.513E-07  1.196E-06  9.649E-07  4.829E-06 -2.907E-07 -1.107E-05 -1.324E-05  9.948E-06 -8.251E-07
           -7.701E-06 -7.336E-06  5.643E-08  5.396E-06  1.831E-06  1.228E-06 -7.018E-07  4.407E-07  2.632E-06 -1.277E-05
           -2.984E-06  6.758E-08  2.205E-06 -1.252E-06 -9.931E-08 -3.128E-06  9.072E-06  2.512E-06 -1.430E-05  8.963E-07
           -1.399E-07 -4.502E-06  2.744E-08  7.050E-06 -4.995E-06  7.954E-06  2.429E-07 -8.226E-07  2.475E-06 -2.452E-06
            1.227E-05  5.127E-06 -4.132E-06  1.357E-06  1.493E-05 -6.277E-06 -1.361E-05  4.575E-07 -7.529E-06 -9.663E-06
           -1.551E-05  1.795E-06  2.507E-06  1.449E-06  1.385E-05  2.149E-06  3.937E-06  4.314E-06  4.509E-08  6.120E-07
            3.674E-06  5.500E-06 -2.714E-06  4.881E-06 -8.430E-06  4.513E-06 -3.060E-06 -1.047E-05  2.531E-06 -1.060E-06
           -6.886E-06  3.806E-07 -1.608E-05 -2.978E-06  1.434E-05 -9.352E-07  1.446E-05 -2.706E-06  4.660E-07  5.722E-07
            1.781E-06  3.772E-06 -1.776E-06 -3.435E-06  2.750E-07 -1.480E-05  2.339E-07 -1.107E-06 -2.711E-07  9.868E-06
            6.076E-06 -1.701E-06  2.136E-07  4.357E-07 -6.373E-06 -1.876E-06 -5.844E-06  5.378E-06  1.430E-05  1.173E-05
            2.347E-06  6.043E-06 -2.604E-06  4.045E-07 -1.758E-06 -6.162E-06 -1.106E-05  4.751E-06 -1.430E-05 -1.471E-05
            5.020E-06 -1.152E-05  5.716E-06 -1.118E-05 -1.065E-05 -6.835E-06 -7.942E-06 -3.224E-06 -4.036E-06 -1.438E-06
           -5.936E-07 -1.539E-06  2.688E-06 -1.241E-05 -8.330E-07 -9.519E-07 -6.451E-06  1.234E-05  6.591E-06 -3.834E-06
            2.524E-06 -9.775E-07 -8.452E-06 -1.342E-06 -2.445E-07 -2.050E-06 -7.076E-07 -8.055E-06  1.819E-06  1.103E-06
           -7.156E-06  6.163E-06 -1.577E-06  1.421E-06 -6.766E-07  7.383E-06 -7.447E-07  4.617E-06 -1.676E-07  4.536E-06
           -1.103E-06  3.414E-06 -1.217E-05  6.174E-06 -9.447E-06  5.818E-06 -5.068E-06  8.763E-06 -2.730E-06  4.129E-07
            6.178E-06  1.432E-06  7.916E-06 -7.160E-07 -1.414E-05 -5.171E-06  1.341E-05  4.527E-06  6.877E-06  7.207E-07
           -1.096E-05  1.013E-06  7.436E-06  9.246E-07 -1.019E-05  1.363E-05 -7.667E-06 -2.427E-06 -3.603E-07 -1.170E-05
            1.500E-06 -3.545E-06  6.613E-06 -2.270E-06 -6.291E-06 -6.522E-06  1.706E-06 -2.942E-06 -5.997E-08 -9.589E-07
            1.080E-05  9.104E-06 -3.770E-07  2.349E-06 -1.003E-06  5.656E-06  4.329E-06  5.298E-06  2.859E-06 -7.085E-06
           -5.179E-06  7.646E-07 -6.735E-07  2.872E-06 -1.460E-05  9.826E-07 -2.804E-06 -6.526E-06  3.378E-06  6.696E-06
           -1.756E-06  1.445E-05 -1.004E-05  1.007E-05 -2.774E-06  2.243E-06  8.896E-07 -1.386E-05 -6.640E-07 -8.725E-07
            1.220E-05  1.768E-07 -9.185E-07  4.458E-06  2.602E-06 -4.283E-06 -1.269E-06  1.207E-05 -1.225E-05  1.541E-06
           -1.599E-06 -6.500E-06  5.240E-06  3.901E-06 -3.746E-06  6.775E-06 -9.394E-06  9.383E-07  5.877E-06  3.044E-06
            1.221E-05  1.809E-06  2.326E-07 -9.821E-07 -2.249E-06 -2.358E-06 -1.876E-06 -2.341E-06 -7.588E-06  1.352E-06
            1.180E-05  3.279E-06  5.449E-06 -6.251E-06 -8.636E-06  1.649E-06 -3.912E-07 -1.327E-05  6.723E-07 -4.256E-06
            6.058E-07  5.990E-06 -6.578E-06  1.536E-05  7.578E-06 -4.615E-06 -1.438E-05  8.825E-06  2.506E-06  7.701E-06
           -8.840E-07  1.655E-06 -1.044E-06 -1.047E-05  1.451E-05 -3.557E-06 -1.395E-05  1.183E-05  6.531E-06  1.357E-05
           -7.244E-08  1.929E-07 -4.296E-07 -2.077E-06 -4.778E-07  1.533E-05 -1.473E-05  9.610E-06  6.010E-06  5.906E-06
           -1.752E-06  2.476E-06  1.590E-06 -1.289E-08 -2.945E-06 -4.976E-06 -4.642E-06  5.863E-07 -5.695E-06 -6.507E-06
           -4.615E-06 -8.418E-06 -2.068E-06  2.686E-07 -5.535E-06  4.035E-07  3.598E-08 -1.289E-05 -1.072E-06  5.140E-06
            6.362E-06  1.029E-05  2.481E-06 -1.172E-05 -1.374E-06 -5.282E-06  2.481E-06  2.900E-06  9.290E-06 -1.446E-05
           -6.993E-06 -5.442E-06  6.789E-07  7.762E-06  2.624E-06 -5.855E-06  5.761E-07  6.809E-06 -5.903E-08 -6.519E-06
            1.199E-06 -4.124E-06  1.385E-05 -5.788E-07 -2.832E-06  8.829E-06 -6.723E-06  1.847E-06  1.114E-05 -9.704E-06
           -1.504E-06 -4.027E-07 -6.484E-06  6.559E-06  3.352E-07 -1.119E-07 -2.948E-06  2.533E-06  2.180E-07 -1.400E-06
           -5.570E-06  8.391E-07 -3.448E-06 -1.059E-06 -3.357E-06  3.460E-06 -1.049E-05  2.928E-06 -6.591E-06 -7.025E-06
           -1.223E-05 -6.678E-06 -9.715E-06 -6.657E-06 -3.673E-06  9.825E-06  1.346E-06 -8.788E-07 -3.677E-08  4.264E-06
            5.345E-06 -8.784E-06  1.147E-05  2.541E-06 -1.162E-05  7.711E-06 -3.576E-06  2.768E-06  5.312E-06 -9.253E-07
           -6.292E-07  4.549E-06  4.793E-06  2.857E-06 -8.338E-06 -1.934E-06 -5.975E-07 -1.212E-06  4.516E-06 -4.170E-06
            1.967E-07 -5.339E-06  2.053E-06 -6.371E-06  4.268E-06 -5.553E-07  6.653E-06 -7.569E-08  2.433E-06 -1.470E-05
           -2.074E-06  6.748E-06  1.157E-06 -1.117E-06 -9.539E-06 -7.741E-07  1.928E-06  7.068E-06  6.055E-06 -3.427E-06
           -1.864E-06 -1.331E-06 -1.166E-05  5.493E-06  2.066E-06  1.407E-05  1.178E-05 -2.629E-06  8.275E-06  4.126E-06
            3.781E-06  4.529E-06  7.675E-08  5.806E-06  3.072E-06  4.570E-06  1.878E-08  2.380E-06  3.573E-06  3.293E-06
            4.805E-06 -5.502E-06  6.942E-06  9.890E-06  9.959E-06  1.557E-05 -2.350E-06  4.339E-06 -1.006E-05 -5.893E-06
           -6.093E-06 -7.830E-07  9.028E-08  3.706E-07  7.311E-09  1.574E-06 -3.619E-07 -2.641E-06  3.676E-06  5.680E-07
            9.227E-06  1.022E-06  1.349E-06 -2.845E-07 -9.871E-08  9.480E-06  1.243E-05  5.667E-07 -5.640E-07  4.032E-06
            5.074E-07 -8.799E-06 -1.084E-06  5.370E-06  8.330E-07 -8.999E-06 -3.052E-06 -3.011E-06 -1.084E-06 -5.885E-06
           -7.055E-06  1.395E-05 -4.121E-06  4.659E-06  2.416E-06 -5.701E-06  2.600E-06 -3.426E-06  9.786E-07  6.409E-06
nodes        448
edges       1337
triangles        890
geomv INP=(mgtest2,b) OUT=mgenlarg  +
   SIZE=(1,1,400,400)
Beginning VICAR task geomv
geomv version 16-jul-07
list mgenlarg linc=39 sinc=39
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:10 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:13 2007
     Samp     1      79     157     235     313     391
   Line
      1       1 130  25 131   4 132   5 134   7 135   0
     40      45 174  47 175  48 177  50 178  51 180   0
     79      90 218  91 220  92 221  94 223  95 224   0
    118     134   7 135   8 137   9 138  11 140  12   0
    157     178  51 180  52 181  54 182  55 184  57   0
    196     223  95 224  97 225  98 227  99 228 101   0
    235      11 140  12 141  14 142  15 144  16 145   0
    274      55 184  57 185  58 187  59 188  61 189   0
    313      99 228 101 230 102 231 104 232 105 234   0
    352     144  16 145  18 147  19 148  21 149  22   0
ibis-gen a version=ibis-1 org=column  datacol=(1,2,3,4)  +
    nc=4 nr=44
Beginning VICAR task ibis
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
Beginning VICAR task mf
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
Beginning VICAR task mf
mf a func=("c3=c1*1.1","c4=c2*1.1")
Beginning VICAR task mf
tieconv INP=a COLS=(1,2,3,4)  OUT=b  +
      NAH=7,NAV=6,MINL=1.,MINS=1.,MAXL=10.,MAXS=10.  +
    'MGEOM
Beginning VICAR task tieconv
tieconv version 28-mar-02
lsq fit x'=ax+by+c
            1.100E+00  1.214E-08 -7.396E-07
lsq fit y'=dx+ey+f
            7.215E-09  1.100E+00  4.174E-07
data
            4.573E+01  4.011E+00  6.240E+01  1.067E+02  3.950E+01  7.242E+01  1.038E+02  2.376E+01  5.218E+01  8.750E+01
            1.552E+01  6.043E+01  6.436E+00  7.353E+01  4.591E+01  3.358E+01  3.654E+01  5.898E+01  1.051E+02  6.491E+01
            4.839E+01  6.394E+01  9.898E+01  6.028E+01  5.785E+01  8.330E+01  4.341E+01  3.140E+01  6.405E+01  3.135E+01
            4.332E+01  9.995E+01  9.124E+01  1.719E+01  1.062E+02  2.825E+01  3.355E+00  3.152E+01  1.112E+01  4.378E+01
            2.789E+01  7.344E+01  7.043E+01  2.727E+01  2.017E+01  3.460E+01  1.547E+01  4.759E+01  4.614E+01  1.043E+02
            2.182E+00  7.806E+01  1.984E+00  1.039E+02  4.555E+01  5.519E+01  2.287E+01  5.020E+01  3.556E+01  8.897E+01
            1.004E+02  6.150E+01  9.062E+01  7.779E+01  2.299E+01  3.623E+01  1.091E+02  3.004E+01  1.901E+01  7.601E+01
            9.105E+01  6.413E+01  9.685E+01  9.601E+01  4.481E+01  7.004E+01  4.492E+01  8.784E+01  9.718E+01  5.618E+01
            8.321E+01  6.828E+01  1.978E+01  3.092E+01  1.094E-01  3.733E+01  4.099E+01  1.043E+02
residuals
           -1.426E-06  1.826E-07  1.248E-06 -1.013E-06 -2.610E-08 -7.548E-07 -3.489E-06 -1.630E-07 -1.010E-06  3.863E-07
            4.086E-07 -6.659E-07  6.548E-07 -1.858E-07  3.068E-07 -1.681E-06  9.772E-07 -1.080E-06  7.919E-08 -3.701E-06
            1.145E-06  1.360E-06  1.559E-06  1.141E-06  1.815E-07  2.329E-06 -2.412E-07 -5.877E-07  1.833E-06 -7.477E-07
            6.531E-07 -1.086E-06 -5.844E-07 -9.596E-07  1.505E-06  3.443E-07 -1.463E-07 -8.273E-07  6.157E-07 -1.113E-06
            1.354E-06  2.247E-06  1.522E-06 -5.429E-07  2.221E-07  1.439E-06 -9.329E-07 -1.151E-06  4.355E-07 -2.492E-06
           -1.124E-06  8.978E-07 -7.253E-07  3.513E-06  2.119E-06 -8.963E-08  2.872E-07  2.094E-07 -1.506E-06  8.257E-07
            3.529E-08  1.823E-06  4.170E-06  3.680E-06  3.935E-07  1.427E-06  3.826E-07 -7.097E-08 -8.102E-07 -1.017E-06
           -1.529E-06  2.002E-06 -2.431E-06 -2.216E-06  1.556E-06 -2.648E-06  1.241E-06 -1.355E-06 -1.945E-06 -1.024E-06
           -4.979E-07  9.083E-08 -8.858E-07 -3.449E-07 -5.952E-07 -5.439E-07  6.177E-07 -1.432E-06
nodes         48
edges        137
triangles         90
geomv INP=mgtest OUT=mgenlarg PARMS=b  +
   SIZE=(1,1,10,10)
Beginning VICAR task geomv
geomv version 16-jul-07
list mgenlarg
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:05 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:16 2007
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         8    52    96   140   184   228   272   316   360     0
      2        52    96   140   184   228   272   316   360   404     0
      3        96   140   184   228   272   316   360   404   448     0
      4       140   184   228   272   316   360   404   448   492     0
      5       184   228   272   316   360   404   448   492   536     0
      6       228   272   316   360   404   448   492   536   580     0
      7       272   316   360   404   448   492   536   580   624     0
      8       316   360   404   448   492   536   580   624   668     0
      9       360   404   448   492   536   580   624   668   712     0
gen mgtest2 1000 1000 SINC=3 LINC=7
Beginning VICAR task gen
GEN Version 6
GEN task completed
ibis-gen a version=ibis-1 org=column  datacol=(1,2,3,4)  +
    nc=4 nr=444
Beginning VICAR task ibis
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
Beginning VICAR task mf
mf a func=("c1=mod(c1,0.0001)*10000000","c2=mod(c2,0.0001)*10000000")
Beginning VICAR task mf
mf a func=("c3=c1*1.1","c4=c2*1.1")
Beginning VICAR task mf
tieconv INP=a COLS=(1,2,3,4)  OUT=b  +
      NAH=200,NAV=200,MINL=1.,MINS=1.,MAXL=1000.,MAXS=1000.  +
    'GEOMV
Beginning VICAR task tieconv
tieconv version 28-mar-02
lsq fit x'=ax+by+c
            1.100E+00  1.420E-09 -1.153E-06
lsq fit y'=dx+ey+f
            4.271E-10  1.100E+00 -6.837E-07
data
            4.573E+02  4.011E+01  6.240E+02  1.067E+03  3.950E+02  7.242E+02  1.038E+03  2.376E+02  5.218E+02  8.750E+02
            1.552E+02  6.043E+02  6.436E+01  7.353E+02  4.591E+02  3.358E+02  3.654E+02  5.898E+02  1.051E+03  6.491E+02
            4.839E+02  6.394E+02  9.898E+02  6.028E+02  5.785E+02  8.330E+02  4.341E+02  3.140E+02  6.405E+02  3.135E+02
            4.332E+02  9.995E+02  9.124E+02  1.719E+02  1.062E+03  2.825E+02  3.355E+01  3.152E+02  1.112E+02  4.378E+02
            2.789E+02  7.344E+02  7.043E+02  2.727E+02  4.555E+02  2.366E+02  7.162E+02  7.941E+02  4.704E+02  8.450E+02
            9.020E+02  6.413E+02  1.079E+03  1.828E+02  6.896E+01  8.213E+02  1.560E+02  2.730E+02  2.402E+02  9.896E+02
            4.892E+02  8.550E+02  1.071E+03  1.053E+03  8.854E+02  5.679E+02  1.005E+02  6.672E+02  1.000E+03  2.510E+02
            4.521E+02  5.033E+02  5.724E+02  5.757E+02  5.130E+02  3.844E+02  2.738E+02  9.726E+01  2.264E+01  9.821E+02
            8.595E+02  7.550E+02  7.523E+02  7.676E+02  8.010E+02  9.362E+02  7.339E+01  4.125E+02  8.535E+02  2.964E+02
            9.412E+02  6.719E+02  5.045E+02  5.229E+02  7.272E+02  1.033E+03  4.254E+02  8.717E+01  9.509E+02  9.843E+02
            1.876E+02  6.766E+02  2.516E+02  9.623E+01  2.946E+02  6.788E+02  2.327E+02  5.647E+01  2.339E+02  6.811E+02
            2.980E+02  2.686E+02  5.929E+02  8.701E+01  1.869E+01  2.201E+02  8.592E+02  6.680E+02  9.144E+02  4.146E+02
            3.523E+02  6.437E+02  2.727E+02  3.394E+02  8.436E+02  6.015E+02  8.810E+02  4.141E+02  4.688E+02  9.611E+02
            8.749E+02  1.264E+02  9.154E+02  2.599E+01  7.581E+02  9.117E+02  4.869E+02  4.997E+02  1.795E+01  5.773E+01
            6.190E+02  6.019E+02  9.018E+01  1.000E+02  7.153E+02  7.521E+02  3.783E+02  5.261E+02  9.533E+01  3.540E+02
            1.181E+02  4.876E+02  3.627E+02  8.431E+02  9.130E+02  4.883E+02  7.530E+02  5.231E+02  9.827E+02  9.476E+02
            5.020E+02  8.297E+02  6.628E+02  8.530E+01  1.972E+02  9.984E+02  2.891E+02  3.530E+02  6.355E+00  3.491E+02
            3.651E+02  5.438E+01  4.331E+02  4.011E+02  1.264E+02  5.411E+02  6.291E+02  3.904E+02  9.250E+02  3.286E+01
            1.014E+03  6.525E+02  9.803E+02  1.065E+03  8.236E+02  3.392E+02  6.280E+02  6.741E+02  4.774E+02  1.054E+03
            2.040E+02  2.950E+02  1.433E+02  7.649E+02  4.376E+01  2.637E+02  1.570E+02  9.914E+02  4.831E+02  8.320E+02
            9.381E+02  8.853E+02  5.898E+02  1.355E+02  5.383E+02  7.823E+02  7.836E+02  6.259E+02  3.095E+02  9.342E+02
            2.161E+02  5.231E+02  5.873E+02  5.766E+02  3.231E+02  1.095E+03  5.235E+02  9.774E+02  2.563E+02  3.925E+02
            4.538E+02  4.401E+02  1.836E+02  1.036E+03  6.299E+02  6.475E+01  5.247E+02  9.097E+02  1.198E+02  3.549E+02
            4.312E+02  4.326E+02  3.590E+02  2.944E+02  7.104E+01  8.727E+02  5.834E+02  1.352E+02  7.960E+02  2.819E+02
            8.767E+02  2.128E+02  7.417E+02  1.179E+01  3.909E+02  7.790E+02  1.092E+03  3.143E+02  6.454E+02  9.017E+02
            6.690E+01  3.411E+02  5.404E+02  8.326E+02  1.050E+03  2.601E+02  4.954E+02  7.397E+02  1.077E+03  2.391E+02
            5.943E+02  9.585E+02  2.317E+02  6.978E+02  1.089E+03  5.570E+02  1.050E+03  5.361E+02  1.150E+02  8.030E+02
            3.999E+02  8.974E+01  9.725E+02  7.643E+02  6.489E+02  6.265E+02  6.971E+02  7.766E+02  9.490E+02  1.983E+02
            5.566E+02  1.008E+03  4.519E+02  1.089E+03  7.189E+02  4.418E+02  3.415E+02  2.502E+02  3.358E+02  5.142E+02
            7.856E+02  1.339E+02  6.751E+02  2.091E+02  1.020E+03  8.239E+02  7.207E+02  7.943E+02  1.045E+03  2.881E+02
            8.083E+02  4.054E+02  9.536E+01  1.062E+03  5.847E+00  1.424E+02  4.559E+02  9.461E+02  5.132E+02  2.572E+02
            1.780E+02  2.756E+02  4.662E+02  9.175E+02  4.457E+02  1.507E+02  3.260E+01  9.130E+01  3.268E+02  8.232E+02
            3.963E+02  1.463E+02  7.316E+01  2.608E+02  6.252E+02  6.652E+01  8.686E+02  7.475E+02  8.872E+02  1.037E+02
            6.810E+02  3.351E+02  2.500E+02  4.256E+02  7.781E+02  2.914E+02  6.545E+01  1.633E+01  2.280E+02  7.004E+02
            2.496E+02  1.436E+02  2.144E+02  6.299E+02  1.222E+02  9.753E+02  9.891E+02  1.637E+02  6.991E+02  3.953E+02
            3.522E+02  6.538E+02  3.226E+01  8.554E+02  8.393E+02  1.084E+03  4.894E+02  1.557E+02  1.666E+02  4.382E+02
            9.707E+02  7.478E+02  7.857E+02  6.824E+01  7.116E+02  5.157E+02  6.645E+02  5.800E+01  8.123E+02  7.273E+02
            9.870E+02  4.914E+02  2.566E+02  3.664E+02  8.210E+02  4.363E+02  3.963E+02  7.010E+02  1.665E+02  6.056E+01
            2.154E+02  7.149E+02  4.592E+02  4.642E+02  8.978E+02  4.921E+02  5.151E+02  7.989E+02  4.112E+02  2.844E+02
            5.861E+02  4.862E+01  1.040E+03  1.756E+02  7.561E+02  5.813E+02  7.511E+02  1.657E+02  8.905E+00  1.968E+02
            2.017E+02  3.460E+02  1.547E+02  4.759E+02  4.614E+02  1.043E+03  2.182E+01  7.806E+02  1.984E+01  1.039E+03
            4.555E+02  5.519E+02  2.287E+02  5.020E+02  3.556E+02  8.897E+02  1.004E+03  6.150E+02  9.062E+02  7.779E+02
            2.299E+02  3.623E+02  1.091E+03  3.004E+02  1.901E+02  7.601E+02  9.105E+02  6.413E+02  9.685E+02  9.601E+02
            4.481E+02  7.004E+02  4.492E+02  8.784E+02  9.718E+02  5.618E+02  8.321E+02  6.828E+02  1.978E+02  3.092E+02
            1.094E+00  3.733E+02  4.099E+02  1.043E+03  2.402E+02  3.393E+01  5.920E+02  7.305E+02  4.493E+02  8.485E+02
            8.281E+02  4.720E+02  7.124E+02  6.171E+02  1.021E+02  2.676E+02  1.340E+01  5.235E+02  6.141E+02  2.850E+02
            6.362E+02  5.679E+02  1.639E+02  4.402E+02  2.970E+02  9.180E+02  1.945E+01  9.013E+02  3.474E+02  4.739E+02
            1.808E+02  6.520E+02  7.035E+02  3.355E+02  7.318E+02  7.084E+02  3.494E+02  6.707E+02  5.724E+02  1.385E+02
            3.849E+02  2.956E+02  8.867E+02  4.211E+01  9.779E+02  4.780E+02  6.585E+02  5.033E+02  1.028E+03  1.179E+02
            9.878E+02  4.220E+02  5.365E+02  3.154E+02  8.585E+02  9.821E+02  6.860E+02  1.382E+02  2.707E+02  1.084E+03
            4.609E+02  6.024E+02  4.083E+02  8.945E+02  1.045E+03  7.760E+02  1.712E+02  3.307E+02  1.546E+02  6.588E+02
            8.273E+02  6.602E+02  7.339E+01  2.509E+02  9.275E+01  6.989E+02  9.693E+02  8.202E+02  3.353E+02  6.148E+02
            5.585E+02  8.264E+01  3.711E+02  3.238E+02  1.041E+03  3.222E+02  3.679E+02  1.094E+03  4.682E+02  5.229E+02
            2.419E+02  7.252E+02  8.727E+02  6.846E+02  1.608E+02  3.174E+02  2.222E+02  8.913E+02  4.084E+01  3.857E+01
            7.167E+02  4.303E+01  4.975E+01  8.208E+02  2.401E+02  3.397E+02  1.876E+02  7.158E+02  9.923E+02  8.492E+02
            4.543E+02  7.398E+02  7.735E+02  4.715E+02  8.499E+02  9.766E+02  7.675E+02  2.228E+02  4.423E+02  4.101E+02
            1.058E+03  3.547E+02  3.315E+02  5.647E+01  5.458E+02  6.994E+02  5.174E+02  8.352E+01  3.301E+02  3.248E+02
            1.084E+03  4.073E+02  5.789E+02  3.309E+02  9.312E+02  9.570E+01  2.455E+01  8.016E+02  1.430E+02  1.648E+02
            1.869E+01  5.530E+02  8.355E+02  7.824E+02  3.935E+02  8.529E+02  9.766E+02  7.646E+02  3.008E+02  6.013E+02
            5.661E+02  2.792E+02  7.566E+02  8.982E+02  7.042E+02  2.584E+02  6.609E+02  6.437E+02  4.587E+02  9.540E+02
            9.764E+01  5.534E+00  7.617E+02  8.210E+01  1.668E+02  1.100E+03  6.810E+02  1.027E+03  1.036E+03  7.944E+02
            2.167E+02  4.034E+02  3.383E+02  2.140E+01  4.688E+02  6.645E+02  5.244E+02  4.871E+01  4.212E+02  5.419E+02
            3.270E+02  9.602E+02  1.578E+02  1.197E+02  1.014E+03  3.881E+02  6.947E+02  6.656E+02  3.007E+02  7.842E+02
            1.016E+03  9.117E+02  5.559E+02  9.644E+02  2.112E+01  1.026E+03  5.953E+02  1.013E+03  1.095E+03  9.246E+02
            5.029E+02  9.293E+02  1.020E+03  7.752E+02  3.624E+02  7.140E+02  7.299E+02  5.779E+02  9.018E+01  4.507E+02
            4.756E+02  2.487E+02  8.700E+02  1.395E+02  1.734E+02  1.055E+03  5.858E+02  8.804E+02  9.232E+02  7.143E+02
            2.536E+02  6.411E+02  6.930E+02  4.931E+02  4.137E+01  3.540E+02  4.148E+02  3.078E+02  9.651E+02  1.867E+02
            3.405E+02  1.585E+02  8.248E+02  1.394E+02  3.021E+02  2.131E+02  8.884E+02  2.119E+02  4.676E+02  3.876E+02
            1.072E+03  4.883E+02  7.530E+02  6.820E+02  4.431E+02  9.685E+02  1.422E+02  1.641E+02  1.034E+03  5.526E+02
            8.353E+02  9.501E+02  8.131E+02  3.405E+02  7.161E+02  9.238E+02  7.959E+02  4.161E+02  8.846E+02  8.530E+01
            5.026E+01  8.635E+02  3.249E+02  6.345E+02  6.924E+02  4.985E+02  5.289E+01  3.715E+02  5.223E+02  4.214E+02
            6.865E+01  5.641E+02  8.079E+02  7.998E+02  5.400E+02  2.842E+01  3.651E+02  4.499E+02  2.830E+02  1.048E+03
            3.779E+02  5.557E+02  4.817E+02  2.399E+02  7.624E+02  1.699E+01  1.199E+02  9.870E+02  5.864E+02  1.034E+03
            1.297E+02  7.373E+01  9.499E+02  3.904E+02  6.791E+02  7.999E+02  6.690E+02  2.864E+02  7.519E+02  9.657E+02
            9.277E+02  6.379E+02  1.803E+02  5.710E+02  6.259E+02  5.969E+02  2.322E+02  7.158E+02  1.031E+03  1.011E+03
            8.236E+02  4.679E+02  8.766E+02  1.033E+03  1.022E+03  7.596E+02  2.451E+02  6.627E+02  7.446E+02  6.587E+02
            3.210E+02  8.315E+02  7.423E+01  1.651E+02  4.293E+00  6.917E+02  2.728E+01  2.950E+02  3.110E+02  7.525E+01
            7.716E+02  1.162E+02  3.091E+02  2.501E+02  2.334E+01  6.448E+02  1.014E+03  1.163E+02  6.636E+01  8.646E+02
            3.112E+02  6.898E+02  8.167E+02  7.758E+02  4.831E+02  1.039E+03  2.424E+02  3.783E+02  2.624E+02  9.947E+02
            4.593E+02  7.720E+02  9.169E+02  8.100E+02  4.514E+02  1.025E+03  2.466E+02  3.166E+02  2.187E+02  9.691E+02
residuals
           -9.139E-06  1.352E-06 -1.273E-05  3.817E-05 -8.108E-06 -3.873E-06  1.497E-05 -6.592E-06 -7.268E-06 -1.072E-06
            1.738E-06 -1.282E-05 -1.728E-07  2.686E-06 -6.326E-06 -7.363E-06 -8.159E-06 -3.723E-07 -2.307E-05 -2.630E-05
            1.160E-05 -1.944E-05 -9.759E-06  1.806E-05 -1.178E-05  1.242E-05  2.662E-06 -9.614E-06 -2.025E-05  1.440E-05
           -1.198E-05 -9.468E-06  1.108E-05  6.930E-06  7.123E-06  3.352E-07  4.872E-07  5.565E-06 -1.543E-06  3.357E-06
           -3.885E-07 -9.335E-06  1.569E-05 -1.228E-05 -1.220E-05  2.022E-06 -2.142E-05  2.552E-05 -5.897E-07  2.425E-05
            2.913E-05 -7.420E-06  9.808E-07 -6.016E-07  2.560E-06 -2.331E-05  5.342E-06  5.169E-06 -1.856E-06 -2.612E-06
            1.096E-05  1.218E-05  2.628E-05  4.462E-05  1.797E-05 -6.386E-06 -3.145E-07  9.768E-06 -1.513E-05 -6.491E-06
           -1.205E-05 -1.073E-05 -6.208E-06  6.402E-06  2.558E-05 -8.194E-06 -8.356E-06 -2.610E-06  6.801E-07  3.846E-06
           -1.209E-05 -1.579E-05 -1.649E-05 -1.574E-05 -1.158E-05 -1.690E-06  2.508E-06 -1.160E-05  5.518E-06  1.586E-05
            9.749E-06 -8.027E-06  1.381E-05 -2.904E-05  2.071E-05 -4.720E-05  9.152E-08 -9.379E-07 -2.005E-05 -3.527E-06
            1.021E-06 -1.447E-05 -4.892E-06  2.465E-06  8.601E-06  1.578E-05 -5.700E-06  1.394E-06 -2.651E-06 -8.533E-06
           -3.484E-07 -4.066E-06  1.857E-05 -3.169E-07  4.321E-07 -1.527E-06 -1.284E-05  2.206E-05 -1.028E-06 -1.179E-05
            7.963E-06  2.355E-05  8.424E-06 -9.761E-06  1.182E-05  2.416E-05  3.018E-05 -9.344E-06  8.575E-06  2.212E-05
           -1.225E-05 -3.318E-06 -7.847E-06  1.034E-06  1.483E-05 -9.459E-07 -3.711E-06 -1.401E-05  3.246E-07  1.744E-06
           -1.335E-05 -6.002E-06  2.925E-06  1.714E-06 -2.095E-05  2.527E-06  4.819E-06  8.964E-07 -6.934E-07 -1.381E-05
            2.554E-06  1.086E-05 -1.697E-06  1.257E-05  4.443E-06  1.360E-05  8.059E-06  1.991E-05  2.175E-05 -2.627E-05
           -2.070E-06  1.301E-05 -1.992E-05  7.345E-07 -2.351E-06  2.763E-06 -8.908E-06 -5.955E-07  6.348E-07 -8.199E-07
           -1.131E-05  2.486E-06  6.165E-06  7.185E-06 -5.339E-07 -1.694E-05 -6.575E-06  6.814E-06  1.730E-05  2.127E-06
           -2.804E-06  2.885E-05 -9.221E-06  1.950E-05  8.889E-07 -1.349E-05  2.274E-05  3.689E-06 -3.605E-06 -2.274E-05
            2.051E-06  1.263E-05 -1.442E-06 -2.289E-05 -1.479E-06  1.487E-05 -1.619E-06  1.520E-05  1.133E-05 -6.121E-06
            1.707E-05 -1.827E-05  1.775E-05 -5.739E-06 -1.715E-06  8.833E-07 -1.691E-05  2.882E-05 -3.922E-06  4.050E-06
           -3.867E-06  1.366E-06  4.105E-08  6.851E-07  8.720E-06 -6.034E-05 -7.955E-06  2.848E-05  1.177E-05  1.321E-05
            2.984E-06 -3.637E-06 -1.553E-06 -2.173E-05  2.265E-05  3.427E-06  4.006E-06  1.086E-05 -3.365E-07 -1.375E-05
            5.642E-06 -3.409E-06 -1.049E-05 -1.265E-05  3.300E-07  5.099E-06  1.811E-05  2.984E-06 -2.993E-05 -9.275E-06
           -6.521E-06 -4.714E-06 -2.254E-05 -2.974E-07  1.266E-06 -1.075E-05  1.288E-05  2.668E-06  2.960E-05  5.207E-06
            3.267E-06  8.608E-06  1.869E-05  1.019E-06  4.504E-05  7.817E-06  1.394E-05 -2.231E-05  3.127E-05  4.140E-06
           -1.221E-05 -1.460E-05 -3.298E-06  1.573E-05 -4.110E-05 -1.152E-05 -5.904E-05 -2.931E-05  1.961E-06  1.382E-05
           -8.059E-06 -2.575E-07 -1.514E-05  2.693E-05  4.838E-06 -1.287E-05  2.744E-05  1.436E-05 -1.957E-06  8.801E-07
            1.503E-05 -2.769E-05  8.579E-06 -3.582E-05 -2.740E-05  1.157E-05 -9.573E-06  3.010E-08  7.711E-06 -1.389E-05
            2.557E-05 -6.062E-06  3.606E-06  3.755E-06 -2.215E-05  1.241E-05 -2.179E-05  7.607E-06 -2.291E-05 -5.277E-06
           -1.054E-05  6.405E-06  1.693E-06  5.028E-05  4.644E-07  8.066E-06 -8.915E-06  4.334E-06 -2.604E-05 -9.611E-06
            4.789E-06 -1.325E-05 -9.613E-07 -7.799E-06  8.989E-06 -5.242E-06 -1.178E-06  2.384E-06 -3.330E-06  5.388E-08
            1.127E-06 -5.826E-06  4.267E-06  1.038E-05 -1.339E-06  1.197E-06 -5.848E-06  2.621E-05 -7.538E-07 -2.456E-06
            1.657E-05  5.914E-06  3.598E-06  4.675E-07  7.621E-06  5.935E-06 -3.054E-06  1.092E-06 -1.767E-06 -2.766E-05
            6.686E-06 -5.872E-06 -5.308E-06 -2.561E-05 -2.335E-06 -2.101E-05 -8.636E-06  2.741E-06  2.111E-05  1.254E-05
           -1.533E-06  1.062E-05 -3.050E-07 -6.738E-06  1.804E-05  3.743E-05 -1.295E-05 -6.169E-06 -4.988E-06 -6.258E-06
            2.217E-05  1.987E-06 -2.228E-05  4.020E-06  3.849E-06  2.252E-05 -2.567E-05  2.170E-06  1.345E-05  3.413E-06
           -9.286E-06 -6.212E-07  1.496E-05 -4.156E-06 -1.078E-05 -3.147E-06  3.357E-06  3.936E-06  6.570E-06  1.378E-06
           -5.500E-06 -1.542E-05  2.233E-06 -1.308E-05  2.966E-05 -4.880E-06 -1.351E-05 -2.296E-05  7.052E-06  5.838E-06
           -8.963E-08  1.378E-06 -1.063E-05 -3.744E-06  8.400E-06 -1.292E-05  2.098E-05 -2.891E-06  8.657E-07  6.266E-06
           -1.121E-06  1.095E-05  1.430E-06  1.364E-05  2.032E-06  1.282E-05 -4.165E-07  7.898E-07 -3.606E-07  4.337E-05
           -1.300E-05 -1.846E-05 -6.984E-08  7.062E-06  1.057E-05  2.874E-05 -4.444E-06 -2.602E-05  3.664E-06 -1.762E-05
           -3.312E-06 -1.867E-06  2.381E-05  1.484E-05 -3.951E-06  2.545E-05  3.804E-06 -8.214E-06  2.679E-05  8.804E-06
            5.378E-06 -1.596E-05  5.166E-06  1.686E-05  2.137E-06 -2.529E-07 -1.864E-05 -2.138E-05 -3.948E-06  2.494E-06
            5.441E-07 -1.132E-05 -1.520E-05 -6.024E-05 -2.014E-06  1.330E-06  4.981E-06  1.738E-06  8.387E-06  5.083E-06
            5.534E-06 -1.052E-05  2.036E-05  2.902E-05  5.760E-07  4.838E-06  5.982E-07 -2.683E-05  1.076E-05 -2.085E-07
           -2.648E-05 -6.721E-06 -2.009E-06 -6.888E-06  1.176E-05 -1.473E-05  1.933E-07  2.224E-05 -4.713E-06  1.095E-05
           -5.212E-06 -8.535E-06  1.466E-05 -1.343E-05  1.817E-06  2.682E-05  1.627E-06  2.171E-05 -6.503E-06 -5.199E-06
           -2.476E-06 -3.644E-07 -7.974E-06 -1.354E-06 -4.006E-06  7.540E-06  9.791E-06 -1.115E-05 -4.792E-05 -2.167E-06
           -2.870E-05 -6.316E-06 -2.722E-05 -1.294E-05  1.710E-05  1.910E-06 -9.295E-06  7.362E-06 -2.915E-06 -4.271E-07
           -3.979E-06 -1.966E-05  3.320E-06  1.651E-05 -6.030E-05 -5.377E-06  2.725E-06 -7.012E-06  3.110E-06 -2.707E-05
            2.410E-05  2.188E-05  2.565E-06 -3.646E-06 -1.476E-06  1.490E-05  8.377E-06  1.191E-05 -4.399E-06  2.288E-05
           -3.256E-06  2.331E-06  1.028E-05  1.440E-05  3.724E-05 -9.230E-07  7.064E-06 -6.148E-05 -7.311E-06 -3.013E-05
            2.361E-06 -1.009E-05 -1.610E-06  1.531E-05 -3.342E-06 -7.035E-06 -4.661E-06  1.643E-05  1.107E-07 -6.151E-07
            1.433E-05 -1.668E-07  1.402E-06  1.212E-05 -5.860E-07  4.717E-06  7.624E-07 -1.613E-05  2.037E-06  2.967E-05
            5.356E-06  2.605E-05 -5.197E-06 -1.429E-06 -7.184E-06  2.056E-05 -5.209E-06  5.993E-06  5.298E-06  9.111E-06
           -6.069E-05 -1.397E-05 -7.264E-06  1.248E-06 -3.037E-05  8.482E-06 -1.143E-05 -2.154E-06  1.133E-05 -1.004E-05
            1.812E-05  1.257E-05 -1.292E-05  8.109E-06  9.547E-06 -3.273E-06  6.328E-07 -3.027E-05 -1.468E-06  1.426E-06
           -3.345E-07  1.507E-05 -3.129E-05  1.874E-05 -1.182E-05 -9.260E-07 -1.001E-05 -1.121E-05 -6.480E-06 -1.978E-05
           -2.474E-05  9.350E-06  2.580E-05  2.227E-05  2.095E-05  1.442E-05  2.191E-05  2.198E-05 -4.043E-06  1.485E-05
           -1.183E-06  5.453E-07 -2.942E-05  2.541E-06  5.759E-06  3.590E-05 -9.318E-06  4.987E-05  9.327E-07  1.241E-05
            1.675E-06  9.432E-06 -1.044E-05  1.108E-06  7.991E-06  1.536E-05  3.571E-06 -6.291E-08  3.019E-06 -1.509E-05
            5.128E-06 -2.177E-05 -3.049E-06 -2.066E-07 -2.918E-05  8.086E-07  1.488E-05 -2.732E-05  2.815E-06  1.287E-05
           -2.916E-05 -2.328E-06 -3.199E-06 -1.570E-05  9.289E-07 -2.957E-05  4.955E-06  2.596E-05  4.822E-05  2.185E-05
           -2.168E-06  2.176E-05 -1.107E-05  2.542E-05  1.278E-06  2.654E-05 -4.467E-06 -2.505E-05  2.155E-06  1.429E-05
           -1.343E-05 -2.167E-06  2.301E-05  5.516E-06  3.883E-06  6.612E-06 -8.924E-07  2.880E-05  9.363E-06 -3.775E-06
           -8.537E-07  1.595E-05 -3.278E-06 -1.408E-05  8.269E-07 -1.385E-05 -1.933E-07 -1.277E-05  2.097E-05  6.721E-06
            4.836E-06 -3.030E-06 -3.104E-05  4.783E-06 -9.630E-06 -2.978E-06 -7.992E-06 -6.061E-06  1.083E-05  6.873E-06
           -4.882E-05 -1.104E-05  7.452E-06  2.749E-06  5.383E-06 -9.753E-06  4.881E-06  1.357E-06 -1.734E-05 -1.234E-05
           -1.900E-05 -2.141E-05 -1.844E-05 -7.297E-06 -1.633E-05 -1.497E-05 -1.196E-05 -1.245E-07 -8.040E-06  1.186E-07
            1.492E-06  1.101E-05 -3.840E-06  2.827E-05 -2.759E-05  1.348E-05  8.055E-07  3.903E-06 -2.079E-05 -9.193E-06
           -2.123E-06 -1.862E-05 -1.214E-05  8.067E-08  1.850E-05  1.492E-06 -1.085E-05 -6.360E-07 -1.211E-05 -4.227E-05
           -1.129E-05 -2.752E-05 -1.052E-05 -1.931E-06 -1.114E-05  1.503E-06 -3.198E-06 -1.640E-05  1.115E-05 -3.555E-05
            5.039E-06 -1.157E-06 -2.756E-05  6.720E-06  2.735E-05 -1.189E-05  1.565E-05 -1.207E-05  7.563E-06  2.683E-05
           -2.627E-06  2.855E-05  7.097E-06  2.992E-05 -7.782E-06  2.308E-05  2.541E-06  2.674E-05 -1.131E-05  1.979E-05
           -2.521E-07  7.883E-06  4.748E-06  3.741E-05  7.154E-06 -1.126E-05  5.488E-06  3.558E-06 -2.276E-05  1.575E-05
            2.014E-06 -3.111E-05  9.452E-07  1.403E-06  2.124E-07  8.850E-06 -3.979E-07  1.213E-05 -6.848E-06  2.470E-06
           -2.371E-05 -2.204E-06  1.478E-05  8.416E-07  2.048E-07  2.218E-05  1.406E-06 -7.609E-07  2.894E-06  2.943E-05
           -3.568E-06  8.815E-06 -6.238E-06 -1.750E-05  1.049E-05  1.912E-05 -3.613E-06 -1.451E-05  9.695E-06 -1.030E-05
           -1.020E-05 -2.335E-05  2.783E-05  2.455E-05 -9.274E-07  1.330E-05 -5.330E-06 -9.778E-06  4.761E-06  2.537E-06
nodes        448
edges       1337
triangles        890
geomv INP=(mgtest2,b) OUT=mgenlarg  +
   SIZE=(1,1,1000,1000)
Beginning VICAR task geomv
geomv version 16-jul-07
list mgenlarg linc=199 sinc=199
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jul 19 12:27:17 2007
 Task:GEOMV     User:lwk       Date_Time:Thu Jul 19 12:27:23 2007
     Samp     1     399     797
   Line
      1       1 146  34 179  68   0
    200     228 142  31 175  64   0
    399     250 138  27 172  60   0
    598     246 135  23 168  57   0
    797     242 131  20 164  53   0
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC

NOTE:  THE LINUX LOG FILE IS IDENTICAL TO THIS ONE, SO IS NOT
INCLUDED IN THE DELIVERY.
$ Return
$!#############################################################################
