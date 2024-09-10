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
   
   status = zvunit(&geounit,inp,instance, NULL);
   status = zvopen(geounit,"OP","READ","OPEN_ACT","SA",
         "LAB_ACT","SA", NULL);
      
   strcpy(buf,"");
   do
      {
      status=zlninfo(geounit,key,valformat,&maxlen,
         &nelement,"ERR_ACT"," ", NULL);
      if (status!=1) break;
      if (strcmp(key,"PROPERTY")==0) continue;
      if (strcmp(key,"NL")==0)
         {
         status=zlget(geounit,"SYSTEM",key,nl,
            "ERR_ACT","SA","FORMAT","INT", NULL);
         }
      if (strcmp(key,"NS")==0)
         {
         status=zlget(geounit,"SYSTEM",key,ns,
            "ERR_ACT","SA","FORMAT","INT", NULL);
         }
      status=zlinfo(geounit,"PROPERTY",key,vformat,
         &maxlen,&nelement,"ERR_ACT"," ",
         "PROPERTY","GEOTIFF", NULL);
      if (status!=1) continue;
      if (strcmp(key,"PROPERTY")==0) continue;
      /* now concatenate the string values / can be vector */
      
      for (i=1;i<=nelement;i++)
         {
         if (nelement==1)
            status=zlget(geounit,"PROPERTY",key,svalue,
               "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
               "PROPERTY","GEOTIFF","ULEN",133, NULL);
         else
            status=zlget(geounit,"PROPERTY",key,svalue,"ELEMENT",i,
               "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
               "PROPERTY","GEOTIFF","ULEN",133, NULL);
         strcat(buf,key);
         strcat(buf,"=");
         strcat(buf,svalue);
         strcat(buf,"\n");
         }
      }
   while (1);
   status = zvclose(geounit, NULL);
   
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
   
   status=zvunit(&geounit,fileparm,nfile, NULL);
   status=zvopen(geounit,"OP","UPDATE","OPEN_ACT","SA",
	"LAB_ACT","SA", NULL);
   if (!add) do
      {
      /*seems only way to delete properties, note ERR_ACT*/
      status=zlninfo(geounit,key,valformat,&maxlen,
      &nelement,"ERR_ACT"," ", NULL);
      if (status!=1) break;
      if (strcmp(key,"PROPERTY")==0) continue;
      status=zlinfo(geounit,"PROPERTY",key,valformat,
      &maxlen,&nelement,"PROPERTY","GEOTIFF",
      "ERR_ACT"," ", NULL);
      if (status!=1) continue;
      status=zldel(geounit,"PROPERTY",key,
      "ERR_ACT","SA","PROPERTY","GEOTIFF", NULL);
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
         "PROPERTY","GEOTIFF", NULL);
      }
   while (p<q);
   zvclose(geounit, NULL);
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
  status = zvunit( &i_unit, "INP", 1, NULL);
  status = zvopen( i_unit, "OPEN_ACT", "SA", "IO_ACT", "SA", NULL);
  /* should have checked status to make sure file opened properly. */
  /* Make sure we have either BYTE or HALF */
  zvget(i_unit,"FORMAT",fmt_str, NULL);
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
  zvget(i_unit,"NL",&inpline,"NS",&inpsamp, NULL);
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
  
  status=zvunit( &o_unit, "OUT", 1, NULL);
  /* note that zvopen is intelligent enough to default to the same */
  /* format as the input file.  */
  status=zvopen( o_unit, "U_NL", nline, "U_NS", nsamp,
		"OP", "WRITE",
		"OPEN_ACT", "SA",
		"IO_ACT", "SA", NULL);
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
  		   "SAMP", ss+1, "NSAMPS", ns, NULL);
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
  		   "SAMP", ss+1, "NSAMPS", ns, NULL);
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
                  "SAMP",1,"NSAMPS", newns, NULL);
         else zvwrit(o_unit,boutbuf,"LINE",outcount,
                  "SAMP",1,"NSAMPS", newns, NULL);
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
   
void main44(void)
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
      status = zvunit(&gridIn,"inp",2, NULL);
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
               "SAMP",1,"NSAMPS", newns, NULL);
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
                  "SAMP",1,"NSAMPS", newns, NULL);
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
   zvclose(o_unit, NULL);
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
      zvclose(o_unit, NULL);
      }
   
   zvclose(i_unit, NULL);
   return 0;
}
