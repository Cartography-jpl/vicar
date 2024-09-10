$!****************************************************************************
$!
$! Build proc for MIPL module tracker3
$! VPACK Version 1.9, Thursday, June 15, 2000, 16:56:26
$!
$! Execute by entering:		$ @tracker3
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
$ write sys$output "*** module tracker3 ***"
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
$ write sys$output "Invalid argument given to tracker3.com file -- ", primary
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
$   if F$SEARCH("tracker3.imake") .nes. ""
$   then
$      vimake tracker3
$      purge tracker3.bld
$   else
$      if F$SEARCH("tracker3.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tracker3
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tracker3.bld "STD"
$   else
$      @tracker3.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tracker3.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tracker3.com -
	-s tracker3.c call_kqkkor.c -
	-i tracker3.imake -
	-p tracker3.pdf -
	-t tsttracker3.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tracker3.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* TRACKER3 */
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "vicmain_c"
#include "kqkkor.h"

#define MAX_LEFT_AREA 50 
#define MAX_RIGHT_AREA 150 

main44()
{
 int unit1,unit2,unit3,unit4,status,nlw,nsw,nlw2,nsw2,count,def,grid;
 int nsout,ptcount,output_line,nl1,ns1,nl,ns,left_line,left_samp;
 int i,ind,right_line,right_samp,mode,limits,pmode,zero;
 int nids,input_line,inpt_count,nl4,ns4,extend,ascii;
 int tmpint; 
 float inbuf[32][4],obuf[32][4],line1,samp1,line2,samp2,percent;
 float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
 float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
 float correl[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
 float line_offset,samp_offset;
 float line_coef[3],samp_coef[3];
 float line_coef_limits[3][2],samp_coef_limits[3][2];
 float line_temp[3],samp_temp[3],quality,line_range,samp_range;
 float coefrange[4],quality_limit,dn_left,dn_right;
 char correl_scheme[8],printit[8],zeromode[8],extendmode[8];
 char msg[80],asciifile[80];
 char header[] = "linel sampl liner sampr  dnl    dnr     X     Y     Z    Dr"; 
 FILE *fd;

 /* Nessesary variables for the KQKKOR correlation algorithm */
 int kq_nlw, kq_nsw, kq_nlw2, kq_nsw2;  /* New sizes for template and
					   search windows */
 KORPAR info; /* correlation parameters */
 short int made_good_guess; /* Control variable for KQKKOR guess loop */

/* get parameters */
 status=zvparm("NLW",&nlw,&count,&def,1,0);
 status=zvparm("NSW",&nsw,&count,&def,1,0);
 status=zvparm("NLAREA",&nlw2,&count,&def,1,0);
 status=zvparm("NSAREA",&nsw2,&count,&def,1,0);
 status=zvparm("GRID",&grid,&count,&def,1,0);
 status=zvparm("PERCENT",&percent,&count,&def,1,0);
 status=zvparm("LIMIT",&limits,&count,&def,1,0);
 status=zvparm("QUALITY",&quality_limit,&count,&def,1,0);
 status=zvparm("CORREL",correl_scheme,&count,&def,1,0);
 if(strcmp(correl_scheme,"LINEAR") == 0) mode=0;
 if(strcmp(correl_scheme,"ANNEAL") == 0) mode=1;
 if(strcmp(correl_scheme,"AMOEBA") == 0) mode=2;
 if(strcmp(correl_scheme,"LINAMO") == 0) mode=3;
 if(strcmp(correl_scheme,"ANNAMO") == 0) mode=4;

 if(strcmp(correl_scheme,"KQKKOR") == 0) mode=5;

 status=zvparm("COEFRANG",coefrange,&count,&def,4,0);
 pmode=0;
 status=zvparm("PRINT",printit,&count,&def,1,0);
 if(strcmp(printit,"PRINT") == 0) pmode=1;
 zero=0;
 status=zvparm("ZERO",zeromode,&count,&def,1,0);
 if(strcmp(zeromode,"ZERO") == 0) zero=1;
 extend=0;
 status=zvparm("EXTEND",extendmode,&count,&def,1,0);
 if(strcmp(extendmode,"EXTEND") == 0) extend=1;
 status=zvparm("ASCIIFILE",asciifile,&count,&def,1,0);
 ascii=1;
 if(strcmp(asciifile,"NONE") == 0) ascii=0;

/* check oddness */
 if(nlw == (nlw/2)*2) nlw += 1;
 if(nsw == (nsw/2)*2) nsw += 1;
 if(nlw2 == (nlw2/2)*2) nlw2 += 1;
 if(nsw2 == (nsw2/2)*2) nsw2 += 1;

/* check overflows */
 if(nlw > MAX_LEFT_AREA){
   sprintf(msg,"nlw exceeds limit of %d",MAX_LEFT_AREA);
   zvmessage(msg," ");
   zabend();
 }
 if(nsw > MAX_LEFT_AREA){
   sprintf(msg,"nsw exceeds limit of %d",MAX_LEFT_AREA);
   zvmessage(msg," ");
   zabend();
 }
 if(nlw2 > MAX_RIGHT_AREA){
   sprintf(msg,"nlarea exceeds limit of %d",MAX_RIGHT_AREA);
   zvmessage(msg," ");
   zabend();
 }
 if(nsw2 > MAX_RIGHT_AREA){
   sprintf(msg,"nsarea exceeds limit of %d",MAX_RIGHT_AREA);
   zvmessage(msg," ");
   zabend();
 }


/* open files */
 status=zvpcnt("INP",&nids);
 nsout=128;
 status=zvunit(&unit1,"INP",1,0);
 status=zvopen(unit1,"OP","READ","U_FORMAT","REAL",0);
 status=zvunit(&unit2,"INP",2,0);
 status=zvopen(unit2,"OP","READ","U_FORMAT","REAL",0);
 status=zvunit(&unit3,"OUT",1,0);
 status=zvopen(unit3,"OP","WRITE","U_FORMAT","REAL","U_NS",nsout,
               "U_NL",100,"O_FORMAT","REAL",0);
 if(nids > 2){
   status=zvunit(&unit4,"INP",3,0);
   status=zvopen(unit4,"OP","READ","U_FORMAT","REAL",0);
 }
 if(ascii == 1){   /* open ascii file "asciifile" */
   if ((fd = fopen(asciifile, "w")) == NULL) {
      fprintf(stderr, "Error opening file %s\n", asciifile);
      zabend();
   }
 }

/* fill initial records in the ascii file */
 if(ascii == 1){
   fprintf(fd,"%s\n","Tracker3 ascii tiepoints file");
   status=zvpone("INP",msg,1,80); /* first input filename */
   fprintf(fd,"%s\n",msg);
   status=zvpone("INP",msg,2,80); /* second input filename */
   fprintf(fd,"%s\n",msg);
   fprintf(fd,"%s\n","Placeholder");
   fprintf(fd,"%s\n","Placeholder");
   fprintf(fd,"%s\n",header);
 }

/* get input image dimensions */
 status=zvget(unit1,"NL",&nl1,"NS",&ns1,0);
 status=zvget(unit2,"NL",&nl,"NS",&ns,0);
 if(nl > nl1) nl=nl1;
 if(ns > ns1) ns=ns1;
 if(nids > 2){
    status=zvget(unit4,"NL",&nl4,"NS",&ns4,0);
 }   

/* initialize constants */
 inpt_count=32;
 input_line=0;
 ptcount=0;
 output_line=0;
 left_samp=(nsw2/2)+1-grid;
 left_line=(nlw2/2)+1 ;
 line_range=((float)nlw2-(float)nlw)/2.0;
 samp_range=((float)nsw2-(float)nsw)/2.0;

/* set coefficient boundary limits */
 line_coef_limits[0][0]=coefrange[2];
 line_coef_limits[0][1]=coefrange[3];
 line_coef_limits[1][0]=coefrange[0];
 line_coef_limits[1][1]=coefrange[1];
 line_coef_limits[2][0]= -line_range;
 line_coef_limits[2][1]= line_range;
 samp_coef_limits[0][0]=coefrange[0];
 samp_coef_limits[0][1]=coefrange[1];
 samp_coef_limits[1][0]=coefrange[2];
 samp_coef_limits[1][1]=coefrange[3];
 samp_coef_limits[2][0]= -samp_range;
 samp_coef_limits[2][1]= samp_range;

/* set initial coefficient temperature values */
 line_temp[0]=(line_coef_limits[0][1]-line_coef_limits[0][0])/12.0;
 line_temp[1]=(line_coef_limits[1][1]-line_coef_limits[1][0])/12.0;
 line_temp[2]=line_range/12.;
 samp_temp[0]=(samp_coef_limits[0][1]-samp_coef_limits[0][0])/12.0;
 samp_temp[1]=(samp_coef_limits[1][1]-samp_coef_limits[1][0])/12.0;
 samp_temp[2]=samp_range/12.;


 
/* acquire a new tiepoint */
 new_point:

/* determine initial tiepoint image location */
 if(nids == 2){
   if(left_samp+grid > ns-(nsw2/2)){
     left_samp=(nsw2/2)+1;
     left_line += grid;
     if(left_line > nl-(nlw2/2)) goto end_points;}
   else{
     left_samp += grid;
   }
   line1=left_line;
   samp1=left_samp;
   line2=line1;
   samp2=samp1;
 }
 else{
   if(inpt_count == 32){
     inpt_count=0;
     input_line += 1;
     if(input_line > nl4) goto end_points;
     status=zvread(unit4,inbuf,"LINE",input_line,0);
   }
   if(extend == 1){ /* true */
     line1=inbuf[inpt_count][2];
     samp1=inbuf[inpt_count][3];
     line2=line1;
     samp2=samp1;
     inpt_count += 1;
     if((input_line == nl4) & (line1 == 0.0) & (samp1 == 0.0)) goto end_points;
   }
   else{
     line1=inbuf[inpt_count][0];
     samp1=inbuf[inpt_count][1];
     line2=inbuf[inpt_count][2];
     samp2=inbuf[inpt_count][3];
     inpt_count += 1;
     if((input_line == nl4) & (line1 == 0.0) & (samp1 == 0.0) &
        (line2 == 0.0) & (samp2 == 0.0)) goto end_points;
   }
 }

/* locate template in search area, return offset */
/* set initial coefficient values */
 line_coef[0]=0.;
 line_coef[1]=1.;
 line_coef[2]=0.;
 samp_coef[0]=1.;
 samp_coef[1]=0.;
 samp_coef[2]=0.;
 /* Choose between Gruen correlation and KQKKOR correlation */
 /* ( Ackermann, 1984)                                      */
 /*                                                         */

 if ( mode != 5 ) {
   /* read left image template */
   read_left_area(left,correl,line1,samp1,unit1,nl,ns,nlw,nsw,zero,&ind);
   if(ind == 1) goto new_point;

   /* read right search area */
   read_right_area(right,correl,line2,samp2,unit2,nl,ns,nlw2,nsw2,zero,&ind);
   if(ind == 1) goto new_point;


   gruen(left,nlw,nsw,right,nlw2,nsw2,correl,&line_offset,&samp_offset,
	 line_coef,samp_coef,line_coef_limits,samp_coef_limits,
	 line_temp,samp_temp,percent,limits,&quality,mode,&ind); 
 } else { 
   /* call_kqkkor routine maps inputs to the kqkkor program */
   /* forms inputs, makes nessesary calls and forms output  */
   /* for tracker3                                          */
   /* Added by Anton B. Ivanov (abi@mipl7), Apr 17 2000     */

 /* If we have found a correlation before, determine next best guess for the
    affine transformation paramters, based on currently obtained
    points. Note that feature tracking, these parameters will vary
    widely across the image.  */


   /*      fprintf( stderr, "%d %f %f\n", ptcount, line2, samp2);        */
   made_good_guess = 0.0; 
   quality = 0.0; 

   /* Make a guess on where the tiepoint should be */
   status = call_affinpar( line1, samp1, obuf, ptcount, &line2, &samp2, line_coef, samp_coef ); 
   /* status = call_affinpar( 0, 0,  obuf, ptcount, &line2, &samp2, line_coef, samp_coef ); */
   /* status = 0; */
   /* If an affine transform is not found, reset to standard best guess */
   if ( status < 1 ) {
     /* Reset line_coef and samp_coef to original values.*/
     line_coef[0]=0.;
     line_coef[1]=1.;
     line_coef[2]=0.;
     samp_coef[0]=1.;
     samp_coef[1]=0.;
     samp_coef[2]=0.;
     made_good_guess = 1.0; 
   }
   
   /* Iterate, until a good guess is obtained           */
   /* By default the guess made by affpar is not good.  */
   /* It is good only if the resulting quality          */
   /* will be higher than 0.0                           */

   do { 

     /* If quality returned by the previous iteration was not good   */
     /* return to the initial guess, and restore some default values */
     if ( quality == -1.0 ) {
        line_coef[0]=0.; line_coef[1]=1.; line_coef[2]=0.;
	samp_coef[0]=1.; samp_coef[1]=0.; samp_coef[2]=0.;
	line2 = line1; samp2 = samp1; 
	made_good_guess = 1; 
     }
     
     /* First find optimal dimensions for KQKKOR operation    */
     kqkkor_dimensions( nlw, nlw2, line_coef, samp_coef, quality_limit, 
			&kq_nlw, &kq_nsw, &kq_nlw2, &kq_nsw2, 
			&info);
 /*     fprintf( stderr, "%d %f %f %f %f %f %f %d %d \n", status, line_coef[0], line_coef[1], line_coef[2],    */
 /* 	      samp_coef[0], samp_coef[1], samp_coef[2], kq_nlw, kq_nlw2);   */

     /* Check kq_nlw for sanity, because sometimes the initial guess is pretty bad */
     if ( kq_nlw != kq_nsw || kq_nlw2 != kq_nsw2 || kq_nlw > nl || 
	  kq_nlw >= MAX_LEFT_AREA || kq_nlw2 >= MAX_RIGHT_AREA) {
        quality = -1.0;
     } else {
       /* Read the data in */
       /* read left image template */
       read_left_area(left,correl,line1,samp1,unit1,nl,ns,kq_nlw,kq_nsw,zero,&ind);
       if(ind == 1 && made_good_guess) goto new_point;
       if ( ind == 1 && !made_good_guess) {
	 quality = -1.0; 
       } else {
	 /* read right search area */
	 read_right_area(right,correl,line2,samp2,unit2,nl,ns,kq_nlw2,kq_nsw2,zero,&ind);
	 if(ind == 1 && made_good_guess ) goto new_point;
	 if ( ind == 1 && !made_good_guess ) {
	   quality = -1.0;
	 } else {
	   /* Do the correlation */
	   /*  fprintf( stderr, "\t\t \t \tActually used : %d %f %f %f %f %f %f %d \n", status, line_coef[0], line_coef[1], line_coef[2], samp_coef[0], samp_coef[1], samp_coef[2], ptcount);  */
	   call_kqkkor( &info, left, kq_nlw, kq_nsw, right, kq_nlw2, kq_nsw2, 
			line_coef, samp_coef, 
			&line_offset, &samp_offset, &quality, &ind); 
	 }
       }
/*         fprintf( stderr, "Tried " );   */
     }
/*      fprintf( stderr, "%d\t%d", kq_nlw, kq_nlw2); fprintf( stderr, "\t%f\n", quality );   */

   } while ( quality < 0.0 && made_good_guess != 1); 
 }

 if(ind == 1) goto new_point;
 if(ind == 2) goto new_point;
 if(quality <= quality_limit) goto new_point;
 
/*  if ( made_good_guess == 0) fprintf( stderr, "Made good guess %f %f\n", line2 - line1, samp2 - samp1); */

/* Compute right image coordinates from offsets */
 line2=line2 + line_offset;
 samp2=samp2 + samp_offset;

/* add new point to output record */
 ptcount += 1;
 obuf[ptcount-1][0]=line1;
 obuf[ptcount-1][1]=samp1;
 obuf[ptcount-1][2]=line2;
 obuf[ptcount-1][3]=samp2;

/* write record to ascii file if desired */
 if(ascii == 1){
   left_area_dn(left,nlw,nsw,&dn_left);
   right_area_dn(right,nlw2,nsw2,nlw,nsw,line_offset,samp_offset,&dn_right);
   fprintf(fd,"%.2f %.2f %.2f %.2f %.2f %.2f\n",line1,samp1,line2,samp2,
         dn_left,dn_right);
 }

/* if(mode != 0){
  printf("LineCoef=%f %f %f\n",line_coef[0],line_coef[1],line_coef[2]);
  printf("SampCoef=%f %f %f\n",samp_coef[0],samp_coef[1],samp_coef[2]);
 } */

 if(pmode == 1){
   sprintf(msg,"Line=%f Samp=%f Line=%f Samp=%f Qual=%f",
       line1,samp1,line2,samp2,quality);
   zvmessage(msg," ");
 }

/* if output record full, write it out */
 if(ptcount == 32){
    ptcount=0;
    output_line += 1;
    status=zvwrit(unit3,obuf,"LINE",output_line,0);
 }

 goto new_point;
 end_points: /* end of tiepoints acquisition cycle */

/* write out partial record of tiepoints */
 if(ptcount > 0){
   for(i=ptcount; i < 32; i++){
     obuf[i][0]=0.0;
     obuf[i][1]=0.0;
     obuf[i][2]=0.0;
     obuf[i][3]=0.0;
   }
     output_line += 1;
     status=zvwrit(unit3,obuf,"LINE",output_line,0);
 }

/* update output file dimensions */
 status=zldel(unit3,"SYSTEM","NL",0);
 zvsignal(unit3,status,1);
 status=zladd(unit3,"SYSTEM","NL",&output_line,"FORMAT","INT",0);
 zvsignal(unit3,status,1);

/* close ascii file if present */
 if(ascii == 1) fclose(fd);
}

/********************************************************************/
/* return mean dn of left area */

left_area_dn(left,nlw,nsw,dn)

float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
int nlw,nsw;
float *dn;
{
 int j,i;
 *dn=0.0;
 for(j=0; j < nlw; j++){
   for(i=0; i < nsw; i++){
     *dn += left[j][i];
   }
 }
 *dn = *dn/(nlw*nsw);
}

/********************************************************************/
/* return mean dn of right area */

right_area_dn(right,nlw2,nsw2,nlw,nsw,line_offset,samp_offset,dn)

float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float line_offset,samp_offset,*dn;
int nlw2,nsw2,nlw,nsw;
{
 int j,i,n,m;
 *dn=0.0;

 n=nlw2/2.0+line_offset-nlw/2.0+0.5;
 m=nsw2/2.0+samp_offset-nsw/2.0+0.5;
 if(n < 0) n=0;
 if(m < 0) m=0;
 if(n+nlw-1 > nlw2) n=nlw2-nlw+1;
 if(m+nsw-1 > nsw2) m=nsw2-nsw+1;

 for(j=n; j < n+nlw; j++){
   for(i=m; i < m+nsw; i++){
     *dn += right[j][i];
   }
 }
 *dn = *dn/(nlw*nsw);
}

/*************************************************************************
*/
amoeba(P,Y,ITER,nlw,nsw,nlw2,nsw2,left,right,
       coef_limits,sumx,left_sum,percent)

float P[6][7],Y[7];
int *ITER;

/* cost function arguments */
int nlw,nsw,nlw2,nsw2;
float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float coef_limits[6][2],percent;
double sumx[10],left_sum[10];
{
 int I,J,MP,NP,NDIM,ITMAX,MPTS,ILO,IHI,INHI,ind;
 float quality;
 float FTOL,RTOL,ALPHA,BETA,GAMMA,YPR,YPRR;
 float PR[7],PRR[7],PBAR[7];

 NP=6;
 MP=7;
 NDIM=6;
 FTOL=.00001;
 ITMAX=5000;
 ALPHA=1.0;
 BETA=0.5;
 GAMMA=2.0;
 MPTS=NDIM+1;
 *ITER=0;

 LOOP:

 ILO=0;
 if(Y[0] > Y[1]){
   IHI=0;
   INHI=1;
 }
 else{
   IHI=1;
   INHI=0;
 }

 for(I=0; I < MPTS; I++){
   if(Y[I] < Y[ILO]) ILO=I;
   if(Y[I] > Y[IHI]){
     INHI=IHI;
     IHI=I;
   }
   else if(Y[I] > Y[INHI]){
       if(I != IHI) INHI=I;
   }
 }

 RTOL=2.*fabs(Y[IHI]-Y[ILO])/(fabs(Y[IHI])+fabs(Y[ILO]));
 if(RTOL < FTOL){
   /*printf("Iterations=%d\n",*ITER);*/
   return;
 }
 if(*ITER == ITMAX){
   printf("Amoeba exceeding maximum iterations\n");
   return; 
 }
 *ITER= *ITER+1;
 for(J=0; J < NDIM; J++){
   PBAR[J]=0.;
 }
 for(I=0; I < MPTS; I++){
   if(I != IHI){
     for(J=0; J < NDIM; J++){
       PBAR[J]=PBAR[J]+P[J][I];
     }
   }
 }
 for(J=0; J < NDIM; J++){
   PBAR[J]=PBAR[J]/NDIM;
   PR[J]=(1.+ALPHA)*PBAR[J]-ALPHA*P[J][IHI];
 }
   cost(PR,coef_limits,left,nlw,nsw,right,nlw2,nsw2,sumx,left_sum,
     percent,&quality,&ind);
   YPR= quality+1.;
   if(YPR <= Y[ILO]){
        for(J=0; J < NDIM; J++){
          PRR[J]=GAMMA*PR[J]+(1.-GAMMA)*PBAR[J];
        }
        cost(PRR,coef_limits,left,nlw,nsw,right,nlw2,nsw2,sumx,left_sum,
             percent,&quality,&ind);
        YPRR= quality+1.;
        if(YPRR < Y[ILO]){
          for(J=0; J < NDIM; J++){
            P[J][IHI]=PRR[J];
          }
          Y[IHI]=YPRR;
        }
        else{
          for(J=0; J < NDIM; J++){
            P[J][IHI]=PR[J];
          }
          Y[IHI]=YPR;
        }
   }
   else if(YPR >= Y[INHI]){
        if(YPR < Y[IHI]){
          for(J=0; J < NDIM; J++){
            P[J][IHI]=PR[J];
          }
          Y[IHI]=YPR;
        }
        for(J=0; J < NDIM; J++){
          PRR[J]=BETA*P[J][IHI]+(1.-BETA)*PBAR[J];
        }
        cost(PRR,coef_limits,left,nlw,nsw,right,nlw2,nsw2,sumx,left_sum,
              percent,&quality,&ind);
        YPRR= quality+1.;
        if(YPRR < Y[IHI]){
          for(J=0; J < NDIM; J++){
            P[J][IHI]=PRR[J];
          }
          Y[IHI]=YPRR;
        }
        else{
          for(I=0; I < MPTS; I++){
            if(I != ILO){
              for(J=0; J < NDIM; J++){
                PR[J]=0.5*(P[J][I]+P[J][ILO]);
                P[J][I]=PR[J];
              }
              cost(PR,coef_limits,left,nlw,nsw,right,nlw2,
                   nsw2,sumx,left_sum,percent,&quality,&ind);
              Y[I]= quality+1.;
            }
          }
        }
   }
   else{
        for(J=0; J < NDIM; J++){
          P[J][IHI]=PR[J];
        }
        Y[IHI]=YPR;
   }
 goto LOOP;
}


/********************************************************************
Objective function to be minimized                                  
Polynomial is of the form:
rightline=answer[0]*leftsamp+answer[1]*leftline+answer[2]
rightsamp=answer[3]*leftsamp+answer[4]*leftline+answer[5]
*/
cost(answer,coef_limits,left,nlw,nsw,right,nlw2,nsw2,sumx,left_sum,
     percent,quality,ind)

int nlw,nsw,nlw2,nsw2,*ind;
float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float *quality,answer[6],coef_limits[6][2],percent;
double sumx[10],left_sum[10];
{

 int j,i,m,n,kl,ks,num_areas,k,kll,kss;
 float left_center_line,left_center_samp,right_center_line,right_center_samp;
 float r1,r2,x,y,right_line,right_samp,wl,wr,wt;
 float top,bot,rnlw2,rnsw2,best_r2;
 double sumy,sumy2,sumxy,rn,right_dn;

 *quality=1.0; /* worst return value possible */

/* check if solution limits are violated */
 for(i=0; i < 6; i++){
   if((answer[i] < coef_limits[i][0]) || 
      (answer[i] > coef_limits[i][1])){
      *ind=1;
      return;
   }
 }

/* constants */
 left_center_line=(nlw-1)/2.0;
 left_center_samp=(nsw-1)/2.0;
 right_center_line=(nlw2-1)/2.0;
 right_center_samp=(nsw2-1)/2.0;

/* check if corners of left area fall within right area */
 rnlw2=nlw2-1;
 rnsw2=nsw2-1;
 y= -left_center_line;
 x= -left_center_samp;
 right_line=x*answer[0]+y*answer[1]+answer[2]+right_center_line;
 right_samp=x*answer[3]+y*answer[4]+answer[5]+right_center_samp;
 if((right_line < 0.0) || (right_line > rnlw2) ||
    (right_samp < 0.0) || (right_samp > rnsw2)){
    *ind=1;
    return;
 }
 y= -left_center_line;
 x= (nsw-1)-left_center_samp;
 right_line=x*answer[0]+y*answer[1]+answer[2]+right_center_line;
 right_samp=x*answer[3]+y*answer[4]+answer[5]+right_center_samp;
 if((right_line < 0.0) || (right_line > rnlw2) ||
    (right_samp < 0.0) || (right_samp > rnsw2)){
    *ind=1;
    return;
 }
 y= (nlw-1)-left_center_line;
 x= -left_center_samp;
 right_line=x*answer[0]+y*answer[1]+answer[2]+right_center_line;
 right_samp=x*answer[3]+y*answer[4]+answer[5]+right_center_samp;
 if((right_line < 0.0) || (right_line > rnlw2) ||
    (right_samp < 0.0) || (right_samp > rnsw2)){
    *ind=1;
    return;
 }
 y=(nlw-1)-left_center_line;
 x=(nsw-1)-left_center_samp;
 right_line=x*answer[0]+y*answer[1]+answer[2]+right_center_line;
 right_samp=x*answer[3]+y*answer[4]+answer[5]+right_center_samp;
 if((right_line < 0.0) || (right_line > rnlw2) ||
    (right_samp < 0.0) || (right_samp > rnsw2)){
    *ind=1;
    return;
 }

/* compute sub area limits if percent < 100 */
 num_areas=1;
 if(percent < 99.99){
   num_areas=5;
   kl=(nlw*(percent/100.)) + .5;
   if(kl > nlw) kl=nlw;
   if(kl < 1) kl=1;
   ks=(nsw*(percent/100.)) + .5;
   if(ks > nlw) ks=nlw;
   if(ks < 1) ks=1;
   kll=nlw-kl-1;
   kss=nsw-ks-1;
 }

/* loop on all sub areas */
 best_r2=0.;
 for(k=0; k < num_areas; k++){

/* compute area of sub area */
   if(k == 0) rn=nlw*nsw;
   if(k == 1) rn=kl*nsw;
   if(k == 2) rn=kl*nsw;
   if(k == 3) rn=ks*nlw;
   if(k == 4) rn=ks*nlw;

/* compute coefficient of determination r2 */
   sumy=0.0;
   sumy2=0.0;
   sumxy=0.0;
   for(j=0; j < nlw; j++){
     y=j-left_center_line;
     /* store redundant terms */
     r1=y*answer[1]+answer[2]+right_center_line;
     r2=y*answer[4]+answer[5]+right_center_samp;
     for(i=0; i < nsw; i++){
       if(k == 0) goto compute;
       if(k == 1){
         if(j > kll) goto compute;
         continue;
       }
       if(k == 2){
         if(j < kl) goto compute;
         continue;
       }
       if(k == 3){
         if(i > kss) goto compute;
         continue;
       }
       if(k == 4){
         if(i < ks) goto compute;
         continue;
       }
       compute:
       x=i-left_center_samp;
       right_line=x*answer[0]+r1;
       right_samp=x*answer[3]+r2;
       m=right_samp;
       n=right_line;
       wl=right_samp-m;
       wr=1.0-wl;
       top=wl*right[n][m+1]+wr*right[n][m]; /* bilinear interpolation */
       bot=wl*right[n+1][m+1]+wr*right[n+1][m];
       wt=right_line-n;
       right_dn=bot*wt+top*(1.0-wt); /* got right DN value by interpolation */
       sumy += right_dn; /* compute sums for least squares fit */
       sumy2 += right_dn*right_dn;
       sumxy += right_dn*left[j][i];
     }
   }
   r2=pow((sumxy-sumx[k]*sumy/rn),2.0)/(left_sum[k]*(sumy2-sumy*sumy/rn));
   if(r2 > best_r2) best_r2=r2;
 }/* end of areas loop */

 *quality=1.0-best_r2;
 *ind=0;

}

/********************************************************************
Routine to perform conventional correlation only.
Correl is filled with the matrix of coefficient of determination values.
Then the peak value is interpolated to sub pixel.
Answer is returned with the equivalent polynomial mapping from
left to right areas centered on the central pixel.
*/

search_area(left,nlw,nsw,right,nlw2,nsw2,correl,sumx,left_sum,
            percent,answer,quality,ind)

float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float correl[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float *quality,answer[6],percent;
double sumx[10],left_sum[10];
int nlw,nsw,nlw2,nsw2,*ind;

{
 double rn,right_dn,sumy,sumxy,sumy2;
 float r2,a,b,c,line_val[10],samp_val[10],qual[10];
 float denom,sample,line;
 int i,j,m,n,kl,ks,num_areas,k,kll,kss;

/* compute sub area limits if percent < 100 */
 num_areas=1;
 if(percent < 99.99){
   num_areas=5;
   kl=(nlw*(percent/100.)) + .5;
   if(kl > nlw) kl=nlw;
   if(kl < 1) kl=1;
   ks=(nsw*(percent/100.)) + .5;
   if(ks > nlw) ks=nlw;
   if(ks < 1) ks=1;
   kll=nlw-kl-1;
   kss=nsw-ks-1;
 }

/* loop on all sub areas */
 for(k=0; k < num_areas; k++){

/* compute area of sub area */
   if(k == 0) rn=nlw*nsw;
   if(k == 1) rn=kl*nsw;
   if(k == 2) rn=kl*nsw;
   if(k == 3) rn=ks*nlw;
   if(k == 4) rn=ks*nlw;

/* fill correlation matrix */
   for(n=0; n < nlw2-nlw+1; n++){
     for(m=0; m < nsw2-nsw+1; m++){
       sumy=0.0;
       sumy2=0.0;
       sumxy=0.0;
       for(j=0; j < nlw; j++){
         for(i=0; i < nsw; i++){
           if(k == 0) goto compute;
           if(k == 1){
             if(j > kll) goto compute;
             continue;
           }
           if(k == 2){
             if(j < kl) goto compute;
             continue;
           }
           if(k == 3){
             if(i > kss) goto compute;
             continue;
           }
           if(k == 4){
             if(i < ks) goto compute;
             continue;
           }
           compute:
           right_dn=right[j+n][i+m];
           sumy += right_dn;
           sumy2 += right_dn*right_dn;
           sumxy += right_dn*left[j][i];
         }
       }
       correl[n][m]=pow((sumxy-sumx[k]*sumy/rn),2.0)/
            (left_sum[k]*(sumy2-sumy*sumy/rn));
     }
   }
  
/* locate highest value */
   r2= -1.0;
   for(n=0; n < nlw2-nlw+1; n++){
     for(m=0; m < nsw2-nsw+1; m++){
       if(r2 < correl[n][m]){
         r2=correl[n][m];
         j=n;
         i=m;
       }
     }
   }

/* reject point if on border */
   if((j == 0) || (j == nlw2-nlw+1) || (i == 0) || (i == nsw2-nsw+1)){
     if(num_areas == 1){
       *ind=1;
       return;
     }
     else{
       qual[k]=0.;
       goto loopend;
     }
   }

/* compute sub-pixel location of best correlation.
   See Numerical Recipes, eqn: 10.2.1, note b+1/2 should read b-1/2   */
   a=correl[j][i-1];
   b=2.0*correl[j][i];
   c=correl[j][i+1];
   denom=2.0*(b-c-a);
   if(denom != 0.0){
     sample=(c-a)/denom+i+(nsw-1)/2;}
   else{
     sample=i+(nsw-1)/2;
   }
   a=correl[j-1][i];
   c=correl[j+1][i];
   denom=2.0*(b-c-a);
   if(denom != 0.0){
     line=(c-a)/denom+j+(nlw-1)/2;}
   else{
     line=j+(nlw-1)/2;
   }

   line_val[k]=line;
   samp_val[k]=sample;
   qual[k]=r2;
   loopend:;
 } /* end of areas loop */

/* locate best area fit */
 if(num_areas > 1){
   i= -1;
   r2=0.;
   for(k=0; k < num_areas; k++){
     if(qual[k] > r2){
       r2=qual[k];
       i=k;
     }
   }
   if(i == -1){
     *ind=1;
     return;
   }
   line=line_val[i];
   sample=samp_val[i];
   *quality=qual[i];
 }
 else
   *quality=correl[j][i];

/* assemble polynomial terms mapping left to right 
 rightline=answer[0]*leftsamp+answer[1]*leftline+answer[2]
 rightsamp=answer[3]*leftsamp+answer[4]*leftline+answer[5]
*/
 answer[0]=0.0;
 answer[1]=1.0;
 answer[2]=line-(nlw2-1)/2;
 answer[3]=1.0;
 answer[4]=0.0;
 answer[5]=sample-(nsw2-1)/2;

 *ind=0;
}


/********************************************************************
ARGUMENTS:

left=left image area template,[MAX_LEFT_AREA][MAX_LEFT_AREA]     float

nlw= # lines left area. odd number                               int

nsw= # samples left area. odd number                             int

right=right image search area, [MAX_RIGHT_AREA][MAX_RIGHT_AREA]  float

nlw2=# lines right area. odd number                              int

nsw2=# samples right area. odd number                            int

correl=correlation matrix, [MAX_RIGHT_AREA][MAX_RIGHT_AREA]      float
       Returned. Only used in mode=0.

line_offset=line shift to correct initial registration, returned float
            Negative means shift upwards.

samp_offset=sample shift to correct initial registration, returned float
            Negative means shift to the left.

line_coef=line equation coefficients,[3], returned               float
          Mapping polynomial from template to search area.
          rightline=line_coef[0]*leftsamp+line_coef[1]*leftline+line_coef[2]
          On entry contains best estimate of line polynomial solution.
          Try: 0., 1., 0.

samp_coef=sample equation coefficients,[3], returned               float
          Mapping polynomial from template to search area.
          rightsamp=samp_coef[0]*leftsamp+samp_coef[1]*leftline+samp_coef[2]
          On entry contains best estimate of sample polynomial solution.
          Try: 1., 0., 0.

line_coef_limits=initial boundary conditions, [3][2]             float
          Only used in modes 1 and 4.
          [][0] is negative limit
          [][1] is positive limit
          These limit the possible range of mapping polynomial values.

samp_coef_limits=initial boundary conditions, [3][2]             float
          Only used in modes 1 and 4.
          [][0] is negative limit
          [][1] is positive limit
          These limit the possible range of mapping polynomial values.

line_temp=line equation coefficients temperature,[3]              float
          Only used in modes 1 and 4.
          The initial guessing range for solutions of polynomial values.
          Try: (line_coef_limits[][1]-line_coef_limits[][0])/12.

samp_temp=sample equation coefficients temperature,[3]           float
          Only used in modes 1 and 4.
          The initial guessing range for solutions of polynomial values.
          Try: (samp_coef_limits[][1]-samp_coef_limits[][0])/12.

percent=Percentage of the template to use in correlations. If percent=100
        then one correlation is computed with the entire area, otherwise
        five correlations are computed, beginning with the entire area. 
        The other four correlations utilize only:
        "percent" lines at the bottom,
        "percent" lines at the top,
        "percent" columns at the right,
        "percent" columns at the left.                               float
        The purpose of this is to permit the exclusion of border points
        which for some reason are incompatible in intensity with the
        data being correlated. 
        For example if percent=80 then one of the correlation attempts
        will only use nlw*0.80 lines either at the top or bottom of the
        window.

limits=The number of iterations permitted in the annealing scheme.    int
       This should be several thousand at the least.

quality=correlation quality. Zero to one.  1=perfect   returned   float

mode= is the type of correlation desired. An integer from 0 to 4.     int
      (all modes are sub-pixel).
      mode=0 linear least squares fit. 
             Fastest, on integral pixel borders. Translation only.
             Accuracy limited to 1/10 pixel.
      mode=1 annealing fit. Very slow. Able to search entire area.
             Accuracy adjustable but around 1/30 pixel.
             Handles rotation,skew,flip,scale,offset.
      mode=2 amoeba fit. deterministic simplex search method.
             Accuracy as good as it is possible to achieve(1/100 pixel).
             Initial estimate must be within 2 pixels or else the
             resulting tiepoint will be in error.
             Speed intermediate between modes 0 and 1.
             Handles rotation,skew,flip,scale,offset.
      mode=3 linear (mode=0) followed by amoeba (mode=2) fit
             Linear fit locates minimum and amoeba fit refines it.
             Handles rotation,skew,flip,scale,offset.
      mode=4 annealing (mode=1) followed by amoeba (mode=2) fit.
             Annealing locates minimum and amoeba refines it.
             Handles rotation,skew,flip,scale,offset.

ind=return status, 0=OK, 1=unable to obtain a correlation.            int
*/

gruen(left,nlw,nsw,right,nlw2,nsw2,correl,line_offset,samp_offset,
       line_coef,samp_coef,line_coef_limits,samp_coef_limits,
       line_temp,samp_temp,percent,limits,quality,mode,ind)

int nlw,nsw,nlw2,nsw2,*ind,mode,limits;
float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float correl[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float *line_offset,*samp_offset,*quality,percent;
float line_coef[3],samp_coef[3];
float line_coef_limits[3][2],samp_coef_limits[3][2];
float line_temp[3],samp_temp[3];
{

 float range[6],answer[6],coef_limits[6][2];
 float P[6][7],Y[7],scr[6];
 double sumx[10],left_sum[10],sumx2,left_dn,rn;
 int numten,norm,i,j,ITER,kl,ks,num_areas,k,kll,kss;

 *ind=0;

/* compute sub area limits if percent < 100 */
 num_areas=1;
 if(percent < 99.99){
   num_areas=5;
   kl=(nlw*(percent/100.)) + .5;
   if(kl > nlw) kl=nlw;
   if(kl < 1) kl=1;
   ks=(nsw*(percent/100.)) + .5;
   if(ks > nlw) ks=nlw;
   if(ks < 1) ks=1;
   kll=nlw-kl-1;
   kss=nsw-ks-1;
 }

/* loop on all sub areas */
 for(k=0; k < num_areas; k++){

/* compute area of sub area */
   if(k == 0) rn=nlw*nsw;
   if(k == 1) rn=kl*nsw;
   if(k == 2) rn=kl*nsw;
   if(k == 3) rn=ks*nlw;
   if(k == 4) rn=ks*nlw;

/* pre-compute left (template) sums for COST function */
   sumx2=0.0;
   sumx[k]=0.0;
   for(j=0; j < nlw; j++){
     for(i=0; i < nsw; i++){
        if(k == 0) goto compute;
        if(k == 1){
          if(j > kll) goto compute;
          continue;
        }
        if(k == 2){
          if(j < kl) goto compute;
          continue;
        }
        if(k == 3){
          if(i > kss) goto compute;
          continue;
        }
        if(k == 4){
          if(i < ks) goto compute;
          continue;
        }
        compute:
        left_dn=left[j][i];
        sumx[k] += left_dn;
        sumx2 += left_dn*left_dn;
     }
   }
   left_sum[k]=sumx2-sumx[k]*sumx[k]/rn;
 }

 if((mode == 0) || (mode == 3)){

   /* Use conventional linear correlation scheme only */

   search_area(left,nlw,nsw,right,nlw2,nsw2,correl,sumx,left_sum,
               percent,answer,quality,ind);
   if(mode == 3) goto Amoeba;
 }


 if((mode == 1) || (mode == 4)){

   /* use Annealing correlation */

   answer[0]=line_coef[0];
   answer[1]=line_coef[1];
   answer[2]=line_coef[2];
   answer[3]=samp_coef[0];
   answer[4]=samp_coef[1];
   answer[5]=samp_coef[2];
   range[0]=line_temp[0];
   range[1]=line_temp[1];
   range[2]=line_temp[2];
   range[3]=samp_temp[0];
   range[4]=samp_temp[1];
   range[5]=samp_temp[2];
   coef_limits[0][0]=line_coef_limits[0][0];
   coef_limits[0][1]=line_coef_limits[0][1];
   coef_limits[1][0]=line_coef_limits[1][0];
   coef_limits[1][1]=line_coef_limits[1][1];
   coef_limits[2][0]=line_coef_limits[2][0];
   coef_limits[2][1]=line_coef_limits[2][1];
   coef_limits[3][0]=samp_coef_limits[0][0];
   coef_limits[3][1]=samp_coef_limits[0][1];
   coef_limits[4][0]=samp_coef_limits[1][0];
   coef_limits[4][1]=samp_coef_limits[1][1];
   coef_limits[5][0]=samp_coef_limits[2][0];
   coef_limits[5][1]=samp_coef_limits[2][1];

   /* set iteration control */
   numten=limits/2;
   norm=numten/7;

   metropolis(left,nlw,nsw,right,nlw2,nsw2,answer,
        coef_limits,range,numten,limits,norm,sumx,left_sum,
        percent,quality,ind);
   if(mode == 4) goto Amoeba;
 }

 if(mode == 2){

   /* use Amoeba correlation */

   answer[0]=line_coef[0];
   answer[1]=line_coef[1];
   answer[2]=line_coef[2];
   answer[3]=samp_coef[0];
   answer[4]=samp_coef[1];
   answer[5]=samp_coef[2];
   Amoeba:
   coef_limits[0][0]= -1000.;
   coef_limits[0][1]=  1000.;
   coef_limits[1][0]= -1000.;
   coef_limits[1][1]=  1000.;
   coef_limits[2][0]= -1000.;
   coef_limits[2][1]=  1000.;
   coef_limits[3][0]= -1000.;
   coef_limits[3][1]=  1000.;
   coef_limits[4][0]= -1000.;
   coef_limits[4][1]=  1000.;
   coef_limits[5][0]= -1000.;
   coef_limits[5][1]=  1000.;

   /* precompute the matrix P of 7 simplex starting points 
      and array Y of corresponding errors */

   for(j=0; j < 7; j++){
     for(i=0; i < 6; i++){
       P[i][j]=answer[i];
     }
     if(j == 0) P[j][j]=P[j][j]-.05;
     if(j == 1) P[j][j]=P[j][j]-.05;
     if(j == 2) P[j][j]=P[j][j]-0.5;
     if(j == 3) P[j][j]=P[j][j]-.05;
     if(j == 4) P[j][j]=P[j][j]-.05;
     if(j == 5) P[j][j]=P[j][j]-0.5;
     for(i=0; i < 6; i++){
       scr[i]=P[i][j];
     }
     cost(scr,coef_limits,left,nlw,nsw,right,nlw2,nsw2,
           sumx,left_sum,percent,quality,ind);
     Y[j]= *quality+1.;
   }

   /* solve for the minimum using the simplex method */

   amoeba(P,Y,&ITER,nlw,nsw,nlw2,nsw2,left,right,
       coef_limits,sumx,left_sum,percent);

   *quality=1.0-(Y[0]-1.0);
   for(i=0; i < 6; i++){
     answer[i]=P[i][0];
   }
 }

 if((mode < 0) || (mode > 4)){
   *ind=1;
   return;
 }


/* restore & return new polynomial coefficients */
 line_coef[0]=answer[0];
 line_coef[1]=answer[1];
 line_coef[2]=answer[2];
 samp_coef[0]=answer[3];
 samp_coef[1]=answer[4];
 samp_coef[2]=answer[5];

/* return offset from initial estimate, Note other polynomial terms are
   zero because multiply by x=y=0 at template origin */
 *line_offset=line_coef[2];
 *samp_offset=samp_coef[2];
 
}

/**********************************************************************/
zrangen(idum,rand_num)
  long *idum;        /* input seed (1st call only)and  returned integer*/
  float *rand_num;       /* returned random number  0> *rand_num>1 */
{
  static long iy,ir[98];
  static int iff=0;
  long M,IA,IC;
  int j;
  char msg[80];

  M=714025; IA=1366; IC=150889;
  if(*idum<0 || iff==0)
  {
    iff=1;
    if ((*idum=(IC - (*idum)) % M) < 0) *idum= -(*idum);
    for (j=1;j<97;j++)
    {
      *idum=(IA*(*idum)+IC) % M;
      ir[j]=(*idum);
    }
    *idum=(IA*(*idum)+IC) % M;
    iy=(*idum);
  }
  j=1 + 97.0*iy/M;
  if (j>97||j<1) 
  {
     sprintf(msg,"RANGEN: This cannot happen.");
     zvmessage(msg,"");
  }
  iy=ir[j];
  *idum=(IA*(*idum)+IC) % M;
  iy=(*idum);
  *rand_num= (float) iy/M;
  return; 
}

/*********************************************************************/
metropolis(left,nlw,nsw,right,nlw2,nsw2,answer,
        coef_limits,range,numten,limits,norm,sumx,left_sum,
        percent,quality,indic)

      float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
      float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
      float *quality,range[6],answer[6],coef_limits[6][2],percent;
      double sumx[10],left_sum[10];
      int nlw,nsw,nlw2,nsw2,numten,limits,norm,*indic;
{
      int fail1,fail2,success1,success2,limit,numreset,j,k,loop,narg,ind;
      unsigned long iseed;
      float temp[6],x[6],minx[6],mincost,pi,pi2,c1,c2,c3,scale,numtenf;
      float costsum,energy,boltzman,rand_max,ran,prob;

      pi=3.14159;
      pi2=pi/2.0;
      limit=limits;
      iseed=10109854;
      narg=6;
      numtenf=numten;
      fail1=0;
      fail2=0;
      success1=0;
      success2=0;
      numreset=numten/10;
      scale=exp((log(0.1))/numtenf);
      loop=0;
      rand_max=pow(2.0,31.0)-1.0;

/*  Compute the cost at position ANSWER and assign to variable C1. */
      cost(answer,coef_limits,left,nlw,nsw,right,nlw2,nsw2,
           sumx,left_sum,percent,&c1,&ind);
      if(ind != 0){
         *indic=1;
         return;
      }

/*  Save the cost in case the user had good reason to inspect this
    solution position. */
      mincost=c1;
      for(j=0; j < narg; j++){
         minx[j]=answer[j];
      }

/*  Set initial temperatures to the range estimates. */
      for(j=0; j < narg; j++){
         temp[j]=range[j];
      }

/*   MAIN LOOP: loop on number of successful changes in solution space. */
      while(loop < limit){

/*   Compute the delta_cost/temperature ratio for
     normalization of probabilities.
     Note that this is the Boltzmann constant for this 'system'.*/

        k=loop/norm;
        if(loop-k*norm == 0){
           costsum=0.0;
           k=0;
           for(j=0; j < narg; j++){
              x[j]=answer[j];
           }
           for(j=0; j < narg; j++){
              x[j]=answer[j]-temp[j];
              cost(x,coef_limits,left,nlw,nsw,right,nlw2,nsw2,
                   sumx,left_sum,percent,&c2,&ind);
              if(ind == 0){
                k=k+1;
                costsum=costsum+fabs(c1-c2)/temp[j];
              }
              x[j]=answer[j]+temp[j];
              cost(x,coef_limits,left,nlw,nsw,right,nlw2,nsw2,
                   sumx,left_sum,percent,&c2,&ind);
              if(ind == 0){
                k=k+1;
                costsum=costsum+fabs(c1-c2)/temp[j];
              }
              x[j]=answer[j];
           }
           if(k == 0){
              *indic=2;
              return;
           }         
           boltzman=5.0*(costsum/k);
        }
                     
/*      Decrement the temperature according to the multiplicative
        cooling schedule.  */
        for(j=0; j < narg; j++){
           temp[j]=temp[j]*scale;
        }
        energy=boltzman*(temp[0]+temp[1]+temp[2]+temp[3]+temp[4]
                        +temp[5])/6.0;

/*      Compute a solution space guess using a Cauchy-Lorentzian
        random probability distribution function. */
A91:
        for(j=0; j < narg; j++){
           zrangen(&iseed,&ran);
           x[j]=temp[j]*tan(pi*ran+pi2)+answer[j];
        }
        cost(x,coef_limits,left,nlw,nsw,right,nlw2,nsw2,
             sumx,left_sum,percent,&c2,&ind);
        if(ind != 0){
           fail1 += 1;
           goto A91;
        }

        if(c2 < c1){

/*          Accept lower cost position.
            We always accept a downhill cost route if offered.*/

            success1 += 1;
            c1=c2;
            for(j=0; j < narg; j++){
               answer[j]=x[j];
            }
        }
        else{
/*          Compute probability of accepting higher cost position.
            This comes from the Boltzmann probability of our system 
            transitioning from energy state c1 to energy state c2.*/

            c3=(c2-c1)/energy;
            if(c3 > 50.){
               goto A92;
            }
            prob=1.0/exp((double)c3);

/*          Evaluate the probability by comparing it against chance.*/

            zrangen(&iseed,&ran);
            if(prob > ran){
/*              Accept higher cost position.*/
                success2 += 1;
                c1=c2;
                for(j=0; j < narg; j++){
                   answer[j]=x[j];
                }
            }
            else{
/*              Reject higher cost position.*/
                fail2 += 1;
                goto A92;
            }
        }

/*       Save the minimum cost and associated solution as we go.*/

        if(c1 < mincost){
            mincost=c1;
            for(j=0; j < narg; j++){
               minx[j]=answer[j];
            }
        }
A92:
        loop=loop+1;

/*       Reset the solution pointer to the minimum cost
         location every numreset successful iterations. */

        k=loop/numreset;
        if(loop-k*numreset == 0){
            c1=mincost;
            for(j=0; j < narg; j++){
               answer[j]=minx[j];
            }
        }

      }  /*   END of MAIN WHILE LOOP  */

/*     Put minimum solution into ANSWER & it's cost into quality  */

      for(j=0; j < narg; j++){
         answer[j]=minx[j];
      }
      *quality=1.0-mincost;

 *indic=0;  
 /*printf("Initial ok=%d bad=%d, Probability ok=%d bad=%d\n",success1,
         fail1,success2,fail2);*/
}


/********************************************************************/
read_left_area(left,correl,left_line,left_samp,unit1,nl,ns,nlw,nsw,zero,ind)

float left[MAX_LEFT_AREA][MAX_LEFT_AREA];
float correl[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float left_line,left_samp;
int unit1,nl,ns,nlw,nsw,zero,*ind;
{
 int status,start_samp,start_line,j,i,m,n;
 float wt,wb,wl,wr,top,bot,dn;

 *ind=0;

 n=left_line;
 m=left_samp;
 if(left_line-n + left_samp-m < .0001){     /* on pixel boundary */
   if(left_line-n > 0.5) n=n+1;
   if(left_samp-m > 0.5) m=m+1;
   start_samp=m - nsw/2;
   start_line=n - nlw/2;
   if(start_samp < 1) *ind=1;
   if(start_line < 1) *ind=1;
   if(start_samp+nsw-1 > ns) *ind=1;
   if(start_line+nlw-1 > nl) *ind=1;
   if(*ind == 1) return;
   for(j=0; j<nlw; j++){
     status=zvread(unit1,&left[j][0],"LINE",j+start_line,"SAMP",start_samp,
                   "NSAMPS",nsw,0);
   }
 }
 else{                             /* interpolate grid to center on a pixel */
   start_samp=m - nsw/2;
   start_line=n - nlw/2;
   if(start_samp < 1) *ind=1;
   if(start_line < 1) *ind=1;
   if(start_samp+nsw > ns) *ind=1;
   if(start_line+nlw > nl) *ind=1;
   if(*ind == 1) return;
   for(j=0; j< nlw+1; j++){
     status=zvread(unit1,&correl[j][0],"LINE",j+start_line,"SAMP",start_samp,
                   "NSAMPS",nsw+1,0);
   }
   wr=left_samp-m;
   wl=1.0-wr;
   wb=left_line-n;
   wt=1.0-wb;
   for(j=0; j < nlw; j++){
     for(i=0; i < nsw; i++){
       top=wl*correl[j][i]+wr*correl[j][i+1];
       bot=wl*correl[j+1][i]+wr*correl[j+1][i+1];
       left[j][i]=top*wt+bot*wb;
     }
   }
 }
 
 if(zero == 1){
   if(left[0][0] == 0.0) *ind=1;
   if(left[nlw-1][0] == 0.0) *ind=1;
   if(left[0][nsw-1] == 0.0) *ind=1;
   if(left[nlw-1][nsw-1] == 0.0) *ind=1;
 }
 if(*ind == 1) return;

 dn=left[0][0];
 for(j=0; j < nlw; j++){
   for(i=0; i < nsw; i++){
     if(left[j][i] != dn) return;
   }
 }
 *ind=1; /* bad */

}

/********************************************************************/
read_right_area(right,correl,left_line,left_samp,unit2,nl,ns,nlw2,nsw2,zero,ind)

float right[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float correl[MAX_RIGHT_AREA][MAX_RIGHT_AREA];
float left_line,left_samp;
int unit2,nl,ns,nlw2,nsw2,zero,*ind;
{
 int status,start_samp,start_line,j,i,m,n;
 float wt,wb,wl,wr,top,bot,dn;

 *ind=0;

 n=left_line;
 m=left_samp;
 if(left_line-n + left_samp-m < .0001){     /* on pixel boundary */
   if(left_line-n > 0.5) n=n+1;
   if(left_samp-m > 0.5) m=m+1;
   start_samp=m - nsw2/2;
   start_line=n - nlw2/2;
   if(start_samp < 1) *ind=1;
   if(start_line < 1) *ind=1;
   if(start_samp+nsw2-1 > ns) *ind=1;
   if(start_line+nlw2-1 > nl) *ind=1;
   if(*ind == 1) return;

   for(j=0; j<nlw2; j++){
     status=zvread(unit2,&right[j][0],"LINE",j+start_line,"SAMP",start_samp,
                   "NSAMPS",nsw2,0);
   }
 }
 else{                             /* interpolate grid to center on a pixel */
   start_samp=m - nsw2/2;
   start_line=n - nlw2/2;
   if(start_samp < 1) *ind=1;
   if(start_line < 1) *ind=1;
   if(start_samp+nsw2 > ns) *ind=1;
   if(start_line+nlw2 > nl) *ind=1;
   if(*ind == 1) return;
   for(j=0; j< nlw2+1; j++){
     status=zvread(unit2,&correl[j][0],"LINE",j+start_line,"SAMP",start_samp,
                   "NSAMPS",nsw2+1,0);
   }
   wr=left_samp-m;
   wl=1.0-wr;
   wb=left_line-n;
   wt=1.0-wb;
   for(j=0; j < nlw2; j++){
     for(i=0; i < nsw2; i++){
       top=wl*correl[j][i]+wr*correl[j][i+1];
       bot=wl*correl[j+1][i]+wr*correl[j+1][i+1];
       right[j][i]=top*wt+bot*wb;
     }
   }
 }

 if(zero == 1){
   if(right[0][0] == 0.0) *ind=1;
   if(right[nlw2-1][0] == 0.0) *ind=1;
   if(right[0][nsw2-1] == 0.0) *ind=1;
   if(right[nlw2-1][nsw2-1] == 0.0) *ind=1;
 }
 if(*ind == 1) return;

 dn=right[0][0];
 for(j=0; j < nlw2; j++){
   for(i=0; i < nsw2; i++){
     if(right[j][i] != dn) return;
   }
 }
 *ind=1; /* bad */

}


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create call_kqkkor.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Subroutine to call kqkkor routine from the tracker3 program */
/* By Anton B. Ivanov (abi@mipl7.jpl.nasa.gov)                 */
/* April 17 2000 (the tax day!)                                */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "kqkkor.h"

#define MAX_LEFT_AREA 50 
#define MAX_RIGHT_AREA 150 

/* Utility patch for conversion of float matrices to int)       */
void flt2int( int* , int , int, float* , int , int);
void print_matrix( int* , int , int );
/* Testing string for KQKKOR

./tracker3 -print "inp=(left.img,right.img) out=pts.img grid=30 nlw=19 nsw=19 nlarea=33 nsarea=33 correl=KQKKOR"

*/

/* This subroutine calls dim_muster_such and finds optimal dimensions for  */
/* the data required by the KQKKOR routine. Returns kq_nlw,kq_nsw and      */
/* kq_nlw2,kq_nsw2 which are reference patch and search window sizes. The  */
/* results are used in read_left_area and read_right_area routines of      */
/* TRACKER3. Note that this program accepts only square dimensions for     */
/* the template and search windows. The sub also returns correlation info  */
/* structure to be used in call_kqkor                                      */

void kqkkor_dimensions( nlw, nlw2, line_coef, samp_coef, quality_limit,
			kq_nlw, kq_nsw, kq_nlw2, kq_nsw2, info)
     int nlw, nlw2;
     int *kq_nlw, *kq_nsw, *kq_nlw2, *kq_nsw2;
     float line_coef[3], samp_coef[3];
     float quality_limit;
     KORPAR *info;     /* parameters for the correlation */
{
  /* Declare KQKKOR inputs */
  INT_MATRIX        musterma; /* pattern/reference matrix       */
  INT_MATRIX        suchma;   /* search matrix                  */
  float deg;
  /* Map the TRACKER3 inputs to KQKOR inputs */

  /* Set values for determination of KQKKOR parameters */

   info -> suchfns = nlw;     /* DEF_SUCHB;   Search window = 10       */ 

   info -> pmkdim  = nlw - 1; /* DEF_PMKDIM;  Patch size for PMK = 9   */ 
   info -> kqkdim  = nlw;    /* Patch size for LSM = 17 */ 
   info -> lsmacu  = 5.0   ; /* Accuracy of the least-squares fit = 0.08  */ 

   /* Calculate search window for KQKKOR, from the nlw and nlw2 of tracker3 */
   /* Search window and search size are different in KQKKOR                 */
   info -> suchfns = nlw2 - 1 - 2 * ( KQK_RAND + info -> kqkdim / 2.0 + 1);
   
   /* info -> suchfns = DEF_SUCHB;    /* Search window = 10      */
   /* info -> pmkdim  = DEF_PMKDIM;   /* Patch size for PMK = 9  */
   /* info -> kqkdim  = 17;           /* Patch size for LSM = 17 */ 
   /* info -> lsmacu  = 0.08;         /* Accuracy of the least-squares fit = 0.08  */

  /* Quality of the correlation input from the tracker3          */
   info -> minpmk  = quality_limit;

  /* Initial coefficients from tracker3                          */
  /* Check if the order is correct                               */
     info -> affin[0] = (double)  line_coef[1];
     info -> affin[1] = (double)  line_coef[0];
     info -> affin[2] = (double)  line_coef[2];
     info -> affin[3] = (double)  samp_coef[1];
     info -> affin[4] = (double)  samp_coef[0];
     info -> affin[5] = (double)  samp_coef[2];
 
/*   info -> affin[0] =   0.903521; */
/*   info -> affin[1] =   0.098851;  */
/*   info -> affin[2] =  -0.127089;  */
/*   info -> affin[3] =  -0.103596; */
/*   info -> affin[4] =   0.904245;  */
/*   info -> affin[5] =   8.392435; */


/*   fprintf( stderr, "DIM : %f %f %f\n", info -> affin[0],  info -> affin[1],info -> affin[2]); */
  
  /* Initialize matrices with minimum dimensions */
  dim_muster_such( info, &musterma, &suchma );
  
/*   printf ("\n minum dimensioning of the reference patch:  (nlw)  dimz:%6d dims:%6d\n",  */
/*   	  musterma.dimz, musterma.dims); */

/*   printf (" minum dimensioning of the search image patch: (nlw2) dimz:%6d dims:%6d\n\n",  */
/*    	  suchma.dimz, suchma.dims); */

  /* Get the minimum required dimensions from the matrices */
  *kq_nlw = musterma.dimz;
  *kq_nsw = musterma.dims;
  *kq_nlw2 = suchma.dimz;
  *kq_nsw2 = suchma.dims;

}

/* ARGUMENTS for call_kqkkor(partly copied from TRACKER3
left=left image area template                                    float*

nlw= # lines left area. odd number                               int

nsw= # samples left area. odd number                             int

right=right image search area                                    float*

nlw2=# lines right area. odd number                              int

nsw2=# samples right area. odd number                            int

line_coef=line equation coefficients,[3], returned               float
          Mapping polynomial from template to search area.
          rightline=line_coef[0]*leftsamp+line_coef[1]*leftline+line_coef[2]
          On entry contains best estimate of line polynomial solution.
          Try: 0., 1., 0.

samp_coef=sample equation coefficients,[3], returned               float
          Mapping polynomial from template to search area.
          rightsamp=samp_coef[0]*leftsamp+samp_coef[1]*leftline+samp_coef[2]
          On entry contains best estimate of sample polynomial solution.
          Try: 1., 0., 0.

line_offset=line shift to correct initial registration, returned   float
            Negative means shift upwards.

samp_offset=sample shift to correct initial registration, returned float
            Negative means shift to the left.

quality = correlation quality. Zero to one.  1=perfect   returned   float

ind = return status, 0=OK, 1=unable to obtain a correlation.            int

*****************************************************************************/
void call_kqkkor( info,
		  left, nlw, nsw, right, nlw2, nsw2,
		  line_coef, samp_coef,
		  line_offset, samp_offset, quality, ind)

KORPAR *info;
float *left;
int nlw, nsw;
float *right;
int nlw2, nsw2;
float line_coef[3],samp_coef[3];
float *line_offset,*samp_offset,*quality;
int *ind;
{
  /* Declare KQKKOR inputs */
  INT_MATRIX        musterma; /* pattern/reference matrix       */
  INT_MATRIX        suchma;   /* search matrix                  */
  ERG               erg;      /* results                        */

  char msg[100];
  /* Map the TRACKER3 inputs to KQKOR inputs */
  /* We have already prepared the info structure in the kqkkor_dimensions routine */

  /* Initialize data in the matrices             */

  /* KQKKOR only accepts integer values for inputs. Patch this problem for testing */
  /* Float matrix must be implemented in KQKKOR for full compatibility with TRACKER3 */

  musterma.dimz   = nlw;
  musterma.dims   = nsw;
  musterma.ptr_m = (int *) malloc( sizeof( int ) * nlw * nsw);
  if ( nlw > MAX_LEFT_AREA) fprintf( stderr, "LSKGJSLKJLSKDJFLF\n"); 
  flt2int( musterma.ptr_m, nlw, nsw, left, MAX_LEFT_AREA, MAX_LEFT_AREA);
  /*print_matrix( musterma.ptr_m, nlw, nsw );*/
  suchma.dimz     = nlw2;
  suchma.dims     = nsw2;

  suchma.ptr_m =  (int *) malloc( sizeof( int ) * nlw2 * nsw2);
  flt2int( suchma.ptr_m,  nlw2, nsw2, right, MAX_RIGHT_AREA , MAX_RIGHT_AREA);

/*   fprintf (stderr, "\n actual dimension of the reference patch:    dimz:%6d dims:%6d\n",  */
/*            musterma.dimz, musterma.dims);  */

/*   fprintf (stderr, " actual dimension of the search image patch: dimz:%6d dims:%6d\n\n",  */
/*            suchma.dimz, suchma.dims); */

  /* Perform the correlation                     */
  kqkkor( info, &musterma, &suchma, &erg );

  /* Map the output to the TRACKER3 parameters   */
  *line_offset = erg.dz;
  *samp_offset = erg.ds;
  /* Correlation coefficient (PMK) after the littlest squares adaptation */
  /* Calculate the square of the result, because KQKKOR returns rho and tracker3 uses rho^2 */
  if ( erg.kqk > 0.0 ) 
    *quality =  erg.kqk * erg.kqk; 
  else
    *quality =  erg.kqk; /* -1 */

/*   sprintf( msg, "This is stderr %f %f\n",  erg.kqk, erg.kqk * erg.kqk ); */
/*   zvmessage( msg, " ");  */

/*   if ( *quality > 0.0 ) { */
/*     fprintf( stderr, "KQKKOR : %f %f %f\n", erg.pmk, erg.kqk, erg.mp ); */
/*   } */
 
  *ind = 0;
  /* return output coefficents as well           */
  
  line_coef[1] = info -> affin[0];
  line_coef[0] = info -> affin[1];
  line_coef[2] = info -> affin[2];
  samp_coef[1] = info -> affin[3];
  samp_coef[0] = info -> affin[4];
  samp_coef[2] = info -> affin[5];
  /*fprintf( stderr, "%f <<<<- %f\n", samp_coef[0], info -> affin[3]);
  */
  /*     fprintf( stderr, "DIM NEW: %f %f %f %f %f %f\n", info -> affin[0],  info -> affin[1],info -> affin[2], 
	 info -> affin[3],  info -> affin[4],info -> affin[5]); */

  free( musterma.ptr_m);
  free( suchma.ptr_m);

}

/*************************************************************************** 
   call_affinpar - interface to affinpar routine from the
   kqkkor/affinpar suite. This function returns 1 if the affine
   transformation was successfully found, otherwise returns status of
   the affinepar subroutine (< 1 ). The procedure works with fixed
   lengths arrays, which are defined in the tracker3. 

   INPUTS : 
   line1, samp1 - Line and sample for the point of interest in the
                  patch (left) image
   obuf         - float[4][n] array, containing previously acquired tiepoints
   ptcount      - number of points in obuf

   OUTPUT : 
   line2, samp2         - best guess for Line and sample in the search
                          image. These are not used yet. 
   line_coef, samp_coef - coefficients for the affine transform,
                          computed based on the previously acquired points. 
****************************************************************************/

int call_affinpar(  line1, samp1, obuf, ptcount,
		     line2, samp2, line_coef, samp_coef)
/* INPUT */
     float line1, samp1;
     float obuf[32*4]; 
     int   ptcount; 	    
/* OUTPUT */     
     float *line2, *samp2; /* line2 and samp2 are not used yet */ 
     float *line_coef, *samp_coef;
{
  int i;
  int status;

  /* Declare affinpar inputs.  */
  double lne_in; /* Coordinates of the input point to project */
  double smp_in; 
  
  double  lne_0[32]; /* Tie points */
  double  smp_0[32]; /* *_0 - patch image  */
  double  lne_1[32]; /* *_1 - search image */
  double  smp_1[32];

  double lne_out; /* Coordinates of the input point in the search*/
  double smp_out; /*   (right) image */
  
  double affcoef[6]; /* Affine transformation coefficients */

  /* Map tracker3 inputs to affinpar inputs. */
  lne_in = (double) line1; 
  smp_in = (double) samp1; 
  for( i = 0; i < ptcount; i++ ) {
    lne_0[i] = (double) obuf[i * 4 + 0];
    smp_0[i] = (double) obuf[i * 4 + 1];
    lne_1[i] = (double) obuf[i * 4 + 2];
    smp_1[i] = (double) obuf[i * 4 + 3];
/*     fprintf( stderr, "Input points : %f %f %f %f\n",  lne_0[i],  smp_0[i],  lne_1[i],  smp_1[i]); */
  }
  
  /* Call affinpar and determine the affine transformation coefs. */
  status = affinpar (lne_in, smp_in, 
		     ptcount,  lne_0,  smp_0,  lne_1,  smp_1, 
		     &lne_out, &smp_out, affcoef);
  if ( status != 1) {
    /* If something's wrong with finding the affine transformation */
    /* Let the calling program decide what to do                   */
    return status; 
  }
  
  /* Create output */
  /* Best guess for line and sample - NOT USED */
  *line2 = lne_out;                  
  *samp2 = smp_out;                  

  /* Affine transformation coeffcient */
  line_coef[1] = affcoef[0];
  line_coef[0] = affcoef[1]; 
  line_coef[2] = affcoef[2]; 
  samp_coef[1] = affcoef[3]; 
  samp_coef[0] = affcoef[4]; 
  samp_coef[2] = affcoef[5]; 

  return 1; 
     
}
void flt2int( out,  sx, sy, data, ix, iy)
     int* out;
     float* data;
     int sx, sy, ix, iy;
{
  int i, j;
  char msg[180];

  if ( out == NULL ) {
    sprintf( msg, "Not enough memory to allocate for the patches in KQKKOR %d %d\n", sx, sy);
    zvmessage( msg, " ");
    zabend();
  }

 /*  fprintf( stderr, "%d %d \n", sx, sy);  */
  for ( j = 0; j < sy; j++ ) {
    for ( i = 0; i < sx; i++ ) {
      out[j * sx + i] = (int) floor(data[j * ix + i] + 0.5);
 /*     fprintf( stderr, "%3d ", out[j * sx + i] ); */
    } 
  /*   fprintf( stderr, "\n" ); */
  }
 
}

void print_matrix( int* data, int sx, int sy)
{
  int i, j;

  for ( j = 0; j < sy; j++ ) {
    for( i = 0; i < sx; i++)
      fprintf( stderr, "%3d ", data[j * sx + i]);
    fprintf( stderr, "\n");
  }
  fprintf( stderr, "-----------------------------------------------------------\n");
  
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tracker3.imake
#define  PROGRAM   tracker3

#define DEBUG

#define MODULE_LIST tracker3.c call_kqkkor.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_C

#define LIB_RTL
#define LIB_P2SUB
#define LIB_TAE
$ Return
$!#############################################################################
$PDF_File:
$ create tracker3.pdf
process help=*
PARM INP     TYPE=STRING  COUNT=(2:3)
PARM OUT     TYPE=STRING  COUNT=1
PARM CORREL  TYPE=KEYWORD COUNT=1 +
  VALID=("ANNEAL","LINEAR","AMOEBA","LINAMO","ANNAMO", "KQKKOR") +
  DEFAULT="LINAMO"
PARM NLW     TYPE=INTEGER COUNT=1    DEFAULT=15
PARM NSW     TYPE=INTEGER COUNT=1    DEFAULT=15
PARM NLAREA  TYPE=INTEGER COUNT=1    DEFAULT=35
PARM NSAREA  TYPE=INTEGER COUNT=1    DEFAULT=35
PARM GRID    TYPE=INTEGER COUNT=1    DEFAULT=30
PARM PERCENT TYPE=REAL    COUNT=1    DEFAULT=100.
PARM QUALITY TYPE=REAL    COUNT=1    DEFAULT=.5
PARM LIMIT   TYPE=INTEGER COUNT=1    DEFAULT=2000
PARM COEFRANG TYPE=REAL   COUNT=4 DEFAULT=(.8,1.2,-.35,.35)
PARM PRINT   TYPE=KEYWORD COUNT=1 +
  VALID=("PRINT","NOPRINT") DEFAULT="NOPRINT"
PARM ZERO   TYPE=KEYWORD COUNT=1 +
  VALID=("ZERO","NOZERO") DEFAULT="NOZERO"
PARM EXTEND  TYPE=KEYWORD COUNT=1 +
  VALID=("EXTEND","NOEXTEND") DEFAULT="NOEXTEND"
PARM ASCIIFILE TYPE=STRING COUNT=1 DEFAULT="NONE"
end-proc

.title
TRACKER3

.help
PURPOSE:
To acquire tiepoints between two images. Tiepoints can be on a sparse grid 
or as close as every pixel. The program operates in batch mode only.
There are two tiepoint acquisition strategies:

1. To acquire points on a regular grid.
2. To read tiepoint locations from an input tiepoint file and correlate these
   locations only. Chaining of tiepoints is permitted.

To edit tiepoints already in a tiepoint file you can use interactive program
tptedt or batch program tptedt2.
This program obsoletes TRACKER and TRACKER2

.page
EXECUTION STATEMENT:

Creating a new grid of tiepoints:
    tracker3 inp=(left,right) out=x 

Reading initial tiepoint locations from a tiepoint file:
    tracker3 inp=(left,right,x) out=y

Chaining the same tiepoints through several images:
    tracker3 inp=(a.img,b.img) out=t1.pts grid=40
    tracker3 inp=(b.img,c.img,t1.pts) out=t2.pts 'extend
    tracker3 inp=(c.img,d.img,t2.pts) out=t3.pts 'extend

.page
METHOD:
  The program has two basic modes and a choice of 5 correlation schemes.

Mode 1:
   For two input files a grid is created on the first input image.
A corresponding grid is created on the second input image at the same
locations and each intersection is correlated. Only the right grid
intersections are permitted to move.

Mode 2:
   For three input images the third input file is read and interpreted as 
initial tiepoint locations. No grid is generated. Each tiepoint is
assumed to consist of 4 numbers representing the first and second input
image locations first(line,sample) coordinates and second(line,sample)
coordinates. Either all 4 are read and applied to their respective images 
or only the second pair is read (see EXTEND keyword). If only the second pair
is read it will be copied and used on both images. This permits chaining
of tiepoints through a movie sequence.

  This program offers a suite of correlation schemes most of which provide
the Gruen correlation method. This method permits distortion of the 
correlation area in order to accomodate parallax, and can provide excellent
accuracy in cases where linear correlators fail completely.
  The Gruen scheme used here permits distortions between correlation areas of
the form:
newline=line*A+sample*B+C
newsamp=line*D+sample*E+F
The task of the correlator is to determine the coefficients A-F which permit 
the best correlation value.
  All the correlation schemes use the same figure of merit or objective
function. This is a least squares fit between the template and the search
area. The correlation value is the coefficient of determination for this fit.
It ranges from 0 to 1. Zero means no correlation at all, and one means perfect
correlation or anti-correlation.
  The effect of the above is that two areas can correlate well even if the
data has been:
1. Multiplied, added to, or complemented (any linear intensity mapping 
   is handled).
2. Rotated, scaled, flipped, transposed, or skewed.

The correlation options include:

LINEAR    
  This is a conventional image domain correlator performed on integral
pixel boundaries followed by an interpolation to achieve sub pixel precision.
No gruen correlation is available here.
Best for large area searches where little rotation or scale differences
occur. Best precision is 1/10 pixel. Fastest method.

ANNEAL
  This is an unconventional non-deterministic search based upon the annealing
algorithm Metropolis. Best for large area searches where rotation and
scale are unknown. Slowest method by far. User tailorable using the 
parameters LIMIT and COEFRANG. Best precision is user determined.
The default value of LIMIT should give a precision of about 1/20 pixel with
clean data.

AMOEBA
  This is a deterministic downhill simplex search. Slightly slower than LINEAR.
Provides the highest precision (to 1/100 pixel) and unlimited distortion 
capability. 
Limitation: The initial guess must be within the correlation peak or else 
the tiepoint will be in error. This might mean that the 6 polynomial
mapping coefficients should ALL be close to the correct result. The only
way to assure this in extreme rotation cases is to use the ANNAMO mode.
(This is not an easy problem even for humans.)

LINAMO   (the default mode)
  This uses the LINEAR mode first to get a starting position and finishes
with the AMOEBA mode as the end game.

ANNAMO
  This uses the ANNEAL mode first to get a starting position and finishes
with the AMOEBA mode as the end game.

KQKKOR
 This mode uses a correlation technique from 
Ackermann F. (1984). Digital image correlation : performance and 
potential application in photogrammetry. 

 It is ultimately uses a least squares fit for 6 parameters described
above. KQKKOR is also used in VICAR TP program. Tests showed that
KQKKOR generally works faster than LINAMO technique. KQKKOR demonstrated 6 
to 10 times improvement in speed over LINAMO. However, the number of matches
produced by KQKKOR was less than number of matches produced by LINAMO
by about 80%. On other hand KQKKOR returns less noisy tiepoints. If
you have a lot of images to do the tracking, we recommend compare
LINAMO and KQKKOR using samples from the sequence and decide which
method works better. Be careful, if the template window size (nlw) is
getting small ( < 5 pixels). Correlator might not have enough
information to find the answer. 

For those of you who wish to embed the Gruen correlator into their own C 
programs just copy out of Tracker3 the subroutine Gruen and it's called
routines into your source code. You only need the two #define statements
# define MAX_LEFT_AREA n  ( n is the max template size dimension, ie: n by n)
# define MAX_RIGHT_AREA m  ( m is the max search size dimension, ie: m by m)

PROGRAM HISTORY

Cognizant programmer: Jean Lorre
Revisions: New
6/15/00 abi Added KQKKOR correlation mode. 

.page
.LEVEL1
.VARI INP
First: left image.
Second: right image.
Third: tiepoints
       (optional)

.VARI OUT
Output tiepoint
file.

.VARI ASCIIFILE
Name of optional
output
ascii tiepoints
file.

.VARI CORREL
The correlation
scheme to use.
LINEAR or
AMOEBA or
ANNEAL or
LINAMO or
ANNAMO

.VARI NLW
Number of lines in
left image template.
Odd number.

.VARI NSW
Number of samples in
left image template.
Odd number.

.VARI NLAREA
Number of lines in
right image search 
area.

.VARI NSAREA
Number of samples in
right image search 
area.

.VARI GRID
Grid interval in
pixels.

.VARI PERCENT
Percentage of
template used in
correlation.

.VARI LIMIT
Number of cycles
permitted in 
annealing
algorithm.
annealing only.

.VARI COEFRANG
Range limits of the
mapping polynomial.
annealing only.

.VARI PRINT
Print tiepoints

.VARI ZERO
Reject areas with
zero dn corners.

.VARI QUALITY
Edit poor quality
correlations.

.VARI EXTEND
Specify which tiepoint
column to use. For
three input files.

.LEVEL2

.VARI INP
The input images to acquire tiepoints from.

First is the left or reference image.
      If there is no third input this will have a regular grid. 
Second is the right image.
      If there is no third input this will have an irregular grid.
Third is an optional tiepoints file to be used as initial input values.

.VARI OUT
Output tiepoint file .
At the moment this is a mark file with 512 byte records and 32 tiepoints per
record in the order left_line left_sample right_line right_sample, left_...
all float format.

.VARI ASCIIFILE
Name of the optional output ascii tiepoints file. Default is no ascii file is
produced. (Default name is "NONE").
If created this file will be in the same order as the binary mark file
but will contain one record per tiepoint. 
All data will be in floating point format, possibly extended precision.
It is intended that records will contain the following 9 columns:
line1 samp1 line2 samp2 dn1 dn2 X Y Z
Tracker3 will fill the first 6 columns:
line1 samp1 line2 samp2 dn1 dn2
Where line1 & samp1 are the left image coordinates.
Where line2 & samp2 are the right image coordinates.
Where dn1 & dn2 are the left & right intensities.

.VARI CORREL
The correlation scheme to use. Your options are:
LINEAR (Exhaustive correlation on integral pixel boundaries).
AMOEBA (Simplex deterministic downhill search method).
ANNEAL (Annealing non-deterministic search method).
LINAMO (LINEAR followed by AMOEBA).
ANNAMO (ANNEAL followed by AMOEBA).

See the method description.

.VARI NLW
Number of lines in the left image template.
This is the reference area or correlation size.
Should be an odd number.

.VARI NSW
Number of samples in the left image template.
This is the reference area or correlation size.
Should be an odd number.

.VARI NLAREA
Number of lines in the right image search area.
Usually much larger than NLW.

.VARI NSAREA
Number of samples in the right image search area.
Usually much larger than NSW.

.VARI GRID
Grid interval in pixels. This is the spacing between columns and rows of 
pixels in the left or reference image. Only used when two input images
are present.

.VARI PERCENT
Percentage of the template used in the correlation.

If percent=100 then only one template is used to correlate with.

If percent < 100 then five templates are used, the best one determining
the tiepoint. Template 1 is the standard one. Templates 2-5 permit only
the top,bottom,left,and right PERCENT of the template area to show.
This permits blemishes to be ignored if they are on the edge of the 
correlation area at the expense of five times the overhead.

.VARI LIMIT
Number of cycles permitted in the annealing algorithm. As a rule:

1. The fewer cycles the less reliable the result.
2. If ANNEAL is followed by AMOEBA then fewer cycles are needed.
3. If the coefficient search range COEFRANG is large more cycles are needed.

.VARI COEFRANG
Range limits of the mapping polynomial for annealing options only.

The polynomial mapping is of the form:
rightline=  C1*leftsample+C2*leftline+C3
rightsample=C4*leftsample+C5*leftline+C6
If you specify: coefrang=(.8,1.2,-.35,.35) then
-.35  < C1 < .35
-.35  < C5 < .35
.8    < C2 < 1.2
.8    < C4 < 1.2
Coefficient solutions will be restricted to this range.

The defaults (above example) permit rotations either way of up to 
about 20 degrees, and scaling of about 20 percent.
ANNEAL and ANNAMO options only.

.VARI QUALITY
Only permit tiepoints to be placed into the output file if their correlation
quality exceeds QUALITY. Quality is computed for each corralation and is
a number from 0 (no correlation at all) to 1 (perfect correlation).

.VARI ZERO
Reject correlation areas if any of the corners are zero dn.
This permits the zero region outside map projected images to be detected
and not included in correlations.
Whether zero is specified or not all areas which are of the same dn 
(flat fields) are rejected anyway.

.VARI PRINT
Print tiepoints and their qualities.

.VARI EXTEND
If there are three input files the initial tiepoints will be obtained from 
the third input. The default is to use the left and right tiepoint pairs for
the first and second input files respectively. If however you wish to chain
existing tiepoints through successive images (to track points ) the EXTEND
option will place the right tiepoint side on the FIRST input file and then
create a copy of this point for the second input file. For example, if you had
a movie sequence with images a b c d you would first create a grid of
points with:
tracker3 inp=(a.img,b.img) out=t1.pts grid=40
and then chain them through the other images with:
tracker3 inp=(b.img,c.img,t1.pts) out=t2.pts 'extend
tracker3 inp=(c.img,d.img,t2.pts) out=t3.pts 'extend

.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttracker3.pdf
procedure
refgbl $echo
refgbl $becho
body
let _onfail="continue"
let $echo="yes"
let $becho="trace"
!
gausnois out=a.img nl=101 ns=101
boxflt2 inp=a.img out=left.img nlw=3 nsw=3
geoma inp=left.img out=right.img nah=1 nav=1 tiepoint= +
  ( 13. 19. 13. 13.      19. 73. 13. 73. +
    67. 13. 73. 13.      73. 67. 73. 73. )
tracker3 inp=(left.img,right.img) out=pts.img grid=30 nlw=11 nsw=11 +
  nlarea=25 nsarea=25 'print asciifile=asciipts
tracker3 inp=(right.img,left.img,pts.img) out=pts2.img nlw=11 nsw=11 +
  nlarea=25 nsarea=25 'extend 'print
tracker3 inp=(left.img,right.img) out=pts.img grid=30 +
  'print 'zero percent=75. quality=.8 
tracker3 inp=(left.img,right.img) out=pts.img grid=30 +
  nlw=11 nsw=11 nlarea=25 nsarea=25 +
  'print correl=ANNEAL coefrang=(.81,1.21,-.35,.351) limit=3000
tracker3 inp=(left.img,right.img) out=pts.img grid=30 +
  nlw=11 nsw=11 nlarea=25 nsarea=25 +
  'print correl=LINEAR
tracker3 inp=(left.img,right.img) out=pts.img grid=30 +
  nlw=11 nsw=11 nlarea=25 nsarea=25 +
  'print correl=AMOEBA
tracker3 inp=(left.img,right.img) out=pts.img grid=30 +
  nlw=11 nsw=11 nlarea=25 nsarea=25 +
  'print correl=ANNAMO
tracker3 inp=(left.img,right.img) out=pts.img grid=30 +
  nlw=11 nsw=11 nlarea=25 nsarea=25 +
  'print correl=LINAMO 
tracker3 inp=(left.img,right.img) out=pts.img grid=30 +
  nlw=11 nsw=11 nlarea=25 nsarea=25 +
  'print correl=KQKKOR 
end-proc
$ Return
$!#############################################################################
