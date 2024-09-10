$!****************************************************************************
$!
$! Build proc for MIPL module sky
$! VPACK Version 1.9, Monday, December 22, 1997, 12:41:19
$!
$! Execute by entering:		$ @sky
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
$ write sys$output "*** module sky ***"
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
$ write sys$output "Invalid argument given to sky.com file -- ", primary
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
$   if F$SEARCH("sky.imake") .nes. ""
$   then
$      vimake sky
$      purge sky.bld
$   else
$      if F$SEARCH("sky.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sky
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sky.bld "STD"
$   else
$      @sky.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sky.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sky.com -
	-s sky.c header.h qfile.h newgetxy.c qread.c xypos.c readhdr.c -
	   utility.c transteq.c hsmooth.c dodecode.c qtreedec.c bitinput.c -
	   condmsrd.c fillhdr.c fillpos.c conrddms.c hdcmprss.c position.c -
	   ppoinv.c amdinv.c traneqst.c decode.c undigitz.c ppopos.c amdpos.c -
	   hinv.c pltmodel.c precess.c b195j200.c -
	-i sky.imake -
	-p sky.pdf -
	-t tstsky.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sky.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* sky */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "header.h"
#include "qfile.h"
#include "vicmain_c"
#include <simcli.h>
#include <mdms_pwdclient.h>

#define PI 3.14159265359
#define MAX_NS 14000
int smooth = 1;

extern void pltlist();
extern int  gethdr();
extern void condmsrd();
extern void conrddms();
extern void b1950_j2000();
extern int  getradec();
extern int  writehhh();
extern int  writefits();

main44()
{
   Header header;
   FILE *IN;
   QFILE *infile;
   char headers[80],data[80],radec[80],filename[80],msg[81];
   char Rgn[10],Plate[10],Survey[10],best_Rgn[10];
   char object[75],scr[2];
   char *p;
   char Dec_sign[2];
   unsigned char cobuf[100];
   int status,count,def,line,grid_dn;
   int ounit,nl,ns,i,j,k,m,n,k1,k2[MAX_NS],disk,best_disk;
   int RA_h,RA_m,Dec_d,Dec_m,count_ra,count_dec;
   short int *image;
   double Dec_1,Dec_2,RA_1,RA_2,Separation,min_sep;
   double grid,hr,min,sec,deg,mind,secd,yr,sign;
   double ra_in[3],dec_in[3],ragrid,decgrid,epoch;
   float mag, colour,x,y,xc,yc,x0,y0,fov;
   float RA_s,Dec_s;

/* for simbad */
   char hostname[256], service[8],userid[MAX_USERID+1], passwd[MAX_USERID+1] ;
   char rec[256], *t1 ;
   int hh, nitems, ndata;

/* for KERBEROS */
   char errorBuff[PWDMAXERRSTRLEN];
   char password[256],userName[L_cuserid];
   char *cuserid_p2();
   int errorNum;

/* for status file, write "failure" to begin with */
   FILE *stat_file;
   char stat_dir[100];
   status=zvparm("STATUS",stat_dir,&count,&def,1,0);
   if(strlen(stat_dir) == 0)strcpy(stat_dir,"status_file.txt");
   else strcat(stat_dir,"/status_file.txt");
   if ((stat_file = fopen(stat_dir, "w")) == NULL) {
      fprintf(stderr, "Error opening status file %s\n",stat_dir);
      zabend();
   }
   if (fputs("failure\n", stat_file) == NULL) {
      fprintf(stderr, "Error writing status file %s\n",stat_dir);
      zabend();
   }

/* vicar parameters */
   status=zveaction("AS","  ");
   status=zvparm("HEADERS",headers,&count,&def,1,0);
   status=zvparm("DATA",data,&count,&def,1,0);
   status=zvparmd("RA",ra_in,&count_ra,&def,3,0);
   status=zvparmd("DEC",dec_in,&count_dec,&def,3,0);
   status=zvparmd("EPOCH",&epoch,&count,&def,1,0);
   status=zvparm("PASSWORD",passwd,&count,&def,1,0);
   status=zvparm("USERID",userid,&count,&def,1,0);
   status=zvparm("NAME",rec,&count,&def,1,0);
   status=zvparm("NL",&nl,&count,&def,1,0);
   status=zvparm("NS",&ns,&count,&def,1,0);
   status=zvparm("FOV",&fov,&count,&def,1,0);
   status=zvparmd("GRID",&grid,&count,&def,1,0);
   status=zvparm("DN",&grid_dn,&count,&def,1,0);

/* retrieve data from SIMBAD data base if ra & dec not specified */
   if((count_ra > 0) && (count_dec > 0)){

     /* Convert parameter input ra & dec to radians  */
     if((count_ra == 3) && (count_dec == 3)){
       RA_1=(ra_in[0]+ra_in[1]/60.+ra_in[2]/3600.)*15.*PI/180.;
       Dec_1=(fabs(dec_in[0])+fabs(dec_in[1])/60.+fabs(dec_in[2])/3600.)
         *PI/180.;
       if((dec_in[0] < 0.) || (dec_in[1] < 0.) || (dec_in[2] < 0.)) 
         Dec_1= -Dec_1;
     }
     else{
       RA_1=(ra_in[0])*PI/180.;
       Dec_1=(dec_in[0])*PI/180.;
     }
   }
   else{

     /* get password from password server through a kerberos ticket */
     if(strcmp(passwd,"none") == 0){
       strcpy(userName, cuserid_p2());
       errorNum = mdms_passwordSrvGet (userName,"simbad", password);
       if(errorNum < PWDSUCCESS){
         zvmessage("Cannot obtain password, you must first:"," ");
         zvmessage("selcat o"," ");
         zvmessage("then run kinit to get a kerberos ticket"," ");
         fputs("Cannot obtain password\n", stat_file);
         fputs("Run kinit to get a kerberos ticket\n", stat_file);
         zabend();
       }
       else{
         strcpy(passwd,password);
       }
     }

     /* start SIMBAD */
     hostname[0]='\0';
     service[0]='\0';
     zvmessage("Simbad data base access begun."," ");
     hh = simbad_connect(hostname,service,userid,passwd) ;
     if (hh < 0){
       printf("SIMBAD: Connection not done: %s.\n",simbad_error(0)) ;
       fputs("SIMBAD: Connection not done\n", stat_file);
       zabend();
     }
 
     nitems = simbad_query(hh,rec,"") ;
     if (nitems >= 1){
       simbad_retrieve(hh,1) ;  /* retrieve 1st object */
       ndata = simbad_findata(hh,"J","") ;
       if (ndata == 1){
         strcpy(msg,simbad_getdata(hh,1));
         t1=strtok(msg," ");
         RA_1=atof(t1);
         t1=strtok(NULL," ");
         Dec_1=atof(t1);
         printf("Coord B2000 RA=%f Dec=%f \n",RA_1,Dec_1);
         RA_1=RA_1*PI/180.;  /* convert to radians */
         Dec_1=Dec_1*PI/180.;
       }
       else{
         printf("SIMBAD: No coordinates\n") ;
         fputs("SIMBAD: No coordinates\n", stat_file);
         zabend();
       }
     }
     else{
       printf("SIMBAD: Object not found.\n") ;
       fputs("SIMBAD: Object not found\n", stat_file);
       zabend();
     } 
     simbad_disconnect(hh) ;
   }

/* precess */
   /* precess from b1950 to j2000 */
   if(fabs(epoch-1950.0) < .01){
     RA_2=RA_1;
     Dec_2=Dec_1;
     b1950_j2000(RA_2,Dec_2,&RA_1,&Dec_1);
   }
   /* are in j2000 already */
   else if(fabs(epoch-2000.0) < .01){
   }
   /* precess ra & dec at any epoch to j2000  */
   else{
     RA_1=RA_1*180./PI;
     Dec_1=Dec_1*180./PI;
     precess(RA_1,Dec_1,epoch,&RA_2,&Dec_2);
     RA_1=RA_2*PI/180.;
     Dec_1=Dec_2*PI/180.;
   }

/* Decimate RA grid if near pole */
   decgrid=grid;
   sign=fabs(Dec_1*180./PI);
   if(sign >= 89.9)sign = 89.9;
   i=1.0/cos(sign*PI/180.);
   ragrid=grid*i;

/* search master header for nearest region */
   strcpy(filename,headers);
   strcat(filename,"/");
   strcat(filename,"lo_comp.lis");
   zvmessage("searching master header file:"," ");
   zvmessage(filename," ");
   if((IN = fopen(filename, "r")) == NULL){
     fprintf(stderr, "Error opening file %s\n", filename);
     fputs("Error opening file\n", stat_file);
     zabend();
   }
   min_sep=1.e+10;
   best_disk=0;
   /* old loop, quit working 11/1/97 ???   for(i=0; i < 43; i++){ */
   for(i=0; i < 50; i++){
     if((fgets(msg, sizeof(msg), IN)) == NULL) goto endit;
   }
   for(i=0; i < 100000; i++){
      if((fgets(msg, sizeof(msg), IN)) == NULL) goto endit;

      /* read the dec sign & fill in blank after sign*/
      p=strchr(msg,'-');
      if(p != NULL){
        sign=-1.0;
        if(*(p+1) == ' ') *(p+1)='0';
      }      
      p=strchr(msg,'+');
      if(p != NULL){
        sign=1.0;
        if(*(p+1) == ' ') *(p+1)='0';
      }      

      sscanf(msg,"%s %s %lf %lf %lf %lf %lf %lf %lf %s %d",
            Rgn,Plate,&hr,&min,&sec,&deg,&mind,&secd,&yr,Survey,&disk);
      RA_2=(hr+min/60.+sec/3600.)*15.*PI/180.;
      Dec_2=sign*(fabs(deg)+fabs(mind)/60.+fabs(secd)/3600.)*PI/180.;
      Separation= acos(
            cos( PI/2. - Dec_2 )*cos( PI/2. - Dec_1 ) +
            sin( PI/2. - Dec_2 )*sin( PI/2. - Dec_1 )*cos( RA_1 - RA_2 ) );
      if(Separation < min_sep){
        min_sep=Separation;
        strcpy(best_Rgn,Rgn);
        best_disk=disk;
      }
   }
   endit:;
   if(best_disk == 0){
     zvmessage("The CD was identified but is unavailable"," ");
     fputs("The CD was identified but is unavailable\n", stat_file);
     zabend();
   }
   for(i=0; i < strlen(best_Rgn); i++) best_Rgn[i]=tolower(best_Rgn[i]);
   printf("The nearest sky region is %s \n",best_Rgn);
   printf("Located on CD number %d\n",best_disk);

/* read the header file for the best region */
   strcpy(filename,headers);
   strcat(filename,"/");
   strcat(filename,best_Rgn);
   strcat(filename,".hhh");
   zvmessage("searching for calibration data in plate header file:"," ");
   zvmessage(filename," ");
   infile = qopen(filename, 80, 0);
   readhdr(infile, &header);
   qclose(infile);

/* compute nl & ns from field of view */
   if(ns == 0){
     ns=fov*60000./(header.plt_scale*header.x_pixel_size);
     if(ns > MAX_NS){
       zvmessage("Too large a field of view"," ");
       zvmessage("Computed NS too large,reset"," ");
       ns=MAX_NS;
     }
     nl=ns;
   }

/* compute x,y plate location of desired ra & dec */
    mag = 0.0;
    colour = 0.0;
    xypos(&header,RA_1,Dec_1,mag,colour,&x,&y);
    printf("The target x,y plate location = %f %f pixels\n",x,y);
    xc=x; yc=y;
    if ( (x < 0) || (x>13999) || (y < 0) || (y > 13998) ) {
        zvmessage(" RA, Dec position lies off edge of plate"," ");
        fputs("RA, Dec position lies off edge of plate\n", stat_file);
        zabend();
    }

/* extract image into "image" */
   strcpy(filename,data);
   strcat(filename,"/usa_aura_stsi_dss1_");
   sprintf(msg,"%4d",best_disk);
      p=strchr(msg,' ');
      if(p != NULL) *(p)='0';
      p=strchr(msg,' ');
      if(p != NULL) *(p)='0';
      p=strchr(msg,' ');
      if(p != NULL) *(p)='0';
   strcat(filename,msg);
   strcat(filename,"/");
   strcat(filename,best_Rgn);
   strcat(filename,"/");
   strcat(filename,best_Rgn);
   strcat(filename,".YX");
   zvmessage("searching symbolic data file (the YX is fictitious):"," ");
   zvmessage(filename," ");
   image = (short *) malloc(sizeof(short)*ns*nl);
   newgetxy(&header,object,x,y,ns,nl,image,filename);
   zvmessage("   "," ");

/* we now have a picture in buffer image with consecutive lines, but
   DEC is reversed. Flip the image. */
   for(j=0; j < nl/2; j++){
     m=ns*j;
     n=ns*(nl-1-j);
     for(i=0; i < ns; i++) k2[i]=image[n+i];
     for(i=0; i < ns; i++) image[n+i]=image[m+i];
     for(i=0; i < ns; i++) image[m+i]=k2[i];
   }

/* draw RA & Dec lines every grid interval */
   x0 = xc -(ns/2.0); /* left edge of image */
   y0 = yc -(nl/2.0); /* bottom of image */
   if(grid > 0.0){
     zvmessage("Drawing RA & DEC lines"," ");
     for(line=nl; line >= 1; line--){       /* line */
       for(i=ns; i >= 1; i--){              /* sample */

         /* convert pixel to x,y plate coordinates */
         x=i+x0-1.0;
         y=nl-line+y0;

         /* convert plate coordinates to RA & Dec */
         position(&header,x,y,mag,colour,&RA_2,&Dec_2);
         RA_2 = RA_2*180/PI;
         Dec_2 = Dec_2*180/PI;

         /* RA */
         j = (RA_2 * 60.0)/ragrid;
         if(i == ns){
           k1=j;
         }
         else{
           if(j != k1){
             image[(line-1)*ns+i-1]=grid_dn;
             k1=j;
           }
         }

         /* Dec */
         j = (Dec_2 * 60.0)/decgrid;
         if(line == nl){
           k2[i-1]=j;
         }
         else{
           if(j != k2[i-1]){
             image[(line-1)*ns+i-1]=grid_dn;
             k2[i-1]=j;
           }
         }

       }
     }

     /* Annotate the grid lines */

     /* draw RA annotation */
     for(i=ns; i >= 1; i--){              /* sample */

       /* convert pixel to x,y plate coordinates */
       x=i+x0-1.0;
       y=nl-1+y0;

       /* convert plate coordinates to RA & Dec */
       position(&header,x,y,mag,colour,&RA_2,&Dec_2);
       RA_2 = RA_2*180/PI;

       j = (RA_2 * 60.0)/ragrid;
       if(i == ns){
         k1=j;
       }
       else{
         if(j != k1){
           RA_2=(double)j*ragrid/60.0;
           conrddms(RA_2*PI/180.,Dec_2,&RA_h,&RA_m,&RA_s,&Dec_sign,
                    &Dec_d,&Dec_m,&Dec_s);
           k1=j;
           for(j=0; j < 7; j++){
             ztext("RA",2,j,cobuf,6,1);
             for(k=0; k < 2*6; k++){
               m=k+i-6;
               if((m >= 0) && (m <= ns)){
                 if(cobuf[k] == 1) image[j*ns+m]=grid_dn;
                 else image[j*ns+m]=0;
               }
             }
           }
           sprintf(object,"%-2d",RA_h);
           for(j=0; j < 7; j++){
             ztext(object,2,j,cobuf,6,1);
             for(k=0; k < 2*6; k++){
               m=k+i-6;
               if((m >= 0) && (m <= ns)){
                 if(cobuf[k] == 1) image[(j+8)*ns+m]=grid_dn;
                 else image[(j+8)*ns+m]=0;
               }
             }
           }
           sprintf(object,"%-2d",RA_m);
           for(j=0; j < 7; j++){
             ztext(object,2,j,cobuf,6,1);
             for(k=0; k < 2*6; k++){
               m=k+i-6;
               if((m >= 0) && (m <= ns)){
                 if(cobuf[k] == 1) image[(j+16)*ns+m]=grid_dn;
                 else image[(j+16)*ns+m]=0;
               }
             }
           }
           sprintf(object,"%-5.2f",RA_s);
           for(j=0; j < 7; j++){
             ztext(object,5,j,cobuf,6,1);
             for(k=0; k < 5*6; k++){
               m=k+i-6;
               if((m >= 0) && (m <= ns)){
                 if(cobuf[k] == 1) image[(j+24)*ns+m]=grid_dn;
                 else image[(j+24)*ns+m]=0;
               }
             }
           }
         }
       }
     }

     /* draw DEC annotation */
     for(line=nl; line >= 1; line--){          /* line */

       /* convert pixel to x,y plate coordinates */
       x=x0;
       y=nl-line+y0;

       /* convert plate coordinates to RA & Dec */
       position(&header,x,y,mag,colour,&RA_2,&Dec_2);
       Dec_2 = Dec_2*180/PI;

       j = (Dec_2 * 60.0)/decgrid;
       if(line == nl){
         k1=j;
       }
       else{
         if(j != k1){
           if(Dec_2 >= 0.0) Dec_2=(double)j*decgrid/60.0;
           else            Dec_2=(double)(j-1)*decgrid/60.0;
           k1=j;
           conrddms(RA_2,Dec_2*PI/180.,&RA_h,&RA_m,&RA_s,&Dec_sign,
                    &Dec_d,&Dec_m,&Dec_s);
           sprintf(object,"%s %-2d",Dec_sign,Dec_d);
           for(j=0; j < 7; j++){
             if(j+line <= nl){
               ztext(object,4,j,cobuf,6,1);
               for(k=0; k < 4*6; k++){
                 if(cobuf[k] == 1) image[(j+line-1)*ns+k]=grid_dn;
                 else image[(j+line-1)*ns+k]=0;
               }
             }
           }
           sprintf(object,"%-2d",Dec_m);
           for(j=0; j < 7; j++){
             if(j+7+line <= nl){
               ztext(object,2,j,cobuf,6,1);
               for(k=0; k < 2*6; k++){
                 if(cobuf[k] == 1) image[(j+7+line-1)*ns+k]=grid_dn;
                 else image[(j+8+line-1)*ns+k]=0;
               }
             }
           }
           sprintf(object,"%-3.1f",Dec_s);
           for(j=0; j < 7; j++){
             if(j+14+line <= nl){
               ztext(object,3,j,cobuf,6,1);
               for(k=0; k < 3*6; k++){
                 if(cobuf[k] == 1) image[(j+14+line-1)*ns+k]=grid_dn;
                 else image[(j+16+line-1)*ns+k]=0;
               }
             }
           }
         }
       }
     }
   
   } /* end of drawing */

/* open vicar file */
   status=zvunit(&ounit,"OUT",1,0);
   zvsignal(ounit,status,1);
   status=zvopen(ounit,"OP","WRITE","U_NL",nl,"U_NS",ns,"U_NB",1,
                      "U_FORMAT","HALF","O_FORMAT","HALF",0);
   zvsignal(ounit,status,1);

/* copy header structure data to picture label */
   status=zladd(ounit,"HISTORY","plate_root",header.plate_root,
          "FORMAT","STRING",0);
   status=zladd(ounit,"HISTORY","header_dir",header.header_dir,
          "FORMAT","STRING",0);
   status=zladd(ounit,"HISTORY","compression",header.compression,
          "FORMAT","STRING",0);
   status=zladd(ounit,"HISTORY","plate_name",header.plate_name,
          "FORMAT","STRING",0);
   status=zladd(ounit,"HISTORY","plate_id",header.plate_id,
          "FORMAT","STRING",0);
   status=zladd(ounit,"HISTORY","region_no",header.region_no,
          "FORMAT","STRING",0);
   status=zladd(ounit,"HISTORY","section_x_length",&header.section_x_length,
          "FORMAT","INT",0);
   status=zladd(ounit,"HISTORY","section_y_length",&header.section_y_length,
          "FORMAT","INT",0);
   status=zladd(ounit,"HISTORY","section_x_corner",&header.section_x_corner,
          "FORMAT","REAL",0);
   status=zladd(ounit,"HISTORY","section_y_corner",&header.section_y_corner,
          "FORMAT","REAL",0);
   status=zladd(ounit,"HISTORY","ra_h",&header.ra_h,
          "FORMAT","INT",0);
   status=zladd(ounit,"HISTORY","ra_m",&header.ra_m,
          "FORMAT","INT",0);
   status=zladd(ounit,"HISTORY","dec_d",&header.dec_d,
          "FORMAT","INT",0);
   status=zladd(ounit,"HISTORY","dec_m",&header.dec_m,
          "FORMAT","INT",0);
   status=zladd(ounit,"HISTORY","ra_s",&header.ra_s,
          "FORMAT","REAL",0);
   status=zladd(ounit,"HISTORY","dec_s",&header.dec_s,
          "FORMAT","REAL",0);
   scr[0]=header.dec_sign; scr[1]='\0';
   status=zladd(ounit,"HISTORY","dec_sign",scr,
          "FORMAT","STRING",0);
   status=zladd(ounit,"HISTORY","x",&header.x,
          "FORMAT","REAL",0);
   status=zladd(ounit,"HISTORY","y",&header.y,
          "FORMAT","REAL",0);
   status=zladd(ounit,"HISTORY","plt_scale",&header.plt_scale,
          "FORMAT","DOUB",0);
   status=zladd(ounit,"HISTORY","x_pixel_size",&header.x_pixel_size,
          "FORMAT","DOUB",0);
   status=zladd(ounit,"HISTORY","y_pixel_size",&header.y_pixel_size,
          "FORMAT","DOUB",0);
   status=zladd(ounit,"HISTORY","plt_center_vec",header.plt_center_vec,
          "FORMAT","DOUB","NELEMENT",3,0);
   status=zladd(ounit,"HISTORY","ppo_coeff",header.ppo_coeff,
          "FORMAT","DOUB","NELEMENT",6,0);
   status=zladd(ounit,"HISTORY","amd_x_coeff",header.amd_x_coeff,
          "FORMAT","DOUB","NELEMENT",20,0);
   status=zladd(ounit,"HISTORY","amd_y_coeff",header.amd_y_coeff,
          "FORMAT","DOUB","NELEMENT",20,0);
   status=zladd(ounit,"HISTORY","plt_center_ra",&header.plt_center_ra,
          "FORMAT","DOUB",0);
   status=zladd(ounit,"HISTORY","plt_center_dec",&header.plt_center_dec,
          "FORMAT","DOUB",0);
   scr[0]=header.amd_flag;
   status=zladd(ounit,"HISTORY","amd_flag",scr,
          "FORMAT","STRING",0);
   scr[0]=header.ppo_flag;
   status=zladd(ounit,"HISTORY","ppo_flag",scr,
          "FORMAT","STRING",0);
   scr[0]=header.plate_data_flag;
   status=zladd(ounit,"HISTORY","plate_data_flag",scr,
          "FORMAT","STRING",0);
   scr[0]=header.special_plate_flag;
   status=zladd(ounit,"HISTORY","special_plate_flag",scr,
          "FORMAT","STRING",0);
   status=zladd(ounit,"HISTORY","plt_grade",&header.plt_grade,
          "FORMAT","INT",0);
   status=zladd(ounit,"HISTORY","emulsion_code",&header.emulsion_code,
          "FORMAT","INT",0);
   status=zladd(ounit,"HISTORY","scaling",&header.scaling,
          "FORMAT","REAL",0);
   status=zladd(ounit,"HISTORY","mag_limit",&header.mag_limit,
          "FORMAT","REAL",0);
   status=zladd(ounit,"HISTORY","plt_date",&header.plt_date,
          "FORMAT","REAL",0);
   status=zladd(ounit,"HISTORY","object_name",header.object_name,
          "FORMAT","STRING",0);
   status=zladd(ounit,"HISTORY","exposure_time",header.exposure_time,
          "FORMAT","STRING",0);
   status=zladd(ounit,"HISTORY","seeing",header.seeing,
          "FORMAT","STRING",0);
   status=zladd(ounit,"HISTORY","origin",header.origin,
          "FORMAT","STRING",0);
   status=zladd(ounit,"HISTORY","plate_label",header.plate_label,
          "FORMAT","STRING",0);

   status=zladd(ounit,"HISTORY","x_of_sample_1",&x0,
          "FORMAT","REAL",0);
   status=zladd(ounit,"HISTORY","y_of_line_1",&y0,
          "FORMAT","REAL",0);

/* copy picture data to vicar file */
   for(line=1; line <= nl; line++){
     status=zvwrit(ounit,&image[(line-1)*ns],"LINE",line,0);
     zvsignal(ounit,status,1);
   }                      /* line loop */

/* for status file, write "success" */
   fclose(stat_file);
   if ((stat_file = fopen(stat_dir, "w")) == NULL) {
      fprintf(stderr, "Error opening status file %s\n",stat_dir);
      zabend();
   }
   if (fputs("success\n", stat_file) == NULL) {
      fprintf(stderr, "Error writing status file %s\n",stat_dir);
      zabend();
   }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create header.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* structure for FITS header information */
typedef struct {
    /*
     * plate directory information
     */
    char plate_root[80];    /* directory root for plates                */
    char header_dir[80];    /* directory root for header files          */
    char compression[7];    /* compression to use (low, medium, high)   */
    char plate_name[7];     /* name of this plate                       */
    /*
     * plate id, region identification
     */
    char plate_id[4], region_no[6];
    /*
     * size of section and corner position 
     */
    int section_x_length, section_y_length;
    float section_x_corner, section_y_corner;
    /*
     * target position
     */
    int ra_h, ra_m, dec_d, dec_m;
    float ra_s, dec_s;
    char dec_sign;
    float x, y;
    /*
     * plate solution parameters
     */
    double plt_scale, x_pixel_size, y_pixel_size,
        plt_center_vec[3], ppo_coeff[6],
        amd_x_coeff[20], amd_y_coeff[20],
        plt_center_ra, plt_center_dec;
    char amd_flag, ppo_flag, plate_data_flag,
        special_plate_flag;
    /*
     * miscellaneous information from header keywords
     */
    int plt_grade, emulsion_code;
    float scaling, mag_limit, plt_date;
    char object_name[19], exposure_time[19], seeing[4], origin[19],
        plate_label[7];
    /*
     * header lines
     */
    char hlines[200][81];
    int nhlines;
} Header;
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create qfile.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* qfile.h   File structure definition
 *
 * Programmer: R. White     Date: 16 June 1992
 */

struct qfile {
    FILE *file;                 /* File descriptor pointer          */
    char *filename;             /* File name                        */
    int recordsize;             /* Record size (bytes)              */
    int bufsize;                /* Buffer size (bytes)              */
    unsigned char write;        /* Write flag (0 for readonly)      */
    unsigned char crrat;        /* CR attribute flag (0 for no CRs) */
    unsigned char *buffer;      /* Record buffer                    */
    int bptr;                   /* Current pointer in buffer        */
    };

#define QFILE struct qfile

/*
 * External procedures
 */
extern QFILE *qcreat();
extern QFILE *qopen();
extern void  qread();
extern void  qwrite();
extern void  fillbuff();
extern void  dumpbuff();
extern int   readint();
extern void  writeint();

/*
 * Macros to get and put characters to files
 * Usage:
 *     c = qgetc(qfile);
 *     qputc(c,qfile);
 */
#define qgetc(f) ((f->bptr<f->bufsize) ? f->buffer[f->bptr++]                  \
                                       : (fillbuff(f), f->buffer[f->bptr++]))

#define qputc(c,f) { if(f->bptr >= f->recordsize) dumpbuff(f);                 \
                     f->buffer[f->bptr++] = c; }
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create newgetxy.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       NEWGETXY
 *
 * PURPOSE:
 *       Extract a subimage from a plate by specifying the central position
 *       XC,YC and the size of the section NX,NY in pixels.
 *
 * CALLING SEQUENCE:
 *       newgetxy(h,object,xc,yc,nx,ny,image,filename)
 *
 * INPUTS:
 *       h          (* Header) header structure
 *       object     (* char) name of object
 *       xc,yc      (float) center of image on plate
 *       nx,ny      (int) size of image
 *       smooth     (external int) non-zero to use smoothing
 *
 * OUTPUTS:
 *       image      (short [ny][nx]) image section
 *       h          the header is updated to reflect the position of section
 *
 * PROCEDURE:
 *       The plate is assumed to be stored in compressed 500x500 blocks
 *       (or 500x499 for the last row).  The required blocks are decompressed
 *       one at a time and stored into image.  The header is updated to
 *       include the current values of target RA, Dec, etc.  If part
 *       of the image is off the edge of the plate, zeros are returned.
 *       A warning is printed if the entire image is off the plate.
 *       The smooth flag (external variable) is passed to hdecompress and
 *       determines whether the images are smoothed as they are decompressed.
 *
 * MODIFICATION HISTORY
 *       Created by R. White, 31 July 1991
 *       Added smoothing flag, 14 April 1992
 *       Modified to handle Ultrix uppercase filenames, 26 May 1993
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "header.h"

int smooth;

#define max(a,b)    (((a) > (b)) ? (a) : (b))
#define min(a,b)    (((a) < (b)) ? (a) : (b))

#ifdef ULTRIX
static char translate[37] = "0123456789ABCDEFGHIJKLMNOPQR";
#else
static char translate[37] = "0123456789abcdefghijklmnopqr";
#endif

/*extern char *makepname();*/
extern void position();
extern void conrddms();
extern void hdecompress();
extern void fillhdr();
extern void fillpos();

extern void
newgetxy(h,object,xc,yc,nx,ny,image,filename)
Header *h;
char *object;
double xc,yc;
int nx,ny;
short image[];
char *filename;
{
int i, j, ii, jj, i1, i2, j1, j2, b, x0, y0, x1, x2, y1, y2,
    si1, si2, sii, sj1, sj2, sjj, imi1, imi2, imj1, imj2,
    snx, sny, ss, ncy;
double ra, dec;
float mag, colour;
int rah, ram, decd, decm;
char decsign;
float ras, decs;
int nf;
int *subim;

    /*
     * calculate corner position
     */
    x0=xc-(nx/2);
    y0=yc-(ny/2);
    /*
     * calculate RA and Dec of xc,yc
     */
    mag = 0.0;
    colour = 0.0;
    position(h,xc,yc,mag,colour,&ra,&dec);
    conrddms(ra, dec, &rah, &ram, &ras, &decsign, &decd, &decm, &decs);
    /*
     * get plate file template
     */
    /*filename = makepname(h);*/
    nf = strlen(filename)+1;
    /*
     * get range of image pixels which we will extract if we're not off edge
     */
    x1 = x0;
    x2 = x0+nx-1;
    y1 = y0;
    y2 = y0+ny-1;
    /*
     * now get actual range of sections (limited to 0 -- 27)
     */
    i1=x1/500; if (i1< 0) i1 = 0;
    i2=x2/500; if (i2>27) i1 = 27;
    j1=y1/500; if (j1< 0) j1 = 0;
    j2=y2/500; if (j2>27) j2 = 27;
    /*
     * initialize image to zero
     */
    for (i=0; i<nx*ny; i++) image[i] = 0;
    if ((i2 < i1) || (j2 < j1)) {
        fprintf(stderr,
            "warning: image section lies entirely off edge of plate\n");
        return;
    }
    /*
     * read each section and copy required pixels to image
     */
    b=0;
    fprintf(stderr,"decompressing %1d blocks: ",(i2-i1+1)*(j2-j1+1));
    for (i = i1; i <= i2; i++) {
        /*
         * get subscript ranges on subimage, image
         */
        si1 = x1-i*500;
        si2 = x2-i*500;
        imi1 = max(-si1,0);
        imi2 = x2-x1-max(si2-499,0);
        si1 = max(si1,0);
        si2 = min(si2,499);
        for (j = j1; j <= j2; j++) {
            b = b + 1;
            fprintf(stderr," %1d",b);
            /*
             * read subimage i,j
             */
            filename[nf-3] = translate[j];
            filename[nf-2] = translate[i];
            /*
             * Decompress image in filename
             * Returns address subim and size snx,sny
             */
            hdecompress(&subim,&sny,&snx,filename,smooth);
            if (j == 27) {
                ncy=499;
            } else {
                ncy=500;
            }
            if ((snx != 500) || (sny != ncy)) {
                fprintf(stderr, "\nerror: bad image size for %s\n", filename);
                fprintf(stderr, "should be 500 %3d, but is %3d %3d\n",
                    ncy,snx,sny);
                exit(-1);
            }
            sj1 = y1-j*500;
            sj2 = y2-j*500;
            imj1 = max(-sj1,0);
            imj2 = y2-y1-max(sj2-(ncy-1),0);
            sj1 = max(sj1,0);
            sj2 = min(sj2,ncy-1);
            sjj = sj1;
            for (jj=imj1; jj<=imj2; jj++) {
                sii = si1;
                for (ii=imi1; ii<=imi2; ii++) {
                    /*
                     * force pixel value into range of short integers
                     */
                    ss = subim[sii+snx*sjj];
                    if (ss>32767) {
                        ss = 32767;
                    } else if (ss < -32768) {
                        ss = -32768;
                    }
                    image[ii+nx*jj] = ss;
                    sii += 1;
                }
                sjj += 1;
            }
            free((char *) subim);
        }
    }
    /*
     * now fill in header values for this position
    printf("one\n");
    fillhdr(h, object, nx, ny, x0, y0, image);
    printf("two\n");
    fillpos(h, rah, ram, ras, decsign, decd, decm, decs, xc, yc);
    printf("three\n");
    fprintf(stderr," extracted image %5d x %5d\n",x2-x1+1,y2-y1+1);
    printf("four\n");
    free(filename);
    printf("five\n");
     */
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create qread.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* qread.c    Routines to read and write fixed-length record files as
 *            binary stream files.
 *
 * For VMS machines use qread_vms.c instead of these routines.  (The
 * standard VMS stream-I/O routines do not read the CD-ROM correctly in
 * some cases.)
 *
 * Summary of procedures:
 *
 * Opening and closing files:
 *
 * QFILE *qcreat(filename,recordsize,crrat)  Creates file filename.
 * QFILE *qopen( filename,recordsize,crrat)  Opens file filename.
 * void  qclose(qfile)                       Closes qfile.
 *
 * Stream I/O:
 *
 * void qread(qfile, buffer, n)              Reads next n bytes from qfile.
 * void qwrite(qfile, buffer, n)             Writes n bytes to qfile.
 * void fillbuff(qfile)                      Fills record buffer from qfile.
 * void dumpbuff(qfile)                      Dumps record buffer to qfile.
 * int  readint(qfile)                       Read a 4-byte integer from qfile.
 * void writeint(qfile,a)                    Write a 4-byte integer to qfile.
 *
 * All routines abort with an error message if there are problems.
 *
 * Programmer: R. White     Date: 16 June 1992
 */
#include <stdio.h>
#include <stdlib.h>
#include "qfile.h"

/*
 * ---------------- error exit ----------------
 */
static void
qerror(operation,qfile)
char  *operation;
QFILE *qfile;
{
    fprintf(stderr, "Error: %s failed for file %s\n",
        operation, qfile->filename);
    perror(operation);
    exit(-1);
}

/*
 * --------------- create a fixed-record-length file ---------------
 */
extern QFILE
*qcreat(filename,recordsize,crrat)
char *filename;
int  recordsize;        /* Record length in bytes                      */
int  crrat;             /* Carriage return attributes flag: 0 = no CRs */
{
QFILE *qfile;

    /*
     * Allocate memory for file access block and copy filename
     */
    qfile = (QFILE *) malloc(sizeof(QFILE));
    qfile->filename = filename;
    /*
     * Create the file
     */
    if ((qfile->file = fopen(filename,"w")) == NULL) {
            qerror("Create", qfile);
    }
    /*
     * File attributes
     */
    qfile->recordsize = recordsize;
    qfile->crrat = (crrat != 0);
    qfile->write = 1;
    /*
     * Create buffer, initialize pointer
     */
    qfile->bufsize = recordsize + qfile->crrat;
    qfile->buffer = (unsigned char *) malloc(qfile->bufsize);
    qfile->bptr = 0;
    /*
     * Return pointer to structure
     */
    return(qfile);
}

/*
 * --------------- open a fixed-record-length file ---------------
 */
extern QFILE
*qopen(filename,recordsize,crrat)
char *filename;
int  recordsize;
int  crrat;
{
QFILE *qfile;

    /*
     * Allocate memory for file access block and copy filename
     */
    qfile = (QFILE *) malloc(sizeof(QFILE));
    qfile->filename = filename;
    /*
     * Open the file
     */
    if ((qfile->file = fopen(filename,"r")) == NULL) {
            qerror("Open", qfile);
    }
    /*
     * File attributes
     */
    qfile->recordsize = recordsize;
    qfile->crrat = (crrat != 0);
    qfile->write = 0;
    /*
     * Create buffer, initialize pointer
     */
    qfile->bufsize = recordsize + qfile->crrat;
    qfile->buffer = (unsigned char *) malloc(qfile->bufsize);
    qfile->bptr = qfile->bufsize;
    /*
     * Return pointer to structure
     */
    return(qfile);
}

/*
 * --------------- Close file ---------------
 */
extern void
qclose(qfile)
QFILE *qfile;
{
    /*
     * If we're writing to file, dump partially full buffer
     */
    if (qfile->write) dumpbuff(qfile);
    /*
     * Close the file
     */
    fclose(qfile->file);
    /*
     * Free buffer and qfile structure
     */
    free(qfile->buffer);
    free(qfile);
    return;
}

/*
 * --------------- Fill buffer from next record ---------------
 */
extern void
fillbuff(qfile)
QFILE *qfile;
{
    qfile->bufsize = fread(qfile->buffer, 1, qfile->recordsize+qfile->crrat,
        qfile->file);
    qfile->bptr = 0;
    if (qfile->bufsize <= 0) {
        /*
         * End of file or error
         */
        qerror("Read", qfile);
    }
    /*
     * Remove the newline character if crrat is set
     */
    if (qfile->crrat) {
        if (qfile->buffer[qfile->bufsize-1] == '\n') {
            qfile->bufsize -= 1;
        } else {
            fprintf(stderr, "Error: no newline at end of record from %s\n",
                qfile->filename);
            fprintf(stderr, "Record='%.*s'\n",
                qfile->bufsize, qfile->buffer);
            exit(-1);
        }
    }
}

/*
 * --------------- Dump (full or partial) buffer to file ---------------
 */
extern void
dumpbuff(qfile)
QFILE *qfile;
{
    if (qfile->bptr > 0) {
        /*
         * Add newline if crrat is set
         */
        if (qfile->crrat) qfile->buffer[qfile->bptr++] = '\n';
        if (fwrite(qfile->buffer, 1, qfile->bptr, qfile->file) == 0) {
            qerror("Write",qfile);
        }
        qfile->bptr = 0;
    }
}

/*
 * --------------- Buffered input: Get next n bytes from file ---------------
 */
extern void
qread(qfile, buffer, n)
QFILE *qfile;
char  *buffer;
int   n;
{
int i;

    for (i=0; i<n; i++) {
        /*
         * Fill buffer if it is empty
         */
        if (qfile->bptr >= qfile->bufsize) fillbuff(qfile);
        buffer[i] = qfile->buffer[qfile->bptr++];
    }
}

/*
 * --------------- Buffered output: Put next n bytes to file ---------------
 */
extern void
qwrite(qfile, buffer, n)
QFILE *qfile;
char  *buffer;
int   n;
{
int i;

    for (i=0; i<n; i++) {
        /*
         * Dump buffer if it is full
         */
        if (qfile->bptr >= qfile->recordsize) dumpbuff(qfile);
        qfile->buffer[qfile->bptr++] = buffer[i];
    }
}

extern int
readint(infile)
QFILE *infile;
{
int a,i;
unsigned char b[4];

    /* Read integer A one byte at a time from infile.
     *
     * This is portable from Vax to Sun since it eliminates the
     * need for byte-swapping.
     */
    for (i=0; i<4; i++) b[i] = qgetc(infile);
    a = b[0];
    for (i=1; i<4; i++) a = (a<<8) + b[i];
    return(a);
}

extern void
writeint(outfile,a)
QFILE *outfile;
int a;
{
int i;
unsigned char b[4];

    /* Write integer A one byte at a time to outfile.
     *
     * This is portable from Vax to Sun since it eliminates the
     * need for byte-swapping.
     */
    for (i=3; i>=0; i--) {
        b[i] = a & 0xff;
        a >>= 8;
    }
    for (i=0; i<4; i++) qputc(b[i],outfile);
    return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create xypos.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       XYPOS
 *
 * PURPOSE:
 *       Routine to compute x,y from RA and Dec.
 *
 * CALLING SEQUENCE:
 *       XYPOS(RA,DEC,MAG,COL,X,Y)
 *
 * INPUTS:
 *       RA     - REAL*8 Right Ascension (radians) (J2000 coordinates)
 *       DEC    - REAL*8 Declination
 *       MAG    - Magnitude
 *       COL    - Colour
 *
 * OUTPUTS:
 *       X,Y - plate position  (pixels)
 *
 * PROCEDURE:
 *       Routine takes given position and uses the CALOBCC solution if
 *       available, otherwise it reverts to the DIG25CC orientation soln.
 *
 * MODIFICATION HISTORY
 *       Converted from IDL routine to Fortran, R. White, 7 May 1990
 *       Converted from Fortran to C, R. White, 30 July 1991
 */ 
#include <math.h>
#include "header.h"

extern void amdinv();
extern void ppoinv();

extern void
xypos(header,ra,dec,mag,col,x,y)
Header *header;
double ra,dec;
float mag,col;
float *x,*y;
{
    if (header->amd_flag == 'T') {
        amdinv(header,ra,dec,mag,col,x,y);
    } else {
        ppoinv(header,ra,dec,x,y);
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create readhdr.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       READHDR
 *
 * PURPOSE:
 *       Routine to read FITS header into structure
 *
 * CALLING SEQUENCE:
 *       readhdr(infile, h)
 *
 * INPUTS:
 *       infile     (* QFILE) header file pointer
 *
 * OUTPUTS:
 *       h          (* Header) header structure
 *
 * MODIFICATION HISTORY
 *       Converted from IDL routine to Fortran, R. White, 4 May 1990
 *       Converted from Fortran to C, R. White, 30 July 1991
 *       Modified to handle VMS fixed-length records, R. White, 16 June 1992
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "qfile.h"
#include "header.h"

/*
 * simple macros for keyword testing
 */
#define ival(skey,v)  if (strcmp(keyword,skey)==0) { v=atoi(value); continue; }
#define fval(skey,v)  if (strcmp(keyword,skey)==0) { v=atof(value); continue; }
#define cvalq(skey,v) if (strcmp(keyword,skey)==0) {                           \
                      strncpy(v,remquotes(value),sizeof(v));                   \
                      continue; }
#define cvalt(skey,v) if (strcmp(keyword,skey)==0) {                           \
                      strncpy(v,strtrim(remquotes(value)),sizeof(v));          \
                      continue; }

/*
 * external functions and procedures
 */
extern double dtotal();
extern char *remquotes(), *strtrim();
extern void condmsrd();

extern void
readhdr(infile,h)
QFILE *infile;
Header *h;
{
int i, linelen=80;
char keyword[9], value[21], line[80];

    /*
     * initial plate solution coefficients to zero
     */
    for (i = 0; i<6; i++) {
        h->ppo_coeff[i] = 0.0;
    }
    for (i = 0; i<20; i++) {
        h->amd_x_coeff[i] = 0.0;
        h->amd_y_coeff[i] = 0.0;
    }
    h->nhlines = 0;
    while (1) {
        qread(infile, line, linelen);
        /*
         * check for SIMPLE on first line, BITPIX on second line
         */
        if (h->nhlines == 0 && strncmp(line, "SIMPLE", 6) != 0) {
            fprintf(stderr, "Bad FITS header file %s\n", infile->filename);
            fprintf(stderr, "First keyword should be SIMPLE, not\n");
            fprintf(stderr, "%.80s\n", line);
            exit(-1);
        } else if (h->nhlines == 1 && strncmp(line, "BITPIX", 6) != 0) {
            fprintf(stderr, "Bad FITS header file %s\n", infile->filename);
            fprintf(stderr, "Second keyword should be BITPIX, not\n");
            fprintf(stderr, "%.80s\n", line);
            exit(-1);
        }
        strncpy(h->hlines[h->nhlines],line,80);
        h->hlines[h->nhlines++][80] = '\0';
        /*
         *  Extract keyword name, value and put in null-terminated string
         */
        strncpy(keyword, &line[0], 8);
        keyword[8] = '\0';
        strncpy(value, &line[10], 20);
        value[20] = '\0';
        /*
         *  Get standard keywords
         */
        ival("NAXIS1  ", h->section_x_length);
        ival("NAXIS2  ", h->section_y_length);
        fval("CRPIX1  ", h->section_x_corner);
        fval("CRPIX2  ", h->section_y_corner);
        fval("CDELT1  ", h->scaling);
        cvalq("OBJECT  ",h->object_name);
        /*
         *  Get GSSS Specific keywords
         */
        fval("MAGLIM  ", h->mag_limit);
        ival("PLTGRADE", h->plt_grade);
        cvalt("PLATEID ",h->plate_id);
        cvalt("REGION  ",h->region_no);
        cvalt("PLTLABEL",h->plate_label);
        cvalt("EXPOSURE",h->exposure_time);
        cvalt("SEEING  ",h->seeing);
        cvalt("ORIGIN  ",h->origin);
        /*
         *  Get GSSS calibration keywords
         */
        fval("EPOCH   ", h->plt_date);
        ival("BANDPASS", h->emulsion_code);
        fval("PLTSCALE", h->plt_scale);
        fval("XPIXELSZ", h->x_pixel_size);
        fval("YPIXELSZ", h->y_pixel_size);
        ival("PLTRAH  ", h->ra_h);
        ival("PLTRAM  ", h->ra_m);
        fval("PLTRAS  ", h->ra_s);
        if (strcmp(keyword,"PLTDECSN")==0) {
            if(strchr(value,'-') == NULL) {
                h->dec_sign = '+';
            } else {
                h->dec_sign = '-';
            }
        }
        ival("PLTDECD ", h->dec_d);
        ival("PLTDECM ", h->dec_m);
        fval("PLTDECS ", h->dec_s);
        fval("PLTVEC1 ", h->plt_center_vec[0]);
        fval("PLTVEC2 ", h->plt_center_vec[1]);
        fval("PLTVEC3 ", h->plt_center_vec[2]);
        fval("PPO1    ", h->ppo_coeff[0]);
        fval("PPO2    ", h->ppo_coeff[1]);
        fval("PPO3    ", h->ppo_coeff[2]);
        fval("PPO4    ", h->ppo_coeff[3]);
        fval("PPO5    ", h->ppo_coeff[4]);
        fval("PPO6    ", h->ppo_coeff[5]);
        fval("AMDX1   ", h->amd_x_coeff[ 0]);
        fval("AMDX2   ", h->amd_x_coeff[ 1]);
        fval("AMDX3   ", h->amd_x_coeff[ 2]);
        fval("AMDX4   ", h->amd_x_coeff[ 3]);
        fval("AMDX5   ", h->amd_x_coeff[ 4]);
        fval("AMDX6   ", h->amd_x_coeff[ 5]);
        fval("AMDX7   ", h->amd_x_coeff[ 6]);
        fval("AMDX8   ", h->amd_x_coeff[ 7]);
        fval("AMDX9   ", h->amd_x_coeff[ 8]);
        fval("AMDX10  ", h->amd_x_coeff[ 9]);
        fval("AMDX11  ", h->amd_x_coeff[10]);
        fval("AMDX12  ", h->amd_x_coeff[11]);
        fval("AMDX13  ", h->amd_x_coeff[12]);
        fval("AMDX14  ", h->amd_x_coeff[13]);
        fval("AMDX15  ", h->amd_x_coeff[14]);
        fval("AMDX16  ", h->amd_x_coeff[15]);
        fval("AMDX17  ", h->amd_x_coeff[16]);
        fval("AMDX18  ", h->amd_x_coeff[17]);
        fval("AMDX19  ", h->amd_x_coeff[18]);
        fval("AMDX20  ", h->amd_x_coeff[19]);
        fval("AMDY1   ", h->amd_y_coeff[ 0]);
        fval("AMDY2   ", h->amd_y_coeff[ 1]);
        fval("AMDY3   ", h->amd_y_coeff[ 2]);
        fval("AMDY4   ", h->amd_y_coeff[ 3]);
        fval("AMDY5   ", h->amd_y_coeff[ 4]);
        fval("AMDY6   ", h->amd_y_coeff[ 5]);
        fval("AMDY7   ", h->amd_y_coeff[ 6]);
        fval("AMDY8   ", h->amd_y_coeff[ 7]);
        fval("AMDY9   ", h->amd_y_coeff[ 8]);
        fval("AMDY10  ", h->amd_y_coeff[ 9]);
        fval("AMDY11  ", h->amd_y_coeff[10]);
        fval("AMDY12  ", h->amd_y_coeff[11]);
        fval("AMDY13  ", h->amd_y_coeff[12]);
        fval("AMDY14  ", h->amd_y_coeff[13]);
        fval("AMDY15  ", h->amd_y_coeff[14]);
        fval("AMDY16  ", h->amd_y_coeff[15]);
        fval("AMDY17  ", h->amd_y_coeff[16]);
        fval("AMDY18  ", h->amd_y_coeff[17]);
        fval("AMDY19  ", h->amd_y_coeff[18]);
        fval("AMDY20  ", h->amd_y_coeff[19]);
        if (strcmp(keyword,"END     ")==0) break;
    }
    /*
     *  Decide if GSSS scan or special plate
     */
    if (strncmp(h->plate_id,"    ",4) == 0) {
        h->special_plate_flag = 'T';
    } else {
        h->special_plate_flag = 'F';
    }
    /*
     *  Fudge pixel size for early scan data
     *  (I'd be surprised if this works when roundoff error is included)
     */
    if ((h->special_plate_flag == 'F') && (h->x_pixel_size == 25.284)) {
         h->x_pixel_size = 25.28445;
         h->y_pixel_size = 25.28445;
    }
    /*
     *  See if AMD calibration exists
     */
    if (dtotal(h->amd_x_coeff,20) == 0.0) {
        h->amd_flag = 'F';
    } else {
        h->amd_flag = 'T';
    }
    /*
     *  See if PPO calibration exists
     */
    if (dtotal(h->ppo_coeff,6) == 0.0) {
        h->ppo_flag = 'F';
    } else {
        h->ppo_flag = 'T';
    }
    /*
     *  See if plate data exists
     */
    if (strncmp(h->plate_id,"    ",4) != 0) {
        h->plate_data_flag = 'T';
    } else {
        h->plate_data_flag = 'F';
    }
    /*
     *  Convert plate center position to radians
     */
    condmsrd(h->ra_h, h->ra_m, h->ra_s,
        h->dec_sign, h->dec_d, h->dec_m, h->dec_s,
        &h->plt_center_ra, &h->plt_center_dec);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create utility.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* utility.c   general purpose routines for gasp stuff
 *
 * char *remquotes(string)  strips quotes (') off string
 * char *strtrim(string)    strips leading blanks off string
 * double *dtotal(a,n)      sums double precision array a(n)
 */
#include <string.h>

extern char
*remquotes(string)
char string[];
{
char *i1, *i2;

    i1 = strchr(string,'\'');
    if (i1 == NULL) return(string);
    i2 = strrchr(string,'\'');
    if (i2 == i1) return(string);
    i1 += 1;
    *i2 = '\0';
    return(i1);
}

#define BLANKS  " "

extern char
*strtrim(string)
char string[];
{
int c;

      c = strspn(string, BLANKS);
      return(&(string[c]));
}

extern double
dtotal(a,n)
double a[];
int n;
{
int i, d;

      d = 0.0;
      for (i=0; i<n; i++) d += a[i];
      return(d);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create transteq.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       TRANSTDEQ
 * PURPOSE:
 *       Routine to convert standard coordinates on a plate to RA and Dec
 *       in radians.
 * CALLING SEQUENCE:
 *       TRANSTDEQ(H,XI,ETA,&RA,&DEC)
 * INPUTS:
 *       H       -  (* Header) header structure
 *       XI,ETA  -  Standard Coords (arcsec)
 * OUTPUTS:
 *       RA,DEC  -  (* double) Equatorial Coords (radians)
 * MODIFICATION HISTORY
 *       Converted from IDL to Fortran, R. White, 7 Dec 1990
 *       Converted from Fortran to C, R. White, 31 July 1991
 */
#include <math.h>

#include "header.h"

#define ARCSEC_PER_RAD 206264.8062470964
#define PI 3.141592653589793238

extern void
transtdeq(h,xi,eta,ra,dec)
Header *h;
double xi, eta;
double *ra, *dec;
{
double object_xi,object_eta,numerator,denominator;

    /*
     *  Convert to radians
     */
    object_xi = xi/ARCSEC_PER_RAD;
    object_eta = eta/ARCSEC_PER_RAD;
    /*
     *  Convert to RA and Dec
     */
    numerator = object_xi/cos(h->plt_center_dec);
    denominator = 1-object_eta*tan(h->plt_center_dec);
    *ra = atan2(numerator,denominator)+h->plt_center_ra;
    if (*ra < 0.0) *ra = (*ra)+2*PI;

    numerator = cos((*ra) - h->plt_center_ra);
    denominator = (1-object_eta*tan(h->plt_center_dec))/
                (object_eta+tan(h->plt_center_dec));
    *dec = atan(numerator/denominator);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create hsmooth.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* hsmooth.c    Smooth H-transform image by adjusting coefficients toward
 *              interpolated values
 *
 * Programmer: R. White     Date: 13 April 1992
 */
#include <stdio.h>
#include <math.h>

#define min(a,b) (((a)<(b)) ? (a) : (b))
#define max(a,b) (((a)>(b)) ? (a) : (b))

extern void
hsmooth(a,nxtop,nytop,ny,scale)
int a[];            /* array of H-transform coefficients        */
int nxtop,nytop;    /* size of coefficient block to use         */
int ny;             /* actual 1st dimension of array            */
int scale;          /* truncation scale factor that was used    */
{
int i, j;
int ny2, s10, s00, diff, dmax, dmin, s, smax;
int hm, h0, hp, hmm, hpm, hmp, hpp, hx2, hy2;
int m1,m2;

    /*
     * Maximum change in coefficients is determined by scale factor.
     * Since we rounded during division (see digitize.c), the biggest
     * permitted change is scale/2.
     */
    smax = (scale >> 1);
    if (smax <= 0) return;
    ny2 = ny << 1;
    /*
     * We're indexing a as a 2-D array with dimensions (nxtop,ny) of which
     * only (nxtop,nytop) are used.  The coefficients on the edge of the
     * array are not adjusted (which is why the loops below start at 2
     * instead of 0 and end at nxtop-2 instead of nxtop.)
     */
    /*
     * Adjust x difference hx
     */
    for (i = 2; i<nxtop-2; i += 2) {
        s00 = ny*i;             /* s00 is index of a[i,j]   */
        s10 = s00+ny;           /* s10 is index of a[i+1,j] */
        for (j = 0; j<nytop; j += 2) {
            /*
             * hp is h0 (mean value) in next x zone, hm is h0 in previous x zone
             */
            hm = a[s00-ny2];
            h0 = a[s00];
            hp = a[s00+ny2];
            /*
             * diff = 8 * hx slope that would match h0 in neighboring zones
             */
            diff = hp-hm;
            /*
             * monotonicity constraints on diff
             */
            dmax = max( min( (hp-h0), (h0-hm) ), 0 ) << 2;
            dmin = min( max( (hp-h0), (h0-hm) ), 0 ) << 2;
            /*
             * if monotonicity would set slope = 0 then don't change hx.
             * note dmax>=0, dmin<=0.
             */
            if (dmin < dmax) {
                diff = max( min(diff, dmax), dmin);
                /*
                 * Compute change in slope limited to range +/- smax.
                 * Careful with rounding negative numbers when using
                 * shift for divide by 8.
                 */
                s = diff-(a[s10]<<3);
                s = (s>=0) ? (s>>3) : ((s+7)>>3) ;
                s = max( min(s, smax), -smax);
                a[s10] = a[s10]+s;
            }
            s00 += 2;
            s10 += 2;
        }
    }
    /*
     * Adjust y difference hy
     */
    for (i = 0; i<nxtop; i += 2) {
        s00 = ny*i+2;
        s10 = s00+ny;
        for (j = 2; j<nytop-2; j += 2) {
            hm = a[s00-2];
            h0 = a[s00];
            hp = a[s00+2];
            diff = hp-hm;
            dmax = max( min( (hp-h0), (h0-hm) ), 0 ) << 2;
            dmin = min( max( (hp-h0), (h0-hm) ), 0 ) << 2;
            if (dmin < dmax) {
                diff = max( min(diff, dmax), dmin);
                s = diff-(a[s00+1]<<3);
                s = (s>=0) ? (s>>3) : ((s+7)>>3) ;
                s = max( min(s, smax), -smax);
                a[s00+1] = a[s00+1]+s;
            }
            s00 += 2;
            s10 += 2;
        }
    }
    /*
     * Adjust curvature difference hc
     */
    for (i = 2; i<nxtop-2; i += 2) {
        s00 = ny*i+2;
        s10 = s00+ny;
        for (j = 2; j<nytop-2; j += 2) {
            /*
             * ------------------    y
             * | hmp |    | hpp |    |
             * ------------------    |
             * |     | h0 |     |    |
             * ------------------    -------x
             * | hmm |    | hpm |
             * ------------------
             */
            hmm = a[s00-ny2-2];
            hpm = a[s00+ny2-2];
            hmp = a[s00-ny2+2];
            hpp = a[s00+ny2+2];
            h0  = a[s00];
            /*
             * diff = 64 * hc value that would match h0 in neighboring zones
             */
            diff = hpp + hmm - hmp - hpm;
            /*
             * 2 times x,y slopes in this zone
             */
            hx2 = a[s10  ]<<1;
            hy2 = a[s00+1]<<1;
            /*
             * monotonicity constraints on diff
             */
            m1 = min(max(hpp-h0,0)-hx2-hy2, max(h0-hpm,0)+hx2-hy2);
            m2 = min(max(h0-hmp,0)-hx2+hy2, max(hmm-h0,0)+hx2+hy2);
            dmax = min(m1,m2) << 4;
            m1 = max(min(hpp-h0,0)-hx2-hy2, min(h0-hpm,0)+hx2-hy2);
            m2 = max(min(h0-hmp,0)-hx2+hy2, min(hmm-h0,0)+hx2+hy2);
            dmin = max(m1,m2) << 4;
            /*
             * if monotonicity would set slope = 0 then don't change hc.
             * note dmax>=0, dmin<=0.
             */
            if (dmin < dmax) {
                diff = max( min(diff, dmax), dmin);
                /*
                 * Compute change in slope limited to range +/- smax.
                 * Careful with rounding negative numbers when using
                 * shift for divide by 64.
                 */
                s = diff-(a[s10+1]<<6);
                s = (s>=0) ? (s>>6) : ((s+63)>>6) ;
                s = max( min(s, smax), -smax);
                a[s10+1] = a[s10+1]+s;
            }
            s00 += 2;
            s10 += 2;
        }
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create dodecode.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* dodecode.c   Decode stream of characters on infile and return array
 *
 * This version encodes the different quadrants separately
 *
 * Programmer: R. White     Date: 9 May 1991
 *
 * Modified to read VMS fixed-length files, R. White, 16 June 1992
 */

#include <stdio.h>
#include "qfile.h"

#define input_nybble(infile)    input_nbits(infile,4)

extern void qtree_decode();
extern void start_inputing_bits();
extern int input_bit();
extern int input_nbits();

extern void
dodecode(infile,a,nx,ny,nbitplanes)
QFILE *infile;
int a[];                            /* Array of values to decode            */
int nx,ny;                          /* Array dimensions are [nx][ny]        */
unsigned char nbitplanes[3];        /* Number of bit planes in quadrants    */
{
int i, nel, nx2, ny2;

    nel = nx*ny;
    nx2 = (nx+1)/2;
    ny2 = (ny+1)/2;
    /*
     * initialize a to zero
     */
    for (i=0; i<nel; i++) a[i] = 0;
    /*
     * Initialize bit input
     */
    start_inputing_bits();
    /*
     * read bit planes for each quadrant
     */
    qtree_decode(infile, &a[0],          ny, nx2,  ny2,  nbitplanes[0]);
    qtree_decode(infile, &a[ny2],        ny, nx2,  ny/2, nbitplanes[1]);
    qtree_decode(infile, &a[ny*nx2],     ny, nx/2, ny2,  nbitplanes[1]);
    qtree_decode(infile, &a[ny*nx2+ny2], ny, nx/2, ny/2, nbitplanes[2]);
    /*
     * make sure there is an EOF symbol (nybble=0) at end
     */
    if (input_nybble(infile) != 0) {
        fprintf(stderr, "dodecode: bad bit plane values\n");
        exit(-1);
    }
    /*
     * now get the sign bits
     * Re-initialize bit input
     */
    start_inputing_bits();
    for (i=0; i<nel; i++) {
        if (a[i] != 0) {
            if (input_bit(infile) != 0) a[i] = -a[i];
        }
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create qtreedec.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* qtree_decode.c   Read stream of codes from infile and construct bit planes
 *                  in quadrant of 2-D array using binary quadtree coding
 *
 * Programmer: R. White     Date: 7 May 1991
 *
 * Modified to read VMS fixed-length files, R. White, 16 June 1992
 */

#include <stdio.h>
#include <math.h>
#include "qfile.h"

#define input_nybble(infile)    input_nbits(infile,4)

extern int  input_nbits();
extern int  input_bit();

static void qtree_expand();
static void qtree_bitins();
static void qtree_copy();
static void read_bdirect();
static int  input_huffman();

extern void
qtree_decode(infile,a,n,nqx,nqy,nbitplanes)
QFILE *infile;
int a[];                            /* a is 2-D array with dimensions (n,n) */
int n;                              /* length of full row in a              */
int nqx;                            /* partial length of row to decode      */
int nqy;                            /* partial length of column (<=n)       */
int nbitplanes;                     /* number of bitplanes to decode        */
{
int log2n, k, bit, b, nqmax;
int nx,ny,nfx,nfy,c;
int nqx2, nqy2;
unsigned char *scratch;

    /*
     * log2n is log2 of max(nqx,nqy) rounded up to next power of 2
     */
    nqmax = (nqx>nqy) ? nqx : nqy;
    log2n = log((float) nqmax)/log(2.0)+0.5;
    if (nqmax > (1<<log2n)) {
        log2n += 1;
    }
    /*
     * allocate scratch array for working space
     */
    nqx2=(nqx+1)/2;
    nqy2=(nqy+1)/2;
    scratch = (unsigned char *) malloc(nqx2*nqy2);
    if (scratch == (unsigned char *) NULL) {
        fprintf(stderr, "qtree_decode: insufficient memory\n");
        exit(-1);
    }
    /*
     * now decode each bit plane, starting at the top
     * A is assumed to be initialized to zero
     */
    for (bit = nbitplanes-1; bit >= 0; bit--) {
        /*
         * Was bitplane was quadtree-coded or written directly?
         */
        b = input_nybble(infile);
        if(b == 0) {
            /*
             * bit map was written directly
             */
            read_bdirect(infile,a,n,nqx,nqy,scratch,bit);
        } else if (b != 0xf) {
            fprintf(stderr, "qtree_decode: bad format code %x\n",b);
            exit(-1);
        } else {
            /*
             * bitmap was quadtree-coded, do log2n expansions
             *
             * read first code
             */
            scratch[0] = input_huffman(infile);
            /*
             * now do log2n expansions, reading codes from file as necessary
             */
            nx = 1;
            ny = 1;
            nfx = nqx;
            nfy = nqy;
            c = 1<<log2n;
            for (k = 1; k<log2n; k++) {
                /*
                 * this somewhat cryptic code generates the sequence
                 * n[k-1] = (n[k]+1)/2 where n[log2n]=nqx or nqy
                 */
                c = c>>1;
                nx = nx<<1;
                ny = ny<<1;
                if (nfx <= c) { nx -= 1; } else { nfx -= c; }
                if (nfy <= c) { ny -= 1; } else { nfy -= c; }
                qtree_expand(infile,scratch,nx,ny,scratch);
            }
            /*
             * now copy last set of 4-bit codes to bitplane bit of array a
             */
            qtree_bitins(scratch,nqx,nqy,a,n,bit);
        }
    }
    free(scratch);
}


/*
 * do one quadtree expansion step on array a[(nqx+1)/2,(nqy+1)/2]
 * results put into b[nqx,nqy] (which may be the same as a)
 */
static void
qtree_expand(infile,a,nx,ny,b)
QFILE *infile;
unsigned char a[];
int nx;
int ny;
unsigned char b[];
{
int i;

    /*
     * first copy a to b, expanding each 4-bit value
     */
    qtree_copy(a,nx,ny,b,ny);
    /*
     * now read new 4-bit values into b for each non-zero element
     */
    for (i = nx*ny-1; i >= 0; i--) {
        if (b[i] != 0) b[i] = input_huffman(infile);
    }
}

/*
 * copy 4-bit values from a[(nx+1)/2,(ny+1)/2] to b[nx,ny], expanding
 * each value to 2x2 pixels
 * a,b may be same array
 */
static void
qtree_copy(a,nx,ny,b,n)
unsigned char a[];
int nx;
int ny;
unsigned char b[];
int n;      /* declared y dimension of b */
{
int i, j, k, nx2, ny2;
int s00, s10;

    /*
     * first copy 4-bit values to b
     * start at end in case a,b are same array
     */
    nx2 = (nx+1)/2;
    ny2 = (ny+1)/2;
    k = ny2*(nx2-1)+ny2-1;          /* k   is index of a[i,j]           */
    for (i = nx2-1; i >= 0; i--) {
        s00 = 2*(n*i+ny2-1);        /* s00 is index of b[2*i,2*j]       */
        for (j = ny2-1; j >= 0; j--) {
            b[s00] = a[k];
            k -= 1;
            s00 -= 2;
        }
    }
    /*
     * now expand each 2x2 block
     */
    for (i = 0; i<nx-1; i += 2) {
        s00 = n*i;                  /* s00 is index of b[i,j]   */
        s10 = s00+n;                /* s10 is index of b[i+1,j] */
        for (j = 0; j<ny-1; j += 2) {
            b[s10+1] =  b[s00]     & 1;
            b[s10  ] = (b[s00]>>1) & 1;
            b[s00+1] = (b[s00]>>2) & 1;
            b[s00  ] = (b[s00]>>3) & 1;
            s00 += 2;
            s10 += 2;
        }
        if (j < ny) {
            /*
             * row size is odd, do last element in row
             * s00+1, s10+1 are off edge
             */
            b[s10  ] = (b[s00]>>1) & 1;
            b[s00  ] = (b[s00]>>3) & 1;
        }
    }
    if (i < nx) {
        /*
         * column size is odd, do last row
         * s10, s10+1 are off edge
         */
        s00 = n*i;
        for (j = 0; j<ny-1; j += 2) {
            b[s00+1] = (b[s00]>>2) & 1;
            b[s00  ] = (b[s00]>>3) & 1;
            s00 += 2;
        }
        if (j < ny) {
            /*
             * both row and column size are odd, do corner element
             * s00+1, s10, s10+1 are off edge
             */
            b[s00  ] = (b[s00]>>3) & 1;
        }
    }
}

/*
 * Copy 4-bit values from a[(nx+1)/2,(ny+1)/2] to b[nx,ny], expanding
 * each value to 2x2 pixels and inserting into bitplane BIT of B.
 * A,B may NOT be same array (it wouldn't make sense to be inserting
 * bits into the same array anyway.)
 */
static void
qtree_bitins(a,nx,ny,b,n,bit)
unsigned char a[];
int nx;
int ny;
int b[];
int n;      /* declared y dimension of b */
int bit;
{
int i, j, k;
int s00, s10;

    /*
     * expand each 2x2 block
     */
    k = 0;                          /* k   is index of a[i/2,j/2]   */
    for (i = 0; i<nx-1; i += 2) {
        s00 = n*i;                  /* s00 is index of b[i,j]       */
        s10 = s00+n;                /* s10 is index of b[i+1,j]     */
        for (j = 0; j<ny-1; j += 2) {
            b[s10+1] |= ( a[k]     & 1) << bit;
            b[s10  ] |= ((a[k]>>1) & 1) << bit;
            b[s00+1] |= ((a[k]>>2) & 1) << bit;
            b[s00  ] |= ((a[k]>>3) & 1) << bit;
            s00 += 2;
            s10 += 2;
            k += 1;
        }
        if (j < ny) {
            /*
             * row size is odd, do last element in row
             * s00+1, s10+1 are off edge
             */
            b[s10  ] |= ((a[k]>>1) & 1) << bit;
            b[s00  ] |= ((a[k]>>3) & 1) << bit;
            k += 1;
        }
    }
    if (i < nx) {
        /*
         * column size is odd, do last row
         * s10, s10+1 are off edge
         */
        s00 = n*i;
        for (j = 0; j<ny-1; j += 2) {
            b[s00+1] |= ((a[k]>>2) & 1) << bit;
            b[s00  ] |= ((a[k]>>3) & 1) << bit;
            s00 += 2;
            k += 1;
        }
        if (j < ny) {
            /*
             * both row and column size are odd, do corner element
             * s00+1, s10, s10+1 are off edge
             */
            b[s00  ] |= ((a[k]>>3) & 1) << bit;
            k += 1;
        }
    }
}

static void
read_bdirect(infile,a,n,nqx,nqy,scratch,bit)
QFILE *infile;
int a[];
int n;
int nqx;
int nqy;
unsigned char scratch[];
int bit;
{
int i;

    /*
     * read bit image packed 4 pixels/nybble
     */
    for (i = 0; i < ((nqx+1)/2) * ((nqy+1)/2); i++) {
        scratch[i] = input_nybble(infile);
    }
    /*
     * insert in bitplane BIT of image A
     */
    qtree_bitins(scratch,nqx,nqy,a,n,bit);
}

/*
 * Huffman decoding for fixed codes
 *
 * Coded values range from 0-15
 *
 * Huffman code values (hex):
 *
 *  3e, 00, 01, 08, 02, 09, 1a, 1b,
 *  03, 1c, 0a, 1d, 0b, 1e, 3f, 0c
 *
 * and number of bits in each code:
 *
 *  6,  3,  3,  4,  3,  4,  5,  5,
 *  3,  5,  4,  5,  4,  5,  6,  4
 */
static int
input_huffman(infile)
QFILE *infile;
{
int c;

    /*
     * get first 3 bits to start
     */
    c = input_nbits(infile,3);
    if (c < 4) {
        /*
         * this is all we need
         * return 1,2,4,8 for c=0,1,2,3
         */
        return(1<<c);
    }
    /*
     * get the next bit
     */
    c = input_bit(infile) | (c<<1);
    if (c < 13) {
        /*
         * OK, 4 bits is enough
         */
        switch (c) {
            case  8 : return(3);
            case  9 : return(5);
            case 10 : return(10);
            case 11 : return(12);
            case 12 : return(15);
        }
    }
    /*
     * get yet another bit
     */
    c = input_bit(infile) | (c<<1);
    if (c < 31) {
        /*
         * OK, 5 bits is enough
         */
        switch (c) {
            case 26 : return(6);
            case 27 : return(7);
            case 28 : return(9);
            case 29 : return(11);
            case 30 : return(13);
        }
    }
    /*
     * need the 6th bit
     */
    c = input_bit(infile) | (c<<1);
    if (c == 62) {
        return(0);
    } else {
        return(14);
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create bitinput.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* BIT INPUT ROUTINES */

/*
 * Modified by R. White, 16 June 1992, to handle VMS fixed-length records
 */

#include <stdio.h>
#include "qfile.h"

/* THE BIT BUFFER */

static int buffer;                  /* Bits waiting to be input             */
static int bits_to_go;              /* Number of bits still in buffer       */


/* INITIALIZE BIT INPUT */

extern void
start_inputing_bits()
{
    /*
     * Buffer starts out with no bits in it
     */
    bits_to_go = 0;
}


/* INPUT A BIT */

extern int
input_bit(infile)
QFILE *infile;
{
    /* Read the next byte if no bits are left in buffer  */
    if (bits_to_go == 0) {
        buffer = qgetc(infile);
        if (buffer == EOF) {
            /*
             * end of file is an error for this application
             */
            fprintf(stderr, "input_bit: unexpected end-of-file\n");
            exit(-1);
        }
        bits_to_go = 8;
    }
    /*
     * Return the next bit
     */
    bits_to_go -= 1;
    return((buffer>>bits_to_go) & 1);
}


/* INPUT N BITS (N must be <= 8) */

extern int
input_nbits(infile,n)
QFILE *infile;
int n;
{
int c;

    if (bits_to_go < n) {
        /*
         * need another byte's worth of bits
         */
        buffer <<= 8;
        c = qgetc(infile);
        if (c == EOF) {
            /*
             * end of file is an error for this application
             */
            fprintf(stderr, "input_nbits: unexpected end-of-file\n");
            exit(-1);
        }
        buffer |= c;
        bits_to_go += 8;
    }
    /*
     * now pick off the first n bits
     */
    bits_to_go -= n;
    return( (buffer>>bits_to_go) & ((1<<n)-1) );
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create condmsrd.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       CONDMSRD
 * 
 * PURPOSE:
 *      This routine takes the Right Ascension and Declination and 
 *      converts from traditional units to radians.
 * 
 * CALLING SEQUENCE:
 *      CONDMSRD(RA_H,RA_M,RA_S,DEC_SIGN,DEC_D,DEC_M,DEC_S,
 *                OBJECT_RA,OBJECT_DEC)
 * 
 * INPUTS:
 *       RA_H,RA_M,RA_S              - Right Ascension hms
 *       DEC_SIGN,DEC_D,DEC_M,DEC_S  - Declination     dms
 * 
 * OUTPUTS:
 *       OBJECT_RA,OBJECT_DEC        - REAL*8 Radians
 * 
 * MODIFICATION HISTORY
 *       Converted from IDL by R. White, 4 May 1990
 */
#define ARCSEC_PER_RAD 206264.8062470964
#define SEC_PER_RAD (ARCSEC_PER_RAD/15.0)

extern void
condmsrd(ra_h, ra_m, ra_s, dec_sign, dec_d, dec_m, dec_s,
    object_ra, object_dec)
int ra_h, ra_m;
double ra_s;
char dec_sign;
int dec_d, dec_m;
double dec_s;
double *object_ra, *object_dec;
{
    /*
     *  Convert right ascension
     */
    *object_ra = (ra_h*3600.0+ra_m*60.0+ra_s)/SEC_PER_RAD;
    /*
     *  Convert declination
     */
    *object_dec = (dec_d*3600.0+dec_m*60.0+dec_s)/ARCSEC_PER_RAD;
    if (dec_sign == '-') *object_dec = -(*object_dec);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create fillhdr.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       FILLHDR
 *
 * PURPOSE:
 *       Change FITS header parameters to match the new extraction parameters
 *
 * CALLING SEQUENCE:
 *       fillhdr(h,object,nx,ny,x0,y0,image)
 *
 * INPUTS:
 *       h          (* Header) header structure
 *       object     (* char) name of object
 *       nx,ny      (int) size of image
 *       x0,y0      (int) corner of image on plate
 *       image      (short int) image data
 *
 * OUTPUTS:
 *       h          (* Header) modified header structure
 *
 * MODIFICATION HISTORY
 *       Created by R. White, 31 July 1991
 */
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <string.h>

#include "header.h"

extern void
fillhdr(h,object,nx,ny,x0,y0,image)
Header *h;
char *object;
int nx,ny,x0,y0;
short image[];
{
int i,j;
char keyword[9], hlast[81];
int itime;
char *cdate, date[18];
short datamin,datamax;
int no_naxis1, no_naxis2, no_crpix1, no_crpix2, no_datamax, no_datamin,
    no_object, no_date;


    /*
     * set size of section and corner position
     */
    h->section_x_length = nx;
    h->section_y_length = ny;
    h->section_x_corner = x0;
    h->section_y_corner = y0;
    /*
     * object name, padded with blanks
     */
    for (i=0; i<sizeof(h->object_name)-1; i++) h->object_name[i] = ' ';
    h->object_name[i] = '\0';
    for (i=0; (i<sizeof(h->object_name)-1) && (object[i] != '\0'); i++)
        h->object_name[i] = object[i];
    /*
     * get date & time
     * original format is 'wkd mmm dd hh:mm:ss yyyy\n\0'
     * change to 'mmm dd hh:mm yyyy\0' so it fits in <18 characters
     */
    itime = time(NULL);
    cdate = ctime(&itime);
    for (i=0; i<12; i++) date[i] = cdate[i+4];
    for (; i<17; i++) date[i] = cdate[i+7];
    date[i] = '\0';
    /*
     * get data max, min
     */
    datamin = image[0];
    datamax = image[0];
    for (i=1; i<nx*ny; i++) {
        if (image[i] > datamax) {
            datamax = image[i];
        } else if (image[i] < datamin) {
            datamin = image[i];
        }
    }
    /*
     * look for existing keywords and change them
     */
    no_naxis1 = 1;
    no_naxis2 = 1;
    no_crpix1 = 1;
    no_crpix2 = 1;
    no_datamax = 1;
    no_datamin = 1;
    no_object = 1;
    no_date = 1;
    for (i=0; i < h->nhlines; i++) {
        /*
         *  Extract keyword name and put in null-terminated string
         */
        strncpy(keyword, h->hlines[i], 8);
        keyword[8] = '\0';
        /* 
         *  Find keywords to change
         */
        if        (strcmp(keyword,"NAXIS1  ") == 0) {
            sprintf(&h->hlines[i][10],"%20d",h->section_x_length);
            h->hlines[i][30] = ' ';
            no_naxis1 = 0;
        } else if (strcmp(keyword,"NAXIS2  ") == 0) {
            sprintf(&h->hlines[i][10],"%20d",h->section_y_length);
            h->hlines[i][30] = ' ';
            no_naxis2 = 0;
        } else if (strcmp(keyword,"CRPIX1  ") == 0) {
            sprintf(&h->hlines[i][10],"%20.0f",h->section_x_corner);
            h->hlines[i][30] = ' ';
            no_crpix1 = 0;
        } else if (strcmp(keyword,"CRPIX2  ") == 0) {
            sprintf(&h->hlines[i][10],"%20.0f",h->section_y_corner);
            h->hlines[i][30] = ' ';
            no_crpix2 = 0;
        } else if (strcmp(keyword,"DATAMAX ") == 0) {
            sprintf(&h->hlines[i][10],"%20d",datamax);
            h->hlines[i][30] = ' ';
            no_datamax = 0;
        } else if (strcmp(keyword,"DATAMIN ") == 0) {
            sprintf(&h->hlines[i][10],"%20d",datamin);
            h->hlines[i][30] = ' ';
            no_datamin = 0;
        } else if (strcmp(keyword,"OBJECT  ") == 0) {
            sprintf(&h->hlines[i][10],"'%-18.18s'",h->object_name);
            h->hlines[i][30] = ' ';
            no_object = 0;
        } else if (strcmp(keyword,"DATE    ") == 0) {
            sprintf(&h->hlines[i][10],"'%-18.18s'",date);
            h->hlines[i][30] = ' ';
            no_date = 0;
        }
    }
    /*
     * if some of these keywords were absent, insert them just before
     *  the last line of the header
     */
    i = h->nhlines-1;
    for (j=0; j<sizeof(hlast); j++) hlast[j] = h->hlines[i][j];
    if (no_naxis1) {
        sprintf(h->hlines[i], "%-8.8s= %20d /%-50.50s",
            "NAXIS1", h->section_x_length, "Length X axis");
        i = i + 1;
    }
    if (no_naxis2) {
        sprintf(h->hlines[i], "%-8.8s= %20d /%-50.50s",
            "NAXIS2", h->section_y_length, "Length Y axis");
        i = i + 1;
    }
    if (no_crpix1) {
        sprintf(h->hlines[i], "%-8.8s= %20d /%-50.50s",
            "NCRPIX1", h->section_x_corner, "X corner");
        i = i + 1;
    }
    if (no_crpix2) {
        sprintf(h->hlines[i], "%-8.8s= %20d /%-50.50s",
            "NCRPIX2", h->section_y_corner, "Y corner");
        i = i + 1;
    }
    if (no_datamax) {
        sprintf(h->hlines[i], "%-8.8s= %20d /%-50.50s",
            "DATAMAX", datamax, "Maximum data value");
        i = i + 1;
    }
    if (no_datamin) {
        sprintf(h->hlines[i], "%-8.8s= %20d /%-50.50s",
            "DATAMIN", datamin, "Minimum data value");
        i = i + 1;
    }
    if (no_object) {
        sprintf(h->hlines[i], "%-8.8s= '%-18.18s' /%-50.50s",
            "OBJECT", h->object_name, "Object ID");
        i = i + 1;
    }
    if (no_date) {
        sprintf(h->hlines[i], "%-8.8s= '%-18.18s' /%-50.50s",
            "DATE", date, "Creation Date");
        i = i + 1;
    }
    /*
     * put last line back on the end
     */
    if (i >= h->nhlines) {
        for (j=0; j<sizeof(hlast); j++) h->hlines[i][j] = hlast[j];
        h->nhlines = i+1;
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create fillpos.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       FILLPOS
 *
 * PURPOSE:
 *       Add FITS header parameters for RA, Dec for this object
 *
 * CALLING SEQUENCE:
 *       fillhdr(h,rah,ram,ras,decsign,decd,decm,decs,x,y)
 *
 * INPUTS:
 *       h              (* Header) header structure
 *       rah,ram,ras    RA for object
 *       decsign,decd,  Dec for object
 *        decm,decs
 *       x,y            (float) position of object on plate in pixels
 *
 * OUTPUTS:
 *       h          (* Header) modified header structure
 *
 * MODIFICATION HISTORY
 *       Created by R. White, 31 July 1991
 */
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <string.h>

#include "header.h"

extern void
fillpos(h,rah,ram,ras,decsign,decd,decm,decs,x,y)
Header *h;
int rah,ram;
double ras;
char decsign;
int decd,decm;
double decs;
double x,y;
{
int i,j;
char keyword[9], hlast[81], rastr[13], decstr[13];
int no_ra, no_dec, no_x, no_y;

    /*
     * set position in header structure
     */
    h->ra_h = rah;
    h->ra_m = ram;
    h->ra_s = ras;
    h->dec_d = decd;
    h->dec_m = decm;
    h->dec_s = decs;
    h->dec_sign = decsign;
    h->x = x;
    h->y = y;
    /*
     * round for printing with ss.sss (RA), ss.ss (Dec)
     */
    if (ras+0.0005 >= 60.0) {
        ras = 0.0;
        ram += 1;
        if (ram == 60) {
            ram = 0;
            rah += 1;
            if (rah == 24) {
                rah = 0;
            }
        }
    }
    if (decs+0.005 >= 60.0) {
        decs = 0.0;
        decm += 1;
        if (decm == 60) {
            decm = 0;
            decd += 1;
        }
    }
    /*
     * strings for RA, Dec
     */
    sprintf(rastr, "%2.2d %2.2d %06.3f", rah, ram, ras);
    sprintf(decstr, "%c%2.2d %2.2d %05.2f", decsign, decd, decm, decs);
    /*
     * look for existing OBJCTRA, OBJCTDEC, OBJCTX, OBJCTY keywords and
     * change them
     */
    no_ra = 1;
    no_dec = 1;
    no_x = 1;
    no_y = 1;
    for (i=0; i < h->nhlines; i++) {
        /*
         *  Extract keyword name and put in null-terminated string
         */
        strncpy(keyword, h->hlines[i], 8);
        keyword[8] = '\0';
        /* 
         *  Find keywords to change
         */
        if        (strcmp(keyword,"OBJCTRA ") == 0) {
            sprintf(&h->hlines[i][10],"'%-18.18s'",rastr);
            h->hlines[i][30] = ' ';
            no_ra = 0;
        } else if (strcmp(keyword,"OBJCTDEC") == 0) {
            sprintf(&h->hlines[i][10],"'%-18.18s'",decstr);
            h->hlines[i][30] = ' ';
            no_dec = 0;
        } else if (strcmp(keyword,"OBJCTX  ") == 0) {
            sprintf(&h->hlines[i][10],"%20.2f",x);
            h->hlines[i][30] = ' ';
            no_x = 0;
        } else if (strcmp(keyword,"OBJCTY  ") == 0) {
            sprintf(&h->hlines[i][10],"%20.2f",y);
            h->hlines[i][30] = ' ';
            no_y = 0;
        }
    }
    /*
     * if some of these keywords were absent, insert them just before
     *  the last line of the header
     */
    i = h->nhlines-1;
    for (j=0; j<sizeof(hlast); j++) hlast[j] = h->hlines[i][j];
    if (no_ra) {
        sprintf(h->hlines[i], "%-8.8s= '%-18.18s' /%-50.50s",
            "OBJCTRA", rastr, "Object Right Ascension (J2000)");
        i = i+1;
    }
    if (no_dec) {
        sprintf(h->hlines[i], "%-8.8s= '%-18.18s' /%-50.50s",
            "OBJCTDEC", decstr, "Object Declination (J2000)");
        i = i+1;
    }
    if (no_x) {
        sprintf(h->hlines[i], "%-8.8s= %20.2f /%-50.50s",
            "OBJCTX", x, "Object X on plate (pixels)");
        i = i+1;
    }
    if (no_y) {
        sprintf(h->hlines[i], "%-8.8s= %20.2f /%-50.50s",
            "OBJCTY", y, "Object Y on plate (pixels)");
        i = i+1;
    }
    /*
     * put last line back on the end
     */
    if (i >= h->nhlines) {
        for (j=0; j<sizeof(hlast); j++) h->hlines[i][j] = hlast[j];
        h->nhlines = i+1;
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create conrddms.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*         CONRDDMS
 *
 *   PURPOSE:
 *         This routine takes the Right Ascension and Declination and 
 *         converts from radians to traditional units.
 *
 *   CALLING SEQUENCE:
 *         CONRDDMS(RA, DEC, &RA_H, &RA_M, &RA_S,
 *              &DEC_SIGN, &DEC_D, &DEC_M, &DEC_S)
 *
 *   INPUTS:
 *         RA   - Right Ascension (radians)
 *         DEC  - Declination     (radians)
 *
 *   OUTPUTS:
 *         RA_H,RA_M,RA_S             - Right Ascension hms
 *         DEC_SIGN,DEC_D,DEC_M,DEC_S - Declination dms
 *
 *   MODIFICATION HISTORY
 *         Converted from IDL to C, R. White, 31 July 1991
 */
#include <math.h>

#define ARCSEC_PER_RAD 206264.8062470964
#define SEC_PER_RAD (ARCSEC_PER_RAD/15.0)

extern void
conrddms(ra,dec,ra_h,ra_m,ra_s,dec_sign,dec_d,dec_m,dec_s)
double ra, dec;
int *ra_h, *ra_m;
float *ra_s;
char *dec_sign;
int *dec_d, *dec_m;
float *dec_s;
{
double object_ra, object_dec;

    /*
     * Convert right ascension
     * Assume 0 <= ra < 2*pi
     */
    object_ra = ra*SEC_PER_RAD;
    *ra_h = (int) (object_ra/3600);
    *ra_m = (int) ((object_ra- (*ra_h)*3600)/60);
    *ra_s = object_ra - ((*ra_h)*60 + (*ra_m))*60;
    /*
     * Convert declination
     */
    if (dec < 0) {
        *dec_sign = '-';
    } else {
        *dec_sign = '+';
    }
    object_dec = fabs(dec)*ARCSEC_PER_RAD;

    *dec_d = (int) (object_dec/3600);
    *dec_m = (int) ((object_dec - (*dec_d)*3600)/60);
    *dec_s = object_dec - ((*dec_d)*60 + (*dec_m))*60;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create hdcmprss.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* hdecompress.c Return image from compressed image file created by hcompress
 *
 * Programmer: R. White     Date: 20 May 1991
 *
 * Modified to allowing smoothing during decompression, R. White, 14 April 1992
 * Modified to read VMS fixed-length files, R. White, 16 June 1992
 */

#include <stdio.h>
#include "qfile.h"

/*
 * if verbose is set to non-zero value, some information about
 * compression efficiency is printed on stderr
 */
int verbose;

extern void decode();
extern void undigitize();
extern void hinv();

extern void
hdecompress(a,nx,ny,filename,smooth)
int  **a;                   /* (*a)[nx][ny] is the image array              */
int  *nx;
int  *ny;                   /* Note that ny is the fast-varying dimension   */
char *filename;             /* Name of input file                           */
int  smooth;                /* 0 for no smoothing, else smooth in hinv      */
{
QFILE *infile;
int scale;

    /*
     * open input file: 512 byte records, no carriage control
     */
    infile = qopen(filename,512,0);
    decode(infile,a,nx,ny,&scale);      /* Read from infile and decode      */
                                        /* Returns address & size           */
    undigitize(*a,*nx,*ny,scale);       /* Un-Digitize                      */
    hinv(*a,*nx,*ny,smooth,scale);      /* Inverse H-transform              */
    if (verbose) {
        fprintf(stderr, "Image size (%d,%d)  Scale factor %d\n",
            *ny,*nx,scale);
    }
    qclose(infile);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create position.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*         POSITION
 *   PURPOSE:
 *         Routine to compute RA and Dec from x,y.
 *   CALLING SEQUENCE:
 *         POSITION(H,X,Y,MAG,COL,&RA,&DEC)
 *   INPUTS:
 *         H   - (Header *) header structure
 *         X,Y - plate position  (pixels)
 *         MAG - Magnitude
 *         COL - Colour
 *   OUTPUTS:
 *         RA     - (double *) Right Ascension (radians)
 *         DEC    - (double *) Declination
 *   PROCEDURE:
 *         Routine takes given position and uses the CALOBCC solution if
 *         available otherwise it reverts to the DIG25CC orientation soln.
 *   MODIFICATION HISTORY
 *         Converted from IDL to C, R. White, 31 July 1991
 */

#include "header.h"

#define PI 3.141592653589793238
#define TWO_PI (2*PI)

extern void amdpos();
extern void ppopos();

extern void
position(h,x,y,mag,col,ra,dec)
Header *h;
double x,y,mag,col;
double *ra, *dec;
{
    /*
     * Compute ra,dec
     */
    if (h->amd_flag == 'T') {
        amdpos(h,x,y,mag,col,ra,dec);
    } else if (h->ppo_flag == 'T') {
        ppopos(h,x,y,ra,dec);
    } else {
        *ra = 0.0;
        *dec = 0.0;
    }
    /*
     * Check RA range
     */
    if (*ra >= TWO_PI) *ra = *ra - TWO_PI;
    if (*ra < 0.0)  *ra = *ra + TWO_PI;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ppoinv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       PPOINV
 * 
 * PURPOSE:
 *       Routine to convert RA,Dec to x,y using the PDS orientation 
 *       solution.
 * 
 * CALLING SEQUENCE:
 *       call PPOINV(RA,DEC,X,Y)
 *
 * INPUTS:
 *       RA        - REAL*8 Right Ascension (radians)
 *       DEC       - REAL*8 Declination
 *
 * OUTPUTS:
 *       X,Y  - position (pixels)
 *
 * PROCEDURE:
 *       Transforms the standard coords to pixels using the orientation
 *       solution.
 *
 * MODIFICATION HISTORY
 *       Converted from IDL routine to Fortran, R. White, 7 May 1990
 *       Converted from Fortran to C, R. White, 30 July 1991
 */
#include <math.h>
#include "header.h"

#define ARCSEC_PER_RADIAN 206264.8062470964

extern void traneqstd();

extern void
ppoinv(header,ra,dec,x,y)
Header *header;
double ra, dec;
float *x,*y;
{
double object_xi, object_eta, object_x, object_y;

    /*
     *  Convert RA and Dec to standard coords.
     */
    traneqstd(header,ra,dec,&object_xi,&object_eta);
    /*
     *  Convert st.coords from arcsec to radians
     */
    object_xi  = object_xi /ARCSEC_PER_RADIAN;
    object_eta = object_eta/ARCSEC_PER_RADIAN;
    /*
     *  Compute PDS coordinates from solution
     */
    object_x = header->ppo_coeff[0]*object_xi
        + header->ppo_coeff[1]*object_eta + header->ppo_coeff[2];
    object_y = header->ppo_coeff[3]*object_xi
        + header->ppo_coeff[4]*object_eta + header->ppo_coeff[5];
    /*
     * Convert x,y from microns to pixels
     */
    *x = object_x/header->x_pixel_size;
    *y = object_y/header->y_pixel_size;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create amdinv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*  AMDINV
 *
 * PURPOSE:
 *  Compute the x,y position on the plate from the RA,Dec using
 *  the inverse of the CALOBCC solution.
 *
 * CALLING SEQUENCE:
 *  AMDINV(HEADER,RA,DEC,MAG,COL,X,Y)
 *
 * INPUTS:
 *  HEADER   - Structure with header information
 *  RA,DEC   - REAL*8 Celestial Coords. (radians)
 *  MAG      - Magnitude
 *  COL      - Colour
 *
 * OUTPUTS:
 *  X,Y      - pixel position
 *
 * PROCEDURE:
 *  Newtons method is used to iterate from a starting position
 *  till convergence is reached.
 *
 * MODIFICATION HISTORY
 *  Converted from GASP IDL routine to Fortran, R. White, 7 May 1990
 *  Converted from Fortran to C, R. White, 30 July 1991
 */

#include <stdio.h>
#include <math.h>
#include "header.h"

extern void traneqstd();
extern void pltmodel();

extern void
amdinv(header,ra,dec,mag,col,x,y)
Header *header;
double ra, dec;
float mag, col;
float *x, *y;
{
int i, max_iterations;
float tolerance;
double xi, eta, object_x, object_y, delta_x, delta_y, f, fx, fy, g, gx, gy;

    /*
     *  Initialize
     */
    i = 0;
    max_iterations = 50;
    tolerance = 0.0000005;
    delta_x = tolerance;
    delta_y = tolerance;
    /*
     *  Convert RA and Dec to St.coords
     */
    traneqstd(header,ra,dec,&xi,&eta);
    /*
     *  Set initial value for x,y
     */
    object_x = xi/header->plt_scale;
    object_y = eta/header->plt_scale;
    /*
     *  Iterate by Newtons method
     */
    for(i = 0; i < max_iterations; i++) {
        pltmodel(header,object_x,object_y,mag,col,&f,&fx,&fy,&g,&gx,&gy);
        f = f-xi;
        g = g-eta;
        delta_x = (-f*gy+g*fy)/(fx*gy-fy*gx);
        delta_y = (-g*fx+f*gx)/(fx*gy-fy*gx);
        object_x = object_x+delta_x;
        object_y = object_y+delta_y;
        if ((fabs(delta_x) < tolerance) && (fabs(delta_y) < tolerance)) break;
    }
    /*
     *  Convert mm from plate center to pixels
     */
    *x = (header->ppo_coeff[2]-object_x*1000.0)/header->x_pixel_size;
    *y = (header->ppo_coeff[5]+object_y*1000.0)/header->y_pixel_size;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create traneqst.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       TRANEQSTD
 * PURPOSE:
 *       Routine to convert RA and Dec in radians to standard coordinates
 *       on a plate.
 * CALLING SEQUENCE:
 *       TRANEQSTD(OBJECT_RA,OBJECT_DEC,OBJECT_XI,OBJECT_ETA)
 * INPUTS:
 *       OBJECT_RA,OBJECT_DEC - REAL*8 Equatorial Coords (radians)
 * OUTPUTS:
 *       OBJECT_XI,OBJECT_ETA - REAL*8 Standard Coords  (arcsec)
 * MODIFICATION HISTORY
 *       Converted to Fortran from IDL routine, R. White, 7 May 1990
 *       Converted from Fortran to C, R. White, 30 July 1991
 */
#include <math.h>
#include "header.h"

#define ARCSEC_PER_RAD 206264.8062470964

extern void
traneqstd(header,object_ra,object_dec,object_xi,object_eta)
Header *header;
double object_ra, object_dec;
double *object_xi, *object_eta;
{
double div;

    /*
     *  Find divisor
     */
    div=(sin(object_dec)*sin(header->plt_center_dec)+
        cos(object_dec)*cos(header->plt_center_dec)*
        cos(object_ra - header->plt_center_ra));
    /* 
     *  Compute standard coords and convert to arcsec
     */
    *object_xi=cos(object_dec)*sin(object_ra - header->plt_center_ra)*
        ARCSEC_PER_RAD/div;

    *object_eta=(sin(object_dec)*cos(header->plt_center_dec)-
        cos(object_dec)*sin(header->plt_center_dec)*
        cos(object_ra - header->plt_center_ra))*
        ARCSEC_PER_RAD/div;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create decode.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* decode.c     read codes from infile and construct array
 *
 * Programmer: R. White     Date: 9 May 1991
 *
 * Modified to read VMS fixed-length files, R. White, 16 June 1992
 */

#include <stdio.h>
#include "qfile.h"

unsigned char code_magic[2] = { 0xDD, 0x99 };

extern void dodecode();

extern void
decode(infile,a,nx,ny,scale)
QFILE *infile;                           /* input file (already opened)      */
int **a;                                /* address of output array [nx][ny] */
int *nx,*ny;                            /* size of output array             */
int *scale;                             /* scale factor for digitization    */
{
int nel, sumall;
unsigned char nbitplanes[3];
char tmagic[2];

    /*
     * read magic value in first two bytes
     */
    qread(infile, tmagic, sizeof(tmagic));
    if (memcmp(tmagic,code_magic,sizeof(code_magic)) != 0) {
        fprintf(stderr, "bad file format\n");
        exit(-1);
    }
    *nx =readint(infile);               /* x size of image                  */
    *ny =readint(infile);               /* y size of image                  */
    *scale=readint(infile);             /* scale factor for digitization    */
    /*
     * allocate memory for array
     */
    nel = (*nx) * (*ny);
    *a = (int *) malloc(nel*sizeof(int));
    if (*a == (int *) NULL) {
        fprintf(stderr, "decode: insufficient memory\n");
        exit(-1);
    }
    /* sum of all pixels    */
    sumall=readint(infile);
    /* # bits in quadrant   */
    qread(infile, (char *) nbitplanes, sizeof(nbitplanes));
    dodecode(infile, *a, *nx, *ny, nbitplanes);
    /*
     * put sum of all pixels back into pixel 0
     */
    (*a)[0] = sumall;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create undigitz.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* undigitize.c     undigitize H-transform
 *
 * Programmer: R. White     Date: 9 May 1991
 */
#include <stdio.h>

extern void
undigitize(a,nx,ny,scale)
int a[];
int nx,ny;
int scale;
{
int *p;

    /*
     * multiply by scale
     */
    if (scale <= 1) return;
    for (p=a; p <= &a[nx*ny-1]; p++) *p = (*p)*scale;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ppopos.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*          PPOPOS
 *
 *    PURPOSE:
 *          Routine to convert x,y to RA,Dec using the PDS orientation solution.
 *
 *    CALLING SEQUENCE:
 *          PPOPOS(H,X,Y,&RA,&DEC)
 *
 *    INPUTS:
 *          H    - (* Header) header structure
 *          X,Y  - position (pixels)
 *
 *    OUTPUTS:
 *          RA        - (* double) Right Ascension (radians)
 *          DEC       - (* double) Declination
 *
 *    PROCEDURE:
 *          Computes the standard coords using the inverse of the orientation
 *          solution which is then converted to celestial coords.
 *
 *    MODIFICATION HISTORY
 *          Converted from IDL to C, R. White, 31 July 1991
 */

#include "header.h"

#define ARCSEC_PER_RAD 206264.8062470964

extern void transtdeq();

extern void
ppopos(h,x,y,ra,dec)
Header *h;
double x,y;
double *ra, *dec;
{
double a,b,c,d,e,f;
double object_x, object_y, object_xi, object_eta;

    /*
     * Set up local variables
     */
    a = h->ppo_coeff[0];
    b = h->ppo_coeff[1];
    c = h->ppo_coeff[2];
    d = h->ppo_coeff[3];
    e = h->ppo_coeff[4];
    f = h->ppo_coeff[5];
    /*
     * Convert x,y from pixels to microns
     */
    object_x = x * h->x_pixel_size;
    object_y = y * h->y_pixel_size;
    /*
     * Compute standard coordinates from x,y and plate center
     */
    object_xi  = (e*(c-object_x)-b*(f-object_y))/(b*d-a*e);
    object_eta = (a*(f-object_y)-d*(c-object_x))/(b*d-a*e);
    /*
     * Convert st.coords from radians to arcsec
     */
    object_xi  = object_xi*ARCSEC_PER_RAD;
    object_eta = object_eta*ARCSEC_PER_RAD;
    /*
     * Convert to RA and Dec
     */
    transtdeq(h,object_xi,object_eta,ra,dec);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create amdpos.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       AMDPOS
 * 
 * PURPOSE:
 *       Routine to convert x,y to RA,Dec using the 
 *       CALOBCC solution.
 * 
 * CALLING SEQUENCE:
 *       AMDPOS(H,X,Y,MAG,COLOUR,RA,DEC)
 * 
 * INPUTS:
 *       H         - (* Header) header structure
 *       X         - x position  (pixels)
 *       Y         - y position
 *       MAG       - magnitude
 *       COLOUR    - colour
 * 
 * OUTPUTS:
 *       RA        - (* double) Right ascension (radians)
 *       DEC       - (* double) Declination
 * 
 * MODIFICATION HISTORY
 *       Converted from IDL to Fortran, RLW, 21 Sept 1990
 *       Converted from Fortran to C, RLW, 31 July 1991
 */
#include "header.h"

#define ARCSEC_PER_RAD 206264.8062470964

extern void transtdeq();

extern void
amdpos(h,x,y,mag,colour,ra,dec)
Header *h;
double x,y,mag,colour;
double *ra, *dec;
{
double ox,oy,ox2,oy2,ox3,oy3,object_xi,object_eta;

    /*
     *  Convert x,y from pixels to mm measured from plate center
     */
    ox = (h->ppo_coeff[2]   - x*h->x_pixel_size)/1000.0;
    oy = (y*h->y_pixel_size - h->ppo_coeff[5]  )/1000.0;
    ox2 = ox*ox;
    oy2 = oy*oy;
    ox3 = ox*ox2;
    oy3 = oy*oy2;
    /*
     *  Compute standard coordinates from x,y and plate model
     */
    object_xi =
     h->amd_x_coeff[ 0]*ox                    + h->amd_x_coeff[ 1]*oy     +
     h->amd_x_coeff[ 2]                       + h->amd_x_coeff[ 3]*ox2    +
     h->amd_x_coeff[ 4]*ox*oy                 + h->amd_x_coeff[ 5]*oy2    +
     h->amd_x_coeff[ 6]*(ox2+oy2)             + h->amd_x_coeff[ 7]*ox3    +
     h->amd_x_coeff[ 8]*ox2*oy                + h->amd_x_coeff[ 9]*ox*oy2 +
     h->amd_x_coeff[10]*oy3                   +
     h->amd_x_coeff[11]*ox*(ox2+oy2)          +
     h->amd_x_coeff[12]*ox*(ox2+oy2)*(ox2*oy2)+
     h->amd_x_coeff[13]*mag                   + h->amd_x_coeff[14]*mag*mag+
     h->amd_x_coeff[15]*mag*mag*mag           + h->amd_x_coeff[16]*mag*ox +
     h->amd_x_coeff[17]*mag*(ox2+oy2)         +
     h->amd_x_coeff[18]*mag*ox*(ox2+oy2)      +
     h->amd_x_coeff[19]*colour;

    object_eta =
     h->amd_y_coeff[ 0]*oy                    + h->amd_y_coeff[ 1]*ox     +
     h->amd_y_coeff[ 2]                       + h->amd_y_coeff[ 3]*oy2    +
     h->amd_y_coeff[ 4]*oy*ox                 + h->amd_y_coeff[ 5]*ox2    +
     h->amd_y_coeff[ 6]*(oy2+ox2)             + h->amd_y_coeff[ 7]*oy3    +
     h->amd_y_coeff[ 8]*oy2*ox                + h->amd_y_coeff[ 9]*oy*ox2 +
     h->amd_y_coeff[10]*ox3                   +
     h->amd_y_coeff[11]*oy*(oy2+ox2)          +
     h->amd_y_coeff[12]*oy*(oy2+ox2)*(oy2*ox2)+
     h->amd_y_coeff[13]*mag                   + h->amd_y_coeff[14]*mag*mag+
     h->amd_y_coeff[15]*mag*mag*mag           + h->amd_y_coeff[16]*mag*oy +
     h->amd_y_coeff[17]*mag*(oy2+ox2)         +
     h->amd_y_coeff[18]*mag*oy*(oy2+ox2)      +
     h->amd_y_coeff[19]*colour;
    /*
     *  Convert to RA and Dec 
     *  Note that ra and dec are already pointers, so we don't need
     *  to pass by address
     */
    transtdeq(h,object_xi,object_eta,ra,dec);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create hinv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* hinv.c   Inverse H-transform of NX x NY integer image
 *
 * Programmer: R. White     Date: 10 April 1992
 */
#include <stdio.h>
#include <math.h>

static void unshuffle();
extern void hsmooth();

extern void
hinv(a,nx,ny,smooth,scale)
int a[];
int nx,ny;
int smooth;   /* 0 for no smoothing, else smooth during inversion */
int scale;    /* used if smoothing is specified */
{
int nmax, log2n, i, j, k;
int b11, b10, b01, b00;
int nxtop,nytop,nxf,nyf,c;
int oddx,oddy;
int shift,dp, dm;
int s10, s00;
int *tmp;

    /*
     * log2n is log2 of max(nx,ny) rounded up to next power of 2
     */
    nmax = (nx>ny) ? nx : ny;
    log2n = log((float) nmax)/log(2.0)+0.5;
    if ( nmax > (1<<log2n) ) {
        log2n += 1;
    }
    /*
     * get temporary storage for shuffling elements
     */
    tmp = (int *) malloc(((nmax+1)/2)*sizeof(int));
    if (tmp == (int *) NULL) {
        fprintf(stderr, "hinv: insufficient memory\n");
        exit(-1);
    }
    /*
     * do log2n expansions
     *
     * We're indexing a as a 2-D array with dimensions (nx,ny).
     * dp,dm are used for rounding positive, negative numbers during shift.
     */
    shift = 1;
    dp = 0;
    dm = 1;
    nxtop = 1;
    nytop = 1;
    nxf = nx;
    nyf = ny;
    c = 1<<log2n;
    for (k = log2n-1; k>=0; k--) {
        /*
         * this somewhat cryptic code generates the sequence
         * ntop[k-1] = (ntop[k]+1)/2, where ntop[log2n] = n
         */
        c = c>>1;
        nxtop = nxtop<<1;
        nytop = nytop<<1;
        if (nxf <= c) { nxtop -= 1; } else { nxf -= c; }
        if (nyf <= c) { nytop -= 1; } else { nyf -= c; }
        /*
         * double divisor on last pass
         */
        if (k == 0) {
            shift = 2;
            dp = 2;
            /* for negative numbers, dm = - dp + 2**shift -1 = -2+3 = +1 */
            dm = 1;
        }
        /*
         * unshuffle in each dimension to interleave coefficients
         */
        for (i = 0; i<nxtop; i++) {
            unshuffle(&a[ny*i],nytop,1,tmp);
        }
        for (j = 0; j<nytop; j++) {
            unshuffle(&a[j],nxtop,ny,tmp);
        }
        /*
         * smooth by interpolating coefficients if SMOOTH != 0
         */
        if (smooth) hsmooth(a,nxtop,nytop,ny,scale);
        oddx = nxtop % 2;
        oddy = nytop % 2;
        for (i = 0; i<nxtop-oddx; i += 2) {
            s00 = ny*i;             /* s00 is index of a[i,j]   */
            s10 = s00+ny;           /* s10 is index of a[i+1,j] */
            for (j = 0; j<nytop-oddy; j += 2) {
                /*
                 * Divide sums by 2 (4 last time) (shift right 1 (or 2) bits).
                 * Variable d allows rounding.
                 *
                 * For negative numbers a different value is added so rounding
                 * is symmetrical for +,-.  This could be simplified for images
                 * that must be positive, but for generality the stuff for
                 * negative numbers was left in.
                 */
                b11 = a[s00] + a[s10] + a[s00+1] + a[s10+1];
                b10 = a[s00] + a[s10] - a[s00+1] - a[s10+1];
                b01 = a[s00] - a[s10] + a[s00+1] - a[s10+1];
                b00 = a[s00] - a[s10] - a[s00+1] + a[s10+1];
                a[s10+1] = ((b11>0) ? (b11+dp) : (b11+dm)) >> shift;
                a[s10  ] = ((b10>0) ? (b10+dp) : (b10+dm)) >> shift;
                a[s00+1] = ((b01>0) ? (b01+dp) : (b01+dm)) >> shift;
                a[s00  ] = ((b00>0) ? (b00+dp) : (b00+dm)) >> shift;
                s00 += 2;
                s10 += 2;
            }
            if (oddy) {
                /*
                 * do last element in row if row length is odd
                 * s00+1, s10+1 are off edge
                 */
                b10 = a[s00] + a[s10];
                b00 = a[s00] - a[s10];
                a[s10  ] = ((b10>0) ? (b10+dp) : (b10+dm)) >> shift;
                a[s00  ] = ((b00>0) ? (b00+dp) : (b00+dm)) >> shift;
            }
        }
        if (oddx) {
            /*
             * do last row if column length is odd
             * s10, s10+1 are off edge
             */
            s00 = ny*i;
            for (j = 0; j<nytop-oddy; j += 2) {
                b01 = a[s00] + a[s00+1];
                b00 = a[s00] - a[s00+1];
                a[s00+1] = ((b01>0) ? (b01+dp) : (b01+dm)) >> shift;
                a[s00  ] = ((b00>0) ? (b00+dp) : (b00+dm)) >> shift;
                s00 += 2;
            }
            if (oddy) {
                /*
                 * do corner element if both row and column lengths are odd
                 * s00+1, s10, s10+1 are off edge
                 */
                b00 = a[s00];
                a[s00  ] = ((b00>0) ? (b00+dp) : (b00+dm)) >> shift;
            }
        }
    }
    free(tmp);
}


static void
unshuffle(a,n,n2,tmp)
int a[];    /* array to shuffle                 */
int n;      /* number of elements to shuffle    */
int n2;     /* second dimension                 */
int tmp[];  /* scratch storage                  */
{
int i;
int nhalf;
int *p1, *p2, *pt;

    /*
     * copy 2nd half of array to tmp
     */
    nhalf = (n+1)>>1;
    pt = tmp;
    p1 = &a[n2*nhalf];              /* pointer to a[i]          */
    for (i=nhalf; i<n; i++) {
        *pt = *p1;
        p1 += n2;
        pt += 1;
    }
    /*
     * distribute 1st half of array to even elements
     */
    p2 = &a[ n2*(nhalf-1) ];        /* pointer to a[i]          */
    p1 = &a[(n2*(nhalf-1))<<1];     /* pointer to a[2*i]        */
    for (i=nhalf-1; i >= 0; i--) {
        *p1 = *p2;
        p2 -= n2;
        p1 -= (n2+n2);
    }
    /*
     * now distribute 2nd half of array (in tmp) to odd elements
     */
    pt = tmp;
    p1 = &a[n2];                    /* pointer to a[i]          */
    for (i=1; i<n; i += 2) {
        *p1 = *pt;
        p1 += (n2+n2);
        pt += 1;
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create pltmodel.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       PLTMODEL
 * PURPOSE:
 *       Compute values of plate model and its partial derivatives
 *       for use in computing inverse.
 * CALLING SEQUENCE:
 *       PLTMODEL(header,X,Y,MAG,COLOUR,F,FX,FY,G,GX,GY)
 * INPUTS:
 *       h        - header structure
 *       X,Y      - REAL*8 position
 *       MAG      - magnitude
 *       COLOUR   - colour
 * OUTPUTS:
 *       F        - REAL*8 St.coord xi
 *       FX       - REAL*8 Deriv. xi wrt x
 *       FY       - REAL*8 Deriv. xi wrt y
 *       G        - REAL*8 St.coord eta
 *       GX       - REAL*8 Deriv. eta wrt x
 *       GY       - REAL*8 Deriv. eta wrt y
 * PROCEDURE:
 *       Computes value of plate model and the partial derivatives.
 * MODIFICATION HISTORY
 *       Converted to Fortran from IDL, R. White, 7 May 1990
 *       Converted to C from Fortran, R. White, 30 July 1991
 */
#include "header.h"

extern void
pltmodel(h,x,y,mag,colour,f,fx,fy,g,gx,gy)
Header *h;
double x,y,mag,colour;
double *f, *fx, *fy, *g, *gx, *gy;
{
double cjunk,x4,y4;

    /*
     *  X plate model
     */
    cjunk=(x*x+y*y)*(x*x+y*y);
    x4 = (x*x)*(x*x);
    y4 = (y*y)*(y*y);
    *f=h->amd_x_coeff[0]*x                 + h->amd_x_coeff[1]*y              +
       h->amd_x_coeff[2]                   + h->amd_x_coeff[3]*x*x            +
       h->amd_x_coeff[4]*x*y               + h->amd_x_coeff[5]*y*y            +
       h->amd_x_coeff[6]*(x*x+y*y)         + h->amd_x_coeff[7]*x*x*x          +
       h->amd_x_coeff[8]*x*x*y             + h->amd_x_coeff[9]*x*y*y          +
       h->amd_x_coeff[10]*y*y*y            + h->amd_x_coeff[11]*x*(x*x+y*y)   +
       h->amd_x_coeff[12]*x*cjunk          + h->amd_x_coeff[13]*mag           +
       h->amd_x_coeff[14]*mag*mag          + h->amd_x_coeff[15]*mag*mag*mag   +
       h->amd_x_coeff[16]*mag*x            + h->amd_x_coeff[17]*mag*(x*x+y*y) +
       h->amd_x_coeff[18]*mag*x*(x*x+y*y)  + h->amd_x_coeff[19]*colour;
    /*
     *  Derivative of X model wrt x
     */
    *fx=h->amd_x_coeff[0]                              +
        h->amd_x_coeff[3]*2.0*x                        +
        h->amd_x_coeff[4]*y                            +
        h->amd_x_coeff[6]*2.0*x                        +
        h->amd_x_coeff[7]*3.0*x*x                      +
        h->amd_x_coeff[8]*2.0*x*y                      +
        h->amd_x_coeff[9]*y*y                          +
        h->amd_x_coeff[11]*(3.0*x*x+y*y)               +
        h->amd_x_coeff[12]*(5.0*x4  +6.0*x*x*y*y+y4  ) +
        h->amd_x_coeff[16]*mag                         +
        h->amd_x_coeff[17]*mag*2.0*x                   +
        h->amd_x_coeff[18]*mag*(3.0*x*x+y*y);
    /*
     *  Derivative of X model wrt y
     */
    *fy=h->amd_x_coeff[1]                     +
        h->amd_x_coeff[4]*x                   +
        h->amd_x_coeff[5]*2.0*y               +
        h->amd_x_coeff[6]*2.0*y               +
        h->amd_x_coeff[8]*x*x                 +
        h->amd_x_coeff[9]*x*2.0*y             +
        h->amd_x_coeff[10]*3.0*y*y            +
        h->amd_x_coeff[11]*2.0*x*y            +
        h->amd_x_coeff[12]*4.0*x*y*(x*x+y*y)  +
        h->amd_x_coeff[17]*mag*2.0*y          +
        h->amd_x_coeff[18]*mag*2.0*x*y;
    /*
     *  Y plate model
     */
    *g=h->amd_y_coeff[0]*y                + h->amd_y_coeff[1]*x              +
       h->amd_y_coeff[2]                  + h->amd_y_coeff[3]*y*y            +
       h->amd_y_coeff[4]*y*x              + h->amd_y_coeff[5]*x*x            +
       h->amd_y_coeff[6]*(x*x+y*y)        + h->amd_y_coeff[7]*y*y*y          +
       h->amd_y_coeff[8]*y*y*x            + h->amd_y_coeff[9]*y*x*x          +
       h->amd_y_coeff[10]*x*x*x           + h->amd_y_coeff[11]*y*(x*x+y*y)   +
       h->amd_y_coeff[12]*y*cjunk         + h->amd_y_coeff[13]*mag           +
       h->amd_y_coeff[14]*mag*mag         + h->amd_y_coeff[15]*mag*mag*mag   +
       h->amd_y_coeff[16]*mag*y           + h->amd_y_coeff[17]*mag*(x*x+y*y) +
       h->amd_y_coeff[18]*mag*y*(x*x+y*y) + h->amd_y_coeff[19]*colour;
    /*
     *  Derivative of Y model wrt x
     */
    *gx=h->amd_y_coeff[1]                    +
        h->amd_y_coeff[4]*y                  +
        h->amd_y_coeff[5]*2.0*x              +
        h->amd_y_coeff[6]*2.0*x              +
        h->amd_y_coeff[8]*y*y                +
        h->amd_y_coeff[9]*y*2.0*x            +
        h->amd_y_coeff[10]*3.0*x*x           +
        h->amd_y_coeff[11]*2.0*x*y           +
        h->amd_y_coeff[12]*4.0*x*y*(x*x+y*y) +
        h->amd_y_coeff[17]*mag*2.0*x         +
        h->amd_y_coeff[18]*mag*y*2.0*x;
    /*
     *  Derivative of Y model wrt y
     */
    *gy=h->amd_y_coeff[0]                              +
        h->amd_y_coeff[3]*2.0*y                        + 
        h->amd_y_coeff[4]*x                            +
        h->amd_y_coeff[6]*2.0*y                        +
        h->amd_y_coeff[7]*3.0*y*y                      +
        h->amd_y_coeff[8]*2.0*y*x                      +
        h->amd_y_coeff[9]*x*x                          +
        h->amd_y_coeff[11]*3.0*y*y                     +
        h->amd_y_coeff[12]*(5.0*y4  +6.0*x*x*y*y+x4  ) +
        h->amd_y_coeff[16]*mag                         +
        h->amd_y_coeff[17]*mag*2.0*y                   +
        h->amd_y_coeff[18]*mag*(x*x+3.0*y*y);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create precess.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* in_ra & in_dec in degrees
   epoch in decimal years like 1950.0
   out_ra & out_dec in degrees J2000
*/
#include <math.h>
#define PI 3.141592653589793238
extern void
precess(in_ra,in_dec,epoch,out_ra,out_dec)
double in_ra,in_dec,epoch;
double *out_ra,*out_dec;
{
 double T,zeta_A,z_A,theta_A,az,de,th,A,B,C;
 double alpha2000,delta2000,alpha_in,delta_in;

 T = (epoch - 2000.0)/100.0;
 zeta_A  = 0.6406161* T + 0.0000839* T*T + 0.0000050* T*T*T;
 z_A     = 0.6406161* T + 0.0003041* T*T + 0.0000051* T*T*T;
 theta_A = 0.5567530* T - 0.0001185* T*T + 0.0000116* T*T*T;
 
 alpha_in=in_ra;
 delta_in=in_dec;
 az=(alpha_in - z_A)*PI/180.;
 de=(delta_in)*PI/180.;
 th=(theta_A)*PI/180;
 A = sin(az) * cos(de);
 B = cos(az) * cos(th) * cos(de) + sin(th) * sin(de);
 C = -cos(az) * sin(th) * cos(de) + cos(th) * sin(de);
 
 alpha2000 = atan2(A,B);
 alpha2000 = alpha2000*180./PI - zeta_A;

 if(alpha2000 < 0.0) alpha2000 = 360.+alpha2000;
 if(alpha2000 > 360.) alpha2000 = alpha2000-360.;

 delta2000 = asin(C);
 delta2000 = delta2000*180./PI;
 *out_ra=alpha2000;
 *out_dec=delta2000; 

}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create b195j200.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* b1950_j2000.c    Convert from B1950 coordinates to J2000
 *
 * Programmer: R. White     Date: 5 April 1989
 */
#include <math.h>

#define PI 3.141592653589793238

/*
 * conversion between B1950 and J2000 coordinates
 * from Aoki, X., Soma, M., Kinoshita, H., Inoue, K. (1983) A.Ap. 128, 263.
 *      and Astronomical Almanac, 1989, pp. B42-B43 and
 * Simplified for the case of no proper motions or parallaxes
 * in J2000 coordinate system.  Note that a fixed target would
 * have a spurious proper motion (the E-term of the aberration)
 * in the B1950 coordinates, so for fixed targets the correct assumption
 * is no PM in the J2000 frame.
 */

static double a[3] = { -1.62557e-6, -0.31919e-6, -0.13843e-6 };
static double adot[3] = { 1.244e-3, -1.579e-3, -0.660e-3 };
static double m[6][6] = {
    {  0.9999256782, -0.0111820611, -0.0048579477, 
         0.00000242395018, -0.00000002710663, -0.00000001177656 },
    {  0.0111820610,  0.9999374784, -0.0000271765,
         0.00000002710663,  0.00000242397878, -0.00000000006587 },
    {  0.0048579479, -0.0000271474,  0.9999881997,
         0.00000001177656, -0.00000000006582,  0.00000242410173 },
    {  -0.000551, -0.238565,  0.435739,  0.99994704, -0.01118251, -0.00485767 },
    {   0.238514, -0.002667, -0.008541,  0.01118251,  0.99995883, -0.00002718 },
    {  -0.435623,  0.012254,  0.002117,  0.00485767, -0.00002714,  1.00000956 }
};

/* convert B1950 to J2000 coordinates assuming that proper motion and
 * parallax in J2000 frame are zero
 */
extern void
b1950_j2000(in_ra,in_dec,out_ra,out_dec)
double in_ra,in_dec;
double *out_ra,*out_dec;
{
double r0[3], r[6], a1[3];
double ar = 0.0, cd, t1, t;
int i, j;

    cd = cos(in_dec);
    r0[0] = cd*cos(in_ra);
    r0[1] = cd*sin(in_ra);
    r0[2] = sin(in_dec);

    /* correct for E-terms of aberration
     * assume epoch of observation was about 1980; there ought to be
     * another version of the program when the epoch of the observations
     * are known.
     */
    t1 = (1980.0 - 1950.0)/100.0;
    t = (t1-0.5)*(PI/(180.0*3600.0));
    t1 = t1*(PI/(180.0*3600.0));
    for (i=0; i<3; i++) a1[i] = a[i] + adot[i]*t1;
    for (i=0; i<3; i++) ar += r0[i]*a1[i];
    for (i=0; i<3; i++) r0[i] = (1.0+ar)*r0[i] - a1[i];

    /* precess from 1950 to 2000 */
    for(i=0; i<6; i++) {
        r[i] = 0.0;
        for (j=0; j<3; j++) r[i] += m[i][j]*r0[j];
    }
    /* modify r for assumed 1980 epoch */
    for (i=0; i<3; i++) r[i] += r[i+3]*t;

    /* convert from x,y,z to RA, Dec */
    *out_dec = asin( r[2] / sqrt(r[0]*r[0]+r[1]*r[1]+r[2]*r[2]) );
    *out_ra = atan2(r[1],r[0]);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sky.imake
#define  PROGRAM   sky

#define MODULE_LIST sky.c newgetxy.c qread.c xypos.c readhdr.c utility.c \
 transteq.c hsmooth.c dodecode.c qtreedec.c bitinput.c
#define MODULE_LIST2 condmsrd.c fillhdr.c fillpos.c conrddms.c hdcmprss.c
#define MODULE_LIST3 position.c ppoinv.c amdinv.c traneqst.c decode.c
#define MODULE_LIST4 undigitz.c ppopos.c amdpos.c hinv.c pltmodel.c precess.c \
 b195j200.c
#define INCLUDE_LIST header.h qfile.h
#define MAIN_LANG_C
#define R2LIB 

#define USES_C

#define LIB_RTL
#define LIB_P2SUB
#define LIB_TAE
#define LIB_MDMS
#define LIB_KERBEROS
#define LIB_SIMBAD

$ Return
$!#############################################################################
$PDF_File:
$ create sky.pdf
process help=*
PARM OUT TYPE=STRING COUNT=1
PARM HEADERS    TYPE=(STRING,80) COUNT=(0:1) +
   DEFAULT=/project/jukebox/poss/usa_aura_stsi_dss1_0102/headers
PARM DATA    TYPE=(STRING,80) COUNT=(0:1) +
   DEFAULT=/project/jukebox/poss
PARM NAME  TYPE=STRING COUNT=(0:1) DEFAULT="ngc 1097"
PARM USERID  TYPE=STRING COUNT=(0:1) DEFAULT="u2141ozs"
PARM PASSWORD  TYPE=STRING COUNT=(0:1) DEFAULT="none"
PARM RA    TYPE=REAL COUNT=(0:3) DEFAULT=--
PARM DEC    TYPE=REAL COUNT=(0:3) DEFAULT=--
PARM EPOCH TYPE=REAL COUNT=(0:1) DEFAULT=2000
PARM NL TYPE=INTEGER COUNT=(0:1) VALID=(0:4000) DEFAULT=0
PARM NS TYPE=INTEGER COUNT=(0:1) VALID=(0:4000) DEFAULT=0
PARM FOV TYPE=REAL COUNT=(0:1) VALID=(.01:1000.) DEFAULT=15.0
PARM GRID TYPE=REAL COUNT=(0:1) VALID=(0.0:21600.) DEFAULT=5.0
PARM DN TYPE=INTEGER COUNT=(0:1) VALID=(0:32767) DEFAULT=15000
PARM STATUS TYPE=(STRING,100) COUNT=(0:1) DEFAULT=""
END-PROC

.TITLE
VICAR program SKY

.HELP
PURPOSE:
To create a vicar picture containing an image of the night sky.
The data is from the Space Telescope Institute digitized sky survey.
There are three ways to run this program. See METHOD section.

EXECUTION:
sky out=picture.img ra=(14,22,33.854) dec=(0,-13,37.57) fov=15
or
sky out=picture.img name="ngc 1097" fov=15 password="   "
or
sky out=picture.img name="ngc 1097" fov=15

.PAGE
METHOD:
There are three ways to run SKY:

METHOD 1.
If you provide RA & Dec coordinates you do not need a password.
The execution looks like this:
sky out=picture.img ra=(14,22,33.854) dec=(0,-13,37.57) fov=15

METHOD 2.
If you know the SIMBAD password which accompanies the defaulted userrid 
(see USERID keyword) then execution looks like:
sky out=picture.img name="ngc 1097" fov=15 password="   "
 
METHOD 3.
If you permit Kerberos to get the password for you from the password server
then execution looks like:
sky out=picture.img name="ngc 1097" fov=15
In order to do this you'll need to first:
selcat o
kinit (which will ask you for your kerberos password).
vicar


Sky uses the mips jukebox to get at the Palomar Sky Survey digitized data set.
Adapted from software written by the Space Telescope Institute.
This software contains the following logo:
Copyright (c) 1993, 1994, Association of Universities for Research in
Astronomy, Inc.  All rights reserved.

Obtains coordinates of objects by name from the SIMBAD data base.

HISTORY:
2-1-97  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1

.VARI OUT
Output image

.VARI FOV
Field of view
in arc minutes.

.VARI NL
number lines in
output images.
Overrides FOV

.VARI NS
number samples in
output images.
Overrides FOV

.VARI HEADERS
Directory path for
header files

.VARI DATA
Directory path for
sky survey files

.VARI RA
The RA 
of the output
image center.

.VARI DEC
The DEC of the
output image center.

.VARI EPOCH
Year of the RA & DEC
Defaults to 2000

.VARI NAME
Object name

.VARI USERID
Simbad userid
 
.VARI PASSWORD
Simbad password

.VARI GRID
The grid interval
in minutes of arc.
(not time)

.VARI DN
The intensity of
the grid lines.

.VARI STATUS
Directory to contain
status file.
Defaults to local.

.LEVEL2

.VARI OUT
Output image name

.VARI FOV
Desired field of view in arc minutes.
Defaults to 15
This results in an image about 530 by 530 pixels. The upper limit is 14000
by 14000.

.VARI NL
number of lines in the output image.
Overrides the value computed from the FOV keyword.

.VARI NS
number of samples in the output image.
Overrides the value computed from the FOV keyword.

.VARI HEADERS
Directory path for the sky survey header files

.VARI DATA
Directory path for the compressed sky survey files

.VARI RA
The RA of the output image center. 
If 3 values are given they represent: hours minutes seconds (of time) 
If 1 value  is given it represents: degrees
Defaults to j2000 coordinates.
(See the NAME keyword, only used if NAME not used)

.VARI DEC
The DEC of the output image center.
If 3 values are given they represent: degrees minutes seconds (of arc)
If 1 value  is given it represents: degrees
Example: if the dec is 34 12 45.663 then dec=(34,12,45.663)
Example: if the dec is -0 12 45.663 then dec=(0,-12,45.663)
(-0,12,45.663 might work if the machine has a -0)
Defaults to j2000 coordinates.
(See the NAME keyword, only used if NAME not used)

.VARI EPOCH
The decimal year of the given RA & DEC.
If epoch is other than 2000 the RA & DEC will be precessed to j2000.
Defaults to 2000.

.VARI NAME
The name of the object about which the picture is to be generated.
Example: name="ngc 1097"
A query will be sent to SIMBAD to retrieve the object's coordinates.
The image will be generated with this coordinate at the center.

.VARI USERID
Simbad userid. Only used if NAME is used.
 
.VARI PASSWORD
Simbad password. Only used if NAME is used.

.VARI GRID
The grid interval in minutes of arc.
Default 2
If GRID is 0 no grid is drawn.

.VARI DN
The intensity of the grid lines.
Default 15000

.VARI STATUS
Directory to contain the status file.
Defaults to the local directory.
The status file is called "status_file.txt"
If the program was successful it will contain the string "success".
If the program began but failed to run it will contain the string "failure" on
record 1 followed by optional records containing ascii messages describing
the nature of the error.

$ Return
$!#############################################################################
$Test_File:
$ create tstsky.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
! Create an image of NGC1097 without need of SIMBAD data base
sky out=picture.img ra=(2,46,30.0) dec=(-30,14,0.0) fov=20.
xvd picture.img
!
! Create an image of NGC1097 with SIMBAD data base
! Warning to testers: You need a Kerberos ticket do test this option.
sky out=picture.img name="ngc 1097" fov=20.
xvd picture.img
!
sky out=picture.img ra=(2,46,30.0) dec=(-30,14,0.0) fov=120 grid=0
xvd picture.img
!
end-proc
$ Return
$!#############################################################################
