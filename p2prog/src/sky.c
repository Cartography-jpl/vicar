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

void main44(void)
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
   status=zvunit(&ounit,"OUT",1, NULL);
   zvsignal(ounit,status,1);
   status=zvopen(ounit,"OP","WRITE","U_NL",nl,"U_NS",ns,"U_NB",1,
                      "U_FORMAT","HALF","O_FORMAT","HALF", NULL);
   zvsignal(ounit,status,1);

/* copy header structure data to picture label */
   status=zladd(ounit,"HISTORY","plate_root",header.plate_root,
          "FORMAT","STRING", NULL);
   status=zladd(ounit,"HISTORY","header_dir",header.header_dir,
          "FORMAT","STRING", NULL);
   status=zladd(ounit,"HISTORY","compression",header.compression,
          "FORMAT","STRING", NULL);
   status=zladd(ounit,"HISTORY","plate_name",header.plate_name,
          "FORMAT","STRING", NULL);
   status=zladd(ounit,"HISTORY","plate_id",header.plate_id,
          "FORMAT","STRING", NULL);
   status=zladd(ounit,"HISTORY","region_no",header.region_no,
          "FORMAT","STRING", NULL);
   status=zladd(ounit,"HISTORY","section_x_length",&header.section_x_length,
          "FORMAT","INT", NULL);
   status=zladd(ounit,"HISTORY","section_y_length",&header.section_y_length,
          "FORMAT","INT", NULL);
   status=zladd(ounit,"HISTORY","section_x_corner",&header.section_x_corner,
          "FORMAT","REAL", NULL);
   status=zladd(ounit,"HISTORY","section_y_corner",&header.section_y_corner,
          "FORMAT","REAL", NULL);
   status=zladd(ounit,"HISTORY","ra_h",&header.ra_h,
          "FORMAT","INT", NULL);
   status=zladd(ounit,"HISTORY","ra_m",&header.ra_m,
          "FORMAT","INT", NULL);
   status=zladd(ounit,"HISTORY","dec_d",&header.dec_d,
          "FORMAT","INT", NULL);
   status=zladd(ounit,"HISTORY","dec_m",&header.dec_m,
          "FORMAT","INT", NULL);
   status=zladd(ounit,"HISTORY","ra_s",&header.ra_s,
          "FORMAT","REAL", NULL);
   status=zladd(ounit,"HISTORY","dec_s",&header.dec_s,
          "FORMAT","REAL", NULL);
   scr[0]=header.dec_sign; scr[1]='\0';
   status=zladd(ounit,"HISTORY","dec_sign",scr,
          "FORMAT","STRING", NULL);
   status=zladd(ounit,"HISTORY","x",&header.x,
          "FORMAT","REAL", NULL);
   status=zladd(ounit,"HISTORY","y",&header.y,
          "FORMAT","REAL", NULL);
   status=zladd(ounit,"HISTORY","plt_scale",&header.plt_scale,
          "FORMAT","DOUB", NULL);
   status=zladd(ounit,"HISTORY","x_pixel_size",&header.x_pixel_size,
          "FORMAT","DOUB", NULL);
   status=zladd(ounit,"HISTORY","y_pixel_size",&header.y_pixel_size,
          "FORMAT","DOUB", NULL);
   status=zladd(ounit,"HISTORY","plt_center_vec",header.plt_center_vec,
          "FORMAT","DOUB","NELEMENT",3, NULL);
   status=zladd(ounit,"HISTORY","ppo_coeff",header.ppo_coeff,
          "FORMAT","DOUB","NELEMENT",6, NULL);
   status=zladd(ounit,"HISTORY","amd_x_coeff",header.amd_x_coeff,
          "FORMAT","DOUB","NELEMENT",20, NULL);
   status=zladd(ounit,"HISTORY","amd_y_coeff",header.amd_y_coeff,
          "FORMAT","DOUB","NELEMENT",20, NULL);
   status=zladd(ounit,"HISTORY","plt_center_ra",&header.plt_center_ra,
          "FORMAT","DOUB", NULL);
   status=zladd(ounit,"HISTORY","plt_center_dec",&header.plt_center_dec,
          "FORMAT","DOUB", NULL);
   scr[0]=header.amd_flag;
   status=zladd(ounit,"HISTORY","amd_flag",scr,
          "FORMAT","STRING", NULL);
   scr[0]=header.ppo_flag;
   status=zladd(ounit,"HISTORY","ppo_flag",scr,
          "FORMAT","STRING", NULL);
   scr[0]=header.plate_data_flag;
   status=zladd(ounit,"HISTORY","plate_data_flag",scr,
          "FORMAT","STRING", NULL);
   scr[0]=header.special_plate_flag;
   status=zladd(ounit,"HISTORY","special_plate_flag",scr,
          "FORMAT","STRING", NULL);
   status=zladd(ounit,"HISTORY","plt_grade",&header.plt_grade,
          "FORMAT","INT", NULL);
   status=zladd(ounit,"HISTORY","emulsion_code",&header.emulsion_code,
          "FORMAT","INT", NULL);
   status=zladd(ounit,"HISTORY","scaling",&header.scaling,
          "FORMAT","REAL", NULL);
   status=zladd(ounit,"HISTORY","mag_limit",&header.mag_limit,
          "FORMAT","REAL", NULL);
   status=zladd(ounit,"HISTORY","plt_date",&header.plt_date,
          "FORMAT","REAL", NULL);
   status=zladd(ounit,"HISTORY","object_name",header.object_name,
          "FORMAT","STRING", NULL);
   status=zladd(ounit,"HISTORY","exposure_time",header.exposure_time,
          "FORMAT","STRING", NULL);
   status=zladd(ounit,"HISTORY","seeing",header.seeing,
          "FORMAT","STRING", NULL);
   status=zladd(ounit,"HISTORY","origin",header.origin,
          "FORMAT","STRING", NULL);
   status=zladd(ounit,"HISTORY","plate_label",header.plate_label,
          "FORMAT","STRING", NULL);

   status=zladd(ounit,"HISTORY","x_of_sample_1",&x0,
          "FORMAT","REAL", NULL);
   status=zladd(ounit,"HISTORY","y_of_line_1",&y0,
          "FORMAT","REAL", NULL);

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
