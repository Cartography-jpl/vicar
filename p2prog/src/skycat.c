/* skycat */
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
#define MAX_NS 4000
#define MAX_NAREAS 10

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

/* for SIMBAD */
 char hostname[256], service[8],userid[MAX_USERID+1], passwd[MAX_USERID+1] ;
 char rec[256],co_radius[100],*astrotyplist,*typ;
 int hh, nitems, ndata, status;
 
/* for KERBEROS */
 char errorBuff[PWDMAXERRSTRLEN];
 char password[256],userName[L_cuserid];
 char *cuserid_p2();
 int errorNum;

   Header header;
   FILE *fd,*fout;
   char path[80],radec[80],filename[80],msg[200],temp[80];
   char Rgn[10],Plate[10],Survey[10],best_Rgn[10];
   char object[75],scr[2],catalogue[80];
   char name[20],buf[81];
   char *p;
   char Dec_sign[2];
   char lo_dec_sign,hi_dec_sign,decsign;
   char regions[MAX_NAREAS][6];
   char temp1[10],temp2[10];
   char item_spclass[50],item_class[50],item_morphology[50],item_id[50];
   unsigned char cobuf[100];
   int count,def,line,save_region[MAX_NAREAS],ntarget;
   int unit,nl,ns,i,j,k,m,n,have_coordinate;
   int lo_ra_h,lo_ra_m,hi_ra_h,hi_ra_m,region;
   int lo_dec_d,hi_dec_d,nareas;
   int mag_band,class,n_gsc,rah,ram,decd,decm; 
   double Dec_1,Dec_2,RA_1,RA_2;
   double min_dec,max_dec,min_ra,max_ra;
   double ra_pix[4],dec_pix[4];
   double lo_ra,hi_ra,lo_dec,hi_dec;
   double b_mag,v_mag,prop_mot_ra,prop_mot_dec,Separation;
   float lo_ra_s,hi_ra_s,lo_dec_m,hi_dec_m;
   float mag, colour,x,y,x0,y0;
   float RA_s,Dec_s,rline,sample;
   float pos_err,magnitude,mag_err,ras,decs;

/* table of gsc directory names */
   static char directory_name[24][6] = { 
   {"n0000"},{"n0730"},{"n1500"},{"n2230"},{"n3000"},{"n3730"},{"n4500"},
   {"n5230"},{"n6000"},{"n6730"},{"n7500"},{"n8230"},
   {"s0000"},{"s0730"},{"s1500"},{"s2230"},{"s3000"},{"s3730"},{"s4500"},
   {"s5230"},{"s6000"},{"s6730"},{"s7500"},{"s8230"}};
/* table of corresponding region bounds */
   static int region_boundary[24][2] = {
{1,593},{594,1177},{1178,1728},{1729,2258},{2259,2780},{2781,3245},{3246,3651},
{3652,4013},{4014,4293},{4294,4491},{4492,4614},{4615,4662},
{4663,5259},{5260,5837},{5838,6411},{6412,6988},{6989,7522},{7523,8021},
{8022,8463},{8464,8839},{8840,9133},{9134,9345},{9346,9489},{9490,9537}};

/* for status file, write "failure" to begin with */
   FILE *stat_file;
   char stat_dir[100];
   status=zvparm("STATUS",stat_dir,&count,&def,1,0);
   if(strlen(stat_dir) == 0)strcpy(stat_dir,"status_file.txt");
   else strcat(stat_dir,"/status_file.txt");
   if ((stat_file = fopen(stat_dir, "w")) == NULL) {
      fprintf(stderr, "Error opening status file %s\n",stat_dir);
      zabend();;
   }
   if (fputs("failure\n", stat_file) == NULL) {
      fprintf(stderr, "Error writing status file %s\n",stat_dir);
      zabend();
   }

/* vicar parameters */
   status=zveaction("AS","  ");
   status=zvparm("PATH",path,&count,&def,1,0);
   status=zvparm("PASSWORD",passwd,&count,&def,1,0);
   status=zvparm("USERID",userid,&count,&def,1,0);

/* get password from password server through a kerberos ticket */
 if(strcmp(passwd,"none") == 0){
   strcpy(userName, cuserid_p2());
   errorNum = mdms_passwordSrvGet (userName,"simbad", password);
   if(errorNum < PWDSUCCESS){
     zvmessage("Cannot obtain kerberos password, you must first:"," ");
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

/* open input vicar file */
   status=zvunit(&unit,"INP",1, NULL);
   zvsignal(unit,status,1);
   status=zvopen(unit,"U_FORMAT","HALF", NULL);
   zvsignal(unit,status,1);
   status=zvget(unit,"NL",&nl,"NS",&ns, NULL);
   zvsignal(unit,status,1);

/* copy label info into structure */
   status=zlget(unit,"HISTORY","plate_root",header.plate_root,
          "FORMAT","STRING", NULL);
   status=zlget(unit,"HISTORY","header_dir",header.header_dir,
          "FORMAT","STRING", NULL);
   status=zlget(unit,"HISTORY","compression",header.compression,
          "FORMAT","STRING", NULL);
   status=zlget(unit,"HISTORY","plate_name",header.plate_name,
          "FORMAT","STRING", NULL);
   status=zlget(unit,"HISTORY","plate_id",header.plate_id,
          "FORMAT","STRING", NULL);
   status=zlget(unit,"HISTORY","region_no",header.region_no,
          "FORMAT","STRING", NULL);
   status=zlget(unit,"HISTORY","section_x_length",&header.section_x_length,
          "FORMAT","INT", NULL);
   status=zlget(unit,"HISTORY","section_y_length",&header.section_y_length,
          "FORMAT","INT", NULL);
   status=zlget(unit,"HISTORY","section_x_corner",&header.section_x_corner,
          "FORMAT","REAL", NULL);
   status=zlget(unit,"HISTORY","section_y_corner",&header.section_y_corner,
          "FORMAT","REAL", NULL);
   status=zlget(unit,"HISTORY","ra_h",&header.ra_h,
          "FORMAT","INT", NULL);
   status=zlget(unit,"HISTORY","ra_m",&header.ra_m,
          "FORMAT","INT", NULL);
   status=zlget(unit,"HISTORY","dec_d",&header.dec_d,
          "FORMAT","INT", NULL);
   status=zlget(unit,"HISTORY","dec_m",&header.dec_m,
          "FORMAT","INT", NULL);
   status=zlget(unit,"HISTORY","ra_s",&header.ra_s,
          "FORMAT","REAL", NULL);
   status=zlget(unit,"HISTORY","dec_s",&header.dec_s,
          "FORMAT","REAL", NULL);
   status=zlget(unit,"HISTORY","dec_sign",scr,
          "FORMAT","STRING", NULL);
   header.dec_sign=scr[0];
   status=zlget(unit,"HISTORY","x",&header.x,
          "FORMAT","REAL", NULL);
   status=zlget(unit,"HISTORY","y",&header.y,
          "FORMAT","REAL", NULL);
   status=zlget(unit,"HISTORY","plt_scale",&header.plt_scale,
          "FORMAT","DOUB", NULL);
   status=zlget(unit,"HISTORY","x_pixel_size",&header.x_pixel_size,
          "FORMAT","DOUB", NULL);
   status=zlget(unit,"HISTORY","y_pixel_size",&header.y_pixel_size,
          "FORMAT","DOUB", NULL);
   status=zlget(unit,"HISTORY","plt_center_vec",header.plt_center_vec,
          "FORMAT","DOUB","NELEMENT",3, NULL);
   status=zlget(unit,"HISTORY","ppo_coeff",header.ppo_coeff,
          "FORMAT","DOUB","NELEMENT",6, NULL);
   status=zlget(unit,"HISTORY","amd_x_coeff",header.amd_x_coeff,
          "FORMAT","DOUB","NELEMENT",20, NULL);
   status=zlget(unit,"HISTORY","amd_y_coeff",header.amd_y_coeff,
          "FORMAT","DOUB","NELEMENT",20, NULL);
   status=zlget(unit,"HISTORY","plt_center_ra",&header.plt_center_ra,
          "FORMAT","DOUB", NULL);
   status=zlget(unit,"HISTORY","plt_center_dec",&header.plt_center_dec,
          "FORMAT","DOUB", NULL);
   status=zlget(unit,"HISTORY","amd_flag",scr,
          "FORMAT","STRING", NULL);
   header.amd_flag=scr[0];
   status=zlget(unit,"HISTORY","ppo_flag",scr,
          "FORMAT","STRING", NULL);
   header.ppo_flag=scr[0];
   status=zlget(unit,"HISTORY","plate_data_flag",scr,
          "FORMAT","STRING", NULL);
   header.plate_data_flag=scr[0];
   status=zlget(unit,"HISTORY","special_plate_flag",scr,
          "FORMAT","STRING", NULL);
   header.special_plate_flag=scr[0];
   status=zlget(unit,"HISTORY","plt_grade",&header.plt_grade,
          "FORMAT","INT", NULL);
   status=zlget(unit,"HISTORY","emulsion_code",&header.emulsion_code,
          "FORMAT","INT", NULL);
   status=zlget(unit,"HISTORY","scaling",&header.scaling,
          "FORMAT","REAL", NULL);
   status=zlget(unit,"HISTORY","mag_limit",&header.mag_limit,
          "FORMAT","REAL", NULL);
   status=zlget(unit,"HISTORY","plt_date",&header.plt_date,
          "FORMAT","REAL", NULL);
   status=zlget(unit,"HISTORY","object_name",header.object_name,
          "FORMAT","STRING", NULL);
   status=zlget(unit,"HISTORY","exposure_time",header.exposure_time,
          "FORMAT","STRING", NULL);
   status=zlget(unit,"HISTORY","seeing",header.seeing,
          "FORMAT","STRING", NULL);
   status=zlget(unit,"HISTORY","origin",header.origin,
          "FORMAT","STRING", NULL);
   status=zlget(unit,"HISTORY","plate_label",header.plate_label,
          "FORMAT","STRING", NULL);

   status=zlget(unit,"HISTORY","x_of_sample_1",&x0,
          "FORMAT","REAL", NULL);
   status=zlget(unit,"HISTORY","y_of_line_1",&y0,
          "FORMAT","REAL", NULL);

/* compute ra& dec for 4 corners of the image */
   sample=1; rline=1;
   x=sample+x0-1.0;
   y=nl-rline+y0;
   position(&header,x,y,mag,colour,&RA_1,&Dec_1);
   ra_pix[0] = RA_1*180/PI;
   dec_pix[0] = Dec_1*180/PI;
   sample=ns; rline=1;
   x=sample+x0-1.0;
   y=nl-rline+y0;
   position(&header,x,y,mag,colour,&RA_1,&Dec_1);
   ra_pix[1] = RA_1*180/PI;
   dec_pix[1] = Dec_1*180/PI;
   sample=1; rline=nl;
   x=sample+x0-1.0;
   y=nl-rline+y0;
   position(&header,x,y,mag,colour,&RA_1,&Dec_1);
   ra_pix[2] = RA_1*180/PI;
   dec_pix[2] = Dec_1*180/PI;
   sample=ns; rline=nl;
   x=sample+x0-1.0;
   y=nl-rline+y0;
   position(&header,x,y,mag,colour,&RA_1,&Dec_1);
   ra_pix[3] = RA_1*180/PI;
   dec_pix[3] = Dec_1*180/PI;

/* determine the min & max ra & dec */
   min_ra=ra_pix[0]; max_ra=ra_pix[0];
   min_dec=dec_pix[0]; max_dec=dec_pix[0];
   for (i=1; i < 4; i++){
      if(ra_pix[i] < min_ra)min_ra=ra_pix[i];
      if(ra_pix[i] > max_ra)max_ra=ra_pix[i];
      if(dec_pix[i] < min_dec)min_dec=dec_pix[i];
      if(dec_pix[i] > max_dec)max_dec=dec_pix[i];
   }
   printf("min_dec=%f max_dec=%f min_ra=%f max_ra=%f\n",min_dec,max_dec,
           min_ra,max_ra);

/* compute plate center as a string for simbad */
   sample=ns/2.0; rline=nl/2.0;
   x=sample+x0-1.0;
   y=nl-rline+y0;
   position(&header,x,y,mag,colour,&RA_1,&Dec_1);
   conrddms(RA_1,Dec_1,&rah,&ram,&ras,&decsign,&decd,&decm,&decs);
   sprintf(rec,"%d %d %f %c%d %d %f\n",rah,ram,ras,decsign,decd,decm,decs);

/* compute plate radius as a string from distance between plate corners*/
   RA_1=ra_pix[0]*PI/180.;
   Dec_1=dec_pix[0]*PI/180.;
   RA_2=ra_pix[3]*PI/180.;
   Dec_2=dec_pix[3]*PI/180.;
   Separation= acos(
            cos( PI/2. - Dec_2 )*cos( PI/2. - Dec_1 ) +
            sin( PI/2. - Dec_2 )*sin( PI/2. - Dec_1 )*cos( RA_1 - RA_2 ) );
   Separation=(Separation/2.0)*(180/PI); /* radius in degrees */
   i=Separation;
   j=(Separation-i)*60.;
   k=(Separation*60.-i*60.-j)*60.;
   /*sprintf(co_radius,"%f",Separation);*/
   sprintf(co_radius,"cooradius=%d %d %d",i,j,k);

/* open output ascii catalogue */
   status=zvpone("OUT",catalogue,1,sizeof(catalogue));
   if ((fout = fopen(catalogue, "w")) == NULL) {
      fprintf(stderr, "Error opening file %s\n", catalogue);
      fputs("Error opening catalogue\n", stat_file);
      zabend();
   }

 fputs("#   RA       Dec     line      sample   b_mag    v_mag   prop_mot_ra  prop_mot_dec  classification  morphology  spectrum  name\n", fout);

/********** READ HST GUIDE STAR CATALOGUE **********************************/

/* Read the regions.tbl file to extract the ra & dec range of each region  */
   strcpy(filename,path);
   strcat(filename,"/usa_aura_stsi_gsc1_1001/tables/regions.tbl");
   if ((fd = fopen(filename, "r")) == NULL) {
      fprintf(stderr, "Error opening file %s\n",filename);
      fputs("Error opening file\n", stat_file);
      zabend();;
   }
   for (i = 0; i < 180; i++){
      fgets(buf, 81, fd);    /* skip first 179 80 byte card images */
   }
   j=0;
   nareas=0;
   for (i = 1; i < 10000; i++) {
      if (fgets(buf, 48+1, fd) == NULL) break; /* read 48 byte blocks */
      sscanf(&buf[1], 
          "%d %d %d %f %d %d %f %d %f %d %f",
          &region,&lo_ra_h,&lo_ra_m,&lo_ra_s,&hi_ra_h,&hi_ra_m,&hi_ra_s,
          &lo_dec_d,&lo_dec_m,&hi_dec_d,&hi_dec_m);
      if(region-j != 1)break; /* check for sequential */
      j=region;
      lo_dec_sign=buf[31];
      hi_dec_sign=buf[40];
      if(lo_dec_d < 0) lo_dec_d=-lo_dec_d;
      if(hi_dec_d < 0) hi_dec_d=-hi_dec_d;

      /* compute ra & dec limits for the patch in degrees */
      lo_ra=(lo_ra_h+(double)lo_ra_m/60.+(double)lo_ra_s/3600.)*15.;
      lo_dec=(lo_dec_d+(double)lo_dec_m/60.);
      if(lo_dec_sign == '-') lo_dec=-lo_dec;
      hi_ra=(hi_ra_h+(double)hi_ra_m/60.+(double)hi_ra_s/3600.)*15.;
      hi_dec=(hi_dec_d+(double)hi_dec_m/60.);
      if(hi_dec_sign == '-') hi_dec=-hi_dec;

      if((hi_ra < .0001) && (hi_ra < lo_ra)) hi_ra=360.0;
      if(hi_dec < lo_dec){
        x=hi_dec;
        hi_dec=lo_dec;
        lo_dec=x;
      }
      if((hi_dec < lo_dec) || (hi_ra < lo_ra)){
         printf("warning: %f %f %f %f\n",lo_dec,hi_dec,lo_ra,hi_ra);
      }

/* debug only
      if((i < 10) || (i > 9530)){
         printf("i= %d\n",i);
         printf("%f %f %f %f\n",lo_dec,hi_dec,lo_ra,hi_ra);
         printf(" %-48.48s \n", buf);
         printf(
             "%5d %2d %2d %5.2f %2d %2d %5.2f %c %2d %4.1f %c %2d %4.1f\n",
            region,lo_ra_h,lo_ra_m,lo_ra_s,hi_ra_h,hi_ra_m,hi_ra_s,
             lo_dec_sign,lo_dec_d,lo_dec_m,hi_dec_sign,hi_dec_d,hi_dec_m);
      }
*/

      /* keep regions with coverage within the picture limits */
      if(hi_dec < min_dec)goto no_good;
      if(lo_dec > max_dec)goto no_good;
      if(max_ra - min_ra < 180.){       /* common case */
        if(hi_ra < min_ra)goto no_good;
        if(lo_ra > max_ra)goto no_good;
      }
      else {
        if(hi_ra < min_ra)goto keep_it; /* image brackets zero RA meridian */
        if(lo_ra < min_ra)goto keep_it;
        if(hi_ra > max_ra)goto keep_it;
        if(lo_ra > max_ra)goto keep_it;
        goto no_good;
      }
      keep_it:;
      nareas += 1;
      if(nareas > MAX_NAREAS){
        zvmessage("MAX_NAREAS exceeded"," ");
        fputs("MAX_NAREAS exceeded\n", stat_file);
        zabend();
      }
      strncpy(&regions[nareas-1][0],buf,5);
      save_region[nareas-1]=region;
      /*printf("region name is:%s\n",&regions[nareas-1][0]);*/
      no_good:;
   }   
   fclose(fd);
   printf("Searched %d areas in regions file\n",i-1);
   printf("Located %d overlapping areas in regions file\n",nareas);
   if(nareas == 0){
     fputs("No areas\n", stat_file);
     zabend();
   }

/* Search regions for objects */
   mag = 0.0;
   colour = 0.0;
   ntarget=0;
   m=0;
   for(i=0; i < nareas; i++){
     region=save_region[i];
     for(j=0; j < 24; j++){
       if((region_boundary[j][0] <= region) && 
          (region_boundary[j][1] >= region)){
         k=j;
         break;
       }
     }
     strcpy(filename,path);
     if(region < 4663)
        strcat(filename,"/usa_aura_stsi_gsc1_1001/gsc/");
     else
        strcat(filename,"/usa_aura_stsi_gsc1_1002/gsc/");
     strcat(filename,&directory_name[k][0]);
     strcat(filename,"/");
     strcat(filename,&regions[i][1]);
     strcat(filename,".gsc");
     if ((fd = fopen(filename, "r")) == NULL) {
        fprintf(stderr, "Error opening file %s\n",filename);
        fputs("Error opening file\n", stat_file);
        zabend();;
     }
     for (j = 0; j < 108; j++){
        fgets(buf, 81, fd);    /* skip first 107 80 byte card images */
     }
     for (j = 1; j < 10000; j++) {
       if (fgets(buf, 45+1, fd) == NULL) break; /* read 45 byte blocks */

       strncpy(msg,&buf[0],5); msg[5]='\0'; sscanf(msg,"%d",&k);
       strncpy(msg,&buf[5],9); msg[9]='\0'; sscanf(msg,"%lf",&RA_2);
       strncpy(msg,&buf[14],9); msg[9]='\0'; sscanf(msg,"%lf",&Dec_2);
       strncpy(msg,&buf[23],5); msg[5]='\0'; sscanf(msg,"%f",&pos_err);
       strncpy(msg,&buf[28],5); msg[5]='\0'; sscanf(msg,"%f",&magnitude);
       strncpy(msg,&buf[33],4); msg[4]='\0'; sscanf(msg,"%f",&mag_err);
       strncpy(msg,&buf[37],2); msg[2]='\0'; sscanf(msg,"%d",&mag_band);
       strncpy(msg,&buf[39],1); msg[1]='\0'; sscanf(msg,"%d",&class);
       if((j == 1) && (k != 1)){
         zvmessage("First object not numbered 1"," ");
         fputs("First object not numbered 1\n", stat_file);
         zabend();
       }

/* degug only
       if(j < 10){
          printf("%s\n",buf);
          printf("%d %f %f %f %f %f %d %d\n",
             k,RA_2,Dec_2,pos_err,magnitude,mag_err,mag_band,class);
       } 
*/

       if(k == m) goto outside_picture;  /* duplicate entry */
       m=k;

       /*  compute x,y plate location of target ra & dec */
       RA_2=RA_2*PI/180.;
       Dec_2=Dec_2*PI/180.;
       xypos(&header,RA_2,Dec_2,mag,colour,&x,&y);
       sample=x-x0+1.0;
       rline=nl-y+y0;

       /* check if inside picture */
       if(sample < 1.0) goto outside_picture;
       if(sample > ns) goto outside_picture;
       if(rline < 1.0) goto outside_picture;
       if(rline > nl) goto outside_picture;
       ntarget += 1;

       /* add an entry to the output catalogue */
       b_mag=-99.;
       v_mag=magnitude;
       prop_mot_ra=-99.;
       prop_mot_dec=-99.;
       strcpy(item_class,"none");
       if(class == 0)strcpy(item_class,"Star");
       if(class == 1)strcpy(item_class,"Galaxy");
       if(class == 2)strcpy(item_class,"blend");
       if(class == 3)strcpy(item_class,"nonstar");
       if(class == 5)strcpy(item_class,"artifact");
       strcpy(item_morphology,"none");
       strcpy(item_spclass,"none");
       strcpy(item_id,"Hubble_guide_star");
       RA_2=RA_2*180./PI;
       Dec_2=Dec_2*180./PI;
       sprintf(msg,"%d %16.10f %16.10f %f %f %f %f %f %f %s %s %s %s\n",
         ntarget,RA_2,Dec_2,rline,sample,b_mag,v_mag,prop_mot_ra,prop_mot_dec,
         item_class,item_morphology,item_spclass,item_id);
       if (fputs(msg, fout) == NULL) {
         fprintf(stderr, "Error writing file %s\n", catalogue);
         fputs("Error writing file\n", stat_file);
         zabend();
       }
       outside_picture:;
     }
     printf("%d catalogue targets inspected\n",j);
   }
   printf("%d G.S.C. objects located within image field\n",ntarget);
   n_gsc=ntarget;

/***************** GET SIMBAD CATALOGUE ****************************/

 hostname[0]='\0';
 service[0]='\0';
 hh = simbad_connect(hostname,service,userid,passwd) ;
 if (hh < 0)
   {
   printf("Simbad connection not done: %s.\n",simbad_error(0)) ;
   fputs("Simbad connection not done\n", stat_file);
   zabend();
   }
 printf("Simbad connection done: Handle = %d\n",hh) ;
 
 printf("plate center: %s\n",rec);
 printf("plate radius: %s\n",co_radius);
/* strcpy(rec,"2 46 20 -30 17 0");*/
/* strcpy(co_radius,"0 15");*/

 nitems = simbad_query(hh,rec,co_radius) ; /* get # objects within radius*/
 printf("%d Simbad objects found within search area.\n",nitems);
 if(nitems == 0)zvmessage("No objects located in the Simbad data base"," ");
 if(nitems < 0){
   j=simbad_errno(hh);
   if(j == 1)zvmessage("A network error occurred"," ");
   if(j == 2)zvmessage("A server error occurred"," ");
   if(j == 3)zvmessage("A client error occurred"," ");
   if(j == 11)zvmessage("A simbad error occurred"," ");
   zvmessage("Check your coordinate format"," ");
 }

 for(i=1; i <= nitems; i++){
 
     have_coordinate=0;
     strcpy(item_class,"none");
     strcpy(item_morphology,"none");
     strcpy(item_id,"none");
     strcpy(item_spclass,"none");
     b_mag=-99.;
     v_mag=-99.;
     prop_mot_ra=-99.;
     prop_mot_dec=-99.;
     status=simbad_retrieve(hh,0) ;  /* retrieve next object */
     if(status <= 0){
       continue;
     }
 
     astrotyplist = simbad_telldata(hh); /* get list of object data types */
     for (typ = strtok(astrotyplist," ") ;
          typ != NULL ;
          typ = strtok(NULL," ")){
 
        if(strcmp(typ,"J") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return b2000 coordinates */
          if(ndata > 0){
            p=simbad_getdata(hh,0);
            sscanf(p,"%lf %lf",&RA_2,&Dec_2);
            have_coordinate=1;
          }
        }
        if(strcmp(typ,"C") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return classification */
          if(ndata > 0){
            p=simbad_getdata(hh,0);
            strcpy(item_class,p);
            for(j=1; j < strlen(item_class); j++){
              if(isgraph(item_class[j])){
              }
              else {
                item_class[j] = '_';
              }
            }
          }
        }
        if(strcmp(typ,"M.B") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return b magnitude */
          if(ndata > 0){
            p=simbad_getdata(hh,0);
            sscanf(p,"%lf",&b_mag);
          }
        }
        if(strcmp(typ,"M.V") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return v magnitude */
          if(ndata > 0){
            p=simbad_getdata(hh,0);
            sscanf(p,"%lf",&v_mag);
          }
        }
        if(strcmp(typ,"S") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return spectral class */
          if(ndata > 0){
            p=simbad_getdata(hh,0);
            strcpy(item_spclass,p);
            for(j=1; j < strlen(item_spclass); j++){
              if(isgraph(item_spclass[j])){
              }
              else {
                item_spclass[j] = '_';
              }
            }
          }
        }
        if(strcmp(typ,"T") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return morphology */
          if(ndata > 0){
            p=simbad_getdata(hh,0);
            strcpy(item_morphology,p);
            for(j=1; j < strlen(item_morphology); j++){
              if(isgraph(item_morphology[j])){
              }
              else {
                item_morphology[j] = '_';
              }
            }
          }
        }
        if(strcmp(typ,"P") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return proper motion */
          if(ndata > 0){
            p=simbad_getdata(hh,0);
            strcpy(msg,p);
            sscanf(msg,"%lf",&prop_mot_ra);
            for(j=2; j < strlen(msg); j++){
              if((msg[j] == '+') || (msg[j] == '-'))
                sscanf(&msg[j],"%lf",&prop_mot_dec);
            }
          }
        }
        if(strcmp(typ,"I.0") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return identifications */
          if(ndata > 0){
            p=simbad_getdata(hh,0);
            strcpy(item_id,p);
            for(j=1; j < strlen(item_id); j++){
              if(isgraph(item_id[j])){
              }
              else {
                item_id[j] = '_';
              }
            }
          }
        }
     }
     /*  compute x,y plate location of target ra & dec */
     if(have_coordinate == 0)continue;  /* no ra & dec */
     RA_2=RA_2*PI/180.;
     Dec_2=Dec_2*PI/180.;
     xypos(&header,RA_2,Dec_2,mag,colour,&x,&y);
     sample=x-x0+1.0;
     rline=nl-y+y0;

     /* check if inside picture */
     if(sample < 1.0) continue;
     if(sample > ns) continue;
     if(rline < 1.0) continue;
     if(rline > nl) continue;
     /* write object record */
     ntarget += 1;
     sprintf(msg,"%d",ntarget);
     RA_2=RA_2*180./PI;
     Dec_2=Dec_2*180./PI;
     sprintf(msg,"%d %16.10f %16.10f %f %f %f %f %f %f %s %s %s %s\n",
       ntarget,RA_2,Dec_2,rline,sample,b_mag,v_mag,prop_mot_ra,prop_mot_dec,
       item_class,item_morphology,item_spclass,item_id);

     /* add an entry to the output catalogue */
     if (fputs(msg, fout) == NULL) {
       fprintf(stderr, "Error writing file %s\n", catalogue);
       fputs("Error writing file\n", stat_file);
       zabend();
     }
 }
 printf("%d Simbad objects located within image field\n",ntarget-n_gsc);
 simbad_disconnect(hh) ;
 fclose(fout); /* output catalogue */

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
