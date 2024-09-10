/* skycat */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "mp_routines.h"
#include "header.h"
#include "qfile.h"
#include "vicmain_c"

#define PI 3.14159265359
#define MAX_NS 4500
#define MAX_NS_OUT 17000
#define INTERVAL 50

extern void pltlist();
extern int  gethdr();
extern void condmsrd();
extern void conrddms();
extern void b1950_j2000();
extern int  getradec();
extern int  writehhh();
extern int  writefits();
void  DSIMQ();

void main44(void)
{
   MP mp;
   Header header;
   FILE *fin,*fout,*fpin,*fcin;
   char path[80],radec[80],filename[80],msg[200],temp[80];
   char Rgn[10],Plate[10],Survey[10],best_Rgn[10];
   char object[75],scr[2],catalogue_in[80],catalogue_out[80];
   char name[20],buf[81],pix_filename[100],cat_filename[100];
   char *p,listoffiles[100];
   char Dec_sign[2];
   char lo_dec_sign,hi_dec_sign,decsign;
   char temp1[10],temp2[10];
   char item_spclass[50],item_class[50],item_morphology[50],item_id[50];
   unsigned char cobuf[100];
   short int obuf[MAX_NS_OUT];
   short int n_pts_good[(MAX_NS_OUT/INTERVAL)+2];
   short int **image;
   int count,def,line,ntarget;
   int unit,ounit,nl,ns,nl_in,ns_in,i,j,k,m,n,have_coordinate;
   int lo_ra_h,lo_ra_m,hi_ra_h,hi_ra_m,region;
   int lo_dec_d,hi_dec_d,nareas;
   int mag_band,class,n_gsc,rah,ram,decd,decm; 
   int nids,nods,status,file,next_interval,ind,each,k_line;
   int n_columns,n_rows;
   double Dec_1,Dec_2,RA_1,RA_2,dline,dsample;
   double min_dec,max_dec,min_ra,max_ra;
   double ra_pix[4],dec_pix[4];
   double lo_ra,hi_ra,lo_dec,hi_dec;
   double b_mag,v_mag,prop_mot_ra,prop_mot_dec,Separation;
   double a[17],b[17],coefx[5],coefy[5];
   double ycoef[(MAX_NS_OUT/INTERVAL)+2][4],xcoef[(MAX_NS_OUT/INTERVAL)+2][4];
   float y_coord[(MAX_NS_OUT/INTERVAL)+2][2],x_coord[(MAX_NS_OUT/INTERVAL)+2][2];
   float s_coord[(MAX_NS_OUT/INTERVAL)+2],l_coord[(MAX_NS_OUT/INTERVAL)+2];
   float lo_ra_s,hi_ra_s,lo_dec_m,hi_dec_m;
   float mag, colour,x,y,x0,y0;
   float RA_s,Dec_s,rline,sample;
   float pos_err,magnitude,mag_err,ras,decs;
   float wl,wr,top,bot;

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

/* create 2-d array */
   image = (short int **)malloc(MAX_NS * sizeof(short int *));
   for (i = 0; i < MAX_NS; i++) {
        image[i] = (short int *)malloc(MAX_NS * sizeof(short int));
   }

/* vicar parameters */
   status=zvpcnt("INP",&nids);
   status=zvpcnt("OUT",&nods);
   status=zvparm("MODE",msg,&count,&def,1,0);
   each=0;
   if((strcmp(msg,"exact") == 0) || (strcmp(msg,"EXACT") == 0))each=1;
   status=zveaction("AS","  ");

/* open input vicar file with map label*/
   status=zvunit(&unit,"INP",1, NULL);
   zvsignal(unit,status,1);
   status=zvopen(unit, NULL);
   zvsignal(unit,status,1);
   status=zvget(unit,"NL",&nl,"NS",&ns, NULL);
   zvsignal(unit,status,1);
   if(ns >= MAX_NS_OUT){
     zvmessage("Output projection too large "," ");
     fputs("Output projection too large\n", stat_file);
     zabend();
   }

/* extract the label & close the unit */
   status=mpInit(&mp);
   status=mpLabelRead(mp,unit);
   status=zvclose(unit,"CLOS_ACT","FREE", NULL);

/* open output vicar file */
   status=zvunit(&ounit,"OUT",1, NULL);
   zvsignal(ounit,status,1);
   status=zvopen(ounit,"OP","WRITE","U_NL",nl,"U_NS",ns,
                      "U_FORMAT","HALF","O_FORMAT","HALF", NULL);
   zvsignal(ounit,status,1);

/* write zero's in the output */
   for(j=0; j < ns; j++) obuf[j]=0;
   for(j=1; j <= nl; j++){
     status=zvwrit(ounit,obuf,"LINE",j, NULL);
     zvsignal(ounit,status,1);
   }

/* reopen output for update */
   status=zvclose(ounit, NULL);
   zvsignal(ounit,status,1);
   status=zvopen(ounit,"OP","UPDATE","U_NL",nl,"U_NS",ns,
                      "U_FORMAT","HALF","O_FORMAT","HALF", NULL);
   zvsignal(ounit,status,1);

/* open input ascii list of pictures */
   status=zvpone("INP",listoffiles,2,sizeof(listoffiles));
   if ((fpin = fopen(listoffiles, "r")) == NULL) {
      fprintf(stderr, "Error opening list of pictures %s\n", listoffiles);
      fputs("Error opening list of pictures\n", stat_file);
      zabend();
   }

   if((nids > 2) && (nods > 1)){
     /* open input ascii list of catalogues */
     status=zvpone("INP",catalogue_in,3,sizeof(catalogue_in));
     if ((fin = fopen(catalogue_in, "r")) == NULL) {
        fprintf(stderr, "Error opening list of catalogues %s\n", catalogue_in);
        fputs("Error opening list of catalogues\n", stat_file);
        zabend();
     }
     /* open output ascii catalogue */
     status=zvpone("OUT",catalogue_out,2,sizeof(catalogue_out));
     if ((fout = fopen(catalogue_out, "w")) == NULL) {
        fprintf(stderr, "Error opening output catalogue %s\n", catalogue_out);
        fputs("Error opening output catalogue\n", stat_file);
        zabend();
     }
   }

/********************* MAIN PICTURE LOOP ***************************/
 for(file = 1; file < 999999; file++){

   /* read picture filename */
   if(fgets(pix_filename, sizeof(pix_filename), fpin) == NULL)break;
   for(j=0; j<sizeof(pix_filename); j++) /* clear non printing stuff */
      if(isprint(pix_filename[j]) == 0 )pix_filename[j]='\0';

   printf("processing file %s\n",pix_filename);

   /* open Palomar Sky Survey input vicar file with image to be projected*/
   status=zvunit(&unit,"OLD",file,"U_NAME",pix_filename, NULL);
   zvsignal(unit,status,1);
   status=zvopen(unit,"U_FORMAT","HALF", NULL);
   zvsignal(unit,status,1);
   status=zvget(unit,"NL",&nl_in,"NS",&ns_in, NULL);
   zvsignal(unit,status,1);
   if((nl_in >= MAX_NS) || (ns_in >= MAX_NS)){
     zvmessage("Palomar Sky Survey image too large "," ");
     fputs("Palomar Sky Survey image too large\n", stat_file);
     zabend();
   }

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

/* read input picture into memory */
   for(j=0; j<nl_in; j++){
     status=zvread(unit,&image[j][0],"LINE",j+1, NULL);
     zvsignal(unit,status,1);
   }

/* create a row of tiepoints, line,sample in input image which come from
      a grid along the output at line 1 */
   if(each == 0){
     /* number of grid intersections per row */
     for(k=1; k < 999999; k++){
       if((k-1)*INTERVAL+1 < ns){
         s_coord[k]=(k-1)*INTERVAL+1;
       }
       else{
         s_coord[k]=ns;
         n_columns=k;
         break;
       }
     }
     /* number of grid intersections per column */
     for(k=1; k < 999999; k++){
       if((k-1)*INTERVAL+1 < nl){
         l_coord[k]=(k-1)*INTERVAL+1;
       }
       else{
         l_coord[k]=nl;
         n_rows=k;
         break;
       }
     }
     dline=l_coord[1];;
     for(k=1; k <= n_columns; k++){
         y_coord[k][1]=-1.0;
         x_coord[k][1]=-1.0;
         dsample=s_coord[k];
         /* convert projection line,sample to RA,Dec. */
         status=mpxy2ll(mp,dline,dsample,&Dec_1,&RA_1,1);
         if(status != 0) continue;
         /* convert ra,dec to line,sample in PSS image*/
         RA_2=RA_1*PI/180.;
         Dec_2=Dec_1*PI/180.;
         xypos(&header,RA_2,Dec_2,mag,colour,&x,&y);
         sample=x-x0+1.0;
         rline=nl_in-y+y0;
         if((sample < 1.0) || (sample > ns_in) ||
           (rline < 1.0) || (rline > nl_in)) continue;
         y_coord[k][1]=rline;
         x_coord[k][1]=sample;
     }
     next_interval=1;
   }

/* map project image */
   for(j=1; j <= nl; j++){

     /* create a row of tiepoints, line,sample in input image which come from
        a grid along the output at line j+INTERVAL  */
     if(each == 0){
       if(j == next_interval){
         /* copy bottom grid to top row */
         for(k=1; k <= n_columns; k++){ 
           y_coord[k][0]=y_coord[k][1];
           x_coord[k][0]=x_coord[k][1];
         }
         k_line=((j-1)/INTERVAL)+1;
         dline=l_coord[k_line+1];
         for(k=1; k <= n_columns; k++){
           y_coord[k][1]=-1.0; /* value if projection is bad */
           x_coord[k][1]=-1.0;
           dsample=s_coord[k];
           /* convert projection line,sample to RA,Dec. */
           status=mpxy2ll(mp,dline,dsample,&Dec_1,&RA_1,1);
           if(status != 0) continue;
           /* convert ra,dec to line,sample in PSS image*/
           RA_2=RA_1*PI/180.;
           Dec_2=Dec_1*PI/180.;
           xypos(&header,RA_2,Dec_2,mag,colour,&x,&y);
           sample=x-x0+1.0;
           rline=nl_in-y+y0;
           y_coord[k][1]=rline;
           x_coord[k][1]=sample;
         }
         next_interval += INTERVAL;

        /* compute polynomial fit from projection to input image. */
         for(k=1; k < n_columns; k++){
           a[1]=s_coord[k]; /* upper left */
           a[5]=l_coord[k_line];
           a[9]=(double)s_coord[k]*(double)l_coord[k_line];
           a[13]=1.0;
           coefy[1]=y_coord[k][0];
           coefx[1]=x_coord[k][0];
           a[2]=s_coord[k+1]; /* upper right */
           a[6]=l_coord[k_line];
           a[10]=(double)l_coord[k_line]*(double)s_coord[k+1];
           a[14]=1.0;
           coefy[2]=y_coord[k+1][0];
           coefx[2]=x_coord[k+1][0];
           a[3]=s_coord[k]; /* lower left */
           a[7]=l_coord[k_line+1];
           a[11]=(double)s_coord[k]*(double)l_coord[k_line+1];
           a[15]=1.0;
           coefy[3]=y_coord[k][1];
           coefx[3]=x_coord[k][1];
           a[4]=s_coord[k+1]; /* lower right */
           a[8]=l_coord[k_line+1];
           a[12]=(double)l_coord[k_line+1]*(double)s_coord[k+1];
           a[16]=1.0;
           coefy[4]=y_coord[k+1][1];
           coefx[4]=x_coord[k+1][1];
           for(n=1; n <= 17; n++){
             b[n]=a[n];
           }
           /* check if projection itself is undefined */
           n=4;
           if((y_coord[k][0] == -1.0) && (x_coord[k][0] == -1.0))n -= 1;
           if((y_coord[k+1][0] == -1.0) && (x_coord[k+1][0] == -1.0))n -= 1;
           if((y_coord[k][1] == -1.0) && (x_coord[k][1] == -1.0))n -= 1;
           if((y_coord[k+1][1] == -1.0) && (x_coord[k+1][1] == -1.0))n -= 1;
           n_pts_good[k]=n;
           /* check if all 4 corners fall outside input image */
           if(n_pts_good[k] == 4){
             n=4;
             if((x_coord[k][0] < 1.0) || (x_coord[k][0] > ns_in) || 
                (y_coord[k][0] < 1.0) || (y_coord[k][0] > nl_in)) n -= 1;
             if((x_coord[k+1][0] < 1.0) || (x_coord[k+1][0] > ns_in) || 
                (y_coord[k+1][0] < 1.0) || (y_coord[k+1][0] > nl_in)) n -= 1;
             if((x_coord[k][1] < 1.0) || (x_coord[k][1] > ns_in) || 
                (y_coord[k][1] < 1.0) || (y_coord[k][1] > nl_in)) n -= 1;
             if((x_coord[k+1][1] < 1.0) || (x_coord[k+1][1] > ns_in) || 
                (y_coord[k+1][1] < 1.0) || (y_coord[k+1][1] > nl_in)) n -= 1;
             if(n == 0)n_pts_good[k]=5;
           }
           if(n_pts_good[k] == 4){
             DSIMQ(a,coefx,4,&ind);
             if(ind != 0){
               zvmessage("DSIMQ error"," ");
               fputs("DSIMQ error\n", stat_file);
               zabend();
             }
             DSIMQ(b,coefy,4,&ind);
             if(ind != 0){
               zvmessage("DSIMQ error"," ");
               fputs("DSIMQ error\n", stat_file);
               zabend();
             }
             for(n=0; n < 4; n++){
               xcoef[k][n]=coefx[n+1];
               ycoef[k][n]=coefy[n+1];
             }
           }
         }
       }
     }

     dline=j;
     status=zvread(ounit,&obuf[1],"LINE",j, NULL); /* re read output line */

     for(i=1; i <= ns; i++){
       if(obuf[i] != 0)continue;  /* pixel already filled */
       dsample=i;

       if(each == 0){               /* checks */
         k=((i-1)/INTERVAL)+1;      /* compute grid number */
         if(n_pts_good[k] == 5){    /* 4 corners off image, skip pixel */
           continue;
         }
         if(n_pts_good[k] == 0){    /* 4 corners unprojectable, don't revisit pixel */
           obuf[i]=-1;
           continue;
         }
       }

       if((each == 1) || (n_pts_good[k] < 4)){   /* each pixel */
         /* convert projection line,sample to RA,Dec. */
         status=mpxy2ll(mp,dline,dsample,&Dec_1,&RA_1,1);
         if(status != 0) continue;
         /* convert ra,dec to line,sample in PSS image*/
         RA_2=RA_1*PI/180.;
         Dec_2=Dec_1*PI/180.;
         xypos(&header,RA_2,Dec_2,mag,colour,&x,&y);
         sample=x-x0+1.0;
         rline=nl_in-y+y0;
       }
       else{                /* polynomial approximation */
         sample=dsample*xcoef[k][0]+dline*xcoef[k][1]+
                dsample*dline*xcoef[k][2]+xcoef[k][3];
         rline=dsample*ycoef[k][0]+dline*ycoef[k][1]+
                dsample*dline*ycoef[k][2]+ycoef[k][3];
       }
       if((sample < 1.0) || (sample > ns_in) || 
         (rline < 1.0) || (rline > nl_in)) continue;
  
       /* get dn value by interpolation */
       m=sample;
       n=rline;
       wl=(sample-m);
       wr=1.0-wl;
       top=wl*image[n][m+1]+wr*image[n][m];
       bot=wl*image[n+1][m+1]+wr*image[n+1][m];
       obuf[i]=bot*(rline-n)+top*(n+1-rline);
     }
     status=zvwrit(ounit,&obuf[1],"LINE",j, NULL);
   }
   status=zvclose(unit,"CLOS_ACT","FREE", NULL); /* input picture */

   /* concatenate & convert oputput catalogue */
   if((nids > 2) && (nods > 1)){

     /* read catalogue filename */
     if(fgets(cat_filename, sizeof(pix_filename), fin) == NULL){
       fprintf(stderr, "Error reading catalogue file record  %d\n", file);
       fputs("Error reading catalogue file record\n", stat_file);
       zabend();
     }
     for(j=0; j<sizeof(cat_filename); j++) /* clear non printing stuff */
      if(isprint(cat_filename[j]) == 0 )cat_filename[j]='\0';

     /* open input ascii catalogue */
     if ((fcin = fopen(cat_filename, "r")) == NULL) {
        fprintf(stderr, "Error opening file %s\n", catalogue_in);
        fputs("Error opening file\n", stat_file);
        zabend();
     }

     if(file == 1){
       /* copy header record to output once only*/
       fgets(msg, sizeof(msg), fcin);
       fputs(msg, fout);
       ntarget=0;
     }
     else{
       /* skip header record */
       fgets(msg, sizeof(msg), fcin);
     }

     for(i=0; i<9999999; i++){

       /* unpack input record */
       if (fgets(msg, sizeof(msg), fcin) == NULL) break;
       sscanf(msg,"%d %lf %lf %f %f %lf %lf %lf %lf %s %s %s %s",
         &j,&RA_2,&Dec_2,&rline,&sample,&b_mag,&v_mag,
         &prop_mot_ra,&prop_mot_dec,
         item_class,item_morphology,item_spclass,item_id);

       /* convert ra & dec to projection line & sample */
       status=mpll2xy(mp,&dline,&dsample,Dec_2,RA_2,1);
       if(status != 0)continue;
       if(dsample < 1.0) continue;
       if(dsample > ns) continue;
       if(dline < 1.0) continue;
       if(dline > nl) continue;

       /* write object record */
       ntarget += 1;
       rline=dline;
       sample=dsample;
       sprintf(msg,"%d %16.10f %16.10f %f %f %f %f %f %f %s %s %s %s\n",
         ntarget,RA_2,Dec_2,rline,sample,b_mag,v_mag,prop_mot_ra,prop_mot_dec,
         item_class,item_morphology,item_spclass,item_id);

       /* add an entry to the output catalogue */
       if (fputs(msg, fout) == NULL) {
         fprintf(stderr, "Error writing file %s\n", catalogue_out);
         fputs("Error writing file\n", stat_file);
         zabend();
       }
     }
     fclose(fcin); /* input catalogue */
   }
 } 
 if((nids > 2) && (nods > 1))fclose(fout); /* output catalogue */

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

/*********************************************************************
C        PURPOSE
C           OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS,
C           AX=B
C
C        USAGE
C          SIMQ(A,B,N,KS)
C
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS
 
c      do i=1,4
c         a(i,1)=x
c         a(i,2)=y
c         a(i,3)=dble(x)*dble(y)
c         a(i,4)=1.d0
c         coefy(i)=ynew
c         coefx(i)=xnew
c      enddo
c      call mve(8,16,a,b,1,1)
c
c      call dsimq(a,coefx,4,ind)
c      if(ind.ne.0)then
c         call ifmessage('DSIMQ: singular polynomial solution, abort',)
c         return
c      endif
c      call dsimq(b,coefy,4,ind)
c      if(ind.ne.0)then
c         call ifmessage('DSIMQ: singular polynomial solution, abort')
c         return
c      endif
c
c      xnew=x*coefx(1)+y*coefx(2)+dble(x)*dble(y)* coefx(3)+coefx(4)
c      ynew=x*coefy(1)+y*coefy(2)+dble(x)*dble(y)*coefy(3)+coefy(4)
 
***********************************************************************/
void  DSIMQ(A,B,N,KS)
 double A[17],B[5];   /* for N=4, dimension is N+1 */
 int N,*KS;
{
 double BIGA,SAVE,TOL;
 int JX,JJ,JY,J,IT,I,IJ,IMAX,I1,K,I2,IQS,IX,IXJ,IXJX,JJX,NY,IA,IB,IC;
 
      TOL=0.0;
      *KS=0;
      JJ=-N;
      for(J=1; J <= N; J++){
        JY=J+1;
        JJ=JJ+N+1;
        BIGA=0.0;
        IT=JJ-J;
        for(I=J; I <= N; I++){
          IJ=IT+I;
          if(fabs(BIGA)-fabs(A[IJ]) < 0.0){
            BIGA=A[IJ];
            IMAX=I;
          }
        }
        if(fabs(BIGA)-TOL <= 0.0){
          *KS=1;
          return;
        }
        I1=J+N*(J-2);
        IT=IMAX-J;
        for(K=J; K <= N; K++){
          I1=I1+N;
          I2=I1+IT;
          SAVE=A[I1];
          A[I1]=A[I2];
          A[I2]=SAVE;
          A[I1]=A[I1]/BIGA;
        }
        SAVE=B[IMAX];
        B[IMAX]=B[J];
        B[J]=SAVE/BIGA;
        if(J-N == 0)goto seventy;
        IQS=N*(J-1);
        for(IX=JY; IX <= N; IX++){
          IXJ=IQS+IX;
          IT=J-IX;
          for(JX=JY; JX <= N; JX++){
            IXJX=N*(JX-1)+IX;
            JJX=IXJX+IT;
            A[IXJX]=A[IXJX]-(A[IXJ]*A[JJX]);
          }
          B[IX]=B[IX]-(B[J]*A[IXJ]);
        }
      }
      seventy:;
      NY=N-1;
      IT=N*N;
      for(J=1; J <= NY; J++){
        IA=IT-J;
        IB=N-J;
        IC=N;
          for(K=1; K <= J; K++){
            B[IB]=B[IB]-A[IA]*B[IC];
            IA=IA-N;
            IC=IC-1;
          }
      }
}
