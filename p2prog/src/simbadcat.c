/* skycat */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "vicmain_c"
#include <simcli.h>
#include <mdms_pwdclient.h>

#define PI 3.14159265359

void main44(void)
{

/* for SIMBAD */
 char hostname[256], service[8],userid[MAX_USERID+1], passwd[MAX_USERID+1] ;
 char rec[256],co_radius[100],*astrotyplist,*typ,*p;
 int i,j,hh, nitems, ndata, status;
 
/* for KERBEROS */
 char errorBuff[PWDMAXERRSTRLEN];
 char password[256],userName[L_cuserid];
 char *cuserid_p2();
 int errorNum;

   FILE *fout;
   char catalogue[80];
   char name[20],msg[200];
   char temp1[10],temp2[10];
   char item_spclass[50],item_class[50],item_morphology[50],item_id[50];
   unsigned char cobuf[100];
   int count,def,ntarget;
   int unit,nl,ns,k,m,n,have_coordinate;
   int mag_band,class,n_gsc,rah,ram,decd,decm; 
   double Dec_1,Dec_2,RA_1,RA_2;
   double b_mag,v_mag,prop_mot_ra,prop_mot_dec,Separation;
   float mag, colour,x,y,x0,y0;
   float rline,sample;
   float pos_err,magnitude,mag_err,ras,decs;

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
   status=zvparm("PASSWORD",passwd,&count,&def,1,0);
   status=zvparm("USERID",userid,&count,&def,1,0);
   status=zvparm("NAME",rec,&count,&def,1,0);

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

/* open output ascii catalogue */
   status=zvpone("OUT",catalogue,1,sizeof(catalogue));
   if ((fout = fopen(catalogue, "w")) == NULL) {
      fprintf(stderr, "Error opening file %s\n", catalogue);
      fputs("Error opening output catalogue\n", stat_file);
      zabend();
   }

 fputs("#   RA       Dec     line      sample   b_mag    v_mag   prop_mot_ra  prop_mot_dec  classification  morphology  spectrum  name\n", fout);

 hostname[0]='\0';
 service[0]='\0';
 co_radius[0]='\0';
 hh = simbad_connect(hostname,service,userid,passwd) ;
 if (hh < 0)
   {
   printf("Simbad connection not done: %s.\n",simbad_error(0)) ;
   fputs("Simbad connection not done\n", stat_file);
   zabend();
   }
 printf("Simbad connection done: Handle = %d\n",hh) ;
 
 nitems = simbad_query(hh,rec,co_radius) ; /* get # objects within radius*/
 if(nitems == 0)zvmessage("No object located in the Simbad data base"," ");
 if(nitems < 0){
   j=simbad_errno(hh);
   if(j == 1)zvmessage("A network error occurred"," ");
   if(j == 2)zvmessage("A server error occurred"," ");
   if(j == 3)zvmessage("A client error occurred"," ");
   if(j == 11)zvmessage("A simbad error occurred"," ");
   zvmessage("Check your object name"," ");
 }

 ntarget=0;
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
     sample=-99.0;
     rline=-99.0;
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

     /* write object record */
     ntarget += 1;
     sprintf(msg,"%d",ntarget);
     sprintf(msg,"%d %16.10f %16.10f %f %f %f %f %f %f %s %s %s %s\n",
       ntarget,RA_2,Dec_2,rline,sample,b_mag,v_mag,prop_mot_ra,prop_mot_dec,
       item_class,item_morphology,item_spclass,item_id);

     /* add an entry to the output catalogue */
     if (fputs(msg, fout) == NULL) {
       fprintf(stderr, "Error writing file %s\n", catalogue);
       fputs("Error writing catalogue\n", stat_file);
       zabend();
     }
 }
 printf("%d Simbad objects located\n",ntarget);
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
