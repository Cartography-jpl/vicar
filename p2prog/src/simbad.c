/* mpfcahv */
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "vicmain_c"
#include <string.h>
#include <simcli.h>
#include <mdms_pwdclient.h>

void main44(void)
{
 int count,count1,count2,def;
 char msg[100];

/* for SIMBAD */
 char hostname[256], service[8],userid[MAX_USERID+1], passwd[MAX_USERID+1] ;
 char rec[256],co_radius[100],*astrotyplist,*typ,*p;
 int i,j,hh, nitems, ndata, status;

/* for KERBEROS */
 char errorBuff[PWDMAXERRSTRLEN];
 char password[256],userName[L_cuserid];
 char *cuserid_p2();
 int errorNum;

/* get parameters */
 status=zvparm("PASSWORD",passwd,&count,&def,1,0);
 status=zvparm("USERID",userid,&count,&def,1,0);
 status=zvparm("NAME",rec,&count1,&def,1,0);
 status=zvparm("LOCATION",rec,&count2,&def,1,0);
 status=zvparm("RADIUS",co_radius,&count,&def,1,0);
 if(count1+count2 != 1){
   zvmessage("Must specify either NAME or LOCATION"," ");
   zabend();
 }

/* get password from password server through a kerberos ticket */
 if(strcmp(passwd,"none") == 0){
   strcpy(userName, cuserid_p2());
   errorNum = mdms_passwordSrvGet (userName,"simbad", password);
   if(errorNum < PWDSUCCESS){
     zvmessage("Cannot obtain kerberos password, you must first:"," ");
     zvmessage("selcat o"," ");
     zvmessage("then run kinit to get a kerberos ticket"," ");
     zabend();
   }
   else{
     strcpy(passwd,password);
   }
 }

 hostname[0]='\0';
 service[0]='\0';
 hh = simbad_connect(hostname,service,userid,passwd) ;
 if (hh < 0)
   {
   printf("Connection not done: %s.\n",simbad_error(0)) ;
   zabend();
   }
 printf("Connection done: Handle = %d\n",hh) ;
 
 if(count2 > 0){
   strcpy(msg,"cooradius=");
   strcat(msg,co_radius);
   nitems = simbad_query(hh,rec,msg) ; /* get # objects within radius*/
   printf("%d objects found within search area.\n",nitems);
 }
 else{
   nitems = simbad_query(hh,rec,"");
 }
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

     printf("   \n");
     printf("Object %d\n",i);
     status=simbad_retrieve(hh,0) ;  /* retrieve next object */
     if(status <= 0){
       zvmessage("Cannot locate anything for this object"," ");
       continue;
     }

     astrotyplist = simbad_telldata(hh); /* get list of object data types */
     for (typ = strtok(astrotyplist," ") ;
          typ != NULL ;
          typ = strtok(NULL," ")){

        if(strcmp(typ,"C") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return classification */
          for(j=1; j <= ndata; j++){
            p=simbad_getdata(hh,0);
            printf("classification: %s\n",p);
          }
        }
        if(strcmp(typ,"J") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return b2000 coordinates */
          for(j=1; j <= ndata; j++){
            p=simbad_getdata(hh,0);
            printf("RA,Dec (decimal deg,b2000): %s\n",p);
          }
        }
        if(strcmp(typ,"M.B") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return b magnitude */
          for(j=1; j <= ndata; j++){
            p=simbad_getdata(hh,0);
            printf("B magnitude: %s\n",p);
          }
        }
        if(strcmp(typ,"M.V") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return v magnitude */
          for(j=1; j <= ndata; j++){
            p=simbad_getdata(hh,0);
            printf("V magnitude: %s\n",p);
          }
        }
        if(strcmp(typ,"S") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return spectral class */
          for(j=1; j <= ndata; j++){
            p=simbad_getdata(hh,0);
            printf("Spectral class: %s\n",p);
          }
        }
        if(strcmp(typ,"T") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return morphology */
          for(j=1; j <= ndata; j++){
            p=simbad_getdata(hh,0);
            printf("Morphology class: %s\n",p);
          }
        }
        if(strcmp(typ,"P") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return proper motion */
          for(j=1; j <= ndata; j++){
            p=simbad_getdata(hh,0);
            printf("Proper motion (sec arc RA*cosDec,Dec: %s\n",p);
          }
        }
        if(strcmp(typ,"D") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return diameter */
          for(j=1; j <= ndata; j++){
            p=simbad_getdata(hh,0);
            printf("Log mag25 diameter (major,minor,pa): %s\n",p);
          }
        }
        if(strcmp(typ,"I.0") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return identifications */
          for(j=1; j <= ndata; j++){
            p=simbad_getdata(hh,0);
            printf("Principal identification: %s\n",p);
          }
        }
     }
 }

 simbad_disconnect(hh) ;
}
