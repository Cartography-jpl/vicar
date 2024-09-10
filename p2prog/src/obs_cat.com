$!****************************************************************************
$!
$! Build proc for MIPL module obs_cat
$! VPACK Version 1.9, Thursday, August 27, 1998, 14:40:45
$!
$! Execute by entering:		$ @obs_cat
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
$ write sys$output "*** module obs_cat ***"
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
$ write sys$output "Invalid argument given to obs_cat.com file -- ", primary
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
$   if F$SEARCH("obs_cat.imake") .nes. ""
$   then
$      vimake obs_cat
$      purge obs_cat.bld
$   else
$      if F$SEARCH("obs_cat.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake obs_cat
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @obs_cat.bld "STD"
$   else
$      @obs_cat.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create obs_cat.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack obs_cat.com -
	-s obs_cat.c -
	-i obs_cat.imake -
	-p obs_cat.pdf -
	-t tstobs_cat.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create obs_cat.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* obs_cat */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "vicmain_c"
#include <simcli.h>
#include <mdms_pwdclient.h>

#define PI 3.141592653589793238
#define MAXTABLE 1000
#define SOLRAD_AU 0.004655
#define EPOCH_HIP 1991.25

main44()
{

/* for SIMBAD */
 char hostname[256], service[8],userid[MAX_USERID+1], passwd[MAX_USERID+1] ;
 char rec[256],co_radius[100],*astrotyplist,*typ,*p;
 int i,j,hh, nitems, ndata, status;
 
/* for KERBEROS */
 char errorBuff[PWDMAXERRSTRLEN];
 char password[256],userName[L_cuserid];
 int errorNum;

/* hipparcos */
 char hip_name[2];
 int hip_number;
 double hip_ra_j1991,hip_dec_j1991;

/* general */
   FILE *fout,*fd;
   char *ptr;
   char catalogue[80],path[80],buf[500];
   char name[20],msg[200];
   char temp1[10],temp2[10],basename[20];
   char item_spclass[50],item_class[50],item_morphology[50],item_id[50];
   char decsign,Hip_sp_type[MAXTABLE][20],simbad_description[MAXTABLE][50];
   char hip_sp_type[20],star_ctype,color[15];
   char hip_annex[3],hip_quality[3],owner[30],obser_mode[10],obj_type;
   char Hip_annex[MAXTABLE][3],Hip_quality[MAXTABLE][3];
   char local_time[50],coordinated_universal_time[50];
   unsigned char cobuf[100];
   int hip_components,Hip_components[MAXTABLE];
   int count,def,ntarget,hip_ids_scr;
   int unit,nl,ns,k,m,n,have_coordinate,number,hd_number;
   int mag_band,class,n_gsc,rah,ram,decd,decm; 
   int Hip_ids[MAXTABLE],luminosity[6]; 
   int star_itype,star_iclass,found_target,hip_count,hd_count;
   int repeats,priority,spacing,time_obs,obs_mode;
   double Dec_1,Dec_2,RA_1,RA_2;
   double b_mag,v_mag,prop_mot_ra,prop_mot_dec,Separation;
   double fov,nearest,hip_ra_scr,hip_dec_scr,dist_scr;
   double Hip_ra[MAXTABLE],Hip_dec[MAXTABLE],distance[MAXTABLE];
   double Hip_pm_ra[MAXTABLE],Hip_pm_dec[MAXTABLE],Hip_V_mag[MAXTABLE];
   double Star_kmag[MAXTABLE],Hip_parallax[MAXTABLE],Star_eas[MAXTABLE];
   double hip_V_mag,hip_parallax,hip_pm_ra,hip_pm_dec;
   double star_kmag,star_lsize,star_eas;
   double v_mag_limit,k_mag_limit,lo_dec_limit,hi_dec_limit,par_limit;
   float mag, colour,x,y,x0,y0;
   float rline,sample;
   float pos_err,magnitude,mag_err,ras,decs;

   struct tm *time_structure;
   time_t sec;
   char *cuserid_p2();

   static char version[]="  Obs_cat_version=1\n";
   static char header[]=
"#  HD          RA(2000.0) dec(2000.0)  pmRA    pmDEC   V    K   SpTyp  Separ Paral  Angle O C Flag  Simbad\n";

static double VK_V[6][10]={ /* V-K magnitudes for luminosity class V */
 {-99.,-99.,-99.,-0.75,-0.67,-0.59,-0.51,-0.43,-0.35,-0.175},  /*B3-B9*/
 {0.0,0.07,0.14,0.22,0.3,0.38,0.44,0.5,0.57,0.63},             /*A0-A9*/
 {0.7,0.76,0.82,0.91,1.01,1.1,1.21,1.32,1.35,1.38},            /*F0-F9*/
 {1.41,1.435,1.46,1.495,1.53,1.585,1.64,1.72,1.8,1.88},        /*G0-G9*/
 {1.96,2.09,2.22,2.425,2.63,2.85,3.005,3.16,3.32,3.49},        /*K0-K9*/
 {3.65,3.87,4.11,4.65,5.26,6.12,7.3,8.0,8.7,9.4}};             /*M0-M9*/

static double VK_III[3][10]={ /* V-K magnitudes for luminosity class III */
 {1.75,1.82,1.9,1.97,2.05,2.1,2.15,2.15,2.16,2.16},            /*G0-G9*/
 {2.31,2.50,2.70,3.00,3.26,3.6,3.7,3.7,3.7,3.7},               /*K0-K9*/
 {3.85,4.05,4.3,4.64,5.1,5.96,6.84,7.8,8.8,9.8}};              /*M0-M9*/

static double RR_I[7][10]={ /* log(R_star/R_sun) for luminosity class I */
 {1.0,1.0,1.0,1.1,1.1,1.1,1.1,1.2,1.2,1.2},                    /*O0-O9*/
 {1.3,1.3,1.4,1.4,1.5,1.5,1.5,1.6,1.6,1.6},                    /*B0-B9*/
 {1.6,1.6,1.6,1.7,1.7,1.7,1.7,1.8,1.8,1.8},                    /*A0-A9*/
 {1.8,1.8,1.8,1.9,1.9,1.9,1.9,2.0,2.0,2.0},                    /*F0-F9*/
 {2.0,2.0,2.0,2.1,2.1,2.1,2.1,2.2,2.2,2.2},                    /*G0-G9*/
 {2.3,2.3,2.4,2.5,2.6,2.6,2.6,2.7,2.7,2.7},                    /*K0-K9*/
 {2.9,2.9,2.9,3.0,3.0,3.1,3.1,3.2,3.3,3.4}};                   /*M0-M9*/

static double RR_III[7][10]={ /* log(R_star/R_sun) for luminosity class III */
 {1.4,1.4,1.4,1.3,1.3,1.3,1.3,1.2,1.2,1.2},                    /*O0-O9*/
 {1.2,1.2,1.1,1.1,1.0,1.0,1.0,0.9,0.9,0.8},                    /*B0-B9*/
 {0.8,0.8,0.8,0.7,0.7,0.7,0.7,0.7,0.7,0.7},                    /*A0-A9*/
 {0.7,0.7,0.7,0.6,0.6,0.6,0.6,0.5,0.5,0.5},                    /*F0-F9*/
 {0.8,0.8,0.9,0.9,1.0,1.0,1.0,1.1,1.1,1.1},                    /*G0-G9*/
 {1.2,1.2,1.3,1.3,1.4,1.4,1.4,1.5,1.5,1.6},                    /*K0-K9*/
 {1.8,1.9,2.0,2.0,2.1,2.2,2.2,2.3,2.4,2.5}};                   /*M0-M9*/

static double RR_V[7][10]={ /* log(R_star/R_sun) for luminosity class V */
 {1.44,1.40,1.36,1.33,1.29,1.25,1.20,1.16,1.11,1.06},          /*O0-O9*/
 {.87,.81,.75,.70,.64,.58,.56,.53,.51,.49},                    /*B0-B9*/
 {.40,.37,.34,.30,.27,.24,.23,.21,.19,.18},                    /*A0-A9*/
 {.13,.12,.11,.10,.09,.08,.08,.07,.06,.05},                    /*F0-F9*/
 {.02,.01,0.0,-.01,-.02,-.03,-.03,-.04,-.05,-.05},             /*G0-G9*/
 {-.07,-.08,-.09,-.10,-.12,-.13,-.14,-.15,-.15,-.16},          /*K0-K9*/
 {-.2,-.3,-.3,-.4,-.4,-.5,-.6,-.7,-.9,-1.0}};                  /*M0-M9*/

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
   status=zvparmd("FOV",&fov,&count,&def,1,0);
   fov=fov/2.0;
   status=zvparm("PATH",path,&count,&def,1,0);
   status=zvparmd("VLIMIT",&v_mag_limit,&count,&def,1,0);
   status=zvparmd("KLIMIT",&k_mag_limit,&count,&def,1,0);
   status=zvparmd("PARLIMIT",&par_limit,&count,&def,1,0);
   status=zvparmd("LODEC",&lo_dec_limit,&count,&def,1,0);
   status=zvparmd("HIDEC",&hi_dec_limit,&count,&def,1,0);
   status=zvparm("COLOR",color,&count,&def,1,0);
   status=zvparm("LUMINOSITY",&luminosity[1],&count,&def,5,0);
   status=zvparm("HD",&hd_number,&hd_count,&def,1,0);
   status=zvparm("HIP",&hip_number,&hip_count,&def,1,0);

   status=zvparm("TIME",&time_obs,&count,&def,1,0);
   status=zvparm("REPS",&repeats,&count,&def,1,0);
   status=zvparm("PRIORITY",&priority,&count,&def,1,0);
   status=zvparm("OWNER",owner,&count,&def,1,0);
   status=zvparm("SPACING",&spacing,&count,&def,1,0);
   status=zvparm("BASENAME",basename,&count,&def,1,0);
   if(zvptst("SSM")){
     strcpy(obser_mode,"ssm");
     obs_mode=1;
   }
   if(zvptst("DSM")){
     strcpy(obser_mode,"dsm");
     obs_mode=2;
   }
   if(zvptst("DIA")){
     strcpy(obser_mode,"dia");
     obs_mode=3;
   }

/* get utc and local time */
   sec=time(NULL); /* seconds since 00:00;00 Jan 1 1970 */
   time_structure=gmtime(&sec);
   ptr=asctime(time_structure);
   strcpy(coordinated_universal_time,ptr);
   ptr=ctime(&sec);
   strcpy(local_time,ptr);

/* open output ascii catalogue */
   status=zvpone("OUT",catalogue,1,sizeof(catalogue));
   if ((fout = fopen(catalogue, "w")) == NULL) {
      fprintf(stderr, "Error opening file %s\n", catalogue);
      fputs("Error opening output catalogue\n", stat_file);
      zabend();
   }

/* write header record */
   strcpy(msg,"# UTC= \"");
   strncat(msg,coordinated_universal_time,strlen(coordinated_universal_time)-1);
   strcat(msg,"\"  Local= \"");
   strncat(msg,local_time,strlen(local_time)-1);
   strcat(msg,"\"");
   time_structure=gmtime(&sec);
   sprintf(buf," GMT=%04d%03d%02d%02d%02d ",time_structure->tm_year + 1900,
     time_structure->tm_yday +1,time_structure->tm_hour,
     time_structure->tm_min,time_structure->tm_sec);
   strcat(msg,buf);
   strcat(msg,version);
   if (fputs(msg, fout) == NULL) {
     fprintf(stderr, "Error writing file %s\n", catalogue);
     fputs("Error writing catalogue\n", stat_file);
     zabend();
   }


/* skip simbad if hd or hip number specified */
 if((hd_count > 0) || (hip_count > 0))goto skip_simbad;

/* get password from password server through a kerberos ticket */
 if(strcmp(passwd,"none") == 0){
   strcpy(userName,cuserid_p2());
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

/******************** get simbad data ********************************/

 hd_number=0;
 hip_number=0;
 hostname[0]='\0'; /* generic for Strasbourg */
 strcpy ( hostname, "simbad.harvard.edu" ) ; /* for the USA mirror site */
 service[0]='\0';
 co_radius[0]='\0';
 hh = simbad_connect(hostname,service,userid,passwd) ;
 if (hh < 0)
   {
   printf("Simbad connection not done: %s\n",simbad_error(0)) ;
   fputs("Simbad connection not done\n", stat_file);
   zabend();
   }
 printf("Simbad connection done: Handle = %d\n",hh) ;
 
 nitems = simbad_query(hh,rec,co_radius) ; /* get # objects within radius*/
 if(nitems == 0){
   zvmessage("No object located in the Simbad data base"," ");
   fputs("No object located in the Simbad data base\n",stat_file);
   zabend();
 }
 if(nitems < 0){
   j=simbad_errno(hh);
   if(j == 1)zvmessage("A network error occurred"," ");
   if(j == 2)zvmessage("A server error occurred"," ");
   if(j == 3)zvmessage("A client error occurred"," ");
   if(j == 11)zvmessage("A simbad error occurred"," ");
   zvmessage("Error occurred"," ");
   fputs("A Simbad error occurred\n",stat_file);
   fputs("This usually means the object could not be found\n",stat_file);
   fputs("Check spelling\n",stat_file);
   zabend();
 }
 
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
       zvmessage("Simbad retrieval error"," ");
       fputs("Simbad retrieval error\n",stat_file);
       zabend();
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
            RA_2 *= PI/180.;
            Dec_2 *= PI/180.;
          }
        }
        if(strcmp(typ,"C") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return classification */
          if(ndata > 0){
            p=simbad_getdata(hh,0);
            strcpy(item_class,p);
            printf("%s is classified as a%s\n",rec,item_class);
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
        if(strcmp(typ,"I") == 0){
          ndata = simbad_findata(hh,typ,"") ;/* return identifications */
          printf("%s is also known as:\n",rec);
          for(i=1; i <= ndata; i++){
            p=simbad_getdata(hh,0);
            strcpy(item_id,p);
            printf("%s \n",item_id);
            sscanf(item_id,"%s %d",name,&number);
            if(strcmp(name,"HIP") == 0) hip_number=number;
            if(strcmp(name,"HD") == 0) hd_number=number;
          }
        }
     }
 simbad_disconnect(hh);

/* print simbad coordinates */
 conrddms(RA_2,Dec_2,&rah,&ram,&ras,&decsign,&decd,&decm,&decs);
 printf(
   "Simbad coordinates(b2000): %02d %02d %04.1f %c%02d %02d %04.1f\n",
   rah,ram,ras,decsign,decd,decm,decs);

/* sanity check */
 if(hip_number == 0){
   zvmessage("No HIP number returned by simbad"," ");
   fputs("No HIP number returned by simbad\n", stat_file);
   zabend();
 }
 if(have_coordinate == 0){
   zvmessage("No RA & Dec returned by simbad"," ");
   fputs("No RA & Dec returned by simbad\n", stat_file);
   zabend();
 }
 skip_simbad:;

/********************** initial Hipparcos search **********************/
/* If the hd or hip number is user specified get the coordinates from 
   the Hipparcos catalogue directly */
 if((hd_count > 0) || (hip_count > 0)){

   zvmessage("Opening Hipparcos catalogue for target coordinates"," ");

   if ((fd = fopen(path, "r")) == NULL) { /* open hipparcos catalogue */
      fprintf(stderr, "Error opening file %s\n",path);
      fputs("Error opening file\n", stat_file);
      zabend();
   }
   found_target=0;
   for (;;) {
     if (fgets(&buf[1], 490, fd) == NULL) break; /* read 450 byte blocks */

     /* reject Tycho objects */
     strncpy(hip_name,&buf[1],1); hip_name[1]='\0'; /* H or T */
     if(strcmp(hip_name,"T") == 0)continue;

     /* locate HIP record */
     if(hip_count > 0){
       strncpy(msg,&buf[3],12); msg[12]='\0'; sscanf(msg,"%d",&number);
       if(number == hip_number){
         found_target=1;
         hip_number=number;
       }
     }
     /* locate HD record */
     if(hd_count > 0){
       strncpy(msg,&buf[391],6); msg[6]='\0'; sscanf(msg,"%d",&number);
       if(number == hd_number){
         found_target=1;
         strncpy(msg,&buf[3],12); msg[12]='\0'; sscanf(msg,"%d",&number);
         hip_number=number;
       }
     }
     /* get RA, Dec */
     if(found_target == 1){
       strncpy(msg,&buf[52],12); msg[12]='\0'; sscanf(msg,"%lf",&RA_2);
       strncpy(msg,&buf[65],12); msg[12]='\0'; sscanf(msg,"%lf",&Dec_2);
       RA_2 *= PI/180.;
       Dec_2 *= PI/180.;
       break;
     }
   }
   if(found_target == 0){
     zvmessage("Target HIP or HD number not in Hipparcos catalogue."," ");
     fputs("Target HIP or HD number not in Hipparcos catalogue\n",stat_file);
     zabend();
   }
   fclose(fd); /* hipparcos catalogue */
 }


/******************** get hipparcos data *******************************/
 zvmessage("Opening Hipparcos catalogue"," ");

   if ((fd = fopen(path, "r")) == NULL) { /* open hipparcos catalogue */
      fprintf(stderr, "Error opening file %s\n",path);
      fputs("Error opening file\n", stat_file);
      zabend();
   }
 
/* write output catalogue, first 2 headers */
   if((hd_count > 0) || (hip_count > 0)){
     if(hd_count > 0)
       sprintf(msg,"# Target HIP%d known as HD%d\n",hip_number,hd_number);
     if(hip_count > 0)
       sprintf(msg,"# Target HIP%d known as HIP%d\n",hip_number,hip_number);
   }
   else
     /* get rid of blanks in target simbad name */
     for(j=1; j < strlen(rec); j++){
       if(isgraph(rec[j])){
       }
       else {
         rec[j] = '_';
       }
     }
     sprintf(msg,"# Target HIP%d known as %s is a %s\n",hip_number,rec,item_class);

   if (fputs(msg, fout) == NULL) {
       fprintf(stderr, "Error writing file %s\n", catalogue);
       fputs("Error writing catalogue\n", stat_file);
       zabend();
   }
   sprintf(msg,"# owner=%s mode=%s priority=%d repeats=%d spacing=%d time=%d baseline=%s\n",
     owner,obser_mode,priority,repeats,spacing,time_obs,basename); 
   fputs(msg, fout);
   fputs(header, fout);

   /* search hipparcos catalogue for all objects within the fov */
   ntarget=0;
   count=0;
   found_target=0;
   for (;;) {
     if (fgets(&buf[1], 490, fd) == NULL) break; /* read 450 byte blocks */
 
     /* reject Tycho objects */
     strncpy(hip_name,&buf[1],1); hip_name[1]='\0'; /* H or T */
     if(strcmp(hip_name,"T") == 0)continue;

     /* get HIP # */
     strncpy(msg,&buf[3],12); msg[12]='\0'; sscanf(msg,"%d",&number);
     if(number == hip_number)found_target=1;

     /* get RA, Dec */
     strncpy(msg,&buf[52],12); msg[12]='\0'; sscanf(msg,"%lf",&hip_ra_j1991);
     strncpy(msg,&buf[65],12); msg[12]='\0'; sscanf(msg,"%lf",&hip_dec_j1991);

     /* reject if too far north or south */
     if(number == hip_number){
       printf("ra= %f dec= %f\n",hip_ra_j1991,hip_dec_j1991);
     }
     else{
       if((hip_dec_j1991 < lo_dec_limit) || (hip_dec_j1991 > hi_dec_limit))continue;
     }

     /* reject if out of field of view */
     if(number == hip_number){
     }
     else{
       RA_1=hip_ra_j1991*PI/180.;
       Dec_1=hip_dec_j1991*PI/180.;
       Separation=
            cos( PI/2. - Dec_2 )*cos( PI/2. - Dec_1 ) +
            sin( PI/2. - Dec_2 )*sin( PI/2. - Dec_1 )*cos( RA_1 - RA_2 );
       if(Separation < -1.)Separation=-1.;
       if(Separation >  1.)Separation= 1.;
       Separation= acos(Separation)*(180/PI); /* radius in degrees */
       if(Separation > fov)continue;   
     }

     /* get V magnitude, spectral type */
     strncpy(msg,&buf[231],6); msg[6]='\0'; sscanf(msg,"%lf",&hip_V_mag);
     strncpy(msg,&buf[436],12); msg[12]='\0'; sscanf(msg,"%s",hip_sp_type);

     /* extract luminosity class, 1=I, 2=II, 3=III, 4=IV, 5=V */
     star_iclass = 0;
     if((strchr(hip_sp_type, 'V')!= NULL)&&
        (strchr(hip_sp_type, 'I')== NULL))star_iclass = 5;
     if (strstr(hip_sp_type, "IV")!= NULL)star_iclass = 4;
     if (strstr(hip_sp_type, "III")!= NULL)star_iclass = 3;
     if((strstr(hip_sp_type, "II")!= NULL)&&
        (strstr(hip_sp_type, "III")== NULL))star_iclass = 2;
     if((strchr(hip_sp_type, 'I')!= NULL)&&
        (strstr(hip_sp_type, "II")== NULL)&&
        (strstr(hip_sp_type, "IV")== NULL)&&
        (strstr(hip_sp_type, "III")== NULL))star_iclass = 1;

     /* extract star MK color(OBAFGKM) and numeric index(1-9) */
     sscanf(hip_sp_type,"%c%i", &star_ctype, &star_itype);

     /* compute K magnitude */
     star_kmag = 0.;
     if((star_itype >= 0) && (star_itype <= 9)){ /* MK sub type */
       if( star_iclass == 3){  /* luminosity type III*/
         if( star_ctype == 'G')star_kmag=hip_V_mag - VK_III[0][star_itype];
         if( star_ctype == 'K')star_kmag=hip_V_mag - VK_III[1][star_itype];
         if( star_ctype == 'M')star_kmag=hip_V_mag - VK_III[2][star_itype];
       }
       if( star_iclass == 5){  /* luminosity type V*/
         if( star_ctype == 'B')star_kmag=hip_V_mag - VK_V[0][star_itype];
         if( star_ctype == 'A')star_kmag=hip_V_mag - VK_V[1][star_itype];
         if( star_ctype == 'F')star_kmag=hip_V_mag - VK_V[2][star_itype];
         if( star_ctype == 'G')star_kmag=hip_V_mag - VK_V[3][star_itype];
         if( star_ctype == 'K')star_kmag=hip_V_mag - VK_V[4][star_itype];
         if( star_ctype == 'M')star_kmag=hip_V_mag - VK_V[5][star_itype];
       }
       if( star_iclass == 4){  /* luminosity type IV*/
         if( star_ctype == 'G')star_kmag=hip_V_mag - 
           (VK_III[0][star_itype]+VK_V[3][star_itype])/2.0;
         if( star_ctype == 'K')star_kmag=hip_V_mag - 
           (VK_III[1][star_itype]+VK_V[4][star_itype])/2.0;
         if( star_ctype == 'M')star_kmag=hip_V_mag - 
           (VK_III[2][star_itype]+VK_V[5][star_itype])/2.0;
       }
     }

    /* reject if V or K magnitude too faint */
     if(number == hip_number){
       printf("v magnitude= %f k magnitude= %f\n",hip_V_mag,star_kmag);
     }
     else{
       if(star_kmag > k_mag_limit)continue;
       if(hip_V_mag > v_mag_limit)continue;
    }
    
    /* reject if unacceptable luminosity class */
     if(number == hip_number){
       printf("spectral type %s\n",hip_sp_type);
     }
     else{
       if(star_iclass == 0)continue;  /* no class in catalogue */
       if((luminosity[1] == 0) && (star_iclass == 1))continue;
       if((luminosity[2] == 0) && (star_iclass == 2))continue;
       if((luminosity[3] == 0) && (star_iclass == 3))continue;
       if((luminosity[4] == 0) && (star_iclass == 4))continue;
       if((luminosity[5] == 0) && (star_iclass == 5))continue;
     }
 
    /* reject if not acceptable MK color type */
     if(number == hip_number){}
     else{
       if(strchr(color,star_ctype)== NULL)continue;
     }

     /* get parallax */
     strncpy(msg,&buf[80],7); msg[7]='\0'; sscanf(msg,"%lf",&hip_parallax);

     /* reject if too close */
     if(number == hip_number){
       printf("parallax= %f\n",hip_parallax);
     }
     else{
       if(hip_parallax > par_limit)continue;
     }

     /* compute angular size of star in seconds */
     star_lsize = 0.; /* ratio of stellar diameter to solar diameter */
     if((star_itype >= 0) && (star_itype <= 9)){ /* MK sub type */
       if( star_iclass == 1){  /* luminosity type I*/
         if( star_ctype == 'O')star_lsize= pow(10.,RR_I[0][star_itype]);
         if( star_ctype == 'B')star_lsize= pow(10.,RR_I[1][star_itype]);
         if( star_ctype == 'A')star_lsize= pow(10.,RR_I[2][star_itype]);
         if( star_ctype == 'F')star_lsize= pow(10.,RR_I[3][star_itype]);
         if( star_ctype == 'G')star_lsize= pow(10.,RR_I[4][star_itype]);
         if( star_ctype == 'K')star_lsize= pow(10.,RR_I[5][star_itype]);
         if( star_ctype == 'M')star_lsize= pow(10.,RR_I[6][star_itype]);
       }
       if( star_iclass == 3){  /* luminosity type III*/
         if( star_ctype == 'O')star_lsize= pow(10.,RR_III[0][star_itype]);
         if( star_ctype == 'B')star_lsize= pow(10.,RR_III[1][star_itype]);
         if( star_ctype == 'A')star_lsize= pow(10.,RR_III[2][star_itype]);
         if( star_ctype == 'F')star_lsize= pow(10.,RR_III[3][star_itype]);
         if( star_ctype == 'G')star_lsize= pow(10.,RR_III[4][star_itype]);
         if( star_ctype == 'K')star_lsize= pow(10.,RR_III[5][star_itype]);
         if( star_ctype == 'M')star_lsize= pow(10.,RR_III[6][star_itype]);
       }
       if( star_iclass == 5){  /* luminosity type V*/
         if( star_ctype == 'O')star_lsize= pow(10.,RR_V[0][star_itype]);
         if( star_ctype == 'B')star_lsize= pow(10.,RR_V[1][star_itype]);
         if( star_ctype == 'A')star_lsize= pow(10.,RR_V[2][star_itype]);
         if( star_ctype == 'F')star_lsize= pow(10.,RR_V[3][star_itype]);
         if( star_ctype == 'G')star_lsize= pow(10.,RR_V[4][star_itype]);
         if( star_ctype == 'K')star_lsize= pow(10.,RR_V[5][star_itype]);
         if( star_ctype == 'M')star_lsize= pow(10.,RR_V[6][star_itype]);
       }
       if( star_iclass == 3){  /* luminosity type III*/
         if( star_ctype == 'O')star_lsize= 
          (pow(10.,RR_I[0][star_itype])+pow(10.,RR_III[0][star_itype])/2.0);
         if( star_ctype == 'B')star_lsize= 
          (pow(10.,RR_I[1][star_itype])+pow(10.,RR_III[1][star_itype])/2.0);
         if( star_ctype == 'A')star_lsize= 
          (pow(10.,RR_I[2][star_itype])+pow(10.,RR_III[2][star_itype])/2.0);
         if( star_ctype == 'F')star_lsize= 
          (pow(10.,RR_I[3][star_itype])+pow(10.,RR_III[3][star_itype])/2.0);
         if( star_ctype == 'G')star_lsize= 
          (pow(10.,RR_I[4][star_itype])+pow(10.,RR_III[4][star_itype])/2.0);
         if( star_ctype == 'K')star_lsize= 
          (pow(10.,RR_I[5][star_itype])+pow(10.,RR_III[5][star_itype])/2.0);
         if( star_ctype == 'M')star_lsize= 
          (pow(10.,RR_I[6][star_itype])+pow(10.,RR_III[6][star_itype])/2.0);
       }
       if( star_iclass == 4){  /* luminosity type IV*/
         if( star_ctype == 'O')star_lsize= 
          (pow(10.,RR_III[0][star_itype])+pow(10.,RR_V[0][star_itype])/2.0);
         if( star_ctype == 'B')star_lsize= 
          (pow(10.,RR_III[1][star_itype])+pow(10.,RR_V[1][star_itype])/2.0);
         if( star_ctype == 'A')star_lsize= 
          (pow(10.,RR_III[2][star_itype])+pow(10.,RR_V[2][star_itype])/2.0);
         if( star_ctype == 'F')star_lsize= 
          (pow(10.,RR_III[3][star_itype])+pow(10.,RR_V[3][star_itype])/2.0);
         if( star_ctype == 'G')star_lsize= 
          (pow(10.,RR_III[4][star_itype])+pow(10.,RR_V[4][star_itype])/2.0);
         if( star_ctype == 'K')star_lsize= 
          (pow(10.,RR_III[5][star_itype])+pow(10.,RR_V[5][star_itype])/2.0);
         if( star_ctype == 'M')star_lsize= 
          (pow(10.,RR_III[6][star_itype])+pow(10.,RR_V[6][star_itype])/2.0);
       }
     }
     star_eas=2.0*6.96e+10*star_lsize*hip_parallax/(1000.*3.0857e+18);
     star_eas *= (180./PI)*3600.*1000.; /* in milli seconds of arc */


     /* get proper motion, */
     strncpy(msg,&buf[88],8); msg[8]='\0'; sscanf(msg,"%lf",&hip_pm_ra);
     strncpy(msg,&buf[97],8); msg[8]='\0'; sscanf(msg,"%lf",&hip_pm_dec);
     hip_pm_ra /= 1000.;
     hip_pm_dec /= 1000.;
     if(number == hip_number){
       printf("pm_ra= %f pm_dec= %f\n",hip_pm_ra,hip_pm_dec);
     }

     /* get number components, annex flag, solution quality */
     strncpy(msg,&buf[344],2); msg[2]='\0'; sscanf(msg,"%d",&hip_components);
     strncpy(hip_annex,&buf[347],1); hip_annex[1]='\0';
     strncpy(hip_quality,&buf[351],1); hip_quality[1]='\0';

     /* keep object, load buffers */
     ntarget += 1;
     Hip_ids[ntarget]=number;
     Hip_ra[ntarget]=hip_ra_j1991;
     Hip_dec[ntarget]=hip_dec_j1991;
     Hip_pm_ra[ntarget]=hip_pm_ra;
     Hip_pm_dec[ntarget]=hip_pm_dec;
     Hip_V_mag[ntarget]=hip_V_mag;
     Star_kmag[ntarget]=star_kmag;
     strcpy(&Hip_sp_type[ntarget],hip_sp_type);
     Hip_parallax[ntarget]=hip_parallax;
     Star_eas[ntarget]=star_eas;
     Hip_components[ntarget]=hip_components;
     strcpy(&Hip_annex[ntarget],hip_annex);
     strcpy(&Hip_quality[ntarget],hip_quality);

     /* apply proper motion from 1991.25 to 2000
     COMMENTED OUT
     Hip_ra[ntarget] += Hip_pm_ra[ntarget]*(2000.-1991.25)/
      (3600.*cos(fabs(Hip_dec[ntarget])*PI/180.));
     Hip_dec[ntarget] += Hip_pm_dec[ntarget]*(2000.-1991.25)/3600.;
     */

     /* convert Ra & Dec from Hipparcos in j1991.25 to j2000 */
     hip_to_j2000(Hip_ra[ntarget],Hip_dec[ntarget],
        Hip_pm_ra[ntarget]*1000.,Hip_pm_dec[ntarget]*1000.,
        Hip_parallax[ntarget],0.0,&RA_1,&Dec_1);
     Hip_ra[ntarget]=RA_1*15.0;
     Hip_dec[ntarget]=Dec_1;

     if(ntarget == MAXTABLE-1){
       if(found_target == 1){
         zvmessage("Table full, no more objects inspected"," ");
         break;
       }
       else{
         zvmessage("Table filled before target reached"," ");
         fputs("Table filled before target reached\n",stat_file);
         zabend();
       }
     }
   }

   /* order table by distance from target (smallest first) */
   j=0;
   for(i=1; i <= ntarget; i++){
     if(Hip_ids[i] == hip_number){  /* locate target by HIP number */
       RA_2=Hip_ra[i]*PI/180.;
       Dec_2=Hip_dec[i]*PI/180.;
       j=1;
       break;
     }
   }
   if(j == 0){
     zvmessage("Target HIP number not in Hipparcos catalogue."," ");
     fputs("Target HIP number not in Hipparcos catalogue\n",stat_file);
     zabend();
   }

   for(i=1; i <= ntarget; i++){     /* compute distances to target */
     RA_1=Hip_ra[i]*PI/180.;    
     Dec_1=Hip_dec[i]*PI/180.;
     Separation=
          cos( PI/2. - Dec_2 )*cos( PI/2. - Dec_1 ) +
          sin( PI/2. - Dec_2 )*sin( PI/2. - Dec_1 )*cos( RA_1 - RA_2 );
     if(Separation < -1.)Separation=-1.;
     if(Separation >  1.)Separation= 1.;
     distance[i]= acos(Separation)*(180/PI); /* radius in degrees */
   }
   for(j=1; j < ntarget; j++){      /* sort in place */
     nearest=distance[ntarget];
     n=ntarget;
     for(i=j; i <= ntarget; i++){
       if(distance[i] < nearest){
         nearest=distance[i];
         n=i;
       }
     }
     dist_scr=distance[j];
     hip_ids_scr=Hip_ids[j];
     hip_ra_scr=Hip_ra[j];
     hip_dec_scr=Hip_dec[j];
     hip_pm_ra=Hip_pm_ra[j];
     hip_pm_dec=Hip_pm_dec[j];
     hip_V_mag=Hip_V_mag[j];
     star_kmag=Star_kmag[j];
     strcpy(hip_sp_type,&Hip_sp_type[j]);
     hip_parallax=Hip_parallax[j];
     star_eas=Star_eas[j];
     hip_components=Hip_components[j];
     strcpy(hip_annex,&Hip_annex[j]);
     strcpy(hip_quality,&Hip_quality[j]);
      distance[j]=distance[n];
      Hip_ids[j]=Hip_ids[n];
      Hip_ra[j]=Hip_ra[n];
      Hip_dec[j]=Hip_dec[n];
      Hip_pm_ra[j]=Hip_pm_ra[n];
      Hip_pm_dec[j]=Hip_pm_dec[n];
      Hip_V_mag[j]=Hip_V_mag[n];
      Star_kmag[j]=Star_kmag[n];
      strcpy(&Hip_sp_type[j],&Hip_sp_type[n]);
      Hip_parallax[j]=Hip_parallax[n];
      Star_eas[j]=Star_eas[n];
      Hip_components[j]=Hip_components[n];
      strcpy(&Hip_annex[j],&Hip_annex[n]);
      strcpy(&Hip_quality[j],&Hip_quality[n]);
       distance[n]=dist_scr;
       Hip_ids[n]=hip_ids_scr;
       Hip_ra[n]=hip_ra_scr;
       Hip_dec[n]=hip_dec_scr;
       Hip_pm_ra[n]=hip_pm_ra;
       Hip_pm_dec[n]=hip_pm_dec;
       Hip_V_mag[n]=hip_V_mag;
       Star_kmag[n]=star_kmag;
       strcpy(&Hip_sp_type[n],hip_sp_type);
       Hip_parallax[n]=hip_parallax;
       Star_eas[n]=star_eas;
       Hip_components[n]=hip_components;
       strcpy(&Hip_annex[n],hip_annex);
       strcpy(&Hip_quality[n],hip_quality);
   }

   if(ntarget == 0){
     zvmessage("All objects rejected within fov"," ");
     fputs("All objects rejected within fov\n", stat_file);
     zabend();
   }

/* limit objects to 1 if in stellar diameter mode */
   if(obs_mode == 3)ntarget=1;
   else printf("%d hipparcos objects located within fov\n",ntarget);

/* Call Simbad to tell us what each object & calibrator is */
 hh = simbad_connect(hostname,service,userid,passwd) ;
 if (hh >= 0){
   for(i=1; i <= ntarget; i++){
      strcpy(item_class,"   "); /* clear field in case of a failure */
      sprintf(rec,"HIP %d\n",Hip_ids[i]);
      nitems = simbad_query(hh,rec,co_radius) ; /* get # objects within radius*/
      if(nitems > 0){

        status=simbad_retrieve(hh,0) ;  /* retrieve next object */
        if(status > 0){
 
          astrotyplist = simbad_telldata(hh); /* get list of object data types */
          for (typ = strtok(astrotyplist," ") ;
            typ != NULL ;
            typ = strtok(NULL," ")){
  
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
          }
        }
      }
      strcpy(&simbad_description[i][0],item_class);
   }
 }
 simbad_disconnect(hh);

/* write output catalogue records */
   for(i=1; i <= ntarget; i++){
     if(i == 1)obj_type='p';
     if(i != 1)obj_type='c';
     if(strncmp(&Hip_annex[i]," ",1) == 0)strcpy(&Hip_annex[i],"-");
     if(strncmp(&Hip_quality[i]," ",1) == 0)strcpy(&Hip_quality[i],"-");
     /* convert from decimal to h,m,s,d,m,s */
     conrddms(Hip_ra[i]*PI/180.,Hip_dec[i]*PI/180.,
        &rah,&ram,&ras,&decsign,&decd,&decm,&decs);
     if(Star_kmag[i] != 0.)
       sprintf(msg,
        " HIP%-9d %02d %02d %05.2f %c%02d %02d %04.1f %+7.3f %+7.3f %4.1f %4.1f %-6.6s %5.2f %5.2f %6.3f %c %02d %s %s %s\n",
        Hip_ids[i],rah,ram,ras,decsign,decd,decm,decs,Hip_pm_ra[i],
        Hip_pm_dec[i],Hip_V_mag[i],Star_kmag[i],Hip_sp_type[i],
        distance[i],
        Hip_parallax[i],Star_eas[i],obj_type,Hip_components[i],
        Hip_annex[i],Hip_quality[i],simbad_description[i]);
     else
       sprintf(msg,
        " HIP%-9d %02d %02d %05.2f %c%02d %02d %04.1f %+7.3f %+7.3f %4.1f %4.0f %-6.6s %5.2f %5.2f %6.3f %c %02d %s %s %s\n",
        Hip_ids[i],rah,ram,ras,decsign,decd,decm,decs,Hip_pm_ra[i],
        Hip_pm_dec[i],Hip_V_mag[i],Star_kmag[i],Hip_sp_type[i],
        distance[i],
        Hip_parallax[i],Star_eas[i],obj_type,Hip_components[i],
        Hip_annex[i],Hip_quality[i],simbad_description[i]);
     if (fputs(msg, fout) == NULL) {
       fprintf(stderr, "Error writing file %s\n", catalogue);
       fputs("Error writing catalogue\n", stat_file);
       zabend();
     }
   }

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

/*************************************************************************/
/* conversion of NOVAS hipparcos routine catran.f */
 hip_to_j2000 (RAH,DECH,PMRAH,PMDECH,PARXH,RVH,RA2,DEC2)
 
 double RAH,DECH,PMRAH,PMDECH,PARXH,RVH,*RA2,*DEC2;
 
/*
C     THIS SUBROUTINE CONVERTS HIPPARCOS DATA AT EPOCH J1991.25
C     TO EPOCH J2000.0 AND FK5-STYLE UNITS.  TO BE USED ONLY FOR
C     HIPPARCOS OR TYCHO STARS WITH LINEAR SPACE MOTION.
 
C          RAH    = HIPPARCOS RIGHT ASCENSION IN DEGREES (IN)
C          DECH   = HIPPARCOS DECLINATION IN DEGREES (IN)
C          PMRAH  = HIPPARCOS PROPER MOTION IN RA * COS(DECH)
C                   IN MILLIARCSECONDS PER YEAR (IN)
C          PMDECH = HIPPARCOS PROPER MOTION IN DEC
C                   IN MILLIARCSECONDS PER YEAR (IN)
C          PARXH  = HIPPARCOS PARALLAX IN MILLIARCSECONDS (IN)
C          RVH    = RADIAL VELOCITY AT HIPPARCOS EPOCH
C                   IN KILOMETERS PER SECOND (IN)
C          RA2    = RIGHT ASCENSION AT J2000.0 IN HOURS (OUT)
C          DEC2   = DECLINATION AT J2000.0 IN DEGREES (OUT)
 
C          IT     = TRANSFORMATION OPTION (IN)
C                   SET IT=1 TO CHANGE EPOCH (SAME EQUATOR AND EQUINOX)
C                   SET IT=2 TO CHANGE EQUATOR AND EQUINOX (SAME EPOCH)
C                   SET IT=3 TO CHANGE EQUATOR AND EQUINOX AND EPOCH
C          DATE1  = TDB JULIAN DATE, OR YEAR, OF ORIGINAL CATALOG
C                   DATA (THE FOLLOWING SIX ARGUMENTS) (IN)
C          RA1    = ORIGINAL RIGHT ASCENSION IN HOURS (IN)
C          DEC1   = ORIGINAL DECLINATION IN DEGREES (IN)
C          PMRA1  = ORIGINAL PROPER MOTION IN RA
C                   IN SECONDS OF TIME PER JULIAN CENTURY (IN)
C          PMDEC1 = ORIGINAL PROPER MOTION IN DEC
C                   IN SECONDS OF ARC PER JULIAN CENTURY (IN)
C          PARX1  = ORIGINAL PARALLAX IN SECONDS OF ARC (IN)
C          RV1    = ORIGINAL RADIAL VELOCITY IN KILOMETERS
C          DATE2  = TDB JULIAN DATE, OR YEAR, FOR TRANSFORMED
C                   OUTPUT DATA (THE FOLLOWING SIX ARGUMENTS) (IN)
C          RA2    = TRANSFORMED RIGHT ASCENSION IN HOURS (OUT)
C          DEC2   = TRANSFORMED DECLINATION IN DEGREES (OUT)
C          PMRA2  = TRANSFORMED PROPER MOTION IN RA
C                   IN SECONDS OF TIME PER JULIAN CENTURY (OUT)
C          PMDEC2 = TRANSFORMED PROPER MOTION IN DEC
C                   IN SECONDS OF ARC PER JULIAN CENTURY (OUT)
C          PARX2  = TRANSFORMED PARALLAX IN SECONDS OF ARC (OUT)
C          RV2    = TRANSFORMED RADIAL VELOCITY IN KILOMETERS
C                   PER SECOND (OUT)
*/

{
 double  RADCON,DATE1,DATE2,RA1,DEC1,PMRA1,PMDEC1,PARX1,RV1;
 double  PMRA2,PMDEC2,PARX2,RV2,SECCON,KMAU,TJD1,TJD2;
 double  PARALX,DIST,R,D,CRA,SRA,CDC,SDC,PMR,PMD,RVL,XYPROJ;
 double  POS1[4], VEL1[4], POS2[4], VEL2[4];
 int IT,J;
 
      RADCON = 0.0174532925199433;
      DATE1 = 2448349.0625;
      DATE2 = 2451545.;
      SECCON = 206264.8062470964;
      KMAU = 1.49597870;
 
      RA1 = RAH / 15.;
      DEC1 = DECH;
      PMRA1 = PMRAH / ( 150. * cos ( DEC1 * RADCON ) );
      PMDEC1 = PMDECH / 10.;
      PARX1 = PARXH / 1000.;
      RV1 = RVH;
      IT = 1;
      TJD1 = DATE1;
      TJD2 = DATE2;
 
/* --- CONVERT INPUT ANGULAR COMPONENTS TO VECTORS --------------------
C     IF PARALLAX IS UNKNOWN, UNDETERMINED, OR ZERO, SET IT TO 1E-7
C     SECOND OF ARC, CORRESPONDING TO A DISTANCE OF 10 MEGAPARSECS*/
      PARALX = PARX1;
      if ( PARALX <= 0.0 ) PARALX = .00000001;
 
/*    CONVERT RIGHT ASCENSION, DECLINATION, AND PARALLAX TO POSITION
C     VECTOR IN EQUATORIAL SYSTEM WITH UNITS OF AU*/
      DIST = SECCON / PARALX;
      R = RA1 * 54000.0 / SECCON;
      D = DEC1 * 3600.0 / SECCON;
      CRA = cos(R);
      SRA = sin(R);
      CDC = cos(D);
      SDC = sin(D);
      POS1[1] = DIST * CDC * CRA;
      POS1[2] = DIST * CDC * SRA;
      POS1[3] = DIST * SDC;
 
/*    CONVERT PROPER MOTION AND RADIAL VELOCITY TO ORTHOGONAL
C     COMPONENTS OF MOTION, IN SPHERICAL POLAR SYSTEM AT STAR'S
C     ORIGINAL POSITION, WITH UNITS OF AU/DAY*/
      PMR = PMRA1 * 15.0 * CDC / ( PARALX * 36525.0 );
      PMD = PMDEC1              / ( PARALX * 36525.0 );
      RVL = RV1 * 86400.0 / KMAU;
 
/*    TRANSFORM MOTION VECTOR TO EQUATORIAL SYSTEM */
      VEL1[1] = - PMR * SRA - PMD * SDC * CRA + RVL * CDC * CRA;
      VEL1[2] =   PMR * CRA - PMD * SDC * SRA + RVL * CDC * SRA;
      VEL1[3] =               PMD * CDC       + RVL * SDC;
 
/*--- UPDATE STAR'S POSITION VECTOR FOR SPACE MOTION -----------------*/
       for(J=1; J <= 3; J++){
           POS2[J] = POS1[J] + VEL1[J] * ( TJD2 - TJD1 );
           VEL2[J] = VEL1[J];
       }
 
/*--- CONVERT VECTORS BACK TO ANGULAR COMPONENTS FOR OUTPUT ----------
C     FROM UPDATED POSITION VECTOR, OBTAIN STAR'S NEW POSITION
C     EXPRESSED AS ANGULAR QUANTITIES*/
      XYPROJ = sqrt ( POS2[1]*POS2[1] + POS2[2]*POS2[2] );
      R = atan2 ( POS2[2], POS2[1] );
      D = atan2 ( POS2[3], XYPROJ  );
      *RA2 = R * SECCON / 54000.0;
      *DEC2 = D * SECCON / 3600.0;
      if ( *RA2 < 0.0 ) *RA2 = *RA2 + 24.0;
}

/*************************************************************************/
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
#define ARCSEC_PER_RAD 206264.8062470964
#define SEC_PER_RAD (ARCSEC_PER_RAD/15.0)
 
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

/**********************************************************************
c* Copyright (c) 1990 by Craig Counterman. All rights reserved.
c*
c* This software may be redistributed freely, not sold.
c* This copyright notice and disclaimer of warranty must remain
c*    unchanged.
c*
c* No representation is made about the suitability of this
c* software for any purpose.  It is provided "as is" without express or
c* implied warranty, to the extent permitted by applicable law.
c*
c* Rigorous precession. From Astronomical Ephemeris 1989, p. B18
c*
c* Converted from c to fortran

 in_ra & in_dec in degrees
   epoch in decimal years like 1950.0
   out_ra & out_dec in degrees J2000
*/

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
$ Return
$!#############################################################################
$Imake_File:
$ create obs_cat.imake
#define  PROGRAM   obs_cat

#define MODULE_LIST obs_cat.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_C

#define LIB_TAE_NOSHR
#define LIB_RTL_NOSHR
#define LIB_MDMS
#define LIB_KERBEROS
#define LIB_SIMBAD
#define LIB_P2SUB
#define DEBUG
$ Return
$!#############################################################################
$PDF_File:
$ create obs_cat.pdf
process help=*
PARM OUT TYPE=STRING COUNT=1
PARM USERID  TYPE=STRING COUNT=(0:1) DEFAULT="u2141ozs"
PARM PASSWORD  TYPE=STRING COUNT=(0:1) DEFAULT="none"
PARM PATH    TYPE=(STRING,80) COUNT=(0:1) +
   DEFAULT=/project/test_work/testdata/general/hip_main.dat
PARM MODE TYPE=KEYWORD COUNT=(0:1) VALID=(ssm,dsm,dia) DEFAULT=ssm
PARM TIME TYPE=INTEGER COUNT=(0:1) DEFAULT=125
PARM REPS TYPE=INTEGER COUNT=(0:1) DEFAULT=5
PARM PRIORITY TYPE=INTEGER COUNT=(0:1) DEFAULT=1
PARM OWNER TYPE=STRING COUNT=(0:1) DEFAULT="none"
PARM SPACING TYPE=INTEGER COUNT=(0:1) DEFAULT=30
PARM NAME  TYPE=STRING COUNT=1 DEFAULT="none"
PARM STATUS TYPE=(STRING,100) COUNT=(0:1) DEFAULT=""
PARM FOV TYPE=REAL COUNT=(0:1) DEFAULT=20.
PARM VLIMIT TYPE=REAL COUNT=(0:1) DEFAULT=8.0
PARM KLIMIT TYPE=REAL COUNT=(0:1) DEFAULT=4.5
PARM PARLIMIT TYPE=REAL COUNT=(0:1) DEFAULT=20.
PARM LODEC TYPE=REAL COUNT=(0:1) DEFAULT=0.
PARM HIDEC TYPE=REAL COUNT=(0:1) DEFAULT=50.
PARM LUMINOSITY TYPE=INTEGER COUNT=(0,5) DEFAULT=(0,0,0,0,5)
PARM COLOR TYPE=STRING COUNT=(0:1) DEFAULT="FGKM"
PARM HD TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM HIP TYPE=INTEGER COUNT=(0:1) DEFAULT=--
PARM BASENAME TYPE=KEYWORD COUNT=(0:1) VALID=(ns,nw) DEFAULT=ns
END-PROC

.TITLE
VICAR program OBS_CAT (observation catalogue)

.HELP
PURPOSE:
To create an ascii catalogue for input to the Palomar Test Bed Interferometer.
The catalogue can be used as an observing list but is intended to be given
to program OBS_LIST for conversion into a schedule.

USAGE:
You specify an astronomical object by name.
Obs_cat queries Simbad, the french data base in Strasbourg, to resolve the
name aliases and return the Hipparcos number. With the HIP number the program
searches the Hipparcos catalogue for candidate calibrators. You can control the
selection of calibrators by use of parameters. When OBS_CAT is done it will
have created a single target catalogue. Many such catalogues can be concatenated
together (without the time header) to provide input for the scheduler OBS_LIST.

THE OUTPUT CATALOGUE:
The catalogue header contains the date and time that the program executed.
The output catalogue contains ascii columns in a format acceptable to the 
sequencer. Columns are:
1.   Hipparcos number.
2-4. RA in j2000.
5-7. Dec in j2000.
8.   Proper motion in RA * cos(dec), seconds of arc/year.
9.   Proper motion in Dec, seconds of arc/year.
10.  v magnitude.
11.  k magnitude. "0" if unavailable.
12.  Spectral type.
13.  Separation from primary target on the sky, degrees.
14.  Parallax, milli arc seconds.
15.  Angle of resolved disk, milli arc seconds.
16.  Object class: "p" for primary target, "s" for secondary target,
     "c" for calibrator. Calibrators are sorted by distance to primary,
     nearest to primary first, farthest from primary last.
17.  Number of Hipparcos components.
18.  Double/Multiple systems annex flag from Hipparcos catalogue.
19.  Hipparcos solution quality.
20.  Simbad description of object.

EXECUTION:
obs_cat out=obs__catalogue.ascii name="HD166" password="    "

GETTING INTO SIMBAD:
The Simbad data base wants a password. You can access SIMBAD two ways:
 
1. Give the password:
obs_cat ...  name="..." password="   "
 
2. Let kerberos get it for you by running the following unix code before
getting into vicar:
selcat o
kinit (which will ask you for your initials and kerberos password).
vicar
obs_cat ...  name="..."

.PAGE
METHOD:
1. You specify a target.
2. The objects coordinates and Hipparcos name are returned by Simbad.
3. All objects in the Hipparcos catalogue within the fov/2 of the target 
   are selected and subjected to a series of tests.
4. The target coordinates are updated using Hipparcos values.
5. Simbad is queried for each object to determine it's type.
6. All surviving objects (target first) are written to the output catalogue
   in order of apparent proximity to the target (nearest first).

Example of the output catalogue:

# UTC= "Fri May  8 16:17:11 1998"  Local= "Fri May  8 09:17:11 1998" GMT=1998128161711   Obs_cat_version=1
# Target HIP67275 known as tau_boo is a  V*
# owner=none mode=ssm priority=1 repeats=5 spacing=30 time=125 baseline=ns
#  HD          RA(2000.0) dec(2000.0)  pmRA    pmDEC   V    K   SpTyp  Separ Paral  Angle O C Flag  Simbad
 HIP67275     13 47 15.74 +17 27 24.9  -0.480  +0.054  4.5  3.2 F7V     0.00 64.12  0.701 p 01 - -  V*
 HIP66974     13 43 35.70 +14 21 56.2  -0.315  -0.007  7.0  5.5 G2V     3.21 16.55  0.154 c 01 - -  PM*
 HIP67470     13 49 36.06 +13 00 37.1  -0.148  +0.025  6.7  5.4 F8V     4.48 17.51  0.187 c 01 - -  Star
 HIP65350     13 23 37.41 +17 19 16.2  -0.035  -0.006  8.0  6.6 F7V     5.64  6.46  0.071 c 01 - -  Star
 HIP69178     14 09 37.20 +15 37 17.6  +0.042  -0.064  6.9  5.6 F7V     5.66 18.19  0.199 c 01 - -  Star
 HIP65085     13 20 25.71 +17 34 50.2  -0.110  +0.048  7.4  6.5 F2V     6.40 17.27  0.207 c 01 - -  Star
 HIP66640     13 39 34.62 +10 44 46.6  -0.113  -0.013  5.6  4.9 F0V     6.96 18.68  0.234 c 02 C A  **
 HIP67616     13 51 16.33 +24 41 44.7  -0.000  +0.000  7.7  5.5 K2V     7.30 17.51  0.132 c 01 - S  V*
 HIP64779     13 16 32.26 +19 47 07.0  -0.110  +0.030  6.5  5.8 F0V     7.64 11.65  0.146 c 02 C A  **
 HIP68064     13 56 09.52 +25 55 07.4  -0.097  -0.005  6.8  6.0 F2V     8.71  8.88  0.106 c 01 - -  EB*Algol
 HIP64081     13 07 54.87 +19 55 58.0  +0.000  -0.067  7.9  6.9 F4V     9.64  6.83  0.078 c 01 - -  Star

HISTORY:
2-1-98  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1

.VARI OUT
Output catalogue

.VARI USERID
Simbad userid
 
.VARI PASSWORD
Simbad password

.VARI PATH
full pathname of
hipparcos data file.

.VARI MODE
ssm (simgle star mode) or
dsm (dual star mode) or
dia (stellar diameter mode).

.VARI TIME
Scan duration.
(seconds)

.VARI REPS
Number of observations.

.VARI PRIORITY
Object priority.

.VARI OWNER
Your name.

.VARI SPACING
Time between observations.

.VARI NAME
Object name

.VARI FOV
diameter of the field
of view within which to
list Hipparcos objects.
(degrees)

.VARI VLIMIT
Limiting v magnitude.

.VARI KLIMIT
Limiting k magnitude.

.VARI PARLIMIT
Limiting parallax.

.VARI LODEC
Lower declination limit.

.VARI HIDEC
Upper declination limit.

.VARI STATUS
Directory to contain
status file.
Defaults to local.
 
.VARI COLOR
Acceptable MK colors

.VARI LUMINOSITY
Acceptable luminosities

.VARI HD
Target HD number.
Bypasses Simbad.

.VARI HIP
Target HIP number.
Bypasses Simbad.

.VARI BASENAME
The name of the baseline.
Defaults to ns.

.LEVEL2

.VARI OUT
Output ascii catalogue.

.VARI USERID
Simbad userid. 
This defaults to a valid userid unless you have your own.
 
.VARI PASSWORD
Simbad password.
See the METHOD section to permit kerberos to get it for you.

.VARI PATH
The full pathname of the hipparcos catalogue.

.VARI MODE
ssm (simgle star mode) or
dsm (dual star mode) or
dia (stellar diameter mode).
Defaults to ssm.
In dia mode only the target object is written to the output file. There are no
calibrators for this case.
 
.VARI TIME
Scan duration in seconds.
Defaults to 125
 
.VARI REPS
Number of observations per object.
Defaults to 5.
 
.VARI PRIORITY
Object priority. High numbers represent high priority.
Defaults to 1.
 
.VARI OWNER
Your name.
 
.VARI SPACING
Time between observations in minutes.
Defaults to 30 minutes.

.VARI NAME
The name of the object whose information goes in the output catalogue.
Example: name="betelgeuse"
A query will be sent to SIMBAD to retrieve the object's information.
 
.VARI FOV
The diameter of the field of view within which to list Hipparcos objects.
In degrees.

.VARI VLIMIT
Limiting v magnitude.
Objects with magnitudes above this (fainter) will be rejected.

.VARI KLIMIT
Limiting k magnitude.
Objects with magnitudes above this (fainter) will be rejected.

.VARI PARLIMIT
Limiting parallax.
Objects with parallax above this will be rejected.
In milli arc seconds per year.

.VARI LODEC
Lower declination limit.
Objects with declinations below this will be rejected.
Degrees.

.VARI HIDEC
Upper declination limit.
Objects with declinations above this will be rejected.
Degrees.

.VARI COLOR
Acceptable MK colors.
List all the acceptable MK color types as an upper case string.
Default is: colors="FGKM".
Just list the upper case letters.

.VARI LUMINOSITY
Acceptable luminosities classes.
You input a set of 5 integers representing luminosities: I II III IV V.
If the integer is 0 the class is ignored.
Default is: luminosity=(0,0,0,0,5) meaning only objects of class V are accepted.
(note: Any integer other than a 0 means to accept this class.)

.VARI HD
Target HD number.
HD specifies the target Henry Draper number directly. 
This causes the Simbad query to be skipped and the data to come 
directly from the Hipparcos catalogue.
Example: hd=39801

.VARI HIP
Target HIP number.
HIP specifies the target Hipparcos number directly. 
This causes the Simbad query to be skipped and the data to come 
directly from the Hipparcos catalogue.
Example HIP=27989

.VARI BASENAME
The name of the interferometer baseline to use during observations.
Currently acceptable names are: ns (for pti north-south) 
and nw (for pti north-west).
Defaults to ns.

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
$ create tstobs_cat.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
obs_cat out=cat.ascii name="Betelgeuse" color="GKM"
!
end-proc
$ Return
$!#############################################################################
