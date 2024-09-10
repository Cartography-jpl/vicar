$!****************************************************************************
$!
$! Build proc for MIPL module obs_list
$! VPACK Version 1.9, Thursday, August 27, 1998, 14:40:55
$!
$! Execute by entering:		$ @obs_list
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
$ write sys$output "*** module obs_list ***"
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
$ write sys$output "Invalid argument given to obs_list.com file -- ", primary
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
$   if F$SEARCH("obs_list.imake") .nes. ""
$   then
$      vimake obs_list
$      purge obs_list.bld
$   else
$      if F$SEARCH("obs_list.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake obs_list
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @obs_list.bld "STD"
$   else
$      @obs_list.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create obs_list.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack obs_list.com -
	-s obs_list.c novas.h novascon.h novascon.c solsys3.c -
	-i obs_list.imake -
	-p obs_list.pdf -
	-t tstobs_list.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create obs_list.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* obs_list */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "novas.h"
#include "vicmain_c"

#define PI 3.141592653589793238
#define MAXTABLE 1000    /* max number of records in input file */
#define MAXTARGET 100    /* max number of targets */
#define MAXSTAR 10       /* max stars per target (target & calibrators) */
#define N_INTERVALS 300   /* max number of timeline steps */

void main44()
{

   FILE *fout,*fin,*fout2,*fout3;
   char *ptr;
   char catalogue[80],observlist[80],timelinefile[80];
   char msg[200],msg2[200],sign,local_time[50],coordinated_universal_time[50];
   char buf[MAXTABLE+1][200],target_name[MAXTARGET+1][50];
   char locus_dir[100],header[200];
   char basenames[20];
   int basenum[MAXTARGET+1];
   int i,j,k,status,count,def,nrecs,ra_hr,ra_min,dec_deg,dec_min;
   int priority[MAXTARGET+1],repeats[MAXTARGET+1],spacing[MAXTARGET+1];
   int save_priority[MAXTARGET+1],best_location;
   int nstars[MAXTARGET+1],start[MAXTARGET+1],otime[MAXTARGET+1];
   int nrecs_per_target[MAXTARGET+1];
   int ntarget,tacquire,lst_time_hour,lst_time_minute,lst_time_second;
   int ut_time_hour,ut_time_minute,ut_time_second;
   int local_time_hour,local_time_minute,local_time_second;
   int m,n,kk,nn,inter,nbins,nspacing,nlst,n_intervals,locdate[6],nlocdate;
   int utdate[6],nutdate,ut_hour,local_hour,priority_range;
   int bins_filled,reset_repeats,observations[MAXTARGET+1],fixed_baseline;
   short int ind,visible[N_INTERVALS+1][MAXTARGET+1],objectlist[N_INTERVALS+1];
   double day,year,month,hour,minute,second,a,b,jd,array[4];
   double mobl,tobl,equinox,dpsi,deps,gst,lst;
   double ra[MAXTARGET+1][MAXSTAR+1],dec[MAXTARGET+1][MAXSTAR+1];
   double ra_sec,dec_sec,zenith_limit,delay_limit;
   double coszen,zenith,ha,bx,by,bz,delay,decr;
   double pos_sun[3],vel_sun[3],ra_sun,dec_sun,ha_sun,zenith_sun;
   double OBSERVATORY_LATITUDE,OBSERVATORY_LONGITUDE;
   double X_BASELINE,Y_BASELINE,Z_BASELINE,DELAY_CONSTANT;
   double SUN_ZENITH_LIMIT,jd_1970,ut_time,local_times;
   double ut_of_jd,ut_minus_lst,e,d,c,alpha,z,jdtemp,f,local_minus_ut;
   double ha_sum,quality,best_quality,start_timeline;
   float interval,timeline[N_INTERVALS+1],sun_zenith[N_INTERVALS+1];

   struct tm *time_structure;
   time_t sec,start_observations;
   static char version[]="  Obs_list_version=1\n";

/* baselines */
   static double baseline[3][4]={
       {0.,0.,0.,0.},
       {3.3,-37.16,-103.3,-12.906},              /* ns baseline */
       {3.109468,-81.685854,-28.211718,0.082780} /* nw baseline */
   };

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

/* get parameters */
   status=zvparm("TACQUIRE",&tacquire,&count,&def,1,0);
   status=zvparmd("ZENITH",&zenith_limit,&count,&def,1,0);
   status=zvparmd("DELAY",&delay_limit,&count,&def,1,0);
   status=zvparmd("LST",&lst,&nlst,&def,1,0);
   status=zvparm("LOCDATE",locdate,&nlocdate,&def,6,0);
   status=zvparm("UTDATE",utdate,&nutdate,&def,6,0);
   status=zvparmd("LATITUDE",&OBSERVATORY_LATITUDE,&count,&def,1,0);
   status=zvparmd("LONGITUDE",&OBSERVATORY_LONGITUDE,&count,&def,1,0);
   OBSERVATORY_LONGITUDE /= 15.; /*convert to hours*/
   status=zvparmd("SUNZENITH",&SUN_ZENITH_LIMIT,&count,&def,1,0);

   /* baselines */
   fixed_baseline=0;
   if(zvptst("NS")){
     printf("N-S baseline selected\n");
     X_BASELINE=baseline[1][0];
     Y_BASELINE=baseline[1][1];
     Z_BASELINE=baseline[1][2];
     DELAY_CONSTANT=baseline[1][3];
     fixed_baseline=1;
   }
   if(zvptst("NW")){
     printf("N-W baseline selected\n");
     X_BASELINE=baseline[2][0];
     Y_BASELINE=baseline[2][1];
     Z_BASELINE=baseline[2][2];
     DELAY_CONSTANT=baseline[2][3];
     fixed_baseline=1;
   }
   status=zvparmd("BASELINE",array,&count,&def,4,0);
   if(count == 4){
     X_BASELINE=array[0];
     Y_BASELINE=array[1];
     Z_BASELINE=array[2];
     DELAY_CONSTANT=array[3];
     fixed_baseline=1;
   }

/* compute julian date of 00:00:00 Jan 1 1970 (when God created unix)*/
   year=1969.;
   month=13.;
   day=1.0;
   a=floor(year/100.);
   b=2.0-a+floor(a/4.);
   jd_1970=floor(365.25*(year+4716.))+floor(30.6001*(month+1.))+day+b-1524.5;

/* get utc and local time for NOW */
   sec=time(NULL); /* seconds since 00:00;00 Jan 1 1970 */
   time_structure=gmtime(&sec);
   ut_hour=time_structure->tm_hour;
   ptr=asctime(time_structure);
   strcpy(coordinated_universal_time,ptr);
   ptr=ctime(&sec);
   local_hour=time_structure->tm_hour;
   strcpy(local_time,ptr);
   if(time_structure->tm_isdst > 0)printf("Daylight savings time in effect\n");
   if(time_structure->tm_isdst = 0)printf("Daylight savings time not in effect\n");
   if(time_structure->tm_isdst < 0)printf("Daylight savings time unknown\n");

/* create header record */
   strcpy(header,"# UTC= \"");
   strncat(header,coordinated_universal_time,strlen(coordinated_universal_time)-1);
   strcat(header,"\"  Local= \"");
   strncat(header,local_time,strlen(local_time)-1);
   strcat(header,"\"");
   time_structure=gmtime(&sec);
   sprintf(msg," GMT=%04d%03d%02d%02d%02d ",time_structure->tm_year + 1900,
     time_structure->tm_yday +1,time_structure->tm_hour,
     time_structure->tm_min,time_structure->tm_sec);
   strcat(header,msg);
   strcat(header,version);

/* compute jd from local time if provided, convert to ut then to jd. */
   if(nlocdate > 0){
     year=locdate[0];
     month=locdate[1];
     day=locdate[2];
     if(ut_hour > local_hour){ /* convert to ut */
       hour=locdate[3] + ut_hour - local_hour;
     }
     else{
       hour=locdate[3] + ut_hour - local_hour +24.;
     }
     if(hour > 24.){
       hour -= 24.;
       day += 1.;
     }
     minute=locdate[4];
     second=locdate[5];
     if(floor(month+.0001) < 3){
       year -= 1;
       month += 12.;
     }
     day += hour/24. +minute/(24.*60.) +second/(24.*60.*60);
     a=floor(year/100.);
     b=2.0-a+floor(a/4.);
     jd=floor(365.25*(year+4716.))+floor(30.6001*(month+1.))+day+b-1524.5;
   }

/* compute jd from ut if provided */
   if(nutdate > 0){
     year=utdate[0];
     month=utdate[1];
     day=utdate[2];
     hour=utdate[3];
     minute=utdate[4];
     second=utdate[5];
     if(floor(month+.0001) < 3){
       year -= 1;
       month += 12.;
     }
     day += hour/24. +minute/(24.*60.) +second/(24.*60.*60);
     a=floor(year/100.);
     b=2.0-a+floor(a/4.);
     jd=floor(365.25*(year+4716.))+floor(30.6001*(month+1.))+day+b-1524.5;
   }

/* if no date given get jd of now */
   if((nlocdate == 0) && (nutdate == 0))jd=jd_1970+sec/86400.;

/* compute local meridian (the RA of zero Hour angle) */
   if(nlst > 0){
     jd=0.0;
   }
   else{ 
     printf("times for start of timeline:\n");
     printf("julian date %f\n",jd);
     /* compute equation of the equinoxes */
     earthtilt(jd,&mobl,&tobl,&equinox,&dpsi,&deps);
     /* compute sidereal time */
     sidereal_time(jd,0.,equinox,&gst); /* Greenwich apparent sidereal time */
     printf("Greenwich local sidereal time %f\n",gst);
     /* compute local sidereal time */
     lst=gst-OBSERVATORY_LONGITUDE;
     if(lst < 0.0) lst += 24.0;
   }
   printf("local sidereal time %f\n",lst);

/* compute ut from jd */
   jdtemp=jd+0.5;
   z=floor(jdtemp);
   f=jdtemp-z;
   if(z < 2299161.0){
     a=z;
   }
   else{
     alpha=floor((z-1867216.25)/36524.25);
     a=z+1.0+alpha-floor(alpha/4.0);
   }
   b=a+1524.;
   c=floor((b-122.1)/365.25);
   d=floor(365.25*c);
   e=floor((b-d)/30.6001);
   day=b-d-floor(30.6001*e)+f;      /* day of month */
   ut_of_jd=(day-floor(day))*24.0;
   printf("UT at JD = %f\n",ut_of_jd);

/* compute differences in times */
   ut_minus_lst=ut_of_jd - lst;
   local_minus_ut=local_hour - ut_hour;
   printf("ut_minus_lst %f\n",ut_minus_lst);
   printf("local_minus_ut %f\n",local_minus_ut);

/* compute seconds from jan 1 1970 until the start of the timeline jd */
   start_timeline=(jd - jd_1970)*60.*60.*24.;

/* open output ascii catalogue of observing list for the scheduler*/
   status=zvpone("OUT",catalogue,1,sizeof(catalogue));
   if ((fout = fopen(catalogue, "w")) == NULL) {
      fprintf(stderr, "Error opening file %s\n", catalogue);
      fputs("Error opening output catalogue\n", stat_file);
      zabend();
   }

/* open output ascii status file timeline*/
   status=zvpone("OUT",timelinefile,2,sizeof(timelinefile));
   if ((fout2 = fopen(timelinefile, "w")) == NULL) {
      fprintf(stderr, "Error opening file %s\n", timelinefile);
      fputs("Error opening output timelinefile\n", stat_file);
      zabend();
   }

/* write header record */
   if (fputs(header, fout) == NULL) {
     fprintf(stderr, "Error writing file %s\n", catalogue);
     fputs("Error writing catalogue\n", stat_file);
     zabend();
   }

/* open input ascii catalogue of candidate objects*/
   status=zvpone("INP",observlist,1,sizeof(observlist));
   if ((fin = fopen(observlist, "r")) == NULL) { /* open catalogue */
      fprintf(stderr, "Error opening file %s\n",observlist);
      fputs("Error opening file\n", stat_file);
      zabend();
   }
 
/* read input catalogue into memory */
   for (i=1; i <= MAXTABLE; i++) {
     if (i == MAXTABLE){
       fprintf(stderr, "Too many records in file %s\n", observlist);
       fputs("Too many records in file\n", stat_file);
       zabend();
     }
     if (fgets(&buf[i][0], 200, fin) == NULL) break; /* read  blocks */
     nrecs=i;
   }

/* unpack input catalogue and create pointers to target locations */
/* start is the record number of each target block 
   nstars is the number of stars per target block 
   ntarget is the number of target blocks 
*/
   ntarget=0;
   for(i=1; i <= nrecs; i++){
     if(buf[i][0] == '#'){              /* comment line */
        if(buf[i][1] == '#'){           /* pure comment */
          nrecs_per_target[ntarget] += 1;
          continue;
        }
        if(buf[i][2] == 'U'){           /* header record comment */
          nrecs_per_target[ntarget] += 1; /* usually updates [0] only */
          continue;
        }
        if(buf[i][3] == 'H'){           /* columns comment */
          nrecs_per_target[ntarget] += 1;
          continue;
        }
        if(buf[i][2] == 'T'){           /* target line */
          ntarget += 1;
          if (ntarget > MAXTARGET){
            fprintf(stderr, "Too many targets in file %s\n", observlist);
            fputs("Too many targets in file\n", stat_file);
            zabend();
          }
          j=0; /* number of stars per target*/
          nrecs_per_target[ntarget]=1; /* number of records per target */
          start[ntarget]=i;
          sscanf(&buf[i][0],"%*s %*s %*s %*s %*s %s",&target_name[ntarget][0]); 
          continue;
        } 
        if(buf[i][2] == 'o'){           /* owner line */
          sscanf(&buf[i][0],"%*s %*s %*s %*9s%d %*8s%d %*8s%d %*5s%d %*9s%s",
                 &priority[ntarget],&repeats[ntarget],&spacing[ntarget],
                 &otime[ntarget],basenames);
          if(repeats[ntarget] < 1)repeats[ntarget]=1;
          /*printf("%d %d %d %d %s\n",priority[ntarget],repeats[ntarget],
                  spacing[ntarget],otime[ntarget],basenames);*/
          basenum[ntarget]=0;
          if(strcmp(basenames,"ns") == 0)basenum[ntarget]=1;
          if(strcmp(basenames,"nw") == 0)basenum[ntarget]=2;
          if(basenum[ntarget] == 0){
            fprintf(stderr, "unrecognizable baseline name %s\n",basenames);
            fputs("Unrecognizable baseline name\n", stat_file);
            zabend();
          }
          nrecs_per_target[ntarget] += 1;
          continue;
        }
        printf("unrecognized record # %d\n",i);
        printf("%s\n",&buf[i][0]);
        fprintf(stderr, "Unrecognized comment record\n");
        fputs("Unrecognized comment record\n", stat_file);
        zabend();
     }
     /* process a star */
     sscanf(&buf[i][0],"%*s %d %d %lf %c%d %d %lf",
            &ra_hr,&ra_min,&ra_sec,&sign,&dec_deg,&dec_min,&dec_sec);
     /*printf("%d %d %f %c%d %d %f\n",ra_hr,ra_min,ra_sec,sign,
           dec_deg,dec_min,dec_sec);*/
     j += 1;
     nrecs_per_target[ntarget] += 1;
     if (j > MAXSTAR){
       fprintf(stderr, "Too many calibrators for target %d\n", ntarget);
       fputs("Too many calibrators for a target\n", stat_file);
       zabend();
     }
     ra[ntarget][j]=(ra_hr+ra_min/60.+ra_sec/3600.)*15.*PI/180.;
     dec[ntarget][j]=(dec_deg+dec_min/60.+dec_sec/3600.)*PI/180.;
     if(sign == '-')dec[i][j]=-dec[i][j];
     nstars[ntarget]=j; 
   }
   for(j=1; j <= ntarget; j++){
     save_priority[j]=priority[j];
   }
   printf("%d target objects located in observing list\n",ntarget);

/* compute the range of priority */
   j=priority[1];
   k=j;
   for(i=1; i <= ntarget; i++){
     if(priority[i] > j)j=priority[i];
     if(priority[i] < k)k=priority[i];
   }
   priority_range=j-k+1;

/* set up timeline in local sidereal time */
   i=otime[1];
   for(j=1; j <= ntarget; j++){    /* get shortest observing time */
     if(otime[j] < i)i=otime[j];
   } 
   interval=tacquire+i/60.;  /* timeline interval in minutes */
   for(i=1; i <= N_INTERVALS; i++){
     timeline[i]=fmod((double)(lst+(i-1)*interval/60.),24.0); /* lst in hours */
     sun_zenith[i]=180.0;
   }
   printf("Time interval in minutes %f\n",interval);

/* compute sun zenith angle */
   n_intervals=N_INTERVALS;
   if(jd > 1.0){
    for(i=1; i <= N_INTERVALS; i++){
     /* get ra & dec of earth as seen from sun center */
     ind=solarsystem(jd+(i-1)*interval/1440.,2,1,pos_sun,vel_sun);
     if(ind > 0){
            fprintf(stderr, "SOLARSYSTEM: time out of range %s\n", observlist);
            fputs("SOLARSYSTEM: time out of range\n", stat_file);
            zabend();
     }
     /* reverse vector to see sun from the earth */
     pos_sun[0] = -pos_sun[0];
     pos_sun[1] = -pos_sun[1];
     pos_sun[2] = -pos_sun[2];
     ind=vector2radec(pos_sun,&ra_sun,&dec_sun); /* ra in hours, dec in degrees */
     if(ind > 0){
       printf("error: vector2radec\n");
     }
     ha_sun=(timeline[i]-ra_sun)*15.*PI/180.; /* hour angle in radians*/
     coszen=sin(OBSERVATORY_LATITUDE*PI/180.)*sin(dec_sun*PI/180.)+
          cos(OBSERVATORY_LATITUDE*PI/180.)*cos(dec_sun*PI/180.)*cos(ha_sun);
     zenith_sun=acos(coszen)*180./PI;
     sun_zenith[i]=zenith_sun;
    }

    /* set n_intervals to the index at sunrise */
    for(i=2; i <= N_INTERVALS; i++){
     if((sun_zenith[i-1] > 90.) && (sun_zenith[i] <= 90.)){
       n_intervals = i;
       if(n_intervals < 10){
         n_intervals=N_INTERVALS;
       }
       break;
     }
    }
   }
   printf("Timeline contains %d intervals\n",n_intervals);

/* see which objects can be seen in each timeline bin */
   for(i=1; i <= n_intervals; i++){
     if(sun_zenith[i] < SUN_ZENITH_LIMIT){   /* day time, mark as not visible */
       for(j=1; j <= ntarget; j++){
         visible[i][j]=0;   /* false, not visible */
       }
       continue;
     }
     for(j=1; j <= ntarget; j++){
       visible[i][j]=1;         /* default to visible */
       ha_sum=0.0;

       /* rotate interferometer baseline into earth coordinates z=pole y=east */
       if(!fixed_baseline){;
         k=basenum[j];
         X_BASELINE=baseline[k][0];
         Y_BASELINE=baseline[k][1];
         Z_BASELINE=baseline[k][2];
         DELAY_CONSTANT=baseline[k][3];
       }
       bx=X_BASELINE*cos(-OBSERVATORY_LATITUDE*PI/180.)+
          Z_BASELINE*sin(-OBSERVATORY_LATITUDE*PI/180.);
       bz=-X_BASELINE*sin(-OBSERVATORY_LATITUDE*PI/180.)+
          Z_BASELINE*cos(-OBSERVATORY_LATITUDE*PI/180.);
       by=Y_BASELINE;

       for(k=1; k <= nstars[j]; k++){
         ha=timeline[i]*15.*PI/180.-ra[j][k]; /* hour angle in radians*/
         ha_sum += fabs(ha);
         /* compute zenith angle */
         coszen=sin(OBSERVATORY_LATITUDE*PI/180.)*sin(dec[j][k])+
                cos(OBSERVATORY_LATITUDE*PI/180.)*cos(dec[j][k])*cos(ha);
         zenith=acos(coszen)*180./PI;
         if(zenith > zenith_limit){
           visible[i][j]=0;   /* false, not visible */
           break;
         }
         /* compute delay line length */
         delay=DELAY_CONSTANT+bz*sin(dec[j][k])-by*cos(dec[j][k])*sin(ha)+
               bx*cos(dec[j][k])*cos(ha);
         delay=fabs(delay);
         if(delay > delay_limit){
           visible[i][j]=0;   /* false, not visible */
           break;
         }
       }
       /* replace visible profile with weighting highest at zero hour angle */
       if(visible[i][j]){
         visible[i][j]=zenith_limit-ha_sum*(180./PI)/nstars[j]+1;
         if(visible[i][j] < 1)visible[i][j]=1;
       }
     }
   }

/* fill the timeline with target id numbers */
   for(i=1; i <= n_intervals; i++)objectlist[i]=0;
   reset_repeats=0;
   another_pass:;
   bins_filled=0;
   for(kk=1; kk <= ntarget; kk++){       /* loop until all targets exhausted */
     k=priority[1];
     m=1;
     for(j=1; j <= ntarget; j++){        /* find highest priority object */
       if(k < priority[j]){
         k=priority[j];
         m=j;
       }
     }
     inter=nstars[m]*(tacquire+otime[m]/60.);  /* obs time in minutes */
     nbins=(inter/interval) + 1;         /* observation time in timeline bins */
     nspacing=(spacing[m]/interval) + 1; /* spacing of observations in bins */
     if(nspacing < nbins)nspacing=nbins;
 
     if(spacing[m] > 0){
       /* find best empty timeline bins as those closest to the meridian */
       best_location=0;
       best_quality=0;
       for(i=1; i <= n_intervals; i++){
         quality=0.0;
         for(j=1; j <= repeats[m]; j++){
           n=i+(j-1)*nspacing; 
           for(k=n; k <= n+nbins-1; k++){
             if(k > n_intervals)   goto next_interval; /* no more time */ 
             if(! visible[k][m])   goto next_interval; /* not visible */
             if(objectlist[k] != 0)goto next_interval; /* bin occupied */
             quality += visible[k][m];     /* weighting is sum of visible */
           }
         }
         if(quality > best_quality){
           best_quality=quality;
           best_location=i;
         }
         next_interval:;
       }
     }
     else{
       /* find best empty timeline bins as those spaced the widest apart */
       best_location=0;
       for(nn=n_intervals/repeats[m]; nn > nbins; nn--){
         nspacing=nn;
         for(i=1; i <= n_intervals; i++){
           for(j=1; j <= repeats[m]; j++){
             n=i+(j-1)*nspacing; 
             for(k=n; k <= n+nbins-1; k++){
               if(k > n_intervals)   goto next_bin; /* no more time */ 
               if(! visible[k][m])   goto next_bin; /* not visible */
               if(objectlist[k] != 0)goto next_bin; /* bin occupied */
             }
           }
           best_location=i;
           goto found_one;
           next_bin:;
         }
       }
       found_one:;
     }

     /* mark the bins as used if test is true */
     if(best_location != 0){
       for(j=1; j <= repeats[m]; j++){   /* fill bins */
         n=best_location+(j-1)*nspacing; 
         for(k=n; k <= n+nbins-1; k++){
           if(k == n)objectlist[k]=m;    /* mark bin with target id */
           else objectlist[k]=-m;        /* mark bin as continuation */
           bins_filled += 1;
         }
       }
     }
     priority[m] -= priority_range;/* flag target as used by dropping priority*/
   }

   if(bins_filled > 0)goto another_pass;  /* try to fill empty timeline bins */
   if(reset_repeats == 0){
     for(i=1; i <= ntarget; i++)repeats[i]=1; /* set repeats to 1 to fill bins */
     reset_repeats=1;
     goto another_pass;
   }

/* print timeline and write the timeline file */
   for(i=1; i <= ntarget; i++)observations[i]=0;
   for(i=1; i <= n_intervals; i++){
     if(objectlist[i] > 0)observations[objectlist[i]] += 1;
   }
   for(i=1; i <= ntarget; i++){
     sprintf(msg,"Target # %d is %20s priority %d observations %d\n",
       i,&target_name[i][0],save_priority[i],observations[i]);
     printf(msg);
     if (fputs(msg, fout2) == NULL) {
       fprintf(stderr, "Error writing file %s\n",timelinefile);
       fputs("Error writing timeline file\n", stat_file);
       zabend();
     }
   }
   sprintf(msg,"  LST      UTC      Local  Sunzen   target visible_targets\n");
   printf(msg);
   if (fputs(msg, fout2) == NULL) {
       fprintf(stderr, "Error writing file %s\n",timelinefile);
       fputs("Error writing timeline file\n", stat_file);
       zabend();
   }
   for(i=1; i <= n_intervals; i++){
     lst_time_hour=timeline[i];
     lst_time_minute=(timeline[i]-lst_time_hour)*60.;
     lst_time_second=timeline[i]*3600.-lst_time_hour*3600.-lst_time_minute*60.;
     ut_time=timeline[i] + ut_minus_lst;
     if(ut_time < 0.0)ut_time += 24.0;
     if(ut_time > 24.0)ut_time -= 24.0;
     ut_time_hour=ut_time;
     ut_time_minute=(ut_time - ut_time_hour)*60.;
     ut_time_second=ut_time*3600.-ut_time_hour*3600.-ut_time_minute*60.;
     local_times=ut_time + local_minus_ut;
     if(local_times < 0.0)local_times += 24.0;
     if(local_times > 24.0)local_times -= 24.0;
     local_time_hour=local_times;
     local_time_minute=(local_times - local_time_hour)*60.;
     local_time_second=local_times*3600.-local_time_hour*3600.-local_time_minute*60.;
     if(objectlist[i] == 0){
       sign=' ';
       strcpy(msg2,"        ");
     }
     else{
       if(objectlist[i] > 0){
         sign='+';
         strcpy(msg2,&target_name[objectlist[i]][0]);
       }
       else{
         sign='-';
         strcpy(msg2,&target_name[-objectlist[i]][0]);
       }
     } 
     sprintf(msg,"%02d:%02d:%02d %02d:%02d:%02d %02d:%02d:%02d %03.0f %c%s ",
           lst_time_hour,lst_time_minute,lst_time_second,
           ut_time_hour,ut_time_minute,ut_time_second,
           local_time_hour,local_time_minute,local_time_second,
           sun_zenith[i],sign,msg2);
     for(j=1; j <= ntarget; j++){
       if(visible[i][j]){
         sprintf(msg2,"%s ",&target_name[j][0]);
         strcat(msg,msg2);
       }
     }
     if(i > 1){
       if((sun_zenith[i-1] > 90.) && (sun_zenith[i] <= 90.))strcat(msg," sunrise");
       if((sun_zenith[i-1] < 90.) && (sun_zenith[i] >= 90.))strcat(msg," sunset");
     }
     sprintf(msg2,"\n");
     strcat(msg,msg2);
     printf(msg);
     if (fputs(msg, fout2) == NULL) {
       fprintf(stderr, "Error writing file %s\n",timelinefile);
       fputs("Error writing timeline file\n", stat_file);
       zabend();
     }
   }

/* get ut of first observation bin */
   for(i=1; i <= n_intervals; i++){
     if(objectlist[i] > 0){
       start_observations=start_timeline + (i-1)*interval*60.; /* in seconds */
       time_structure=gmtime(&start_observations);
       sprintf(msg,"# UT_start=%04d%03d%02d%02d%02d\n",
         time_structure->tm_year + 1900,
         time_structure->tm_yday +1,time_structure->tm_hour,
         time_structure->tm_min,time_structure->tm_sec);
       if (fputs(msg, fout) == NULL) {
         fprintf(stderr, "Error writing file %s\n", catalogue);
         fputs("Error writing catalogue\n", stat_file);
         zabend();
       }
       break;
     }
   }

/* write output catalogue records */
   for(i=1; i <= n_intervals; i++){
     if(objectlist[i] > 0){
       lst_time_hour=timeline[i];
       lst_time_minute=(timeline[i]-lst_time_hour)*60.;
       lst_time_second=timeline[i]*3600.-lst_time_hour*3600.-lst_time_minute*60.;
       ut_time=timeline[i] + ut_minus_lst;
       if(ut_time < 0.0)ut_time += 24.0;
       if(ut_time > 24.0)ut_time -= 24.0;
       ut_time_hour=ut_time;
       ut_time_minute=(ut_time - ut_time_hour)*60.;
       ut_time_second=ut_time*3600.-ut_time_hour*3600.-ut_time_minute*60.;
       local_times=ut_time + local_minus_ut;
       if(local_times < 0.0)local_times += 24.0;
       if(local_times > 24.0)local_times -= 24.0;
       local_time_hour=local_times;
       local_time_minute=(local_times - local_time_hour)*60.;
       local_time_second=local_times*3600.-local_time_hour*3600.-local_time_minute*60.;
       sprintf(msg,"# LST=%02d:%02d:%02d UTC=%02d:%02d:%02d Local=%02d:%02d:%02d\n",
          lst_time_hour,lst_time_minute,lst_time_second,
          ut_time_hour,ut_time_minute,ut_time_second,
          local_time_hour,local_time_minute,local_time_second);
       if (fputs(msg, fout) == NULL) {
         fprintf(stderr, "Error writing file %s\n", catalogue);
         fputs("Error writing catalogue\n", stat_file);
         zabend();
       }
       m=objectlist[i];
       for(j=start[m]; j < start[m]+nrecs_per_target[m]; j++){
         if (fputs(&buf[j][0], fout) == NULL) {
           fprintf(stderr, "Error writing file %s\n", catalogue);
           fputs("Error writing catalogue\n", stat_file);
           zabend();
         }
       }
     }
   }
   fclose(fout); /* output catalogue */

/* write the locus of observable sky in file sky_locus.dat */
   status=zvparm("STATUS",locus_dir,&count,&def,1,0);
   if(strlen(locus_dir) == 0)strcpy(locus_dir,"sky_locus.dat");
   else strcat(locus_dir,"/sky_locus.dat");
   if ((fout3 = fopen(locus_dir, "w")) == NULL) {
      fprintf(stderr, "Error opening status file %s\n",locus_dir);
      zabend();
   }
   for(j=90; j >= -20; j--){   /* dec */
     decr=j*PI/180.;
     k=-1;
     for(i=180; i >= 0; i--){  /* ra  */ 
         ha=(90.-i)*PI/180.; /* hour angle for meridian of 90 deg */
         /* compute zenith angle */
         coszen=sin(OBSERVATORY_LATITUDE*PI/180.)*sin(decr)+
                cos(OBSERVATORY_LATITUDE*PI/180.)*cos(decr)*cos(ha);
         zenith=acos(coszen)*180./PI;
         /* compute delay line length */
         delay=DELAY_CONSTANT+bz*sin(decr)-by*cos(decr)*sin(ha)+
               bx*cos(decr)*cos(ha);
         delay=fabs(delay);
         if((zenith < zenith_limit) && (delay < delay_limit) && (k == -1))k=i;
         if(((zenith > zenith_limit) || (delay > delay_limit)) && (k != -1)){
           sprintf(msg,"Dec %f HA_limits %f %f\n",
            decr*180./PI,(90.-k)/15.,(90.-i)/15.);
           if (fputs(msg, fout) == NULL) {
             fprintf(stderr, "Error writing file %s\n",locus_dir);
             fputs("Error writing sky_locus.dat\n", stat_file);
             zabend();
           }
           break;
         }
     }
   }



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

/***********************************************************************
   NOVAS-C

   Naval Observatory Vector Astrometry Subroutines
   C Version 1.0
   June, 1996

   U. S. Naval Observatory
   Astronomical Applications Dept.
   3450 Massachusetts Ave., NW
   Washington, DC  20392-5420
*/

/********app_star */

short int app_star (double tjd, short int earth, fk5_entry *star,

                    double *ra, double *dec)
/*
------------------------------------------------------------------------

   PURPOSE:
      Computes the apparent place of a star at date 'tjd', given its
      mean place, proper motion, parallax, and radial velocity for
      J2000.0.

   REFERENCES:
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDT Julian date for apparent place.
      earth (short int)
         Body identification number for the Earth.
      *star (struct fk5_entry)
         Pointer to catalog entry structure (defined in novas.h).

   OUTPUT
   ARGUMENTS:
      *ra (double)
         Apparent right ascension in hours, referred to true equator
         and equinox of date 'tjd'.
      *dec (double)
         Apparent declination in degrees, referred to true equator
         and equinox of date 'tjd'.

   RETURNED
   VALUE:
      (short int)
          0...Everything OK.
         >0...Error code from function 'solarsystem'.

   GLOBALS
   USED:
      T0, FN0

   FUNCTIONS
   CALLED:
      get_earth       novas.c
      starvectors     novas.c
      proper_motion   novas.c
      geocentric      novas.c
      sun_field       novas.c
      aberration      novas.c
      precession      novas.c
      nutate          novas.c
      vector2radec    novas.c

   VER./DATE/
   PROGRAMMER:
      V1.0/01-93/TKB (USNO/NRL Optical Interfer.) Translate Fortran.
      V1.1/07-93/WTH (USNO/AA) Update to C standards.
      V1.2/10-95/WTH (USNO/AA) Added call to 'get_earth'.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'apstar'.

------------------------------------------------------------------------
*/
{
   double tdb,time2,peb[3],veb[3],pes[3],ves[3],pos1[3],pos2[3],pos3[3],
          pos4[3],pos5[3],pos6[3],pos7[3],vel1[3];

   short int error=0;

   if (error = get_earth (tjd,earth, &tdb,peb,veb,pes,ves))
   {
      *ra = 0.0;
      *dec = 0.0;
      return error;
   }

/*
   Compute apparent place
*/

   starvectors (star, pos1,vel1);
   proper_motion (T0,pos1,vel1,tdb, pos2);

   geocentric (pos2,peb, pos3,&time2);
   sun_field (pos3,pes, pos4);
   aberration (pos4,veb,time2, pos5);
   precession (T0,pos5,tdb, pos6);
   nutate (tdb,FN0,pos6, pos7);

   vector2radec (pos7, ra,dec);

   return 0;
}

/********topo_star */

short int topo_star (double tjd, short int earth, double deltat,
                     fk5_entry *star, site_info *location, 

                     double *ra, double *dec)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Computes the topocentric place of a star at date 'tjd', given its
      mean place, proper motion, parallax, and radial velocity for
      J2000.0 and the location of the observer.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDT Julian date for topocentric place.
      earth (short int)
         Body identification number for the Earth.
      deltat (double)
         Difference TDT-UT1 at 'tjd', in seconds.
      *star (struct fk5_entry)
         Pointer to catalog entry structure (defined in novas.h).
      *location (struct site_info)
         Pointer to structure containing observer's location (defined
         in novas.h).

   OUTPUT
   ARGUMENTS:
      *ra (double)
         Topocentric right ascension in hours, referred to true equator
         and equinox of date 'tjd'.
      *dec (double)
         Topocentric declination in degrees, referred to true equator
         and equinox of date 'tjd'.

   RETURNED
   VALUE:
      (short int)
          0...Everything OK.
         >0...Error code from function 'solarsystem'.

   GLOBALS
   USED:
      T0, FN1, FN0

   FUNCTIONS
   CALLED:
      get_earth       novas.c
      earthtilt       novas.c
      sidereal_time   novas.c
      terra           novas.c
      nutate          novas.c
      precession      novas.c
      starvectors     novas.c
      proper_motion   novas.c
      geocentric      novas.c
      sun_field       novas.c
      aberration      novas.c
      vector2radec    novas.c

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.
      V1.1/10-95/WTH (USNO/AA) Added call to 'get_earth'.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'tpstar'.

------------------------------------------------------------------------
*/
{
   double lighttime,ujd;

   double pob[3],pog[3],vob[3],vog[3],pos[3],gast,pos1[3],pos2[3],
          pos3[3],pos4[3],pos5[3],pos6[3],pos7[3],vel1[3],vel2[3],
          tdb,peb[3],veb[3],pes[3],ves[3],oblm,oblt,eqeq,psi,eps;

   short int j,error=0;

/*
   Compute 'ujd', the UT1 Julian date corresponding to 'tjd'.
*/

   ujd = tjd - (deltat / 86400.0);

/*
   Compute position and velocity of the observer, on mean equator
   and equinox of J2000.0, wrt the solar system barycenter and
   wrt to the center of the Sun.
*/

   if (error = get_earth (tjd,earth, &tdb,peb,veb,pes,ves))
   {
      *ra = 0.0;
      *dec = 0.0;
      return error;
   }

   earthtilt (tdb, &oblm,&oblt,&eqeq,&psi,&eps);

   sidereal_time (ujd,0.0,eqeq, &gast);
   terra (location,gast, pos1,vel1);
   nutate (tdb,FN1,pos1, pos2);
   precession (tdb,pos2,T0, pog);

   nutate (tdb,FN1,vel1, vel2);
   precession (tdb,vel2,T0, vog);

   for (j = 0; j < 3; j++)
   {
      pob[j] = peb[j] + pog[j];
      vob[j] = veb[j] + vog[j];
      pos[j] = pes[j] + pog[j];
   }

/*
   Finish topocentric place calculation.
*/

   starvectors (star, pos1,vel1);
   proper_motion (T0,pos1,vel1,tdb, pos2);
   geocentric (pos2,pob, pos3,&lighttime);
   sun_field (pos3,pos, pos4);
   aberration (pos4,vob,lighttime, pos5);
   precession (T0,pos5,tdb, pos6);
   nutate (tdb,FN0,pos6, pos7);

   vector2radec (pos7, ra,dec);

   return 0;
 }

/********app_planet */

short int app_planet (double tjd, short int planet, short int earth, 

                      double *ra, double *dec, double *dis)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Compute the apparent place of a planet or other solar system body.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDT Julian date for apparent place.
      planet (short int)
         Body identification number for desired planet.
      earth (short int)
         Body identification number for the Earth.

   OUTPUT
   ARGUMENTS:
      *ra (double)
         Apparent right ascension in hours, referred to true equator
         and equinox of date 'tjd'.
      *dec (double)
         Apparent declination in degrees, referred to true equator
         and equinox of date 'tjd'.
      *dis (double)
         True distance from Earth to planet at 'tjd' in AU.

   RETURNED
   VALUE:
      (short int)
          0...Everything OK.
         >0...Error code from function 'solarsystem'.

   GLOBALS
   USED:
      BARYC, C, T0, FN0

   FUNCTIONS
   CALLED:
      get_earth      novas.c
      solarsystem    (user's choice)
      geocentric     novas.c
      sun_field      novas.c
      aberration     novas.c
      precession     novas.c
      nutate         novas.c
      vector2radec   novas.c
      fabs           math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.
      V1.1/10-95/WTH (USNO/AA) Added call to 'get_earth'.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'applan'.

------------------------------------------------------------------------
*/
{
   double tdb,peb[3],veb[3],pes[3],ves[3],t2,t3,lighttime,pos1[3],
          vel1[3],pos2[3],pos3[3],pos4[3],pos5[3],pos6[3];

   short int error=0;

   if (error = get_earth (tjd,earth, &tdb,peb,veb,pes,ves))
   {
      *ra = 0.0;
      *dec = 0.0;
      return error;
   }

/*
   Get position of planet wrt barycenter of solar system.
*/

   if (error = solarsystem (tdb,planet,BARYC, pos1,vel1))
   {
      *ra = 0.0;
      *dec = 0.0;
      *dis = 0.0;
      return error;
   }

   geocentric (pos1,peb, pos2,&lighttime);
   *dis = lighttime * C;
   t3 = tdb - lighttime;

   do
   {
      t2 = t3;
      if (error = solarsystem (t2,planet,BARYC, pos1,vel1))
      {
         *ra = 0.0;
         *dec = 0.0;
         *dis = 0.0;
         return error;
      }

      geocentric (pos1,peb, pos2,&lighttime);
      t3 = tdb - lighttime;
   } while (fabs (t3-t2) > 1.0e-8);

/*
   Finish apparent place computation.
*/

   sun_field (pos2,pes, pos3);
   aberration (pos3,veb,lighttime, pos4);
   precession (T0,pos4,tdb, pos5);
   nutate (tdb,FN0,pos5, pos6);
   vector2radec (pos6, ra,dec);

   return 0;
}

/********topo_planet */

short int topo_planet (double tjd, short int planet, short int earth,
                       double deltat, site_info *location, 

                       double *ra, double *dec, double *dis)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Computes the topocentric place of a planet, given the location of
      the observer.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDT Julian date for topocentric place.
      planet (short int)
         Body identification number for desired planet.
      earth (short int)
         Body identification number for the Earth.
      deltat (double)
         Difference TDT-UT1 at 'tjd', in seconds.
      *location (struct site_info)
         Pointer to structure containing observer's location (defined
         in novas.h).

   OUTPUT
   ARGUMENTS:
      *ra (double)
         Topocentric right ascension in hours, referred to true
         equator and equinox of date 'tjd'.
      *dec (double)
         Topocentric declination in degrees, referred to true equator
         and equinox of date 'tjd'.
      *dis (double)
         True distance from observer to planet at 'tjd' in AU.

   RETURNED
   VALUE:
      (short int)
          0...Everything OK.
         >0...Error code from function 'solarsystem'.

   GLOBALS
   USED:
      T0, FN0, FN1, BARYC

   FUNCTIONS
   CALLED:
      get_earth        novas.c
      earthtilt        novas.c
      sidereal_time    novas.c
      terra            novas.c
      nutate           novas.c
      precession       novas.c
      geocentric       novas.c
      sun_field        novas.c
      aberration       novas.c
      vector2radec     novas.c
      solarsystem      (user's choice)
      fabs             math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.
      V1.1/10-95/WTH (USNO/AA) Added call to 'get_earth'.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'tpplan'. 

------------------------------------------------------------------------
*/
{
   short int j,error=0;

   double ujd,t2,t3,gast,pos1[3],pos2[3],pos4[3],pos5[3],pos6[3],
          pos7[3],vel1[3],vel2[3],pog[3],vog[3],pob[3],vob[3],pos[3],
          lighttime,tdb,peb[3],veb[3],pes[3],ves[3],oblm,oblt,
          eqeq,psi,eps;

/*
   Compute 'ujd', the UT1 Julian date corresponding to 'tjd'.
*/

   ujd = tjd - (deltat / 86400.0);

/*
   Compute position and velocity of the observer, on mean equator
   and equinox of J2000.0, wrt the solar system barycenter and
   wrt to the center of the Sun.
*/

   if (error = get_earth (tjd,earth, &tdb,peb,veb,pes,ves))
   {
      *ra = 0.0;
      *dec = 0.0;
      *dis = 0.0;
      return error;
   }

   earthtilt (tdb, &oblm,&oblt,&eqeq,&psi,&eps);

   sidereal_time (ujd,0.0,eqeq, &gast);
   terra (location,gast, pos1,vel1);
   nutate (tdb,FN1,pos1, pos2);
   precession (tdb,pos2,T0, pog);

   nutate (tdb,FN1,vel1, vel2);
   precession (tdb,vel2,T0, vog);

   for (j = 0; j < 3; j++)
   {
      pob[j] = peb[j] + pog[j];
      vob[j] = veb[j] + vog[j];
      pos[j] = pes[j] + pog[j];
   }

/*
   Compute the apparent place of the planet using the position and
   velocity of the observer.

   Get position of planet wrt barycenter of solar system.
*/

   if (error = solarsystem (tdb,planet,BARYC, pos1,vel1))
   {
      *ra = 0.0;
      *dec = 0.0;
      *dis = 0.0;
      return error;
   }

   geocentric (pos1,pob, pos2,&lighttime);
   *dis = lighttime * C;
   t3 = tdb - lighttime;

   do
   {
      t2 = t3;

      if (error = solarsystem (t2,planet,BARYC, pos1,vel1))
      {
         *ra = 0.0;
         *dec = 0.0;
         *dis = 0.0;
         return error;
      }
      geocentric (pos1,pob, pos2,&lighttime);
      t3 = tdb - lighttime;

   } while (fabs (t3-t2) > 1.0e-8);

/*
   Finish topocentric place.
*/

   sun_field (pos2,pos, pos4);
   aberration (pos4,vob,lighttime, pos5);
   precession (T0,pos5,tdb, pos6);
   nutate (tdb,FN0,pos6, pos7);
   vector2radec (pos7, ra,dec);

   return error ;
}


/********virtual_star */

short int virtual_star (double tjd, short int earth, fk5_entry *star,

                        double *ra, double *dec)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Computes the virtual place of a star at date 'tjd', given its
      mean place, proper motion, parallax, and radial velocity for
      J2000.0.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDT Julian date for virtual place.
      earth (short int)
         Body identification number for the earth.
      *star (struct fk5_entry)
         Pointer to catalog entry structure (defined in novas.h).

   OUTPUT
   ARGUMENTS:
      *ra (double)
         Virtual right ascension in hours, referred to mean equator
         and equinox of J2000.
      *dec (double)
         Virtual declination in degrees, referred to mean equator
         and equinox of J2000.

   RETURNED
   VALUE:
      (short int)
          0...Everything OK.
         >0...Error code from function 'solarsystem'.

   GLOBALS
   USED:
      T0

   FUNCTIONS
   CALLED:
      get_earth       novas.c
      starvectors     novas.c
      proper_motion   novas.c
      geocentric      novas.c
      sun_field       novas.c
      aberration      novas.c
      vector2radec    novas.c

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.
      V1.1/10-95/WTH (USNO/AA) Added call to 'get_earth'.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'vpstar'.

------------------------------------------------------------------------
*/
{
   double pos1[3],vel1[3],pos2[3],pos3[3],pos4[3],pos5[3],
          tdb,peb[3],veb[3],pes[3],ves[3],lighttime;

   short int error=0;


   if (error = get_earth (tjd,earth, &tdb,peb,veb,pes,ves))
   {
      *ra = 0.0;
      *dec = 0.0;
      return error;
   }

/*
   Compute virtual place.
*/

   starvectors (star, pos1,vel1);
   proper_motion (T0,pos1,vel1,tdb, pos2);
   geocentric (pos2,peb, pos3,&lighttime);
   sun_field (pos3,pes, pos4);
   aberration (pos4,veb,lighttime, pos5);

   vector2radec (pos5, ra,dec);

   return 0;
 }

/********local_star */

short int local_star (double tjd, short int earth, double deltat,
                      fk5_entry *star, site_info *location,

                      double *ra, double *dec)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Computes the local place of a star, given its mean place, proper
      motion, parallax, and radial velocity for J2000.0, and the
      location of the observer.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDT Julian date for local place.
      earth (short int)
         Body identification number for the Earth.
      deltat (double)
         Difference TDT-UT1 at 'tjd', in seconds.
      *star (struct fk5_entry)
         Pointer to catalog entry structure (defined in novas.h).
      *location (struct site_info)
         Pointer to structure containing observer's location (defined
         in novas.h).

   OUTPUT
   ARGUMENTS:
      *ra (double)
         Local right ascension in hours, referred to mean equator and
         equinox of J2000.
      *dec (double)
         Local declination in degrees, referred to mean equator and
         equinox of J2000.

   RETURNED
   VALUE:
      (short int)
          0...Everything OK.
         >0...Error code from function 'solarsystem'.

   GLOBALS
   USED:
      T0, FN1

   FUNCTIONS
   CALLED:
      get_earth        novas.c
      earthtilt        novas.c
      sidereal_time    novas.c
      terra            novas.c
      nutate           novas.c
      precession       novas.c
      starvectors      novas.c
      proper_motion    novas.c
      geocentric       novas.c
      sun_field        novas.c
      aberration       novas.c
      vector2radec     novas.c

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.
      V1.1/10-95/WTH (USNO/AA) Added call to 'get_earth'.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'lpstar'.

------------------------------------------------------------------------
*/
{
   double gast,lighttime,ujd,pog[3],vog[3],pb[3],vb[3],ps[3],vs[3],
          pos1[3],vel1[3],pos2[3],vel2[3],pos3[3],pos4[3],pos5[3],
          tdb,peb[3],veb[3],pes[3],ves[3],oblm,oblt,eqeq,psi,eps;

   short int j,error=0;

/*
   Compute 'ujd', the UT1 Julian date corresponding to 'tjd'.
*/

   ujd = tjd - (deltat / 86400.0);

/*
   Get position and velocity of observer wrt barycenter of solar system
   and wrt center of the sun.
*/

   if (error = get_earth (tjd,earth, &tdb,peb,veb,pes,ves))
   {
      *ra = 0.0;
      *dec = 0.0;
      return error;
   }

   earthtilt (tdb, &oblm,&oblt,&eqeq,&psi,&eps);

   sidereal_time (ujd,0.0,eqeq, &gast);
   terra (location,gast, pos1,vel1);
   nutate (tdb,FN1,pos1, pos2);
   precession (tdb,pos2,T0, pog);
   nutate (tdb,FN1,vel1, vel2);
   precession (tdb,vel2,T0, vog);

   for (j = 0; j < 3; j++)
   {
      pb[j] = peb[j] + pog[j];
      vb[j] = veb[j] + vog[j];
      ps[j] = pes[j] + pog[j];
      vs[j] = ves[j] + vog[j];
   }

/*
   Compute local place.
*/

   starvectors (star, pos1,vel1);
   proper_motion (T0,pos1,vel1,tdb, pos2);
   geocentric (pos2,pb, pos3,&lighttime);
   sun_field (pos3,ps, pos4);
   aberration (pos4,vb,lighttime, pos5);

   vector2radec (pos5, ra,dec);

   return 0;
}

/********virtual_planet */

short int virtual_planet (double tjd, short int planet, short int earth,

                          double *ra, double *dec, double *dis)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Computes the virtual place of a planet or other solar system body.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDT Julian date for virtual place.
      earth (short int)
         Body identification number for the Earth.
      planet (short int)
         Body identification number for desired planet.

   OUTPUT
   ARGUMENTS:
      *ra (double)
         Virtual right ascension in hours, referred to mean equator
         and equinox of J2000.
      *dec (double)
         Virtual declination in degrees, referred to mean equator
         and equinox of J2000.
      *dis (double)
         True distance from Earth to planet in AU.

   RETURNED
   VALUE:
      (short int)
          0...Everything OK.
         >0...Error code from function 'solarsystem'.

   GLOBALS
   USED:
      BARYC, C 

   FUNCTIONS
   CALLED:
      solarsystem    (user's choice)
      geocentric     novas.c
      sun_field      novas.c
      aberration     novas.c
      vector2radec   novas.c
      get_earth      novas.c
      earthtilt      novas.c
      fabs           math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.
      V1.1/10-95/WTH (USNO/AA) Added call to 'get_earth'.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'vpplan'.

------------------------------------------------------------------------
*/
{
   double t2=0.0,t3=0.0,lighttime,pos1[3],vel1[3],pos2[3],pos3[3],
          pos4[3],tdb,peb[3],veb[3],pes[3],ves[3],oblm,oblt,eqeq,psi,
          eps;

   short int error=0;

/*
   Get position of Earth wrt barycenter of solar system.
*/

   if (error = get_earth (tjd,earth, &tdb,peb,veb,pes,ves))
   {
      *ra = 0.0;
      *dec = 0.0;
      return error;
   }

   earthtilt (tdb, &oblm,&oblt,&eqeq,&psi,&eps);

/*
   Get position of planet wrt barycenter of solar system.
*/

   if (error = solarsystem (tdb,planet,BARYC, pos1,vel1))
   {
      *ra = 0.0;
      *dec = 0.0;
      return error;
   }
   geocentric (pos1,peb, pos2,&lighttime);
   *dis = lighttime * C;
   t3 = tdb - lighttime;

   do
   {
      t2 = t3;
      if (error = solarsystem (t2,planet,BARYC, pos1,vel1))
      {
         *ra = 0.0;
         *dec = 0.0;
         return error;
      }
      geocentric (pos1,peb, pos2,&lighttime);
      t3 = tdb - lighttime;
   } while (fabs (t3 - t2) > 1.0e-8);

/*
   Finish virtual place computation.
*/

   sun_field (pos2,pes, pos3);
   aberration (pos3,veb,lighttime, pos4);
   vector2radec (pos4, ra,dec);

   return 0;
}

/********local_planet */

short int local_planet (double tjd, short int planet, short int earth,
                        double deltat, site_info *location,

                        double *ra, double *dec, double *dis)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Computes the local place of a planet or other solar system body,
      given the location of the observer.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDT Julian date for local place.
      earth (short int)
         Body identification number for the Earth.
      planet (short int)
         Body identification number for desired planet.
      deltat (double)
         Difference TDT-UT1 at 'tjd', in seconds.
      *location (struct site_info)
         Pointer to structure containing observer's location (defined
         in novas.h).

   OUTPUT
   ARGUMENTS:
      *ra (double)
         Local right ascension in hours, referred to mean equator and
         equinox of J2000.
      *dec (double)
         Local declination in degrees, referred to mean equator and
         equinox of J2000.
      *dis (double)
         True distance from Earth to planet in AU.

   RETURNED
   VALUE:
      (short int)
          0...Everything OK.
         >0...Error code from function 'solarsystem'.

   GLOBALS
   USED:
      T0, BARYC, FN1

   FUNCTIONS
   CALLED:
      get_earth       novas.c
      earthtilt       novas.c
      sidereal_time   novas.c
      terra           novas.c
      nutate          novas.c
      precession      novas.c
      geocentric      novas.c
      sun_field       novas.c
      aberration      novas.c
      vector2radec    novas.c
      solarsystem     (user's choice)
      fabs            math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.
      V1.1/10-95/WTH (USNO/AA) Added call to 'get_earth'.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'lpplan'.

------------------------------------------------------------------------
*/
{

   double t2=0.0,t3=0.0,gast,lighttime,ujd,pog[3],vog[3],pb[3],vb[3],
          ps[3],vs[3],pos1[3],vel1[3],pos2[3],vel2[3],pos3[3],pos4[3],
          tdb,peb[3],veb[3],pes[3],ves[3],oblm,oblt,eqeq,psi,eps;

   short int j, error=0;

/*
   Compute 'ujd', the UT1 Julian date corresponding to 'tjd'.
*/

   ujd = tjd - (deltat / 86400.0);

/*
   Get position of Earth wrt the center of the Sun and the barycenter
   of solar system.
*/

   if (error = get_earth (tjd,earth, &tdb,peb,veb,pes,ves))
   {
      *ra = 0.0;
      *dec = 0.0;
      return error;
   }

   earthtilt (tdb, &oblm,&oblt,&eqeq,&psi,&eps);

/*
   Get position and velocity of observer wrt center of the Earth.
*/

   sidereal_time (ujd,0.0,eqeq, &gast);
   terra (location,gast, pos1,vel1);
   nutate (tdb,FN1,pos1, pos2);
   precession (tdb,pos2,T0, pog);
   nutate(tdb,FN1,vel1, vel2);
   precession (tdb,vel2,T0, vog);

/*
   Get position and velocity of observer wrt barycenter of solar system
   and wrt center of the sun.
*/

   for (j = 0; j < 3; j++)
   {
      pb[j] = peb[j] + pog[j];
      vb[j] = veb[j] + vog[j];
      ps[j] = pes[j] + pog[j];
      vs[j] = ves[j] + vog[j];
   }

/*
   Get position of planet wrt barycenter of solar system.
*/

   if (error = solarsystem (tdb,planet,BARYC, pos1,vel1))
   {
      *ra= 0.0;
      *dec = 0.0;
      *dis = 0.0;
      return error;
   }
   geocentric (pos1,pb, pos2,&lighttime);

   *dis = lighttime * C;
   t3 = tdb - lighttime;

   do
   {
      t2 = t3;
      if (error = solarsystem (t2,planet,BARYC, pos1,vel1))
      {
         *ra = 0.0;
         *dec = 0.0;
         return error;
      }
      geocentric (pos1,pb, pos2,&lighttime);
      t3 = tdb - lighttime;
   } while (fabs (t3 - t2) > 1.0e-8);

/*
   Finish local place computation.
*/

   sun_field (pos2,ps, pos3);
   aberration (pos3,vb,lighttime, pos4);
   vector2radec (pos4, ra,dec);

   return 0;
}

/********astro_star */

short int astro_star (double tjd, short int earth, fk5_entry *star,

                      double *ra, double *dec)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Computes the astrometric place of a star, given its mean place,
      proper motion, parallax, and radial velocity for J2000.0.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDT Julian date for astrometric place.
      earth (short int)
         Body identification number for the Earth.
      *star (struct fk5_entry)
         Pointer to catalog entry structure (defined in novas.h).

   OUTPUT
   ARGUMENTS:
      *ra (double)
         Astrometric right ascension in hours, referred to mean equator
         and equinox of J2000.
      *dec (double)
         Astrometric declination in degrees, referred to mean equator
         and equinox of J2000.

   RETURNED
   VALUE:
      (short int)
          0...Everything OK.
         >0...Error code from function 'solarsystem'.

   GLOBALS
   USED:
      T0

   FUNCTIONS
   CALLED:
      get_earth       novas.c
      starvectors     novas.c
      proper_motion   novas.c
      geocentric      novas.c
      vector2radec    novas.c

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.
      V1.1/10-95/WTH (USNO/AA) Added call to 'get_earth'.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'asstar'.

------------------------------------------------------------------------
*/
{
   double lighttime,pos1[3],vel1[3],pos2[3],pos3[3],tdb,peb[3],veb[3],
          pes[3],ves[3];

   short int error=0;

   if (error = get_earth (tjd,earth, &tdb,peb,veb,pes,ves))
   {
      *ra = 0.0;
      *dec = 0.0;
      return error;
   }

/*
   Compute astrometric place.
*/

   starvectors (star, pos1,vel1);
   proper_motion (T0,pos1,vel1,tdb, pos2);
   geocentric (pos2,peb, pos3,&lighttime);

   vector2radec (pos3, ra,dec);

   return 0;
}

/********astro_planet */

short int astro_planet (double tjd, short int planet, short int earth,

                        double *ra, double *dec, double *dis)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Computes the astrometric place of a planet or other solar system
      body.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDT Julian date for calculation.
      planet (short int)
         Body identification number for desired planet.
      earth (short int)
         Body identification number for the Earth.

   OUTPUT
   ARGUMENTS:
      *ra (double)
         Astrometric right ascension in hours, referred to mean equator
         and equinox of J2000.
      *dec (double)
         Astrometric declination in degrees, referred to mean equator
         and equinox of J2000.
      *dis (double)
         True distance from Earth to planet in AU.

   RETURNED
   VALUE:
      (short int)
          0...Everything OK.
         >0...Error code from function 'solarsystem'.

   GLOBALS
   USED:
      C, BARYC

   FUNCTIONS
   CALLED:
      get_earth      novas.c
      solarsystem    (user's choice)
      geocentric     novas.c
      vector2radec   novas.c
      fabs           math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.
      V1.1/10-95/WTH (USNO/AA) Added call to 'get_earth'.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'asplan'.

------------------------------------------------------------------------
*/
{
   double t2=0.0,t3=0.0,lighttime,pos1[3],vel1[3],pos2[3],tdb,peb[3],
          veb[3],pes[3],ves[3];

   short int error=0;

/*
   Get position of the Earth wrt center of Sun and barycenter of the
   solar system.
*/

   if (error = get_earth (tjd,earth, &tdb,peb,veb,pes,ves))
   {
      *ra = 0.0;
      *dec = 0.0;
      return error;
   }

/*
   Get position of planet wrt barycenter of solar system.
*/

   if (error = solarsystem (tdb,planet,BARYC, pos1,vel1))
   {
      *ra = 0.0;
      *dec = 0.0;
      *dis = 0.0;
      return error;
   }
   geocentric (pos1,peb, pos2,&lighttime);
   *dis = lighttime * C;
   t3 = tdb - lighttime;
   do
   {
      t2 = t3;
      if (error = solarsystem (t2,planet,BARYC, pos1,vel1))
      {
         *ra = 0.0;
         *dec = 0.0;
         *dis = 0.0;
         return error;
      }
      geocentric (pos1,peb, pos2,&lighttime);
      t3 = tdb - lighttime;
   } while (fabs (t3 - t2) > 1.0e-8);

/*
   Finish astrometric place computation.
*/

   vector2radec (pos2, ra,dec);

   return 0;
}

/********mean_star */

short int mean_star (double tjd, short int earth, double ra, double dec,

                     double *mra, double *mdec)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Computes the mean place of a star for J2000.0, given its apparent
      place at date 'tjd'.  Proper motion, parallax and radial velocity
      are assumed to be zero.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDT Julian date of apparent place.
      earth (short int)
         Body identification number for the Earth.
      ra (double)
         Apparent right ascension in hours, referred to true equator
         and equinox of date 'tjd'.
      dec (double)
         Apparent declination in degrees, referred to true equator
         and equinox of date 'tjd'.

   OUTPUT
   ARGUMENTS:
      *mra (double)
         Mean right ascension J2000.0 in hours.
      *mdec (double)
         Mean declination J2000.0 in degrees.

   RETURNED
   VALUE:
      (short int)
           0...Everything OK.
           1...Iterative process did not converge after 20 iterations.
         >10...Error from function 'app_star'.

   GLOBALS
   USED:
      None.

   FUNCTIONS
   CALLED:
      app_star     novas.c
      fmod         math.h
      fabs         math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'mpstar'.

------------------------------------------------------------------------
*/
{
   double newmra,newdec,oldmra,olddec,ra2,dec2,deltara,deltadec;

   short int iter=0, error=0;

   fk5_entry tempstar={"dummy",0,0.0,0.0,0.0,0.0,0.0,0.0,0.0};

   newmra = fmod (ra,24.0);
   if (newmra < 0.0) 
      newmra += 24.0;
   newdec = dec;

   do
   {
      oldmra = newmra;
      olddec = newdec;
      tempstar.ra = oldmra;
      tempstar.dec = olddec;
      if (error = app_star (tjd,earth,&tempstar, &ra2,&dec2))
      {
         *mra = 0.0;
         *mdec = 0.0;
         return (error + 10);
      }
      deltara = ra2 - oldmra;
      deltadec = dec2 - olddec;
      if (deltara < -12.0)
         deltara += 24.0;
      if (deltara > 12.0)
         deltara -= 24.0;
      newmra = ra - deltara;
      newdec = dec - deltadec;

      if (iter >= 20)
      {
         *mra = 0.0;
         *mdec = 0.0;
         return 1;
      }
       else
         iter++;

   } while ((fabs (newmra - oldmra) > 1.0e-10) && 
            (fabs (newdec - olddec) > 1.0e-9));

   *mra = newmra;
   *mdec = newdec;
   if (*mra < 0.0) 
      *mra += 24.0;
   if (*mra >= 24.0) 
      *mra -= 24.0;

   return 0;
}

/******** get_earth */

short int get_earth (double tjd, short int earth,

                     double *tdb,
                     double *bary_earthp, double *bary_earthv,
                     double *helio_earthp, double *helio_earthv)
/*
------------------------------------------------------------------------

   PURPOSE:
      Obtains the barycentric & heliocentric positions and velocities
      of the Earth from the solar system ephemeris.

   REFERENCES:
      None.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDT Julian date.
      earth (short int)
         Body identification number for the Earth.

   OUTPUT
   ARGUMENTS:
      *tdb (double)
         TDB Julian date corresponding to 'tjd'.
      *bary_earthp (double)
         Barycentric position vector of Earth at 'tjd'; equatorial
         rectangular coordinates in AU referred to the mean equator
         and equinox of J2000.0.
      *bary_earthv (double)
         Barycentric velocity vector of Earth at 'tjd'; equatorial
         rectangular system referred to the mean equator and equinox 
         of J2000.0, in AU/Day.
      *helio_earthp (double)
         Heliocentric position vector of Earth at 'tjd'; equatorial
         rectangular coordinates in AU referred to the mean equator
         and equinox of J2000.0.
      *helio_earthv (double)
         Heliocentric velocity vector of Earth at 'tjd'; equatorial
         rectangular system referred to the mean equator and equinox
         of J2000.0, in AU/Day.

   RETURNED
   VALUE:
      (short int)
          0...Everything OK.
         >0...Error code from function 'solarsystem'.

   GLOBALS
   USED:
      BARYC, HELIOC

   FUNCTIONS
   CALLED:
      convert_tdb2tdt     novas.c
      solarsystem         (user's choice)
      fabs                math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/10-95/WTH (USNO/AA)

   NOTES:
      None.

------------------------------------------------------------------------
*/
{
   static double time1,tjd_last=0.0,peb[3],veb[3],pes[3],ves[3];

   double dummy,secdiff;

   short int i, error=0;

/*
   Compute the TDB Julian date corresponding to 'tjd'.
*/

   if (fabs (tjd - tjd_last) > 1.0e-6)
   {
      convert_tdb2tdt (tjd, &dummy,&secdiff);
      time1 = tjd + secdiff / 86400.0;

/*
   Get position and velocity of the Earth wrt barycenter of solar system
   and wrt center of the sun.
*/

      if (error = solarsystem (time1,earth,BARYC, peb,veb))
      {
         tjd_last = 0.0;
         return error;
      }

      if (error = solarsystem (time1,earth,HELIOC, pes,ves))
      {
         tjd_last = 0.0;
         return error;
      }
      tjd_last = tjd;
   }

   *tdb = time1;
   for (i = 0; i < 3; i++)
   {
      bary_earthp[i] = peb[i];
      bary_earthv[i] = veb[i];
      helio_earthp[i] = pes[i];
      helio_earthv[i] = ves[i];
   }

   return error;
}

/******** sidereal_time */

void sidereal_time (double julianhi, double julianlo, double ee,

                    double *gst)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Computes the Greenwich apparent sidereal time, at Julian date
      'julianhi' + 'julianlo'.

   REFERENCES: 
      Aoki, et al. (1982) Astronomy and Astrophysics 105, 359-361.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      julianhi (double)
         Julian date, integral part.
      julianlo (double)
         Julian date, fractional part.
      ee (double)
         Equation of the equinoxes (seconds of time)

   OUTPUT
   ARGUMENTS:
      *gst (double)
         Greenwich apparent sidereal time, in hours.

   RETURNED
   VALUE:
      None.

   GLOBALS
   USED:
      T0

   FUNCTIONS
   CALLED:
      pow     math.h
      fmod    math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/06-92/TKB (USNO/NRL Optical Interfer.) Translate Fortran.
      V1.1/08-93/WTH (USNO/AA) Update to C programing standards.

   NOTES:
      1. This function is based on Fortran NOVAS routine 'sidtim'.
      2. The Julian date may be split at any point, but for highest
      precision, set 'julianhi' to be the integral part of the Julian
      date, and set 'julianlo' to be the fractional part.

------------------------------------------------------------------------
*/
{
   double timehi, timelo, time, st;

   timehi = (julianhi -  T0) / 36525.0;
   timelo = julianlo / 36525.0;
   time = timehi + timelo;

   st =  - 6.2e-6 * pow (time, 3.0) + 0.093104 * pow (time, 2.0)
      + 67310.54841 + 8640184.812866 * timelo + 3155760000.0 * timelo
      + 8640184.812866 * timehi + 3155760000.0 * timehi;

   *gst = (st + ee) / 3600.0;
   *gst = fmod (*gst, 24.0);

   if (*gst < 0.0)
      *gst += 24.0;

   return;
}

/********pnsw */

void pnsw (double tjd, double gast, double x, double y, double *vece,

           double *vecs)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Transforms a vector from an Earth-fixed geographic system to a
      space-fixed system based on mean equator and equinox of J2000.0;
      applies rotations for wobble, spin, nutation, and precession.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDT Julian date
      gast (double)
         Greenwich apparent sidereal time, in hours.
      x (double)
         Conventionally-defined X coordinate of rotational pole with
         respect to CIO, in arcseconds.
      y (double)
         Conventionally-defined Y coordinate of rotational pole with
         respect to CIO, in arcseconds.
      vece[3] (double)
         Vector in geocentric rectangular Earth-fixed system,
         referred to geographic equator and Greenwich meridian.

   OUTPUT
   ARGUMENTS:
      vecs[3] (double)
         Vector in geocentric rectangular space-fixed system,
         referred to mean equator and equinox of J2000.0.

   RETURNED
   VALUE:
      None.

   GLOBALS
   USED:
      T0, FN0

   FUNCTIONS
   CALLED:
      convert_tdb2tdt   novas.c
      wobble            novas.c
      spin              novas.c
      nutate            novas.c
      precession        novas.c

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'pnsw'.
      2. 'tjd' = 0 means no precession/nutation transformation.
      3. 'gast' = 0 means no spin transformation.
      4. 'x' = 'y' = 0 means no wobble transformation.

------------------------------------------------------------------------
*/
{
   double dummy,secdiff,v1[3],v2[3],v3[3],tdb;

   short int j;

/*
   Compute 'tdb', the TDB Julian date corresponding to 'tjd'.
*/

   if (tjd != 0.0)
   {
      convert_tdb2tdt (tjd, &dummy,&secdiff);
      tdb = tjd + secdiff / 86400.0;
   }

   if ((x == 0.0) && (y == 0.0))
   {
      for (j = 0; j < 3; j++)
         v1[j] = vece[j];
   }
    else
      wobble (x,y,vece, v1);

   if (gast == 0.0)
   {
      for (j = 0; j < 3; j++)
         v2[j] = v1[j];
   }
    else
      spin (gast,v1, v2);

   if (tjd == 0.0)
   {
      for (j = 0; j < 3; j++)
         vecs[j] = v2[j];
   }
    else
   {
      nutate (tdb,FN1,v2, v3);
      precession (tdb,v3,T0, vecs);
   }

   return;
}

/********spin */

void spin (double st, double *pos1,

           double *pos2)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Transforms geocentric rectangular coordinates from rotating system
      based on rotational equator and orthogonal reference meridian to 
      non-rotating system based on true equator and equinox of date.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      st (double)
         Local apparent sidereal time at reference meridian, in hours.
      pos1[3] (double)
         Vector in geocentric rectangular rotating system, referred
         to rotational equator and orthogonal reference meridian.

   OUTPUT
   ARGUMENTS:
      pos2[3] (double)
         Vector in geocentric rectangular non-rotating system,
         referred to true equator and equinox of date.

   RETURNED
   VALUE:
      None.

   GLOBALS
   USED:
      DEG2RAD

   FUNCTIONS
   CALLED:
      sin     math.h
      cos     math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'spin'.

------------------------------------------------------------------------
*/
{
   double str,cosst,sinst,xx,yx,xy,yy;

   str = st * 15.0 * DEG2RAD;
   cosst = cos (str);
   sinst = sin (str);

/*
   Sidereal time rotation matrix follows.
*/

   xx =  cosst;
   yx = -sinst;
   xy =  sinst;
   yy =  cosst;

/*
   Perform rotation.
*/

   pos2[0] = xx * pos1[0] + yx * pos1[1];
   pos2[1] = xy * pos1[0] + yy * pos1[1];
   pos2[2] = pos1[2];

   return;
}

/********wobble */

void wobble (double x, double y, double *pos1,

             double *pos2)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Corrects Earth-fixed geocentric rectangular coordinates for polar
      motion.  Transforms a vector from Earth-fixed geographic system to
      rotating system based on rotational equator and orthogonal
      Greenwich meridian through axis of rotation.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      x (double)
         Conventionally-defined X coordinate of rotational pole with
         respect to CIO, in arcseconds.
      y (double)
         Conventionally-defined Y coordinate of rotational pole with
         respect to CIO, in arcseconds.
      pos1[3] (double)
         Vector in geocentric rectangular Earth-fixed system,
         referred to geographic equator and Greenwich meridian.

   OUTPUT
   ARGUMENTS:
      pos2[3] (double)
         Vector in geocentric rectangular rotating system, referred
         to rotational equator and orthogonal Greenwich meridian

   RETURNED
   VALUE:
      None.

   GLOBALS
   USED:
      SECS2RADS

   FUNCTIONS
   CALLED:
      None.

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'wobble'.

------------------------------------------------------------------------
*/
{
   double xpole,ypole,zx,zy,xz,yz;

   xpole = x / SECS2RADS;
   ypole = y / SECS2RADS;
   
/*
   Wobble rotation matrix follows.
*/

   zx = -xpole;
   zy =  ypole;
   xz =  xpole;
   yz = -ypole;

/*
   Perform rotation.
*/

   pos2[0] = pos1[0] + zx * pos1[2];
   pos2[1] = pos1[1] + zy * pos1[2];
   pos2[2] = xz * pos1[0] + yz * pos1[1] + pos1[2];

   return;
}

/********proper_motion */

void proper_motion (double tjd1, double *pos, double *vel,
                    double tjd2,

                    double *pos2)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Applies proper motion, including foreshortening effects, to a
      star's position.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd1 (double)
         TDB Julian date of first epoch.
      pos[3] (double)
         Position vector at first epoch.
      vel[3] (double)
         Velocity vector at first epoch.
      tjd2 (double)
         TDB Julian date of second epoch.

   OUTPUT
   ARGUMENTS:
      pos2[3] (double)
         Position vector at second epoch.

   RETURNED
   VALUE:
      None.

   GLOBALS
   USED:
      None.

   FUNCTIONS
   CALLED:
      None.

   VER./DATE/
   PROGRAMMER:
      V1.0/01-93/TKB (USNO/NRL Optical Interfer.) Translate Fortran.
      V1.1/08-93/WTH (USNO/AA) Updated to C programming standards.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'propmo'.

------------------------------------------------------------------------
*/
{
    short int j;

    for (j = 0; j < 3; j++)
       pos2[j] = pos[j] + (vel[j] * (tjd2 - tjd1));

    return;
}

/********geocentric */

void geocentric (double *pos, double *earthvector,

                 double *pos2, double *lighttime)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Moves the origin of coordinates from the barycenter of the
      solar system to the center of mass of the Earth; i.e. corrects
      for parallax.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      pos1[3] (double)
         Position vector, referred to origin at solar system barycenter,
         components in AU.
      earthvector[3] (double)
         Position vector of center of mass of the Earth, referred to
         origin at solar system barycenter, components in AU.

   OUTPUT
   ARGUMENTS:
      pos2[3] (double)
         Position vector, referred to origin at center of mass of the
         Earth, components in AU.
      *lighttime (double)
         Light time from body to Earth in days.

   RETURNED
   VALUE:
      None.

   GLOBALS
   USED:
      C

   FUNCTIONS
   CALLED:
      pow     math.h
      sqrt    math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/01-93/TKB (USNO/NRL Optical Interfer.) Translate Fortran.
      V1.1/08-93/WTH (USNO/AA) Update to C Standards.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'geocen'.

------------------------------------------------------------------------
*/
{
   short int j;

   double sum_of_squares;

/*
   Translate vector to geocentric coordinates.
*/

   for (j = 0; j < 3; j++)
      pos2[j] = pos[j] - earthvector[j];

/*
   Calculate length of vector in terms of light time.
*/

   sum_of_squares = pow (pos2[0], 2) + pow (pos2[1], 2)
                  + pow (pos2[2], 2);

   *lighttime = sqrt (sum_of_squares) / C;

   return;
}

/********aberration */

short int aberration (double *pos, double *ve, double lighttime,

                      double *pos2)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Corrects position vector for aberration of light.  Algorithm
      includes relativistic terms.

   REFERENCES: 
      Murray, C. A. (1981) Mon. Notices Royal Ast. Society 195, 639-648.
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      pos[3] (double)
         Position vector, referred to origin at center of mass of the
         Earth, components in AU.
      ve[3] (double)
         Velocity vector of center of mass of the Earth, referred to
         origin at solar system barycenter, components in AU/day.
      lighttime (double)
         Light time from body to Earth in days.

   OUTPUT
   ARGUMENTS:
      pos2[3] (double)
         Position vector, referred to origin at center of mass of the
         Earth, corrected for aberration, components in AU

   RETURNED
   VALUE:
      (short int)
         0...Everything OK.

   GLOBALS
   USED:
      C

   FUNCTIONS
   CALLED:
      sqrt      math.h
      pow       math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/01-93/TKB (USNO/NRL Optical Interfer.) Translate Fortran.
      V1.1/08-93/WTH (USNO/AA) Update to C Standards.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'aberat'.
      2. If 'lighttime' = 0 on input, this function will compute it.

------------------------------------------------------------------------
*/
{
   short int j;

   double p1mag, vemag, beta, dot,cosd, gammai, p, q, r;

   if (lighttime == 0.0)
   {
      p1mag = sqrt (pow (pos[0], 2.0) + pow (pos[1], 2.0)
                  + pow (pos[2], 2.0));
      lighttime = p1mag / C;
   }
    else
      p1mag = lighttime * C;

   vemag = sqrt (pow (ve[0], 2.0) + pow (ve[1], 2.0) 
               + pow (ve[2], 2.0));
   beta = vemag / C;
   dot = pos[0] * ve[0] + pos[1] * ve[1] + pos[2] * ve[2];

   cosd = dot / (p1mag * vemag);
   gammai = sqrt (1.0 - pow (beta, 2.0));
   p = beta * cosd;
   q = (1.0 + p / (1.0 + gammai)) * lighttime;
   r = 1.0 + p;

   for (j = 0; j < 3; j++)
      pos2[j] = (gammai * pos[j] + q * ve[j]) / r;

   return 0;
}

/********precession */

short int precession (double tjd1, double *pos, double tjd2,

                      double *pos2)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Precesses equatorial rectangular coordinates from one epoch to
      another.  The coordinates are referred to the mean equator and
      equinox of the two respective epochs.

   REFERENCES:
      Explanatory Supplement to AE and AENA (1961); pp. 30-34.
      Lieske, J., et al. (1977). Astron. & Astrophys. 58, 1-16. 
      Lieske, J. (1979). Astron. & Astrophys. 73, 282-284. 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd1 (double)
         TDB Julian date of first epoch.
      pos[3] (double)
         Position vector, geocentric equatorial rectangular coordinates,
         referred to mean equator and equinox of first epoch.
      tjd2 (double)
         TDB Julian date of second epoch.

   OUTPUT
   ARGUMENTS:
      pos2[3] (double)
         Position vector, geocentric equatorial rectangular coordinates,
         referred to mean equator and equinox of second epoch.

   RETURNED
   VALUE:
      (short int)
         0...Everything OK.

   GLOBALS
   USED:
      T0, SECS2RADS

   FUNCTIONS
   CALLED:
      sin    math.h
      cos    math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/01-93/TKB (USNO/NRL Optical Interfer.) Translate Fortran.
      V1.1/08-93/WTH (USNO/AA) Update to C Standards.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'preces'.

------------------------------------------------------------------------
*/
{

   double xx,yx,zx,xy,yy,zy,xz,yz,zz,t,t1,t02,t2,t3,zeta0,zee,theta;

/*
   't' and 't1' below correspond to Lieske's "big T" and "little t".
*/

   t = (tjd1 - T0) / 36525.0;
   t1 = (tjd2 - tjd1) / 36525.0;
   t02 = t * t;
   t2 = t1 * t1;
   t3 = t2 * t1;

/*
   'zeta0', 'zee', 'theta' below correspond to Lieske's "zeta-sub-a",
   "z-sub-a", and "theta-sub-a".
*/

   zeta0 = (2306.2181 + 1.39656 * t - 0.000139 * t02) * t1
         + (0.30188 - 0.000344 * t) * t2 + 0.017998 * t3;

   zee = (2306.2181 + 1.39656 * t - 0.000139 * t02) * t1
       + (1.09468 + 0.000066 * t) * t2 + 0.018203 * t3;

   theta = (2004.3109 - 0.85330 * t - 0.000217 * t02) * t1
         + (-0.42665 - 0.000217 * t) * t2 - 0.041833 * t3;

   zeta0 /= SECS2RADS;
   zee /= SECS2RADS;
   theta /= SECS2RADS;

/*
   Precession rotation matrix follows.
*/

   xx = cos (zeta0) * cos (theta) * cos (zee) - sin (zeta0) * sin (zee);
   yx = -sin (zeta0) * cos (theta) * cos (zee) - cos (zeta0) *
        sin (zee);
   zx = -sin (theta) * cos (zee);
   xy = cos (zeta0) * cos (theta) * sin (zee) + sin (zeta0) * cos (zee);
   yy = -sin (zeta0) * cos (theta) * sin (zee) + cos (zeta0) *
        cos (zee);
   zy = -sin (theta) * sin (zee);
   xz = cos (zeta0) * sin (theta);
   yz = -sin (zeta0) * sin (theta);
   zz = cos (theta);

/*
   Perform rotation.
*/

   pos2[0] = xx * pos[0] + yx * pos[1] + zx * pos[2];
   pos2[1] = xy * pos[0] + yy * pos[1] + zy * pos[2];
   pos2[2] = xz * pos[0] + yz * pos[1] + zz * pos[2];

   return 0;
}

/********vector2radec */

short int vector2radec (double *pos, 

                        double *ra, double *dec)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Converts an vector in equatorial rectangular coordinates to
      equatorial spherical coordinates.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      pos[3] (double)
         Position vector, equatorial rectangular coordinates.

   OUTPUT
   ARGUMENTS:
      *rightascension (double)
         Right ascension in hours.
      *declination (double)
         Declination in degrees.

   RETURNED
   VALUE:
      (short int)
         0...Everything OK.
         1...All vector components are zero; 'ra' and 'dec' are
             indeterminate.
         2...Both vec[0] and vec[1] are zero, but vec[2] is nonzero;
             'ra' is indeterminate.
   GLOBALS
   USED:
      SECS2RADS

   FUNCTIONS
   CALLED:
      sqrt     math.h
      pow      math.h
      atan2    math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/01-93/TKB (USNO/NRL Optical Interfer.) Translate Fortran.
      V1.1/08-93/WTH (USNO/AA) Update to C Standards.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'angles'.

------------------------------------------------------------------------
*/
{
   double xyproj;

   xyproj = sqrt (pow (pos[0], 2.0) + pow (pos[1], 2.0));
   if ((xyproj == 0.0) && (pos[2] == 0))
   {
      *ra = 0.0;
      *dec = 0.0;
      return 1;
   }
    else if (xyproj == 0.0)
   {
      *ra = 0.0;
      if (pos[2] < 0.0)
         *dec = -90.0;
       else
         *dec = 90.0;
      return 2;
   }
    else
   {
      *ra = atan2 (pos[1], pos[0]) * SECS2RADS / 54000.0;
      *dec = atan2 (pos[2], xyproj) * SECS2RADS / 3600.0;

      if (*ra < 0.0)
         *ra += 24.0;
   }
   return 0;
}

/********angle2vector */

void angle2vector (double ra, double dec, double dist,

                   double *vector)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Converts equatorial spherical coordinates to a vector (equatorial
      rectangular coordinates).

   REFERENCES: 
      None.

   INPUT
   ARGUMENTS:
      ra (double)
         Right ascension (hours).
      dec (double)
         Declination (degrees).

   OUTPUT
   ARGUMENTS:
      vector[3] (double)
         Position vector, equatorial rectangular coordinates (AU).

   RETURNED
   VALUE:
      (short int)
         0...Everything OK.

   GLOBALS
   USED:
      DEG2RAD

   FUNCTIONS
   CALLED:
      cos     math.h
      sin     math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/05-92/TKB (USNO/NRL Optical Interfer.) Translate Fortran.
      V1.1/08-93/WTH (USNO/AA) Update to C Standards.

   NOTES:
      None.

------------------------------------------------------------------------
*/
{

   vector[0] = dist * cos (DEG2RAD * dec) * cos (DEG2RAD * 15.0 * ra);
   vector[1] = dist * cos (DEG2RAD * dec) * sin (DEG2RAD * 15.0 * ra);
   vector[2] = dist * sin (DEG2RAD * dec);

   return;
}

/********starvectors */

void starvectors (fk5_entry *star,

                  double *pos, double *vel)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Converts angular quanities for stars to vectors.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      *star (struct fk5_entry)
         Pointer to catalog entry structure (defined in novas.h).

   OUTPUT
   ARGUMENTS:
      pos[3] (double)
         Position vector, equatorial rectangular coordinates,
         components in AU.
      vel[3] (double)
         Velocity vector, equatorial rectangular coordinates,
         components in AU/Day.

   RETURNED
   VALUE:
      None.

   GLOBALS
   USED:
      SECS2RADS, DEG2RAD, KMAU

   FUNCTIONS
   CALLED:
      sin     math.h
      cos     math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/01-93/TKB (USNO/NRL Optical Interfer.) Translate Fortran.
      V1.1/08-93/WTH (USNO/AA) Updated to C programming standards.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'vectrs'.

------------------------------------------------------------------------
*/
{
   double paralx, dist, r, d, cra, sra, cdc, sdc, pmr, pmd, rvl;

/*
   If parallax is unknown, undetermined, or zero, set it to 1e-7 second
   of arc, corresponding to a distance of 10 megaparsecs.
*/

   paralx = star->parallax;

   if (star->parallax <= 0.0)
      paralx = 1.0e-7;

/*
   Convert right ascension, declination, and parallax to position vector
   in equatorial system with units of AU.
*/

   dist = SECS2RADS / paralx;
   r = (star->ra) * 15.0 * DEG2RAD;
   d = (star->dec) * DEG2RAD;
   cra = cos (r);
   sra = sin (r);
   cdc = cos (d);
   sdc = sin (d);

   pos[0] = dist * cdc * cra;
   pos[1] = dist * cdc * sra;
   pos[2] = dist * sdc;

/*
   Convert proper motion and radial velocity to orthogonal components of
   motion with units of AU/Day.
*/

   pmr = star->promora * 15.0 * cdc / (paralx * 36525.0);
   pmd = star->promodec / (paralx * 36525.0);
   rvl = star->radialvelocity * 86400.0 / KMAU;

/*
   Transform motion vector to equatorial system.
*/

   vel[0] = - pmr * sra - pmd * sdc * cra + rvl * cdc * cra;
   vel[1] =   pmr * cra - pmd * sdc * sra + rvl * cdc * sra;
   vel[2] =   pmd * cdc + rvl * sdc;

   return;
}

/********calcnutation */

short int calcnutation (double tdbtime,

                        double *longnutation, double *obliqnutation)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Provides fast evaluation of the nutation components according to
      the 1980 IAU Theory of Nutation.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210, and references therein.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.
      Miller, B. R. (1989). Proceedings of the ACM-SIGSAM International
         Symposium on Symbolic and Algebraic Computation; pp. 199-206.

   INPUT
   ARGUMENTS:
      tdbtime (double)
         TDB time in Julian centuries since J2000.0

   OUTPUT
   ARGUMENTS:
      *longnutation (double)
         Nutation in longitude in seconds of arc.
      *obliqnutation (double)
         Nutation in obliquity in seconds of arc.

   RETURNED
   VALUE:
      (short int)
         0...Everything OK.

   GLOBALS
   USED:
      None.

   FUNCTIONS
   CALLED:
      sin     math.h
      cos     math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/11-88/BRM (NIST)
      V1.1/08-93/WTH (USNO/AA) Translate Fortran.

   NOTES:
      1. This function is based on computer-generated Fortran code.
      Original Fortran code generated on 11/29/88 16:35:35 at the
      National Institutes of Standards and Technology (NIST), by 
      Bruce R. Miller.
      2. This function is the "C" version of Fortran NOVAS routine
      'nod', member 'vanut1f'.

------------------------------------------------------------------------
*/
{
   double clng[106] = {1.0,   1.0,  -1.0, -1.0,   1.0,  -1.0,  -1.0,
                      -1.0,  -1.0,  -1.0, -1.0,   1.0,  -1.0,   1.0,
                      -1.0,   1.0,   1.0, -1.0,  -1.0,   1.0,   1.0,
                      -1.0,   1.0,  -1.0,  1.0,  -1.0,  -1.0,  -1.0,
                       1.0,  -1.0,  -1.0,  1.0,  -1.0,   1.0,   2.0,
                       2.0,   2.0,   2.0,  2.0,  -2.0,   2.0,   2.0,
                       2.0,   3.0,  -3.0, -3.0,   3.0,  -3.0,   3.0,
                      -3.0,   3.0,   4.0,  4.0,  -4.0,  -4.0,   4.0,
                      -4.0,   5.0,   5.0,  5.0,  -5.0,   6.0,   6.0,
                       6.0,  -6.0,   6.0, -7.0,   7.0,   7.0,  -7.0,
                      -8.0,  10.0,  11.0, 12.0, -13.0, -15.0, -16.0,
                     -16.0,  17.0, -21.0,-22.0,  26.0,  29.0,  29.0,
                     -31.0, -38.0, -46.0, 48.0, -51.0,  58.0,  59.0,
                      63.0,  63.0,-123.0,129.0,-158.0,-217.0,-301.0,
                    -386.0,-517.0, 712.0,1426.0,2062.0,-2274.0,
                  -13187.0,-171996.0},
      clngx[14]={ 0.1,-0.1,0.1,0.1,0.1,0.1,0.2,-0.2,-0.4,0.5,1.2,
                 -1.6,-3.4,-174.2},
       cobl[64]={    1.0,    1.0,    1.0,   -1.0,   -1.0,   -1.0,
                     1.0,    1.0,    1.0,    1.0,    1.0,   -1.0,
                     1.0,   -1.0,    1.0,   -1.0,   -1.0,   -1.0,
                     1.0,   -1.0,    1.0,    1.0,   -1.0,   -2.0,
                    -2.0,   -2.0,    3.0,    3.0,   -3.0,    3.0,
                     3.0,   -3.0,    3.0,    3.0,   -3.0,    3.0,
                     3.0,    5.0,    6.0,    7.0,   -7.0,    7.0,
                    -8.0,    9.0,  -10.0,  -12.0,   13.0,   16.0,
                   -24.0,   26.0,   27.0,   32.0,  -33.0,  -53.0,
                    54.0,  -70.0,  -95.0,  129.0,  200.0,  224.0,
                  -895.0,  977.0, 5736.0,92025.0},
       coblx[8]={ -0.1, -0.1,  0.3,  0.5, -0.5, -0.6, -3.1,  8.9};

   short int i,ii,i1,i2,iop;
   short int nav1[10]={0,0,1,0,2,1,3,0,4,0},
       nav2[10]={ 0, 0, 0, 5, 1, 1, 3, 3, 4, 4},
       nav[183]={ 2, 0, 1, 1, 5, 2, 2, 0, 2, 1, 0, 3, 2, 5, 8, 1,17, 8,
                  1,18, 0, 2, 0, 8, 0, 1, 3, 2, 1, 8, 0,17, 1, 1,15, 1,
                  2,21, 1, 1, 2, 8, 2, 0,29, 1,21, 2, 2, 1,29, 2, 0, 9,
                  2, 5, 4, 2, 0, 4, 0, 1, 9, 2, 1, 4, 0, 2, 9, 2, 2, 4,
                  1,14,44, 2, 0,45, 2, 5,44, 2,50, 0, 1,36, 2, 2, 5,45,
                  1,37, 2, 2, 1,45, 2, 1,44, 2,53, 1, 2, 8, 4, 1,40, 3,
                  2,17, 4, 2, 0,64, 1,39, 8, 2,27, 4, 1,50,18, 1,21,47,
                  2,44, 3, 2,44, 8, 2,45, 8, 1,46, 8, 0,67, 2, 1, 5,74,
                  1, 0,74, 2,50, 8, 1, 5,78, 2,17,53, 2,53, 8, 2, 0,80,
                  2, 0,81, 0, 7,79, 1, 7,81, 2, 1,81, 2,24,44, 1, 1,79,
                  2,27,44},
      llng[106]={ 57, 25, 82, 34, 41, 66, 33, 36, 19, 88, 18,104, 93,
                  84, 47, 28, 83, 86, 69, 75, 89, 30, 58, 73, 46, 77,
                  23, 32, 59, 72, 31, 16, 74, 22, 98, 38, 62, 96, 37,
                  35,  6, 76, 85, 51, 26, 10, 13, 63,105, 52,102, 67,
                  99, 15, 24, 14,  3,100, 65, 11, 55, 68, 20, 87, 64,
                  95, 27, 60, 61, 80, 91, 94, 12, 43, 71, 42, 97, 70,
                   7, 49, 29,  2,  5, 92, 50, 78, 56, 17, 48, 40, 90,
                   8, 39, 54, 81, 21,103, 53, 45,101,  0,  1,  9, 44,
                  79,  4},
      llngx[14]={ 81, 7, 97, 0, 39, 40, 9, 44, 45,103,101, 79, 1, 4},
      lobl[64]={  51, 98, 17, 21,  5,  2, 63,105, 38, 52,102, 62, 96,
                  37, 35, 76, 36, 88, 85,104, 93, 84, 83, 67, 99,  8,
                  68,100, 60, 61, 91, 87, 64, 80, 95, 65, 55, 94, 43,
                  97,  0, 71, 70, 42, 49, 92, 50, 78, 56, 90, 48, 40,
                  39, 54,  1, 81,103, 53, 45,101,  9, 44, 79,  4},
      loblx[8] ={ 53,  1,103,  9, 44,101, 79,  4};

   double a[5],angle,cc,ss1,cs,sc,c[106],s[106],lng,lngx,obl,oblx;

   a[0] = 2.3555483935439407 + tdbtime * (8328.691422883896
                             + tdbtime * (1.517951635553957e-4
                             + 3.1028075591010306e-7 * tdbtime));
   a[1] = 6.240035939326023 + tdbtime * (628.3019560241842
                            + tdbtime * (-2.7973749400020225e-6
                            - 5.817764173314431e-8 * tdbtime));
   a[2] = 1.6279019339719611 + tdbtime * (8433.466158318453 
                             + tdbtime * (-6.427174970469119e-5
                             + 5.332950492204896e-8 * tdbtime));
   a[3] = 5.198469513579922 + tdbtime * (7771.377146170642
                            + tdbtime * (-3.340851076525812e-5
                            + 9.211459941081184e-8 * tdbtime));
   a[4] = 2.1824386243609943 + tdbtime * (-33.75704593375351
                             + tdbtime * (3.614285992671591e-5
                             + 3.878509448876288e-8 * tdbtime));

   i = 0;
   for (ii = 0; ii < 10; ii += 2)
   {
      angle = a[nav1[ii]] * (double) (nav1[1+ii]+1);
      c[i] = cos (angle);
      s[i] = sin (angle);
      i += 1;
   }

   i = 5;
   for (ii = 0; ii < 10; ii += 2)
   {
      i1 = nav2[ii];
      i2 = nav2[1+ii];

      c[i] = c[i1] * c[i2] - s[i1] * s[i2];
      s[i] = s[i1] * c[i2] + c[i1] * s[i2];
      i += 1;
   }

   i = 10;
   for (ii = 0; ii < 183; ii += 3)
   {
      iop = nav[ii];
      i1 = nav[1+ii];
      i2 = nav[2+ii];
      switch (iop)
      {
         case 0:
            c[i] = c[i1] * c[i2] - s[i1] * s[i2];
            s[i] = s[i1] * c[i2] + c[i1] * s[i2];
            i += 1;
            break;
         case 1:
            c[i] = c[i1] * c[i2] + s[i1] * s[i2];
            s[i] = s[i1] * c[i2] - c[i1] * s[i2];
            i += 1;
            break;
         case 2:
            cc = c[i1] * c[i2];
            ss1 = s[i1] * s[i2];
            sc = s[i1] * c[i2];
            cs = c[i1] * s[i2];
            c[i] = cc - ss1;
            s[i] = sc + cs;
            i += 1;
            c[i] = cc + ss1;
            s[i] = sc - cs;
            i += 1;
            break;
      }
      if (iop == 3)
         break;
   }

   lng = 0.0;
   for (i = 0; i < 106; i++)
      lng += clng[i] * s[llng[i]];

   lngx = 0.0;
   for (i = 0; i < 14; i++)
      lngx += clngx[i] * s[llngx[i]];

   obl = 0.0;
   for (i = 0; i < 64; i++)
      obl += cobl[i] * c[lobl[i]];

   oblx = 0.0;
   for (i = 0; i < 8; i++)
      oblx += coblx[i] * c[loblx[i]];

   *longnutation = (lng + tdbtime * lngx) / 10000.0;
   *obliqnutation = (obl + tdbtime * oblx) / 10000.0;

   return 0;
}

/********nutate */

short int nutate (double tjd, short int fn, double *pos, 

                  double *pos2)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Nutates equatorial rectangular coordinates from mean equator and
      equinox of epoch to true equator and equinox of epoch. Inverse
      transformation may be applied by setting flag 'fn'.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tdb (double)
         TDB julian date of epoch.
      fn (short int)
         Flag determining 'direction' of transformation;
            fn  = 0 transformation applied, mean to true.
            fn != 0 inverse transformation applied, true to mean.
      pos[3] (double)
         Position vector, geocentric equatorial rectangular coordinates,
         referred to mean equator and equinox of epoch.

   OUTPUT
   ARGUMENTS:
      pos2[3] (double)
         Position vector, geocentric equatorial rectangular coordinates,
         referred to true equator and equinox of epoch.

   RETURNED
   VALUE:
      (short int)
         0...Everything OK.

   GLOBALS
   USED:
      DEG2RAD, SECS2RADS

   FUNCTIONS
   CALLED:
      earthtilt     novas.c
      cos           math.h
      sin           math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/01-93/TKB (USNO/NRL Optical Interfer.) Translate Fortran.
      V1.1/08-93/WTH (USNO/AA) Update to C Standards.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'nutate'.

------------------------------------------------------------------------
*/
{
   double cobm,sobm,cobt,sobt,cpsi,spsi,xx,yx,zx,xy,yy,zy,xz,yz,zz,oblm,
          oblt,eqeq,psi,eps;

   earthtilt (tjd, &oblm,&oblt,&eqeq,&psi,&eps);

   cobm = cos (oblm * DEG2RAD);
   sobm = sin (oblm * DEG2RAD);
   cobt = cos (oblt * DEG2RAD);
   sobt = sin (oblt * DEG2RAD);
   cpsi = cos (psi / SECS2RADS);
   spsi = sin (psi / SECS2RADS);

/*
   Nutation rotation matrix follows.
*/

   xx = cpsi;
   yx = -spsi * cobm;
   zx = -spsi * sobm;
   xy = spsi * cobt;
   yy = cpsi * cobm * cobt + sobm * sobt;
   zy = cpsi * sobm * cobt - cobm * sobt;
   xz = spsi * sobt;
   yz = cpsi * cobm * sobt - sobm * cobt;
   zz = cpsi * sobm * sobt + cobm * cobt;

   if (!fn)
   {

/*
   Perform rotation.
*/

      pos2[0] = xx * pos[0] + yx * pos[1] + zx * pos[2];
      pos2[1] = xy * pos[0] + yy * pos[1] + zy * pos[2];
      pos2[2] = xz * pos[0] + yz * pos[1] + zz * pos[2];
   }
    else
   {

/*
   Perform inverse rotation.
*/

      pos2[0] = xx * pos[0] + xy * pos[1] + xz * pos[2];
      pos2[1] = yx * pos[0] + yy * pos[1] + yz * pos[2];
      pos2[2] = zx * pos[0] + zy * pos[1] + zz * pos[2];
   }

   return 0;
}

/********convert_tdb2tdt */

void convert_tdb2tdt (double tdb,

                      double *tdtjd, double *secdiff)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Computes the terrestrial dynamical time (TDT) Julian date
      corresponding to a barycentric dynamical time (TDB) Julian date.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tdb (double)
         TDB Julian date.

   OUTPUT
   ARGUMENTS:
      *tdtjd (double)
         TDT Julian date.
      *secdiff (double)
         Difference tdbjd-tdtjd, in seconds.

   RETURNED
   VALUE:
      None.

   GLOBALS
   USED:
      None.

   FUNCTIONS
   CALLED:
      sin   math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/07-92/TKB (USNO/NRL Optical Interfer.) Translate Fortran.
      V1.1/08-93/WTH (USNO/AA) Update to C Standards.

   NOTES:
      1. Expressions used in this version are approximations resulting
      in accuracies of about 20 microseconds.
      2. This function is the "C" version of Fortran NOVAS routine
      'times'.
------------------------------------------------------------------------
*/
{
    double t, m, llj, e;

    t = (tdb - 2433282.5) / 36525.0;
    m = 6.248291 + 628.3019415 * t;
    llj = 5.652593 + 575.3380832 * t;
    e = m + 0.01671 * sin (m);

    *secdiff = 1.658e-3 * sin (e) + 20.73e-6 * sin (llj);
    *tdtjd = tdb - *secdiff / 86400.0;

    return;
}

/********sun_field */

short int sun_field (double *pos, double *earthvector,

                     double *pos2)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Corrects position vector for the deflection of light in the
      gravitational field of the Sun.  This function is valid for
      bodies within the solar system as well as for stars.

   REFERENCES:
      Misner, C., Thorne, K., and Wheeler, J. (1973). Gravitation;
         pp. 184-185. 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      pos[3] (double)
         Position vector, referred to origin at center of mass of the
         Earth, components in AU.
      earthvector[3] (double)
         Position vector of center of mass of the Earth, referred to
         origin at center of mass of the Sun, components in AU.

   OUTPUT
   ARGUMENTS:
      pos2[3] (double)
         Position vector, referred to origin at center of mass of the
         Earth, corrected for gravitational deflection, components
         in AU.

   RETURNED
   VALUE:
      (short int)
         0...Everything OK.

   GLOBALS
   USED:
      C, MAU, GS

   FUNCTIONS
   CALLED:
      fabs    math.h
      sqrt    math.h
      pow     math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/01-93/TKB (USNO/NRL Optical Interfer.) Translate Fortran.
      V1.1/08-93/WTH (USNO/AA) Update to C Standards.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'sunfld', member 'vasun1'.

------------------------------------------------------------------------
*/
{

/*
   c = speed of light in meters/second.
*/

   double c = (C * MAU) / 86400.0;

   double p1mag,pemag,f=0.0,cosd,sind,b,bm,pqmag,zfinl,zinit,xifinl,
          xiinit,delphi,delphp,delp,p1hat[3],pehat[3];

   short int j;

/*
   Compute vector magnitudes and unit vectors.
*/

   p1mag = sqrt (pow (pos[0], 2.0) + pow (pos[1], 2.0)
               + pow (pos[2], 2.0));
   pemag = sqrt (pow (earthvector[0], 2.0) + pow (earthvector[1], 2.0) 
               + pow (earthvector[2], 2.0));

   for (j = 0; j < 3; j++)
   {
      p1hat[j] = pos[j] / p1mag;
      pehat[j] = earthvector[j] / pemag;
   }

/*
   Compute geometrical quantities.

   'cosd' and 'sind' are cosine and sine of d, the angular separation
   of the body from the Sun as viewed from the Earth.
*/

   cosd = - pehat[0] * p1hat[0] - pehat[1] * p1hat[1] 
          - pehat[2] * p1hat[2];

   if (fabs (cosd) > 0.9999999999)
   {
      for (j = 0; j < 3; j++)
         pos2[j] = pos[j];
   }
    else
   {
      sind = sqrt (1.0 - pow (cosd, 2.0));

/*
   'b' is the impact parameter for the ray.
*/

      b = pemag * sind;
      bm = b * MAU;

/*
   'pqmag' is the distance of the body from the sun.
*/

      pqmag = sqrt (pow (p1mag, 2.0) + pow (pemag, 2.0) 
                 - 2.0 * p1mag * pemag * cosd);

/*
   Compute 'delphi', the angle of deflection of the ray.
*/

      zfinl = pemag * cosd;
      zinit = -p1mag + zfinl;
      xifinl = zfinl / b;
      xiinit = zinit / b;

      delphi = 2.0 * GS / (bm * c * c) * (xifinl / 
                sqrt (1.0 + pow (xifinl, 2.0)) - xiinit /
                sqrt (1.0 + pow (xiinit, 2.0)));

/*
   Compute 'delphp', the change in angle as seen at the Earth.
*/

      delphp = delphi / (1.0 + (pemag / pqmag));

/*
   Fix up position vector.
   'pos2' is 'pos' rotated through angle 'delphp' in plane defined by
   'pos' and 'earthvector'.
*/

      f = delphp * p1mag / sind;

      for (j = 0; j < 3; j++)
      {
         delp = f * (cosd * p1hat[j] + pehat[j]);
         pos2[j] = pos[j] + delp;
      }
   }

   return 0;
}

/********terra */

void terra (site_info *locale, double st,

            double *pos, double *vel)
/*
------------------------------------------------------------------------

   PURPOSE:
      Computes the position and velocity vectors of a terrestrial
      observer with respect to the center of the Earth.

   REFERENCES:
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      glon (double)
         Longitude of observer with respect to reference meridian
         (East +) in degrees.
      glat (double)
         Geodetic latitude (North +) of observer in degrees.
      ht (double)
         Height of observer in meters.
      st (double)
         Local apparent sidereal time at reference meridian in hours.

   OUTPUT
   ARGUMENTS:
      pos[3] (double)
         Position vector of observer with respect to center of Earth,
         equatorial rectangular coordinates, referred to true equator
         and equinox of date, components in AU.
      vel[3] (double)
         Velocity vector of observer with respect to center of Earth,
         equatorial rectangular coordinates, referred to true equator
         and equinox of date, components in AU/Day.

   RETURNED
   VALUE:
      None.

   GLOBALS
   USED:
      KMAU, EARTHRAD, DEG2RAD

   FUNCTIONS
   CALLED:
      pow    math.h
      sin    math.h
      cos    math.h
      sqrt   math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/04-93/WTH (USNO/AA):  Translate Fortran.

   NOTES:
      1. If reference meridian is Greenwich and st=0, 'pos' is
      effectively referred to equator and Greenwich.
      2. This function is the "C" version of Fortran NOVAS routine
      'terra'.

------------------------------------------------------------------------
*/
{

/*
   'f' = Earth ellipsoid flattening
*/

   const double f = 0.00335281;

/*
   'omega' = rotational angular velocity of Earth in radians/sec
*/

   const double omega = 7.2921151467e-5;

   short int j;
   double df2,sinphi,cosphi,c,s,ach,ash,stlocl,sinst,cosst;

/*
   Compute parameters relating to geodetic to geocentric conversion.
*/

   df2 = pow ((1.0 - f),2);

   sinphi = sin (locale->latitude * DEG2RAD);
   cosphi = cos (locale->latitude * DEG2RAD);
   c = 1.0 / sqrt (pow (cosphi,2.0) + df2 * pow (sinphi,2));
   s = df2 * c;
   ach = EARTHRAD * c + (locale->height / 1000.0);
   ash = EARTHRAD * s + (locale->height / 1000.0);

/*
   Compute local sidereal time factors at the observer's longitude.
*/

   stlocl = (st * 15.0 + locale->longitude) * DEG2RAD;
   sinst = sin (stlocl);
   cosst = cos (stlocl);

/*
   Compute position vector components in kilometers.
*/

   pos[0] = ach * cosphi * cosst;
   pos[1] = ach * cosphi * sinst;
   pos[2] = ash * sinphi;

/*
   Compute velocity vector components in kilometers/sec.
*/

   vel[0] = -omega * ach * cosphi * sinst;
   vel[1] =  omega * ach * cosphi * cosst;
   vel[2] =  0.0;

/*
   Convert position and velocity components to AU and AU/DAY.
*/

   for (j = 0; j < 3; j++)
   {
      pos[j] /= KMAU;
      vel[j] /= KMAU;
      vel[j] *= 86400.0;
   }

   return;
}

/********earthtilt */

void earthtilt (double tjd, 

                double *mobl, double *tobl, double *eq, double *dpsi,
                double *deps)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Computes quantities related to the orientation of the Earth's
      rotation axis at Julian date 'tjd'.

   REFERENCES: 
      Kaplan, G. H. et. al. (1989). Astron. Journ. Vol. 97, 
         pp. 1197-1210.
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDB Julian date of the desired time

   OUTPUT
   ARGUMENTS:
      *mobl (double)
         Mean obliquity of the ecliptic in degrees at 'tjd'.
      *tobl (double)
         True obliquity of the ecliptic in degrees at 'tjd'.
      *eq (double)
         Equation of the equinoxes in seconds of time at 'tjd'.
      *dpsi (double)
         Nutation in longitude in seconds of arc at 'tjd'.
      *deps (double)
         Nutation in obliquity in seconds of arc at 'tjd'.

   RETURNED
   VALUE:
      None.

   GLOBALS
   USED:
      SECS2RADS

   FUNCTIONS
   CALLED:
      calcnutation     novas.c
      fabs             math.h
      pow              math.h
      cos              math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/08-93/WTH (USNO/AA) Translate Fortran.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'etilt'.

------------------------------------------------------------------------
*/
{
   double t;

   static double tjd_last = 0.0,d_psi,d_eps,mean_obliq,true_obliq,eq_eq;

   if (fabs (tjd - tjd_last) > 1.0e-6)
   {
      t = (tjd - T0) / 36525.0;

/*
   Obtain nutation parameters in seconds of arc.
*/

      calcnutation (t, &d_psi,&d_eps);

/*
   Compute mean obliquity of the ecliptic in seconds of arc.
*/

      mean_obliq = 84381.4480 - 46.8150 * t - 0.00059 * pow (t, 2.0)
                 + 0.001813 * pow (t, 3.0);

/*
   Compute true obliquity of the ecliptic in seconds of arc.
*/

      true_obliq = mean_obliq + d_eps;

/*
   Compute equation of the equinoxes in seconds of time.
*/

      eq_eq = (d_psi / 15.0) * cos (true_obliq / SECS2RADS);

/*
   Convert obliquity values to degrees.
*/

      mean_obliq /= 3600.0;
      true_obliq /= 3600.0;

      tjd_last = tjd;
   }

   *dpsi = d_psi;
   *deps = d_eps;
   *eq = eq_eq;
   *mobl = mean_obliq;
   *tobl = true_obliq;

   return;
}


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create novas.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
   NOVAS-C
   Header file for novas.c

   Naval Observatory Vector Astrometry Subroutines
   C Version 1.0
   June, 1996

   U. S. Naval Observatory
   Astronomical Applications Dept.
   3450 Massachusetts Ave., NW
   Washington, DC  20392-5420
*/

#ifndef _NOVAS_
   #define _NOVAS_

   #ifndef __STDIO__
      #include "stdio.h"
   #endif

   #ifndef __MATH__
      #include "math.h"
   #endif

   #ifndef __STDLIB__
      #include "stdlib.h"
   #endif

   #ifndef __CTYPE__
      #include "ctype.h"
   #endif

   #ifndef _CONSTS_
      #include "novascon.h"
   #endif


/*
   Structures.
*/

/*
   struct fk5_entry: J2000.0 catalog data for a star.

   starname[9]        = name of star
   starnumber         = integer identifier assigned to star
   ra                 = mean right acsension, hours.
   dec                = mean declination, degrees.
   promora            = proper motion in RA, seconds per century.
   promodec           = proper motion in declination, arcsec per
                        century.
   parallax           = parallax, arcsec.
   radialvelocity     = radial velocity, km/sec.
   visualmag          = visual magnitude.
*/

   typedef struct
   {
      char starname[9];
      short int starnumber;
      double ra;
      double dec;
      double promora;
      double promodec;
      double parallax;
      double radialvelocity;
      double visualmag;
   } fk5_entry;

/*
   struct site_info: Observer's location.

   latitude           = geodetic latitude in degrees; north positive.
   longitude          = geodetic longitude in degrees; east positive.
   height             = height of the observer in meters.
*/

   typedef struct
   {
      double latitude;
      double longitude;
      double height;
   } site_info;


/*
   Define constants.
*/

   #define BARYC  0
   #define HELIOC 1

   #ifndef _CONSTS_
      #include "novas_consts.h"
   #endif


/*
   Function prototypes
*/

   short int app_star (double tjd, short int earth, fk5_entry *star, 

                       double *ra, double *dec);

   short int topo_star (double tjd, short int earth, double deltat,
                        fk5_entry *star, site_info *location, 

                        double *ra ,double *dec);

   short int app_planet (double tjd, short int planet, short int earth,

                         double *ra, double *dec, double *dis);

   short int topo_planet (double tjd, short int planet, short int earth,
                          double deltat, site_info *location,

                          double *ra, double *dec, double *dis);

   short int virtual_star (double tjd, short int earth, fk5_entry *star,

                           double *ra,double *dec);

   short int virtual_planet (double tjd, short int planet,
                             short int earth,

                             double *ra, double *dec, double *dis);

   short int local_star (double tjd, short int earth, double deltat,
                         fk5_entry *star, site_info *location,

                         double *ra, double *dec);

   short int local_planet (double tjd, short int planet,
                           short int earth, double deltat,
                           site_info *location,

                           double *ra, double *dec, double *dis);

   short int astro_star (double tjd, short int earth, fk5_entry *star,

                         double *ra, double *dec);

   short int astro_planet (double tjd, short int planet,
                           short int earth,

                        double *ra, double *dec, double *dis);

   short int mean_star (double tjd, short int earth, double ra,
                        double dec,

                        double *mra, double *mdec);

   short int get_earth (double tjd, short int earth,

                        double *tdb, double *bary_earthp,
                        double *bary_earthv, double *helio_earthp,
                        double *helio_earthv);

   void sidereal_time (double julianhi, double julianlo, double ee,

                       double *gst);

   void proper_motion (double tjd1, double *pos1, double *vel1,
                       double tjd2,

                       double *pos2);

   void geocentric (double *pos, double *earthvector,

                    double *pos2, double *lighttime);

   short int aberration (double *pos, double *vel, double lighttime,

                         double *pos2);

   short int precession (double tjd1, double *pos, double tjd2,

                         double *pos2);

   short int vector2radec (double *pos,

                           double *ra, double *dec);

   void pnsw (double tjd, double gast, double x, double y, double *vece,

              double *vecs);

   void angle2vector (double ra, double dec, double dist,

                      double *vector);

   void starvectors (fk5_entry *star,

                     double *pos, double *vel);

   short int calcnutation (double tdbtime,

                           double *longnutation, double *obliqnutation);

   short int nutate (double tjd, short int fn1, double *pos,

                     double *pos2);

   void convert_tdb2tdt (double tdb,

                         double *tdtjd, double *secdiff);

   short int sun_field (double *pos, double *earthvector,

                        double *pos2);

   void earthtilt (double tjd,

                   double *mobl, double *tobl, double *eqeq,
                   double *psi, double *eps);

   void terra (site_info *locale, double st,

               double *pos, double *vel);

   void spin (double st, double *pos1,

              double *pos2);

   void wobble (double x, double y, double *pos1,

               double *pos2);

   short int solarsystem (double tjd, short int body, short int origin, 

                          double *pos, double *vel);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create novascon.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
   NOVAS-C
   Header file for novascon.c

   Naval Observatory Vector Astrometry Subroutines
   C Version 1.0
   June, 1996

   U. S. Naval Observatory
   Astronomical Applications Dept.
   3450 Massachusetts Ave., NW
   Washington, DC  20392-5420
*/

#ifndef _CONSTS_
   #define _CONSTS_

   extern const short int FN1;
   extern const short int FN0;

/*
   TDB Julian date of epoch J2000.0.
*/

   extern const double T0;

/*
   Astronomical Unit in kilometers.
*/

   extern const double KMAU;

/*
   Astronomical Unit in meters.
*/

   extern const double MAU;

/*
   Speed of light in AU/Day.
*/

   extern const double C;

/*
   Heliocentric gravitational constant.
*/

   extern const double GS;

/*
   Radius of Earth in kilometers.
*/

   extern const double EARTHRAD;

/*
   Value of pi in radians.
*/

   extern const double PI;
   extern const double TWOPI;

/*
   Angle conversion constants.
*/

   extern const double SECS2RADS;
   extern const double DEG2RAD;
   extern const double RAD2DEG;

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create novascon.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
   NOVAS-C
   Constants file

   Naval Observatory Vector Astrometry Subroutines
   C Version 1.0
   June, 1996

   U. S. Naval Observatory
   Astronomical Applications Dept.
   3450 Massachusetts Ave., NW
   Washington, DC  20392-5420
*/

#ifndef _CONSTS_
   #include "novascon.h"
#endif

const short int FN1 =       1;
const short int FN0 =       0;

/*
   TDB Julian date of epoch J2000.0.
*/

const double T0 =       2451545.0;

/*
   Astronomical Unit in kilometers.
*/

const double KMAU =     1.49597870e+8;

/*
   Astronomical Unit in meters.
*/

const double MAU =      1.49597870e+11;

/*
   Speed of light in AU/Day.
*/

const double C =        173.1446333;

/*
   Heliocentric gravitational constant.
*/

const double GS =       1.32712438e+20;

/*
   Radius of Earth in kilometers.
*/

const double EARTHRAD = 6378.140;

/*
   Value of pi in radians.
*/

const double PI =       3.14159265358979323846;
const double TWOPI =    6.28318530717958647692;

/*
   Angle conversion constants.
*/

const double SECS2RADS= 206264.806247096355;
const double DEG2RAD =  0.017453292519943296;
const double RAD2DEG =  57.295779513082321;

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create solsys3.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
   NOVAS-C
   Solar System function; version 3.

   Naval Observatory Vector Astrometry Subroutines
   C Version 1.0
   June, 1996

   U. S. Naval Observatory
   Astronomical Applications Dept.
   3450 Massachusetts Ave., NW
   Washington, DC  20392-5420
*/

#ifndef _NOVAS_
   #include "novas.h"
#endif
 
#include "math.h"

/*
   Additional function prototype.
*/

void novas_sun (double jd,

          double *ra, double *dec, double *dis);


/********solarsystem */

short int solarsystem (double tjd, short int body, short int origin, 

                       double *pos, double *vel)
/*
------------------------------------------------------------------------

   PURPOSE:    
      Provides the position and velocity of the Earth at epoch 'tjd'
      by evaluating a closed-form theory without reference to an 
      external file.  This function can also provide the position
      and velocity of the Sun.

   REFERENCES: 
      Kaplan, G. H. "NOVAS: Naval Observatory Vector Astrometry
         Subroutines"; USNO internal document dated 20 Oct 1988;
         revised 15 Mar 1990.

   INPUT
   ARGUMENTS:
      tjd (double)
         TDB Julian date.
      body (short int)
         Body identification number.
         Set 'body' = 0 or 'body' = 1 or 'body' = 10 for the Sun.
         Set 'body' = 2 or 'body' = 3 for the Earth.
      origin (short int)
         Origin code; solar system barycenter   = 0,
                      center of mass of the Sun = 1.

   OUTPUT
   ARGUMENTS:
      pos[3] (double)
         Position vector of 'body' at 'tjd'; equatorial rectangular
         coordinates in AU referred to the mean equator and equinox
         of J2000.0.
      vel[3] (double)
         Velocity vector of 'body' at 'tjd'; equatorial rectangular
         system referred to the mean equator and equinox of J2000.0,
         in AU/Day.

   RETURNED
   VALUE:
      (short int)
         0...Everything OK.
         1...Input Julian date ('tjd') out of range.
         2...Invalid value of 'body'.

   GLOBALS
   USED:
      T0, TWOPI.

   FUNCTIONS
   CALLED:
      novas_sun              solsys3.c
      angle2vector     novas.c
      precession       novas.c
      sin              math.h
      cos              math.h
      fabs             math.h
      fmod             math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/05-96/JAB (USNO/AA) Convert to C; substitute new theory of
                               Sun.

   NOTES:
      1. This function is the "C" version of Fortran NOVAS routine
      'solsys' version 3; member 'vasol3'.

------------------------------------------------------------------------
*/
{
   short int ierr = 0;
   short int i;

/*
   The arrays below contain data on the four largest planets.  This 
   data is used for barycenter computations.
*/

   const double pm[4] = {1047.355, 3498.5, 22869.0, 19314.0};
   const double pa[4] = {5.202803, 9.538843, 19.181951, 30.057779};
   const double pl[4] = {0.5999, 0.8728, 5.4714, 5.3269};
   const double pn[4] = {1.450216e-3, 5.839824e-4, 2.047627e-4,
                         1.043900e-4};

/*
   'obl' is the obliquity of ecliptic at epoch J2000.0 in degrees.
*/

   const double obl = 23.43929111;

   static double tlast = 0.0;
   static double sine, cose, tmass, pbary[3], vbary[3];

   double oblr, qjd, ras, decs, diss, pos1[3], p[3][3], dlon, sinl,
      cosl, x, y, z, xdot, ydot, zdot, f;

/*
   Initialize constants.
*/

   if (tlast == 0.0)
   {
      oblr = obl * TWOPI / 360.0;
      sine = sin (oblr);
      cose = cos (oblr);
      tmass = 1.0;
      for (i = 0; i < 4; i++)
         tmass += 1.0 / pm[i];
      tlast = 1.0;
   }

/*
   Check if input Julian date is within range.
*/

   if ((tjd < 2340000.5) || (tjd > 2560000.5))
      return (ierr = 1);

/*
   Form helicentric coordinates of the Sun or Earth, depending on
   'body'.
*/

   if ((body == 0) || (body == 1) || (body == 10))
      for (i = 0; i < 3; i++)
         pos[i] = vel[i] = 0.0;

    else if ((body == 2) || (body == 3))
    {
      for (i = 0; i < 3; i++)
      {
         qjd = tjd + (double) (i - 1) * 0.1;
         novas_sun (qjd, &ras,&decs,&diss);
         angle2vector (ras,decs,diss, pos1);
         precession (qjd,pos1,T0, pos);
         p[i][0] = -pos[0];
         p[i][1] = -pos[1];
         p[i][2] = -pos[2];
      }
      for (i = 0; i < 3; i++)
      {
         pos[i] = p[1][i];
         vel[i] = (p[2][i] - p[0][i]) / 0.2;
      }
    }

    else
      return (ierr = 2);
           
/*
   If 'origin' = 0, move origin to solar system barycenter.

   Solar system barycenter coordinates are computed from rough
   approximations of the coordinates of the four largest planets.
*/

   if (origin == 0)
   {
      if (fabs (tjd - tlast) >= 1.0e-06)
      {
         for (i = 0; i < 3; i++)
            pbary[i] = vbary[i] = 0.0;

/*
   The following loop cycles once for each of the four planets.

   'sinl' and 'cosl' are the sine and cosine of the planet's mean
   longitude.
*/

         for (i = 0; i < 4; i++)
         {
            dlon = pl[i] + pn[i] * (tjd - T0);
            dlon = fmod (dlon, TWOPI);
            sinl = sin (dlon);
            cosl = cos (dlon);

            x =  pa[i] * cosl;
            y =  pa[i] * sinl * cose;
            z =  pa[i] * sinl * sine;
            xdot = -pa[i] * pn[i] * sinl;
            ydot =  pa[i] * pn[i] * cosl * cose;
            zdot =  pa[i] * pn[i] * cosl * sine;

            f = 1.0 / (pm[i] * tmass);

            pbary[0] += x * f;
            pbary[1] += y * f;
            pbary[2] += z * f;
            vbary[0] += xdot * f;
            vbary[1] += ydot * f;
            vbary[2] += zdot * f;
         }

         tlast = tjd;
      }

      for (i = 0; i < 3; i++)
      {
         pos[i] -= pbary[i];
         vel[i] -= vbary[i];
      }
   }

   return (ierr);
}

/********novas_sun */

void novas_sun (double jd,

          double *ra, double *dec, double *dis)
/*
------------------------------------------------------------------------

   PURPOSE:
      To compute equatorial spherical coordinates of Sun referred to
      the mean equator and equinox of date.

   REFERENCES:
      Bretagnon, P. and Simon, J.L. (1986).  Planetary Programs and
         Tables from -4000 to + 2800. (Richmond, VA: Willmann-Bell).

   INPUT
   ARGUMENTS:
      jd (double)
         Julian date on TDT or ET time scale.

   OUTPUT
   ARGUMENTS:
      ra (double)
         Right ascension referred to mean equator and equinox of date
         (hours).
      dec (double)
         Declination referred to mean equator and equinox of date 
         (degrees).
      dis (double)
         Geocentric distance (AU).

   RETURNED
   VALUE:
      None.

   GLOBALS
   USED:
      T0
      TWOPI
      RAD2DEG

   FUNCTIONS
   CALLED:
      sin           math.h
      cos           math.h
      asin          math.h
      atan2         math.h

   VER./DATE/
   PROGRAMMER:
      V1.0/08-94/JAB (USNO/AA)
      V1.1/05-96/JAB (USNO/AA): Compute mean coordinates instead of
                                apparent.

   NOTES:
      1. Quoted accuracy is 2.0 + 0.03 * T^2 arcsec, where T is
      measured in units of 1000 years from J2000.0.  See reference.

------------------------------------------------------------------------
*/
{
   short int i;

   double sum_lon = 0.0;
   double sum_r = 0.0;
   const double factor = 1.0e-07;
   double u, arg, lon, lat, t, t2, emean, sin_lon;

   struct sun_con
   {
   double l;
   double r;
   double alpha;
   double nu;
   };

   static const struct sun_con con[50] =
      {{403406.0,      0.0, 4.721964,     1.621043},
       {195207.0, -97597.0, 5.937458, 62830.348067}, 
       {119433.0, -59715.0, 1.115589, 62830.821524}, 
       {112392.0, -56188.0, 5.781616, 62829.634302}, 
       {  3891.0,  -1556.0, 5.5474  , 125660.5691 }, 
       {  2819.0,  -1126.0, 1.5120  , 125660.9845 }, 
       {  1721.0,   -861.0, 4.1897  ,  62832.4766 }, 
       {     0.0,    941.0, 1.163   ,      0.813  }, 
       {   660.0,   -264.0, 5.415   , 125659.310  }, 
       {   350.0,   -163.0, 4.315   ,  57533.850  }, 
       {   334.0,      0.0, 4.553   ,    -33.931  }, 
       {   314.0,    309.0, 5.198   , 777137.715  }, 
       {   268.0,   -158.0, 5.989   ,  78604.191  }, 
       {   242.0,      0.0, 2.911   ,      5.412  }, 
       {   234.0,    -54.0, 1.423   ,  39302.098  }, 
       {   158.0,      0.0, 0.061   ,    -34.861  }, 
       {   132.0,    -93.0, 2.317   , 115067.698  }, 
       {   129.0,    -20.0, 3.193   ,  15774.337  }, 
       {   114.0,      0.0, 2.828   ,   5296.670  }, 
       {    99.0,    -47.0, 0.52    ,  58849.27   }, 
       {    93.0,      0.0, 4.65    ,   5296.11   }, 
       {    86.0,      0.0, 4.35    ,  -3980.70   }, 
       {    78.0,    -33.0, 2.75    ,  52237.69   }, 
       {    72.0,    -32.0, 4.50    ,  55076.47   }, 
       {    68.0,      0.0, 3.23    ,    261.08   }, 
       {    64.0,    -10.0, 1.22    ,  15773.85   }, 
       {    46.0,    -16.0, 0.14    ,  188491.03  }, 
       {    38.0,      0.0, 3.44    ,   -7756.55  }, 
       {    37.0,      0.0, 4.37    ,     264.89  }, 
       {    32.0,    -24.0, 1.14    ,  117906.27  }, 
       {    29.0,    -13.0, 2.84    ,   55075.75  }, 
       {    28.0,      0.0, 5.96    ,   -7961.39  }, 
       {    27.0,     -9.0, 5.09    ,  188489.81  }, 
       {    27.0,      0.0, 1.72    ,    2132.19  }, 
       {    25.0,    -17.0, 2.56    ,  109771.03  }, 
       {    24.0,    -11.0, 1.92    ,   54868.56  }, 
       {    21.0,      0.0, 0.09    ,   25443.93  }, 
       {    21.0,     31.0, 5.98    ,  -55731.43  }, 
       {    20.0,    -10.0, 4.03    ,   60697.74  }, 
       {    18.0,      0.0, 4.27    ,    2132.79  }, 
       {    17.0,    -12.0, 0.79    ,  109771.63  }, 
       {    14.0,      0.0, 4.24    ,   -7752.82  }, 
       {    13.0,     -5.0, 2.01    ,  188491.91  }, 
       {    13.0,      0.0, 2.65    ,     207.81  }, 
       {    13.0,      0.0, 4.98    ,   29424.63  }, 
       {    12.0,      0.0, 0.93    ,      -7.99  }, 
       {    10.0,      0.0, 2.21    ,   46941.14  }, 
       {    10.0,      0.0, 3.59    ,     -68.29  }, 
       {    10.0,      0.0, 1.50    ,   21463.25  }, 
       {    10.0,     -9.0, 2.55    ,  157208.40  }};

/*
   Define the time unit 'u', measured in units of 10000 Julian years
   from J2000.0.
*/

   u = (jd - T0) / 3652500.0;
   
/*
   Compute longitude and distance terms from the series.
*/

   for (i = 0; i < 50; i++)
   {
      arg = con[i].alpha + con[i].nu * u;
      sum_lon += con[i].l * sin (arg);
      sum_r += con[i].r * cos (arg);
   }

/*
   Compute longitude, latitude, and distance referred to mean equinox
   and ecliptic of date.
*/

   lon = 4.9353929 + 62833.1961680 * u + factor * sum_lon;

   lon = fmod (lon, TWOPI);
   if (lon < 0.0)
      lon += TWOPI;

   lat = 0.0;

   *dis = 1.0001026 + factor * sum_r;

/*
   Compute mean obliquity of the ecliptic.
*/

   t = u * 100.0;
   t2 = t * t;
   emean = (0.001813 * t2 * t - 0.00059 * t2 - 46.8150 * t +
      84381.448) / SECS2RADS;

/*
   Compute equatorial spherical coordinates referred to the mean equator 
   and equinox of date.
*/

   sin_lon = sin (lon);
   *ra = atan2 ((cos (emean) * sin_lon), cos (lon)) * RAD2DEG;
   *ra = fmod (*ra, 360.0);
   if (*ra < 0.0)
      *ra += 360.0;
   *ra = *ra / 15.0;

   *dec = asin (sin (emean) * sin_lon) * RAD2DEG;
   
   return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create obs_list.imake
#define  PROGRAM   obs_list

#define MODULE_LIST obs_list.c novascon.c solsys3.c
#define INCLUDE_LIST novas.h novascon.h

#define MAIN_LANG_C
#define R2LIB 

#define USES_ANSI_C

#define LIB_TAE_NOSHR
#define LIB_RTL_NOSHR
/*#define LIB_P2SUB*/
/*#define DEBUG*/
$ Return
$!#############################################################################
$PDF_File:
$ create obs_list.pdf
process help=*
PARM INP TYPE=STRING COUNT=1
PARM OUT TYPE=STRING COUNT=2
PARM STATUS TYPE=(STRING,100) COUNT=(0:1) DEFAULT=""
PARM TACQUIRE TYPE=INTEGER COUNT=(0:1) DEFAULT=7
PARM LOCDATE TYPE=INTEGER COUNT=(0,6) DEFAULT=--
PARM UTDATE TYPE=INTEGER COUNT=(0,6) DEFAULT=--
PARM LST TYPE=REAL COUNT=(0:1) DEFAULT=--
PARM ZENITH TYPE=REAL COUNT=(0:1) DEFAULT=35.0
PARM DELAY TYPE=REAL COUNT=(0:1) DEFAULT=38.3
PARM LATITUDE TYPE=REAL COUNT=(0:1) DEFAULT=33.356
PARM LONGITUDE TYPE=REAL COUNT=(0:1) DEFAULT=116.86
PARM BASENAME TYPE=KEYWORD COUNT=(0:1) VALID=(ns,nw,none) DEFAULT=none
PARM BASELINE TYPE=REAL COUNT=(0,4) DEFAULT=--
PARM SUNZENITH TYPE=REAL COUNT=(0:1) DEFAULT=100.
END-PROC

.TITLE
VICAR program OBS_LIST ( observing schedule list generator).
This program accepts a list of objects as generated by program obs_cat. 
It produces a time ordered list for acceptance by the PTI sequencer.

.HELP
PURPOSE:
To create an ascii observing list for input to the Palomar Test Bed 
Interferometer sequencer.

USAGE:
First run obs_cat a number of times to obtain a list of potential objects.
Then concatenate the list together omitting the time stamp header from all but
the first record.
Then run obs_list to order them in an observing list. 

You may specify the time to begin observations four ways:

1. If you default the date & time it will create a sequence beginning "now" and 
   extending until sunrise.

2. You can  specify the local date and time via parameter LOCDATE.

3. You can  specify the UT (GMT) date and time via parameter UTDATE.

4. You can just specify the sidereal time without a date via parameter LST.
   If you start it at some local sidereal time it has no date so it will 
   create a timeline of about 24 hours in which the sun is always below 
   the horizon.

EXECUTION:
obs_cat out=pti_catalogue.ascii name="Betelgeuse" password="    "
obs_list inp=pti_catalogue.ascii out=(pti_list.ascii,pti_timeline.ascii)

THE ALGORITHM:
Obs_list creates a timeline consisting of discreet bins of about 7 minutes each.
The length of time for each bin corresponds to the shortest observing time
for any target input including it's calibrators. The bins extend from the
start time (now or the specified time from the LOCDATE keyword or UTDATE 
keyword) until sunrise. 
Each timeline bin contains the local sidereal time, the sun zenith angle and which
target can be seen from the interferometer for each time bin. This computation
includes a solar ephemeris and a baseline limitation. Once this is known
the program loops through the targets in order of priority to
find sequences of empty time bins into which each target and it's calibrators
(together) can be observed. 

There are two strategies for assigning targets to time bins.
Strategy 1:
Targets are assigned to time bins such that the hour angle of the observation 
is minimized. This places targets towards the center of their observing
windows. This strategy is used if the requested spacing between observations is
greater than zero. 
Strategy 2:
Targets are assigned to time bins such that the separation between 
observations is maximized. This places targets at the very edges of their
observing windows. This strategy is used if the requested spacing between 
observations is zero.
You may mix the two strategies.

Many passes are made through the targets until no further matches can be made,
ie: all the bins are occupied. A last pass is made setting the "repeat"
interval to 1 in order to squeeze single observations into any remaining 
short sequences of empty bins.

OUTPUT FILES:
There are four output files. Two of them are specified on the vicar command
line and the third is created locally under the name sky_locus.dat and the 
fourth locally under the name status_file.txt. The STATUS keyword can
redirect these files to another directory.

The first output file is input to the sequencer.
Record 1 contains times when the program was run.
Record 2 contains the ut time of the first observation.
It consists of time stamped repeats of targets with their calibrators.
The time stamps are:
first     local sidereal time hh:mm:ss
second    coordinated universal time hh:mm:ss
third     Local time (hh:mm:ss)

The second output file lists the timeline as discreet bins with the columns:
1. LST (local sidereal time hh:mm:ss).
2. UTC (coordinated universal time hh:mm:ss).
3. Local time (hh:mm:ss).
4. Sunzen (solar zenith angle in degrees).
5. The target assigned to this time bin. A blank means no assignment possible.
   + means the start of an observation.
   - means the continuation of an observation.
6-n. The names (if any) of all the targets which are visible.

The third output file contains three columns which define the region within
which the interferometer can see on the sky. Column 1 is the declination
in degrees and columns 2 and 3 contain the left and right most limits of
the field of view in hour angle (hours) for this declination.
This file is created locally or at the directory specified by the STATUS keyword
Note: This file depends upon the selected baseline. If you override the
baselines in the input file using the BASENAME keyword then this baseline
will be used throughout. If you don't override it then you'll get whatever
baseline was last used in the program if there are different baselines 
specified for different objects in the input file.

The fourth output file contains the status of the job.

Example of output file 1.

# UTC= "Fri May  8 16:17:11 1998"  Local= "Fri May  8 09:17:11 1998" GMT=1998128161711   Obs_list_version=1
# UT_start=1998002014729
# LST=10:35:11 UTC=03:25:10 Local=20:25:10
# Target HIP57632 known as Denebola is a  PulsV*delSct
# owner=lorre mode=ssm priority=1 repeats=2 spacing=0 time=50 baseline=ns
#  HD          RA(2000.0) dec(2000.0)  pmRA    pmDEC   V    K   SpTyp  Separ Paral  Angle O C Flag  Simbad
 HIP57632     11 49 03.58 +14 34 19.4  -0.499  -0.114  2.1  1.9 A3Vvar  0.00 90.16  1.674 p 01 - -  PulsV*delSct
 HIP57606     11 48 38.71 +14 17 03.2  -0.103  +0.005  5.9  5.2 F0V     0.30 13.48  0.169 c 02 C A  **
 HIP57821     11 51 24.72 +11 48 18.6  -0.221  +0.002  7.1  5.9 F6V     2.83 18.59  0.208 c 02 C B  PM*
# LST=10:58:41 UTC=03:48:40 Local=20:48:40
# Target HIP57632 known as Denebola is a  PulsV*delSct
# owner=lorre mode=ssm priority=1 repeats=2 spacing=0 time=50 baseline=ns
#  HD          RA(2000.0) dec(2000.0)  pmRA    pmDEC   V    K   SpTyp  Separ Paral  Angle O C Flag  Simbad
 HIP57632     11 49 03.58 +14 34 19.4  -0.499  -0.114  2.1  1.9 A3Vvar  0.00 90.16  1.674 p 01 - -  PulsV*delSct
 HIP57606     11 48 38.71 +14 17 03.2  -0.103  +0.005  5.9  5.2 F0V     0.30 13.48  0.169 c 02 C A  **
 HIP57821     11 51 24.72 +11 48 18.6  -0.221  +0.002  7.1  5.9 F6V     2.83 18.59  0.208 c 02 C B  PM*
# LST=11:22:11 UTC=04:12:10 Local=21:12:10
# Target HIP57632 known as Denebola is a  PulsV*delSct
# owner=lorre mode=ssm priority=1 repeats=2 spacing=0 time=50 baseline=ns
#  HD          RA(2000.0) dec(2000.0)  pmRA    pmDEC   V    K   SpTyp  Separ Paral  Angle O C Flag  Simbad
 HIP57632     11 49 03.58 +14 34 19.4  -0.499  -0.114  2.1  1.9 A3Vvar  0.00 90.16  1.674 p 01 - -  PulsV*delSct
 HIP57606     11 48 38.71 +14 17 03.2  -0.103  +0.005  5.9  5.2 F0V     0.30 13.48  0.169 c 02 C A  **
 HIP57821     11 51 24.72 +11 48 18.6  -0.221  +0.002  7.1  5.9 F6V     2.83 18.59  0.208 c 02 C B  PM*

Example of a fragment of output file 2.

  LST      UTC      Local  Sunzen   target visible_targets
15:09:21 07:59:20 00:59:20 130 -Alphecca Izar Alphecca 
15:17:11 08:07:10 01:07:10 130 +Izar Izar Alphecca 
15:25:01 08:14:60 01:14:60 129 -Izar Izar Alphecca 
15:32:51 08:22:50 01:22:50 129 +Izar Izar Alphecca 
15:40:41 08:30:40 01:30:40 129 -Izar Izar Alphecca 
15:48:31 08:38:30 01:38:30 128 +Alphecca Izar Alphecca 
15:56:21 08:46:20 01:46:20 128 -Alphecca Izar Alphecca 
16:04:11 08:54:10 01:54:10 127 -Alphecca Izar Alphecca 
16:12:01 09:01:60 02:01:60 127           Izar Alphecca 
16:19:51 09:09:50 02:09:50 126 +Izar Izar Alphecca 
16:27:41 09:17:40 02:17:40 125 -Izar Izar Alphecca 
16:35:31 09:25:30 02:25:30 125           Izar Alphecca 


Example of a fragment of output file 3 "sky_locus.dat":

Dec 18.000000 HA_limits -2.333333 2.400000
Dec 17.000000 HA_limits -2.266667 2.333333
Dec 16.000000 HA_limits -2.200000 2.266667
Dec 15.000000 HA_limits -2.133333 2.200000
Dec 14.000000 HA_limits -2.133333 2.200000
Dec 13.000000 HA_limits -2.066667 2.133333

Examples of output file 4 "status_file.txt"

success

or

failure
Error opening output catalogue

HISTORY:
3-1-98  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1

.VARI IN
Input catalogue

.VARI OUT
1. Output observing list
2. Output timeline

.VARI STATUS
Directory to contain
status file.
Defaults to local.

.VARI TACQUIRE
Time per acquisition.
(minutes).

.VARI LOCDATE
y,m,d,h,m,s

.VARI UTDATE
y,m,d,h,m,s

.VARI LST
Local sidereal time.

.VARI ZENITH
Zenith angle limit.

.VARI DELAY
Delay line limit. 

.VARI LATITUDE
Observatory latitude
degrees.

.VARI LONGITUDE
Observatory west
longitude in degrees.

.VARI BASENAME
Baseline name
ns or nw.

.VARI BASELINE
Interferometer baseline
and delay constant.

.VARI SUNZENITH
Sun zenith angle to
begin & end observing.
Degrees.

.LEVEL2

.VARI IN
Input catalogue

.VARI OUT
1. Output observing list
2. Output timeline

.VARI STATUS
Directory to contain the status file.
Defaults to the local directory.
The status file is called "status_file.txt"
If the program was successful it will contain the string "success".
If the program began but failed to run it will contain the string "failure" on
record 1 followed by optional records containing ascii messages describing
the nature of the error.

.VARI TACQUIRE
Time per acquisition (minutes).
Defaults to 7 minutes.

.VARI LOCDATE
The local time and date to begin the timeline.
6 integers in the order:
year,month,day,hour,minute,second.
If LOCDATE and UTDATE and LST are omitted the program computes 
the time for "now".

.VARI UTDATE
The coordinated universal time and date to begin the timeline.
6 integers in the order:
year,month,day,hour,minute,second.
If LOCDATE and UTDATE and LST are omitted the program computes 
the time for "now".

.VARI LST
Local sidereal time for start of night run.
If LOCDATE and UTDATE and LST are omitted the program computes 
the time for "now".

.VARI ZENITH 
Zenith angle limit. Objects with zenith angles greater than this 
cannot be observed, (degrees).
Defaults to 35 degrees.
 
.VARI DELAY
Delay line limit. Objects causing a delay line greater than this cannot
be observed. (meters).
Defaults to 33.8 meters.

.VARI LATITUDE
Observatory latitude in degrees.
Defaults to 33.356 , Palomar
 
.VARI LONGITUDE
Observatory west longitude in degrees.
Defaults to 116.86 , Palomar
 
.VARI BASENAME
The name of the baseline.
Currently acceptable names are: ns and nw.
Overrides all baseline names in the input file.
Defaults to the baseline names in the input file which may be different for
each object..

.VARI BASELINE
Used to create a new baseline not currently supported by BASENAME.
Overrides all baseline names in the input file.
Interferometer baseline x,y,z coordinates vector components followd by the
delay constant. All in meters. 4 numbers. The xyz coordinate system has 
+x pointing up to the zenith,
+y pointing east,
+z pointing north tangent to the ground.
All units in meters.
For example to mimic the pti ns baseline you would specify:
baseline=(3.3,-37.16,-103.13,-12.906)

.VARI SUNZENITH
Solar zenith angle to begin and end observing.
Degrees.
Defaults to 100. (ten degrees below the horizon).
$ Return
$!#############################################################################
$Test_File:
$ create tstobs_list.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
obs_cat out=cat1 name="Alpheratz" color="FGKM" +
 owner="lorre" reps=2 spacing=10 time=50
obs_cat out=cat2 name="Hamal" color="FGKM" +
 owner="lorre" reps=2 spacing=10 time=50
obs_cat out=cat3 name="Vega" color="FGKM" +
 owner="lorre" reps=2 spacing=10 time=50
obs_cat out=cat4 name="Aldebaran" color="FGKM" +
 owner="lorre" reps=2 spacing=10 time=50
obs_cat out=cat5 name="Pollux" color="FGKM" +
 owner="lorre" reps=2 spacing=10 time=50
obs_cat out=cat6 name="Denebola" color="FGKM" +
 owner="lorre" reps=2 spacing=10 time=50
obs_cat out=cat7 name="Izar" color="FGKM" +
 owner="lorre" reps=2 spacing=10 time=50
obs_cat out=cat8 name="Alphecca" color="FGKM" +
 owner="lorre" reps=2 spacing=10 time=50
obs_cat out=cat9 name="Deneb" color="FGKM" +
 owner="lorre" reps=2 spacing=10 time=50
ush cat cat1 cat2 cat3 cat4 cat5 cat6 cat7 cat8 cat9 > catalogue.ascii 
! testers.. Edit file catalogue.ascii to:
! 1. Remove all records beginning with the word "UTC=" except the first record.
! 2. Remove, for each target star, all calibrator records over 2 or 3 in number.
obs_list inp=catalogue.ascii out=(list.ascii,timeline.ascii)
!
end-proc
$ Return
$!#############################################################################
