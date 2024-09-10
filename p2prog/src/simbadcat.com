$!****************************************************************************
$!
$! Build proc for MIPL module simbadcat
$! VPACK Version 1.9, Monday, December 22, 1997, 12:55:07
$!
$! Execute by entering:		$ @simbadcat
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
$ write sys$output "*** module simbadcat ***"
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
$ write sys$output "Invalid argument given to simbadcat.com file -- ", primary
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
$   if F$SEARCH("simbadcat.imake") .nes. ""
$   then
$      vimake simbadcat
$      purge simbadcat.bld
$   else
$      if F$SEARCH("simbadcat.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake simbadcat
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @simbadcat.bld "STD"
$   else
$      @simbadcat.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create simbadcat.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack simbadcat.com -
	-s simbadcat.c -
	-i simbadcat.imake -
	-p simbadcat.pdf -
	-t tstsimbadcat.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create simbadcat.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* skycat */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "vicmain_c"
#include <simcli.h>
#include <mdms_pwdclient.h>

#define PI 3.14159265359

main44()
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create simbadcat.imake
#define  PROGRAM   simbadcat

#define MODULE_LIST simbadcat.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_C

#define LIB_TAE
#define LIB_RTL
#define LIB_P2SUB
#define LIB_MDMS
#define LIB_KERBEROS
#define LIB_SIMBAD
$ Return
$!#############################################################################
$PDF_File:
$ create simbadcat.pdf
process help=*
PARM OUT TYPE=STRING COUNT=1
PARM USERID  TYPE=STRING COUNT=(0:1) DEFAULT="u2141ozs"
PARM PASSWORD  TYPE=STRING COUNT=(0:1) DEFAULT="none"
PARM NAME  TYPE=STRING COUNT=(0:1) DEFAULT="ngc 1097"
PARM STATUS TYPE=(STRING,100) COUNT=(0:1) DEFAULT=""
END-PROC

.TITLE
VICAR program SIMBADCAT

.HELP
PURPOSE:
To create an ascii catalogue in the same format as generated by program skycat
but containing only the information for one object in the SIMBAD data base.

EXECUTION:
simbadcat out=catalogue.ascii name="NGC 1097" password="    "
If the object is not found the catalogue will only have a header record.

GETTING INTO SIMBAD:
The Simbad data base wants a password. You can access SIMBAD two ways:
 
1. Give the password:
simbadcat ...  name="..." password="   "
 
2. Let kerberos get it for you by running the following unix code before
getting into vicar:
selcat o
kinit (which will ask you for your initials and kerberos password).
vicar
simbadcat ...  name="..."

.PAGE
METHOD:

FORMAT of the output catalogue:
The object occupies one ascii record with 13 fields (columns).

Field    Description
1     Object entry number (integer 1- n).
2     RA ( double, degrees j2000 ).
3     Dec ( double, degrees j2000 ).
4     line ( double, image line number 1-n ).
5     sample ( double, image sample number 1-n ).
6     b magnitude. double
7     v magnitude. double
8     Proper motion in RA ( double, arcseconds/year ).
9     Proper motion in Dec ( double, arcseconds/year ).
10    Object classification ( string, ie: Star,Galaxy,Radio..).
11    Morphology ( string, ie: SBbc,E,...).
12    Spectrum ( string, ie: G2,A0,...).
13    Object name (ascii string from the originating catalogue ).

Numerical fields which are empty are filled with -99.000000
Ascii fields which are empty are filled with "none"

HISTORY:
4-1-97  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1

.VARI OUT
Output catalogue

.VARI USERID
Simbad userid
 
.VARI PASSWORD
Simbad password

.VARI NAME
Object name

.VARI STATUS
Directory to contain
status file.
Defaults to local.
 
.LEVEL2

.VARI OUT
Output ascii catalogue.

.VARI USERID
Simbad userid. 
This defaults to a valid userid unless you have your own.
 
.VARI PASSWORD
Simbad password.
See the METHOD section to permit kerberos to get it for you.

.VARI NAME
The name of the object whose information goes in the output catalogue.
Example: name="ngc 1097"
A query will be sent to SIMBAD to retrieve the object's information.
 
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
$ create tstsimbadcat.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
simbadcat out=catalogue.ascii name="NGC 1100"
!
end-proc
$ Return
$!#############################################################################
