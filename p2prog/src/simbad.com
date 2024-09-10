$!****************************************************************************
$!
$! Build proc for MIPL module simbad
$! VPACK Version 1.9, Friday, December 12, 1997, 09:38:42
$!
$! Execute by entering:		$ @simbad
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
$ write sys$output "*** module simbad ***"
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
$ write sys$output "Invalid argument given to simbad.com file -- ", primary
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
$   if F$SEARCH("simbad.imake") .nes. ""
$   then
$      vimake simbad
$      purge simbad.bld
$   else
$      if F$SEARCH("simbad.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake simbad
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @simbad.bld "STD"
$   else
$      @simbad.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create simbad.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack simbad.com -
	-s simbad.c -
	-i simbad.imake -
	-p simbad.pdf -
	-t tstsimbad.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create simbad.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* mpfcahv */
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "vicmain_c"
#include <string.h>
#include <simcli.h>
#include <mdms_pwdclient.h>

main44()
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create simbad.imake
#define  PROGRAM   simbad

#define MODULE_LIST simbad.c

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
$ create simbad.pdf
process help=*
PARM USERID  TYPE=STRING COUNT=(0:1) DEFAULT="u2141ozs"
PARM PASSWORD  TYPE=STRING COUNT=(0:1) DEFAULT="none"
PARM NAME  TYPE=STRING COUNT=(0:1) DEFAULT=--
PARM LOCATION TYPE=STRING COUNT=(0:1) DEFAULT=--
PARM RADIUS TYPE=STRING COUNT=(0:1) DEFAULT="0 15"
end-proc

.title
SIMBAD

.help
PURPOSE:
To return information from the SIMBAD astronomical data base in 
Strasbourg France.

EXECUTION STATEMENT:
simbad password="   " name="   "
simbad name="   "

GETTING INTO SIMBAD:
The Simbad data base wants a password. You can access SIMBAD two ways:

1. Give the password:
simbad password="   " name="   "

2. Let kerberos get it for you by running the following unix code before
getting into vicar:
selcat o
kinit (which will ask you for your kerberos password).
vicar
simbad name="   "

PROGRAM USAGE:
You can run SIMBAD two ways:

1. Specifying an object NAME, see NAME keyword,
Example:  simbad name="ngc 1097"

2. Specifying a coordinate in RA & Dec with LOCATION and a RADIUS. See keywords.
Example:  simbad location="20 10 -23 11" radius="0 30"

.page
PROGRAM HISTORY

Cognizant programmer: Jean Lorre
Revisions: 9-1-97

.page
.LEVEL1

.VARI USERID
Simbad userid

.VARI PASSWORD
Simbad password

.VARI NAME
Object name

.VARI LOCATION
Object RA & Dec

.VARI RADIUS
Radius about Location
to find objects within.
default: radius="0 15"

.LEVEL2

.VARI USERID
Simbad userid. 
This defaults to a valid userid unless you have your own.

.VARI PASSWORD
Simbad password.
See the METHOD section to permit kerberos to get it for you.

.VARI NAME
Object name.
In this mode RADIUS is ignored and information on the object name is
returned. Example: name="ngc 1097"

.VARI LOCATION
Object location in RA & Dec (b 2000 coordinates).
In this mode information is returned on all objects within RADIUS of this
location. This is a string. You list RA first and then Dec. The sign on
Dec tells Simbad that it's Declination information.
Example: location="10 5 22 +45 10 11"

If you prefix the location with g= you get galactic coordinates.
If you prefix the location with e= you get ecliptical coordinates.

.VARI RADIUS
Search radius about LOCATION.
This is a string.
Example: radius="0 11"  ( 11 minutes of arc radius )
default: radius="0 15"

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstsimbad.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
simbad name="ngc 1097"
simbad location="5 5 5 +5 5 5" radius="0 10"
!
end-proc
$ Return
$!#############################################################################
