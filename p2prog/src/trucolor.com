$!****************************************************************************
$!
$! Build proc for MIPL module trucolor
$! VPACK Version 1.8, Thursday, September 19, 1996, 18:22:47
$!
$! Execute by entering:		$ @trucolor
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
$ write sys$output "*** module trucolor ***"
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
$ write sys$output "Invalid argument given to trucolor.com file -- ", primary
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
$   if F$SEARCH("trucolor.imake") .nes. ""
$   then
$      vimake trucolor
$      purge trucolor.bld
$   else
$      if F$SEARCH("trucolor.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake trucolor
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @trucolor.bld "STD"
$   else
$      @trucolor.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create trucolor.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack trucolor.com -
	-s trucolor.f -
	-i trucolor.imake -
	-p trucolor.pdf -
	-t tsttrucolor.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create trucolor.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
c 2 JAN 95 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
c
      include 'VICMAIN_FOR'
      subroutine main44
      implicit NONE

      integer ngrid,maxin,maxcolors,ntable,mapxy
      parameter (ngrid=5,maxin=10,maxcolors=10,ntable=30)
      parameter (mapxy=256)

c ngrid=     the number of grid points in each dimension in the 
c            xyY space for the display device.
c ntable=    the number of grid points in each dimension in the 
c            xyY space for the display device interpolation table
c maxin=     the largest number of input images
c maxcolors= the largest number of special colors
c mapxy=     the nl and ns of the 4th output chromaticity map.
c
c WARNING: change array sizes in DLSQP if you change MAXIN or MAXCOLORS

      character*4 device
      character*5 project
      character*80 msg

      integer*4 count,def,colors(maxcolors),filter(maxin),status,jj
      integer*4 unit(maxin),camera,fds,label_data(80),ncolors,nconv
      integer*4 look(3,maxcolors*2),cube(ngrid,ngrid,ngrid),ounit(3)
      integer*4 nli(maxin),nsi(maxin),tunit,nin,nout,inc,nterms,nfits
      integer*4 nioverf,nfilter,nxyz,nres,maxdn,i,ns,nl,ind,j,k,ios,kk
      integer*4 npolygon,nequations,nunknowns,ibounds,jbounds,maxhist
      integer*4 npixels,n,l,m,index,irange,idist,jdist,ir,ksave,jsave
      integer*4 isave,icolor,kbounds,lbounds,i_red,i_green,i_blue,ii

      integer*2 buf(4000,maxin),xybuf(mapxy,mapxy)

      real*4 conv(maxin),ioverf(maxin),yt,range,r,reall,realm,realn,xr
      real*4 special_response(maxin,maxcolors),yr,zr,dn_bot,dn_top
      real*4 special_XYZ(3,maxcolors),solution(maxin,3),xyunit,xy_slope
      real*4 corner(2,4),point(2),triangle(2,5),Yval(4),yt_offset
      real*4 matrix(4,4),dn_vs_Yxy(3,ntable,ntable,ntable),y_renorm
      real*4 dn(3),units(maxin),storage(maxin*maxcolors),yt_slope
      real*4 reflectance_XYZ(3,8),radiance_XYZ(3,8),y_offset,xy_offset
      real*4 reflectance_response(0:7,8),radiance_response(0:7,8)
      real*4 macbeth(3,24),dn_steps(ngrid),x_slope,x_offset,y_slope
      real*4 Yxy_table(3,ngrid,ngrid,ngrid),yt_min,x_max,y_max,xy_sl
      real*4 Yxy_tv(3,ngrid,ngrid,ngrid),bright,smal_ratio,y_min,yt_max
      real*4 tristim_X,tristim_Y,tristim_Z,sum,x,y,ymax,ratio,x_min
      real*4 Yxy_table_a(3,5,5,2),yxy_table_b(3,5,5,2),Yxy_tv_a(3,5,5,2)
      real*4 Yxy_table_c(3,5,5,1),Yxy_tv_b(3,5,5,2),Yxy_tv_c(3,5,5,1)

      real*8 c(maxcolors*2,maxin),cl(maxcolors*2),soln(maxin,3)
      real*8 resid(maxcolors*2),err,err_soln(maxin),wts(maxcolors*2)
      real*8 coef(maxin,3),err_muw(3),err_rgb(3)

      logical xvptst,noauto,inside,file_exists,use_macbeth
      logical cube_exists

C Due to a limit of continuation lines on UNIX, the following data
C statements must be split up.

      equivalence (yxy_table,yxy_table_a)
      equivalence (yxy_table(1,1,1,3),yxy_table_b)
      equivalence (yxy_table(1,1,1,5),yxy_table_c)
      equivalence (yxy_tv,yxy_tv_a)
      equivalence (yxy_tv(1,1,1,3),yxy_tv_b)
      equivalence (yxy_tv(1,1,1,5),yxy_tv_c)

      data err_muw/3*0.d0/,err_rgb/3*0.d0/
c reflectance tristimulus values for special colors
      data reflectance_XYZ/
     +       .2193,.0944,4.85e-4,     ! special color 1
     +       .0638,.0759,.00374,      ! special color 2
     +       .0168,.0179,.02451,      ! special color 3
     +       .0268,.01841,.01175,     ! special color 4
     +       .0177,.0156,.01124,      ! special color 5
     +       25.39,24.38,20.49,       ! desert        6
     +       8.280,8.049,10.91,       ! water         7
     +       76.80,76.88,76.96/       ! cloud         8
     

c radiance tristimulus values for special colors
      data radiance_XYZ/
     +       .7152,.3111,.001834,     ! special color 1
     +       .2278,.2773,.01439,      ! special color 2
     +       .05965,.06550,.09461,    ! special color 3
     +       .09139,.06507,.04511,    ! special color 4
     +       .06144,.05607,.04347,    ! special color 5
     +       14.32,14.16,12.33,       ! desert        6
     +       4.711,4.701,6.532,       ! water         7
     +       43.54,44.82,46.35/       ! cloud         8

c dn * I/F ( 10000 means 100% reflectance ) for special colors
c filter#1=green filter#2=red filter#3=blue
      data reflectance_response/            ! filters 0-7
     +       0.,593.5,  89244.,  238.4,  0.,0.,0.,0.,  ! special color 1   
     +       0.,6424.3, 2482.,   89.24,  0.,0.,0.,0.,  ! special color 2
     +       0.,751.9,  1680.,   345.5,  0.,0.,0.,0.,  ! special color 3
     +       0.,627.6,  6117.,   218.8,  0.,0.,0.,0.,  ! special color 4
     +       0.,725.9,  2523.,   193.6,  0.,0.,0.,0.,  ! special color 5
     +       .253e+4,.228e+4,.275e+4,.197e+4,.240e+4,.288e+4, !desert 6
     +         .266e+4,.320e+4,
     +       .0721e+4,.0745e+4,.0667e+4,.126e+4,.0559e+4,      !water 7
     +         .0520e+4,.0608e+4,.0676e+4,
     +       .711e+4,.722e+4,.711e+4,.724e+4,.710e+4,.669e+4,  !cloud 8
     +         .708e+4,.660e+4/

c dn * CONV (units of nanowatts/cm**2 ster nano ) for special colors
      data radiance_response/    ! filters 0-7
     +       0.,129.3,  15958.,  39.16, 0.,0.,0.,0.,  ! special color 1
     +       0.,1399.7, 444.,    14.66, 0.,0.,0.,0.,  ! special color 2
     +       0.,163.8,  300.5,   56.76, 0.,0.,0.,0.,  ! special color 3
     +       0.,136.7,  1094.,   35.94, 0.,0.,0.,0.,  ! special color 4
     +       0.,158.2,  451.,    31.8,  0.,0.,0.,0.,  ! special color 5
     +       .121e+5,.134e+5,.135e+5,.0982e+5,.0954e+5,      ! desert 6
     +         .0715e+5,.122e+5,.098e+5,
     +       .0344e+5,.0438e+5,.0327e+5,.0626e+5,.0222e+5,    ! water 7
     +         .0129e+5,.0256e+5,.0208e+5,
     +       .339e+5,.425e+5,.349e+5,.361e+5,.282e+5,.166e+5, ! cloud 8
     +         .298e+5,.203e+5/

c table of macbeth Yxy values to test algorithm accuracy.
c only used with the MACBETH keyword.
      data macbeth/
     +  10.1,.400,.350,  35.8,.377,.345,  19.3,.247,.251,
     +  13.3,.337,.422,  24.3,.265,.240,  43.1,.261,.343,
     +  30.1,.506,.407,  12.0,.211,.175,  19.8,.453,.306,
     +   6.6,.285,.202,  44.3,.380,.489,  43.1,.473,.438,
     +   6.1,.187,.129,  23.4,.305,.478,  12.0,.539,.313,
     +  59.1,.448,.470,  19.8,.364,.233,  19.8,.196,.252,
     +  90.0,.310,.316,  59.1,.310,.316,  36.2,.310,.316,
     +  19.8,.310,.316,   9.0,.310,.316,   3.1,.310,.316/

c the dn steps on the cube Yxy_table
      data dn_steps/0.,64.,128.,192.,255./

c Table of the (Y,x,y) values recorded for the film recorder 
c at all combinations of the dn's in dn_steps() for red,green,blue.
c first index:  tristimulus Y, chromaticity x, chromaticity y
c second index: red dn
c third index:  green dn
c fourth index: blue dn
c blue=0 block
c red=0  64 128 192 255 red -->
c green=0
      data Yxy_table_a/   1.23,.2745,.2961,  2.26,.3853,.3268,
     +  5.04,.5096,.3389, 8.36,.5674,.3345, 10.11,.5826,.3319,
c green=64
     +  2.25,.2696,.3687,  3.74,.3670,.3850,  7.26,.4820,.3776,
     + 11.29,.5448,.3639, 13.43,.5637,.3572,
c green=128
     +  5.33,.2596,.4741,  7.76,.3349,.4726, 12.73,.4358,.4449,
     + 18.37,.5044,.4152, 21.28,.5303,.4005,
c green=192
     +  9.67,.2556,.5270, 13.41,.3126,.5254, 20.75,.3956,.4988,
     + 29.63,.4634,.4645, 34.17,.4957,.4438,
c green=255
     + 13.21,.2683,.5266, 17.14,.3098,.5278, 25.60,.3758,.5134,
     + 36.44,.4372,.4876, 44.98,.4729,.4674,
c blue=64 block
     +  1.48,.2235,.2194,  2.56,.3100,.2593,  5.49,.4398,.2977,
     +  9.04,.5143,.3075, 11.09,.5345,.3088,
     +  2.70,.2254,.2922,  4.30,.3043,.3223,  7.91,.4243,.3416,
     + 12.53,.4985,.3412, 14.97,.5213,.3386,
     +  6.35,.2266,.4037,  8.96,.2909,.4190, 14.25,.3907,.4157,
     + 20.89,.4665,.3976, 24.18,.4950,.3870,
     + 11.11,.2273,.4603, 15.13,.2762,.4740, 23.15,.3583,.4708,
     + 33.77,.4316,.4494, 39.08,.4660,.4326,
     + 14.70,.2370,.4600, 18.84,.2743,.4736, 27.78,.3395,.4802,
     + 41.18,.4076,.4688, 50.61,.4461,.4545/
c blue=128 block
      data Yxy_table_b/   1.93,.1813,.1446,  2.97,.2288,.1717,
     +  5.80,.3347,.2212, 9.61,.4198,.2512, 12.12,.4532,.2608,
     +  3.37,.1853,.1977,  4.97,.2335,.2257,  8.54,.3332,.2651,
     + 13.08,.4167,.2865, 16.12,.4496,.2915,
     +  7.62,.1910,.2941, 10.40,.2351,.3168, 15.85,.3213,.3397,
     + 22.10,.4026,.3467, 26.39,.4360,.3431,
     + 13.41,.1944,.3551, 19.83,.2322,.3804, 26.55,.3033,.4022,
     + 37.35,.3782,.4046, 43.64,.4183,.3971,
     + 17.76,.2038,.3615, 22.16,.2356,.3890, 32.09,.2925,.4147,
     + 46.64,.3599,.4262, 57.46,.4025,.4242,
c blue=192 block
     +  2.81,.1641,.1154,  4.05,.1916,.1341,  7.27,.2604,.1697,
     + 11.91,.3334,.2000, 15.38,.3668,.2140,
     +  4.41,.1658,.1480,  6.14,.1934,.1669, 10.24,.2591,.1993,
     + 15.86,.3293,.2247, 19.95,.3632,.2365, 
     +  9.10,.1690,.2131, 11.91,.1965,.2312, 17.76,.2549,.2555,
     + 25.69,.3226,.2731, 30.86,.3581,.2800,
     + 15.69,.1724,.2681, 20.43,.1965,.2883, 29.78,.2473,.3141,
     + 42.65,.3105,.3309, 50.43,.3496,.3339,
     + 20.02,.1811,.2858, 25.06,.2016,.3051, 36.09,.2433,.3328,
     + 53.05,.3007,.3566, 65.44,.3424,.3657/
c blue=255 block
      data Yxy_table_c/
     +  4.45,.1635,.1315,  6.00,.1876,.1462,  9.73,.2429,.1731,
     + 15.13,.3063,.1986, 19.22,.3413,.2118,
     +  6.15,.1650,.1546,  8.15,.1884,.1686, 12.70,.2411,.1929,
     + 18.94,.3025,.2147, 23.47,.3364,.2258,
     + 10.93,.1670,.2017, 14.20,.1897,.2155, 20.57,.2370,.2336,
     + 29.12,.2947,.2494, 34.77,.3278,.2567,
     + 17.74,.1690,.2443, 23.06,.1894,.2596, 33.46,.2307,.2796,
     + 47.02,.2837,.2949, 55.60,.3175,.3005,
     + 22.16,.1772,.2610, 27.63,.1940,.2749, 39.40,.2288,.2975,
     + 57.57,.2768,.3187, 71.25,.3116,.3285/

c Table of the (Y,x,y) values recorded for the TV monitor
c at all combinations of the dn's in dn_steps() for red,green,blue.
c first index:  tristimulus Y, chromaticity x, chromaticity y
c second index: red dn
c third index:  green dn
c fourth index: blue dn
c blue=0 block
c red=0  64 128 192 255 red -->
c green=0
      data Yxy_tv_a/ 0.,.142,.571, .1,.478,.347, .4,.630,.315,
     + 1.2,.65,.302, 2.3,.656,.305,
c green=64
     + .1,.136,.750, .2,.296,.555, .5,.579,.355, 1.3,.629,.324,
     + 2.5,.642,.315,
c green=128
     + .9,.163,.706, 1.,.228,.657, 1.3,.409,.5, 2.1,.531,.407,
     + 3.4,.586,.363,
c green=192
     + 2.7,.177,.695, 2.8,.202,.671, 3.1,.304,.589, 3.9,.420,.496,
     + 5.1,.496,.432,
c green=255
     + 5.5,.186,.685, 5.6,.198,.678, 5.9,.257,.630, 6.8,.343,.560,
     + 8.0,.419,.497,
c blue=64 block
     + 0.,.117,.058, .1,.368,.368, .4,.539,.303, 1.2,.644,.268,
     + 2.5,.640,.297,
     + .1,.134,.407, .2,.282,.369, .6,.527,.333, 1.4,.605,.308,
     + 2.6,.629,.308,
     + 1.,.159,.610, 1.,.230,.571, 1.4,.397,.462, 2.2,.515,.390,
     + 3.5,.572,.353,
     + 2.8,.183,.650, 2.8,.207,.637, 3.2,.303,.566, 4.,.416,.480,
     + 5.3,.493,.423,
     + 5.6,.186,.666, 5.7,.198,.657, 6.,.255,.613, 6.8,.340,.549,
     + 8.1,.417,.489/
c blue=128 block
      data Yxy_tv_b/   .1,.128,.068, .1,.178,.093, .5,.375,.169,
     + 1.3,.501,.234, 2.5,.566,.260,
     + .2,.131,.139, .3,.193,.144, .6,.359,.203, 1.5,.490,.248,
     + 2.7,.556,.271,
     + 1.,.150,.350, 1.1,.189,.348, 1.5,.311,.335, 2.3,.441,.323,
     + 3.5,.516,.315,
     + 2.8,.171,.5, 2.9,.188,.492, 3.2,.269,.459, 4.1,.373,.417,
     + 5.3,.454,.384,
     + 5.7,.179,.576, 5.8,.196,.569, 6.1,.239,.539, 7.,.319,.497,
     + 8.2,.394,.452,
c blue=192 block
     + .3,.137,.060, .3,.160,.064, .7,.254,.113, 1.5,.372,.166,
     + 2.7,.459,.209,
     + .4,.138,.082, .5,.161,.094, .8,.253,.130, 1.6,.369,.178,
     + 2.9,.453,.217,
     + 1.2,.147,.196, 1.3,.167,.198, 1.6,.242,.214, 2.5,.344,.238,
     + 3.7,.431,.254,
     + 3.,.158,.336, 3.1,.172,.333, 3.4,.228,.331, 4.3,.315,.324,
     + 5.5,.393,.319,
     + 5.9,.168,.443, 5.9,.178,.439, 6.3,.217,.427, 7.1,.284,.409,
     + 8.4,.353,.389/
c blue=255 block
      data Yxy_tv_c/
     + .6,.140,.056, .6,.150,.060, 1.,.201,.089, 1.8,.286,.125,
     + 3.,.367,.163,
     + .7,.140,.067, .7,.150,.071, 1.1,.203,.094, 1.9,.286,.134,
     + 3.1,.365,.170,
     + 1.5,.145,.132, 1.5,.155,.135, 1.9,.202,.150, 2.7,.277,.176,
     + 4.,.355,.201,
     + 3.3,.152,.231, 3.4,.161,.231, 3.7,.199,.238, 4.5,.264,.247,
     + 5.8,.333,.256,
     + 6.2,.162,.329, 6.2,.167,.329, 6.6,.197,.327, 7.4,.250,.323,
     + 8.7,.311,.320/

      call ifmessage('TRUCOLOR version 2-JAN-95')
      call xvpcnt('INP',nin)
      if(nin.lt.3)then
         call mabend('Require at least 3 inputs')
      endif
      call xvpcnt('OUT',nout)
      if(nout.lt.3)then
         call mabend('Require 3 outputs')
      endif
      call xvparm('COLORS',colors,ncolors,def,maxcolors)
      if(ncolors.lt.nin.and.ncolors.gt.0)then
         call mabend('#special colors >= #inputs')
      endif
      call xvparm('CONV',conv,nconv,def,maxin)
      call xvparm('IOVF',ioverf,nioverf,def,maxin)
      call xvparm('FILTER',filter,nfilter,def,maxin)
      call xvparm('XYZ',special_XYZ,nxyz,def,3*maxcolors)
      call xvparm('RESPONSE',storage,nres,def,maxin*maxcolors)
      call xvparm('DEVICE',device,count,def,1)
      call xvparm('INC',inc,count,def,1)
      call xvparm('NTERMS',nterms,count,def,1)
      call xvparm('NFITS',nfits,count,def,1)
      call xvparm('ILLUMIN',bright,count,def,1)
      use_macbeth=xvptst('MACBETH')
      noauto=xvptst('NOAUTO')
      if(noauto) smal_ratio=1.0
      if(nfits.le.nterms)then
         call mabend('NTERMS < NFITS')
      endif
      maxdn=255

c determine # special colors
      if(ncolors.eq.0)then
        if(nfilter.gt.0) ncolors=nfilter
        if(nxyz.gt.0)    ncolors=nxyz/3
        if(nres.gt.0)    ncolors=nres/nin
        if(ncolors.eq.0)then
          call xvmessage('Must specify either special colors or',' ')
          call xvmessage('XYZ and RESPONSE keywords.',' ')
          call abend
        endif
      endif

c open all inputs
      do i=1,nin
        call xvunit(unit(i),'INP',i,status,' ')
        call xvopen(unit(i),status,'U_FORMAT','HALF',
     &		'OPEN_ACT','SA','IO_ACT','SA',' ')
        call xvget(unit(i),status,'NL',nli(i),'NS',nsi(i),' ')
      enddo

c determine output image size
      ns=nsi(1)
      nl=nli(1)
      do i=2,nin
         nl=min(nl,nli(i))
         ns=min(ns,nsi(i))
      enddo

c extract project ID and label information
      do i=1,nin
        call getproj(unit(i),project,camera,fds,ind)
        call getlabcon(unit(i),project,label_data,ind)
        if(ind.le.1)then
          if(i.gt.nfilter) filter(i)=label_data(4)        
          if(i.gt.nconv) conv(i)=float(label_data(29)) * 1.0e+12
          if(i.gt.nioverf) ioverf(i)=float(label_data(28)) * 1.0e+4  
        else
          if(i.gt.nfilter) filter(i)=-1        
          if(i.gt.nconv) conv(i)=-1.
          if(i.gt.nioverf) ioverf(i)=-1.  
        endif
      enddo

c print status
      call xvmessage(' ',' ')
      call xvmessage('input filter        conv        ioverf',' ')
      do i=1,nin
        write(msg,100) i,filter(i),conv(i),ioverf(i)
100     format(3x,i2,5x,i2,8x,e10.3,f10.3)
        call xvmessage(msg,' ')
      enddo

c extract the XYZ values to be used in the fit
      if(nxyz.eq.0)then
        do i=1,ncolors
          do j=1,3
            if(device.eq.'TV')then
              special_XYZ(j,i)=radiance_XYZ(j,colors(i))
            else
              special_XYZ(j,i)=reflectance_XYZ(j,colors(i))
            endif
          enddo
        enddo        
      endif

c extract the response values to be used in the fit
      if(nres.eq.0)then
        do i=1,ncolors
          do j=1,nin
            if(device.eq.'TV')then
              special_response(j,i)=
     +                    radiance_response(filter(j),colors(i)) *
     +                    bright
            else
              special_response(j,i)=
     +                    reflectance_response(filter(j),colors(i)) *
     +                    bright
            endif
          enddo
        enddo        
      else
        k=0
        do i=1,ncolors
          do j=1,nin
            k=k+1
            special_response(j,i)=storage(k)
          enddo
        enddo
      endif

c extract the correct Yxy table to use.
c This may require the reading of either cube.film or cube.tv file.
      if(device.eq.'TV') then
        inquire(FILE='CUBE.TV',EXIST=cube_exists)
        if(cube_exists)then
          open(unit=1,file='CUBE.TV',STATUS='OLD',err=1002,iostat=ios)
          if(ios.gt.0) goto 1002
          do i=1,5
            do j=1,5
              do k=1,5
                read(1,*,err=1002,iostat=ios) 
     +                 (Yxy_table(kk,k,j,i),kk=1,3)
                if(ios.gt.0) goto 1002
              enddo
            enddo
          enddo
          close(1)
        else    ! move table to working table
          call mve(7,3*ngrid*ngrid*ngrid,Yxy_tv,Yxy_table,1,1)
        endif
      else
        inquire(FILE='CUBE.FILM',EXIST=cube_exists)
        if(cube_exists)then
          open(unit=1,file='CUBE.FILM',STATUS='OLD',err=1002,
     +         iostat=ios)
          do i=1,5
            do j=1,5
              do k=1,5
                read(1,*,err=1002,iostat=ios) 
     +                 (Yxy_table(kk,k,j,i),kk=1,3)
                if(ios.gt.0) goto 1002
              enddo
            enddo
          enddo
          close(1)
        endif    ! internal table already in place
      endif
      goto 1003
1002    call mabend('trouble reading the cube file')
1003  continue

c convert luminance to square-root of luminance to better redistribute
c the information and to linearize the table of Yxy values.
      do i=1,ngrid
        do j=1,ngrid
          do k=1,ngrid
            Yxy_table(1,k,j,i)=sqrt(Yxy_table(1,k,j,i))
          enddo
        enddo
      enddo

c extract the scaling values to use (I/F or CONV).
      do i=1,nin
        if(device.eq.'TV')then
          units(i)=conv(i)
        else
          units(i)=ioverf(i)
        endif
      enddo

c set up triangle spanning the possible set of chromaticity coordinates.
c We use this later to reject non physical chromaticities.
      if(device.eq.'TV')then
        npolygon=3
        triangle(1,1)=Yxy_table(2,ngrid,1,1) + .02  ! x
        triangle(2,1)=Yxy_table(3,ngrid,1,1)        ! y
        triangle(1,2)=Yxy_table(2,1,ngrid,1) - .01  ! x
        triangle(2,2)=Yxy_table(3,1,ngrid,1) + .02  ! y
        triangle(1,3)=Yxy_table(2,1,1,ngrid) - .015 ! x
        triangle(2,3)=Yxy_table(3,1,1,ngrid) - .02  ! y
      else
        npolygon=5
        triangle(1,1)=Yxy_table(2,1,ngrid,1) - .02       ! x
        triangle(2,1)=Yxy_table(3,1,ngrid,1) + .02       ! y
        triangle(1,2)=Yxy_table(2,ngrid,ngrid,1) + .01   ! x
        triangle(2,2)=Yxy_table(3,ngrid,ngrid,1) + .01   ! y
        triangle(1,3)=Yxy_table(2,ngrid,1,1) + .02       ! x
        triangle(2,3)=Yxy_table(3,ngrid,1,1)             ! y
        triangle(1,4)=Yxy_table(2,1,1,ngrid) - .01       ! x
        triangle(2,4)=Yxy_table(3,1,1,ngrid) - .03       ! y
        triangle(1,5)=Yxy_table(2,1,ngrid,ngrid) - .015  ! x
        triangle(2,5)=Yxy_table(3,1,ngrid,ngrid)         ! y
      endif

c set up and solve the least squares problem relating camera response 
c through each of the input filters to special color XYZ tristimulus
c values. The equations are like:
c  X(color1)=Ax*response(filter1)+Bx*response(filter2)+...
c  X(color2)=Ax*response(filter1)+Bx*response(filter2)+...
c  X.....................................................
c solve for Ax,Bx...
c  Y(color1)=Ay*response(filter1)+By*response(filter2)+...
c  Y(color2)=Ay*response(filter1)+By*response(filter2)+...
c  Y.....................................................
c solve for Ay,By...
c  the same for Z
      
      nequations=ncolors
      nunknowns=nin
      if(nequations.lt.nunknowns)then
        call mabend('#special colors must be > #inputs')
      else if(nequations.eq.nunknowns)then
        nequations=2*nequations     ! to trick DLSQP
      endif

c     Solve for X coefficients   soln(1-nin,1)
      call xvmessage(' ',' ')
      call
     *   xvmessage('Solution for DN to Tristimulus transformation.',' ')
      do i=1,nequations
        ii=mod(i,ncolors)
        if(ii.eq.0)ii=ncolors
        cl(i)=special_XYZ(1,ii)
        do j=1,nunknowns
          c(i,j)=special_response(j,ii)
        enddo
        wts(i)=1.d0
      enddo
      call dlsqp(nequations,nunknowns,c,cl,
     +           soln(1,1),resid,err,err_soln,wts,ind)
      if(ind.eq.1)then
        call mabend('DLSQP: divide by zero in X tristimulus fit')
      endif
      call xvmessage('X solutions',' ')
      call xvmessage('input_file  coefficient  uncertainty',' ')
      do i=1,nunknowns
        write(msg,101)i,soln(i,1),err_soln(i)
101     format(4x,i2,6x,d11.4,4x,d11.4)
        call xvmessage(msg,' ')
      enddo
      call xvmessage('input_point tristim_value  residual',' ')
      do i=1,ncolors
        write(msg,101)i,cl(i),resid(i)
        call xvmessage(msg,' ')
      enddo

c     Solve for Y coefficients   soln(1-nin,2)
      do i=1,nequations
        ii=mod(i,ncolors)
        if(ii.eq.0)ii=ncolors
        cl(i)=special_XYZ(2,ii)
      enddo
      call dlsqp(nequations,nunknowns,c,cl,
     +           soln(1,2),resid,err,err_soln,wts,ind)
      if(ind.eq.1)then
        call mabend('DLSQP: divide by zero in Y tristimulus fit')
      endif
      call xvmessage('Y solutions',' ')
      call xvmessage('input_file  coefficient  uncertainty',' ')
      do i=1,nunknowns
        write(msg,101)i,soln(i,2),err_soln(i)
        call xvmessage(msg,' ')
      enddo
      call xvmessage('input_point tristim_value  residual',' ')
      do i=1,ncolors
        write(msg,101)i,cl(i),resid(i)
        call xvmessage(msg,' ')
      enddo

c     Solve for Z coefficients   soln(1-nin,3)
      do i=1,nequations
        ii=mod(i,ncolors)
        if(ii.eq.0)ii=ncolors
        cl(i)=special_XYZ(3,ii)
      enddo
      call dlsqp(nequations,nunknowns,c,cl,
     +           soln(1,3),resid,err,err_soln,wts,ind)
      if(ind.eq.1)then
        call mabend('DLSQP: divide by zero in Z tristimulus fit')
      endif
      call xvmessage('Z solutions',' ')
      call xvmessage('input_file  coefficient  uncertainty',' ')
      do i=1,nunknowns
        write(msg,101)i,soln(i,3),err_soln(i)
        call xvmessage(msg,' ')
      enddo
      call xvmessage('input_point tristim_value  residual',' ')
      do i=1,ncolors
        write(msg,101)i,cl(i),resid(i)
        call xvmessage(msg,' ')
      enddo

c convert solutions from real*8 to real*4
      do j=1,3
        do i=1,nin
          solution(i,j)=soln(i,j)
        enddo
      enddo

c auto scaling ***********************************************
c refer to OPERATION section for explanation under SCALING.
      if(noauto) goto 200
      call xvmessage(' ',' ')
      call xvmessage('Solution for the Y scaling factor.',' ')
      ibounds=0
      jbounds=0
      npixels=0
      smal_ratio=1.0e+30
      do jj=1,nl,inc                     ! line loop
        do k=1,nin                      ! picture loop
          call xvread(unit(k),buf(1,k),status,'LINE',jj,' ')
        enddo
        do ii=1,ns,inc                   ! pixel loop
          npixels=npixels+1

c         Compute tristimulus values from coefficients.
          tristim_X=0.
          tristim_Y=0.
          tristim_Z=0.
          do n=1,nin                    ! picture loop
            tristim_X = tristim_X + solution(n,1)*buf(ii,n)*units(n)
            tristim_Y = tristim_Y + solution(n,2)*buf(ii,n)*units(n)
            tristim_Z = tristim_Z + solution(n,3)*buf(ii,n)*units(n)
          enddo
          
c         Convert to Y,x,y
          sum=tristim_X + tristim_Y + tristim_Z
          if(sum.ne.0.0)then
            x=tristim_X/sum
            y=tristim_Y/sum
          else
            x=0.0
            y=0.0
          endif

c         Ignore point if Y is negative.
c         We can get a negative scaling otherwise.
          if( tristim_Y .le. 0.0 )then
            jbounds=jbounds+1
            goto 20
          endif

c         Slip in the macbeth table values instead of input data 
c         for scaling.
          if(use_macbeth)then
            if(npixels.le.24)then
               tristim_Y=macbeth(1,npixels)
                       x=macbeth(2,npixels)
                       y=macbeth(3,npixels)
            else
               goto 200
            endif
          endif

          tristim_Y=sqrt(tristim_Y)

c         At each (x,y) determine the largest possible Y for that
c         display device, and keep track of the smallest ratio of:
c         (largest possible Y)/(desired Y).
          point(1)=x
          point(2)=y

c         Ignore points if color is clearly unreproducible.
          if(.not.inside(point,triangle,npolygon))then
            ibounds=ibounds+1
            goto 20
          endif

c         search the red-green face of the data cube.
          do i=1,ngrid-1    ! green
            do j=1,ngrid-1  ! red
              corner(1,1)=Yxy_table(2,j,i,ngrid)     !x
              corner(2,1)=Yxy_table(3,j,i,ngrid)     !y
              corner(1,2)=Yxy_table(2,j+1,i,ngrid)
              corner(2,2)=Yxy_table(3,j+1,i,ngrid)
              corner(1,3)=Yxy_table(2,j+1,i+1,ngrid)
              corner(2,3)=Yxy_table(3,j+1,i+1,ngrid)
              corner(1,4)=Yxy_table(2,j,i+1,ngrid)
              corner(2,4)=Yxy_table(3,j,i+1,ngrid)
              if(inside(point,corner,4)) then
                Yval(1)=Yxy_table(1,j,i,ngrid)       !Y
                Yval(2)=Yxy_table(1,j+1,i,ngrid)
                Yval(3)=Yxy_table(1,j+1,i+1,ngrid)
                Yval(4)=Yxy_table(1,j,i+1,ngrid)
                goto 10
              endif
            enddo
          enddo
c         search the green-blue face of the data cube.
          do i=1,ngrid-1    ! blue
            do j=1,ngrid-1  ! green
              corner(1,1)=Yxy_table(2,ngrid,j,i)
              corner(2,1)=Yxy_table(3,ngrid,j,i)
              corner(1,2)=Yxy_table(2,ngrid,j+1,i)
              corner(2,2)=Yxy_table(3,ngrid,j+1,i)
              corner(1,3)=Yxy_table(2,ngrid,j+1,i+1)
              corner(2,3)=Yxy_table(3,ngrid,j+1,i+1)
              corner(1,4)=Yxy_table(2,ngrid,j,i+1)
              corner(2,4)=Yxy_table(3,ngrid,j,i+1)
              if(inside(point,corner,4)) then
                Yval(1)=Yxy_table(1,ngrid,j,i)
                Yval(2)=Yxy_table(1,ngrid,j+1,i)
                Yval(3)=Yxy_table(1,ngrid,j+1,i+1)
                Yval(4)=Yxy_table(1,ngrid,j,i+1)
                goto 10
              endif
            enddo
          enddo
c         search the blue-red face of the data cube.
          do i=1,ngrid-1    ! blue
            do j=1,ngrid-1  ! red
              corner(1,1)=Yxy_table(2,j,ngrid,i)
              corner(2,1)=Yxy_table(3,j,ngrid,i)
              corner(1,2)=Yxy_table(2,j+1,ngrid,i)
              corner(2,2)=Yxy_table(3,j+1,ngrid,i)
              corner(1,3)=Yxy_table(2,j+1,ngrid,i+1)
              corner(2,3)=Yxy_table(3,j+1,ngrid,i+1)
              corner(1,4)=Yxy_table(2,j,ngrid,i+1)
              corner(2,4)=Yxy_table(3,j,ngrid,i+1)
              if(inside(point,corner,4)) then
                Yval(1)=Yxy_table(1,j,ngrid,i)
                Yval(2)=Yxy_table(1,j+1,ngrid,i)
                Yval(3)=Yxy_table(1,j+1,ngrid,i+1)
                Yval(4)=Yxy_table(1,j,ngrid,i+1)
                goto 10
              endif
            enddo
          enddo

c         If you are here then no polygon contains (x,y) 
          ibounds=ibounds+1
          goto 20

10        continue
c         fit surface to determine the Y amplitude maximum.
c         surface is of the form:  Y=Ax+By+Cxy+D
          do k=1,4
            matrix(k,1)=corner(1,k)              !x
            matrix(k,2)=corner(2,k)              !y
            matrix(k,3)=corner(1,k)*corner(2,k)  !xy
            matrix(k,4)=1.0
          enddo
          call newsimq(matrix,Yval,4,1,ind)
          if(ind.eq.1) then
            call xvmessage('SIMQ singular in Y surface computation',' ')
            goto 20
          endif

c         compute Y amplitude
          Ymax=Yval(1)*x+Yval(2)*y+Yval(3)*x*y+Yval(4)

c         remember the smallest Ymax/tristim_Y
          ratio=Ymax/tristim_Y
          if(ratio.lt.smal_ratio) smal_ratio=ratio

20      continue
        enddo        ! pixel loop
      enddo          ! line loop
      call prnt(7,1,smal_ratio,'Y scaling factor=.')
      call prnt(7,1,(100.*ibounds)/npixels,
     +           '% of points missing all polygons=.')
      call prnt(7,1,(100.*jbounds)/npixels,
     +           '% of points inside xy space with negative Y=.')
200   continue

c Check if the dn to Yxy interpolation table exists on disk.
      if(device.eq.'TV') then
        inquire(FILE='TRUCOLOR.TV',EXIST=file_exists)
      else
        inquire(FILE='TRUCOLOR.FILM',EXIST=file_exists)
      endif

c Construct a regular grid of points in Yxy space from the Yxy_table().
c At each grid point fit a polynomial to surrounding points
c and save the coefficients to permit rapid interpolation from Yxy
c to dn_red,green & blue.
c The function is of the form:
c dn_red=  Ax+By+CY+G
c dn_green=Ax+By+CY+G
c dn_blue= Ax+By+CY+G

c     locate range of x,y,Y
c     Yt=tristimulus_Y x,y are chromaticity
      x_min=1.0e+30
      y_min=1.0e+30
      Yt_min=1.0e+30
      x_max=-1.0e+30
      y_max=-1.0e+30
      Yt_max=-1.0e+30
      do i=1,ngrid
        do j=1,ngrid
          do k=1,ngrid
            if(Yxy_table(1,k,j,i).lt.Yt_min) Yt_min=Yxy_table(1,k,j,i)
            if(Yxy_table(1,k,j,i).gt.Yt_max) Yt_max=Yxy_table(1,k,j,i)
            if(Yxy_table(2,k,j,i).lt.x_min) x_min=Yxy_table(2,k,j,i)
            if(Yxy_table(2,k,j,i).gt.x_max) x_max=Yxy_table(2,k,j,i)
            if(Yxy_table(3,k,j,i).lt.y_min) y_min=Yxy_table(3,k,j,i)
            if(Yxy_table(3,k,j,i).gt.y_max) y_max=Yxy_table(3,k,j,i)
          enddo
        enddo
      enddo 

c     extend limits just a bit so we don't get roundoff errors leading
c     to index computations < 1
      Yt_min=Yt_min - .0001
      x_min=x_min   - .0001
      y_min=y_min   - .0001

c     set up conversion from Yxy dimensions to indices later used to
c     locate the fit to use. Indices are the grid points in COEF.
      x_slope=(ntable-1)/(x_max - x_min)
      x_offset=ntable - x_slope * x_max
      y_slope=(ntable-1)/(y_max - y_min)
      y_offset=ntable - y_slope * y_max
      Yt_slope=(ntable-1)/(Yt_max - Yt_min)
      Yt_offset=ntable - Yt_slope * Yt_max

      if(file_exists) goto 400

c     Compute Y rescaling constant. Used to reduce Y range to the same
c     as x and y for weighting and least squares.
      Y_renorm=(((x_max-x_min)+(y_max-y_min))/2.0)/(Yt_max-Yt_min)

      call xvmessage(' ',' ')
      call
     *  xvmessage('Solution for the Yxy to DN interpolation table.',' ') 
c     loop over all output grid locations
      do l=1,ntable       ! x
        x=(l - x_offset)/x_slope
        do m=1,ntable     ! y
          y=(m - y_offset)/y_slope
          do n=1,ntable   ! Y
c           compute the Yxy coords that correspond to the indexes (l,m,n)
            Yt=(n - Yt_offset)/Yt_slope

c           search for nearest point to (x,y,Yt)
            range=1.0e+30
            do i=1,ngrid       ! blue
              do j=1,ngrid     ! green
                do k=1,ngrid   ! red
                  r=((Yxy_table(1,k,j,i)-Yt)*Y_renorm)**2 +
     +              (Yxy_table(2,k,j,i)-x )**2 +
     +              (Yxy_table(3,k,j,i)-y )**2 
                  if(r.lt.range)then
                    range=r
                    ii=i
                    jj=j
                    kk=k
                  endif
                enddo
              enddo
            enddo 

c           compute points to use, store in look(r,g,b)
c           select the nearest nfits indexes to kk,jj,ii.
            do i=1,ngrid
              do j=1,ngrid
                do k=1,ngrid
                  cube(k,j,i)=0
                enddo
              enddo
            enddo
            do index=1,nfits
              irange=100
              do i=1,ngrid
                idist=(i-ii)**2
                do j=1,ngrid
                  jdist=(j-jj)**2
                  do k=1,ngrid
                    if(cube(k,j,i).eq.0)then
                      ir=(k-kk)**2+jdist+idist
                      if(ir.lt.irange)then
                        irange=ir
                        ksave=k
                        jsave=j
                        isave=i
                      endif
                    endif
                  enddo
                enddo
              enddo
              cube(ksave,jsave,isave)=1
              look(1,index)=ksave  ! red index
              look(2,index)=jsave  ! green index
              look(3,index)=isave  ! blue index
            enddo                        

c           compute weights to use in least squares fit
            do i=1,nfits
              kk=look(1,i)
              jj=look(2,i)
              ii=look(3,i)
              wts(i)=range / sqrt(((Yxy_table(1,kk,jj,ii)-Yt)*Y_renorm)
     +                            **2 +
     +                            (Yxy_table(2,kk,jj,ii)-x)**2 +
     +                            (Yxy_table(3,kk,jj,ii)-y)**2) 
            enddo
              
c           set up 3 solutions, for icolor=1(red), icolor=2(green)
c           icolor=3(blue)
            do i=1,nfits
              kk=look(1,i)
              jj=look(2,i)
              ii=look(3,i)
              c(i,1)=Yxy_table(2,kk,jj,ii)    ! x
              c(i,2)=Yxy_table(3,kk,jj,ii)    ! y
              c(i,3)=Yxy_table(1,kk,jj,ii)*Y_renorm    ! Y
              if(nterms.gt.4)then
                c(i,4)=Yxy_table(2,kk,jj,ii)*Yxy_table(3,kk,jj,ii)  !xy
                c(i,5)=Yxy_table(2,kk,jj,ii)*Yxy_table(1,kk,jj,ii)  !xY
     +                *Y_renorm
                c(i,6)=Yxy_table(3,kk,jj,ii)*Yxy_table(1,kk,jj,ii)  !yY
     +                *Y_renorm
                c(i,7)=Yxy_table(2,kk,jj,ii)*Yxy_table(2,kk,jj,ii)  !xx
                c(i,8)=Yxy_table(3,kk,jj,ii)*Yxy_table(3,kk,jj,ii)  !yy
                c(i,9)=Yxy_table(1,kk,jj,ii)*Yxy_table(1,kk,jj,ii)  !YY
     +                *Y_renorm*Y_renorm
              endif
              c(i,nterms)=1.0
            enddo

            do icolor=1,3
              do i=1,nfits
                cl(i)=dn_steps(look(icolor,i))
              enddo
              call dlsqp(nfits,nterms,c,cl,coef(1,icolor),resid,err,
     +                   err_soln,wts,ind)
              if(ind.eq.1)then
                call xvmessage
     +                 ('DLSQP: singular in interpolation table',' ')
                write(msg,*)l,m,n,x,y,Yt
                call xvmessage(msg,' ')
              else
                err_muw(icolor)=err_muw(icolor) + err
                err_rgb(icolor)=err_rgb(icolor) + dabs(resid(1))
              endif
            enddo

c           use the solution transformation at this grid to compute the
c           desired dn's which reflect the Yxy coord for this device.
c           dn_vs_Yxy(k,l,m,n) has structure:
c           k=1 reddn, k=2 greendn, k=3 bluedn
c           l=x index,  m=y index  n=Y index
            do k=1,3   
              dn_vs_Yxy(k,l,m,n)=coef(1,k)*x + 
     +                           coef(2,k)*y +
     +                           coef(3,k)*Yt*Y_renorm +
     +                           coef(nterms,k)
              if(nterms.gt.4)then
                 dn_vs_Yxy(k,l,m,n)=dn_vs_Yxy(k,l,m,n) +
     +                           coef(4,k)*x*y +
     +                           coef(5,k)*x*Yt*Y_renorm +
     +                           coef(6,k)*y*Yt*Y_renorm +
     +                           coef(7,k)*x*x +
     +                           coef(8,k)*y*y +
     +                           coef(9,k)*Yt*Yt*Y_renorm*Y_renorm
              endif
            enddo

          enddo  ! Y
        enddo    ! y
      enddo      ! x            

      do i=1,3
        err_muw(i)=err_muw(i)/dble(ntable)**3
        err_rgb(i)=err_rgb(i)/dble(ntable)**3
      enddo

      call xvmessage('Error of the mean unit weight (in dn):',' ')
      call xvmessage('      red       green    blue',' ')
      write(msg,102)err_muw(1),err_muw(2),err_muw(3)
102   format(3f10.4)
      call xvmessage(msg,' ')
      call xvmessage('Error of the nearest point (in dn):',' ')
      call xvmessage('      red       green    blue',' ')
      write(msg,102)err_rgb(1),err_rgb(2),err_rgb(3)
      call xvmessage(msg,' ')

      call
     *    xvmessage('Computation for interpolation table complete.',' ')
      call xvmessage(' ',' ')


c As a check process all the Yxy data in buffer Yxy_table to see
c how well one can retrieve the RGB dn values.
      err_rgb(1)=0.0
      err_rgb(2)=0.0
      err_rgb(3)=0.0
      do i=1,ngrid      ! blue
        do j=1,ngrid    ! green
          do k=1,ngrid  ! red
            tristim_Y=Yxy_table(1,k,j,i)
            x=Yxy_table(2,k,j,i)
            y=Yxy_table(3,k,j,i)
            reall=x * x_slope + x_offset
            realm=y * y_slope + y_offset
            realn=tristim_Y * Yt_slope + Yt_offset
            l=reall
            m=realm
            n=realn
            xr=reall-l
            yr=realm-m
            zr=realn-n
            do kk=1,3
              dn_bot=(xr * dn_vs_Yxy(kk,l+1,m,n) +
     +              (1.0-xr) * dn_vs_Yxy(kk,l,m,n)) * (1.0-yr) +
     +             (xr * dn_vs_Yxy(kk,l+1,m+1,n) +
     +              (1.0-xr) * dn_vs_Yxy(kk,l,m+1,n)) * yr
              dn_top=(xr * dn_vs_Yxy(kk,l+1,m,n+1) +
     +              (1.0-xr) * dn_vs_Yxy(kk,l,m,n+1)) * (1.0-yr) +
     +             (xr * dn_vs_Yxy(kk,l+1,m+1,n+1) +
     +              (1.0-xr) * dn_vs_Yxy(kk,l,m+1,n+1)) * yr
              dn(kk)=dn_bot * (1.0-zr) + dn_top * zr
            enddo
            err_rgb(1)=err_rgb(1)+abs(dn(1)-dn_steps(k))
            err_rgb(2)=err_rgb(2)+abs(dn(2)-dn_steps(j))
            err_rgb(3)=err_rgb(3)+abs(dn(3)-dn_steps(i))
          enddo
        enddo
      enddo
      err_rgb(1)=err_rgb(1)/ngrid**3      
      err_rgb(2)=err_rgb(2)/ngrid**3      
      err_rgb(3)=err_rgb(3)/ngrid**3      
      call xvmessage
     *     ('Mean error of the Yxy to DNs in the table (in dn):',' ')
      call xvmessage('      red       green    blue',' ')
      write(msg,102)err_rgb(1),err_rgb(2),err_rgb(3)
      call xvmessage(msg,' ')
      call xvmessage(' ',' ')

c Copy dn_vs_Yxy table to output file: TRUCOLOR.TV or TRUCOLOR.FILM
      if(device.eq.'TV')then
        call xvmessage
     *          ('Writing interpolation table file: TRUCOLOR.TV',' ')
        call xvunit(tunit,'NEW',1,status,'U_NAME','TRUCOLOR.TV',' ')
      else
        call xvmessage
     *         ('Writing interpolation table file: TRUCOLOR.FILM',' ')
        call xvunit(tunit,'NEW',1,status,'U_NAME','TRUCOLOR.FILM',' ')
      endif
      call xvopen(tunit,status,'U_NL',ntable*ntable,'U_NS',ntable*3,
     +            'OP','WRITE','METHOD','SEQ',
     +            'O_FORMAT','REAL','U_FORMAT','REAL',' ')
      do i=1,ntable
        do j=1,ntable
          call xvwrit(tunit,dn_vs_Yxy(1,1,j,i),status,' ')
        enddo
      enddo
      call xvmessage('Write complete',' ')

400   continue

c copy existing Yxy to DN table from disk into buffer dn_vs_Yxy
      if(file_exists)then
        if(device.eq.'TV')then
          call xvmessage
     *           ('Reading interpolation table file: TRUCOLOR.TV',' ')
          call xvunit(tunit,'NEW',1,status,'U_NAME','TRUCOLOR.TV',' ')
        else
          call xvmessage
     *           ('Reading interpolation table file: TRUCOLOR.FILM',' ')
          call xvunit(tunit,'NEW',1,status,'U_NAME','TRUCOLOR.FILM',' ')
        endif
        call xvopen(tunit,status,'OP','READ','METHOD','SEQ',' ')
        do i=1,ntable
          do j=1,ntable
            call xvread(tunit,dn_vs_Yxy(1,1,j,i),status,' ')
          enddo
        enddo
        call xvmessage('Read complete',' ')
      endif        


c open outputs
      do i=1,3
        call xvunit(ounit(i),'OUT',i,status,' ')
        call xvopen(ounit(i),status,'O_FORMAT','BYTE','U_FORMAT','HALF',
     +              'U_NL',nl,'U_NS',ns,'OP','WRITE',' ')
      enddo

      if(nout.eq.4)then
        call xvunit(xyunit,'OUT',4,status,' ')
        call xvopen(xyunit,status,'O_FORMAT','BYTE','U_FORMAT','HALF',
     +              'U_NL',mapxy,'U_NS',mapxy,'OP','WRITE',' ')
        xy_slope=mapxy/1.2
        xy_offset=mapxy- xy_slope*1.1
        call mve(2,mapxy*mapxy,100,xybuf,0,1)  ! set buffer to 100
        k=nint(xy_offset)
        l=mapxy- nint(xy_offset) +1
        do i=1,mapxy
          xybuf(k,i)=0 ! draw y axis
          xybuf(i,l)=0 ! draw x axis
          xybuf(i,i)=0 ! draw diagonal
        enddo
      endif

c process the image**************************************************

      jbounds=0
      ibounds=0
      kbounds=0
      lbounds=0
      do j=1,nl                         ! line loop
        do k=1,nin                      ! picture loop
          call xvread(unit(k),buf(1,k),status,'LINE',j,' ')
        enddo
        do i=1,ns                       ! pixel loop

c         Compute tristimulus values from coefficients.
          tristim_X=0.
          tristim_Y=0.
          tristim_Z=0.
          do n=1,nin                    ! picture loop
            tristim_X = tristim_X + solution(n,1)*buf(i,n)*units(n)
            tristim_Y = tristim_Y + solution(n,2)*buf(i,n)*units(n)
            tristim_Z = tristim_Z + solution(n,3)*buf(i,n)*units(n)
          enddo
          
c         Convert to Y,x,y
          sum=tristim_X + tristim_Y + tristim_Z
          if(sum.ne.0.0)then
            x=tristim_X/sum
            y=tristim_Y/sum
          else
            x=0.0
            y=0.0
          endif

c         Substitute macbeth table values for first 24 pixels
c         for internal test of algorithm.
          if(use_macbeth)then
            if(i.le.24.and.j.eq.1)then
               tristim_Y=macbeth(1,i)
                       x=macbeth(2,i)
                       y=macbeth(3,i)
            endif
          endif

c         update the 4th output image if desired
          if(nout.eq.4)then
            l=mapxy- nint(y*xy_slope+xy_offset) +1 ! line
            m=nint(x*xy_slope+xy_offset)           ! sample
            if(l.lt.1) l=1
            if(m.lt.1) m=1
            if(l.gt.mapxy) l=mapxy
            if(m.gt.mapxy) m=mapxy
            if(xybuf(m,l).lt.32767) xybuf(m,l)=xybuf(m,l)+1
          endif

c         check for negative luminance
          if(tristim_Y.lt.0.0)then
            lbounds=lbounds+1
            buf(i,1)=0
            buf(i,2)=0
            buf(i,3)=0
            goto 300
          endif

          tristim_Y=sqrt(tristim_Y)

c         scale Y to fill display device dynamic range.
          tristim_Y = tristim_Y * smal_ratio

c         check for luminances below minimum possible
          if(tristim_Y.lt.Yt_min)then
            lbounds=lbounds+1
            buf(i,1)=0
            buf(i,2)=0
            buf(i,3)=0
            goto 300
          endif

c         write zero's if color is clearly unreproducible.
          point(1)=x
          point(2)=y
          if(.not.inside(point,triangle,npolygon))then
            ibounds=ibounds+1
            buf(i,1)=0
            buf(i,2)=0
            buf(i,3)=0
            goto 300
          endif

c         Compute indices closest to but not above the Yxy value
          reall=x * x_slope + x_offset
          realm=y * y_slope + y_offset
          realn=tristim_Y * Yt_slope + Yt_offset
          l=reall
          m=realm
          n=realn
c         Permit saturation in intensity
          if(n.gt.ntable-1) n=ntable-1

c         Write zero's if outside interpolation table
          if((l.lt.1) .or. (l.gt.ntable-1) .or. (m.lt.1) .or.
     +       (m.gt.ntable-1) .or. (n.lt.1)) then
            jbounds=jbounds+1
            buf(i,1)=0
            buf(i,2)=0
            buf(i,3)=0
            goto 300
          endif

c         interpolate to get the DN value
          xr=reall-l
          yr=realm-m
          zr=realn-n
          do k=1,3
            dn_bot=(xr * dn_vs_Yxy(k,l+1,m,n) +
     +              (1.0-xr) * dn_vs_Yxy(k,l,m,n)) * (1.0-yr) +
     +             (xr * dn_vs_Yxy(k,l+1,m+1,n) +
     +              (1.0-xr) * dn_vs_Yxy(k,l,m+1,n)) * yr
            dn_top=(xr * dn_vs_Yxy(k,l+1,m,n+1) +
     +              (1.0-xr) * dn_vs_Yxy(k,l,m,n+1)) * (1.0-yr) +
     +             (xr * dn_vs_Yxy(k,l+1,m+1,n+1) +
     +              (1.0-xr) * dn_vs_Yxy(k,l,m+1,n+1)) * yr
            dn(k)=dn_bot * (1.0-zr) + dn_top * zr
          enddo

c         check for overflow
          ind=0
          i_red=nint(dn(1))
          if(i_red.lt.0) ind=1
          if(i_red.gt.maxdn) i_red = maxdn
          i_green=nint(dn(2))
          if(i_green.lt.0) ind=1
          if(i_green.gt.maxdn) i_green = maxdn
          i_blue=nint(dn(3))
          if(i_blue.lt.0) ind=1
          if(i_blue.gt.maxdn) i_blue = maxdn
          if(ind.eq.1)then
            kbounds=kbounds+1
            buf(i,1)=0
            buf(i,2)=0
            buf(i,3)=0
            goto 300
          else
            buf(i,1)=i_red
            buf(i,2)=i_green
            buf(i,3)=i_blue
          endif

c         print out in macbeth test only
          if(use_macbeth)then
             if(i.eq.1.and.j.eq.1)then
                call xvmessage('patch, Macbeth:Yxy, DN:rgb',' ')
             endif
             if(i.le.24.and.j.eq.1)then
                write(msg,1001)i,macbeth(1,i),macbeth(2,i),macbeth(3,i),
     +                       i_red,i_green,i_blue
1001            format(1x,i4,3f7.3,3i6)
                call xvmessage(msg,' ')
             endif
          endif

300       continue
        enddo   ! pixel loop      

        do k=1,3             ! picture loop
          call xvwrit(ounit(k),buf(1,k),status,' ')
        enddo

      enddo     ! line loop

      call xvmessage(' ',' ')
      call prnt(4,1,ibounds,'# points outside device chromaticity=.')
      call prnt(4,1,jbounds,'# points outside interpolation table=.')
      call prnt(4,1,kbounds,'# DN values negative or saturated=.')
      call prnt(4,1,lbounds,'# of luminances below minimum=.')

c stretch and write out the 4th output image
      if(nout.eq.4)then
c       stretch xybuf
        maxhist=100
        do l=2,mapxy-1
          do m=2,mapxy-1
            if(xybuf(m,l).gt.maxhist) maxhist=xybuf(m,l)
          enddo
        enddo
        if(maxhist.gt.100)then
          xy_sl=155./(maxhist-100)
          do l=1,mapxy
            do m=1,mapxy
              if(xybuf(m,l).gt.100)then
                k=(xybuf(m,l)-100)*xy_sl +100.5
                if(k.gt.255) k=255
                xybuf(m,l)=k
              endif
            enddo
          enddo
        endif                    

c       Place all the (x,y) pairs in the calibration table in the image
        do i=1,ngrid      ! blue
          do j=1,ngrid    ! green
            do k=1,ngrid  ! red
              x=Yxy_table(2,k,j,i)
              y=Yxy_table(3,k,j,i)
              l=mapxy- nint(y*xy_slope+xy_offset) +1 ! line
              m=nint(x*xy_slope+xy_offset)           ! sample
              if(l.gt.0.and.l.le.mapxy.and.m.gt.0.and.m.le.mapxy)then
                xybuf(m,l)=0
              endif
            enddo
          enddo
        enddo
        do l=1,mapxy             ! picture loop
          call xvwrit(xyunit,xybuf(1,l),status,' ')
        enddo
      endif

      end

c*************************************************************
      SUBROUTINE NEWSIMQ(A,B,N,ipass,IFAIL)

C Obtains the solution of a set of simultaneous linear equations using
C    MATH77 (from LINPACK) routines SGEFA AND SGESLD.

      REAL A(N,N), B(N), zz(100)
      INTEGER N, IFAIL
C  WORK ARRAYS
      INTEGER IPVT(100)

      IF (N .GT. 100) THEN
	call xvmessage('SIMQ: matrix is too big',' ')
	ifail=1
        return
      END IF

      IFAIL = 0

C..GET THE LU DECOMPOSITION OF MATRIX A.

      if(ipass.eq.1)then
        CALL SGECO( A,N,N, IPVT, RINFO , ZZ)
        IF ( RINFO .eq. 0. )  THEN
           call xvmessage('SIMQ:MATRIX is singular',' ')
           ifail=1
           return
        endif
        IF ( RINFO .lt. 1.0e-6 )  THEN
           call xvmessage('SIMQ:MATRIX is ill conditioned',' ')
           call prnt(7,1,rinfo,' Reciprocal condition # =.')
           ifail=1
           return
        endif
      endif

C..IF NOT SINGULAR, THEN SOLVE FOR RIGHT HAND SIDE

      CALL SGESLD( A,N,N, IPVT, B)

      RETURN
      END

c**************************************************************
      SUBROUTINE DLSQP(NE,NU,C,CL,X1,V,E,EX,wts,ind)
C
C1    GENERAL LEAST SQUARES SOLUTION OF NE EQUATIONS WITH NU UNKNOWNS,
C     C(I,1)*X1(1)+C(I,2)*X1(2)+...+C(I,NU)=CL(I) OF EQUAL WEIGHTS,WITH
C     I RANGING FROM 1 TO NE.
C
C2    THE INFORMATION FROM THE MAIN PROGRAM IS:
C          C(I,J) = COEFFICIENT MATRIX
C          CL(I) = ARRAY OF FREE TERMS
C          NE = NUMBER OF EQUATIONS
C          NU=NUMBER OF UNKNOWNS
c          wts(i)=weights
C
C3    THE INFORMATION RETURNED TO THE MAIN PROGRAM IS:
C          X1(J) = COMPUTED VALUES OF THE UNKNOWNS
C          V(I) = RESIDUALS  (I.E. OBSERVED MINUS COMPUTED)
C          E = MEAN ERROR OF THE UNIT WEIGHT
C          EX(J) = MEAN ERRORS OF THE UNKNOWNS
c          ind=0 if OK  =1 if division by zero
C
C5    ALL THE STATEMENTS BELOW ARE VALID FOR ANY NU LARGER THAN 1 AND
C     ANY NE LARGER THAN NU.
C
      integer s,ss,ind
c      parameter (s=8,ss=20)
      parameter (s=10,ss=20)
      REAL*8  A(s,s),AL(s),R(s,s),RL(s),Q(s,s),X(s),SL,SQ,P,SUM
      REAL*8 C(ss,s),CL(ss),X1(s),V(ss),EX(s),wts(ss),E
C
      ind=0
      DO 57 J = 1,NU
      DO 57 I=1,NU
      A(I,J)=0.
      R(I,J)=0.
57    Q(I,J)=0.
      DO 100 I=1,NU
      DO 100 J=1,NU
      DO 100 K=1,NE
100   A(I,J)=A(I,J)+C(K,I)*C(K,J)*wts(k)
      DO 102 I=1,NU
      AL(I)=0.
      DO 102 K=1,NE
102   AL(I)=AL(I)+C(K,I)*CL(K)*wts(k)
      NUM=NU-1
      NUP=NU+1
      DO 110 I=1,NUM
      K=I+1
      DO 110 J=K,NU
      if(a(i,i).ne.0.d0)then
        R(I,J)=A(I,J)/A(I,I)
      else
        ind=1
        return
      endif
      DO 110 L=1,I
110   A(K,J)=A(K,J)-R(L,K)*A(L,J)
      if(a(1,1).ne.0.d0)then
        RL(1)=AL(1)/A(1,1)
      else
        ind=1
        return
      endif
      DO 125 I=2,NU
      DO 122 J=1,I
122   AL(I)=AL(I)-R(J,I)*AL(J)
      if(a(i,i).ne.0.d0)then
        RL(I)=AL(I)/A(I,I)
      else
        ind=1
        return
      endif
125   continue
      X(NU)=RL(NU)
      DO 131 I=1,NUM
      IX=NU-I
      IXI=IX+1
      SUM=0.
      DO 130 J=IXI,NU
130   SUM=SUM-R(IX,J)* X(J)
131    X(IX)=RL(IX)+SUM
      DO 200 J=1,NU
200   X1(J)=X(J)
      if(a(nu,nu).ne.0.d0)then
        Q(NU,NU)=1./A(NU,NU)
      else
        ind=1
        return
      endif
      DO 150 I=1,NUM
      NP=NUP-1
      DO 135 J=I,NUM
      NM=NU-J
      JP=NM+1
      P=0.
      DO 135 K=JP,NU
      P=P-R(NM,K)*Q(NP,K)
      Q(NP,NM)=P
135   Q(NM,NP)=P
      NPM=NP-1
      SQ=0.
      DO 145 L=NP,NU
145   SQ=SQ-R(NPM,L)*Q(L,NPM)
      if(a(npm,npm).ne.0.d0)then
        Q(NPM,NPM)=1./A(NPM,NPM)+SQ
      else
        ind=1
        return
      endif
150   continue
      DO 151 I=1,NE
      V(I)=0.
      DO 151 J=1,NU
151   V(I)=V(I)+C(I,J)* X(J)
      SL=0.
      DO 153 I=1,NE
      V(I)=CL(I)-V(I)
153   SL=SL+V(I)*V(I)
      FNE=NE
      FNU=NU
      if(fne.ne.fnu)then
        E=DSQRT(SL/(FNE-FNU))
      else
        ind=1
        return
      endif
      DO 160 I=1,NU
        IF ( Q(I,I) .GE. 0.D0 ) THEN
          EX(I)=E*DSQRT(Q(I,I))
        ELSE
          EX(I)= 0.0                ! HANDLE NEGATIVES DUE TO ROUNDOFF.
        END IF
160   CONTINUE      
      RETURN
      END


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create trucolor.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM trucolor

   To Create the build file give the command:

		$ vimake trucolor			(VMS)
   or
		% vimake trucolor			(Unix)


************************************************************************/


#define PROGRAM	trucolor
#define R2LIB

#define MODULE_LIST trucolor.f

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create trucolor.pdf
process help=*

parm INP	count = 3:10
parm OUT	count = 3:4
parm COLORS	type=integer +
                count = 0:10 +			! special colors
                default = --
parm CONV	type = real +
  		count = 0:10 +
  		default = --
parm IOVF	type = real +
  		count = 0:10 +
  		default = --
parm FILTER	type = integer +
  		count = 0:10 +
  		default = --
parm XYZ	type = real +
  		count = 0:30 +
  		default = --
parm RESPONSE	type = real +
  		count = 0:100 +
  		default = --
parm DEVICE	type = keyword +		! output device
  		valid = ("TV","FILM") +
  		default = "TV"
parm INC	type = integer +		! line/sample increment
  		count = 0:1 +
  		default = 10			
parm NOAUTO       type=keyword +
                valid=("AUTO","NOAUTO") + 
                default="AUTO"
parm NTERMS     type=integer +
                valid=(4,10) +
                count=(0:1) +
                default=4
parm NFITS      type=integer +
                valid=(5:20) +
                count=(0:1) +
                default=7
parm ILLUMIN    type=real +
                count=(0:1) +
                default=1.0
parm MACBETH    type=keyword +
                valid=("MACBETH","NOMACBET") +
                default="NOMACBET"
end-proc

.title
VICAR2 program "trucolor"

.help
PURPOSE
"trucolor" transforms
images taken through several filters into a color image in which an
attempt is made to reproduce accurately certain designated spectra ("special
colors").  At this time, the only output devices supported by this program are
the CONRAC 7211 color monitor, and the MDA Color Fire film recorder. 


EXECUTION
  The program is executed by specifying up to 10 images
taken through different filters, up to 10 special
colors, and, three output files (red, green, and blue), 
a conversion factor for each input image, and an image display device.

  The input images should be 
reflectance images if the specified DEVICE is a film recorder, and radiance 
images if the specified DEVICE is a color monitor. 

  When the program begins it will look in your local directory for
a file called "trucolor".TV or "trucolor".FILM . If it finds this file
it will read it to acquire the Yxy to DN interpolation table. If it
cannot find it it will recompute the table (taking several cpu minutes)
and then write it out for another execution of "trucolor" to find.
The file is about the size of a flight image.

  Also when the program begins it will look in your local directory
for a file called CUBE.TV or CUBE.FILM. If it finds such a file it will
read it and use the information to replace an internal table which
tabulates the Yxy values for 125 combinations of DN values, thus
modelling the device in question (either the tv or film recorder).
All values are real format in ascii.
If you create such a file it is ordered as 125 records with 3 real
values per record in the order Y (tristimulus value) x (chromaticity)
and y (chromaticity). 
The order of the sets of 3 points is:
dnred=0,64,128,192,255  for green=0  blue=0
         "                        64      0  
         "                        128     0  
         "                        192     0  
         "                        255     0  
then repeat the above for blue=64
then repeat the above for blue=128
then repeat the above for blue=192
then repeat the above for blue=255

  If you provide four output files the last output will contain
a chromaticity map histogram of all the data points in the inputs.

OUTPUT FILES:
All outputs are in BYTE format.

  If, for any reason, the output dn value cannot be computed it will be
set to red=green=blue=0(zero). This is true for ANY saturation in ANY
part of a red,green,blue value pixel, for Yxy values outside the
device range, or negative Y.

  The fourth output (optional) contains a chromaticity 2-d histogram.
The file is 256 by 256 byte format on a 100 dn background. 
The x chromaticity axis is in the sample direction.
The y chromaticity axis is in the line direction.
Each coordinate value for the display device mapping dn values into
Yxy values is recorded as ZERO dn or black. There are 125 points which
are black corresponding to the 125 values in the lookup table.
Each input dn value in the input pictures
is converted to Yxy and the (x,y) portion is accumulated above the
100 dn gray level as a 2-d histogram. This histogram will reveal the
clustering of colors in the images. Values outside the triangle formed
by the 125 black points will be set to zero dn in the output images
since they lie outside the available color range of the recording
device.

EXAMPLE
In the following examples the user creates output files, OUT.*, using input
images INP.*, and special colors, 11/7, 7/6, and 5/7.
The resulting images will be displayed on a
Conrac 7211 monitor. In this mode (radiance) Galsos must produce images
in radiance units.

  VICAR>trucolor INP=(INP.ORA,INP.CLR,INP.GRE) OUT=(OUT.RED,OUT.GRE,OUT.BLU) +
  VICAR>+ COLORS=(5,4,3) 'TV
 
WRITTEN BY: 		J Lorre 1/20/92

Made portable for UNIX  A Scop (CRI) 1/2/95

INTERNAL TABLES:
  You will note in the program two tables labelled:
Yxy_table  (for the film recorder)
Yxy_tv     (for the TV monitors)
These tables contain the Yxy values at each 64 dn levels for all
combinations of input dn values for blue,green, and red. You can
recreate these tables by measuring the Yxy values from the
photographic paper or from the TV monitor. The following .pdf
files were created to assist in this process.

TV monitor procedure:

procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
local (red,green,blue) integer
local (epa,return) string
write "specify display device: example EPA2"
getpar epa 
use &epa
vids
jcolor
jerase
for red=0,64,128,192,255
  gen &"red".img nl=200 ns=200 ival=&red linc=0 sinc=0
end-for
write "Center the probe on this patch"
jdisp (255.img,255.img,255.img) loc=(200,200)
getpar return
write "Now we cycle through all 125 combinations"
for blue=0,64,128,192,255
  for green=0,64,128,192,255
    for red=0,64,128,192,255
      write "red_dn=&red green_dn=&green blue_dn=&blue"
      jdisp (&"red".img,&"green".img,&"blue".img) loc=(200,200)
      getpar return
    end-for
  end-for
end-for
for red=0,64,128,192,255
  dcl del &"red".img;1
end-for
jstop
free &epa
end-proc

The above procedure displays patches at the center of a 512 TV monitor
in the order input to the tables. You may tape the Minolta probe to
the TV tube and write the Yxy values down as they come up.

Film procedure:

procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
! creates the FILM image to calibrate the film recorder
gen out=0.img nl=500 ns=100 ival=0 linc=0 sinc=0
gen out=1.img nl=500 ns=100 ival=64 linc=0 sinc=0
gen out=2.img nl=500 ns=100 ival=128 linc=0 sinc=0
gen out=3.img nl=500 ns=100 ival=192 linc=0 sinc=0
gen out=4.img nl=500 ns=100 ival=255 linc=0 sinc=0
fastmos inp=(0.img,1.img,2.img,3.img,4.img) out=r.img +
  nl=500 ns=500 +
  off1=(1,1) off2=(1,101) off3=(1,201) off4=(1,301) off5=(1,401)
fastmos inp=(r.img,r.img,r.img,r.img,r.img) out=v2$scratch:red.img +
  nl=1500 ns=1000 +
  off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) off5=(1001,1)
flot inp=r.img out=g.img 'clock
fastmos inp=(g.img,g.img,g.img,g.img,g.img) out=v2$scratch:green.img +
  nl=1500 ns=1000 +
  off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) off5=(1001,1)
gen out=0.img nl=500 ns=500 ival=0 linc=0 sinc=0
gen out=1.img nl=500 ns=500 ival=64 linc=0 sinc=0
gen out=2.img nl=500 ns=500 ival=128 linc=0 sinc=0
gen out=3.img nl=500 ns=500 ival=192 linc=0 sinc=0
gen out=4.img nl=500 ns=500 ival=255 linc=0 sinc=0
fastmos inp=(0.img,1.img,2.img,3.img,4.img) out=v2$scratch:blue.img +
  nl=1500 ns=1000 +
  off1=(1,1) off2=(1,501) off3=(501,1) off4=(501,501) off5=(1001,1)
barne inp=("v2$scratch:red.img,v2$scratch:green.img, +
  v2$scratch:blue.img") miplbox=21 primary=42057
dcl del %.img;1
!
end-proc

The above procedure creates a single print for the Minolta chromaticity
meter held by Dave Deats at the photo lab. The print is ordered as 5
squares like this:

*******************
*        *        *
*   0    *   64   *
*        *        *
*******************
*        *        *
*  128   *  192   *
*        *        *
*******************
*        *
*  255   *
*        *
**********

where the numbers are the BLUE dn values.
Each square has 25 patches arranged as a 5 by 5 grid with red increasing
from left to right 0 64 128 192 255, and green increasing from top down
0 64 128 192 255.


THEORY:
                          Color Calibration

   This document describes the process of color calibration for the
purpose of producing color products which look the same to the eye
as the original. 

   We first address the camera signal (dn) and the computation of U 
and I/F which are the decalibrated camera response. The response (dn) 
of a ccd detector is:

    t*O*a
dn= ----- integral(S*T*F*U*P*dl) + dn0                        (1)
     g*c
 
where:
dn=camera output number
g =gain state ratio factor
t =exposure time (sec)
c =high gain conversion factor (e/pixel/dn)
O =optics solid angle (sr)
a =area of ccd pixel (cm**2/pixel)
S =ccd spectral sensitivity (e/photon)
T =optics spectral transmission
F =filter spectral transmission
U =spectral scene radiance (watts/cm**2/sr/nm)
P =wavelength (nm) /1.9862e-16 (photons/watt-sec)
l =wavelength (nm)
dn0=dark current output number

   We wish to decalibrate this sensor before using the response (dn).
To do this we must extract the radiance U from the integral and solve
for it. The only way to extract U is to assume it is independent of 
wavelength. This is a serious error but one which must be made if
only one filter position is available. Typically we have three or more
filters if we are to perform color reconstruction, but for now we will
ignore this problem.

   Solving (1) for U we have:

      (dn-dn0)*g*c
U=-------------------------         watts/cm**2/sr/nm         (2)
  t*O*a*integral(S*T*F*P*dl)

or, in terms of reflectance I/F we have:

      (dn-dn0)*g*c*pi
I/F=-----------------------------     dimensionless albedo     (3)
     t*O*a*integral(S*T*F*H*P*dl)

where H is the solar irradiance incident on the surface.

To simplify things we denote from now on in the text both U
and I/F as symbol Q. In general U is used to produce color
for an additive device like a color TV monitor and I/F is used
for a subtractive device like a film recorder. 

We adopt a set of i special colors with known spectra C(i).
These spectra represent colors which we intend the camera to
reproduce with some precision. For example Kodak uses sky blue,
white cloud, green grass, and skin color as it's four special
colors for film process control. We could use Jupiter 
characteristic spectra.
The tristimulus values of these colors are computed from:

X(i)=integral(C(i)*x*dl)
Y(i)=integral(C(i)*y*dl)     1<=i<=I                           (4)
Z(i)=integral(C(i)*z*dl)

where: x,y,z are the standard CIE color matching functions.

We expose the camera to each of the colors to record the Q(ij) 
responses given by equation(2 or 3) in each of J filter positions.

Because the form of equations (1) and (4) are similar and linear it
is possible to represent the tristimulus values as a linear
combination of the camera responses Q(ij) to each special color i
in filter position j:

X(i)=A1*Q(i1)+A2*Q(i2)+A3*Q(i3)...AJ*Q(iJ)    
Y(i)=B1*Q(i1)+B2*Q(i2)+B3*Q(i3)...BJ*Q(iJ)   1<=i<=I        (5)
Z(i)=C1*Q(i1)+C2*Q(i2)+C3*Q(i3)...CJ*Q(iJ)    

There are as many X equations as there are special colors and as many
A terms as filters. We can solve for the A,B,C coefficients from each
group of the above equations. For input Q(ij) values taken through the
same filters used to solve equations (5) we can compute the tristimulus
values X,Y,Z. These are the values the eye should see.

To calibrate the display device we create a numerical step target
with discrete combinations of red,green,blue spanning the range of
numeric values. The target is displayed on the TV monitors and on
a print. For each discreet step we compute the tristimulus values:

for TV
X=integral(U*x*dl)
Y=integral(U*y*dl)                                   (6)
Z=integral(U*z*dl)

for prints
X=integral(R*P*x*dl)
Y=integral(R*P*y*dl)                                 (7)
Z=integral(R*P*z*dl)

where:
U =the spectral radiance of the TV monitor.
R =the spectral reflectance of the print.
P =a standard illuminant source like D65.

We know by now the dn(blue,green,red) versus the X,Y,Z tabular
relationship for the output device. To facilitate interpolation it is
necessary to create a regularly spaced interpolation table which
returns output blue,green,red dn's from a desired input X,Y,Z.

To create a color product from a set of J input filters we do:

First:
1. Select a set of I representative special colors for which
   camera responses are known (from equations 2 or 3) and
   tristimulus values are known (from equation 4).
2. Solve equation (5) for the 3*J coefficients A(j),B(j),C(j).

Then, for each pixel:
3. From equation (5) compute the X,Y,Z tristimulus values.
4. Interpolate in the 3-d table of X,Y,Z values to obtain the
   output red,green,blue dn values.

   Scaling:

   Scaling of the results may be required in order to produce
unsaturated images. It will be evident from the interpolation table 
which gives the output dn versus tristimulus values what the
tristimulus value limit is (corresponding to dn's of 255). 
Simply scaling the tristimulus values such that the luminance
Y does not exceed a limit will not be satisfactory. This is 
because the maximum Y is a function of the chromaticity coordinates
(x,y). To perform scaling we must:

1. Convert the XYZ values to Yxy using the relation
      x=X/(X+Y+Z) and y=Y/(X+Y+Z)
   We perform this BOTH for input data and for the interpolation
   table.
   We retain Y as the luminance since it is defined as such
   if we use the CIE 1931 color matching functions.

2. For each point xy in the data search the interpolation
   table for the largest possible Y value. Call it Ymax.
   Compute the smallest ratio of Ymax/Y. Call it Sc.

3. When processing images convert XYZ to Yxy and impose
   scaling by multiplying each Y by Sc.

Note that I/F reflectance may not require scaling.

   Discussion

   We must present an argument for the validity of using U or I/F
since we have approximated these functions as independent of
wavelength. Equation (5) is justified because it is a linear
representation of responses which are themselves linear integrals.
Any approximation which is linear such as substituting a flat
function for the scene radiance can be compensated for by the
three sets of least squares fit coefficients A,B, and C.
The important criterion is that I/F be a measurable quantity.
Since dn is proportional with exposure and I/F is proportional
with dn then I/F is measurable and correlatable to X,Y, and Z.
Precisely the same method is used in determining chemical
abundances from spectra such as in NIMS. Equation (5) locks
the camera response to XYZ for the colors of interest.
   One way to get into trouble with equation (5) is to include
filter positions which fall partially out of the spectral
band of the color matching functions x,y,z. Any IR filter would
cause this problem. Another way is to include filters which 
overlap so much spectrally that the images they produce are
nearly identical. Both these cases can be mitigated to some
extent by using the first three terms in the principal 
component transformation of the input images. We must use the
same rotation matrix obtained from the special colors to rotate
the images of unknown scenes. 
   Equation (5) provides a least squares fit between 
the U or I/F values and tristimulus values, both obtained from
special color targets. Later on these same A,B,C coefficients
will be used to convert from U or I/F to tristimulus values for
each pixel in the image. It is imperative that identical
means be used to compute U or I/F (Q) in both cases. Thus the
cameras must be exposed to each of the special colors rather than
U or I/F being computed theoretically. The point is that both U
and I/F are wrong ! They are wrong because of the limitations imposed
by broad band radiometry. To get consistent results we must make the
same error at both the calibration and the evaluation ends.
   One can compute the I/F values theoretically for the special
colors instead of exposing the camera to them. As discussed this
is not correct but, for the sake of completeness, we present
the formula:

      integral(S*T*F*U*dl)
I/F =-------------------------
      integral(S*T*F*H*dl)

where H= The incident spectral irradiance to a surface.
      U= The reflected spectral radiance from the surface.
      U= H*R/pi, where R= The spectral reflectance of the surface.

   The color cube approach to calibrating the film recorder or the
TV monitor rather than modelling these devices as logarithmic or
exponential is, I think, necessary. An interpolation table can 
model any device AND can tell if the requested tristimulus values are
beyond those accessible for the device. Film recorders+film+paper
are highly nonlinear creations with color crosstalk between channels.

   Alternative color method.

   There is an alternative method to color reconstruction.
This is to estimate the true spectral distribution of U or
I/F from a synthesis of the U or I/F values obtained through a
set of different camera filters (the very case we have). In this
scenario we attempt to guess the spectral radiance U which 
solves equation (1) for the J dn values AT EACH PIXEL.
Then we can integrate this function using equation (6) to
achieve the tristimulus values at each pixel directly
without using equation (5). This has been done by
Steve Wall JGR,Vol 8,No 28,pp 4401-4411. In this case special
colors are used more as a means of refining the method of
estimating U (since it is known) and of verifying results.

   Required calibration data:

1. Special colors targets imaged by the camera.
   This includes knowledge of the illuminant and target reflectance
   or transmittance curves, and the camera I/F or U responses.

2. A matrix spanning all dn ranges for calibrating the film recorder
   and TV monitor. This includes computations of the tristimulus
   values at each matrix point. A 5 by 5 by 5 cube would be adequate.

.page
PROGRAM HISTORY

Written by Jean Lorre circa 1990
Ported by CRI, Jan 2 1995
Current cognizant programer: Jean Lorre
Revision history:
  Sep 19 95  GMY  Replace call to xveaction with 'SA' parameters in xvopen
                  to avoid abending in xlget when 'EDRTAPE' item is missing
                  from VICAR label (FR 89820)

.level1

.vari INP
input image files

.vari OUT
output image files

.vari COLORS
special colors

.vari CONV
input image conversion factors
for radiance.

.vari IOVF
input image conversion factors
for reflectance.

.vari FILTER
GLL filter positions.

.vari XYZ
Tristimulus values
for special colors

.vari RESPONSE
The radiance or reflectance 
for special colors

.vari DEVICE
output device

.vari INC
 Line and Sample increment.

.vari NOAUTO
AUTO scaling or
NOAUTO scaling
default=AUTO

.vari NTERMS
Number of terms in
Yxy to DN fit.

.vari NFITS
Number of points used
in the Yxy to DN fit.

.vari ILLUMIN
Illumination scaling
factor.

.vari MACBETH
Internal test.

.level2
.vari INP
INP = (input1,...,inputN), where the inputs are up to ten 
images decalibrated by the program GALSOS.  The images must have been taken
with different filters and must all be the same size.

The input images should be reflectance images if the specified DEVICE is a film 
recorder, and radiance images if the specified DEVICE is a color monitor. 

.vari OUT
OUT = (redimage,greenimage,blueimage), where the outputs are image filenames 
to be used as inputs to the display device.
Outputs are always BYTE format.
There is an optional fourth output file containing a 2-d histogram of
the chromaticity space. See help file for specifics.

.vari COLORS
COLORS = (1,...,10), where the colors are up to 10 special 
colors.  There must be at least as many colors named as there are input images.
Eight valid colors have been defined for GLL, they have the names:
NAME     number       description                 comments
1/3        1       GLL bright red patch
1/12       2       GLL bright green patch
5/7        3       GLL bright blue patch
7/6        4       GLL dark blue patch
11/7       5       GLL darkest blue patch
desert     6       Earth desert yellow  
water      7       Earth ocean blue     
cloud      8       Earth clouds         
Tables of XYZ tristimulus values and response in both radiance and
reflectance for these 8 colors are built into the program already.
If you specify the XYZ and RADIANCE values by parameter input then
you need not specify special colors.
Example: colors=(3,4,5)

.vari CONV
CONV=(factor1,...,factorN), where each conversion factor corresponds to an 
input image (as they are ordered in INP).  CONV allows all input images to be
scaled in the same units, and should be the same as those specified in the
program GALSOS.  Each factor is multiplied by each pixel in the corresponding 
input image.  The number of factors specified must be at most equal to the 
number of inputs specified.
The default is to use CONV values in the picture labels.
Only used in the 'TV mode.

.vari IOVF
IOVF=(factor1,...,factorN), where each conversion factor corresponds to an 
input image (as they are ordered in INP).  IOVF allows all input images to be
scaled in the same units, and should be the same as those specified in the
program GALSOS.  Each factor is multiplied by each pixel in the corresponding 
input image.  The number of factors specified must be at most equal to the 
number of inputs specified.
The default is to use IOVF values in the picture labels.
Only used in the 'FILM mode.

.vari FILTER
The GLL filter position numbers (0 to 7) for each of the input images
in the order of input. Defaults to the filter position in the image
label.
The first 5 special colors exist only for filters 1-3.

.vari XYZ
Tristimulus values in the order X, Y, Z for each special color.
These offer a manual input. Usually they come from internal
tables pointed to by the COLORS keyword.

.vari RESPONSE
The radiance or reflectance values for each special color and for
each filter position input. In the order of each input for the
first special color followed by the order of input for the second
color...
These offer a manual input. Usually they come from internal
tables pointed to by the COLORS keyword and the FILTER keyword.
The I/F and CONV values are included in the RESPONSE.

.vari DEVICE
[DEVICE = imagedevice], where "imagedevice" specifies the device on which the 
output image files are to be displayed.  The only valid devices are 'TV 
(Conrac 7211) and 'FILM (MDA Color Fire). 

.vari INC
The interval between lines and samples to skip in the initial pass
through the input data to obtain auto scaling.
Default is 10

.vari NOAUTO
To inhibit auto scaling of the Y tristimulus value. AUTO assures that
the full dynamic range of the display is utilized.
Defaults to 'AUTO

.vari NTERMS
Number of terms to use in the polynomial fit connecting the Yxy 
values to DN . Can be 4 or 10. Defaults to 4. Do not try other than
the default value.

.vari NFITS
Number of points to use in the polynomial fit connecting Yxy to
DN. Can be from NTERMS+1 to 20. Defaults to 7. Do not use other than
the default.

.vari ILLUMIN
Illumination scaling factor. The DN*CONV or DN*I/F values used for the
special colors only can be multiplied by ILLUMIN to assist in 
controlling brightness. This only affects the least squares fits
relating special colors to tristimulus values.
You should not need to use this factor.
DEfaults to 1.0 .

.VARI MACBETH
Initiates an internal test of the program.
To use this test run the program as you would normally with images.
The output images should be ignored. Instead a printout of the
output dn values which correspond to 24 macbeth color patches will
be printed. Give these to Chris Capraro to load into the film
recorder to see how close the printed patches come to the desired
table of Yxy values.

.end
$ Return
$!#############################################################################
$Test_File:
$ create tsttrucolor.pdf
procedure
!  TO RUN ON VMS, TYPE     TSTTRUCOLOR
!  TO RUN ON UNIX MACHINE, MOVE THE TEST FILES TO THE MACHINE FROM THE VAX
!    IF NOT AVAILABLE ON THAT MACHINE, AND TYPE
!     tsttrucolor DIR=dirname
!       where dirname = pathname of directory containing files with trailing /
!           or        = "" if in current directory.
!
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let $autousage="none"
Let _onfail="continue"
Let $echo="yes"     
local dir string
if ($syschar(1) = "VAX_VMS")
   let dir="wms_test_work:[testdata.mipl.gll]"
else
   let dir="/project/test_work/testdata/mipl/gll/"
end-if
LOCAL RAD660 TYPE=STRING
LOCAL RAD560 TYPE=STRING
LOCAL RAD445 TYPE=STRING
LOCAL REF660 TYPE=STRING
LOCAL REF560 TYPE=STRING
LOCAL REF445 TYPE=STRING
let RAD660 = "&DIR"//"660.rad"
let RAD560 = "&DIR"//"560.rad"
let RAD445 = "&DIR"//"445.rad"
let REF660 = "&DIR"//"660.ref"
let REF560 = "&DIR"//"560.ref"
let REF445 = "&DIR"//"445.ref"
! make sure to also get cube.film and store it under the name CUBE.TEMP
! in the current directory
let _onfail="continue"
let $echo="yes"
!
trucolor inp=(&RAD660, &RAD560, &RAD445) +
         out=(r.img,g.img,b.img,hist.img) +
         colors=(3,4,5) conv=(1.,.2,.01) 'TV filter=(2,1,3)
list inp=r.img linc=15 sinc=15
list inp=g.img linc=15 sinc=15
list inp=b.img linc=15 sinc=15

trucolor inp=(&REF660, &REF560, &REF445) +
        out=(r.img,g.img,b.img,hist.img) filter=(2,1,3) +
        colors=(3,4,5) iovf=(6.,.9,.04) 'FILM
list inp=r.img linc=15 sinc=15
list inp=g.img linc=15 sinc=15
list inp=b.img linc=15 sinc=15

! manual override of autoset parameters.
trucolor inp=(&REF660, &REF560, &REF445) +
     out=(r.img,g.img,b.img,hist.img) +
     iovf=(6.,.9,.04) 'FILM +
     xyz=(.0168,.0179,.02451, .0268,.01841,.01175, .0177,.0156,.01124) +
     response=(1680.,751.9,345.5, 6117.,627.6,218.8, 2523.,725.9,193.6)
list inp=r.img linc=15 sinc=15
list inp=g.img linc=15 sinc=15
list inp=b.img linc=15 sinc=15
!
! test of reading a color cube table and of macbeth keyword
! note copy of cube.film file to local directory
! and deletion of trucolor.film file to force creation of a new one
! using the cube data.
if ($syschar(1)="UNIX")
   ush cp CUBE.TEMP CUBE.FILM 
   ush rm TRUCOLOR.FILM
else
   dcl copy cube.temp cube.film
   dcl del trucolor.film;*
end-if
trucolor inp=(&REF660, &REF560, &REF445) +
     out=(r.img,g.img,b.img) filter=(2,1,3) +
     colors=(3,4,5) iovf=(6.,.9,.04) +
     'FILM 'MACBETH
!

end-proc
$ Return
$!#############################################################################
