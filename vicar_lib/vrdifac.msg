!
!	Message file for facility VIC2
!
!  This file contains the message id's and detailed texts for
!  the VRDI internal errors.
!
!
!	NOTE :  This file is built by VRDIERRORS.COM.  It should never
!		be modified directly.  To make changes to the source,
!		change VRDIERRORS.COM.
!
!
.KEY AFGNOTACT	! AFG_NOT_ACTIVE
Explanation:

  An attempt has been made to access an  alphanumeric
font generator which has not been activated.
 
User action:
 
  Notify the cognizant programmer of the error.

Programmer Action:

  Activate the  alphanumeric font  generator  with a 
XDAON prior to accessing it.

.KEY NOAFG	! AFG_NOT_AVAILABLE
Explanation:

  An attempt was  made to  use  an alphanumeric  font 
generator on a device which does not have one.

User Action:

  See the cognizant programmer.  Only the RAMTEK RMA2
device is supported  by the VRDI as  an  alphanumeric 
font generator.

.KEY AWOFFIMP	! AW_NOT_ON_IMP
Explanation:

  The access window has been defined  larger then the
image plane size of the display device.

User Action:

  See the cognizant programmer.

Programmer Action:

  The size of the image planes can be  determined  by
calling XDDINFO.  This information  can then be  used
in the call to XDIAWSET.

.KEY AWNOTSQ	! AW_NOT_SQUARE
Explanation:

  The  access window was not square  when an  attempt
was  made to  rotate  the image.   The  largest  size
square within the access  window  was  used  and  the
resulting image rotated.

User Action:

  If  the   results   of   the  image  rotation   are 
inadequate,  have  the  cognizant  programmer  define
the access window as square.

.KEY AWDIFF	! AW_SIZES_DIFFERENT
Explanation:

  An  attempt was  made  to  perform  an image  plane
operation  (i.e.  XDIICOPY, XDIISHIFT) and  the image
planes have access windows of  different  sizes.  The
largest  rectangle  that  will fit within  the access
windows has been used.

User Action:

  If the  results of  the image  plane  operation are
inadequate,  have  the  cognizant  programmer  define
access windows of the same size.

.KEY BADAW	! BAD_ACCESS_WINDOW
Explanation:

  The  access  window has been  defined  incorrectly.
This is  a program error. LEFT has been defined as to
the  right of  RIGHT or TOP  has been  defined  below
bottom.

User Action:

  Consult the cognizant programmer.

.KEY BADASCII	! BAD_ASCII_CODE
Explanation:

  An   invalid   Ascii   character  code   has   been
encountered in the font file.

User Action:

  Have  the  VRDI cognizant  programmer  correct  the
error.

.KEY BADCOORCT	! BAD_COORDINATE_COUNT
Explanation:

  An  attempt  was  made  to  draw  a  vector   while
supplying only one point. This is a program
error.

User Action:

  Have the cognizant programmer correct the error.

.KEY BADDW	! BAD_DISPLAY_WINDOW
Explanation:

  An  attempt  has  been  made  to  set  the  display
window  beyond the display boundaries.   TOP or  LEFT
has  been set less then 1 or  LEFT beyond  the number
of  samples or TOP beyond the number of lines  in the
image plane.

User Action:

  Consult  the  cognizant programmer  to  correct the
error.

.KEY BADPIXCNT	! BAD_PIXEL_COUNT
Explanation:

  The  pixel  count  specified  in  an area  write is
less than 1. 

User Action:

  Consult  the  cognizant  programmer to  correct the
error.

.KEY BADVECCNT	! BAD_VECTOR_COUNT
Explanation:

  An invalid  vector  count has been  encountered  in
the font file.

User Action:

  Have  the  VRDI  cognizant  programmer correct  the
error.

.KEY NOBYPASS	! CANNOT_BYPASS_LUT
Explanation:

  An  attempt was  made to  bypass the  look-up table
and send the pixel values directly to the DACs.

User Action:

  Consult the cognizant programmer.

Programmer Action:

  Use  XDDINFO  to  determine  whether   the  look-up 
tables can be bypassed to avoid this error.

.KEY CANTCNCT	! CANNOT_CONNECT_LUT
Explanation:

  An   attempt  to  connect  an  image  plane  to  a
look-up table  for a  display device which does  not
support this function.

User Action:

  Consult  VRDI  cognizant programmer.  All  devices
currently support this function.

.KEY NOTOWNER	! CANNOT_DEALLOCATE
Explanation:

  An  attempt  was  made to  FREE a  display  device
that was not allocated.

User Action:

  Do  a  SHOWDISP to  determine   which  device   is
currently allocated to you and FREE it.

.KEY CANTZOOM	! CANNOT_ZOOM
Explanation:

  An  attempt  was made to  perform a hardware  zoom
of an  image  plane by  a  factor not  supported  by
the device.

User Action:

  Enter a  smaller zoom  factor or use the  software
zoom if available in the program.

.KEY DEVACTIVE	! DEVICE_ALREADY_ACTIVE
Explanation:

  An  attempt to  activate a  display  device   has
been made when the device is already active.

User Action:

  Notify  cognizant  programmer  of  message.  This
is only a warning and not a problem.

.KEY DEVALLOC	! DEVICE_ALREADY_ALLOC
Explanation:

  An  attempt to  allocate a  display  device  has
been made when the device is already allocated.

User Action:

  No action.

.KEY DEVOPEN	! DEVICE_ALREADY_OPEN
Explanation:

  An  attempt to  open  a display  device has been
made when the device is already open.

User Action:

  Notify  cognizant  programmer of  message.  This
is only a warning and not a problem.

.KEY DEVCANTDO	! DEVICE_CANNOT_DO_IT
Explanation:

  An  attempt has been made to perform a  function
on a device  which it does not  support and  which
cannot or has not yet been emulated.

User Action:

  If  you require the function,  consult the  VRDI
User's  Reference Guide to determine which display
device will perform it, and use that device.

.KEY DEVNOTACT	! DEVICE_NOT_ACTIVE
Explanation:

  An  attempt  has been  made to access a  display
device which has not been activated.

User Action:

  Notify cognizant programmer of error.

.KEY DEVNOTALL	! DEVICE_NOT_ALLOC
Explanation:

  An  attempt has  been made to  access a  display
device which has not been allocated.

User Action:

  Allocate device.

.KEY DEVNOTAVL	! DEVICE_NOT_AVAIL
Explanation:

  An  attempt  has   been  made  to   allocate   a
device which is not currently available for use.

User Action:

  Allocate another device.

.KEY DEVNOTOPN	! DEVICE_NOT_OPEN
Explanation:

  An  attempt has  been  made to access a  display
device which has not been opened.

User Action:

  Notify cognizant programmer of error.

.KEY FNTEOF	! FONT_FILE_EOF
Explanation:

  An  unexpected  end-of-file  has  been  encountered
while reading the font definition file.

User Action:

  Have  the VRDI  cognizant  programmer  correct  the
error.

.KEY FNTOPNERR	! FONT_FILE_OPEN_ERROR
Explanation:

  An  unexpected  error occurred  while  opening  the
font definition file.

User Action:

  Have  the VRDI  cognizant  programmer  correct  the
error.

.KEY FNTRDERR	! FONT_FILE_READ_ERROR
Explanation:

  An  unexpected  error occurred  while  reading  the
font definition file.

User Action:

  Have  the VRDI  cognizant  programmer  correct  the
error.

.KEY FNTTBLOVR	! FONT_TABLE_OVERFLOW
Explanation:

 Insufficient  memory   for  operation.  The VRDI was
not able   to    allocate    sufficient memory for an
internal function.
 
User action:
 
 This  error  probably    indicates   a  memory quota
was  exceeded.  See  the  system  manager  or  system
programmer to determine  the  exact cause and perhaps
increase the amount of memory available.

.KEY FNCNOTIMP	! FUNC_NOT_IMPLEMENTED
Explanation:

  An  attempt has been  made to execute a function of
the VRDI which has not been implemented yet.

User Action:

  Check with the  VRDI  cognizant  programmer for the
schedule of implementation.

.KEY IMPSMALL	! IMP_TOO_SMALL
Explanation:

  A  device  configuration  has been  requested  which 
defines the  image plane to be  smaller then the video
output.

User Action:

  Notify the  cognizant programmer  of the error.  The 
available configurations for each device are available
using XDDINFO.

.KEY INVARG	! INVALID_ARGUMENT
Explanation:

  A call to  XDDNAME  has been made  with  an invalid 
value in the FLAG parameter.

User Action:

  Have the cognizant programmer correct the error.

.KEY INVAWROT	! INVALID_AW_ROTATION
Explanation:

  A call has been made to  XDIROTATE with an  invalid
value in the rotation FLAG parameter.

User Action:

  Have the cognizant programmer correct the error.

.KEY INVCHCNT	! INVALID_CHAR_COUNT
Explanation:

  A negative value has been supplied as the character
count to either the XDTTEXT or the XDTLENGTH routine.
This is a program error.

User Action:

  Report the error to the cognizant programmer.

.KEY INVCOORD	! INVALID_COORDINATES
Explanation:

  Coordinate  values  either  less than 1  or greater
than the size of the  video output  has been supplied
to the XDCSET routine.

User Action:

  Limit your  coordinate selection to the size of the
video output.

.KEY INVDEVREQ	! INVALID_DEVICE_REQ
Explanation:

  The   device  cannot  be  configured  in   the  way
requested.

User Action:

  Consult  the  VRDI  User's  Reference   Guide    to
determine  the legal configurations for the device or
notify the cognizant programmer.

.KEY INVDEVTYP	! INVALID_DEVICE_TYPE
Explanation:

  A  request to  read the  value of an interactive IO
device which  is  not defined  as two dimensional has
been made.

User Action:

  Notify the cognizant programmer of the error.

Programmer Action:

  The  information  on  interactive IO devices can be
determined by calling XDDINFO.

.KEY INVFNTHGT	! INVALID_FONT_HEIGHT
Explanation:

  A  negative value has been  supplied to the XDTSIZE
routine for the font height parameter.

User Action:

  Notify the cognizant programmer of the error.

.KEY INVFNTLOC	! INVALID_FONT_LOCATION
Explanation:

  An  invalid value has been  supplied to the XDTTEXT
routine  for  the  text  location  parameter.   Valid
values are 1, 2, or 3.

User Action:

  Notify the cognizant programmer of the error.

.KEY INVIMPSIZ	! INVALID_IMP_SIZE
Explanation:

  This error is no longer used.

User Action:

  No action.

.KEY INVINFO	! INVALID_INFO_REQUEST
Explanation:

  An  invalid  request  for  device  information  via
XDDINFO has  been made.  Either the  START  parameter
is less  then zero or the  combination  of the  START
and  LENGTH  parameters is  larger then  the  size of
the device information block.

User Action:

  Notify the cognizant programmer of the error.

.KEY INVOUTSIZ	! INVALID_OUTPUT_SIZE
Explanation:

  This error is no longer used.

User Action:

  No action.

.KEY INVRADIUS	! INVALID_RADIUS
Explanation:

  A  value less  than 1  has  been  supplied  to  the
XDICIRCLE routine for the radius parameter.

User Action:

  Input  a  valid  radius  or  notify  the  cognizant
programmer of the error.

.KEY INVFNTSC	! INVALID_FONT_SCALE
Explanation:

  A  negative value has been supplied to the  XDTSIZE
routine for the font scale parameter.

User Action:

  Inform the cognizant programmer of the error.

.KEY INVTXTPRC	! INVALID_TEXT_PREC
Explanation:

  A value less than zero or  greater  than 1 has been
supplied to the  XDTCOLOR routine for  the  PRECISION
parameter. 

User Action:

  Inform the cognizant programmer of the error.  

.KEY MEMERR	! MEMORY_ERROR
Explanation:

 Insufficient  memory   for  operation.  The VRDI was
not able   to    allocate    sufficient memory for an
internal function.
 
User action:
 
 This  error  probably    indicates   a  memory quota
was  exceeded.  See  the  system  manager  or  system
programmer to determine  the  exact cause and perhaps
increase the amount of memory available.

.KEY MUSTSETDW	! MUST_SET_ALL_DWS
Explanation:

  This in a  warning message  informing  you that the
device has had to set the display window of all image
planes  when a  request was made  to  set the display 
window of one of the image planes.

User Action:

  As  this  is  just  an  informational  message,  no
action is required.  If  you  do  not  like  being so
warned,  request  the  cognizant programmer to ignore
warning messages and not have them output.

.KEY MUSTZOOM	! MUST_ZOOM_ALL
Explanation:

  This in a  warning message  informing  you that the
device  has  had  to  zoom  all  image  planes when a
request was made to zoom one of the image planes.

User Action:

  As  this  is  just  an  informational  message,  no
action is required.  If  you  do  not  like  being so
warned,  request  the  cognizant programmer to ignore
warning messages and not have them output.

.KEY NOTINAW	! NOT_IN_ACCESS_WINDOW
Explanation:

  A request was made for either a pixel read or write
with coordinates not in the currently  defined access
window.

User Action:

Notify cognizant programmer of the error.

.KEY NOAUTO	! NO_AUTO_TRACK
Explanation:

  The device you are currently using does not support
autotracking.

User Action:

  Consult  the  VRDI   User's   Reference   Guide  to
determine which devices support  autotracking and use
one of them,  if autotracking  is  imperative to your
application. If the device used supports autotracking,
notify the VRDI cognizant programmer of the error.

.KEY NOCURS	! NO_SUCH_CURSOR
Explanation:

  The  CURSOR  number parameter used in a call to one
of the VRDI  cursor  routines  was  invalid  for  the
device.

User Action:

  Select a valid cursor number (see the User's Guide)
or if not selectable, see the cognizant programmer.

Programmer Action:

  The number of cursors available with any device can
be  determined  by calling XDDINFO.  This information
can  then be  used  in  subsequent  calls   requiring
cursor number.

.KEY NOCURSFRM	! NO_SUCH_CURSOR_FORM
Explanation:

  The  CURSOR  form parameter used in a call to XDCON
was an invalid form for the device in use.

User Action:

  Select a valid  cursor  form (see the User's Guide)
or if not selectable, see the cognizant programmer.

Programmer Action:

  The  number  of  cursor  forms  available  with any
device can be  determined  by calling  XDDINFO.  This
information can  then be  used  in  subsequent  calls
to XDCON.

.KEY NOCURSRAT	! NO_SUCH_CURSOR_RATE
Explanation:

  The CURSOR blink rate parameter used in a  call to 
XDCON was an invalid rate for the device in use.

User Action:

  Select a valid  blink  rate (see the  User's Guide)
or if not selectable, see the cognizant programmer.

Programmer Action:

  The  number  of  blink  rates  available  with  any
device can be  determined  by calling  XDDINFO.  This
information can  then be  used  in  subsequent  calls
to XDCON.

.KEY NOFONT	! NO_SUCH_FONT
Explanation:

  A  request was made to  open  a font file which has
not been either found or defined.  Either an  invalid
font file number was used or the logical name XDTFONT
is not defined correctly.

User Action:

  The  VRDI  User's  Reference  Guide  lists the font
files available and also describes how to define your
own font files. If you cannot  select the  font used,
notify the cognizant programmer to correct the error.

Programmer Action:

  See  the  VRDI  cognizant  programmer  if the error
occurs and you have followed the guidelines.

.KEY NOIMP	! NO_SUCH_IMP
Explanation:

  An attempt  was made to  reference an  image  plane
not defined for the device in use. 

User Action:

  Notify the cognizant programmer of the error.

Programmer Action:

  The  number  of  image planes  available  with  any
device can be  determined  by calling  XDDINFO.  This
information can  then be  used  in  subsequent  calls
to routines reading from or writing to  image  planes
to limit access to those available.

.KEY NOINPDEV	! NO_SUCH_INPUT_DEVICE
Explanation:

  An attempt  was  made to  reference an input device
not defined for the device in use. 

User Action:

  Notify the cognizant programmer of the error.

Programmer Action:

  The  number  of  input devices available  with  any
device can be  determined  by calling  XDDINFO.  This
information can  then be  used  in  subsequent  calls
to  routines  requiring  the  input  device number to
limit access to those available.

.KEY NOKNOB	! NO_SUCH_KNOB
Explanation:

  This error is no longer used as the routine  XDX1D
has been eliminated.

User Action:

  No action.

.KEY NOLUT	! NO_SUCH_LUT
Explanation:

  An attempt  was made to  reference a look-up  table
not defined for the device in use. 

User Action:

  Notify the cognizant programmer of the error.

Programmer Action:

  The  number  of look-up tables available  with  any
device can be  determined  by calling  XDDINFO.  This
information can  then be  used  in  subsequent  calls
to  routines  requiring  the  look-up table number to
limit access to those available.

.KEY NOLUTSECT	! NO_SUCH_LUT_SECTION
Explanation:

  An attempt  was made to  reference a look-up  table
section not defined for the device in use. 

User Action:

  Notify the cognizant programmer of the error.

Programmer Action:

  The  number  of look-up  table  sections  available
with any device can be determined by calling XDDINFO.
This  information  can  then be  used  in  subsequent
calls to routines requiring the look-up table section
number to limit access to those available.

.KEY NOSWITCH	! NO_SUCH_SWITCH
Explanation:

  An attempt  was made to  reference an input  switch
not defined for the device in use. 

User Action:

  Notify the cognizant programmer of the error.

Programmer Action:

  The  number and  type of input  switches  available
with any device can be determined by calling XDDINFO.
This  information  can  then be  used  in  subsequent
calls to routines requiring the  input  switch number
to limit access to those available.

.KEY NOOVRLY	! OVERLAY_NOT_AVAILABLE
Explanation:

  A  graphics overlay plane is not available for this
device in this configuration.

User Action:

  Consult  the  VRDI   User's   Reference   Guide  to
determine which devices have graphics planes in which
configurations and  use one of them,  if  a  graphics
plane  is  imperative  to  your  application.  If the
device used has a graphics plane,  notify   the  VRDI
cognizant programmer of the error.

.KEY UNDEFOP	! UNDEFINED_OPERATION
Explanation:

  An   invalid  value  was  supplied  to  either  the
XDIILOGICAL   or   XDIIARITHMETIC   routine  for  the
OPERATION parameter.

User Action:

  Select a valid  value or if not selectable,  notify
the cognizant programmer of the error.

.KEY UNOUTRNG	! UNIT_OUT_OF_RANGE
Explanation:

  The  UNIT  number  parameter  supplied  to  a  VRDI
routine was either  less  than  zero  or greater than
the maximum allowed.

User Action:

  Notify the cognizant programmer of the error.

.KEY DIBOPEN	! DIB_OPEN_ERROR
Explanation:

  An  unexpected  error occurred  while  opening  the
device information block file.

User Action:

  Have  the VRDI  cognizant  programmer  correct  the
error.

.KEY DIBERROR	! DIB_FILE_ERROR
Explanation:

  An  unexpected  error occurred  while  reading  the
device information block file.

User Action:

  Have  the VRDI  cognizant  programmer  correct  the
error.

.KEY NOOWNGDEV	! NO_OWNED_GENERIC_DEV
Explanation:

  An attempt was made to deallocate (FREE) a  display
device using a generic device name and no such device
had been allocated.

User Action:

  Do  a  SHOWDISP to  determine   which  device   is
currently allocated to you and FREE it.

.KEY NOAVLGDEV	! NO_AVAIL_GENERIC_DEV
Explanation:

  An  attempt was  made to  allocate (USE) a  display
device using a generic device name and no such device
is available to be allocated.

User Action:

  Do  a  SHOWDISP to  determine   which  devices  are
available to be allocated and USE one of them.

.KEY NODEFDEV	! NO_DEFAULT_DEVICE
Explanation:

  An  attempt was  made to  allocate (USE) a  display
device  using  the  default  device  option  and  the
terminal used does not have a default device assigned
to it.

User Action:

  Do  a  SHOWDISP to  determine   which  devices  are
available to be allocated and USE one of them.

.KEY NOSUCHDEV	! NO_SUCH_DEVICE
Explanation:

  An  attempt was  made to  allocate (USE) a  display
device unknown to the system.

User Action:

  Do  a  SHOWDISP to  determine   which  devices  are
available to be allocated and USE one of them.

.KEY CANTALLDEV	! CANNOT_ALLOC_DEVICE
Explanation:

  An  attempt was  made to  allocate (USE) a  display
device allocated to another user.

User Action:

  Do  a  SHOWDISP to  determine   which  devices  are
available to be allocated and USE one of them.

.KEY CANTALLHI	! CANNOT_ALLOC_HIRES
Explanation:

  Currently not used.

User Action:

  No action.

.KEY CANTALLOW	! CANNOT_ALLOC_LORES
Explanation:

  Currently not used.

User Action:

  No action.

.KEY CRELNMERR	! CREATE_LNM_ERROR
Explanation:

  A  system error occurred in the  device  allocation
routine while attempting to create the  logical  name
required by the VRDI routines. 

User Action:

  Report the error to the VRDI cognizant programmer.

.KEY DELNMERR	! DELETE_LNM_ERROR
Explanation:

  A  system error occurred in the device deallocation
routine while attempting to delete the  logical  name
created by the device allocation routine.

User Action:

  Report the error to the VRDI cognizant programmer.

.KEY GETPIDERR	! GET_PID_ERROR
Explanation:

  A  system error occurred in the  device  allocation
routine  while   attempting  to  get   your   process
identifcation number.

User Action:

  Report the error to the VRDI cognizant programmer.

.KEY GETNAMERR	! GET_OWNER_NAME_ERROR
Explanation:

  A  system error occurred in the  device  allocation
routine   while   attempting  to   get   the  process
identifcation  number  of  the  owner of the  display
device you wish to allocate.

User Action:

  Report the error to the VRDI cognizant programmer.

.KEY GETERMERR	! GET_OWNER_TERM_ERROR
Explanation:

  A  system error occurred in the  device  allocation
routine   while   attempting  to   get   the terminal
name of  the owner of the display device you wish  to
allocate.

User Action:

  Report the error to the VRDI cognizant programmer.

.KEY GETVAXERR	! GET_VAX_SYMBOL_ERROR
Explanation:

  A  system error occurred in the  device  allocation
routine   while   attempting  to   get   the value of
the VAX system symbol.

User Action:

  Report the error to the VRDI cognizant programmer.

.KEY GETOPIDERR	! GET_OWNER_PID_ERROR
Explanation:

  A  system error occurred in the  device  allocation
routine   while   attempting  to   get   the  process
identification  number of  the  owner  of the display
device you wish to allocate.

User Action:

  Report the error to the VRDI cognizant programmer.

.KEY CANTDALLDEV	! CANNOT_DEALL_DEVICE
Explanation:

  A  system error occurred in the device deallocation
routine while attempting to deallocate the device.

User Action:

  Report the error to the VRDI cognizant programmer.

.KEY INVWRNACT	! INVALID_WARNING_ACTION
Explanation:

  An  invalid  value  was  supplied to the  XDEACTION
routine for the WARNING parameter.  Valid  values are
1, 2, or 3.

User Action:

  Inform  the  cognizant  applications  programmer of
the error.

.KEY INVERRACT	! INVALID_ERROR_ACTION
Explanation:

  An  invalid  value  was  supplied to the  XDEACTION
routine for the ERROR parameter. Valid values are  1,
2, or 3.

User Action:

  Inform  the  cognizant  applications  programmer of
the error.

.KEY INVFTLACT	! INVALID_FATAL_ACTION
Explanation:

  An  invalid  value  was  supplied to the  XDEACTION
routine for the FATAL parameter. Valid values are  1,
2, or 3.

User Action:

  Inform  the  cognizant  applications  programmer of
the error.

.KEY TRANLNMERR	! TRANS_LNM_ERROR
Explanation:

  A  system error  occurred in  the  XDDUNIT  routine
while  attempting  to   translate  the  logical  name
created by the device allocation routine.

User Action:

  Report the error to the VRDI cognizant programmer.

User Action:


.KEY PRESETERR	! CANNOT_RESET_MPC
Explanation:

  An error has ocurred while attempting to  reset the
MPC Peripheral Controller on the Adage display device.
The MPC provides autotracking for the pen and tablet.

User Action:

  Report the error to the VRDI cognizant programmer.

.KEY CANTPOSCUR	! CANNOT_POS_CURSOR
Explanation:

  An error has ocurred while  attempting to  position
the  cursor  on  the  ADAGE  with  an   XDCSET  while
autotracking is enabled.

User Action:

  Disable  autotracking before  manually setting  the
cursor  position.  Enabling  autotracking  again will
position the cursor to the upper left corner.

.KEY DEVFLERR	! DEVICE_FILE_OPEN_ERROR
Explanation:

  The file which controls allocation cannot be created 
or accessed.  This error holds only for UNIX VRDI systems.
The user needs to be able to open a file in the /tmp
directory and needs to be able to read and write the
/tmp/VRDI.LCK file.

User Action:
  
  If access to /tmp or /tmp/VRDI.LCK is denied, then 
report this error to the system manager. Otherwise, report 
the error to the VRDI cognizant programmer. 

.KEY SRVNOTAVL	! SERVER_NOT_AVAILABLE
Explanation:

  A windowing system is not currently active and is needed
for an allocation of a VRDI device. Currently, X Windows
is the only windowing system available with VRDI.

User Action:

  Start the windowing system.  If it is not available or the
startup command is unknown, contact the system manager.

.KEY SHMINVLDSIZ	! SHMEM_INVALID_SIZE
Explanation:

  A system error has occurred. The size of shared 
memory to be allocated is either greater than the 
system imposed maximum or less than the system imposed 
minimum.  Or, the size of the previously allocated shared
memory is less than the size requested.

User action:

  Report error to cognizant applications programmer.

.KEY SHMACCVIOL	! SHMEM_ACCESS_VIOLATION
Explanation:

  A system error has occurred.  The shared memory
identification number is currently in use by another process.

User action:

  Contact cognizant VRDI programmer.

.KEY SHMNOID	! SHMEM_NO_SUCH_ID
Explanation:

  A system error has occurred. The shared memory identfier 
produced by the VRDI ALLOC_SHMEM or ATTACH_SHMEM functions
no longer exists.

User action: 

  Report error to VRDI cognizant programmer.

.KEY SHMNOMOREIDS	! SHMEM_NO_MORE_IDS
Explanation:

  A system error has occurred in XDDAllocate. The system-imposed
limit on the number of shared memory identifiers has been 
exceeded.

User action:

  Report error to VRDI cognizant error.  The user can deallocate
an allocated device to free shared memory identifiers for other
use.

.KEY SHMNOMEM	! SHM_OUT_OF_MEMORY
Explanation:

  A system error has occurred.  The requested size of shared memory
exceeds the physical memory limit.

User action:

  Report error to VRDI cognizant programmer.

.KEY CANTALLSHM	! SHMEM_CANNOT_ALLOC
Explanation:

  An unknown system error has occurred during a shared
memory allocation or attachment.

User action:

  Report error to VRDI cognizant programmer.

.KEY CANTREMSHM	! SHMEM_CANNOT_REMOVE
Explanation:

  An unknown system error has occurred during a shared
memory deallocation.

User action:

  Report error to VRDI cognizant programmer.

.KEY CANTDETSHM	! SHMEM_CANNOT_DETACH
Explanation:

  An unknown system error has occurred during a shared
memory detachment.

User action:

  Report error to VRDI cognizant programmer.

.KEY INVCURSIZ	! INVALID_CURSOR_SIZE
Explanation:

  An invalid cursor size was selected.  The maximum cursor
size is device-dependent.

User action:

  Consult  the  VRDI  User's  Reference   Guide    to
determine  the legal cursor sizes for the device or
notify the cognizant programmer.

.KEY INVCLR	! INVALID_COLOR
Explanation:

  An invalid color value was selected.  Valid values are
0 to 255 inclusive.

User action:

  Re-enter a value between 0 and 255 or notify the cognizant
programmer.

.KEY SHMACT	! SHMEM_ALREADY_ACTIVE
Explanation:

  The shared memory block has already been created by this sub-process
or by another sub-process in the same process tree.  Instead of creating
a new shared memory block, the process shared memory will be mapped to
the existing shared memory block.

User action:

  No action necessary.

Programmer Action:

  This message indicates that the process shared memory was successfully
mapped to an existing shared memory block rather than a new block being
created.  The message is included for information only--no programmer
action is necessary.
