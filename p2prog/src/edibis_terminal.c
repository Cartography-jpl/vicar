/*
 *  edibis_terminal.c
 *
 *   Handles ALL terminal I/O  and does non-blocking
 *   reads direct from keyboard/pad.
 *
 *   NB: This is not fully ported to all hosts.
 */

#include <stdio.h>
#include "xvmaininc.h"
#include <zvproto.h>
#include "ftnbridge.h"
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#if VMS_OS
#include <descrip.h>
#include <iodef.h>
#include <ssdef.h>
#else
#include <termios.h>
#include <fcntl.h>
#include <errno.h>
#define RESET		0
#define RAW		1
#define STDCHAR		2
#define CTRLCHAR	3
#endif

#ifndef TRUE
#define TRUE		1
#define FALSE		0
#endif

#ifndef BOOLEAN
typedef unsigned char 	BOOLEAN;
#endif

#define WAIT_INPUT	20
#define POLL_INPUT	21

#if UNIX_OS
static void settermmode(int terminal, int mode);
#endif

/*  This routine waits for the user to press a key, then processes the key    */
/*  and returns the value to the calling context.  The 'startup' parameter    */
/*  should be set to True (1) while getting input.  The routine should be     */
/*  called one last time when you are through gathering input with a para-    */
/*  meter value of False (0) to restore the terminal to normal.               */

/*  --Borrowed from GETINPUT.COM. May be eliminated if and when the           */
/*  "waitforinput" call can indicate whether the key returned was a Keypad    */
/*   escape sequence or not. This routine returns everything.                 */

int F77_FUNC_(get_char,GET_CHAR)(startup)
int *startup;
{
   char inpchar, errmsg[81];
   static BOOLEAN first = TRUE;

#if VMS_OS
   int stat;
   static int  channel;
   static struct {
      unsigned short status;
      unsigned short count;
      long info;
   } iosb;
   $DESCRIPTOR(name, "sys$command");

   if (*startup) {
      if (first) {
         stat = SYS$ASSIGN(&name, &channel, 0, 0);
         if (stat != SS$_NORMAL) {
            zvmessage("Error assigning a channel to your terminal:", "");
            sprintf(errmsg, "     status = %d", stat);
            zvmessage(errmsg, "");
            return (0);
         }
         first = FALSE;
      }

      stat = SYS$QIOW(0, channel, IO$_READVBLK | IO$M_NOECHO | IO$M_NOFILTR,
           &iosb, 0, 0, &inpchar, 1, 0, 0, 0, 0);
      if (stat != SS$_NORMAL || iosb.status != SS$_NORMAL) {
         zvmessage("Error reading terminal input:");
         sprintf(errmsg, "     status = %d, iosb status = %d", stat, iosb.status);
         zvmessage(errmsg, "");
         return (0);
      }
   }
   else {
      if (!first) {
         SYS$CANCEL(channel);
         SYS$DASSGN(channel);
         first = TRUE;
      }
      return ((int) '\0');
   }

#else

   int stat;
   static int terminal;

   if (startup) {
      if (first) {
         first = FALSE;
         terminal = open("/dev/tty", O_RDONLY, 0);
         if (terminal == -1) {
	   zvmessage("Unable to obtain terminal file descriptor--using default instead.", 0);
            terminal = 0;
         }
         settermmode(terminal, STDCHAR);
      }

      stat = read(terminal, &inpchar, sizeof(inpchar));
      if (stat <= 0) {
	zvmessage("Error reading terminal input:", 0);
         sprintf(errmsg, "     errno = %d; ", errno);
         zvmessage(errmsg, "");
         perror(NULL);
         return (0);
      }
   }
   else {
      if (!first) {
         settermmode(terminal, RESET);
         close(terminal);
         first = TRUE;
      }
      return ((int) '\0');
   }
#endif

   /* deal with VAX/UNIX differences in <return> codes */
  
   if ((int)inpchar == 10 || (int)inpchar == 13)
      return (13);
   else
      return ((int) inpchar);
}



#if UNIX_OS
static void settermmode(int terminal, int mode)
{
   static struct termios tbufsave;
   struct termios tbuf;
   static int termset = FALSE;
   char errmsg[81];

   if (mode != RESET) {
      if (tcgetattr(terminal, &tbuf) == -1) {
	zvmessage("Error getting tty info:",0);
         sprintf(errmsg, "     errno = %d; ", errno);
         zvmessage(errmsg, "");
         perror(NULL);
      }
      tbufsave = tbuf;
      termset = TRUE;

      if (mode == RAW) {
         tbuf.c_iflag &= ~(INLCR | ICRNL | ISTRIP | IXON | BRKINT);
         tbuf.c_oflag &= ~OPOST;
         tbuf.c_lflag &= ~(ICANON | ISIG | ECHO);
      }
      else if (mode == STDCHAR) {
         tbuf.c_lflag &= ~(ICANON | ECHO);
      }
      else if (mode == CTRLCHAR) {
         tbuf.c_iflag &= ~BRKINT;
         tbuf.c_lflag &= ~(ICANON | ISIG | ECHO);
      }

      tbuf.c_cc[4] = sizeof(char);
      tbuf.c_cc[5] = 2;
      if (tcsetattr(terminal, TCSANOW, &tbuf) == -1) {
	zvmessage("Error setting terminal mode:",0);
         sprintf(errmsg, "     errno = %d; ", errno);
         zvmessage(errmsg, "");
         perror(NULL);
      }
   }
   else {
      if (termset) {
         if (tcsetattr(terminal, TCSANOW, &tbufsave) == -1) {
	   zvmessage("Error re-setting terminal:",0);
            sprintf(errmsg, "     errno = %d; ", errno);
            zvmessage(errmsg, "");
            perror(NULL);
         }
         termset = FALSE;
      }
   }
}
#endif



