/*
 * Portable "Curses" window screen management for EDIBIS
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <curses.h>

#define MAX_STRING 200

#if VMS_OS
#  define 	STANDOUT(win) wsetattr(win,_REVERSE)
#  define 	STANDEND(win) wclrattr(win,_REVERSE)
#else
#  ifdef A_REVERSE /* Then attributes work */
#    define 	STANDOUT(win) wattron(win,A_REVERSE)
#    define 	STANDEND(win) wattroff(win,A_REVERSE)
#  else /* try the only thing left */
#    define  STANDOUT(win) wstandout(win)
#    define  STANDEND(win) wstandend(win)
#  endif
#endif


#  define beep() printf("%c",7); 

#define PROMPTPOS 22
#define STATUSPOS 22
#define DATAPOS 4
#define DATASIZE 18
#define RCPOS 1
#define ROWPOS 5
#define COLPOS 22
#define FORMATPOS 2

static WINDOW *WEdit=(WINDOW *)0;
static WINDOW *WHeader=(WINDOW *)0;
static WINDOW *WFormats=(WINDOW *)0;
static WINDOW *WColNum=(WINDOW *)0;
static WINDOW *WRowNum=(WINDOW *)0;
static WINDOW *WData=(WINDOW *)0;
static WINDOW *WPrompt=(WINDOW *)0;
static WINDOW *WStatus=(WINDOW *)0;
static char keypad_mode[20];

void F77_FUNC(initscreen, INITSCREEN)(batch)
int batch;
{

#if VMS_OS
	strcpy(keypad_mode,"\033=");
#endif
#if UNIX_OS
	strcpy(keypad_mode,"\033[?1h\033=");
#endif

	printf(keypad_mode);  /* use "keypad()" for SYSV */

	initscr();
	noecho(); /* don't echo chars on terminal */
	crmode(); /* immediately transmit chars; no filter */
	nonl();   /* don't convert nl->CR/LF */

	WEdit = newwin(0,0,0,0);
		WHeader = subwin( WEdit,   2,0,0,0);
			WRowNum = subwin( WEdit, 1,6, RCPOS,  ROWPOS);
			WColNum = subwin( WEdit, 1,5, RCPOS,  COLPOS);
		WFormats = subwin( WEdit,  2,0,FORMATPOS,0);
		WData = subwin( WEdit,DATASIZE ,0,DATAPOS,0);

}

void F77_FUNC(endscreen, ENDSCREEN)()
{
	endwin();
}

void F77_FUNC_(emit_beep,EMIT_BEEP)()
{
	beep();
}

void F77_FUNC_(high_write,HIGH_WRITE)(int *y, int *x, char *string, ZFORSTR_PARAM)
{
	ZFORSTR_BLOCK
        char c_string[MAX_STRING+1];
	
        zsfor2c(c_string, MAX_STRING, string, &y, 3, 3, 1, string);
	wmove(WEdit,(*y)-1,(*x)-1);
	STANDOUT(WEdit);
	waddstr(WEdit,c_string);
	touchwin(WEdit);
	wrefresh(WEdit);
	STANDEND(WEdit);
}

void F77_FUNC(gotoposition, GOTOPOSITION)(vert,horiz)
int *vert;
int *horiz;
{
	wmove(WEdit,(*vert)-1,(*horiz)-1);
}

void F77_FUNC(gotoy,GOTOY)(vert)
int *vert;
{
	wmove(WEdit,(*vert)-1,0);
}

void F77_FUNC(eraseline, ERASELINE)()
{
	wclrtoeol(WEdit);
 }


void F77_FUNC_(delete_char,DELETE_CHAR)()
{
	int y,x;
	
	getyx(WEdit,y,x);
	wmove(WEdit,y,x-1);
	wclrtoeol(WEdit);
	touchwin(WEdit);
	wrefresh(WEdit);
}

void F77_FUNC_(show_char,SHOW_CHAR)(thechar)
int *thechar;
{
	waddch(WEdit,(char)(*thechar));
	touchwin(WEdit);
	wrefresh(WEdit);
}


void F77_FUNC(newscreen,NEWSCREEN)()
{
	clear(); 
}


void F77_FUNC_(show_help,SHOW_HELP)()
{
	int line=0;

	wclear(WEdit);

#define WP(str) mvwaddstr(WEdit,line++,0,(str));
WP("          EDIBIS COMMANDS                 +-----------------------------------+")
WP("    (For NOKEYPAD help, hit return)       |        |        | FNDNXT | DELETE |")
WP("Arrow keys move current cell one place.   |  GOLD  |  HELP  |        |  (ROW) |")
WP("ADVANCE and BACKUP set direction.         |        |        |  FIND  | INSERT |")
WP("                                          |--------+--------+--------+--------|")
WP("FIND Search Strings:                      |        | PAGE   |        |  CUT   |")
WP("----------------------                    |   --   | UP/DWN |   --   | (CELL) |")
WP("  VALUE     -Exact Match                  |        |        |        | PASTE  |")
WP("  MIN:MAX   -Between values               |--------+--------+--------+--------|")
WP("  MIN:      -Value and above              | ADVANCE| BACKUP |        |        |")
WP("  :MAX      -Value and below              |        |        |   --   |   --   |")
WP("                                          | BOTTOM |  TOP   |        |        |")
WP("Other Commands:                           |--------+--------+--------+--------|")
WP("---------------                           |        | PAGE   |        | GO TO  |")
WP("CTRL/R      Refresh screen                |   --   | RT/LFT |   --   |  ROW   |")
WP("CTRL/F      Display values using format   |        |        |        |        |")
WP("QUIT        Quit and exit to VICAR        |--------+--------+--------|        |")
WP("EXIT        Save and exit to VICAR        |                 |        | GO TO  |")
WP("<RETURN>    Change cell value to input    |      --         |   --   |  COL   |")
WP("/ <command> Use NOKEYPAD command          +-----------------+--------+--------+")
WP("                                           To exit, press the spacebar.        ")

	touchwin( WEdit );
	wrefresh(WEdit);
}


void F77_FUNC_(show_nokeypad_help,SHOW_NOKEYPAD_HELP)()
{
	int line=0;

	wclear(WEdit);

#define WP(str) mvwaddstr(WEdit,line++,0,(str));
WP("NOKEYPAD: The no-keypad commands may  be used interactively by placing the     ")
WP("command on the EDIBIS command-line, prefaced by a '/' character, e.g.          ")
WP("                                                                               ")             
WP("    ]  /(1,2) set 5.1                                                          ") 
WP("                                                                               ")             
WP("The (row,col) parameter is optional and defaults to current cursor position.   ")
WP("                                                                               ")             
WP("Command (may be abbreviated)                     Function                      ")
WP("-----------------------------   ---------------------------------------------  ")
WP("(row,col) SET <value>           Change the value of cell (row,col) to <value>  ")
WP("(row,col) DEL*ETE <numrows>     Delete <numrows> rows, starting at <row>       ")
WP("(row,col) INS*ERT <numrows>     Delete <numrows> rows, starting at <row>       ")
WP("(row,col) JUM*P                 Jump to position (row,col) in file.            ")
WP("(row,col) CUT                   Copy current cell value into buffer, and clear ")
WP("(row,col) PAS*TE                Paste buffer into current cell                 ")
WP("(row,col) SEA*RCH <string>      Search fwd/backwd in column for range <string>.")
WP("(row,col) FOR*MAT (formt)       (IBIS-1 only) Set the column FORTRAN FORMAT.   ")
WP("          FWD/BAC*KWARD         Set search direction ForWarD(down)/BACkwd(Up).")
WP("          TOP/BOTTOM            Go to top/bottom of file                       ")
WP("          LEF*T/RIG*HT          Go one page left/right in file                 ")
WP("          ROW <rownum>          Go to row <rownum>, same column                ")
WP("          COL*UMN <colnum>      Go to column <colnum>, same row                ")
WP("To exit, press any key.                                                        ")

	touchwin( WEdit );
	wrefresh(WEdit);
}

void F77_FUNC_(show_formats,SHOW_FORMATS)()
{
	touchwin( WEdit );
	wrefresh( WFormats );
}


void F77_FUNC_(show_data,SHOW_DATA)()
{
	touchwin( WEdit );
}

void F77_FUNC_(clear_data,CLEAR_DATA)()
{
   wclear(WData);
   touchwin(WData);
}

void F77_FUNC_(show_header,SHOW_HEADER)()
{
	touchwin( WEdit );
	wrefresh( WHeader );
}

void F77_FUNC_(show_rownum,SHOW_ROWNUM)()
{
	touchwin( WRowNum );
}

void F77_FUNC_(show_colnum,SHOW_COLNUM)()
{
	touchwin( WColNum );
}


/*
 *  Append a string of characters, with no
 *  end-of-line return. This avoids the VAX-VMS prompting
 *  '+','$' carriage-control extension, which is non-portable.
 */


void F77_FUNC_(row_write,ROW_WRITE)(char *string, int *row, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
   char *cptr;
   
   zsfor2c(c_string, MAX_STRING, string, &string, 2, 1, 1, row);
   wmove(WEdit,DATAPOS + (*row)-1,0);
   waddstr(WEdit,c_string);
}

void F77_FUNC_(rownum_write,ROWNUM_WRITE)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
   char *cptr;
   
   wclear(WRowNum);
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   wmove(WEdit,RCPOS,ROWPOS);
   waddstr(WEdit,c_string);
}

void F77_FUNC_(colnum_write,COLNUM_WRITE)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
   char *cptr;
   
   wclear(WColNum);
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   wmove(WEdit,RCPOS,COLPOS);
   waddstr(WEdit,c_string);
}

void F77_FUNC_(screen_write,SCREEN_WRITE)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
   char *cptr;
   
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   waddstr(WEdit,c_string);
}

void F77_FUNC_(display_prompt,DISPLAY_PROMPT)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
   char *cptr;
   
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   wmove(WEdit,PROMPTPOS,0);
   wclrtoeol(WEdit);
   wmove(WEdit,PROMPTPOS,0);
   waddstr(WEdit,c_string);
   wrefresh(WEdit);
}

void F77_FUNC_(display_status,DISPLAY_STATUS)(char *string, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_string[MAX_STRING+1];
   char *cptr;
   
   zsfor2c(c_string, MAX_STRING, string, &string, 1, 1, 1, string);
   wmove(WEdit,STATUSPOS,0);
   wclrtoeol(WEdit);
   wmove(WEdit,STATUSPOS,0);
   waddstr(WEdit,c_string);
   waddstr(WEdit," (hit <return> to continue)");
   wrefresh(WEdit);
   wgetch(WEdit);
   wmove(WEdit,STATUSPOS,0);
   wclrtoeol(WEdit);
   wrefresh(WEdit);
}


