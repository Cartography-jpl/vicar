$!****************************************************************************
$!
$! Build proc for MIPL module mf
$! VPACK Version 1.7, Thursday, November 17, 1994, 08:27:58
$!
$! Execute by entering:		$ @mf
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
$ write sys$output "*** module mf ***"
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
$ write sys$output "Invalid argument given to mf.com file -- ", primary
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
$   if F$SEARCH("mf.imake") .nes. ""
$   then
$      vimake mf
$      purge mf.bld
$   else
$      if F$SEARCH("mf.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mf
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mf.bld "STD"
$   else
$      @mf.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mf.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mf.com -
	-s mf.c -
	-i mf.imake -
	-p mf.pdf -
	-t tstmf.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mf.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*************************************************************************
 *
 *  mf.c:  Portable IBIS-2 module.
 *
 * Blurb from the FORTRAN code:
 *
  MF ALLOWS THE USER TO CREATE FORTRAN LIKE EXPRESSIONS TO PERFORM
  GENERAL MATHEMATICAL OPERATIONS ON ONE OR MORE IBIS INTERFACE FILE
  THE EXPRESSIONS ARE CODED IN A PARAMETER STRING.  THE STRING IS
  INTERPRETED TO DETERMINE THE INPUT AND OUTPUT COLUMNS AND
  OPERATIONS TO BE PERFORMED.  THE VARIABLES REPRESENTING A DATA
  COLUMN ARE WRITTEN AS KEYWORDS TO ALLOW THE USER FLEXIBILITY IN WRITI
  THE EXPRESSION.  THE FUNCTIONS AVAILABLE ARE: SQRT,ALOG,ALOG10,
  AINT, SIN, COS, TAN, ASIN, ACOS, ATAN, ATAN2, ABS, MIN OR AMIN1,
  MAX OR AMAX1, MOD OR AMOD, ALONG WITH STANDARD BINARY OPERATIONS
  +-* / AND ** AND LOGIC OPERATIONS .AND. .OR. .XOR. AND .NOT.
  ALL OPERATIONS OPERATE AS IN FORTRAN IV WITH THE EXCEPTION OF MIN AND
  MAX WHICH ARE BINARY ONLY.  A SPECIAL FEATURE ALLOWS FOR THE CALCULAT
  OF COLUMN STATISTICS.

  USER PARAMETERS:

  FUNCTION,"ARITHMETIC FUNCTION STRING" - THE STRING SPECIFIES THE FUNC
            BE PERFORMED.

 Revision history

  11/13/94     ndr    Added Graphics capability, "POLY" variable to
  				Obsolete old "GF" function.

  1/5/94       ndr    Ported to Unix, with IBIS-2 library calls.

  1/5/87       ejb    fixed bug in sci. not. conversion that
                      mistook '.E'  for a '.EQ.' logical operation

  12/6/87      ejb    fixed to convert to upper case before
                      parsing and compiling the functions
                      also changed CVSCIN to do Sci. Not. conversion
                      for a more general case

 *************************************************************************/
 
#include "vicmain_c"
#include "ibisfile.h"
#include <ctype.h>
#include <math.h>

typedef enum {
        OpNone=0,
	OpFunction,
	OpMean,
	OpSigma,
	OpMin,
	OpMax,
	OpFirst,
	OpSum,
	OpRunSum,
	OpRunDiff,
	OpEnd
} optype;

static char *OpNames[] = {
	"xxx",
	"xxx",
	"mean",
	"sigma",
	"min",
	"max",
	"first",
	"sum",
	"rsum",
	"rdiff",
	"xxx"
};

#define MAXSTRING  1024
#define MAXCOL 1024
#define NROWS 1024
#define MAXPOLYS 100000L

static int indexcol=0;
static int polycol=0;
static int graphics_mode=0;
static int *zero_rows=(int *)0;

main44()
{
	int num_funcs;
	int function;
	int status;
	int unit;
	int ibis;
	int gr1dim,count; /* dimension for GRAPHICS-1 */
	
	zvpcnt("function",&num_funcs);
	zvunit(&unit,"inp",1,0);
	zvp("gr1dim",&gr1dim,&count);
	graphics_mode = zvptst("SKIP"); /* skip zeroes */
	
	status = IBISFileOpen(unit,&ibis,IMODE_UPDATE,gr1dim,0,0,0);
	if (status != 1) IBISSignalU(unit,status,1);

	if (graphics_mode) find_zeroes(ibis);
	
	for (function=0;function<num_funcs;function++)
		process_function(ibis,function);
	
	status = IBISFileClose(ibis,0);
	if (status != 1) IBISSignalU(unit,status,1);
}

process_function(ibis,function)
int ibis;
int function;
{
	char instring[MAXSTRING+1];
	char fstring[MAXSTRING+1];
	int incol[MAXCOL],outcol,opnum,ncol,status;

	zvpone("function",instring,function+1,MAXSTRING);
	parse_function_string(instring,fstring,incol,&ncol,&outcol,&opnum);

	status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,IFMT_REAL,outcol);
	if (status!=1) IBISSignal(ibis,status,1);
	
	if (opnum==OpFunction) 
		do_function_op( ibis, fstring, incol, ncol, outcol);
	else
		do_column_op( ibis, opnum,incol[0],outcol );
}

/* This routine creates a list of all zero-rows */

find_zeroes(ibis)
int ibis;
{
	int record,status,nc,nr,row;
	int cols[MAXCOL],count,i,*rptr,*endptr;
	float rval[MAXCOL];
	
	if (zero_rows) return;

	zero_rows = (int *)calloc(1L, MAXPOLYS * sizeof(int));
	if (!zero_rows)
	{
		zvmessage("Error allocating space for polygon array"," ");
		zabend();
	}
	rptr = zero_rows;
	endptr = rptr + MAXPOLYS;
	
	status = IBISFileGet(ibis,IFILE_NR,&nr,1,1,0);
	if (status!=1) IBISSignal(ibis,status,1);
	status = IBISFileGet(ibis,IFILE_NC,&nc,1,1,0);
	if (status!=1) IBISSignal(ibis,status,1);
	
	/* Check for new Graphics groups, if any */
	count = IBISColumnFind(ibis,ITYPE_ANY,"C_POSITION & C_ROOT",cols,1,0);
	if (count == 0)
	{
		/* find all non-ASCII columns */
		for (i=0;i<nc;i++) cols[i] = i+1;
		IBISGroupNew(ibis,ITYPE_LOCAL,"$all",cols,nc,0);
		count = IBISColumnFind(ibis,ITYPE_ANY,"$all - format:ASCII",cols,1,0);
	}
	
	if (count <=0)
	{
		zvmessage("Failed to find any non-ASCII columns in file", " ");
		zabend();
	}
	
	status = IBISRecordOpen(ibis,&record,0,cols,count,IFMT_REAL);
	if (status < 0) IBISSignal(ibis,status,1);
	
	for (row = 1; row <= nr; row++)
	{
		status = IBISRecordRead(record,rval,row);
		if (status < 0) IBISSignal(ibis,status,1);
		
		for (i=0;i<nc;i++)
			if (rval[i] != 0.0) break;
		if (i==nc)
		{
			if (rptr==endptr)
			{
				zvmessage("Max # polygons exceeded"," ");
				zabend();
			}
			*rptr++ = row;
		}
	}
	
	IBISRecordClose(record);
}

/*****************************************************************************
 *****************************************************************************
 *                        String Parsing Section                             *
 *****************************************************************************
 *****************************************************************************/


/*
 *  Grab the next 'Cnnn' or 'Xnnn' expression and length.
 *  The C or X must the first alpha encountered (or else AMAX1
 *  would be mis-interpreted).
 */
char *nextcolnum(string,val,len)
char *string;
int *val;
int *len;
{
	int length;
	char *str;
	int found=0;
	
	while (!found && *string)
	{
		while (!isalpha(*string) && *string) string++;
		if (!*string) goto bad;
		
		switch (*string++)
		{
			case 'c':
			case 'x':
				found = isdigit(*string); 
				if (found) break;
				/* else fall through */
			default:
				/* skip rest of alpha word and move on */
				while (isalpha(*string)) string++;
				continue; 
		}
	}
	if (!found) goto bad;
	
	/* If we got here, we found a good variable */
	
	*val = atoi(string);
	length=0;
	for (str=string; isdigit(*str); str++) length++;
	*len = length;
	
	return string;
bad:
	*val=0;
	return (char *)0;
	
}

/*
 * Determine if string is a local #COMMAND format
 * function (and what type), or else a KNUTH function string.
 */
 
int function_type(instring)
char *instring;
{
	int op,opfound,len;
	char *str;
	
	op = OpFunction;
	str = (char *)strchr(instring,'#');
	if (str)
	{
		str++;
		opfound=0;
		/* determine which column operation is flagged */
		for (op=OpMean;op<OpEnd;op++)
		{
			len = strlen(OpNames[op]);
			if (!strncmp(OpNames[op],str,len))
			{
				opfound = op;
				break;
			}
		}
		return opfound;
	}

	return OpFunction;
}


parse_function_string(instring,fstring,incol,ncol,outcol,opnum)
char *instring;
char *fstring;
int *incol;
int *ncol;
int *outcol;
int *opnum;
{
	char *outstr;
	char *str;
	int len;
	

	/* convert to lowercase */
	for (str=instring;*str;str++) *str = tolower(*str);
	
	/* get output column number */
	instring = nextcolnum(instring, outcol, &len);
	if (!instring) goto bad;
	instring += len;
	while (isspace(*instring)) instring++;
	
	/* scan past '=' */
	if (*instring++ != '=') goto bad;
	while (isspace(*instring)) instring++;

	/* Determine whether this is a knuth or #local function */
	*opnum = function_type(instring);
	if (!*opnum) goto bad;

	convert_fstring(instring,fstring,incol,ncol);
	
	indexcol = replace_variable(fstring,"index",*ncol+1);
	polycol = replace_variable(fstring,"poly",*ncol+2);
		
	return 1;
bad:
	zvmessage("*** Bad Function String***"," ");
	zabend();
}

replace_variable(fstring,varname,column)
char *fstring;
char *varname;
int column;
{
	char colstring[30];
	char *str;
	int namelen=strlen(varname);
	int foundcol=0;

	/* convert varname to column indexin */
	for (str = fstring; *str; str++)
	{
		if (!strncmp(str,varname,namelen))
		{
			if (!foundcol)
			{
				foundcol = column;
				sprintf(colstring,"c%-d        ",column);
			}
			strncpy(str,colstring,namelen);
			str += namelen-1;
		}
	}
	return foundcol;
}

convert_fstring(instring,fstring,incol,ncol)
char *instring;
char *fstring;
int *incol;
int *ncol;
{
	int col;
	int len;
	int ncols;
	int colnum[MAXCOL];
	char *outstr;
	char *str;
	char *next;

	memset(colnum,0,sizeof(colnum));

	/* scan and count Cnnn columns and convert */
	str = instring;
	outstr = fstring;
	ncols = 0;
	while (*str)
	{
		/* find next col number */
		next = nextcolnum(str, &col,&len);
		if (next)
		{
			/* copy everything up to next */
			while (str < next) *outstr++ = *str++;
			
			/* skip Cnnn number */
			str += len;
			
			/* check for new column reference */
			if (!colnum[col])
			{
				incol[ncols] = col;
				ncols++;
				colnum[col] = ncols;
			}
			
			/* write out new column number */
			sprintf(outstr,"%-d",colnum[col]);
			outstr += strlen(outstr);
		}
		else while (*str) *outstr++ = *str++;
	}
	*outstr='\0'; /* just to be safe */
	*ncol=ncols;
}

/*****************************************************************************
 *****************************************************************************
 *                        Knuth Function Processing                          *
 *****************************************************************************
 *****************************************************************************/


do_function_op(ibis, fstring, incol, ncol, outcol)
int ibis;
char *fstring;
int *incol;
int ncol;
int *outcol;
{
	int ier,nrow,row,outrow,rows_now;
	int poly=0;
	int status;
	int record;
	int skip;
	int was_zero=1;
	int gmode = graphics_mode || polycol;
	int next_zero,*zptr;
	char message[80];
	float fbuf[300];
	float outbuf[NROWS];
	
	status = IBISFileGet(ibis,IFILE_NR,&nrow,1,1,0);
	if (status!=1) IBISSignal(ibis,status,1);
	
	if (gmode)
	{
		if (!zero_rows) find_zeroes(ibis);
		zptr = zero_rows;
		next_zero = *zptr;
	}
	
	ier=zknuth( fstring, fbuf);
        if (ier)
	{
	   switch (ier)
	   {
		case 1: sprintf(message,"LINE,SAMP, or BAND in Function string:"); break;
		case 2: sprintf(message,"Bad Function:"); break;
		case 3: sprintf(message,"Evaluation error:"); break;
	   }
	   zvmessage(message," ");
	   zvmessage(fstring," ");
	   zabend();
	}
	if (ncol>0)
	{
		status = IBISRecordOpen(ibis,&record,0,incol,ncol,IFMT_REAL);
		if (status!=1) IBISSignal(ibis,status,1);
	}
	
	outrow=1;
	rows_now=0;
	for (row=1;row<=nrow;row++)
	{
		skip = (gmode && row==next_zero);
		if (skip)
		{
			/* outbuf is zero */
			next_zero = *++zptr;
			was_zero = 1;
			outbuf[rows_now]=0.0;
		}
		else /* need to compute outbuf */
		{
		   if (ncol>0)
		   {
			status=IBISRecordRead(record,fbuf,row);
			if (status!=1) IBISSignal(ibis,status,1);
		   }

		   if (indexcol) fbuf[indexcol-1] = row; 
		   if (polycol)
		   {
			if (was_zero) poly++;
			fbuf[polycol-1] = poly;
			was_zero = 0;
		   }
		
		   zxknuth(fbuf,outbuf+rows_now);
		}			
		rows_now++;
		if (rows_now==NROWS)
		{
			status=IBISColumnWrite(ibis,outbuf,outcol,outrow,rows_now);
			if (status!=1) IBISSignal(ibis,status,1);
			outrow += rows_now;
			rows_now = 0;
		}
	}
	if (rows_now) /* some rows left over */
	{
		status=IBISColumnWrite(ibis,outbuf,outcol,outrow,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
	}
	
	if (ncol>0) IBISRecordClose(record);
}

int check_zero(buf,ncol)
float *buf;
int ncol;
{
	register int i;
	
	for (i=0;i<ncol;i++)
		if (*buf++) return 0;
	
	return 1;
}

/*****************************************************************************
 *****************************************************************************
 *                        #Column Operation Processing                       *
 *****************************************************************************
 *****************************************************************************/

 
do_column_op(ibis, opnum,incol,outcol )
int ibis;
int opnum;
int *incol;
int outcol;
{
	int nrow;
	int status;
	float mean,sig,minval,maxval,first;
	
	status = IBISFileGet(ibis,IFILE_NR,&nrow,1,1,0);
	if (status!=1) IBISSignal(ibis,status,1);
	
	status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,IFMT_REAL,incol);
	if (status!=1) IBISSignal(ibis,status,1);

	/* preprocessing step */
	switch (opnum)
	{
		case OpMean: case OpSigma: case OpSum:
			compute_stats(ibis,incol,nrow,&mean,&sig);
			break;
		case OpMin: case OpMax:
			compute_minmax(ibis,incol,nrow,&minval,&maxval);
			break;
		case OpFirst:
			status=IBISColumnRead(ibis,&first,incol,1,1);
			if (status!=1) IBISSignal(ibis,status,1);
			break;
	}

	/* processing step */
	switch (opnum)
	{
		case OpMean: write_column_value(ibis,outcol,nrow,mean); break;
		case OpSigma:write_column_value(ibis,outcol,nrow,sig); break;
		case OpSum:  write_column_value(ibis,outcol,nrow,mean*nrow); break;
		case OpMin:  write_column_value(ibis,outcol,nrow,minval); break;
		case OpMax:  write_column_value(ibis,outcol,nrow,maxval); break;
		case OpFirst:write_column_value(ibis,outcol,nrow,first); break;
		
		case OpRunSum: write_column_sum(ibis,incol,outcol,nrow); break;
		case OpRunDiff:write_column_diff(ibis,incol,outcol,nrow); break;
	}
}


compute_stats(ibis,incol,nrow,mean,sig)
int ibis;
int incol;
int nrow;
float *mean;
float *sig;
{
	register float meanval,diff,sigval=0,sum=0;
	float inbuf[NROWS],denom;
	int row,rows_left,rows_now,thisrow;
	int status;
	
	/* first loop -- compute sum and mean value */
	rows_left = nrow;
	for (row=1;row<=nrow;row+=rows_now)
	{
		rows_now = (rows_left > NROWS) ? NROWS : rows_left;
		status = IBISColumnRead(ibis,inbuf,incol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		
		for (thisrow=0;thisrow<rows_now;thisrow++)
			sum += inbuf[thisrow];
		rows_left -= rows_now;
	}
	meanval = sum/nrow;
	
	/* second loop -- compute sigma */
	rows_left = nrow;
	for (row=1;row<=nrow;row+=rows_now)
	{
		rows_now = (rows_left > NROWS) ? NROWS : rows_left;
		status = IBISColumnRead(ibis,inbuf,incol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		
		for (thisrow=0;thisrow<rows_now;thisrow++)
		{
			diff = inbuf[thisrow]-meanval;
			sigval += diff*diff;
		}
		rows_left -= rows_now;
	}
	sigval = sqrt(sigval/((nrow<2) ? 1 : nrow-1));
	
	*mean = meanval;
	*sig = sigval;
}

compute_minmax(ibis,incol,nrow,minval,maxval)
int ibis;
int incol;
int nrow;
float *minval;
float *maxval;
{
	register float minv,maxv;
	float inbuf[NROWS];
	int row,rows_left,rows_now,thisrow;
	int first=1;
	int status;
	
	rows_left = nrow;
	for (row=1;row<=nrow;row+=rows_now)
	{
		rows_now = (rows_left > NROWS) ? NROWS : rows_left;
		status = IBISColumnRead(ibis,inbuf,incol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		
		if (first) {minv=maxv=inbuf[0];first=0;}
		
		for (thisrow=0;thisrow<rows_now;thisrow++)
		{
			if (inbuf[thisrow] < minv)
				minv = inbuf[thisrow];
			else if (inbuf[thisrow] > maxv)
				maxv = inbuf[thisrow];
		}
		rows_left -= rows_now;
	}
	*minval = minv;
	*maxval = maxv;
}

write_column_value(ibis,outcol,nrow,value)
int ibis;
int outcol;
int nrow;
float value;
{
	register float minv,maxv;
	float outbuf[NROWS];
	int row,rows_left,rows_now,thisrow;
	int first=1;
	int status;
	
	/* same value, over and over again */
	for (row=0;row<NROWS;row++) outbuf[row]=value;

	rows_left = nrow;
	for (row=1;row<=nrow;row+=rows_now)
	{
		rows_now = (rows_left > NROWS) ? NROWS : rows_left;
		status = IBISColumnWrite(ibis,outbuf,outcol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		rows_left -= rows_now;
	}
}

write_column_sum(ibis,incol,outcol,nrow)
int ibis;
int incol;
int outcol;
int nrow;
{
	register float sum=0;
	float inbuf[NROWS];
	float outbuf[NROWS];
	int row,rows_left,rows_now,thisrow;
	int status;
	
	rows_left = nrow;
	for (row=1;row<=nrow;row+=rows_now)
	{
		rows_now = (rows_left > NROWS) ? NROWS : rows_left;
		status = IBISColumnRead(ibis,inbuf,incol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		
		for (thisrow=0;thisrow<rows_now;thisrow++)
		{
			sum += inbuf[thisrow];
			outbuf[thisrow] = sum;
		}
		status = IBISColumnWrite(ibis,outbuf,outcol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		rows_left -= rows_now;
	}
}


write_column_diff(ibis,incol,outcol,nrow)
int ibis;
int incol;
int outcol;
int nrow;
{
	register float prev=0,diff;
	float inbuf[NROWS];
	float outbuf[NROWS];
	int row,rows_left,rows_now,thisrow;
	int status;
	
	rows_left = nrow;
	for (row=1;row<=nrow;row+=rows_now)
	{
		rows_now = (rows_left > NROWS) ? NROWS : rows_left;
		status = IBISColumnRead(ibis,inbuf,incol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		
		for (thisrow=0;thisrow<rows_now;thisrow++)
		{
			diff = inbuf[thisrow] - prev;
			prev = inbuf[thisrow];
			outbuf[thisrow] = diff;
		}
		status = IBISColumnWrite(ibis,outbuf,outcol,row,rows_now);
		if (status!=1) IBISSignal(ibis,status,1);
		rows_left -= rows_now;
	}
}



$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mf.imake
#define PROGRAM mf
#define USES_C
#define MAIN_LANG_C
#define MODULE_LIST mf.c
#define LIB_TAE
#define LIB_RTL
#define LIB_P2SUB

$ Return
$!#############################################################################
$PDF_File:
$ create mf.pdf
PROCESS        HELP=*
! MF PDF - VICAR/IBIS SOFTWARE
PARM INP TYPE=STRING
PARM FUNCTION TYPE=(STRING,200),COUNT=(1:50)
PARM GR1DIM TYPE=INTEGER COUNT=1 DEFAULT=0
PARM ZEROES  TYPE=KEYWORD VALID=(SKIP,INCLUDE) DEF=INCLUDE
END-PROC
.TITLE
VICAR/IBIS Program MF
.HELP
PURPOSE

     MF   allows   the  user  to  create  FORTRAN or C -like 
     expressions to perform general mathematical operations on 
     one  or more IBIS/graphics file columns.   The  expressions 
     are  written as a parameter string.   The parameter  is 
     interpreted  to determine the input and output  columns 
     and   operations  to  be  performed.    The   variables 
     representing  a data column are written as keywords  to 
     allow  the user flexibility in writing the  expression.  
     The functions available are:  SQRT, ALOG, ALOG10, AINT, 
     SIN,  COS TAN,  ASIN,  ACOS,  ATAN,  ATAN2, ABS, MIN or 
     AMIN1,  MAX  or  AMAX1,  and MOD or  AMOD,  along  with 
     standard binary operations +,  -,  *,  /, **, and logic 
     operations,   .AND.,   .OR.,   .XOR.,  and  .NOT.   All 
     operations operate as in FORTRAN IV and C.   A  special 
     feature   allows   for   the  calculation   of   column 
     statistics.

TAE COMMAND LINE FORMAT

     MF INP=int PARAMS

     where

     int                 is a random access file.  Since it
                         is used for both input and  output, 
                         no output file is specified.

     PARAMS              is   a  standard  VICAR   parameter 
                         field.
.PAGE
METHOD

     MF performs arithmetic operations on an interface file.  
     The  program  uses  two  library  routines KNUTH  and 
     XKNUTH,   to   compile  and  interpret   FORTRAN or C  like 
     expressions  entered by the parameters in an expression 
     such as:

                     C35 = (100*C34)/C4

     In this expression,  C34 and C4 are the input  columns.  
     KNUTH    compiles   the   expression   into    machine 
     instructions.   The  expression is applied to the input 
     column in XKNUTH to produce the output column, C35. For
     compatibility with program GF, the variable names X5,
     etc, may be used in place of C5, etc.


RESTRICTIONS

     Maximum number of columns in one execution is 50.
.PAGE
EXAMPLE

     MF INP=FILE.INT FUNCTION=("C5 = C2/C3+100+SQRT(C2)")

     In this example,  C2 is divided by C3 and added to  100 
     plus the square root of C2.   The results are placed in 
     C5.  Further examples of allowable functions follow:

                FUNCTION=("C5 = !(C3  || C2)")

     logical   operations  are  performed  bitwise  on   the 
     operands. The  logical values T and F are converted to 1.  and 0. 
     for storage in column C5

                FUNCTION=("X5 = X3.LE.INDEX")

     Column 5 is 1.0 if column 3 has a value < its row value (INDEX).
     
                FUNCTION=("X5 = POLY * 2")

     the operator POLY returns the current polygon number,
     where polygons are delimited by rows of all zero values.

                FUNCTION=("C5 = #MEAN(C3)")

     In this example, the mean of column 3 is calculated and 
     that  value is placed in every row entry in  column  5.  
     This  operation  is different than the  arithmetic  and 
     logic operations given earlier because it operates on a 
     vertical  column instead of horizontally across a  row.  
     These  operations  cannot  be  used  in  an  arithmetic 
     expression  such as C5 = #MEAN(C3)*10.   The  allowable 
     column operations are:

                       C2 = #MEAN(C4)
                       C2 = #SIGMA(C4)
                       C2 = #MIN(C4)
                       C2 = #MAX(C4)
                       C2 = #FIRST(C4)
                       C2 = #SUM(C4)
                       C2 = #RSUM(C4) (running sum)
                       C2 = #RDIFF(C4) (running diff)
					   
	See the FUNCTION help for more examples.

Original Programmer:  A. L. Zobrist, 15 December 1976

Cognizant Programmer:  N. D. Ritter

Revision:  3,        13 November 1994 NDR - Graphics Functions

.LEVEL1
.VARIABLE INP
Input IBIS interface file
.VARIABLE FUNCTION
Specifies function and columns
.VARIABLE GR1DIM
Dimension (Graphics-1 only)
.VARIABLE ZEROES
Process rows with all zeroes?
.LEVEL2
.VARIABLE INP
                        Specifies IBIS interface file. There
                        is no output file. Results of MF are
                        written in INP. Graphics-1 files may
			also be processed, using the "GR1DIM"
			parameter.
.VARIABLE FUNCTION
     FUNCTION            
	 
	 this keyword specifies the function to be applied,  and the columns  to 
	 which  it applies.   Functions  are delimited  by  double  quotes.  "C" 
	 followed  by a number indicates the  columns  used as input  or  output.  
	 Up  to nineteen columns can be used  as  input.   The term INDEX can  be 
	 used  in the arithmetic  expression to  introduce row number  into  the 
	 calculation   (see  examples).    A   special notation is used for column 
	 statistics   (see  examples).    By using   ","   separator,    several 
	 functions can be placed in one call (i.e. FUNC=("C1=INDEX","C2=C1*C1").
	 Maximum number of columns in one execution is 50. 
	 
	 Also, to support GRAPHICS "GF" capabilities, the "POLY" operator may
	 also be used. This function returns the polygon number of the
	 current row (which starts at 1), or a zero if end-of-polygon. To
	 further support MF as a replacement for GF, the columns may be
	 referenced by "Xnnn" rather than "Cnnn" to indicate coordinate value.
	 
.VARIABLE GR1DIM
GRAPHICS-1 files do not have any explicit dimensions in the
file label. This allows the user to properly dimension a
graphics file so that it may be used by this program.
IBIS-2 format graphics files do not need or use this.
.VARIABLE ZEROES
This keyword indicates whether to SKIP or INCLUDE rows in which all
values are zero. This is primarily used for GRAPHICS files,
in which an all-zero row signals "pen-up" or next polygon.
NOTE: the #MEAN type column operators do not honor this keyword,
and will write a value in every row. The POLY function
always skips zeroes, while the INDEX function honors this
keyword.

.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmf.pdf
procedure
refgbl $autousage
body
let $autousage="none"

! Create a new IBIS-2  file, and throw in an ASCII column
!
ibis-gen a nc=5 nr=10 format=(FULL,REAL,DOUB,REAL,A4)
!
! Test various KNUTH functions with C and FORTRAN constructs
!
mf inp=a function=("c1=index","c2 = c1*c1" , "c3 = sin(c1/10)*(c1.LT.5)" )
ibis-list a
!
! Test Column Operations
mf inp=a function=("c2=#mean(c1)","c3=#sigma(c1)","c4=#min(c1)")
ibis-list a
mf inp=a function=("c2=#max(c1)","c3=#first(c1)","c4=#sum(c1)")
ibis-list a
mf inp=a function=("c2=#rsum(c1)","c3=#rdiff(c1)")
ibis-list a

! Create an old IBIS-1  file and test same options
!
ibis-gen a nc=5 nr=10 'IBIS-1 'COLUMN
!
! Test various KNUTH functions with C and FORTRAN constructs
!
mf inp=a function=("c1=index","c2 = c1*c1" , "c3 = sin(c1/10)*(c1.LT.5)" )
ibis-list a
!
! Test Column Operations
mf inp=a function=("c2=#mean(c1)","c3=#sigma(c1)","c4=#min(c1)")
ibis-list a
mf inp=a function=("c2=#max(c1)","c3=#first(c1)","c4=#sum(c1)")
ibis-list a
mf inp=a function=("c2=#rsum(c1)","c3=#rdiff(c1)")
ibis-list a

! Create an old GRAPHICS-1  file and test same options
! (must use GR1DIM, as dimension is not built-into file)
!
ibis-gen a nc=5 nr=10 'IBIS-1 'ROW
!
! Test various KNUTH functions with C and FORTRAN constructs
!
mf inp=a function=("c1=index","c2 = c1*c1" , "c3 = sin(c1/10)*(c1.LT.5)" ) GR1DIM=5
ibis-list a GR1DIM=5
!
! Test Column Operations
mf inp=a function=("c2=#mean(c1)","c3=#sigma(c1)","c4=#min(c1)") GR1DIM=5
ibis-list a GR1DIM=5
mf inp=a function=("c2=#max(c1)","c3=#first(c1)","c4=#sum(c1)") GR1DIM=5
ibis-list a GR1DIM=5
mf inp=a function=("c2=#rsum(c1)","c3=#rdiff(c1)") GR1DIM=5
ibis-list a GR1DIM=5

!Test new graphics handling, including Xnnn parsing:
ibis-gen a nc=3 nr=12 'IBIS-1 'ROW datacol=(1,2) +
  data=(1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,0)

mf inp=a function=("x3=poly*2") GR1DIM=3
ibis-list a GR1DIM=3 nr=12

mf inp=a function=("x3=AMAX1(x1,x2)+1") GR1DIM=3 'skip
ibis-list a GR1DIM=3 nr=12

mf inp=a function=("x3=x1+1") GR1DIM=3
ibis-list a GR1DIM=3 nr=12

mf inp=a function=("x3=INDEX") GR1DIM=3 'skip
ibis-list a GR1DIM=3 nr=12

end-proc
$ Return
$!#############################################################################
