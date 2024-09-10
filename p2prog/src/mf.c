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

void main44(void)
{
	int num_funcs;
	int function;
	int status;
	int unit;
	int ibis;
	int gr1dim,count; /* dimension for GRAPHICS-1 */
	
	zvpcnt("function",&num_funcs);
	zvunit(&unit,"inp",1, NULL);
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



