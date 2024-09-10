#include <stdio.h>
#include <string.h>
#include "taeconf.inp"
#include "parblk.inc"
#include "pgminc.inc"
#include "vicmain_c"

/*  Revision History                                                    */
/*    9-86   SP   changed fscanf to sscanf and changed                  */
/*                format specifier from "%5d" to "10%d"                 */
/*                to allow users to modify the first record in file.    */
/*                added code to check for first record having a         */
/*                different length than expected and to handle this.    */
/*    9-86   SP   modified to output to a TAE variable the number of the*/
/*                file (as done in program CNT) per Charlie Avis.       */
/*                note that this change the calling sequence of NXT by  */
/*                adding another required parameter, NXTFIL.            */
/*    2-87   SP   Added code to handle  NO RECORDS in SRCH file.        */
/*    6-94   SVH  Ported to UNIX - Steve Hwan				*/
/*    8-97   RRD  Added first fseek call after #else to fix position    */
/*    7-1998 TIH  Changed constant value with FILENAME_LEN to fix       */
/*                AR-100461                                             */

void main44(void)
{

#define MAXR1    80             /* buffer length for first record.      */
#define LENGTH1  17             /* number of chars in first record      */
#define FILENAME_LEN	255
   
    FILE *unit, *ounit;	        /* C I/O control block			*/
    int length;                 /* first record length                  */
    int count,def;		/* COUNT, DEF for XVPARM		*/
    struct PARBLK out_parb;	/* Local parameter block for XQ calls	*/
    int next_file;		/* Next file to be retrieved		*/
    int stat,i;			/* Return status ind, increment var	*/
    int j;                      /* loop variable, first record length   */
    int tape_pos;		/* File position number for tapes	*/
    int is_tape;		/* 1 if file is tape; 0 if not		*/
    char c;
    char rec1[MAXR1];           /* buffer for first record in file.     */
    int rec_tmp[MAXR1];           /* buffer for first record in file.     */
    char file_name[FILENAME_LEN];	/* name of file to reset	*/
    char new_name[FILENAME_LEN];	/* process specific  file_name	*/
    char output_name[FILENAME_LEN];	/* Name of file for output	*/
    char output_name_final[FILENAME_LEN];	/* Name of file for output */
    char *name_ptr;
    char msg[80];		/* Message buffer for zvmessage		*/
    long off_dst,off_src;	/* used for fseek */
    int c_buf_size, c_buf_indx;

    zvparm("INPUT",file_name,&count,&def,1,0);
    zvfilename(file_name,new_name,FILENAME_LEN);

    unit = fopen(new_name,"r+");	/* Open the file		*/
    if (unit == NULL)
    {
	zvmessage(" File open error.  Check specification.","");
	zabend();
    }

     /*   Load first record into rec1 buffer.                           */
    for (i=0; (rec1[i] = getc(unit) ) != '\n' && i < MAXR1-1; ++i)
         ;
    length = i;
    /* Start source offset at character right after the first \n. */
    off_src=i+1;

    if ( strncmp(rec1,"NO RECORDS",10) == 0 )
    {
       stat = EOF;                   /* case of no records found by SRCH */
    } else {
      stat = sscanf(rec1, "NEXT FILE = %10d", &next_file);
      if (stat != 1)
      {
  	zvmessage(" Error reading number of next file","");
  	zvmessage(" First line of input file must be of the form:","");
  	zvmessage(" NEXT FILE = file_number","");
  	zabend();
      }
  
      if (next_file <= 0) next_file = 1;	/* Protect from user error */
  
      for (i = 1; i <= next_file; i++)
      {
  	stat = fscanf(unit,"%s",output_name);
  	if (stat == EOF) break;
  	if (stat != 1)
  	{
  	    zvmessage(" Read error on input","");
  	    zabend();
  	}
      }
    }

    if (stat == EOF)
    {
	strcpy(output_name_final,"END_OF_FILE");
	tape_pos = 0;
	is_tape = 0;
	zvmessage(" NXT encountered end of file","");
    }
    else if (output_name[0] == '?')	/* if tape is used */
    {
	for (i = 1; output_name[i] != '/'; i++)
	  ;
	strncpy(output_name_final,&output_name[1],i-1);
	output_name_final[i-1] = '\0';
	sscanf(&output_name[i+1],"%d",&tape_pos);
	is_tape = 1;
	sprintf(msg," Output %d is tape %.*s file %d",
		next_file,i-1,&output_name[1],tape_pos);
        zvmessage(msg,"");
    }
    else				/* situation normal */
    {
	strcpy(output_name_final,output_name);
	tape_pos = 0;
	is_tape = 0;
	sprintf(msg," Output %d is %s",next_file,output_name);
	zvmessage(msg,"");
    }
	
    q_init(&out_parb,500,P_ABORT);	/* Initialize a local par block	*/

/* output variables ONAM, ISTAPE, and TAPEPOS to vblock			*/
    name_ptr=output_name_final;
    q_string(&out_parb,"ONAM",1,&name_ptr,P_ADD);
    q_intg(&out_parb,"ISTAPE",1,&is_tape,P_ADD);
    q_intg(&out_parb,"TAPEPOS",1,&tape_pos,P_ADD);
    q_intg(&out_parb,"NXTFIL",1,&next_file,P_ADD);

    zvq_out(&out_parb);		/* Output vblock to TM		*/

    next_file++;

    if ( strncmp(output_name_final,"END_OF_FILE",11) == 0)
    {
      fclose(unit);                     /* skip update if at end of file*/
    }
    else if ( length >= LENGTH1 )      /* If no change in record length just */
    {                             /* change first record.               */
      rewind(unit);				/* Write it out		*/
      sprintf(rec1,"NEXT FILE = ");
      for(i=0; i< (length-LENGTH1); i++)
        strcat(rec1," ");
      sprintf(&rec1[12+length-LENGTH1],"%5d\n",next_file);
      stat = fprintf(unit,rec1);
      if (stat != length+1)
      {
 	zvmessage(" Write (update) error on input","");
	zabend();
      }
      fclose(unit);
    } else { 
/* sorry guys.  I tried to get VMS to use the same code as UNIX, but */
/* it just refused, and i tried 3 different approaches, so we're stuck */
/* with 2 seperate routines. */
#if VMS_OS
      /* open up another file and just copy into it - the reason this */
      /* works is because VMS just opens up another version. */
      ounit = fopen(new_name,"w");
       if (ounit == NULL)
      {
	zvmessage(" File open error.  Check specification.","");
	zabend();
      }
      rewind(unit);
      for(i=0; getc(unit)!= '\n' && i < MAXR1 -1; i++);
      sprintf(rec1,"NEXT FILE = %5d\n",next_file);

      for (i=0; i <= LENGTH1; ++i)
        putc( rec1[i], ounit );          /* write out first record. */

      while ( (c=getc(unit)) != EOF )
        putc( c, ounit);                 /* copy the rest of the file. */

      fclose(unit);
      fclose(ounit);

#else
      /* determine size needed and fill circular buffer */
      c_buf_size = LENGTH1-length;
      c_buf_indx = 0;
      fseek(unit,off_src,0);    /* fscanf moved position so go back */  
      for(j=0; (j<c_buf_size) && (rec_tmp[c_buf_indx] = getc(unit)) != EOF;
		j++) {
        c_buf_indx = (c_buf_indx+1)%(c_buf_size+1);
        off_src++;
      }
      /* off_scr is now sitting at the first read element (0) which will */
      /* be the next written over. */

      rewind(unit);
      /* write 1st record */
      sprintf(rec1,"NEXT FILE = %5d\n",next_file);
      for (i=0; i <= LENGTH1; ++i)
        putc( rec1[i], unit );          /* write out first record. */
      off_dst = strlen(rec1);

      /* copy over remainder of file through circular buffer */
      /* until we hit end of file */
      fseek(unit,off_src,0);
      while ( (rec_tmp[c_buf_indx]=getc(unit)) != EOF )
      {
        fseek(unit,off_dst,0);
        /* write out oldest element */
        putc(rec_tmp[ (c_buf_indx+1) % (c_buf_size+1)] , unit);
        off_dst++;
        off_src++;
        c_buf_indx = (c_buf_indx+1)%(c_buf_size+1);
        fseek(unit,off_src,0);
      }

      /* wrap up the file */
      c_buf_indx = (c_buf_indx+1)%(c_buf_size+1);
      for(j=0; j<c_buf_size ; j++) {
        putc(rec_tmp[c_buf_indx],unit);
        c_buf_indx = (c_buf_indx+1)%(c_buf_size+1);
        off_dst++;
      }

      fclose(unit);
#endif
    }

}



