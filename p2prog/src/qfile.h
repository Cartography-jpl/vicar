/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* qfile.h   File structure definition
 *
 * Programmer: R. White     Date: 16 June 1992
 */

struct qfile {
    FILE *file;                 /* File descriptor pointer          */
    char *filename;             /* File name                        */
    int recordsize;             /* Record size (bytes)              */
    int bufsize;                /* Buffer size (bytes)              */
    unsigned char write;        /* Write flag (0 for readonly)      */
    unsigned char crrat;        /* CR attribute flag (0 for no CRs) */
    unsigned char *buffer;      /* Record buffer                    */
    int bptr;                   /* Current pointer in buffer        */
    };

#define QFILE struct qfile

/*
 * External procedures
 */
extern QFILE *qcreat();
extern QFILE *qopen();
extern void  qread();
extern void  qwrite();
extern void  fillbuff();
extern void  dumpbuff();
extern int   readint();
extern void  writeint();

/*
 * Macros to get and put characters to files
 * Usage:
 *     c = qgetc(qfile);
 *     qputc(c,qfile);
 */
#define qgetc(f) ((f->bptr<f->bufsize) ? f->buffer[f->bptr++]                  \
                                       : (fillbuff(f), f->buffer[f->bptr++]))

#define qputc(c,f) { if(f->bptr >= f->recordsize) dumpbuff(f);                 \
                     f->buffer[f->bptr++] = c; }
