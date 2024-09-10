/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* qread.c    Routines to read and write fixed-length record files as
 *            binary stream files.
 *
 * For VMS machines use qread_vms.c instead of these routines.  (The
 * standard VMS stream-I/O routines do not read the CD-ROM correctly in
 * some cases.)
 *
 * Summary of procedures:
 *
 * Opening and closing files:
 *
 * QFILE *qcreat(filename,recordsize,crrat)  Creates file filename.
 * QFILE *qopen( filename,recordsize,crrat)  Opens file filename.
 * void  qclose(qfile)                       Closes qfile.
 *
 * Stream I/O:
 *
 * void qread(qfile, buffer, n)              Reads next n bytes from qfile.
 * void qwrite(qfile, buffer, n)             Writes n bytes to qfile.
 * void fillbuff(qfile)                      Fills record buffer from qfile.
 * void dumpbuff(qfile)                      Dumps record buffer to qfile.
 * int  readint(qfile)                       Read a 4-byte integer from qfile.
 * void writeint(qfile,a)                    Write a 4-byte integer to qfile.
 *
 * All routines abort with an error message if there are problems.
 *
 * Programmer: R. White     Date: 16 June 1992
 */
#include <stdio.h>
#include <stdlib.h>
#include "qfile.h"

/*
 * ---------------- error exit ----------------
 */
static void
qerror(operation,qfile)
char  *operation;
QFILE *qfile;
{
    fprintf(stderr, "Error: %s failed for file %s\n",
        operation, qfile->filename);
    perror(operation);
    exit(-1);
}

/*
 * --------------- create a fixed-record-length file ---------------
 */
extern QFILE
*qcreat(filename,recordsize,crrat)
char *filename;
int  recordsize;        /* Record length in bytes                      */
int  crrat;             /* Carriage return attributes flag: 0 = no CRs */
{
QFILE *qfile;

    /*
     * Allocate memory for file access block and copy filename
     */
    qfile = (QFILE *) malloc(sizeof(QFILE));
    qfile->filename = filename;
    /*
     * Create the file
     */
    if ((qfile->file = fopen(filename,"w")) == NULL) {
            qerror("Create", qfile);
    }
    /*
     * File attributes
     */
    qfile->recordsize = recordsize;
    qfile->crrat = (crrat != 0);
    qfile->write = 1;
    /*
     * Create buffer, initialize pointer
     */
    qfile->bufsize = recordsize + qfile->crrat;
    qfile->buffer = (unsigned char *) malloc(qfile->bufsize);
    qfile->bptr = 0;
    /*
     * Return pointer to structure
     */
    return(qfile);
}

/*
 * --------------- open a fixed-record-length file ---------------
 */
extern QFILE
*qopen(filename,recordsize,crrat)
char *filename;
int  recordsize;
int  crrat;
{
QFILE *qfile;

    /*
     * Allocate memory for file access block and copy filename
     */
    qfile = (QFILE *) malloc(sizeof(QFILE));
    qfile->filename = filename;
    /*
     * Open the file
     */
    if ((qfile->file = fopen(filename,"r")) == NULL) {
            qerror("Open", qfile);
    }
    /*
     * File attributes
     */
    qfile->recordsize = recordsize;
    qfile->crrat = (crrat != 0);
    qfile->write = 0;
    /*
     * Create buffer, initialize pointer
     */
    qfile->bufsize = recordsize + qfile->crrat;
    qfile->buffer = (unsigned char *) malloc(qfile->bufsize);
    qfile->bptr = qfile->bufsize;
    /*
     * Return pointer to structure
     */
    return(qfile);
}

/*
 * --------------- Close file ---------------
 */
extern void
qclose(qfile)
QFILE *qfile;
{
    /*
     * If we're writing to file, dump partially full buffer
     */
    if (qfile->write) dumpbuff(qfile);
    /*
     * Close the file
     */
    fclose(qfile->file);
    /*
     * Free buffer and qfile structure
     */
    free(qfile->buffer);
    free(qfile);
    return;
}

/*
 * --------------- Fill buffer from next record ---------------
 */
extern void
fillbuff(qfile)
QFILE *qfile;
{
    qfile->bufsize = fread(qfile->buffer, 1, qfile->recordsize+qfile->crrat,
        qfile->file);
    qfile->bptr = 0;
    if (qfile->bufsize <= 0) {
        /*
         * End of file or error
         */
        qerror("Read", qfile);
    }
    /*
     * Remove the newline character if crrat is set
     */
    if (qfile->crrat) {
        if (qfile->buffer[qfile->bufsize-1] == '\n') {
            qfile->bufsize -= 1;
        } else {
            fprintf(stderr, "Error: no newline at end of record from %s\n",
                qfile->filename);
            fprintf(stderr, "Record='%.*s'\n",
                qfile->bufsize, qfile->buffer);
            exit(-1);
        }
    }
}

/*
 * --------------- Dump (full or partial) buffer to file ---------------
 */
extern void
dumpbuff(qfile)
QFILE *qfile;
{
    if (qfile->bptr > 0) {
        /*
         * Add newline if crrat is set
         */
        if (qfile->crrat) qfile->buffer[qfile->bptr++] = '\n';
        if (fwrite(qfile->buffer, 1, qfile->bptr, qfile->file) == 0) {
            qerror("Write",qfile);
        }
        qfile->bptr = 0;
    }
}

/*
 * --------------- Buffered input: Get next n bytes from file ---------------
 */
extern void
qread(qfile, buffer, n)
QFILE *qfile;
char  *buffer;
int   n;
{
int i;

    for (i=0; i<n; i++) {
        /*
         * Fill buffer if it is empty
         */
        if (qfile->bptr >= qfile->bufsize) fillbuff(qfile);
        buffer[i] = qfile->buffer[qfile->bptr++];
    }
}

/*
 * --------------- Buffered output: Put next n bytes to file ---------------
 */
extern void
qwrite(qfile, buffer, n)
QFILE *qfile;
char  *buffer;
int   n;
{
int i;

    for (i=0; i<n; i++) {
        /*
         * Dump buffer if it is full
         */
        if (qfile->bptr >= qfile->recordsize) dumpbuff(qfile);
        qfile->buffer[qfile->bptr++] = buffer[i];
    }
}

extern int
readint(infile)
QFILE *infile;
{
int a,i;
unsigned char b[4];

    /* Read integer A one byte at a time from infile.
     *
     * This is portable from Vax to Sun since it eliminates the
     * need for byte-swapping.
     */
    for (i=0; i<4; i++) b[i] = qgetc(infile);
    a = b[0];
    for (i=1; i<4; i++) a = (a<<8) + b[i];
    return(a);
}

extern void
writeint(outfile,a)
QFILE *outfile;
int a;
{
int i;
unsigned char b[4];

    /* Write integer A one byte at a time to outfile.
     *
     * This is portable from Vax to Sun since it eliminates the
     * need for byte-swapping.
     */
    for (i=3; i>=0; i--) {
        b[i] = a & 0xff;
        a >>= 8;
    }
    for (i=0; i<4; i++) qputc(b[i],outfile);
    return;
}
