/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* hdecompress.c Return image from compressed image file created by hcompress
 *
 * Programmer: R. White     Date: 20 May 1991
 *
 * Modified to allowing smoothing during decompression, R. White, 14 April 1992
 * Modified to read VMS fixed-length files, R. White, 16 June 1992
 */

#include <stdio.h>
#include "qfile.h"

/*
 * if verbose is set to non-zero value, some information about
 * compression efficiency is printed on stderr
 */
int verbose;

extern void decode();
extern void undigitize();
extern void hinv();

extern void
hdecompress(a,nx,ny,filename,smooth)
int  **a;                   /* (*a)[nx][ny] is the image array              */
int  *nx;
int  *ny;                   /* Note that ny is the fast-varying dimension   */
char *filename;             /* Name of input file                           */
int  smooth;                /* 0 for no smoothing, else smooth in hinv      */
{
QFILE *infile;
int scale;

    /*
     * open input file: 512 byte records, no carriage control
     */
    infile = qopen(filename,512,0);
    decode(infile,a,nx,ny,&scale);      /* Read from infile and decode      */
                                        /* Returns address & size           */
    undigitize(*a,*nx,*ny,scale);       /* Un-Digitize                      */
    hinv(*a,*nx,*ny,smooth,scale);      /* Inverse H-transform              */
    if (verbose) {
        fprintf(stderr, "Image size (%d,%d)  Scale factor %d\n",
            *ny,*nx,scale);
    }
    qclose(infile);
}
