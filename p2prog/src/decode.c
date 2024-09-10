/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* decode.c     read codes from infile and construct array
 *
 * Programmer: R. White     Date: 9 May 1991
 *
 * Modified to read VMS fixed-length files, R. White, 16 June 1992
 */

#include <stdio.h>
#include "qfile.h"

unsigned char code_magic[2] = { 0xDD, 0x99 };

extern void dodecode();

extern void
decode(infile,a,nx,ny,scale)
QFILE *infile;                           /* input file (already opened)      */
int **a;                                /* address of output array [nx][ny] */
int *nx,*ny;                            /* size of output array             */
int *scale;                             /* scale factor for digitization    */
{
int nel, sumall;
unsigned char nbitplanes[3];
char tmagic[2];

    /*
     * read magic value in first two bytes
     */
    qread(infile, tmagic, sizeof(tmagic));
    if (memcmp(tmagic,code_magic,sizeof(code_magic)) != 0) {
        fprintf(stderr, "bad file format\n");
        exit(-1);
    }
    *nx =readint(infile);               /* x size of image                  */
    *ny =readint(infile);               /* y size of image                  */
    *scale=readint(infile);             /* scale factor for digitization    */
    /*
     * allocate memory for array
     */
    nel = (*nx) * (*ny);
    *a = (int *) malloc(nel*sizeof(int));
    if (*a == (int *) NULL) {
        fprintf(stderr, "decode: insufficient memory\n");
        exit(-1);
    }
    /* sum of all pixels    */
    sumall=readint(infile);
    /* # bits in quadrant   */
    qread(infile, (char *) nbitplanes, sizeof(nbitplanes));
    dodecode(infile, *a, *nx, *ny, nbitplanes);
    /*
     * put sum of all pixels back into pixel 0
     */
    (*a)[0] = sumall;
}
