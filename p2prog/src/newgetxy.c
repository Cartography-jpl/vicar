/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       NEWGETXY
 *
 * PURPOSE:
 *       Extract a subimage from a plate by specifying the central position
 *       XC,YC and the size of the section NX,NY in pixels.
 *
 * CALLING SEQUENCE:
 *       newgetxy(h,object,xc,yc,nx,ny,image,filename)
 *
 * INPUTS:
 *       h          (* Header) header structure
 *       object     (* char) name of object
 *       xc,yc      (float) center of image on plate
 *       nx,ny      (int) size of image
 *       smooth     (external int) non-zero to use smoothing
 *
 * OUTPUTS:
 *       image      (short [ny][nx]) image section
 *       h          the header is updated to reflect the position of section
 *
 * PROCEDURE:
 *       The plate is assumed to be stored in compressed 500x500 blocks
 *       (or 500x499 for the last row).  The required blocks are decompressed
 *       one at a time and stored into image.  The header is updated to
 *       include the current values of target RA, Dec, etc.  If part
 *       of the image is off the edge of the plate, zeros are returned.
 *       A warning is printed if the entire image is off the plate.
 *       The smooth flag (external variable) is passed to hdecompress and
 *       determines whether the images are smoothed as they are decompressed.
 *
 * MODIFICATION HISTORY
 *       Created by R. White, 31 July 1991
 *       Added smoothing flag, 14 April 1992
 *       Modified to handle Ultrix uppercase filenames, 26 May 1993
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "header.h"

int smooth;

#define max(a,b)    (((a) > (b)) ? (a) : (b))
#define min(a,b)    (((a) < (b)) ? (a) : (b))

#ifdef ULTRIX
static char translate[37] = "0123456789ABCDEFGHIJKLMNOPQR";
#else
static char translate[37] = "0123456789abcdefghijklmnopqr";
#endif

/*extern char *makepname();*/
extern void position();
extern void conrddms();
extern void hdecompress();
extern void fillhdr();
extern void fillpos();

extern void
newgetxy(h,object,xc,yc,nx,ny,image,filename)
Header *h;
char *object;
double xc,yc;
int nx,ny;
short image[];
char *filename;
{
int i, j, ii, jj, i1, i2, j1, j2, b, x0, y0, x1, x2, y1, y2,
    si1, si2, sii, sj1, sj2, sjj, imi1, imi2, imj1, imj2,
    snx, sny, ss, ncy;
double ra, dec;
float mag, colour;
int rah, ram, decd, decm;
char decsign;
float ras, decs;
int nf;
int *subim;

    /*
     * calculate corner position
     */
    x0=xc-(nx/2);
    y0=yc-(ny/2);
    /*
     * calculate RA and Dec of xc,yc
     */
    mag = 0.0;
    colour = 0.0;
    position(h,xc,yc,mag,colour,&ra,&dec);
    conrddms(ra, dec, &rah, &ram, &ras, &decsign, &decd, &decm, &decs);
    /*
     * get plate file template
     */
    /*filename = makepname(h);*/
    nf = strlen(filename)+1;
    /*
     * get range of image pixels which we will extract if we're not off edge
     */
    x1 = x0;
    x2 = x0+nx-1;
    y1 = y0;
    y2 = y0+ny-1;
    /*
     * now get actual range of sections (limited to 0 -- 27)
     */
    i1=x1/500; if (i1< 0) i1 = 0;
    i2=x2/500; if (i2>27) i1 = 27;
    j1=y1/500; if (j1< 0) j1 = 0;
    j2=y2/500; if (j2>27) j2 = 27;
    /*
     * initialize image to zero
     */
    for (i=0; i<nx*ny; i++) image[i] = 0;
    if ((i2 < i1) || (j2 < j1)) {
        fprintf(stderr,
            "warning: image section lies entirely off edge of plate\n");
        return;
    }
    /*
     * read each section and copy required pixels to image
     */
    b=0;
    fprintf(stderr,"decompressing %1d blocks: ",(i2-i1+1)*(j2-j1+1));
    for (i = i1; i <= i2; i++) {
        /*
         * get subscript ranges on subimage, image
         */
        si1 = x1-i*500;
        si2 = x2-i*500;
        imi1 = max(-si1,0);
        imi2 = x2-x1-max(si2-499,0);
        si1 = max(si1,0);
        si2 = min(si2,499);
        for (j = j1; j <= j2; j++) {
            b = b + 1;
            fprintf(stderr," %1d",b);
            /*
             * read subimage i,j
             */
            filename[nf-3] = translate[j];
            filename[nf-2] = translate[i];
            /*
             * Decompress image in filename
             * Returns address subim and size snx,sny
             */
            hdecompress(&subim,&sny,&snx,filename,smooth);
            if (j == 27) {
                ncy=499;
            } else {
                ncy=500;
            }
            if ((snx != 500) || (sny != ncy)) {
                fprintf(stderr, "\nerror: bad image size for %s\n", filename);
                fprintf(stderr, "should be 500 %3d, but is %3d %3d\n",
                    ncy,snx,sny);
                exit(-1);
            }
            sj1 = y1-j*500;
            sj2 = y2-j*500;
            imj1 = max(-sj1,0);
            imj2 = y2-y1-max(sj2-(ncy-1),0);
            sj1 = max(sj1,0);
            sj2 = min(sj2,ncy-1);
            sjj = sj1;
            for (jj=imj1; jj<=imj2; jj++) {
                sii = si1;
                for (ii=imi1; ii<=imi2; ii++) {
                    /*
                     * force pixel value into range of short integers
                     */
                    ss = subim[sii+snx*sjj];
                    if (ss>32767) {
                        ss = 32767;
                    } else if (ss < -32768) {
                        ss = -32768;
                    }
                    image[ii+nx*jj] = ss;
                    sii += 1;
                }
                sjj += 1;
            }
            free((char *) subim);
        }
    }
    /*
     * now fill in header values for this position
    printf("one\n");
    fillhdr(h, object, nx, ny, x0, y0, image);
    printf("two\n");
    fillpos(h, rah, ram, ras, decsign, decd, decm, decs, xc, yc);
    printf("three\n");
    fprintf(stderr," extracted image %5d x %5d\n",x2-x1+1,y2-y1+1);
    printf("four\n");
    free(filename);
    printf("five\n");
     */
}
