/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       FILLPOS
 *
 * PURPOSE:
 *       Add FITS header parameters for RA, Dec for this object
 *
 * CALLING SEQUENCE:
 *       fillhdr(h,rah,ram,ras,decsign,decd,decm,decs,x,y)
 *
 * INPUTS:
 *       h              (* Header) header structure
 *       rah,ram,ras    RA for object
 *       decsign,decd,  Dec for object
 *        decm,decs
 *       x,y            (float) position of object on plate in pixels
 *
 * OUTPUTS:
 *       h          (* Header) modified header structure
 *
 * MODIFICATION HISTORY
 *       Created by R. White, 31 July 1991
 */
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <string.h>

#include "header.h"

extern void
fillpos(h,rah,ram,ras,decsign,decd,decm,decs,x,y)
Header *h;
int rah,ram;
double ras;
char decsign;
int decd,decm;
double decs;
double x,y;
{
int i,j;
char keyword[9], hlast[81], rastr[13], decstr[13];
int no_ra, no_dec, no_x, no_y;

    /*
     * set position in header structure
     */
    h->ra_h = rah;
    h->ra_m = ram;
    h->ra_s = ras;
    h->dec_d = decd;
    h->dec_m = decm;
    h->dec_s = decs;
    h->dec_sign = decsign;
    h->x = x;
    h->y = y;
    /*
     * round for printing with ss.sss (RA), ss.ss (Dec)
     */
    if (ras+0.0005 >= 60.0) {
        ras = 0.0;
        ram += 1;
        if (ram == 60) {
            ram = 0;
            rah += 1;
            if (rah == 24) {
                rah = 0;
            }
        }
    }
    if (decs+0.005 >= 60.0) {
        decs = 0.0;
        decm += 1;
        if (decm == 60) {
            decm = 0;
            decd += 1;
        }
    }
    /*
     * strings for RA, Dec
     */
    sprintf(rastr, "%2.2d %2.2d %06.3f", rah, ram, ras);
    sprintf(decstr, "%c%2.2d %2.2d %05.2f", decsign, decd, decm, decs);
    /*
     * look for existing OBJCTRA, OBJCTDEC, OBJCTX, OBJCTY keywords and
     * change them
     */
    no_ra = 1;
    no_dec = 1;
    no_x = 1;
    no_y = 1;
    for (i=0; i < h->nhlines; i++) {
        /*
         *  Extract keyword name and put in null-terminated string
         */
        strncpy(keyword, h->hlines[i], 8);
        keyword[8] = '\0';
        /* 
         *  Find keywords to change
         */
        if        (strcmp(keyword,"OBJCTRA ") == 0) {
            sprintf(&h->hlines[i][10],"'%-18.18s'",rastr);
            h->hlines[i][30] = ' ';
            no_ra = 0;
        } else if (strcmp(keyword,"OBJCTDEC") == 0) {
            sprintf(&h->hlines[i][10],"'%-18.18s'",decstr);
            h->hlines[i][30] = ' ';
            no_dec = 0;
        } else if (strcmp(keyword,"OBJCTX  ") == 0) {
            sprintf(&h->hlines[i][10],"%20.2f",x);
            h->hlines[i][30] = ' ';
            no_x = 0;
        } else if (strcmp(keyword,"OBJCTY  ") == 0) {
            sprintf(&h->hlines[i][10],"%20.2f",y);
            h->hlines[i][30] = ' ';
            no_y = 0;
        }
    }
    /*
     * if some of these keywords were absent, insert them just before
     *  the last line of the header
     */
    i = h->nhlines-1;
    for (j=0; j<sizeof(hlast); j++) hlast[j] = h->hlines[i][j];
    if (no_ra) {
        sprintf(h->hlines[i], "%-8.8s= '%-18.18s' /%-50.50s",
            "OBJCTRA", rastr, "Object Right Ascension (J2000)");
        i = i+1;
    }
    if (no_dec) {
        sprintf(h->hlines[i], "%-8.8s= '%-18.18s' /%-50.50s",
            "OBJCTDEC", decstr, "Object Declination (J2000)");
        i = i+1;
    }
    if (no_x) {
        sprintf(h->hlines[i], "%-8.8s= %20.2f /%-50.50s",
            "OBJCTX", x, "Object X on plate (pixels)");
        i = i+1;
    }
    if (no_y) {
        sprintf(h->hlines[i], "%-8.8s= %20.2f /%-50.50s",
            "OBJCTY", y, "Object Y on plate (pixels)");
        i = i+1;
    }
    /*
     * put last line back on the end
     */
    if (i >= h->nhlines) {
        for (j=0; j<sizeof(hlast); j++) h->hlines[i][j] = hlast[j];
        h->nhlines = i+1;
    }
}
