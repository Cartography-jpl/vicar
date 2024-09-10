/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       FILLHDR
 *
 * PURPOSE:
 *       Change FITS header parameters to match the new extraction parameters
 *
 * CALLING SEQUENCE:
 *       fillhdr(h,object,nx,ny,x0,y0,image)
 *
 * INPUTS:
 *       h          (* Header) header structure
 *       object     (* char) name of object
 *       nx,ny      (int) size of image
 *       x0,y0      (int) corner of image on plate
 *       image      (short int) image data
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
fillhdr(h,object,nx,ny,x0,y0,image)
Header *h;
char *object;
int nx,ny,x0,y0;
short image[];
{
int i,j;
char keyword[9], hlast[81];
int itime;
char *cdate, date[18];
short datamin,datamax;
int no_naxis1, no_naxis2, no_crpix1, no_crpix2, no_datamax, no_datamin,
    no_object, no_date;


    /*
     * set size of section and corner position
     */
    h->section_x_length = nx;
    h->section_y_length = ny;
    h->section_x_corner = x0;
    h->section_y_corner = y0;
    /*
     * object name, padded with blanks
     */
    for (i=0; i<sizeof(h->object_name)-1; i++) h->object_name[i] = ' ';
    h->object_name[i] = '\0';
    for (i=0; (i<sizeof(h->object_name)-1) && (object[i] != '\0'); i++)
        h->object_name[i] = object[i];
    /*
     * get date & time
     * original format is 'wkd mmm dd hh:mm:ss yyyy\n\0'
     * change to 'mmm dd hh:mm yyyy\0' so it fits in <18 characters
     */
    itime = time(NULL);
    cdate = ctime(&itime);
    for (i=0; i<12; i++) date[i] = cdate[i+4];
    for (; i<17; i++) date[i] = cdate[i+7];
    date[i] = '\0';
    /*
     * get data max, min
     */
    datamin = image[0];
    datamax = image[0];
    for (i=1; i<nx*ny; i++) {
        if (image[i] > datamax) {
            datamax = image[i];
        } else if (image[i] < datamin) {
            datamin = image[i];
        }
    }
    /*
     * look for existing keywords and change them
     */
    no_naxis1 = 1;
    no_naxis2 = 1;
    no_crpix1 = 1;
    no_crpix2 = 1;
    no_datamax = 1;
    no_datamin = 1;
    no_object = 1;
    no_date = 1;
    for (i=0; i < h->nhlines; i++) {
        /*
         *  Extract keyword name and put in null-terminated string
         */
        strncpy(keyword, h->hlines[i], 8);
        keyword[8] = '\0';
        /* 
         *  Find keywords to change
         */
        if        (strcmp(keyword,"NAXIS1  ") == 0) {
            sprintf(&h->hlines[i][10],"%20d",h->section_x_length);
            h->hlines[i][30] = ' ';
            no_naxis1 = 0;
        } else if (strcmp(keyword,"NAXIS2  ") == 0) {
            sprintf(&h->hlines[i][10],"%20d",h->section_y_length);
            h->hlines[i][30] = ' ';
            no_naxis2 = 0;
        } else if (strcmp(keyword,"CRPIX1  ") == 0) {
            sprintf(&h->hlines[i][10],"%20.0f",h->section_x_corner);
            h->hlines[i][30] = ' ';
            no_crpix1 = 0;
        } else if (strcmp(keyword,"CRPIX2  ") == 0) {
            sprintf(&h->hlines[i][10],"%20.0f",h->section_y_corner);
            h->hlines[i][30] = ' ';
            no_crpix2 = 0;
        } else if (strcmp(keyword,"DATAMAX ") == 0) {
            sprintf(&h->hlines[i][10],"%20d",datamax);
            h->hlines[i][30] = ' ';
            no_datamax = 0;
        } else if (strcmp(keyword,"DATAMIN ") == 0) {
            sprintf(&h->hlines[i][10],"%20d",datamin);
            h->hlines[i][30] = ' ';
            no_datamin = 0;
        } else if (strcmp(keyword,"OBJECT  ") == 0) {
            sprintf(&h->hlines[i][10],"'%-18.18s'",h->object_name);
            h->hlines[i][30] = ' ';
            no_object = 0;
        } else if (strcmp(keyword,"DATE    ") == 0) {
            sprintf(&h->hlines[i][10],"'%-18.18s'",date);
            h->hlines[i][30] = ' ';
            no_date = 0;
        }
    }
    /*
     * if some of these keywords were absent, insert them just before
     *  the last line of the header
     */
    i = h->nhlines-1;
    for (j=0; j<sizeof(hlast); j++) hlast[j] = h->hlines[i][j];
    if (no_naxis1) {
        sprintf(h->hlines[i], "%-8.8s= %20d /%-50.50s",
            "NAXIS1", h->section_x_length, "Length X axis");
        i = i + 1;
    }
    if (no_naxis2) {
        sprintf(h->hlines[i], "%-8.8s= %20d /%-50.50s",
            "NAXIS2", h->section_y_length, "Length Y axis");
        i = i + 1;
    }
    if (no_crpix1) {
        sprintf(h->hlines[i], "%-8.8s= %20d /%-50.50s",
            "NCRPIX1", h->section_x_corner, "X corner");
        i = i + 1;
    }
    if (no_crpix2) {
        sprintf(h->hlines[i], "%-8.8s= %20d /%-50.50s",
            "NCRPIX2", h->section_y_corner, "Y corner");
        i = i + 1;
    }
    if (no_datamax) {
        sprintf(h->hlines[i], "%-8.8s= %20d /%-50.50s",
            "DATAMAX", datamax, "Maximum data value");
        i = i + 1;
    }
    if (no_datamin) {
        sprintf(h->hlines[i], "%-8.8s= %20d /%-50.50s",
            "DATAMIN", datamin, "Minimum data value");
        i = i + 1;
    }
    if (no_object) {
        sprintf(h->hlines[i], "%-8.8s= '%-18.18s' /%-50.50s",
            "OBJECT", h->object_name, "Object ID");
        i = i + 1;
    }
    if (no_date) {
        sprintf(h->hlines[i], "%-8.8s= '%-18.18s' /%-50.50s",
            "DATE", date, "Creation Date");
        i = i + 1;
    }
    /*
     * put last line back on the end
     */
    if (i >= h->nhlines) {
        for (j=0; j<sizeof(hlast); j++) h->hlines[i][j] = hlast[j];
        h->nhlines = i+1;
    }
}
