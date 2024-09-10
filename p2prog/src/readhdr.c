/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       READHDR
 *
 * PURPOSE:
 *       Routine to read FITS header into structure
 *
 * CALLING SEQUENCE:
 *       readhdr(infile, h)
 *
 * INPUTS:
 *       infile     (* QFILE) header file pointer
 *
 * OUTPUTS:
 *       h          (* Header) header structure
 *
 * MODIFICATION HISTORY
 *       Converted from IDL routine to Fortran, R. White, 4 May 1990
 *       Converted from Fortran to C, R. White, 30 July 1991
 *       Modified to handle VMS fixed-length records, R. White, 16 June 1992
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "qfile.h"
#include "header.h"

/*
 * simple macros for keyword testing
 */
#define ival(skey,v)  if (strcmp(keyword,skey)==0) { v=atoi(value); continue; }
#define fval(skey,v)  if (strcmp(keyword,skey)==0) { v=atof(value); continue; }
#define cvalq(skey,v) if (strcmp(keyword,skey)==0) {                           \
                      strncpy(v,remquotes(value),sizeof(v));                   \
                      continue; }
#define cvalt(skey,v) if (strcmp(keyword,skey)==0) {                           \
                      strncpy(v,strtrim(remquotes(value)),sizeof(v));          \
                      continue; }

/*
 * external functions and procedures
 */
extern double dtotal();
extern char *remquotes(), *strtrim();
extern void condmsrd();

extern void
readhdr(infile,h)
QFILE *infile;
Header *h;
{
int i, linelen=80;
char keyword[9], value[21], line[80];

    /*
     * initial plate solution coefficients to zero
     */
    for (i = 0; i<6; i++) {
        h->ppo_coeff[i] = 0.0;
    }
    for (i = 0; i<20; i++) {
        h->amd_x_coeff[i] = 0.0;
        h->amd_y_coeff[i] = 0.0;
    }
    h->nhlines = 0;
    while (1) {
        qread(infile, line, linelen);
        /*
         * check for SIMPLE on first line, BITPIX on second line
         */
        if (h->nhlines == 0 && strncmp(line, "SIMPLE", 6) != 0) {
            fprintf(stderr, "Bad FITS header file %s\n", infile->filename);
            fprintf(stderr, "First keyword should be SIMPLE, not\n");
            fprintf(stderr, "%.80s\n", line);
            exit(-1);
        } else if (h->nhlines == 1 && strncmp(line, "BITPIX", 6) != 0) {
            fprintf(stderr, "Bad FITS header file %s\n", infile->filename);
            fprintf(stderr, "Second keyword should be BITPIX, not\n");
            fprintf(stderr, "%.80s\n", line);
            exit(-1);
        }
        strncpy(h->hlines[h->nhlines],line,80);
        h->hlines[h->nhlines++][80] = '\0';
        /*
         *  Extract keyword name, value and put in null-terminated string
         */
        strncpy(keyword, &line[0], 8);
        keyword[8] = '\0';
        strncpy(value, &line[10], 20);
        value[20] = '\0';
        /*
         *  Get standard keywords
         */
        ival("NAXIS1  ", h->section_x_length);
        ival("NAXIS2  ", h->section_y_length);
        fval("CRPIX1  ", h->section_x_corner);
        fval("CRPIX2  ", h->section_y_corner);
        fval("CDELT1  ", h->scaling);
        cvalq("OBJECT  ",h->object_name);
        /*
         *  Get GSSS Specific keywords
         */
        fval("MAGLIM  ", h->mag_limit);
        ival("PLTGRADE", h->plt_grade);
        cvalt("PLATEID ",h->plate_id);
        cvalt("REGION  ",h->region_no);
        cvalt("PLTLABEL",h->plate_label);
        cvalt("EXPOSURE",h->exposure_time);
        cvalt("SEEING  ",h->seeing);
        cvalt("ORIGIN  ",h->origin);
        /*
         *  Get GSSS calibration keywords
         */
        fval("EPOCH   ", h->plt_date);
        ival("BANDPASS", h->emulsion_code);
        fval("PLTSCALE", h->plt_scale);
        fval("XPIXELSZ", h->x_pixel_size);
        fval("YPIXELSZ", h->y_pixel_size);
        ival("PLTRAH  ", h->ra_h);
        ival("PLTRAM  ", h->ra_m);
        fval("PLTRAS  ", h->ra_s);
        if (strcmp(keyword,"PLTDECSN")==0) {
            if(strchr(value,'-') == NULL) {
                h->dec_sign = '+';
            } else {
                h->dec_sign = '-';
            }
        }
        ival("PLTDECD ", h->dec_d);
        ival("PLTDECM ", h->dec_m);
        fval("PLTDECS ", h->dec_s);
        fval("PLTVEC1 ", h->plt_center_vec[0]);
        fval("PLTVEC2 ", h->plt_center_vec[1]);
        fval("PLTVEC3 ", h->plt_center_vec[2]);
        fval("PPO1    ", h->ppo_coeff[0]);
        fval("PPO2    ", h->ppo_coeff[1]);
        fval("PPO3    ", h->ppo_coeff[2]);
        fval("PPO4    ", h->ppo_coeff[3]);
        fval("PPO5    ", h->ppo_coeff[4]);
        fval("PPO6    ", h->ppo_coeff[5]);
        fval("AMDX1   ", h->amd_x_coeff[ 0]);
        fval("AMDX2   ", h->amd_x_coeff[ 1]);
        fval("AMDX3   ", h->amd_x_coeff[ 2]);
        fval("AMDX4   ", h->amd_x_coeff[ 3]);
        fval("AMDX5   ", h->amd_x_coeff[ 4]);
        fval("AMDX6   ", h->amd_x_coeff[ 5]);
        fval("AMDX7   ", h->amd_x_coeff[ 6]);
        fval("AMDX8   ", h->amd_x_coeff[ 7]);
        fval("AMDX9   ", h->amd_x_coeff[ 8]);
        fval("AMDX10  ", h->amd_x_coeff[ 9]);
        fval("AMDX11  ", h->amd_x_coeff[10]);
        fval("AMDX12  ", h->amd_x_coeff[11]);
        fval("AMDX13  ", h->amd_x_coeff[12]);
        fval("AMDX14  ", h->amd_x_coeff[13]);
        fval("AMDX15  ", h->amd_x_coeff[14]);
        fval("AMDX16  ", h->amd_x_coeff[15]);
        fval("AMDX17  ", h->amd_x_coeff[16]);
        fval("AMDX18  ", h->amd_x_coeff[17]);
        fval("AMDX19  ", h->amd_x_coeff[18]);
        fval("AMDX20  ", h->amd_x_coeff[19]);
        fval("AMDY1   ", h->amd_y_coeff[ 0]);
        fval("AMDY2   ", h->amd_y_coeff[ 1]);
        fval("AMDY3   ", h->amd_y_coeff[ 2]);
        fval("AMDY4   ", h->amd_y_coeff[ 3]);
        fval("AMDY5   ", h->amd_y_coeff[ 4]);
        fval("AMDY6   ", h->amd_y_coeff[ 5]);
        fval("AMDY7   ", h->amd_y_coeff[ 6]);
        fval("AMDY8   ", h->amd_y_coeff[ 7]);
        fval("AMDY9   ", h->amd_y_coeff[ 8]);
        fval("AMDY10  ", h->amd_y_coeff[ 9]);
        fval("AMDY11  ", h->amd_y_coeff[10]);
        fval("AMDY12  ", h->amd_y_coeff[11]);
        fval("AMDY13  ", h->amd_y_coeff[12]);
        fval("AMDY14  ", h->amd_y_coeff[13]);
        fval("AMDY15  ", h->amd_y_coeff[14]);
        fval("AMDY16  ", h->amd_y_coeff[15]);
        fval("AMDY17  ", h->amd_y_coeff[16]);
        fval("AMDY18  ", h->amd_y_coeff[17]);
        fval("AMDY19  ", h->amd_y_coeff[18]);
        fval("AMDY20  ", h->amd_y_coeff[19]);
        if (strcmp(keyword,"END     ")==0) break;
    }
    /*
     *  Decide if GSSS scan or special plate
     */
    if (strncmp(h->plate_id,"    ",4) == 0) {
        h->special_plate_flag = 'T';
    } else {
        h->special_plate_flag = 'F';
    }
    /*
     *  Fudge pixel size for early scan data
     *  (I'd be surprised if this works when roundoff error is included)
     */
    if ((h->special_plate_flag == 'F') && (h->x_pixel_size == 25.284)) {
         h->x_pixel_size = 25.28445;
         h->y_pixel_size = 25.28445;
    }
    /*
     *  See if AMD calibration exists
     */
    if (dtotal(h->amd_x_coeff,20) == 0.0) {
        h->amd_flag = 'F';
    } else {
        h->amd_flag = 'T';
    }
    /*
     *  See if PPO calibration exists
     */
    if (dtotal(h->ppo_coeff,6) == 0.0) {
        h->ppo_flag = 'F';
    } else {
        h->ppo_flag = 'T';
    }
    /*
     *  See if plate data exists
     */
    if (strncmp(h->plate_id,"    ",4) != 0) {
        h->plate_data_flag = 'T';
    } else {
        h->plate_data_flag = 'F';
    }
    /*
     *  Convert plate center position to radians
     */
    condmsrd(h->ra_h, h->ra_m, h->ra_s,
        h->dec_sign, h->dec_d, h->dec_m, h->dec_s,
        &h->plt_center_ra, &h->plt_center_dec);
}
