/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       PPOINV
 * 
 * PURPOSE:
 *       Routine to convert RA,Dec to x,y using the PDS orientation 
 *       solution.
 * 
 * CALLING SEQUENCE:
 *       call PPOINV(RA,DEC,X,Y)
 *
 * INPUTS:
 *       RA        - REAL*8 Right Ascension (radians)
 *       DEC       - REAL*8 Declination
 *
 * OUTPUTS:
 *       X,Y  - position (pixels)
 *
 * PROCEDURE:
 *       Transforms the standard coords to pixels using the orientation
 *       solution.
 *
 * MODIFICATION HISTORY
 *       Converted from IDL routine to Fortran, R. White, 7 May 1990
 *       Converted from Fortran to C, R. White, 30 July 1991
 */
#include <math.h>
#include "header.h"

#define ARCSEC_PER_RADIAN 206264.8062470964

extern void traneqstd();

extern void
ppoinv(header,ra,dec,x,y)
Header *header;
double ra, dec;
float *x,*y;
{
double object_xi, object_eta, object_x, object_y;

    /*
     *  Convert RA and Dec to standard coords.
     */
    traneqstd(header,ra,dec,&object_xi,&object_eta);
    /*
     *  Convert st.coords from arcsec to radians
     */
    object_xi  = object_xi /ARCSEC_PER_RADIAN;
    object_eta = object_eta/ARCSEC_PER_RADIAN;
    /*
     *  Compute PDS coordinates from solution
     */
    object_x = header->ppo_coeff[0]*object_xi
        + header->ppo_coeff[1]*object_eta + header->ppo_coeff[2];
    object_y = header->ppo_coeff[3]*object_xi
        + header->ppo_coeff[4]*object_eta + header->ppo_coeff[5];
    /*
     * Convert x,y from microns to pixels
     */
    *x = object_x/header->x_pixel_size;
    *y = object_y/header->y_pixel_size;
}
