/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*          PPOPOS
 *
 *    PURPOSE:
 *          Routine to convert x,y to RA,Dec using the PDS orientation solution.
 *
 *    CALLING SEQUENCE:
 *          PPOPOS(H,X,Y,&RA,&DEC)
 *
 *    INPUTS:
 *          H    - (* Header) header structure
 *          X,Y  - position (pixels)
 *
 *    OUTPUTS:
 *          RA        - (* double) Right Ascension (radians)
 *          DEC       - (* double) Declination
 *
 *    PROCEDURE:
 *          Computes the standard coords using the inverse of the orientation
 *          solution which is then converted to celestial coords.
 *
 *    MODIFICATION HISTORY
 *          Converted from IDL to C, R. White, 31 July 1991
 */

#include "header.h"

#define ARCSEC_PER_RAD 206264.8062470964

extern void transtdeq();

extern void
ppopos(h,x,y,ra,dec)
Header *h;
double x,y;
double *ra, *dec;
{
double a,b,c,d,e,f;
double object_x, object_y, object_xi, object_eta;

    /*
     * Set up local variables
     */
    a = h->ppo_coeff[0];
    b = h->ppo_coeff[1];
    c = h->ppo_coeff[2];
    d = h->ppo_coeff[3];
    e = h->ppo_coeff[4];
    f = h->ppo_coeff[5];
    /*
     * Convert x,y from pixels to microns
     */
    object_x = x * h->x_pixel_size;
    object_y = y * h->y_pixel_size;
    /*
     * Compute standard coordinates from x,y and plate center
     */
    object_xi  = (e*(c-object_x)-b*(f-object_y))/(b*d-a*e);
    object_eta = (a*(f-object_y)-d*(c-object_x))/(b*d-a*e);
    /*
     * Convert st.coords from radians to arcsec
     */
    object_xi  = object_xi*ARCSEC_PER_RAD;
    object_eta = object_eta*ARCSEC_PER_RAD;
    /*
     * Convert to RA and Dec
     */
    transtdeq(h,object_xi,object_eta,ra,dec);
}
