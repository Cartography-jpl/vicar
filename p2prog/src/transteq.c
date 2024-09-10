/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       TRANSTDEQ
 * PURPOSE:
 *       Routine to convert standard coordinates on a plate to RA and Dec
 *       in radians.
 * CALLING SEQUENCE:
 *       TRANSTDEQ(H,XI,ETA,&RA,&DEC)
 * INPUTS:
 *       H       -  (* Header) header structure
 *       XI,ETA  -  Standard Coords (arcsec)
 * OUTPUTS:
 *       RA,DEC  -  (* double) Equatorial Coords (radians)
 * MODIFICATION HISTORY
 *       Converted from IDL to Fortran, R. White, 7 Dec 1990
 *       Converted from Fortran to C, R. White, 31 July 1991
 */
#include <math.h>

#include "header.h"

#define ARCSEC_PER_RAD 206264.8062470964
#define PI 3.141592653589793238

extern void
transtdeq(h,xi,eta,ra,dec)
Header *h;
double xi, eta;
double *ra, *dec;
{
double object_xi,object_eta,numerator,denominator;

    /*
     *  Convert to radians
     */
    object_xi = xi/ARCSEC_PER_RAD;
    object_eta = eta/ARCSEC_PER_RAD;
    /*
     *  Convert to RA and Dec
     */
    numerator = object_xi/cos(h->plt_center_dec);
    denominator = 1-object_eta*tan(h->plt_center_dec);
    *ra = atan2(numerator,denominator)+h->plt_center_ra;
    if (*ra < 0.0) *ra = (*ra)+2*PI;

    numerator = cos((*ra) - h->plt_center_ra);
    denominator = (1-object_eta*tan(h->plt_center_dec))/
                (object_eta+tan(h->plt_center_dec));
    *dec = atan(numerator/denominator);
}
