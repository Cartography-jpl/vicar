/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*         CONRDDMS
 *
 *   PURPOSE:
 *         This routine takes the Right Ascension and Declination and 
 *         converts from radians to traditional units.
 *
 *   CALLING SEQUENCE:
 *         CONRDDMS(RA, DEC, &RA_H, &RA_M, &RA_S,
 *              &DEC_SIGN, &DEC_D, &DEC_M, &DEC_S)
 *
 *   INPUTS:
 *         RA   - Right Ascension (radians)
 *         DEC  - Declination     (radians)
 *
 *   OUTPUTS:
 *         RA_H,RA_M,RA_S             - Right Ascension hms
 *         DEC_SIGN,DEC_D,DEC_M,DEC_S - Declination dms
 *
 *   MODIFICATION HISTORY
 *         Converted from IDL to C, R. White, 31 July 1991
 */
#include <math.h>

#define ARCSEC_PER_RAD 206264.8062470964
#define SEC_PER_RAD (ARCSEC_PER_RAD/15.0)

extern void
conrddms(ra,dec,ra_h,ra_m,ra_s,dec_sign,dec_d,dec_m,dec_s)
double ra, dec;
int *ra_h, *ra_m;
float *ra_s;
char *dec_sign;
int *dec_d, *dec_m;
float *dec_s;
{
double object_ra, object_dec;

    /*
     * Convert right ascension
     * Assume 0 <= ra < 2*pi
     */
    object_ra = ra*SEC_PER_RAD;
    *ra_h = (int) (object_ra/3600);
    *ra_m = (int) ((object_ra- (*ra_h)*3600)/60);
    *ra_s = object_ra - ((*ra_h)*60 + (*ra_m))*60;
    /*
     * Convert declination
     */
    if (dec < 0) {
        *dec_sign = '-';
    } else {
        *dec_sign = '+';
    }
    object_dec = fabs(dec)*ARCSEC_PER_RAD;

    *dec_d = (int) (object_dec/3600);
    *dec_m = (int) ((object_dec - (*dec_d)*3600)/60);
    *dec_s = object_dec - ((*dec_d)*60 + (*dec_m))*60;
}
