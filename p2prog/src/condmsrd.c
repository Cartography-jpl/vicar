/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       CONDMSRD
 * 
 * PURPOSE:
 *      This routine takes the Right Ascension and Declination and 
 *      converts from traditional units to radians.
 * 
 * CALLING SEQUENCE:
 *      CONDMSRD(RA_H,RA_M,RA_S,DEC_SIGN,DEC_D,DEC_M,DEC_S,
 *                OBJECT_RA,OBJECT_DEC)
 * 
 * INPUTS:
 *       RA_H,RA_M,RA_S              - Right Ascension hms
 *       DEC_SIGN,DEC_D,DEC_M,DEC_S  - Declination     dms
 * 
 * OUTPUTS:
 *       OBJECT_RA,OBJECT_DEC        - REAL*8 Radians
 * 
 * MODIFICATION HISTORY
 *       Converted from IDL by R. White, 4 May 1990
 */
#define ARCSEC_PER_RAD 206264.8062470964
#define SEC_PER_RAD (ARCSEC_PER_RAD/15.0)

extern void
condmsrd(ra_h, ra_m, ra_s, dec_sign, dec_d, dec_m, dec_s,
    object_ra, object_dec)
int ra_h, ra_m;
double ra_s;
char dec_sign;
int dec_d, dec_m;
double dec_s;
double *object_ra, *object_dec;
{
    /*
     *  Convert right ascension
     */
    *object_ra = (ra_h*3600.0+ra_m*60.0+ra_s)/SEC_PER_RAD;
    /*
     *  Convert declination
     */
    *object_dec = (dec_d*3600.0+dec_m*60.0+dec_s)/ARCSEC_PER_RAD;
    if (dec_sign == '-') *object_dec = -(*object_dec);
}
