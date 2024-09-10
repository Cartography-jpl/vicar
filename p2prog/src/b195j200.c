/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* b1950_j2000.c    Convert from B1950 coordinates to J2000
 *
 * Programmer: R. White     Date: 5 April 1989
 */
#include <math.h>

#define PI 3.141592653589793238

/*
 * conversion between B1950 and J2000 coordinates
 * from Aoki, X., Soma, M., Kinoshita, H., Inoue, K. (1983) A.Ap. 128, 263.
 *      and Astronomical Almanac, 1989, pp. B42-B43 and
 * Simplified for the case of no proper motions or parallaxes
 * in J2000 coordinate system.  Note that a fixed target would
 * have a spurious proper motion (the E-term of the aberration)
 * in the B1950 coordinates, so for fixed targets the correct assumption
 * is no PM in the J2000 frame.
 */

static double a[3] = { -1.62557e-6, -0.31919e-6, -0.13843e-6 };
static double adot[3] = { 1.244e-3, -1.579e-3, -0.660e-3 };
static double m[6][6] = {
    {  0.9999256782, -0.0111820611, -0.0048579477, 
         0.00000242395018, -0.00000002710663, -0.00000001177656 },
    {  0.0111820610,  0.9999374784, -0.0000271765,
         0.00000002710663,  0.00000242397878, -0.00000000006587 },
    {  0.0048579479, -0.0000271474,  0.9999881997,
         0.00000001177656, -0.00000000006582,  0.00000242410173 },
    {  -0.000551, -0.238565,  0.435739,  0.99994704, -0.01118251, -0.00485767 },
    {   0.238514, -0.002667, -0.008541,  0.01118251,  0.99995883, -0.00002718 },
    {  -0.435623,  0.012254,  0.002117,  0.00485767, -0.00002714,  1.00000956 }
};

/* convert B1950 to J2000 coordinates assuming that proper motion and
 * parallax in J2000 frame are zero
 */
extern void
b1950_j2000(in_ra,in_dec,out_ra,out_dec)
double in_ra,in_dec;
double *out_ra,*out_dec;
{
double r0[3], r[6], a1[3];
double ar = 0.0, cd, t1, t;
int i, j;

    cd = cos(in_dec);
    r0[0] = cd*cos(in_ra);
    r0[1] = cd*sin(in_ra);
    r0[2] = sin(in_dec);

    /* correct for E-terms of aberration
     * assume epoch of observation was about 1980; there ought to be
     * another version of the program when the epoch of the observations
     * are known.
     */
    t1 = (1980.0 - 1950.0)/100.0;
    t = (t1-0.5)*(PI/(180.0*3600.0));
    t1 = t1*(PI/(180.0*3600.0));
    for (i=0; i<3; i++) a1[i] = a[i] + adot[i]*t1;
    for (i=0; i<3; i++) ar += r0[i]*a1[i];
    for (i=0; i<3; i++) r0[i] = (1.0+ar)*r0[i] - a1[i];

    /* precess from 1950 to 2000 */
    for(i=0; i<6; i++) {
        r[i] = 0.0;
        for (j=0; j<3; j++) r[i] += m[i][j]*r0[j];
    }
    /* modify r for assumed 1980 epoch */
    for (i=0; i<3; i++) r[i] += r[i+3]*t;

    /* convert from x,y,z to RA, Dec */
    *out_dec = asin( r[2] / sqrt(r[0]*r[0]+r[1]*r[1]+r[2]*r[2]) );
    *out_ra = atan2(r[1],r[0]);
}
