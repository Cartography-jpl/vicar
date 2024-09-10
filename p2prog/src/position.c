/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*         POSITION
 *   PURPOSE:
 *         Routine to compute RA and Dec from x,y.
 *   CALLING SEQUENCE:
 *         POSITION(H,X,Y,MAG,COL,&RA,&DEC)
 *   INPUTS:
 *         H   - (Header *) header structure
 *         X,Y - plate position  (pixels)
 *         MAG - Magnitude
 *         COL - Colour
 *   OUTPUTS:
 *         RA     - (double *) Right Ascension (radians)
 *         DEC    - (double *) Declination
 *   PROCEDURE:
 *         Routine takes given position and uses the CALOBCC solution if
 *         available otherwise it reverts to the DIG25CC orientation soln.
 *   MODIFICATION HISTORY
 *         Converted from IDL to C, R. White, 31 July 1991
 */

#include "header.h"

#define PI 3.141592653589793238
#define TWO_PI (2*PI)

extern void amdpos();
extern void ppopos();

extern void
position(h,x,y,mag,col,ra,dec)
Header *h;
double x,y,mag,col;
double *ra, *dec;
{
    /*
     * Compute ra,dec
     */
    if (h->amd_flag == 'T') {
        amdpos(h,x,y,mag,col,ra,dec);
    } else if (h->ppo_flag == 'T') {
        ppopos(h,x,y,ra,dec);
    } else {
        *ra = 0.0;
        *dec = 0.0;
    }
    /*
     * Check RA range
     */
    if (*ra >= TWO_PI) *ra = *ra - TWO_PI;
    if (*ra < 0.0)  *ra = *ra + TWO_PI;
}
