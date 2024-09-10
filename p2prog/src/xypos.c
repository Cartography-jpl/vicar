/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       XYPOS
 *
 * PURPOSE:
 *       Routine to compute x,y from RA and Dec.
 *
 * CALLING SEQUENCE:
 *       XYPOS(RA,DEC,MAG,COL,X,Y)
 *
 * INPUTS:
 *       RA     - REAL*8 Right Ascension (radians) (J2000 coordinates)
 *       DEC    - REAL*8 Declination
 *       MAG    - Magnitude
 *       COL    - Colour
 *
 * OUTPUTS:
 *       X,Y - plate position  (pixels)
 *
 * PROCEDURE:
 *       Routine takes given position and uses the CALOBCC solution if
 *       available, otherwise it reverts to the DIG25CC orientation soln.
 *
 * MODIFICATION HISTORY
 *       Converted from IDL routine to Fortran, R. White, 7 May 1990
 *       Converted from Fortran to C, R. White, 30 July 1991
 */ 
#include <math.h>
#include "header.h"

extern void amdinv();
extern void ppoinv();

extern void
xypos(header,ra,dec,mag,col,x,y)
Header *header;
double ra,dec;
float mag,col;
float *x,*y;
{
    if (header->amd_flag == 'T') {
        amdinv(header,ra,dec,mag,col,x,y);
    } else {
        ppoinv(header,ra,dec,x,y);
    }
}
