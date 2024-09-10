/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       TRANEQSTD
 * PURPOSE:
 *       Routine to convert RA and Dec in radians to standard coordinates
 *       on a plate.
 * CALLING SEQUENCE:
 *       TRANEQSTD(OBJECT_RA,OBJECT_DEC,OBJECT_XI,OBJECT_ETA)
 * INPUTS:
 *       OBJECT_RA,OBJECT_DEC - REAL*8 Equatorial Coords (radians)
 * OUTPUTS:
 *       OBJECT_XI,OBJECT_ETA - REAL*8 Standard Coords  (arcsec)
 * MODIFICATION HISTORY
 *       Converted to Fortran from IDL routine, R. White, 7 May 1990
 *       Converted from Fortran to C, R. White, 30 July 1991
 */
#include <math.h>
#include "header.h"

#define ARCSEC_PER_RAD 206264.8062470964

extern void
traneqstd(header,object_ra,object_dec,object_xi,object_eta)
Header *header;
double object_ra, object_dec;
double *object_xi, *object_eta;
{
double div;

    /*
     *  Find divisor
     */
    div=(sin(object_dec)*sin(header->plt_center_dec)+
        cos(object_dec)*cos(header->plt_center_dec)*
        cos(object_ra - header->plt_center_ra));
    /* 
     *  Compute standard coords and convert to arcsec
     */
    *object_xi=cos(object_dec)*sin(object_ra - header->plt_center_ra)*
        ARCSEC_PER_RAD/div;

    *object_eta=(sin(object_dec)*cos(header->plt_center_dec)-
        cos(object_dec)*sin(header->plt_center_dec)*
        cos(object_ra - header->plt_center_ra))*
        ARCSEC_PER_RAD/div;
}
