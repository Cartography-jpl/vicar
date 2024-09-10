/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       AMDPOS
 * 
 * PURPOSE:
 *       Routine to convert x,y to RA,Dec using the 
 *       CALOBCC solution.
 * 
 * CALLING SEQUENCE:
 *       AMDPOS(H,X,Y,MAG,COLOUR,RA,DEC)
 * 
 * INPUTS:
 *       H         - (* Header) header structure
 *       X         - x position  (pixels)
 *       Y         - y position
 *       MAG       - magnitude
 *       COLOUR    - colour
 * 
 * OUTPUTS:
 *       RA        - (* double) Right ascension (radians)
 *       DEC       - (* double) Declination
 * 
 * MODIFICATION HISTORY
 *       Converted from IDL to Fortran, RLW, 21 Sept 1990
 *       Converted from Fortran to C, RLW, 31 July 1991
 */
#include "header.h"

#define ARCSEC_PER_RAD 206264.8062470964

extern void transtdeq();

extern void
amdpos(h,x,y,mag,colour,ra,dec)
Header *h;
double x,y,mag,colour;
double *ra, *dec;
{
double ox,oy,ox2,oy2,ox3,oy3,object_xi,object_eta;

    /*
     *  Convert x,y from pixels to mm measured from plate center
     */
    ox = (h->ppo_coeff[2]   - x*h->x_pixel_size)/1000.0;
    oy = (y*h->y_pixel_size - h->ppo_coeff[5]  )/1000.0;
    ox2 = ox*ox;
    oy2 = oy*oy;
    ox3 = ox*ox2;
    oy3 = oy*oy2;
    /*
     *  Compute standard coordinates from x,y and plate model
     */
    object_xi =
     h->amd_x_coeff[ 0]*ox                    + h->amd_x_coeff[ 1]*oy     +
     h->amd_x_coeff[ 2]                       + h->amd_x_coeff[ 3]*ox2    +
     h->amd_x_coeff[ 4]*ox*oy                 + h->amd_x_coeff[ 5]*oy2    +
     h->amd_x_coeff[ 6]*(ox2+oy2)             + h->amd_x_coeff[ 7]*ox3    +
     h->amd_x_coeff[ 8]*ox2*oy                + h->amd_x_coeff[ 9]*ox*oy2 +
     h->amd_x_coeff[10]*oy3                   +
     h->amd_x_coeff[11]*ox*(ox2+oy2)          +
     h->amd_x_coeff[12]*ox*(ox2+oy2)*(ox2*oy2)+
     h->amd_x_coeff[13]*mag                   + h->amd_x_coeff[14]*mag*mag+
     h->amd_x_coeff[15]*mag*mag*mag           + h->amd_x_coeff[16]*mag*ox +
     h->amd_x_coeff[17]*mag*(ox2+oy2)         +
     h->amd_x_coeff[18]*mag*ox*(ox2+oy2)      +
     h->amd_x_coeff[19]*colour;

    object_eta =
     h->amd_y_coeff[ 0]*oy                    + h->amd_y_coeff[ 1]*ox     +
     h->amd_y_coeff[ 2]                       + h->amd_y_coeff[ 3]*oy2    +
     h->amd_y_coeff[ 4]*oy*ox                 + h->amd_y_coeff[ 5]*ox2    +
     h->amd_y_coeff[ 6]*(oy2+ox2)             + h->amd_y_coeff[ 7]*oy3    +
     h->amd_y_coeff[ 8]*oy2*ox                + h->amd_y_coeff[ 9]*oy*ox2 +
     h->amd_y_coeff[10]*ox3                   +
     h->amd_y_coeff[11]*oy*(oy2+ox2)          +
     h->amd_y_coeff[12]*oy*(oy2+ox2)*(oy2*ox2)+
     h->amd_y_coeff[13]*mag                   + h->amd_y_coeff[14]*mag*mag+
     h->amd_y_coeff[15]*mag*mag*mag           + h->amd_y_coeff[16]*mag*oy +
     h->amd_y_coeff[17]*mag*(oy2+ox2)         +
     h->amd_y_coeff[18]*mag*oy*(oy2+ox2)      +
     h->amd_y_coeff[19]*colour;
    /*
     *  Convert to RA and Dec 
     *  Note that ra and dec are already pointers, so we don't need
     *  to pass by address
     */
    transtdeq(h,object_xi,object_eta,ra,dec);
}
