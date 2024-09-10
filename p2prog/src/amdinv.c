/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*  AMDINV
 *
 * PURPOSE:
 *  Compute the x,y position on the plate from the RA,Dec using
 *  the inverse of the CALOBCC solution.
 *
 * CALLING SEQUENCE:
 *  AMDINV(HEADER,RA,DEC,MAG,COL,X,Y)
 *
 * INPUTS:
 *  HEADER   - Structure with header information
 *  RA,DEC   - REAL*8 Celestial Coords. (radians)
 *  MAG      - Magnitude
 *  COL      - Colour
 *
 * OUTPUTS:
 *  X,Y      - pixel position
 *
 * PROCEDURE:
 *  Newtons method is used to iterate from a starting position
 *  till convergence is reached.
 *
 * MODIFICATION HISTORY
 *  Converted from GASP IDL routine to Fortran, R. White, 7 May 1990
 *  Converted from Fortran to C, R. White, 30 July 1991
 */

#include <stdio.h>
#include <math.h>
#include "header.h"

extern void traneqstd();
extern void pltmodel();

extern void
amdinv(header,ra,dec,mag,col,x,y)
Header *header;
double ra, dec;
float mag, col;
float *x, *y;
{
int i, max_iterations;
float tolerance;
double xi, eta, object_x, object_y, delta_x, delta_y, f, fx, fy, g, gx, gy;

    /*
     *  Initialize
     */
    i = 0;
    max_iterations = 50;
    tolerance = 0.0000005;
    delta_x = tolerance;
    delta_y = tolerance;
    /*
     *  Convert RA and Dec to St.coords
     */
    traneqstd(header,ra,dec,&xi,&eta);
    /*
     *  Set initial value for x,y
     */
    object_x = xi/header->plt_scale;
    object_y = eta/header->plt_scale;
    /*
     *  Iterate by Newtons method
     */
    for(i = 0; i < max_iterations; i++) {
        pltmodel(header,object_x,object_y,mag,col,&f,&fx,&fy,&g,&gx,&gy);
        f = f-xi;
        g = g-eta;
        delta_x = (-f*gy+g*fy)/(fx*gy-fy*gx);
        delta_y = (-g*fx+f*gx)/(fx*gy-fy*gx);
        object_x = object_x+delta_x;
        object_y = object_y+delta_y;
        if ((fabs(delta_x) < tolerance) && (fabs(delta_y) < tolerance)) break;
    }
    /*
     *  Convert mm from plate center to pixels
     */
    *x = (header->ppo_coeff[2]-object_x*1000.0)/header->x_pixel_size;
    *y = (header->ppo_coeff[5]+object_y*1000.0)/header->y_pixel_size;
}
