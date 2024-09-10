/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/*       PLTMODEL
 * PURPOSE:
 *       Compute values of plate model and its partial derivatives
 *       for use in computing inverse.
 * CALLING SEQUENCE:
 *       PLTMODEL(header,X,Y,MAG,COLOUR,F,FX,FY,G,GX,GY)
 * INPUTS:
 *       h        - header structure
 *       X,Y      - REAL*8 position
 *       MAG      - magnitude
 *       COLOUR   - colour
 * OUTPUTS:
 *       F        - REAL*8 St.coord xi
 *       FX       - REAL*8 Deriv. xi wrt x
 *       FY       - REAL*8 Deriv. xi wrt y
 *       G        - REAL*8 St.coord eta
 *       GX       - REAL*8 Deriv. eta wrt x
 *       GY       - REAL*8 Deriv. eta wrt y
 * PROCEDURE:
 *       Computes value of plate model and the partial derivatives.
 * MODIFICATION HISTORY
 *       Converted to Fortran from IDL, R. White, 7 May 1990
 *       Converted to C from Fortran, R. White, 30 July 1991
 */
#include "header.h"

extern void
pltmodel(h,x,y,mag,colour,f,fx,fy,g,gx,gy)
Header *h;
double x,y,mag,colour;
double *f, *fx, *fy, *g, *gx, *gy;
{
double cjunk,x4,y4;

    /*
     *  X plate model
     */
    cjunk=(x*x+y*y)*(x*x+y*y);
    x4 = (x*x)*(x*x);
    y4 = (y*y)*(y*y);
    *f=h->amd_x_coeff[0]*x                 + h->amd_x_coeff[1]*y              +
       h->amd_x_coeff[2]                   + h->amd_x_coeff[3]*x*x            +
       h->amd_x_coeff[4]*x*y               + h->amd_x_coeff[5]*y*y            +
       h->amd_x_coeff[6]*(x*x+y*y)         + h->amd_x_coeff[7]*x*x*x          +
       h->amd_x_coeff[8]*x*x*y             + h->amd_x_coeff[9]*x*y*y          +
       h->amd_x_coeff[10]*y*y*y            + h->amd_x_coeff[11]*x*(x*x+y*y)   +
       h->amd_x_coeff[12]*x*cjunk          + h->amd_x_coeff[13]*mag           +
       h->amd_x_coeff[14]*mag*mag          + h->amd_x_coeff[15]*mag*mag*mag   +
       h->amd_x_coeff[16]*mag*x            + h->amd_x_coeff[17]*mag*(x*x+y*y) +
       h->amd_x_coeff[18]*mag*x*(x*x+y*y)  + h->amd_x_coeff[19]*colour;
    /*
     *  Derivative of X model wrt x
     */
    *fx=h->amd_x_coeff[0]                              +
        h->amd_x_coeff[3]*2.0*x                        +
        h->amd_x_coeff[4]*y                            +
        h->amd_x_coeff[6]*2.0*x                        +
        h->amd_x_coeff[7]*3.0*x*x                      +
        h->amd_x_coeff[8]*2.0*x*y                      +
        h->amd_x_coeff[9]*y*y                          +
        h->amd_x_coeff[11]*(3.0*x*x+y*y)               +
        h->amd_x_coeff[12]*(5.0*x4  +6.0*x*x*y*y+y4  ) +
        h->amd_x_coeff[16]*mag                         +
        h->amd_x_coeff[17]*mag*2.0*x                   +
        h->amd_x_coeff[18]*mag*(3.0*x*x+y*y);
    /*
     *  Derivative of X model wrt y
     */
    *fy=h->amd_x_coeff[1]                     +
        h->amd_x_coeff[4]*x                   +
        h->amd_x_coeff[5]*2.0*y               +
        h->amd_x_coeff[6]*2.0*y               +
        h->amd_x_coeff[8]*x*x                 +
        h->amd_x_coeff[9]*x*2.0*y             +
        h->amd_x_coeff[10]*3.0*y*y            +
        h->amd_x_coeff[11]*2.0*x*y            +
        h->amd_x_coeff[12]*4.0*x*y*(x*x+y*y)  +
        h->amd_x_coeff[17]*mag*2.0*y          +
        h->amd_x_coeff[18]*mag*2.0*x*y;
    /*
     *  Y plate model
     */
    *g=h->amd_y_coeff[0]*y                + h->amd_y_coeff[1]*x              +
       h->amd_y_coeff[2]                  + h->amd_y_coeff[3]*y*y            +
       h->amd_y_coeff[4]*y*x              + h->amd_y_coeff[5]*x*x            +
       h->amd_y_coeff[6]*(x*x+y*y)        + h->amd_y_coeff[7]*y*y*y          +
       h->amd_y_coeff[8]*y*y*x            + h->amd_y_coeff[9]*y*x*x          +
       h->amd_y_coeff[10]*x*x*x           + h->amd_y_coeff[11]*y*(x*x+y*y)   +
       h->amd_y_coeff[12]*y*cjunk         + h->amd_y_coeff[13]*mag           +
       h->amd_y_coeff[14]*mag*mag         + h->amd_y_coeff[15]*mag*mag*mag   +
       h->amd_y_coeff[16]*mag*y           + h->amd_y_coeff[17]*mag*(x*x+y*y) +
       h->amd_y_coeff[18]*mag*y*(x*x+y*y) + h->amd_y_coeff[19]*colour;
    /*
     *  Derivative of Y model wrt x
     */
    *gx=h->amd_y_coeff[1]                    +
        h->amd_y_coeff[4]*y                  +
        h->amd_y_coeff[5]*2.0*x              +
        h->amd_y_coeff[6]*2.0*x              +
        h->amd_y_coeff[8]*y*y                +
        h->amd_y_coeff[9]*y*2.0*x            +
        h->amd_y_coeff[10]*3.0*x*x           +
        h->amd_y_coeff[11]*2.0*x*y           +
        h->amd_y_coeff[12]*4.0*x*y*(x*x+y*y) +
        h->amd_y_coeff[17]*mag*2.0*x         +
        h->amd_y_coeff[18]*mag*y*2.0*x;
    /*
     *  Derivative of Y model wrt y
     */
    *gy=h->amd_y_coeff[0]                              +
        h->amd_y_coeff[3]*2.0*y                        + 
        h->amd_y_coeff[4]*x                            +
        h->amd_y_coeff[6]*2.0*y                        +
        h->amd_y_coeff[7]*3.0*y*y                      +
        h->amd_y_coeff[8]*2.0*y*x                      +
        h->amd_y_coeff[9]*x*x                          +
        h->amd_y_coeff[11]*3.0*y*y                     +
        h->amd_y_coeff[12]*(5.0*y4  +6.0*x*x*y*y+x4  ) +
        h->amd_y_coeff[16]*mag                         +
        h->amd_y_coeff[17]*mag*2.0*y                   +
        h->amd_y_coeff[18]*mag*(x*x+3.0*y*y);
}
