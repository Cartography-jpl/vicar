/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* hinv.c   Inverse H-transform of NX x NY integer image
 *
 * Programmer: R. White     Date: 10 April 1992
 */
#include <stdio.h>
#include <math.h>

static void unshuffle();
extern void hsmooth();

extern void
hinv(a,nx,ny,smooth,scale)
int a[];
int nx,ny;
int smooth;   /* 0 for no smoothing, else smooth during inversion */
int scale;    /* used if smoothing is specified */
{
int nmax, log2n, i, j, k;
int b11, b10, b01, b00;
int nxtop,nytop,nxf,nyf,c;
int oddx,oddy;
int shift,dp, dm;
int s10, s00;
int *tmp;

    /*
     * log2n is log2 of max(nx,ny) rounded up to next power of 2
     */
    nmax = (nx>ny) ? nx : ny;
    log2n = log((float) nmax)/log(2.0)+0.5;
    if ( nmax > (1<<log2n) ) {
        log2n += 1;
    }
    /*
     * get temporary storage for shuffling elements
     */
    tmp = (int *) malloc(((nmax+1)/2)*sizeof(int));
    if (tmp == (int *) NULL) {
        fprintf(stderr, "hinv: insufficient memory\n");
        exit(-1);
    }
    /*
     * do log2n expansions
     *
     * We're indexing a as a 2-D array with dimensions (nx,ny).
     * dp,dm are used for rounding positive, negative numbers during shift.
     */
    shift = 1;
    dp = 0;
    dm = 1;
    nxtop = 1;
    nytop = 1;
    nxf = nx;
    nyf = ny;
    c = 1<<log2n;
    for (k = log2n-1; k>=0; k--) {
        /*
         * this somewhat cryptic code generates the sequence
         * ntop[k-1] = (ntop[k]+1)/2, where ntop[log2n] = n
         */
        c = c>>1;
        nxtop = nxtop<<1;
        nytop = nytop<<1;
        if (nxf <= c) { nxtop -= 1; } else { nxf -= c; }
        if (nyf <= c) { nytop -= 1; } else { nyf -= c; }
        /*
         * double divisor on last pass
         */
        if (k == 0) {
            shift = 2;
            dp = 2;
            /* for negative numbers, dm = - dp + 2**shift -1 = -2+3 = +1 */
            dm = 1;
        }
        /*
         * unshuffle in each dimension to interleave coefficients
         */
        for (i = 0; i<nxtop; i++) {
            unshuffle(&a[ny*i],nytop,1,tmp);
        }
        for (j = 0; j<nytop; j++) {
            unshuffle(&a[j],nxtop,ny,tmp);
        }
        /*
         * smooth by interpolating coefficients if SMOOTH != 0
         */
        if (smooth) hsmooth(a,nxtop,nytop,ny,scale);
        oddx = nxtop % 2;
        oddy = nytop % 2;
        for (i = 0; i<nxtop-oddx; i += 2) {
            s00 = ny*i;             /* s00 is index of a[i,j]   */
            s10 = s00+ny;           /* s10 is index of a[i+1,j] */
            for (j = 0; j<nytop-oddy; j += 2) {
                /*
                 * Divide sums by 2 (4 last time) (shift right 1 (or 2) bits).
                 * Variable d allows rounding.
                 *
                 * For negative numbers a different value is added so rounding
                 * is symmetrical for +,-.  This could be simplified for images
                 * that must be positive, but for generality the stuff for
                 * negative numbers was left in.
                 */
                b11 = a[s00] + a[s10] + a[s00+1] + a[s10+1];
                b10 = a[s00] + a[s10] - a[s00+1] - a[s10+1];
                b01 = a[s00] - a[s10] + a[s00+1] - a[s10+1];
                b00 = a[s00] - a[s10] - a[s00+1] + a[s10+1];
                a[s10+1] = ((b11>0) ? (b11+dp) : (b11+dm)) >> shift;
                a[s10  ] = ((b10>0) ? (b10+dp) : (b10+dm)) >> shift;
                a[s00+1] = ((b01>0) ? (b01+dp) : (b01+dm)) >> shift;
                a[s00  ] = ((b00>0) ? (b00+dp) : (b00+dm)) >> shift;
                s00 += 2;
                s10 += 2;
            }
            if (oddy) {
                /*
                 * do last element in row if row length is odd
                 * s00+1, s10+1 are off edge
                 */
                b10 = a[s00] + a[s10];
                b00 = a[s00] - a[s10];
                a[s10  ] = ((b10>0) ? (b10+dp) : (b10+dm)) >> shift;
                a[s00  ] = ((b00>0) ? (b00+dp) : (b00+dm)) >> shift;
            }
        }
        if (oddx) {
            /*
             * do last row if column length is odd
             * s10, s10+1 are off edge
             */
            s00 = ny*i;
            for (j = 0; j<nytop-oddy; j += 2) {
                b01 = a[s00] + a[s00+1];
                b00 = a[s00] - a[s00+1];
                a[s00+1] = ((b01>0) ? (b01+dp) : (b01+dm)) >> shift;
                a[s00  ] = ((b00>0) ? (b00+dp) : (b00+dm)) >> shift;
                s00 += 2;
            }
            if (oddy) {
                /*
                 * do corner element if both row and column lengths are odd
                 * s00+1, s10, s10+1 are off edge
                 */
                b00 = a[s00];
                a[s00  ] = ((b00>0) ? (b00+dp) : (b00+dm)) >> shift;
            }
        }
    }
    free(tmp);
}


static void
unshuffle(a,n,n2,tmp)
int a[];    /* array to shuffle                 */
int n;      /* number of elements to shuffle    */
int n2;     /* second dimension                 */
int tmp[];  /* scratch storage                  */
{
int i;
int nhalf;
int *p1, *p2, *pt;

    /*
     * copy 2nd half of array to tmp
     */
    nhalf = (n+1)>>1;
    pt = tmp;
    p1 = &a[n2*nhalf];              /* pointer to a[i]          */
    for (i=nhalf; i<n; i++) {
        *pt = *p1;
        p1 += n2;
        pt += 1;
    }
    /*
     * distribute 1st half of array to even elements
     */
    p2 = &a[ n2*(nhalf-1) ];        /* pointer to a[i]          */
    p1 = &a[(n2*(nhalf-1))<<1];     /* pointer to a[2*i]        */
    for (i=nhalf-1; i >= 0; i--) {
        *p1 = *p2;
        p2 -= n2;
        p1 -= (n2+n2);
    }
    /*
     * now distribute 2nd half of array (in tmp) to odd elements
     */
    pt = tmp;
    p1 = &a[n2];                    /* pointer to a[i]          */
    for (i=1; i<n; i += 2) {
        *p1 = *pt;
        p1 += (n2+n2);
        pt += 1;
    }
}
