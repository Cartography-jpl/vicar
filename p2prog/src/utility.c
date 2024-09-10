/* Copyright (c) 1993 Association of Universities for Research
 * in Astronomy. All rights reserved. Produced under National
 * Aeronautics and Space Administration Contract No. NAS5-26555.
 */
/* utility.c   general purpose routines for gasp stuff
 *
 * char *remquotes(string)  strips quotes (') off string
 * char *strtrim(string)    strips leading blanks off string
 * double *dtotal(a,n)      sums double precision array a(n)
 */
#include <string.h>

extern char
*remquotes(string)
char string[];
{
char *i1, *i2;

    i1 = strchr(string,'\'');
    if (i1 == NULL) return(string);
    i2 = strrchr(string,'\'');
    if (i2 == i1) return(string);
    i1 += 1;
    *i2 = '\0';
    return(i1);
}

#define BLANKS  " "

extern char
*strtrim(string)
char string[];
{
int c;

      c = strspn(string, BLANKS);
      return(&(string[c]));
}

extern double
dtotal(a,n)
double a[];
int n;
{
int i, d;

      d = 0.0;
      for (i=0; i<n; i++) d += a[i];
      return(d);
}
