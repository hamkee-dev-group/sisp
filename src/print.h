#ifndef SISP_PRINT_H
#define SISP_PRINT_H

#include <stdio.h>

#include <stddef.h>

#include "sisp.h"

/* Enough for any double rendered by format_double. "%.*f" produces at most
   (digits before the point) + 1 + precision. A double large enough to have
   many integer digits is necessarily an integer, so it round-trips at a tiny
   precision and never reaches the cap; the cap is only reached by values near
   zero, where there is a single digit before the point. Worst case is
   therefore about 1 + 1 + DOUBLE_TEXT_PRECISION_MAX. */
#define DOUBLE_TEXT_MAX 512

/* Renders VALUE at the shortest precision that reads back to the same double,
   so floats and matrix cells round-trip through the reader. Returns the
   length written. One implementation, because printing a float and
   num-to-str'ing one are the same question asked twice. */
extern int format_double(char *buffer, size_t size, double value);
extern void print_object(FILE *, const struct object *);
#endif
