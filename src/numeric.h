#ifndef SISP_NUMERIC_H
#define SISP_NUMERIC_H

#include <stdint.h>

#include "sisp.h"

long int gcd(long int, long int);
long int checked_add(long int, long int);
long int checked_sub(long int, long int);
long int checked_mul(long int, long int);

double number_to_double(const struct object *);
objectp make_number_from_double(double);
int number_compare(objectp, objectp);

void rng_seed(uint64_t);
uint64_t rng_u64(void);
double rng_uniform(void);

#endif
