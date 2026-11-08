#include <limits.h>
#include <math.h>
#include <time.h>

#include "sisp.h"
#include "runtime.h"
#include "numeric.h"

/* Always non-negative, so every caller can divide by the result without a
   sign check. The Euclidean loop alone is sign-dependent -- gcd(4, -6) came
   out -2 and gcd(-3, 6) came out -3 -- and dividing by a -1 result is
   LONG_MIN / -1, which is undefined. Three call sites guarded against that
   by hand and a fourth (builtin_subtract) had been missed; normalising here
   removes the hazard and all four guards.

   |LONG_MIN| is not representable, so that one case answers 1: no reduction
   happens, which is always safe, merely not maximal. */
long int
gcd(long int a, long int b)
{
	long int tmp;

	if (a < b)
	{
		tmp = a;
		a = b;
		b = tmp;
	}
	while (b != 0L)
	{
		tmp = (b == -1L) ? 0L : a % b;
		a = b;
		b = tmp;
	}
	if (a == LONG_MIN)
		return 1L;
	return a < 0L ? -a : a;
}

long int
checked_add(long int a, long int b)
{
	long int result;

	if (__builtin_add_overflow(a, b, &result))
		sisp_error("INTEGER OVERFLOW");
	return result;
}

long int
checked_sub(long int a, long int b)
{
	long int result;

	if (__builtin_sub_overflow(a, b, &result))
		sisp_error("INTEGER OVERFLOW");
	return result;
}

long int
checked_mul(long int a, long int b)
{
	long int result;

	if (__builtin_mul_overflow(a, b, &result))
		sisp_error("INTEGER OVERFLOW");
	return result;
}

double
number_to_double(const struct object *number)
{
	if (number->type == OBJ_INTEGER)
		return (double)number->value.i;
	if (number->type == OBJ_RATIONAL)
		return (double)number->value.rational.n / (double)number->value.rational.d;
	return number->value.f;
}

objectp
make_number_from_double(double value)
{
	if (isfinite(value) && value >= (double)LONG_MIN && value < (double)LONG_MAX &&
		value == (double)(long int)value)
		return make_integer((long int)value);
	if (!isfinite(value))
		sisp_error("OVERFLOW");
	return make_float(value);
}

static int
safe_product_compare(long int a, long int b, long int c, long int d)
{
#ifdef __SIZEOF_INT128__
	__int128 left = (__int128)a * b;
	__int128 right = (__int128)c * d;
#else
	long long left = (long long)a * b;
	long long right = (long long)c * d;
#endif

	return (left < right) ? -1 : (left > right) ? 1 : 0;
}

int
number_compare(objectp left, objectp right)
{
	double left_value, right_value;

	if (left->type == OBJ_FLOAT || right->type == OBJ_FLOAT)
	{
		left_value = number_to_double(left);
		right_value = number_to_double(right);
		return (left_value < right_value) ? -1 : (left_value > right_value) ? 1 : 0;
	}
	if (left->type == OBJ_INTEGER && right->type == OBJ_INTEGER)
		return (left->value.i < right->value.i)	  ? -1
			   : (left->value.i > right->value.i) ? 1
												  : 0;
	if (left->type == OBJ_INTEGER && right->type == OBJ_RATIONAL)
		return safe_product_compare(left->value.i, right->value.rational.d,
									right->value.rational.n, 1L);
	if (left->type == OBJ_RATIONAL && right->type == OBJ_INTEGER)
		return safe_product_compare(left->value.rational.n, 1L, left->value.rational.d,
									right->value.i);
	return safe_product_compare(left->value.rational.n, right->value.rational.d,
								right->value.rational.n, left->value.rational.d);
}

static uint64_t rng_state[4];
static int rng_seeded;

static uint64_t
splitmix64(uint64_t *state)
{
	uint64_t value = (*state += 0x9e3779b97f4a7c15ULL);

	value = (value ^ (value >> 30)) * 0xbf58476d1ce4e5b9ULL;
	value = (value ^ (value >> 27)) * 0x94d049bb133111ebULL;
	return value ^ (value >> 31);
}

void
rng_seed(uint64_t seed)
{
	uint64_t state = seed;
	int index;

	for (index = 0; index < 4; index++)
		rng_state[index] = splitmix64(&state);
	rng_seeded = 1;
}

static uint64_t
rotate_left(uint64_t value, int bits)
{
	return (value << bits) | (value >> (64 - bits));
}

uint64_t
rng_u64(void)
{
	uint64_t result, shifted;

	if (!rng_seeded)
		rng_seed((uint64_t)time(NULL));
	result = rotate_left(rng_state[1] * 5ULL, 7) * 9ULL;
	shifted = rng_state[1] << 17;
	rng_state[2] ^= rng_state[0];
	rng_state[3] ^= rng_state[1];
	rng_state[1] ^= rng_state[2];
	rng_state[0] ^= rng_state[3];
	rng_state[2] ^= shifted;
	rng_state[3] = rotate_left(rng_state[3], 45);
	return result;
}

double
rng_uniform(void)
{
	return (double)(rng_u64() >> 11) * (1.0 / 9007199254740992.0);
}
