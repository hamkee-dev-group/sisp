#include <math.h>
#include <errno.h>
#include <stdint.h>

#include <limits.h>

#include "sisp.h"
#include "doc.h"
#include "runtime.h"
#include "builtins.h"
#include "eval.h"
#include "numeric.h"
#include "print.h"

/* a NaN float (e.g. a matrix cell from sqrt of a negative) is unordered:
   every comparison with it is false. number_compare alone reports 0 ("equal").
 */
static int
number_is_nan(objectp value)
{
	return value->type == OBJ_FLOAT && value->value.f != value->value.f;
}

/*
 * Thin wrappers over <math.h>, generated so each is a single line.
 *   MATH1     : double -> double, e.g. (sin x)
 *   MATH1_DOM : same, but traps EDOM (sqrt of negative, log of <= 0, ...)
 *   MATH2     : (double, double) -> double, e.g. (atan2 y x)
 * Numeric args are promoted with number_to_double; the result demotes to an
 * integer when integral via make_number_from_double. Overflow (ERANGE) is left
 * to produce inf, which prints and round-trips within double range. The
 * generated symbols are builtin_<name>, declared in builtins.h and registered
 * in builtins.c.
 */
#define MATH1(cfn, operation)                                                          \
	objectp builtin_##cfn(const struct object *args)                                   \
	{                                                                                  \
		objectp a;                                                                     \
		a = eval(car(args));                                                           \
		require_numeric(a, operation);                                                 \
		return make_number_from_double(cfn(number_to_double(a)));                      \
	}

#define MATH1_DOM(cfn, operation)                                                      \
	objectp builtin_##cfn(const struct object *args)                                   \
	{                                                                                  \
		objectp a;                                                                     \
		double r;                                                                      \
		a = eval(car(args));                                                           \
		require_numeric(a, operation);                                                 \
		errno = 0;                                                                     \
		r = cfn(number_to_double(a));                                                  \
		require_condition((errno != EDOM), operation, "DOMAIN ERROR", a);              \
		return make_number_from_double(r);                                             \
	}

#define MATH2(cfn, operation)                                                          \
	objectp builtin_##cfn(const struct object *args)                                   \
	{                                                                                  \
		objectp a, b;                                                                  \
		a = eval(car(args));                                                           \
		require_numeric(a, operation);                                                 \
		b = eval(cadr(args));                                                          \
		require_numeric(b, operation);                                                 \
		return make_number_from_double(cfn(number_to_double(a), number_to_double(b))); \
	}

/* Matrix-generic variants apply the named unary operation elementwise. */
#define MATH1_MAT(cfn, operation, matfn)                                               \
	objectp builtin_##cfn(const struct object *args)                                   \
	{                                                                                  \
		objectp a;                                                                     \
		a = eval(car(args));                                                           \
		if (a->type == OBJ_MATRIX)                                                     \
			return matrix_unary_math(a, matfn);                                        \
		require_numeric(a, operation);                                                 \
		return make_number_from_double(cfn(number_to_double(a)));                      \
	}
#define MATH1_DOM_MAT(cfn, operation, matfn)                                           \
	objectp builtin_##cfn(const struct object *args)                                   \
	{                                                                                  \
		objectp a;                                                                     \
		double r;                                                                      \
		a = eval(car(args));                                                           \
		if (a->type == OBJ_MATRIX)                                                     \
			return matrix_unary_math(a, matfn);                                        \
		require_numeric(a, operation);                                                 \
		errno = 0;                                                                     \
		r = cfn(number_to_double(a));                                                  \
		require_condition((errno != EDOM), operation, "DOMAIN ERROR", a);              \
		return make_number_from_double(r);                                             \
	}

MATH1(sin, "SIN")
MATH1(cos, "COS")
MATH1(tan, "TAN")
MATH1(atan, "ATAN")
MATH1(sinh, "SINH")
MATH1(cosh, "COSH")
MATH1(asinh, "ASINH")
MATH1(cbrt, "CBRT")
MATH1_MAT(exp, "EXP", MATRIX_EXP)
MATH1_MAT(tanh, "TANH", MATRIX_TANH)
MATH1_DOM_MAT(sqrt, "SQRT", MATRIX_SQRT)
MATH1_DOM(asin, "ASIN")
MATH1_DOM(acos, "ACOS")
MATH1_DOM(acosh, "ACOSH")
MATH1_DOM(atanh, "ATANH")
MATH1_DOM(log, "LOG")
MATH1_DOM(log10, "LOG10")
MATH1_DOM(log2, "LOG2")

/* relu: max(0,x) on a scalar, elementwise on a matrix — the one activation
   with no libc scalar analog, so it is spelled out. */
objectp
builtin_relu(const struct object *args)
{
	objectp a;
	double x;

	a = eval(car(args));
	if (a->type == OBJ_MATRIX)
		return matrix_unary_math(a, MATRIX_RELU);
	require_numeric(a, "RELU");
	x = number_to_double(a);
	return make_number_from_double(x > 0.0 ? x : 0.0);
}
MATH2(atan2, "ATAN2")
MATH2(hypot, "HYPOT")

/* ---- exact-aware rounding of a rational n/d (d > 0) to an integer ----
   mode: 0 floor (-> -inf), 1 ceil (-> +inf), 2 truncate (-> 0), 3 round
   half-even */
static long int
round_rat(long int n, long int d, int mode)
{
	long int q = n / d;		  /* C truncates toward zero */
	long int r = n - q * d; /* same sign as n, in (-d, d) */
	long int fq, fr, other;

	if (r == 0)
		return q;
	if (mode == 2)
		return q; /* truncate */
	if (mode == 0)
		return (n < 0) ? q - 1 : q; /* floor */
	if (mode == 1)
		return (n > 0) ? q + 1 : q; /* ceil */
	/* round half-even: work from the floor */
	fq = (n < 0) ? q - 1 : q;
	fr = n - fq * d; /* 0 < fr < d */
	other = d - fr;	 /* avoids 2*fr overflow */
	if (fr < other)
		return fq;
	if (fr > other)
		return fq + 1;
	return (fq % 2 == 0) ? fq : fq + 1; /* exact half -> even */
}

DOC("abs", "Magnitude, discarding sign.\n"
		   "  (abs -3) => 3\n"
		   "See: signum max")
objectp
builtin_abs(const struct object *args)
{
	objectp a, r;
	a = eval(car(args));
	require_numeric(a, "ABS");
	if (a->type == OBJ_FLOAT)
		return make_number_from_double(fabs(a->value.f));
	if (a->type == OBJ_INTEGER)
	{
		require_condition((a->value.i != LONG_MIN), "ABS", "INTEGER OVERFLOW", a);
		if (a->value.i >= 0)
			return a;
		r = make_integer(-a->value.i);
		return r;
	}
	/* rational: denominator is normalised positive, so negate the numerator */
	require_condition((a->value.rational.n != LONG_MIN), "ABS", "INTEGER OVERFLOW", a);
	if (a->value.rational.n >= 0)
		return a;
	r = make_rational(-a->value.rational.n, a->value.rational.d);
	return r;
}

DOC("floor", "Largest integer not greater than the argument.\n"
			 "  (floor 2.7) => 2\n"
			 "  (floor -2.1) => -3\n"
			 "See: ceiling round truncate")
objectp
builtin_floor(const struct object *args)
{
	objectp a, r;
	a = eval(car(args));
	require_numeric(a, "FLOOR");
	if (a->type == OBJ_INTEGER)
		return a;
	if (a->type == OBJ_FLOAT)
		return make_number_from_double(floor(a->value.f));
	r = make_integer(round_rat(a->value.rational.n, a->value.rational.d, 0));
	return r;
}

DOC("ceiling", "Smallest integer not less than the argument.\n"
			   "  (ceiling 2.1) => 3\n"
			   "See: floor round")
objectp
builtin_ceiling(const struct object *args)
{
	objectp a, r;
	a = eval(car(args));
	require_numeric(a, "CEILING");
	if (a->type == OBJ_INTEGER)
		return a;
	if (a->type == OBJ_FLOAT)
		return make_number_from_double(ceil(a->value.f));
	r = make_integer(round_rat(a->value.rational.n, a->value.rational.d, 1));
	return r;
}

DOC("truncate", "Integer part, discarding the fraction, toward zero.\n"
				"  (truncate -2.7) => -2\n"
				"  (truncate 2.7) => 2\n"
				"See: floor ceiling round")
objectp
builtin_truncate(const struct object *args)
{
	objectp a, r;
	a = eval(car(args));
	require_numeric(a, "TRUNCATE");
	if (a->type == OBJ_INTEGER)
		return a;
	if (a->type == OBJ_FLOAT)
		return make_number_from_double(trunc(a->value.f));
	r = make_integer(round_rat(a->value.rational.n, a->value.rational.d, 2));
	return r;
}

DOC("round", "Nearest integer.\n"
			 "  (round 2.5) => 2\n"
			 "  (round 2.7) => 3\n"
			 "Note: Halfway cases round to even, so 2.5 gives 2 and 3.5 gives 4.\n"
			 "See: floor ceiling truncate")
objectp
builtin_round(const struct object *args)
{
	objectp a, r;
	a = eval(car(args));
	require_numeric(a, "ROUND");
	if (a->type == OBJ_INTEGER)
		return a;
	if (a->type == OBJ_FLOAT)
		return make_number_from_double(
			rint(a->value.f)); /* half-even (default FP mode) */
	r = make_integer(round_rat(a->value.rational.n, a->value.rational.d, 3));
	return r;
}

DOC("signum", "-1, 0 or 1 according to the sign.\n"
			  "  (signum -3) => -1\n"
			  "  (signum 0) => 0\n"
			  "See: abs")
objectp
builtin_signum(const struct object *args)
{
	objectp a, r;
	long int v;
	double d;

	a = eval(car(args));
	require_numeric(a, "SIGNUM");
	if (a->type == OBJ_FLOAT)
	{
		d = a->value.f;
		return make_number_from_double((double)((d > 0.0) - (d < 0.0)));
	}
	v = (a->type == OBJ_INTEGER) ? a->value.i : a->value.rational.n;
	r = make_integer((v > 0) - (v < 0));
	return r;
}

DOC("gcd", "Greatest common divisor.\n"
		   "  (gcd 12 18) => 6\n"
		   "See: lcm")
objectp
builtin_gcd(const struct object *args)
{
	objectp a, r;
	long int g = 0;
	while (args != nil)
	{
		a = eval(car(args));
		require_type(a, OBJ_INTEGER, "GCD");
		require_condition((a->value.i != LONG_MIN), "GCD", "INTEGER OVERFLOW", a);
		g = gcd(g, a->value.i < 0 ? -a->value.i : a->value.i);
		args = cdr(args);
	}
	r = make_integer(g);
	return r;
}

DOC("lcm", "Least common multiple.\n"
		   "  (lcm 4 6) => 12\n"
		   "See: gcd")
objectp
builtin_lcm(const struct object *args)
{
	objectp a, r;
	long int l = 1, v;

	while (args != nil)
	{
		a = eval(car(args));
		require_type(a, OBJ_INTEGER, "LCM");
		require_condition((a->value.i != LONG_MIN), "LCM", "INTEGER OVERFLOW", a);
		v = a->value.i < 0 ? -a->value.i : a->value.i;
		if (v == 0)
			l = 0;
		else if (l != 0)
			l = checked_mul(l / gcd(l, v), v);
		args = cdr(args);
	}
	r = make_integer(l);
	return r;
}

DOC("evenp", "Whether an integer is even.\n"
			 "  (evenp 2) => t\n"
			 "See: oddp")
objectp
builtin_evenp(const struct object *args)
{
	objectp a;
	a = eval(car(args));
	require_type(a, OBJ_INTEGER, "EVENP");
	return (a->value.i % 2 == 0) ? t : nil;
}

DOC("oddp", "Whether an integer is odd.\n"
			"  (oddp 3) => t\n"
			"See: evenp")
objectp
builtin_oddp(const struct object *args)
{
	objectp a;
	a = eval(car(args));
	require_type(a, OBJ_INTEGER, "ODDP");
	return (a->value.i % 2 != 0) ? t : nil;
}

DOC("random",
	"A pseudo-random float in [0,1).\n"
	"Mutates: The generator state, so successive calls differ.\n"
	"Note: Seeded from the wall clock on first use, so a program that does not "
	"call seed produces different output on every run.\n"
	"See: seed")
objectp
builtin_random(const struct object *args)
{
	objectp a, r;
	uint64_t bound, threshold, random_value;
	double numeric_bound;

	if (args == nil)
		return make_number_from_double(rng_uniform());
	require_condition((cdr(args) == nil), "RANDOM", "TAKES ZERO OR ONE ARGUMENT", args);
	a = eval(car(args));
	require_numeric(a, "RANDOM");
	if (a->type == OBJ_INTEGER)
	{
		require_condition((a->value.i > 0), "RANDOM", "MUST BE POSITIVE", a);
		bound = (uint64_t)a->value.i;
		threshold = (0ULL - bound) % bound; /* 2^64 mod bound -> unbiased */
		do
			random_value = rng_u64();
		while (random_value < threshold);
		r = make_integer((long int)(random_value % bound));
		return r;
	}
	/* rational or float bound -> float in [0, bound) */
	numeric_bound = number_to_double(a);
	require_condition((numeric_bound > 0.0), "RANDOM", "MUST BE POSITIVE", a);
	return make_number_from_double(rng_uniform() * numeric_bound);
}

DOC("seed", "Set the pseudo-random generator's starting state.\n"
			"  (progn (seed 1) (numberp (random))) => t\n"
			"Mutates: The global generator state. Seeding with a fixed value "
			"makes a run using random reproducible.\n"
			"See: random")
objectp
builtin_seed(const struct object *args)
{
	objectp a;
	a = eval(car(args));
	require_type(a, OBJ_INTEGER, "SEED");
	rng_seed((uint64_t)a->value.i);
	return a;
}

/* ---- rationalize: simplest rational within tol of x (continued fraction) ----
 */
static void
simplest_rational(double lo, double hi, long int *pn, long int *pd, int depth)
{
	double c, floor_value;
	long int numerator, denominator;

	/* A zero (or sub-precision) tolerance leaves lo == hi, so the continued
	   fraction never terminates on its own; cap the depth and non-finite
	   inputs so it converges to the nearest integer instead of overflowing
	   the stack. 60 terms exhaust a double's precision. */
	if (depth >= 60 || !isfinite(lo) || !isfinite(hi) || fabs(lo) >= (double)LONG_MAX)
	{
		*pn =
			isfinite(lo) && fabs(lo) < (double)LONG_MAX ? (long int)floor(lo + 0.5) : 0;
		*pd = 1;
		return;
	}
	if (lo <= 0.0 && hi >= 0.0)
	{
		*pn = 0;
		*pd = 1;
		return;
	}
	if (hi < 0.0)
	{
		simplest_rational(-hi, -lo, pn, pd, depth);
		*pn = -*pn;
		return;
	}
	/* 0 < lo <= hi */
	c = ceil(lo);
	if (c <= hi)
	{
		*pn = (long int)c; /* an integer lies in [lo, hi] */
		*pd = 1;
		return;
	}
	floor_value = floor(lo);
	simplest_rational(1.0 / (hi - floor_value), 1.0 / (lo - floor_value), &numerator,
					  &denominator, depth + 1);
	/* convergents grow exponentially; a tolerance too small to fit a
	   long ratio raises INTEGER OVERFLOW rather than wrapping */
	*pn = checked_add(checked_mul((long int)floor_value, numerator), denominator);
	*pd = numerator;
}

DOC("rationalize", "The exact rational nearest a float.\n"
				   "  (rationalize 0.5) => 1/2\n"
				   "See: / num-to-str")
objectp
builtin_rationalize(const struct object *args)
{
	objectp x, r, tolerance_object;
	double xv, tol;
	long int n, d;
	require_condition((args != nil), "RATIONALIZE", "TAKES ONE OR TWO ARGUMENTS", args);
	x = eval(car(args));
	require_numeric(x, "RATIONALIZE");
	xv = number_to_double(x);
	if (cdr(args) != nil)
	{
		tolerance_object = eval(cadr(args));
		require_numeric(tolerance_object, "RATIONALIZE");
		tol = fabs(number_to_double(tolerance_object));
	}
	else
		tol = 1e-9;
	simplest_rational(xv - tol, xv + tol, &n, &d, 0);
	r = make_rational(n, d);
	return eval_rational(r); /* reduce + demote to int when d == 1 */
}

DOC("mod", "Remainder after division.\n"
		   "  (mod 7 3) => 1\n"
		   "  (mod -7 3) => 2\n"
		   "Note: The result takes the sign of the divisor, so it is a modulus "
		   "rather than C's truncating remainder.\n"
		   "See: / truncate")
objectp
builtin_mod(const struct object *args)
{
	objectp d, n, r;
	double numerator, denominator, remainder;

	n = eval(car(args));
	d = eval(car(cdr(args)));

	if (n->type == OBJ_FLOAT || d->type == OBJ_FLOAT)
	{
		require_numeric(n, "MOD");
		require_numeric(d, "MOD");
		numerator = number_to_double(n);
		denominator = number_to_double(d);
		require_condition((denominator != 0.0), "MOD", "MODULUS ZERO", d);
		remainder = fmod(numerator, denominator);
		if (remainder != 0.0 && ((remainder < 0.0) != (denominator < 0.0)))
			remainder += denominator;
		return make_number_from_double(remainder);
	}

	require_type(n, OBJ_INTEGER, "MOD");
	require_type(d, OBJ_INTEGER, "MOD");
	require_condition((d->value.i != 0), "MOD", "MODULUS ZERO", d);

	r = make_integer(0);
	if (d->value.i == -1L)
		r->value.i = 0;
	else
	{
		r->value.i = n->value.i % d->value.i;
		if (r->value.i != 0 && ((r->value.i < 0) != (d->value.i < 0)))
			r->value.i += d->value.i;
	}
	return r;
}

DOC("min", "Smallest of the arguments.\n"
		   "  (min 3 1 2) => 1\n"
		   "See: max")
/* min and max are the same walk with one comparison reversed. Both accept
   either a single collection or a list of arguments, and both reject an
   improper tail rather than silently treating (1 2 . 3) as (1 2). `keep`
   is the number_compare result that means "the candidate wins": -1 for min,
   1 for max. */
static objectp
extreme(const struct object *args, const char *operation, int keep)
{
	objectp a, best, b, node;

	a = eval(car(args));
	if (a->type == OBJ_CONS || a->type == OBJ_SET)
	{
		require_condition((cdr(args) == nil), operation, "TAKES A SINGLE COLLECTION", a);
		require_condition((a != empty), operation, "EMPTY COLLECTION", a);
		best = car(a);
		require_numeric(best, operation);
		for (node = cdr(a); node != nil; node = cdr(node))
		{
			require_condition((node->type == a->type), operation, "IMPROPER COLLECTION",
							  a);
			b = car(node);
			require_numeric(b, operation);
			if (number_compare(b, best) == keep)
				best = b;
		}
		return best;
	}
	require_numeric(a, operation);
	best = a;
	while ((args = cdr(args)) != nil)
	{
		b = eval(car(args));
		require_numeric(b, operation);
		if (number_compare(b, best) == keep)
			best = b;
	}
	return best;
}

objectp
builtin_min(const struct object *args)
{
	return extreme(args, "MIN", -1);
}
DOC("max", "Largest of the arguments.\n"
		   "  (max 3 1 2) => 3\n"
		   "See: min")
objectp
builtin_max(const struct object *args)
{
	return extreme(args, "MAX", 1);
}
/* <, <=, >, >= and = are one chained comparison with one predicate swapped.
   They were five byte-identical copies, which is how a fix lands in four of
   them and misses the fifth. number_compare answers -1, 0 or 1, so a three-bit
   mask of the acceptable answers names the operator and the loop is written
   once. */
#define CMP_LT 1
#define CMP_EQ 2
#define CMP_GT 4

static objectp
compare_chain(const struct object *args, const char *operation, int accepted)
{
	objectp a, b;

	a = eval(car(args));
	require_numeric(a, operation);
	while ((args = cdr(args)) != nil)
	{
		b = eval(car(args));
		require_numeric(b, operation);
		/* NaN is unordered, so every comparison with it is false.
		   number_compare on its own would report 0, i.e. "equal". */
		if (number_is_nan(a) || number_is_nan(b))
			return nil;
		if (!(accepted & (1 << (number_compare(a, b) + 1))))
			return nil;
		a = b;
	}
	return t;
}

DOC("<=", "Non-decreasing order.\n"
		  "  (<= 2 2) => t\n"
		  "See: < >=")
objectp
builtin_less_equal(const struct object *args)
{
	return compare_chain(args, "<=", CMP_LT | CMP_EQ);
}

DOC(">", "Strictly decreasing order.\n"
		 "  (> 2 1) => t\n"
		 "See: >= <")
objectp
builtin_greater(const struct object *args)
{
	return compare_chain(args, ">", CMP_GT);
}

DOC(">=", "Non-increasing order.\n"
		  "  (>= 2 2) => t\n"
		  "See: > <=")
objectp
builtin_greater_equal(const struct object *args)
{
	return compare_chain(args, ">=", CMP_GT | CMP_EQ);
}

DOC("=", "Numeric equality, across types.\n"
		 "  (= 1 1.0) => t\n"
		 "  (= 1/2 0.5) => t\n"
		 "Errors: Signals for a non-numeric argument, unlike equal.\n"
		 "See: /= equal eq")
objectp
builtin_numeric_equal(const struct object *args)
{
	return compare_chain(args, "=", CMP_EQ);
}

static int
list_has_matrix(objectp values)
{
	for (; values != nil; values = cdr(values))
		if (values->value.cons.car->type == OBJ_MATRIX)
			return 1;
	return 0;
}

DOC("+", "Sum of the arguments. No arguments is 0.\n"
		 "  (+ 1 2) => 3\n"
		 "  (+) => 0\n"
		 "  (+ 1/2 1/2) => 1\n"
		 "  (+ 1 2.5) => 3.5\n"
		 "  (+ [1 2] [3 4]) => [[4 6]]\n"
		 "Note: Matrices add elementwise; a scalar broadcasts to every cell.\n"
		 "Errors: ADD: NOT NUMERIC for an unsupported argument.\n"
		 "See: - * /")
objectp
builtin_add(const struct object *args)
{
	long int i, d, n, g, denominator_gcd;
	objectp value, values;
	double f = 0.0;
	int isfloat = 0;
	i = n = 0L;
	d = 1L;
	if (args == nil)
	{
		value = make_integer(0L);
		return value;
	}
	values = eval_arguments(args);
	if (list_has_matrix(values))
		return matrix_fold(values, '+');
	do
	{
		value = car(values);
		if (isfloat)
		{
			require_numeric(value, "+");
			f += number_to_double(value);
		}
		else if (value->type == OBJ_INTEGER)
			i = checked_add(i, value->value.i);
		else if (value->type == OBJ_RATIONAL)
		{
			denominator_gcd = gcd(d, value->value.rational.d);
			n = checked_add(checked_mul(n, value->value.rational.d / denominator_gcd),
							checked_mul(d / denominator_gcd, value->value.rational.n));
			d = checked_mul(d, value->value.rational.d / denominator_gcd);
			denominator_gcd = gcd(n, d);
			if (denominator_gcd > 1L)
			{
				n /= denominator_gcd;
				d /= denominator_gcd;
			}
		}
		else if (value->type == OBJ_FLOAT)
		{
			f = (double)i + (double)n / (double)d + value->value.f;
			isfloat = 1;
		}
		else
			require_numeric(value, "+");
	} while ((values = cdr(values)) != nil);

	if (isfloat)
		return make_number_from_double(f);

	if (n == 0L)
	{
		value = make_integer(i);
		return value;
	}
	else
	{
		if (i != 0L)
			n = checked_add(n, checked_mul(d, i));
		g = gcd(n, d);
		value = make_rational(n / g, d / g);
	}
	return eval(value);
}

DOC("*", "Product of the arguments. No arguments is 1.\n"
		 "  (* 2 3) => 6\n"
		 "  (*) => 1\n"
		 "  (* 1/2 4) => 2\n"
		 "  (* [1 2] 3) => [[3 6]]\n"
		 "Note: Matrix times scalar scales every cell. Matrix times matrix is "
		 "matrix multiplication; use mat-mul for the Hadamard product.\n"
		 "See: + - / mat* mat-mul")
objectp
builtin_multiply(const struct object *args)
{
	long int i, d, n, g, numerator_gcd, denominator_gcd;
	objectp value, values;
	double f = 1.0;
	int isfloat = 0;

	i = d = n = 1L;
	if (args == nil)
	{
		value = make_integer(1L);
		return value;
	}
	values = eval_arguments(args);
	if (list_has_matrix(values))
		return matrix_fold(values, '*');
	do
	{
		value = car(values);
		if (isfloat)
		{
			require_numeric(value, "*");
			f *= number_to_double(value);
		}
		else if (value->type == OBJ_INTEGER)
			i = checked_mul(i, value->value.i);
		else if (value->type == OBJ_RATIONAL)
		{
			numerator_gcd = gcd(n, value->value.rational.d);
			denominator_gcd = gcd(value->value.rational.n, d);

			n = checked_mul(n / numerator_gcd,
							value->value.rational.n / denominator_gcd);
			d = checked_mul(d / denominator_gcd,
							value->value.rational.d / numerator_gcd);
		}
		else if (value->type == OBJ_FLOAT)
		{
			f = (double)i * (double)n / (double)d * value->value.f;
			isfloat = 1;
		}
		else
			require_numeric(value, "*");
	} while ((values = cdr(values)) != nil);

	if (isfloat)
		return make_number_from_double(f);

	if (d == 1L)
	{
		return make_integer(checked_mul(i, n));
	}
	else
	{
		n = checked_mul(n, i);
		g = gcd(n, d);
		value = make_rational(n / g, d / g);
	}
	return eval(value);
}

static objectp
div_two(long int nu, long int du, long int nv, long int dv)
{
	long int u, v, g;
	objectp rat;
	long int g1 = gcd(nu, nv), g2 = gcd(dv, du);
	u = checked_mul(nu / g1, dv / g2);
	v = checked_mul(du / g2, nv / g1);
	g = gcd(u, v);
	u = u / g;
	v = v / g;
	/* Put the sign on the numerator BEFORE testing for a unit denominator.
	   Testing first only worked while gcd returned a signed result that
	   happened to carry the minus across for us: (/ 6 -3) reduced to u=2,
	   v=-1, missed the v==1 demotion and answered -2/1 instead of -2. This
	   is the one caller that normalises its own result rather than handing
	   it to eval_rational. */
	if (v < 0L)
	{
		u = checked_sub(0L, u);
		v = checked_sub(0L, v);
	}
	if (v == 1L)
	{
		rat = make_integer(u);
		return rat;
	}
	rat = make_rational(u, v);
	return rat;
}

DOC("/", "Quotient, kept exact: integers divide into rationals.\n"
		 "  (/ 6 3) => 2\n"
		 "  (/ 1 3) => 1/3\n"
		 "  (/ 1.0 4) => 0.25\n"
		 "Errors: DIVIDE: DIVISION BY ZERO.\n"
		 "See: * mod rationalize")
objectp
builtin_divide(const struct object *args)
{
	long int g, rn, rd;
	objectp value, values;
	double f = 0.0, divisor;
	int isfloat = 0;

	values = eval_arguments(args);
	if (list_has_matrix(values))
		return matrix_fold(values, '/');
	value = car(values);
	require_numeric(value, "/");

	if (value->type == OBJ_INTEGER)
	{
		rn = value->value.i;
		rd = 1L;
	}
	else if (value->type == OBJ_RATIONAL)
	{
		rn = value->value.rational.n;
		rd = value->value.rational.d;
	}
	else
	{
		rn = 0L;
		rd = 1L;
		f = value->value.f;
		isfloat = 1;
	}

	if (cdr(values) == nil)
	{
		if (isfloat)
		{
			require_condition((f != 0.0), "/", "DIVISION BY ZERO", value);
			return make_number_from_double(1.0 / f);
		}
		require_condition((rn != 0), "/", "DIVISION BY ZERO", value);
		return div_two(1L, 1L, rn, rd);
	}

	values = cdr(values);
	do
	{
		value = car(values);
		require_numeric(value, "/");
		if (isfloat || value->type == OBJ_FLOAT)
		{
			divisor = number_to_double(value);
			if (!isfloat)
			{
				f = (double)rn / (double)rd;
				isfloat = 1;
			}
			require_condition((divisor != 0.0), "/", "DIVISION BY ZERO", value);
			f /= divisor;
			continue;
		}
		if (value->type == OBJ_INTEGER)
		{
			require_condition((value->value.i != 0), "/", "DIVISION BY ZERO", value);
			g = value->value.i;
			value = make_rational(g, 1L);
		}
		require_condition((value->value.rational.n != 0), "/", "DIVISION BY ZERO",
						  value);
		require_condition((value->value.rational.d != 0), "/", "ZERO DENOMINATOR",
						  value);
		require_condition((rd != 0), "/", "ZERO DENOMINATOR", value);
		value = div_two(rn, rd, value->value.rational.n, value->value.rational.d);
		if (value->type == OBJ_INTEGER)
		{
			rn = value->value.i;
			rd = 1L;
		}
		else
		{
			rn = value->value.rational.n;
			rd = value->value.rational.d;
		}
	} while ((values = cdr(values)) != nil);

	if (isfloat)
		return make_number_from_double(f);

	return value;
}
static int
mul_overflows(long int a, long int b)
{
	if (a == 0 || b == 0)
		return 0;
	if (a > 0)
		return b > 0 ? a > LONG_MAX / b : b < LONG_MIN / a;
	else
		return b > 0 ? a < LONG_MIN / b : a < LONG_MAX / b;
}

static long int
ipow_checked(long int base, long int exp, int *overflow)
{
	long int result = 1;
	if (base == 0L)
		return exp == 0L ? 1L : 0L;
	if (base == 1L)
		return 1L;
	if (base == -1L)
		return (exp & 1L) ? -1L : 1L;
	while (exp > 0)
	{
		if ((exp & 1L) != 0)
		{
			if (mul_overflows(result, base))
			{
				*overflow = 1;
				return 0;
			}
			result *= base;
		}
		exp >>= 1;
		if (exp == 0)
			break;
		if (mul_overflows(base, base))
		{
			*overflow = 1;
			return 0;
		}
		base *= base;
	}
	return result;
}

DOC("^", "Exponentiation with an integer exponent.\n"
		 "  (^ 2 10) => 1024\n"
		 "  (^ 2 -1) => 1/2\n"
		 "Errors: ^: NOT INTEGER if the exponent is not an integer, so roots "
		 "must go through sqrt or cbrt.\n"
		 "See: sqrt cbrt exp")
objectp
builtin_power(const struct object *args)
{
	objectp arg1, arg2, r;
	long int exp, n, d;
	long int tmp;
	int ovf = 0;

	arg1 = eval(car(args));
	arg2 = eval(cadr(args));
	require_numeric(arg1, "^");
	require_type(arg2, OBJ_INTEGER, "^");
	exp = arg2->value.i;

	if (arg1->type == OBJ_FLOAT)
		return make_number_from_double(pow(arg1->value.f, (double)exp));

	if (arg1->type == OBJ_INTEGER)
	{
		n = arg1->value.i;
		d = 1L;
	}
	else
	{
		n = arg1->value.rational.n;
		d = arg1->value.rational.d;
	}

	if (exp == 0)
	{
		r = make_integer(1);
		return r;
	}

	if (exp < 0)
	{
		require_condition((exp > LONG_MIN), "^", "EXPONENT OVERFLOW", arg2);
		require_condition((n != 0), "^", "DIVISION BY ZERO", arg1);
		exp = -exp;
		tmp = n;
		n = d;
		d = tmp;
	}

	n = ipow_checked(n, exp, &ovf);
	require_condition((!ovf), "^", "EXPONENT OVERFLOW", arg1);
	d = ipow_checked(d, exp, &ovf);
	require_condition((!ovf), "^", "EXPONENT OVERFLOW", arg1);

	if (d == 1L)
	{
		r = make_integer(n);
		return r;
	}

	r = make_rational(n, d);
	return eval_rational(r);
}

DOC("<", "Strictly increasing order.\n"
		 "  (< 1 2) => t\n"
		 "See: <= > =")
objectp
builtin_less(const struct object *args)
{
	return compare_chain(args, "<", CMP_LT);
}
DOC("and", "True when every argument is true. Arguments are booleans.\n"
		   "  (and t t) => t\n"
		   "  (and t nil) => nil\n"
		   "See: or not xor")
objectp
builtin_and(const struct object *args)
{
	objectp value;
	if (args == nil)
		return t;
	do
	{
		value = eval(car(args));
		require_boolean(value, "AND");
		if (value == nil)
			return nil;
	} while ((args = cdr(args)) != nil);
	return value;
}

DOC("or", "True when any argument is true.\n"
		  "  (or nil t) => t\n"
		  "See: and not")
objectp
builtin_or(const struct object *args)
{
	objectp value;

	/* Loop on the arguments rather than on a trailing cdr test: (or) has no
	   first argument, and the do-while form evaluated car(nil) as though it
	   were one. */
	for (; args != nil; args = cdr(args))
	{
		value = eval(car(args));
		require_boolean(value, "OR");
		if (value != nil)
			return value;
	}
	return nil;
}

DOC("not", "Boolean negation.\n"
		   "  (not nil) => t\n"
		   "Errors: NOT: NOT BOOLEAN for a non-boolean. Use null to "
		   "test for the empty list.\n"
		   "See: null and or")
objectp
builtin_not(const struct object *args)
{
	objectp value;
	value = eval(car(args));
	require_boolean(value, "NOT");
	return value == t ? nil : t;
}

DOC("xor", "True when an odd number of arguments are true.\n"
		   "  (xor t nil) => t\n"
		   "  (xor t t) => nil\n"
		   "See: or and")
objectp
builtin_xor(const struct object *args)
{
	objectp r, value;
	r = eval(car(args));
	require_boolean(r, "XOR");

	/* (xor x) has nothing after the first argument; the do-while form
	   evaluated car(nil) as a second operand and got the right answer only
	   because (nil != r) happens to be the identity here. */
	for (args = cdr(args); args != nil; args = cdr(args))
	{
		value = eval(car(args));
		require_boolean(value, "XOR");
		r = (value != r) ? t : nil;
	}
	return r;
}

DOC("=>", "Logical implication: false only when the first is true and the "
		  "second false.\n"
		  "  (=> nil t) => t\n"
		  "  (=> t nil) => nil\n"
		  "See: <=> and or")
objectp
builtin_imply(const struct object *args)
{
	objectp a, b;
	a = eval(car(args));
	b = eval(car(cdr(args)));
	require_boolean(a, "=>");
	require_boolean(b, "=>");

	if (a == t && b == nil)
		return nil;
	return t;
}

DOC("<=>", "Logical equivalence: true when both sides agree.\n"
		   "  (<=> t t) => t\n"
		   "  (<=> t nil) => nil\n"
		   "Note: This is iff, not a three-way comparison.\n"
		   "See: => and")
objectp
builtin_iff(const struct object *args)
{
	objectp r, value;
	r = eval(car(args));
	require_boolean(r, "<=>");

	args = cdr(args);
	do
	{
		value = eval(car(args));
		require_boolean(value, "<=>");
		r = (value != r) ? nil : t;
	} while ((args = cdr(args)) != nil);
	return r;
}

DOC("-", "Difference. With one argument, negation.\n"
		 "  (- 5 2) => 3\n"
		 "  (- 3) => -3\n"
		 "  (- 1/2) => -1/2\n"
		 "  (- [3 4] [1 2]) => [[2 2]]\n"
		 "Note: Matrices subtract elementwise; a scalar broadcasts.\n"
		 "See: + * /")
objectp
builtin_subtract(const struct object *args)
{
	long int i, d, n, g, denominator_gcd;
	objectp value, values;
	double f = 0.0;
	int isfloat = 0;

	values = eval_arguments(args);
	if (list_has_matrix(values))
		return matrix_fold(values, '-');
	value = car(values);
	require_numeric(value, "-");

	if (value->type == OBJ_INTEGER)
	{
		i = value->value.i;
		n = 0L;
		d = 1L;
	}
	else if (value->type == OBJ_RATIONAL)
	{
		i = 0L;
		n = value->value.rational.n;
		d = value->value.rational.d;
	}
	else
	{
		i = 0L;
		n = 0L;
		d = 1L;
		f = value->value.f;
		isfloat = 1;
	}

	if (cdr(values) == nil)
	{
		if (isfloat)
			return make_number_from_double(-f);
		if (n == 0L)
		{
			value = make_integer(checked_sub(0L, i));
			return value;
		}
		value = make_rational(checked_sub(0L, n), d);
		return value;
	}

	values = cdr(values);
	do
	{
		value = car(values);
		if (isfloat)
		{
			require_numeric(value, "-");
			f -= number_to_double(value);
		}
		else if (value->type == OBJ_INTEGER)
			i = checked_sub(i, value->value.i);
		else if (value->type == OBJ_RATIONAL)
		{
			denominator_gcd = gcd(d, value->value.rational.d);
			n = checked_sub(checked_mul(n, value->value.rational.d / denominator_gcd),
							checked_mul(d / denominator_gcd, value->value.rational.n));
			d = checked_mul(d, value->value.rational.d / denominator_gcd);
			denominator_gcd = gcd(n, d);
			if (denominator_gcd > 1L)
			{
				n /= denominator_gcd;
				d /= denominator_gcd;
			}
		}
		else if (value->type == OBJ_FLOAT)
		{
			f = (double)i + (double)n / (double)d - value->value.f;
			isfloat = 1;
		}
		else
			require_numeric(value, "-");
	} while ((values = cdr(values)) != nil);

	if (isfloat)
		return make_number_from_double(f);

	if (n == 0L)
	{
		value = make_integer(i);
		return value;
	}
	if (i != 0L)
		n = checked_add(n, checked_mul(d, i));
	g = gcd(n, d);
	value = make_rational(n / g, d / g);
	return eval(value);
}

DOC("/=", "Numeric inequality.\n"
		  "  (/= 1 2) => t\n"
		  "See: =")
objectp
builtin_numeric_not_equal(const struct object *args)
{
	const struct object *outer, *inner;
	objectp a, b;
	objectp values = eval_arguments(args);

	for (outer = values; outer != nil; outer = cdr(outer))
	{
		a = car(outer);
		require_numeric(a, "/=");
		for (inner = cdr(outer); inner != nil; inner = cdr(inner))
		{
			b = car(inner);
			require_numeric(b, "/=");
			/* a NaN operand is distinct from everything, so it never makes /=
			 * fail */
			if (!number_is_nan(a) && !number_is_nan(b) && number_compare(a, b) == 0)
				return nil;
		}
	}
	return t;
}

/* Generated by the MATH macros above; entries kept beside them. */
DOC("acos", "Arccosine, in radians.\n"
			"  (acos 1) => 0\n"
			"See: cos asin")

DOC("acosh", "Inverse hyperbolic cosine.\n"
			 "  (acosh 1) => 0\n"
			 "See: cosh")

DOC("asin", "Arcsine, in radians.\n"
			"  (asin 0) => 0\n"
			"See: sin acos")

DOC("asinh", "Inverse hyperbolic sine.\n"
			 "  (asinh 0) => 0\n"
			 "See: sinh")

DOC("atan", "Arctangent, in radians.\n"
			"  (atan 0) => 0\n"
			"See: tan atan2")

DOC("atan2", "Arctangent of y/x, using both signs to pick the quadrant.\n"
			 "  (atan2 0 1) => 0\n"
			 "See: atan")

DOC("atanh", "Inverse hyperbolic tangent.\n"
			 "  (atanh 0) => 0\n"
			 "See: tanh")

DOC("cbrt", "Cube root.\n"
			"  (cbrt 27) => 3\n"
			"See: sqrt ^")

DOC("cos", "Cosine, in radians.\n"
		   "  (cos 0) => 1\n"
		   "See: sin tan acos")

DOC("cosh", "Hyperbolic cosine.\n"
			"  (cosh 0) => 1\n"
			"See: sinh acosh")

DOC("exp", "e raised to the argument, elementwise for a matrix.\n"
		   "  (exp 0) => 1\n"
		   "See: log ^")

DOC("hypot", "Length of the hypotenuse, without overflowing on the squares.\n"
			 "  (hypot 3 4) => 5\n"
			 "See: sqrt")

DOC("log", "Natural logarithm.\n"
		   "  (log 1) => 0\n"
		   "See: log2 log10 exp")

DOC("log10", "Base-10 logarithm.\n"
			 "  (log10 100) => 2\n"
			 "See: log log2")

DOC("log2", "Base-2 logarithm.\n"
			"  (log2 8) => 3\n"
			"See: log log10")

DOC("relu", "The argument if positive, otherwise zero; elementwise for a matrix.\n"
			"  (relu -1) => 0\n"
			"  (relu 2) => 2\n"
			"See: max")

DOC("sin", "Sine, in radians.\n"
		   "  (sin 0) => 0\n"
		   "See: cos tan asin")

DOC("sinh", "Hyperbolic sine.\n"
			"  (sinh 0) => 0\n"
			"See: cosh asinh")

DOC("sqrt", "Square root, elementwise for a matrix.\n"
			"  (sqrt 16) => 4\n"
			"See: cbrt ^ hypot")

DOC("tan", "Tangent, in radians.\n"
		   "  (tan 0) => 0\n"
		   "See: sin cos atan")

DOC("tanh", "Hyperbolic tangent, elementwise for a matrix.\n"
			"  (tanh 0) => 0\n"
			"See: sinh cosh")
