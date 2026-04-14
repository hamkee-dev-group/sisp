#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <math.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>

#include <limits.h>

#include "sisp.h"
#include "extern.h"
#include "funcs.h"
#include "eval.h"
#include "misc.h"

static int safe_mul_cmp(long int a, long int b, long int c, long int d)
{
    /* Compare a*b vs c*d without overflow using double for magnitude check
       then falling back to exact cross-multiply when safe */
    double lhs = (double)a * (double)b;
    double rhs = (double)c * (double)d;
    if (lhs < rhs - 1.0) return -1;
    if (lhs > rhs + 1.0) return 1;
    /* close values: use 128-bit if available, else long long */
#ifdef __SIZEOF_INT128__
    __int128 la = (__int128)a * b;
    __int128 ra = (__int128)c * d;
    return (la < ra) ? -1 : (la > ra) ? 1 : 0;
#else
    long long la = (long long)a * b;
    long long ra = (long long)c * d;
    return (la < ra) ? -1 : (la > ra) ? 1 : 0;
#endif
}

int cmp_numeric(objectp arg1, objectp arg2)
{
    if (arg1->type == OBJ_INTEGER && arg2->type == OBJ_INTEGER)
        return (arg1->value.i < arg2->value.i) ? -1
             : (arg1->value.i > arg2->value.i) ? 1 : 0;
    if (arg1->type == OBJ_INTEGER && arg2->type == OBJ_RATIONAL)
        return safe_mul_cmp(arg1->value.i, arg2->value.r.d, arg2->value.r.n, 1L);
    if (arg1->type == OBJ_RATIONAL && arg2->type == OBJ_INTEGER)
        return safe_mul_cmp(arg1->value.r.n, 1L, arg1->value.r.d, arg2->value.i);
    /* both rational */
    return safe_mul_cmp(arg1->value.r.n, arg2->value.r.d, arg2->value.r.n, arg1->value.r.d);
}

objectp
F_mod(const struct object *args)
{
    objectp d, n, r;

    n = eval(car(args));
    d = eval(car(cdr(args)));

    _ASSERTP(n->type == OBJ_INTEGER, NOT INTEGER, MOD, n);
    _ASSERTP(d->type == OBJ_INTEGER, NOT INTEGER, MOD, d);
    _ASSERTP(d->value.i >= 1, MODULUS < 1, MOD, d);

    r = new_object(OBJ_INTEGER);
    r->value.i = n->value.i % d->value.i;
    if(r->value.i < 0)
        r->value.i += d->value.i;
    return r;
}

objectp
F_lesseq(const struct object *args)
{
    objectp arg1, arg2;
    arg1 = eval(car(args));
    arg2 = eval(cadr(args));
    _ASSERTP(ISNUMERIC(arg1), NOT NUMERIC, <=, arg1);
    _ASSERTP(ISNUMERIC(arg2), NOT NUMERIC, <=, arg2);
    return (cmp_numeric(arg1, arg2) <= 0) ? t : nil;
}

objectp
F_great(const struct object *args)
{
    objectp arg1, arg2;
    arg1 = eval(car(args));
    arg2 = eval(cadr(args));
    _ASSERTP(ISNUMERIC(arg1), NOT NUMERIC, >, arg1);
    _ASSERTP(ISNUMERIC(arg2), NOT NUMERIC, >, arg2);
    return (cmp_numeric(arg1, arg2) > 0) ? t : nil;
}

objectp
F_greateq(const struct object *args)
{
    objectp arg1, arg2;
    arg1 = eval(car(args));
    arg2 = eval(cadr(args));
    _ASSERTP(ISNUMERIC(arg1), NOT NUMERIC, >=, arg1);
    _ASSERTP(ISNUMERIC(arg2), NOT NUMERIC, >=, arg2);
    return (cmp_numeric(arg1, arg2) >= 0) ? t : nil;
}

objectp
F_numeq(const struct object *args)
{
    objectp arg1, arg2;
    arg1 = eval(car(args));
    arg2 = eval(cadr(args));
    _ASSERTP(ISNUMERIC(arg1), NOT NUMERIC, =, arg1);
    _ASSERTP(ISNUMERIC(arg2), NOT NUMERIC, =, arg2);
    return (cmp_numeric(arg1, arg2) == 0) ? t : nil;
}

objectp
F_add(const struct object *args)
{
    long int i, d, n, g;
    objectp p;
    i = n = 0L;
    d = 1L;
    if (args == nil)
    {
        p = new_object(OBJ_INTEGER);
        p->value.i = 0L;
        return p;
    }
    do
    {
        p = eval(car(args));
        if (p->type == OBJ_INTEGER)
            i += p->value.i;
        else if (p->type == OBJ_RATIONAL)
        {
            n = (n * p->value.r.d) + (d * p->value.r.n);
            d *= p->value.r.d;
        }
        else
            _ASSERTP(false, NOT NUMERIC, ADD, p);
    } while ((args = cdr(args)) != nil);

    if (n == 0L)
    {
        p = new_object(OBJ_INTEGER);
        p->value.i = i;
        return p;
    }
    else
    {
        p = new_object(OBJ_RATIONAL);
        if (i != 0L)
            n += d * i;
        g = gcd(n, d);
        p->value.r.n = n / g;
        p->value.r.d = d / g;
    }
    return eval(p);
}

objectp
F_prod(const struct object *args)
{
    long int i, d, n, g;
    objectp p;

    i = d = n = 1L;
    if (args == nil)
    {
        p = new_object(OBJ_INTEGER);
        p->value.i = 1L;
        return p;
    }
    do
    {
        p = eval(car(args));
        if (p->type == OBJ_INTEGER)
            i *= p->value.i;
        else if (p->type == OBJ_RATIONAL)
        {
            d *= p->value.r.d;
            n *= p->value.r.n;
        }
        else
            _ASSERTP(false, NOT NUMERIC ARGUMENT, PROD, p);
    } while ((args = cdr(args)) != nil);

    if (d == 1L)
    {
        p = new_object(OBJ_INTEGER);
        p->value.i = i;
        return p;
    }
    else
    {
        p = new_object(OBJ_RATIONAL);
        n = n * i;
        g = gcd(n, d);
        p->value.r.n = n / g;
        p->value.r.d = d / g;
    }
    return eval(p);
}

static objectp
div_two(long int nu, long int du, long int nv, long int dv)
{
    long int u, v, g;
    objectp rat;
    u = nu * dv;
    v = du * nv;
    g = gcd(u, v);
    u = u / g;
    v = v / g;
    if (v == 1L)
    {
        rat = new_object(OBJ_INTEGER);
        rat->value.i = u;
        return rat;
    }
    rat = new_object(OBJ_RATIONAL);
    if (v < 0L)
    {
        rat->value.r.n = -u;
        rat->value.r.d = -v;
    }
    else
    {
        rat->value.r.n = u;
        rat->value.r.d = v;
    }
    return rat;
}

objectp
F_div(const struct object *args)
{
    long int g, rn, rd;
    objectp p;

    p = eval(car(args));
    _ASSERTP(ISNUMERIC(p), NOT NUMERIC, DIV, p);

    if (p->type == OBJ_INTEGER)
    { rn = p->value.i; rd = 1L; }
    else
    { rn = p->value.r.n; rd = p->value.r.d; }

    if (cdr(args) == nil)
    {
        _ASSERTP(rn != 0, DIVISION BY ZERO, DIV, p);
        return div_two(1L, 1L, rn, rd);
    }

    args = cdr(args);
    do
    {
        p = eval(car(args));
        _ASSERTP(ISNUMERIC(p), NOT NUMERIC, DIV, p);
        if (p->type == OBJ_INTEGER)
        {
            _ASSERTP(p->value.i != 0, DIVISION BY ZERO, DIV, p);
            g = p->value.i;
            p = new_object(OBJ_RATIONAL);
            p->value.r.n = g;
            p->value.r.d = 1L;
        }
        _ASSERTP(p->value.r.n != 0, DIVISION BY ZERO, DIV, p);
        _ASSERTP(p->value.r.d != 0, ZERO DENOMINATOR, DIV, p);
        _ASSERTP(rd != 0, ZERO DENOMINATOR, DIV, p);
        p = div_two(rn, rd, p->value.r.n, p->value.r.d);
        if (p->type == OBJ_INTEGER)
        { rn = p->value.i; rd = 1L; }
        else
        { rn = p->value.r.n; rd = p->value.r.d; }
    } while ((args = cdr(args)) != nil);

    return p;
}
static int
mul_overflows(long int a, long int b)
{
    if (a == 0 || b == 0) return 0;
    if (a > 0)
        return b > 0 ? a > LONG_MAX / b : b < LONG_MIN / a;
    else
        return b > 0 ? a < LONG_MIN / b : a < LONG_MAX / b;
}

static long int
ipow_checked(long int base, long int exp, int *overflow)
{
    long int result = 1;
    for (; exp > 0; exp--)
    {
        if (mul_overflows(result, base))
        {
            *overflow = 1;
            return 0;
        }
        result *= base;
    }
    return result;
}

objectp
F_pow(const struct object *args)
{
    objectp arg1, arg2, r;
    long int exp, n, d;
    int ovf = 0;

    arg1 = eval(car(args));
    arg2 = eval(cadr(args));
    _ASSERTP(ISNUMERIC(arg1), NOT NUMERIC, ^, arg1);
    _ASSERTP(arg2->type == OBJ_INTEGER, NOT INTEGER, ^, arg2);
    exp = arg2->value.i;

    if (arg1->type == OBJ_INTEGER)
    { n = arg1->value.i; d = 1L; }
    else
    { n = arg1->value.r.n; d = arg1->value.r.d; }

    if (exp == 0)
    {
        r = new_object(OBJ_INTEGER);
        r->value.i = 1;
        return r;
    }

    if (exp < 0)
    {
        _ASSERTP(exp > LONG_MIN, EXPONENT OVERFLOW, ^, arg2);
        _ASSERTP(n != 0, DIVISION BY ZERO, ^, arg1);
        exp = -exp;
        long int tmp = n;
        n = d;
        d = tmp;
    }

    n = ipow_checked(n, exp, &ovf);
    _ASSERTP(!ovf, EXPONENT OVERFLOW, ^, arg1);
    d = ipow_checked(d, exp, &ovf);
    _ASSERTP(!ovf, EXPONENT OVERFLOW, ^, arg1);

    if (d == 1L)
    {
        r = new_object(OBJ_INTEGER);
        r->value.i = n;
        return r;
    }

    r = new_object(OBJ_RATIONAL);
    r->value.r.n = n;
    r->value.r.d = d;
    return eval_rat(r);
}

objectp
F_less(const struct object *args)
{
    objectp arg1, arg2;
    arg1 = eval(car(args));
    arg2 = eval(cadr(args));
    _ASSERTP(ISNUMERIC(arg1), NOT NUMERIC, <, arg1);
    _ASSERTP(ISNUMERIC(arg2), NOT NUMERIC, <, arg2);
    return (cmp_numeric(arg1, arg2) < 0) ? t : nil;
}
objectp
F_and(const struct object *args)
{
    objectp p1;
    do
    {
        p1 = eval(car(args));
        _ASSERTP(ISBOOL(p1), NOT BOOL EXPRESSION, AND, p1);
        if (p1 == nil)
            return nil;
    } while ((args = cdr(args)) != nil);
    return p1;
}

objectp
F_or(const struct object *args)
{
    objectp p1;
    do
    {
        p1 = eval(car(args));
        _ASSERTP(ISBOOL(p1), NOT BOOL EXPRESSION, OR, p1);

        if (p1 != nil)
            return p1;
    } while ((args = cdr(args)) != nil);
    return nil;
}

objectp
F_not(const struct object *args)
{
    objectp p1;
    p1 = eval(car(args));
    _ASSERTP(ISBOOL(p1), NOT BOOL EXPRESSION, NOT, p1);
    return p1 == t ? nil : t;
}

objectp
F_xor(const struct object *args)
{
    objectp r, p1;
    r = eval(car(args));
    _ASSERTP(ISBOOL(r), NOT BOOL EXPRESSION, XOR, r);

    args = cdr(args);
    do
    {
        p1 = eval(car(args));
        _ASSERTP(ISBOOL(p1), NOT BOOL EXPRESSION, XOR, p1);
        r = (p1 != r) ? t : nil;
    } while ((args = cdr(args)) != nil);
    return r;
}

objectp
F_imply(const struct object *args)
{
    objectp a, b;
    a = eval(car(args));
    b = eval(car(cdr(args)));
    _ASSERTP(ISBOOL(a), NOT BOOL EXPRESSION, = >, a);
    _ASSERTP(ISBOOL(b), NOT BOOL EXPRESSION, = >, b);

    if (a == t && b == nil)
        return nil;
    return t;
}

objectp
F_iff(const struct object *args)
{
    objectp r, p1;
    r = eval(car(args));
    _ASSERTP(ISBOOL(r), NOT BOOL EXPRESSION, IFF, r);

    args = cdr(args);
    do
    {
        p1 = eval(car(args));
        _ASSERTP(ISBOOL(p1), NOT BOOL EXPRESSION, IFF, p1);
        r = (p1 != r) ? nil : t;
    } while ((args = cdr(args)) != nil);
    return r;
}

objectp
F_sub(const struct object *args)
{
    long int i, d, n, g;
    objectp p;

    p = eval(car(args));
    _ASSERTP(ISNUMERIC(p), NOT NUMERIC, SUB, p);

    if (p->type == OBJ_INTEGER)
    { i = p->value.i; n = 0L; d = 1L; }
    else
    { i = 0L; n = p->value.r.n; d = p->value.r.d; }

    if (cdr(args) == nil)
    {
        if (n == 0L)
        {
            p = new_object(OBJ_INTEGER);
            p->value.i = -i;
            return p;
        }
        p = new_object(OBJ_RATIONAL);
        p->value.r.n = -n;
        p->value.r.d = d;
        return p;
    }

    args = cdr(args);
    do
    {
        p = eval(car(args));
        if (p->type == OBJ_INTEGER)
            i -= p->value.i;
        else if (p->type == OBJ_RATIONAL)
        {
            n = (n * p->value.r.d) - (d * p->value.r.n);
            d *= p->value.r.d;
        }
        else
            _ASSERTP(false, NOT NUMERIC, SUB, p);
    } while ((args = cdr(args)) != nil);

    if (n == 0L)
    {
        p = new_object(OBJ_INTEGER);
        p->value.i = i;
        return p;
    }
    p = new_object(OBJ_RATIONAL);
    if (i != 0L)
        n += d * i;
    g = gcd(n, d);
    p->value.r.n = n / g;
    p->value.r.d = d / g;
    return eval(p);
}

objectp
F_numneq(const struct object *args)
{
    const struct object *outer, *inner;
    objectp a, b;

    for (outer = args; outer != nil; outer = cdr(outer))
    {
        a = eval(car(outer));
        _ASSERTP(ISNUMERIC(a), NOT NUMERIC, /=, a);
        for (inner = cdr(outer); inner != nil; inner = cdr(inner))
        {
            b = eval(car(inner));
            _ASSERTP(ISNUMERIC(b), NOT NUMERIC, /=, b);
            if (cmp_numeric(a, b) == 0)
                return nil;
        }
    }
    return t;
}
