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

objectp
F_div(const struct object *args)
{
    long int g, u, v;
    objectp d, n, rat;

    n = eval(car(args));
    d = eval(car(cdr(args)));
    _ASSERTP(ISNUMERIC(n), NOT NUMERIC, DIV, n);
    _ASSERTP(ISNUMERIC(d), NOT NUMERIC, DIV, d);

    if (n->type == OBJ_INTEGER)
    {
        g = n->value.i;
        n = new_object(OBJ_RATIONAL);
        n->value.r.n = g;
        n->value.r.d = 1L;
    }
    if (d->type == OBJ_INTEGER)
    {
        _ASSERTP(d->value.i != 0, DIVISION BY ZERO, DIV, d);
        g = d->value.i;
        d = new_object(OBJ_RATIONAL);
        d->value.r.n = g;
        d->value.r.d = 1L;
    }
    _ASSERTP(d->value.r.d != 0, ZERO DENOMINATOR, DIV, d);
    _ASSERTP(n->value.r.d != 0, ZERO DENOMINATOR, DIV, n);
    u = n->value.r.n * d->value.r.d;
    v = n->value.r.d * d->value.r.n;
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
F_pow(const struct object *args)
{
    objectp arg1, arg2, r;
    long int exp;
    arg1 = eval(car(args));
    arg2 = eval(cadr(args));
    _ASSERTP(ISNUMERIC(arg1), NOT NUMERIC, ^, arg1);
    _ASSERTP(arg2->type == OBJ_INTEGER, NOT INTEGER, ^, arg2);
    exp = arg2->value.i;
    if(exp < 0) {
        _ASSERTP(exp > LONG_MIN, EXPONENT OVERFLOW, ^, arg2);
        exp = -exp;
        if (arg1->type == OBJ_INTEGER)
            _ASSERTP(arg1->value.i != 0, DIVISION BY ZERO, ^, arg1);
        else
            _ASSERTP(arg1->value.r.n != 0, DIVISION BY ZERO, ^, arg1);
        r = new_object(OBJ_RATIONAL);
        if (arg1->type == OBJ_INTEGER) {
            r->value.r.n = 1;
            r->value.r.d = (long int)pow((double)arg1->value.i, (double)exp);
        } else {
            r->value.r.n = (long int)pow((double)arg1->value.r.d, (double)exp);
            r->value.r.d = (long int)pow((double)arg1->value.r.n, (double)exp);
        }
        return r;
    }
    if (arg1->type == OBJ_INTEGER)
    {
        r = new_object(OBJ_INTEGER);
        r->value.i = (long int)pow((double)arg1->value.i, (double)exp);
    }
    else
    {
        r = new_object(OBJ_RATIONAL);
        r->value.r.n = (long int)pow((double)arg1->value.r.n, (double)exp);
        r->value.r.d = (long int)pow((double)arg1->value.r.d, (double)exp);
    }
    return r;
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
