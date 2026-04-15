#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <math.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>

#include "sisp.h"
#include "extern.h"
#include "funcs.h"
#include "eval.h"
#include "misc.h"

/* Convert an extension set {1 2 3} to comprehension {tau : (in tau {1 2 3})} */
__inline__ static objectp
ext_to_comp(objectp p)
{
	objectp ret;
	ret = new_object(OBJ_SET);
	ret->value.c.car = tau;
	ret->value.c.cdr = new_object(OBJ_CONS);
	ret->vcdr->vcar = new_object(OBJ_IDENTIFIER);
	ret->vcdr->vcar->value.id = strdup("in");
	ret->vcdr->vcdr = new_object(OBJ_CONS);
	ret->vcdr->vcdr->vcar = tau;
	ret->vcdr->vcdr->vcdr = new_object(OBJ_CONS);
	ret->vcdr->vcdr->vcdr->vcar = p;
	return ret;
}

__inline__ static objectp
unionbyextension(const struct object *args)
{
	objectp p, p1, first, prev, ret;
	first = prev = NULL;

	do
	{
		p = eval(car(args));
		if (p->type == OBJ_SET && !COMPSET(p))
			p = ext_to_comp(p);
		p1 = new_object(OBJ_CONS);
		p1->vcar = cdr(p);
		_ASSERTP(COMPSET(p), NOT COMPREHENSION SET, UNION, p);
		if (first == NULL)
			first = p1;
		if (prev != NULL)
			prev->vcdr = p1;
		prev = p1;
	} while ((args = cdr(args)) != nil);
	ret = new_object(OBJ_SET);
	ret->value.c.car = tau;
	ret->value.c.cdr = new_object(OBJ_CONS);
	ret->vcdr->vcar = new_object(OBJ_IDENTIFIER);
	ret->vcdr->vcar->value.id = strdup("or");
	ret->vcdr->vcdr = first;

	return ret;
}

__inline__ static objectp 
capbyextension(const struct object *args)
{
	objectp p, p1, first, prev, ret;
	first = prev = NULL;

	do
	{
		p = eval(car(args));
		if (p->type == OBJ_SET && !COMPSET(p))
			p = ext_to_comp(p);
		p1 = new_object(OBJ_CONS);
		p1->vcar = cdr(p);
		_ASSERTP(COMPSET(p), NOT COMPREHENSION SET, CAP, p);
		if (first == NULL)
			first = p1;
		if (prev != NULL)
			prev->vcdr = p1;
		prev = p1;
	} while ((args = cdr(args)) != nil);
	ret = new_object(OBJ_SET);
	ret->value.c.car = tau;
	ret->value.c.cdr = new_object(OBJ_CONS);
	ret->vcdr->vcar = new_object(OBJ_IDENTIFIER);
	ret->vcdr->vcar->value.id = strdup("and");
	ret->vcdr->vcdr = first;

	return ret;
}

__inline__ static objectp
diffbyextension(objectp p1, objectp p2)
{
	objectp ret;

	if (p1->type == OBJ_SET && !COMPSET(p1))
		p1 = ext_to_comp(p1);
	if (p2->type == OBJ_SET && !COMPSET(p2))
		p2 = ext_to_comp(p2);

	ret = new_object(OBJ_SET);
	ret->value.c.car = tau;
	ret->value.c.cdr = new_object(OBJ_CONS);
	ret->vcdr->vcar = new_object(OBJ_IDENTIFIER);
	ret->vcdr->vcar->value.id = strdup("and");
	ret->vcdr->vcdr = new_object(OBJ_CONS);
	ret->vcdr->vcdr->vcar = cdr(p1);
	ret->vcdr->vcdr->vcdr = new_object(OBJ_CONS);
	ret->vcdr->vcdr->vcdr->vcar = new_object(OBJ_CONS);
	ret->vcdr->vcdr->vcdr->vcar->vcar = new_object(OBJ_IDENTIFIER);
	ret->vcdr->vcdr->vcdr->vcar->vcar->value.id = strdup("not");
	ret->vcdr->vcdr->vcdr->vcar->vcdr = new_object(OBJ_CONS);
	ret->vcdr->vcdr->vcdr->vcar->vcdr->vcar = cdr(p2);

	return ret;
}

__inline__ static objectp symdiffbyextension(objectp p1, objectp p2)
{
	objectp ret;

	if (p1->type == OBJ_SET && !COMPSET(p1))
		p1 = ext_to_comp(p1);
	if (p2->type == OBJ_SET && !COMPSET(p2))
		p2 = ext_to_comp(p2);

	ret = new_object(OBJ_SET);
	ret->value.c.car = tau;
	ret->value.c.cdr = new_object(OBJ_CONS);
	ret->vcdr->vcar = new_object(OBJ_IDENTIFIER);
	ret->vcdr->vcar->value.id = strdup("xor");
	ret->vcdr->vcdr = new_object(OBJ_CONS);
	ret->vcdr->vcdr->vcar = cdr(p1);
	ret->vcdr->vcdr->vcdr = new_object(OBJ_CONS);
	ret->vcdr->vcdr->vcdr->vcar = cdr(p2);

	return ret;
}
objectp
F_symdiff(const struct object *args)
{
	objectp arg1, arg2, tmp, tmp1, c;
	objectp first = NULL, prev = NULL, p1;
	int found = 0;
	arg1 = eval(car(args));
	arg2 = eval(cadr(args));
	_ASSERTP((arg1->type == OBJ_CONS && arg2->type == OBJ_CONS) ||
				 (arg1->type == OBJ_SET && arg2->type == OBJ_SET),
			 NOT CONS, DIFF, args);
	if (COMPSET(arg1) || COMPSET(arg2))
		return symdiffbyextension(arg1, arg2);
	tmp1 = arg1;
	do
	{
		tmp = arg2;
		found = 0;
		do
		{
			if (arg1->vcar->type == tmp->vcar->type)
			{
				switch (arg1->vcar->type)
				{
				case OBJ_INTEGER:
					if (arg1->vcar->value.i == tmp->vcar->value.i)
					{
						found = 1;
					}
					break;
				case OBJ_RATIONAL:
					if (arg1->vcar->value.r.n == tmp->vcar->value.r.n &&
						arg1->vcar->value.r.d == tmp->vcar->value.r.d)
					{
						found = 1;
					}
					break;
				case OBJ_T:
				case OBJ_NIL:
				case OBJ_TAU:
					found = 1;
					break;
				case OBJ_IDENTIFIER:
					if (strcmp(arg1->vcar->value.id, tmp->vcar->value.id) == 0)
					{
						found = 1;
					}
					break;
				case OBJ_STRING:
					if (strcmp(arg1->vcar->value.s.str, tmp->vcar->value.s.str) == 0)
					{
						found = 1;
					}
					break;
				case OBJ_CONS:
				case OBJ_SET:
					c = (tmp->vcar->type == OBJ_SET) ? eqset(arg1->vcar, tmp->vcar) : eqcons(arg1->vcar, tmp->vcar);
					if (c == t)
					{
						found = 1;
					}
					break;
				default:
					found = 0;
					break;
				}
			}
		} while ((tmp = cdr(tmp)) != nil);
		if (found == 0)
		{
			p1 = new_object(arg1->type);
			p1->vcar = arg1->vcar;
			if (first == NULL)
				first = p1;
			if (prev != NULL)
				prev->vcdr = p1;
			prev = p1;
		}

	} while ((arg1 = cdr(arg1)) != nil);
	arg1 = tmp1;
	do
	{
		tmp = arg1;
		found = 0;
		do
		{
			if (arg2->vcar->type == tmp->vcar->type)
			{
				switch (arg2->vcar->type)
				{
				case OBJ_INTEGER:
					if (arg2->vcar->value.i == tmp->vcar->value.i)
					{
						found = 1;
					}
					break;
				case OBJ_RATIONAL:
					if (arg2->vcar->value.r.n == tmp->vcar->value.r.n &&
						arg2->vcar->value.r.d == tmp->vcar->value.r.d)
					{
						found = 1;
					}
					break;
				case OBJ_T:
				case OBJ_NIL:
				case OBJ_TAU:
					found = 1;
					break;
				case OBJ_IDENTIFIER:
					if (strcmp(arg2->vcar->value.id, tmp->vcar->value.id) == 0)
					{
						found = 1;
					}
					break;
				case OBJ_STRING:
					if (strcmp(arg2->vcar->value.s.str, tmp->vcar->value.s.str) == 0)
					{
						found = 1;
					}
					break;
				case OBJ_CONS:
				case OBJ_SET:
					c = (tmp->vcar->type == OBJ_SET) ? eqset(arg2->vcar, tmp->vcar) : eqcons(arg2->vcar, tmp->vcar);
					if (c == t)
					{
						found = 1;
					}
					break;
				default:
					found = 0;
					break;
				}
			}
		} while ((tmp = cdr(tmp)) != nil);
		if (found == 0)
		{
			p1 = new_object(arg2->type);
			p1->vcar = arg2->vcar;
			if (first == NULL)
				first = p1;
			if (prev != NULL)
				prev->vcdr = p1;
			prev = p1;
		}

	} while ((arg2 = cdr(arg2)) != nil);
	return first == NULL ? empty : first;
}
objectp
F_union(const struct object *args)
{
	objectp p, p1, first, prev;
	a_type type;
	first = prev = NULL;
	/* Check if any argument is a comprehension set */
	{
		const struct object *scan = args;
		do {
			p = eval(car(scan));
			if (COMPSET(p))
				return unionbyextension(args);
		} while ((scan = cdr(scan)) != nil);
	}
	p = eval(car(args));
	type = p->type;
	_ASSERTP(((p->type == OBJ_CONS && !DOTPAIRP(p)) || p->type == OBJ_SET), NOT CONS, APPEND, p);

	do
	{
		p1 = new_object(type);
		p1->vcar = car(p);
		if (first == NULL)
			first = p1;
		if (prev != NULL)
			prev->vcdr = p1;
		prev = p1;
		p = cdr(p);
	} while (p != nil);
	args = cdr(args);
	if (args == nil)
		return (type == OBJ_SET) ? eval_set(first) : first;
	do
	{
		p = eval(car(args));
		_ASSERTP(((p->type == OBJ_CONS && !DOTPAIRP(p)) || (p->type == OBJ_SET && !COMPSET(p))), NOT CONS, APPEND, p);
		_ASSERTP((p->type == type), TYPE MISMATCH, APPEND, p);

		do
		{
			p1 = new_object(type);
			p1->vcar = car(p);
			if (first == NULL)
				first = p1;
			if (prev != NULL)
				prev->vcdr = p1;
			prev = p1;
			p = cdr(p);
		} while (p != nil);
	} while ((args = cdr(args)) != nil);
	return (type == OBJ_SET) ? eval_set(first) : first;
}

objectp
F_subset(const struct object *args)
{
	objectp a, b, tmp, ret;
	a = eval(car(args));
	b = eval(car(cdr(args)));
	if (a == empty)
		return t;

	_ASSERTP(a->type == OBJ_SET && !COMPSET(a), NOT EXTENSION SET, SUBSET, a);
	_ASSERTP(b->type == OBJ_SET, NOT SET, SUBSET, b);
	if (COMPSET(b))
	{
		do
		{
			tmp = sst(tau, a->vcar, cdr(b));
			ret = eval(tmp);
			if (ret == nil)
				return ret;
		} while ((a = cdr(a)) != nil);
	}
	else
	{
		do
		{
			if (!in_set(a->vcar, b))
				return nil;
		} while ((a = cdr(a)) != nil);
	}
	return t;
}

objectp F_notin(const struct object *arg)
{
	return (F_member(arg) == nil) ? t : nil;
}

__inline__ static objectp
prodbyextension(objectp a, objectp b)
{
	objectp ret, cons, in, inb;
	if (unsafe_sanitize(a) == 0)
	{
		fprintf(stderr, "; PROD: FIRST OPERAND:\n; ");
		princ_object(stderr, a);
		fprintf(stderr, "\n; CONTAINS AN EXTENSION SET OF THE FORM: "
						" (and (in (car tau) {tau : ...}) (in (cdr tau) {tau : ...}))\n"
						"; THIS MAY LEAD TO LEFT ASSOCIATIVE LISTS.\n");
	}
	in = new_object(OBJ_CONS);
	in->vcar = new_object(OBJ_IDENTIFIER);
	in->vcar->value.id = strdup("in");
	in->vcdr = new_object(OBJ_CONS);
	in->vcdr->vcar = new_object(OBJ_CONS);
	in->vcdr->vcar->vcar = new_object(OBJ_IDENTIFIER);
	in->vcdr->vcar->vcar->value.id = strdup("car");
	in->vcdr->vcar->vcdr = new_object(OBJ_CONS);
	in->vcdr->vcar->vcdr->vcar = tau;
	in->vcdr->vcar->vcdr->vcdr = nil;
	in->vcdr->vcdr = new_object(OBJ_CONS);
	in->vcdr->vcdr->vcar = a;

	inb = new_object(OBJ_CONS);
	inb->vcar = new_object(OBJ_IDENTIFIER);
	inb->vcar->value.id = strdup("in");
	inb->vcdr = new_object(OBJ_CONS);
	inb->vcdr->vcar = new_object(OBJ_CONS);
	inb->vcdr->vcar->vcar = new_object(OBJ_IDENTIFIER);
	inb->vcdr->vcar->vcar->value.id = strdup("cdr");
	inb->vcdr->vcar->vcdr = new_object(OBJ_CONS);
	inb->vcdr->vcar->vcdr->vcar = tau;
	inb->vcdr->vcar->vcdr->vcdr = nil;
	inb->vcdr->vcdr = new_object(OBJ_CONS);
	inb->vcdr->vcdr->vcar = b;

	cons = new_object(OBJ_CONS);
	cons->vcar = new_object(OBJ_IDENTIFIER);
	cons->vcar->value.id = strdup("consp");
	cons->vcdr = new_object(OBJ_CONS);
	cons->vcdr->vcar = tau;

	ret = new_object(OBJ_SET);
	ret->value.c.car = tau;
	ret->value.c.cdr = new_object(OBJ_CONS);
	ret->vcdr->vcar = new_object(OBJ_IDENTIFIER);
	ret->vcdr->vcar->value.id = strdup("and");
	ret->vcdr->vcdr = new_object(OBJ_CONS);
	ret->vcdr->vcdr->vcar = in;
	ret->vcdr->vcdr->vcdr = new_object(OBJ_CONS);
	ret->vcdr->vcdr->vcdr->vcar = inb;

	return ret;
}
__inline__ static objectp
flatten(objectp p)
{
	objectp first = NULL, prev = NULL, p1, p2, p3;
	p = car(p);

	do
	{
		p1 = car(p);
		if (p1->type == OBJ_CONS)
		{
			do
			{
				p2 = car(p1);
				p3 = new_object(OBJ_CONS);
				p3->value.c.car = p2;
				if (first == NULL)
					first = p3;
				if (prev != NULL)
					prev->vcdr = p3;
				prev = p3;
			} while ((p1 = cdr(p1)) != nil);
		}
		else
		{
			p3 = new_object(OBJ_CONS);
			p3->value.c.car = p1;
			if (first == NULL)
				first = p3;
			if (prev != NULL)
				prev->vcdr = p3;
			prev = p3;
		}
	} while ((p = cdr(p)) != nil);

	return first;
}
objectp
F_powerset(const struct object *arg)
{
	objectp p, ret;
	p = eval(car(arg));
	_ASSERTP(p->type == OBJ_SET, NOT SET, POW, p);
	if (COMPSET(p))
	{
		ret = new_object(OBJ_SET);
		ret->vcar = tau;
		ret->vcdr = new_object(OBJ_CONS);
		ret->vcdr->vcar = new_object(OBJ_IDENTIFIER);
		ret->vcdr->vcar->value.id = strdup("subset");
		ret->vcdr->vcdr = new_object(OBJ_CONS);
		ret->vcdr->vcdr->vcar = tau;
		ret->vcdr->vcdr->vcdr = new_object(OBJ_CONS);
		ret->vcdr->vcdr->vcdr->vcar = p;
		return ret;
	}
	ret = set_to_array(p);
	return ret;
}
objectp
F_setprod(const struct object *args)
{
	objectp b, a, tmp;
	objectp first = NULL, prev = NULL, p1, p2;
	a = eval(car(args));
	b = eval(car(cdr(args)));

	_ASSERTP(a->type == OBJ_SET, NOT SET, PROD, a);
	_ASSERTP(b->type == OBJ_SET, NOT SET, PROD, b);
	if (COMPSET(a) && COMPSET(b))
		return prodbyextension(a, b);
	tmp = b;
	do
	{
		b = tmp;
		do
		{

			p1 = new_object(OBJ_SET);
			p1->vcar = new_object(OBJ_CONS);
			p1->vcar->vcar = car(a);
			p1->vcar->vcdr = new_object(OBJ_CONS);
			p1->vcar->vcdr->vcar = car(b);
			p2 = new_object(OBJ_SET);
			p2->vcar = flatten(p1);
			if (first == NULL)
				first = p2;
			if (prev != NULL)
				prev->vcdr = p2;
			prev = p2;

		} while ((b = cdr(b)) != nil);
	} while ((a = cdr(a)) != nil);

	return first;
}
objectp
F_cap(const struct object *args)
{
	objectp arg1, arg2, tmp, c;
	objectp first = NULL, prev = NULL, p1;
	a_type type;
	arg1 = eval(car(args));
	arg2 = eval(cadr(args));
	if (COMPSET(arg1) || COMPSET(arg2))
		return capbyextension(args);
	_ASSERTP((arg1->type == OBJ_CONS && arg2->type == OBJ_CONS) ||
				 (arg1->type == OBJ_SET && arg2->type == OBJ_SET),
			 NOT CONS, CAP, args);
	type = arg1->type;
	do
	{
		tmp = arg2;
		do
		{
			if (arg1->vcar->type == tmp->vcar->type)
			{
				switch (arg1->vcar->type)
				{
				case OBJ_INTEGER:
					if (arg1->vcar->value.i == tmp->vcar->value.i)
					{
						p1 = new_object(type);
						p1->vcar = new_object(OBJ_INTEGER);
						p1->vcar->value.i = arg1->vcar->value.i;
						if (first == NULL)
							first = p1;
						if (prev != NULL)
							prev->vcdr = p1;
						prev = p1;
					}
					break;
				case OBJ_RATIONAL:
					if (arg1->vcar->value.r.n == tmp->vcar->value.r.n &&
						arg1->vcar->value.r.d == tmp->vcar->value.r.d)
					{
						p1 = new_object(type);
						p1->vcar = new_object(OBJ_RATIONAL);
						p1->vcar->value.r.n = arg1->vcar->value.r.n;
						p1->vcar->value.r.d = arg1->vcar->value.r.d;
						if (first == NULL)
							first = p1;
						if (prev != NULL)
							prev->vcdr = p1;
						prev = p1;
					}
					break;
				case OBJ_T:
				case OBJ_NIL:
				case OBJ_TAU:
					p1 = new_object(type);
					p1->vcar = arg1->vcar;
					if (first == NULL)
						first = p1;
					if (prev != NULL)
						prev->vcdr = p1;
					prev = p1;
					break;
				case OBJ_IDENTIFIER:
					if (strcmp(arg1->vcar->value.id, tmp->vcar->value.id) == 0)
					{
						p1 = new_object(type);
						p1->vcar = new_object(OBJ_IDENTIFIER);
						p1->vcar->value.id = strdup(arg1->vcar->value.id);
						if (first == NULL)
							first = p1;
						if (prev != NULL)
							prev->vcdr = p1;
						prev = p1;
					}
					break;
				case OBJ_STRING:
					if (strcmp(arg1->vcar->value.s.str, tmp->vcar->value.s.str) == 0)
					{
						p1 = new_object(type);
						p1->vcar = new_object(OBJ_STRING);
						p1->vcar->value.s.str = strdup(arg1->vcar->value.s.str);
						if (first == NULL)
							first = p1;
						if (prev != NULL)
							prev->vcdr = p1;
						prev = p1;
					}
					break;
				case OBJ_CONS:
				case OBJ_SET:
					c = (tmp->vcar->type == OBJ_SET) ? eqset(arg1->vcar, tmp->vcar) : eqcons(arg1->vcar, tmp->vcar);
					if (c == t)
					{
						p1 = new_object(type);
						p1->vcar = arg1->vcar;
						if (first == NULL)
							first = p1;
						if (prev != NULL)
							prev->vcdr = p1;
						prev = p1;
					}
					break;

				default:
					break;
				}
			}
		} while ((tmp = cdr(tmp)) != nil);
	} while ((arg1 = cdr(arg1)) != nil);

	return first == NULL ? empty : first;
}

objectp
F_diff(const struct object *args)
{
	objectp arg1, arg2, tmp, c;
	objectp first = NULL, prev = NULL, p1;
	int found = 0;
	arg1 = eval(car(args));
	arg2 = eval(cadr(args));
	_ASSERTP((arg1->type == OBJ_CONS && arg2->type == OBJ_CONS) ||
				 (arg1->type == OBJ_SET && arg2->type == OBJ_SET),
			 NOT CONS, DIFF, args);
	if (COMPSET(arg1) || COMPSET(arg2))
		return diffbyextension(arg1, arg2);
	do
	{
		tmp = arg2;
		found = 0;
		do
		{
			if (arg1->vcar->type == tmp->vcar->type)
			{
				switch (arg1->vcar->type)
				{
				case OBJ_INTEGER:
					if (arg1->vcar->value.i == tmp->vcar->value.i)
					{
						found = 1;
					}
					break;
				case OBJ_RATIONAL:
					if (arg1->vcar->value.r.n == tmp->vcar->value.r.n &&
						arg1->vcar->value.r.d == tmp->vcar->value.r.d)
					{
						found = 1;
					}
					break;
				case OBJ_T:
				case OBJ_NIL:
				case OBJ_TAU:
					found = 1;
					break;
				case OBJ_IDENTIFIER:
					if (strcmp(arg1->vcar->value.id, tmp->vcar->value.id) == 0)
					{
						found = 1;
					}
					break;
				case OBJ_STRING:
					if (strcmp(arg1->vcar->value.s.str, tmp->vcar->value.s.str) == 0)
					{
						found = 1;
					}
					break;
				case OBJ_CONS:
				case OBJ_SET:
					c = (tmp->vcar->type == OBJ_SET) ? eqset(arg1->vcar, tmp->vcar) : eqcons(arg1->vcar, tmp->vcar);
					if (c == t)
					{
						found = 1;
					}
					break;
				default:
					found = 0;
					break;
				}
			}
		} while ((tmp = cdr(tmp)) != nil);
		if (found == 0)
		{
			p1 = new_object(arg1->type);
			p1->vcar = arg1->vcar;
			if (first == NULL)
				first = p1;
			if (prev != NULL)
				prev->vcdr = p1;
			prev = p1;
		}

	} while ((arg1 = cdr(arg1)) != nil);

	return first == NULL ? empty : first;
}
objectp
F_complement(const struct object *args)
{
	objectp p, ret;
	p = eval(car(args));

	_ASSERTP(p->type == OBJ_SET, NOT SET, COMPLEMENT, p);

	ret = new_object(OBJ_SET);
	ret->value.c.car = tau;
	ret->value.c.cdr = new_object(OBJ_CONS);
	ret->vcdr->vcar = new_object(OBJ_IDENTIFIER);
	ret->vcdr->vcar->value.id = strdup("not");
	ret->vcdr->vcdr = new_object(OBJ_CONS);
	if (COMPSET(p))
	{
		ret->vcdr->vcdr->vcar = cdr(p);
	}
	else
	{
		ret->vcdr->vcdr->vcar = new_object(OBJ_CONS);
		ret->vcdr->vcdr->vcar->vcar = new_object(OBJ_IDENTIFIER);
		ret->vcdr->vcdr->vcar->vcar->value.id = strdup("in");
		ret->vcdr->vcdr->vcar->vcdr = new_object(OBJ_CONS);
		ret->vcdr->vcdr->vcar->vcdr->vcar = tau;
		ret->vcdr->vcdr->vcar->vcdr->vcdr = new_object(OBJ_CONS);
		ret->vcdr->vcdr->vcar->vcdr->vcdr->vcar = p;
	}
	return ret;
}
objectp
F_member(const struct object *args)
{
	objectp m, x, set, ret, tmp, tv;
	test_type test = TEST_EQL;
	m = eval(car(args));
	set = eval(cadr(args));
	if (cddr(args) != nil)
		test = parse_test_arg(car(cddr(args)));
	if (COMPSET(set))
	{
		if (m->type == OBJ_CONS)
		{
			tv = new_object(OBJ_CONS);
			tv->vcar = new_object(OBJ_IDENTIFIER);
			tv->vcar->value.id = strdup("quote");
			tv->vcdr = new_object(OBJ_CONS);
			tv->vcdr->vcar = m;
		}
		else
		{
			tv = m;
		}
		tmp = sst(tau, tv, cdr(set));
		ret = eval(tmp);
		_ASSERTP(ret == nil || ret == t, NOT LOGIC EXPRESSION, MEMBERP, cdr(set));
		return ret;
	}
	_ASSERTP(set->type == OBJ_CONS || set->type == OBJ_SET, NOT CONS, MEMBERP, set);
	do
	{
		x = car(set);
		_ASSERTP(x->type != OBJ_NULL, IS NULL, MEMBERP, x);
		if (compare_by_test(m, x, test))
			return t;
		set = cdr(set);
	} while (set->type != OBJ_NIL);
	return nil;
}
