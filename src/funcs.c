#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
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

__inline__ static objectp F_car(const struct object *args)
{
	return car(eval(car(args)));
}

__inline__ static objectp F_cdr(const struct object *args)
{
	return cdr(eval(car(args)));
}

static objectp
F_strlen(const struct object *args)
{
	objectp arg1, result;
	arg1 = eval(car(args));
	_ASSERTP(arg1->type == OBJ_STRING, NOT STRING, STRLEN, arg1);
	result = new_object(OBJ_INTEGER);
	result->value.i = arg1->value.s.len;
	return result;
}

static objectp
F_cat(const struct object *args)
{
	objectp arg1, arg2, result;
	int i, j;
	arg1 = eval(car(args));
	arg2 = eval(cadr(args));
	_ASSERTP(arg1->type == OBJ_STRING, NOT STRING, CAT, arg1);
	_ASSERTP(arg2->type == OBJ_STRING, NOT STRING, CAT, arg2);

	result = new_object(OBJ_STRING);
	result->value.s.str = malloc((arg1->value.s.len + arg2->value.s.len + 1) * sizeof(char));
	if (result->value.s.str == NULL)
	{
		fprintf(stderr, "; CAT: UNABLE TO ALLOCATE MEMORY\n");
		return nil;
	}
	for (i = 0; i < arg1->value.s.len; i++)
	{
		result->value.s.str[i] = arg1->value.s.str[i];
	}
	for (j = 0; j < arg2->value.s.len; j++)
	{
		result->value.s.str[i] = arg2->value.s.str[j];
		i++;
	}
	result->value.s.str[i] = '\0';
	result->value.s.len = arg1->value.s.len + arg2->value.s.len;
	return result;
}

objectp
F_substr(const struct object *args)
{
	objectp arg1, arg2, arg3;
	objectp result;
	int offset = 0;
	arg1 = eval(car(args));
	arg2 = eval(car(cdr(args)));
	arg3 = eval(car(cdr(cdr(args))));
	_ASSERTP(arg1->type == OBJ_INTEGER, NON INTEGER, SUBSTR, arg1);
	_ASSERTP(arg2->type == OBJ_INTEGER, NON INTEGER, SUBSTR, arg2);
	_ASSERTP(arg3->type == OBJ_STRING, NON STRING, SUBSTR, arg3);

	if (arg1->value.i < 0)
	{
		fprintf(stderr, "; SUBSTR: NEGATIVE START INDEX %ld\n", arg1->value.i);
		return nil;
	}
	if (arg2->value.i < 0)
	{
		fprintf(stderr, "; SUBSTR: NEGATIVE LENGTH %ld\n", arg2->value.i);
		return nil;
	}
	result = new_object(OBJ_STRING);
	if (arg1->value.i > arg3->value.s.len)
	{
		fprintf(stderr, "; SUBSTR: INDEX OUT OF BOUNDS %ld\n", arg1->value.i);
		return nil;
	}
	if (arg1->value.i + arg2->value.i > arg3->value.s.len)
		offset = arg3->value.s.len - arg1->value.i;
	else
		offset = arg2->value.i;
	result->value.s.str = malloc((offset + 1) * sizeof(char));
	if (result->value.s.str == NULL)
	{
		fprintf(stderr, "; SUBSTR: UNABLE TO ALLOCATE MEMORY\n");
		return nil;
	}
	memcpy(result->value.s.str, arg3->value.s.str + arg1->value.i, offset);
	result->value.s.str[offset] = '\0';
	result->value.s.len = offset;
	return result;
}

static objectp
F_seq(const struct object *args)
{
	objectp arg1, arg2;
	objectp first = NULL, prev = NULL, p1;
	int i;
	arg1 = eval(car(args));
	arg2 = eval(cadr(args));
	_ASSERTP(arg1->type == OBJ_INTEGER, NOT INTEGER, SEQ, arg1);
	_ASSERTP(arg2->type == OBJ_INTEGER, NOT INTEGER, SEQ, arg2);
	_ASSERTP(arg1->value.i < arg2->value.i, INVALID BOUNDS, SEQ, args);
	i = arg1->value.i;
	do
	{
		p1 = new_object(OBJ_CONS);
		p1->vcar = new_object(OBJ_INTEGER);
		p1->vcar->value.i = i;
		i++;
		if (first == NULL)
			first = p1;
		if (prev != NULL)
			prev->vcdr = p1;
		prev = p1;
	} while (i <= arg2->value.i);
	return first;
}

objectp
F_atom(const struct object *args)
{
	switch (eval(car(args))->type)
	{
	case OBJ_T:
	case OBJ_NIL:
	case OBJ_IDENTIFIER:
	case OBJ_INTEGER:
	case OBJ_RATIONAL:
	case OBJ_STRING:
	case OBJ_TAU:
		return t;
	case OBJ_CONS:
	case OBJ_SET:
		return nil;
	default:
		return null;
	}
}

objectp
F_consp(const struct object *args)
{
	if (eval(car(args))->type == OBJ_CONS)
		return t;
	return nil;
}

objectp
F_listp(const struct object *args)
{
	objectp p;
	p = eval(car(args));
	if (p == nil)
		return t;
	if (p->type != OBJ_CONS)
		return nil;
	/* proper list: walk to the end, cdr must be nil */
	while (p->type == OBJ_CONS)
		p = cdr(p);
	return (p == nil) ? t : nil;
}

objectp
F_null(const struct object *args)
{
	if (eval(car(args)) == nil)
		return t;
	return nil;
}

objectp
F_symbolp(const struct object *args)
{
	objectp p;
	p = eval(car(args));
	if (p->type == OBJ_IDENTIFIER || p == nil || p == t)
		return t;
	return nil;
}

objectp
F_endp(const struct object *args)
{
	objectp p;
	p = eval(car(args));
	if (p == nil)
		return t;
	if (p->type == OBJ_CONS)
		return nil;
	fprintf(stderr, "; ENDP: NOT A LIST\n");
	return null;
}

objectp
F_loadfile(const struct object *args)
{
	objectp p;
	size_t i, len;
	char *f_name;
	p = eval(car(args));
	if (p->type != OBJ_IDENTIFIER)
		return null;
	len = strlen(p->value.id);
	f_name = malloc(len + 5);
	if (f_name == NULL)
	{
		fprintf(stderr, "; LOADFILE: ALLOCATING MEMORY\n");
		return nil;
	}
	snprintf(f_name, len + 5, "%s.lsp", p->value.id);
	for (i = 0; i < len + 4; i++)
	{
		f_name[i] = tolower(f_name[i]);
	}
	process_input(f_name);
	free(f_name);
	return nil;
}

objectp
F_typeof(const struct object *args)
{
	objectp p;
	p = new_object(OBJ_IDENTIFIER);
	switch (eval(car(args))->type)
	{
	case OBJ_RATIONAL:
		p->value.id = strdup("rational");
		break;
	case OBJ_STRING:
		p->value.id = strdup("string");
		break;
	case OBJ_INTEGER:
		p->value.id = strdup("integer");
		break;
	case OBJ_NULL:
		p->value.id = strdup("undefined");
		break;
	case OBJ_NIL:
		p->value.id = strdup("nil");
		break;
	case OBJ_T:
		p->value.id = strdup("t");
		break;
	case OBJ_CONS:
		p->value.id = strdup("cons");
		break;
	case OBJ_SET:
		p->value.id = strdup("set");
		break;
	case OBJ_TAU:
		p->value.id = strdup("tau");
		break;
	case OBJ_IDENTIFIER:
		p->value.id = strdup("identifier");
		break;
	}
	return p;
}

objectp
F_if(const struct object *args)
{
	if (eval(car(args)) != nil)
		return eval(cadr(args));
	args = cddr(args);
	if (args == nil)
		return nil;
	do
	{
		if (cdr(args) == nil)
			break;
		eval(car(args));
	} while ((args = cdr(args)) != nil);
	return eval(car(args));
}

objectp
F_cond(const struct object *args)
{
	do
	{
		if (eval(car(car(args))) != nil)
			return F_progn(cdar(args));
	} while ((args = cdr(args)) != nil);
	return nil;
}

objectp
F_ord(const struct object *args)
{
	objectp q, p;
	register int i;

	p = eval(car(args));
	i = 0L;
	q = new_object(OBJ_INTEGER);
	if (p == nil)
	{
		q->value.i = 0L;
		return q;
	}
	else if (DOTPAIRP(p))
	{
		q->value.i = 1L;
		return q;
	}
	_ASSERTP(p->type == OBJ_CONS || (p->type == OBJ_SET && !COMPSET(p)), NON CONS ARGUMENT, ORD, p);
	do
	{
		i++;
	} while ((p = cdr(p)) != nil);
	q->value.i = (long int)i;
	return q;
}

objectp
F_nth(const struct object *args)
{
	objectp p, n;
	register int i;
	n = eval(car(args));
	p = eval(car(cdr(args)));
	i = 1L;
	_ASSERTP(n->type == OBJ_INTEGER, NOT INTEGER INDEX, nth, n);
	if (n->value.i < 1)
	{
		fprintf(stderr, "; NTH: INDEX '%ld' OUT OF BOUNDS", n->value.i);
		return null;
	}
	if (p == nil || p == empty)
	{
		return nil;
	}
	_ASSERTP(p->type == OBJ_CONS && !DOTPAIRP(p), NON CONS ARGUMENT, ORD, p);
	if (n->value.i == 1)
		return car(p);
	do
	{
		i++;
	} while (((p = cdr(p)) != nil) && i < n->value.i);
	if (p == nil)
	{
		fprintf(stderr, "; NTH: INDEX '%ld' OUT OF BOUNDS", n->value.i);
		return null;
	}
	return car(p);
}

objectp
F_cons(const struct object *args)
{
	objectp p;
	p = new_object(OBJ_CONS);

	p->vcar = eval(car(args));
	p->vcdr = eval(cadr(args));
	return p;
}

objectp
F_list(const struct object *args)
{
	objectp first, prev, p1;
	p1 = new_object(OBJ_CONS);
	p1->vcar = eval(car(args));
	first = p1;
	prev = p1;
	args = cdr(args);
	if (args == nil)
		return first;
	do
	{
		p1 = new_object(OBJ_CONS);
		p1->vcar = eval(car(args));
		prev->vcdr = p1;
		prev = p1;
	} while ((args = cdr(args)) != nil);
	return first;
}

objectp
F_evlis(const struct object *args)
{
	objectp first = NULL, prev = NULL, p1;
	args = car(args);
	do
	{
		p1 = new_object(OBJ_CONS);
		p1->vcar = eval(car(args));
		if (first == NULL)
			first = p1;
		if (prev != NULL)
			prev->vcdr = p1;
		prev = p1;
	} while ((args = cdr(args)) != nil);
	return first;
}

objectp
F_map(const struct object *args)
{
	objectp p, p1, first, prev;

	first = prev = NULL;
	p1 = eval(cadr(args));
	p = car(args);
	_ASSERTP(p1->type == OBJ_CONS, NOT CONS, MAP, p1);
	_ASSERTP(p->type == OBJ_IDENTIFIER, NOT IDENTIFER, MAP, p);

	do
	{
		p = new_object(OBJ_CONS);
		p->vcar = new_object(OBJ_CONS);
		p->vcar->vcar = car(args);
		if (car(p1)->type == OBJ_CONS)
		{
			p->vcar->vcdr = car(p1);
		}
		else
		{
			p->vcar->vcdr = new_object(OBJ_CONS);
			p->vcar->vcdr->vcar = car(p1);
		}
		if (first == NULL)
			first = p;
		if (prev != NULL)
			prev->vcdr = p;
		prev = p;
	} while ((p1 = cdr(p1)) != nil);

	p = first;
	first = prev = NULL;
	do
	{
		p1 = new_object(OBJ_CONS);
		p1->vcar = eval(p->vcar);
		if (first == NULL)
			first = p1;
		if (prev != NULL)
			prev->vcdr = p1;
		prev = p1;
	} while ((p = cdr(p)) != nil);
	return first;
}

objectp
F_quit(const struct object *args)
{
	int exit_code = 0;
	if (car(args)->type == OBJ_INTEGER)
		exit_code = car(args)->value.i;
	clean_pools();
	clean_objects();
	clean_buffers();
	exit(exit_code);
	return NULL;
}

objectp
F_quote(const struct object *args)
{
	return car(args);
}

objectp
F_assoc(const struct object *args)
{
	objectp var, val;
	objectp assoc;
	test_type test = TEST_EQL;
	var = eval(car(args));
	assoc = eval(cadr(args));
	if (cddr(args) != nil)
		test = parse_test_arg(car(cddr(args)));
	do
	{
		val = caar(assoc);
		if (compare_by_test(var, val, test))
			return car(assoc);
	} while ((assoc = cdr(assoc)) != nil);
	return nil;
}

__inline__ objectp
F_progn(const struct object *args)
{
	do
	{
		if (cdr(args) == nil)
			break;
		eval(car(args));
	} while ((args = cdr(args)) != nil);
	return eval(car(args));
}

objectp
F_prog1(const struct object *args)
{
	objectp p1;
	p1 = eval(car(args));
	args = cdr(args);
	if (args == nil)
		return p1;
	do
	{
		eval(car(args));
	} while ((args = cdr(args)) != nil);
	return p1;
}

objectp
F_prog2(const struct object *args)
{
	objectp p1;
	eval(car(args));
	args = cdr(args);
	if (args == nil)
		return nil;
	p1 = eval(car(args));
	args = cdr(args);
	if (args == nil)
		return p1;
	do
	{
		eval(car(args));
	} while ((args = cdr(args)) != nil);
	return p1;
}

/* eq: identity for cons/set/string, value for numbers/identifiers/singletons */
static objectp
F_eq(const struct object *args)
{
	objectp a, b;
	a = eval(car(args));
	b = eval(cadr(args));
	if (a == b)
		return t;
	if (a->type != b->type)
		return nil;
	switch (a->type)
	{
	case OBJ_INTEGER:
		return (a->value.i == b->value.i) ? t : nil;
	case OBJ_RATIONAL:
		return ((a->value.r.n == b->value.r.n) &&
				(a->value.r.d == b->value.r.d))
				   ? t
				   : nil;
	case OBJ_IDENTIFIER:
		return strcmp(a->value.id, b->value.id) == 0 ? t : nil;
	default:
		return nil;
	}
}

/* eql: same as eq for SISP's type system */
static objectp
F_eql(const struct object *args)
{
	return F_eq(args);
}

/* equal: deep structural comparison (original SISP equality) */
objectp
F_equal(const struct object *args)
{
	objectp a, b;
	a = eval(car(args));
	b = eval(cadr(args));
	if (a == b)
		return t;
	if (a->type != b->type)
		return nil;
	switch (a->type)
	{
	case OBJ_IDENTIFIER:
		return strcmp(a->value.id, b->value.id) == 0 ? t : nil;
	case OBJ_STRING:
		return strcmp(a->value.s.str, b->value.s.str) == 0 ? t : nil;
	case OBJ_CONS:
		return eqcons(a, b);
	case OBJ_SET:
		return eqset(a, b);
	case OBJ_RATIONAL:
		return ((a->value.r.n == b->value.r.n) &&
				(a->value.r.d == b->value.r.d))
				   ? t
				   : nil;
	case OBJ_INTEGER:
		return (a->value.i == b->value.i) ? t : nil;
	default:
		return t;
	}
	return null;
}

/* equalp: like equal but case-insensitive strings and cross-type numeric */
static objectp
F_equalp(const struct object *args)
{
	objectp a, b;
	a = eval(car(args));
	b = eval(cadr(args));
	if (a == b)
		return t;
	if (ISNUMERIC(a) && ISNUMERIC(b))
		return (cmp_numeric(a, b) == 0) ? t : nil;
	if (a->type != b->type)
		return nil;
	switch (a->type)
	{
	case OBJ_IDENTIFIER:
		return strcmp(a->value.id, b->value.id) == 0 ? t : nil;
	case OBJ_STRING:
		return strcasecmp(a->value.s.str, b->value.s.str) == 0 ? t : nil;
	case OBJ_CONS:
		return eqcons_p(a, b);
	case OBJ_SET:
		return eqset_p(a, b);
	default:
		return t;
	}
	return null;
}

objectp
F_defun(const struct object *args)
{
	objectp body, params_node;
	body = new_object(OBJ_CONS);
	body->vcar = new_object(OBJ_IDENTIFIER);

	if (current_env != nil)
	{
		body->vcar->value.id = strdup("closure");
		body->vcdr = new_object(OBJ_CONS);
		body->vcdr->vcar = current_env;
		params_node = new_object(OBJ_CONS);
		body->vcdr->vcdr = params_node;
	}
	else
	{
		body->vcar->value.id = strdup("lambda");
		params_node = new_object(OBJ_CONS);
		body->vcdr = params_node;
	}

	if (cdr(car(args)) == nil)
	{
		params_node->vcar = nil;
		params_node->vcdr = cdr(args);
	}
	else
	{
		params_node->vcar = cdr(car(args));
		params_node->vcdr = cdr(args);
	}
	if (car(cdr(args))->type == OBJ_SET && COMPSET(car(cdr(args))))
	{
		args->vcdr->vcar = eval_set(car(cdr(args)));
	}
	set_function(car(car(args)), body);
	return body;
}

objectp
F_setq(const struct object *args)
{
	objectp p2;
	if (car(args)->type == OBJ_CONS)
		return F_defun(args);
	do
	{
		p2 = eval(cadr(args));
		set_object(car(args), p2);
	} while ((args = cddr(args)) != nil);
	return p2;
}

objectp
F_defun_alias(const struct object *args)
{
	/* CL-style (defun name (params) body...) */
	objectp name_and_params, p;

	/* Build a (name params...) cons to reuse F_defun. */
	name_and_params = new_object(OBJ_CONS);
	name_and_params->vcar = car(args);
	if (cadr(args) == nil)
		name_and_params->vcdr = nil;
	else
	{
		name_and_params->vcdr = new_object(OBJ_CONS);
		p = name_and_params->vcdr;
		objectp params = cadr(args);
		do
		{
			p->vcar = car(params);
			if (cdr(params) != nil)
			{
				p->vcdr = new_object(OBJ_CONS);
				p = p->vcdr;
			}
		} while ((params = cdr(params)) != nil);
	}

	/* Build ((name params...) body...) for F_defun. */
	p = new_object(OBJ_CONS);
	p->vcar = name_and_params;
	p->vcdr = cddr(args);
	return F_defun(p);
}

objectp
F_setq_alias(const struct object *args)
{
	/* CL-style (setq name value ...) — value bindings only. */
	objectp p2;
	do
	{
		p2 = eval(cadr(args));
		set_object(car(args), p2);
	} while ((args = cddr(args)) != nil);
	return p2;
}

objectp
F_pair(const struct object *args)
{
	objectp p, p1, p2, first = NULL, prev = NULL;
	p1 = eval(car(args));
	p2 = eval(cadr(args));
	_ASSERTP(p1->type == OBJ_CONS, NOT CONS, PAIR, p1);
	_ASSERTP(p2->type == OBJ_CONS, NOT CONS, PAIR, p2);
	_ASSERTP(!DOTPAIRP(p1), IS PAIR, PAIR, p1);
	_ASSERTP(!DOTPAIRP(p2), IS PAIR, PAIR, p2);
	do
	{
		p = new_object(OBJ_CONS);
		p->vcar = new_object(OBJ_CONS);
		p->vcar->vcar = car(p1);
		p->vcar->vcdr = new_object(OBJ_CONS);
		p->vcar->vcdr->vcar = car(p2);
		if (first == NULL)
			first = p;
		if (prev != NULL)
			prev->vcdr = p;
		prev = p;
	} while ((p1 = cdr(p1)) != nil && (p2 = cdr(p2)) != nil);
	return first;
}

objectp
F_bquote(const struct object *args)
{
	objectp p;
	if (car(args)->type == OBJ_IDENTIFIER)
		return car(args);

	p = new_object(OBJ_CONS);
	p->value.c.car = args->value.c.car;
	p->value.c.cdr = args->value.c.cdr;
	return car(eval_bquote(p));
}

objectp
F_comma(const struct object *args)
{
	return eval(car(args));
}

objectp
F_let(const struct object *args)
{
	objectp var, bind_list, body, r, q, new_env, entry;
	objectp val_first, val_prev, saved_env;
	val_first = val_prev = NULL;
	bind_list = car(args);
	if (bind_list->type != OBJ_CONS)
		return F_progn(cdr(args));
	body = cdr(args);
	saved_env = current_env;

	/* First pass: evaluate all initforms before installing any binding. */
	do
	{
		var = eval(cadr(car(bind_list)));
		r = new_object(OBJ_CONS);
		r->vcar = var;
		if (val_first == NULL)
			val_first = r;
		if (val_prev != NULL)
			val_prev->vcdr = r;
		val_prev = r;
	} while ((bind_list = cdr(bind_list)) != nil);

	/* Second pass: build new environment with all bindings. */
	new_env = current_env;
	bind_list = car(args);
	r = val_first;
	do
	{
		entry = new_object(OBJ_CONS);
		entry->vcar = caar(bind_list);
		entry->vcdr = car(r);
		q = new_object(OBJ_CONS);
		q->vcar = entry;
		q->vcdr = new_env;
		new_env = q;
		r = cdr(r);
	} while ((bind_list = cdr(bind_list)) != nil);

	current_env = new_env;
	__PROGN(body);
	q = eval(car(body));
	current_env = saved_env;

	return q;
}

objectp
F_letstar(const struct object *args)
{
	objectp var, bind_list, body, q, entry, node, saved_env;
	bind_list = car(args);
	if (bind_list->type != OBJ_CONS)
		return F_progn(cdr(args));
	body = cdr(args);
	saved_env = current_env;

	do
	{
		var = eval(cadr(car(bind_list)));
		entry = new_object(OBJ_CONS);
		entry->vcar = caar(bind_list);
		entry->vcdr = var;
		node = new_object(OBJ_CONS);
		node->vcar = entry;
		node->vcdr = current_env;
		current_env = node;
	} while ((bind_list = cdr(bind_list)) != nil);

	__PROGN(body);
	q = eval(car(body));
	current_env = saved_env;

	return q;
}

objectp
F_subst(const struct object *args)
{
	objectp sym, val, body;
	val = eval(car(args));
	if (val->type == OBJ_CONS)
	{
		body = eval(car(cdr(args)));
		_ASSERTP(body->type == OBJ_CONS, NOT CONS, SUBST, body);
		do
		{
			body = sst(car(car(val)), cdr(car(val)), body);
		} while ((val = cdr(val)) != nil);
		return body;
	}
	else
	{
		sym = eval(car(cdr(args)));
		body = eval(car(cddr(args)));
		_ASSERTP(body->type == OBJ_CONS, NOT CONS, SUBST, body);
		return sst(val, sym, body);
	}
}

objectp
F_labels(const struct object *args)
{
	objectp var, bind, bind_list, body, q, s, saved_env;
	objectp entry_first, entry_last, node, entry;
	entry_first = entry_last = NULL;
	bind_list = car(args);
	_ASSERTP((bind_list->type == OBJ_CONS && bind_list != nil), EMPTY CONS, LABELS, bind_list);
	body = cdr(args);
	_ASSERTP((body->type == OBJ_CONS && body != nil), EMPTY CONS, LABELS, body);

	saved_env = current_env;

	/* Step 1: create env entries with placeholders for all function names. */
	node = bind_list;
	do
	{
		bind = car(caar(node));
		entry = new_object(OBJ_CONS);
		entry->vcar = bind;
		entry->vcdr = nil;
		q = new_object(OBJ_CONS);
		q->vcar = entry;
		q->vcdr = nil;
		if (entry_first == NULL)
			entry_first = q;
		else
			entry_last->vcdr = q;
		entry_last = q;
	} while ((node = cdr(node)) != nil);

	/* Link placeholder entries to current_env. */
	entry_last->vcdr = current_env;
	current_env = entry_first;

	/* Step 2: create closures capturing current_env, fill in placeholders. */
	node = entry_first;
	q = bind_list;
	do
	{
		var = new_object(OBJ_CONS);
		var->vcar = cdr(caar(q));
		var->vcdr = cdar(q);
		s = new_object(OBJ_CONS);
		s->vcar = new_object(OBJ_IDENTIFIER);
		s->vcar->value.id = strdup("closure");
		s->vcdr = new_object(OBJ_CONS);
		s->vcdr->vcar = current_env;
		s->vcdr->vcdr = var;
		node->vcar->vcdr = s;
		node = node->vcdr;
	} while ((q = cdr(q)) != nil);

	/* Step 3: evaluate body. */
	__PROGN(body);
	q = eval(car(body));

	current_env = saved_env;
	return q;
}

objectp
F_eval(const struct object *args)
{
	return eval(eval(car(args)));
}

objectp
F_defmacro(const struct object *args)
{
	objectp func_name, body;

	func_name = car(car(args));
	body = new_object(OBJ_CONS);
	body->vcar = new_object(OBJ_IDENTIFIER);
	body->vcar->value.id = strdup("macro");
	body->vcdr = new_object(OBJ_CONS);
	body->vcdr->vcar = cdr(car(args));
	body->vcdr->vcdr = cdr(args);
	set_function(func_name, body);

	return body;
}

objectp
F_macroexpand_1(const struct object *args)
{
	objectp form, func_name, macro_def;

	form = eval(car(args));
	_ASSERTP(form->type == OBJ_CONS, NOT A LIST, MACROEXPAND-1, form);

	func_name = car(form);
	_ASSERTP(func_name->type == OBJ_IDENTIFIER, NOT A SYMBOL, MACROEXPAND-1, func_name);

	macro_def = get_function(func_name);
	_ASSERTP(is_macro(macro_def), NOT A MACRO, MACROEXPAND-1, func_name);

	return expand_macro(macro_def, cdr(form));
}

objectp
F_pop(const struct object *stack)
{
	objectp p, q, r;
	_ASSERTP(car(stack)->type == OBJ_IDENTIFIER, NOT IDENTIFIER, POP, car(stack));
	r = eval(car(stack));
	_ASSERTP(r->type == OBJ_CONS, NOT CONS, POP, r);
	q = car(r);
	p = cdr(r);
	set_object(car(stack), p);
	return q;
}

objectp
F_push(const struct object *args)
{
	objectp s, e, r, first, prev;
	first = prev = NULL;
	_ASSERTP(car(cdr(args))->type == OBJ_IDENTIFIER, NOT IDENTIFIER, PUSH, car(cdr(args)));
	e = eval(car(args));
	s = eval(car(cdr(args)));
	_ASSERTP(s->type == OBJ_CONS, NOT CONS, PUSH, s);
	r = new_object(OBJ_CONS);
	r->vcar = e;
	first = r;
	prev = r;
	do
	{
		r = new_object(OBJ_CONS);
		r->vcar = car(s);
		prev->vcdr = r;
		prev = r;
	} while ((s = cdr(s)) != nil);
	set_object(car(cdr(args)), first);
	return first;
}

objectp
F_dump(const struct object *args)
{
	objectp pn;
	pn = eval(car(args));
	if (pn == nil)
		dump_object(0);
	else if (pn->type == OBJ_INTEGER && pn->value.i >= 3 && pn->value.i <= 8)
		dump_object((int)pn->value.i);
	return null;
}

objectp
F_undef(const struct object *args)
{
	objectp p;
	do
	{
		p = car(args);

		if (p->type != OBJ_IDENTIFIER)
			return nil;
		remove_object(p);
	} while ((args = cdr(args)) != nil);
	return t;
}

objectp
F_print(const struct object *arg)
{
	do
	{
		princ_object(stdout, eval(car(arg)));
		fputc(' ', stdout);
	} while ((arg = cdr(arg)) != nil);
	return null;
}

objectp
F_numberp(const struct object *arg)
{
	if (ISNUMERIC(eval(car(arg))))
		return t;
	return nil;
}
objectp
F_gc(const struct object *arg)
{
	if (arg->type != OBJ_NULL)
		garbage_collect();
	return t;
}
objectp
F_lazy(const struct object *arg)
{
	objectp p;
	p = eval(car(arg));
	if (p == t)
	{
		printf("; LAZY EVAL: ENABLED\n");
		lazy_eval = true;
	}
	else
	{
		printf("; LAZY EVAL: DISABLED\n");
		lazy_eval = false;
	}
	return t;
}

const funcs functions[] = {
	{"*", F_prod},
	{"+", F_add},
	{"/", F_div},
	{"<", F_less},
	{"<=", F_lesseq},
	{"<=>", F_iff},
	{"=", F_numeq},
	{"=>", F_imply},
	{">", F_great},
	{">=", F_greateq},
	{"\\", F_diff},
	{"^", F_pow},
	{"and", F_and},
	{"append", F_union},
	{"assoc", F_assoc},
	{"atom", F_atom},
	{"atomp", F_atom},
	{"bquote", F_bquote},
	{"cap", F_cap},
	{"car", F_car},
	{"cat", F_cat},
	{"cdr", F_cdr},
	{"comma", F_comma},
	{"comp", F_complement},
	{"cond", F_cond},
	{"cons", F_cons},
	{"consp", F_consp},
	{"define", F_setq},
	{"defmacro", F_defmacro},
	{"defun", F_defun_alias},
	{"diff", F_diff},
	{"dump", F_dump},
	{"endp", F_endp},
	{"eq", F_eq},
	{"eql", F_eql},
	{"equal", F_equal},
	{"equalp", F_equalp},
	{"eval", F_eval},
	{"evlis", F_evlis},
	{"gc", F_gc},
	{"if", F_if},
	{"in", F_member},
	{"labels", F_labels},
	{"lazy", F_lazy},
	{"let", F_let},
	{"let*", F_letstar},
	{"list", F_list},
	{"listp", F_listp},
	{"load", F_loadfile},
	{"macroexpand-1", F_macroexpand_1},
	{"map", F_map},
	{"memberp", F_member},
	{"mod", F_mod},
	{"not", F_not},
	{"notin", F_notin},
	{"nth", F_nth},
	{"null", F_null},
	{"numberp", F_numberp},
	{"or", F_or},
	{"ord", F_ord},
	{"par", F_pair},
	{"pop", F_pop},
	{"pow", F_powerset},
	{"print", F_print},
	{"prod", F_setprod},
	{"prog1", F_prog1},
	{"prog2", F_prog2},
	{"progn", F_progn},
	{"push", F_push},
	{"quit", F_quit},
	{"quote", F_quote},
	{"seq", F_seq},
	{"setq", F_setq_alias},
	{"sisp-equal", F_equal},
	{"strlen", F_strlen},
	{"subset", F_subset},
	{"subst", F_subst},
	{"substr", F_substr},
	{"symbolp", F_symbolp},
	{"symdiff", F_symdiff},
	{"typeof", F_typeof},
	{"undef", F_undef},
	{"union", F_union},
	{"xor", F_xor},
};
