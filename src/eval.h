#ifndef SISP_EVAL_H
#define SISP_EVAL_H

#include "sisp.h"
#include "runtime.h"
#ifdef SISP_DEBUG
#include "print.h"
#endif

/* eval is inline so its argument is evaluated exactly once. Pair accessors
   live in runtime.h because they are fundamental object operations. */

objectp eval_bquote(objectp);
objectp eval_rational(const struct object *);
objectp eval_float(const struct object *);
objectp eval_cons(const struct object *);
objectp eval_set(const struct object *);
objectp eval_arguments(const struct object *);
objectp eval_body_prefix(objectp);
objectp invoke_builtin(const struct builtin_spec *, const struct object *);
objectp apply_function(objectp, objectp, const char *);
objectp expand_macro(objectp, objectp, int);
int object_is_macro(objectp);
#ifdef SISP_DEBUG
extern bool trace_enabled;
#endif

static inline bool
object_is_numeric(const struct object *object)
{
	return object->type == OBJ_INTEGER || object->type == OBJ_RATIONAL ||
		   object->type == OBJ_FLOAT;
}

static inline bool
object_is_boolean(const struct object *object)
{
	return object->type == OBJ_T || object->type == OBJ_NIL;
}

static inline bool
object_is_dotted_pair(const struct object *object)
{
	return object->type == OBJ_CONS && cdr(object)->type != OBJ_CONS &&
		   cdr(object) != nil;
}

static inline bool
object_is_comprehension_set(const struct object *object)
{
	return object->type == OBJ_SET && cdr(object)->type != OBJ_SET &&
		   cdr(object) != nil;
}

static inline objectp
eval(const struct object *p)
{
#ifdef SISP_DEBUG
	objectp result;
#endif

	switch (p->type)
	{
	case OBJ_T:
		return t;
	case OBJ_NIL:
		return nil;
	case OBJ_TAU:
		return tau;
	case OBJ_RATIONAL:
		return eval_rational(p);
	case OBJ_FLOAT:
		return eval_float(p);
	case OBJ_IDENTIFIER:
#ifdef SISP_DEBUG
		result = object_get_value(p);
		if (trace_enabled)
		{
			fprintf(stderr, "; %s => ", p->value.identifier.name);
			print_object(stderr, result);
			fputc('\n', stderr);
		}
		return result;
#else
		return object_get_value(p);
#endif
	case OBJ_CONS:
		return eval_cons(p);
	case OBJ_SET:
		return eval_set(p);
	default:
		return (objectp)p; /* integer, string, matrix, builtin */
	}
}

#endif
