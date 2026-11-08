/* Equality is centralized in objects_equal:
   eq compares composite identity and atomic values; equal compares structure,
   with unordered set comparison and predicate comparison for comprehensions.
   Numeric comparison lives in builtins-math.c. */
#include <string.h>

#include "sisp.h"
#include "runtime.h"
#include "eval.h"
#include "builtins.h"
#include "equal.h"
#include "numeric.h"

static objectp sets_equal(objectp a, objectp b);

/* Deep structural equality. Numbers must match in type as well as value:
   1 and 1.0 are equal under = but not under equal, which is what keeps exact
   and inexact distinguishable. */
static objectp
objects_equal(objectp a, objectp b)
{
	STACK_CHECK();
	if (a->type != b->type)
		return nil;
	switch (a->type)
	{
	case OBJ_INTEGER:
		return a->value.i == b->value.i ? t : nil;
	case OBJ_FLOAT:
		return a->value.f == b->value.f ? t : nil;
	case OBJ_RATIONAL:
		return (a->value.rational.n == b->value.rational.n &&
				a->value.rational.d == b->value.rational.d)
				   ? t
				   : nil;
	case OBJ_T:
	case OBJ_NIL:
	case OBJ_TAU:
		return t;
	case OBJ_IDENTIFIER:
		return strcmp(a->value.identifier.name, b->value.identifier.name) == 0 ? t
																			   : nil;
	case OBJ_STRING:
		return strcmp(a->value.string.data, b->value.string.data) == 0 ? t : nil;
	case OBJ_CONS:
		return objects_equal(car(a), car(b)) == t
				   ? objects_equal(cdr(a), cdr(b))
				   : nil;
	case OBJ_SET:
		return sets_equal(a, b);
	case OBJ_MATRIX:
		return matrices_equal(a, b) ? t : nil;
	case OBJ_BUILTIN:
		/* Distinct names may intentionally share one implementation. */
		return a->value.builtin->function == b->value.builtin->function ? t : nil;
	default:
		return t;
	}
}

/* Set equality: same cardinality, and every element of a matches one in b.
   Comprehension sets compare by their predicate structure. */
static objectp
sets_equal(objectp a, objectp b)
{
	objectp candidate_node, count_node;
	int found;

	STACK_CHECK();
	if (a == b)
		return t;
	if (object_is_comprehension_set(a) || object_is_comprehension_set(b))
	{
		if (!object_is_comprehension_set(a) || !object_is_comprehension_set(b))
			return nil;
		return objects_equal(car(a), car(b)) == t
				   ? objects_equal(cdr(a), cdr(b))
				   : nil;
	}
	for (count_node = b; a != nil; a = cdr(a), count_node = cdr(count_node))
	{
		found = 0;
		for (candidate_node = b; candidate_node != nil;
			 candidate_node = cdr(candidate_node))
			if (objects_equal(car(a), car(candidate_node)) == t)
			{
				found = 1;
				break;
			}
		if (!found)
			return nil;
	}
	return count_node == nil ? t : nil;
}

enum equality_test
parse_equality_test(objectp arg)
{
	objectp symbol = eval(arg);
	if (symbol->type == OBJ_IDENTIFIER)
	{
		if (!strcmp(symbol->value.identifier.name, "eq"))
			return TEST_EQ;
		if (!strcmp(symbol->value.identifier.name, "equal"))
			return TEST_EQUAL;
	}
	sisp_raise(make_error_value("ASSOC/MEMBER: INVALID TEST", symbol));
	return TEST_EQ; /* unreachable */
}

int
objects_equal_by_test(objectp a, objectp b, enum equality_test test)
{
	switch (test)
	{
	case TEST_EQ:
		if (a == b)
			return 1;
		if (a->type != b->type)
			return 0;
		switch (a->type)
		{
		case OBJ_INTEGER:
			return a->value.i == b->value.i;
		case OBJ_FLOAT:
			return a->value.f == b->value.f;
		case OBJ_RATIONAL:
			return (a->value.rational.n == b->value.rational.n) &&
				   (a->value.rational.d == b->value.rational.d);
		case OBJ_IDENTIFIER:
			return strcmp(a->value.identifier.name, b->value.identifier.name) == 0;
		case OBJ_T:
		case OBJ_NIL:
		case OBJ_TAU:
			return 1;
		default:
			return 0;
		}
	case TEST_EQUAL:
		return objects_equal(a, b) == t;
	}
	return 0;
}

/* Set construction and cap/diff/symdiff/subset share equal semantics with in. */
int
collection_contains(objectp value, objectp collection)
{
	if (collection == NULL)
		return 0;
	for (; collection != nil; collection = cdr(collection))
		if (objects_equal(value, car(collection)) == t)
			return 1;
	return 0;
}
