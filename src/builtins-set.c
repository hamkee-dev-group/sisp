#include <stdlib.h>

#include "sisp.h"
#include "doc.h"
#include "runtime.h"
#include "builtins.h"
#include "eval.h"
#include "equal.h"
#include "print.h"
#include "comprehension.h"

static int
properp(objectp collection)
{
	while (collection != nil)
	{
		if (collection->type != OBJ_CONS && collection->type != OBJ_SET)
			return 0;
		collection = cdr(collection);
	}
	return 1;
}

/* Common Lisp APPEND for lists.  All arguments except the last must be proper
   lists; the final value becomes the result tail and may be any object. */
DOC("append", "Concatenate lists into a new list. Only the last is shared.\n"
			  "  (append (quote (1 2)) (quote (3))) => (1 2 3)\n"
			  "  (append) => nil\n"
			  "See: cons list flatten")
objectp
builtin_append(const struct object *args)
{
	objectp value, head = nil, tail = NULL, node;

	if (args == nil)
		return nil;
	while (cdr(args) != nil)
	{
		value = eval(car(args));
		require_condition((properp(value)), "APPEND", "IMPROPER LIST", value);
		while (value != nil)
		{
			node = make_object(OBJ_CONS);
			node->value.cons.car = car(value);
			if (head == nil)
				head = node;
			else
				tail->value.cons.cdr = node;
			tail = node;
			value = cdr(value);
		}
		args = cdr(args);
	}
	value = eval(car(args));
	if (head == nil)
		return value;
	tail->value.cons.cdr = value;
	return head;
}

/* Convert an extension set {1 2 3} to comprehension {tau : (in tau {1 2 3})} */
static objectp
set_to_class(objectp set)
{
	return make_set(tau, cons(id_in, cons(tau, cons(set, nil))));
}

/* Every set operation on a comprehension has the same shape: coerce each
   operand to a predicate class, take its predicate, and join the predicates
   under one logical connective. Only the connective differs, so that is the
   only thing the four operations below still say for themselves. */
static objectp
class_predicate(objectp set, const char *operation)
{
	if (set->type == OBJ_SET && !object_is_comprehension_set(set))
		set = set_to_class(set);
	require_condition((object_is_comprehension_set(set)), operation,
					  "NOT COMPREHENSION SET", set);
	return cdr(set);
}

/* n-ary: (or p1 p2 ...) for union, (and p1 p2 ...) for cap */
static objectp
class_join(const struct object *values, objectp connective, const char *operation)
{
	struct list predicates;

	list_init(&predicates, OBJ_CONS);
	do
		list_append(&predicates, class_predicate(car(values), operation));
	while ((values = cdr(values)) != nil);
	return make_set(tau, cons(connective, predicates.head));
}

static objectp
class_diff(objectp left, objectp right)
{
	objectp keep = class_predicate(left, "DIFF");
	objectp drop = class_predicate(right, "DIFF");

	return make_set(tau,
					cons(id_and, cons(keep, cons(cons(id_not, cons(drop, nil)), nil))));
}

static objectp
class_symdiff(objectp left, objectp right)
{
	objectp a = class_predicate(left, "SYMDIFF");
	objectp b = class_predicate(right, "SYMDIFF");

	return make_set(tau, cons(id_xor, cons(a, cons(b, nil))));
}
DOC("symdiff", "Elements in exactly one of the two sets.\n"
			   "  (symdiff {1 2} {2 3}) => {1 3}\n"
			   "See: union cap diff")
objectp
builtin_symdiff(const struct object *args)
{
	objectp left = eval(car(args)), right = eval(cadr(args)), node;
	struct list result;
	if (left == empty && right->type == OBJ_SET)
		return right;
	if (right == empty && left->type == OBJ_SET)
		return left;
	require_condition(((left->type == OBJ_CONS && right->type == OBJ_CONS) ||
					   (left->type == OBJ_SET && right->type == OBJ_SET)),
					  "SYMDIFF", "NOT CONS", args);
	if (object_is_comprehension_set(left) || object_is_comprehension_set(right))
		return class_symdiff(left, right);
	require_condition((properp(left) && properp(right)), "SYMDIFF", "IMPROPER LIST",
					  left);
	list_init(&result, left->type);
	for (node = left; node != nil; node = cdr(node))
		if (!collection_contains(car(node), right))
			list_append(&result, car(node));
	for (node = right; node != nil; node = cdr(node))
		if (!collection_contains(car(node), left))
			list_append(&result, car(node));
	return result.head == nil ? empty : result.head;
}
DOC("union", "Every element of either set.\n"
			 "  (union {1} {2}) => {1 2}\n"
			 "See: cap diff symdiff")
objectp
builtin_union(const struct object *args)
{
	objectp collection, values, scan;
	enum object_type type = OBJ_SET;
	int have_type = 0;
	struct list result;

	list_init(&result, OBJ_CONS);
	values = eval_arguments(args);
	/* Check if any argument is a comprehension set. The empty set is
	   represented as a comprehension but is the extensional unit of
	   union, so it is skipped instead of forcing a symbolic result. */
	scan = values;
	do
	{
		collection = car(scan);
		if (collection != empty && object_is_comprehension_set(collection))
			return class_join(values, id_or, "UNION");
	} while ((scan = cdr(scan)) != nil);
	do
	{
		collection = car(values);
		if (collection == empty)
			continue;
		require_condition(
			(((collection->type == OBJ_CONS && !object_is_dotted_pair(collection)) ||
			  collection->type == OBJ_SET)),
			"UNION", "NOT CONS", collection);
		if (!have_type)
		{
			/* The node type follows the first non-empty operand, so the
			   builder is started here rather than before the loop.  Nothing
			   has been appended yet, so this runs exactly once. */
			type = collection->type;
			have_type = 1;
			list_init(&result, type);
		}
		require_condition(((collection->type == type)), "UNION", "TYPE MISMATCH",
						  collection);
		do
		{
			list_append(&result, car(collection));
			collection = cdr(collection);
		} while (collection != nil);
	} while ((values = cdr(values)) != nil);
	if (!have_type || result.head == nil)
		return empty;
	return (type == OBJ_SET) ? eval_set(result.head) : result.head;
}

DOC("subset", "Whether every element of the first set is in the second.\n"
			  "  (subset {1} {1 2}) => t\n"
			  "See: in union")
objectp
builtin_subset(const struct object *args)
{
	objectp subset, superset, predicate, result;
	subset = eval(car(args));
	superset = eval(cadr(args));
	if (subset == empty)
		return t;

	require_condition((subset->type == OBJ_SET && !object_is_comprehension_set(subset)),
					  "SUBSET", "NOT EXTENSION SET", subset);
	require_type(superset, OBJ_SET, "SUBSET");
	if (object_is_comprehension_set(superset))
	{
		do
		{
			predicate = substitute_tree(tau, car(subset), cdr(superset));
			result = eval(predicate);
			if (result == nil)
				return result;
		} while ((subset = cdr(subset)) != nil);
	}
	else
	{
		do
		{
			if (!collection_contains(car(subset), superset))
				return nil;
		} while ((subset = cdr(subset)) != nil);
	}
	return t;
}

DOC("notin", "Non-membership.\n"
			 "  (notin 3 {1 2}) => t\n"
			 "See: in")
objectp
builtin_notin(const struct object *args)
{
	return (builtin_in(args) == nil) ? t : nil;
}

/* Product involving an infinite class: represent it as the predicate
   class of ordered pairs whose first component is in a and second in b.
   Membership of a candidate pair p tests (in (car p) a) and (in (cadr p) b). */
static objectp
class_product(objectp left, objectp right)
{
	objectp car_tau = cons(id_car, cons(tau, nil));
	objectp cadr_tau = cons(id_car, cons(cons(id_cdr, cons(tau, nil)), nil));
	objectp left_predicate = cons(id_in, cons(car_tau, cons(left, nil)));
	objectp right_predicate = cons(id_in, cons(cadr_tau, cons(right, nil)));
	objectp result = make_object(OBJ_SET);

	result->value.cons.car = tau;
	result->value.cons.cdr =
		cons(id_and, cons(left_predicate, cons(right_predicate, nil)));
	return result;
}
static objectp
finite_powerset(objectp set)
{
	objectp original_set = set;
	struct list result, subset;
	objectp *elements;
	int cardinality = 0;
	int index, subset_count;

	do
	{
		cardinality++;
	} while ((set = cdr(set)) != nil);
	if (cardinality >= 31)
	{
		sisp_error("POWERSET: SET TOO LARGE");
		return nil;
	}

	subset_count = 1L << cardinality;
	elements = xmalloc((size_t)cardinality * sizeof(*elements));
	set = original_set;
	for (index = 0; index < cardinality; index++)
	{
		elements[index] = car(set);
		set = cdr(set);
	}

	list_init(&result, OBJ_SET);
	for (int mask = 0; mask < subset_count; mask++)
	{
		list_init(&subset, OBJ_SET);
		for (index = 0; index < cardinality; index++)
			if (mask & (1 << index))
				list_append(&subset, elements[index]);
		/* the empty subset is the canonical empty-set singleton */
		list_append(&result, subset.head == nil ? empty : subset.head);
	}
	free(elements);
	return result.head;
}

DOC("powerset", "The set of all subsets.\n"
				"  (powerset {1 2}) => {{} {1} {2} {1 2}}\n"
				"See: cartesian-product k-subsets")
objectp
builtin_powerset(const struct object *args)
{
	objectp set = eval(car(args));
	require_type(set, OBJ_SET, "POW");
	if (set == empty)
		return make_set(empty, nil);
	if (object_is_comprehension_set(set))
		return make_set(tau, cons(id_subset, cons(tau, cons(set, nil))));
	return finite_powerset(set);
}
DOC("cartesian-product", "All ordered pairs from two sets.\n"
						 "  (cartesian-product {1} {2 3}) => {(1 2) (1 3)}\n"
						 "See: powerset pairs-such-that")
objectp
builtin_cartesian_product(const struct object *args)
{
	objectp left, right, right_head;
	struct list result;

	left = eval(car(args));
	right = eval(cadr(args));

	require_type(left, OBJ_SET, "PROD");
	require_type(right, OBJ_SET, "PROD");
	/* A x {} = {} x B = {}, extensionally empty. */
	if (left == empty || right == empty)
		return empty;
	/* Any infinite factor makes the product a predicate class, not an
	   enumerable extension; only finite x finite materializes. */
	if (object_is_comprehension_set(left) || object_is_comprehension_set(right))
		return class_product(left, right);
	list_init(&result, OBJ_SET);
	right_head = right;
	do
	{
		right = right_head;
		do
		{
			/* the ordered pair (car a) (car b) as a proper two-list */
			list_append(&result, cons(car(left), cons(car(right), nil)));
		} while ((right = cdr(right)) != nil);
	} while ((left = cdr(left)) != nil);

	return result.head;
}
DOC("cap", "Elements common to all the sets given.\n"
		   "  (cap {1 2} {2 3}) => {2}\n"
		   "See: union diff")
objectp
builtin_cap(const struct object *args)
{
	objectp values, left, value, node, value_node, remaining;
	struct list result;
	enum object_type collection_type;
	int include;

	/* the two-argument minimum lives in the builtin table, not here */
	values = eval_arguments(args);
	left = car(values);

	for (value_node = values; value_node != nil; value_node = cdr(value_node))
	{
		value = car(value_node);
		if (value == nil)
			return nil;
		if (value == empty)
			return empty;
	}

	for (value_node = values; value_node != nil; value_node = cdr(value_node))
		if (object_is_comprehension_set(car(value_node)))
			return class_join(values, id_and, "CAP");

	collection_type = left->type;
	require_condition((collection_type == OBJ_CONS || collection_type == OBJ_SET),
					  "CAP", "NOT A COLLECTION", left);
	for (value_node = values; value_node != nil; value_node = cdr(value_node))
	{
		value = car(value_node);
		require_condition((value->type == collection_type), "CAP", "TYPE MISMATCH",
						  value);
		require_condition((properp(value)), "CAP", "IMPROPER COLLECTION", value);
	}

	list_init(&result, left->type);
	for (node = left; node != nil; node = cdr(node))
	{
		include = 1;
		for (remaining = cdr(values); remaining != nil; remaining = cdr(remaining))
			if (!collection_contains(car(node), car(remaining)))
			{
				include = 0;
				break;
			}
		if (include)
			list_append(&result, car(node));
	}
	return result.head == nil ? empty : result.head;
}

DOC("diff", "Elements of the first set that are not in the second.\n"
			"  (diff {1 2} {2}) => {1}\n"
			"See: union cap symdiff")
objectp
builtin_diff(const struct object *args)
{
	objectp left = eval(car(args)), right = eval(cadr(args)), node;
	struct list result;
	if (left == nil)
		return nil;
	if (right == nil && left->type == OBJ_CONS)
		return left;
	if (left == empty)
		return empty;
	if (right == empty && left->type == OBJ_SET)
		return left;
	require_condition(((left->type == OBJ_CONS && right->type == OBJ_CONS) ||
					   (left->type == OBJ_SET && right->type == OBJ_SET)),
					  "DIFF", "NOT CONS", args);
	if (object_is_comprehension_set(left) || object_is_comprehension_set(right))
		return class_diff(left, right);
	require_condition((properp(left) && properp(right)), "DIFF", "IMPROPER LIST", left);
	list_init(&result, left->type);
	for (node = left; node != nil; node = cdr(node))
		if (!collection_contains(car(node), right))
			list_append(&result, car(node));
	return result.head == nil ? empty : result.head;
}
DOC("complement",
	"Complement of a set: a comprehension over everything not in it.\n"
	"  (in 3 (complement {1 2})) => t\n"
	"Note: The result has no finite enumeration until applied to a carrier.\n"
	"See: enumerate notin")
objectp
builtin_complement(const struct object *args)
{
	objectp set, predicate;
	set = eval(car(args));

	require_type(set, OBJ_SET, "COMPLEMENT");

	if (object_is_comprehension_set(set))
		predicate = cdr(set);
	else
		predicate = cons(id_in, cons(tau, cons(set, nil)));
	return make_set(tau, cons(id_not, cons(predicate, nil)));
}
DOC("in", "Membership. Works for both kinds of set.\n"
		  "  (in 1 {1 2}) => t\n"
		  "See: notin subset contains")
objectp
builtin_in(const struct object *args)
{
	objectp member, element, set, result, predicate, substituted_value;
	enum equality_test test = TEST_EQ;
	member = eval(car(args));
	set = eval(cadr(args));
	if (set == nil)
		return nil;
	if (cddr(args) != nil)
		test = parse_equality_test(car(cddr(args)));
	else if (set->type == OBJ_SET)
		test = TEST_EQUAL; /* sets deduplicate structurally, so membership
							  must use the same equality */
	if (object_is_comprehension_set(set))
	{
		if (member->type == OBJ_CONS)
			substituted_value = make_quoted(member);
		else
			substituted_value = member;
		predicate = substitute_tree(tau, substituted_value, cdr(set));
		result = eval(predicate);
		require_condition((result == nil || result == t), "MEMBERP",
						  "NOT LOGIC EXPRESSION", cdr(set));
		return result;
	}
	require_condition((set->type == OBJ_CONS || set->type == OBJ_SET), "MEMBERP",
					  "NOT CONS", set);
	do
	{
		element = car(set);
		if (objects_equal_by_test(member, element, test))
			return t;
		set = cdr(set);
	} while (set->type != OBJ_NIL);
	return nil;
}
