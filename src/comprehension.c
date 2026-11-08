/* Set comprehensions capture free variables and test membership by
   substitution. Enumeration requires a finite carrier (prelude enumerate).
   Set literals are evaluated and structurally deduplicated here. */
#include <string.h>

#include "sisp.h"
#include "runtime.h"
#include "eval.h"
#include "builtins.h"
#include "equal.h"
#include "comprehension.h"

/* Replace every occurrence of `target` in `body` with `replacement`.
   Matches use equal semantics.

   Two things the walk must respect:
   - The node type is preserved, so recursing into a set rebuilds a set rather
     than turning {1 2} into (1 2).
   - Comprehension sets are NOT descended into. Their cdr is the predicate, not
     a tail, so walking one as a list would flatten {m : pred} into
     [m, car(pred), ...]. Doing that properly means treating the inner bound
     variable as a shadow, which is a separate job; they are left intact, as
     they already were. This also protects the empty-set singleton, which is
     represented as a comprehension. */
objectp
substitute_tree(objectp target, objectp replacement, objectp body)
{
	objectp element, substituted;
	struct list result;

	list_init(&result, body->type);
	do
	{
		element = car(body);
		if (objects_equal_by_test(target, element, TEST_EQUAL))
			substituted = replacement;
		else if (element->type == OBJ_CONS ||
				 (element->type == OBJ_SET && !object_is_comprehension_set(element)))
			substituted = substitute_tree(target, replacement, element);
		else
			substituted = element;
		list_append(&result, substituted);
	} while ((body = cdr(body)) != nil);

	return result.head;
}

/* Is identifier id present (by name) in the shadow set? */
static int
shadow_contains_name(objectp identifier, objectp shadow)
{
	for (; shadow != NULL && shadow->type == OBJ_CONS; shadow = cdr(shadow))
		if (car(shadow)->type == OBJ_IDENTIFIER &&
			!strcmp(car(shadow)->value.identifier.name,
					identifier->value.identifier.name))
			return 1;
	return 0;
}

/* push each identifier of a lambda parameter list onto the shadow set */
static objectp
shadow_parameters(objectp parameters, objectp shadow)
{
	for (; parameters != NULL && parameters->type == OBJ_CONS;
		 parameters = cdr(parameters))
		if (car(parameters)->type == OBJ_IDENTIFIER)
			shadow = cons(car(parameters), shadow);
	return shadow;
}

static objectp
shadow_bindings(objectp bindings, objectp shadow)
{
	objectp binding, name;

	for (; bindings != NULL && bindings->type == OBJ_CONS; bindings = cdr(bindings))
	{
		binding = car(bindings);
		if (binding->type != OBJ_CONS)
			continue;
		name = car(binding);
		if (name->type == OBJ_CONS)
			name = car(name);
		if (name->type == OBJ_IDENTIFIER)
			shadow = cons(name, shadow);
	}
	return shadow;
}

static objectp capture(objectp body, objectp shadow);

/* map capture over a proper list, sharing one shadow set */
static objectp
capture_list(objectp list, objectp shadow)
{
	if (list == NULL || list->type != OBJ_CONS)
		return list;
	return cons(capture(car(list), shadow), capture_list(cdr(list), shadow));
}

/* let bindings: keep each name verbatim, capture its value expression (outer
 * scope) */
static objectp
capture_bindings(objectp bindings, objectp shadow)
{
	objectp binding;
	if (bindings == NULL || bindings->type != OBJ_CONS)
		return bindings;
	binding = car(bindings);
	if (binding->type == OBJ_CONS)
		binding = cons(car(binding), capture_list(cdr(binding), shadow));
	return cons(binding, capture_bindings(cdr(bindings), shadow));
}

static objectp
capture_labels(objectp definitions, objectp shadow)
{
	objectp definition, header, parameters;

	if (definitions == NULL || definitions->type != OBJ_CONS)
		return definitions;
	definition = car(definitions);
	if (definition->type == OBJ_CONS)
	{
		header = car(definition);
		if (header->type == OBJ_CONS)
		{
			parameters = cdr(header);
			definition =
				cons(header, capture_list(cdr(definition),
										  shadow_parameters(parameters, shadow)));
		}
	}
	return cons(definition, capture_labels(cdr(definitions), shadow));
}

/* Substitute the values of environment-bound identifiers into a comprehension
   predicate, but only where they occur FREE: never inside (quote ...), and
   never where lambda, let, let*, or labels binds the name locally.  Names that
   resolve to builtins stay symbolic — they are operators, not captured data. */
static objectp
capture(objectp body, objectp shadow)
{
	objectp head, resolved;
	if (body == NULL)
		return body;
	if (body->type == OBJ_IDENTIFIER)
	{
		if (shadow_contains_name(body, shadow))
			return body;
		resolved = object_try_value(body);
		return (resolved != NULL && resolved->type != OBJ_BUILTIN) ? resolved : body;
	}
	if (body->type != OBJ_CONS)
		return body;
	head = car(body);
	if (head->type == OBJ_IDENTIFIER)
	{
		if (!strcmp(head->value.identifier.name, "quote"))
			return body;
		if (!strcmp(head->value.identifier.name, "lambda"))
			return cons(
				head,
				cons(cadr(body),
					 capture_list(cddr(body), shadow_parameters(cadr(body), shadow))));
		if (!strcmp(head->value.identifier.name, "let") ||
			!strcmp(head->value.identifier.name, "let*"))
			return cons(head, cons(capture_bindings(cadr(body), shadow),
								   capture_list(cddr(body),
												shadow_bindings(cadr(body), shadow))));
		if (!strcmp(head->value.identifier.name, "labels"))
			return cons(
				head,
				cons(capture_labels(cadr(body), shadow_bindings(cadr(body), shadow)),
					 capture_list(cddr(body), shadow_bindings(cadr(body), shadow))));
	}
	return capture_list(body, shadow);
}

objectp
eval_set(const struct object *set)
{
	objectp element, comprehension;
	struct list result;

	if (set == empty)
		return empty;
	if (object_is_comprehension_set(set))
	{
		comprehension = make_object(OBJ_SET);
		comprehension->value.cons.car = tau;
		comprehension->value.cons.cdr = substitute_tree(car(set), tau, cdr(set));
		comprehension->value.cons.cdr = capture(cdr(comprehension), nil);
		return comprehension;
	}

	list_init(&result, OBJ_SET);
	do
	{
		element = car(set);
		if (element->type == OBJ_RATIONAL)
			element = eval_rational(element);
		if (!collection_contains(element, result.head))
			list_append(&result, element);
	} while ((set = cdr(set)) != nil);

	return result.head;
}
