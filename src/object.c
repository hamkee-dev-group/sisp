#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sisp.h"
#include "runtime.h"
#include "equal.h"
#include "print.h"
#include "pool.h"

objectp nil;
objectp t;
objectp tau;
objectp empty;

/* Interned identifier singletons for names the runtime fabricates
   constantly (closure creation, quote wrapping, symbolic set algebra).
   Builtin names reuse the immortal identifier builtins_init made;
   the non-builtin ones are rooted explicitly in tag_whole_tree. */
objectp id_quote, id_closure, id_lambda, id_macro;
objectp id_bquote, id_comma, id_comma_at, id_diff;
objectp id_in, id_and, id_or, id_not, id_xor;
objectp id_car, id_cdr, id_consp, id_subset, id_error;
objectp current_env;

objectp
make_object(enum object_type type)
{
	objectp object = pool_allocate();

	object->type = type;
	object->gc = 0;
	object->next = object_pools[type].used;
	object_pools[type].used = object;
	object_pools[type].used_size++;
	if (type == OBJ_CONS || type == OBJ_SET)
		object->value.cons.car = object->value.cons.cdr = nil;
	if (type == OBJ_STRING)
	{
		object->value.string.length = 0;
		object->value.string.data = NULL;
	}
	if (type == OBJ_IDENTIFIER)
	{
		object->value.identifier.name = NULL;
		object->value.identifier.binding = NULL;
	}
	if (type == OBJ_MATRIX)
	{
		object->value.matrix.data = NULL;
		object->value.matrix.rows = 0;
		object->value.matrix.cols = 0;
	}

	return object;
}

void
free_object(objectp object)
{
	if (object->type == OBJ_IDENTIFIER)
	{
		free(object->value.identifier.name);
		object->value.identifier.name = NULL;
		object->value.identifier.binding = NULL;
	}
	else if (object->type == OBJ_STRING)
	{
		free(object->value.string.data);
		object->value.string.data = NULL;
		object->value.string.length = 0;
	}
	else if (object->type == OBJ_MATRIX)
	{
		free(object->value.matrix.data);
		object->value.matrix.data = NULL;
		object->value.matrix.rows = 0;
		object->value.matrix.cols = 0;
	}
}

objectp
cons(objectp car_value, objectp cdr_value)
{
	objectp object = make_object(OBJ_CONS);
	object->value.cons.car = car_value;
	object->value.cons.cdr = cdr_value;
	return object;
}

objectp
make_set(objectp car_value, objectp cdr_value)
{
	objectp object = make_object(OBJ_SET);
	object->value.cons.car = car_value;
	object->value.cons.cdr = cdr_value;
	return object;
}

objectp
make_quoted(objectp value)
{
	return cons(id_quote, cons(value, nil));
}

void
list_init(struct list *list, enum object_type type)
{
	list->head = nil;
	list->tail = NULL;
	list->type = type;
}

void
list_append(struct list *list, objectp value)
{
	objectp node;

	node = make_object(list->type);
	node->value.cons.car = value;
	node->value.cons.cdr = nil;
	if (list->tail == NULL)
		list->head = node;
	else
		list->tail->value.cons.cdr = node;
	list->tail = node;
}

objectp
make_integer(long int value)
{
	objectp object = make_object(OBJ_INTEGER);
	object->value.i = value;
	return object;
}

objectp
make_rational(long int numerator, long int denominator)
{
	objectp object;

	object = make_object(OBJ_RATIONAL);
	object->value.rational.n = numerator;
	object->value.rational.d = denominator;
	return object;
}

objectp
make_float(double value)
{
	objectp object = make_object(OBJ_FLOAT);
	object->value.f = value;
	return object;
}

objectp
make_string_buffer(size_t length)
{
	objectp object = make_object(OBJ_STRING);
	object->value.string.data = xmalloc(length + 1);
	object->value.string.data[length] = '\0';
	object->value.string.length = (long int)length;
	return object;
}

objectp
make_string(const char *data, size_t length)
{
	objectp object = make_string_buffer(length);
	memcpy(object->value.string.data, data, length);
	return object;
}

/* Only identifiers are interned: bindings and environment lookup depend on
   one object per name. intern_identifier is their sole constructor.
   Numbers and strings are allocated directly from the object pool. */
static objectp
find_identifier(const char *name)
{
	objectp object;

	for (object = object_pools[OBJ_IDENTIFIER].used; object != NULL;
		 object = object->next)
		if (object->value.identifier.name != NULL &&
			!strcmp(object->value.identifier.name, name))
			return object;

	return (objectp)NULL;
}

static objectp
make_identifier(const char *name)
{
	objectp object = make_object(OBJ_IDENTIFIER);
	object->value.identifier.name = xstrdup(name);
	return object;
}

objectp
intern_identifier(const char *name)
{
	objectp object = find_identifier(name);
	return object != NULL ? object : make_identifier(name);
}

void
object_init(void)
{
	int pool_type;

	/* Initialize every used list before creating tracked singletons. */
	for (pool_type = 0; pool_type < OBJ_TYPE_COUNT; pool_type++)
	{
		object_pools[pool_type].used = NULL;
		object_pools[pool_type].used_size = 0;
	}

	nil = make_object(OBJ_NIL);
	t = make_object(OBJ_T);
	tau = make_object(OBJ_TAU);
	current_env = nil;

	empty = make_set(
		tau, cons(intern_identifier("not"),
				  cons(cons(intern_identifier("="), cons(tau, cons(tau, nil))), nil)));
}

/* Called once from main after builtins_init, so builtin names hit the
   already-immortal identifiers and only lambda/closure/macro are new. */
void
runtime_identifiers_init(void)
{
	id_quote = intern_identifier("quote");
	id_closure = intern_identifier("closure");
	id_lambda = intern_identifier("lambda");
	id_macro = intern_identifier("macro");
	id_bquote = intern_identifier("bquote");
	id_comma = intern_identifier("comma");
	id_comma_at = intern_identifier("comma-at");
	id_diff = intern_identifier("diff");
	id_in = intern_identifier("in");
	id_and = intern_identifier("and");
	id_or = intern_identifier("or");
	id_not = intern_identifier("not");
	id_xor = intern_identifier("xor");
	id_car = intern_identifier("car");
	id_cdr = intern_identifier("cdr");
	id_consp = intern_identifier("consp");
	id_subset = intern_identifier("subset");
	id_error = intern_identifier("error");
}

void
object_unbind(objectp name)
{
	if (name->type == OBJ_IDENTIFIER && name->value.identifier.binding != NULL &&
		name->value.identifier.binding->type != OBJ_BUILTIN)
		name->value.identifier.binding = NULL;
}

void
object_bind(objectp name, objectp value)
{
	if (value == NULL)
		sisp_error("SET OBJECT: NULL VALUE");
	if (name->type != OBJ_IDENTIFIER)
		sisp_error("SET OBJECT: NOT IDENTIFIER NAME");
	if (name->value.identifier.binding != NULL &&
		name->value.identifier.binding->type == OBJ_BUILTIN)
	{
		sisp_errorf("SET OBJECT: `%s` IS A BUILTIN FUNCTION",
					name->value.identifier.name);
	}

	// circular definition set by comprehension
	if (value->type == OBJ_SET && value->value.cons.cdr->type != OBJ_SET &&
		value->value.cons.cdr != nil)
	{
		if (collection_contains(name, value->value.cons.cdr))
		{
			sisp_error("SET OBJECT: VALUE CANNOT CONTAIN NAME");
		}
	}
	name->value.identifier.binding = value;
}

void
object_assign(objectp name, objectp value)
{
	objectp binding;

	if (name->type != OBJ_IDENTIFIER)
		sisp_error("SETQ: NOT IDENTIFIER NAME");
	if (value == NULL)
		sisp_error("SETQ: NULL VALUE");

	binding = object_find_local_binding(name);
	if (binding != NULL)
	{
		binding->value.cons.cdr = value;
		return;
	}

	object_bind(name, value);
}

objectp
object_find_local_binding(const struct object *name)
{
	objectp environment_node, binding, binding_name;

	if (name == NULL || name->type != OBJ_IDENTIFIER)
		return NULL;
	/* One string compare per frame, per lookup, was the single hottest string
	   operation in the interpreter -- and a pointless one: identifiers are
	   interned, so equal names are already the same object. Comparing the
	   pointer decides it in one instruction, and subsumes the type test (only
	   an identifier can equal an identifier). */
	for (environment_node = current_env; environment_node != nil;
		 environment_node = cdr(environment_node))
	{
		binding = car(environment_node);
		binding_name = car(binding);
		if (binding_name == name)
			return binding;
	}
	return NULL;
}

objectp
object_try_function(const struct object *name)
{
	if (name == NULL)
		return NULL;
	if (name->type == OBJ_IDENTIFIER && name->value.identifier.binding != NULL)
		return name->value.identifier.binding;
	return object_try_value(name);
}

objectp
object_try_value(const struct object *name)
{
	objectp binding;

	if (name == NULL)
		return NULL;
	binding = object_find_local_binding(name);
	if (binding != NULL)
		return cdr(binding);
	/* builtins are first-class values: (define f car) works */
	if (name->type == OBJ_IDENTIFIER && name->value.identifier.binding != NULL)
		return name->value.identifier.binding;
	return NULL;
}

objectp
object_get_value(const struct object *name)
{
	objectp binding;

	if (name == NULL)
		sisp_error("GET OBJECT: NULL NAME");
	binding = object_find_local_binding(name);
	if (binding != NULL)
		return cdr(binding);
	/* builtins are first-class values: (map car xs) works */
	if (name->type == OBJ_IDENTIFIER && name->value.identifier.binding != NULL)
		return name->value.identifier.binding;

	sisp_errorf("OBJECT '%s' NOT FOUND", name->value.identifier.name);
	return NULL;
}
