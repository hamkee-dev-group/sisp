#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "sisp.h"
#include "doc.h"
#include "eval.h"
#include "builtins.h"
#include "runtime.h"
#include "equal.h"
#include "numeric.h"
#include "print.h"
int tail_call_pending = 0;
#ifdef SISP_DEBUG
bool trace_enabled = false;
#endif
static unsigned long require_list_length(objectp list, const char *operation,
										 const char *message);

static int
compare_builtin_names(const void *first, const void *second)
{
	return strcmp(((const struct builtin_spec *)first)->name,
				  ((const struct builtin_spec *)second)->name);
}

/* The error model, kept small and made of data.
   An error is an ordinary list (error MESSAGE CULPRIT): the tag identifier,
   a message string, and the offending value (nil when there is none).
   sisp_raise hands that value to the nearest enclosing catch, or, if there is
   none, prints it and unwinds to the top level. Because collection only happens
   between top-level forms, a sisp_raise->catch jump never leaks or frees a live
   object, so no unwinding of allocations is needed. */

struct handler
{
	jmp_buf buf;
	objectp saved_environment;
	struct handler *previous;
};
static struct handler *handler_stack = NULL;
static objectp raised_error = NULL;

/* (error "message" culprit) from a C string and an object */
objectp
make_error_value(const char *msg, const struct object *culprit)
{
	objectp message = make_string(msg, strlen(msg));
	objectp value;

	/* Error values retain the ordinary mutable SISP object.  const only
	   promises that error construction itself does not modify the culprit. */
	value = culprit ? (objectp)culprit : nil;
	return cons(id_error, cons(message, cons(value, nil)));
}

void
require_condition(bool condition, const char *operation, const char *message,
				  const struct object *culprit)
{
	char buffer[ERROR_MESSAGE_MAX];

	if (!condition)
	{
		snprintf(buffer, sizeof buffer, "%s: %s", operation, message);
		sisp_raise(make_error_value(buffer, culprit));
	}
}

static const char *
type_error_message(enum object_type type)
{
	switch (type)
	{
	case OBJ_INTEGER:
		return "NOT INTEGER";
	case OBJ_STRING:
		return "NOT STRING";
	case OBJ_IDENTIFIER:
		return "NOT IDENTIFIER";
	case OBJ_CONS:
		return "NOT CONS";
	case OBJ_SET:
		return "NOT SET";
	case OBJ_MATRIX:
		return "NOT MATRIX";
	case OBJ_BUILTIN:
		return "NOT BUILTIN";
	default:
		return "WRONG TYPE";
	}
}

void
require_type(const struct object *value, enum object_type type, const char *operation)
{
	require_condition(value->type == type, operation, type_error_message(type), value);
}

void
require_numeric(const struct object *value, const char *operation)
{
	require_condition(object_is_numeric(value), operation, "NOT NUMERIC", value);
}

void
require_boolean(const struct object *value, const char *operation)
{
	require_condition(object_is_boolean(value), operation, "NOT BOOLEAN", value);
}

static void
report_error(objectp error)
{
	objectp message = cadr(error);
	objectp culprit = caddr(error);

	fputs("; ", stderr);
	if (message->type == OBJ_STRING)
		fputs(message->value.string.data, stderr);
	else
		print_object(stderr, message);
	if (culprit != nil)
	{
		fputs(": ", stderr);
		print_object(stderr, culprit);
	}
	fputc('\n', stderr);
}

objectp
sisp_raise(objectp error)
{
	raised_error = error;
	if (handler_stack != NULL)
		longjmp(handler_stack->buf, 1);
	report_error(error);
	runtime_recover();
	return NULL;
}

/* All the old fixed-message errors keep working and become catchable for free.
 */
objectp
sisp_error(const char *message)
{
	return sisp_raise(make_error_value(message, nil));
}

/* Same, for a printf-style message — so error sites that report a name or a
   count keep their exact text while becoming catchable. */
objectp
sisp_errorf(const char *fmt, ...)
{
	char buf[ERROR_MESSAGE_MAX];
	va_list ap;
	va_start(ap, fmt);
	vsnprintf(buf, sizeof(buf), fmt, ap);
	va_end(ap);
	return sisp_error(buf);
}

void
error_state_reset(void)
{
	handler_stack = NULL;
	raised_error = nil;
}

/* A nested load must not leave the caller's catch current. If it did, an
   error in the loaded file would jump straight to that handler, skipping the
   nested form loop, in_load--, and the caller's own cleanup -- which is
   exactly why an error in a loaded file's FIRST form used to surface as the
   raw error while the same error one form later surfaced as LOAD FAILED. */
void *
error_handlers_suspend(void)
{
	void *saved = handler_stack;
	handler_stack = NULL;
	return saved;
}
void
error_handlers_restore(void *saved_handlers)
{
	handler_stack = saved_handlers;
}

/* (error MESSAGE [CULPRIT]) — sisp_raise an error carrying a value. */
DOC("error", "Raise an error with a message and an optional culprit value.\n"
			 "  (catch (error \"boom\") (lambda (e) (cadr e))) => \"boom\"\n"
			 "Note: An error is an ordinary value: (error message culprit), so "
			 "cadr is the message and caddr the culprit.\n"
			 "See: catch")
objectp
builtin_error(const struct object *args)
{
	objectp message = eval(car(args));
	objectp culprit = (cdr(args) != nil) ? eval(cadr(args)) : nil;
	return sisp_raise(cons(id_error, cons(message, cons(culprit, nil))));
}

/* (catch BODY HANDLER) — evaluate BODY; if it raises, call the one-argument
   HANDLER on the error value and return that; otherwise return BODY's value. */
DOC("catch", "Evaluate a body, handing any raised error to a handler.\n"
			 "  (catch (car 0) (lambda (e) (quote caught))) => caught\n"
			 "  (catch 1 (lambda (e) 2)) => 1\n"
			 "Note: Stack exhaustion is NOT catchable: the guard unwinds past every "
			 "handler, because a handler cannot run on an exhausted stack.\n"
			 "See: error")
objectp
builtin_catch(const struct object *args)
{
	struct handler frame;
	objectp value;

	/* the body-and-handler count is the table's 2,2 entry */
	frame.saved_environment = current_env;
	frame.previous = handler_stack;
	handler_stack = &frame;

	if (setjmp(frame.buf) == 0)
	{
		value = eval(car(args));
		handler_stack = frame.previous;
		return value;
	}
	handler_stack = frame.previous;
	current_env = frame.saved_environment;
	return eval(cons(cadr(args), cons(make_quoted(raised_error), nil)));
}

/* Binding names must be identifiers. The constants t/nil/tau parse to
   their singleton objects, so a parameter named t silently created an
   unusable binding; now every binding site rejects it loudly.

   The list itself must also be proper. A dotted or non-list parameter list
   was never callable — every call raised a misleading "EXPECTED n ARGUMENTS"
   or "CDR: UNDEFINED" from deep inside the call machinery — so it is
   rejected here, at the definition, where the culprit is still visible. */
void
validate_parameter_names(objectp parameters, const char *operation)
{
	objectp parameter_node, previous_node;
	char message[ERROR_MESSAGE_MAX];

	if (parameters != nil && parameters->type != OBJ_CONS)
	{
		snprintf(message, sizeof message, "%s: NOT A PARAMETER LIST", operation);
		sisp_raise(make_error_value(message, parameters));
	}

	for (parameter_node = parameters; parameter_node != nil;
		 parameter_node = cdr(parameter_node))
	{
		if (parameter_node->type != OBJ_CONS)
		{
			snprintf(message, sizeof message, "%s: IMPROPER PARAMETER LIST", operation);
			sisp_raise(make_error_value(message, parameters));
		}
		if (car(parameter_node)->type != OBJ_IDENTIFIER)
		{
			snprintf(message, sizeof message, "%s: CANNOT BE A PARAMETER NAME",
					 operation);
			sisp_raise(make_error_value(message, car(parameter_node)));
		}
		for (previous_node = parameters; previous_node != parameter_node;
			 previous_node = cdr(previous_node))
			if (!strcmp(car(previous_node)->value.identifier.name,
						car(parameter_node)->value.identifier.name))
			{
				snprintf(message, sizeof message, "%s: DUPLICATE PARAMETER", operation);
				sisp_raise(make_error_value(message, car(parameter_node)));
			}
	}
}

/* The single door to every builtin.  Builtins read their arguments with
   car/cadr/caddr, so without a count a missing argument silently reads as nil
   and a surplus one is silently dropped — which is how (cat "a" "b" "c") lost
   its third string and (cap a b c) its third set.  Checking here fixes the
   whole class at once, for named calls and for builtins passed as values. */
objectp
invoke_builtin(const struct builtin_spec *spec, const struct object *args)
{
	objectp result;
	unsigned long count;

	/* Count once and reject a dotted tail while doing it, so an improper call
	   cannot silently discard its tail or masquerade as a missing argument. */
	count = require_list_length((objectp)args, spec->name, "IMPROPER ARGUMENT LIST");

	if (spec->min_args > 0 && count < (unsigned long)spec->min_args)
		sisp_errorf("%s: EXPECTED AT LEAST %d ARGUMENT%s", spec->name, spec->min_args,
					spec->min_args == 1 ? "" : "S");
	if (spec->max_args != ARITY_ANY && count > (unsigned long)spec->max_args)
		sisp_errorf("%s: EXPECTED AT MOST %d ARGUMENT%s", spec->name, spec->max_args,
					spec->max_args == 1 ? "" : "S");
	result = spec->function(args);
	if (result == NULL)
		sisp_errorf("%s: RETURNED NO VALUE", spec->name);
	return result;
}

objectp
eval_arguments(const struct object *arguments)
{
	struct list result;
	list_init(&result, OBJ_CONS);
	for (; arguments != nil; arguments = cdr(arguments))
		list_append(&result, eval(car(arguments)));
	return result.head;
}

objectp
eval_body_prefix(objectp body)
{
	while (cdr(body) != nil)
	{
		eval(car(body));
		body = cdr(body);
	}
	return body;
}

objectp
eval_float(const struct object *number)
{
	return make_number_from_double(number->value.f);
}
objectp
eval_rational(const struct object *number)
{
	objectp result;
	long int n, d, g;
	short sign = 0;

	n = number->value.rational.n;
	d = number->value.rational.d;
	require_condition((d != 0L), "EVAL RAT", "DIVISION BY ZERO", number);
	if (n < 0)
	{
		sign = 1;
		n = checked_sub(0L, n);
	}
	if (d < 0)
	{
		sign += 1;
		d = checked_sub(0L, d);
	}
	g = gcd(n, d);

	if (d / g == 1L)
	{
		result = make_integer(n / g);
		if (sign == 1)
			result->value.i = -result->value.i;
	}
	else
	{
		result = make_rational(n / g, d / g);
		if (sign == 1)
			result->value.rational.n = -result->value.rational.n;
	}

	return result;
}

enum callable_kind
{
	CALLABLE_NONE,
	CALLABLE_LAMBDA,
	CALLABLE_CLOSURE,
	CALLABLE_MACRO
};

/* Up to three string compares ran on every call to classify the callable.
   The three tags are interned singletons -- the reader hands back the very
   same object for a `lambda` written in source, and the runtime builds
   closures out of these -- so the pointer settles it. Matching one of them
   also proves the tag is an identifier, so that test goes too. */
static enum callable_kind
callable_kind(objectp function)
{
	objectp tag;

	if (function == NULL || function->type != OBJ_CONS)
		return CALLABLE_NONE;
	tag = car(function);
	if (tag == id_lambda)
		return CALLABLE_LAMBDA;
	if (tag == id_closure)
		return CALLABLE_CLOSURE;
	if (tag == id_macro)
		return CALLABLE_MACRO;
	return CALLABLE_NONE;
}

static int
object_is_closure(objectp function)
{
	return callable_kind(function) == CALLABLE_CLOSURE;
}

int
object_is_macro(objectp function)
{
	return callable_kind(function) == CALLABLE_MACRO;
}

static int
object_is_user_function(objectp function)
{
	return callable_kind(function) != CALLABLE_NONE;
}

static objectp
callable_parameters(objectp function)
{
	return object_is_closure(function) ? car(cddr(function)) : cadr(function);
}

/* A macro call binds its params to UNEVALUATED arguments, evaluates the body
   in that environment, and yields the expansion. */
objectp
expand_macro(objectp macro, objectp args, int tail_position)
{
	objectp parameters, body, saved_environment, environment;
	objectp parameter_node, binding, environment_node, result;

	parameters = cadr(macro);
	body = cddr(macro);
	saved_environment = current_env;

	if (parameters != nil)
	{
		environment = nil;
		parameter_node = parameters;
		do
		{
			binding = cons(car(parameter_node), car(args));
			environment_node = cons(binding, environment);
			environment = environment_node;
			args = cdr(args);
		} while ((parameter_node = cdr(parameter_node)) != nil);
		current_env = environment;
	}

	body = eval_body_prefix(body);
	result = eval(car(body));
	current_env = saved_environment;
	if (tail_position)
		tail_call_pending = 1;
	return result;
}

/* A callable is either a bare (lambda PARAMS . BODY) or a closure that also
   carries its captured environment: (closure ENV PARAMS . BODY). Split out
   the three parts so both evaluators read the same way. */
static void
unpack_callable(objectp callable, objectp *captured_environment, objectp *parameters,
				objectp *body)
{
	if (object_is_closure(callable))
	{
		*captured_environment = cadr(callable);
		*parameters = car(cddr(callable));
		*body = cdr(cddr(callable));
	}
	else
	{
		*captured_environment = nil;
		*parameters = cadr(callable);
		*body = cddr(callable);
	}
}

static objectp
eval_function(objectp callable, objectp args)
{
	objectp parameters, body, captured_environment, environment;
	objectp parameter_node, environment_node, binding;
	objectp evaluated_node;
	struct list evaluated;

	unpack_callable(callable, &captured_environment, &parameters, &body);

	if (parameters == nil)
	{
		current_env = captured_environment;
		body = eval_body_prefix(body);
		tail_call_pending = 1;
		return car(body);
	}

	/* Evaluate all arguments in the caller's environment. */
	list_init(&evaluated, OBJ_CONS);
	parameter_node = parameters;
	do
	{
		list_append(&evaluated, eval(car(args)));
		args = cdr(args);
	} while ((parameter_node = cdr(parameter_node)) != nil);

	/* Build new environment: param bindings prepended to captured env. */
	environment = captured_environment;
	parameter_node = parameters;
	evaluated_node = evaluated.head;
	do
	{
		binding = cons(car(parameter_node), car(evaluated_node));
		environment_node = cons(binding, environment);
		environment = environment_node;
		evaluated_node = cdr(evaluated_node);
	} while ((parameter_node = cdr(parameter_node)) != nil);

	/* Evaluate body in the new environment. */
	current_env = environment;
	body = eval_body_prefix(body);
	tail_call_pending = 1;
	return car(body);
}

static objectp expand_backquote(objectp form, int depth);

/* Rebuild a template LIST, splicing ,@ and unquoting , only at depth 1. */
static objectp
expand_backquote_list(objectp list, int depth)
{
	objectp head = nil, tail = NULL, node, element, splice_node, dotted_tail;

	/* stop the spine walk at a dotted comma tail, e.g. `(a . ,b) */
	while (list->type == OBJ_CONS && car(list) != id_comma &&
		   car(list) != id_comma_at)
	{
		element = car(list);
		if (element->type == OBJ_CONS && car(element) == id_comma_at)
		{
			if (depth == 1)
			{
				for (splice_node = eval(cadr(element)); splice_node != nil;
					 splice_node = cdr(splice_node))
				{
					require_condition((splice_node->type == OBJ_CONS), "SPLICE",
									  "NOT LIST", splice_node);
					node = cons(car(splice_node), nil);
					tail ? (tail->value.cons.cdr = node) : (head = node);
					tail = node;
				}
				list = cdr(list);
				continue;
			}
			node = cons(cons(id_comma_at,
							 cons(expand_backquote(cadr(element), depth - 1), nil)),
						nil);
		}
		else
			node = cons(expand_backquote(element, depth), nil);
		tail ? (tail->value.cons.cdr = node) : (head = node);
		tail = node;
		list = cdr(list);
	}
	if (list != nil) /* dotted tail (atom or a comma form) */
	{
		dotted_tail = expand_backquote(list, depth);
		tail ? (tail->value.cons.cdr = dotted_tail) : (head = dotted_tail);
	}
	return head;
}

/* Quasiquote with proper nesting: a comma fires only when it brings the depth
   to 0; each nested backquote raises the depth, each comma lowers it. */
static objectp
expand_backquote(objectp form, int depth)
{
	STACK_CHECK();
	if (form->type != OBJ_CONS)
		return form;
	if (car(form) == id_comma)
		return depth == 1
				   ? eval(cadr(form))
				   : cons(id_comma, cons(expand_backquote(cadr(form), depth - 1), nil));
	if (car(form) == id_comma_at)
		return depth == 1 ? eval(cadr(form))
						  : cons(id_comma_at,
								 cons(expand_backquote(cadr(form), depth - 1), nil));
	if (car(form) == id_bquote)
		return cons(id_bquote, cons(expand_backquote(cadr(form), depth + 1), nil));
	return expand_backquote_list(form, depth);
}

objectp
eval_bquote(objectp template)
{
	return expand_backquote(template, 1);
}

/* One arity gate: a user function must get exactly as many args as it has
   parameters. Used by both apply_function and check_arity. */
static void
require_arity(objectp parameters, objectp args, const char *name)
{
	unsigned long expected;

	expected = require_list_length(parameters, name, "IMPROPER PARAMETER LIST");
	if (require_list_length(args, name, "IMPROPER ARGUMENT LIST") != expected)
		sisp_errorf("%s: EXPECTED %lu ARGUMENTS", name, expected);
}

objectp
apply_function(objectp function, objectp args, const char *name)
{
	/* builtins are first-class: they evaluate their own arguments,
	   which is exactly what the unevaluated forms handed here expect */
	if (function->type == OBJ_BUILTIN)
		return invoke_builtin(function->value.builtin, args);

	require_condition((callable_kind(function) == CALLABLE_LAMBDA ||
					   callable_kind(function) == CALLABLE_CLOSURE),
					  "EVAL", "NOT A FUNCTION", function);

	require_arity(callable_parameters(function), args, name);
	return eval_function(function, args);
}

/* Function-position lookup in the lexical environment only. Returns the
   bound value when the identifier is lexically bound to a callable
   (lambda/closure/macro), else NULL. Lets local bindings shadow global
   user functions; builtins keep absolute precedence. */
static objectp
find_environment_function(const struct object *name)
{
	objectp binding, function;

	binding = object_find_local_binding(name);
	if (binding != NULL)
	{
		function = cdr(binding);
		if (object_is_user_function(function) || function->type == OBJ_BUILTIN)
			return function;
	}
	return NULL;
}

/* Invoke a user-defined callable: check arity, then expand it if it is a
   macro, else evaluate its body strictly or lazily. */
static objectp
call_user_function(objectp function, objectp args, const char *name)
{
	require_condition((callable_kind(function) != CALLABLE_NONE), "EVAL",
					  "NOT A FUNCTION", function);
	require_arity(callable_parameters(function), args, name);
	if (object_is_macro(function))
		return expand_macro(function, args, 1);
	return eval_function(function, args);
}

/* Builtin names are reserved in operator position, even with a local value
   binding of the same name. User functions and prelude functions can be
   shadowed normally. */
static objectp
eval_named_call(const struct object *form)
{
	objectp name, function, resolved_function;
	/* Zero-initialised, not just `.name`: compare_builtin_names reads only the
	   name today, but a partially-filled key leaves any future comparator --
	   and any analyser -- looking at garbage in the other three fields. */
	struct builtin_spec key = {0}, *builtin;

	name = car(form);
	function = name->value.identifier.binding;
	if (function == NULL || function->type != OBJ_BUILTIN)
	{
		resolved_function = find_environment_function(name);
		if (resolved_function != NULL)
			function = resolved_function;
	}
	if (function != NULL && function->type == OBJ_BUILTIN)
		return invoke_builtin(function->value.builtin, cdr(form));
	if (function != NULL && object_is_user_function(function))
		return call_user_function(function, cdr(form), name->value.identifier.name);

	key.name = name->value.identifier.name;
	builtin = bsearch(&key, builtins, builtin_count, sizeof(builtins[0]),
					  compare_builtin_names);
	if (builtin != NULL)
		return invoke_builtin(builtin, cdr(form));

	function = object_get_value(name);
	if (function->type == OBJ_BUILTIN)
		return invoke_builtin(function->value.builtin, cdr(form));
	return call_user_function(function, cdr(form), name->value.identifier.name);
}

objectp
eval_cons(const struct object *form)
{
	objectp operator, result, caller_environment;

	STACK_CHECK();

	caller_environment = current_env;

	for (;;)
	{
#ifdef SISP_DEBUG
		if (trace_enabled)
		{
			fputs("; eval ", stderr);
			print_object(stderr, form);
			fputc('\n', stderr);
		}
#endif
		operator= car(form);
		if (operator->type == OBJ_CONS)
			result = apply_function(eval(operator), cdr(form), "LAMBDA");
		else if (operator->type == OBJ_BUILTIN)
			result = invoke_builtin(operator->value.builtin, cdr(form));
		else
		{
			require_condition((operator->type == OBJ_IDENTIFIER), "EVAL",
							  "NOT A FUNCTION", operator);
			if (operator== id_lambda)
			{
				validate_parameter_names(cadr(form), "LAMBDA");
				result = current_env == nil
							 ? (objectp)form
							 : cons(id_closure, cons(current_env, cdr(form)));
			}
			else
				result = eval_named_call(form);
		}

		if (!tail_call_pending)
		{
			current_env = caller_environment;
			return result;
		}
		tail_call_pending = 0;
		if (result->type != OBJ_CONS)
		{
			result = eval(result);
			current_env = caller_environment;
			return result;
		}
		form = result;
	}
}

/* Proper-list length; the arity currency of calls and parameter binding. */
static unsigned long
require_list_length(objectp list, const char *operation, const char *message)
{
	unsigned long length;

	length = 0;
	while (list != nil)
	{
		require_condition((list->type == OBJ_CONS), operation, message, list);
		list = cdr(list);
		length++;
	}
	return length;
}
