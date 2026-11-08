#ifndef SISP_RUNTIME_H
#define SISP_RUNTIME_H

#include <stdio.h>
#include <setjmp.h>
#include <stdint.h>
#include <stddef.h>

#include "sisp.h"

extern void *xmalloc(size_t);
extern void *xcalloc(size_t, size_t);
extern char *xstrdup(const char *);
extern uintptr_t stack_guard;

struct recovery_context
{
	jmp_buf jump;
	struct recovery_context *previous;
};

extern struct recovery_context *current_recovery;

/* Unwinds to the innermost frame; never returns. With no frame active --
   only possible before the first form loop starts -- there is nothing to
   unwind to, so it reports and exits rather than jumping through an
   uninitialised target. */
extern void runtime_recover(void);

/* The evaluator's guard: reaching it means the C stack is nearly gone, so it
   bypasses SISP catch handlers entirely -- a handler would run with no stack
   left to run in. Stack exhaustion is therefore not catchable. */
#define STACK_CHECK()                                                                  \
	do                                                                                 \
	{                                                                                  \
		char _p;                                                                       \
		if ((uintptr_t)&_p < stack_guard)                                              \
		{                                                                              \
			fprintf(stderr, "; STACK EXHAUSTED.\n");                                   \
			runtime_recover();                                                         \
		}                                                                              \
	} while (0)

/* The reader's guard is deliberately separate: a parse-time overflow belongs
   to the stream being read and unwinds to that reader, not to the evaluator. */
#define READER_STACK_CHECK(where)                                                      \
	do                                                                                 \
	{                                                                                  \
		char _p;                                                                       \
		if ((uintptr_t)&_p < stack_guard)                                              \
		{                                                                              \
			fprintf(stderr, "; STACK EXHAUSTED.\n");                                   \
			longjmp((where), 1);                                                       \
		}                                                                              \
	} while (0)
/* object.c */
extern objectp nil;
extern objectp t;
extern objectp tau;
extern objectp empty;
extern objectp id_quote, id_closure, id_lambda, id_macro;
extern objectp id_bquote, id_comma, id_comma_at, id_diff;
extern objectp id_in, id_and, id_or, id_not, id_xor;
extern objectp id_car, id_cdr, id_consp, id_subset, id_error;
extern void runtime_identifiers_init(void);
extern void validate_parameter_names(objectp, const char *);
extern int tail_call_pending;
extern void object_init(void);
extern objectp make_object(enum object_type);
extern void free_object(objectp);
extern objectp cons(objectp, objectp);
extern objectp make_set(objectp, objectp);
extern objectp make_quoted(objectp);
extern objectp make_integer(long int);
extern objectp make_rational(long int, long int);
extern objectp make_float(double);
extern objectp make_string(const char *, size_t);
extern objectp make_string_buffer(size_t);
/* intern_identifier is the ONLY identifier constructor: make_identifier and
   find_identifier are static to object.c so nothing can create a second
   object for a name the environment would then fail to match by pointer. */
extern objectp intern_identifier(const char *);
extern objectp make_matrix(long int, long int);
extern void list_init(struct list *, enum object_type);
extern void list_append(struct list *, objectp);

extern int in_load;
extern objectp current_env;
extern void object_bind(objectp, objectp);
extern void object_assign(objectp, objectp);
extern objectp object_find_local_binding(const struct object *);

extern objectp object_get_value(const struct object *);

extern void object_unbind(objectp);
extern objectp object_try_value(const struct object *);
extern objectp object_try_function(const struct object *);
/* One size for every error-message buffer. There were three (128, 256, 512)
   for the same concept, which meant one site silently truncated shorter than
   its neighbours. Messages are one line naming an operation and a fault; the
   culprit value is carried separately in the error object, not formatted in. */
#define ERROR_MESSAGE_MAX 256

extern objectp sisp_error(const char *);
extern objectp sisp_errorf(const char *, ...);

/* Pair access belongs to the common object runtime, not the evaluator.
   Inline functions preserve the Lisp idiom without evaluating arguments
   more than once. */
static inline objectp
car(const struct object *object)
{
	if (object->type == OBJ_CONS || object->type == OBJ_SET)
		return object->value.cons.car;
	if (object->type == OBJ_NIL)
		return nil;
	return sisp_error("CAR: UNDEFINED");
}

static inline objectp
cdr(const struct object *object)
{
	if (object->type == OBJ_CONS || object->type == OBJ_SET)
		return object->value.cons.cdr;
	if (object->type == OBJ_NIL)
		return nil;
	return sisp_error("CDR: UNDEFINED");
}

#define cddr(object) cdr(cdr(object))
#define cadr(object) car(cdr(object))
#define caar(object) car(car(object))
#define cdar(object) cdr(car(object))
#define caddr(object) car(cddr(object))

extern void *error_handlers_suspend(void);
extern void error_handlers_restore(void *);
extern objectp sisp_raise(objectp);
extern objectp make_error_value(const char *, const struct object *);
extern void require_condition(bool, const char *, const char *, const struct object *);
extern void require_type(const struct object *, enum object_type, const char *);
extern void require_numeric(const struct object *, const char *);
extern void require_boolean(const struct object *, const char *);
extern void error_state_reset(void);
extern void gc_collect(void);
/* process.c */
extern int force_interactive;
extern int process_input(const char *);
extern int process_file_stream(FILE *);
/* builtins.c */
extern void builtins_init(void);

#endif
