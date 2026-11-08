#ifndef SISP_H
#define SISP_H

#include <stdbool.h>

/* Lexer token codes (what the reader emits), distinct from the object
   type tags below: the parser turns a TOK_ into the matching OBJ_. Single
   characters like '(' and '{' travel as their own ASCII value, so these
   named codes start past that range. */
#define TOK_IDENTIFIER 3
#define TOK_INTEGER 5
#define TOK_RATIONAL 6
#define TOK_STRING 7
#define TOK_FLOAT 9
#define TOK_COMMA_AT 10

#define OBJ_SIZE sizeof(struct object)

enum object_type
{
	OBJ_NIL,
	OBJ_T,
	OBJ_IDENTIFIER,
	OBJ_CONS,
	OBJ_INTEGER,
	OBJ_RATIONAL,
	OBJ_STRING,
	OBJ_SET,
	OBJ_TAU,
	OBJ_BUILTIN,
	OBJ_FLOAT,
	OBJ_MATRIX,
	OBJ_TYPE_COUNT
};

typedef struct object *objectp;

struct builtin_spec;

/* One uniform object for every SISP value: a type tag, a GC mark epoch,
   a payload union (only the arm named by type is live), and the next pointer
   that threads it onto a per-type used list or the shared free list. Fixed
   size, so a reclaimed object can be reused for any type. */
struct object
{
	enum object_type type;
	unsigned int gc;
	union
	{
		struct
		{
			char *name;
			objectp binding;
		} identifier;
		/* the spec, not a bare function pointer, so a builtin reached as a
		   value — (map car lst) — is arity-checked like a named call */
		const struct builtin_spec *builtin;
		long int i;
		double f;
		struct
		{
			char *data;
			long int length;
		} string;
		struct
		{
			long int n;
			long int d;
		} rational;
		struct
		{
			objectp car;
			objectp cdr;
		} cons;
		struct
		{
			double *data; /* contiguous row-major doubles, malloc'd */
			int rows;
			int cols;
		} matrix;
	} value;
	objectp next;
};

struct list
{
	objectp head;
	objectp tail;
	enum object_type type;
};

/* Builtin names, handlers, and accepted argument counts.
   invoke_builtin checks arity; ARITY_ANY permits an unbounded maximum. */
#define ARITY_ANY (-1)

struct builtin_spec
{
	char *name;
	objectp (*function)(const struct object *);
	int min_args;
	int max_args;
};

#endif
