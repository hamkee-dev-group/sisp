#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <errno.h>

#include "sisp.h"
#include "runtime.h"
#include "reader.h"
#include "numeric.h"

static objectp parse_token(struct reader *, int);

static objectp
parse_required(struct reader *reader)
{
	objectp object = parser_parse_object(reader);
	if (object == NULL)
		reader_error(reader, "UNEXPECTED EOF");
	return object;
}

static objectp
parse_form(struct reader *reader)
{
	struct list result;
	int token;

	list_init(&result, OBJ_CONS);

	while ((token = lexer_next_token(reader)) != ')' && token != EOF)
	{
		if (token == '.')
		{
			token = lexer_next_token(reader);
			if (result.tail == NULL)
				reader_error(reader, "DOT WITHOUT A PRECEDING LIST ELEMENT");
			/* reader_error is _Noreturn, in the header and the definition
			   both, so tail cannot be NULL below. cppcheck does not honour
			   _Noreturn for this check; gcc -fanalyzer agrees the site is
			   safe. */
			/* cppcheck-suppress nullPointerRedundantCheck */
			result.tail->value.cons.cdr = parse_token(reader, token);
			token = lexer_next_token(reader);
			if (token != ')')
			{
				reader_error(reader, token == EOF ? "UNTERMINATED LIST AT EOF"
												  : "EXPECTED ')' AFTER DOTTED TAIL");
			}
			break;
		}
		list_append(&result, parse_token(reader, token));
	}
	if (token == EOF)
		reader_error(reader, "UNTERMINATED LIST AT EOF");
	// this is for '()
	return result.head;
}

static objectp
parse_set(struct reader *reader)
{
	struct list result;
	int token;

	list_init(&result, OBJ_SET);

	while ((token = lexer_next_token(reader)) != '}' && token != EOF)
	{
		if (token == ':')
		{
			token = lexer_next_token(reader);
			if (result.tail == NULL)
				reader_error(reader, "':' WITHOUT A PRECEDING SET ELEMENT");
			/* reader_error is _Noreturn, in the header and the definition
			   both, so tail cannot be NULL below. cppcheck does not honour
			   _Noreturn for this check; gcc -fanalyzer agrees the site is
			   safe. */
			/* cppcheck-suppress nullPointerRedundantCheck */
			result.tail->value.cons.cdr = parse_token(reader, token);
			token = lexer_next_token(reader);
			if (token != '}')
			{
				reader_error(reader, token == EOF ? "UNTERMINATED SET AT EOF"
												  : "EXPECTED '}' AFTER SET PREDICATE");
			}
			break;
		}
		list_append(&result, parse_token(reader, token));
	}
	if (token == EOF)
		reader_error(reader, "UNTERMINATED SET AT EOF");
	// this is for '()
	return result.head == nil ? empty : result.head;
}

static int
object_is_numeric_literal(objectp o)
{
	return o->type == OBJ_INTEGER || o->type == OBJ_RATIONAL || o->type == OBJ_FLOAT;
}

static double
numeric_literal_to_double(objectp o)
{
	switch (o->type)
	{
	case OBJ_INTEGER:
		return (double)o->value.i;
	case OBJ_RATIONAL:
		return (double)o->value.rational.n / (double)o->value.rational.d;
	case OBJ_FLOAT:
		return o->value.f;
	default:
		return 0.0;
	}
}

// [ ] matrix literal. Numbers only (like { } sets, it is literal, not
// evaluated):
//   [1 2 3]       -> a 1xN row matrix
//   [[1 2][3 4]]  -> an MxN matrix, one row per nested [ ]
// Ragged rows, or a cell that is neither a number nor a row, are parse errors.
static objectp
parse_matrix(struct reader *reader)
{
	struct list cells;
	objectp node, matrix, row;
	int element_count = 0, index, columns;
	int token;

	list_init(&cells, OBJ_CONS);

	while ((token = lexer_next_token(reader)) != ']' && token != EOF)
	{
		list_append(&cells, parse_token(reader, token));
		element_count++;
	}
	if (token == EOF)
		reader_error(reader, "UNTERMINATED MATRIX AT EOF");
	if (element_count == 0)
		reader_error(reader, "EMPTY MATRIX");

	if (object_is_numeric_literal(car(cells.head)))
	{
		matrix = make_matrix(1, element_count);
		for (node = cells.head, index = 0; node != nil;
			 node = node->value.cons.cdr, index++)
		{
			if (!object_is_numeric_literal(node->value.cons.car))
			{
				free(matrix->value.matrix.data);
				matrix->value.matrix.data = NULL;
				reader_error(reader, "MATRIX ROW MIXES NUMBERS AND ROWS");
			}
			matrix->value.matrix.data[index] =
				numeric_literal_to_double(node->value.cons.car);
		}
		return matrix;
	}
	if (car(cells.head)->type == OBJ_MATRIX)
	{
		columns = car(cells.head)->value.matrix.cols;
		matrix = make_matrix(element_count, columns);
		for (node = cells.head, index = 0; node != nil;
			 node = node->value.cons.cdr, index++)
		{
			row = node->value.cons.car;
			if (row->type != OBJ_MATRIX || row->value.matrix.rows != 1 ||
				row->value.matrix.cols != columns)
			{
				free(matrix->value.matrix.data);
				matrix->value.matrix.data = NULL;
				reader_error(reader, "RAGGED OR MALFORMED MATRIX ROWS");
			}
			memcpy(matrix->value.matrix.data + (size_t)index * (size_t)columns,
				   row->value.matrix.data, (size_t)columns * sizeof(double));
		}
		return matrix;
	}
	reader_error(reader, "MATRIX CELL MUST BE A NUMBER OR A ROW");
	return nil;
}

static objectp
parse_token(struct reader *reader, int token)
{
	objectp object = (objectp)NULL;
	long int integer, numerator, denominator;
	long int divisor;
	double floating;
	char *delimiter;

	READER_STACK_CHECK(reader->error_jmp);

	switch (token)
	{
	case EOF:
		return (objectp)NULL;
	case '`':
		reader->bquote_depth++;
		object = cons(id_bquote, cons(parse_required(reader), nil));
		reader->bquote_depth--;
		break;
	case ',':
		if (reader->bquote_depth <= 0)
			reader_error(reader, "COMMA OUTSIDE BACKQUOTE");
		object = cons(id_comma, cons(parse_required(reader), nil));
		break;
	case TOK_COMMA_AT:
		if (reader->bquote_depth <= 0)
			reader_error(reader, "COMMA-AT OUTSIDE BACKQUOTE");
		object = cons(id_comma_at, cons(parse_required(reader), nil));
		break;
	case '\'':
		object = make_quoted(parse_required(reader));
		break;
	case '(':
		object = parse_form(reader);
		break;
	case '{':
		object = parse_set(reader);
		break;
	case '[':
		object = parse_matrix(reader);
		break;
	case TOK_IDENTIFIER:
		if (!strcmp(reader->token_buffer, "t"))
			object = t;
		else if (!strcmp(reader->token_buffer, "nil"))
			object = nil;
		else if (!strcmp(reader->token_buffer, "tau"))
			object = tau;
		else
			object = intern_identifier(reader->token_buffer);
		break;
	case TOK_STRING:
		object = make_string(reader->token_buffer, strlen(reader->token_buffer));
		break;
	case TOK_INTEGER:
		errno = 0;
		integer = strtol(reader->token_buffer, NULL, 10);
		if (errno == ERANGE)
			reader_error(reader, "INTEGER OVERFLOW");
		object = make_integer(integer);
		break;

	case TOK_RATIONAL:
		errno = 0;
		numerator = strtol(reader->token_buffer, &delimiter, 10);
		delimiter++;
		denominator = strtol(delimiter, NULL, 10);
		if (errno == ERANGE)
			reader_error(reader, "RATIONAL OVERFLOW");
		if (denominator == 0)
			reader_error(reader, "RATIONAL DIVISION BY ZERO");
		/* Canonicalize at read time — reduce, put the sign on the numerator,
		   and demote d==1 to an integer — so a quoted rational is stored the
		   same as an evaluated one and 1/2 equals 2/4. */
		/* gcd is non-negative and non-zero here (a zero denominator was
		   already rejected), so the division needs no sign or overflow
		   guard and it leaves the sign where it was. */
		divisor = gcd(numerator, denominator);
		numerator /= divisor;
		denominator /= divisor;
		if (denominator < 0 && denominator != LONG_MIN && numerator != LONG_MIN)
		{
			numerator = -numerator;
			denominator = -denominator;
		}
		if (denominator == 1)
			object = make_integer(numerator);
		else
			object = make_rational(numerator, denominator);
		break;
	case TOK_FLOAT:
		errno = 0;
		floating = strtod(reader->token_buffer, NULL);
		if (errno == ERANGE)
			reader_error(reader, "FLOAT OUT OF RANGE");

		object = make_float(floating);
		break;

	default:
		reader_error(reader, "UNEXPECTED TOKEN");
	}

	return object;
}

objectp
parser_parse_object(struct reader *reader)
{
	return parse_token(reader, lexer_next_token(reader));
}
