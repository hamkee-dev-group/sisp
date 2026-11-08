/* print.c — the printer.
   Every SISP value has one printed form; floats print at the shortest
   precision that round-trips, matrices print bounded, the empty set
   prints as {}. The printer trusts its data: cycles are the caller's
   problem. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sisp.h"
#include "runtime.h"
#include "builtins.h"
#include "print.h"

/* 340 decimals exhaust a double: the smallest denormal is near 5e-324, so no
   value needs more to read back exactly. */
#define DOUBLE_TEXT_PRECISION_MAX 340

int
format_double(char *buffer, size_t size, double value)
{
	int precision;

	for (precision = 1; precision < DOUBLE_TEXT_PRECISION_MAX; precision++)
	{
		snprintf(buffer, size, "%.*f", precision, value);
		if (strtod(buffer, NULL) == value)
			break;
	}
	return (int)strlen(buffer);
}

static void
print_double(FILE *output, double value)
{
	char buffer[DOUBLE_TEXT_MAX];

	format_double(buffer, sizeof buffer, value);
	fputs(buffer, output);
}

void
print_object(FILE *output, const struct object *object)
{
	long int index;
	int rows, columns, row, column;
	const double *data;

	STACK_CHECK();
	switch (object->type)
	{
	case OBJ_NIL:
		fputs("nil", output);
		break;
	case OBJ_T:
		fputc('t', output);
		break;
	case OBJ_TAU:
		fputs("tau", output);
		break;
	case OBJ_IDENTIFIER:
		if (object->value.identifier.name != NULL)
			fputs(object->value.identifier.name, output);
		break;
	case OBJ_STRING:
		if (object->value.string.data != NULL)
		{
			fputc('"', output);
			for (index = 0; index < object->value.string.length; index++)
				switch (object->value.string.data[index])
				{
				case '\n':
					fputs("\\n", output);
					break;
				case '\r':
					fputs("\\r", output);
					break;
				case '\t':
					fputs("\\t", output);
					break;
				case '"':
					fputs("\\\"", output);
					break;
				case '\\':
					fputs("\\\\", output);
					break;
				default:
					fputc((unsigned char)object->value.string.data[index], output);
					break;
				}
			fputc('"', output);
		}
		break;
	case OBJ_INTEGER:
		fprintf(output, "%li", object->value.i);
		break;
	case OBJ_FLOAT:
		print_double(output, object->value.f);
		break;
	case OBJ_MATRIX:
		/* A matrix prints as the literal the reader accepts, in full — like
		   a list, it is data, not a summary. [c ...] for a single row,
		   [[..][..]] otherwise; read(print(m)) == m. */
		rows = object->value.matrix.rows;
		columns = object->value.matrix.cols;
		data = object->value.matrix.data;
		if (rows != 1)
			fputc('[', output);
		for (row = 0; row < rows; row++)
		{
			fputc('[', output);
			for (column = 0; column < columns; column++)
			{
				if (column)
					fputc(' ', output);
				print_double(output, data[(size_t)row * columns + column]);
			}
			fputc(']', output);
		}
		if (rows != 1)
			fputc(']', output);
		break;
	case OBJ_BUILTIN:
		/* Primitive names evaluate back to the same first-class function. */
		fputs(object->value.builtin->name, output);
		break;
	case OBJ_RATIONAL:
		fprintf(output, "%li/%li", object->value.rational.n, object->value.rational.d);
		break;
	case OBJ_CONS:
		fputc('(', output);
		do
		{
			print_object(output, object->value.cons.car);
			object = object->value.cons.cdr;
			if (object != nil)
			{
				fputc(' ', output);
				if (object->type != OBJ_CONS)
				{
					fputs(". ", output);
					print_object(output, object);
				}
			}
		} while (object != nil && object->type == OBJ_CONS);
		fputc(')', output);
		break;
	case OBJ_SET:
		if (object == empty)
		{
			fputs("{}", output);
			break;
		}
		fputc('{', output);
		do
		{
			print_object(output, object->value.cons.car);
			object = object->value.cons.cdr;
			if (object != nil)
			{
				fputc(' ', output);
				if (object->type != OBJ_SET)
				{
					fputs(": ", output);
					print_object(output, object);
				}
			}
		} while (object != nil && object->type == OBJ_SET);
		fputc('}', output);
		break;
	default:
		return;
	}
}
