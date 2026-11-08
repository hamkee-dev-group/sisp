/* builtins-matrix.c — a dense row-major matrix of doubles (OBJ_MATRIX) plus
   native numeric kernels. Storage is contiguous f64; operations interoperate
   with SISP's numeric tower only at the edges (scalar args via
   number_to_double, scalar results via make_number_from_double). The matrix
   header is pooled like a string; the data buffer is malloc'd and freed by the
   GC sweep. */
#include <string.h>
#include <math.h>

#include "sisp.h"
#include "doc.h"
#include "runtime.h"
#include "builtins.h"
#include "eval.h"
#include "numeric.h"
#include "print.h"

#define MAT_MAX_ELEMS (128UL * 1024 * 1024)

/* Matrix storage remains floating-point even when a cell happens to contain an
   integral value. make_float therefore intentionally does not demote. */
objectp
make_matrix(long int rows, long int columns)
{
	objectp matrix;
	unsigned long element_count;

	if (rows < 0 || columns < 0)
		return sisp_error("MATRIX: NEGATIVE DIMENSION");
	if ((unsigned long)rows > MAT_MAX_ELEMS || (unsigned long)columns > MAT_MAX_ELEMS)
		return sisp_error("MATRIX: TOO LARGE");
	element_count = (unsigned long)rows * (unsigned long)columns;
	if (element_count > MAT_MAX_ELEMS)
		return sisp_error("MATRIX: TOO LARGE");
	matrix = make_object(OBJ_MATRIX);
	matrix->value.matrix.rows = (int)rows;
	matrix->value.matrix.cols = (int)columns;
	matrix->value.matrix.data =
		xcalloc(element_count ? element_count : 1, sizeof(double));
	return matrix;
}
static unsigned long
matrix_size(objectp matrix)
{
	return (unsigned long)matrix->value.matrix.rows *
		   (unsigned long)matrix->value.matrix.cols;
}

static void
require_matrix(objectp value, const char *operation)
{
	require_type(value, OBJ_MATRIX, operation);
}

static void
require_same_shape(objectp left, objectp right, const char *operation)
{
	require_matrix(left, operation);
	require_matrix(right, operation);
	require_condition((left->value.matrix.rows == right->value.matrix.rows &&
					   left->value.matrix.cols == right->value.matrix.cols),
					  operation, "SHAPE MISMATCH", right);
}

static int
require_axis(objectp value, const char *operation)
{
	require_type(value, OBJ_INTEGER, operation);
	require_condition((value->value.i == 0 || value->value.i == 1), operation,
					  "INVALID AXIS", value);
	return (int)value->value.i;
}

/* structural matrix equality: same shape and identical entries */
int
matrices_equal(objectp left, objectp right)
{
	unsigned long index, element_count;

	if (left->value.matrix.rows != right->value.matrix.rows ||
		left->value.matrix.cols != right->value.matrix.cols)
		return 0;
	element_count = matrix_size(left);
	for (index = 0; index < element_count; index++)
		if (left->value.matrix.data[index] != right->value.matrix.data[index])
			return 0;
	return 1;
}

/* ---- construction / inspection ---------------------------------------- */

DOC("make-matrix", "A rows-by-cols matrix of zeros.\n"
				   "  (mat-sum (make-matrix 2 2)) => 0.0\n"
				   "See: matrix mat-zero")
objectp
builtin_make_matrix(const struct object *args)
{
	objectp rows, columns, matrix, fill;
	double fill_value;
	unsigned long index, element_count;

	rows = eval(car(args));
	columns = eval(cadr(args));
	require_type(rows, OBJ_INTEGER, "MAKE-MATRIX");
	require_type(columns, OBJ_INTEGER, "MAKE-MATRIX");
	matrix = make_matrix(rows->value.i, columns->value.i);
	if (cddr(args) != nil) /* optional fill value */
	{
		fill = eval(car(cddr(args)));
		element_count = matrix_size(matrix);
		require_numeric(fill, "MAKE-MATRIX");
		fill_value = number_to_double(fill);
		for (index = 0; index < element_count; index++)
			matrix->value.matrix.data[index] = fill_value;
	}
	return matrix;
}

DOC("matrix-from-list",
	"Build a rows-by-cols matrix from one flat list, row by row.\n"
	"  (mat-ref (matrix-from-list 2 2 (quote (1 2 3 4))) 0 1) => 2.0\n"
	"See: matrix make-matrix")
objectp
builtin_matrix_from_list(const struct object *args)
{
	objectp rows, columns, list, matrix, node;
	unsigned long index, element_count;

	rows = eval(car(args));
	columns = eval(cadr(args));
	list = eval(car(cddr(args)));
	require_type(rows, OBJ_INTEGER, "MATRIX-FROM-LIST");
	require_type(columns, OBJ_INTEGER, "MATRIX-FROM-LIST");
	matrix = make_matrix(rows->value.i, columns->value.i);
	element_count = matrix_size(matrix);
	index = 0;
	for (node = list; node != nil; node = cdr(node), index++)
	{
		require_condition((node->type == OBJ_CONS), "MATRIX-FROM-LIST",
						  "IMPROPER ELEMENT LIST", list);
		require_condition((index < element_count), "MATRIX-FROM-LIST",
						  "TOO MANY ELEMENTS", list);
		require_numeric(car(node), "MATRIX-FROM-LIST");
		matrix->value.matrix.data[index] = number_to_double(car(node));
	}
	require_condition((index == element_count), "MATRIX-FROM-LIST", "TOO FEW ELEMENTS",
					  list);
	return matrix;
}

DOC("mat-rows", "How many rows a matrix has.\n"
				"  (mat-rows (matrix (list (list 1 2)))) => 1\n"
				"See: mat-cols")
objectp
builtin_mat_rows(const struct object *args)
{
	objectp matrix;

	matrix = eval(car(args));
	require_matrix(matrix, "MAT-ROWS");
	return make_integer(matrix->value.matrix.rows);
}

DOC("mat-cols", "How many columns a matrix has.\n"
				"  (mat-cols (matrix (list (list 1 2)))) => 2\n"
				"See: mat-rows")
objectp
builtin_mat_cols(const struct object *args)
{
	objectp matrix;

	matrix = eval(car(args));
	require_matrix(matrix, "MAT-COLS");
	return make_integer(matrix->value.matrix.cols);
}

DOC("mat-ref", "The element at a row and column, zero-based.\n"
			   "  (mat-ref (matrix (list (list 1 2) (list 3 4))) 0 1) => 2.0\n"
			   "See: mat-set mat-rows")
objectp
builtin_mat_ref(const struct object *args)
{
	objectp matrix, row_object, column_object;
	long int row, column;

	matrix = eval(car(args));
	row_object = eval(cadr(args));
	column_object = eval(car(cddr(args)));
	require_matrix(matrix, "MAT-REF");
	require_type(row_object, OBJ_INTEGER, "MAT-REF");
	require_type(column_object, OBJ_INTEGER, "MAT-REF");
	row = row_object->value.i;
	column = column_object->value.i;
	require_condition((row >= 0 && row < matrix->value.matrix.rows), "MAT-REF",
					  "INDEX OUT OF BOUNDS", row_object);
	require_condition((column >= 0 && column < matrix->value.matrix.cols), "MAT-REF",
					  "INDEX OUT OF BOUNDS", column_object);
	return make_float(
		matrix->value.matrix.data[row * matrix->value.matrix.cols + column]);
}

DOC("mat-set", "Store a value at a row and column.\n"
			   "  (mat-ref (mat-set (make-matrix 1 1) 0 0 7) 0 0) => 7.0\n"
			   "Mutates: The matrix passed in, in place. Every reference to it "
			   "sees the new value.\n"
			   "See: mat-ref mat-zero")
objectp
builtin_mat_set(const struct object *args)
{
	objectp matrix, row_object, column_object, value;
	long int row, column;

	matrix = eval(car(args));
	row_object = eval(cadr(args));
	column_object = eval(car(cddr(args)));
	value = eval(car(cdr(cddr(args))));
	require_matrix(matrix, "MAT-SET");
	require_numeric(value, "MAT-SET");
	require_type(row_object, OBJ_INTEGER, "MAT-SET");
	require_type(column_object, OBJ_INTEGER, "MAT-SET");
	row = row_object->value.i;
	column = column_object->value.i;
	require_condition((row >= 0 && row < matrix->value.matrix.rows), "MAT-SET",
					  "INDEX OUT OF BOUNDS", row_object);
	require_condition((column >= 0 && column < matrix->value.matrix.cols), "MAT-SET",
					  "INDEX OUT OF BOUNDS", column_object);
	matrix->value.matrix.data[row * matrix->value.matrix.cols + column] =
		number_to_double(value);
	return matrix;
}

/* ---- kernels ----------------------------------------------------------- */

/* Matrix product on already-evaluated matrices. */
static objectp
multiply(objectp left, objectp right)
{
	objectp result;
	int rows, shared_size, columns, row, column, inner;
	double *left_data, *right_data, *result_data, sum;

	require_matrix(left, "MAT*");
	require_matrix(right, "MAT*");
	require_condition((left->value.matrix.cols == right->value.matrix.rows), "MAT*",
					  "SHAPE MISMATCH", right);
	rows = left->value.matrix.rows;
	shared_size = left->value.matrix.cols;
	columns = right->value.matrix.cols;
	result = make_matrix(rows, columns);
	left_data = left->value.matrix.data;
	right_data = right->value.matrix.data;
	result_data = result->value.matrix.data;
	for (row = 0; row < rows; row++)
		for (column = 0; column < columns; column++)
		{
			sum = 0.0;
			for (inner = 0; inner < shared_size; inner++)
				sum += left_data[row * shared_size + inner] *
					   right_data[inner * columns + column];
			result_data[row * columns + column] = sum;
		}
	return result;
}
DOC("mat*", "Matrix multiplication.\n"
			"  (mat-ref (mat* (matrix (list (list 1 2) (list 3 4))) (matrix "
			"(list (list 5 6) (list 7 8)))) 0 0) => 19.0\n"
			"Note: Not the same as mat-mul, which is elementwise. The names "
			"are easy to confuse and mat* is the ordinary matrix product.\n"
			"Errors: MAT*: SHAPE MISMATCH unless the inner dimensions agree.\n"
			"See: mat-mul mat-transpose")
objectp
builtin_matrix_multiply(const struct object *args)
{
	return multiply(eval(car(args)), eval(cadr(args)));
}

enum matrix_binary_operation
{
	MATRIX_ADD,
	MATRIX_MULTIPLY,
	MATRIX_SUBTRACT,
	MATRIX_DIVIDE
};

static objectp
elementwise(objectp left, objectp right, enum matrix_binary_operation operation,
			const char *operation_name)
{
	objectp result;
	unsigned long index, element_count;
	double *left_data, *right_data, *result_data;

	require_same_shape(left, right, operation_name);
	element_count = matrix_size(left);
	if (operation == MATRIX_DIVIDE)
		for (index = 0; index < element_count; index++)
			require_condition((right->value.matrix.data[index] != 0.0), operation_name,
							  "DIVISION BY ZERO", right);
	result = make_matrix(left->value.matrix.rows, left->value.matrix.cols);
	left_data = left->value.matrix.data;
	right_data = right->value.matrix.data;
	result_data = result->value.matrix.data;
	for (index = 0; index < element_count; index++)
		switch (operation)
		{
		case MATRIX_ADD:
			result_data[index] = left_data[index] + right_data[index];
			break;
		case MATRIX_MULTIPLY:
			result_data[index] = left_data[index] * right_data[index];
			break;
		case MATRIX_SUBTRACT:
			result_data[index] = left_data[index] - right_data[index];
			break;
		case MATRIX_DIVIDE:
			result_data[index] = left_data[index] / right_data[index];
			break;
		}
	return result;
}
DOC("mat-mul", "Elementwise (Hadamard) product, NOT matrix multiplication.\n"
			   "  (mat-ref (mat-mul (matrix (list (list 1 2) (list 3 4))) "
			   "(matrix (list (list 5 6) (list 7 8)))) 0 0) => 5.0\n"
			   "Bugs: The name reads like matrix multiplication, which is "
			   "mat*. This has caught people out and is worth renaming.\n"
			   "See: mat* mat-div")
objectp
builtin_mat_mul(const struct object *args)
{
	return elementwise(eval(car(args)), eval(cadr(args)), MATRIX_MULTIPLY, "MAT-MUL");
}
DOC("mat-transpose", "The transpose, as a new matrix.\n"
					 "  (mat-cols (mat-transpose (matrix (list (list 1 2))))) => 1\n"
					 "See: mat*")
objectp
builtin_mat_transpose(const struct object *args)
{
	objectp matrix, result;
	int rows, columns, row, column;

	matrix = eval(car(args));
	require_matrix(matrix, "MAT-TRANSPOSE");
	rows = matrix->value.matrix.rows;
	columns = matrix->value.matrix.cols;
	result = make_matrix(columns, rows);
	for (row = 0; row < rows; row++)
		for (column = 0; column < columns; column++)
			result->value.matrix.data[(long)column * rows + row] =
				matrix->value.matrix.data[(long)row * columns + column];
	return result;
}

DOC("mat-sum", "Sum of every element.\n"
			   "  (mat-sum (matrix (list (list 1 2)))) => 3.0\n"
			   "See: mat-sum-axis mat-norm")
objectp
builtin_mat_sum(const struct object *args)
{
	objectp matrix;
	double sum;
	unsigned long index, element_count;

	matrix = eval(car(args));
	require_matrix(matrix, "MAT-SUM");
	sum = 0.0;
	element_count = matrix_size(matrix);
	for (index = 0; index < element_count; index++)
		sum += matrix->value.matrix.data[index];
	return make_float(sum);
}

objectp
matrix_unary_math(objectp matrix, enum matrix_unary_operation operation)
{
	objectp result;
	unsigned long index, element_count;
	double x;

	require_matrix(matrix, "MATRIX UNARY MATH");
	element_count = matrix_size(matrix);
	if (operation == MATRIX_SQRT)
		for (index = 0; index < element_count; index++)
			require_condition((matrix->value.matrix.data[index] >= 0.0), "SQRT",
							  "NEGATIVE ELEMENT", matrix);
	result = make_matrix(matrix->value.matrix.rows, matrix->value.matrix.cols);
	for (index = 0; index < element_count; index++)
	{
		x = matrix->value.matrix.data[index];
		switch (operation)
		{
		case MATRIX_EXP:
			result->value.matrix.data[index] = exp(x);
			break;
		case MATRIX_TANH:
			result->value.matrix.data[index] = tanh(x);
			break;
		case MATRIX_RELU:
			result->value.matrix.data[index] = x > 0.0 ? x : 0.0;
			break;
		case MATRIX_SQRT:
			result->value.matrix.data[index] = sqrt(x);
			break;
		}
	}
	return result;
}
DOC("mat-div", "Elementwise division.\n"
			   "  (mat-ref (mat-div (matrix (list (list 4 6))) (matrix (list "
			   "(list 2 3)))) 0 0) => 2.0\n"
			   "See: mat-mul *")
objectp
builtin_mat_div(const struct object *args)
{
	return elementwise(eval(car(args)), eval(cadr(args)), MATRIX_DIVIDE, "MAT-DIV");
}

/* ---- generic-operator dispatch: matrices flow through +,-,*,/ and the
   elementwise math builtins. `*` is THE matrix product (matmul); matrix+scalar
   and scalar*matrix broadcast; matrix±matrix and the unary math functions apply
   elementwise. The Hadamard product and elementwise division keep their own
   names (mat-mul / mat-div) — they are not `*`/`/`. These take
   already-evaluated operands and are reached only when at least one operand is
   a matrix. ---- */

/* Return a new matrix whose cells are matrix[i] * multiplier + addend. */
static objectp
scale_add(objectp matrix, double multiplier, double addend)
{
	objectp result;
	unsigned long index, element_count;

	result = make_matrix(matrix->value.matrix.rows, matrix->value.matrix.cols);
	element_count = matrix_size(matrix);
	for (index = 0; index < element_count; index++)
		result->value.matrix.data[index] =
			matrix->value.matrix.data[index] * multiplier + addend;
	return result;
}

static objectp
arithmetic(objectp left, objectp right, int operator)
{
	int left_is_matrix, right_is_matrix;
	objectp matrix, scalar_object;
	double scalar, left_value, right_value;

	left_is_matrix = left->type == OBJ_MATRIX;
	right_is_matrix = right->type == OBJ_MATRIX;
	if (left_is_matrix && right_is_matrix)
	{
		if (operator== '+')
			return elementwise(left, right, MATRIX_ADD, "+");
		if (operator== '-')
			return elementwise(left, right, MATRIX_SUBTRACT, "-");
		if (operator== '*')
			return multiply(left, right);
		return sisp_error("/: NO MATRIX DIVISION (elementwise is mat-div)");
	}
	if (left_is_matrix || right_is_matrix)
	{
		matrix = left_is_matrix ? left : right;
		scalar_object = left_is_matrix ? right : left;
		require_numeric(scalar_object, "MATRIX ARITHMETIC");
		scalar = number_to_double(scalar_object);
		if (operator== '+')
			return scale_add(matrix, 1.0, scalar);
		if (operator== '*')
			return scale_add(matrix, scalar, 0.0);
		if (operator== '-')
			return left_is_matrix ? scale_add(matrix, 1.0, -scalar)
								  : scale_add(matrix, -1.0, scalar);
		if (left_is_matrix)
		{
			require_condition((scalar != 0.0), "/", "DIVISION BY ZERO", scalar_object);
			return scale_add(matrix, 1.0 / scalar, 0.0);
		}
		return sisp_error("/: SCALAR / MATRIX UNDEFINED");
	}
	/* both scalar: an intermediate step of a matrix-valued fold */
	left_value = number_to_double(left);
	right_value = number_to_double(right);
	if (operator== '/')
		require_condition((right_value != 0.0), "/", "DIVISION BY ZERO", right);
	return make_number_from_double(operator== '+'	? left_value + right_value
								   : operator== '-' ? left_value - right_value
								   : operator== '*' ? left_value * right_value
													: left_value / right_value);
}

/* fold a variadic operator over an argument list that contains >=1 matrix */
objectp
matrix_fold(objectp values, int operator)
{
	objectp result, remaining;

	result = car(values);
	remaining = cdr(values);
	if (remaining == nil)
	{
		if (operator== '-')
			return scale_add(result, -1.0, 0.0);
		if (operator== '/')
			return sisp_error("/: UNARY MATRIX RECIPROCAL UNDEFINED");
		return result;
	}
	for (; remaining != nil; remaining = cdr(remaining))
		result = arithmetic(result, car(remaining), operator);
	return result;
}

/* ---- broadcast, reductions, softmax, gather/scatter, in-place, init ---- */

DOC("mat-add-row", "Add a row vector to every row.\n"
				   "Mutates: Nothing; returns a new matrix.\n"
				   "See: + mat-axpy")
objectp
builtin_mat_add_row(const struct object *args) /* add a 1 x cols vector to every row */
{
	objectp matrix, row_vector, result;
	int rows, columns, row, column;

	matrix = eval(car(args));
	row_vector = eval(cadr(args));
	require_matrix(matrix, "MAT-ADD-ROW");
	require_matrix(row_vector, "MAT-ADD-ROW");
	require_condition((row_vector->value.matrix.rows == 1 &&
					   row_vector->value.matrix.cols == matrix->value.matrix.cols),
					  "MAT-ADD-ROW", "SHAPE MISMATCH", row_vector);
	rows = matrix->value.matrix.rows;
	columns = matrix->value.matrix.cols;
	result = make_matrix(rows, columns);
	if (columns == 0)
		return result;
	for (row = 0; row < rows; row++)
		for (column = 0; column < columns; column++)
			result->value.matrix.data[(long)row * columns + column] =
				matrix->value.matrix.data[(long)row * columns + column] +
				row_vector->value.matrix.data[column];
	return result;
}

DOC("mat-sum-axis", "Sums along one axis, as a matrix.\n"
					"See: mat-sum mat-max-axis")
objectp
builtin_mat_sum_axis(
	const struct object *args) /* axis 0 -> 1 x cols; axis 1 -> rows x 1 */
{
	objectp matrix, axis_object, result;
	int rows, columns, row, column, axis;
	double sum;

	matrix = eval(car(args));
	axis_object = eval(cadr(args));
	require_matrix(matrix, "MAT-SUM-AXIS");
	axis = require_axis(axis_object, "MAT-SUM-AXIS");
	rows = matrix->value.matrix.rows;
	columns = matrix->value.matrix.cols;
	if (axis == 0)
	{
		result = make_matrix(1, columns);
		for (column = 0; column < columns; column++)
		{
			sum = 0.0;
			for (row = 0; row < rows; row++)
				sum += matrix->value.matrix.data[(long)row * columns + column];
			result->value.matrix.data[column] = sum;
		}
	}
	else
	{
		result = make_matrix(rows, 1);
		for (row = 0; row < rows; row++)
		{
			sum = 0.0;
			for (column = 0; column < columns; column++)
				sum += matrix->value.matrix.data[(long)row * columns + column];
			result->value.matrix.data[row] = sum;
		}
	}
	return result;
}

DOC("mat-max-axis", "Maxima along one axis, as a matrix.\n"
					"See: mat-sum-axis mat-softmax")
objectp
builtin_mat_max_axis(const struct object *args)
{
	objectp matrix, axis_object, result;
	int rows, columns, row, column, axis;
	long int offset;
	double maximum;

	matrix = eval(car(args));
	axis_object = eval(cadr(args));
	require_matrix(matrix, "MAT-MAX-AXIS");
	axis = require_axis(axis_object, "MAT-MAX-AXIS");
	rows = matrix->value.matrix.rows;
	columns = matrix->value.matrix.cols;
	require_condition((rows > 0 && columns > 0), "MAT-MAX-AXIS", "EMPTY MATRIX",
					  matrix);
	if (axis == 0)
	{
		result = make_matrix(1, columns);
		for (column = 0; column < columns; column++)
		{
			maximum = matrix->value.matrix.data[column];
			for (row = 1; row < rows; row++)
			{
				offset = (long)row * columns + column;
				if (matrix->value.matrix.data[offset] > maximum)
					maximum = matrix->value.matrix.data[offset];
			}
			result->value.matrix.data[column] = maximum;
		}
	}
	else
	{
		result = make_matrix(rows, 1);
		for (row = 0; row < rows; row++)
		{
			maximum = matrix->value.matrix.data[(long)row * columns];
			for (column = 1; column < columns; column++)
			{
				offset = (long)row * columns + column;
				if (matrix->value.matrix.data[offset] > maximum)
					maximum = matrix->value.matrix.data[offset];
			}
			result->value.matrix.data[row] = maximum;
		}
	}
	return result;
}

DOC("mat-softmax", "Row-wise softmax: each row is scaled to sum to one.\n"
				   "  (mat-sum (mat-softmax (matrix (list (list 1 1))))) => 1.0\n"
				   "See: exp mat-max-axis")
objectp
builtin_mat_softmax(const struct object *args) /* numerically-stable, row-wise */
{
	objectp matrix, result;
	int rows, columns, row, column;
	double maximum, normalizer, exponential;

	matrix = eval(car(args));
	require_matrix(matrix, "MAT-SOFTMAX");
	rows = matrix->value.matrix.rows;
	columns = matrix->value.matrix.cols;
	result = make_matrix(rows, columns);
	if (columns == 0)
		return result;
	for (row = 0; row < rows; row++)
	{
		maximum = matrix->value.matrix.data[(long)row * columns];
		for (column = 1; column < columns; column++)
			if (matrix->value.matrix.data[(long)row * columns + column] > maximum)
				maximum = matrix->value.matrix.data[(long)row * columns + column];
		normalizer = 0.0;
		for (column = 0; column < columns; column++)
		{
			exponential =
				exp(matrix->value.matrix.data[(long)row * columns + column] - maximum);
			result->value.matrix.data[(long)row * columns + column] = exponential;
			normalizer += exponential;
		}
		for (column = 0; column < columns; column++)
			result->value.matrix.data[(long)row * columns + column] /= normalizer;
	}
	return result;
}

DOC("mat-gather-rows", "A new matrix of the rows at the indices given.\n"
					   "See: mat-scatter-add mat-ref")
objectp
builtin_mat_gather_rows(const struct object *args) /* select rows by an index list */
{
	objectp source, indices, result, index_node;
	int columns, destination_row, row_count;
	long int source_row;

	source = eval(car(args));
	indices = eval(cadr(args));
	require_matrix(source, "MAT-GATHER-ROWS");
	columns = source->value.matrix.cols;
	row_count = 0;
	for (index_node = indices; index_node != nil; index_node = cdr(index_node))
	{
		require_condition((index_node->type == OBJ_CONS), "MAT-GATHER-ROWS",
						  "IMPROPER INDEX LIST", indices);
		require_type(car(index_node), OBJ_INTEGER, "MAT-GATHER-ROWS");
		source_row = car(index_node)->value.i;
		require_condition((source_row >= 0 && source_row < source->value.matrix.rows),
						  "MAT-GATHER-ROWS", "INDEX OUT OF BOUNDS", car(index_node));
		row_count++;
	}
	result = make_matrix(row_count, columns);
	destination_row = 0;
	for (index_node = indices; index_node != nil; index_node = cdr(index_node))
	{
		source_row = car(index_node)->value.i;
		memcpy(result->value.matrix.data + (long)destination_row * columns,
			   source->value.matrix.data + source_row * columns,
			   (size_t)columns * sizeof(double));
		destination_row++;
	}
	return result;
}

DOC("mat-scatter-add", "Add rows into a destination at the row indices given.\n"
					   "Mutates: The destination matrix, in place.\n"
					   "See: mat-gather-rows mat-axpy")
objectp
builtin_mat_scatter_add(
	const struct object *args) /* destination[index[i]] += source[i] */
{
	objectp destination, indices, source, index_node;
	int columns, source_row, column;
	long int destination_row;
	double *destination_data, *source_data;

	destination = eval(car(args));
	indices = eval(cadr(args));
	source = eval(car(cddr(args)));
	require_matrix(destination, "MAT-SCATTER-ADD");
	require_matrix(source, "MAT-SCATTER-ADD");
	require_condition((destination->value.matrix.cols == source->value.matrix.cols),
					  "MAT-SCATTER-ADD", "SHAPE MISMATCH", source);
	columns = destination->value.matrix.cols;
	destination_data = destination->value.matrix.data;
	source_data = source->value.matrix.data;

	/* Validate the complete operation before mutating destination. */
	source_row = 0;
	for (index_node = indices; index_node != nil;
		 index_node = cdr(index_node), source_row++)
	{
		require_condition((index_node->type == OBJ_CONS), "MAT-SCATTER-ADD",
						  "IMPROPER INDEX LIST", indices);
		require_type(car(index_node), OBJ_INTEGER, "MAT-SCATTER-ADD");
		destination_row = car(index_node)->value.i;
		require_condition(
			(destination_row >= 0 && destination_row < destination->value.matrix.rows),
			"MAT-SCATTER-ADD", "INDEX OUT OF BOUNDS", car(index_node));
	}
	require_condition((source_row == source->value.matrix.rows), "MAT-SCATTER-ADD",
					  "ROW COUNT MISMATCH", source);

	source_row = 0;
	for (index_node = indices; index_node != nil;
		 index_node = cdr(index_node), source_row++)
	{
		destination_row = car(index_node)->value.i;
		for (column = 0; column < columns; column++)
			destination_data[destination_row * columns + column] +=
				source_data[(long)source_row * columns + column];
	}
	return destination;
}

DOC("mat-axpy", "Add a scalar multiple of one matrix into another.\n"
				"Mutates: The destination matrix, in place. This is the usual "
				"gradient update step, which is why it avoids allocating.\n"
				"See: mat-scale-into +")
objectp
builtin_mat_axpy(const struct object *args) /* destination += scalar * source */
{
	objectp destination, scalar_object, source;
	double scalar;
	unsigned long index, element_count;

	destination = eval(car(args));
	scalar_object = eval(cadr(args));
	source = eval(car(cddr(args)));
	require_same_shape(destination, source, "MAT-AXPY");
	require_numeric(scalar_object, "MAT-AXPY");
	scalar = number_to_double(scalar_object);
	element_count = matrix_size(destination);
	for (index = 0; index < element_count; index++)
		destination->value.matrix.data[index] +=
			scalar * source->value.matrix.data[index];
	return destination;
}

DOC("mat-scale-into",
	"Multiply every element by a scalar, writing into the matrix.\n"
	"Mutates: The matrix passed in, in place, rather than allocating a "
	"result.\n"
	"See: * mat-axpy")
objectp
builtin_mat_scale_into(const struct object *args) /* matrix *= scalar */
{
	objectp matrix, scalar_object;
	double scalar;
	unsigned long index, element_count;

	matrix = eval(car(args));
	scalar_object = eval(cadr(args));
	require_matrix(matrix, "MAT-SCALE-INTO");
	require_numeric(scalar_object, "MAT-SCALE-INTO");
	scalar = number_to_double(scalar_object);
	element_count = matrix_size(matrix);
	for (index = 0; index < element_count; index++)
		matrix->value.matrix.data[index] *= scalar;
	return matrix;
}

DOC("mat-zero", "Set every element of a matrix to zero.\n"
				"  (mat-sum (mat-zero (matrix (list (list 1 2))))) => 0.0\n"
				"Mutates: The matrix passed in, in place.\n"
				"See: make-matrix mat-set")
objectp
builtin_mat_zero(const struct object *args)
{
	objectp matrix;

	matrix = eval(car(args));
	require_matrix(matrix, "MAT-ZERO");
	memset(matrix->value.matrix.data, 0, matrix_size(matrix) * sizeof(double));
	return matrix;
}

DOC("mat-norm", "Euclidean norm of all the elements.\n"
				"  (mat-norm (matrix (list (list 3 4)))) => 5.0\n"
				"See: mat-sum")
objectp
builtin_mat_norm(const struct object *args)
{
	objectp matrix;
	double sum, value;
	unsigned long index, element_count;

	matrix = eval(car(args));
	require_matrix(matrix, "MAT-NORM");
	sum = 0.0;
	element_count = matrix_size(matrix);
	for (index = 0; index < element_count; index++)
	{
		value = matrix->value.matrix.data[index];
		sum += value * value;
	}
	return make_float(sqrt(sum));
}

DOC("matrix-randn", "A matrix of standard normal samples.\n"
					"Mutates: Draws from the global generator, so it advances "
					"the random state.\n"
					"See: matrix random seed")
objectp
builtin_matrix_randn(const struct object *args) /* Gaussian(0, scale) via Box-Muller */
{
	objectp rows, columns, scale_object, matrix;
	double scale, first_uniform, second_uniform;
	unsigned long index, element_count;

	rows = eval(car(args));
	columns = eval(cadr(args));
	require_type(rows, OBJ_INTEGER, "MATRIX-RANDN");
	require_type(columns, OBJ_INTEGER, "MATRIX-RANDN");
	scale = 1.0;
	if (cddr(args) != nil)
	{
		scale_object = eval(car(cddr(args)));
		require_numeric(scale_object, "MATRIX-RANDN");
		scale = number_to_double(scale_object);
	}
	matrix = make_matrix(rows->value.i, columns->value.i);
	element_count = matrix_size(matrix);
	for (index = 0; index < element_count; index++)
	{
		first_uniform = rng_uniform();
		second_uniform = rng_uniform();
		if (first_uniform < 1e-300)
			first_uniform = 1e-300;
		matrix->value.matrix.data[index] =
			scale * sqrt(-2.0 * log(first_uniform)) *
			cos(2.0 * 3.14159265358979323846 * second_uniform);
	}
	return matrix;
}
