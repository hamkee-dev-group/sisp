#ifndef SISP_BUILTINS_H
#define SISP_BUILTINS_H

#include "sisp.h"

#include <stddef.h>
/* Arithmetic and numeric builtins. */
extern objectp builtin_power(const struct object *);
extern objectp builtin_less(const struct object *);
extern objectp builtin_less_equal(const struct object *);
extern objectp builtin_greater(const struct object *);
extern objectp builtin_greater_equal(const struct object *);
extern objectp builtin_numeric_equal(const struct object *);
extern objectp builtin_add(const struct object *);
extern objectp builtin_multiply(const struct object *);
extern objectp builtin_divide(const struct object *);
extern objectp builtin_subtract(const struct object *);
extern objectp builtin_numeric_not_equal(const struct object *);
extern objectp builtin_mod(const struct object *);
extern objectp builtin_and(const struct object *);
extern objectp builtin_or(const struct object *);
extern objectp builtin_not(const struct object *);
extern objectp builtin_xor(const struct object *);
extern objectp builtin_imply(const struct object *);
extern objectp builtin_iff(const struct object *);
extern objectp builtin_min(const struct object *);
extern objectp builtin_max(const struct object *);
/* <math.h> wrappers (MATH1/MATH1_DOM/MATH2 generated). */
extern objectp builtin_sin(const struct object *);
extern objectp builtin_cos(const struct object *);
extern objectp builtin_tan(const struct object *);
extern objectp builtin_asin(const struct object *);
extern objectp builtin_acos(const struct object *);
extern objectp builtin_atan(const struct object *);
extern objectp builtin_atan2(const struct object *);
extern objectp builtin_sinh(const struct object *);
extern objectp builtin_cosh(const struct object *);
extern objectp builtin_tanh(const struct object *);
extern objectp builtin_asinh(const struct object *);
extern objectp builtin_acosh(const struct object *);
extern objectp builtin_atanh(const struct object *);
extern objectp builtin_exp(const struct object *);
extern objectp builtin_log(const struct object *);
extern objectp builtin_log10(const struct object *);
extern objectp builtin_log2(const struct object *);
extern objectp builtin_sqrt(const struct object *);
extern objectp builtin_cbrt(const struct object *);
extern objectp builtin_hypot(const struct object *);
/* Exact-aware, integer, and stateful numeric builtins. */
extern objectp builtin_abs(const struct object *);
extern objectp builtin_floor(const struct object *);
extern objectp builtin_ceiling(const struct object *);
extern objectp builtin_truncate(const struct object *);
extern objectp builtin_round(const struct object *);
extern objectp builtin_signum(const struct object *);
extern objectp builtin_gcd(const struct object *);
extern objectp builtin_lcm(const struct object *);
extern objectp builtin_evenp(const struct object *);
extern objectp builtin_oddp(const struct object *);
extern objectp builtin_random(const struct object *);
extern objectp builtin_seed(const struct object *);
extern objectp builtin_rationalize(const struct object *);
extern objectp builtin_relu(const struct object *);

/* Set and collection builtins. */
extern objectp builtin_cap(const struct object *);
extern objectp builtin_union(const struct object *);
extern objectp builtin_append(const struct object *);
extern objectp builtin_diff(const struct object *);
extern objectp builtin_subset(const struct object *);
extern objectp builtin_cartesian_product(const struct object *);
extern objectp builtin_complement(const struct object *);
extern objectp builtin_in(const struct object *);
extern objectp builtin_notin(const struct object *);
extern objectp builtin_powerset(const struct object *);
extern objectp builtin_symdiff(const struct object *);

// eval: error model
extern objectp builtin_error(const struct object *);
extern objectp builtin_catch(const struct object *);

/* Matrix builtins. */
enum matrix_unary_operation
{
	MATRIX_EXP,
	MATRIX_TANH,
	MATRIX_RELU,
	MATRIX_SQRT
};

extern int matrices_equal(objectp, objectp);
extern objectp builtin_make_matrix(const struct object *);
extern objectp builtin_matrix_from_list(const struct object *);
extern objectp builtin_mat_rows(const struct object *);
extern objectp builtin_mat_cols(const struct object *);
extern objectp builtin_mat_ref(const struct object *);
extern objectp builtin_mat_set(const struct object *);
extern objectp builtin_matrix_multiply(const struct object *);
extern objectp builtin_mat_mul(const struct object *);
extern objectp builtin_mat_transpose(const struct object *);
extern objectp builtin_mat_sum(const struct object *);
extern objectp builtin_mat_div(const struct object *);
extern objectp builtin_mat_add_row(const struct object *);
extern objectp builtin_mat_sum_axis(const struct object *);
extern objectp builtin_mat_max_axis(const struct object *);
extern objectp builtin_mat_softmax(const struct object *);
extern objectp builtin_mat_gather_rows(const struct object *);
extern objectp builtin_mat_scatter_add(const struct object *);
extern objectp builtin_mat_axpy(const struct object *);
extern objectp builtin_mat_scale_into(const struct object *);
extern objectp builtin_mat_zero(const struct object *);
extern objectp builtin_mat_norm(const struct object *);
extern objectp builtin_matrix_randn(const struct object *);
/* generic-operator dispatch (matrices flow through + - * / and math builtins)
 */
extern objectp matrix_fold(objectp, int);
extern objectp matrix_unary_math(objectp, enum matrix_unary_operation);

extern const struct builtin_spec builtins[];
extern const size_t builtin_count;
#endif
