#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>

#include "sisp.h"
#include "runtime.h"
#include "builtins.h"
#include "eval.h"
#include "equal.h"
#include "numeric.h"
#include "print.h"
#include "comprehension.h"
#include "special.h"
#include "pool.h"
#include "reader.h"
#include "doc.h"

/* A nested load owns an independent reader, and its form loop pushes its own
   recovery frame, so nothing here has to preserve the caller's -- the frame
   stack does it. What does need handling is the caller's catch: it is
   suspended for the duration, so a failure in the loaded file unwinds to that
   file's own loop and comes back as one LOAD FAILED, rather than jumping
   straight into the caller's handler and skipping the in_load bookkeeping. */
static int
load_isolated(const char *fname)
{
	void *saved_handlers = error_handlers_suspend();
	int success;

	in_load++;
	success = process_input(fname);
	in_load--;
	error_handlers_restore(saved_handlers);
	return success;
}

DOC("car", "First element of a cons pair, or of a set.\n"
		   "  (car (quote (1 2 3))) => 1\n"
		   "  (car nil) => nil\n"
		   "Errors: CAR: UNDEFINED unless the argument is a cons, a set, or nil.\n"
		   "See: cdr caar cadr")
static objectp
builtin_car(const struct object *args)
{
	return car(eval(car(args)));
}

DOC("cdr", "Everything after the first element of a cons pair, or of a set.\n"
		   "  (cdr (quote (1 2 3))) => (2 3)\n"
		   "  (cdr nil) => nil\n"
		   "Errors: CDR: UNDEFINED unless the argument is a cons, a set, or nil.\n"
		   "See: car cddr cadr")
static objectp
builtin_cdr(const struct object *args)
{
	return cdr(eval(car(args)));
}

/* ---- native list operations ------------------------------------------
   Each follows plain car/cdr semantics, edge cases included: car/cdr of
   nil are nil, so (last nil) and (butlast nil) are nil, while an improper
   tail still raises CAR/CDR UNDEFINED from car/cdr itself. */

/* take/drop count down by one per element and stop at (<= k 0), so a
   fractional count takes ceil(k) elements. This is deliberate: callers may
   pass a rational such as (/ n 2). */
static long int
step_count(objectp k, const char *operation)
{
	double value;

	if (k->type == OBJ_INTEGER)
		return k->value.i;
	if (k->type == OBJ_RATIONAL)
		value = (double)k->value.rational.n / (double)k->value.rational.d;
	else if (k->type == OBJ_FLOAT)
		value = k->value.f;
	else
		return (long int)(sisp_errorf("%s: NON NUMERIC COUNT", operation), 0);
	return (long int)ceil(value);
}

DOC("reverse", "A new list with the elements in the opposite order.\n"
			   "  (reverse (quote (1 2 3))) => (3 2 1)\n"
			   "  (reverse {1 2 3}) => (3 2 1)\n"
			   "Mutates: Nothing. The result is freshly allocated.\n"
			   "Note: A set is walked like a list, so reversing one yields a list.\n"
			   "Errors: CDR: UNDEFINED on an improper list.\n"
			   "See: append sort")
static objectp
builtin_reverse(const struct object *args)
{
	objectp node, next, result = nil;

	/* No type test: this walks with car/cdr, which keeps two behaviours it
	   would be easy to lose -- a set walks like a list, and an improper tail
	   reports CDR (taken first) rather than CAR. */
	for (node = eval(car(args)); node != nil; node = next)
	{
		next = cdr(node);
		result = cons(car(node), result);
	}
	return result;
}

DOC("last", "The final element of a list.\n"
			"  (last (quote (1 2 3))) => 3\n"
			"  (last nil) => nil\n"
			"Note: car and cdr of nil are nil, which is why the empty list "
			"answers nil rather than raising.\n"
			"See: butlast car")
static objectp
builtin_last(const struct object *args)
{
	objectp node = eval(car(args));

	while (cdr(node) != nil)
		node = cdr(node);
	return car(node);
}

DOC("butlast", "Everything except the final element, as a new list.\n"
			   "  (butlast (quote (1 2 3))) => (1 2)\n"
			   "  (butlast nil) => nil\n"
			   "See: last take")
static objectp
builtin_butlast(const struct object *args)
{
	objectp node = eval(car(args));
	struct list result;

	list_init(&result, OBJ_CONS);
	for (; cdr(node) != nil; node = cdr(node))
		list_append(&result, car(node));
	return result.head;
}

DOC("drop", "The list with its first k elements removed.\n"
			"  (drop (quote (1 2 3 4)) 2) => (3 4)\n"
			"  (drop (quote (1 2)) 9) => nil\n"
			"  (drop (quote (1 2)) 0) => (1 2)\n"
			"Errors: DROP: NON NUMERIC COUNT if k is not a number.\n"
			"See: take nth")
static objectp
builtin_drop(const struct object *args)
{
	objectp node = eval(car(args));
	long int remaining = step_count(eval(cadr(args)), "DROP");

	while (remaining > 0 && node != nil)
	{
		node = cdr(node);
		remaining--;
	}
	return node;
}

DOC("take", "The first k elements, or the whole list if it is shorter.\n"
			"  (take (quote (1 2 3 4)) 2) => (1 2)\n"
			"  (take (quote (1 2)) 9) => (1 2)\n"
			"  (take (quote (1 2)) -1) => nil\n"
			"Note: A fractional count takes ceil(k) elements, because the original "
			"prelude version counted down by one and stopped at (<= k 0).\n"
			"Errors: TAKE: NON NUMERIC COUNT if k is not a number.\n"
			"See: drop butlast")
static objectp
builtin_take(const struct object *args)
{
	objectp node = eval(car(args));
	long int remaining = step_count(eval(cadr(args)), "TAKE");
	struct list result;

	list_init(&result, OBJ_CONS);
	for (; node != nil && remaining > 0; node = cdr(node), remaining--)
		list_append(&result, car(node));
	return result.head;
}

DOC("contains", "Whether a list holds a value, compared with equal.\n"
				"  (contains 2 (quote (1 2 3))) => t\n"
				"  (contains 9 (quote (1 2))) => nil\n"
				"Note: equal is structural, so 1 and 1.0 compare alike here.\n"
				"See: index-of in assoc")
static objectp
builtin_contains(const struct object *args)
{
	objectp wanted = eval(car(args));
	objectp node;

	for (node = eval(cadr(args)); node != nil; node = cdr(node))
		if (objects_equal_by_test(wanted, car(node), TEST_EQUAL))
			return t;
	return nil;
}

DOC("index-of", "The zero-based position of a value in a list, or nil.\n"
				"  (index-of 3 (quote (1 2 3))) => 2\n"
				"  (index-of 9 (quote (1 2))) => nil\n"
				"See: contains nth")
static objectp
builtin_index_of(const struct object *args)
{
	objectp wanted = eval(car(args));
	objectp node;
	long int position = 0;

	for (node = eval(cadr(args)); node != nil; node = cdr(node), position++)
		if (objects_equal_by_test(wanted, car(node), TEST_EQUAL))
			return make_integer(position);
	return nil;
}

/* Depth-first leaf collection. As in reverse there is no type test, so a set
   walks like a list and an improper tail raises CAR: UNDEFINED from car().
   Only a cons element is descended into, so a set element is collected
   whole rather than flattened. */
static void
flatten_into(struct list *result, objectp node)
{
	objectp element;

	for (; node != nil; node = cdr(node))
	{
		element = car(node);
		if (element != nil && element->type == OBJ_CONS)
			flatten_into(result, element);
		else
			list_append(result, element);
	}
}

DOC("flatten", "All leaves of a nested list, in order, as one flat list.\n"
			   "  (flatten (quote (1 (2 (3 4)) 5))) => (1 2 3 4 5)\n"
			   "Note: Only cons elements are descended into; a set element is "
			   "kept whole.\n"
			   "Errors: CAR: UNDEFINED on an improper list.\n"
			   "See: flatmap deep-flatmap append")
static objectp
builtin_flatten(const struct object *args)
{
	struct list result;

	list_init(&result, OBJ_CONS);
	flatten_into(&result, eval(car(args)));
	return result.head;
}

DOC("zip", "Pair up two lists, stopping at the shorter one.\n"
		   "  (zip (quote (1 2 3)) (quote (a b))) => ((1 a) (2 b))\n"
		   "See: map")
static objectp
builtin_zip(const struct object *args)
{
	objectp left = eval(car(args));
	objectp right = eval(cadr(args));
	struct list result;

	list_init(&result, OBJ_CONS);
	for (; left != nil && right != nil; left = cdr(left), right = cdr(right))
		list_append(&result, cons(car(left), cons(car(right), nil)));
	return result.head;
}

/* Quadratic: each candidate is compared against everything kept so far.
   Order of first appearance is preserved. */
DOC("dedup", "Remove repeats, keeping the first occurrence of each.\n"
			 "  (dedup (quote (3 1 3 2 1))) => (3 1 2)\n"
			 "Note: Compares with equal, so 1 and 1.0 stay distinct -- unlike "
			 "routing through a set, which treats them as the same number.\n"
			 "Bugs: Quadratic: every candidate is compared against everything "
			 "kept so far. Fine for short lists, slow for long ones.\n"
			 "See: contains list-to-set")
static objectp
builtin_dedup(const struct object *args)
{
	objectp node, kept;
	struct list result;

	list_init(&result, OBJ_CONS);
	for (node = eval(car(args)); node != nil; node = cdr(node))
	{
		for (kept = result.head; kept != nil; kept = cdr(kept))
			if (objects_equal_by_test(car(node), car(kept), TEST_EQUAL))
				break;
		if (kept == nil)
			list_append(&result, car(node));
	}
	return result.head;
}

DOC("set-to-list", "A set's elements as a list.\n"
				   "  (set-to-list {1 2 3}) => (1 2 3)\n"
				   "  (set-to-list {}) => nil\n"
				   "Errors: SET-TO-LIST: COMPREHENSION HAS NO FINITE ENUMERATION for a "
				   "comprehension set; use enumerate with a carrier instead.\n"
				   "See: list-to-set enumerate")
static objectp
builtin_set_to_list(const struct object *args)
{
	objectp set = eval(car(args));
	struct list result;

	if (set == nil || set == empty)
		return nil;
	if (object_is_comprehension_set(set))
		sisp_raise(
			make_error_value("SET-TO-LIST: COMPREHENSION HAS NO FINITE ENUMERATION; "
							 "USE (enumerate class carrier)",
							 set));
	list_init(&result, OBJ_CONS);
	for (; set != nil; set = cdr(set))
		list_append(&result, car(set));
	return result.head;
}

struct sort_comparison
{
	objectp form;
	objectp left_slot;
	objectp right_slot;
};

static int
sort_precedes(struct sort_comparison *comparison, objectp left, objectp right)
{
	comparison->left_slot->value.cons.car = left;
	comparison->right_slot->value.cons.car = right;
	return eval(comparison->form) != nil;
}

static objectp
merge_sorted_lists(objectp left, objectp right, struct sort_comparison *comparison)
{
	objectp head, tail, next;

	head = nil;
	tail = NULL;
	while (left != nil && right != nil)
	{
		if (sort_precedes(comparison, car(right), car(left)))
		{
			next = right;
			right = cdr(right);
		}
		else
		{
			next = left;
			left = cdr(left);
		}
		if (tail == NULL)
			head = next;
		else
			tail->value.cons.cdr = next;
		tail = next;
	}
	tail->value.cons.cdr = left != nil ? left : right;
	return head;
}

static objectp
merge_sort_list(objectp list, struct sort_comparison *comparison)
{
	objectp slow, fast, right, left;

	if (list == nil || cdr(list) == nil)
		return list;

	slow = list;
	fast = cdr(list);
	while (fast != nil)
	{
		fast = cdr(fast);
		if (fast != nil)
		{
			slow = cdr(slow);
			fast = cdr(fast);
		}
	}
	right = cdr(slow);
	slow->value.cons.cdr = nil;
	left = merge_sort_list(list, comparison);
	right = merge_sort_list(right, comparison);
	return merge_sorted_lists(left, right, comparison);
}

DOC("sort", "Sort a list under a two-argument less-than predicate.\n"
			"  (sort < (quote (3 1 2))) => (1 2 3)\n"
			"  (sort > (quote (1 3 2))) => (3 2 1)\n"
			"Mutates: Nothing. The argument list is copied first.\n"
			"Errors: SORT: IMPROPER LIST if the argument is not a proper list.\n"
			"Note: A stable merge sort: elements the predicate calls equal keep their "
			"original order. It was an anti-stable insertion sort before, so code that "
			"depended on ties being reversed will see different output.\n"
			"See: filter reverse")
static objectp
builtin_sort(const struct object *args)
{
	objectp function, list, node, left_argument, right_argument;
	struct list copy;
	struct sort_comparison comparison;

	function = eval(car(args));
	list = eval(cadr(args));
	list_init(&copy, OBJ_CONS);
	for (node = list; node != nil; node = cdr(node))
	{
		require_condition((node->type == OBJ_CONS), "SORT", "IMPROPER LIST", list);
		list_append(&copy, car(node));
	}
	if (copy.head == nil || cdr(copy.head) == nil)
		return copy.head;

	left_argument = make_quoted(nil);
	right_argument = make_quoted(nil);
	comparison.form =
		cons(make_quoted(function), cons(left_argument, cons(right_argument, nil)));
	comparison.left_slot = cdr(left_argument);
	comparison.right_slot = cdr(right_argument);
	return merge_sort_list(copy.head, &comparison);
}

DOC("set-car",
	"Replace the car of an existing pair, in place.\n"
	"  (set-car (list 1 2) 9) => (9 2)\n"
	"Mutates: The pair passed in. Every other reference to that pair sees the "
	"new value too, including bindings made before the call.\n"
	"Errors: SET-CAR: NOT CONS if the first argument is not a cons pair.\n"
	"See: set-cdr cons car")
static objectp
builtin_set_car(const struct object *args)
{
	objectp pair = eval(car(args));
	objectp value = eval(cadr(args));
	require_type(pair, OBJ_CONS, "SET-CAR");
	pair->value.cons.car = value;
	return pair;
}

DOC("set-cdr",
	"Replace the cdr of an existing pair, in place.\n"
	"  (set-cdr (list 1 2) nil) => (1)\n"
	"Mutates: The pair passed in, and therefore every list that shares this "
	"pair as a tail. Building a cycle with it is possible and will make print "
	"and equal loop forever.\n"
	"Errors: SET-CDR: NOT CONS if the first argument is not a cons pair.\n"
	"Bugs: A cyclic structure built with set-cdr cannot be printed or "
	"compared; SISP has no cycle detection yet.\n"
	"See: set-car cons cdr")
static objectp
builtin_set_cdr(const struct object *args)
{
	objectp pair = eval(car(args));
	objectp value = eval(cadr(args));
	require_type(pair, OBJ_CONS, "SET-CDR");
	pair->value.cons.cdr = value;
	return pair;
}

DOC("cat", "Concatenate strings.\n"
		   "  (cat \"a\" \"b\") => \"ab\"\n"
		   "  (cat) => \"\"\n"
		   "Errors: CAT: NOT STRING for a non-string argument.\n"
		   "See: substr list-to-str")
static objectp
builtin_cat(const struct object *args)
{
	struct list strings;
	objectp string, result, node;
	long int source_index, destination_index, total_length;

	total_length = 0;
	destination_index = 0;
	list_init(&strings, OBJ_CONS);

	/* Pass 1: evaluate each argument exactly once (they may have side
	   effects), type-check it, and sum the lengths.  The evaluated strings
	   are held in a fresh cons chain rather than a malloc'd array so that
	   a require_condition longjmp on a bad argument leaks nothing -- the chain
	   is GC-managed.  GC never runs mid-form, so it needs no rooting. */
	while (args != nil)
	{
		string = eval(car(args));
		require_type(string, OBJ_STRING, "CAT");
		total_length += string->value.string.length;
		list_append(&strings, string);
		args = cdr(args);
	}

	/* Pass 2: the total is known, so one exact allocation and no realloc. */
	result = make_string_buffer((size_t)total_length);
	node = strings.head;
	while (node != nil)
	{
		string = car(node);
		for (source_index = 0; source_index < string->value.string.length;
			 source_index++, destination_index++)
		{
			result->value.string.data[destination_index] =
				string->value.string.data[source_index];
		}
		node = cdr(node);
	}
	result->value.string.data[total_length] = '\0';
	return result;
}

DOC("substr", "The substring of a given length from a start index.\n"
			  "  (substr 1 2 \"hello\") => \"el\"\n"
			  "Errors: SUBSTR: NEGATIVE START INDEX, NEGATIVE LENGTH, or INDEX "
			  "OUT OF BOUNDS. A length past the end is clamped.\n"
			  "See: cat str-find length")
static objectp
builtin_substr(const struct object *args)
{
	objectp start = eval(car(args));
	objectp length = eval(cadr(args));
	objectp string = eval(car(cddr(args)));
	long int available, result_length;
	require_type(start, OBJ_INTEGER, "SUBSTR");
	require_type(length, OBJ_INTEGER, "SUBSTR");
	require_type(string, OBJ_STRING, "SUBSTR");

	if (start->value.i < 0)
		return sisp_errorf("SUBSTR: NEGATIVE START INDEX %ld", start->value.i);
	if (length->value.i < 0)
		return sisp_errorf("SUBSTR: NEGATIVE LENGTH %ld", length->value.i);
	if (start->value.i > string->value.string.length)
		return sisp_errorf("SUBSTR: INDEX OUT OF BOUNDS %ld", start->value.i);
	available = string->value.string.length - start->value.i;
	result_length = length->value.i < available ? length->value.i : available;
	return make_string(string->value.string.data + start->value.i,
					   (size_t)result_length);
}

/* a char is a one-character string; (str->list "ab") -> ("a" "b") */
DOC("str-to-list", "A string as a list of one-character strings.\n"
				   "  (str-to-list \"ab\") => (\"a\" \"b\")\n"
				   "See: list-to-str char-code")
static objectp
builtin_str_to_list(const struct object *args)
{
	struct list result;
	objectp string, character;
	int index;

	string = eval(car(args));
	require_type(string, OBJ_STRING, "STRTOLIST");
	list_init(&result, OBJ_CONS);
	for (index = 0; index < (int)string->value.string.length; index++)
	{
		character = make_string(string->value.string.data + index, 1);
		list_append(&result, character);
	}
	return result.head;
}

/* (list->set l) -> extension set with l's elements, deduplicated. nil -> {}.
   A set argument passes through unchanged; elements are already evaluated. */
DOC("list-to-set",
	"An extension set holding a list's elements, deduplicated.\n"
	"  (list-to-set (quote (1 2 1))) => {1 2}\n"
	"Note: Deduplication uses set equality, which treats 1 and 1.0 as the same "
	"number. dedup uses equal and keeps them apart.\n"
	"See: set-to-list dedup")
static objectp
builtin_list_to_set(const struct object *args)
{
	objectp list;
	struct list result;

	list = eval(car(args));
	if (list == nil)
		return empty;
	if (list->type == OBJ_SET)
		return list;
	require_condition((list->type == OBJ_CONS), "LISTTOSET", "NOT LIST", list);
	list_init(&result, OBJ_SET);
	for (; list != nil; list = cdr(list))
		list_append(&result, car(list));
	return eval_set(result.head);
}

/* (list->str ("a" "b" "c")) -> "abc"; rejects sets/comprehensions (not a list)
 */
DOC("list-to-str", "A list of one-character strings joined into one string.\n"
				   "  (list-to-str (quote (\"a\" \"b\"))) => \"ab\"\n"
				   "Errors: LISTTOSTR: NOT LIST, or NOT STRING ELEMENT.\n"
				   "See: str-to-list cat")
static objectp
builtin_list_to_str(const struct object *args)
{
	objectp list, node, result;
	size_t total = 0;
	int position = 0;
	list = eval(car(args));
	require_condition((list == nil || list->type == OBJ_CONS), "LISTTOSTR", "NOT LIST",
					  list);
	for (node = list; node != nil; node = cdr(node))
	{
		require_condition((node->type == OBJ_CONS), "LISTTOSTR", "NOT LIST", list);
		require_type(car(node), OBJ_STRING, "LISTTOSTR");
		total += car(node)->value.string.length;
	}
	result = make_string_buffer(total);
	for (node = list; node != nil; node = cdr(node))
	{
		memcpy(result->value.string.data + position, car(node)->value.string.data,
			   car(node)->value.string.length);
		position += (int)car(node)->value.string.length;
	}
	result->value.string.data[position] = '\0';
	result->value.string.length = position;
	return result;
}

/* (num->str 1/2) -> "1/2"; renders any number in reader-readable form */
DOC("num-to-str", "A number rendered in reader-readable form.\n"
				  "  (num-to-str 1/2) => \"1/2\"\n"
				  "See: str-to-num print")
static objectp
builtin_num_to_str(const struct object *args)
{
	objectp n;
	char buf[DOUBLE_TEXT_MAX];
	int len;
	n = eval(car(args));
	require_numeric(n, "NUMTOSTR");
	if (n->type == OBJ_INTEGER)
		len = snprintf(buf, sizeof(buf), "%ld", n->value.i);
	else if (n->type == OBJ_RATIONAL)
		len = snprintf(buf, sizeof(buf), "%ld/%ld", n->value.rational.n,
					   n->value.rational.d);
	else
		/* the same shortest-round-trip rendering the printer uses, so the
		   two cannot drift apart */
		len = format_double(buf, sizeof buf, n->value.f);
	return make_string(buf, (size_t)len);
}

/* (str->num "3/4") -> 3/4; parses int/rational/float, else nil */
DOC("str-to-num", "Parse a number from a string; nil when it is not one.\n"
				  "  (str-to-num \"3/4\") => 3/4\n"
				  "See: num-to-str read-from-string")
static objectp
builtin_str_to_num(const struct object *args)
{
	objectp string, result;
	const char *text, *slash;
	char *end, *denominator_end;
	size_t length;
	long int numerator, denominator, integer;
	double floating;
	string = eval(car(args));
	require_type(string, OBJ_STRING, "STRTONUM");
	text = string->value.string.data;
	length = string->value.string.length;
	if (length == 0)
		return nil;
	slash = memchr(text, '/', length);
	if (slash != NULL)
	{
		errno = 0;
		numerator = strtol(text, &end, 10);
		if (end != slash || errno)
			return nil;
		denominator = strtol(slash + 1, &denominator_end, 10);
		if (*denominator_end != '\0' || denominator_end == slash + 1 || errno ||
			denominator == 0)
			return nil;
		result = make_rational(numerator, denominator);
		return eval_rational(result);
	}
	if (memchr(text, '.', length) != NULL)
	{
		errno = 0;
		floating = strtod(text, &end);
		if (*end != '\0' || end == text || errno)
			return nil;
		return make_number_from_double(floating);
	}
	errno = 0;
	integer = strtol(text, &end, 10);
	if (*end != '\0' || end == text || errno)
		return nil;
	result = make_integer(integer);
	return result;
}

/* (str-find "hello" "lo") -> 3 (index of needle in haystack), else nil */
DOC("str-find", "Index of the first occurrence of a needle in a string, or nil.\n"
				"  (str-find \"hello\" \"ll\") => 2\n"
				"  (str-find \"hello\" \"z\") => nil\n"
				"Note: The string comes first, the needle second.\n"
				"See: substr contains")
static objectp
builtin_str_find(const struct object *args)
{
	objectp haystack, needle, result;
	size_t haystack_length, needle_length, index;
	haystack = eval(car(args));
	needle = eval(cadr(args));
	require_type(haystack, OBJ_STRING, "STRFIND");
	require_type(needle, OBJ_STRING, "STRFIND");
	haystack_length = haystack->value.string.length;
	needle_length = needle->value.string.length;
	if (needle_length > haystack_length)
		return nil;
	for (index = 0; index + needle_length <= haystack_length; index++)
		if (memcmp(haystack->value.string.data + index, needle->value.string.data,
				   needle_length) == 0)
		{
			result = make_integer((long int)index);
			return result;
		}
	return nil;
}

DOC("seq", "The integers from one bound to another, inclusive.\n"
		   "  (seq 1 5) => (1 2 3 4 5)\n"
		   "  (seq 3 1) => nil\n"
		   "See: list map")
static objectp
builtin_seq(const struct object *args)
{
	objectp start, end;
	long int value;
	struct list result;

	start = eval(car(args));
	end = eval(cadr(args));
	require_type(start, OBJ_INTEGER, "SEQ");
	require_type(end, OBJ_INTEGER, "SEQ");
	/* total on all bounds, following the CL loop / Racket range
	   convention: an empty range is a value, not an error */
	if (start->value.i > end->value.i)
		return nil;
	list_init(&result, OBJ_CONS);
	value = start->value.i;
	for (;;)
	{
		list_append(&result, make_integer(value));
		/* compare before incrementing: value++ at LONG_MAX would wrap */
		if (value == end->value.i)
			break;
		value++;
	}
	return result.head;
}

DOC("atom", "Whether a value is not a cons pair.\n"
			"  (atom 1) => t\n"
			"  (atom (quote (1))) => nil\n"
			"See: consp listp")
static objectp
builtin_atom(const struct object *args)
{
	switch (eval(car(args))->type)
	{
	case OBJ_T:
	case OBJ_NIL:
	case OBJ_IDENTIFIER:
	case OBJ_BUILTIN:
	case OBJ_INTEGER:
	case OBJ_RATIONAL:
	case OBJ_FLOAT:
	case OBJ_STRING:
	case OBJ_MATRIX:
	case OBJ_TAU:
		return t;
	case OBJ_CONS:
	case OBJ_SET:
		return nil;
	case OBJ_TYPE_COUNT:
		return sisp_error("ATOM: INVALID INTERNAL TYPE");
	}
	return sisp_error("ATOM: INVALID INTERNAL TYPE");
}

DOC("consp", "Whether a value is a cons pair. nil is not.\n"
			 "  (consp (quote (1))) => t\n"
			 "  (consp nil) => nil\n"
			 "See: listp atom null")
static objectp
builtin_consp(const struct object *args)
{
	if (eval(car(args))->type == OBJ_CONS)
		return t;
	return nil;
}

DOC("listp", "Whether a value is a proper list, including the empty one.\n"
			 "  (listp nil) => t\n"
			 "  (listp (quote (1 . 2))) => nil\n"
			 "See: consp null")
static objectp
builtin_listp(const struct object *args)
{
	objectp list = eval(car(args));
	if (list == nil)
		return t;
	if (list->type != OBJ_CONS)
		return nil;
	/* proper list: walk to the end, cdr must be nil */
	while (list->type == OBJ_CONS)
		list = cdr(list);
	return (list == nil) ? t : nil;
}

DOC("null", "Whether a value is nil.\n"
			"  (null nil) => t\n"
			"  (null 0) => nil\n"
			"Note: Unlike not, this accepts any value rather than only booleans.\n"
			"See: not endp")
static objectp
builtin_null(const struct object *args)
{
	if (eval(car(args)) == nil)
		return t;
	return nil;
}

DOC("symbolp", "Whether a value is an identifier.\n"
			   "  (symbolp (quote x)) => t\n"
			   "See: typeof")
static objectp
builtin_symbolp(const struct object *args)
{
	objectp value = eval(car(args));
	if (value->type == OBJ_IDENTIFIER || value == nil || value == t)
		return t;
	return nil;
}

DOC("endp", "Whether a list has ended.\n"
			"  (endp nil) => t\n"
			"Errors: Signals for a non-list, where null would answer nil.\n"
			"See: null listp")
static objectp
builtin_endp(const struct object *args)
{
	objectp list = eval(car(args));
	if (list == nil)
		return t;
	if (list->type == OBJ_CONS)
		return nil;
	return sisp_error("ENDP: NOT A LIST");
}

DOC("setp", "Whether a value is a set of either kind.\n"
			"  (setp {1}) => t\n"
			"See: ext-set-p comp-set-p")
static objectp
builtin_setp(const struct object *args)
{
	if (eval(car(args))->type == OBJ_SET)
		return t;
	return nil;
}

DOC("ext-set-p", "Whether a value is an extension set, one that lists its members.\n"
				 "  (ext-set-p {1}) => t\n"
				 "  (ext-set-p {}) => nil\n"
				 "See: setp comp-set-p")
static objectp
builtin_ext_set_p(const struct object *args)
{
	objectp set = eval(car(args));
	if (set->type == OBJ_SET && !object_is_comprehension_set(set))
		return t;
	return nil;
}

DOC("comp-set-p", "Whether a value is a comprehension set, one defined by a rule.\n"
				  "  (comp-set-p {1}) => nil\n"
				  "See: setp ext-set-p enumerate")
static objectp
builtin_comp_set_p(const struct object *args)
{
	objectp set = eval(car(args));
	if (set->type == OBJ_SET && object_is_comprehension_set(set))
		return t;
	return nil;
}

DOC("load", "Read and evaluate a file, then return t.\n"
			"Mutates: The environment, with everything the file defines.\n"
			"Errors: LOAD FAILED with the file name if any form in it fails, "
			"and FILE NOT FOUND if it does not exist. Both are catchable.\n"
			"Note: A failure inside the file aborts the rest of that file, and "
			"the caller sees one LOAD FAILED regardless of which form failed.\n"
			"See: read-file eval")
static objectp
builtin_load(const struct object *args)
{
	objectp source;
	size_t index, length;
	char *filename;
	char message[ERROR_MESSAGE_MAX];

	source = eval(car(args));
	/* A string is a path, loaded verbatim — any file, any directory. */
	if (source->type == OBJ_STRING)
	{
		if (!load_isolated(source->value.string.data))
		{
			return sisp_errorf("LOAD FAILED: %s", source->value.string.data);
		}
		return t;
	}
	/* A bare identifier loads <name>.lsp, lowercased, from the current dir. */
	require_condition((source->type == OBJ_IDENTIFIER), "LOAD", "NOT A STRING OR NAME",
					  source);
	length = strlen(source->value.identifier.name);
	filename = xmalloc(length + 5);
	snprintf(filename, length + 5, "%s.lsp", source->value.identifier.name);
	for (index = 0; index < length + 4; index++)
	{
		filename[index] = (char)tolower((unsigned char)filename[index]);
	}
	if (!load_isolated(filename))
	{
		snprintf(message, sizeof(message), "LOAD FAILED: %s", filename);
		free(filename);
		return sisp_error(message);
	}
	free(filename);
	return t;
}

/* Writes the text out verbatim rather than handing it to print_object: the
   printer escapes strings so they can be read back, which is right for data
   and wrong for a manual page. Nothing else needs to change for a doc to
   render with its newlines and tabs intact. */
DOC("doc", "Print the manual entry for a name.\n"
		   "  (progn (doc doc) (quote done)) => done\n"
		   "Note: The name is NOT evaluated. It cannot be: a prelude function "
		   "evaluates to a bare lambda that has lost its name, so evaluating "
		   "would work for the C builtins and fail for everything else.\n"
		   "See: typeof")
static objectp
builtin_doc(const struct object *args)
{
	objectp name = car(args);
	objectp text_object;
	const char *text;
	size_t length;

	require_condition((name->type == OBJ_IDENTIFIER), "DOC", "NOT A NAME", name);

	/* (doc name text) writes the entry; the text is evaluated, so it can be
	   computed rather than only written as a literal. */
	if (cdr(args) != nil)
	{
		text_object = eval(cadr(args));
		require_type(text_object, OBJ_STRING, "DOC");
		documentation_register(name->value.identifier.name,
							   text_object->value.string.data);
		return t;
	}

	/* (doc name) writes the entry out and answers whether there was one, so
	   the result is usable in a test rather than being the text itself --
	   which would print escaped through the string printer. */
	text = documentation_lookup(name->value.identifier.name, &length);
	if (text == NULL)
		return nil;
	fwrite(text, 1, length, stdout);
	fputc('\n', stdout);
	fflush(stdout);
	return t;
}

DOC("typeof", "The self-evaluating string naming a value's type.\n"
			  "  (typeof 1) => \"integer\"\n"
			  "  (typeof \"a\") => \"string\"\n"
			  "  (typeof (quote (1))) => \"cons\"\n"
			  "  (typeof {1}) => \"set\"\n"
			  "See: numberp symbolp consp")
static objectp
builtin_typeof(const struct object *args)
{
	const char *type;
	switch (eval(car(args))->type)
	{
	case OBJ_RATIONAL:
		type = "rational";
		break;
	case OBJ_STRING:
		type = "string";
		break;
	case OBJ_INTEGER:
		type = "integer";
		break;
	case OBJ_FLOAT:
		type = "float";
		break;
	case OBJ_MATRIX:
		type = "matrix";
		break;
	case OBJ_NIL:
		type = "nil";
		break;
	case OBJ_T:
		type = "t";
		break;
	case OBJ_CONS:
		type = "cons";
		break;
	case OBJ_SET:
		type = "set";
		break;
	case OBJ_TAU:
		type = "tau";
		break;
	case OBJ_IDENTIFIER:
	case OBJ_BUILTIN:
		type = "identifier";
		break;
	default:
		return sisp_error("TYPEOF: INVALID INTERNAL TYPE");
	}
	return make_string(type, strlen(type));
}

/* (length x) -- the size of any sequence: list, string, or extension set.
   This is the single size operator: set cardinality, list length, and
   character count. A comprehension set has no finite enumeration, so it is
   rejected rather than answered. */
DOC("length", "Size of any sequence: a list, a string, or an extension set.\n"
			  "  (length (quote (1 2 3))) => 3\n"
			  "  (length \"abc\") => 3\n"
			  "  (length {1 2 3}) => 3\n"
			  "  (length nil) => 0\n"
			  "Errors: LENGTH: NOT A SEQUENCE for anything else, including a "
			  "comprehension set, which has no finite enumeration. An improper "
			  "list is rejected too.\n"
			  "See: count")
static objectp
builtin_length(const struct object *args)
{
	objectp collection;
	enum object_type node_type;
	long int count;

	collection = eval(car(args));
	if (collection == nil || collection == empty)
		return make_integer(0L);
	if (collection->type == OBJ_STRING)
		return make_integer(collection->value.string.length);
	require_condition(
		(collection->type == OBJ_CONS ||
		 (collection->type == OBJ_SET && !object_is_comprehension_set(collection))),
		"LENGTH", "NOT A SEQUENCE", collection);
	node_type = collection->type;
	count = 0;
	while (collection != nil)
	{
		require_condition((collection->type == node_type), "LENGTH",
						  "IMPROPER SEQUENCE", collection);
		count++;
		collection = cdr(collection);
	}
	return make_integer(count);
}

DOC("nth", "The element at a zero-based index.\n"
		   "  (nth 0 (quote (a b c))) => a\n"
		   "  (nth 5 (quote (a b))) => nil\n"
		   "Errors: NTH: INDEX OUT OF BOUNDS for a negative index. Past the "
		   "end is nil.\n"
		   "See: car drop last")
static objectp
builtin_nth(const struct object *args)
{
	objectp list, index;
	int position = 0;
	index = eval(car(args));
	list = eval(cadr(args));
	require_type(index, OBJ_INTEGER, "NTH");
	if (index->value.i < 0)
		return sisp_errorf("NTH: INDEX '%ld' OUT OF BOUNDS", index->value.i);
	if (list == nil || list == empty)
	{
		return nil;
	}
	require_condition((list->type == OBJ_CONS && !object_is_dotted_pair(list)), "NTH",
					  "NON CONS ARGUMENT", list);
	if (index->value.i == 0)
		return car(list);
	do
	{
		position++;
	} while (((list = cdr(list)) != nil) && position < index->value.i);
	if (list == nil)
	{
		return nil;
	}
	return car(list);
}

DOC("cons", "Build a pair. With a list as the second argument, prepends to it.\n"
			"  (cons 1 (quote (2 3))) => (1 2 3)\n"
			"  (cons 1 2) => (1 . 2)\n"
			"See: car cdr list push")
static objectp
builtin_cons(const struct object *args)
{
	return cons(eval(car(args)), eval(cadr(args)));
}

DOC("list", "A fresh list of the evaluated arguments.\n"
			"  (list 1 2 3) => (1 2 3)\n"
			"  (list) => nil\n"
			"See: cons append")
static objectp
builtin_list(const struct object *args)
{
	struct list result;

	list_init(&result, OBJ_CONS);
	while (args != nil)
	{
		list_append(&result, eval(car(args)));
		args = cdr(args);
	}
	return result.head;
}

DOC("map", "Apply a function to every element, preserving list or finite-set type.\n"
		   "  (map (lambda (x) (* x 2)) (quote (1 2 3))) => (2 4 6)\n"
		   "  (map (lambda (x) (mod x 2)) {1 2 3}) => {0 1}\n"
		   "Note: Finite-set traversal order is unspecified, and duplicate results "
		   "collapse as they do in every set.\n"
		   "Errors: MAP: NOT A FINITE COLLECTION for a comprehension set or atom.\n"
		   "See: filter foldl")
static objectp
builtin_map(const struct object *args)
{
	objectp function, collection, node, call_node;
	struct list calls, result;
	enum object_type result_type;

	collection = eval(cadr(args));
	/* Resolve the function once, including any side effects. */
	function = eval(car(args));
	if (collection == nil)
		return nil;
	if (collection == empty)
		return empty;
	require_condition(
		(collection->type == OBJ_CONS ||
		 (collection->type == OBJ_SET && !object_is_comprehension_set(collection))),
		"MAP", "NOT A FINITE COLLECTION", collection);
	result_type = collection->type;

	/* Build every call form first, then evaluate them in order.  The two
	   passes are kept apart so a function that modifies the collection cannot
	   change the calls still to be made. */
	list_init(&calls, OBJ_CONS);
	node = collection;
	do
	{
		/* ((quote <function>) (quote <element>)). Both are quoted: the
		   function because a closure in operator position would otherwise be
		   evaluated again as a form, and the element so that a list-valued one
		   stays a single literal argument instead of splicing in as several. */
		list_append(&calls,
					cons(make_quoted(function), cons(make_quoted(car(node)), nil)));
	} while ((node = cdr(node)) != nil);

	list_init(&result, result_type);
	for (call_node = calls.head; call_node != nil; call_node = cdr(call_node))
		list_append(&result, eval(car(call_node)));
	return result_type == OBJ_SET ? eval_set(result.head) : result.head;
}

DOC("quit", "Exit, with an optional status code.\n"
			"Mutates: Ends the process; nothing after it runs.\n"
			"See: load")
static objectp
builtin_quit(const struct object *args)
{
	int exit_code = 0;
	if (car(args)->type == OBJ_INTEGER)
		exit_code = car(args)->value.i;
	pool_destroy();
	exit(exit_code);
	return nil;
}

DOC("assoc", "Look a key up in a list of (key value) pairs.\n"
			 "  (assoc (quote b) (quote ((a 1) (b 2)))) => (b 2)\n"
			 "  (assoc (quote z) (quote ((a 1)))) => nil\n"
			 "Note: An optional third argument names the comparison: eq or "
			 "equal. It defaults to equal.\n"
			 "See: zip get-names get-values")
static objectp
builtin_assoc(const struct object *args)
{
	objectp key, candidate_key, alist;
	/* equal, not eq: eq on a string means identity, so a string key would
	   find nothing unless the caller handed back the very same object, and
	   every string-keyed alist would silently fail -- see group-by in
	   lib/collections/lists.lsp. Common Lisp defaults to eql here and
	   inherits the same trap; Scheme's assoc uses equal? and does not. */
	enum equality_test test = TEST_EQUAL;
	key = eval(car(args));
	alist = eval(cadr(args));
	if (cddr(args) != nil)
		test = parse_equality_test(car(cddr(args)));
	do
	{
		candidate_key = caar(alist);
		if (objects_equal_by_test(key, candidate_key, test))
			return car(alist);
	} while ((alist = cdr(alist)) != nil);
	return nil;
}

static objectp
eval_equality(const struct object *arguments, enum equality_test test)
{
	objectp left = eval(car(arguments));
	objectp right = eval(cadr(arguments));
	return objects_equal_by_test(left, right, test) ? t : nil;
}

DOC("eq", "Identity for pairs, sets and strings; value for numbers and names.\n"
		  "  (eq 1 1) => t\n"
		  "  (eq (list 1 2) (list 1 2)) => nil\n"
		  "Note: Absorbed eql, which was the same code under another name. Two "
		  "separately built lists are never eq; use equal for structure.\n"
		  "See: equal =")
static objectp
builtin_eq(const struct object *args)
{
	return eval_equality(args, TEST_EQ);
}

DOC("equal", "Deep structural comparison.\n"
			 "  (equal (list 1 2) (list 1 2)) => t\n"
			 "  (equal \"AB\" \"ab\") => nil\n"
			 "See: eq =")
static objectp
builtin_equal(const struct object *args)
{
	return eval_equality(args, TEST_EQUAL);
}


DOC("pop", "Remove the first element from the list in a binding and return it.\n"
		   "  (progn (define s (list 1 2 3)) (list (pop s) s)) => (1 (2 3))\n"
		   "  (progn (define e nil) (pop e)) => nil\n"
		   "Mutates: The named binding, which is left holding the tail.\n"
		   "See: push car cdr")
static objectp
builtin_pop(const struct object *args)
{
	objectp list, value, rest;
	require_condition((car(args)->type == OBJ_IDENTIFIER), "POP", "NOT IDENTIFIER",
					  car(args));
	list = eval(car(args));
	if (list == nil)
		return nil;
	require_type(list, OBJ_CONS, "POP");
	value = car(list);
	rest = cdr(list);
	object_assign(car(args), rest);
	return value;
}

DOC("push", "Prepend a value to the list held in a binding, and store it back.\n"
			"  (progn (define s (list 2 3)) (push 1 s) s) => (1 2 3)\n"
			"Mutates: The named binding, not the list. The name must already be "
			"bound.\n"
			"See: pop cons")
static objectp
builtin_push(const struct object *args)
{
	objectp stack, element, result;
	require_condition((car(cdr(args))->type == OBJ_IDENTIFIER), "PUSH",
					  "NOT IDENTIFIER", car(cdr(args)));
	element = eval(car(args));
	stack = eval(cadr(args));
	result = cons(element, stack);
	object_assign(cadr(args), result);
	return result;
}

DOC("undef", "Remove a binding.\n"
			 "  (progn (define z 1) (undef z) (catch z (lambda (e) (quote "
			 "gone)))) => gone\n"
			 "Mutates: The environment: the name becomes unbound, and later "
			 "references to it fail.\n"
			 "See: define")
static objectp
builtin_undef(const struct object *args)
{
	objectp name;
	do
	{
		name = car(args);

		/* Raise rather than return nil: returning would stop the loop
		   silently, so (undef x 5 y) would unbind x, ignore y, and report
		   nothing. */
		require_condition((name->type == OBJ_IDENTIFIER), "UNDEF", "NOT IDENTIFIER",
						  name);
		object_unbind(name);
		/* the entry describes a binding that no longer exists */
		documentation_forget(name->value.identifier.name);
	} while ((args = cdr(args)) != nil);
	return t;
}

DOC("print", "Print values separated by spaces and return the last one, or nil "
			 "with no arguments.\n"
			 "  (progn (print \"\") (quote done)) => done\n"
			 "See: doc num-to-str write-file")
static objectp
builtin_print(const struct object *args)
{
	objectp result;

	result = nil;
	while (args != nil)
	{
		result = eval(car(args));
		print_object(stdout, result);
		fputc(' ', stdout);
		args = cdr(args);
	}
	fflush(stdout);
	return result;
}

DOC("numberp", "Whether a value is numeric.\n"
			   "  (numberp 1) => t\n"
			   "  (numberp \"1\") => nil\n"
			   "See: typeof symbolp")
static objectp
builtin_numberp(const struct object *args)
{
	if (object_is_numeric(eval(car(args))))
		return t;
	return nil;
}
DOC("runtime", "Processor time used so far, in seconds.\n"
			   "  (numberp (runtime)) => t\n"
			   "Note: Useful for timing, but it makes a program's output "
			   "differ between runs, so avoid printing it in a test.")
static objectp
builtin_runtime(const struct object *args)
{
	(void)args;
	return make_float((double)clock() / (double)CLOCKS_PER_SEC);
}

#ifdef SISP_DEBUG
DOC("trace", "Control evaluator tracing.\n"
			 "  (trace t) enables tracing; (trace nil) disables it.\n"
			 "See: print")
static objectp
builtin_trace(const struct object *args)
{
	objectp enabled;

	enabled = eval(car(args));
	require_boolean(enabled, "TRACE");
	trace_enabled = enabled != nil;
	return enabled;
}
#endif

/* ---- minimal file I/O (length-aware, embedded-NUL safe) ---- */
#define FILE_MAX (256UL * 1024 * 1024) /* refuse to slurp more than 256 MB */

DOC("read-file", "Read a whole file as a string.\n"
				 "Errors: Signals when the file cannot be read.\n"
				 "See: write-file load")
static objectp
builtin_read_file(const struct object *args)
{
	objectp path, result;
	FILE *file;
	long file_size;
	size_t bytes_read;
	path = eval(car(args));
	require_type(path, OBJ_STRING, "READ-FILE");
	file = fopen(path->value.string.data, "rb");
	/* Spelled out rather than left to require_condition, so that the handle is
	   visibly checked before it is used -- the same shape write_file_mode has.
	   sisp_raise(make_error_value(...)) is exactly what require_condition does,
	   so the raised error value is unchanged. */
	if (file == NULL)
		return sisp_raise(make_error_value("READ-FILE: FILE NOT FOUND", path));
	if (fseek(file, 0, SEEK_END) != 0 || (file_size = ftell(file)) < 0)
	{
		fclose(file);
		return sisp_error("READ-FILE: CANNOT SIZE FILE");
	}
	if ((unsigned long)file_size > FILE_MAX)
	{
		fclose(file);
		return sisp_error("READ-FILE: FILE TOO LARGE");
	}
	rewind(file);
	result = make_string_buffer((size_t)file_size);
	bytes_read = fread(result->value.string.data, 1, (size_t)file_size, file);
	if (bytes_read != (size_t)file_size)
	{
		fclose(file);
		return sisp_error("READ-FILE: SHORT READ");
	}
	fclose(file);
	if (memchr(result->value.string.data, '\0', bytes_read) != NULL)
		return sisp_error("READ-FILE: EMBEDDED NUL IS NOT TEXT");
	result->value.string.data[bytes_read] = '\0';
	result->value.string.length = bytes_read;
	return result;
}

/* (read-from-string s) -> a list of every top-level form parsed from s.
   The reader half of read/eval/print, exposing the same parser the REPL
   and `load` use. It follows read-file's whole-content convention — all
   forms, never a truncated prefix — rather than Common Lisp's one-form
   read: a single-form string yields a one-element list, the empty string
   yields nil, and `(car (read-from-string s))` recovers the classic
   single-form read. A malformed string raises a normal SISP error.
   The local reader makes this safe inside evaluation without swapping any
   state belonging to the enclosing REPL or file loader. */
/* Owns the setjmp so its caller does not have to, exactly as process.c's
   eval_with_recovery does. After a longjmp, a non-volatile automatic of the
   function that called setjmp holds an indeterminate value if it changed in
   between -- and `reader` changes on every token. Keeping setjmp in here means
   the caller's reader, result and input are not subject to that rule at all;
   only `failed` is read on the recovery path, and it is volatile. */
static int
read_every_form(struct reader *reader, struct list *result)
{
	volatile int failed;
	objectp form;

	failed = setjmp(reader->error_jmp);
	if (failed == 0)
		while ((form = parser_parse_object(reader)) != NULL)
			list_append(result, form);
	return failed;
}

DOC("read-from-string", "Parse SISP source text into a form, without evaluating it.\n"
						"  (read-from-string \"(1 2)\") => ((1 2))\n"
						"See: eval read-file")
static objectp
builtin_read_from_string(const struct object *args)
{
	struct reader reader;
	struct list result;
	objectp string;
	FILE *input;
	int failed;

	string = eval(car(args));
	require_type(string, OBJ_STRING, "READ-FROM-STRING");
	if (string->value.string.length == 0)
		return nil;

	input =
		fmemopen(string->value.string.data, (size_t)string->value.string.length, "r");
	if (input == NULL)
		return sisp_error("READ-FROM-STRING: CANNOT OPEN STRING");
	reader_init(&reader, input, "<string>");
	list_init(&result, OBJ_CONS);

	failed = read_every_form(&reader, &result);

	reader_destroy(&reader);
	fclose(input);
	if (failed)
		return sisp_error("READ-FROM-STRING: PARSE ERROR");
	return result.head;
}

static objectp
write_file_mode(const struct object *args, const char *mode, const char *operation)
{
	objectp path, str;
	FILE *fp;
	size_t wrote;
	path = eval(car(args));
	str = eval(cadr(args));
	require_type(path, OBJ_STRING, "WRITE-FILE");
	require_type(str, OBJ_STRING, "WRITE-FILE");
	fp = fopen(path->value.string.data, mode);
	/* Every failure path returns. sisp_errorf never comes back, but nothing in
	   its signature says so, and falling through here would reach
	   fwrite(..., NULL). Returning is also how the rest of this file spells a
	   failure -- see substr. */
	if (fp == NULL)
		return sisp_errorf("%s: CANNOT OPEN '%s'", operation,
						   path->value.string.data);
	wrote = fwrite(str->value.string.data, 1, str->value.string.length, fp);
	if (fclose(fp) != 0)
		return sisp_errorf("%s: WRITE FAILED", operation);
	if (wrote != (size_t)str->value.string.length)
		return sisp_errorf("%s: SHORT WRITE", operation);
	return make_integer((long int)wrote);
}

DOC("write-file", "Write a string to a file, replacing what was there.\n"
				  "Mutates: The named file on disk: its previous contents are lost.\n"
				  "See: append-file read-file")
static objectp
builtin_write_file(const struct object *args)
{
	return write_file_mode(args, "wb", "WRITE-FILE");
}
DOC("append-file", "Append a string to a file.\n"
				   "Mutates: The named file on disk, adding to the end rather "
				   "than replacing.\n"
				   "See: write-file read-file")
static objectp
builtin_append_file(const struct object *args)
{
	return write_file_mode(args, "ab", "APPEND-FILE");
}

void
builtins_init(void)
{
	size_t index;
	objectp name, builtin;

	for (index = 1; index < builtin_count; index++)
		if (strcmp(builtins[index - 1].name, builtins[index].name) >= 0)
		{
			fprintf(stderr, "; BUILTIN TABLE IS NOT SORTED: %s, %s\n",
					builtins[index - 1].name, builtins[index].name);
			exit(EXIT_FAILURE);
		}
	for (index = 0; index < builtin_count; index++)
	{
		name = intern_identifier(builtins[index].name);
		builtin = make_object(OBJ_BUILTIN);
		builtin->value.builtin = &builtins[index];
		name->value.identifier.binding = builtin;
	}
}
/* char-code and code-char cross the byte/integer boundary that pure SISP
   cannot: a character is a one-byte string. char-code yields the first byte
   (0-255) of a string; code-char builds the one-byte string for a byte value.
 */
DOC("char-code", "The code point of a one-character string.\n"
				 "  (char-code \"A\") => 65\n"
				 "See: code-char str-to-list")
static objectp
builtin_char_code(const struct object *args)
{
	objectp string = eval(car(args));
	require_type(string, OBJ_STRING, "CHAR-CODE");
	require_condition((string->value.string.length >= 1), "CHAR-CODE", "EMPTY STRING",
					  string);
	return make_integer((unsigned char)string->value.string.data[0]);
}

DOC("code-char", "The one-character string for a code point.\n"
				 "  (code-char 66) => \"B\"\n"
				 "See: char-code")
static objectp
builtin_code_char(const struct object *args)
{
	objectp code;
	char byte;

	code = eval(car(args));
	require_type(code, OBJ_INTEGER, "CODE-CHAR");
	require_condition((code->value.i >= 1 && code->value.i <= 255), "CODE-CHAR",
					  "TEXT BYTE OUT OF RANGE", code);
	byte = (char)code->value.i;
	return make_string(&byte, 1);
}

const struct builtin_spec builtins[] = {
	{"*", builtin_multiply, 0, ARITY_ANY},
	{"+", builtin_add, 0, ARITY_ANY},
	{"-", builtin_subtract, 1, ARITY_ANY},
	{"/", builtin_divide, 1, ARITY_ANY},
	{"/=", builtin_numeric_not_equal, 0, ARITY_ANY},
	{"<", builtin_less, 1, ARITY_ANY},
	{"<=", builtin_less_equal, 1, ARITY_ANY},
	{"<=>", builtin_iff, 2, ARITY_ANY},
	{"=", builtin_numeric_equal, 1, ARITY_ANY},
	{"=>", builtin_imply, 2, 2},
	{">", builtin_greater, 1, ARITY_ANY},
	{">=", builtin_greater_equal, 1, ARITY_ANY},
	{"^", builtin_power, 2, 2},
	{"abs", builtin_abs, 1, 1},
	{"acos", builtin_acos, 1, 1},
	{"acosh", builtin_acosh, 1, 1},
	{"and", builtin_and, 0, ARITY_ANY},
	{"append", builtin_append, 0, ARITY_ANY},
	{"append-file", builtin_append_file, 2, 2},
	{"apply", builtin_apply, 1, ARITY_ANY},
	{"asin", builtin_asin, 1, 1},
	{"asinh", builtin_asinh, 1, 1},
	{"assoc", builtin_assoc, 2, 3},
	{"atan", builtin_atan, 1, 1},
	{"atan2", builtin_atan2, 2, 2},
	{"atanh", builtin_atanh, 1, 1},
	{"atom", builtin_atom, 1, 1},
	{"bquote", builtin_bquote, 0, 1},
	{"butlast", builtin_butlast, 1, 1},
	{"cap", builtin_cap, 2, ARITY_ANY},
	{"car", builtin_car, 1, 1},
	{"cartesian-product", builtin_cartesian_product, 2, 2},
	{"cat", builtin_cat, 0, ARITY_ANY},
	{"catch", builtin_catch, 2, 2},
	{"cbrt", builtin_cbrt, 1, 1},
	{"cdr", builtin_cdr, 1, 1},
	{"ceiling", builtin_ceiling, 1, 1},
	{"char-code", builtin_char_code, 1, 1},
	{"code-char", builtin_code_char, 1, 1},
	{"comma", builtin_comma, 1, 1},
	{"comp-set-p", builtin_comp_set_p, 1, 1},
	{"complement", builtin_complement, 1, 1},
	{"cond", builtin_cond, 1, ARITY_ANY},
	{"cons", builtin_cons, 2, 2},
	{"consp", builtin_consp, 1, 1},
	{"contains", builtin_contains, 2, 2},
	{"cos", builtin_cos, 1, 1},
	{"cosh", builtin_cosh, 1, 1},
	{"dedup", builtin_dedup, 1, 1},
	{"define", builtin_define, 2, ARITY_ANY},
	{"defmacro", builtin_defmacro, 1, ARITY_ANY},
	{"diff", builtin_diff, 2, 2},
	{"doc", builtin_doc, 1, 2},
	{"drop", builtin_drop, 2, 2},
	{"endp", builtin_endp, 1, 1},
	{"eq", builtin_eq, 2, 2},
	{"equal", builtin_equal, 2, 2},
	{"error", builtin_error, 1, 2},
	{"eval", builtin_eval, 1, 1},
	{"evenp", builtin_evenp, 1, 1},
	{"exp", builtin_exp, 1, 1},
	{"ext-set-p", builtin_ext_set_p, 1, 1},
	{"flatten", builtin_flatten, 1, 1},
	{"floor", builtin_floor, 1, 1},
	{"funcall", builtin_funcall, 1, ARITY_ANY},
	{"gcd", builtin_gcd, 0, ARITY_ANY},
	{"hypot", builtin_hypot, 2, 2},
	{"if", builtin_if, 2, ARITY_ANY},
	{"in", builtin_in, 2, 3},
	{"index-of", builtin_index_of, 2, 2},
	{"labels", builtin_labels, 1, ARITY_ANY},
	{"last", builtin_last, 1, 1},
	{"lcm", builtin_lcm, 0, ARITY_ANY},
	{"length", builtin_length, 1, 1},
	{"let", builtin_let, 1, ARITY_ANY},
	{"let*", builtin_letstar, 1, ARITY_ANY},
	{"list", builtin_list, 0, ARITY_ANY},
	{"list-to-set", builtin_list_to_set, 1, 1},
	{"list-to-str", builtin_list_to_str, 1, 1},
	{"listp", builtin_listp, 1, 1},
	{"load", builtin_load, 1, 1},
	{"log", builtin_log, 1, 1},
	{"log10", builtin_log10, 1, 1},
	{"log2", builtin_log2, 1, 1},
	{"macroexpand", builtin_macroexpand, 1, 1},
	{"make-matrix", builtin_make_matrix, 2, 3},
	{"map", builtin_map, 2, 2},
	{"mat*", builtin_matrix_multiply, 2, 2},
	{"mat-add-row", builtin_mat_add_row, 2, 2},
	{"mat-axpy", builtin_mat_axpy, 3, 3},
	{"mat-cols", builtin_mat_cols, 1, 1},
	{"mat-div", builtin_mat_div, 2, 2},
	{"mat-gather-rows", builtin_mat_gather_rows, 2, 2},
	{"mat-max-axis", builtin_mat_max_axis, 2, 2},
	{"mat-mul", builtin_mat_mul, 2, 2},
	{"mat-norm", builtin_mat_norm, 1, 1},
	{"mat-ref", builtin_mat_ref, 3, 3},
	{"mat-rows", builtin_mat_rows, 1, 1},
	{"mat-scale-into", builtin_mat_scale_into, 2, 2},
	{"mat-scatter-add", builtin_mat_scatter_add, 3, 3},
	{"mat-set", builtin_mat_set, 4, 4},
	{"mat-softmax", builtin_mat_softmax, 1, 1},
	{"mat-sum", builtin_mat_sum, 1, 1},
	{"mat-sum-axis", builtin_mat_sum_axis, 2, 2},
	{"mat-transpose", builtin_mat_transpose, 1, 1},
	{"mat-zero", builtin_mat_zero, 1, 1},
	{"matrix-from-list", builtin_matrix_from_list, 3, 3},
	{"matrix-randn", builtin_matrix_randn, 2, 3},
	{"max", builtin_max, 1, ARITY_ANY},
	{"min", builtin_min, 1, ARITY_ANY},
	{"mod", builtin_mod, 2, 2},
	{"not", builtin_not, 1, 1},
	{"notin", builtin_notin, 2, 3},
	{"nth", builtin_nth, 2, 2},
	{"null", builtin_null, 1, 1},
	{"num-to-str", builtin_num_to_str, 1, 1},
	{"numberp", builtin_numberp, 1, 1},
	{"oddp", builtin_oddp, 1, 1},
	{"or", builtin_or, 0, ARITY_ANY},
	{"pop", builtin_pop, 1, 1},
	{"powerset", builtin_powerset, 1, 1},
	{"print", builtin_print, 0, ARITY_ANY},
	{"prog1", builtin_prog1, 1, ARITY_ANY},
	{"prog2", builtin_prog2, 1, ARITY_ANY},
	{"progn", builtin_progn, 0, ARITY_ANY},
	{"push", builtin_push, 2, 2},
	{"quit", builtin_quit, 0, 1},
	{"quote", builtin_quote, 1, 1},
	{"random", builtin_random, 0, 1},
	{"rationalize", builtin_rationalize, 1, 2},
	{"read-file", builtin_read_file, 1, 1},
	{"read-from-string", builtin_read_from_string, 1, 1},
	{"relu", builtin_relu, 1, 1},
	{"reverse", builtin_reverse, 1, 1},
	{"round", builtin_round, 1, 1},
	{"runtime", builtin_runtime, 0, 0},
	{"seed", builtin_seed, 1, 1},
	{"seq", builtin_seq, 2, 2},
	{"set-car", builtin_set_car, 2, 2},
	{"set-cdr", builtin_set_cdr, 2, 2},
	{"set-to-list", builtin_set_to_list, 1, 1},
	{"setp", builtin_setp, 1, 1},
	{"setq", builtin_setq, 2, ARITY_ANY},
	{"signum", builtin_signum, 1, 1},
	{"sin", builtin_sin, 1, 1},
	{"sinh", builtin_sinh, 1, 1},
	{"sort", builtin_sort, 2, 2},
	{"sqrt", builtin_sqrt, 1, 1},
	{"str-find", builtin_str_find, 2, 2},
	{"str-to-list", builtin_str_to_list, 1, 1},
	{"str-to-num", builtin_str_to_num, 1, 1},
	{"subset", builtin_subset, 2, 2},
	{"subst", builtin_subst, 2, 3},
	{"substr", builtin_substr, 3, 3},
	{"symbolp", builtin_symbolp, 1, 1},
	{"symdiff", builtin_symdiff, 2, 2},
	{"take", builtin_take, 2, 2},
	{"tan", builtin_tan, 1, 1},
	{"tanh", builtin_tanh, 1, 1},
#ifdef SISP_DEBUG
	{"trace", builtin_trace, 1, 1},
#endif
	{"truncate", builtin_truncate, 1, 1},
	{"typeof", builtin_typeof, 1, 1},
	{"undef", builtin_undef, 1, ARITY_ANY},
	{"union", builtin_union, 1, ARITY_ANY},
	{"write-file", builtin_write_file, 2, 2},
	{"xor", builtin_xor, 1, ARITY_ANY},
	{"zip", builtin_zip, 2, 2},
};

const size_t builtin_count = sizeof(builtins) / sizeof(builtins[0]);
