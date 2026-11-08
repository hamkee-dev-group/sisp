#include "sisp.h"
#include "runtime.h"
#include "eval.h"
#include "comprehension.h"
#include "special.h"
#include "doc.h"

DOC("if", "One of two expressions, by a test.\n"
		  "  (if t 1 2) => 1\n"
		  "  (if nil 1 2) => 2\n"
		  "See: cond")
objectp
builtin_if(const struct object *args)
{
	if (eval(car(args)) != nil)
	{
		tail_call_pending = 1;
		return cadr(args);
	}
	args = cddr(args);
	if (args == nil)
		return nil;
	while (cdr(args) != nil)
	{
		eval(car(args));
		args = cdr(args);
	}
	tail_call_pending = 1;
	return car(args);
}

DOC("cond", "The first branch whose test is true.\n"
			"  (cond (nil 1) (t 2)) => 2\n"
			"See: if")
objectp
builtin_cond(const struct object *args)
{
	objectp test, clause;
	do
	{
		clause = car(args);
		test = eval(car(clause));
		if (test != nil)
			return cdr(clause) == nil ? test : builtin_progn(cdr(clause));
	} while ((args = cdr(args)) != nil);
	return nil;
}

DOC("quote", "The argument, unevaluated.\n"
			 "  (quote (a b)) => (a b)\n"
			 "See: bquote comma eval")
objectp
builtin_quote(const struct object *args)
{
	return car(args);
}

DOC("bquote", "Backquote: quote a structure, but evaluate the comma parts.\n"
			  "  (bquote (1 (comma (+ 1 1)))) => (1 2)\n"
			  "See: quote comma defmacro")
objectp
builtin_bquote(const struct object *args)
{
	if (args == nil)
		return nil;
	return eval_bquote(car(args));
}

DOC("comma", "Inside bquote, marks a part to evaluate rather than quote.\n"
			 "  (bquote ((comma (+ 1 1)))) => (2)\n"
			 "See: bquote")
objectp
builtin_comma(const struct object *args)
{
	tail_call_pending = 1;
	return car(args);
}

DOC("progn", "Evaluate expressions in order; the value is the last.\n"
			 "  (progn 1 2 3) => 3\n"
			 "See: prog1 prog2")
objectp
builtin_progn(const struct object *args)
{
	args = eval_body_prefix((objectp)args);
	tail_call_pending = 1;
	return car(args);
}

DOC("prog1", "Evaluate expressions in order; the value is the first.\n"
			 "  (prog1 1 2 3) => 1\n"
			 "See: progn prog2")
objectp
builtin_prog1(const struct object *args)
{
	objectp result = eval(car(args));

	args = cdr(args);
	if (args == nil)
		return result;
	do
	{
		eval(car(args));
	} while ((args = cdr(args)) != nil);
	return result;
}

DOC("prog2", "Evaluate expressions in order; the value is the second.\n"
			 "  (prog2 1 2 3) => 2\n"
			 "See: progn prog1")
objectp
builtin_prog2(const struct object *args)
{
	objectp result;

	eval(car(args));
	args = cdr(args);
	if (args == nil)
		return nil;
	result = eval(car(args));
	args = cdr(args);
	if (args == nil)
		return result;
	do
	{
		eval(car(args));
	} while ((args = cdr(args)) != nil);
	return result;
}

/* Copy parameters so caller mutations cannot change an existing definition. */
static objectp
copy_parameter_list(objectp parameters)
{
	struct list result;
	objectp node;

	list_init(&result, OBJ_CONS);
	for (node = parameters; node != nil; node = cdr(node))
		list_append(&result, car(node));
	return result.head;
}

static objectp
define_function(const struct object *args)
{
	objectp header, parameters, body, callable;

	header = car(args);
	parameters = cdr(header);
	body = cdr(args);
	validate_parameter_names(parameters, "DEFINE");
	parameters = copy_parameter_list(parameters);
	/* A leading string is documentation, not a value -- but only when a form
	   follows it, so (define (f x) "hi") still returns "hi". */
	body = documentation_capture(car(header), body);
	if (car(body)->type == OBJ_SET && object_is_comprehension_set(car(body)))
		body->value.cons.car = eval_set(car(body));
	callable = cons(parameters, body);
	if (current_env == nil)
		callable = cons(id_lambda, callable);
	else
		callable = cons(id_closure, cons(current_env, callable));
	object_bind(car(header), callable);
	return callable;
}
DOC("define",
	"Bind a name, to a value or to a function.\n"
	"  (progn (define x 5) x) => 5\n"
	"  (progn (define (sq n) (* n n)) (sq 4)) => 16\n"
	"Mutates: The environment: it creates or replaces a binding in the current "
	"scope. Redefining a name any later code depends on changes that code's "
	"behaviour.\n"
	"Errors: SET OBJECT: name IS A BUILTIN FUNCTION when the name is a C "
	"builtin. Prelude definitions can be redefined; builtins cannot.\n"
	"See: setq undef labels")
objectp
builtin_define(const struct object *args)
{
	objectp value;

	if (car(args)->type == OBJ_CONS)
		return define_function(args);
	do
	{
		value = eval(cadr(args));
		object_bind(car(args), value);
	} while ((args = cddr(args)) != nil);
	return value;
}

DOC("setq", "Assign to an existing binding.\n"
			"  (progn (define y 1) (setq y 2) y) => 2\n"
			"Mutates: The nearest existing binding of the name, rather than creating "
			"one in the current scope the way define does.\n"
			"Errors: SET OBJECT: name IS A BUILTIN FUNCTION for a builtin name.\n"
			"See: define push pop")
objectp
builtin_setq(const struct object *args)
{
	/* CL-style (setq name value ...) — value bindings only. */
	objectp value;
	do
	{
		value = eval(cadr(args));
		object_assign(car(args), value);
	} while ((args = cddr(args)) != nil);
	return value;
}

DOC("let", "Bind names to values for one body. All the values are computed "
		   "first, so a binding cannot refer to an earlier one.\n"
		   "  (let ((x 1) (y 2)) (+ x y)) => 3\n"
		   "Note: A builtin name can be bound, and reading it gives the bound "
		   "value, but CALLING it still reaches the builtin -- builtin names "
		   "are reserved in operator position.\n"
		   "  (let ((car 5)) car) => 5\n"
		   "  (let ((car (lambda (x) 42))) (car (quote (1 2)))) => 1\n"
		   "Prelude and user functions shadow normally; only the C builtins "
		   "are reserved.\n"
		   "See: let* labels define")
objectp
builtin_let(const struct object *args)
{
	objectp bindings, body, binding_node, value;
	objectp environment, environment_node, binding;
	objectp value_node;
	struct list values;

	bindings = car(args);
	if (bindings->type != OBJ_CONS)
		return builtin_progn(cdr(args));
	body = cdr(args);

	/* First pass: evaluate all initforms before installing any binding. */
	list_init(&values, OBJ_CONS);
	do
	{
		require_condition((caar(bindings)->type == OBJ_IDENTIFIER), "LET",
						  "CANNOT BE A BINDING NAME", caar(bindings));
		value = eval(cadr(car(bindings)));
		list_append(&values, value);
	} while ((bindings = cdr(bindings)) != nil);

	/* Second pass: build new environment with all bindings. */
	environment = current_env;
	binding_node = car(args);
	value_node = values.head;
	do
	{
		binding = cons(caar(binding_node), car(value_node));
		environment_node = cons(binding, environment);
		environment = environment_node;
		value_node = cdr(value_node);
	} while ((binding_node = cdr(binding_node)) != nil);

	current_env = environment;
	body = eval_body_prefix(body);
	tail_call_pending = 1;
	return car(body);
}

DOC("let*", "Like let, but each binding can use the ones before it.\n"
			"  (let* ((x 1) (y (+ x 1))) y) => 2\n"
			"See: let")
objectp
builtin_letstar(const struct object *args)
{
	objectp bindings, body, value, binding, environment_node;

	bindings = car(args);
	if (bindings->type != OBJ_CONS)
		return builtin_progn(cdr(args));
	body = cdr(args);

	do
	{
		require_condition((caar(bindings)->type == OBJ_IDENTIFIER), "LET*",
						  "CANNOT BE A BINDING NAME", caar(bindings));
		value = eval(cadr(car(bindings)));
		binding = cons(caar(bindings), value);
		environment_node = cons(binding, current_env);
		current_env = environment_node;
	} while ((bindings = cdr(bindings)) != nil);

	body = eval_body_prefix(body);
	tail_call_pending = 1;
	return car(body);
}

DOC("labels", "Define local, mutually recursive functions for one expression.\n"
			  "  (labels (((f n) (cond ((= n 0) 1) (t (* n (f (- n 1))))))) (f "
			  "4)) => 24\n"
			  "Note: The functions have no global name, which is how the "
			  "prelude hides its own helpers.\n"
			  "See: define let")
objectp
builtin_labels(const struct object *args)
{
	objectp definitions, body, definition_node, definition;
	objectp header, parameters, parameter_node, body_node, function_name;
	objectp environment_node, binding, callable;
	struct list environment;

	definitions = car(args);
	require_condition(((definitions->type == OBJ_CONS && definitions != nil)), "LABELS",
					  "EMPTY CONS", definitions);
	body = cdr(args);
	require_condition(((body->type == OBJ_CONS && body != nil)), "LABELS", "EMPTY CONS",
					  body);
	for (definition_node = definitions; definition_node != nil;
		 definition_node = cdr(definition_node))
	{
		require_condition((definition_node->type == OBJ_CONS), "LABELS",
						  "IMPROPER DEFINITIONS LIST", definition_node);
		definition = car(definition_node);
		require_condition((definition->type == OBJ_CONS), "LABELS",
						  "MALFORMED DEFINITION", definition);
		header = car(definition);
		require_condition((header->type == OBJ_CONS), "LABELS",
						  "MALFORMED FUNCTION HEADER", header);
		function_name = car(header);
		require_condition((function_name->type == OBJ_IDENTIFIER), "LABELS",
						  "FUNCTION NAME IS NOT AN IDENTIFIER", function_name);
		parameters = cdr(header);
		for (parameter_node = parameters;
			 parameter_node != nil && parameter_node->type == OBJ_CONS;
			 parameter_node = cdr(parameter_node))
			;
		require_condition((parameter_node == nil), "LABELS", "IMPROPER PARAMETER LIST",
						  parameters);
		validate_parameter_names(parameters, "LABELS");
		body_node = cdr(definition);
		require_condition((body_node != nil), "LABELS", "MISSING FUNCTION BODY",
						  definition);
		for (; body_node != nil && body_node->type == OBJ_CONS;
			 body_node = cdr(body_node))
			;
		require_condition((body_node == nil), "LABELS", "IMPROPER FUNCTION BODY",
						  definition);
	}

	/* Step 1: create env entries with placeholders for all function names. */
	list_init(&environment, OBJ_CONS);
	definition_node = definitions;
	do
	{
		function_name = car(caar(definition_node));
		binding = cons(function_name, nil);
		list_append(&environment, binding);
	} while ((definition_node = cdr(definition_node)) != nil);

	/* Link placeholder entries to current_env. */
	environment.tail->value.cons.cdr = current_env;
	current_env = environment.head;

	/* Step 2: create closures capturing current_env, fill in placeholders. */
	environment_node = environment.head;
	definition_node = definitions;
	do
	{
		callable = cons(cdr(caar(definition_node)), cdar(definition_node));
		callable = cons(id_closure, cons(current_env, callable));
		car(environment_node)->value.cons.cdr = callable;
		environment_node = cdr(environment_node);
	} while ((definition_node = cdr(definition_node)) != nil);

	/* Step 3: evaluate body. */
	body = eval_body_prefix(body);
	tail_call_pending = 1;
	return car(body);
}

DOC("defmacro", "Define a macro: a function from unevaluated forms to a form.\n"
				"Mutates: The environment, as define does.\n"
				"See: macroexpand bquote define")
objectp
builtin_defmacro(const struct object *args)
{
	objectp function_name, body;

	function_name = car(car(args));
	validate_parameter_names(cdr(car(args)), "DEFMACRO");
	body = cons(id_macro,
				cons(cdr(car(args)), documentation_capture(function_name, cdr(args))));
	object_bind(function_name, body);

	return body;
}

DOC("macroexpand", "The form a macro call expands to, without evaluating it.\n"
				   "See: defmacro")
objectp
builtin_macroexpand(const struct object *args)
{
	objectp form, macro_def;
	int steps = 0;

	form = eval(car(args));
	while (form->type == OBJ_CONS && car(form)->type == OBJ_IDENTIFIER &&
		   object_is_macro(macro_def = object_try_function(car(form))))
	{
		form = expand_macro(macro_def, cdr(form), 0);
		require_condition((++steps < 100000), "MACROEXPAND",
						  "EXPANSION DID NOT TERMINATE", form);
	}
	return form;
}

DOC("eval", "Evaluate a structure as code.\n"
			"  (eval (quote (+ 1 2))) => 3\n"
			"See: quote apply read-from-string")
objectp
builtin_eval(const struct object *args)
{
	objectp form = eval(car(args));
	tail_call_pending = 1;
	return form;
}

DOC("funcall", "Call a function with arguments given individually.\n"
			   "  (funcall + 1 2) => 3\n"
			   "See: apply")
objectp
builtin_funcall(const struct object *args)
{
	return apply_function(eval(car(args)), cdr(args), "FUNCALL");
}

DOC("apply", "Call a function with a list of arguments.\n"
			 "  (apply + (quote (1 2 3))) => 6\n"
			 "See: funcall eval")
objectp
builtin_apply(const struct object *args)
{
	objectp function, argument_node, list;
	struct list arguments;

	function = eval(car(args));
	argument_node = cdr(args);
	list_init(&arguments, OBJ_CONS);
	while (argument_node != nil && cdr(argument_node) != nil)
	{
		list_append(&arguments, car(argument_node));
		argument_node = cdr(argument_node);
	}
	list = (argument_node == nil) ? nil : eval(car(argument_node));
	require_condition((list == nil || list->type == OBJ_CONS || list->type == OBJ_SET),
					  "APPLY", "NOT LIST", list);
	while (list != nil)
	{
		list_append(&arguments, make_quoted(car(list)));
		list = cdr(list);
	}
	return apply_function(function, arguments.head, "APPLY");
}

DOC("subst", "Replace every occurrence of one value with another, throughout a "
			 "structure.\n"
			 "  (subst (quote x) (quote y) (quote (x y x))) => (y y y)\n"
			 "Mutates: Nothing; the result is a new structure.\n"
			 "See: map")
objectp
builtin_subst(const struct object *args)
{
	objectp symbol, value, body;

	value = eval(car(args));
	if (value->type == OBJ_CONS)
	{
		body = eval(car(cdr(args)));
		if (body == nil)
			return nil;
		require_type(body, OBJ_CONS, "SUBST");
		do
		{
			body = substitute_tree(car(car(value)), cdr(car(value)), body);
		} while ((value = cdr(value)) != nil);
		return body;
	}
	else
	{
		symbol = eval(car(cdr(args)));
		body = eval(car(cddr(args)));
		if (body == nil)
			return nil;
		require_type(body, OBJ_CONS, "SUBST");
		return substitute_tree(value, symbol, body);
	}
}
