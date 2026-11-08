#include <setjmp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "sisp.h"
#include "eval.h"
#include "runtime.h"
#include "print.h"
#include "reader.h"

uintptr_t stack_guard;
int in_load;

/* Set by -i. Forces the session behaviour on when stdin is not a terminal --
   the same thing python -i does, and what the golden corpus needs: it feeds
   ~25k expressions on a pipe and reads their echoed values. */
int force_interactive;

struct recovery_context *current_recovery = NULL;

void
runtime_recover(void)
{
	/* Only reachable before the first form loop runs -- an allocation
	   failure while main builds the embedded prelude, say. There is no frame
	   to unwind to, and jumping through an uninitialised buffer would be far
	   worse than stopping. */
	if (current_recovery == NULL)
	{
		fputs("; FATAL: RUNTIME FAILURE BEFORE ANY RECOVERY POINT\n", stderr);
		exit(EXIT_FAILURE);
	}
	longjmp(current_recovery->jump, 1);
}

/* Owns the whole setjmp contract so its callers do not have to. After a
   longjmp, a non-volatile automatic of the function that called setjmp holds
   an indeterminate value if it was modified in between; keeping setjmp in
   here means process_stream's locals are not subject to that rule at all.
   Only `recovered` is read on the recovery path, and it is volatile.

   gcc's -Wclobbered flags `form` here. That one is conservative rather than
   real: `form` is a by-value parameter, is only ever read, and is not touched
   at all on the recovery path -- the rule bites only for locals CHANGED
   between setjmp and longjmp. Left as-is rather than marking a parameter
   volatile to quiet a warning clang does not raise. */
static int
eval_with_recovery(objectp form, objectp *result)
{
	struct recovery_context frame;
	volatile int recovered;

	frame.previous = current_recovery;
	current_recovery = &frame;

	recovered = setjmp(frame.jump);
	if (recovered == 0)
		*result = eval(form);
	else
		*result = NULL;

	current_recovery = frame.previous;
	return recovered;
}

static int
process_stream(FILE *input, bool interactive, const char *source_name)
{
	struct reader reader;
	objectp form = NULL;
	objectp result = NULL;
	volatile int parse_failed, eval_failed;
	/* volatile for the same reason as the two above: this function contains a
	   setjmp, and a non-volatile local changed after it holds an indeterminate
	   value once a longjmp fires. Safe today only because the one assignment
	   is immediately followed by break; volatile makes that structural instead
	   of argued, and silences gcc's -Wclobbered. */
	volatile int success = 1;

	reader_init(&reader, input, source_name);
	for (;;)
	{
		parse_failed = 0;
		eval_failed = 0;

		/* The prompt is chrome, not data, so it goes to stderr -- every other
		   interpreter does this, and it is what let `sisp prog.lsp > out`
		   put ": " in the output file. Values still go to stdout below. */
		if (interactive)
			fputs(": ", stderr);
		if (setjmp(reader.error_jmp) == 0)
			form = parser_parse_object(&reader);
		else
		{
			form = NULL;
			parse_failed = 1;
		}
		reader_reset(&reader);

		if (form != NULL)
		{
			/* One frame per form. A nested load pushes its own inside eval
			   and pops it again, so this one is still the innermost when
			   control comes back. */
			eval_failed = (eval_with_recovery(form, &result) != 0);
			current_env = nil;
			error_state_reset();
		}
		/* A clean NULL is parser_parse_object's EOF result. Reader errors
		   arrive through error_jmp, never as NULL. */
		else if (!parse_failed)
			break;

		if (!interactive && (parse_failed || eval_failed))
		{
			if (parse_failed)
				fputs("; PARSER ERROR: MALFORMED FORM, REMAINDER OF INPUT "
					  "SKIPPED\n",
					  stderr);
			success = 0;
			break;
		}

		if (interactive)
		{
			if (parse_failed)
				fputs("; PARSER ERROR.\n", stderr);
			else if (form != NULL && result != NULL)
				print_object(stdout, result);
			fputc('\n', stdout);
		}
		fflush(stdout);
		if (in_load == 0)
			gc_collect();
	}

	reader_destroy(&reader);
	return success;
}

int
process_file_stream(FILE *input)
{
	return process_stream(input, false, "<prelude>");
}

int
process_input(const char *filename)
{
	FILE *input;
	int success;

	/* Echo values and prompt for terminal input or an explicit -i session. */
	if (filename == NULL || strcmp(filename, "-") == 0)
		return process_stream(stdin, force_interactive || isatty(STDIN_FILENO),
							  "<stdin>");
	input = fopen(filename, "r");
	if (input == NULL)
	{
		fprintf(stderr, "; %s: FILE NOT FOUND\n", filename);
		return 0;
	}
	success = process_stream(input, false, filename);
	fclose(input);
	return success;
}
