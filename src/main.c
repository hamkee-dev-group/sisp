#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>

#include "sisp.h"
#include "runtime.h"
#include "eval.h"
#include "print.h"
#include "libsisp.h"

#define STACK_MARGIN (256UL * 1024)
#define STACK_DEFAULT (512UL * 1024 * 1024)

int
main(int argc, char **argv)
{
	(void)argc; /* the argv walk below is NULL-terminated */
	char stack_top;
	struct rlimit rl;
	rlim_t budget;
	size_t total, length, index;
	char *prelude, *cursor;
	FILE *prelude_stream;
	int status = 0;

	if (getrlimit(RLIMIT_STACK, &rl) == -1)
	{
		budget = STACK_DEFAULT;
	}
	else
	{
		budget = (rl.rlim_cur == RLIM_INFINITY) ? STACK_DEFAULT : (rlim_t)rl.rlim_cur;
	}
	stack_guard = (uintptr_t)&stack_top - (uintptr_t)budget + STACK_MARGIN;

	object_init();
	builtins_init();
	runtime_identifiers_init();

	total = 0;
	for (index = 0; builtin_lines[index] != NULL; index++)
		total += strlen(builtin_lines[index]);
	prelude = xmalloc(total + 1);
	cursor = prelude;
	for (index = 0; builtin_lines[index] != NULL; index++)
	{
		length = strlen(builtin_lines[index]);
		memcpy(cursor, builtin_lines[index], length);
		cursor += length;
	}
	*cursor = '\0';
	prelude_stream = fmemopen(prelude, total, "r");

	if (!process_file_stream(prelude_stream))
	{
		fputs("; PRELUDE FAILED TO LOAD\n", stderr);
		fclose(prelude_stream);
		free(prelude);
		return EXIT_FAILURE;
	}
	fclose(prelude_stream);
	free(prelude);

	/* -i forces the session behaviour even when stdin is a pipe, as python -i
	   does. Without it, stdin is a session only when it is a terminal. */
	while (*++argv)
	{
		if (strcmp(*argv, "-i") == 0)
		{
			force_interactive = 1;
			continue;
		}
		if (!process_input(*argv))
			status = 1;
	}

	/* Read stdin after file arguments, entering a REPL for terminal input. */
	process_input(NULL);

	return status;
}
