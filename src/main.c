#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <setjmp.h>
#include <time.h>
#include <sys/resource.h>

#include "sisp.h"
#include "extern.h"
#include "eval.h"
#include "misc.h"
#include "libsisp.h"

int main(int argc, char **argv)
{
	struct rlimit rl;
	getrlimit(RLIMIT_STACK, &rl);
	rl.rlim_cur = rl.rlim_max;
	setrlimit(RLIMIT_STACK, &rl);
	getrlimit(RLIMIT_RSS, &rl);
	rl.rlim_cur = rl.rlim_max;
	setrlimit(RLIMIT_RSS, &rl);
	getrlimit(RLIMIT_DATA, &rl);
	rl.rlim_cur = rl.rlim_max;
	setrlimit(RLIMIT_DATA, &rl);
	init_objects();
	input_file = fmemopen(builtin_functions, strlen(builtin_functions), "r");
	process_file();
	fclose(input_file);
	if (argc > 1)
		while (*++argv)
			process_input(*argv);

	process_input(NULL);

	return 0;
}
