#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <setjmp.h>
#include <time.h>

#include "sisp.h"
#include "extern.h"
#include "eval.h"
#include "misc.h"

jmp_buf jb, je;
int frame = 0;
void clean_buffers(void)
{
	free(token_buffer);
	fflush(stdout);
}

void process_file(void)
{
	objectp p = NULL;
	init_lex();
	while (true)
	{
		p = (!setjmp(jb)) ? parse_object(0) : NULL;
		done_lex();
		if (p != NULL)
		{
			if (!setjmp(je))
			{
				eval(p);
			}
			current_env = nil;
		}
		else
			break;
		garbage_collect();
	}
	clean_buffers();
}
static void
process_stdin(void)
{
	objectp p, q;
	p = q = NULL;
	init_lex();

	while (1)
	{
		if (feof(input_file))
			break;
		printf(": ");
		p = (!setjmp(jb)) ? parse_object(0) : NULL;
		done_lex();
		if (p != NULL)
		{
			if (!setjmp(je))  {
				q = eval(p);
			} else {
				q = NULL;
			}
			current_env = nil;
		}
		else
		{
			if (feof(input_file))
				break;
			fprintf(stderr, "; PARSER ERROR.");
		}
		if (p != NULL && q != NULL)
			princ_object(stdout, q);
		puts("");
		garbage_collect();
	}

	free(token_buffer);
}

void process_input(const char *filename)
{
	if (filename != NULL && strcmp(filename, "-") != 0)
	{
		if ((input_file = fopen(filename, "r")) == NULL)
		{
			fprintf(stderr, "; %s: FILE NOT FOUND\n", filename);
			return;
		}
	}
	else
		input_file = stdin;
	if (input_file != stdin)
	{
		process_file();
		fclose(input_file);
	}
	else
		process_stdin();
}
