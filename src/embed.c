/* Convert prelude.lsp to libsisp.h at build time. Each source line becomes a
   separate C literal to stay within the standard's string-length limit.
   main.c joins the literals at startup. */

#include <stdio.h>

int
main(int argc, char **argv)
{
	FILE *input = stdin;
	int character;
	int at_line_start = 1;

	if (argc > 1)
	{
		input = fopen(argv[1], "r");
		if (input == NULL)
		{
			fprintf(stderr, "embed: cannot read %s\n", argv[1]);
			return 1;
		}
	}

	puts("/* generated from prelude.lsp -- do not edit */");
	puts("#ifndef LIBSISP_H");
	puts("static const char *const builtin_lines[] = {");

	while ((character = getc(input)) != EOF)
	{
		if (at_line_start)
		{
			fputs("\t\"", stdout);
			at_line_start = 0;
		}
		switch (character)
		{
		case '\\':
			fputs("\\\\", stdout);
			break;
		case '"':
			fputs("\\\"", stdout);
			break;
		case '?':
			/* \? is a defined C escape for '?', and writing it every time
			   defeats trigraphs: an unescaped ??= inside a prelude string
			   would otherwise reach the compiler and become '#', changing the
			   standard library with nothing worse than a warning. */
			fputs("\\?", stdout);
			break;
		case '\n':
			fputs("\\n\",\n", stdout);
			at_line_start = 1;
			break;
		case '\t':
			fputs("\\t", stdout);
			break;
		default:
			/* Anything not printable ASCII goes out as an octal escape, so no
			   byte can terminate a literal or vanish into a line ending. High
			   bytes are escaped too, which keeps a UTF-8 prelude exact without
			   depending on the compiler's source character set. */
			if (character < ' ' || character >= 0x7f)
				printf("\\%03o", (unsigned)character);
			else
				putchar(character);
			break;
		}
	}
	/* a last line with no trailing newline still gets closed */
	if (!at_line_start)
		fputs("\\n\",\n", stdout);

	puts("\t(const char *)0 };");
	puts("#define LIBSISP_H");
	puts("#endif");

	if (input != stdin)
		fclose(input);
	if (fflush(stdout) != 0 || ferror(stdout))
	{
		fprintf(stderr, "embed: write failed\n");
		return 1;
	}
	return 0;
}
