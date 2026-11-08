#include <setjmp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "reader.h"

#define INITIAL_TOKEN_CAPACITY 64

void
reader_init(struct reader *reader, FILE *input, const char *source)
{
	memset(reader, 0, sizeof(*reader));
	reader->input = input;
	reader->source = source == NULL ? "<input>" : source;
	reader->line = 1;
	reader->pushback = EOF;
	reader->token_cap = INITIAL_TOKEN_CAPACITY;
	reader->token_buffer = malloc(reader->token_cap);
	if (reader->token_buffer == NULL)
	{
		fprintf(stderr, "; OUT OF MEMORY\n");
		exit(EXIT_FAILURE);
	}
	reader->token_buffer[0] = '\0';
}

void
reader_reset(struct reader *reader)
{
	reader->token_len = 0;
	reader->token_buffer[0] = '\0';
	reader->bquote_depth = 0;
}

void
reader_destroy(struct reader *reader)
{
	free(reader->token_buffer);
	reader->token_buffer = NULL;
	reader->token_cap = 0;
	reader->token_len = 0;
}

int
reader_get(struct reader *reader)
{
	int character;

	reader->prev_line = reader->line;
	reader->prev_col = reader->col;
	if (reader->pushback != EOF)
	{
		character = reader->pushback;
		reader->pushback = EOF;
	}
	else
		character = fgetc(reader->input);

	if (character == '\n')
	{
		reader->line++;
		reader->col = 0;
	}
	else if (character != EOF)
		reader->col++;
	return character;
}

void
reader_unget(struct reader *reader, int character)
{
	if (character == EOF)
		return;
	if (reader->pushback != EOF)
		reader_error(reader, "READER PUSHBACK OVERFLOW");
	reader->pushback = character;
	reader->line = reader->prev_line;
	reader->col = reader->prev_col;
}

void
reader_discard_line(struct reader *reader)
{
	int character;

	do
	{
		character = reader_get(reader);
	} while (character != '\n' && character != EOF);
}

_Noreturn void
reader_error(struct reader *reader, const char *message)
{
	fprintf(stderr, "; %s:%zu:%zu: %s\n", reader->source, reader->line, reader->col,
			message);
	reader_discard_line(reader);
	reader_reset(reader);
	longjmp(reader->error_jmp, 1);
}

void
reader_token_clear(struct reader *reader)
{
	reader->token_len = 0;
	reader->token_buffer[0] = '\0';
}

void
reader_token_append(struct reader *reader, int character)
{
	char *buffer;
	size_t capacity;

	if (reader->token_len + 1 >= reader->token_cap)
	{
		if (reader->token_cap > SIZE_MAX / 2)
			reader_error(reader, "TOKEN TOO LONG");
		capacity = reader->token_cap * 2;
		buffer = realloc(reader->token_buffer, capacity);
		if (buffer == NULL)
			reader_error(reader, "OUT OF MEMORY");
		reader->token_buffer = buffer;
		reader->token_cap = capacity;
	}
	reader->token_buffer[reader->token_len++] = (char)character;
	reader->token_buffer[reader->token_len] = '\0';
}
