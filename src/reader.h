#ifndef SISP_READER_H
#define SISP_READER_H

#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>

#include "sisp.h"

struct reader
{
	FILE *input;
	const char *source;
	char *token_buffer;
	size_t token_cap;
	size_t token_len;
	size_t line;
	size_t col;
	size_t prev_line;
	size_t prev_col;
	int pushback;
	int bquote_depth;
	jmp_buf error_jmp;
};

void reader_init(struct reader *, FILE *, const char *);
void reader_reset(struct reader *);
void reader_destroy(struct reader *);
int reader_get(struct reader *);
void reader_unget(struct reader *, int);
void reader_discard_line(struct reader *);
_Noreturn void reader_error(struct reader *, const char *);
void reader_token_clear(struct reader *);
void reader_token_append(struct reader *, int);
int lexer_next_token(struct reader *);
objectp parser_parse_object(struct reader *);

#endif
