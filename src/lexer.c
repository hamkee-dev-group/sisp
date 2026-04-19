#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <string.h>

#include "sisp.h"
#include "extern.h"

#define BUFFER_SIZE 64
#define XGETC() ((lex_bufp > lex_buf) ? *--lex_bufp : fgetc(input_file))
#define XUNGETC(c) *lex_bufp++ = (c)

#define CLEAN_BUFFER                                \
	do                                              \
	{                                               \
		XUNGETC(c);                                 \
		while (((c = XGETC()) == '\n') && c != EOF) \
			;                                       \
		fflush(stdout);								\
		memset(token_buffer, 0, BUFFER_SIZE);       \
		longjmp(jl, 1);                             \
	} while (0)

#define CHECK_TOKEN_SIZE                                           \
	do                                                             \
	{                                                              \
		if (p - token_buffer == BUFFER_SIZE)                       \
		{                                                          \
			fprintf(stderr, "; TOKEN LENGTH > %d\n", BUFFER_SIZE); \
			CLEAN_BUFFER;                                          \
		}                                                          \
	} while (0)

FILE *input_file;
static int lex_buf[2];
static int *lex_bufp;
char *token_buffer;
static int par = 0;
static int keysym = 0;
jmp_buf jl;

void init_lex(void)
{
	token_buffer = malloc(BUFFER_SIZE);
	if (token_buffer == NULL)
	{
		fprintf(stderr, "allocating memory\n");
		return;
	}
	memset(token_buffer, 0, BUFFER_SIZE);

	lex_bufp = lex_buf;
}

void done_lex(void)
{
	memset(token_buffer, 0, BUFFER_SIZE);
}

int gettoken(void)
{
	char *p;
	int c;
	size_t string_buffer, offset;
	void *tmp;
	while (true)
	{
		c = XGETC();
		switch (c)
		{
		case ',':
			c = XGETC();
			if (c == '@')
				return COMMA_AT;
			if (c == ')')
				CLEAN_BUFFER;
			XUNGETC(c);
			return ',';
		case '`':
			c = XGETC();
			if (c == ')')
				CLEAN_BUFFER;
			else
			{
				XUNGETC(c);
				c = '`';
				return c;
			}
		case '(':
			par++;
			return c;

		case ')':
			par--;
			if (par < 0)
			{
				par = 0;
				CLEAN_BUFFER;
			}
			c = ')';
			return c;
		case '{':
			keysym++;
			return c;
		case '}':
			keysym--;
			if (keysym < 0)
			{
				keysym = 0;
				CLEAN_BUFFER;
			}
			c = '}';
			return c;

		case ' ':
			while (isspace(c = XGETC()))
				;
			XUNGETC(c);
		case '\f':
		case '\t':
		case '\v':
		case '\r':
		case '\n':
			break;
		case ';':
			do
			{
				c = XGETC();
			} while (c != '\n' && c != EOF);
			XUNGETC(c);
			break;
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
		case '-':
			if (c == '-')
			{
				int next = XGETC();
				if (!isdigit(next))
				{
					XUNGETC(next);
					p = token_buffer;
					do
					{
						CHECK_TOKEN_SIZE;
						*p++ = tolower(c);
						c = XGETC();
					} while (isalnum(c) || strchr("*+/<=>-_#", c) != NULL);
					XUNGETC(c);
					*p = '\0';
					return IDENTIFIER;
				}
				XUNGETC(next);
			}
			p = token_buffer;
			do
			{
				CHECK_TOKEN_SIZE;
				*p++ = c;
				c = XGETC();
			} while (isdigit(c));
			if (c == ' ' || c == '}' || c == '{' ||
				c == ')' || c == '(' || c == '\n' || c == EOF)
			{
				XUNGETC(c);
				*p = '\0';
				return INTEGER;
			}
			else if (c == '/')
			{
				CHECK_TOKEN_SIZE;
				*p++ = c;
				c = XGETC();
				if (c == '-')
				{
					CHECK_TOKEN_SIZE;
					*p++ = c;
					c = XGETC();
				}
				if (!isdigit(c))
				{
					CLEAN_BUFFER;
				}
				do
				{
					CHECK_TOKEN_SIZE;
					*p++ = c;
					c = XGETC();
				} while (isdigit(c));
				if (c == ' ' || c == '}' || c == '{' ||
					c == ')' || c == '(' || c == '\n' || c == EOF)
				{
					XUNGETC(c);
					*p = '\0';
					return RATIONAL;
				}
			}
			else
				CLEAN_BUFFER;
		case '"':
			p = token_buffer;
			string_buffer = BUFFER_SIZE;
			while ((c = XGETC()) != '"')
			{
				if (c == EOF)
				{
					fprintf(stderr, "; UNTERMINATED STRING\n");
					CLEAN_BUFFER;
				}
				if (p - token_buffer >= (long)string_buffer)
				{
					offset = p - token_buffer;
					string_buffer += BUFFER_SIZE;
					tmp = realloc(token_buffer, string_buffer);
					if(tmp == NULL) {
						CLEAN_BUFFER;
					}
					token_buffer = (char *) tmp;
					p = token_buffer + offset;
				}
				*p++ = c;
			}
			*p++ = '\0';
			return STRING;
		case '*':
		case '+':
		case '/':
		case '<':
		case '=':
		case '>':
		case '_':
		case '#':
		case 'a':
		case 'b':
		case 'c':
		case 'd':
		case 'e':
		case 'f':
		case 'g':
		case 'h':
		case 'i':
		case 'j':
		case 'k':
		case 'l':
		case 'm':
		case 'n':
		case 'o':
		case 'p':
		case 'q':
		case 'r':
		case 's':
		case 't':
		case 'u':
		case 'v':
		case 'w':
		case 'x':
		case 'y':
		case 'z':
		case 'A':
		case 'B':
		case 'C':
		case 'D':
		case 'E':
		case 'F':
		case 'G':
		case 'H':
		case 'I':
		case 'J':
		case 'K':
		case 'L':
		case 'M':
		case 'N':
		case 'O':
		case 'P':
		case 'Q':
		case 'R':
		case 'S':
		case 'T':
		case 'U':
		case 'V':
		case 'W':
		case 'X':
		case 'Y':
		case 'Z':
		case '^':
		case '\\':
			p = token_buffer;
			do
			{
				CHECK_TOKEN_SIZE;
				*p++ = tolower(c);
				c = XGETC();
			} while (isalnum(c) || strchr("*+/<=>-_#", c) != NULL);
			XUNGETC(c);
			*p = '\0';
			return IDENTIFIER;
		default:
			return c;
		}
	}
}
