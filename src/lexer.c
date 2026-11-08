#include <ctype.h>
#include <string.h>

#include "reader.h"

static int
character_is_digit(int character)
{
	return character != EOF && isdigit((unsigned char)character);
}

static int
character_starts_identifier(int character)
{
	return character != EOF && (isalpha((unsigned char)character) ||
								strchr("*+/<=>_#^\\", character) != NULL);
}

static int
character_continues_identifier(int character)
{
	return character != EOF && (isalnum((unsigned char)character) ||
								strchr("*+/<=>-_#", character) != NULL);
}

static int
character_delimits_number(int character)
{
	return character == EOF || isspace((unsigned char)character) ||
		   strchr("{}()[];", character) != NULL;
}

static int
read_identifier(struct reader *reader, int character)
{
	reader_token_clear(reader);
	do
	{
		reader_token_append(reader, tolower((unsigned char)character));
		character = reader_get(reader);
	} while (character_continues_identifier(character));
	reader_unget(reader, character);
	return TOK_IDENTIFIER;
}

static int
read_number(struct reader *reader, int character)
{
	reader_token_clear(reader);
	do
	{
		reader_token_append(reader, character);
		character = reader_get(reader);
	} while (character_is_digit(character));

	if (character_delimits_number(character))
	{
		reader_unget(reader, character);
		return TOK_INTEGER;
	}

	if (character == '/')
	{
		reader_token_append(reader, character);
		character = reader_get(reader);
		if (character == '-')
		{
			reader_token_append(reader, character);
			character = reader_get(reader);
		}
		if (!character_is_digit(character))
			reader_error(reader, "MALFORMED RATIONAL");
		do
		{
			reader_token_append(reader, character);
			character = reader_get(reader);
		} while (character_is_digit(character));
		if (!character_delimits_number(character))
			reader_error(reader, "MALFORMED RATIONAL");
		reader_unget(reader, character);
		return TOK_RATIONAL;
	}

	if (character == '.')
	{
		reader_token_append(reader, character);
		character = reader_get(reader);
		if (!character_is_digit(character))
			reader_error(reader, "MALFORMED FLOAT");
		do
		{
			reader_token_append(reader, character);
			character = reader_get(reader);
		} while (character_is_digit(character));
		if (!character_delimits_number(character))
			reader_error(reader, "MALFORMED FLOAT");
		reader_unget(reader, character);
		return TOK_FLOAT;
	}

	reader_error(reader, "MALFORMED NUMBER");
	return EOF;
}

static int
read_string(struct reader *reader)
{
	int character;

	reader_token_clear(reader);
	while ((character = reader_get(reader)) != '"')
	{
		if (character == EOF)
			reader_error(reader, "UNTERMINATED STRING");
		if (character == '\\')
		{
			character = reader_get(reader);
			switch (character)
			{
			case 'n':
				character = '\n';
				break;
			case 'r':
				character = '\r';
				break;
			case 't':
				character = '\t';
				break;
			case '"':
				character = '"';
				break;
			case '\\':
				character = '\\';
				break;
			default:
				reader_error(reader, "INVALID STRING ESCAPE");
			}
		}
		reader_token_append(reader, character);
	}
	return TOK_STRING;
}

int
lexer_next_token(struct reader *reader)
{
	int character, next;

	for (;;)
	{
		character = reader_get(reader);
		if (character == EOF)
			return EOF;
		if (isspace((unsigned char)character))
			continue;
		if (character == ';')
		{
			reader_discard_line(reader);
			continue;
		}
		if (character_is_digit(character))
			return read_number(reader, character);
		if (character_starts_identifier(character))
			return read_identifier(reader, character);

		switch (character)
		{
		case '-':
			next = reader_get(reader);
			if (!character_is_digit(next))
			{
				reader_unget(reader, next);
				return read_identifier(reader, character);
			}
			reader_unget(reader, next);
			return read_number(reader, character);
		case '"':
			return read_string(reader);
		case ',':
			next = reader_get(reader);
			if (next == '@')
				return TOK_COMMA_AT;
			if (next == ')')
				reader_error(reader, "COMMA BEFORE ')'");
			reader_unget(reader, next);
			return ',';
		case '`':
			next = reader_get(reader);
			if (next == ')')
				reader_error(reader, "BACKQUOTE BEFORE ')'");
			reader_unget(reader, next);
			return '`';
		default:
			return character;
		}
	}
}
