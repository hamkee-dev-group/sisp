/* Documentation stays outside the object heap, so it adds no GC work. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "runtime.h"
#include "doc.h"

struct documentation_entry
{
	char *name;
	char *text;
	size_t length;
	struct documentation_entry *next;
};

static struct documentation_entry *documentation;

static struct documentation_entry *
find_entry(const char *name)
{
	struct documentation_entry *entry;

	for (entry = documentation; entry != NULL; entry = entry->next)
		if (strcmp(entry->name, name) == 0)
			return entry;
	return NULL;
}

/* Used while registering the builtin table, whose names are known unique.
   Skipping the duplicate check keeps startup linear rather than quadratic. */
void
documentation_append(const char *name, const char *text)
{
	struct documentation_entry *entry;

	entry = xmalloc(sizeof(*entry));
	entry->name = xstrdup(name);
	entry->text = xstrdup(text);
	entry->length = strlen(text);
	entry->next = documentation;
	documentation = entry;
}

void
documentation_register(const char *name, const char *text)
{
	struct documentation_entry *entry;

	entry = find_entry(name);

	if (entry != NULL)
	{
		/* redefining a name replaces its documentation */
		free(entry->text);
		entry->text = xstrdup(text);
		entry->length = strlen(text);
		return;
	}
	documentation_append(name, text);
}

/* undef drops the entry too, or it outlives the binding it describes. */
void
documentation_forget(const char *name)
{
	struct documentation_entry *entry;
	struct documentation_entry *previous;

	previous = NULL;

	for (entry = documentation; entry != NULL; entry = entry->next)
	{
		if (strcmp(entry->name, name) == 0)
		{
			if (previous == NULL)
				documentation = entry->next;
			else
				previous->next = entry->next;
			free(entry->name);
			free(entry->text);
			free(entry);
			return;
		}
		previous = entry;
	}
}

const char *
documentation_lookup(const char *name, size_t *length)
{
	struct documentation_entry *entry;

	entry = find_entry(name);
	if (entry == NULL)
		return NULL;
	if (length != NULL)
		*length = entry->length;
	return entry->text;
}

/* A leading string is documentation rather than a value, but only when a
   form follows it -- so (define (f x) "hi") still returns "hi".

   Returns the body with that string removed, because once it is recorded
   here it has no business staying in the lambda: leaving it would print with
   the function and keep an object alive per definition for a copy of text we
   already hold. */
objectp
documentation_capture(objectp name, objectp body)
{
	if (name->type != OBJ_IDENTIFIER || body == nil)
		return body;
	if (car(body)->type == OBJ_STRING && cdr(body) != nil)
	{
		documentation_register(name->value.identifier.name,
							   car(body)->value.string.data);
		return cdr(body);
	}
	return body;
}
