#include <stdio.h>
#include <stdlib.h>

#include <string.h>

#include "sisp.h"
#include "runtime.h"
#include "pool.h"

/* Per-type used lists support collection and interning. Every object has the
   same size, so reclaimed objects share one free list. */
struct object_pool object_pools[OBJ_TYPE_COUNT];
static objectp free_objects;
static size_t free_count;

void
pool_destroy(void)
{
	objectp object;
	int pool_type;

	for (pool_type = 0; pool_type < OBJ_TYPE_COUNT; pool_type++)
	{
		while ((object = object_pools[pool_type].used) != NULL)
		{
			object_pools[pool_type].used = object->next;
			free_object(object);
			free(object);
		}
	}
	while ((object = free_objects) != NULL)
	{
		free_objects = object->next;
		free(object);
	}
	free_count = 0;
}

void
pool_recycle(objectp object)
{
	object->next = free_objects;
	free_objects = object;
	free_count++;
}

void
pool_trim(size_t keep)
{
	objectp object;

	while (free_count > keep)
	{
		object = free_objects;
		free_objects = object->next;
		free(object);
		free_count--;
	}
}

objectp
pool_allocate(void)
{
	objectp object;

	if (free_objects == NULL)
		return xmalloc(OBJ_SIZE);
	object = free_objects;
	free_objects = object->next;
	free_count--;
	return object;
}

/* checked heap wrappers: every allocation failure becomes a
   recoverable interpreter error instead of a NULL dereference */
void *
xmalloc(size_t n)
{
	void *p = malloc(n);
	if (p == NULL)
	{
		fprintf(stderr, "; OUT OF MEMORY\n");
		runtime_recover();
	}
	return p;
}

void *
xcalloc(size_t n, size_t size)
{
	void *p = calloc(n, size);
	if (p == NULL)
	{
		fprintf(stderr, "; OUT OF MEMORY\n");
		runtime_recover();
	}
	return p;
}

char *
xstrdup(const char *s)
{
	char *p = strdup(s);
	if (p == NULL)
	{
		fprintf(stderr, "; OUT OF MEMORY\n");
		runtime_recover();
	}
	return p;
}
