#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <signal.h>
#include <pthread.h>

#include "sisp.h"
#include "extern.h"
#include "misc.h"
#include "vmm.h"

objectp nil;
objectp t;
objectp null;
objectp u;
objectp tau;
objectp empty;
static object_pairp setobjs_list = NULL;
static object_pairp funcobjs_list = NULL;
static unsigned int gc_id = 0;
func_cache function_cache[CACHE_SIZE];
objectp current_env;

__inline__ objectp
new_object(a_type type)
{
	objectp p;

	p = oballoc(type);
	p->type = type;
	p->gc = 0;
	p->next = pool[type].head.u;
	pool[type].head.u = p;
	pool[type].used_size++;
	if (type == OBJ_CONS || type == OBJ_SET)
		p->vcar = p->vcdr = nil;
	if (type == OBJ_STRING)
		p->value.s.len = 0;

	return p;
}

__inline__ objectp
search_object_rational(const long int num, const long int den)
{
	objectp p;

	for (p = pool[OBJ_RATIONAL].head.u; p != NULL; p = p->next)
		if (p->value.r.n == num && p->value.r.d == den)
			return p;

	return (objectp)NULL;
}

__inline__ objectp
search_object_integer(const long int i)
{
	objectp p;

	for (p = pool[OBJ_INTEGER].head.u; p != NULL; p = p->next)
		if (p->value.i == i)
			return p;

	return (objectp)NULL;
}

__inline__ objectp
search_object_string(const char *s)
{
	objectp p;

	for (p = pool[OBJ_STRING].head.u; p != NULL; p = p->next)
		if (!strcmp(p->value.s.str, s))
			return p;

	return (objectp)NULL;
}

__inline__ objectp
search_object_identifier(const char *s)
{
	objectp p;

	for (p = pool[OBJ_IDENTIFIER].head.u; p != NULL; p = p->next)
		if (!strcmp(p->value.id, s))
			return p;

	return (objectp)NULL;
}

void init_objects(void)
{
	int i, j;
	objectp new_heap_list = NULL;

	null = new_object(OBJ_NULL);
	nil = new_object(OBJ_NIL);
	t = new_object(OBJ_T);
	tau = malloc(OBJ_SIZE);
	tau->type = OBJ_TAU;
	u = malloc(OBJ_SIZE);

	current_env = nil;
	empty = new_object(OBJ_SET);
	empty->vcar = tau;
	empty->vcdr = new_object(OBJ_CONS);
	empty->vcdr->vcar = new_object(OBJ_IDENTIFIER);
	empty->vcdr->vcar->value.id = strdup("not");
	empty->vcdr->vcdr = new_object(OBJ_CONS);
	empty->vcdr->vcdr->vcar = new_object(OBJ_CONS);
	empty->vcdr->vcdr->vcar->vcar = new_object(OBJ_IDENTIFIER);
	empty->vcdr->vcdr->vcar->vcar->value.id = strdup("=");
	empty->vcdr->vcdr->vcar->vcdr = new_object(OBJ_CONS);
	empty->vcdr->vcdr->vcar->vcdr->vcar = tau;
	empty->vcdr->vcdr->vcar->vcdr->vcdr = new_object(OBJ_CONS);
	empty->vcdr->vcdr->vcar->vcdr->vcdr->vcar = tau;
	for (i = 0; i < CACHE_SIZE; i++)
	{
		function_cache[i].name = NULL;
		function_cache[i].func = NULL;
	}
	for (i = 3; i <= 8; i++)
	{
		pool[i].head.u = NULL;
		pool[i].head.f = NULL;
		pool[i].free_size = 0;
		pool[i].used_size = 0;
	}
	for (i = 3; i <= 4; i++)
	{
		pool[i].head.f = calloc(1, OBJ_SIZE);
		if (pool[i].head.f == NULL) {
			fprintf(stderr, "; ERROR allocating memory\n");
			exit(1);
		}
		pool[i].head.f->next = NULL;
		new_heap_list = pool[i].head.f;
		j = (i == 3 ? 31 : 255);
		pool[i].free_size = (unsigned int)j + 1;
		while (j--)
		{
			pool[i].head.f->next = calloc(1, OBJ_SIZE);
			if (pool[i].head.f->next == NULL) {
				fprintf(stderr, "; ERROR allocating memory\n");
				exit(1);
			}
			pool[i].head.f = pool[i].head.f->next;
		}
		pool[i].head.f->next = NULL;
		pool[i].head.f = new_heap_list;
	}
	pool[OBJ_SET].head.f = calloc(1, OBJ_SIZE);
	if (pool[OBJ_SET].head.f == NULL) {
		fprintf(stderr, "; ERROR allocating memory\n");
		exit(1);
	}
	pool[OBJ_SET].head.f->next = NULL;
	new_heap_list = pool[OBJ_SET].head.f;
	j = 127;
	pool[OBJ_SET].free_size = (unsigned int)j + 1;
	while (j--)
	{
		pool[OBJ_SET].head.f->next = calloc(1, OBJ_SIZE);
		if (pool[OBJ_SET].head.f->next == NULL) {
			fprintf(stderr, "; ERROR allocating memory\n");
			exit(1);
		}
		pool[OBJ_SET].head.f = pool[OBJ_SET].head.f->next;
	}
	pool[OBJ_SET].head.f->next = NULL;
	pool[OBJ_SET].head.f = new_heap_list;
}

void remove_object(objectp name)
{
	object_pairp p, prev, next;
	prev = NULL;
	for (p = setobjs_list; p != NULL; prev = p, p = next)
	{
		next = p->next;
		if (name->type == OBJ_IDENTIFIER &&
			!strcmp(name->value.id, p->name->value.id))
		{
			if (prev == NULL)
				setobjs_list = next;
			else
				prev->next = next;
			free(p);
			break;
		}
	}
	prev = NULL;
	for (p = funcobjs_list; p != NULL; prev = p, p = next)
	{
		next = p->next;
		if (name->type == OBJ_IDENTIFIER &&
			!strcmp(name->value.id, p->name->value.id))
		{
			if (prev == NULL)
				funcobjs_list = next;
			else
				prev->next = next;
			free(p);
			break;
		}
	}
	for (int i = 0; i < CACHE_SIZE; i++)
	{
		if (function_cache[i].name != NULL &&
			!strcmp(name->value.id, function_cache[i].name))
		{
			free(function_cache[i].name);
			function_cache[i].name = NULL;
			function_cache[i].func = NULL;
		}
	}
}

void set_object(objectp name, objectp value)
{
	object_pairp p, next;

	if (value == NULL || value == null)
	{
		fprintf(stderr, "; SET OBJECT: NULL VALUE.");
		longjmp(je, 1);
	}
	if (name->type != OBJ_IDENTIFIER)
	{
		fprintf(stderr, "; SET OBJECT: NOT IDENTIFIER NAME.");
		longjmp(je, 1);
	}
	// circular definition set by comprehension
	if (value->type == OBJ_SET &&
		value->value.c.cdr->type != OBJ_SET &&
		value->value.c.cdr != nil)
	{
		if (in_set(name, value->value.c.cdr))
		{
			fprintf(stderr, "; SET OBJECT: VALUE CANNOT CONTAIN NAME.");
			longjmp(je, 1);
		}
	}
	for (p = setobjs_list; p != NULL; p = next)
	{
		next = p->next;
		if (!strcmp(name->value.id, p->name->value.id))
		{
			for (int i = 0; i < CACHE_SIZE; i++)
			{
				if (function_cache[i].name != NULL &&
					!strcmp(name->value.id, function_cache[i].name))
				{
					free(function_cache[i].name);
					function_cache[i].name = NULL;
					function_cache[i].func = NULL;
				}
			}
			p->value = value;
			return;
		}
	}
	p = malloc(sizeof(struct object_pair));
	if (p == NULL)
	{
		fprintf(stderr, "allocating memory\n");
		return;
	}
	p->next = setobjs_list;
	setobjs_list = p;
	p->name = name;
	p->value = value;
}

void set_function(objectp name, objectp value)
{
	object_pairp p, next;

	if (value == NULL || value == null)
	{
		fprintf(stderr, "; SET FUNCTION: NULL VALUE.");
		longjmp(je, 1);
	}
	if (name->type != OBJ_IDENTIFIER)
	{
		fprintf(stderr, "; SET FUNCTION: NOT IDENTIFIER NAME.");
		longjmp(je, 1);
	}
	for (p = funcobjs_list; p != NULL; p = next)
	{
		next = p->next;
		if (!strcmp(name->value.id, p->name->value.id))
		{
			for (int i = 0; i < CACHE_SIZE; i++)
			{
				if (function_cache[i].name != NULL &&
					!strcmp(name->value.id, function_cache[i].name))
				{
					free(function_cache[i].name);
					function_cache[i].name = NULL;
					function_cache[i].func = NULL;
				}
			}
			p->value = value;
			return;
		}
	}
	p = malloc(sizeof(struct object_pair));
	if (p == NULL)
	{
		fprintf(stderr, "allocating memory\n");
		return;
	}
	p->next = funcobjs_list;
	funcobjs_list = p;
	p->name = name;
	p->value = value;
}

objectp
get_function(const struct object *name)
{
	object_pairp p;

	if (name == NULL)
	{
		fprintf(stderr, "; GET FUNCTION: NULL NAME.");
		longjmp(je, 1);
	}
	for (p = funcobjs_list; p != NULL; p = p->next)
		if (!strcmp(name->value.id, p->name->value.id))
			return p->value;

	/* Fall back to value namespace for backward compatibility. */
	return get_object(name);
}

objectp
try_function(const struct object *name)
{
	object_pairp p;

	if (name == null || name == NULL)
		return null;
	for (p = funcobjs_list; p != NULL; p = p->next)
		if (!strcmp(name->value.id, p->name->value.id))
			return p->value;
	return try_object(name);
}

objectp
try_object(const struct object *name)
{
	object_pairp p;
	objectp env;

	if (name == null || name == NULL)
		return null;
	for (env = current_env; env != nil; env = env->vcdr)
		if (env->vcar->vcar->type == OBJ_IDENTIFIER &&
			!strcmp(name->value.id, env->vcar->vcar->value.id))
			return env->vcar->vcdr;
	for (p = setobjs_list; p != NULL; p = p->next)
		if (!strcmp(name->value.id, p->name->value.id))
			return p->value;
	for (p = funcobjs_list; p != NULL; p = p->next)
		if (!strcmp(name->value.id, p->name->value.id))
			return p->value;
	return null;
}

objectp
get_object(const struct object *name)
{
	object_pairp p;
	objectp env;
	if (name == NULL)
	{
		fprintf(stderr, "; GET OBJECT: NULL NAME.");
		longjmp(je, 1);
	}
	for (env = current_env; env != nil; env = env->vcdr)
		if (env->vcar->vcar->type == OBJ_IDENTIFIER &&
			!strcmp(name->value.id, env->vcar->vcar->value.id))
			return env->vcar->vcdr;
	for (p = setobjs_list; p != NULL; p = p->next)
		if (!strcmp(name->value.id, p->name->value.id))
			return p->value;
	/* Fall back to function namespace so functions remain first-class. */
	for (p = funcobjs_list; p != NULL; p = p->next)
		if (!strcmp(name->value.id, p->name->value.id))
			return p->value;

	fprintf(stderr, "; OBJECT '%s' NOT FOUND.", name->value.id);
	longjmp(je, 1);
	return null;
}

void clean_objects(void)
{
	object_pairp p;
	while ((p = setobjs_list) != NULL)
	{
		setobjs_list = setobjs_list->next;
		free(p);
	}
	setobjs_list = NULL;
	while ((p = funcobjs_list) != NULL)
	{
		funcobjs_list = funcobjs_list->next;
		free(p);
	}
	funcobjs_list = NULL;
}

void dump_object(int pool_number)
{
	objectp q;
	object_pairp p;
	int i;
	const char *pool_name[] = {NULL, NULL, NULL, "ID", "CONS", "INT", "RAT", "STR", "SET"};
	if (pool_number == 0)
	{
		printf("FUNCTION CACHE:\n");
		for (unsigned int j = 0; j < CACHE_SIZE; j++)
		{
			if (function_cache[j].name != NULL)
			{
				printf("%s: ", function_cache[j].name);
				princ_object(stdout, function_cache[j].func);
				printf("\n");
			}
		}
		printf("FUNCTION BINDINGS:\n");
		for (p = funcobjs_list; p != NULL; p = p->next)
		{
			princ_object(stdout, p->name);
			printf(": ");
			princ_object(stdout, p->value);
			printf("\n");
		}
		printf("------------\n");
		printf("VALUE BINDINGS:\n");
		for (p = setobjs_list; p != NULL; p = p->next)
		{
			princ_object(stdout, p->name);
			printf(": ");
			princ_object(stdout, p->value);
			printf("\n");
		}
		printf("|POOL\t   USED\t  FREE|\n");
		for (i = 3; i <= 8; i++)
		{
			if (pool[i].used_size > 0)
				printf("|%s\t %6zu %6zu|\n", pool_name[i], pool[i].used_size, pool[i].free_size);
		}
	}
	else if (pool_number >= 3 && pool_number <= 8)
	{

		for (q = pool[pool_number].head.u; q != NULL; q = q->next)
		{
			princ_object(stdout, q);
			printf(" ");
		}
		printf("\n");
		printf("|POOL\t   USED\t  FREE|\n");
		printf("|%s\t %6zu %6zu|\n", pool_name[pool_number], pool[pool_number].used_size, pool[pool_number].free_size);
	}
	else
	{
		fprintf(stderr, "; NO SUCH POOL %d", pool_number);
	}
}
#ifdef GCTHREADS
static void *tt(void *tree)
{
	objectp *t_ptr = (objectp *)tree;
	if ((*t_ptr)->gc == gc_id)
		return NULL;
	(*t_ptr)->gc = gc_id;
	if ((*t_ptr)->type == OBJ_CONS || (*t_ptr)->type == OBJ_SET)
	{
		tt(&((*t_ptr)->vcar));
		tt(&((*t_ptr)->vcdr));
	}
	return NULL;
}
__inline__ static void
tag_whole_tree(void)
{
	object_pairp p;
	pthread_t tag_name, tag_value;
	for (p = setobjs_list; p != NULL; p = p->next)
	{
		pthread_create(&tag_name, NULL, tt, &(p->name));
		pthread_create(&tag_value, NULL, tt, &(p->value));
		pthread_join(tag_name, NULL);
		pthread_join(tag_value, NULL);
	}
	for (p = funcobjs_list; p != NULL; p = p->next)
	{
		pthread_create(&tag_name, NULL, tt, &(p->name));
		pthread_create(&tag_value, NULL, tt, &(p->value));
		pthread_join(tag_name, NULL);
		pthread_join(tag_value, NULL);
	}
	if (current_env != nil)
		tt(&current_env);
	return;
}
#else
static void tag_tree(objectp p)
{
	if (p->gc == gc_id)
		return;
	p->gc = gc_id;
	if (p->type == OBJ_CONS || p->type == OBJ_SET)
	{
		tag_tree(p->vcar);
		tag_tree(p->vcdr);
	}
}

static void tag_whole_tree(void)
{
	object_pairp p;

	for (p = setobjs_list; p != NULL; p = p->next)
	{
		tag_tree(p->name);
		tag_tree(p->value);
	}
	for (p = funcobjs_list; p != NULL; p = p->next)
	{
		tag_tree(p->name);
		tag_tree(p->value);
	}
	if (current_env != nil)
		tag_tree(current_env);
}

#endif
void garbage_collect(void)
{
	objectp p, prev, next;
	objectp new_used_objs_list;
	a_type i;
	if (++gc_id == UINT_MAX - 1)
		gc_id = 1;

	tag_whole_tree();

	for (i = 3; i <= 8; i++)
	{
		prev = NULL;
		new_used_objs_list = NULL;
		for (p = pool[i].head.u; p != NULL; p = next)
		{
			next = p->next;
			prev = p;
			if (p->gc != gc_id && p != null)
			{
				prev->next = next;
				p->next = pool[i].head.f;
				pool[i].head.f = p;
				pool[i].free_size++;
				pool[i].used_size--;
			}
			else
			{
				p->next = new_used_objs_list;
				new_used_objs_list = p;
			}
		}
		pool[i].head.u = new_used_objs_list;
		recycle_pool(i);
	}
}
