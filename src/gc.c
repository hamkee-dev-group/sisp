/* gc.c — mark-and-sweep over the per-type used lists.
   THE CONTRACT: collection may only run between top-level forms
   (process.c is the sole caller). During evaluation, temporaries in
   C locals — including the form being executed — are reachable from
   no root, so any mid-evaluation collection is unsound; commit
   33fc5eb records the use-after-free that proved it.
   The roots, exactly: every global identifier's function cell, the
   current environment, the empty-set singleton, and the four interned
   runtime identifiers without builtin bindings. Identifiers with a
   non-NULL binding are immortal; their pooled builtin or closure values
   are marked through that binding.
   Marking recurses on cars and iterates cdrs; an epoch counter makes
   marks self-clearing. The sweep rebuilds each used list, frees the
   owned payloads of dead identifiers/strings/matrices, and returns
   nodes to the shared free list. */
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdint.h>

#include "sisp.h"
#include "runtime.h"
#include "pool.h"

static unsigned int gc_epoch = 0;

/* Marking is iterative: the cdr spine walks in place and each car is
   pushed onto an explicit worklist, so the C stack stays O(1) for any
   nesting depth or cycle (the epoch check makes shared/cyclic structure
   terminate). The worklist grows on demand and is kept between passes. */
static objectp *mark_stack = NULL;
static long mark_capacity = 0;

static void
mark_tree(objectp node)
{
	long stack_size = 0;
	long new_capacity;
	objectp *grown;

	for (;;)
	{
		while (node->gc != gc_epoch)
		{
			node->gc = gc_epoch;
			if (node->type != OBJ_CONS && node->type != OBJ_SET)
				break;
			if (stack_size == mark_capacity)
			{
				new_capacity = mark_capacity ? mark_capacity * 2 : 1024;
				grown = realloc(mark_stack, (size_t)new_capacity * sizeof(objectp));
				if (grown == NULL)
				{
					fprintf(stderr, "; GC: MARK STACK EXHAUSTED.\n");
					exit(1);
				}
				mark_stack = grown;
				mark_capacity = new_capacity;
			}
			mark_stack[stack_size++] = node->value.cons.car;
			node = node->value.cons.cdr;
		}
		if (stack_size == 0)
			return;
		node = mark_stack[--stack_size];
	}
}

static void
mark_roots(void)
{
	objectp symbol;

	for (symbol = object_pools[OBJ_IDENTIFIER].used; symbol != NULL;
		 symbol = symbol->next)
		if (symbol->value.identifier.binding != NULL)
			mark_tree(symbol->value.identifier.binding);
	if (current_env != nil)
		mark_tree(current_env);
	if (empty != NULL)
		mark_tree(empty);
	/* the fabricated-name singletons without a sym.func must never be
	   swept; closure/lambda/macro/comma-at are not builtin names, the
	   rest are immortal through their builtin binding */
	if (id_lambda != NULL)
	{
		mark_tree(id_closure);
		mark_tree(id_lambda);
		mark_tree(id_macro);
		mark_tree(id_comma_at);
	}
}

void
gc_collect(void)
{
	objectp node, next, live_objects;
	enum object_type pool_type;
	size_t live_count = 0;
	size_t free_limit;
	if (++gc_epoch == UINT_MAX - 1)
		gc_epoch = 1;

	mark_roots();

	for (pool_type = OBJ_IDENTIFIER; pool_type < OBJ_TYPE_COUNT; pool_type++)
	{
		live_objects = NULL;
		for (node = object_pools[pool_type].used; node != NULL; node = next)
		{
			next = node->next;
			if (node->gc != gc_epoch && !(node->type == OBJ_IDENTIFIER &&
										  node->value.identifier.binding != NULL))
			{
				free_object(node);
				pool_recycle(node);
				object_pools[pool_type].used_size--;
			}
			else
			{
				node->next = live_objects;
				live_objects = node;
			}
		}
		object_pools[pool_type].used = live_objects;
		live_count += object_pools[pool_type].used_size;
	}
	free_limit = live_count > SIZE_MAX / 2 ? SIZE_MAX : live_count * 2;
	pool_trim(free_limit);
}
