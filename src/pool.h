#ifndef SISP_POOL_H
#define SISP_POOL_H

#include <stddef.h>

#include "sisp.h"

extern objectp pool_allocate(void);
extern void pool_recycle(objectp);
extern void pool_trim(size_t);
extern void pool_destroy(void);

struct object_pool
{
	size_t used_size;
	objectp used;
};
/* One real definition lives in pool.c; declaring it extern here (rather
   than a tentative definition in the header, which every including TU
   would emit) removes the -fcommon dependency and is portable to
   compilers that default to -fno-common, including MSVC. */
extern struct object_pool object_pools[OBJ_TYPE_COUNT];

#endif
