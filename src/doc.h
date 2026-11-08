#ifndef SISP_DOC_H
#define SISP_DOC_H

#include <stddef.h>

#include "sisp.h"

/* The documentation registry. Entries are plain C records outside the object
   pools, so the collector never marks or sweeps them. */
extern void documentation_append(const char *name, const char *text);
extern void documentation_register(const char *name, const char *text);
extern void documentation_forget(const char *name);
extern const char *documentation_lookup(const char *name, size_t *length);
extern objectp documentation_capture(objectp name, objectp body);

/* DOC places a manual entry beside the function it describes, rather than in
   a table somewhere else. It registers before main -- which is safe because
   its plain C records need nothing initialised first.

   The name is a string, not a token, because SISP names contain hyphens. */
#define DOC_JOIN(a, b) DOC_JOIN_(a, b)
#define DOC_JOIN_(a, b) a##b
#define DOC(name, text)                                                    \
	static void __attribute__((constructor))                                \
		DOC_JOIN(documentation_entry_, __LINE__)(void)                       \
	{                                                                       \
		documentation_append((name), (text));                                \
	}

#endif
