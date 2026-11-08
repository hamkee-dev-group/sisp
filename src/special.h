#ifndef SISP_SPECIAL_H
#define SISP_SPECIAL_H

#include "sisp.h"
extern objectp builtin_if(const struct object *);
extern objectp builtin_cond(const struct object *);
extern objectp builtin_quote(const struct object *);
extern objectp builtin_bquote(const struct object *);
extern objectp builtin_comma(const struct object *);
extern objectp builtin_progn(const struct object *);
extern objectp builtin_prog1(const struct object *);
extern objectp builtin_prog2(const struct object *);
extern objectp builtin_define(const struct object *);
extern objectp builtin_setq(const struct object *);
extern objectp builtin_let(const struct object *);
extern objectp builtin_letstar(const struct object *);
extern objectp builtin_labels(const struct object *);
extern objectp builtin_defmacro(const struct object *);
extern objectp builtin_macroexpand(const struct object *);
extern objectp builtin_eval(const struct object *);
extern objectp builtin_funcall(const struct object *);
extern objectp builtin_apply(const struct object *);
extern objectp builtin_subst(const struct object *);
#endif
