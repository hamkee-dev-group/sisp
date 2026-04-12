#ifndef _EVAL_H

#define _ASSERTP(EXPR, ARG, F, OBJ)         \
	do                                      \
	{                                       \
		if (!(EXPR))                        \
		{                                   \
			fprintf(stderr, "; " #F ": '"); \
			princ_object(stderr, OBJ);      \
			fprintf(stderr, "' " #ARG "."); \
			longjmp(je, 1);                 \
		}                                   \
	} while (0)

#define eval(p) (                                              		                \
	((p)->type) == OBJ_T ? t : ((p)->type) == OBJ_NIL	   	? nil                   \
						 : ((p)->type) == OBJ_TAU		   	? tau                   \
						 : ((p)->type) == OBJ_NULL	   		? handsig("EVAL ERROR") \
						 : ((p)->type) == OBJ_INTEGER	   	? (p)                   \
						 : ((p)->type) == OBJ_RATIONAL   	? eval_rat((p))         \
						 : ((p)->type) == OBJ_IDENTIFIER 	? get_object((p))       \
						 : ((p)->type) == OBJ_STRING	   	? (p)                   \
						 : ((p)->type) == OBJ_CONS	   		? eval_cons((p))        \
						 : ((p)->type) == OBJ_SET		   	? eval_set((p))         \
													   : 	(p))

#define try_eval(p) (                                                    		\
	((p)->type) == OBJ_T ? t : ((p)->type) == OBJ_NIL	    ? nil           	\
						 : ((p)->type) == OBJ_TAU			? tau            	\
						 : ((p)->type) == OBJ_NULL	   		? null            	\
						 : ((p)->type) == OBJ_INTEGER	   	? (p)            	\
						 : ((p)->type) == OBJ_RATIONAL   	? eval_rat((p))   	\
						 : ((p)->type) == OBJ_IDENTIFIER 	? try_object((p))	\
						 : ((p)->type) == OBJ_STRING	   	? (p)            	\
						 : ((p)->type) == OBJ_CONS	   		? eval_cons((p))  	\
						 : ((p)->type) == OBJ_SET		   	? eval_set((p))  	\
													   		: (p))

#define car(p) (                                        	\
	((((p)->type) == OBJ_CONS) || (((p)->type) == OBJ_SET)) \
		? (p)->value.c.car                               	\
		: ((p)->type) == OBJ_NIL ? nil                      \
		: handsig("CAR: UNDEFINED"))

#define cdr(p) (                                        	\
	((((p)->type) == OBJ_CONS) || (((p)->type) == OBJ_SET)) \
		? (p)->value.c.cdr                                	\
		: ((p)->type) == OBJ_NIL ? nil                      \
		: handsig("CDR: UNDEFINED"))


#define cddr(p) cdr(cdr(p))
#define cadr(p) car(cdr(p))
#define caar(p) car(car(p))
#define cdar(p) cdr(car(p))
objectp eval_bquote(objectp);
objectp eval_rat(const struct object *);
objectp eval_cons(const struct object *);
objectp eval_set(const struct object *);
objectp expand_macro(objectp, objectp);
int is_macro(objectp);
int in_set(objectp, objectp);

#define _EVAL_H

#endif
