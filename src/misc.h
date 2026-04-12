#ifndef _MISC_H
extern objectp sst(objectp, objectp, objectp);
extern objectp eqcons(objectp, objectp);
extern objectp eqset(objectp, objectp);
extern objectp eqcons_p(objectp, objectp);
extern objectp eqset_p(objectp, objectp);
extern long int gcd(long int, long int);
extern void princ_object(FILE *, const struct object *);
extern int unsafe_sanitize(objectp);
extern unsigned long card(objectp);
extern objectp set_to_array(objectp);
int in_set(objectp, objectp);

/* Equality test selector for assoc/member :test parameter */
typedef enum { TEST_EQL, TEST_EQ, TEST_EQUAL, TEST_EQUALP } test_type;
extern test_type parse_test_arg(objectp arg);
extern int compare_by_test(objectp a, objectp b, test_type test);

#define _MISC_H
#endif
