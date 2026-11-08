#ifndef SISP_EQUAL_H
#define SISP_EQUAL_H

#include "sisp.h"
extern int collection_contains(objectp, objectp);

/* equality selector for assoc/member :test arguments */
enum equality_test
{
	TEST_EQ,
	TEST_EQUAL
};
extern enum equality_test parse_equality_test(objectp arg);
extern int objects_equal_by_test(objectp a, objectp b, enum equality_test test);
#endif
