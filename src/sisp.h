#ifndef SISP_H
#define IDENTIFIER  3
#define INTEGER     5
#define RATIONAL    6
#define STRING      7
#define SET         8

#define OBJ_SIZE sizeof(struct object)

typedef enum
{
  false,
  true
} bool;

typedef enum
{
  OBJ_NULL,
  OBJ_NIL,
  OBJ_T,
  OBJ_IDENTIFIER,
  OBJ_CONS,
  OBJ_INTEGER,
  OBJ_RATIONAL,
  OBJ_STRING,
  OBJ_SET,
  OBJ_TAU,
} a_type;

typedef struct object *objectp;

struct object
{
  a_type type;
  unsigned int gc;
  union
  {
    char *id;
    long int i;
    struct {
      char *str;
      long int len;
    } s;
    struct
    {
      long int n;
      long int d;
    } r;
    struct
    {
      objectp car;
      objectp cdr;
    } c;
  } value;
  objectp next;
};

typedef struct object_pair *object_pairp;

struct object_pair
{
  objectp name;
  objectp value;
  object_pairp next;
};

typedef struct
{
  char *name;
  objectp (*func)(const struct object *);
} funcs;

typedef struct {
  char *name;
  objectp func;
} func_cache;

#define vcar value.c.car
#define vcdr value.c.cdr

#define SISP_H

#endif
