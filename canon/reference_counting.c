#include <stdint.h>
#include <stdio.h>

// tags
typedef enum T0Tag { T0_F0, T0_F2 } T0Tag;
typedef enum T1Tag { T1_F1 } T1Tag;

// closure declarations
// typedef struct T0 T0; // Z -> Z
// typedef struct T1 T1; // T0 -> T0

// environments
typedef struct F0 { int64_t rc; T0 f; } F0;
typedef struct F1 { int64_t rc; } F1;
typedef struct F2 { int64_t rc; } F2;

// closures
struct T0 { T0Tag tag; void* env; };
struct T1 { T1Tag tag; void* env; };

static void dealloc_f0(F0* env)
{
  env->rc -= 1;
  if (env->rc <= 0)
  {
  }
}

static int64_t apply_t0(T0 f, int64_t arg)
{
  switch (f.tag) {
    case T0_F0:
      {
        F0* env = f.env;
        int64_t r0 = apply_t0(env->f, arg);
        int64_t r1 = apply_t0(env->f, r0);
        return r1;
      } break;
    case T0_F2:
      {
        int64_t r0 = 1;
        int64_t r1 = arg + r0;
        return r1;
      } break;
  }
}

static T0 apply_t1(T1 f, T0 arg)
{
  switch (f.tag) {
    case T1_F1:
      {
        T0 r0;
        r0.tag = T0_F0;
        r0.env = alloc_f0(arg);
        return r0;
      } break;
  }
}

int main(int argc, char** argv)
{
  T1 r0;
  r0.tag = T1_F1;
  r0.env = alloc_f1();
  T0 r1;
  r1.tag = T0_F2;
  r1.env = alloc_f2();
  T0 r2 = apply_t1(r0, r1);
  int64_t r3 = 0;
  int64_t r4 = apply_t0(r2, r3);
  printf("result: %lld\n", r4);
}
