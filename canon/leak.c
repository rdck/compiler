// f0 {["f" := Z64 -> Z64]} ("x" := Z64) :=
// r0 : Z64 := call f x
// r1 : Z64 := call f r0
// ret r1
// 
// f1 {[]} ("f" := Z64 -> Z64) :=
// r0 : Z64 -> Z64 := close f0 {f}
// ret r0
// 
// f2 {[]} ("x" := Z64) :=
// r0 : Z64 := 1
// r1 : Z64 := + x r0
// ret r1
// 
// r0 : (Z64 -> Z64) -> Z64 -> Z64 := close f1 {}
// r1 : Z64 -> Z64 := close f2 {}
// r2 : Z64 -> Z64 := call r0 r1
// r3 : Z64 := 0
// r4 : Z64 := call r2 r3

////////////////////////////////////////////////////////////////////////////////

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// typedef enum T0Tag {
//   T0_F0,
//   T0_F2,
// } T0Tag;

typedef enum T0Tag T0Tag;
enum T0Tag {
  T0_F0,
  T0_F2,
};

typedef enum T1Tag {
  T1_F1,
} T1Tag;

typedef struct T0 T0; // Z64 -> Z64
typedef struct T1 T1; // T0 -> T0

struct T0 {
  T0Tag tag;
  void* env;
};

struct T1 {
  T1Tag tag;
  void* env;
};

typedef struct F0 { int64_t rc; T0 f ; } F0;
typedef struct F1 { int64_t rc; } F1;
typedef struct F2 { int64_t rc; } F2;

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
        F2* env = f.env;
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
        F1* env = f.env;
        T0 r0;
        r0.tag = T0_F0;
        { F0* allocated = malloc(sizeof(*allocated)); allocated->f = arg; r0.env = allocated; }
        return r0;
      } break;
  }
}

int main(int argc, char** argv)
{

  T1 r0;
  r0.tag = T1_F1;
  { F1* allocated = malloc(sizeof(*allocated)); r0.env = allocated; }

  T0 r1;
  r1.tag = T0_F2;
  { F2* allocated = malloc(sizeof(*allocated)); r1.env = allocated; }

  T0 r2 = apply_t1(r0, r1);
  int64_t r3 = 0;
  int64_t r4 = apply_t0(r2, r3);

  printf("result: %lld\n", r4);
  return 0;

}
