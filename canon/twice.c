#include <stdint.h>
#include <stdio.h>

// tags
typedef enum T0Tag { T0_F0, T0_F2 } T0Tag;
typedef enum T1Tag { T1_F1 } T1Tag;

// closure declarations
typedef struct T0 T0;
typedef struct T1 T1;

// environments
typedef struct F0 { int64_t x; T0* f; } F0;
typedef struct F1 { T0* f; } F1;
typedef struct F2 { int64_t x; } F2;

// closures
struct T0 { T0Tag tag; union { F0 f0; F2 f2; } u; };
struct T1 { T1Tag tag; union { F1 f1; } u; };

int main(int argc, char** argv)
{
  return 0;
}
