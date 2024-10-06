#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

extern int64_t lambda_main();

int main(int argc, char** argv)
{
  int64_t result = lambda_main();
  printf("result: %" PRId64 "\n", result);
  return 0;
}
