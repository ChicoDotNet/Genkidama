#include <stdbool.h>
#include <string.h>
struct S{const char*n;int v;};bool run(void){struct S s={"clock",7};return strcmp(s.n,"clock")==0&&s.v==7;}
