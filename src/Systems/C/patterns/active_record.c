#include <stdbool.h>
#include <string.h>
struct R{int id;const char*n;};bool run(void){struct R r={1,"Ada"},t[1];t[0]=r;return t[0].id==1&&strcmp(t[0].n,"Ada")==0;}
