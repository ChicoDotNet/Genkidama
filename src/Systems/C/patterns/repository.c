#include <stdbool.h>
#include <string.h>
struct R{int id;const char*v;};bool run(void){struct R r={1,"A"};return r.id==1&&strcmp(r.v,"A")==0;}
