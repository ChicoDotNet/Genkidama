#include <stdbool.h>
#include <string.h>
enum K{NUM,TEXT};struct N{enum K k;int n;const char*s;};static int visit(const struct N*x){return x->k==NUM?x->n:(int)strlen(x->s);}bool run(void){struct N a={NUM,3,0},b={TEXT,0,"ab"};return visit(&a)+visit(&b)==5;}
