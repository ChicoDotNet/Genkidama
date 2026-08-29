#include <stdbool.h>
#include <string.h>
struct Row{int id;const char*n;};struct D{int id;const char*n;};bool run(void){struct Row r={1,"Ada"};struct D d={r.id,r.n};return d.id==1&&strcmp(d.n,"Ada")==0;}
