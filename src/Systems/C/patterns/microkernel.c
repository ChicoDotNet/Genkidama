#include <stdbool.h>
static int a(int x){return x+1;}static int b(int x){return x*2;}bool run(void){int(*p[2])(int)={a,b};int v=3;for(int i=0;i<2;i++)v=p[i](v);return v==8;}
