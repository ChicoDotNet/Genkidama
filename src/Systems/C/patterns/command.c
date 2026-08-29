#include <stdbool.h>
static int a(int x){return x+50;}static int s(int x){return x-20;}bool run(void){int b=100;int(*o[2])(int)={a,s};for(int i=0;i<2;i++)b=o[i](b);return b==130;}
