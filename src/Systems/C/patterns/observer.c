#include <stdbool.h>
static int a(int x){return x+1;}static int b(int x){return x*2;}bool run(void){int(*o[2])(int)={a,b};return o[0](3)==4&&o[1](3)==6;}
