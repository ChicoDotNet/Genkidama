#include <stdbool.h>
static int a(int x,int y){return x+y;}static int m(int x,int y){return x*y;}bool run(void){return a(2,3)==5&&m(2,3)==6;}
