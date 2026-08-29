#include <stdbool.h>
static int a(int x){return x+1;}static int b(int x){return x+2;}bool run(void){int(*s[2])(int)={a,b};return s[0](5)==6&&s[1](5)==7;}
