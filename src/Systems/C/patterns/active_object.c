#include <stdbool.h>
static int a(void){return 2;}static int b(void){return 3;}bool run(void){int(*q[2])(void)={a,b};return q[0]()+q[1]()==5;}
