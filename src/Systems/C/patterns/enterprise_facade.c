#include <stdbool.h>
static bool stock(const char*s){(void)s;return true;}static bool pay(int n){return n==9;}bool run(void){return stock("A")&&pay(9);}
