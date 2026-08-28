#include <stdbool.h>
#include <string.h>
static bool stock(const char*s){return strcmp(s,"A")==0;}static int price(const char*s){return strcmp(s,"A")==0?9:0;}bool run(void){return stock("A")&&price("A")==9;}
