#include <stdbool.h>
#include <string.h>
static int broker(const char*t){return strcmp(t,"price")==0?9:0;}bool run(void){return broker("price")==9;}
