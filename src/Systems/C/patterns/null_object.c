#include <stdbool.h>
static int null_write(const char*s){(void)s;return 0;}bool run(void){int(*write)(const char*)=null_write;return write("ignored")==0;}
