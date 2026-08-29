#include <stdbool.h>
#include <stdio.h>
#include <string.h>
static void impl(char*out,const char*s){snprintf(out,16,"[%s]",s);}bool run(void){char o[16];impl(o,"x");return strcmp(o,"[x]")==0;}
