#include <stdbool.h>
#include <stdio.h>
#include <string.h>
static void fmt(char*out,int n){snprintf(out,16,"#%d",n);}bool run(void){char o[16];void(*f)(char*,int)=fmt;f(o,7);return strcmp(o,"#7")==0;}
