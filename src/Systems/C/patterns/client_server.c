#include <stdbool.h>
#include <stdio.h>
#include <string.h>
static void server(char*out,const char*r){snprintf(out,32,"echo:%s",r);}bool run(void){char o[32];server(o,"ping");return strcmp(o,"echo:ping")==0;}
