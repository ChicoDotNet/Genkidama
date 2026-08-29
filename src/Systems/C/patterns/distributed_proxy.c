#include <stdbool.h>
#include <stdio.h>
#include <string.h>
static void remote(char*out,int id){snprintf(out,32,"item:%d",id);}bool run(void){char o[32];remote(o,7);return strcmp(o,"item:7")==0;}
