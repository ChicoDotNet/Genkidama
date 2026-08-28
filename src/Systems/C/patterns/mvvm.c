#include <stdbool.h>
#include <stdio.h>
#include <string.h>
bool run(void){char vm[16];snprintf(vm,sizeof vm,"count:%d",5);return strcmp(vm,"count:5")==0;}
