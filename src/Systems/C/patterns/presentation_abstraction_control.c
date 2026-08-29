#include <stdbool.h>
#include <stdio.h>
#include <string.h>
bool run(void){int a=3,c=a+1;char p[16];snprintf(p,sizeof p,"n=%d",c);return strcmp(p,"n=4")==0;}
