#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
bool run(void){const char*t[3]={"2","+","3"};int a=atoi(t[0]),b=atoi(t[2]);return (strcmp(t[1],"+")==0?a+b:0)==5;}
