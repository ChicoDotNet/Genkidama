#include <stdbool.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
static void hook(char*out,const char*s){int i=0;for(;s[i];i++)out[i]=(char)toupper((unsigned char)s[i]);out[i]=0;}bool run(void){char h[16],o[32];hook(h,"work");snprintf(o,sizeof o,"start>%s>end",h);return strcmp(o,"start>WORK>end")==0;}
