#include <stdbool.h>
#include <ctype.h>
#include <string.h>
bool run(void){char v[16];const char*m="ready";int i=0;for(;m[i];i++)v[i]=(char)toupper((unsigned char)m[i]);v[i]=0;return strcmp(v,"READY")==0;}
