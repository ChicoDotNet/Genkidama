#include <stdbool.h>
#include <stdio.h>
#include <string.h>
static void peer(char*out,const char*p,const char*m){snprintf(out,32,"%s:%s",p,m);}bool run(void){char a[32],b[32];peer(a,"a","hi");peer(b,"b",a);return strcmp(b,"b:a:hi")==0;}
