#include <stdbool.h>
#include <string.h>
struct B{const char*x;};bool run(void){struct B b={0};b.x="created";const char*x=b.x;b.x=0;return strcmp(x,"created")==0;}
