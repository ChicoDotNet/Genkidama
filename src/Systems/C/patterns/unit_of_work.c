#include <stdbool.h>
bool run(void){const char*p[2]={"insert","update"};int committed=2;p[0]=0;p[1]=0;return committed==2&&p[0]==0;}
