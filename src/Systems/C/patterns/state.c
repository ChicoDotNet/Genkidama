#include <stdbool.h>
bool run(void){enum S{IDLE,RUNNING};enum S s=IDLE;if(s==IDLE)s=RUNNING;return s==RUNNING;}
