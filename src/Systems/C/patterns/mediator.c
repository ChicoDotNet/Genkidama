#include <stdbool.h>
#include <string.h>
static const char*route(const char*m){return strcmp(m,"ping")==0?"pong":"unknown";}bool run(void){return strcmp(route("ping"),"pong")==0;}
