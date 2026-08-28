#include <stdbool.h>
struct L{int v;bool init;};static int get(struct L*l){if(!l->init){l->v=7;l->init=true;}return l->v;}bool run(void){struct L l={0};return get(&l)==7&&get(&l)==7&&l.init;}
