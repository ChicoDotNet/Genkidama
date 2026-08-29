#include <stdbool.h>
struct M{int n;};static void inc(struct M*m){m->n++;}bool run(void){struct M m={0};inc(&m);return m.n==1;}
