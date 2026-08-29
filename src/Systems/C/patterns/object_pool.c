#include <stdbool.h>
struct P{int available;};bool run(void){struct P p={1};p.available--;int token=1;(void)token;p.available++;return p.available==1;}
