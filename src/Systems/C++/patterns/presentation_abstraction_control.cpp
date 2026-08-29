#include <string>
bool run(){int a=3;auto c=[](int x){return x+1;};auto p=[](int x){return "n="+std::to_string(x);};return p(c(a))=="n=4";}
