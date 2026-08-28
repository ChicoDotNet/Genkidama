#include <string>
bool run(){int m=4;auto c=[](int x){return x+1;};auto v=[](int x){return std::string("value=")+std::to_string(x);};return v(c(m))=="value=5";}
