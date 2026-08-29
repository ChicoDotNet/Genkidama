#include <string>
bool run(){int m=5;auto vm=[](int x){return std::string("count:")+std::to_string(x);};return vm(m)=="count:5";}
