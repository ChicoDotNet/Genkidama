#include <functional>
#include <string>
bool run(){std::function<std::string(std::string)>i=[](std::string s){return "["+s+"]";};auto a=[&](std::string s){return i(s);};return a("x")=="[x]";}
