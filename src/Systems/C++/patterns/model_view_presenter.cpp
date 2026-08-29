#include <cctype>
#include <string>
bool run(){std::string m="ready";auto p=[](std::string s){for(auto&c:s)c=(char)std::toupper((unsigned char)c);return s;};return p(m)=="READY";}
