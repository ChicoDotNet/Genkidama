#include <cctype>
#include <string>
bool run(){auto h=[](std::string s){for(auto&c:s)c=(char)std::toupper((unsigned char)c);return s;};return "start>"+h("work")+">end"=="start>WORK>end";}
