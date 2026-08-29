#include <string>
bool run(){auto a=[](std::string m){return "a:"+m;};auto b=[](std::string m){return "b:"+m;};return b(a("hi"))=="b:a:hi";}
