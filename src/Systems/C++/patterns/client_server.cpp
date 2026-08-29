#include <string>
bool run(){auto s=[](std::string r){return "echo:"+r;};auto c=[&](std::string v){return s(v);};return c("ping")=="echo:ping";}
