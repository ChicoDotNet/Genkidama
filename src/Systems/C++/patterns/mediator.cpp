#include <string>
bool run(){auto m=[](std::string s){return s=="ping"?"pong":"unknown";};return m("ping")=="pong";}
