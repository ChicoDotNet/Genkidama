#include <map>
#include <string>
bool run(){std::map<std::string,int>h{{"price",9}};auto b=[&](std::string t){return h.contains(t)?h[t]:0;};return b("price")==9;}
