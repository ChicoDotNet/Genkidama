#include <string>
bool run(){auto r=[](int id){return "item:"+std::to_string(id);};auto p=[&](int id){return r(id);};return p(7)=="item:7";}
