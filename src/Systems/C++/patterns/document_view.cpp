#include <string>
bool run(){std::string d="abc";auto a=[](const std::string&s){return s.size();};auto b=[](const std::string&s){return std::distance(s.begin(),s.end());};return a(d)==3&&b(d)==3;}
