#include <map>
#include <string>
bool run(){std::map<std::string,int>s{{"clock",7}};return s.at("clock")==7;}
