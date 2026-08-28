#include <map>
#include <string>
bool run(){struct R{int id;std::string name;};std::map<int,R>t;R r{1,"Ada"};t[r.id]=r;return t.at(1).name=="Ada";}
