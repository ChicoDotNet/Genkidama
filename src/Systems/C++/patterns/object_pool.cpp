#include <string>
#include <vector>
bool run(){std::vector<std::string>p={"conn"};auto x=p.back();p.pop_back();p.push_back(x);return p.size()==1&&p[0]=="conn";}
