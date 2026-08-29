#include <string>
#include <vector>
bool run(){std::vector<std::string>p={"insert","update"};auto c=p;p.clear();return c.size()==2&&p.empty();}
