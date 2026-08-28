#include <string>
#include <utility>
bool run(){std::pair<int,std::string>r{1,"Ada"};struct D{int id;std::string name;};D d{r.first,r.second};return d.id==1&&d.name=="Ada";}
