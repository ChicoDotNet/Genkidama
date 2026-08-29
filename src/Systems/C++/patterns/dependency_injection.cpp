#include <functional>
#include <string>
bool run(){std::function<std::string(int)>f=[](int n){return "#"+std::to_string(n);};auto s=[&]{return f(7);};return s()=="#7";}
