#include <string>
bool run(){auto stock=[](std::string s){return s=="A";};auto price=[](std::string s){return s=="A"?9:0;};return stock("A")&&price("A")==9;}
