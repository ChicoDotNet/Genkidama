#include <string>
#include <vector>
bool run(){std::vector<std::string>t={"2","+","3"};int a=std::stoi(t[0]),b=std::stoi(t[2]);return (t[1]=="+"?a+b:0)==5;}
