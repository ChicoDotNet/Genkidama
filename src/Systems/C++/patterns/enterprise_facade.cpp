#include <string>
bool run(){auto stock=[](std::string){return true;};auto pay=[](int n){return n==9;};auto facade=[&](std::string s,int n){return stock(s)&&pay(n);};return facade("A",9);}
