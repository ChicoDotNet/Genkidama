#include <functional>
#include <vector>
bool run(){std::vector<std::function<int(int)>>s={[](int x){return x+1;},[](int x){return x+2;}};return s[0](5)==6&&s[1](5)==7;}
