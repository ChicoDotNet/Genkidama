#include <functional>
#include <vector>
bool run(){std::vector<std::function<int(int)>>o={[](int x){return x+1;},[](int x){return x*2;}};return o[0](3)==4&&o[1](3)==6;}
