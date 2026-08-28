#include <functional>
#include <vector>
bool run(){std::vector<std::function<int(int)>>p={[](int x){return x+1;},[](int x){return x*2;}};int v=3;for(auto&f:p)v=f(v);return v==8;}
