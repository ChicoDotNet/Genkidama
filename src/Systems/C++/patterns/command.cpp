#include <functional>
bool run(){int b=100;std::function<int(int)>o[]={[](int x){return x+50;},[](int x){return x-20;}};for(auto&f:o)b=f(b);return b==130;}
