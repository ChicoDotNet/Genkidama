#include <numeric>
#include <vector>
bool run(){std::vector<int>v={1,2,3};return std::accumulate(v.begin(),v.end(),0)==6;}
