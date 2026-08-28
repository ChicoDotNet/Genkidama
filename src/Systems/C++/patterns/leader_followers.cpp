#include <algorithm>
#include <string>
#include <vector>
bool run(){std::vector<std::string>w={"leader","follower","follower"};return std::find(w.begin(),w.end(),"leader")==w.begin();}
