#include <queue>
#include <string>
bool run(){std::queue<std::string>q;q.push("created");auto x=q.front();q.pop();return x=="created";}
