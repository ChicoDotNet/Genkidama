#include <functional>
#include <queue>
bool run(){std::queue<std::function<int()>>q;q.push([]{return 2;});q.push([]{return 3;});int n=q.front()();q.pop();n+=q.front()();q.pop();return n==5;}
