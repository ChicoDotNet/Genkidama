#include <mutex>
bool run(){struct M{std::mutex mu;int n=0;void inc(){std::lock_guard<std::mutex>g(mu);++n;}int get(){std::lock_guard<std::mutex>g(mu);return n;}}m;m.inc();return m.get()==1;}
