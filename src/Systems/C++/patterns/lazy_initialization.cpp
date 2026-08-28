#include <optional>
bool run(){std::optional<int>c;auto g=[&](){if(!c)c=7;return *c;};return g()==7&&g()==7;}
