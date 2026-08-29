#include <string>
bool run(){struct L{virtual int write(std::string)=0;virtual~L()=default;};struct N:L{int write(std::string)override{return 0;}}n;return n.write("ignored")==0;}
