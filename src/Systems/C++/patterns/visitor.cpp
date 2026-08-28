#include <string>
#include <variant>
#include <type_traits>
bool run(){using N=std::variant<int,std::string>;auto v=[](const N&n){return std::visit([](const auto&x)->int{using T=std::decay_t<decltype(x)>;if constexpr(std::is_same_v<T,int>)return x;else return(int)x.size();},n);};return v(N{3})+v(N{std::string("ab")})==5;}
