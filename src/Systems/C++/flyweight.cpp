#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <tuple>
#include <vector>

struct TextStyle { std::string font; int size; std::string color; };
class StyleFactory {
    std::map<std::string, std::shared_ptr<const TextStyle>> styles;
public:
    std::shared_ptr<const TextStyle> get(const std::string& font, int size, const std::string& color) {
        auto key = font + "|" + std::to_string(size) + "|" + color;
        auto it = styles.find(key);
        if (it == styles.end()) it = styles.emplace(key, std::make_shared<TextStyle>(TextStyle{font,size,color})).first;
        return it->second;
    }
    size_t count() const { return styles.size(); }
};
int main() {
    StyleFactory f; auto r1=f.get("Inter",12,"red"), r2=f.get("Inter",12,"red"), b=f.get("Inter",12,"blue");
    std::vector<std::tuple<char,int,std::shared_ptr<const TextStyle>>> g{{'A',1,r1},{'B',2,r2},{'C',3,b}};
    std::cout << "styles=" << f.count() << ";shared=" << (std::get<2>(g[0])==std::get<2>(g[1])?"true":"false") << ";text=ABC\n";
}
