#include <iostream>
#include <memory>
#include <utility>
#include <vector>

class Component {
public:
    virtual ~Component() = default;
    virtual int size() const = 0;
};

class FileLeaf final : public Component {
public:
    explicit FileLeaf(int bytes) : bytes_(bytes) {}
    int size() const override { return bytes_; }

private:
    int bytes_;
};

class FolderComposite final : public Component {
public:
    explicit FolderComposite(std::vector<std::shared_ptr<Component>> children)
        : children_(std::move(children)) {}

    int size() const override {
        int total = 0;
        for (const auto& child : children_) {
            total += child->size();
        }
        return total;
    }

private:
    std::vector<std::shared_ptr<Component>> children_;
};

int main() {
    auto readme = std::make_shared<FileLeaf>(2);
    auto docs = std::make_shared<FolderComposite>(std::vector<std::shared_ptr<Component>>{
        std::make_shared<FileLeaf>(3), std::make_shared<FileLeaf>(5)});
    auto root = std::make_shared<FolderComposite>(std::vector<std::shared_ptr<Component>>{readme, docs});

    std::cout << "leaf=" << readme->size() << '\n';
    std::cout << "docs=" << docs->size() << '\n';
    std::cout << "root=" << root->size() << '\n';
}
