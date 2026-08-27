#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>

class DocumentStore {
public:
    virtual ~DocumentStore() = default;
    virtual std::string get(int id) = 0;
};

class RemoteDocumentStore final : public DocumentStore {
public:
    std::string get(int id) override {
        ++fetch_count_;
        return "doc(" + std::to_string(id) + ")";
    }

    int fetch_count() const { return fetch_count_; }

private:
    int fetch_count_ = 0;
};

class DocumentStoreProxy final : public DocumentStore {
public:
    std::string get(int id) override {
        if (const auto it = cache_.find(id); it != cache_.end()) {
            return it->second;
        }
        if (!backend_) {
            backend_ = std::make_unique<RemoteDocumentStore>();
        }
        auto value = backend_->get(id);
        cache_.emplace(id, value);
        return value;
    }

    int backend_count() const { return backend_ ? 1 : 0; }
    int fetch_count() const { return backend_ ? backend_->fetch_count() : 0; }

private:
    std::unique_ptr<RemoteDocumentStore> backend_;
    std::unordered_map<int, std::string> cache_;
};

int main() {
    DocumentStoreProxy store;
    const auto first = store.get(42);
    const auto second = store.get(42);
    std::cout << "backend=" << store.backend_count()
              << ";fetches=" << store.fetch_count()
              << ";first=" << first
              << ";second=" << second << '\n';
}
