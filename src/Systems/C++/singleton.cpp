#include <iostream>

class ProcessRegistry {
public:
    static ProcessRegistry& instance() {
        static ProcessRegistry value;
        return value;
    }

    void increment() { ++count_; }
    [[nodiscard]] int count() const { return count_; }

    ProcessRegistry(const ProcessRegistry&) = delete;
    ProcessRegistry& operator=(const ProcessRegistry&) = delete;

private:
    ProcessRegistry() = default;
    int count_ = 0;
};

int main() {
    auto& first = ProcessRegistry::instance();
    auto& second = ProcessRegistry::instance();
    first.increment();
    std::cout << "same=" << std::boolalpha << (&first == &second) << '\n';
    std::cout << "count=" << second.count() << '\n';
}
