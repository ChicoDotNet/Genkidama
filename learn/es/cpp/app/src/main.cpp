#include "threadseek/indexer.hpp"

#include <exception>
#include <iostream>
#include <string_view>

int main(const int argc, const char* argv[]) {
    if (argc < 2 || argc > 3) {
        std::cerr << "Uso: threadseek <directorio> [texto]\n";
        return 2;
    }

    try {
        const threadseek::FileIndex index(argv[1]);
        const auto query = argc == 3 ? std::string_view(argv[2]) : std::string_view{};
        const auto matches = index.search(query);

        std::cout << "Indexados: " << index.files().size() << " archivos, "
                  << index.total_size_bytes() << " bytes\n";
        std::cout << "Coincidencias: " << matches.size() << '\n';
        for (const auto& record : matches) {
            std::cout << record.path.generic_string() << "\t" << record.size_bytes << " bytes\n";
        }
        return 0;
    } catch (const std::exception& exception) {
        std::cerr << "Error: " << exception.what() << '\n';
        return 1;
    }
}
