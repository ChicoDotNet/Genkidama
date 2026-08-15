#include "threadseek/indexer.hpp"

#include <filesystem>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>

namespace {

int failures = 0;

void expect(const bool condition, const std::string& message) {
    if (!condition) {
        ++failures;
        std::cerr << "FAIL: " << message << '\n';
    }
}

class TempDirectory {
public:
    TempDirectory() : path_(std::filesystem::temp_directory_path() / "threadseek-tests") {
        std::error_code error;
        std::filesystem::remove_all(path_, error);
        std::filesystem::create_directories(path_ / "docs");
        write(path_ / "README.md", "hola");
        write(path_ / "docs" / "Manual.TXT", "1234567890");
        write(path_ / "notes.txt", "abc");
    }

    ~TempDirectory() {
        std::error_code error;
        std::filesystem::remove_all(path_, error);
    }

    [[nodiscard]] const std::filesystem::path& path() const noexcept { return path_; }

private:
    static void write(const std::filesystem::path& path, const std::string& content) {
        std::ofstream output(path, std::ios::binary);
        output << content;
    }

    std::filesystem::path path_;
};

void indexes_regular_files_and_totals_bytes() {
    const TempDirectory fixture;
    const threadseek::FileIndex index(fixture.path());
    expect(index.files().size() == 3, "debe indexar tres archivos regulares");
    expect(index.total_size_bytes() == 17, "debe sumar 17 bytes");
}

void searches_filename_case_insensitively() {
    const TempDirectory fixture;
    const threadseek::FileIndex index(fixture.path());
    expect(index.search("txt").size() == 2, "debe encontrar dos nombres .txt");
}

void rejects_missing_root() {
    bool rejected = false;
    try {
        const threadseek::FileIndex index(std::filesystem::temp_directory_path() / "threadseek-missing-root");
        (void)index;
    } catch (const std::invalid_argument&) {
        rejected = true;
    }
    expect(rejected, "debe rechazar una raíz inexistente");
}

void persists_and_reconstructs_index() {
    const TempDirectory fixture;
    const threadseek::FileIndex original(fixture.path());
    const auto stored = fixture.path() / "index.threadseek";

    threadseek::IndexStore::save(original, stored);
    const auto restored = threadseek::IndexStore::load(stored);

    expect(restored.files().size() == 3, "debe reconstruir tres registros");
    expect(restored.total_size_bytes() == 17, "debe conservar tamaños");
    expect(restored.search("manual").size() == 1, "debe conservar búsqueda");
}

void rejects_corrupt_index() {
    const TempDirectory fixture;
    const auto stored = fixture.path() / "broken.threadseek";
    {
        std::ofstream output(stored);
        output << "NOPE\n";
    }

    bool rejected = false;
    try {
        const auto index = threadseek::IndexStore::load(stored);
        (void)index;
    } catch (const std::runtime_error&) {
        rejected = true;
    }
    expect(rejected, "debe rechazar formato inválido");
}

}  // namespace

int main() {
    indexes_regular_files_and_totals_bytes();
    searches_filename_case_insensitively();
    rejects_missing_root();
    persists_and_reconstructs_index();
    rejects_corrupt_index();

    if (failures == 0) {
        std::cout << "5 pruebas pasaron\n";
        return 0;
    }
    std::cerr << failures << " pruebas fallaron\n";
    return 1;
}
