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

void write_file(const std::filesystem::path& path, const std::string& content) {
    std::ofstream output(path, std::ios::binary);
    output << content;
}

class TempDirectory {
public:
    TempDirectory() : path_(std::filesystem::temp_directory_path() / "threadseek-tests") {
        std::error_code error;
        std::filesystem::remove_all(path_, error);
        std::filesystem::create_directories(path_ / "docs");
        write_file(path_ / "README.md", "hola");
        write_file(path_ / "docs" / "Manual.TXT", "1234567890");
        write_file(path_ / "notes.txt", "abc");
    }

    ~TempDirectory() {
        std::error_code error;
        std::filesystem::remove_all(path_, error);
    }

    [[nodiscard]] const std::filesystem::path& path() const noexcept { return path_; }

private:
    std::filesystem::path path_;
};

class ParallelDirectory {
public:
    ParallelDirectory() : path_(std::filesystem::temp_directory_path() / "threadseek-parallel-tests") {
        std::error_code error;
        std::filesystem::remove_all(path_, error);
        std::filesystem::create_directories(path_ / "docs");
        std::filesystem::create_directories(path_ / "src" / "nested");
        write_file(path_ / "root.txt", "root");
        write_file(path_ / "docs" / "manual.txt", "manual");
        write_file(path_ / "src" / "main.cpp", "int main(){}\n");
        write_file(path_ / "src" / "nested" / "worker.cpp", "void work(){}\n");
    }

    ~ParallelDirectory() {
        std::error_code error;
        std::filesystem::remove_all(path_, error);
    }

    [[nodiscard]] const std::filesystem::path& path() const noexcept { return path_; }

private:
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

void parallel_discovery_matches_sequential_results() {
    const ParallelDirectory fixture;
    const auto sequential = threadseek::discover_files(fixture.path());
    const auto parallel = threadseek::discover_files_parallel(fixture.path(), 2);

    expect(sequential.size() == parallel.size(), "ambos modos deben descubrir el mismo número de archivos");
    for (std::size_t index = 0; index < sequential.size() && index < parallel.size(); ++index) {
        expect(sequential[index].path == parallel[index].path, "ambos modos deben conservar orden determinista");
        expect(sequential[index].size_bytes == parallel[index].size_bytes, "ambos modos deben conservar tamaños");
    }
}

void rejects_zero_parallel_workers() {
    const ParallelDirectory fixture;
    bool rejected = false;
    try {
        (void)threadseek::discover_files_parallel(fixture.path(), 0);
    } catch (const std::invalid_argument&) {
        rejected = true;
    }
    expect(rejected, "debe rechazar worker_count cero");
}

void measures_without_assuming_parallel_is_faster() {
    const ParallelDirectory fixture;
    const auto sequential = threadseek::measure_discovery(fixture.path(), threadseek::DiscoveryMode::sequential);
    const auto parallel = threadseek::measure_discovery(fixture.path(), threadseek::DiscoveryMode::parallel, 2);

    expect(sequential.records.size() == parallel.records.size(), "medir no debe cambiar resultados");
    expect(sequential.workers_requested == 1, "modo secuencial debe reportar un worker");
    expect(parallel.workers_requested == 2, "modo paralelo debe reportar workers solicitados");
    expect(sequential.elapsed.count() >= 0 && parallel.elapsed.count() >= 0, "duraciones deben ser válidas");
}

}  // namespace

int main() {
    indexes_regular_files_and_totals_bytes();
    searches_filename_case_insensitively();
    rejects_missing_root();
    persists_and_reconstructs_index();
    rejects_corrupt_index();
    parallel_discovery_matches_sequential_results();
    rejects_zero_parallel_workers();
    measures_without_assuming_parallel_is_faster();

    if (failures == 0) {
        std::cout << "8 pruebas pasaron\n";
        return 0;
    }
    std::cerr << failures << " pruebas fallaron\n";
    return 1;
}
