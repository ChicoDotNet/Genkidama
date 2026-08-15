#include "threadseek/indexer.hpp"

#include <algorithm>
#include <cctype>
#include <fstream>
#include <iomanip>
#include <stdexcept>
#include <system_error>

namespace threadseek {
namespace {

std::string ascii_lower(std::string text) {
    std::transform(text.begin(), text.end(), text.begin(), [](const unsigned char ch) {
        return static_cast<char>(std::tolower(ch));
    });
    return text;
}

void sort_records(std::vector<FileRecord>& records) {
    std::sort(records.begin(), records.end(), [](const FileRecord& left, const FileRecord& right) {
        return left.path.generic_string() < right.path.generic_string();
    });
}

}  // namespace

std::vector<FileRecord> discover_files(const std::filesystem::path& root) {
    std::error_code error;
    if (!std::filesystem::exists(root, error) || error) {
        throw std::invalid_argument("La ruta raíz no existe: " + root.string());
    }
    if (!std::filesystem::is_directory(root, error) || error) {
        throw std::invalid_argument("La ruta raíz no es un directorio: " + root.string());
    }

    std::vector<FileRecord> records;
    const auto options = std::filesystem::directory_options::skip_permission_denied;
    std::filesystem::recursive_directory_iterator iterator(root, options, error);
    const std::filesystem::recursive_directory_iterator end;

    while (iterator != end) {
        if (error) {
            error.clear();
            iterator.increment(error);
            continue;
        }

        const auto& entry = *iterator;
        if (entry.is_regular_file(error) && !error) {
            const auto size = entry.file_size(error);
            if (!error) {
                records.push_back(FileRecord{entry.path(), size});
            }
        }
        error.clear();
        iterator.increment(error);
    }

    sort_records(records);
    return records;
}

FileIndex::FileIndex(const std::filesystem::path& root) : files_(discover_files(root)) {}

FileIndex::FileIndex(std::vector<FileRecord> records) : files_(std::move(records)) {
    sort_records(files_);
}

const std::vector<FileRecord>& FileIndex::files() const noexcept { return files_; }

std::vector<FileRecord> FileIndex::search(const std::string_view query) const {
    const auto normalized_query = ascii_lower(std::string(query));
    std::vector<FileRecord> matches;
    for (const auto& record : files_) {
        const auto filename = ascii_lower(record.path.filename().string());
        if (filename.contains(normalized_query)) {
            matches.push_back(record);
        }
    }
    return matches;
}

std::uintmax_t FileIndex::total_size_bytes() const noexcept {
    std::uintmax_t total = 0;
    for (const auto& record : files_) {
        total += record.size_bytes;
    }
    return total;
}

void IndexStore::save(const FileIndex& index, const std::filesystem::path& destination) {
    const auto temporary = destination.string() + ".tmp";
    {
        std::ofstream output(temporary, std::ios::binary | std::ios::trunc);
        if (!output) {
            throw std::runtime_error("No se pudo abrir el índice temporal para escritura");
        }
        output << "THREADSEEK\t1\n";
        for (const auto& record : index.files()) {
            output << std::quoted(record.path.generic_string()) << '\t' << record.size_bytes << '\n';
        }
        if (!output) {
            throw std::runtime_error("No se pudo escribir el índice completo");
        }
    }

    std::error_code error;
    std::filesystem::rename(temporary, destination, error);
    if (error) {
        std::filesystem::remove(destination, error);
        error.clear();
        std::filesystem::rename(temporary, destination, error);
    }
    if (error) {
        std::filesystem::remove(temporary, error);
        throw std::runtime_error("No se pudo reemplazar el índice persistido");
    }
}

FileIndex IndexStore::load(const std::filesystem::path& source) {
    std::ifstream input(source, std::ios::binary);
    if (!input) {
        throw std::runtime_error("No se pudo abrir el índice persistido");
    }

    std::string header;
    if (!std::getline(input, header) || header != "THREADSEEK\t1") {
        throw std::runtime_error("Formato de índice no reconocido");
    }

    std::vector<FileRecord> records;
    std::string path;
    std::uintmax_t size = 0;
    while (input >> std::quoted(path) >> size) {
        records.push_back(FileRecord{std::filesystem::path(path), size});
    }
    if (!input.eof()) {
        throw std::runtime_error("Índice persistido corrupto");
    }
    return FileIndex(std::move(records));
}

}  // namespace threadseek
