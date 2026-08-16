#include "threadseek/indexer.hpp"

#include <algorithm>
#include <cctype>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <stdexcept>
#include <thread>
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

void validate_root(const std::filesystem::path& root) {
    std::error_code error;
    if (!std::filesystem::exists(root, error) || error) {
        throw std::invalid_argument("La ruta raíz no existe: " + root.string());
    }
    if (!std::filesystem::is_directory(root, error) || error) {
        throw std::invalid_argument("La ruta raíz no es un directorio: " + root.string());
    }
}

void append_regular_file(
    const std::filesystem::directory_entry& entry,
    std::vector<FileRecord>& records) {
    std::error_code error;
    if (entry.is_regular_file(error) && !error) {
        const auto size = entry.file_size(error);
        if (!error) {
            records.push_back(FileRecord{entry.path(), size});
        }
    }
}

std::vector<FileRecord> discover_subtree(const std::filesystem::path& root) {
    std::vector<FileRecord> records;
    std::error_code error;
    const auto options = std::filesystem::directory_options::skip_permission_denied;
    std::filesystem::recursive_directory_iterator iterator(root, options, error);
    const std::filesystem::recursive_directory_iterator end;

    while (iterator != end) {
        if (error) {
            error.clear();
            iterator.increment(error);
            continue;
        }
        append_regular_file(*iterator, records);
        error.clear();
        iterator.increment(error);
    }
    return records;
}

}  // namespace

std::vector<FileRecord> discover_files(const std::filesystem::path& root) {
    validate_root(root);
    auto records = discover_subtree(root);
    sort_records(records);
    return records;
}

std::vector<FileRecord> discover_files_parallel(
    const std::filesystem::path& root,
    const std::size_t worker_count) {
    validate_root(root);
    if (worker_count == 0) {
        throw std::invalid_argument("worker_count debe ser mayor que cero");
    }

    std::vector<FileRecord> direct_files;
    std::vector<std::filesystem::path> subdirectories;
    std::error_code error;
    const auto options = std::filesystem::directory_options::skip_permission_denied;
    std::filesystem::directory_iterator iterator(root, options, error);
    const std::filesystem::directory_iterator end;

    while (iterator != end) {
        if (error) {
            error.clear();
            iterator.increment(error);
            continue;
        }
        if (iterator->is_directory(error) && !error) {
            subdirectories.push_back(iterator->path());
        } else {
            error.clear();
            append_regular_file(*iterator, direct_files);
        }
        error.clear();
        iterator.increment(error);
    }

    if (subdirectories.empty()) {
        sort_records(direct_files);
        return direct_files;
    }

    std::sort(subdirectories.begin(), subdirectories.end());
    const auto actual_workers = std::min(worker_count, subdirectories.size());
    std::vector<std::vector<FileRecord>> local_batches(actual_workers);
    std::vector<std::jthread> workers;
    workers.reserve(actual_workers);

    for (std::size_t worker = 0; worker < actual_workers; ++worker) {
        workers.emplace_back([&, worker] {
            for (std::size_t index = worker; index < subdirectories.size(); index += actual_workers) {
                auto discovered = discover_subtree(subdirectories[index]);
                local_batches[worker].insert(
                    local_batches[worker].end(),
                    std::make_move_iterator(discovered.begin()),
                    std::make_move_iterator(discovered.end()));
            }
        });
    }
    workers.clear();  // jthread destruction joins before combining local batches.

    for (auto& batch : local_batches) {
        direct_files.insert(
            direct_files.end(),
            std::make_move_iterator(batch.begin()),
            std::make_move_iterator(batch.end()));
    }
    sort_records(direct_files);
    return direct_files;
}

DiscoveryReport measure_discovery(
    const std::filesystem::path& root,
    const DiscoveryMode mode,
    std::size_t worker_count) {
    const auto start = std::chrono::steady_clock::now();
    DiscoveryReport report;

    if (mode == DiscoveryMode::sequential) {
        report.records = discover_files(root);
        report.workers_requested = 1;
    } else {
        if (worker_count == 0) {
            worker_count = std::max<std::size_t>(1, std::thread::hardware_concurrency());
        }
        report.records = discover_files_parallel(root, worker_count);
        report.workers_requested = worker_count;
    }

    report.elapsed = std::chrono::steady_clock::now() - start;
    return report;
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
