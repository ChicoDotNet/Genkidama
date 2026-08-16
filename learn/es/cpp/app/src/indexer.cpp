#include "threadseek/indexer.hpp"

#include <algorithm>
#include <atomic>
#include <cctype>
#include <fstream>
#include <iomanip>
#include <iterator>
#include <mutex>
#include <stdexcept>
#include <system_error>
#include <thread>

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

bool records_equal(const std::vector<FileRecord>& left, const std::vector<FileRecord>& right) {
    if (left.size() != right.size()) {
        return false;
    }
    for (std::size_t index = 0; index < left.size(); ++index) {
        if (left[index].path != right[index].path || left[index].size_bytes != right[index].size_bytes) {
            return false;
        }
    }
    return true;
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

class DiscoveryContext {
public:
    explicit DiscoveryContext(const DiscoveryOptions& options) : options_(options) {}

    [[nodiscard]] bool stop_requested() const noexcept {
        return options_.stop_token.stop_requested();
    }

    void visit(const std::filesystem::path& path) {
        entries_visited_.fetch_add(1, std::memory_order_relaxed);
        notify(path);
    }

    void discovered() noexcept { files_discovered_.fetch_add(1, std::memory_order_relaxed); }
    void skipped() noexcept { entries_skipped_.fetch_add(1, std::memory_order_relaxed); }

    [[nodiscard]] DiscoveryProgress snapshot() const {
        return DiscoveryProgress{
            entries_visited_.load(std::memory_order_relaxed),
            files_discovered_.load(std::memory_order_relaxed),
            entries_skipped_.load(std::memory_order_relaxed),
            {}};
    }

private:
    void notify(const std::filesystem::path& path) {
        if (!options_.on_progress) {
            return;
        }
        std::scoped_lock lock(callback_mutex_);
        auto progress = snapshot();
        progress.current_path = path;
        options_.on_progress(progress);
    }

    const DiscoveryOptions& options_;
    std::atomic_size_t entries_visited_{0};
    std::atomic_size_t files_discovered_{0};
    std::atomic_size_t entries_skipped_{0};
    std::mutex callback_mutex_;
};

void inspect_regular_candidate(
    const std::filesystem::directory_entry& entry,
    std::vector<FileRecord>& records,
    DiscoveryContext& context,
    const bool already_visited = false) {
    if (!already_visited) {
        context.visit(entry.path());
    }
    if (context.stop_requested()) {
        return;
    }

    std::error_code error;
    const bool regular = entry.is_regular_file(error);
    if (error) {
        context.skipped();
        return;
    }
    if (!regular) {
        return;
    }

    const auto size = entry.file_size(error);
    if (error) {
        context.skipped();
        return;
    }
    records.push_back(FileRecord{entry.path(), size});
    context.discovered();
}

std::vector<FileRecord> discover_subtree(
    const std::filesystem::path& root,
    DiscoveryContext& context) {
    std::vector<FileRecord> records;
    std::error_code error;
    const auto options = std::filesystem::directory_options::skip_permission_denied;
    std::filesystem::recursive_directory_iterator iterator(root, options, error);
    const std::filesystem::recursive_directory_iterator end;

    if (error) {
        context.skipped();
        return records;
    }

    while (iterator != end && !context.stop_requested()) {
        if (error) {
            context.skipped();
            error.clear();
            iterator.increment(error);
            continue;
        }
        inspect_regular_candidate(*iterator, records, context);
        if (context.stop_requested()) {
            break;
        }
        error.clear();
        iterator.increment(error);
    }
    return records;
}

}  // namespace

std::vector<FileRecord> discover_files(const std::filesystem::path& root) {
    return discover_files_controlled(root).records;
}

std::vector<FileRecord> discover_files_parallel(
    const std::filesystem::path& root,
    const std::size_t worker_count) {
    return discover_files_parallel_controlled(root, worker_count).records;
}

ControlledDiscoveryReport discover_files_controlled(
    const std::filesystem::path& root,
    const DiscoveryOptions& options) {
    validate_root(root);
    DiscoveryContext context(options);
    auto records = discover_subtree(root, context);
    sort_records(records);
    return ControlledDiscoveryReport{std::move(records), context.snapshot(), context.stop_requested()};
}

ControlledDiscoveryReport discover_files_parallel_controlled(
    const std::filesystem::path& root,
    const std::size_t worker_count,
    const DiscoveryOptions& options) {
    validate_root(root);
    if (worker_count == 0) {
        throw std::invalid_argument("worker_count debe ser mayor que cero");
    }

    DiscoveryContext context(options);
    std::vector<FileRecord> direct_files;
    std::vector<std::filesystem::path> subdirectories;
    std::error_code error;
    const auto directory_options = std::filesystem::directory_options::skip_permission_denied;
    std::filesystem::directory_iterator iterator(root, directory_options, error);
    const std::filesystem::directory_iterator end;

    if (error) {
        context.skipped();
    }

    while (iterator != end && !context.stop_requested()) {
        if (error) {
            context.skipped();
            error.clear();
            iterator.increment(error);
            continue;
        }

        context.visit(iterator->path());
        if (context.stop_requested()) {
            break;
        }

        const bool directory = iterator->is_directory(error);
        if (error) {
            context.skipped();
        } else if (directory) {
            subdirectories.push_back(iterator->path());
        } else {
            inspect_regular_candidate(*iterator, direct_files, context, true);
        }

        error.clear();
        iterator.increment(error);
    }

    if (!context.stop_requested() && !subdirectories.empty()) {
        std::sort(subdirectories.begin(), subdirectories.end());
        const auto actual_workers = std::min(worker_count, subdirectories.size());
        std::vector<std::vector<FileRecord>> local_batches(actual_workers);
        std::vector<std::jthread> workers;
        workers.reserve(actual_workers);

        for (std::size_t worker = 0; worker < actual_workers; ++worker) {
            workers.emplace_back([&, worker] {
                for (std::size_t index = worker;
                     index < subdirectories.size() && !context.stop_requested();
                     index += actual_workers) {
                    auto discovered = discover_subtree(subdirectories[index], context);
                    local_batches[worker].insert(
                        local_batches[worker].end(),
                        std::make_move_iterator(discovered.begin()),
                        std::make_move_iterator(discovered.end()));
                }
            });
        }
        workers.clear();

        for (auto& batch : local_batches) {
            direct_files.insert(
                direct_files.end(),
                std::make_move_iterator(batch.begin()),
                std::make_move_iterator(batch.end()));
        }
    }

    sort_records(direct_files);
    return ControlledDiscoveryReport{
        std::move(direct_files), context.snapshot(), context.stop_requested()};
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

DiscoveryComparison compare_discovery(
    const std::filesystem::path& root,
    const std::size_t worker_count) {
    auto sequential = measure_discovery(root, DiscoveryMode::sequential);
    auto parallel = measure_discovery(root, DiscoveryMode::parallel, worker_count);
    const bool equivalent = records_equal(sequential.records, parallel.records);
    return DiscoveryComparison{std::move(sequential), std::move(parallel), equivalent};
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
