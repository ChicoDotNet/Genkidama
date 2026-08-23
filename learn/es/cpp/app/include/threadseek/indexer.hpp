#pragma once

#include <chrono>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <functional>
#include <stop_token>
#include <string>
#include <string_view>
#include <vector>

namespace threadseek {

/// Describes one regular file discovered by the indexer.
struct FileRecord {
    std::filesystem::path path;
    std::uintmax_t size_bytes{};
};

/// Selects the discovery strategy used by measure_discovery().
enum class DiscoveryMode {
    sequential,
    parallel,
};

/// Captures one measured discovery run without asserting that faster is always better.
struct DiscoveryReport {
    std::vector<FileRecord> records;
    std::chrono::nanoseconds elapsed{};
    std::size_t workers_requested{1};
};

/// Reports observable discovery progress without exposing internal worker state.
struct DiscoveryProgress {
    std::size_t entries_visited{};
    std::size_t files_discovered{};
    std::size_t entries_skipped{};
    std::filesystem::path current_path;
};

/// Optional cancellation and progress hooks for long-running discovery.
struct DiscoveryOptions {
    std::stop_token stop_token{};
    std::function<void(const DiscoveryProgress&)> on_progress{};
};

/// Captures a controlled discovery run, including cancellation and skipped entries.
struct ControlledDiscoveryReport {
    std::vector<FileRecord> records;
    DiscoveryProgress progress;
    bool cancelled{};
};

/// Compares sequential and parallel discovery without imposing a timing threshold.
struct DiscoveryComparison {
    DiscoveryReport sequential;
    DiscoveryReport parallel;
    bool equivalent{};
};

/// Discovers regular files recursively using one calling thread.
[[nodiscard]] std::vector<FileRecord> discover_files(const std::filesystem::path& root);

/// Discovers regular files by partitioning top-level subdirectories across bounded workers.
[[nodiscard]] std::vector<FileRecord> discover_files_parallel(
    const std::filesystem::path& root,
    std::size_t worker_count);

/// Runs sequential discovery with optional cooperative cancellation and progress reporting.
[[nodiscard]] ControlledDiscoveryReport discover_files_controlled(
    const std::filesystem::path& root,
    const DiscoveryOptions& options = {});

/// Runs bounded parallel discovery with shared cooperative cancellation and serialized progress callbacks.
[[nodiscard]] ControlledDiscoveryReport discover_files_parallel_controlled(
    const std::filesystem::path& root,
    std::size_t worker_count,
    const DiscoveryOptions& options = {});

/// Measures one discovery strategy with a monotonic clock.
/// \param worker_count Requested worker count for parallel mode; zero selects hardware_concurrency().
[[nodiscard]] DiscoveryReport measure_discovery(
    const std::filesystem::path& root,
    DiscoveryMode mode,
    std::size_t worker_count = 0);

/// Measures both strategies and reports whether they produced equivalent deterministic records.
[[nodiscard]] DiscoveryComparison compare_discovery(
    const std::filesystem::path& root,
    std::size_t worker_count);

/// Owns an in-memory file index and deterministic search operations.
class FileIndex {
public:
    explicit FileIndex(const std::filesystem::path& root);
    explicit FileIndex(std::vector<FileRecord> records);

    [[nodiscard]] const std::vector<FileRecord>& files() const noexcept;
    [[nodiscard]] std::vector<FileRecord> search(std::string_view query) const;
    [[nodiscard]] std::uintmax_t total_size_bytes() const noexcept;

private:
    std::vector<FileRecord> files_;
};

/// Persists and reconstructs indexes using a small text format.
class IndexStore {
public:
    static void save(const FileIndex& index, const std::filesystem::path& destination);
    [[nodiscard]] static FileIndex load(const std::filesystem::path& source);
};

}  // namespace threadseek
