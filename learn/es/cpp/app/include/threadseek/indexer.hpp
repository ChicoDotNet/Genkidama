#pragma once

#include <chrono>
#include <cstddef>
#include <cstdint>
#include <filesystem>
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

/// Discovers regular files recursively using one calling thread.
[[nodiscard]] std::vector<FileRecord> discover_files(const std::filesystem::path& root);

/// Discovers regular files by partitioning top-level subdirectories across bounded workers.
/// \param root Existing directory to scan.
/// \param worker_count Maximum workers requested; must be greater than zero.
/// \return Deterministically sorted records equivalent to discover_files().
[[nodiscard]] std::vector<FileRecord> discover_files_parallel(
    const std::filesystem::path& root,
    std::size_t worker_count);

/// Measures one discovery strategy with a monotonic clock.
/// \param worker_count Requested worker count for parallel mode; zero selects hardware_concurrency().
[[nodiscard]] DiscoveryReport measure_discovery(
    const std::filesystem::path& root,
    DiscoveryMode mode,
    std::size_t worker_count = 0);

/// Owns an in-memory file index and deterministic search operations.
class FileIndex {
public:
    /// Recursively indexes regular files under root.
    explicit FileIndex(const std::filesystem::path& root);

    /// Reconstructs an index from already discovered or persisted records.
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
    /// Writes the complete index through a temporary file and replaces destination.
    static void save(const FileIndex& index, const std::filesystem::path& destination);

    /// Loads an index previously written by save().
    /// Throws std::runtime_error for malformed or unreadable input.
    [[nodiscard]] static FileIndex load(const std::filesystem::path& source);
};

}  // namespace threadseek
